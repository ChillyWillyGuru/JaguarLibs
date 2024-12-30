; ============================================================
; $File:    spulib.s
; $Begin:   28.10.02
; $Autor:   (c)Robert Jurziga
; $Update:  21.12.02
; $Desc:    Jaguar Sound System library source.
; ============================================================

    .include    "rjaguar.inc"
    .include    "spulib.inc"
    .include    "spu_lib.inc"
    
    .iif    (^^defined libmode), .extern     SPULibrary

    .68000
    .text
    
;;;; String ID ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
    .include    "spuver.inc"
    .long

;=---------------------------------------------------------
; LibFunc:      _SPU_Init
; Description:  Initialize SPU
;
;   - Init Default SPU Data
;   - Copy DSP Code to local DSP ram #
;   - Clear DSP Voice Area
;   - Init Voice Tables #
;
;=---------------------------------------------------------
_SPU_Init:

    movem.l d1/a0/a1,-(sp)
    
; [Init Defualt SPU Data] ---------------------------------
    moveq   #MF_DEFAULT,d0                      ;
    move.w  d0,spul_MixFrequency(a6)            ; default mixup is 22050Hz
   
    moveq   #DEFAULT_VOICES,d0                  ; set default voices
    move.w  d0,spul_VoiceNumb(a6)               ; set voices

    moveq   #SPUMODE_NORMAL,d0                  ; set normal mode
    move.w  d0,spul_Mode(a6)                    ;
    
    moveq   #OSAMPLE_DEFAULT,d0                 ; set defualt
    move.w  d0,spul_cddaOSample(a6)             ; oversampling to NONE
    
    move.w  #MAX_CDDAVOLUME,d0
    move.w  d0,spul_MaxVolumeValue(a6)          ; set max. volume
    move.w  d0,spul_cddaVolume(a6)              ; set cdda volume to max
    
    moveq   #0,d0
    move.w  d0,spul_SFXVolume(a6)               ; set master volume

    move.w  CONFIG,d0                           ; which Jaguar TYPE
    andi.w  #%10000,d0                          ; maskout JagType
    move.w  d0,(a6)                             ; store JagType

    moveq   #0,d0
    move.w  d0,spul_cddaStatus(a6)              ; cddaStatus OFF    


; [Stop DSP code] -----------------------------------------
    lea     DSP_BASE,a0                         ; get the DSP base
    move.l  D_CTRL(a0),d0                       ; if DSP isn't Stoped
    btst    #0,d0                               ; then fore first to stop
    beq.b   .skipStop                           ;

    lea     $F1B000,a1
    move.l  #$9801A114,(a1)
    move.l  #$00F18C02,4(a1)
    move.l  #$BC22E400,8(a1)
    move.l  #$E400E400,12(a1)
    
    move.l  D_CTRL(a0),d0
    bset    #2,d0
    move.l  d0,D_CTRL(a0)

.waitDSP:
    move.l  D_CTRL(a0),d0
    btst    #0,d0
    bne.b   .waitDSP

.skipStop:                                      ;


; [Clear DSP local RAM] -----------------------------------
    lea     DSP_RAM,a0
    lea     DSP_RAM+8192,a1
    moveq   #0,d0
.cDSP:
    move.l  d0,(a0)+
    cmpa.l  a0,a1
    bne.b   .cDSP
    


; [Clear Voice Area] --------------------------------------
    lea     spul_Voices(a6),a0                  ; voice address space
    moveq   #0,d0                               ; zero
    move.w  #((Voice_SIZEOF*MAX_VOICES)/4)-1,d1 ; number of longs

.clrVoices:                                     ;
    move.l  d0,(a0)+                            ; set to ZERO
    dbra    d1,.clrVoices                       ;
            
            
; [Setup internal Voice Table] ----------------------------
    lea     spul_VoiceTable(a6),a0              ; this voiceTable
    lea     dsp_ShadowVoices,a1                 ;
    moveq   #MAX_VOICES-1,d0

.setVT:
    move.l  a1,(a0)+                            ; store voice pointer
    lea     Voice_SIZEOF(a1),a1                 ; next voice
    dbf     d0,.setVT                           ; set it
                    
    movem.l (sp)+,d1/a0/a1                      ;
    rts
        

 

   

;=---------------------------------------------------------
; LibFunc:  _SPU_EnableDSP
;           Start the DSP depended on mode
;           1. the DSP COde is alread loaded into
;              DSP's local ram.
;
; Output:
;   D0:     -1, failer
;   D0:     1, ok
;
; Trashes:
;   D0,D1,A0
;=---------------------------------------------------------
_SPU_EnableDSP:
    move.l  a5,-(sp)

; [Check if DSP is running] -------------------------------
    lea     DSP_BASE,a5                 ; if 
    move.l  D_CTRL(a5),d0               ; Dsp is running
    btst    #0,d0                       ; simply reset the
    bne.b   .isActive                   ; SMODE and CLK register
    
; [Set DSP's PC and CTRL] ---------------------------------

    move.l  #dsp_Init,D_PC(a5)          ; set the dsp_Init 
    moveq   #1,d0                       ; pointer and
    move.l  d0,D_CTRL(a5)               ; start the DSP

.isActive:    
; [Set JERRY's SCLK, SMODE] -------------------------------
    moveq   #0,d0                       ;
    move.w  spul_SMODE(a6),d0           ;
    move.l  d0,SMODE(a5)                ; set SMODE
                                        ;
    move.w  spul_SCLK(a6),d0            ; and 
    beq.b   .skipSCLK                   ;
    move.l  d0,SCLK(a5)                 ; set SCLK

                                        ;
; [Compute Timer1 interval and Samples per frame] ---------
.skipSCLK:                              ;
    bsr.w   _spuprivate_SetJerryTimer1  ;


; [UnMute Audio] ------------------------------------------
    move.w  #$100,JOYSTICK              ; unmute the sound
    movea.l (sp)+,a5                    ; and out
    rts 




;=---------------------------------------------------------
; LibFunc:  _SPU_DisableDSP
;           Stop the DSP.
;=---------------------------------------------------------
_SPU_DisableDSP:
    movem.l d1/d2/a5,-(sp)


; [Stop Timer] --------------------------------------------
    lea     JERRY_BASE,a5
    moveq   #0,d0
    move.w  d0,(a5)
    move.w  d0,JPIT1_DIV(a5)

; [Stop DSP] ----------------------------------------------
    moveq   #spuc_StopDSP,d0                ; issue the DSP
    moveq   #0,d1                           ; to stop itself
    moveq   #0,d2                           ;
    bsr.w   _spuprivate_ISPU                ; issue the SPU
    
    

; [Clear Pending IRQs] ------------------------------------
    lea     DSP_BASE,a5                     ; clear panding IRQs
    move.l  #%10001111100000000,(a5)        ; 17: D_EXT1CLR
                                            ; 13: D_EXT0CLR
                                            ; 12: D_TIM1CLR
                                            ; 11: D_TIM0CLR
                                            ; 10: D_I2SCLR
                                            ; 9 : D_CPUCLR

    movem.l (sp)+,d1/d2/a5                  ; return
    rts                                     ;
    




;=---------------------------------------------------------
; LibFunc:  _SPU_SetMode
; Desc:     Sets the mode of the SPU.
; In:       D0, mode, 0 - Normaal, CDDA
;           D1, mix frequecy
;
; Out:      D0, -1 - error
;           D0, 0   ok
;=---------------------------------------------------------
_SPU_SetMode:
    movem.l d1/d3/a0,-(sp)                      ; keep regs alive
                                                ;
    move.w  d0,d3                               ;    
    tst.w   d0                                  ; SPUMODE_NORMAL
    beq.b   .normal_mode                        ;
    subq.w  #1,d0                               ; -1
    beq.b   .cdda_mode                          ; SPUMODE_CDDA

    movem.l (sp)+,d1/d3/a0                      ; restore regs
    moveq   #-1,d0                              ; set failure
    rts                                         ;
    

; [Normal Mode] -------------------------------------------
.normal_mode:
    move.w  d3,spul_Mode(a6)                    ; save mode
    tst.w   d1                                  ; test frequency range
    blt.b   .nmdef                              ; 0 - MF_MAX
    cmpi.w  #MF_MAX,d1                          ;
    ble.b   .nm0                                ;

.nmdef:
    moveq   #MF_DEFAULT,d1                      ; set default

.nm0:
    move.w  d1,spul_MixFrequency(a6)            ; store MixFrequency


; [Calculate SCLK Value] ----------------------------------    
    move.l  #PAL_CLOCK>>6,d1                    ; PAL Clock /64
    tst.w   (a6)                                ; test Jaguar Type
    beq.b   .pal                                ; is pall
    move.l  #NTSC_CLOCK>>6,d1                   ; NTSC Clock /64

.pal:
    moveq   #0,d0                               ; 
    move.w  spul_MixFrequency(a6),d0            ; get mix frequency index
    add.w   d0,d0                               ; *2
    lea     MixFrequencies(pc),a0               ;
    move.w  (a0,d0.w),d0                        ; look up the frequncy value
    divu.w  d0,d1                               ; divide
    subq.w  #1,d1                               ; -1
    move.w  d1,spul_SCLK(a6)                    ; store SCLK value
    moveq   #$15,d0                             ;
    move.w  d0,spul_SMODE(a6)                   ; store SMODE value

    moveq   #0,d0                               ;
    move.w  d0,spul_cddaStatus(a6)              ; disable CDDA mode


; [Set Frequency Step Table] ------------------------------
    moveq   #0,d0                               ;
    move.w  spul_MixFrequency(a6),d0            ;
    lsl.w   #8,d0                               ; * 512
    add.w   d0,d0                               ;
    lea     FrequencyStepTables(pc),a0          ; step
    adda.l  d0,a0                               ; get pointer
    tst.w   (a6)    
    beq.b   .pal2                               ; is pall
    lea     6*512(a0),a0                        ; skip PAL Freqency steps

.pal2:
    move.l  a0,spul_StepTable(a6)               ; store frequcy step table pointer

    bsr.w   _spuprivate_CopyClassicSPUCore
    bra.b   .leave                              ; set Frequency step table

    
    
; [CDDA Mode] ---------------------------------------------
.cdda_mode:
    move.w  d3,spul_Mode(a6)                    ; set mode
    
    cmpi.b  #MF_11025,d1                        ; only
    beq.b   .cdda0                              ; 11025
    cmpi.b  #MF_22050,d1                        ; and
    beq.b   .cdda0                              ; 22050 allowed, else
    moveq   #MF_22050,d1                        ; defualt 22050 is set
    
.cdda0:
    move.w  d1,spul_MixFrequency(a6)            ; store mix freqency
    moveq   #0,d0                               ;
    move.w  d0,spul_SCLK(a6)                    ; set zero, trigger comes form CD unit
    moveq   #$14,d0                             ; set $14
    move.w  d0,spul_SMODE(a6)                   ; external Interrupt, trigger from CDDA


; [Calcuate Oversample Delay] -----------------------------
    bsr.w   _spuprivate_CalculateOSampleDelay   ; calculate the default OSample skipper
    

; [Set Freqency Steps] ------------------------------------
    moveq   #0,d0                               ; 0 = 11025Hz
    move.w  spul_MixFrequency(a6),d1            ;
    lea     FrequencyStepTables(pc),a0          ; step
    lea     12*512(a0),a0                       ; skip SPUNORMAL_MODE frequencies
    cmpi.w  #MF_22050,d1                        ;
    bne.b   .setF                               ;
    moveq   #1,d0                               ; 1 = 22050Hz

.setF:
    lsl.w   #8,d0                               ; *256+*2
    add.w   d0,d0                               ; * 512
    adda.l  d0,a0                               ; get pointer
    move.l  a0,spul_StepTable(a6)               ; store current step Table
    
    bsr.w   _spuprivate_CopyClassicSPUCore
    
.leave:    
    movem.l (sp)+,d1/d3/a0                      ;
    moveq   #0,d0                               ; set allright
    rts                                         ;
    




;=---------------------------------------------------------
; LibFunction:  InitJSP
; Purpose:      initilaize a Jaguar Sample Pack Image
;               The 16bit samples are in BigEndain already.
; IN:
;   A0:         Pointer to a JSP binary
;
; OUT:
;   D0:         -1  : Failt to initalize
;   D0:         0  : OK
;
;=---------------------------------------------------------

_SPU_InitJSP:
    movem.l d2/d5-d7/a2-a3,-(sp)        ; save registers
    tst.l   (a0)
    beq.w   .done    
    cmpi.l  #JSP_MAGIC,(a0)             ; is a 'JSP!'
    beq.b   .jsp0                       ; EQ->.jsp0
    moveq   #-1,d0                      ; return fault.
    bra.w   .leave

.jsp0:
    clr.l   (a0)                        ; clear to mark, its done already
    move.w  jsp_Samples(a0),d7          ; d7 = how many sample
    subq.w  #1,d7                       ; -1 for dbf
    lea     jsp_SIZEOF(a0),a1           ; get 1st offset
    moveq   #8,d6                       ; resolution
    
.jsp_next:
    move.l  (a1)+,d0                    ; get offset
    lea     (a0,d0.l),a2                ; get sample address
    
    move.l  jsps_size(a2),d0            ; sample size
    move.w  jsps_res(a2),d5             ; get resolution
    lea     jsps_SIZEOF(a2),a2          ; skip header
  
    cmp.w   d5,d6    
    beq.b   .its8bit
    
    
; [Addup Deltas 16bit] -----------------
;    add.l   d0,d0                       ; because of 16 bit
    lea     (a2,d0.l),a3                ; a3 end of sample    
    moveq   #0,d1                       ; 

.l16b:
    add.w   d1,(a2)
    move.w  (a2)+,d1
    cmpa.l  a2,a3
    bgt.b   .l16b
    bra.b   .next_sample
    
    
; [Addup Deltas 8bit] ------------------
.its8bit:
    lea     (a2,d0.l),a3                ; a3 end of sample
    moveq   #0,d1
        
.l8b:
    add.b   d1,(a2)
    move.b  (a2)+,d1
    cmpa.l  a2,a3
    bgt.b   .l8b
    
.next_sample:
    dbf     d7,.jsp_next

.done:
    moveq   #0,d0

.leave:
    movem.l (sp)+,d2/d5-d7/a2-a3        ; restore registers
    rts
    




;=---------------------------------------------------------
; LibFunc:      _SPU_InitRawSample
; Descripton:   Initialize a jsam structure with given data.
;
; Input:
;       A0: *sample
;       A1: spus Structure
;       D0: size            in bytes
;       D1: Volume          1-$100
;       D2: Pan             -127.0.127
;       D3: Resolution      8 or 16
;       D4: LoopType        0=no, 1:forward, 2:pingpong
;       D5: loopBegin       within size
;       D6: loopSize        +loopBegin within size
;       D7: flags           bit0: endian 0=be,1=le
;
;=---------------------------------------------------------
_SPU_InitRawSample:
    
    
; [Fill the Sample structure] -----------------------------    
    cmpi.w  #16,d3                  ; is a 16bit sample
    bne.b   .fill                   ; no-> goon
    lsr.l   #1,d0                   ; (sampleSize)  / 2
    lsr.l   #1,d5                   ; (loopBegin)   / 2
    lsr.l   #1,d6                   ; (loopSize)    / 2
    btst    #JSAMFLAG_ENDIAN,d7     ; test flags
    beq.b   .fill                   ; 0 == Big Endian
    
    movem.l d1/a0/a2,-(sp)          ; save registers

    movea.l a0,a2                   ; copy *sample
    adda.l  d0,a2                   ; *2 size
    adda.l  d0,a2                   ; to end of sample

.convEndian:
    move.w  (a0),d1                 ; get word
    ror.w   #8,d1                   ; swap endian
    move.w  d1,(a0)+                ; write back
    cmpa.l  a0,a2                   ; end reached
    bgt.b   .convEndian             ; no keep looping
 
    movem.l (sp)+,d1/a0/a2          ; restore trashed regs
    
    
; [Fill strucutre] ----------------------------------------
.fill:    
    move.l  a0,(a1)                 ; jsam_mem
    move.l  d0,jsam_size(a1)        ; jsam_size
    move.w  d4,jsam_loopType(a1)    ;
    beq.b   .noLoop
    cmp.l   d5,d0                   ; within bound ???
    ble.b   .noLoop
    move.l  d5,jsam_loopBegin(a1)   ; store loop begin
    add.l   d6,d5
    cmp.l   d5,d0
    bgt.b   .noLoop
    move.l  d6,jsam_loopSize(a1)    ; store loop size
    bra.b   .fillc0

.noLoop:
    moveq   #0,d5
    move.l  d5,jsam_loopBegin(a1)
    move.l  d5,jsam_loopSize(a1)

.fillc0:        
    move.w  d3,jsam_res(a1)
    move.w  d2,jsam_pan(a1)         ; store new pan
    move.l  d1,d0
    move.l  a1,-(sp)
    bsr.w   _spuprivate_CalcVolume
    movea.l (sp)+,a1
    move.w  d0,jsam_vol(a1)  
    rts
    
    


;=---------------------------------------------------------
; LibFunc:      _SPU_InitSample
; Descripton:   Initialize a jsam structure with given data.
;
; Input:
;       A0: *sample
;       A1: spus Structure
;
;=---------------------------------------------------------
_SPU_InitSample:
	moveq	#100,d1					; volume = 100%
    moveq   #PAN_CENTER,d2
	moveq	#0,d4					; no loop type
	moveq	#0,d5					; loopBegin = 0
	moveq	#0,d6					; loopSize  = 0
	moveq	#0,d7					; no Flags
	bsr.b	_spuprivate_IdentifySampleFormat
	tst.l	d0
	bne.b	.initStruct
	move.l	d1,d0
	rts


.initStruct:
    bra     _SPU_InitRawSample




;=---------------------------------------------------------
; Private LibFunc:	IdentifyFormat
; Descripton:   	Identify the sample Format.
;
; Input:
;       A0: *sample
;       A1: spus Structure
;
;=---------------------------------------------------------
_spuprivate_IdentifySampleFormat:	

    movem.l d0-d7/a0/a1/a2,-(sp)    
    
; [Check for Supported Formats] ---------------------------    
    move.l  (a0),d0                         ; get 1sst four bytes
    cmpi.l  #'RIFF',d0                      ; is WAVE
    beq.w   _jsam_WAVE                      ;
    cmpi.l  #'FORM',d0                      ; is IFF / AIFF
    beq.b   _jsam_IFFAIFF                   ;
    cmpi.l  #'.snd',d0                      ; is AU
    beq.w   _jsam_AU                        ;
    cmpi.l  #'Crea',d0                      ; is VOC
    beq.w   _jsam_VOC                       ;


_InitSampleFailt:
    lea     11*4(sp),sp                     ; none of all ->return
    moveq   #JSAM_FAILURE,d0                ; set failure
    rts                                     ; return


_jsam_InitSample:
	movem.l	(sp)+,d0-d7/a0-a2               ; return proper initialized
	rts                                     ; flags and data and return


_jsam_IFFAIFF:
    cmpi.l  #'8SVX',8(a0)                   ; is IFF
    beq.b   _jsam_IFF                       ; AMIGA IFF
	move.l	8(a0),d0
	andi.l	#$ffffff00,d0
	cmpi.l	#'AIF'<<8,d0
	beq.w	_jsam_AIFF


; ===========================------------------------ [IFF RETURN ERROR CODES]
_jsamerr_IFFFormatNotSupported:
	moveq	#JSAMERR_IFFFormatNotSupported,d1
	bra.b	_InitSampleFailt

_jsamerr_IFFNoDestinationBuffer:
	moveq	#JSAMERR_IFFNoDestBuffer,d1
	bra.b	_InitSampleFailt

_jsamerr_IFFCorrupt:
	moveq	#JSAMERR_IFFCorrupt,d1
	bra.b	_InitSampleFailt
		


; ===========================------------------------ [AMIGA IFF]
_jsam_IFF:
    move.l  #'VHDR',d0                      ; get the chunk: 'VHDR'
    move.l  iff_Size(a0),d1                 ;
    bsr.w   _jsam_GetIFFChunk               ;
    bmi.b   _jsamerr_IFFCorrupt             ; if was negative-> FAILT
    movea.l d0,a4                           ; a4 = header
	moveq	#20,d1
	cmp.l	iff_Size(a4),d1
	bne.b	_jsamerr_IFFCorrupt
        
    move.l  #'BODY',d0                      ; chunk BODY
    move.l  iff_Size(a0),d1                 ; within the total size of this sample
    bsr.w   _jsam_GetIFFChunk               ; get address of iff chunk 'BODY'
    bmi.b   _jsamerr_IFFCorrupt             ; if negative then sample failt

    movea.l d0,a5                           ; a5= *BODY
    lea     iff_SIZEOF(a5),a3               ; a3= *sample data
    move.l  a3,8*4(sp)                      ; a0 = *sample
    move.l	iff_Size(a5),d0					;
    move.l	d0,(sp)							; d0 = size
    
    tst.b	iffvhdr_sCompression(a4)		; test compression flag
    beq.b	.iffOK0							; 0 == not compressed
    tst.l	10(sp)							; test if destination address given
	beq.b	_jsamerr_IFFNoDestinationBuffer ;
	
.iffOK0:
	moveq	#1,d0
	cmp.b	iffvhdr_ctOctave(a4),d0			; only one octave ??
	bne.b	_jsamerr_IFFFormatNotSupported	;

	moveq	#8,d0							; set 8bit
	move.l	d0,3*4(sp)						; d3=resolution
    bra.w	_jsam_InitSample
    
    

; ===========================------------------------ [AIFF RETURN ERROR CODES]
_jsamerr_AIFFCorrupt:
	moveq	#JSAMERR_AIFFCorrupt,d1
	bra.w	_InitSampleFailt
		
_jsamerr_AIFFBitWidthNotSupported:
	moveq	#JSAMERR_AIFFBitWidthNotSupported,d1
	bra.w	_InitSampleFailt
	
_jsamerr_AIFFFormatNotSupported:
	moveq	#JSAMERR_AIFFFormatNotSupported,d1
	bra.w	_InitSampleFailt



; ===========================------------------------ [APPLE IFF]
_jsam_AIFF:
	move.l	#'COMM',d0
	move.l	iff_Size(a0),d1
	bsr.w	_jsam_GetIFFChunk
	bmi.b	_jsamerr_AIFFCorrupt
	movea.l	d0,a4

	move.l	aiffcomm_compressionID(a4),d0
	cmpi.l	#'ACE2',d0
	beq.w	_jsamerr_AIFFFormatNotSupported		
	cmpi.l	#'ACE8',d0
	beq.w	_jsamerr_AIFFFormatNotSupported		
	cmpi.l	#'MAC3',d0
	beq.w	_jsamerr_AIFFFormatNotSupported		
	cmpi.l	#'MAC6',d0
	beq.w	_jsamerr_AIFFFormatNotSupported		
		
	move.l	#'SSND',d0
	move.l	iff_Size(a0),d1
	bsr.w	_jsam_GetIFFChunk
	bmi.b	_jsamerr_AIFFCorrupt
	movea.l	d0,a5
	
	lea		aiffssnd_SIZEOF(a5),a3
	move.l	a3,8*4(sp)
	move.l	iff_Size(a5),(sp)

	move.w	aiffcomm_sampleSize(a4),d0
	move.w	aiffcomm_channels(a4),d1
	cmpi.w	#8,d0
	beq.b	.bitOk
	cmpi.w	#16,d0
	bne.w	_jsamerr_AIFFBitWidthNotSupported
	
.bitOk:
	ext.l	d0								; make long
	move.l	d0,3*4(sp)						; d3 = bitwidth
	subq.w	#1,d1							; channles -1
	bne.w	_jsamerr_AIFFFormatNotSupported		
	bra.w	_jsam_InitSample
	

; ===========================------------------------ [WAVE RETURN ERROR CODES]
_jsamerr_WAVECorrupt:
	moveq	#JSAMERR_WAVECorrupt,d1
	bra.w	_InitSampleFailt

_jsamerr_WAVEFormatNotSupported:
	moveq	#JSAMERR_WAVENotSupported,d1
	bra.w	_InitSampleFailt
	
_jsamerr_WAVEBitWidthNotSupported:
	moveq	#JSAMERR_BitWidthNotSupported,d1
	bra.w	_InitSampleFailt
	

; ===========================------------------------ [WAVE]
_jsam_WAVE:
	move.l	8(a0),d0					; get the WAVE id
	cmpi.l	#'WAVE',d0					; is it WAVE
	bne.b	_jsamerr_WAVECorrupt		; no-> wave corrupt

	move.l	#'fmt ',d0					; 'fmt ' id
	move.l	4(a0),d1					; get size
	bsr.w	_jsam_GetWAVEChunk			; search the chunk
	bmi.b	_jsamerr_WAVECorrupt		; error -> corrupt
	movea.l	d0,a4						; a4 = *fmt

	move.w	wavefmt_channels(a4),d1		; get channels
	ror.w	#8,d1						; to BE
	subq.w	#1,d1						; -1
	bne.b	_jsamerr_WAVEFormatNotSupported

	move.w	wavefmt_formatTag(a4),d1	; format
	ror.w	#8,d1						; swap endian
	subq.w	#1,d1						; is it $1 == no compression
	bne.b	_jsamerr_WAVEFormatNotSupported
	
	move.l	#'data',d0					; 'data'
	move.l	4(a0),d1					; within this size
	bsr.w	_jsam_GetWAVEChunk			; search chunk
	bmi.b	_jsamerr_WAVECorrupt		; error -> corrput
	movea.l	d0,a5						; a5 = *data
	
	lea		iff_SIZEOF(a5),a3			; skip 'data'size
	move.l	a3,8*4(sp)					; *sample = a3
	move.l	iff_Size(a5),d0				; get the size
	rol.w	#8,d0						; to Big Endain
	swap	d0							;
	rol.w	#8,d0						;
	move.l	d0,(sp)						; size = wave size

	move.w	wavefmt_bitsPerSample(a4),d1
	ror.w	#8,d1						; to
	cmpi.w	#8,d1						; is this a 8bit wave
	bne.b	.wave16

	movea.l	a3,a2						; 8bit wave -> relocate ZERO point
	adda.l	d0,a3
	move.b	#$80,d0
.l0:add.b	d0,(a2)+
	cmpa.l	a2,a3
	bne.b	.l0

	moveq	#8,d0
	move.l	d0,3*4(sp)
	bra.w	_jsam_InitSample


; -- (16bit wave) ---------------------
.wave16:
	cmpi.w	#16,d1
	bne.w	_jsamerr_WAVEBitWidthNotSupported
	ext.l	d1
	move.l	d1,3*4(sp)
	moveq	#1,d1
	move.l	d1,7*4(sp)					; flag to swap endian
	bra.w	_jsam_InitSample
	
	

; ===========================------------------------ [WAVE RETURN ERROR CODES]
_jsamerr_VOCCorrupt:
	moveq	#JSAMERR_VOCCorrupt,d1
	bra.w	_InitSampleFailt


; ===========================------------------------ [VOC]
_jsam_VOC:
	movea.l	a0,a1
	lea		.creative(pc),a2
	moveq	#19,d0

.ccrea:
	cmpm.b	(a2)+,(a1)+
	dbne	d0,.ccrea
	tst.w	d0
	bge.w	_jsamerr_VOCCorrupt		; no-> wave corrupt

	movea.l	a0,a4
	moveq	#0,d0
	move.w	voc_headerSize(a4),d0
	ror.w	#8,d0
	lea		(a0,d0.l),a0
	
	move.w	voc_versionNumber(a4),d0
	move.w	voc_versionNumberCheck(a4),d1
	ror.w	#8,d0
	ror.w	#8,d1
	not.w	d0
	addi.w	#$1234,d0
	cmp.w	d0,d1
	bne.w	_jsamerr_VOCCorrupt
	
	moveq	#1,d0
	bsr.w	_jsam_GetVOCChunk
	bmi.w	_jsamerr_VOCCorrupt
	
	movea.l	d0,a5
	addq.l	#1,a5
	
	
	bra.w	_jsam_InitSample
	

; -- (Creative ID String) -------------
.creative:	dc.b	"Creative Voice File",$1a
    .long


_jsam_AU:
    bra.w   _InitSampleFailt
	


;=---------------------------------
;Function:		jsam_GetIFFChunk
;Description:	Find a desired chunk in a desired block at
;				beginning at A0
; Input:
;	A0:	*begin
;	D0: chunk
;	D1: within block
;
; Output:
;	D0:	-1 	= failure
;		<>0 = address
;
;=---------------------------------

_jsam_GetIFFChunk:
    movea.l a0,a1

.sChunk:
    cmp.l   (a1),d0
    beq.b   .chunkFound
    addq.l  #2,a1
    subq.l  #2,d1
    bgt.b   .sChunk
    moveq   #-1,d0
    rts

.chunkFound:
    move.l  a1,d0
    rts
    


;=---------------------------------
;Function:		jsam_GetWAVEChunk
;Description:	Get a wave chunk out of the binary
;				beginning at A0
; Input:
;	A0:	*begin
;	D0: chunk
;	D1: within block
;
; Output:
;	D0:	-1 	= failure
;		<>0 = address
;
;=---------------------------------
_jsam_GetWAVEChunk:
	ror.w	#8,d1					; change 32bit endian
	swap	d1						;
	ror.w	#8,d1					;
	bra.b	_jsam_GetIFFChunk		; and search standard 'xxxx' chunk





;=---------------------------------
;Function:		jsam_GetVOCChunk
;Description:	Get a wave chunk out of the binary
;				beginning at A0
; Input:
;	A0:	*begin
;	D0: chunk
;	D1: within block
;
; Output:
;	D0:	-1 	= failure
;		<>0 = address
;
;=---------------------------------
_jsam_GetVOCChunk:	
	movea.l	a0,a1
	movea.l	a0,a2
	adda.l	#$10000,a2
	
.sChunk:
	cmp.b	(a1),d0
	beq.b	.chunkFound
	moveq	#0,d2
	move.b	(a1)+,d2
	beq.b	.vocerr
	cmpi.b	#8,d2
	bhi.b	.vocerr
	add.w	d2,d2
;	add.w	d2,d2
	jsr		.vocChunks(pc,d2.w)
	cmpa.l	a1,a2
	bhi.b	.sChunk	

.vocerr:
	moveq	#-1,d0
	rts
	
.chunkFound:
	move.l	a1,d0
	rts


; -- (VOC Chunks Jump Table) ----------
.vocChunks:
	bra.b	.vocTerminator
	bra.b	.vocSoundData
	bra.b	.vocSoundContinue
	bra.b	.vocSilence
	bra.b	.vocMarker
	bra.b	.vocASCII
	bra.b	.vocRepeat
	bra.b	.vocEndRepeat
	bra.b	.vocExtended

.vocTerminator:
	rts

.vocSoundData:
	bsr.b	.vocGet3ByteInt
	addq.l	#2,d3
	adda.l	d3,a1
	rts
	
.vocSoundContinue:
	bsr.b	.vocGet3ByteInt
	adda.l	d3,a1
	rts

.vocSilence:
	addq.l	#3,a1
	rts
	
.vocMarker:
	addq.l	#2,a1
	rts
	
.vocASCII:
	bsr.b	.vocGet3ByteInt
	addq.l	#1,d3
	adda.l	d3,a1
	rts

.vocRepeat:
	addq.l	#2,a1
	rts

.vocEndRepeat:
	rts

.vocExtended:
	addq.l	#4,a1
	rts
	
.vocGet3ByteInt:
	moveq	#0,d3
	lea		3(a1),a3		; get end of size
	move.b	(a3),d3
	lsl.l	#8,d3
	move.b	-(a3),d3
	lsl.l	#8,d3
	move.b	-(a3),d3
	rts

	

;=---------------------------------------------------------
; LibFunc:      _SPU_SetCDDAOSample
; Description:  Change the oversampling volume
;
; Input:
;   D0:         0: none
;               1: 2x
;               2: 4x
;               3: 8x
;
;=---------------------------------------------------------

_SPU_SetCDDAOSample:
    tst.w   d0
    blt.b   .skip
    cmpi.w  #OSAMPLE_8X,d0
    bgt.b   .skip

    move.l  d1,-(sp)
    move.l  d2,-(sp)
    
    move.w  d0,spul_cddaOSample(a6)    
    bsr.w   _spuprivate_CalculateOSampleDelay
    
    moveq   #0,d1
    move.w  spul_cddaDelay(a6),d1
    moveq   #0,d2
    moveq   #spuc_SetCDDADelay,d0
    bsr.w   _spuprivate_ISPU
    
    move.l  (sp)+,d2
    move.l  (sp)+,d1
    
.skip:
    rts




;=---------------------------------------------------------
; LibFunc:      _SPU_SetCDDAVolume
; Description:  Change the CDDA volume.
;
; Input:
;   D0:         volume 1-100
;
;=---------------------------------------------------------

_SPU_SetCDDAVolume:
    movem.l d1/d2,-(sp)
    
    bsr.w   _spuprivate_CalcVolume          ; convert volume 1-100 to $0000-$4000
    move.w  d0,spul_cddaVolume(a6)          ; save cddaVolume
            
    move.l  d0,d1                           ;
    moveq   #0,d2                           ;
    moveq   #spuc_SetCDDAVolume,d0          ;
    bsr.w   _spuprivate_ISPU                ; issue DSP to take CDDA volume

    movem.l (sp)+,d1/d2
    rts
    
    



;=---------------------------------------------------------
; LibFunc:      _SPU_EnableCDDA
; Description:  Enable CDDA support
;
; Input:
;       none
;
;=---------------------------------------------------------

_SPU_EnableCDDA:
    movem.l d1/d2,-(sp)

    moveq   #1,d1
    cmp.w   spul_cddaStatus(a6),d1
    beq.b   .doneAlready
        
        
; [Set CDDA Status ON] ----------------------------- ------

    moveq   #0,d1                           ; set delay flag
    move.w  spul_cddaDelay(a6),d1           ;
    moveq   #0,d2                           ;
    moveq   #spuc_SetCDDADelay,d0           ;
    bsr.w   _spuprivate_ISPU                ;
                                            ;
    moveq   #0,d1                           ;
    move.w  spul_cddaVolume(a6),d1          ; save cddaVolume
    moveq   #0,d2                           ;
    moveq   #spuc_SetCDDAVolume,d0          ;
    bsr.w   _spuprivate_ISPU                ;
                                            ;
    move.w  d1,spul_cddaStatus(a6)          ;
    moveq   #0,d2                           ; 
    moveq   #spuc_SetCDDAStatus,d0          ; SPU Command: Set CDDA Status
    bsr.w   _spuprivate_ISPU                ; Issue the SPU
                                            ;                                    
.doneAlready:
    movem.l (sp)+,d1/d2
    rts
    




;=---------------------------------------------------------
; LibFunc:      _SPU_DisableCDDA
; Description:  Change the CDDA status 
;
; Input:
;   D0:         0: off
;               1: on
;
;=---------------------------------------------------------
_SPU_DisableCDDA:
    movem.l d1/d2,-(sp)

    tst.w   spul_cddaStatus(a6)
    beq.b   .doneAlready    
    
; [set CDDA flag to off] ------------------------
    moveq   #0,d1                           ; 
    move.w  d1,spul_cddaStatus(a6)          ; set status to off
    moveq   #0,d2                           ; 
    moveq   #spuc_SetCDDAStatus,d0          ; SPU Command: Set CDDA Status
    bsr.w   _spuprivate_ISPU                ; Issue the SPU

.doneAlready:
    movem.l (sp)+,d1/d2
    rts
    



    



;=---------------------------------------------------------
; LibFunc:      _SPU_SetVoiceNumb
; Description:  Sets the voice number that will beeing
;               proccessed by the DSP.
;               Maximu are MAX_VOICES
;
; Input:
;   D0:         voices
;
;=---------------------------------------------------------

_SPU_SetVoiceNumber:
    move.l  d1,-(sp)
    move.l  d2,-(sp)
    
    tst.b   d0
    ble.b   .default
    cmpi.b  #MAX_VOICES,d0
    ble.b   .setIT

.default:
    moveq   #DEFAULT_VOICES,d0
    
.setIT:
    move.w  d0,spul_VoiceNumb(a6)
    ext.l   d0
    move.l  d0,d1
    moveq   #0,d2
    moveq   #spuc_SetVoiceNumber,d0
    bsr.w   _spuprivate_ISPU    

    move.l  (sp)+,d2
    move.l  (sp)+,d1
    rts




;=---------------------------------------------------------
; LibFunc:      _SPU_GetVoiceNumber
; Description:  Gets the number of voices processed by the
;               DSP
;
; Input:
;   <none>
;
; Output:
;   D0:         number of voices
;
;=---------------------------------------------------------
_SPU_GetVoiceNumber:
    moveq   #0,d0
    move.w  spul_VoiceNumb(a6),d0
    rts



;=---------------------------------------------------------
; LibFunc:      _SPU_GetMixFreq
; Description:  Return thes current mix frequency as index
;
; Input:
;   none
;
; Output:
;   D0:        current mix freqency index
;=---------------------------------------------------------
_SPU_GetMixFreq:
    moveq   #0,d0
    move.w  spul_MixFrequency(a6),d0
    rts




;=---------------------------------------------------------
; LibFunc:      _SPU_GetCDDAVolume
; Description:  Return the current cdda volume
;
; Input:
;   none
;
; Output:
;   D0:        current cddaVolume
;=---------------------------------------------------------
_SPU_GetCDDAVolume:
    move.w  spul_cddaVolume(a6),d0
    bra.w   _spuprivate_GetVolumePercent

    
    
    
;=---------------------------------------------------------
; LibFunc:      _SPU_GetOSample
; Description:  Return current oversampling value
;
; Input:
;   none
;
; Output:
;   D0:        current cddaOSample
;=---------------------------------------------------------
_SPU_GetCDDAOSample:
    moveq   #0,d0
    move.w  spul_cddaOSample(a6),d0
    rts
    
    


;=---------------------------------------------------------
; LibFunc:      _SPU_GetNoteFreq
; Description:  Return note frequency.
;
; Input:
;   d0:         note
;
; Output:
;   D0:         Note freqency
;=---------------------------------------------------------
_SPU_GetNoteFreq:
    tst.w   d0
    ble.b   .leave
    cmpi.w  #MAX_NOTE,d0
    bgt.b   .leave

    move.l  a0,-(sp)
    movea.l spul_StepTable(a6),a0
    add.w   d0,d0
    add.w   d0,d0
    move.l  (a0,d1.w),d0
    movea.l (sp)+,a0

.leave:
    rts
    


    
;=---------------------------------------------------------
; LibFunc:      _SPU_GetJaguarType
; Description:  Return Jaguar screen refresh rate to
;               the Jaguar Type
; Input:
;   none
;
; Output:
;   D0:         50 - PAL
;               60 - NTSC
;=---------------------------------------------------------
_SPU_GetJaguarType:
    moveq   #50,d0
    tst.w   (a6)
    beq.b   .ret
    moveq   #60,d0
.ret:
    rts
        


;=---------------------------------------------------------
; LibFunc:      _SPU_KeyOn
; Description:  Find a free voice, and initialise the desierd
;               sample to be played. spul_voice is a static
;               voice structure which will be filled with
;               the desired data.
;
; Input:
;   A0:         *JSP, ==0 A1 = sample stucture
;   D0:         Sample
;   D1:         Note
;
; Output:
;   D0:         0, no free voice
;             <>0, *Shadow Voice 
;=---------------------------------------------------------
_SPU_KeyOn:
    tst.w   d1                                  ; IF note <=0
    ble.b   .noNote                             ; and
    cmpi.w  #NOTE_B7,d1                         ; > B7
    ble.b   .playNote                           ; then leave
.noNote:
    moveq   #0,d0                               ; return 0
    rts                                         ;
    
.playNote:
    movem.l d0/d6/d7/a4/a5,-(sp)                ;
    moveq   #0,d7                               ; set to zero
    move.l  d7,(sp)                             ; store return value
        
    lea     spul_Voice(a6),a5                   ; a5 = voice data to fill
    move.l  a0,d7                               ;
    bne.b   .useJSP                             ;


; [Use JSAM Structure for replay] -------------------------    
.useJSAM:
    move.l  a1,d7                               ; this parameter
    beq.b   .leaveProc                          ; must not be 0
    bsr.w   _spuprivate_SetJSAMSample           ; and set it.
    bra.b   .setVoice                           ;
        
; [Use JSP! for replay] -----------------------------------
.useJSP:
    bsr.w   _spuprivate_SetJSPSample            ; set JSP sample
    bmi.b   .leaveProc                          ; means wrong Parameter
    
.setVoice:    
    moveq   #spuc_SetVoice,d0                   ; issue the SPU to take voice
    move.l  a5,d1                               ; use user voice as parameter
    moveq   #0,d2                               ; search voice indicator
    bsr.w   _spuprivate_ISPU                    ; doit
    bsr.w   _spuprivate_RSPU                    ; get back the in fact used voice or 0
    move.l  d0,(sp)                             ; store return value
    
.leaveProc:    
    movem.l (sp)+,d0/d6/d7/a4/a5                ;
    rts
    
    
         
    


;=---------------------------------------------------------
; LibFunc:      _SPU_KeyOnVoice
; Description:  Find a free voice, and initialise desierd
;               sample to be played
;
; Input:
;   A0:         *JSP, ==0 A1 = sample stucture
;   D0:         Voice
;   D1:         Sample
;   D2:         Note
;
; Output:
;   D0:         0, no free voice
;             <>0, *Shadow Voice 
;=---------------------------------------------------------
_SPU_KeyOnVoice:
    tst.w   d0                              ; is the voice within the
    ble.b   .dnp                            ; range processed by the DSP
    cmp.w   spul_VoiceNumb(a6),d0           ; right voice
    bgt.b   .dnp
    tst.w   d2                              ; a NULL note or below
    ble.b   .dnp                            ; leave  ->leave
    cmpi.w  #NOTE_B7,d2                     ; highre than B7
    ble.b   .playVoice
.dnp:
    moveq   #0,d0
    rts    

; [Play the specified voice function] -------
.playVoice:
    movem.l d0/d6/d7/a4/a5,-(sp)            ; save registers
    moveq   #0,d7
    move.l  d7,(sp)

    lea     spul_VoiceTable(a6),a4          ; a4 = spul_VoiceTable (DSP voices)
    lea     spul_Voice(a6),a5               ; a5 = *to fill structure
    add.w   d0,d0                           ; *
    add.w   d0,d0                           ; * 4
    move.l  (a4,d0.w),d6                    ; d6 = get the target voice pointer

    move.l  a0,d7                           ;
    bne.b   .useJSP                         ;
        

; [Use JSAM as playing source] --------------
.useJSAM:
    move.l  a1,d7                           ; if NULL
    beq.b   .leaveProc                      ; leave
    move.w  d2,d1                           ; note must be in D1
    bsr.w   _spuprivate_SetJSAMSample       ; JSAM Sample
    bra.b   .setVoice                       ;
    


; [Use JSP! as playing source] ------------------
; -----------------------------------------------
.useJSP:                                    ; ---------------------------------
    move.w  d1,d0                           ; samples
    move.w  d2,d1                           ; Note
    bsr.w   _spuprivate_SetJSPSample        ; Set JSP Sample subroutine
    bmi.b   .leaveProc                      ;
       
.setVoice:
    moveq   #spuc_SetVoice,d0               ; issue the SPU to take voice
    move.l  a5,d1                           ; use user voice as parameter
    move.l  d6,d2                           ; search voice indicator
    bsr.w   _spuprivate_ISPU                ; doit
    bsr.w   _spuprivate_RSPU                ; get back the in fact used voice or 0    
    move.l  d0,(sp)                         ; store return value
    
.leaveProc:                                 ; return saved register
    movem.l (sp)+,d0/d6/d7/a4/a5            ; with the return code in D0
    rts                                     ; return
    
    



;=---------------------------------------------------------
; LibFunc:      _SPU_KeyOnSync
; Description:  Find a free voice withing the v_flag VOICEF_LOCK voices,
;               and initialise the desierd sample to be played.
;               spul_voice is a static voice structure which will be filled with
;               the desired data.
;
; Input:
;   A0:         *JSP, ==0 A1 = sample stucture
;   D0:         Sample
;   D1:         Note
;
; Output:
;   D0:         0, no free voice
;             <>0, *Shadow Voice 
;=---------------------------------------------------------
_SPU_KeyOnSync:
    tst.w   d1                                  ; IF note <=0
    ble.b   .noNote                             ; and
    cmpi.w  #NOTE_B7,d1                         ; > B7
    ble.b   .searchVoice                        ; then leave
.noNote:
    moveq   #0,d0                               ; return 0
    rts                                         ;


; [Search a free voice] -----------------------------------
.searchVoice:
    movem.l d0/d5/d6/d7/a4/a5,-(sp)                ;
    moveq   #0,d7                               ; set to zero
    move.l  d7,(sp)                             ; store return value
    
    lea     spul_Voices(a6),a5                  ; get the voices array
    move.w  spul_VoiceNumb(a6),d7               ; how many voices
    subq.w  #1,d7                               ; loop count -1
    moveq   #VOICEF_SYNC,d6                     ; get the flag search for

.searchVoiceLoop:
    move.l  v_flags(a5),d5                      ; get flags
    and.l   d6,d5                               ; is the VOICEF_LOCK set ??
    beq.b   .nextVoice                          ; skip , its not

    tst.l   (a5)                                ; is this voice free
    beq.b   .playNote                           ; yea! -> play note
    
.nextVoice:
    lea     Voice_SIZEOF(a5),a5
    dbra    d7,.searchVoiceLoop
    
.leaveProc:    
    movem.l (sp)+,d0/d5/d6/d7/a4/a5             ;
    rts
    

; [Play the note] -----------------------------------------
.playNote:        
    sub.w   spul_VoiceNumb(a6),d7               ; scan counter
    neg.w   d7                                  ; negate
    lea     spul_VoiceTable(a6),a4              ; get the Voice Table
    add.w   d7,d7                               ;
    add.w   d7,d7                               ; *4
    move.l  -4(a4,d7.w),d7                      ; get found voice
    move.l  d7,(sp)                             ; store as return value
    
    move.l  a0,d7                               ;
    bne.b   .useJSP                             ;

; [Use JSAM Structure for replay] -------------------------    
.useJSAM:
    move.l  a1,d7
    beq.b   .leaveProc
    bsr.w   _spuprivate_SetJSAMSample    
    bra.b   .leaveProc


; [Use JSP Structure for replay] --------------------------
.useJSP:
    bsr.w   _spuprivate_SetJSPSample
    bra.b   .leaveProc
    




;=---------------------------------------------------------
; LibFunc:      _SPU_KeyOnVoiceSync
; Description:  Test if the desired voice is marked as synced
; 
;
; Input:
;   A0:         *JSP, ==0 A1 = sample stucture
;   D0:         Voice
;   D1:         Sample
;   D2:         Note
;
; Output:
;   D0:         0, no free voice
;             <>0, *Shadow Voice 
;=---------------------------------------------------------
_SPU_KeyOnVoiceSync:
    tst.w   d0                              ; is the voice within the
    ble.b   .dnp                            ; range processed by the DSP
    cmp.w   spul_VoiceNumb(a6),d0           ; right voice
    bgt.b   .dnp
    tst.w   d2                              ; a NULL note or below
    ble.b   .dnp                            ; leave  ->leave
    cmpi.w  #NOTE_B7,d2                     ; highre than B7
    ble.b   .playVoice
.dnp:
    moveq   #0,d0
    rts    

; [Play the specified voice function] -------
.playVoice:
    movem.l d0/d6/d7/a4/a5,-(sp)            ; save registers
    moveq   #0,d7
    move.l  d7,(sp)                         ; clear return flag

    lea     spul_VoiceTable(a6),a4          ; a4 = spul_VoiceTable (DSP voices)
    lea     spul_Voice(a6),a5               ; a5 = *to fill structure
    add.w   d0,d0                           ; *
    add.w   d0,d0                           ; * 4
    movea.l (a4,d0.w),a4                    ; a4 = get the target voice pointer
    moveq   #VOICEF_SYNC,d7                 ; get VOICEF_SYNC
    and.l   v_flags(a4),d7                  ; and with VOICEF_SYNC flag bit
    beq.b   .leaveProc                      ; voice is not synced
    
    move.l  a4,(sp)                         ; store voice as return value
    
    move.l  a0,d7                           ; test wich kind of replay is
    bne.b   .useJSP                         ; choosen
        
; [Use JSAM as playing source] --------------
.useJSAM:
    move.l  a1,d7                           ;
    beq.b   .leaveProc                      ;
    move.w  d2,d1                           ; note must be in D1
    bsr.w   _spuprivate_SetJSAMSample       ;
    bra.b   .leaveProc                      ;
    

; [Use JSP! as playing source]
; -----------------------------------------------
.useJSP:
    move.w  d1,d0                           ; get voice
    move.w  d2,d1                           ; and get Note
    bsr.w   _spuprivate_SetJSPSample
    
.leaveProc:                                 ; return saved register
    movem.l (sp)+,d0/d6/d7/a4/a5            ; with the return code in D0
    rts                                     ; return




;=---------------------------------------------------------
; LibFunc:      _SPU_KeyOff
; Description:  Set off voice
;
; Input:
;   A0:         *voice
;
; Output:
;   <none>
;
;=---------------------------------------------------------
_SPU_KeyOff:

    move.l  d1,-(sp)
    move.l  d2,-(sp)
    move.l  a0,d1                               ; voice pointer
    moveq   #0,d2                               ; null
    moveq   #spuc_SetVoiceParameter,d0          ; set voice parameter
    bsr.w   _spuprivate_ISPU                    ; 
    move.l  (sp)+,d2
    move.l  (sp)+,d1
    rts





;=---------------------------------------------------------
; LibFunc:      _SPU_KeyOffVoice
; Description:  Set off voice
;
; Input:
;   D0:         voice number
;
; Output:
;   <none>
;
;=---------------------------------------------------------
_SPU_KeyOffVoice:

    tst.w   d0                                  ; test number
    ble.b   .ret                                ; between
    cmp.w   spul_VoiceNumb(a6),d0                  ; 1-number of voices
    bgt.b   .ret

    movem.l d1/d2/a0,-(sp)                      
    
    add.w   d0,d0                               ; calc user
    add.w   d0,d0                               ; voice pointer
    lea     spul_VoiceTable(a6),a0              ; and
    move.l  (a0,d0.w),d1                        ; get voice
    moveq   #0,d2                               ; parameter = 0
    moveq   #spuc_SetVoiceParameter,d0          ; issue command
    bsr.w   _spuprivate_ISPU                    ; do it

    movem.l (sp)+,d1/d2/a0
.ret:
    rts





;=---------------------------------------------------------
; LibFunc:      _SPU_KeyOffVoices
; Description:  Set all voices off
;
; Input:
;   none
;
; Output:
;   none
;=---------------------------------------------------------

_SPU_KeyOffVoices:

    movem.l d1/d2/d7/a5,-(sp)

    lea     spul_VoiceTable(a6),a5              ; get voice table
    moveq   #MAX_VOICES-1,d7                    ; and all possible voices

.setOffL:    
    move.l  (a5)+,d1                            ; get USER voice pointer
    moveq   #0,d2                               ; set to ZERO
    moveq   #spuc_SetVoiceParameter,d0          ; issue cammand
    bsr.w   _spuprivate_ISPU                    ; doit
    dbra    d7,.setOffL                         ; next voice
    
    movem.l (sp)+,d1/d2/d7/a5
    rts
    



;=---------------------------------------------------------
; LibFunc:      _SPU_SetVoiceAttr
; Description:  Sets the specified voice attributs
;
; Input:
;   d0: voice number
;   d1: attributes
;
; Output:
;   a0: *voice
;=---------------------------------------------------------

_SPU_SetVoiceAttr:

    tst.w   d0                          ; are the
    ble.b   .q                          ; voices between
    cmp.w   spul_VoiceNumb(a6),d0       ; 1 << spul_VoiceNumb
    ble.b   .lockV                      ; yes -> lock the voice
.q: moveq   #0,d0                       ;
    rts

.lockV:
    movem.l d1/d2/a0/a1,-(sp)

    lea     spul_VoiceTable(a6),a0      ; get the voice table
    move.w  d0,d2                       ; keep a copy
    mulu.w  #Voice_SIZEOF,d2            ; compute offset
    lea     spul_Voices(a6),a1          ; get user ram voices
    adda.l  d2,a1
    
    move.l  d1,d2
    add.w   d0,d0                       ; get DSP Voice
    add.w   d0,d0                       ;
    move.l  (a0,d0.w),d0                ; get pointer
    moveq   #v_flags,d1                 ; get v_flags voice parameter
    move.l  d0,-(sp)                    ; keep voice pointer
    add.l   d0,d1                       ; d1 = voice+v_flags

    tst.l   d2                          ; test if clear attributes are desired
    bmi.b   .attrclear                  ; Negative -> yes, clear
    or.l    v_flags(a1),d2              ; or with prevoius flags
    bra.b   .setattr                    ; and set them
    
.attrclear:
    bclr    #31,d2                      ; clear clear flag
    not     d2                          ; make mask
    and.l   v_flags(a1),d2              ; clear attrs with prevoius flags
    
.setattr:
    move.l  d2,v_flags(a1)              ; set flags into user ram voice    
    moveq   #spuc_SetVoiceParameter,d0  ; issue command
    bsr.w   _spuprivate_ISPU            ; do it

    move.l  (sp)+,d0                    ; return the Voice
    movem.l (sp)+,d1/d2/a0/a1
    rts
    



    



;=---------------------------------------------------------
; LibFunc:      _SPU_RetrigVoice
; Description:  Retrig Voice
;
; Input:
;   A0:         Voice Pointer
;   D0:         note
;
; Output:
;   D0:        Note
;=---------------------------------------------------------
_SPU_RetrigVoice:
    move.l  a5,-(sp)
    
    movea.l spul_StepTable(a6),a5           ; get step table
    add.w   d0,d0                           ; get frequency for
    add.w   d0,d0                           ; desired note
    move.l  (a5,d0.w),d2                    ; get freq step
    moveq   #v_freqs,d1                     ; where to set
    add.l   a0,d1                           ; make pointer
    moveq   #spuc_SetVoiceParameter,d0      ; sets the voice parameter
    bsr.w   _spuprivate_ISPU
    
    movea.l (sp)+,a5
    rts
    



;=---------------------------------------------------------
; LibFunc:      _SPU_SetVoicePan
; Description:  Apply new pan to voice
;
; Input:
;   A0:         *voice
;   D0:         pan. -127 .. 0 .. 127
;
; Output:
;   <none>
;=---------------------------------------------------------
_SPU_SetVoicePan:
    movem.l d1/d2/a2/a3,-(sp)
    
    bsr.w   _spuprivate_CalculateRLPan

    lea     v_panl(a0),a2
    lea     v_panr(a0),a3
    move.l  d1,-(sp)            ; save right pan
    move.l  a3,-(sp)            ; save right pan voice pointer
        

    move.l  a2,d1               ; left pan voice location
    move.l  d0,d2               ; left pan value
    moveq   #spuc_SetVoiceParameter,d0
    bsr.w   _spuprivate_ISPU

    move.l  (sp)+,d1            ; right pan voice location
    move.l  (sp)+,d2            ; right pan value
    moveq   #spuc_SetVoiceParameter,d0
    bsr.w   _spuprivate_ISPU

    movem.l (sp)+,d1/d2/a2/a3
    rts
    
    

    

;=---------------------------------------------------------
; LibFunc:      _SPU_SetVoiceVolume
; Description:  Apply new volume to voice
;
; Input:
;   A0:         *voice
;   D0:         volume in percent
;
; Output:
;   <none>
;=---------------------------------------------------------
_SPU_SetVoiceVolume:
    bsr.w   _spuprivate_CalcVolume              ; compute volume
    move.l  d0,d2                               ; copy colume as parameter2
    moveq   #v_vol,d1                           ; set the
    add.l   a0,d1                               ; voice volume register in user voice
    moveq   #spuc_SetVoiceParameter,d0          ; SPU COMMAND: SetVoiceParameter
    bra.w   _spuprivate_ISPU                    ; Issue SPU Command
       



;=---------------------------------------------------------
; LibFunc:      _SPU_SetSFXVolume
; Desc:         Set the main volume for the SFX samples
;
;=---------------------------------------------------------

_SPU_SetSFXVolume:
    bsr.b   _spuprivate_CalcVolume              ; compute volume
    move.w  d0,spul_SFXVolume(a6)               ; set new volume in libdata
    move.l  d0,d1                               ; copy volume as parameter 1
    moveq   #spuc_SetSFXVolume,d0               ; SPU COMMNAD
    moveq   #0,d2                               ; zero
    bra.w   _spuprivate_ISPU                    ; Issue command
    



;=---------------------------------------------------------
; LibFunc:      _SPU_SyncVoices
; Desc:         Issue to syncronize voices at next Timer1 interrupt
;
;=---------------------------------------------------------

_SPU_SyncVoices:
    movem.l d1/d2/a0,-(sp)
    moveq   #spuc_SyncVoices,d0
    lea     spul_Voices(a6),a0
    move.l  a0,d1
    moveq   #0,d2
    bsr.w   _spuprivate_ISPU
    movem.l (sp)+,d1/d2/a0
    rts
    
    



; *****************************************************************************
; P R I V A T E - Section
;
; *****************************************************************************

;=---------------------------------------------------------
; LibFunction:  _SPU_CalculateRLPan
; Description:  Cacluate the new pan values for left and right
;               channel
;               
; Input:        d0: pan value
;
;=---------------------------------------------------------

_spuprivate_CalculateRLPan:

    move.l  d2,-(sp)                ; save reg
    move.l  d3,-(sp)                ; save reg
    
    moveq   #0,d3                   ; clear flag
    move.w  d0,d2                   ; keep a copy
    bpl.b   .panr                   ; if negative
    neg.w   d2                      ; then negate
    moveq   #1,d3                   ; set flag

.panr:
    moveq   #127,d0                 ; left
    moveq   #127,d1                 ; right
    sub.w   d2,d0                   ; clac
    lsr.w   #1,d2                   ;
    add.w   d2,d1                   ;
    tst.w   d3                      ; test flag
    beq.b   .noSwap                 ; ==0, no swap
    exg     d0,d1                   ; swap l<=>r

.noSwap:    
    move.l  (sp)+,d3                ; restore
    move.l  (sp)+,d2                ; and out
    rts
    



;=---------------------------------------------------------
; LibFunction:  _SPU_CalculateOSampleDelay
; Description:  Calcualte the new oversample delay value
;               for the DSP core, store the value in spul data
;               section and DSP core local memory
;               
; Input:        d0: oversample value (not index)
;
;=---------------------------------------------------------

_spuprivate_CalculateOSampleDelay:

    movem.l d1/d2/a0,-(sp)                      ; save parameters

    move.w  spul_cddaOSample(a6),d0             ;        
    move.w  spul_MixFrequency(a6),d1            ;
    move.l  #44100,d2                           ;
    add.w   d1,d1                               ;
    lea     MixFrequencies(pc),a0               ;
    move.w  (a0,d1.w),d1                        ; get Frequency as Value
    divu.w  d1,d2                               ; 44100/frequency
    mulu.w  d0,d2                               ; * oversample
    move.w  d2,spul_cddaDelay(a6)               ; store delay flag
                
    movem.l (sp)+,d1/d2/a0
    rts
    



;=---------------------------------------------------------
; Private LibFunc:  CalcValue
; Description:      Calcualte the value in percent
;
; Input:    d0: volume in 1-100
;
; Output:   d0: scaled volume $0000-$4000
;
;=---------------------------------------------------------
_spuprivate_CalcVolume:
    move.l  d1,-(sp)
    move.l  d2,-(sp)

    moveq   #100,d1
    moveq   #0,d2
    tst.w   d0
    ble.b   .pv
    cmpi.w  #200,d0
    ble.b   .ok
    moveq   #100,d0

.ok:
    move.w  spul_MaxVolumeValue(a6),d2      ; get max Volume
    mulu.w  d0,d2
    divu.w  d1,d2
    ext.l   d2
.pv:
    move.l  d2,d0

    move.l  (sp)+,d2
    move.l  (sp)+,d1
    rts



;=---------------------------------------------------------
; Private LibFunc:  GetVolumePercent
; Description:      Calcualte the volume value in percent
;
; Input:    d0: volume value $0000-$4000
;
; Output:   d0: volume percent 0..100
;
;=---------------------------------------------------------

_spuprivate_GetVolumePercent:

    move.l  d1,-(sp)
    move.l  d2,-(sp)

    moveq   #0,d2
    moveq   #0,d1
    move.w  spul_MaxVolumeValue(a6),d1
    divu.w  #100,d1
    tst.w   d1
    beq.b   .divZ
    ext.l   d1    
    move.w  d0,d2
    divu.w  d1,d2
    ext.l   d2

.divZ:
    move.l  d2,d0

    move.l  (sp)+,d2
    move.l  (sp)+,d1
    rts
    



;=---------------------------------------------------------
; Private LibFunc:  Issue SPU
; Description:      Issue the SPU to perform a task
;                   D0 specified the ID to perform the TASK
;
; Input:    d0:     SPU command
;           d1:     parameter 1
;           d2:     parameter 2
;
;=---------------------------------------------------------
_spuprivate_ISPU:
    movem.l d6/d7/a4/a5,-(sp)
    
    lea     DSP_BASE+D_CTRL,a5
    lea     spu_Command,a4
    move.l  (a5),d7
    btst    #0,d7
    beq.b   .noSPU
    
    move.l  #(1<<6)|(1<<7)|(1<<8),d6 ; IRQ mask for CPU/I2S/TIMER

.waitSPU:
    move.l  (a5),d7
    and.l   d6,d7
    bne.b   .waitSPU
    
    add.l   d0,d0                   ; <<1 makes the offset.
    move.l  d0,(a4)                 ; set Command        
    move.l  d1,4(a4)                ; set param1
    move.l  d2,8(a4)                ; set param2

    move.l  (a5),d7                 ; issue the Command to be exectued
    bset    #2,d7                   ;
    move.l  d7,(a5)                 ;

.noSPU:
    movem.l (sp)+,d6/d7/a4/a5
    rts
 


;=---------------------------------------------------------
; Private LibFunc:  Return SPU Parameters
; Description:      
;                   D0 specified the ID to perform the TASK
;
; Output:   d0:     parameter 1
;           d1:     parameter 2
;
;=---------------------------------------------------------
        
_spuprivate_RSPU:
    movem.l d6/d7/a4/a5,-(sp)

    lea     DSP_BASE+D_CTRL,a5
    lea     spu_Command+4,a4

    move.l  #(1<<6)|(1<<7)|(1<<8),d6 ; IRQ mask for CPU/I2S/TIMER

.waitSPU:
    move.l  (a5),d7                 ; get IRQ bits
    and.l   d6,d7                   ; and with MASK
    bne.b   .waitSPU                ; wait as long as one of the above
                                    ; irq's are active.
    move.l  (a4),d0                 ; get return parameters
    move.l  4(a4),d1                ; form the DSP

    movem.l (sp)+,d6/d7/a4/a5
    rts
    


;=---------------------------------------------------------
; Private LibFunc:  Copy Classic SPU Core
; Description:      
;                   copies the classic SPU core. samples+cdda
;=---------------------------------------------------------

_spuprivate_CopyClassicSPUCore:
    move.l  a0,-(sp)
    move.l  a1,-(sp)

    lea     DSP_BASE+D_CTRL,a0
    move.l  (a0),d0
    btst    #0,d0
    bne.b   .isGoing
    
    lea     DSP_RAM,a0                          ; get DSP local ram pointer
    lea     Classic_SPUCore(pc),a1              ; get the Code
    move.w  #((Classic_SPUCoreEnd-Classic_SPUCore)/4)-1,d0    ;

.cpyl:                                          ; copy DSP Code
    move.l  (a1)+,(a0)+                         ; into DSP Ram
    dbra    d0,.cpyl                            ; -.--------.--------------

.isGoing:
    movea.l (sp)+,a1
    movea.l (sp)+,a0
    rts
    



;=---------------------------------------------------------
; Private LibFunc:  Set Jerry Timer 1
; Description:      Sets the Jerry timer interrupt 1 prescaler
;                   and divider according to the current mix
;                   frequency. Compute Samples per frame
;=---------------------------------------------------------

_spuprivate_SetJerryTimer1:
    
    movem.l d0/d1/d2/d3,-(sp)
    
; [Calcuate Timer Prescaler/Divider] ----------------------
    bsr.w   _SPU_GetJaguarType          ; get Hz
    move.l  #PAL_CLOCK>>9,d1
    tst.w   (a6)                        ; test Jag Type
    beq.b   .c0
    move.l  #NTSC_CLOCK>>9,d1           ; no its NTSC 
.c0:
    divu.w  d0,d1                       ; divide
    
    
; [Set Timer] ---------------------------------------------    
    lea     JERRY_BASE,a5
    move.w  #512+1,(a5)                 ; set prescaler
    addq.w  #1,d1
    move.w  d1,JPIT1_DIV(a5)            ; set divider
    
    
; [Calcualte samples per frame] ---------------------------
    bsr.w   _SPU_GetJaguarType
    move.w  spul_SCLK(a6),d1            ; get SCLK
    beq.b   .cddaMode                   ;
    move.l  #PAL_CLOCK>>6,d2            ; PAL_CLOCK/64
    tst.w   (a6)                        ; test jaguar
    beq.b   .c1                         ;
    move.l  #NTSC_CLOCK>>6,d2           ;    
.c1:
    divu.w  d1,d2                       ; freq = SYSTEM_CLOCK/64/(2*(SCLK+1))
    ext.l   d2
    divu.w  d0,d2                       ; freq/Hz
    move.w  d2,spul_SamplesPerFrame(a6) ; store samples per frame
    bra.b   .leave
    
    
; [Is in CDDA mode] ---------------------------------------    
.cddaMode:
    move.w  spul_MixFrequency(a6),d2    ;
    move.l  #11025,d3                   ;
    cmpi.w  #MF_11025,d2                ;
    beq.b   .ccda                       ;
    move.l  #22050,d3                   ;
.ccda:
    divu.w  d0,d3                       ;
    move.w  d3,spul_SamplesPerFrame(a6) ;


; [Set Samples per interrupt] -----------------------------
.leave:
    moveq   #spuc_SetSamplesPerFrame,d0
    moveq   #0,d1
    move.w  spul_SamplesPerFrame(a6),d1
    moveq   #0,d2
    bsr.w   _spuprivate_ISPU
    
    movem.l (sp)+,d0/d1/d2/d3
    rts
     


;=---------------------------------------------------------
; Private LibFunc:  SetJSAM Sample
; Description:      Get sample data out of the JSAM struct
;                   and init the voice
; Input:
;   a1: JSAM
;   a5: Voice to set
;   d1: Note
;
;=---------------------------------------------------------

_spuprivate_SetJSAMSample:

    move.l  (a1),(a5)                           ; sample pointer

    move.l  jsam_size(a1),v_size(a5)            ; sample size
    move.w  jsam_res(a1),v_res+2(a5)            ; because v_res is a longword
    move.w  jsam_vol(a1),v_vol+2(a5)            ; store volume
    
    move.l  jsam_loopBegin(a1),v_loopBegin(a5)  ; loop begin pointer
    move.l  jsam_loopSize(a1),v_loopSize(a5)    ; loop size pointer
    move.w  jsam_loopType(a1),v_loopType+2(a5)  ; loop type flag
    
    movea.l spul_StepTable(a6),a4               ; get the frequnecy
    add.w   d1,d1                               ; step value
    add.w   d1,d1                               ;   
    move.l  (a4,d1.w),d0                        ;
    move.l  d0,v_freqs(a5)                      ; set it
    moveq   #0,d0                               ; clear freq counter
    move.l  d0,v_freqc(a5)                      ; clear it
    
    move.w  jsam_pan(a1),d0                     ; get pan value
    bsr.w   _spuprivate_CalculateRLPan          ; calcuate pan value
    move.w  d0,v_panl+2(a5)
    move.w  d1,v_panr+2(a5)

    rts
    
    

;=---------------------------------------------------------
; Private LibFunc:  SetJSAM Sample
; Description:      Get sample data out of the JSP! struct
;
; Input:
;   a0: JSP!
;   a5: Voice
;   d0: sample number
;   d1: Note
;
;=---------------------------------------------------------

_spuprivate_SetJSPSample:
        
    tst.w   d0
    ble.b   .wrongParams
    cmp.w   jsp_Samples(a0),d0              ; sample within
    bgt.b   .wrongParams                    ; JSP ?? ->leave

    subq.w  #1,d0                           ; -1
    add.w   d0,d0
    add.w   d0,d0
    lea     jsp_SIZEOF(a0),a4
    move.l  (a4,d0.w),d0
    adda.l  d0,a0
    lea     jsps_SIZEOF(a0),a4              ; a2 = sample data
    
    move.l  a4,v_sample(a5)                 ; store sample Address
    move.l  jsps_size(a0),v_size(a5)        ; store sample size
    move.w  jsps_res(a0),v_res+2(a5)        ; because v_res is a longword
    move.w  jsps_vol(a0),v_vol+2(a5)
    move.l  jsps_loopBegin(a0),v_loopBegin(a5)
    move.l  jsps_loopSize(a0),v_loopSize(a5)
    move.w  jsps_loopType(a0),v_loopType+2(a5)
        
    movea.l spul_StepTable(a6),a4
    add.w   d1,d1
    add.w   d1,d1
    move.l  (a4,d1.w),d0
    move.l  d0,v_freqs(a5)
    moveq   #0,d0
    move.l  d0,v_freqc(a5)

    move.w  jsps_pan(a0),d0                 ; get the pan value
    bsr.w   _spuprivate_CalculateRLPan      ; calcuate pan value
    move.w  d0,v_panl+2(a5)
    move.w  d1,v_panr+2(a5)
    moveq   #1,d0
    rts
    
.wrongParams:    
    moveq   #-1,d0
    rts
    

    
; [Jaguar Sound System Library Springboard] ------------------
; ------------------------------------------------------------
_SPUJumpTable:
    .long
    jmp     _SPU_SetSFXVolume(pc)    
    
    jmp     _SPU_SetVoiceVolume(pc)
    jmp     _SPU_SetVoicePan(pc)
    jmp     _SPU_RetrigVoice(pc)

    jmp     _SPU_SyncVoices(pc)
    jmp     _SPU_SetVoiceAttr(pc)
    jmp     _SPU_KeyOffVoices(pc)
    jmp     _SPU_KeyOffVoice(pc)
    jmp     _SPU_KeyOff(pc)
    jmp     _SPU_KeyOnVoiceSync(pc)
    jmp     _SPU_KeyOnVoice(pc)
    jmp     _SPU_KeyOnSync(pc)
    jmp     _SPU_KeyOn(pc)

    jmp     _SPU_GetCDDAOSample(pc)         ; Get current OSample value
    jmp     _SPU_GetCDDAVolume(pc)          ; Get current CDDA volume
    jmp     _SPU_SetCDDAOSample(pc)         ; Set New OSample mode
    jmp     _SPU_SetCDDAVolume(pc)          ; Set the CDDA volume
    jmp     _SPU_DisableCDDA(pc)            ; Switch for CDDA support ON/OFF    
    jmp     _SPU_EnableCDDA(pc)             ; Switch for CDDA support ON/OFF

    jmp     _SPU_GetNoteFreq(pc)            ; get note freqency
    jmp     _SPU_GetJaguarType(pc)          ; return 50 or 60 hz
    jmp     _SPU_GetMixFreq(pc)             ; Get Current Mix Frequency
    jmp     _SPU_GetVoiceNumber(pc)         ; Get current SPU Voice number 
    jmp     _SPU_SetVoiceNumber(pc)         ; Set Voice Number to execute

    jmp     _SPU_InitSample(pc)             ;
    jmp     _SPU_InitRawSample(pc)          ; Big endian
    jmp     _SPU_InitJSP(pc)                ; Init JaguarSamplePack
    
    jmp     _SPU_SetMode(pc)                ; Set SPU Mode
    jmp     _SPU_DisableDSP(pc)             ; Disable DSP execution
    jmp     _SPU_EnableDSP(pc)              ; Enable DSP execution
    jmp     _SPU_Init(pc)                   ; Init SPU
SPULibName:
    dc.b    'spu.library',0
    .dcb.b  32-(*-SPULibName),0

; [Library Data Section] -------------------------------------
; ------------------------------------------------------------
SPULibrary:
    dcb.b   spul_SIZEOF,0

    
        
        .long 
; [Library Varaibles Section] --------------------------------
; ------------------------------------------------------------
MixFrequencies:
        .dc.w    8000,11025,16000,22050,32000,44100
       
        .long
FrequencyStepTables:
 
; PAL Frequecy: 8147
		.dc.l	$00000083,$0000008b,$00000093,$0000009c,$000000a5,$000000af,$000000b9,$000000c4
		.dc.l	$000000d0,$000000dc,$000000ea,$000000f8,$00000106,$00000116,$00000126,$00000138
		.dc.l	$0000014b,$0000015e,$00000173,$00000189,$000001a1,$000001b9,$000001d4,$000001f0
		.dc.l	$0000020d,$0000022c,$0000024d,$00000271,$00000296,$000002bd,$000002e7,$00000313
		.dc.l	$00000342,$00000373,$000003a8,$000003e0,$0000041b,$00000459,$0000049b,$000004e2
		.dc.l	$0000052c,$0000057b,$000005ce,$00000626,$00000684,$000006e7,$00000750,$000007c0
		.dc.l	$00000836,$000008b3,$00000937,$000009c4,$00000a58,$00000af6,$00000b9d,$00000c4d
		.dc.l	$00000d09,$00000dcf,$00000ea1,$00000f80,$0000106c,$00001166,$0000126f,$00001388
		.dc.l	$000014b1,$000015ec,$0000173a,$0000189b,$00001a12,$00001b9f,$00001d43,$00001f01
		.dc.l	$000020d9,$000022cd,$000024df,$00002710,$00002962,$00002bd8,$00002e74,$00003137
		.dc.l	$00003424,$0000373e,$00003a87,$00003e02,$000041b2,$0000459a,$000049be,$00004e20
		.dc.l	$000052c5,$000057b1,$00005ce8,$0000626f,$00006849,$00006e7d,$0000750e,$00007c04
		.dc.l	$00008364,$00008b34,$0000937c,$00009c41,$0000a58b,$0000af63,$0000b9d1,$0000c4de
		.dc.l	$0000d093,$0000dcfa,$0000ea1d,$0000f809,$000106c9,$00011669,$000126f8,$00013882
		.dc.l	$00014b17,$00015ec7,$000173a3,$000189bc,$0001a126,$0001b9f4,$0001d43b,$0001f013
		.dc.l	$00020d93,$00022cd3,$00024df0,$00027104,$0002962e,$0002bd8e,$0002e746,$00031378

; PAL Frequecy: 11230
		.dc.l	$0000005f,$00000064,$0000006a,$00000071,$00000078,$0000007f,$00000086,$0000008e
		.dc.l	$00000097,$000000a0,$000000a9,$000000b3,$000000be,$000000c9,$000000d5,$000000e2
		.dc.l	$000000f0,$000000fe,$0000010d,$0000011d,$0000012e,$00000140,$00000153,$00000167
		.dc.l	$0000017d,$00000193,$000001ab,$000001c5,$000001e0,$000001fc,$0000021b,$0000023b
		.dc.l	$0000025d,$00000281,$000002a7,$000002cf,$000002fa,$00000327,$00000357,$0000038a
		.dc.l	$000003c0,$000003f9,$00000436,$00000476,$000004ba,$00000502,$0000054e,$0000059f
		.dc.l	$000005f5,$0000064f,$000006af,$00000715,$00000781,$000007f3,$0000086c,$000008ed
		.dc.l	$00000975,$00000a04,$00000a9d,$00000b3f,$00000bea,$00000c9f,$00000d5f,$00000e2b
		.dc.l	$00000f03,$00000fe7,$000010d9,$000011da,$000012ea,$00001409,$0000153b,$0000167e
		.dc.l	$000017d4,$0000193f,$00001abf,$00001c56,$00001e06,$00001fcf,$000021b3,$000023b4
		.dc.l	$000025d4,$00002813,$00002a76,$00002cfc,$00002fa9,$0000327e,$0000357f,$000038ad
		.dc.l	$00003c0c,$00003f9e,$00004367,$00004769,$00004ba8,$00005027,$000054ec,$000059f8
		.dc.l	$00005f52,$000064fd,$00006afe,$0000715b,$00007819,$00007f3d,$000086ce,$00008ed2
		.dc.l	$00009750,$0000a04f,$0000a9d8,$0000b3f1,$0000bea4,$0000c9fa,$0000d5fd,$0000e2b6
		.dc.l	$0000f032,$0000fe7a,$00010d9c,$00011da4,$00012ea0,$0001409f,$000153b0,$000167e3
		.dc.l	$00017d49,$000193f5,$0001abfb,$0001c56d,$0001e064,$0001fcf5,$00021b38,$00023b49

; PAL Frequecy: 16621
		.dc.l	$00000040,$00000044,$00000048,$0000004c,$00000051,$00000055,$0000005b,$00000060
		.dc.l	$00000066,$0000006c,$00000072,$00000079,$00000080,$00000088,$00000090,$00000099
		.dc.l	$000000a2,$000000ab,$000000b6,$000000c0,$000000cc,$000000d8,$000000e5,$000000f3
		.dc.l	$00000101,$00000110,$00000121,$00000132,$00000144,$00000157,$0000016c,$00000181
		.dc.l	$00000198,$000001b1,$000001cb,$000001e6,$00000203,$00000221,$00000242,$00000264
		.dc.l	$00000289,$000002af,$000002d8,$00000303,$00000331,$00000362,$00000396,$000003cc
		.dc.l	$00000406,$00000443,$00000484,$000004c9,$00000512,$0000055f,$000005b1,$00000607
		.dc.l	$00000663,$000006c5,$0000072c,$00000799,$0000080c,$00000887,$00000909,$00000992
		.dc.l	$00000a24,$00000abf,$00000b62,$00000c0f,$00000cc7,$00000d8a,$00000e58,$00000f32
		.dc.l	$00001019,$0000110e,$00001212,$00001325,$00001449,$0000157e,$000016c5,$0000181f
		.dc.l	$0000198f,$00001b14,$00001cb0,$00001e65,$00002033,$0000221d,$00002425,$0000264b
		.dc.l	$00002892,$00002afc,$00002d8a,$0000303f,$0000331e,$00003628,$00003960,$00003cca
		.dc.l	$00004067,$0000443b,$0000484a,$00004c97,$00005124,$000055f8,$00005b14,$0000607f
		.dc.l	$0000663c,$00006c50,$000072c1,$00007994,$000080cf,$00008877,$00009095,$0000992e
		.dc.l	$0000a249,$0000abf0,$0000b629,$0000c0fe,$0000cc78,$0000d8a1,$0000e582,$0000f328
		.dc.l	$0001019e,$000110ef,$0001212a,$0001325c,$00014493,$000157e0,$00016c53,$000181fd

; PAL Frequecy: 23084
		.dc.l	$0000002e,$00000031,$00000034,$00000037,$0000003a,$0000003d,$00000041,$00000045
		.dc.l	$00000049,$0000004d,$00000052,$00000057,$0000005c,$00000062,$00000068,$0000006e
		.dc.l	$00000074,$0000007b,$00000083,$0000008a,$00000093,$0000009b,$000000a5,$000000af
		.dc.l	$000000b9,$000000c4,$000000d0,$000000dc,$000000e9,$000000f7,$00000106,$00000115
		.dc.l	$00000126,$00000137,$0000014a,$0000015e,$00000172,$00000189,$000001a0,$000001b9
		.dc.l	$000001d3,$000001ef,$0000020c,$0000022b,$0000024c,$0000026f,$00000295,$000002bc
		.dc.l	$000002e5,$00000312,$00000340,$00000372,$000003a6,$000003de,$00000419,$00000457
		.dc.l	$00000499,$000004df,$0000052a,$00000578,$000005cb,$00000624,$00000681,$000006e4
		.dc.l	$0000074d,$000007bc,$00000832,$000008af,$00000933,$000009bf,$00000a54,$00000af1
		.dc.l	$00000b97,$00000c48,$00000d03,$00000dc9,$00000e9b,$00000f79,$00001065,$0000115e
		.dc.l	$00001267,$0000137f,$000014a8,$000015e2,$0000172f,$00001890,$00001a06,$00001b92
		.dc.l	$00001d36,$00001ef3,$000020ca,$000022bd,$000024ce,$000026fe,$00002950,$00002bc5
		.dc.l	$00002e5f,$00003121,$0000340d,$00003725,$00003a6c,$00003de6,$00004194,$0000457a
		.dc.l	$0000499c,$00004dfd,$000052a0,$0000578a,$00005cbe,$00006242,$0000681a,$00006e4b
		.dc.l	$000074d9,$00007bcc,$00008329,$00008af5,$00009339,$00009bfa,$0000a540,$0000af14
		.dc.l	$0000b97d,$0000c485,$0000d034,$0000dc96,$0000e9b3,$0000f799,$00010652,$000115eb


; PAL Frequecy: 34627
		.dc.l	$0000001e,$00000020,$00000022,$00000024,$00000026,$00000029,$0000002b,$0000002e
		.dc.l	$00000031,$00000033,$00000037,$0000003a,$0000003d,$00000041,$00000045,$00000049
		.dc.l	$0000004d,$00000052,$00000057,$0000005c,$00000062,$00000067,$0000006e,$00000074
		.dc.l	$0000007b,$00000083,$0000008a,$00000093,$0000009b,$000000a5,$000000ae,$000000b9
		.dc.l	$000000c4,$000000cf,$000000dc,$000000e9,$000000f7,$00000106,$00000115,$00000126
		.dc.l	$00000137,$0000014a,$0000015d,$00000172,$00000188,$0000019f,$000001b8,$000001d2
		.dc.l	$000001ee,$0000020c,$0000022b,$0000024c,$0000026f,$00000294,$000002bb,$000002e5
		.dc.l	$00000311,$0000033f,$00000371,$000003a5,$000003dd,$00000418,$00000456,$00000498
		.dc.l	$000004de,$00000528,$00000577,$000005ca,$00000622,$0000067f,$000006e2,$0000074b
		.dc.l	$000007ba,$00000830,$000008ac,$00000930,$000009bc,$00000a50,$00000aee,$00000b94
		.dc.l	$00000c44,$00000cff,$00000dc5,$00000e96,$00000f75,$00001060,$00001159,$00001261
		.dc.l	$00001379,$000014a1,$000015dc,$00001728,$00001889,$000019fe,$00001b8a,$00001d2d
		.dc.l	$00001eea,$000020c0,$000022b3,$000024c3,$000026f3,$00002943,$00002bb8,$00002e51
		.dc.l	$00003112,$000033fd,$00003715,$00003a5b,$00003dd4,$00004181,$00004566,$00004986
		.dc.l	$00004de6,$00005287,$00005770,$00005ca3,$00006225,$000067fb,$00006e2a,$000074b7
		.dc.l	$00007ba8,$00008302,$00008acc,$0000930d,$00009bcc,$0000a50f,$0000aee0,$0000b946

; PAL Frequecy: 46169
		.dc.l	$00000017,$00000018,$0000001a,$0000001b,$0000001d,$0000001e,$00000020,$00000022
		.dc.l	$00000024,$00000026,$00000029,$0000002b,$0000002e,$00000031,$00000034,$00000037
		.dc.l	$0000003a,$0000003d,$00000041,$00000045,$00000049,$0000004d,$00000052,$00000057
		.dc.l	$0000005c,$00000062,$00000068,$0000006e,$00000074,$0000007b,$00000083,$0000008a
		.dc.l	$00000093,$0000009b,$000000a5,$000000af,$000000b9,$000000c4,$000000d0,$000000dc
		.dc.l	$000000e9,$000000f7,$00000106,$00000115,$00000126,$00000137,$0000014a,$0000015e
		.dc.l	$00000172,$00000189,$000001a0,$000001b9,$000001d3,$000001ef,$0000020c,$0000022b
		.dc.l	$0000024c,$0000026f,$00000294,$000002bc,$000002e5,$00000312,$00000340,$00000372
		.dc.l	$000003a6,$000003de,$00000419,$00000457,$00000499,$000004df,$00000529,$00000578
		.dc.l	$000005cb,$00000624,$00000681,$000006e4,$0000074d,$000007bc,$00000832,$000008af
		.dc.l	$00000933,$000009bf,$00000a53,$00000af1,$00000b97,$00000c48,$00000d03,$00000dc9
		.dc.l	$00000e9b,$00000f79,$00001065,$0000115e,$00001267,$0000137f,$000014a7,$000015e2
		.dc.l	$0000172f,$00001890,$00001a06,$00001b92,$00001d36,$00001ef3,$000020ca,$000022bd
		.dc.l	$000024ce,$000026fe,$0000294f,$00002bc4,$00002e5f,$00003121,$0000340c,$00003725
		.dc.l	$00003a6c,$00003de6,$00004194,$0000457a,$0000499c,$00004dfc,$0000529f,$00005789
		.dc.l	$00005cbe,$00006242,$00006819,$00006e4a,$000074d9,$00007bcc,$00008328,$00008af5

    
; NTSC Frequecy: 8146
		.dc.l	$00000083,$0000008b,$00000093,$0000009c,$000000a5,$000000af,$000000b9,$000000c4
		.dc.l	$000000d0,$000000dd,$000000ea,$000000f8,$00000106,$00000116,$00000127,$00000138
		.dc.l	$0000014b,$0000015e,$00000173,$00000189,$000001a1,$000001ba,$000001d4,$000001f0
		.dc.l	$0000020d,$0000022c,$0000024e,$00000271,$00000296,$000002bd,$000002e7,$00000313
		.dc.l	$00000342,$00000374,$000003a8,$000003e0,$0000041b,$00000459,$0000049c,$000004e2
		.dc.l	$0000052c,$0000057b,$000005ce,$00000627,$00000684,$000006e8,$00000751,$000007c0
		.dc.l	$00000836,$000008b3,$00000938,$000009c4,$00000a59,$00000af6,$00000b9d,$00000c4e
		.dc.l	$00000d09,$00000dd0,$00000ea2,$00000f81,$0000106d,$00001167,$00001270,$00001388
		.dc.l	$000014b2,$000015ed,$0000173a,$0000189c,$00001a13,$00001ba0,$00001d44,$00001f02
		.dc.l	$000020da,$000022ce,$000024e0,$00002711,$00002964,$00002bda,$00002e75,$00003139
		.dc.l	$00003426,$00003740,$00003a89,$00003e04,$000041b4,$0000459c,$000049c0,$00004e22
		.dc.l	$000052c8,$000057b4,$00005ceb,$00006272,$0000684c,$00006e80,$00007512,$00007c08
		.dc.l	$00008368,$00008b39,$00009380,$00009c45,$0000a590,$0000af69,$0000b9d7,$0000c4e4
		.dc.l	$0000d099,$0000dd01,$0000ea25,$0000f811,$000106d1,$00011672,$00012701,$0001388b
		.dc.l	$00014b21,$00015ed2,$000173ae,$000189c8,$0001a133,$0001ba02,$0001d44a,$0001f023
		.dc.l	$00020da3,$00022ce5,$00024e02,$00027117,$00029643,$0002bda4,$0002e75d,$00031391

; NTSC Frequecy: 11229
		.dc.l	$0000005f,$00000064,$0000006b,$00000071,$00000078,$0000007f,$00000086,$0000008e
		.dc.l	$00000097,$000000a0,$000000a9,$000000b3,$000000be,$000000c9,$000000d6,$000000e2
		.dc.l	$000000f0,$000000fe,$0000010d,$0000011d,$0000012e,$00000140,$00000153,$00000167
		.dc.l	$0000017d,$00000193,$000001ac,$000001c5,$000001e0,$000001fd,$0000021b,$0000023b
		.dc.l	$0000025d,$00000281,$000002a7,$000002cf,$000002fa,$00000327,$00000358,$0000038a
		.dc.l	$000003c0,$000003fa,$00000436,$00000476,$000004ba,$00000502,$0000054e,$0000059f
		.dc.l	$000005f5,$0000064f,$000006b0,$00000715,$00000781,$000007f4,$0000086d,$000008ed
		.dc.l	$00000975,$00000a05,$00000a9d,$00000b3f,$00000bea,$00000c9f,$00000d60,$00000e2b
		.dc.l	$00000f03,$00000fe8,$000010da,$000011da,$000012ea,$0000140a,$0000153b,$0000167e
		.dc.l	$000017d5,$0000193f,$00001ac0,$00001c57,$00001e06,$00001fd0,$000021b4,$000023b5
		.dc.l	$000025d4,$00002814,$00002a76,$00002cfd,$00002faa,$0000327f,$00003580,$000038af
		.dc.l	$00003c0d,$00003fa0,$00004368,$0000476a,$00004ba9,$00005029,$000054ed,$000059fa
		.dc.l	$00005f54,$000064ff,$00006b01,$0000715e,$0000781b,$00007f40,$000086d1,$00008ed5
		.dc.l	$00009753,$0000a053,$0000a9db,$0000b3f5,$0000bea9,$0000c9ff,$0000d602,$0000e2bc
		.dc.l	$0000f037,$0000fe80,$00010da2,$00011dab,$00012ea7,$000140a6,$000153b7,$000167eb
		.dc.l	$00017d52,$000193fe,$0001ac04,$0001c578,$0001e06f,$0001fd00,$00021b45,$00023b56

; NTSC Frequecy: 16619
		.dc.l	$00000040,$00000044,$00000048,$0000004c,$00000051,$00000055,$0000005b,$00000060
		.dc.l	$00000066,$0000006c,$00000072,$00000079,$00000080,$00000088,$00000090,$00000099
		.dc.l	$000000a2,$000000ab,$000000b6,$000000c1,$000000cc,$000000d8,$000000e5,$000000f3
		.dc.l	$00000101,$00000110,$00000121,$00000132,$00000144,$00000157,$0000016c,$00000182
		.dc.l	$00000198,$000001b1,$000001cb,$000001e6,$00000203,$00000221,$00000242,$00000264
		.dc.l	$00000289,$000002af,$000002d8,$00000304,$00000331,$00000362,$00000396,$000003cc
		.dc.l	$00000406,$00000443,$00000484,$000004c9,$00000512,$0000055f,$000005b1,$00000608
		.dc.l	$00000663,$000006c5,$0000072c,$00000799,$0000080d,$00000887,$00000909,$00000993
		.dc.l	$00000a24,$00000abf,$00000b62,$00000c10,$00000cc7,$00000d8a,$00000e58,$00000f33
		.dc.l	$0000101a,$0000110f,$00001213,$00001326,$00001449,$0000157e,$000016c5,$00001820
		.dc.l	$0000198f,$00001b14,$00001cb1,$00001e66,$00002034,$0000221f,$00002426,$0000264c
		.dc.l	$00002893,$00002afd,$00002d8b,$00003041,$0000331f,$00003629,$00003962,$00003ccc
		.dc.l	$00004069,$0000443e,$0000484c,$00004c99,$00005127,$000055fa,$00005b17,$00006082
		.dc.l	$0000663f,$00006c53,$000072c4,$00007998,$000080d2,$0000887c,$00009099,$00009932
		.dc.l	$0000a24e,$0000abf5,$0000b62f,$0000c104,$0000cc7e,$0000d8a7,$0000e589,$0000f330
		.dc.l	$000101a5,$000110f8,$00012133,$00013265,$0001449d,$000157eb,$00016c5e,$00018209

; NTSC Frequecy: 23082
		.dc.l	$0000002e,$00000031,$00000034,$00000037,$0000003a,$0000003d,$00000041,$00000045
		.dc.l	$00000049,$0000004d,$00000052,$00000057,$0000005c,$00000062,$00000068,$0000006e
		.dc.l	$00000074,$0000007b,$00000083,$0000008a,$00000093,$0000009b,$000000a5,$000000af
		.dc.l	$000000b9,$000000c4,$000000d0,$000000dc,$000000e9,$000000f7,$00000106,$00000115
		.dc.l	$00000126,$00000137,$0000014a,$0000015e,$00000173,$00000189,$000001a0,$000001b9
		.dc.l	$000001d3,$000001ef,$0000020c,$0000022b,$0000024c,$0000026f,$00000295,$000002bc
		.dc.l	$000002e6,$00000312,$00000340,$00000372,$000003a6,$000003de,$00000419,$00000457
		.dc.l	$00000499,$000004df,$0000052a,$00000578,$000005cc,$00000624,$00000681,$000006e4
		.dc.l	$0000074d,$000007bc,$00000832,$000008af,$00000933,$000009bf,$00000a54,$00000af1
		.dc.l	$00000b98,$00000c48,$00000d03,$00000dc9,$00000e9b,$00000f79,$00001065,$0000115f
		.dc.l	$00001267,$0000137f,$000014a8,$000015e3,$00001730,$00001891,$00001a07,$00001b93
		.dc.l	$00001d37,$00001ef3,$000020cb,$000022be,$000024cf,$000026ff,$00002951,$00002bc6
		.dc.l	$00002e60,$00003122,$0000340e,$00003726,$00003a6e,$00003de7,$00004196,$0000457c
		.dc.l	$0000499e,$00004dfe,$000052a2,$0000578c,$00005cc0,$00006244,$0000681c,$00006e4d
		.dc.l	$000074dc,$00007bcf,$0000832c,$00008af9,$0000933c,$00009bfd,$0000a544,$0000af18
		.dc.l	$0000b981,$0000c489,$0000d039,$0000dc9a,$0000e9b9,$0000f79f,$00010658,$000115f2


; NTSC Frequecy: 34623
		.dc.l	$0000001e,$00000020,$00000022,$00000024,$00000026,$00000029,$0000002b,$0000002e
		.dc.l	$00000031,$00000033,$00000037,$0000003a,$0000003d,$00000041,$00000045,$00000049
		.dc.l	$0000004d,$00000052,$00000057,$0000005c,$00000062,$00000067,$0000006e,$00000074
		.dc.l	$0000007b,$00000083,$0000008a,$00000093,$0000009b,$000000a5,$000000ae,$000000b9
		.dc.l	$000000c4,$000000cf,$000000dc,$000000e9,$000000f7,$00000106,$00000115,$00000126
		.dc.l	$00000137,$0000014a,$0000015d,$00000172,$00000188,$0000019f,$000001b8,$000001d2
		.dc.l	$000001ee,$0000020c,$0000022b,$0000024c,$0000026f,$00000294,$000002bb,$000002e5
		.dc.l	$00000311,$0000033f,$00000371,$000003a5,$000003dd,$00000418,$00000456,$00000498
		.dc.l	$000004de,$00000528,$00000577,$000005ca,$00000622,$0000067f,$000006e2,$0000074b
		.dc.l	$000007ba,$00000830,$000008ad,$00000931,$000009bd,$00000a51,$00000aee,$00000b94
		.dc.l	$00000c45,$00000cff,$00000dc5,$00000e97,$00000f75,$00001060,$0000115a,$00001262
		.dc.l	$0000137a,$000014a2,$000015dc,$00001729,$0000188a,$000019ff,$00001b8b,$00001d2e
		.dc.l	$00001eea,$000020c1,$000022b4,$000024c4,$000026f4,$00002945,$00002bb9,$00002e53
		.dc.l	$00003114,$000033ff,$00003716,$00003a5d,$00003dd5,$00004183,$00004568,$00004988
		.dc.l	$00004de8,$0000528a,$00005772,$00005ca6,$00006228,$000067fe,$00006e2d,$000074ba
		.dc.l	$00007bab,$00008306,$00008ad0,$00009311,$00009bd0,$0000a514,$0000aee5,$0000b94c

; NTSC Frequecy: 46164
		.dc.l	$00000017,$00000018,$0000001a,$0000001b,$0000001d,$0000001e,$00000020,$00000022
		.dc.l	$00000024,$00000026,$00000029,$0000002b,$0000002e,$00000031,$00000034,$00000037
		.dc.l	$0000003a,$0000003d,$00000041,$00000045,$00000049,$0000004d,$00000052,$00000057
		.dc.l	$0000005c,$00000062,$00000068,$0000006e,$00000074,$0000007b,$00000083,$0000008a
		.dc.l	$00000093,$0000009b,$000000a5,$000000af,$000000b9,$000000c4,$000000d0,$000000dc
		.dc.l	$000000e9,$000000f7,$00000106,$00000115,$00000126,$00000137,$0000014a,$0000015e
		.dc.l	$00000173,$00000189,$000001a0,$000001b9,$000001d3,$000001ef,$0000020c,$0000022b
		.dc.l	$0000024c,$0000026f,$00000295,$000002bc,$000002e6,$00000312,$00000340,$00000372
		.dc.l	$000003a6,$000003de,$00000419,$00000457,$00000499,$000004df,$0000052a,$00000578
		.dc.l	$000005cc,$00000624,$00000681,$000006e4,$0000074d,$000007bc,$00000832,$000008af
		.dc.l	$00000933,$000009bf,$00000a54,$00000af1,$00000b98,$00000c48,$00000d03,$00000dc9
		.dc.l	$00000e9b,$00000f79,$00001065,$0000115f,$00001267,$0000137f,$000014a8,$000015e3
		.dc.l	$00001730,$00001891,$00001a07,$00001b93,$00001d37,$00001ef3,$000020cb,$000022be
		.dc.l	$000024cf,$000026ff,$00002951,$00002bc6,$00002e60,$00003122,$0000340e,$00003726
		.dc.l	$00003a6e,$00003de7,$00004196,$0000457c,$0000499e,$00004dfe,$000052a2,$0000578c
		.dc.l	$00005cc0,$00006244,$0000681c,$00006e4d,$000074dc,$00007bcf,$0000832c,$00008af9

; CDDA Frequecy: 11025
		.dc.l	$00000061,$00000066,$0000006c,$00000073,$0000007a,$00000081,$00000089,$00000091
		.dc.l	$0000009a,$000000a3,$000000ad,$000000b7,$000000c2,$000000cd,$000000d9,$000000e6
		.dc.l	$000000f4,$00000103,$00000112,$00000122,$00000134,$00000146,$0000015a,$0000016e
		.dc.l	$00000184,$0000019b,$000001b3,$000001cd,$000001e9,$00000206,$00000225,$00000245
		.dc.l	$00000268,$0000028d,$000002b4,$000002dd,$00000308,$00000336,$00000367,$0000039b
		.dc.l	$000003d2,$0000040c,$0000044a,$0000048b,$000004d1,$0000051a,$00000568,$000005ba
		.dc.l	$00000611,$0000066d,$000006cf,$00000737,$000007a5,$00000819,$00000894,$00000917
		.dc.l	$000009a2,$00000a34,$00000ad0,$00000b74,$00000c23,$00000cdb,$00000d9f,$00000e6e
		.dc.l	$00000f4a,$00001033,$00001129,$0000122f,$00001344,$00001469,$000015a0,$000016e9
		.dc.l	$00001846,$000019b7,$00001b3f,$00001cdd,$00001e95,$00002066,$00002253,$0000245e
		.dc.l	$00002688,$000028d2,$00002b40,$00002dd2,$0000308c,$0000336f,$0000367e,$000039bb
		.dc.l	$00003d2a,$000040cd,$000044a7,$000048bd,$00004d10,$000051a5,$00005680,$00005ba5
		.dc.l	$00006118,$000066de,$00006cfc,$00007377,$00007a54,$0000819a,$0000894f,$0000917a
		.dc.l	$00009a20,$0000a34a,$0000ad00,$0000b74a,$0000c230,$0000cdbc,$0000d9f8,$0000e6ee
		.dc.l	$0000f4a9,$00010335,$0001129f,$000122f4,$00013441,$00014695,$00015a01,$00016e94
		.dc.l	$00018460,$00019b78,$0001b3f0,$0001cddc,$0001e953,$0002066b,$0002253f,$000245e8

; CDDA Frequecy: 22050
		.dc.l	$00000030,$00000033,$00000036,$00000039,$0000003d,$00000040,$00000044,$00000048
		.dc.l	$0000004d,$00000051,$00000056,$0000005b,$00000061,$00000066,$0000006c,$00000073
		.dc.l	$0000007a,$00000081,$00000089,$00000091,$0000009a,$000000a3,$000000ad,$000000b7
		.dc.l	$000000c2,$000000cd,$000000d9,$000000e6,$000000f4,$00000103,$00000112,$00000122
		.dc.l	$00000134,$00000146,$0000015a,$0000016e,$00000184,$0000019b,$000001b3,$000001cd
		.dc.l	$000001e9,$00000206,$00000225,$00000245,$00000268,$0000028d,$000002b4,$000002dd
		.dc.l	$00000308,$00000336,$00000367,$0000039b,$000003d2,$0000040c,$0000044a,$0000048b
		.dc.l	$000004d1,$0000051a,$00000568,$000005ba,$00000611,$0000066d,$000006cf,$00000737
		.dc.l	$000007a5,$00000819,$00000894,$00000917,$000009a2,$00000a34,$00000ad0,$00000b74
		.dc.l	$00000c23,$00000cdb,$00000d9f,$00000e6e,$00000f4a,$00001033,$00001129,$0000122f
		.dc.l	$00001344,$00001469,$000015a0,$000016e9,$00001846,$000019b7,$00001b3f,$00001cdd
		.dc.l	$00001e95,$00002066,$00002253,$0000245e,$00002688,$000028d2,$00002b40,$00002dd2
		.dc.l	$0000308c,$0000336f,$0000367e,$000039bb,$00003d2a,$000040cd,$000044a7,$000048bd
		.dc.l	$00004d10,$000051a5,$00005680,$00005ba5,$00006118,$000066de,$00006cfc,$00007377
		.dc.l	$00007a54,$0000819a,$0000894f,$0000917a,$00009a20,$0000a34a,$0000ad00,$0000b74a
		.dc.l	$0000c230,$0000cdbc,$0000d9f8,$0000e6ee,$0000f4a9,$00010335,$0001129f,$000122f4


        .long
Classic_SPUCore:
        .include    "spulib.das"
        .68000
        .padvalue   0
        .long
Classic_SPUCoreEnd:
        .long            
    
        .print  "M68000 API Code Size = ",/d/l _SPUJumpTable-_SPU_Init
        .end
                 