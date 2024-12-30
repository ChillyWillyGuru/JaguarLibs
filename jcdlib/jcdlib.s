; ============================================================
; $File:    jcdlib.s
; $Begin:   25.10.02
; $Autor:   Robert Jurziga
; $Update:  02.03.03
; $Desc:    Jaguar CDROM library source.
;           Includes CDDA support only. This library is used
;           within the SPU (Jaguar Sound System).
;
;
; ============================================================

    .include    "typedefs.inc"
    .include    "libdefs.inc"
    
    .include    "jcdlib_p.inc"
    .include    "jcdlib.inc"
    .include    "spulib.inc"
    
    .iif    (^^defined libmode), .extern     JCDLibrary

    .68000
    .text
                
; ------------------------------------------------------------
; Function:         InitJCD
; Descripton:       Initialise CDROM for use as CDDA
;
;
; ------------------------------------------------------------
    .include    "jcdver.inc"    
    .long
    
_JCD_Init:    
    movem.l d2-d7/a2-a6,-(sp)
    lea     BUTCH,a5
    
    bsr.w   _jcdprivate_SaveRegisters
    
; [ROM1 read enable] --------------------------------------
;    lea     $F00000,a0
;    move.w  #$1865,(a0)
; ---------------------------------------------------------
   
    
; [Set Default CDROM Varaibles] ---------------------------
    move.l  a5,jcdl_Butch(a6)           ; set BUTCH

    moveq   #0,d0                       ; set default
    move.w  d0,jcdl_Session(a6)         ; clear session
    move.w  d0,jcdl_ErrorCode(a6)       ; clear error code
    move.w  d0,jcdl_playMode(a6)        ; clear playing mode
    move.w  d0,jcdl_curTrack(a6)        ; clear current track
    move.w  d0,jcdl_pause(a6)           ; pause off

    move.w  d0,jcdl_ACK(a6)             ; blocking mode is default
    moveq   #-1,d0
    move.w  d0,jcdl_mute(a6)            ; mute on
    move.w  d0,jcdl_stop(a6)            ; stopped

    moveq   #OSAMPLE_DEFAULT,d0
    move.w  d0,jcdl_OSample(a6)         ; set the oversample to none
; ---------------------------------------------------------        
    


; [Init Reference] ----------------------------------------
    moveq   #0,d0
    move.l  d0,jcdl_SPUReference(a6)
; ---------------------------------------------------------

    
    
; [Clear Pendig IRQs] -------------------------------------
    move.w  _DS_DATA(a5),d0             ; touch _DS_DATA
    move.l  _DSCNTRL(a5),d0             ; and _DSCNTRL
; ---------------------------------------------------------    
    
    
    
    
; [Setup CDROM ] -----------------------------------------
;    move.l  #$0,BUTCH
;    moveq   #0,d0                   ; enable Butch
;    move.l  d0,(a5)

    move.w  #$1000,d0
.ww: dbf     d0,.ww

    move.l  #$100000,_DSCNTRL(a5)
    move.w  #$1000,d0
.ww2: dbf     d0,.ww2

    
    moveq   #$18,d0
    swap    d0
    moveq   #1,d1
    swap    d1
    move.l  d0,(a5)
    move.l  d1,_DSCNTRL(a5)
    
    moveq   #7,d0
    moveq   #1,d1
    move.l  d0,_I2CNTRL(a5)
    move.l  d1,_I2CNTRL(a5)
    
    move.w  jcdl_OSample(a6),d0     ; set oversampling
    ori.w   #$7000,d0               ; to none (standard)
    move.w  d0,_DS_DATA(a5)         ; set it
    bsr.b   ACK_rx                  ; wait for ack
    
    move.l  _I2CNTRL(a5),d0         ; Jerry gots data via
    moveq   #6,d1                   ; I2S
    or.l    d1,d0                   ;
    move.l  d0,_I2CNTRL(a5)         ;
    
    move.w  #$5400,_DS_DATA(a5)     ; command to send # of sessions
    bsr.b   ACK_rx
    move.w  d0,jcdl_Sessions(a6)    ; store sessions number
    
    move.w  #$1501,_DS_DATA(a5)
    bsr.b   ACK_rx
    
    
    ; [Set Single Speed] -------------------------------------
        
.leave:
    movem.l (sp)+,d2-d7/a2-a6
    rts
    




;=---------------------------------------------------------
; LibFunc:  _JCD_InheritSPU
;           Init the reference to SPU Library
;
; Input:    a0: SPULibrary Reference
;
;=---------------------------------------------------------

_JCD_InheritSPU:

    move.l  a5,-(sp)                ; preclear SPU reference
    moveq   #0,d0                       
    move.l  d0,jcdl_SPUReference(a6)    
    
    move.l  a0,d0                   ; check reference
    beq.b   .noSPU                  ; NULL-> no reference
    
    lea     -32(a0),a0              ; get the library name pointer
    lea     .spulibname(pc),a5      ; get the name to compare with

.checkName:
    tst.b   (a0)
    beq.b   .cOther                 ;
    cmpm.b  (a0)+,(a5)+
    beq.b   .checkName
    bra.b   .noSPU

.cOther:
    tst.b   (a5)
    bne.b   .noSPU
    
    move.l  d0,jcdl_SPUReference(a6)
    movea.l (sp)+,a5
    rts
    
.noSPU:
    movea.l (sp)+,a5
    moveq   #0,d0
    rts


.spulibname:
    .dc.b   "spu.library",0    
    .long





;=---------------------------------------------------------
; LibFunc:  _JCD_SetOversample
;
; Input:    d0: oversample value
;
;=---------------------------------------------------------

_JCD_SetOversample:
    move.w  d0,jcdl_OSample(a6)
    tst.l   jcdl_SPUReference(a6)
    beq.b   .noRef
;    CallSPU CalculateOSampleDelay      
    nop
.noRef:
    rts
    




; ------------------------------------------------------------
; Function:     ACK_rx
; Desc:         Wait for CDrom ACK
; Input:        None
; Output:       D0: return form CDROM
; ------------------------------------------------------------

ACK_rx:
.wait:    
    move.l  (a5),d0
    btst    #13,d0
    beq.b   .wait
    
    move.w  _DS_DATA(a5),d0
    move.l  _DSCNTRL(a5),d1
    andi.w  #$ff00,d0
    cmpi.w  #$400,d0
    beq.b   .dd
    rts
    
.dd:
;    move.w  #$ff00,$F00058   
    rts
    

; ------------------------------------------------------------
; Function:     ACK_tx
; Desc:         Wait for CDrom ACK
; Input:        None
; Output:       D0: return form CDROM
; ------------------------------------------------------------
ACK_tx:
.wait:  
    move.l  (a5),d0
    btst    #12,d0
    beq.b   .wait
    
    move.w  _DS_DATA(a5),d0
    move.l  _DSCNTRL(a5),d1
    rts
        
    
    


;=---------------------------------------------------------
; LibFunc:  _JCD_ActivateSession
; Desc:     Select the wrong Session
; In:       D0: session #
;=---------------------------------------------------------
_JCD_ActivateSession:
    


;=---------------------------------------------------------
; LibFunc:  _JCD_PlayMSFRange
; Desc:     Plays range between MSF at d0 and d1.
; In:       d0: start MSF
;           d1: end MSF
;
;=---------------------------------------------------------
_JCD_PlayMSFRange:
    movem.l d2/d3/a5,-(sp)
    movea.l jcdl_Butch(a6),a5
    move.w  #$ff,d3
    cmp.l   d0,d1
    beq.b   .leave
    bhi.b   .ok
    exg     d0,d1

.ok:
    move.l  d0,d2
    swap    d2
    and.w   d3,d2
    ori.w   #$2000,d2
    move.w  d2,_DS_DATA(a5)
    bsr.w   ACK_tx
    
    move.w  d0,d2
    lsr.w   #8,d2
    and.w   d3,d2
    ori.w   #$2100,d2
    move.w  d2,_DS_DATA(a5)
    bsr.w   ACK_tx
    
    and.w   d3,d0
    ori.w   #$2200,d0
    move.w  d0,_DS_DATA(a5)
    bsr.w   ACK_tx
    
    move.l  d1,d2
    swap    d2
    and.w   d3,d2
    ori.w   #$2300,d2
    move.w  d2,_DS_DATA(a5)
    bsr.w   ACK_tx
    
    move.w  d1,d2
    lsr.w   #8,d2
    and.w   d3,d2
    ori.w   #$2400,d2
    move.w  d2,_DS_DATA(a5)
    bsr.w   ACK_tx
    
    and.w   d3,d1
    ori.w   #$2500,d1
    move.w  d1,_DS_DATA(a5)
    bsr.w   ACK_tx
    
    moveq   #PLAYMODE_RANGE,d0
    move.w  d0,jcdl_playMode(a6)
    
    clr.w   jcdl_stop(a6)
    
.leave:
    movem.l (sp)+,d2/d3/a5
    rts
    


;=---------------------------------------------------------
; LibFunc:  _JCD_PlayTrack
; Desc:     Statrs playing a track.
; In:       d0: track number
;
;=---------------------------------------------------------
_JCD_PlayTrack:
    move.l  a5,-(sp)
    movea.l jcdl_Butch(a6),a5
    move.w  d0,jcdl_curTrack(a6)

    ori.w   #$100,d0
    move.w  d0,_DS_DATA(a5)
    moveq   #PLAYMODE_TRACK,d0
    move.w  d0,jcdl_playMode(a6)
    
    tst.w   jcdl_ACK(a6)
    beq.b   .leave
    
    bsr.w   ACK_rx
    
.leave:
    clr.w   jcdl_stop(a6)
    movea.l  (sp)+,a5
    rts
    


;=---------------------------------------------------------
; LibFunc:  _JCD_PlayMSF
; Desc:     Statrs reading disc at MSF pointer
; In:       d0: MMSSFF
;
;=---------------------------------------------------------
_JCD_PlayMSF:
    movem.l d2/a5,-(sp)
    movea.l jcdl_Butch(a6),a5
    move.w  #$ff,d2
    
    
    move.l  d0,d1                   ; get MM
    swap    d1                      ;
    and.w   d2,d1                   ;
    ori.w   #$1000,d1               ;
    move.w  d1,_DS_DATA(a5)         ; send Command
    bsr.w   ACK_tx                  ;
    
    move.w  d0,d1                   ; get SS
    lsr.w   #8,d1                   ;
    and.w   d2,d1                   ;
    ori.w   #$1100,d1               ;
    move.w  d1,_DS_DATA(a5)         ; send Command
    bsr.w   ACK_tx
    
    and.w   d2,d0                   ;
    ori.w   #$1200,d0               ; get FF
    move.w  d0,_DS_DATA(a5)         ; send Command
    bsr.w   ACK_tx                  ;
    
    moveq   #PLAYMODE_MSF,d0      
    move.w  d0,jcdl_playMode(a6)    ; set mode
    clr.w   jcdl_stop(a6)
    
    movem.l (sp)+,d2/a5
    rts
    


;=---------------------------------------------------------
; LibFunc:  _JCD_Wait
; Desc:     Forces wait for completioton of operation
;
;=---------------------------------------------------------
_JCD_Wait:
    move.l  a5,-(sp)
    movea.l jcdl_Butch(a6),a5
    bsr.w   ACK_rx
    movea.l (sp)+,a5
    rts



;=---------------------------------------------------------
; LibFunc:  _JCD_Mute
; Desc:     Mutes unMutes CDROM
;
;=---------------------------------------------------------
_JCD_Mute:
    move.l  a5,-(sp)
    movea.l jcdl_Butch(a6),a5
    not.w   jcdl_mute(a6)
    move.w  jcdl_mute(a6),d0
    ori.w   #$5100,d0
    move.w  d0,_DS_DATA(a5)
    
    tst.w   jcdl_ACK(a6)
    beq.b   .leave
    bsr.w   ACK_rx

.leave:
    movea.l (sp)+,a5
    rts    
    
    
    

;=---------------------------------------------------------
; LibFunc:  _JCD_Stop
; Desc:     Checks the stop flag
;           Calls a stop Command and sets flag
;           Calling stop is only alwed to be done once
;=---------------------------------------------------------
_JCD_Stop:
    move.l  a5,-(sp)
    movea.l jcdl_Butch(a6),a5               ; get Butch address
    tst.w   jcdl_stop(a6)
    bne.b   .leave
    moveq   #PLAYMODE_RANGE,d0              ; is the current
    cmp.w   jcdl_playMode(a6),d0            ; playMode a range MSF playback
    bne.b   .noAB
    move.w  #$2600,_DS_DATA(a5)             ; if yes kill this first1
    bsr.w   ACK_rx                          ; wait, and the stop fianlly
    
.noAB:                                      
    move.w  #$200,_DS_DATA(a5)
    moveq   #-1,d0
    move.w  d0,jcdl_stop(a6)                ; set the stop flag
    
    tst.w   jcdl_ACK(a6)                    ; blocking mode
    beq.b   .leave                          ;
    bsr.w   ACK_rx                          ;
    
.leave:
    movea.l (sp)+,a5
    rts
    

    

;=---------------------------------------------------------
; LibFunc:  CD_Pause
; Desc:     Checks the pause flag and pauses /
;           continues dependet on the flag
;           Default is unpaused. (=0)
;=---------------------------------------------------------
_JCD_Pause:
    move.l  a5,-(sp)
    movea.l jcdl_Butch(a6),a5
    tst.w   jcdl_pause(a6)
    bne.b   .unpause
    move.w  #$400,_DS_DATA(a5)
    bra.b   .cont
    
.unpause:
    move.w  #$500,_DS_DATA(a5)
    
.cont:
    not.w   jcdl_pause(a6)
    tst.w   jcdl_ACK(a6)
    beq.b   .leave
    bsr.w   ACK_rx

.leave:          
    movea.l (sp)+,a5
    rts
        

;=---------------------------------------------------------
; LibFunc:  _JCD_IsPaused, IsStopped, IsMuted
; Desc:     This function tests if the device is paused.
;
;=---------------------------------------------------------
_JCD_IsPaused:
    tst.w   jcdl_pause(a6)
    rts
    
_JCD_IsStopped:
    tst.w   jcdl_stop(a6)
    rts

_JCD_IsMuted:
    tst.w   jcdl_mute(a6)
    rts
    



;=---------------------------------------------------------
; Private LibFunc:  _jcdprivate_SaveRegisters
; Desc:             Save old register contents.
;
;=---------------------------------------------------------

_jcdprivate_SaveRegisters:
    move.l  (a5),jcdl_oldBUTCH(a6)
    move.l  _DSCNTRL(a5),jcdl_DSCNTRL(a6)
    move.w  _DS_DATA(a5),jcdl_DS_DATA(a6)
    move.l  _I2CNTRL(a5),jcdl_I2CNTRL(a6)
    move.l  _SBCNTRL(a5),jcdl_SBCNTRL(a6)
    rts
    



;=---------------------------------------------------------
; Private LibFunc:  _jcdprivate_RestoreRegisters
; Desc:             Restore old register contents.
;
;=---------------------------------------------------------

_jcdprivate_RestoreRegisters:
    move.l  a5,-(sp)
    lea     BUTCH,a5
    move.l  jcdl_oldBUTCH(a6),(a5)
    move.l  jcdl_DSCNTRL(a6),_DSCNTRL(a5)
    move.w  jcdl_DS_DATA(a6),_DS_DATA(a5)
    move.l  jcdl_I2CNTRL(a5),_I2CNTRL(a5)
    move.l  jcdl_SBCNTRL(a5),_SBCNTRL(a5)
    movea.l (sp)+,a5
    rts
      




; /====================\ -------------------------------------
; [Jaguar CDROM Library] -------------------------------------
; \====================/ -------------------------------------
    jmp     _jcdprivate_RestoreRegisters(pc)

    jmp     _JCD_SetOversample(pc)

    jmp     _JCD_PlayMSFRange(pc)
    jmp     _JCD_PlayTrack(pc)
    jmp     _JCD_PlayMSF(pc)
    jmp     _JCD_Stop(pc)
    jmp     _JCD_Mute(pc)
    jmp     _JCD_Pause(pc)

    jmp     _JCD_IsStopped(pc)
    jmp     _JCD_IsMuted(pc)
    jmp     _JCD_IsPaused(pc)
    jmp     _JCD_Wait(pc)
        
    jmp     _JCD_InheritSPU(pc)
    jmp     _JCD_Init(pc)
JCDLibName:
    dc.b    'jcd.library',0
    .dcb.b  32-(*-JCDLibName),0
JCDLibrary:

; [Library Data Section] -------------------------------------
; ------------------------------------------------------------
    dcb.b   jcdl_SIZEOF,0
    .long
    