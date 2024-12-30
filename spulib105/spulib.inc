; ============================================================
; $File:    jcdlib.inc
; $Begin:   25.10.02
; $Autor:   (c)Robert Jurziga
; $Update:  08.11.02
; $Desc:    Jaguar CDROM driver equates and sturctures
; ============================================================

    .include    "rjaguar.inc"


    .if !(^^defined __spulib_inc__)


__spulib_inc__   equ        -1




; [DEVPAC compatible macros] ------------------------------
    .macro  rsreset
        .offset
    .endm

    .macro  rs  value
        .ds\!   \value
    .endm
; ---------------------------------------------------------



; [MISC Defines] ------------------------------------------
JSP_MAGIC           equ     'JSP!'
; ---------------------------------------------------------




; [Voice Defines] -----------------------------------------
DEFAULT_VOICES      equ     8
MAX_VOICES          equ     12
MAX_DSP_TASKS       equ     32              ; 32 possible tasks
DEFAULT_SPF         equ     (16000/50)+1    ; default Samples/Per/Frame
DSPTASK_MIXBUFFER   equ     0               ; bit zero, always the 1st task
; ---------------------------------------------------------







; [DSP Voice Structure] -----------------------------------
; This sturcture is read by the DSP mixing routine
    .offset
v_sample:       .ds.l    1           ; where in memory
v_size:         .ds.l    1           ; sample size in bytes
v_res:          .ds.l    1           ; 8 or 16 bit
v_offset:       .ds.l    1           ; offset in bytes
v_loopBegin:    .ds.l    1           ; loop start point
v_loopSize:     .ds.l    1           ;
v_loopType:     .ds.l    1           ; 0=no loop, 1=forward, 2=pingpong
v_freqc:        .ds.l    1           ; sample freqency counter / added tp every SCLK
v_freqs:        .ds.l    1           ; this value is added up every SCLK to v_freqc
                                     ; both values are stored <<11 because of fixed point
                                     ; math
v_vol:          .ds.l    1           ; Voice volume
v_panl:         .ds.l    1           ; voice Pan Left
v_panr:         .ds.l    1           ; voice Pan right
v_flags:        .ds.l    1           ; voice reserve flag
Voice_SIZEOF:   .ds.w    1           ;

; Risc access vectors for (r14+n),Rn addressing mode
;
dv_sample           equ     v_sample/4
dv_size             equ     v_size/4
dv_res              equ     v_res/4
dv_offset           equ     v_offset/4
dv_loopBegin        equ     v_loopBegin/4
dv_loopSize         equ     v_loopSize/4
dv_loopType         equ     v_loopType/4
dv_freqc            equ     v_freqc/4
dv_freqs            equ     v_freqs/4
dv_vol              equ     v_vol/4
dv_panl             equ     v_panl/4
dv_panr             equ     v_panr/4
dv_flags            equ     v_flags/4
; -----------------------------------------------------------------------------



; [JSP Structure] -----------------------------------------
    .offset
jsp_Magic:       .ds.l    1           ; 'JSP!'
jsp_Version:     .ds.w    1           ; $1
jsp_Samples:     .ds.w    1           ; how many samples has this sample pack
jsp_SIZEOF:      .ds.w    1           ; here start the absolute offsets to this
                                      ; sample pack
; ---------------------------------------------------------




; [JSP Sample Sturcure] ------------------------------------
; in the current version samples are
; stored as DELTA Values, it means better
; compression rations, using any kind of LZ packer.
;

    .offset
jsps_size:      .ds.l    1           ; how big is the sample
jsps_loopBegin: .ds.l    1           ; offset where loop begins
jsps_loopSize:  .ds.l    1           ; end offset of the loop
jsps_res:       .ds.w    1           ; 8=8bit, 16=16bit
jsps_vol:       .ds.w    1           ; volume
jsps_fineTune:  .ds.w    1           ; finetune
jsps_pan:       .ds.w    1           ; panning
jsps_loopType:  .ds.w    1           ; see LOOPB equates above
jsps_relNote:   .ds.w    1           ; relative note to C-4
jsps_SIZEOF:    .ds.w    1
; ---------------------------------------------------------




; [DSP CDDA Structure] ------------------------------------
    .offset
cdda_Activate:      .ds.l   1
cdda_Delay:         .ds.l   1
cdda_Volume:        .ds.l   1
cdda_Pan:           .ds.l   1
cdda_SIZEOF:        .ds.l   1

dcdda_Activate      equ     cdda_Activate/4
dcdda_Delay         equ     cdda_Delay/4
dcdda_Volume        equ     cdda_Volume/4
dcdda_Pan           equ     cdda_Pan/4
; ---------------------------------------------------------





; [IFF Header equates] ------------------------------------
                rsreset
iff_Magic:      rs.l   1                       ; magic
iff_Size:       rs.l   1                       ; header size
iff_SIZEOF:     rs.w   1                       ; sizeOF

	rsreset
iffvhdr_iff:                rs.b   iff_SIZEOF  ; standar iff header
iffvhdr_oneShotHiSamples:   rs.l   1           ; <not supported>
iffvhdr_repeatHiSamples:    rs.l   1           ; <not supported>
iffvhdr_samplesPerHiCycle:  rs.l   1           ; <not supported>
iffvhdr_samplesPerSec:      rs.w   1           ; <not supported>
iffvhdr_ctOctave:           rs.b   1           ; only ==1 supported
iffvhdr_sCompression:       rs.b   1           ; 0: no compression, 1: delta length Fibnocci
iffvhdr_volume:             rs.l   1           ; 16:16 volume 1.0 = full

	rsreset
aiffcomm_iff:				rs.b	iff_SIZEOF 	; standard iff header
aiffcomm_channels:			rs.w	1			; 1 = mono, 2=stereo
aiffcomm_sampleFrames:		rs.l	1			; not used
aiffcomm_sampleSize:		rs.w	1			; 16 = 16bit
aiffcomm_sampleRate:		rs.b	10			; extended 80bit floating
aiffcomm_compressionID:		rs.l	1			; Pack ID

	rsreset
aiffssnd_iff:				rs.b	iff_SIZEOF
aiffssnd_offset:			rs.l	1
aiffssnd_blockSize:			rs.l	1
aiffssnd_SIZEOF:			rs.b	1

	rsreset
wavefmt_iff:				rs.b	iff_SIZEOF	; starndard iff header
wavefmt_formatTag:			rs.w	1			; wave foramt
wavefmt_channels:			rs.w	1			; 1 = mono, 2=stereo
wavefmt_samplesPerSec:		rs.l	1			;
wavefmt_avgBytesPerSec:		rs.l	1
wavefmt_blockAlign:			rs.w	1
wavefmt_bitsPerSample:		rs.w	1

	rsreset
voc_id:						rs.b	20			; 'Creative Voice File',$1a
voc_headerSize:				rs.w	1			; LE size
voc_versionNumber:			rs.w	1			;
voc_versionNumberCheck:		rs.w	1			;

; ---------------------------------------------------------
JSAMFLAG_ENDIAN                     equ     0


JSAM_FAILURE			    		equ		0
JSAM_OK					    		equ		1

JSAMERR_UnknownFormat		    	equ		-1

JSAMERR_IFFFormatNotSupported		equ		1
JSAMERR_IFFNoDestBuffer				equ		2
JSAMERR_IFFCorrupt					equ		3
JSAMERR_AIFFCorrupt					equ		10
JSAMERR_AIFFBitWidthNotSupported	equ		11
JSAMERR_AIFFFormatNotSupported		equ		12
JSAMERR_WAVECorrupt					equ		20
JSAMERR_WAVENotSupported			equ		21
JSAMERR_BitWidthNotSupported		equ		22
JSAMERR_VOCCorrupt					equ		30






; [SPU Library Handle] ------------------------------------
    .offset
spul_JagType:           .ds.w   1
spul_StepTable:         .ds.l   1
spul_VoiceTable:        .ds.l   MAX_VOICES

spul_MaxVolumeValue:    .ds.w   1
spul_SFXVolume:         .ds.w   1

spul_cddaStatus:        .ds.w   1
spul_cddaVolume:        .ds.w   1
spul_cddaOSample:       .ds.w   1
spul_cddaDelay:         .ds.w   1

spul_MixFrequency:      .ds.w   1
spul_VoiceNumb:         .ds.w   1
spul_Mode:              .ds.w   1
spul_SCLK:              .ds.w   1
spul_SMODE:             .ds.w   1
spul_SamplesPerFrame:   .ds.w   2

spul_Voices:            .ds.b   (MAX_VOICES*Voice_SIZEOF)   ; for defined/syncronized voice play
spul_Voice:             .ds.b   (Voice_SIZEOF)              ; key on
spul_SIZEOF:            .ds.b   1
; ---------------------------------------------------------

    .endif
    .end
    