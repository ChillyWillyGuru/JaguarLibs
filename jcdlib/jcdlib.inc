; ============================================================
; $File:    jcdlib.inc
; $Begin:   25.10.02
; $Autor:   (c)Robert Jurziga
; $Update:  26.11.02
; $Version: 1.00a
;
; $Desc:    Jaguar CDROM library jump offsets.
;           Included by user programs
;
; ============================================================

    .include    "typedefs.inc"
    
    .if !(^^defined __jcdlib_inc__)
__jcdlib_inc__         equ     -1

    .globl  JCDLibrary

; [JCD Library Call] ----------------------------------------
    .macro  CallJCD vec
        move.l  a6,-(sp)
        lea     JCDLibrary,a6
        jsr     JCD_\vec(a6)
        movea.l (sp)+,a6    
    .endm
; -----------------------------------------------------------


; [JCD Library Jump Offsets] --------------------------------
    BEGINLIB
        LIBVEC      JCD_Init
        LIBVEC      JCD_InheritSPU

        LIBVEC      JCD_Wait
        LIBVEC      JCD_IsPaused
        LIBVEC      JCD_IsMuted
        LIBVEC      JCD_Stopped

        LIBVEC      JCD_Pause
        LIBVEC      JCD_Mute
        LIBVEC      JCD_Stop

        LIBVEC      JCD_PlayMSF
        LIBVEC      JCD_PlayTrack
        LIBVEC      JCD_PlayMSFRange

        LIBVEC      JCD_SetOversample
    ENDLIB
       
; -----------------------------------------------------------


; [CDDA Playmodes] ------------------------------------------
PLAYMODE_MSF            equ     1
PLAYMODE_TRACK          equ     2
PLAYMODE_RANGE          equ     3
; -----------------------------------------------------------




; [SPU CDDA] ------------------------------------------------
OSAMPLE_NONE            equ     1
OSAMPLE_2X              equ     1<<1
OSAMPLE_4X              equ     1<<2
OSAMPLE_8X              equ     1<<3
OSAMPLE_DEFAULT         equ     OSAMPLE_NONE
; -----------------------------------------------------------

    .endif
    .end
    