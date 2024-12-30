; =========================================================
; $File:    spu_lib.inc
; $Begin:   29.10.02
; $Autor:   (c)Robert Jurziga / jaguar64@nanomonic.de
; $Update:  18.12.02
; $Desc:    Jaguar SPU Library defines and macros.
;           Editing this file may cause unexpected library
;           behavior.
; =========================================================

    .if !(^^defined __spu_lib_inc__)
    
__spu_lib_inc__     equ     1
    
    .globl     SPULibrary
    

; [SPU Library Call Macro] --------------------------------
; -----------------------/
    .macro  CallSPU vec
        move.l  a6,-(sp)                ;
        lea     SPULibrary,a6           ; symbol declared externally
        jsr     SPU_\vec(a6)            ; jump relative to reference
        movea.l (sp)+,a6                ;
    .endm

    .iif (^^defined vec), vec set -36
    
    .macro SPULibVec name
        .iif !(^^defined vec), vec set -36
        \name    equ    vec
        vec     set    vec-4
    .endm


; ---------------------------------------------------------



; [SPU MODES] ---------------------------------------------
SPUMODE_NORMAL      equ     0           ; normal sample playback
SPUMODE_CDDA        equ     1           ; CDDA + sample playback
; ---------------------------------------------------------


; [SPU Mix Frequencies] -----------------------------------
MF_8000             equ     0
MF_11025            equ     1
MF_16000            equ     2
MF_22050            equ     3
MF_32000            equ     4
MF_44100            equ     5
MF_MAX              equ     MF_44100
MF_DEFAULT          equ     MF_16000                    
; ---------------------------------------------------------


; [SPU CDDA OVERSAMPLE] -----------------------------------
    .if !(^^defined __OSAMPLE_TAG__)
__OSAMPLE_TAG__     equ     -1
OSAMPLE_NONE        equ     1
OSAMPLE_2X          equ     1<<1
OSAMPLE_4X          equ     1<<2
OSAMPLE_8X          equ     1<<3
OSAMPLE_DEFAULT     equ     OSAMPLE_NONE
    .endif    
; ---------------------------------------------------------

    

; [SPU Library Vectors] -----------------------------------
    
    SPULibVec  SPU_Init
    SPULibVec  SPU_EnableDSP
    SPULibVec  SPU_DisableDSP
    SPULibVec  SPU_SetMode
    
    SPULibVec  SPU_InitJSP
    SPULibVec  SPU_InitRawSample
    SPULibVec  SPU_InitSample

    SPULibVec  SPU_SetVoiceNumber
    SPULibVec  SPU_GetVoiceNumber
    SPULibVec  SPU_GetMixFreq
    SPULibVec  SPU_GetJaguarType
    SPULibVec  SPU_GetNoteFreq
    
    SPULibVec  SPU_EnableCDDA
    SPULibVec  SPU_DisableCDDA
    SPULibVec  SPU_SetCDDAVolume
    SPULibVec  SPU_SetCDDAOSample
    SPULibVec  SPU_GetCDDAVolume
    SPULibVec  SPU_GetCDDOSample

    SPULibVec  SPU_KeyOn
    SPULibVec  SPU_KeyOnSync
    SPULibVec  SPU_KeyOnVoice
    SPULibVec  SPU_KeyOnVoiceSync
    SPULibVec  SPU_KeyOff
    SPULibVec  SPU_KeyOffVoice
    SPULibVec  SPU_KeyOffVoices
    SPULibVec  SPU_SetVoiceAttr
    SPULibVec  SPU_SyncVoices
        
    SPULibVec  SPU_RetrigVoice
    SPULibVec  SPU_SetVoicePan
    SPULibVec  SPU_SetVoiceVolume

    SPULibVec  SPU_SetSFXVolume
    
    
; ---------------------------------------------------------




; **** STATIC EQUATES *****
; [Misc SPU Equates] --------------------------------------
MAX_VOLUME          equ     $4000
MAX_CDDAVOLUME      equ     MAX_VOLUME
PAN_LEFT            equ     -127
PAN_CENTER          equ     0
PAN_RIGHT           equ     127
; ---------------------------------------------------------



; [LOOP Flags and Bits] -----------------------------------
LOOPB_FORWARD           equ     0       ; Forward loop
LOOPB_PINGPONG          equ     1       ; PingPong loop
LOOPB_PPDIR             equ     2       ; PingPong Directive flag: 0=Forward Phase, 1=Backward Phase
LOOPB_ACTIVE            equ     3       ; Is loop active

LOOPF_FORWARD           equ     1<<LOOPB_FORWARD
LOOPF_PINGPONG          equ     1<<LOOPB_PINGPONG
LOOPF_PPDIR             equ     1<<LOOPB_PPDIR
LOOPF_ACTIVE            equ     1<<LOOPB_ACTIVE
; ---------------------------------------------------------

; [Voice Attributes] --------------------------------------
; If VOICEB_CLEAR is set the corresponding bit is cleard
; ---------------------------------------------------------
; - Bitdefs
VOICEB_SYNC             equ     0
VOICEB_CLEAR            equ     31

; - Flagdefs
VOICEF_SYNC             equ     1<<(VOICEB_SYNC)
VOICEF_CLEAR            equ     1<<(VOICEB_CLEAR)
; ---------------------------------------------------------



; [Standalone Sample Structure] ---------------------------
    .offset
jsam_mem:          .ds.l   1               ; pointer to sample
jsam_size:         .ds.l   1               ; sample size
jsam_loopBegin:    .ds.l   1               ; offet where loop Begin
jsam_loopSize:     .ds.l   1               ; size of the loop
jsam_res:          .ds.w   1               ; sample resolution
jsam_vol:          .ds.w   1               ; volume
jsam_pan:          .ds.w   1               ; panning
jsam_loopType:     .ds.w   1               ; see LOOPB equates
jsam_SIZEOF:       .ds.w   1
; ---------------------------------------------------------


; [Note Equates] ------------------------------------------

MAX_NOTE        equ     120             ; C-0 == 0
                                        ; B-7 == 96
NOTE_C0         equ     1
NOTE_C_0        equ     2
NOTE_D0         equ     3
NOTE_D_0        equ     4
NOTE_E0         equ     5
NOTE_F0         equ     6
NOTE_F_0        equ     7
NOTE_G0         equ     8
NOTE_G_0        equ     9
NOTE_A0         equ     10
NOTE_A_0        equ     11
NOTE_B0         equ     12
NOTE_C1         equ     13
NOTE_C_1        equ     14
NOTE_D1         equ     15
NOTE_D_1        equ     16
NOTE_E1         equ     17
NOTE_F1         equ     18
NOTE_F_1        equ     19
NOTE_G1         equ     20
NOTE_G_1        equ     21
NOTE_A1         equ     22
NOTE_A_1        equ     23
NOTE_B1         equ     24
NOTE_C2         equ     25
NOTE_C_2        equ     26
NOTE_D2         equ     27
NOTE_D_2        equ     28
NOTE_E2         equ     29
NOTE_F2         equ     30
NOTE_F_2        equ     31
NOTE_G2         equ     32
NOTE_G_2        equ     33
NOTE_A2         equ     34
NOTE_A_2        equ     35
NOTE_B2         equ     36
NOTE_C3         equ     37
NOTE_C_3        equ     38
NOTE_D3         equ     39
NOTE_D_3        equ     40
NOTE_E3         equ     41
NOTE_F3         equ     42
NOTE_F_3        equ     43
NOTE_G3         equ     44
NOTE_G_3        equ     45
NOTE_A3         equ     46
NOTE_A_3        equ     47
NOTE_B3         equ     48
NOTE_C4         equ     49
NOTE_C_4        equ     50
NOTE_D4         equ     51
NOTE_D_4        equ     52
NOTE_E4         equ     53
NOTE_F4         equ     54
NOTE_F_4        equ     55
NOTE_G4         equ     56
NOTE_G_4        equ     57
NOTE_A4         equ     58
NOTE_A_4        equ     59
NOTE_B4         equ     60
NOTE_C5         equ     61
NOTE_C_5        equ     62
NOTE_D5         equ     63
NOTE_D_5        equ     64
NOTE_E5         equ     65
NOTE_F5         equ     66
NOTE_F_5        equ     67
NOTE_G5         equ     68
NOTE_G_5        equ     69
NOTE_A5         equ     70
NOTE_A_5        equ     71
NOTE_B5         equ     72
NOTE_C6         equ     73
NOTE_C_6        equ     74
NOTE_D6         equ     75
NOTE_D_6        equ     76
NOTE_E6         equ     77
NOTE_F6         equ     78
NOTE_F_6        equ     79
NOTE_G6         equ     80
NOTE_G_6        equ     81
NOTE_A6         equ     82
NOTE_A_6        equ     83
NOTE_B6         equ     84
NOTE_C7         equ     85
NOTE_C_7        equ     86
NOTE_D7         equ     87
NOTE_D_7        equ     88
NOTE_E7         equ     89
NOTE_F7         equ     90
NOTE_F_7        equ     91
NOTE_G7         equ     92
NOTE_G_7        equ     93
NOTE_A7         equ     94
NOTE_A_7        equ     95
NOTE_B7         equ     96

; ---------------------------------------------------------

    
    .endif
    .end
        