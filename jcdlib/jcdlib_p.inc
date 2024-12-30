; ============================================================
; $File:    jcdlib_p.inc
; $Begin:   25.10.02
; $Autor:   (c)Robert Jurziga
; $Update:  02.11.02
; $Version: 1.00b
;
; $Desc:    Jaguar CDROM driver equates and sturctures
;           Included by jcdlib.inc
;
; ============================================================

    .if !(^^defined __jcdlib_p_inc__)

__jcdlib_p_inc__   equ    -1



; [CDROM Registers] ------------------------------------------
BUTCH           equ     $DFFF00
_DSCNTRL        equ     4
_DS_DATA        equ     $0A
_I2CNTRL        equ     $10
_SBCNTRL        equ     $14
_SUBDATA        equ     $18
_SUBDATB        equ     $1C
_SB_TIME        equ     $20
_FIFODAT        equ     $24
_I2SDAT2        equ     $28
; ------------------------------------------------------------



; [JCD Library structure] ------------------------------------
    .offset
jcdl_res0:          .ds.l   1
jcdl_Butch:         .ds.l   1                   ; base BUTCH address
jcdl_ErrorCode:     .ds.w   1                   ; Error Code
jcdl_SPUReference:  .ds.l   1                   ; SPU Reference.

jcdl_OSample:       .ds.w   1                   ; Oversample 0-none 1-1x etc
jcdl_Session:       .ds.w   1                   ; Operating section
jcdl_ACK:           .ds.w   1                   ; 0- no block, 1- blocking operation
jcdl_pause:         .ds.w   1                   ; pause flag
jcdl_stop:          .ds.w   1                   ; stop flag
jcdl_mute:          .ds.w   1                   ; $ff or $0
jcdl_curTrack:      .ds.w   1                   ; current track beeing played
jcdl_playMode:      .ds.w   1                   ; 0- no play, 1 msf, 2 track, 3 range
jcdl_Sessions:      .ds.w   1                   ; rquested by $5400 command

jcdl_oldBUTCH:      .ds.l   1                   ; old BUTCH contents
jcdl_DSCNTRL:       .ds.l   1                   ; old DSCNTRL
jcdl_DS_DATA:       .ds.w   1                   ; old DS_DATA
jcdl_I2CNTRL:       .ds.l   1                   ; old I2CNTRL
jcdl_SBCNTRL:       .ds.l   1                   ; old SBCNTRL

jcdl_SIZEOF:        .ds.l   1           
; ------------------------------------------------------------
    .endif

    .end
