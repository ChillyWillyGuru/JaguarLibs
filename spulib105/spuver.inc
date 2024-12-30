; ============================================================
; $File:    spuver.inc
; $Begin:   28.11.02
; $Autor:   (c)Robert Jurziga
; $Update:  13.12.02
; $Desc:    Version for SPU library.
; ============================================================

; [SPU Library Version] -----------------------------------
SPUL_Version    equ     $105
; ---------------------------------------------------------

    .dc.b   "$LIB:SPU Library (c)2002-2003 Robert Jurziga",0
    .dc.b   "$VER:",SPUL_Version>>8,SPUL_Version&$ff

