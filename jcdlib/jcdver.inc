; ============================================================
; $File:    jcdver.inc
; $Begin:   28.11.02
; $Autor:   (c)Robert Jurziga
; $Update:  28.11.02
; $Desc:    Version for JCD library.
; ============================================================

; [JCD Version] ----------------------------------------------
JCDL_Version    equ $100                ; 1<<8 

    .dc.b   "$LIB:Jaguar CDROM Library (c)2002 Robert Jurziga",0
    .dc.b   "$VER:",JCDL_Version>>8,JCDL_Version&$ff
