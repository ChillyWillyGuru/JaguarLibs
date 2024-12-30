; =========================================================
; $File:    spulib.gas
; $Begin:   28.10.02
; $Autor:   (c)Robert Jurziga
; $Update:  28.12.02
; $Desc:    Jaguar Sound System DSP core source.
;
;  - - - - - -  - -- - - -- ----- - - - - - - - -  - -- - -

    .include    "riscmnem.inc"
    .include    "rjaguar.inc"
    .include    "spulib.inc"

    .dsp
    .org    DSP_RAM

spu_DSPCore:

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dsp_CPU:
    movei   #dsp_SPU,r1
    movei   #DSP_BASE+D_FLAGS,r30
    jump    (r1)
    nop
    

dsp_I2S:
    movei   #dsp_I2S_Handler,r1
    movei   #DSP_BASE+D_FLAGS,r30
    jump    (r1)
    nop


dsp_Timer1:
    movei   #dsp_Timer1_Handler,r1
    movei   #DSP_BASE+D_FLAGS,r30
    jump    (r1)
    nop

    
            
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; * ---------------------------------------------------------------------------
; * ---- DSP: Init
; * ----

dsp_Init:

; [Init BANK0/1 common registers] -------------------------
    moveq   #0,r0                   ; r0 always0
    moveta  r0,r0                   ; r0 = 0

    movei   #DSP_BASE+D_FLAGS,r29   ; flags
    load    (r29),r1                ; make sure its bank0
    or      r1,r1                   ;
    bclr    #14,r1                  ;
    store   r1,(r29)                ;
    nop                             ;
    nop                             ;
    nop                             ;
 
         
; --- [Setup Stacks] --------------------------------------
    movei   #DSP_RAM+8192,r31       ; set stack
    move    r31,r28
    subqt   #32,r28                 ; eight longs for interrupt stack
    moveta  r28,r31                 ; set stack.1


; --- [Set I2S IRQ Data] ----------------------------------
    movei   #DSP_BASE+L_I2S,r10     ; DAC address
    moveq   #4,r9                   ; L_DAC
    add     r10,r9                  ; R_DAC

    moveq   #1,r12                  ; this ensure a new sample buffer will be computed
    moveq   #DEFAULT_VOICES,r13     ; # of voices        
    moveq   #0,r16                  ; CDDA Flag:    Defualt OFF
    moveq   #0,r17                  ; CDDA Volume:  $0
    moveq   #1,r18                  ; CDDA Delay:   $1
    moveq   #0,r19                  ; clear CDDA irq counter
    moveq   #0,r20                  ; SFX volume default value
    moveq   #1,r21                  ; set Sample per frame to 1 this time
    laddr   dsp_MixBufferInvoker,r22 
    moveq   #0,r23                  ; clear user voice address / 0 means TIMER1 will not
                                    ; set any new voice.
                   
    laddr   dsp_SampleBufferA,r14   ; get sample Buffer A
    laddr   dsp_SampleBufferB,r15   ; get sample Buffer B
    move    r14,r11                 ; sample Buffer

    ;** Enable Interrupt I2S and switch to Bank1 **
    load    (r29),r1                ; get flags
    bset    #4,r1                   ; enable IRQ 0
    bset    #5,r1                   ; enable I2S IRQ
    bset    #6,r1                   ; enable Timer1 IRQ
    bset    #14,r1                  ; Register Bank 1
    store   r1,(r29)
    nop                             ;
    nop                             ;
    nop                             ; delay


; [Init BANK1 dispatcher] ---------------------------------    
; The Dispatcher occupies
; r25   = event request mask
; r26   = event test bit
; r27   = &dsp_Task[]
; ---------------------------------------------------------
    laddr   dsp_EventMask,r25       ; clear event mask
    store   ZERO,(r25)              ; clear event mask

    laddr   dsp_Tasks,r27           ; tasks to do
    laddr   dsp_MixBuffer,r26       ; static dsp task
    store   r26,(r27)               ; register MixBuffer address within the dispatcher

    laddr   dsp_SPUEnd,r1           ; get free memory
    laddr   dsp_FreeStore,r2        ; free store keeper
    store   r1,(r2)                 ; store the 1st free memory address
    

    
; [Dispatcher] ------------------------------------------------------
; ------------
; The main event checking loop
;
dsp_InitDispatcher:
    laddr   dsp_Tasks,r27           ; tasks to do
    moveq   #1,r26                  ; starting event bit
        
dsp_Dispatcher:
    load    (r25),r24               ; get events
    or      r24,r24                 ; 
    cmpq    #0,r24                  ; test if any event registered
    beqs    dsp_Dispatcher          ; loop as long as no tasks to do
                                    ; events are requested by an CPU interrupt
                                    ;                                    
    move    r26,r1                  ; get event bit
    and     r24,r1                  ; and with event bit
    jalrne  dsp_doTask              ; do the task
        
    addqt   #4,r27                  ; next task routine
    shlq    #1,r26                  ; << 1
    bccl    dsp_Dispatcher          ; with
    bral    dsp_InitDispatcher      ; 
; -------------------------------------------------------------------




; [Do Task Subroutine] ----------------------------------------------
dsp_doTask:
    push    LR                      ; keep the link register
    push    r27                     ; keep current task  address pointer
    push    r26                     ; keep event test bit
    push    r25                     ; keep event mask
    push    r24                     ; keep current event mask
    
    load    (r27),r1                ; get jump address
    or      r1,r1                   ; make sure != 0
    cmpq    #0,r1                   ;
   jralrnep r1                      ; jump and link to R1 if != 0
    
    pop     r24                     ; restore current event mask
    pop     r25                     ; restore event mask
    pop     r26                     ; restore event test bit
    pop     r27                     ; restore task address pointer
    pop     LR                      ; restore LINK register

    move    r26,r1                  ; take this event
    not     r1                      ; create clear mask
    and     r1,r24                  ; and mark event as done
    store   r24,(r25)               ;
    
    retr                            ; return to dispatcher
    
 
       
    
        
    
; -------------------------------------------------------------------    
    ; []*** Mix Up Buffer ***[]
; -------------------------------------------------------------------    
dsp_MixBuffer:    
        
    movefa  r14,r13                     ; swap
    movefa  r15,r1                      ; buffers
    moveta  r13,r15                     ;
    moveta  r1,r14                      ;

    moveta  r1,r11                      ; * play buffer
    moveta  r13,r15                     ; * mix buffer
    
    ; * Mixup Buffer at r13
    ; * -------------------------------------
    ; * r13: buffer
    ; * r14: dsp_Voices
    ; * r16: voice sizeof
    ; * r17: Voice count
    ; * r18: Buffer size counter
    ; * r19: sample left
    ; * r20: sample right
    
    movei   #dsp_MixVoices,r21          ; label: dsp_MixVoices
    movei   #dsp_MixLoop,r22            ; label: dsp_MixLoop
    movei   #dsp_NextVoice,r23          ; label: dsp_NextVoice
    movei   #dsp_UpdateSample,r24       ; label: dsp_UpdateSample
    movei   #dsp_LoopHandler,r25        ; label: dsp_LoopHandler
    
    movei   #Voice_SIZEOF,r16           ; sizeof(Voice)
    movefa  r21,r18                     ; get samples per frame
                
dsp_MixLoop:                            ; ========================================= r22
    movefa  r13,r17                     ; get number of vices to process
    movei   #dsp_ShadowVoices,r14
    moveq   #0,r19
    moveq   #0,r20
    
    
dsp_MixVoices:                          ; ========================================= r21
    load    (r14),r1                    ; get sample pointer
    load    (r14+dv_vol),r12            ; get volume
    cmpq    #0,r1                       ; sample * == 0
    jump    EQ,(r23)                    ; yes -> dsp_NextVoice
    nop                                 ;
    load    (r14+dv_res),r8             ; get sample resolution
    load    (r14+dv_loopType),r2        ; r2 = loopType
    load    (r14+dv_offset),r11         ; r11= offset
    load    (r14+dv_freqs),r10          ; r10= freqs
    load    (r14+dv_freqc),r9           ; r9 = freqc        
    cmpq    #0,r12                      ; volume == 0
    jump    EQ,(r24)                    ; yes -> dsp_UpdateSample
    nop                                 ;
    cmpq    #8,r8                       ; is this a 8bit sample
    jr      EQ,.dsp8bit                 ;
    nop                                 ;
    shlq    #1,r11                      ; one sample = 16bit
    add     r11,r1                      ; for 16bit update offset twice
    loadw   (r1),r7                     ; load sample word
    jr      T,dsp_DoSample
    nop
        
        
.dsp8bit:
    add     r11,r1                      ; point to right sample data in sample
    loadb   (r1),r7                     ; get sample byte    
    shlq    #8,r7                       ; adjust to 16 bit
            
            
dsp_DoSample:                           ; r6 = Left
    load    (r14+dv_panl),r4            ; get pan left
    move    r7,r6                       ; r7 = right
    load    (r14+dv_panr),r5            ; get pan right
    imult   r4,r6
    imult   r5,r7                       ; pan*right
    sharq   #8,r6                       ;
    sharq   #8,r7                       ;
    imult   r12,r6                      ; left*vol
    imult   r12,r7                      ; right*vol
    sharq   #14,r6                      ; norm.
    sharq   #14,r7                      ; norm.

    add     r6,r19                      ; left+=newleft
    add     r7,r20                      ; right+=newright
    
dsp_UpdateSample:
    cmpq    #0,r2                       ; is this a looped type sample (has been loaded above)
    jump    NE,(r25)                    ; jump to the loop Handler
    nop
    
    add     r10,r9                      ; freqc+=freqs
    move    r9,r11                      ; t = new freqc
    load    (r14+dv_size),r2            ; get the sample size
    shrq    #11,r11                     ; newOffset = t>>11
    or      r2,r2                       ; make sure varaible is loaded
    subq    #1,r2                       ; avoice clicking
    cmp     r11,r2                      ; end of sample reached
    jr      HI,.noend
    nop
    store   r0,(r14)                    ; disable Voice
    jump    (r23)                       ; []-> dsp_NextVoice
    nop    

.noend:
    store   r11,(r14+dv_offset)         ; store new offset
    store   r9,(r14+dv_freqc)           ; and the freqc for that offset
    nop
        
        
; --------------------------------------\
dsp_NextVoice:                          ; ========================================= r23
    add     r16,r14                     ; next voice
    subq    #1,r17                      ; --1
    jump    NE,(r21)                    ; dsp_MixVoices
    nop                                 ;
                                        ;
    sat16s  r19                         ; adjust sample left
    sat16s  r20                         ; adjust sample right
                                            ;
    shlq    #16,r20                     ; R << 16
    shlq    #16,r19                     ; L << 16
    shrq    #16,r20                     ; R >> 16
    or      r19,r20                     ; STREO = R|L
    store   r20,(r13)
    addqt   #4,r13
                                        ;
    subq    #1,r18                      ; decrement sample buffer count
    jump    NE,(r22)                    ; dsp_MixLoop    
    nop                                 ;

    retr
    
;    movei   #dsp_Tasks+DSPTASK_MIXBUFFER,r3
;    store   r0,(r3)                     ; mark as done

;    movei   #dsp_Dispatcher,r2          ;
;    jump    (r2)                        ; next tasks.
;    nop                                 ;
; --------------------------------------/


; -------------------------------------------------------------------
;   []*** Sample Loop Handler ***[]
;
; r2= loopType value                        
;                                       dsp_LoopHandler
; --------------------------------------\/---------------------------
dsp_LoopHandler:                        ; =========================== r26
    movei   #dsp_PingPongLoop,r3        ;
    btst    #LOOPB_FORWARD,r2           ;
    jump    EQ,(r3)                     ;
    nop                                 ;
                                                                                     ;
    load    (r14+dv_loopBegin),r2       ; get the sample size
    load    (r14+dv_loopSize),r3        ; get loop Size
    or      r2,r2
    move    r2,r4                       ;
    add     r3,r2                       ; get end of looped sample
                                        ;
    add     r10,r9                      ; freqc+=freqs
    move    r9,r11                      ; t = new freqc
    shrq    #11,r11                     ; newOffset = t>>11
    subq    #1,r2                       ; avoice clicking
    cmp     r11,r2                      ; end of sample reached
    jr      HI,.noend                   ;
    nop                                 ;
                                        ;
    move    r4,r11                      ; saved before
    move    r4,r9                       ;
    shlq    #11,r9                      ; new freqc = loopBegin<<11
                                        ;
.noend:                                 ;
    store   r11,(r14+dv_offset)         ; store new offset
    nop
    store   r9,(r14+dv_freqc)           ; and the freqc for that offset
    jump    (r23)                       ;
    nop                                 ;    
;---------------------------------------/----------------------------

;                                       dsp_PingPongLoop
; --------------------------------------\/---------------------------
dsp_PingPongLoop:                       ;
    moveq   #(1<<LOOPB_PPDIR),r6        ; direction flag
    movei   #dsp_PingPongBackStep,r3    ; 
    btst    #LOOPB_PPDIR,r2             ; test direction
    nop
    jump    NE,(r3)                     ;
    nop                                 ;
    move    r2,r7                       ;
    load    (r14+dv_loopBegin),r2       ; get the sample size
    load    (r14+dv_loopSize),r3        ; get loop Size
    or      r2,r2
    move    r2,r4                       ;
    add     r3,r2                       ; get end of looped sample
                                        ;
    add     r10,r9                      ; freqc+=freqs
    move    r9,r11                      ; t = new freqc
    shrq    #11,r11                     ; newOffset = t>>11
    subq    #1,r2                       ; avoice clicking
    cmp     r11,r2                      ; end of sample reached
    nop
    jr      PL,.noend                   ;
    nop                                 ;
                                        ;
    move    r2,r11                      ;
    move    r2,r9                       ;
    shlq    #11,r9                      ; new freqc = loopBegin<<11
    xor     r6,r7                       ;
    store   r7,(r14+dv_loopType)        ; store flag
                                        ;
.noend:                                 ;
    store   r11,(r14+dv_offset)         ; store new offset
    store   r9,(r14+dv_freqc)           ; and the freqc for that offset
    jump    (r23)                       ;
    nop                                 ;
; --------------------------------------/    


;                                       dsp_PingPongBackStep
; --------------------------------------\/---------------------------
dsp_PingPongBackStep:                   ;
    move    r2,r7                       ;
    load    (r14+dv_loopBegin),r2       ; get the sample size
    sub     r10,r9                      ; freqc-=freqs
    move    r9,r11                      ; t = new freqc
    move    r2,r4                       ;
    addq    #1,r2                       ; avoid playing too much
    shrq    #11,r11                     ; newOffset = t>>11
    cmp     r2,r11                      ; end of sample reached
    jr      HI,.noend                   ;
    nop                                 ;
                                        ;
    move    r4,r11                      ;
    move    r4,r9                       ;
    shlq    #11,r9                      ; new freqc = loopBegin<<11
    xor     r6,r7                       ;
    store   r7,(r14+dv_loopType)        ; store flag
                                        ;
.noend:                                 ;
    store   r11,(r14+dv_offset)         ; store new offset
    store   r9,(r14+dv_freqc)           ; and the freqc for that offset
    jump    (r23)                       ;
    nop                                 ;
; --------------------------------------/    
    
    


; ===================================================================
; Timer1 Interrupt Handler routine
; Bank0:
; * r9  = R_DAC
; * r10 = L_DAC
; * r11 = play Sample Buffer pointer
; * r12 = counter *
; * r13 = # of voices processed by DSP
; * r14 = base * to sample buffer
; * r15 = base * to sample buffer 2
; * r16 = CDDA flag
; * r17 = CDDA Volume
; * r18 = CDDA Oversampling step
; * r19 = Irq Counter
; * r20 = SFX Volume
; * r21 = Samples per frame
; * r22 = Mix buffer invoker
; * r23 = sync voices pointer
;
; ===================================================================
dsp_Timer1_Handler:

    load    (r30),r29
    
    movei   #dsp_Timer1_Serviced,r7 ;    
    cmpq    #0,r23                  ; user voices set ??
    jump    EQ,(r7)
    nop
    
    move    r23,r1                  ; voice pointer
    move    r13,r2                  ; number of voices to take
    movei   #v_flags,r3             ; get offset to v_flags.
    movei   #Voice_SIZEOF,r7        ;
    movei   #dsp_ShadowVoices,r24   ; pointer to the shadow voice
    movei   #dsp_SyncVoiceLoop,r25  ; the loop
    movei   #dsp_NextSyncVoice,r28  ;
    
dsp_SyncVoiceLoop:
    move    r1,r4                   ; user voice pointer
    load    (r1),r26
    add     r3,r4                   ; get v_flags address
    or      r26,r26
    cmpq    #0,r26
    jump    EQ,(r28)
    nop
    load    (r4),r5                 ; get flags
    or      r5,r5
    btst    #VOICEB_SYNC,r5         ; is a sync voice
    jump    EQ,(r28)
    nop
    moveq   #Voice_SIZEOF/4,r6
    move    r1,r4                   ; user voice
    move    r24,r5                  ; dsp shadow voice
    
.dsp_CopyVoiceLoop:
    load    (r4),r26                ; get voice long
    or      r26,r26                 ;
    store   r26,(r5)                ; store voice long
    addqt   #4,r4                   ; +=4
    addqt   #4,r5                   ; +=4
    subq    #1,r6                   ; --
    jr      NE,.dsp_CopyVoiceLoop   ;
    nop  
    store   r0,(r1)                 ; mark voice as taken
      
dsp_NextSyncVoice:
    add     r7,r24                  ; next shadow voice
    add     r7,r1                   ; next user voice
    subq    #1,r2
    jump    NE,(r25)
    nop

    moveq   #0,r23                  ; all voices taken, clear issue

dsp_Timer1_Serviced:                        
    bclr    #3,r29                  ; interrupt serviced
    bset    #11,r29                 ; clear int source flag: Timer 1
    load    (r31),r28               ; get return from stack
    addqt   #4,r31                  ; update stack
    addqt   #2,r28                  ; update return address
    jump    (r28)                   ; return from int
    store   r29,(r30)                ; reset flags





; * -----------------------------------------------------------------
; * ---- DSP: I2S Handler
; * ---- This handler should handle an 8x oversampling when using Audio CD.
; * ---- Consideration:
; * ---- sClk   = 25493900
; * ---- CDFreq = 44100
; * ---- osampl = 2x/4x/8x
; * ---- cpi    = sClk / (CDFreq* osampl)
; * ---- Assuming an 8x oversample would make:
; * ---- cpi    = 26593900 / (44100*8)
; * ---- cpi    = 75            (cpi = cycles per interrupt)
; * -----------------------------------------------------------------
; * r9  = R_DAC
; * r10 = L_DAC
; * r11 = Sample Buffer
; * r12 = counter
; * r13 = # of voices processed by DSP
; * r14 = base * to sample buffer
; * r15 = base * to sample buffer 2
; * r16 = CDDA flag
; * r17 = CDDA Volume
; * r18 = CDDA Oversampling step
; * r19 = Irq Counter
; * r20 = SFX Volume
; * r21 = Samples per frame
; * r22 = mix buffer invoker
; * r23 = sync voices
;
; * r29 = DSP_FLAGS

dsp_I2S_Handler:

    load    (r30),r29               ; save flags

    load    (r11),r3                ; get packed sample data
    move    r3,r4                   ; clone
    shlq    #16,r3                  ; Right << 16
    sharq   #16,r4                  ; Left >>16  sign extension
    sharq   #16,r3                  ; Right >> 16 sign extension
    addqt   #4,r11                  ; update pointer
        
    imult   r20,r3                  ; Compute
    imult   r20,r4                  ; SFX Volume
    sharq   #14,r3                  ; . -- - -- .. .
    sharq   #14,r4                  ; - -- -. ..- -- --

    cmpq    #0,r16
    jr      NE,dsp_DoCDDA
    nop
            
    store   r3,(r10)                ; output some noise
    store   r4,(r9)                 ; ditto

    subq    #1,r12                  ; Sample buffer Counter +=1
    jump    EQ,(r22)                ; mix buffer invoker
    nop
    
    bclr    #3,r29                  ; interrupt serviced
    bset    #10,r29                 ; clear int source flag
    load    (r31),r28               ; get return from stack
    addqt   #4,r31                  ; update stack
    addqt   #2,r28                  ; update return address
    jump    (r28)                   ; return from int
    store   r29,(r30)               ; reset r31 flags



; [I2S CDDA Handler] ------------------------------------------------
; 
;
dsp_DoCDDA:
    load    (r10),r6                ; external I2S is done, so
    load    (r9),r7                 ; load the CDDA Sample Data.
 
    subqt   #4,r11                  ; update back the sample buffer
                                
    imult   r17,r6                  ; cdda right*volume
    imult   r17,r7                  ; cdda left*volume
    sharq   #14,r6                  ; norm
    sharq   #14,r7                  ; norm
    add     r3,r6                   ; +=sample right
    add     r4,r7                   ; +=sample left
    sat16s  r6                      ; avoid boosting
    sat16s  r7                      ; avoid boosting

    store   r6,(r10)                ; output noise
    store   r7,(r9)                 ; output noise
    
    addqt   #1,r19                  ; osamplc+=1
    cmp     r19,r18                 ; 
    jr      NE,dsp_I2SServiced      ; if ovsmpl!=counter
    nop
    
    addqt   #4,r11
    moveq   #0,r19                  ; clear counter
    subq    #1,r12                  ; sample counter+=1    
    jump    EQ,(r22)                ; mix buffer invoker
    nop
    
dsp_I2SServiced:
    bclr    #3,r29                  ; interrupt serviced
    bset    #10,r29                 ; clear int source flag
    load    (r31),r28               ; get return from stack
    addqt   #4,r31                  ; update stack
    addqt   #2,r28                  ; update return address
    jump    (r28)                   ; return from int
    store   r29,(r30)               ; reset r31 flags
    

; [Register Mix Buffer Routine] ---------------------------
dsp_MixBufferInvoker:
;    movei   #dsp_Tasks+DSPTASK_MIXBUFFER,r1
;    movei   #dsp_MixBuffer,r2
;    store   r2,(r1)
;    move    r21,r12                 ; restore "samples per frame" counter
;    jr      T,dsp_SampleBufSkip
;    nop
 
    movei   #dsp_EventMask,r1       ; get Event mask holder
    load    (r1),r2                 ; get events
    or      r2,r2                   ; make sure load is loaded
    bset    #DSPTASK_MIXBUFFER,r2   ; mark task to do (this is a mandatory task for sound)
    store   r2,(r1)                 ; store back event mask
    move    r21,r12                 ; restore "samples per frame" counter
    jr      T,dsp_I2SServiced       ; and out of the interrupt
    nop                             ; -:: ---- ::- ;-:--- -- - -- 
    

; ===================================================================
; [SPU Style] -----------------------------   -----------------------
; Register Bank 0
;
; r1: dsp_SPU
; r2: Parameter base
;
; Reserved: 
; r2 = * 2nd parameter
; r5 = parameter 2
; r6 = parameter 1
; r7 = ISR Comeback
; r8 = Flags
; 
; Free:
; r1-r5/r13/r24-r27
; -------------------------------------------------------------------
dsp_SPU:
    load    (r30),r29               ; load flags

    movei   #spu_Command,r2
    load    (r2),r3                 ; r3 = SPU function offset : its the command
    movei   #dsp_SPU_ISR,r7         ; get SPU IS

    cmpq    #0,r3                   ; function == 0 ??
    jump    EQ,(r7)                 ; yes -> just get out of the interrupt
    nop
    
    add     r1,r3                   ; compute jump address
    store   r0,(r2)                 ; reset command: set to zero
    addqt   #4,r2                   ; paramter address

    load    (r2),r6                 ; get parameter 1
    addqt   #4,r2                   ;

    load    (r2),r5                 ; get parameter 2
    jump    (r3)                    ; execute command
    nop
        
dsp_SPU_ISR:    
    bclr    #3,r29                  ; interrupt serviced
    bset    #9,r29                  ; clear int source flag
    load    (r31),r28               ; get return from stack
    addqt   #4,r31                  ; update stack
    addqt   #2,r28                  ; update return address
    jump    (r28)                   ; return from int
    store   r29,(r30)               ; reset r31 flags
    
    
    
; [SPU Command: Set CDDA Volume] ---------------------------
; \----------------------------/
spu_SetCDDAVolume:
    move    r6,r17
    jump    (r7)
    nop
    

; [SPU Command: Set CDDA Delay] ---------------------------
; \---------------------------/
spu_SetCDDADelay:
    move    r6,r18                  ; set delay count
    moveq   #0,r19                  ; clear delay counter
    jump    (r7)                
    nop
    

; [SPU Command: Set CDDA Status] --------------------------
; \----------------------------/
spu_SetCDDAStatus:
    move    r6,r16
    jump    (r7)
    nop
    
            
; [SPU Command: Set SFX Volume] ---------------------------
; \---------------------------/
spu_SetSFXVolume:
    move    r6,r20
    jump    (r7)
    nop
    
   
; [SPU Command: Get Free Voice] ---------------------------
; | r6 = *voice data           \__
; | r5 =  0   . search free voice |
; |      !=0 . target voice       |
; \-------------------------------/
spu_SetVoice:        
    cmpq    #0,r5                           ; search a free voice or is a defined??
    jr      EQ,.spu_SearchFreeVoice         ;
    nop                                     ;
    move    r5,r3                           ; r3=r5=voice pointer
    movei   #spu_VoiceFound,r1              ; get the set voice function
    jump    (r1)                            ; and jump
    nop                                     ; - -- - -- - -- - - - - -- - - - ----

.spu_SearchFreeVoice:
    move    r13,r1                          ; get # of voices processed by DSP
    movei   #Voice_SIZEOF,r26               ; size of Voice in bytes
    movei   #dsp_ShadowVoices,r3            ; get DSP Shadow voices
    movei   #v_flags,r24                    ; offset
                                            ;
.spu_SearchVoice:
    move    r3,r25                          ; copy voice pointer
    add     r24,r25                         ; add offset to parameter v_flags
    load    (r25),r4                        ; get flags
    or      r4,r4                           ;
    btst    #VOICEB_SYNC,r4                 ; is this voice synced
    jr      NE,.spu_VoiceLocked             ;
    nop                                     ;
    load    (r3),r4                         ; get *SAMPLE
    or      r4,r4
    cmpq    #0,r4                           ; is sample * == 0
    jr      EQ,spu_VoiceFound               ; == -> free voice found
    nop

.spu_VoiceLocked:
    add     r26,r3                          ; next voice
    subq    #1,r1                           ; counter --
    cmpq    #0,r1                           ; test again
    jr      NE,.spu_SearchVoice             ;
    nop                                     ;
    subqt   #4,r2                           ; --parameter
    store   r0,(r2)                         ; store ZERO = not found
    jump    (r7)                            ; return form ISR
    nop                                     ;
       
spu_VoiceFound:    
    subqt   #4,r2                           ; parameter -1
    store   r3,(r2)                         ; store pointer of free voice
    moveq   #Voice_SIZEOF/4,r1              ; size of voice in Longs
    subqt   #4,r3                           ; predecrement voice pointer
        
.spu_SetVoiceLoop:
    load    (r6),r4                         ; get user voice long word
    addqt   #4,r3                           ; update pointer dsp voice
    addqt   #4,r6                           ; update pointer user voice
    or      r4,r4
    store   r4,(r3)                         ; store in dsp voice
    subq    #1,r1                           ; counter --
    cmpq    #0,r1
    jr      NE,.spu_SetVoiceLoop            ; set next long
    nop
    jump    (r7)
    nop
    


; [SPU Command: Set Voice Parameter] ----------------------
; \--------------------------------/
spu_SetVoiceParameter:
    store   r5,(r6)                         ; store voice parameter
    jump    (r7)                            ; return to irq serviced routine
    nop
    
    
    
; [SPU Command: Set Voice Number] -------------------------
; \-----------------------------/
spu_SetVoiceNumber:
    move    r6,r13                          ; set voice number
    jump    (r7)                            ; processed by the DSP
    nop                                     ;

    
; [SPU Command: Set SamplesPerFrame] ----------------------
; \--------------------------------/
spu_SetSamplesPerFrame:
    move    r6,r21                          ; set samples per frame
    jump    (r7)                            ; (MixFreq/RefreshRate)
    nop
    

; [SPU Command: SyncVoices] ----------------------
; \-----------------------/
spu_SyncVoices:
    move    r6,r23                          ; copy the pointer of the user voices
    jump    (r7)                            ; to indicate that sync voices can be
    nop                                     ; initialized. After doing this code,
                                            ; the user voice will be taken, by the
                                            ; next TIMER 1 interrupt (r23= user Voices)
    
; [SPU Command: STOP DSP] -------------------------------
; \---------------------/
spu_StopDSP:                                ; stop
    movei   #DSP_BASE+D_CTRL,r1             ; the
    load    (r1),r2                         ; DSP
    or      r2,r2                           ;
    bclr    #0,r2                           ;
    store   r2,(r1)                         ; store back
    nop                                     ; bye bye
    nop                                     ; bye bye
    nop                                     ; bye bye
    nop                                     ; bye bye



; [SPU Command: ADD Task] -------------------------------
; \---------------------/
spu_AddTask:
    ; platz suchen für routine
    ; parameter 1 dataen aus addresse copieren nach freien DSP speicher
    ; mit länge parameter 2
    
    jump    (r7)                            ; processed by the DSP
    nop                                     ;




; [SPU Command: Flush Task] -------------------------------
; \-----------------------/
spu_FlushTasks:    

    ;*** Reset Freestore to the End of the DSP Core ***

    laddr   dsp_SPUEnd,r1           ; get free memory
    laddr   dsp_FreeStore,r2        ; free store keeper
    store   r1,(r2)                 ; store the 1st free memory address


    ;*** Clear dsp_Task array from behind dsp_MixBuffer on ***

    moveq   #MAX_DSP_TASKS-1,r2
    laddr   dsp_Tasks,r3
    
.spu_FlushTasksLoop:
    addqt   #4,r3
    store   ZERO,(r3)
    subq    #1,r2
    jr      NE,.spu_FlushTasksLoop
    nop
    
    jump    (r7)
    nop
    
    
    
; [SPU Command: Execute Task] ---------------------------
; \-------------------------/
spu_ExecuteTask:
    movei   #dsp_EventMask,r1               ; get Event Mask holder
    load    (r1),r2                         ; get event mask
    or      r2,r2                           ; make sure events are loaded
    or      r6,r2                           ; register task to do
    store   r2,(r1)                         ; update Event Mask
    jump    (r7)                            ; and go out
    nop
    

    
; * ---------------------------------------------------------------------------
; * Data Region
; * --- - - - -- -- -- :-

        .long
dsp_EventMask       .equ    *
dsp_Tasks           .equ    *+4
spu_Command         .equ    *+4+(MAX_DSP_TASKS*4)
dsp_ShadowVoices    .equ    *+4+(MAX_DSP_TASKS*4)+12
dsp_SampleBufferA   .equ    *+4+(MAX_DSP_TASKS*4)+12+(MAX_VOICES*Voice_SIZEOF)
dsp_SampleBufferB   .equ    *+4+(MAX_DSP_TASKS*4)+12+(MAX_VOICES*Voice_SIZEOF)+(512*4)
dsp_FreeStore       .equ    *+4+(MAX_DSP_TASKS*4)+12+(MAX_VOICES*Voice_SIZEOF*2)+(2*512*4)

; *** Here is the memory where the first user task will be uploaded **
dsp_SPUEnd          .equ    (*+4+(MAX_DSP_TASKS*4)+12+(MAX_VOICES*Voice_SIZEOF*2)+(2*512*4)+4+7)&-8



; [SPU DSP CORE END] ----------------------------------------------------------


SPUCommandsTable    .equ    1

    .print  "DSP core codesize: ",/d/l *-spu_DSPCore


spuc_SetCDDAVolume      equ     (spu_SetCDDAVolume-dsp_SPU)>>1
spuc_SetCDDADelay       equ     (spu_SetCDDADelay-dsp_SPU)>>1
spuc_SetCDDAStatus      equ     (spu_SetCDDAStatus-dsp_SPU)>>1
spuc_SetSFXVolume       equ     (spu_SetSFXVolume-dsp_SPU)>>1
spuc_SetVoice           equ     (spu_SetVoice-dsp_SPU)>>1
spuc_SetVoiceParameter  equ     (spu_SetVoiceParameter-dsp_SPU)>>1
spuc_SetVoiceNumber     equ     (spu_SetVoiceNumber-dsp_SPU)>>1
spuc_SetSamplesPerFrame equ     (spu_SetSamplesPerFrame-dsp_SPU)>>1
spuc_SyncVoices         equ     (spu_SyncVoices-dsp_SPU)>>1
spuc_StopDSP            equ     (spu_StopDSP-dsp_SPU)>>1
spuc_AddTask            equ     (spu_AddTask-dsp_SPU)>>1
spuc_FlushTasks         equ     (spu_FlushTasks-dsp_SPU)>>1
spuc_ExecuteTask        equ     (spu_ExecuteTask-dsp_SPU)>>1


    .print  "SPU: SetCDDAVolume     = ",/x/l spuc_SetCDDAVolume<<1
    .print  "SPU: SetCDDADelay      = ",/x/l spuc_SetCDDADelay<<1
    .print  "SPU: SetCDDAStatus     = ",/x/l spuc_SetCDDAStatus<<1
    .print  "SPU: SetSFXVolume      = ",/x/l spuc_SetSFXVolume<<1
    .print  "SPU: SetVoice          = ",/x/l spuc_SetVoice<<1
    .print  "SPU: SetVoiceParameter = ",/x/l spuc_SetVoiceParameter<<1
    .print  "SPU: SetVoiceNumber    = ",/x/l spuc_SetVoiceNumber<<1
    .print  "SPU: SetSamplesPerFrame= ",/x/l spuc_SetSamplesPerFrame<<1    
    .print  "SPU: SyncVoices        = ",/x/l spuc_SyncVoices<<1
    .print  "SPU: StopDSP           = ",/x/l spuc_StopDSP<<1
    .print  "SPU: AddTask           = ",/x/l spuc_AddTask<<1
    .print  "SPU: FlushTasks        = ",/x/l spuc_FlushTasks<<1
    .print  "SPU: ExecuteTask       = ",/x/l spuc_ExecuteTask<<1
        
    .print  "TOTAL SPU SIZE:        = ",/d/l dsp_SPUEnd-DSP_RAM
    .print  "DSP local mem free:    = ",/d/l 8192-(dsp_SPUEnd-DSP_RAM)

    .end
    