; ===================================================================
; OS128
;
; ===================================================================

.include "kernal.asm"
.include "c128.asm"
.include "mmu.asm"


#BasicStart128 main

main:

    #FastMode
    #HiresOn
    #SetColors VDC_COLOR_BLACK, VDC_COLOR_LGREY 
    #MoveTo 0,0
    #DrawTo 639, 0
    #DrawTo 639, 199
    #DrawTo 0, 199
    #DrawTo 0, 0

    #Box 10,10,200,50

    #SetPixel 120,120

    #PutString 500,25,stuff
    jsr keyboard_getkey

    ;#SetCursorMode VDC_CURSOR_MODE_NON_BLINK
    ;#SetInterlaceOn
    ;lda #$49
    ;jsr $ffd2

    #Sid_Init
    #Sid_SetVolume 15

    lda #12
    sta SID_BASE + SID_V1_ATTACK_DECAY

    lda #4
    sta SID_BASE + SID_V1_SUSTAIN_RELEASE

    #Sid_PlayNote 1, 4, SID_C, SID_WAVEFORM_TRIANGLE    ; voice 1, octave 4, note C, triangle waveform

    jsr keyboard_getkey
    #Sid_PlayNote 1, 4, SID_D, SID_WAVEFORM_TRIANGLE

    jsr keyboard_getkey
    #Sid_PlayNote 1, 4, SID_E, SID_WAVEFORM_TRIANGLE

    jsr keyboard_getkey
    #Sid_PlayNote 1, 4, SID_F, SID_WAVEFORM_TRIANGLE

    jsr keyboard_getkey
    #Sid_PlayNote 1, 4, SID_G, SID_WAVEFORM_TRIANGLE

    jsr keyboard_getkey
    #Sid_PlayNote 1, 4, SID_A, SID_WAVEFORM_TRIANGLE

    jsr keyboard_getkey
    #Sid_PlayNote 1, 4, SID_B, SID_WAVEFORM_TRIANGLE

    jsr keyboard_getkey
    #Sid_PlayNote 1, 5, SID_C, SID_WAVEFORM_TRIANGLE

    jsr keyboard_getkey
    #Sid_SetVolume 0

    #SetColors VDC_COLOR_LGREY, VDC_COLOR_BLACK  
    #HiresOff

    rts

stuff   .text "Press keys to play music"
        .byte $00

.include "sid.asm"
.include "vic-ii.asm"
.include "vdc_gfx.asm"
.include "keyboard.asm"