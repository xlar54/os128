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

    #SetColors VDC_COLOR_LGREY, VDC_COLOR_BLACK  
    #HiresOff

    #SetCursorMode VDC_CURSOR_MODE_NON_BLINK
    #SetInterlaceOn
    lda #$49
    jsr $ffd2
    jsr keyboard_getkey
    rts

stuff   .text "This is a test"
        .byte $00


.include "vic-ii.asm"
.include "vdc_gfx.asm"
.include "keyboard.asm"