;****************************************************************************
; c128 keyboard                                                             *
;****************************************************************************

KEYNDX                      = $D0       ; Number of keys waiting in keyboard buffer
KEYBUF                      = $034A
FORTY_EIGHTY_KEY            = $D505     ; BIT 7 : 0 = down, 1 = up

keyboard_getkey:
    .block
    lda KEYNDX
    cmp #$00
    bne getit
    jmp keyboard_getkey
getit:
    lda KEYBUF
    sta lastkey
    lda #$00
    sta KEYNDX
    lda lastkey
    rts

lastkey:
    .byte $00
    .bend