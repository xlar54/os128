NDX = $D0
KEYBUF = $034A


keyboard_getkey:
    .block
    lda NDX
    cmp #$00
    beq keyboard_getkey
    lda KEYBUF
    sta lastkey
    lda #$00
    sta NDX
    lda lastkey
    rts

lastkey:
    .byte $00
    .bend