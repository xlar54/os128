;****************************************************************************
; "HIRES80.BIN" hires line plotting package for the Commodore 128 80-col   *
;*  screen.                                                                  *
;*                                                                           *
;*  This package contains a couple of irregularities that I discovered       *
;*  while I was commenting it.  I left them in rather than start all over,   *
;*  since this package was written with a monitor rather than an assembler.  *
;*****************************************************************************
; http://www.ffd2.com/fridge/chacking/c=hacking1.txt
;*****************************************************************************
;*  package entry points                                                     *
;*****************************************************************************

VDC_ADDR_REG        = $D600
VDC_DATA_REG        = $D601

VDC_HZ_TTL          = $00;    total # chars per line, including beam return 
VDC_HZ_DSP          = $01;    actual chars per line
VDC_HZ_SYNC_POS     = $02;    if reduced, left border moves right.  if increased left border moves left
VDC_SYNC_WIDTH      = $03
VDC_VERT_TTL        = $04;    total lines, including beam return
VDC_VERT_TTL_ADJ    = $05;
VDC_VERT_DSP        = $06;    total lines displayed
VDC_VERT_SYNC_POS   = $07;    if increased, the screen moves up.the screen moves down when decreased
VDC_INTLC_MOD       = $08;    interlace modes
VDC_CHAR_TTL_VERT   = $09
VDC_CRSR_MODE       = $0A
VDC_CRSR_END        = $0B
VDC_DSP_HI          = $0C
VDC_DSP_LO          = $0D
VDC_CRSR_POS_HI     = $0E
VDC_CRSR_POS_LO     = $0F
VDC_LGHT_PEN_VERT   = $10
VDC_LGHT_PEN_HZ     = $11
VDC_DATA_HI         = $12
VDC_DATA_LO         = $13
VDC_ATTRIB_HI       = $14
VDC_ATTRIB_LO       = $15
VDC_CHAR_TTL_DSP    = $16
VDC_CHAR_TTL        = $17
VDC_VSCROLL         = $18
VDC_HSCROLL         = $19
VDC_COLORS          = $1A
VDC_ADDR_INC_RW     = $1B
VDC_CSET            = $1C
VDC_UNDRLN_SCAN     = $1D
VDC_COUNT           = $1E
VDC_DATA            = $1F
VDC_BLK_START_HI    = $20
VDC_BLK_START_LO    = $21
VDC_DSP_ENBL_BEGN   = $22
VDC_DSP_ENBL_END    = $23
VDC_DRAM_REFRESH    = $24


VDC_COLOR_BLACK     = $00
VDC_COLOR_GREY      = $01
VDC_COLOR_BLUE      = $02
VDC_COLOR_LBLUE     = $03
VDC_COLOR_GREEN     = $04
VDC_COLOR_LGREEN    = $05
VDC_COLOR_CYAN      = $06
VDC_COLOR_LCYAN     = $07
VDC_COLOR_RED       = $08
VDC_COLOR_LRED      = $09
VDC_COLOR_PURPLE    = $0A
VDC_COLOR_LPURPLE   = $0B
VDC_COLOR_ORANGE    = $0C
VDC_COLOR_YELLOW    = $0D
VDC_COLOR_LGREY     = $0E
VDC_COLOR_WHITE     = $0F

setbank   = $ff00
init80    = $ce0c

HiresOn .macro
    jsr gohires
.endm

HiresOff .macro
    jsr endhires
.endm

SetPixel .macro x, y
    ldy #\y
xl  .var (\x & $ff)
xh  .var ((\x >> 8) & $ff)
    lda #xl
    ldx #xh
    jsr plotpixel
.endm

DrawTo .macro x,y 
    ldy #\y
xl  .var (\x & $ff)
xh  .var ((\x >> 8) & $ff)
    lda #xl
    ldx #xh
    sec
    jsr drawlin
.endm

MoveTo .macro x,y 
    ldy #\y
xl  .var (\x & $ff)
xh  .var ((\x >> 8) & $ff)
    lda #xl
    ldx #xh
    clc
    jsr drawlin
.endm

Box .macro x1, y1, x2, y2
    #MoveTo \x1,\y1
    #DrawTo \x2,\y1
    #DrawTo \x2,\y2
    #DrawTo \x1,\y2
    #DrawTo \x1,\y1
.endm

SetColors .macro fg, bg
    lda #$00      ;switch to bank 15 (kernal bank)
    sta setbank
col .var (\fg * 16 + \bg)
    lda #col
    ldx #$1a
    jsr writereg  ; ROM routine to write VDC register
.endm

PutString .macro x,y, txtPtr
    lda #\y
    sta vdc_y
xl  .var (\x & $ff)
xh  .var ((\x >> 8) & $ff)
    lda #xl
    sta vdc_xlo
    lda #xh
    sta vdc_xhi
    ldy #$00
next:
    tya
    pha
    lda \txtPtr, y
    cmp #$00
    beq done
    ldx vdc_xlo
    ldy vdc_y
    jsr putchar
    pla
    tay
    iny
    lda vdc_xlo
    clc
    adc fwidth
    adc #$01
    sta vdc_xlo
    jmp next
done:
    pla
.endm

;*=$1300

;jmp drawlin   ;jmp to draw line/position pixel cursor
;jmp gohires   ;jmp to enter hires mode
;jmp endhires  ;jmp to exit hires mode
;jmp rsrvd     ;reserved
;jmp rsrvd     ;reserved

;!byte $b3     ;library loaded identifier
;rsrvd:
;rts

;*****************************************************************************
;*  write VDC register
;*****************************************************************************
writereg:
        .block
        stx VDC_ADDR_REG
wait:   bit VDC_ADDR_REG
        bpl wait
        sta VDC_DATA_REG
        rts
        .bend

;*****************************************************************************
;*  write VDC memory
;*****************************************************************************
write80:
        .block
        ldx #VDC_DATA
        stx VDC_ADDR_REG
wait:   bit VDC_ADDR_REG
        bpl wait
        sta VDC_DATA_REG
        rts
        .bend

;*****************************************************************************
;*  read VDC memory
;*****************************************************************************
read80:
        .block
        ldx #VDC_DATA
        stx VDC_ADDR_REG
wait:   bit VDC_ADDR_REG
        bpl wait
        lda VDC_DATA_REG
        rts
        .bend

;*****************************************************************************
;*  enter hires mode                                                         *
;*****************************************************************************
gohires:
        lda #$00      ;switch to bank 15 (kernal bank)
        sta setbank
        lda #$e0      ;set color to light grey on black
        ldx #$1a
        jsr writereg  ; ROM routine to write VDC register
        lda #$87      ;enter bitmap mode (note - for version 2 VDC)
        ldx #$19
        jsr writereg
        lda #$00      ;set VDC RAM address high
        ldx #$12
        jsr writereg
        inx           ;set VDC RAM address low
        jsr writereg
        lda #$00      ;set VDC RAM data register to $00
        jsr write80
        lda #$20      ;select block fill mode
        ldx #$18
        jsr writereg
        lda #$00      ;fill 256*63 VDC bytes with 0
        ldx #$1e      ;  to clear the hires screen
        ldy #$3f
loop1:
        jsr writereg
        dey
        bne loop1
        rts

;*****************************************************************************
;*  exit hires mode                                                          *
;*****************************************************************************
endhires:
        jsr init80     ;reload the character sets
        lda #$93      ;clear the text screen
        jsr $ffd2
        ldx #$19      ;restore color text mode
        lda #$47
        jmp writereg

;*****************************************************************************
;*calculate the bitmap byte address and bit value for pixel given x=.AX,y=.Y *
;*****************************************************************************
calcaddr:
        sty $fa       ;save .A and .Y
        sta $fc
        tya           ;put pixel cursor y position into .A
        ldy #$00      ;clear pixel cursor y position high byte
        sty $fb
        asl a         ;multiply pixel cursor y by 2 giving y*2
        rol $fb       ;  and we must shift the high byte to
        asl a         ;again, giving y*4
        rol $fb
        clc           ;add the original y, giving y*5
        adc $fa
        bcc skip1
        inc $fb
skip1:
        asl a         ;multiply by 2 again, giving y*10
        rol $fb
        asl a         ;again, giving y*20
        rol $fb
        asl a         ;again, giving y*40
        rol $fb
        asl a         ;again, giving y*80: ha!  we are done
        rol $fb
        sta $fa       ;save low byte of y*80
        lda $fc       ;restore x coordinate low byte
        stx $fd       ;set up x coordinate high byte
        lsr $fd       ;divide the x coordinate by 2 giving x/2
        ror a         ;  we must ror the high byte, then the low
        lsr $fd       ;again, giving x/4
        ror a
        lsr $fd       ;again, giving x/8: done
        ror a
        clc           ;now add y*80 and x/8
        adc $fa
        sta $fa
        lda $fd
        adc $fb
        sta $fb       ;giving us the pixel byte address in ($fa)
        lda $fc       ;get x mod 8
        and #$07      ;  ha! we can just extract the low three bits
        tax
        lda bittbl,x   ;look up the bit value in the table
        sta $fc       ;  and save it at $fc
        rts           ;exit with address in ($fa) and value in $fc

;*****************************************************************************
;*  bit value table                                                          *
;*****************************************************************************
bittbl:
  .byte $80,$40,$20,$10        ;bit values stored left to right
  .byte $08,$04,$02,$01

        brk           ;filler - I forget why I put it here
        brk

;*****************************************************************************
;*  plot pixel at x=.AX, y=.Y on bitmap screen                               *
;*****************************************************************************
plotpixel:
        ;ldy y_coord
        ;lda x_coord
        ;ldx x_coord + 1
        jsr calcaddr  ;calculate the pixel address and value
        lda $fb       ;set VDC RAM address high to pixel address
        ldx #$12
        jsr writereg
        lda $fa       ;set VDC RAM address low to pixel address
        inx
        jsr writereg
        jsr read80    ;peek the VDC RAM address
        ora $fc       ;OR on the new pixel value
        tay           ;  and save the result (byte to poke back)
        lda $fb       ;reset the VDC RAM address to the pixel
        ldx #$12      ;  address; this is necessary since the
        jsr writereg  ;  VDC will increment its RAM address on
        lda $fa       ;  every access
        inx
        jsr writereg
        tya
        jmp write80    ;and poke the new pixel byte value

;*****************************************************************************
;*  perform the unsigned 32-bit divide with 16-bit denominator (bottom)      *
;*    [$63 $62 $61 $60] is the numerator (top)                               *
;*            [$51 $50] is the denominator (bottom)                          *
;*    [$67 $66 $65 $64] is the quotient (result)                             *
;*        [$54 $53 $52] is the remainder                                     *
;*****************************************************************************
divide:
        lda #$00      ;set the result to 0
        sta $64
        sta $65
        sta $66
        sta $67

        sta $52       ;clear the remainder
        sta $53
        sta $54

        ldx #$20      ;set the loop count to 32 bits
back1:
        asl $60       ;shift out the high bit of the numerator
        rol $61
        rol $62
        rol $63
        rol $52       ;shift it into the remainder
        rol $53
        rol $54
        lda $54       ;check if the remainder is >= the denominator
        bne skip2
        lda $52
        cmp $50
        lda $53
        sbc $51
        bcc skip3     ;if not, go to next bit
skip2:
        sec           ;subract the denominator from the remainder
        lda $52
        sbc $50
        sta $52
        lda $53
        sbc $51
        sta $53
        bcs skip3
        dec $54
skip3:
        rol $64       ;shift a "1" bit into the quotient.  Note
        rol $65       ;  the first "rol" should have been preceeded
        rol $66       ;  by a "sec"; this is a BUG!  However, it
        rol $67       ;  will fail only if denom >=32768 which
                        ;  cannot happen in this application.
        dex           ;go on to the next bit
        bne back1
        rts

;*****************************************************************************
;*  get the absolute value of the 2's comp number in .AY -> .AY              *
;*****************************************************************************
abs2comp:
        sty $50
        ldx $50
        bpl skip5     ;if the number is positive, exit
        sec           ;else take the 2's complement of the negative
        sta $50       ;  value to get the positive value
        lda #$00
        sbc $50
        pha
        sty $50
        lda #$00
        sbc $50
        tay
        pla
skip5:
        rts

;*****************************************************************************
;*  perform the fractional signed 32-bit divide                              *
;*****************************************************************************
fracdiv:
        pha           ;remember the sign of the result
        lda #$00      ;set the numerator fractional portion to .0
        sta $60
        sta $61
        jsr divide    ;32-bit divide
        pla           ;if the sign of the result is supposed to be
        bpl skip6     ;  positive, then exit
        sec           ;if the sign of the result is negative, take
        lda #$00      ;  get the 2's complement of the positive
        sbc $64       ;  result
        sta $64
        lda #$00
        sbc $65
        sta $65
        lda #$00
        sbc $66
        sta $66
        lda #$00
        sbc $67
        sta $67
skip6:
        rts

;*****************************************************************************
;*  get the X and Y plotting increments and the pixels-to-plot count         *
;*****************************************************************************
calcxyinc
        lda $0c       ;get ABS(DX)
        ldy $0d
        jsr abs2comp
        sta $fa
        sty $fb

        lda $10       ;get ABS(DY)
        ldy $11
        jsr abs2comp
        sta $fc
        sty $fd

        lda $fc       ;compare ABS(DY) to ABS(DX)
        cmp $fa
        lda $fd
        sbc $fb
        bcs skip7     ;if ABS(DY) >= ABS(DX) then branch ahead

        lda $fa       ;set pixels-to-plot count to ABS(DX)
        sta $12
        sta $50
        lda $fb
        sta $13
        sta $51       ;set the numerator (top) to DY and the
        lda $fc       ;  denominator (bottom) to ABS(DX)
        sta $62
        lda $fd
        sta $63
        lda $11       ;get the sign of DY - will be the sign of div
        jsr fracdiv     ;perform the signed fractional division
        ldx #$03      ;store the result in the Y increment value
skp     lda $64,x
        sta $0e,x
        dex
        bpl skp
        lda $0d       ;get the X increment
        bmi shp
        lda #$00      ;if DX is positive, X inc is +1
        sta $0d       ;  (note that DX cannot be 0 here so we don't
        lda #$01      ;   have to worry about that case)
        sta $0c
        bne skip8
shp:    lda #$ff      ;if DX is negative, X inc is -1
        sta $0d
        sta $0c
skip8:
        sec           ;take the negative of the number of pixels
        lda #$00      ;  to plot and exit
        sbc $12       ;I don't remember exactly why I use the
        sta $12       ;  negative; there is not much of a speed
        lda #$00      ;  improvement.  Oh well, t'is done.
        sbc $13
        sta $13
        rts
skip7:
        lda $fc       ;set the pixels-to-plot count to ABS(DY)
        sta $12
        sta $50
        lda $fd
        sta $13
        sta $51       ;set the numerator (top) to DX and the
        lda $fa       ;  denominator(bottom) to ABS(DY)
        sta $62
        lda $fb
        sta $63
        lda $0d       ;get the sign of DX - will be the sign of div
        jsr fracdiv     ;do the 32-bit signed fractional division
        ldx #$03      ;store the result in the X increment
skip9:  
        lda $64,x
        sta $0a,x
        dex
        bpl skip9
        jmp jmpahead  ;jump over the next section
        ;-------
        bit $ffff     ;This section contained junk before and I
        bit $ffff     ;  don't know how it got here.  I replaced
        bit $ffff     ;  it with BITs and now jump over it.
        ;-------
jmpahead
        lda $11
        bmi msh
        lda #$00      ;if DY is positive then Y inc is +1
        sta $11       ;  (we do not have to worry about the case
        lda #$01      ;   of DY being zero since then the increment
        sta $10       ;   would not be important)
        bne juts
msh:    lda #$ff      ;if DY is negative then Y inc is -1
        sta $11
        sta $10
juts:   jmp skip8     ;jump back to the exit

;*****************************************************************************
;*  main routine: draw line or set pixel cursor position                     *
;*****************************************************************************
drawlin:
        bcs hyts     ;goto draw routine if .C=1
back:   sta $8b       ;store x and y pixel cursor coordinates
        stx $8c
        sty $8d
        rts           ;exit set pixel cursor

hyts:   sta $04       ;save draw-to coordinates
        stx $05
        sty $08
        ldx #$07      ;clear $0a-$0d and $0e-$11
        lda #$00
sup:    sta $0a,x
        dex
        bpl sup

        sec           ;get dx value = DrawToX - PixelCursorX
        lda $04       ;  dx is at [$0d $0c . $0b $0a]
        sbc $8b
        sta $0c
        lda $05
        sbc $8c
        sta $0d

        sec           ;get dy value = DrawToY - PixelCursorY
        lda $08       ;  dy is at [$11 $10 . $0f $0e]
        sbc $8d
        sta $10
        lda #$00
        bcs tfdr
        lda #$ff
tfdr:   sta $11

        jsr calcxyinc ;calculate the X and Y plotting increments

        lda $8b       ;set the drawing X position to x+0.5
        sta $04       ;  X is at [$05 $04 . $03 $02]
        lda $8c
        sta $05
        lda #$80
        sta $03
        sta $07
        lda #$00
        sta $02
        sta $06
        lda $8d       ;set the drawing Y position to y+0.5
        sta $08       ;  Y is at [$09 $08 . $07 $06]
        lda #$00
        sta $09

ftrs:   lda $04       ;get the pixel X and Y coordinates
        ldx $05
        ldy $08
        jsr plotpixel ;plot the pixel
        lda $12       ;check the pixels-to-plot count for zero
        ora $13
        beq bye         ;if no more pixels to plot, exit loop
        clc           ;add the X increment to the X coordinate
        lda $02
        adc $0a
        sta $02
        lda $03
        adc $0b
        sta $03
        lda $04
        adc $0c
        sta $04
        lda $05
        adc $0d
        sta $05
        clc           ;add the Y increment to the Y coordinate
        lda $06
        adc $0e
        sta $06
        lda $07
        adc $0f
        sta $07
        lda $08
        adc $10
        sta $08
        lda $09
        adc $11
        sta $09
        inc $12       ;increment the pixels to plot count
        bne ftrs     ;  note that it is stored as the negative of
        inc $13       ;  the count
        jmp ftrs     ;repeat plotting loop

bye:    lda $04       ;exit - set the pixel cursor position to the
        ldx $05       ;  last pixel plotted on the line
        ldy $08
        jmp back


putchar:
    stx tmpxl       ; put the x location in two temp locations
    stx otmpxl
    sty tmpy        ; put the y location in two temp locations
    sty otmpy
    
    cmp #$80        ; fix uppercase to align with font data table (if > 128, subtract 96)
    blt fskip1      ; so ASCII 'A' (193) becomes 'A' (97)
    sec
    sbc #$60
fskip1:
    sec
    sbc #$20        ; subtract 32 from the character to normalize against the font data
    sta fchar       ; store the character

    jsr fcalcchar   ; calculate the base address of the character to display
                    ; fcharlo and fcharhi will contain the base address of the font data for char

    ; get the first three font bytes and store them as counters

    ldy #$00        ; get the value of the 1st font byte
    lda (fcharlo),y
    sta fwidth
    sta ftwidth     ; store it in the width counter
                    ; get the height byte and store as a counter
    clc             
    lda fcharlo     ; get lo byte address
    adc #$01        ; add one
    sta fcharlo     ; store it back
    bcc fcont1      ; if carry..
    inc fcharhi     ; increase hi byte
fcont1:
    ldy #$00        ; get the value of the 2nd font byte
    lda (fcharlo),y
    sta ftheight    ; store it in the height counter

                    ; get the base byte and store as a counter
    clc
    lda fcharlo     ; get lo byte address
    adc #$01        ; add one
    sta fcharlo     ; store it back       
    bcc fcont2      ; if carry..
    inc fcharhi     ; increase hi byte
fcont2:
    ldy #$00        ; get the value of the 3rd font byte
    lda (fcharlo),y
    sta fbase       ; store it in the base counter

    ; fcharlo / fcharhi are now pointing to the 3rd byte

fsettop:
    lda #$07        ; move the y plotting position down
    sec             ; based on the 2nd (height) byte
    sbc ftheight
    clc
    adc tmpy
    sta tmpy

faddbase:           ; move the y plotting position down
    clc             ; based on 3rd (baseline) byte
    lda tmpy
    adc fbase
    sta tmpy

ftestbytes:
    jsr ftestbits
fnextbyte:
    dec ftheight    ; last row byte?
    lda ftheight
    cmp #$ff
    beq fdone
    inc tmpy        ; move down to next scanline
    lda fwidth      ; reset the width counter
    sta ftwidth
    jmp ftestbytes
fdone:
    rts

; tests the bits of a byte and plot pixels based on the data for a single row
ftestbits:   
    ldy #$00        ; get the font byte data for this row
    lda (fcharlo),y
    and fbit        ; test if bit x (fbit) is set
    cmp fbit
    bne fnext       ; if not, skip ahead
    ldy tmpy        ; if so plot the pixel
    lda tmpxl 
    ldx tmpxh
    jsr plotpixel
fnext:
    inc tmpxl       ; increase the x pos of the pixel location (on or off)
    lda fbit        ; check if we have tested the final bit of the byte (row)
    cmp #$01
    beq freset      ; if so, reset our variables to prepare for the next row data
    lsr fbit        ; if not, shift the bits to the right to prepare for testing the next bit
    jmp ftestbits   ; go and test the next bit
freset:
    lda #$80        ; reset our bit comparer - set bit 7
    sta fbit
    lda otmpxl      ; reset the X pixel plotting vakue for next row
    sta tmpxl
    inc fcharlo     ; move to next row byte
    rts             ; return back

; calculate the address of the character to draw
fcalcchar:
    lda #<font      ; store base address of font data
    sta fcharlo
    lda #>font
    sta fcharhi
    lda fchar       ; get the normalized character
    sta fctr        ; copy to a count-down counter
fok: 
    lda fctr        ; load the count-down counter
    cmp #$00        ; < 0?
    beq fcalcdone   ; if so we are done
fcalcagain:
    clc             ; we arent done yet
    lda fcharlo     ; get the low byte address
    adc #$0b        ; add 11 to it (skip 11 bytes)
    sta fcharlo     ; store it back
    bcc fincctr     ; if carry is set..
    inc fcharhi     ; increase the hi byte
fincctr:
    dec fctr        ; decrease the count-down counter
    jmp fok         ; loop
fcalcdone:
    rts             ; address for character info is in fcharlo / fcharhi
    

fcharlo = $26
fcharhi = $27

vdc_xlo     .byte $00
vdc_xhi     .byte $00
vdc_y       .byte $00

fbit:       .byte $80
otmpxl:     .byte $00
otmpxh:     .byte $00
otmpy:      .byte $00
tmpxl:      .byte $00
tmpxh:      .byte $00
tmpy:       .byte $00
ftwidth:    .byte $00
fwidth:     .byte $00
ftheight:   .byte $00
fbase:      .byte $00
fchar:      .byte $00
fctr:       .byte $00


font:
.byte $04,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00; //space
.byte $01,$07,$00,$80,$80,$80,$80,$00,$80,$80,$00; // !
.byte $03,$02,$00,$A0,$A0,$00,$00,$00,$00,$00,$00; // "
.byte $05,$07,$00,$50,$50,$F8,$50,$F8,$50,$50,$00; // #
.byte $05,$08,$00,$78,$A0,$60,$20,$30,$28,$F0,$20; // $
.byte $05,$07,$00,$C0,$C8,$10,$20,$40,$98,$18,$00; // %
.byte $05,$07,$00,$60,$80,$90,$78,$90,$90,$70,$00; // &
.byte $02,$07,$00,$40,$40,$80,$00,$00,$00,$00,$00; // '
.byte $03,$08,$01,$20,$40,$80,$80,$80,$80,$40,$20; // (
.byte $03,$08,$01,$80,$40,$20,$20,$20,$20,$40,$80; // )
.byte $05,$07,$00,$20,$A8,$70,$F8,$70,$A8,$20,$00; // *
.byte $05,$07,$00,$00,$20,$20,$F8,$20,$20,$00,$00; // +
.byte $02,$02,$01,$40,$80,$00,$00,$00,$00,$00,$00; // ,
.byte $04,$07,$00,$00,$00,$00,$F0,$00,$00,$00,$00; // -
.byte $01,$01,$00,$80,$00,$00,$00,$00,$00,$00,$00; // .
.byte $06,$07,$00,$00,$04,$08,$10,$20,$40,$80,$00; // /
.byte $04,$07,$00,$60,$90,$B0,$D0,$90,$90,$60,$00; // 0
.byte $02,$07,$00,$40,$C0,$40,$40,$40,$40,$40,$00; // 1
.byte $03,$07,$00,$C0,$20,$20,$40,$80,$80,$E0,$00; // 2
.byte $03,$07,$00,$C0,$20,$20,$40,$20,$20,$C0,$00; // 3
.byte $04,$07,$00,$10,$30,$50,$90,$F0,$10,$10,$00; // 4
.byte $03,$07,$00,$E0,$80,$C0,$20,$20,$20,$C0,$00; // 5
.byte $04,$07,$00,$20,$40,$80,$E0,$90,$90,$60,$00; // 6
.byte $04,$07,$00,$F0,$10,$20,$40,$40,$40,$40,$00; // 7
.byte $04,$07,$00,$60,$90,$90,$60,$90,$90,$60,$00; // 8
.byte $04,$07,$00,$60,$90,$90,$70,$10,$20,$40,$00; // 9
.byte $01,$04,$00,$80,$00,$00,$80,$00,$00,$00,$00; // :
.byte $02,$05,$01,$40,$00,$00,$40,$80,$00,$00,$00; // ;
.byte $03,$06,$00,$00,$20,$40,$80,$40,$20,$00,$00; // <
.byte $04,$06,$00,$00,$F0,$00,$F0,$00,$00,$00,$00; // =
.byte $03,$06,$00,$00,$80,$40,$20,$40,$80,$00,$00; // >
.byte $05,$07,$00,$70,$88,$10,$20,$20,$00,$20,$00; // ?
.byte $05,$07,$00,$70,$88,$B8,$A8,$B8,$80,$70,$00; // @
.byte $04,$05,$00,$70,$90,$90,$90,$50,$00,$00,$00; // a
.byte $04,$07,$00,$80,$80,$E0,$90,$90,$90,$E0,$00; // b
.byte $04,$05,$00,$60,$90,$80,$80,$70,$00,$00,$00; // c
.byte $04,$07,$00,$10,$10,$70,$90,$90,$90,$70,$00; // d 
.byte $04,$05,$00,$60,$90,$F0,$80,$70,$00,$00,$00; // e
.byte $02,$07,$00,$40,$80,$C0,$80,$80,$80,$80,$00; // f
.byte $04,$07,$02,$70,$90,$90,$90,$70,$10,$20,$00; // g
.byte $04,$07,$00,$80,$80,$E0,$90,$90,$90,$90,$00; // h
.byte $01,$07,$00,$80,$00,$80,$80,$80,$80,$80,$00; // i
.byte $02,$07,$02,$40,$00,$40,$40,$40,$40,$80,$00; // j
.byte $04,$07,$00,$80,$80,$90,$A0,$C0,$A0,$90,$00; // k
.byte $01,$07,$00,$80,$80,$80,$80,$80,$80,$80,$00; // l
.byte $07,$05,$00,$EC,$92,$92,$92,$92,$00,$00,$00; // m
.byte $04,$05,$00,$E0,$90,$90,$90,$90,$00,$00,$00; // n
.byte $05,$05,$00,$70,$88,$88,$88,$70,$00,$00,$00; // o
.byte $04,$07,$02,$E0,$90,$90,$90,$E0,$80,$80,$00; // p
.byte $04,$07,$02,$70,$90,$90,$90,$70,$10,$10,$00; // q
.byte $02,$05,$00,$40,$80,$80,$80,$80,$00,$00,$00; // r
.byte $03,$05,$00,$60,$80,$40,$20,$C0,$00,$00,$00; // s
.byte $02,$07,$00,$80,$C0,$80,$80,$80,$80,$40,$00; // t
.byte $04,$05,$00,$90,$90,$90,$90,$70,$00,$00,$00; // u
.byte $05,$05,$00,$88,$88,$88,$50,$20,$00,$00,$00; // v
.byte $07,$05,$00,$92,$92,$92,$92,$6C,$00,$00,$00; // w
.byte $05,$05,$00,$88,$50,$20,$50,$88,$00,$00,$00; // x
.byte $04,$07,$02,$90,$90,$90,$90,$70,$10,$20,$00; // y
.byte $04,$05,$00,$F0,$20,$40,$80,$F0,$00,$00,$00; // z
.byte $02,$07,$00,$C0,$80,$80,$80,$80,$80,$C0,$00; // [
.byte $07,$07,$00,$80,$40,$20,$10,$08,$04,$02,$00; // slash
.byte $02,$07,$00,$C0,$40,$40,$40,$40,$40,$C0,$00; // ]
.byte $05,$07,$00,$20,$50,$88,$00,$00,$00,$00,$00; // ^
.byte $05,$01,$00,$F8,$00,$00,$00,$00,$00,$00,$00; // _
.byte $02,$07,$00,$80,$80,$40,$00,$00,$00,$00,$00; // `
.byte $05,$07,$00,$20,$50,$88,$88,$F8,$88,$88,$00; // A
.byte $04,$07,$00,$E0,$90,$90,$E0,$90,$90,$E0,$00; // B
.byte $04,$07,$00,$60,$90,$80,$80,$80,$80,$70,$00; // C
.byte $04,$07,$00,$E0,$90,$90,$90,$90,$90,$E0,$00; // D 
.byte $03,$07,$00,$E0,$80,$80,$E0,$80,$80,$E0,$00; // E
.byte $03,$07,$00,$E0,$80,$80,$E0,$80,$80,$80,$00; // F
.byte $04,$07,$00,$60,$90,$80,$80,$B0,$90,$70,$00; // G
.byte $05,$07,$00,$88,$88,$88,$F8,$88,$88,$88,$00; // H
.byte $01,$07,$00,$80,$80,$80,$80,$80,$80,$80,$00; // I
.byte $03,$07,$00,$20,$20,$20,$20,$20,$20,$C0,$00; // J
.byte $05,$07,$00,$88,$90,$A0,$C0,$A0,$90,$88,$00; // K
.byte $03,$07,$00,$80,$80,$80,$80,$80,$80,$E0,$00; // L
.byte $07,$07,$00,$82,$C6,$AA,$92,$82,$82,$82,$00; // M
.byte $05,$07,$00,$88,$C8,$A8,$98,$88,$88,$88,$00; // N
.byte $05,$07,$00,$70,$88,$88,$88,$88,$88,$70,$00; // O
.byte $04,$07,$00,$E0,$90,$90,$90,$E0,$80,$80,$00; // P
.byte $05,$08,$01,$70,$88,$88,$88,$88,$98,$78,$04; // Q
.byte $04,$07,$00,$E0,$90,$90,$90,$E0,$A0,$90,$00; // R
.byte $04,$07,$00,$70,$80,$C0,$20,$10,$10,$E0,$00; // S
.byte $03,$07,$00,$E0,$40,$40,$40,$40,$40,$40,$00; // T
.byte $04,$07,$00,$90,$90,$90,$90,$90,$90,$70,$00; // U
.byte $05,$07,$00,$88,$88,$88,$88,$88,$50,$20,$00; // V
.byte $07,$07,$00,$82,$82,$82,$92,$AA,$C6,$82,$00; // W
.byte $05,$07,$00,$88,$88,$50,$20,$50,$88,$88,$00; // X
.byte $05,$07,$00,$88,$88,$88,$50,$20,$20,$20,$00; // Y
.byte $07,$07,$00,$F8,$08,$10,$20,$40,$80,$F8,$00; // Z


