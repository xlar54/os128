; https://devdef.blogspot.com/2018/03/commodore-128-assembly-part-3-80-column.html

VDC_ADDR_REG = $D600
VDC_DATA_REG = $D601

VDC_HZ_TTL          = 0;    total # chars per line, including beam return 
VDC_HZ_DSP          = 1;    actual chars per line
VDC_HZ_SYNC_POS     = 2;    if reduced, left border moves right.  if increased left border moves left
VDC_SYNC_WIDTH      = 3
VDC_VERT_TTL        = 4;    total lines, including beam return
VDC_VERT_TTL_ADJ    = 5;
VDC_VERT_DSP        = 6;    total lines displayed
VDC_VERT_SYNC_POS   = 7;    if increased, the screen moves up.the screen moves down when decreased
VDC_INTLC_MOD       = 8;    interlace modes
VDC_CHAR_TTL_VERT   = 9
VDC_CRSR_MODE       = 10
VDC_CRSR_END        = 11
VDC_DSP_HI          = 12
VDC_DSP_LO          = 13
VDC_CRSR_POS_HI     = 14
VDC_CRSR_POS_LO     = 15
VDC_LGHT_PEN_VERT   = 16
VDC_LGHT_PEN_HZ     = 17
VDC_DATA_HI         = 18
VDC_DATA_LO         = 19
VDC_ATTRIB_HI       = 20
VDC_ATTRIB_LO       = 21
VDC_CHAR_TTL_DSP    = 22
VDC_CHAR_TTL        = 23
VDC_VSCROLL         = 24
VDC_HSCROLL         = 25
VDC_COLORS          = 26
VDC_ADDR_INC_RW     = 27
VDC_CSET            = 28
VDC_UNDRLN_SCAN     = 29
VDC_COUNT           = 30
VDC_DATA            = 31
VDC_BLK_START_HI    = 32
VDC_BLK_START_LO    = 33
VDC_DSP_ENBL_BEGN   = 34
VDC_DSP_ENBL_END    = 35
VDC_DRAM_REFRESH    = 36

VDC_CURSOR_MODE_NON_BLINK       = $00
VDC_CURSOR_MODE_CURSOR_OFF      = $01
VDC_CURSOR_MODE_SLOW_BLINK      = $02
VDC_CURSOR_MODE_FAST_BLINK      = $03

SetCursorMode .macro mode
        ldx #VDC_CRSR_MODE
        lda #\mode
        #WriteVDC
.endm

; The following procedures must be used to read/write from the VDC RAM:
;
; put the required register # in the address register;
; wait for the ready bit in the address register to go high;
; store or load the value from the data register;

WriteVDC .macro
        .block
        stx VDC_ADDR_REG
 _loop  bit VDC_ADDR_REG
        bpl _loop
        sta VDC_DATA_REG
        .bend
.endm

ReadVDC .macro
        .block
        stx VDC_ADDR_REG
_loop   bit VDC_ADDR_REG
        bpl _loop
        lda VDC_DATA_REG
        .bend
.endm

; Early C128 machines came with 16K of VDC RAM, expandable to
; 64K by added three additional 16K chips. Later models (C128, C128CR)
; had 64K RAM. The VDC is configured to use 16K or 64K modules, and we
; can check that configuration by reading bit 4 of register 28:

;       #SetBank 15  ; set bank 15
;       ldx #28
;       jsr READ_VDC
;       sta $fb
;       lda #010000
;       bit $fb
;       bne small
;       lda #$31
;       jsr $ffd2
;       rts
;   small:
;       lda #$30
;       jsr $ffd2
;       rts

; Of course, a 0 does not mean there's only 16K in your machine,
; only that the VCD is configured to use 16K modules. So there still
; might be 64K available. I have not found a way to check this, apart
; from trying to write something outside the 16K.

; Writing to the VDC RAM takes two steps:
; 1 - Set the update address where you want to write to;
; 2 - Pass data through the internal data register of the VDC (register 31);

SetVDCUpdateAddress .macro address
 ldx #18
 lda #>\address
 #WriteVDC
 inx
 a1 .var <\address
 a2 .var >\address
 .if(a1 != a2) 
  lda #<\address ; include if different from hi-byte.
 .fi
 #WriteVDC
.endm

; ========================================================================
; TEXT SCREEN MATRIX
; ========================================================================

; The Matrix data area in the VDC is located at $0000-$07FF by default.
; It's function is the same as screen memory in the C128 main memory map
; ($0400 by default).  As we have 80 characters in a row, it takes 2000 bytes,
; as opposed to the 1000 bytes on the 40 column display.

; The address in VDC RAM can be retrieved by reading registers 12 and 13.
; You can also change the address of the matrix data by writing a new value
; to these registers. This macro will read the current setting, and store it
; in $FB and $FC:

GetVDCDisplayStart .macro
 ldx #12
 #ReadVDC
 sta $fb
 inx
 #ReadVDC
 sta $fc
.endm

; ========================================================================
; CHARACTER ATTRIBUTES
; ========================================================================

; Attribute data is located at $0800-$0FFF by default, and can be read or
; changed by modifying registers 20 and 21. Attribute data is comparable to 
; VIC color data, but it has more features. Characters can blink, be inverse 
; (without these being part of the character definitions as with the VIC),
; underlined and more. Also, these attribute effects can be modified. for 
; instance, the underline can be moved withing the character, enabling, for 
; instance, a strike-through effect.

; Each byte in the attribute data corresponds to a screen location, and it
; can be written to. Attributes are enabled by setting a bit in each byte:

; 7 - Character set
; 6 - Reverse video
; 5 - Underline
; 4 - Blink
; 3 - Red
; 2 - Green
; 1 - Blue
; 0 - Intensity

; Attributes can be enabled or disabled (this is a global setting, allowing 
; you to use the RAM for something else) by using bit 6 of register 25. So 
; let's play, by writing some bogus values to this area:

; #SetVDCUpdateAddress $0800
;  ldy #0
;  lda #%01110101
;  ldx #31
; - #WriteVDC
;  ror    ; crazy stuff
;  dey
;  bne -
; 
;  ldx #25   ; write to this VDC register
;  #ReadVDC
;  ora #%01000000   ; enable attributes
;  #WriteVDC
;  rts

; ========================================================================
; LOADING ALTERNATE CHARACTER SETS
; ========================================================================
; As on the VIC, the character definitions can be changed on the VDC.
; The definitions are located, by default, at $2000-$3FFF in VDC RAM and
; its config can be changed or read through register 28. As the VDC cannot
; access the character ROM at $d000, and because it has no character
; definition data by itself, the definitions are copied from ROM to VDC RAM
; when the C128 boots. As a VDC character is defined in a 8x16 grid some
; padding is added to the character data each 8 bytes.
;
; This is done by a kernal routine called DLCHR. We could utilize this
; routine, but it has some hard coded values, so here's the macro I've 
; made that steals this idea and makes it more flexible.
;
; To use this, you need to define the start and end of the definitions
; data:
;
; * = $2000
; charset:
; .binary "data.64c"
; charset_end:
; 
; ...
; 
; #CopyDefinitionsToVDC charset, charset_end

CopyDefinitionsToVDC .macro  address, address_end
.block
        lda #<\address
        ldy #>\address
        sta $da   ; pointer to start of data
        sty $db
        lda #>\address_end
        sta $de   ; high byte of data end

        ldx #$12    ; write to $2000 in VDC ram
        lda #$20
        #WriteVDC
        inx
        lda #$00
        #WriteVDC
        ldy #0
loop:
        ldx #31   ; VDC data register
-       lda ($da),y
        #WriteVDC
        iny
        cpy #8
        bcc -
        lda #0     ; add 8 bytes as padding
-       #WriteVDC
        dey
        bne -
        clc
        lda $da
        adc #8
        sta $da
        bcc loop
        inc $db
        lda $db
        cmp $de    ; done all?
        bne loop
.bend
.endm




VDC_EnableTextMode .macro
    ldx #$19            ; talk to register 25
    #ReadVDC
    and $%00111111      ; turn off bit 7 to turn off gfx mode 
    #WriteVDC
.endm

; =============================================================================================

TEMP = $24
ADDR = $26
X1 = $28
Y1 = $29
X2 = $2A
Y2 = $2B
SCRBASE = #$00

* = $1cf0

BITMASK:    .byte $00
BITTAB:     .byte $80,$40,$20,$10,$08,$04,$02,$01

VDC_GfxMode: 
    ldx #$19            ; talk to register 25
    lda #%10000000      ; set bit 7 to turn on gfx mode
    #WriteVDC
    rts

VDCWriteByte:
        ldx     #VDC_DATA
VDCWriteReg:
        .block
        stx VDC_ADDR_REG
 _loop  bit VDC_ADDR_REG
        bpl _loop
        sta VDC_DATA_REG
        rts
        .bend

VDCSetSourceAddr:
        pha
        tya
        ldx     #VDC_DATA_HI
        jsr     VDCWriteReg
        pla
        ldx     #VDC_DATA_LO
        bne     VDCWriteReg
VDCReadByte:
        ldx     #VDC_DATA
VDCReadReg:
        .block
        stx VDC_ADDR_REG
_loop   bit VDC_ADDR_REG
        bpl _loop
        lda VDC_DATA_REG
        rts
        .bend

VDC_Clear:
        .block
        lda     #0
        ldy     #$00                            ; base addr
        jsr     VDCSetSourceAddr
        lda     #0
        ldx     #VDC_VSCROLL
        jsr     VDCWriteReg                     ; set fill mode
        lda     #0
        jsr     VDCWriteByte                    ; put 1rst byte (fill value)
        ldy     #62                             ; 62 times
        lda     #0                              ; 256 bytes
        ldx     #VDC_COUNT
L1:     jsr     VDCWriteReg
        dey
        bne     L1
        lda     #127
        jmp     VDCWriteReg 
        .bend

SETCOLOR:
        .block
        tax
        beq     L1
        lda     #$FF
L1:     sta     BITMASK
        rts
        .bend

; ------------------------------------------------------------------------
; SETPIXEL: Draw one pixel at X1/Y1 = ptr1/ptr2 with the current drawing
; color. The coordinates passed to this function are never outside the
; visible screen area, so there is no need for clipping inside this function.
;
; Must set an error code: NO
;

;SETPIXEL:
;        jsr     CALC            ; Calculate coordinates
;
;        stx     TEMP
;        lda     ADDR
;        ldy     ADDR+1
;        jsr     VDCSetSourceAddr
;        jsr     VDCReadByte
;        ldx     TEMP
;
;        sta     TEMP
;        eor     BITMASK
;        and     BITTAB,X
;        eor     TEMP
;        pha
;        lda     ADDR
;        ldy     ADDR+1
;        jsr     VDCSetSourceAddr
;        pla
;        jsr     VDCWriteByte
;
;L9:    rts

; ------------------------------------------------------------------------
; Calculate all variables to plot the pixel at X1/Y1.
;------------------------
;< X1,Y1 - pixel
;> ADDR - address of card
;> X - bit number (X1 & 7)
;CALC:
;        lda     Y1+1
;        sta     ADDR+1
;        lda     Y1
;        asl
;        rol     ADDR+1
;        asl
;        rol     ADDR+1          ; Y*4
;        clc
;        adc     Y1
;        sta     ADDR
;        lda     Y1+1
;        adc     ADDR+1
;        sta     ADDR+1          ; Y*4+Y=Y*5
;        lda     ADDR
;        asl
;        rol     ADDR+1
;        asl
;        rol     ADDR+1
;        asl
;        rol     ADDR+1
;        asl
;        rol     ADDR+1
;        sta     ADDR            ; Y*5*16=Y*80
;        lda     X1+1
;        sta     TEMP
;        lda     X1
;        lsr     TEMP
;        ror
;        lsr     TEMP
;        ror
;        lsr     TEMP
;        ror
;        clc
;       adc     ADDR
;        sta     ADDR
;        lda     ADDR+1          ; ADDR = Y*80+x/8
;        adc     TEMP
;        sta     ADDR+1
;        lda     ADDR+1
 ;       adc     SCRBASE
 ;       sta     ADDR+1
 ;       lda     X1
 ;       and     #7
 ;       tax
 ;       rts


; Keep-80 - Transactor Magazine
; Feb 1989, Volume 9, Issue 3
;
; To save the current 80 col screen: sys 4864,0,0
; To restore the 80 col screen     : sys 4864,0,1


;	*=$1300

	    ; rom routines
	    wrvdc = $cdca
	    rdvdc = $cdd8
	    vcopy = $c53c

	    ; ram locations
	    svars = $0030	; start of screen variables
	    smaps = $0354	; start of tab and link maps
	    pnt80 = $0a3c	; end pointer for vcopy
	    ztemp = $c3	    ; safe temporary location

keep	bit $d7		; test 80 columns
	    bmi ok80
err	    sec
	    rts

spage	.byte $10	; start page of unused area
epage	.byte $1f	; end page

ok80	cmp #$03
	    bcs err

	    stx ztemp	; direction
	    tay
	    bne edda
	    txa
	    bne rscrn

	; save whole screen

	    jsr rend	; write editor values to $07d0
	
	    lda #$d0	; destination end+1
	    ldx epage
	    sta pnt80
	    stx pnt80+1
	    lda spage	; dest. start
	    ldy #$00
	    jsr addwr
	    lda #$00	; source start = $0000
	    tay
setsrce	ldx #$20
	    jsr addwr+2
setcopy	ldx #$18
	    jsr rdvdc+2
	    ora #$80	; 7=1=copy
	    jsr wrvdc+2
	    jsr vcopy	; call rom routine
	    clc		; no errors
	    rts

	; recall whole screen

rscrn	lda #$d0	; copy everything
	    ldx #$0f	; back to $0000-$0fcf
	    sta pnt80
	    stx pnt80+1
	    lda #$00
	    tay
	    jsr addwr
	    lda spage	; source is unused area
	    ldy #$00
	    jsr setsrce

rend	lda #$07	; hi-byte of editor storage
	    bne edsa

edda	lda #$0f	; store / recall editor values
	    clc		; at $0fd0 or $1fd0
	    dey
	    beq edsa
	    adc #$10
edsa	ldy #$d0	; lo-byte
	

	; store / recall screen editor values

editsr	jsr addwr
	    ldy #$1a
	    lda ztemp	; 0=store
	    beq kloop3

kloop1	jsr rdvdc
	    sta svars, y
	    dey
	    bpl kloop1
	    ldy #$0d
kloop2	jsr rdvdc
	    sta smaps,y
	    dey
	    bpl kloop2
	    rts

kloop3	lda svars,y
	    jsr wrvdc
	    dey
	    bpl kloop3
	    ldy #$0d
kloop4	lda smaps,y
	    jsr wrvdc
	    dey
	    bpl kloop4
	    rts

; routine to write vdc to address registers ($12/$13)
; or any other pair of registers
; a=first byte, y=next byte, x=first register

addwr	ldx #$12
	    jsr wrvdc+2	; here for other pairs
	    tya
	    inx
	    jmp wrvdc+2
