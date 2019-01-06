; The VIC chip in the C128 is configured exactly the same as on the C64. 
; The VIC chip registers are mapped onto $D000 in RAM. By default, the VIC chip 
; uses the same 16K area as the C64 ($0000 to $3FFF) and the screen and
; character memory offsets are also identical.
;
; Also, as on the C64, the VIC chip does not see ROM, only RAM. No matter what
; your memory configuration is, you can use the RAM you selected even if a ROM
; image is mapped in the same location.
;
; As you can see in the memory map above, the VIC can be pointed to 4 blocks
; of RAM, also called banks... The following macro accepts a number from 0 - 3:

SetVICBank .macro bank
 lda $dd00
 and #%11111100
 ora #3-\bank
 sta $dd00
.endm

; Within this 16K block of RAM, we can change the offsets to the character set
; data and the screen display matrix (screen memory) inside that block.
;
; #SetCharacterOffset 4
;This mean that the offset for the character data is 4K. If the VIC bank
;is located at $0000 (the default) then the character data is read from $1000.
;As you can see from the map above, this is the default.

SetCharacterOffset .macro offset
 lda $d018
 and #%11110001  ; clear the 3 offset control bits
 ora #\offset
 sta $d018
.endm


; The offset voor screen memory (screen matrix) can be changed in 1K steps.
; The macro I created for this below:
;
; Using #SetMatrixOffset 1
; will place the screen RAM at $0000 + $400 = $0400, which is, again,
; the default.

SetMatrixOffset .macro  offset
 lda $d018
 and #%00001111  ; clear the 4 offset control bits
 .if(offset > 0) 
   ora #\offset*16
 .fi
 sta $d018
.endm

; So far, nothing different from the C64. There is one new option though:
; you can tell the MMU that the VIC chip should use RAM block 1. 
; Here's the macro to do that, and it accepts a 0 or a 1 as value:
; All Kernal routines etc work with RAM block 0, so why you would want
; to do this, I do not know.  I accept all tips regarding this subject!

SetVICRAMBank .macro value
 lda MMURCR
 and #%10111111    ; clear bit 6
 .if(\value==1) {
  ora #%01111111   ; enable bit 6
 .fi
 sta MMURCR
.endm

