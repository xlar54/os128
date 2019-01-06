; https://devdef.blogspot.com/2018/03/commodore-128-assembly-part-2-memory.html

; MMU bits
;  7   6   5   4   3   2   1   0
; ---:---:---:---:---:---:---:---
;  0   0                             = RAM block 0
;  0   1                             = RAM block 1
;  1   0                             = RAM block 2 (Doesnt exist in a 128, so unusable)
;  1   1                             = RAM block 3 (Doesnt exist in a 127, so unusable)
;          0   0                     = $C000 - $FFFF = Kernal and character ROM
;          0   1                     =               = Internal function ROM
;          1   0                     =               = External function ROM
;          1   1                     =               = RAM (Bit 0 determines contents of $D000-$DFFF)
;                  0   0             = $8000 - $BFFF = BASIC & ML Monitor ROM
;                  0   1             =               = Internal function ROM
;                  1   0             =               = External function ROM
;                  1   1             =               = RAM
;                          0         = $4000 - $7FFF = BASIC ROM (Low)
;                          1         =               = RAM
;                              0     = $D000 - $DFFF = I/O Block RAM
;                              1     =               = RAM or ROM (as selected by bits 4 & 5)
;

; Std Bank configs (eliminating unusable configurations)
; Bank  Value       Addresses
; ===========================================================================================
; $00   $3F         $0000 - $FFFF       RAM from block 0
; $01   $7F         $0000 - $03FF       RAM from block 0
;                   $0400 - $FFFF       RAM from block 1
; $0E   $01         $0000 - $3FFF       RAM from block 0
;                   $4000 - $CFFF       System ROM (BASIC 7.0, ML monitor, screen editor)
;                   $D000 - $DFFF       Character ROM
;                   $E000 - $FFFF       System ROM (Kernal)
; $0F   $00         $0000 - $3FFF       RAM from block 0
;                   $4000 - $CFFF       System ROM (BASIC 7.0, ML monitor, screen editor)
;                   $D000 - $DFFF       I/O block
;                   $E000 - $FFFF       System ROM (Kernal)
;
; All banks: $0000 and $0001 are 8502's I/O port and data direction
; registers.  Locations $FF00 - $FF04 are MMU config and load
; configuration registers
;
; The I/O block of banks 14 and 15 are further divided into 
; I/O registers at $D000-$D800, color RAM at $D800-$DBFF, and
; alternate I/O registers at $DC00-$DFFF
;
; The 128 has two separate 1K banks of color RAM, both seen at the
; same addresses.  $D800 - $DBFF in the I/O block.  Bits 0 and 1 in
; the 8502 processor's on-chip data I/O port (location $01) determines
; which block will be seen by the processor and by the VIC chip

MMUCR   = $FF00     ; Configuration register
MMULCRA = $FF01     ; Load configuration register A
MMULCRB = $FF02     ; Load configuration register B
MMULCRC = $FF03     ; Load configuration register C
MMULCRD = $FF04     ; Load configuration register D
MMURCR  = $FF06

; Banking is about two things:
; selecting a RAM block;
; determining which ROMS are available, or not
; Being an 8 bit processor, the 8502 can only 'see' 64K at a time so
; there needs to be a mechanism to switch between the RAM blocks. 
; This is done by selecting a bank configuration.

; ROM chips can be enabled or disabled. When disabling a ROM, the
; RAM underneath becomes available for reading. You can always write
; to a RAM location 'underneath' a ROM.

; The BASIC 'BANK' command will select one of 15 configurations that Commodore
; has prepared. Bank configuration 15 is the default setting, and it provides
; the configuration shown in the map above.

; In machine code, we need to access the MMU directly, and set the bits of
; the configuration register ($FF00) that we want.  A macro helps with that:

SetBankConfig  .macro id
.if (\id == 0)
    lda #$3F    ; no roms, RAM0
    sta MMUCR
.elsif (\id == 1)
    lda #$7F    ; no roms, RAM1
    sta MMUCR
.elsif (\id == 14)
    lda #$01  ; all roms, RAM0. default setting.
    sta MMUCR
.elsif (\id == 15)
    lda #$00  ; IO, kernal, RAM0. 48K RAM.
    sta MMUCR
.fi
.endm

; By default, common RAM is only present at the bottom of the memory map, 
; has a size of 1K, and runs from $0000 to $03FF.  Common RAM always comes 
; from RAM block 0. The configuration of common RAM has nothing to do with 
; banking; choosing another bank configuration does not change the common RAM settings.

; If your program or data is too large to fit in the default common area, 
; then its size can be changed. It can be 1, 4, 8 or 16K in size. 
; The macro I created to change the size is this:

SetCommonRAM    .macro amount
 lda MMURCR
 and #%11111100  ; clear bits 0 and 1. this is also option 1
 .if(\amount==4)
  ora #%00000001
 .elsif(\amount==8) 
  ora #%00000010
 .elsif(\amount==16)
  ora #%00000011
 .fi
 sta MMURCR
.endm

; Also, we can choose to make the top of memory common, the bottom (default) or both. 
; In total we can have 32K of common RAM. This is done by using the following macro:
;
; Calling #SetCommonEnabled 1 will make the bottom 8K common only.

SetCommonEnabled .macro option
 lda MMURCR
 and #%11110011    ; clear bits 2 and 3
 ora #\option*4
 sta MMURCR
.endm