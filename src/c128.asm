;****************************************************************************
; c128 common                                                               *
;****************************************************************************

SCREEN_SELECT               = $d7       ; 0 = 40 COL / 128 = 80 COL
IRQVEC                      = $0314

C128_CASSETTE_BUFFER        = $0b00     ; 0b00 - 0bff
C128_RS232_INPUT_BUFFER     = $0c00     ; 0c00 - 0cff
C128_RS232_OUTPUT_BUFFER    = $0d00     ; 0d00 - 0dff
C128_SPRITE_DEFS            = $0e00     ; 0e00 - 0fff



; Creates a basic loader for the machine language program
; <address> must be higher than $1c0e or just use $1c0e
; use a label for the address of the main program
BasicStart128 .macro address
.block
    * = $1c01                   ; C128 Basic
    .word upstartEnd            ; link address
    .word 10                    ; line num
    .byte $9e                   ; sys
    .text format("%d", \address)
    .byte 0
upstartEnd:
    .word 0                     ; empty link signals the end of the program
    * = $1c0e                   ;Basic End
.bend
.endm

Go80 .macro
.block
 lda MODE       ; are we in 80 columns mode?
 bmi _ahead     ; bit 7 set? then yes
 jsr SWAPPER    ; swap mode to 80 columns
_ahead:
.bend
.endm