
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