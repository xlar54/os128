; =============================================================================================================================
;
;  64/128 Kernal Equates
; 
; =============================================================================================================================

;Label   Jump   Vector Real  Function                           Function Input/Output                    Register Usage
;        addr    addr  code  Description                        Parameters                             entry  return  used
;-------------------------------------------------------------------------------------------------------------------------
CINT   = $FF81;  ----  FF5B  init VIC & screen editor                                                  - - -  - - -  A X Y
IOINIT = $FF84;  ----  FDA3  initialize CIA & IRQ                                                      - - -  - - -  A X Y
RAMTAS = $FF87;  ----  FD50  RAM test & search RAM end                                                 - - -  - - -  A X Y
RESTOR = $FF8A;  ----  FD15  restore default I/O vectors                                               - - -  - - -  A - Y
VECTOR = $FF8D;  ----  FD1A  read/set I/O vectors               in: C=0 moves from Y/X to vectors      - X Y  - X -  A - Y
;                                                                   C=1 moves vectors to Y/X           - X Y  - X -  A - Y
SETMSG = $FF90;  ----  FE18  enable/disable KERNAL messages     in: A bit7=1 error msgs on             A - -  - - -  A - -
;                                                                     bit6=1 control msgs on
SECOND = $FF93;  ----  EDB9  send secondary addr after listen   in: A=secondary address                A - -  - - -  A - -
TKSA   = $FF96;  ----  EDC7  send secondary addr after talk     in: A=secondary address                A - -  - - -  A - -
MEMTOP = $FF99;  ----  FE25  read/set top of memory             in: C=0; Y/X address                   - X Y  - X Y  - - -
;                                                               out:C=1; Y/X address                   - - -  - X Y  - X Y
MEMBOT = $FF9C;  ----  FE34  read/set bottom of memory          in: C=0; Y/X address                   - X Y  - X Y  - - -
;                                                               out:C=1; Y/X address                   - - -  - X Y  - X Y
SCNKEY = $FF9F;  ----  EA87  scan keyboard                                                             - - -  - - -  A X Y
SETTMO = $FFA2;  ----  FE21  set IEEE timeout                   in: A bit7=1 disable, bit7=0 enable    A - -  A - -  - - -
ACPTR  = $FFA5;  ----  EE13  input byte from SERIAL             out:A=byte, C=1 and ST=2 if timeout    - - -  A - -  A - -
CIOUT  = $FFA8;  ----  EDDD  output byte to SERIAL              in: A=byte, C=1 and ST=3 if timeout    A - -  A - -  - - -
UNTLK  = $FFAB;  ----  EDEF  untalk all SERIAL devices                                                 - - -  - - -  A - -
UNLSN  = $FFAE;  ----  EDFE  unlisten all SERIAL devices                                               - - -  - - -  A - -
LISTEN = $FFB1;  ----  ED0C  make SERIAL device listen          in: A=device number                    A - -  - - -  A - -
TALK   = $FFB4;  ----  ED09  make SERIAL device talk            in: A=device number                    A - -  - - -  A - -
READST = $FFB7;  ----  FE07  read I/O status byte               out:A=status byte                      - - -  A - -  A - -
SETLFS = $FFBA;  ----  FE00  set file parameters                in: A=logical file number              A X Y  A X Y  - - -
;                                                                   X=device number
;                                                                   Y=secondary addr
SETNAM = $FFBD;  ----  FDF9  set file name                      in: A=length of filename               A X Y  A X Y  - - -
;                                                                   Y/X=pointer to name addr
OPEN   = $FFC0;  031A  F34A  open log.file after SETLFS,SETNAM  out:A=error# if C=1                    - - -  - - -  A X Y
CLOSE  = $FFC3;  031C  F291  close a logical file               in: A=logical file number              A - -  - - -  A X Y
CHKIN  = $FFC6;  031E  F20E  open channel for input             in: X=logical file number              - X -  - - -  A X -
CHKOUT = $FFC9;  0320  F250  open channel for output            in: X=logical file number              - X -  - - -  A X -
CLRCHN = $FFCC;  0322  F333  restore default devices                                                   - - -  - - -  A X -
CHRIN  = $FFCF;  0324  F157  input character                    out:A=character, C=1 and ST=error      - - -  A - -  A - -
CHROUT = $FFD2;  0326  F1CA  output character                   in: A=character, C=1 and ST=error      A - -  A - -  - - -
LOAD   = $FFD5;  0330  F49E  load after call SETLFS,SETNAM      in: A=0 load, a=1 verify               A X Y  A X Y  A X Y
;                                                                   Y/X = dest.addr if sec.addr=0
SAVE   = $FFD8;  0332  F5DD  save after call SETLFS,SETNAM      in: A=zero page pointer to start.addr  A X Y  - - -  A X Y
;                                                                   Y/X=ending address
SETTIM = $FFDB;  ----  F6E4  set jiffy clock                    in: A=MSB, X=middle, Y=LSB             A X Y  - - -  - - -
RDTIM  = $FFDE;  ----  F6DD  read jiffy clock                   out:A=MSB, X=middle, Y=LSB             - - -  A X Y  A X Y
STOP   = $FFE1;  0328  F6ED  check stop key                     out:Z=0 if STOP not used; X unchanged  - - -  A - -  A - -
;                                                                   Z=1 if STOP used; X changed        - - -  A - -  A X -
;                                                                   A=last line of keyboard matrix
GETIN  = $FFE4;  032A  F13E  get a byte from channel            out:keyboard:A=0 if puffer empty       - - -  A - -  A X Y
;                                                                   RS232:status byte                  - - -  A - -  A - -
;                                                                   serial:status byte                 - - -  A - -  A - -
;                                                                   tape:status byte                   - - -  A - -  A - Y
CLALL  = $FFE7;  032C  F32F  close or abort all files                                                  - - -  - - -  A X -
UDTIM  = $FFEA;  ----  F69B  update jiffy clock                                                        - - -  - - -  A X -
SCREEN = $FFED;  ----  E505  return screen size                 out:X=columns, Y=rows                  - - -  - X Y  - X Y
PLOT   = $FFF0;  ----  E50A  read/set cursor position           in: C=0, X=row, Y=column               - X Y  - X Y  - - -
;                                                               out:C=1, X=row, Y=column               - - -  - X Y  - X Y
IOBASE = $FFF3;  ----  E500  returns the addr of I/O devices    out:Y/X=addr($DC00)                    - - -  - X Y  - X Y

; =============================================================================================================================
;
; C128 Specific
;
; =============================================================================================================================
SPIN_SPOUT = $FF47      ; setup fast serial ports for I/O.  
                        ; Enter with the status register carry bit clear to establish fast serial 
                        ; input or with the bit set to establish fast serial output. Unless you are 
                        ; writing a custom data transfer routine, it's not necessary to call this 
                        ; routine explicitly. All the standard serial I/O routines already include this setup step.

CLOSE_ALL  = $FF4A      ; close all files on a device
                        ; Enter the routine with the accumulator holding the number of the device on
                        ; which files are to be closed. If the specified device is the current input
                        ; or output device, the channel will be reset to the default device {screen or 
                        ; keyboard). If all files to the device have been successfully closed, the status
                        ; register carry bit will clear upon return. A set carry bit indicates that a device
                        ; error has occurred.

C64MODE    = $FF4D      ; reconfigure system as a C64

DMA_CALL   = $FF50      ; send command to DMA device
                        ; The only DMA peripherals currently available for the 128 are the 1700 and 1750 Memory
                        ; Expansion Modules, controlled by a DMA chip known as the REC (see Chapter 8 for more
                        ; information). Call the routine with the Y register holding the command for the DMA 
                        ; device and the X register holding the bank number for the operation. Additional setup
                        ; steps may be required, depending on the command.

BOOT_CALL  = $FF53      ; boot load program from disk
                        ; Call the routine with the X register holding the device number for the desired disk
                        ; drive (usually 8) and the accumulator holding the character code corresponding to the
                        ; desired drive number—not the actual drive number. (For example, the single drive in 1541
                        ; and 1571 units is designated drive 0, so you would use 48/$30, the character code for zero.)
                        ; If the specified drive is not present or turned off, or tf the disk in the drive does not
                        ; contain valid boot sectors, the routine will return with the status register carry bit set.
                        ; If boot sectors are found and executed, the boot code may or may not return to the calling routine

PHOENIX    = $FF56      ; init function cartridges and attempts to boot a disk from the default drive

LKUPLA     = $FF59      ; checks whether a specified logical file number is currently in use
                        ; Call the routine with the accumulator holding the desired logical file number. If that number
                        ; is used for a currently open file, the status register carry bit will be clear upon return.
                        ; The accumulator will still hold the logical file number; the X register will hold the
                        ; associated device number, and the Y register the secondary address. However, if the logical
                        ; file number is not currently used, the carry bit will instead be set upon return (the logical
                        ; file number will still be in the accumulator).

LKUPSA     = $FF5C      ; checks whether a specified secondary address is currently in use
                        ; Call the routine with the Y register holding the desired secondary address. If that number
                        ; is used for a currently open file, the status register carry bit will be clear upon return.
                        ; The Y register will still hold the secondary address; the accumulator will hold the associated
                        ; logical file number, and the X register the device number. However, if the secondary address
                        ; is not currently used, the carry bit will instead be set upon return (the secondary address value
                        ; will still be in the Y register).

SWAPPER    = $FF5F      ; switch between 40 and 80 columns

DLCHR      = $FF62      ; init 80-col character RAM
                        ; copies character shape data from ROM into the 8563 80-column video chip's private block of RAM
                        ; (the 8563 has no character ROM of its own).

PFKEY      = $FF65      ; PFKEY – program a function key
                        ; The routine assigns a new definition to one of the ten programmable function keys 
                        ; (F1-F8, SHIFTRUN/STOP, and HELP). Call the routine with the accumulator holding the address of a
                        ; three-byte string descriptor in zero page, the X register containing the key number (1-10), and the
                        ; Y register containing the length of the new definition string. The descriptor in zero page should 
                        ; consist of the twobyte address of the new definition string (in standard lowbyte/high-byte order),
                        ; followed by the bank number where the definition string is located. The key number is not checked for
                        ; validity; if you specify a value outside the acceptable range, you may garble existing definitions. Upon 
                        ; return, the carry bit will be clear if the new definition has been successfully added, or it will be set
                        ; if there is insufficient room in the definition table for the new definition.

SETBNK     = $FF68      ; set bank for I/O operations
                        ; establishes the current bank from which data will be read or to which data will be written during 
                        ; load/save operations, as well as the bank where the filename for the I/O operations can be found. 
                        ; Call the routine with the accumulator holding the bank number for data and the X register holding
                        ; the bank for the filename. All register values are preserved during this routine.

GETCFG     = $FF6B      ; lookup MMU data for given bank
                        ; translates a bank number into the MMU register setting which will configure the system for that
                        ; bank. Call the routine with the X register containing the bank number (0-15). Upon return, the 
                        ; accumulator will hold the corresponding MMU configuration register setting value. The routine does 
                        ; not check the validity of the bank number input. If you specify a number outside the acceptable range, 
                        ; the routine will return a meaningless value.

JSRFAR     = $FF6E      ; gosub in another bank
                        ; jumps to a subroutine in a specified bank and returns to the calling routine in bank 15. Prior to
                        ; calling this routine, you must load location 2/$02 with the bank number (0-15) of the target routine
                        ; and locations 3-4/$03-$04 with the address of the target routine (in high-byte/low-byte order, the
                        ; opposite of the normal arrangement). You should also load location 5/$05 with the value you want placed
                        ; in the status register when the target routine is called. (The behavior of many routines is influenced
                        ; by the status register setting, particularly the state of the carry bit. Load 5/$05 with the value 0/$00
                        ; if the target routine is to be called with carry clear, or with l/$01 if it is to be called with carry set,)
                        ; If you wish to pass other register values to the routine you will be calling, store the desired accumulator
                        ; value in location 6/$06, the X register value in 7/$07, and the Y register value in 8/$08. Upon return,
                        ; location 5/$05 will hold the status register value at the time of exit, 6/$06 will hold the accumulator
                        ; value, 7/$07 will hold the X register value, 8/$08 will hold the Y register value, and 9/$ 09 will hold 
                        ; the stack pointer value. The system is always reconfigured for bank 15 upon exit. 

JMPFAR     = $FF71      ; goto another bank
                        ; jumps to a routine in a specified bank, with no return to the calling bank. Prior to calling this routine,
                        ; you must load location 2/$02 with the bank number (0-15) of the target routine and locations 3-4/$03-$04
                        ; with the address of the target routine (in high-byte/low-byte order, the opposite of the normal arrangement).
                        ; You should also load location 5/$05 with the value you want placed in the status register when the target
                        ; routine is entered. (The behavior of many routines is influenced by the status register setting, particularly
                        ; the state of the carry bit. Load 5/$05 with the value 0/$00 if the target routine is to be entered with carry
                        ; clear or with l/$01 if it is to be entered with carry set.) If you wish to pass other register values to the
                        ; routine you will be calling, store the desired accumulator value in location 6/$06, the X register value in
                        ; 7/$07, and the Y register value in 8/$08

INDFET     = $FF74      ; LDA (fetvec),Y from any bank
                        ; retrieves a byte from a specified bank. Prior to calling this routine, you must load a two-byte zeropage
                        ; pointer with the address of the location from which the byte is to be retrieved (or the base location if 
                        ; a series of bytes are to be retrieved). Call the routine with the accumulator holding the address of the
                        ; zero-page pointer, the X register holding the bank number (0-15) for the target location, and the Y register
                        ; holding an offset value which will be added to the address in the pointer to determine the location from which
                        ; the byte is to be loaded. (Load Y with zero if no offset is desired.) Upon return, the accumulator will hold
                        ; the byte from the specified address. The value in the Y register will be preserved during the routine. If you
                        ; are retrieving data from a series of locations, it is necessary to reload the accumulator and X registers with
                        ; the pointer address and bank number before every call to this routine, but you can read up to 256 sequential
                        ; locations without changing the address in the zeropage pointer by simply incrementing the Y register between calls.

INDSTA     = $FF77      ; STA (stavec),Y to any bank
                        ; stores the accumulator contents at an address in a specified bank. Prior to calling this routine, you 
                        ; must load a two-byte zero-page pointer with the address of the location at which the byte is to be stored
                        ; (or the base location if a series of bytes is to be stored); then store the address of this pointer in
                        ; location 697/$02B9. Call the routine with the accumulator holding the byte to be stored, the X register
                        ; holding the bank number (0-15) for the target location, and the Y register holding an offset value which
                        ; will be added to the address in the pointer to determine the location in which the byte is to be stored.
                        ; (Load Y with zero if no offset is desired.) Upon return, the accumulator will still hold the byte value.
                        ; The value in the Y register will also be preserved. If you are writing data to a series of locations, it
                        ; is necessary to reload the X register with the bank number before every call to this routine, but you can
                        ; write to up to 256 sequential locations without changing the address in the zero-page pointer by simply
                        ; incrementing the Y register between calls.

INDCMP     = $FF7A      ; CMP (cmpvec),Y to any bank
                        ; compares the accumulator contents against the contents of a location in a specified bank. Prior to calling
                        ; this routine, you must load a two-byte zero-page pointer with the address of the location with which the byte
                        ; is to be compared (or the base location if a series of bytes are to be compared); then store the address of
                        ; this pointer in location 712/$02C8. Call the routine with the accumulator holding the byte to be compared,
                        ; the X register holding the bank number (0-15) for the target location, and the Y register holding an offset
                        ; value which will be added to the address in the pointer to determine the location with which the byte is to
                        ; be compared. (Load Y with zero if no offset is desired.) Upon return, the accumulator will still hold the byte
                        ; value, and the status register N, Z, and C (carry) bits will reflect the result of the comparison. The value
                        ; in the Y register will also be preserved. If you are comparing a series of locations, it is necessary to reload
                        ; the X register with the bank number before every call to this routine, but you can compare up to 256 sequential
                        ; locations without
                        
PRIMM      = $FF7D      ; print string following the caller’s code
                        ; prints the string of character codes immediately following the JSR to this routine. You must always call
                        ; this routine with JSR, never with JMP. Only JSR places the required address information on the stack. The
                        ; end of the character code string is indicated by a byte containing 0/$00 (the routine continues printing
                        ; bytes as character codes until a zero byte is encountered). When the ending marker is found, the routine
                        ; returns to the address immediately following the zero byte.





