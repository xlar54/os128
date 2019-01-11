;****************************************************************************
; reu                                                                       *
;****************************************************************************

REU_BASE_ADDR   = $DF00

REU_STATUS_REG      = $00;      Status register 
REU_CMD_REG         = $01;      Command register 
REU_RAM_BASE_LO     = $02;      System RAM base address (low byte)
REU_RAM_BASE_HI     = $03;      System RAM base address (high byte) 
REU_EXP_RAM_BASE_LO = $04;      Expansion RAM base address (low byte) 
REU_EXP_RAM_BASE_HI = $05;      Expansion RAM base address (high byte)
REU_EXP_RAM_BANK    = $06;      Expansion RAM bank
REU_XFER_COUNT_LO   = $07;      Count of bytes to transfer (low byte)
REU_XFER_COUNT_HI   = $08;      Count of bytes to transfer (high byte)
REU_INTERRUPT_MASK  = $09;      Interrupt mask register
REU_ADDR_CONTROL    = $0A;      Address control register

