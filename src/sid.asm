;****************************************************************************
; sid                                                                       *
;****************************************************************************

SID_BASE                = $D400

SID_V1_OSC_FREQ_LB      = $00       ; Lower byte of oscillator frequency for voice 1.
SID_V1_OSC_FREQ_UB      = $01       ; Upper byte of oscillator frequency for voice 1
SID_V1_PULSE_WIDTH_LSB  = $02       ; Pulse width LSB for voice 1
SID_V1_PULSE_WIDTH_MSB  = $03       ; Pulse width MSB for voice 1
                                    ; Registers 2 and 3 determine the on/off duty cycle of the square
                                    ; output on voice 1. Only bits 0-3 of register 3 are used
SID_V1_CTRL             = $04       ; Control register for voice 1 
                                    ; Bit 0:KEY     Control bit for the course of the envelope generator.
                                    ;               When changed from 0 to 1,the volume of voice 1 increases
                                    ;               from zero to the maximum value (REG24) within the "attack"
                                    ;               time specified in REG5 and then within the "decay" time
                                    ;               specified in REG5 falls to the "sustain" level programmed 
                                    ;               in REG6, at which it remains until thecontrol bit is changed to
                                    ;               zero again.Then the volume falls to zero within the "release" time
                                    ;               specified in REG6. 
                                    ; Bit 1:SYNC    1=oscillator 1 is synchronized with oscillator 3.This bit also has 
                                    ;               effect when voice three is supposed to be silent. 
                                    ; Bit 2:RING    1=the triangle wave form output of oscillator 1 is replaced by a 
                                    ;               frequency mix (sum and difference of the frequencies of voices 1 and 3).
                                    ;               This effect also occurs when voice three is silent. 
                                    ; Bit 3:TEST    When another waveform is selected along with the noise generator in the
                                    ;               same oscillator, it can occur that the noise generator is disabled. It
                                    ;               can be re-enabled with this bit. 
                                    ; Bit 4:TRI     1=triangle waveform selected. 
                                    ; Bit 5:SAW     1=sawtooth waveform selected. 
                                    ; Bit 6:PUL     1=square waveform selected. The on/off relationship of this waveform 
                                    ;               is controlled in REG2 and REG3. 
                                    ; Bit 7:NSE     1=noise generator selected. Note for bits 4-7 It is possible in practice
                                    ;               to select multiple waveforms at the same time. In addition to what was said
                                    ;               for bit 3,it should be noted that result is not exaclty the sum of all 
                                    ;               of the forms but more of a logical AND of the components.

SID_V1_ATTACK_DECAY     = $05       ; Bits 0-3      These bits determine the time it takes until the volume falls from the maximum
                                    ;               value to the sustain level. The selectable range is from 6ms to 24seconds. 
                                    ; Bits4-7       Here the time it takes for the volume to reach the maximum value after the KEY
                                    ;               bit is set is defined.The selectable range is from  2ms to 8 seconds.

SID_V1_SUSTAIN_RELEASE  = $06       ; Bits 0-3      These bits determine the time within which the volume will fall from the
                                    ;               sustain level after the KEY bit is cleared (end of the tone). The selectable
                                    ;               range is 6ms to 24 seconds
                                    ; Bits 4-7      These bits specify the sustain level, the volume which will be maintained after
                                    ;               the maximum value is reached and be for it falls back.

; These registers control voice 2 in the same manner as do register 0-6, with the following exceptions: 
;   SYNC    synchronizes oscillator 2 with oscillator 3.
;   RING    replaces the triangle output of oscillator three with the frequency mix of oscillators 2 and 3.

SID_V2_OSC_FREQ_LB      = $07
SID_V2_OSC_FREQ_UB      = $08
SID_V2_PULSE_WIDTH_LSB  = $09
SID_V2_PULSE_WIDTH_MSB  = $0a
SID_V2_CTRL             = $0b
SID_V2_ATTACK_DECAY     = $0c
SID_V2_SUSTAIN_RELEASE  = $0d

; These registers control voice 3 in the same manner as do register 0-6, with the following exceptions: 
;   SYNC    synchronizes oscillator 3 with oscillator 2.
;   RING    replaces the triangle wave from oscillator 3 with the frequency mix from oscillators 2 and 3.

SID_V3_OSC_FREQ_LB      = $0e
SID_V3_OSC_FREQ_UB      = $0f
SID_V3_PULSE_WIDTH_LSB  = $10
SID_V3_PULSE_WIDTH_MSB  = $11
SID_V3_CTRL             = $12
SID_V3_ATTACK_DECAY     = $13
SID_V3_SUSTAIN_RELEASE  = $14

SID_FILTER_FREQ_LO      = $15       ; Filter frequency, low-order byte. Only bits 0-2 are used.
SID_FILTER_FREQ_HI      = $16       ; Filter frequency, high-order byte.
                                    ; The 11-bit number in registers 21 and 22 determines the frequency. 
                                    ; In the Commdore 128 this frequency is determined as follows: F=(30+W*5.8)Hz, whereby W is the 11-bit number.


SID_FILTER_RESONANCE    = $17       ; Filter resonance and switch 
                                    ; Bit 0     1=voice 1 is directed to the filter 
                                    ; Bit 1     1=voice 2 is directed to the filter 
                                    ; Bit 2     1=voice 3 is directed to the filter
                                    ; Bit 3     1=the external source is directed to the filter
                                    ; Bits 4-7  These bits determine the resonance frequency of the filter.
                                    ;           These are used to enhance specific sections of the frequency spectrum.The
                                    ;           effect is especially noticeable on the sawtooth waveform.

SID_VOL_AND_FILTER      = $18       ; Bits 0-3  : Total volume
                                    ; Bit 4     : Switches the lowpass filter on 
                                    ; Bit 5     : Switches the bandpass filter on 
                                    ; Bit 6     : Switches the highpass filter on 
                                    ; Bit 7     : l=voice 3 silent. This should be used whenever 
                                    ;             voice 3 is used to control the other voices

; all below are read only
SID_AD_CONVERTER1       = $19       ; Potentiometer (Paddle) x position
SID_AD_CONVERTER2       = $1a       ; Potentiometer (Paddle) y position
SID_NOISE_GEN_V3        = $1b       ; Noise generator for voice 3
                                    ; This register returns a random number which corresponds to the current
                                    ; state of the noise generator 3.The generator must be enabled, but voice 3 can
                                    ; be made inaudible (bit 7 in REG 24=1).

SID_ENV_GEN_V3          = $1c       ; Envelope generator for voice 3.
                                    ; This register returns the current condition of the relative volume of voice 3.
                                    ; This can be used to vary the frequency or filter parameters during the tone creation, for example

SID_WAVEFORM_TRIANGLE   = %00010000
SID_WAVEFORM_SAWTOOTH   = %00100000
SID_WAVEFORM_PULSE      = %01000000
SID_WAVEFORM_NOISE      = %10000000

SID_NOTE_TBL:
.byte 'c', 0
.byte 'd', 4
.byte 'e', 8
.byte 'f', 10
.byte 'g', 14
.byte 'a', 18
.byte 'b', 22

; notes
SID_NOTES:
; Octave 0
.byte $18, $02  ; C
.byte $38, $02  ; C/D
.byte $5a, $02  ; D
.byte $7e, $02  ; D/E
.byte $a4, $02  ; E
.byte $cc, $02  ; F
.byte $f7, $02  ; F/G
.byte $24, $03  ; G
.byte $54, $03  ; G/A
.byte $86, $03  ; A
.byte $bc, $03  ; A/B
.byte $f5, $03  ; B
; Octave 1
.byte $31, $04  ; C
.byte $71, $04  ; C/D
.byte $b4, $04  ; D
.byte $fc, $04  ; D/E
.byte $48, $05  ; E
.byte $98, $05  ; F
.byte $ed, $05  ; F/G
.byte $48, $06  ; G
.byte $a7, $06  ; G/A
.byte $0c, $07  ; A
.byte $78, $07  ; A/B
.byte $e9, $07  ; B
; Octave 2
.byte $62, $08  ; C
.byte $e1, $08  ; C/D
.byte $69, $09  ; D
.byte $f8, $09  ; D/E
.byte $90, $0A  ; E
.byte $30, $0B  ; F
.byte $db, $0B  ; F/G
.byte $8f, $0C  ; G
.byte $4e, $0D  ; G/A
.byte $19, $0E  ; A
.byte $f0, $0E  ; A/B
.byte $d3, $0F  ; B
; Octave 3
.byte $c4, $10  ; C (middle C)
.byte $c3, $11  ; C/D
.byte $d1, $12  ; D
.byte $f0, $13  ; D/E
.byte $1f, $15  ; E
.byte $61, $16  ; F
.byte $b6, $17  ; F/G
.byte $1e, $19  ; G
.byte $9d, $1a  ; G/A
.byte $32, $1c  ; A
.byte $df, $1d  ; A/B
.byte $a6, $1F  ; B
; Octave 4
.byte $88, $21  ; C
.byte $86, $23  ; C/D
.byte $a3, $25  ; D
.byte $e0, $27  ; D/E
.byte $3f, $2a  ; E
.byte $c2, $2c  ; F
.byte $6b, $2f  ; F/G
.byte $3d, $32  ; G
.byte $3a, $35  ; G/A
.byte $64, $38  ; A
.byte $be, $3b  ; A/B
.byte $4c, $3f  ; B
; Octave 5
.byte $0f, $43  ; C
.byte $0c, $47  ; C/D
.byte $46, $4b  ; D
.byte $bf, $4f  ; D/E
.byte $7d, $54  ; E
.byte $83, $59  ; F
.byte $d6, $5e  ; F/G
.byte $7a, $64  ; G
.byte $73, $6a  ; G/A
.byte $c8, $70  ; A
.byte $7c, $77  ; A/B
.byte $97, $7e  ; B
; Octave 6
.byte $1e, $86  ; C
.byte $18, $8e  ; C/D
.byte $8b, $96  ; D
.byte $7f, $9f  ; D/E
.byte $fb, $a8  ; E
.byte $07, $b3  ; F
.byte $ac, $bd  ; F/G
.byte $f3, $c8  ; G
.byte $e6, $d4  ; G/A
.byte $8f, $e1  ; A
.byte $f9, $ee  ; A/B
.byte $2f, $fd  ; B

SetVolume .macro level
    lda #\level
    sta SID_BASE + SID_VOL_AND_FILTER
.endm

SetWaveform .macro voice, waveform
    lda #\waveform
.if \voice == 1
    sta SID_BASE + SID_V1_CTRL
.elsif \voice == 2
    sta SID_BASE + SID_V2_CTRL
.elsif \voice == 3
    sta SID_BASE + SID_V3_CTRL
.fi
.endm

PlayNote .macro voice, octave, note

.if \voice == 1
    lda SID_BASE + SID_V1_CTRL
    ora #%00000001
    sta SID_BASE + SID_V1_CTRL
.elsif \voice == 2
    lda SID_BASE + SID_V2_CTRL
    ora #%00000001
    sta SID_BASE + SID_V2_CTRL
.elsif \voice == 3
    lda SID_BASE + SID_V3_CTRL
    ora #%00000001
    sta SID_BASE + SID_V3_CTRL
.fi

    lda SID_NOTES + (24 * \octave) + \note

.if \voice == 1
    sta SID_BASE + SID_V1_OSC_FREQ_LB
.elsif \voice == 2
    sta SID_BASE + SID_V2_OSC_FREQ_LB
.elsif \voice == 3
    sta SID_BASE + SID_V3_OSC_FREQ_LB
.fi 

    lda SID_NOTES + (24 * \octave) + \note + 1

.if \voice == 1
    sta SID_BASE + SID_V1_OSC_FREQ_UB
.elsif \voice == 2
    sta SID_BASE + SID_V2_OSC_FREQ_UB
.elsif \voice == 3
    sta SID_BASE + SID_V3_OSC_FREQ_UB
.fi

.endm

StopNote .macro voice
    lda #$00
.if \voice == 1
    sta SID_BASE + SID_V1_CTRL
.elsif \voice == 2
    sta SID_BASE + SID_V2_CTRL
.elsif \voice == 3
    sta SID_BASE + SID_V3_CTRL
.fi

.endm