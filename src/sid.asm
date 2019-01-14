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

SID_WAVEFORM_TRIANGLE   = %00010001
SID_WAVEFORM_SAWTOOTH   = %00100001
SID_WAVEFORM_PULSE      = %01000001
SID_WAVEFORM_NOISE      = %10000001

SID_C   = 0;
SID_Csh = 2;
SID_D   = 4;
SID_Dsh = 6;
SID_E   = 8;
SID_F   = 10;
SID_Fsh = 12;
SID_G   = 14;
SID_Gsh = 16;
SID_A   = 18;
SID_Ash = 20;
SID_B   = 22;

; notes
SID_NOTES:
; Octave 0
.byte 12,  1  ; C
.byte 28,  1  ; C/D
.byte 45,  1  ; D
.byte 62,  1  ; D/E
.byte 81,  1  ; E
.byte 102, 1  ; F
.byte 123, 1  ; F/G
.byte 145, 1  ; G
.byte 169, 1  ; G/A
.byte 195, 1  ; A
.byte 221, 1  ; A/B
.byte 250, 1  ; B
; Octave 1
.byte 24,  2  ; C
.byte 56,  2  ; C/D
.byte 90,  2  ; D
.byte 125, 2  ; D/E
.byte 163, 2  ; E
.byte 204, 2  ; F
.byte 246, 2  ; F/G
.byte 35,  3  ; G
.byte 83,  3  ; G/A
.byte 134, 3  ; A
.byte 187, 3  ; A/B
.byte 244, 3  ; B
; Octave 2
.byte 48,  4  ; C
.byte 112, 4  ; C/D
.byte 180, 4  ; D
.byte 251, 4  ; D/E
.byte 71,  5  ; E
.byte 152, 5  ; F
.byte 237, 5  ; F/G
.byte 71,  6  ; G
.byte 167, 6  ; G/A
.byte 12,  7  ; A
.byte 119, 7  ; A/B
.byte 233, 7  ; B
; Octave 3
.byte 97,   8  ; C 
.byte 225,  8  ; C/D
.byte 104,  9  ; D
.byte 247,  9  ; D/E
.byte 143, 10  ; E
.byte 48,  11  ; F
.byte 218, 11  ; F/G
.byte 143, 12  ; G
.byte 78,  13  ; G/A
.byte 24,  14  ; A
.byte 239, 14  ; A/B
.byte 210, 15  ; B
; Octave 4
.byte 195, 16  ; C (middle C)
.byte 195, 17  ; C/D
.byte 209, 18  ; D
.byte 239, 19  ; D/E
.byte 31,  21  ; E
.byte 96,  22  ; F
.byte 181, 23  ; F/G
.byte 30,  25  ; G
.byte 156, 26  ; G/A
.byte 49,  28  ; A
.byte 223, 29  ; A/B
.byte 165, 31  ; B
; Octave 5
.byte 135, 33  ; C
.byte 134, 35  ; C/D
.byte 162, 37  ; D
.byte 223, 39  ; D/E
.byte 62,  42  ; E
.byte 193, 44  ; F
.byte 107, 47  ; F/G
.byte 60,  50  ; G
.byte 57,  53  ; G/A
.byte 99,  56  ; A
.byte 190, 59  ; A/B
.byte 75,  63  ; B
; Octave 6
.byte 15,   67  ; C
.byte 12,   71  ; C/D
.byte 69,   75  ; D
.byte 191,  79  ; D/E
.byte 125,  84  ; E
.byte 131,  89  ; F
.byte 214,  94  ; F/G
.byte 121, 100  ; G
.byte 115, 106  ; G/A
.byte 199, 112  ; A
.byte 124, 119  ; A/B
.byte 151, 126  ; B
; Octave 7
.byte 30,  134  ; C
.byte 24,  142  ; C/D
.byte 139, 150  ; D
.byte 126, 159  ; D/E
.byte 250, 168  ; E
.byte 6,   179  ; F
.byte 172, 189  ; F/G
.byte 243, 200  ; G
.byte 230, 212  ; G/A
.byte 143, 225  ; A
.byte 248, 238  ; A/B
.byte 46,  253  ; B

Sid_Init .macro
    ldy #$00
    lda #$00
again:
    sta SID_BASE, y
    iny
    cpy #$19
    bne again
.endm

Sid_SetVolume .macro level
    lda #\level
    sta SID_BASE + SID_VOL_AND_FILTER
.endm


Sid_PlayNote .macro voice, octave, note, waveform

    #Sid_StopNote \voice
    
    ldx SID_NOTES + 24 * \octave + \note
    ldy SID_NOTES + 24 * \octave + \note + 1
    lda #\waveform

.if \voice == 1
    stx SID_BASE + SID_V1_OSC_FREQ_LB
    sty SID_BASE + SID_V1_OSC_FREQ_UB
    sta SID_BASE + SID_V1_CTRL
.elsif \voice == 2
    stx SID_BASE + SID_V2_OSC_FREQ_LB
    sty SID_BASE + SID_V2_OSC_FREQ_UB
    sta SID_BASE + SID_V2_CTRL
.elsif \voice == 3
    stx SID_BASE + SID_V3_OSC_FREQ_LB
    sty SID_BASE + SID_V3_OSC_FREQ_UB
    sta SID_BASE + SID_V3_CTRL
.fi 
.endm

Sid_StopNote .macro voice
    lda #$00
.if \voice == 1
    sta SID_BASE + SID_V1_CTRL
.elsif \voice == 2
    sta SID_BASE + SID_V2_CTRL
.elsif \voice == 3
    sta SID_BASE + SID_V3_CTRL
.fi

.endm