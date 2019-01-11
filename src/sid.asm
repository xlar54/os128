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

