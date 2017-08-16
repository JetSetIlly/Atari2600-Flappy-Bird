
; these functions require a table of values labelled SFX_TABLE
; SFX_TABLE byte pattern:
;
;		queue instruction; frames; noise/tone; frequency; volume
;
;		queue instruction  -> FF == no queue (sound off)
;													00 == jump SFX_ENTRY_LEN
;
;		frames						-> 1 to 255 (0 will result in odd behavior)
;		noise/tone				-> 00 to 0F	(see noise/tone table below)
;		frequency					-> 00 to 31
;		volume						-> 00 to 0F
;
; the first sequence must be the off or idle state for the sfx engine
; for example:
;
;		SFX_TABLE FF 00 00 00 00

; noise/tone values (taken from Paul Slocum's "Atari 2600 Music And Sound Programming Guide" v1.02)

; #		Name		Description
; =======================
; 01	Saw     sounds similar to a saw waveform
; 03	Engine  many 2600 games use this for an engine sound
; 04	Square  a high pitched square waveform
; 06	Bass    fat bass sound
; 07	Pitfall log sound in pitfall, low and buzzy
; 08	Noise   white noise
; 0C	Lead    lower pitch square wave sound
; 0F	Buzz    atonal buzz, good for percussion

SFX_NO_EVENT	= $FF
SFX_ENTRY_LEN = $05

	MAC SFX_LOAD
		LDA #({1} - SFX_TABLE)
		STA __SFX_NEW_EVENT
	ENDM

	MAC SFX_FORCE_OFF
		LDA #$00
		STA AUDC0
		STA AUDF0
		STA AUDV0
		STA __SFX_SUB_FRAMES
		LDA #SFX_NO_EVENT
		STA __SFX_NEW_EVENT
		STA __SFX_QUEUE_EVENT
	ENDM

	MAC SFX_ENGINE_INIT
		SFX_FORCE_OFF
	ENDM

	MAC SFX_ENGINE
		LDX __SFX_NEW_EVENT
		CPX #SFX_NO_EVENT
		BEQ .sfx_cont
.sfx_new_event
		; load new sfx data if __SFX_NEW_EVENT is not SFX_NO_EVENT

		; load queued event
		LDA SFX_TABLE,X
		CMP #SFX_NO_EVENT
		BEQ .sfx_queued_event
		TXA
		CLC
		ADC #SFX_ENTRY_LEN
.sfx_queued_event
		STA __SFX_QUEUE_EVENT
		INX
		LDA SFX_TABLE,X
		STA __SFX_SUB_FRAMES
		INX
		LDA SFX_TABLE,X
		STA AUDC0
		INX
		LDA SFX_TABLE,X
		STA AUDF0
		INX
		LDA SFX_TABLE,X
		STA AUDV0

		; new sfx event has been handled
		LDA #SFX_NO_EVENT
		STA __SFX_NEW_EVENT

.sfx_cont
		; decrease number of frames until it reaches zero
		DEC __SFX_SUB_FRAMES
		BPL .sfx_done
		; end of last sfx event check queued sfx event
		LDX __SFX_QUEUE_EVENT
		CPX #SFX_NO_EVENT
		BNE .sfx_new_event

		; no queued sfx event - turn sfx event off
		SFX_LOAD SFX_TABLE

.sfx_done
	ENDM
