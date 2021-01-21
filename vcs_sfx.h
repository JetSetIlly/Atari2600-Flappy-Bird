; Copyright (c) 2017-2021, Stephen Illingworth
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
; 
; 1. Redistributions of source code must retain the above copyright notice, this
;    list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

; v1.0 - 19/08/2017

	include vcs_mdl.txt
	include dasm_extra.h

; these macros require a table of values labelled SFX_TABLE
; SFX_TABLE byte pattern:
;
;		queue instruction  -> FF == no queue (sound off)
;													00 == jump SFX_ENTRY_LEN
;
;		frames						-> 1 to 255 (0 will result in odd behavior)
;
;		noise/tone 				-> 00 to FF	(see noise/tone table below)
;											-> high nibble is channel 1
;											-> low nibble is channel 0
;
;		frequency 0				-> 00 to 31
;
;		frequency 1				-> 00 to 31
;
;		volume						-> 00 to FF
;											-> high nibble is channel 1
;											-> low nibble is channel 0
;
; the first sequence must be the off or idle state for the sfx engine
; for example:
;
;		SFX_TABLE FF 00 00 00 00 00

; noise/tone values (info taken from Paul Slocum's "Atari 2600 Music And Sound Programming Guide" v1.02)

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

; the macros also require the following:
;
; __SFX_SUB_FRAMES
; __SFX_NEW_EVENT
; __SFX_QUEUE_EVENT
;

SFX_NO_EVENT	= $FF
SFX_ENTRY_LEN = $06

	MAC SFX_LOAD
		; > {sfx address} [V]
		; ! SFX_TABLE
		; ! __SFX_NEW_EVENT
		; + ACZVN
		LDA #({1} - SFX_TABLE)
		STA __SFX_NEW_EVENT
	ENDM

	MAC SFX_FORCE_OFF
		; ! SFX_TABLE
		; ! __SFX_SUB_FRAMES
		; ! __SFX_NEW_EVENT
		; ! __SFX_QUEUE_EVENT
		; + ACZVN
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
		; ! SFX_TABLE
		; ! __SFX_SUB_FRAMES
		; ! __SFX_NEW_EVENT
		; ! __SFX_QUEUE_EVENT
		; + ACZVN
		SFX_FORCE_OFF
	ENDM

	MAC SFX_ENGINE
		; ! SFX_TABLE
		; ! __SFX_SUB_FRAMES
		; ! __SFX_NEW_EVENT
		; ! __SFX_QUEUE_EVENT
		; + ACZVN

		; check for new sfx event
		LDX __SFX_NEW_EVENT
		CPX #SFX_NO_EVENT
		BEQ .sfx_cont
.sfx_new_event

		; load queue instruction
		LDA SFX_TABLE,X
		CMP #SFX_NO_EVENT
		BEQ .sfx_queue_event
		; if the queue instruction is not the no event value
		; then queue the next sfx event in the sequence
		TXA
		CLC
		ADC #SFX_ENTRY_LEN
.sfx_queue_event
		STA __SFX_QUEUE_EVENT

		; load the rest of the event data
		INX
		LDA SFX_TABLE,X
		STA __SFX_SUB_FRAMES

		; load low nibble into control channel 0
		INX
		LDA SFX_TABLE,X
		AND #$0F
		STA AUDC0

		; load high nibble into control channel 1
		LDA SFX_TABLE,X
		LSR
		LSR
		LSR
		LSR
		AND #$0F
		STA AUDC1

		; frequency channel 0
		INX
		LDA SFX_TABLE,X
		STA AUDF0

		; frequency channel 1
		INX
		LDA SFX_TABLE,X
		STA AUDF1

		; load low nibble into volume channel 0
		INX
		LDA SFX_TABLE,X
		STA AUDV0
		LDA SFX_TABLE,X

		; load high nibble into volume channel 1
		LSR
		LSR
		LSR
		LSR
		AND #$0F
		STA AUDV1

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
