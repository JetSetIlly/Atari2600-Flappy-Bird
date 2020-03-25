; Copyright (c) 2017-2020, Stephen Illingworth
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

NULL = 0
TRUE = 1
FALSE = 0

_MACRO_ERROR = "MACRO ERROR: "
_MSG_MARKER = ". "

	MAC DASM_MACRO_ERROR
		ECHO _MACRO_ERROR, {#}
		ERR
	ENDM

	MAC DASM_MESSAGE
		ECHO _MSG_MARKER, {#}
	ENDM

	MAC PAGE_CHECK
_PAGE_CHECK SET >*
	ENDM

	MAC PAGE_CHECK_END
	IF _PAGE_CHECK != >*
		DASM_MACRO_ERROR "page check fail:", {1}, "spans more than one page"
	ENDIF
	ENDM

	MAC MULT_3
		TSX				; store stack position
		PHA				; stack acumulator value
		ASL				; multiply by 2
		CLC
		ADC 0,X		; add last stack value - cumulative effect is multiplying by three
		TXS				; intentionally clobber SP - resetting stack position
	ENDM

	MAC SWAP
		LDA {1}
		PHA
		LDA {2}
		STA {1}
		PLA
		STA {2}
	ENDM

