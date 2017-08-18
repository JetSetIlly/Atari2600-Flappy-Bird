
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

