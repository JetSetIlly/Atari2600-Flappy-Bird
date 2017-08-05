
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

