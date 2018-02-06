'----------------------------------------------------------------------------------------------------------------------
' Tokenizer.bi
' #include once "Toknizer.bi"
'----------------------------------------------------------------------------------------------------------------------
Operator *(ByVal t As TToken) As String 
	'
	' "text: >"; t.txt; "<"
	? "token type: ";	*Cast(TokenType, t.token_type)
	? "locate: "; t.index
	? "length: "; Len(t.index)
	Return ""
	'return t.txt
End Operator
'----------------------------------------------------------------------------------------------------------------------
Constructor TTokenizer()
	'
	this._text = StrPtr(source_text)
	this.symbols.read_symbols()
End Constructor
Constructor TTokenizer(ByRef source As Const String)
	'
	source_text = source
	this._text = StrPtr(source_text)
	this.symbols.read_symbols()
End Constructor
'----------------------------------------------------------------------------------------------------------------------
'----------------------------------------------------------------------------------------------------------------------
'Sub TTokenizer.create_bas_file(ByRef file_name As String)
'	'
'	Dim As Integer fnum
'	Dim As String s
'	Dim As ttoken t
'	
'	fnum = FreeFile()
'	If Open(file_name, For Output, As fnum) <> 0 Then
'		Cls
'		? "Can't open output file: " & file_name & " in " & __Function__ & " at line " & __LINE__ 
'		Sleep
'		End
'		'Return
'	EndIf
'		For n As Integer = 1 To this._token_count
'			t = this.tokens(n)
'			If t.token_type <> ttStatementEnd Then 
'				s &= t.txt 
'				If t.token_type <> ttQuoteOpen AndAlso t.token_type <> ttQuotedLiteral Then
'					s &= " "
'				EndIf
'
'			Else
'				Print #fnum, s
'				s = ""
'			EndIf
'		Next
'	Close fnum
'End Sub
Sub TTokenizer._tokenize_source()
	'
	Enum modes 
		mdNone
		mdMultiComment
		mdComment
		mdIdentifier
		mdNumber
		mdLiteral
		mdQuote
		mdOperator
		mdType			' type has special rules
		mdSciNumber
		mdLineCont
	End Enum	

	Dim As modes mode = modes.mdNone
	Dim As ULongInt tlen = Len(*this._text)
	'Dim As ZString Ptr z = StrPtr(this._text)
	Dim As ZString Ptr z = this._text
	Dim As Byte this_char, id_loc		' id loc is where the id char is in the string _valid_id_chars
	Dim As ULongInt Ptr n = @this._char_index
	'Dim As Function(ByRef tkn As TToken Ptr) As ULongInt add_tk = @TTokenizer.add_token()  
	Dim As boolean is_letter

	Dim As TToken t
	Dim As TokenType tt
	Dim As Integer nono
	Dim As TCharString t_text		' token text
	
	While *n < tlen
		' 
		this_char = z[*n]
		'--------------
		If mode = mdNone Then
			If __TS.is_CRLF_char(this_char) = TRUE Then		' crlf character
				this._tokenize_eol(TRUE)
				Continue While
			
				
			ElseIf __TS.is_number_char(this_char) = TRUE Then
				' this is the start of a number
				mode = mdNumber
				this._tokenize_decimal_number()
				mode = mdNone

			ElseIf this_char = 38 Then		' & - this could be a hex binary oct number
				mode = mdNumber
					If this._is_number_string(t) = TRUE Then
						this.tokens.add_token(@t, this._char_line) 
					Else 
						' if & is not a number it could only be the concat operator
						 t = 0
						 t_text &= this_char
						 t = ttOperator
						 t.index.first_char = *n
						 t.index.last_char = *n
						 this.tokens.add_token(@t, this._char_line) 
					EndIf
				
				mode = mdNone
			
			ElseIf this_char = 34 Then	' " - this is the beginning of a quoted literal
				mode = mdQuote
					t_text &= this_char
					t = ttQuoteOpen
					t.index = *n
					this.tokens.add_token(@t, this._char_line) 
					
					t = 0
					t.index.first_char = *n + 1
					t = ttQuotedLiteral

			ElseIf this_char = 47 Then		' / - this could be the start of a multi line comment
				If *n + 1 < tlen Then
					If z[*n + 1] = 39	Then	' ' -
						mode = mdMultiComment
							this._tokenize_multi_comment()
						mode = mdNone 
					EndIf
				EndIf
			
			ElseIf __TS.is_operator(this_char) = TRUE Then	' operator must be checked after number because of &
				 t_text &= this_char
				 t = ttOperator
				 t.index.first_char = *n
				 t.index.last_char = *n
				 this.tokens.add_token(@t, this._char_line) 

			
			ElseIf this_char = 39 Then		' ' - this is a comment
				mode = mdComment
					this._tokenize_comment()
				mode = mdNone
			
			ElseIf __TS.is_identifier_char(this_char, id_loc) = TRUE Then	' identifier char
				If this_char = 95 Then
					If *n + 1 < tlen Then
						If __TS.is_identifier_char(z[*n + 1][0]) = TRUE Then 
							' this cannot be a line cont char and must be an identifier
							mode = mdIdentifier
							' this is a valid id char so this could be the start of keyword, macro, variable, or operator word
							this._tokenize_identifier()
							mode = mdNone
						Else
							' this is a line cont char
							' this can be safely replaced with a space
							' the compiler ignores everything to the right up to the crlf 
							' but I am going to store it as ttIgnoreText so we have to tokenize it
							mode = mdLineCont
							this._tokenize_line_cont()
							mode = mdNone
							Continue While 
						EndIf
					Else
						' to get here we have an _ that ends the code
						? "Illegal operator: _ found at end of code. "
						? __ERROR_INFO    
						Sleep
						End
					EndIf
				ElseIf id_loc <= 53 Then	' 53 is the last char before numbers
					mode = mdIdentifier
					' this is a valid id char so this could be the start of keyword, macro, variable, or operator word
					this._tokenize_identifier()
					
					mode = mdNone
				Else
					' we should never get here todo: remove this
					? "Error: We got to illegal initial identifier character in " 
					? __ERROR_INFO
					Sleep
					End 
				EndIf
			
			ElseIf this_char = 32 Then			' space 
				' just some whitespace to bypass 
				*n += 1
				Continue While 

			ElseIf this_char = 9 Then			' tab
				' just some whitespace to bypass 
				*n += 1
				Continue While 
			
			Else
				? "Error: Unhandled char '" & this_char & "'" & this._char_line 
				? __ERROR_INFO				 
				Sleep
				End
				
			EndIf
		ElseIf mode = mdQuote Then
			If this_char = 34 Then		' " - this is either the closing quote or an escaped quote
				If (*n + 1) < tlen Then
					*n += 1
					If z[*n][0] = 34 Then 
						' this is the second " of an escaped quote 
						' we ignore it and continue collecting the quoted literal
						*n += 1
						Continue While
					Else					
						' there is not a " here so we back up *n and get out of mode mdQuote
						*n -= 1
						t.index.last_char = *n - 1
						this.tokens.add_token(@t, this._char_line)  	' add the quoted literal 
						
						t = 0
						t_text &= this_char
						t.index = *n 
						t = ttQuoteClose
						this.tokens.add_token(@t, this._char_line) 
						mode = mdNone
						t = 0 
					EndIf
					
				Else	' thats it for code
					t.index.last_char = *n
					t = ttQuotedLiteral
					this.tokens.add_token(@t, this._char_line) 
					Exit While 
				EndIf
			ElseIf __TS.is_CRLF_char(this_char) = TRUE Then
				' this is the end of a quoted literal that has no closing quote
				t.index.last_char = *n - 1
				t = ttQuotedLiteral
				this.tokens.add_token(@t, this._char_line) 
				mode = mdNone				
				t = 0
			Else
				' this is any character except " and crlf
				t_text &= this_char
				If (*n + 1) >= tlen Then
					t.index.last_char = *n
					this.tokens.add_token(@t, this._char_line) 
					t = 0 
					Exit while
				EndIf
			EndIf 
		EndIf 
		'--------------
		*n += 1
	Wend
End Sub
Sub TTokenizer._tokenize_EOL(ByRef add_end_of_statement_token As boolean = TRUE)
	'
	Dim As ULongInt tlen = Len(*this._text)
	Dim As ZString Ptr z = this._text
	Dim As ULongInt Ptr n = @this._char_index
	Dim As Byte this_char = z[*n][0]
	'Dim As Function(ByRef token As TToken ptr, ByRef tl As Const TTokenizer) As ULongInt add_tk = @TTokenizer._add_token()
	Dim As TToken t
	Dim As TCharString t_text
	
	If this_char = 13 Then
		If *n + 1 < tlen Then
			*n += 1
			this_char = z[*n][0]
			If this_char <> 10 Then
				' there is something strange going on as this is a CR without the LF
				' maybe some kind of warning error system. 
				? "CR no LF: Chr(13) found in text without matching Chr(10)"
				? "Character location: "; this._char_line
				? __ERROR_INFO  
				Sleep
				End 
			Else
				' this is crlf 
				If add_end_of_statement_token = TRUE Then
					t = 0
					t_text &= 0
					t = ttStatementEnd
					this.tokens.add_token(@t, this._char_line) 
				EndIf
				*n += 1
			EndIf
		Else
			' somehow ended the text with a cr no lf
			' there is something strange going on as this is a CR without the LF
			' maybe some kind of warning error system. 
			? "CR no LF: Chr(13) found in text without matching Chr(10)"
			? "Line: " & this._char_line
		 	? __ERROR_INFO 
			Sleep
			End 
		EndIf
	ElseIf this_char = 10 Then
		' this is a line feed char
		If add_end_of_statement_token = TRUE Then
			t = 0
			t_text &= 0
			t = ttStatementEnd
			this.tokens.add_token(@t, this._char_line) 
		EndIf
		*n += 1
	EndIf 
	' if we made it here there was a legit line feed 
	this._char_line += 1
End Sub
Sub TTokenizer._tokenize_line_cont()
	'
	Dim As ULongInt tlen = Len(*this._text)
	Dim As ZString Ptr z = this._text
	Dim As ULongInt Ptr n = @this._char_index
	Dim As Byte this_char = z[*n][0]
	'Dim As Function(ByRef tkn As TToken Ptr) As ULongInt add_tk = @TTokenizer.add_token()  
	Dim As TToken t
	Dim As TCharString t_text

	' right now this_char = _ and we don't store that
	*n += 1
	t.index = *n		' both first and last char index = *n
	t.token_type = ttNone
	While *n < tlen
		this_char = z[*n][0]
		If __TS.is_CRLF_char(this_char) = TRUE Then
			this._tokenize_EOL(FALSE)
			Exit While
		EndIf
		t_text &= this_char
		t.index.last_char = *n
		t.token_type = ttIgnoreText
		*n += 1
	Wend
	If t.token_type <> ttNone Then
		this.tokens.add_token(@t, this._char_line) 
	EndIf
End Sub
Sub TTokenizer._tokenize_comment()
	'
	Dim As ULongInt tlen = Len(*this._text)
	Dim As ZString Ptr z = this._text
	Dim As ULongInt Ptr n = @this._char_index
	Dim As Byte this_char = z[*n][0]
	'Dim As Function(ByRef tkn As TToken Ptr) As ULongInt add_tk = @TTokenList.add_token()
	Dim As TCharString t_text  

	Scope 
		Dim As TToken t = TToken(this_char, ttSingleLineComment, *n, *n)
		this.tokens.add_token(@t, this._char_line) 
	End Scope 

	Dim As TToken t
	t = ttCommentText
	*n += 1
	t.index.first_char = *n
	While *n < tlen
		this_char = z[*n]
		If __TS.is_CRLF_char(this_char) = TRUE Then
			' must add line number
			'this._char_line += 1
			Exit While
		EndIf
		t_text &= z[*n][0]
		*n += 1
	Wend 
	'*n -= 1
	t.index.last_char = *n - 1
	this.tokens.add_token(@t, this._char_line) 
	'tokenize and add statement ender
	this._tokenize_eol(TRUE)

End Sub
Sub TTokenizer._tokenize_multi_comment()
	'
	Dim As ULongInt tlen = Len(*this._text)
	Dim As ZString Ptr z = this._text
	Dim As Byte this_char
	Dim As ULongInt Ptr n = @this._char_index
	'Dim As Function(ByRef tkn As TToken Ptr) As ULongInt add_tk = @TTokenList._add_token()  
	Dim As TCharString t_text
	Scope 
		Dim As TToken t = TToken(z, ttMultiLineCommentStart, *n, *n + 1)
		this.tokens.add_token(@t, this._char_line) 
	End Scope

	Dim As TToken t
	t = ttCommentText
	*n += 2
	t.index.first_char = *n
	While *n < tlen
		this_char = z[*n]
		If this_char = 39 Then	' ' - this could be the end of a multi line comment
			*n += 1
			If *n < tlen Then
				If z[*n] = 47 Then 	' / - this is the end of a multi line comment
					t.index.last_char = *n - 2
					this.tokens.add_token(@t, this._char_line) 
					Scope 
						Dim As TToken t = TToken(z, ttMultiLineCommentEnd, *n-1, *n)
						this.tokens.add_token(@t, this._char_line) 
					End Scope 
					Exit While 
				Else
					Continue While
				EndIf
			Else
				' we reached end of code prior to end of multi line comment
				' store the token, flag an error but don't stop
				t.index.last_char = *n - 1
				this.tokens.add_token(@t, this._char_line) 
				Scope 
					Dim As TToken t = TToken(z, ttMultiLineCommentEnd, *n-1, *n)
					this.tokens.add_token(@t, this._char_line) 
				End Scope 
				Return  
			EndIf
			
		ElseIf this_char = 47 Then 	' / - this could be the start of a nested multi comment
			If *n + 1< tlen Then
				If z[*n + 1][0] = 39 Then	' this is a nested multicomment
					this._tokenize_multi_comment()
				EndIf
			Else
				' we reached end of code prior to end of multi line comment
				' store the token, flag an error but don't stop
				t.index.last_char = *n - 1
				this.tokens.add_token(@t, this._char_line) 
				Scope 
					Dim As TToken t = TToken(z, ttMultiLineCommentEnd, *n-1, *n)
					this.tokens.add_token(@t, this._char_line) 
				End Scope 
				Return  
			EndIf
		EndIf
		t_text &= z[*n][0]
		*n += 1
	Wend

End Sub 
'Sub TTokenizer._dump_tokens()
'	'
'	For x As ULongInt = 1 To this._token_count
'		? *this.tokens(x)
'	Next
' 	
'End Sub
Function TTokenizer._is_binary_string(ByRef t As TToken) As boolean
	'
	Dim As ZString Ptr z = this._text
	Dim As Byte this_char
	Dim As ULongInt Ptr n = @this._char_index
	Dim As ULongInt tlen = Len(*this._text)
	Dim As ULongInt orig_n = *n		' store the char location in case we need to go back to it
	Dim As TCharString t_text

	*n += 1
	' only numbers 1 and 0 and suffixes
	While *n < tlen
		this_char = z[*n][0]
		t.index.last_char = *n
		If __TS.is_suffix_char(this_char, FALSE) = TRUE Then 
			' any suffix char is valid and ends the number 
			this._tokenize_suffix(t_text, t, this_char, FALSE) ' the false means its not hex 
			Return TRUE 
		ElseIf __TS.is_binary_char(this_char) = FALSE Then
			t.index.last_char = *n - 1
			Exit While  
		EndIf
		t_text &= this_char
		*n += 1
	Wend
	If Len(t_text) > 2 Then
		' we have found at least &bx where x = 1 or 0 or a suffix
		Return TRUE
	EndIf 

	' all we have found is &b
	t = 0
	*n = orig_n
	Return FALSE 
	
End Function
Function TTokenizer._is_oct_string(ByRef t As TToken) As boolean
	'
	Dim As ZString Ptr z = this._text
	Dim As Byte this_char
	Dim As ULongInt Ptr n = @this._char_index
	Dim As ULongInt tlen = Len(*this._text)
	Dim As ULongInt orig_n = *n		' store the char location in case we need to go back to it
	Dim As TCharString t_text

	*n += 1
	' only numbers  0 to 7 and suffixes 
	While *n < tlen
		this_char = z[*n][0]
		t.index.last_char = *n

		If __TS.is_suffix_char(this_char, FALSE) = TRUE Then 
			' any suffix char is valid and ends the number 
			this._tokenize_suffix(t_text, t, this_char, FALSE) ' the false means its not hex 
			Return TRUE 
		ElseIf __TS.is_oct_char(this_char) = FALSE Then
			t.index.last_char = *n - 1
			Exit While  
		EndIf	
		
		t_text &= this_char
		*n += 1
	Wend
	If Len(t_text) > 2 Then
		' we have found at least &Ox where x = 0 through 7 or a suffix
		Return TRUE
	EndIf 

	' all we have found is &O
	t = 0
	*n = orig_n
	Return FALSE 
End Function
Function TTokenizer._is_hex_string(ByRef t As TToken) As boolean
	'
	Dim As ZString Ptr z = this._text
	Dim As Byte this_char
	Dim As ULongInt Ptr n = @this._char_index
	Dim As ULongInt tlen = Len(*this._text)
	Dim As ULongInt orig_n = *n		' store the char location in case we need to go back to it
	Dim As TCharString t_text

	*n += 1
	' only numbers  0 to 9 and letters a to z and suffixes (not including def) 
	While *n < tlen
		this_char = z[*n][0]
		t.index.last_char = *n

		If __TS.is_suffix_char(this_char, TRUE) = TRUE Then	 
			this._tokenize_suffix(t_text, t, this_char, TRUE)
			Return TRUE 
		
		ElseIf __TS.is_hex_char(this_char) = FALSE Then
			*n -= 1
			t.index.last_char = *n
			Exit While  

		EndIf	  
		
		t_text &= this_char
		*n += 1
	Wend
	If Len(t_text) > 2 Then
		' we have found at least &Hx where x = 0 through 9 or a through e or a suffix
		Return TRUE
	EndIf 

	' all we have found is &h
	t = 0
	*n = orig_n
	Return FALSE 

End Function
Sub TTokenizer._tokenize_suffix(ByRef t_text As TCharString, ByRef t As TToken, ByRef this_char As Byte, ByRef is_hex As Const boolean = FALSE)
	' ! # D E F D! D# DF E! E# EF - floating point suffixes
	' % & L LL U u% u& UL ULL	- integer suffixes
	' ! # - hex float suffixes
	Dim As ULongInt tlen = Len(*this._text)
	Dim As ZString Ptr z = this._text
	Dim As ULongInt Ptr n = @this._char_index
	
	t_text &= this_char

	' single char float suffixes
	If this_char = 33 Then	' ! - single
		t = ttSingle
		Return
	ElseIf this_char = 35 Then		' # - double
		t = ttDouble
		Return
	ElseIf cbool((this_char Or 32) = 102) AndAlso is_hex = FALSE Then		' F - single
		t = ttSingle
		Return
	EndIf
	
	' single char int suffixes
	If this_char = 37 Then	' % - integer
		t = ttInteger
		Return
	ElseIf this_char = 38 Then 	' & - long
		t = ttLong
		Return
	EndIf
	
	' multi char suffixes

	Dim As Byte c = this_char Or 32	' all the multi suf. are letters so we lower case it
	Dim As boolean float_flag = FALSE
	
	If c = 100 then 	' d - double
		float_flag = TRUE
		t = ttDouble
	ElseIf c = 101 Then	' e - single
		float_flag = TRUE 
		t = ttSingle
	ElseIf c = 108 Then	' l - long
		t = ttLong
	ElseIf c = 117 Then	' u - uinteger
		t = ttUInteger
	Else
		' we should never get here
		Cls
		? "Invalid suffix char '" & this_char & "' in code at line " & this._char_line 
		? __ERROR_INFO  
		Sleep
		End
	EndIf
	
	*n += 1
	If *n < tlen Then
		c = z[*n][0]
		If float_flag = TRUE Then 
			' valid second suffixes are ! # F
			If c = 33 Then		' ! - single
				t_text &= c
				t = ttSingle
				Return
			ElseIf c = 35 Then		' # - double
				t_text &= c
				t = ttDouble
				Return
			ElseIf cbool((c Or 32) = 102) Then	' F - single
				t_text &= c
				t = ttSingle
				Return
			EndIf
			*n -= 1	' back it off
			Return
		Else
			' is this an integer second suffix LL u% u& UL ULL
			If (this_char Or 32)= 117 Then
				If c  = 37 Then 	' % - U% - integer
					t_text &= c
					t = ttInteger
					Return  
				ElseIf c = 38 Then	' & - U& - long
					t_text &= c
					t = ttLong
					Return 
				ElseIf (c Or 32) = 108 Then
					' UL or ULL
					t_text &= c
					t = ttULong
					*n += 1
					If *n < tlen Then
						If z[*n][0] = 108 Then
							t_text &= z[*n][0]
							t = ttULongInt
							Return
						Else
							*n -= 1
						EndIf  
					Else
						*n -= 1
					EndIf
					Return 
				Else
					' this is not a valid second suffix char
					Return
				EndIf
			Else
				' this_char has to be an L so the only valid second suffix is L 
				If (c Or 32) = 108 Then		' l - LL LongInt
					t_text &= c
					t = ttLongInt
					Return 
				EndIf 
			EndIf
		EndIf
		 					
	Else
		*n -= 1		' back it off
	EndIf
	
End Sub 	
Sub TTokenizer._tokenize_sci_note(ByRef t_text As TCharString, ByRef t As TToken, ByRef this_char As Byte)
	'
	Dim As ULongInt tlen = Len(*this._text)
	Dim As ZString Ptr z = this._text
	Dim As ULongInt Ptr n = @this._char_index
	t_text &= this_char
	t = ttSciNoteNumber
	
	If *n + 1 >= tlen Then
		' this is a sci note char tacked on the end of a number which makes it a precision suffix instead
		' thats it for the number then (store it and return)
		If this_char = 100 Then
			t = ttDouble
		Else
			t = ttSingle
		EndIf
		t.index.last_char = *n
		Return 
	EndIf
	
	*n += 1
	
	' valid characters at this point are numbers + - and sci suffixes								
	If __TS.is_plus_minus(z[*n][0]) = TRUE Then
		' this is + - 
		t_text &= z[*n][0]
		*n += 1
	ElseIf __TS.is_number_char(z[*n][0]) = FALSE  AndAlso __TS.is_sci_suffix(z[*n][0], 0) = FALSE Then
		' this is a number ending in a precision designated by the d or e we already stored
		If (this_char Or 32) = 100 Then
			t = ttDouble
		Else
			t = ttSingle
		EndIf   
		*n -= 1	' back up a character
		t.index.last_char = *n
		Return 
	EndIf

	' valid chars are now numbers and sci suffixes
	While *n < tlen
		this_char = z[*n][0]
		If __TS.is_sci_suffix(this_char, t.token_type) = TRUE Then
			' this is sci suffix and we end our number here
			t_text &= this_char
			t.index.last_char = *n
			Exit While 
		ElseIf __TS.is_number_char(this_char) = FALSE Then
			t.index.last_char = *n - 1
			Exit While
		EndIf 
		t_text &= this_char
		*n += 1
	Wend
End Sub
Sub TTokenizer._tokenize_decimal_number()
	'
	Dim As ULongInt tlen = Len(*this._text)
	Dim As ZString Ptr z = this._text
	Dim As Byte this_char
	Dim As ULongInt Ptr n = @this._char_index
	'Dim As Function(ByRef tkn As TToken Ptr) As ULongInt add_tk = @TTokenizer.add_token()  
	Dim As Boolean decimal_point_flag, is_letter
	Dim As TToken t
	Dim As TCharString t_text

	t = ttDecimalNumber
	t.index.first_char = *n
	t_text &= z[*n][0]
	
	*n += 1
	While *n < tlen
		this_char = z[*n][0]
		If __TS.is_number_char(this_char) = TRUE Then		' this is a number
			t_text &= this_char
			*n += 1
			Continue While
		ElseIf cbool(this_char = 46) AndAlso decimal_point_flag = FALSE Then		' decimal point
			t_text &= this_char
			*n += 1
			decimal_point_flag = TRUE 
			Continue While
		ElseIf __TS.is_sci_note(this_char) = TRUE Then 		' e/d - start of sci note exponent
   		this._tokenize_sci_note(t_text, t, this_char)
   		Exit While 

		ElseIf __TS.is_suffix_char(this_char, FALSE) = TRUE Then	
			this._tokenize_suffix(t_text, t, this_char, FALSE) 
			Exit While 
		Else	' this is not any kind of number character
			*n -= 1	' back up char index
			Exit While
		EndIf
	Wend
	
	t.index.last_char = *n
	'add_tk(@t)
	this.tokens.add_token(@t, this._char_line)
	
End Sub
Sub TTokenizer._tokenize_identifier()
'	'
	Dim As TToken t 
	Dim As ZString Ptr z = this._text
	Dim As ULongInt Ptr n = @this._char_index
	Dim As Byte this_char = z[*n][0]
	Dim As ULongInt tlen = Len(*this._text)
	'Dim As Function(ByRef token As TToken Ptr, ByRef tl As TTokenizer) As ULongInt add_tk = @TTokenizer._add_token()
	Dim As TCharString t_text
	
	t.index.first_char = *n
	t_text &= this_char
	
 	While *n + 1 < tlen 
		*n += 1 
		this_char = z[*n][0]
		If __TS.is_identifier_char(this_char) = TRUE Then		 
			t_text &= this_char
		Else
			*n -=1 
			Exit While
		EndIf
 	Wend		
	t.index.last_char = *n '- 1
	' at this point we have our identifier - we need to determine what it is
	If this.symbols.find_word(t_text) <> 0 Then
		' this is a reserved word
		t = ttReservedWord ' this is a reserved word
	Else
		t = ttIdentifier	' this is an identifier
	EndIf
	
	this.tokens.add_token(@t, this._char_line) 

	If get_low_case(t_text) = "enum" Then
		' this stores all occurences of non quoted, non commented, word 'enum' in an array
		t = ttEnum										' we are parsing enums so save time and index them (sort of)
		this.add_enum(this.tokens.get_last_item())		' the array will be filled with nodes that are the enum
	EndIf

	
End Sub
Function TTokenizer._is_number_string(ByRef  tkn As TToken) As boolean
	'
	Enum NumberModes
		nmNone
		nmBase10
		nmNonBase10
	End Enum
	
	Dim As ZString Ptr z = this._text
	Dim As Byte this_char
	Dim As ULongInt Ptr n = @this._char_index
	Dim As ULongInt tlen = Len(*this._text)
	'Dim As Function(ByRef tkn As TToken Ptr) As ULongInt add_tk = @TTokenizer.add_token()
	Dim As ULongInt orig_n = *n		' store the char location in case we need to go back to it
	Dim As TToken t
	Dim As TCharString t_text
	Dim As NumberModes mode = nmNone
	
	this_char = z[*n][0]
	If this_char = 38 Then		' & - this could be a hex, oct or bin number
		mode = nmNonBase10
		t.index.first_char = *n
		t_text &= this_char
		*n += 1
		If *n < tlen Then
			this_char = z[*n][0]
			this_char = this_char Or 32	' lower case me
			' valid characters for a number are now h, o, b, and chars 0 through 9 104 98 111
			If this_char = 98 Then 	' b
				t = ttBinNumber
				t_text &= this_char
				If this._is_binary_string(t) = TRUE Then
					 tkn = t
					Return TRUE
				EndIf
				*n = orig_n
				Return FALSE 
				
			ElseIf this_char = 104 Then	' h
				t = ttHexNumber
				t_text &= this_char
				If this._is_hex_string(t) = TRUE Then
					 tkn = t
					Return TRUE 
				EndIf
				*n = orig_n
				Return FALSE 
				
			ElseIf this_char = 111 Then 	' o 
				t = ttOctNumber
				t_text &= this_char
				If this._is_oct_string(t) = TRUE Then
					 tkn = t
					Return TRUE 
				EndIf
				*n = orig_n
				Return FALSE 
			Else
				' there is no valid follow up to this &, therefore this is not a number string
				 tkn = 0
				*n = orig_n
				Return FALSE
			EndIf 
		Else
			' *n > then length of text this is not a number
			 tkn = 0
			*n = orig_n
			Return FALSE
		EndIf 
		' this does not start with an &
		Cls
		? "not hex or binary or octal"
		Sleep
		End 
	EndIf

End Function
Function TTokenizer.load_file(ByRef file_name As String) As boolean
	'
	Dim As Integer fnum 
	fnum = FreeFile()
	If Open(file_name, For Input, As fnum) <> 0 Then
		Return FALSE 
	EndIf
		source_text = Input(Lof(fnum), fnum)
		this._text = StrPtr(source_text)
	Close fnum
	Return TRUE  
 	
End Function
