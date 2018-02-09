'------------------------------------------------------------------------------------------
'  Symbols.bas version 1.03
'------------------------------------------------------------------------------------------
' #include once "Symbols.bas" 
'------------------------------------------------------------------------------------------
' assumes sorted list
'------------------------------------------------------------------------------------------
Enum AccessModifier 
	amNone 
	amPublic
	amPrivate
	amProtected 
End Enum

Operator *(ByVal am As AccessModifier) As String 
	'
	Select Case am
		Case amNone
			Return "amNone"
		Case amPublic
			Return "amPublic"
		Case amPrivate
			Return "amPrivate"
		Case amProtected
			Return "amProtected"
	End Select
End Operator

Enum TokenType
	ttNone
	ttBinNumber	
	ttComma		
	ttComment
	ttCommentText
	ttDecimalNumber
	ttDouble
	ttEnum
	ttEnumEnd
	ttFloatingPointNumber	
	ttHexNumber
	ttIdentifier
	ttIgnoreText
	ttInteger
	ttKeyword	
	ttLong
	ttLongInt
	ttMultiLineCommentEnd
	ttMultiLineCommentStart 	
	ttNumber		
	ttOctNumber
	ttOperator
	ttParameter	
	ttQuoteClose
	ttQuoteOpen
	ttQuotedLiteral
	ttReservedWord
	ttSciNoteNumber
	ttSingle
	ttSingleLineComment 	
	ttSpace
	ttSpecial	
	ttStatementEnd
	ttString
	ttType
	ttUInteger	
	ttULong
	ttULongInt
	
	ttUnknown   
End Enum
Operator *(ByVal tt As TokenType) As String
	Select Case Tt
		Case ttBinNumber
			Return "ttBinNumber"
		Case ttComma
			Return "ttComma"
		Case ttComment
			Return "ttComment"
		Case ttCommentText
			Return "ttCommentText"
		Case ttDecimalNumber
			Return "ttDecimalNumber"
		Case ttDouble
			Return "ttDouble"
		Case ttEnum
			Return "ttEnum"
		Case ttEnumEnd
			Return "ttEnumEnd"
		Case ttFloatingPointNumber
			Return "ttFloatingPointNumber"
		Case ttHexNumber
			Return "ttHexNumber"
		Case ttIdentifier
			Return "ttIdentifier"
		Case ttIgnoreText
			Return "ttIgnoreText"
		Case ttInteger
			Return "ttInteger"
		Case ttKeyword	
			Return "ttKeyword"
		Case ttLong
			Return "ttLong"
		Case ttLongInt
			Return "ttLongInt"
		Case ttMultiLineCommentStart
			Return "ttMultiLineCommentStart"
		Case ttMultiLineCommentEnd
			Return "ttMultiLineCommentEnd"
		Case ttNone
			Return "ttNone"
		Case ttNumber	
			Return "ttNumber"
		Case ttOctNumber
			Return "ttOctNumber"
		Case ttOperator
			Return "ttOperator"
		Case ttParameter
			Return "ttParameter"
		Case ttQuoteOpen
			Return "ttQuoteOpen"
		Case ttQuoteClose
			Return "ttQuoteClose"
		Case ttQuotedLiteral
			Return "ttQuotedLiteral"
		Case ttSciNoteNumber
			Return "ttSciNoteNumber"
		Case ttSingle
			Return "ttSingle"
		Case ttString
			Return "ttString"
		Case ttReservedWord
			Return "ttReservedWord"
		Case ttSingleLineComment
			Return "ttSingleLineComment"
		Case ttSpace
			Return "ttSpace"
		Case ttSpecial	
			Return "ttSpecial"
		Case ttStatementEnd
			Return "ttStatementEnd"
		Case ttType
			Return "ttType"
		Case ttUInteger
			Return "ttUInteger"
		Case ttULong
			Return "ttULong"
		Case ttULongInt
			Return "ttULongInt"
		Case ttUnknown
			Return "ttUnknown"
		
		Case Else
			Return "not in list"
	End Select
	
End Operator
'------------------------------------------------------------------------------------------
#Ifdef CRLF
	#Undef CRLF
#EndIf
#Define CRLF Chr(13) & Chr(10)
#Ifdef LF
	#Undef LF
#EndIf
#Define LF Chr(10)
'------------------------------------------------------------------------------------------
' FB keywords and macro names are stored in data statements (FBSymbols:) 
' The first data statement is the count 
#Include "SymbolsData.bas"		
'------------------------------------------------------------------------------------------
#Define __TS FBSymbols
Namespace FBSymbols
	Static Shared As String symbols(Any)
	Static Shared As UShort symbol_count
	Static Shared As UShort symbol_index(33 To 125)
	Static Shared As Byte index_count
												'123456789 123456789 123456789 123456789 123456789 12345
	Const _VALID_ID_CHARS ="ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz0123456789"

	Declare Sub read_symbols() 'Constructor 
	Declare Sub _dump_symbols()

	Declare Function is_plus_minus(ByRef char As Const Byte) As boolean
	Declare Function is_number_char(ByRef char As Const Byte) As boolean
	Declare Function is_sci_suffix(ByRef char As Const Byte, ByRef tt As TokenType) As boolean
	Declare Function is_sci_note(ByRef char As Const Byte) As boolean
	Declare Function is_suffix_char(ByRef char As Const Byte, ByRef is_hex As Const boolean) As boolean
	Declare Function is_keyword(ByRef keyword As Const String, ByRef word As Const string) As boolean 
	Declare Function is_binary_char(ByRef char As Const Byte) As boolean
	Declare Function is_oct_char(ByRef char As Const Byte) As boolean
	Declare Function is_hex_char(ByRef char As Const Byte) As boolean
	Declare Function is_CRLF_char(ByRef char As Const byte) As boolean
	Declare Function is_identifier_char(ByRef char As Const Byte, ByRef index As Byte = 0) As boolean
	Declare Function is_operator(ByRef char As Const Byte) As boolean
	Declare Function find_word(ByRef word As Const String) As UShort

	Declare Function is_access_modifier(ByRef word As Const String, ByRef modifier As AccessModifier = amNone) As boolean
	Declare Function is_end(ByRef word As Const String) As boolean
	Declare Function is_valid_char(ByRef char As Const Byte, ByRef list As Const String) As boolean
'------------------------------------------------------------------------------------------
	Sub dump_symbols()
		'
		For x As Integer = 1 To symbol_count
			? symbols(x)
		Next
	
	End Sub
	Sub read_symbols() Constructor
		'
		Dim As Byte char 
		Dim As string s
		Dim As Integer char_count
		Restore FreeBASIC_Symbols 
		Read symbol_count
		ReDim symbols(1 To symbol_count)
		For x As UShort = 1 To symbol_count
			Read s
			symbols(x) = s 
			If s[0] <> char Then  'AndAlso (s[0] Or 32) <> char Then
				char = s[0]
				char_count += 1
				If char >= 65 AndAlso char <=90 Then
					char or= 32
				EndIf
				symbol_index(char) = x
				index_count = char_count 
			EndIf
		Next   
	End Sub
	Function is_plus_minus(ByRef char As Const Byte) As boolean
		'
		return cbool(char = 43 OrElse char = 45) 
	End Function
	Function is_number_char(ByRef char As Const Byte) As boolean
		'
		If char >= 48 AndAlso char <= 57 Then
			Return TRUE
		EndIf
		Return FALSE
	End Function
	Function is_sci_suffix(ByRef char As Const Byte, ByRef tt As TokenType) As boolean
		 ' !, # 
		Dim As Byte c = char Or 32
		If c = 33  Then	' !
			tt = ttSingle
			Return TRUE
		ElseIf c = 35 Then 	' #
			tt = ttDouble
			Return TRUE
		EndIf
	End Function
	Function is_sci_note(ByRef char As Const Byte) As boolean
		'
		Dim As Byte c = char Or 32
		If c = 101 orelse c = 100 Then
			Return TRUE
		EndIf
		return FALSE 
		
	End Function
	Function is_suffix_char(ByRef char As Const Byte, ByRef is_hex As Const boolean) As boolean	
	' ! # D E F D! D# DF E! E# EF - floating point suffixes	
	' % & L LL U u% u& UL ULL	- integer suffixes
		If char < 33 OrElse char > 117 Then 
			Return FALSE
		EndIf 
		If char = 33 OrElse char = 35 OrElse char = 37 OrElse char = 38 Then				' !#%&
			Return TRUE 
		EndIf	
		Dim As Byte c = char Or 32	
		If c = 100 OrElse c = 101 OrElse c = 102 Then	' def
				If is_hex = FALSE Then 
					Return TRUE
				EndIf 
		EndIf   
		If c = 108 OrElse c = 117 Then	'ul
			Return TRUE 
		EndIf
		Return FALSE 
	End Function 
	Function is_keyword(ByRef keyword As Const String, ByRef word As Const string) As boolean
		'
		If (word[0] Or 32) <> (keyword[0] Or 32) OrElse Len(word) <> Len(keyword) Then Return FALSE
		'Sleep 
		If lcase(word) = LCase(keyword) Then
			Return TRUE 
		EndIf
		Return FALSE 
	End Function
	Function is_binary_char(ByRef char As Const Byte) As boolean
		'
		Return cbool(char >= 48 And char <= 49)
	End Function
	Function is_oct_char(ByRef char As Const Byte) As boolean
		'
		Return cbool(char >= 48 And char <= 55)
	End Function
	Function is_hex_char(ByRef char As Const Byte) As boolean
		'
		Dim As Byte c = char Or 32
		If c >= 97 AndAlso c <= 102 Then
			Return TRUE
		EndIf
		If is_number_char(char) = TRUE Then
			Return TRUE 
		EndIf
		Return FALSE 
	End Function
	Function is_CRLF_char(ByRef char As Const byte) As boolean
		'
		Return cbool(char = 10 OrElse char = 13) 
	End Function 
	Function is_identifier_char(ByRef char As Const Byte, ByRef index As Byte = 0) As boolean
		'
		index = InStr(_VALID_ID_CHARS, Chr(char))
		Return cbool(index) 	
		
	End Function  
	Function find_word(ByRef word As Const String) As UShort
		'
		Dim As Byte char
		Dim As UShort x
		char = word[0]
		char = IIf(char = 95, char, char Or 32)
		x = symbol_index(char)
		
		If x = 0 Then Return 0		' this letter is not in the index 
		 
		For n As UShort = x To symbol_count
			'If LCase(symbols(n)) = LCase(word) Then
			If symbols(n) = LCase(word) Then
				Return n
			'ElseIf (symbols(n)[0] Or 32) <> (word[0] Or 32) Then
			ElseIf symbols(n)[0] <> (word[0] Or 32) Then
				Exit For 
			EndIf 
		Next
		Return 0
	End Function
	Function is_operator(ByRef char As Const byte) As boolean
		'
		If char = 38 OrElse char = 96 OrElse char = 123 OrElse char = 125 Then
			Return TRUE 
		EndIf
	
		If char < 33 Then 
			Return FALSE
		EndIf
		
		' char >= 33
		If char < 37 Then
			Return TRUE
		EndIf
		
		' char either = 37 or >=39
		If char < 40 Then
			Return FALSE 
		EndIf
		
		' char >= 40 
		If char < 48 Then
			Return TRUE
		EndIf
	
		' char >49
		If char < 58 Then
			Return FALSE
		EndIf
		
		' char >= 58
		If char < 65 Then 
			Return TRUE 
		EndIf
	 	
	 	' char >=65
	 	If char < 91 Then 
	 		Return FALSE 
	 	EndIf
	 	
	 	' char >= 91
		If char < 95 Then 
			Return TRUE
		EndIf

		Return FALSE 
	End Function

	Function is_valid_char(ByRef char As Const Byte, ByRef list As Const String) As boolean
		'
		For x As Byte = 0 to Len(list) - 1
			If char = list[x] Then
				Return TRUE
			EndIf
		Next
		Return FALSE 
	End Function 
	Function is_end(ByRef word As Const String) As boolean
		'
		If (word[0] Or 32) <> 101 Then Return FALSE 
		If Len(word) = 3 AndAlso LCase(word) = "end" Then Return TRUE 
		Return FALSE		
	End Function
	Function is_access_modifier(ByRef word As Const String, ByRef modifier As AccessModifier = amNone) As boolean
		'
		modifier = amNone
		If (word[0] Or 32) <> 112 Then Return FALSE 			' all access mods begin with p
		If Len(word) = 6 AndAlso LCase(word) = "public" Then
			modifier = amPublic
			Return TRUE 
		EndIf
		If Len(word) = 7 AndAlso LCase(word) = "private" Then
			modifier = amPrivate
			Return TRUE 
		EndIf
		If Len(word) = 9 AndAlso lcase(word) = "protected" Then
			modifier = amProtected
			Return TRUE 
		EndIf
		
		Return FALSE 
		
	End Function

End Namespace 

'    1    2    3    4    5    6    7    8    9    0    1    2    3    4    5    6
'Data "!", "@", "#", "$", "^", "&", "*", "(", ")", "-", "+", "=", "{", "}", "[", "]"
''    1    1    1     2    2    2    2    2    2    2    2    2
''    7    8    9     0    1    2    3    4    5    6    7    8
'Data ":", ";", """", "'", "<", ",", ">", ".", "?", "/", "\", "_" 
