'------------------------------------------------------------------------------------------
'  TSymbols.bi
'------------------------------------------------------------------------------------------
' #include once "SymbolList.bi" 
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
Type __TS As  TSymbols
Type  TSymbols
	As String symbols(Any)
	As UShort symbol_count
	As UShort symbol_index(33 To 125)
	As Byte index_count
												'123456789 123456789 123456789 123456789 123456789 12345
	Const _VALID_ID_CHARS ="ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz0123456789"

	Declare Sub read_symbols()
	Declare Function find_word(ByRef word As Const String) As UShort
	Declare Function is_statement_separator (ByRef char As Byte) As boolean

	Declare Static Function is_access_modifier(ByRef word As Const String, ByRef modifier As AccessModifier = amNone) As boolean
	Declare Static Function is_keyword(ByRef keyword As Const String, ByRef word As Const string) As boolean 
	
	Declare Static Function is_suffix_char(ByRef char As Const Byte, ByRef is_hex As Const boolean) As boolean
	Declare Static Function is_CRLF_char(ByRef char As Const byte) As boolean
	Declare Static Function is_operator(ByRef char As Const Byte) As boolean
	Declare Static Function is_number_char(ByRef char As Const Byte) As boolean
	Declare Static Function is_binary_char(ByRef char As Const Byte) As boolean
	Declare Static Function is_hex_char(ByRef char As Const Byte) As boolean
	Declare Static Function is_oct_char(ByRef char As Const Byte) As boolean
	Declare Static Function is_identifier_char(ByRef char As Const Byte, ByRef index As Byte = 0) As boolean
	
	Declare Static Function is_end(ByRef word As Const String) As boolean

	Declare Static Function is_sci_note(ByRef char As Const Byte) As boolean
	Declare Static Function is_sci_suffix(ByRef char As Const Byte, ByRef tt As TokenType) As boolean
	
	Declare Static Function is_plus_minus(ByRef char As Const Byte) As boolean
	Declare Static Function is_valid_char(ByRef char As Const Byte, ByRef list As Const String) As boolean
	 
End Type
Function TSymbols.is_keyword(ByRef keyword As Const String, ByRef word As Const string) As boolean
	'
	If (word[0] Or 32) <> (keyword[0] Or 32) OrElse Len(word) <> Len(keyword) Then Return FALSE
	'Sleep 
	If get_low_case(word) = get_low_case(keyword) Then
		Return TRUE 
	EndIf
	Return FALSE 
End Function

Function TSymbols.is_end(ByRef word As Const String) As boolean
	'
	If (word[0] Or 32) <> 101 Then Return FALSE 
	If Len(word) = 3 AndAlso get_low_case(word) = "end" Then Return TRUE 
	Return FALSE		
End Function

Function TSymbols.is_access_modifier(ByRef word As Const String, ByRef modifier As AccessModifier = amNone) As boolean
	'
	modifier = amNone
	If (word[0] Or 32) <> 112 Then Return FALSE 			' all access mods begin with p
	If Len(word) = 6 AndAlso get_low_case(word) = "public" Then
		modifier = amPublic
		Return TRUE 
	EndIf
	If Len(word) = 7 AndAlso get_low_case(word) = "private" Then
		modifier = amPrivate
		Return TRUE 
	EndIf
	If Len(word) = 9 AndAlso get_low_case(word) = "protected" Then
		modifier = amProtected
		Return TRUE 
	EndIf
	
	Return FALSE 
	
End Function
Function TSymbols.is_identifier_char(ByRef char As Const Byte, ByRef index As Byte = 0) As boolean
	'
	index = InStr(_VALID_ID_CHARS, Chr(char))
	Return cbool(index) 	
	
End Function  

Function  TSymbols.is_CRLF_char(ByRef char As Const byte) As boolean
	'
	Return cbool(char = 10 OrElse char = 13) 
End Function 
Function  TSymbols.is_oct_char(ByRef char As Const Byte) As boolean
	'
	Return cbool(char >= 48 And char <= 55)
End Function

Function  TSymbols.is_binary_char(ByRef char As Const Byte) As boolean
	'
	Return cbool(char >= 48 And char <= 49)
End Function
Function  TSymbols.is_valid_char(ByRef char As Const Byte, ByRef list As Const String) As boolean
	'
	For x As Byte = 0 to Len(list) - 1
		If char = list[x] Then
			Return TRUE
		EndIf
	Next
	Return FALSE 
End Function 
Function  TSymbols.is_suffix_char(ByRef char As Const Byte, ByRef is_hex As Const boolean) As boolean
	' ! # D E F D! D# DF E! E# EF - floating point suffixes
	' % & L LL U u% u& UL ULL	- integer suffixes
	If char < 33 OrElse char > 117 Then 
		Return FALSE
	EndIf 
	If char = 33 OrElse _				' !
		char = 35 OrElse _				' #		 
		char = 37 OrElse _				' %
		char = 38 Then						' &
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
Function TSymbols.is_plus_minus(ByRef char As Const Byte) As boolean
	'
	return cbool(char = 43 OrElse char = 45) 

End Function
Function  TSymbols.is_sci_suffix(ByRef char As Const Byte, ByRef tt As TokenType) As boolean
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
Function  TSymbols.is_sci_note(ByRef char As Const Byte) As boolean
	'
	Dim As Byte c = char Or 32
	If c = 101 orelse c = 100 Then
		Return TRUE
	EndIf
	return FALSE 
	
End Function
Function  TSymbols.is_hex_char(ByRef char As Const Byte) As boolean
	'
	Dim As Byte c = char Or 32
	If c >= 97 AndAlso c <= 102 Then
		Return TRUE
	EndIf
	If __TS.is_number_char(char) = TRUE Then
		Return TRUE 
	EndIf
	Return FALSE 
End Function

Function  TSymbols.is_number_char(ByRef char As Const Byte) As boolean
	'
	If char >= 48 AndAlso char <= 57 Then
		Return TRUE
	EndIf
	Return FALSE
End Function

Function  TSymbols.is_operator(ByRef char As Const byte) As boolean
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

Sub  TSymbols.read_symbols()
	'
	Dim As Byte char 
	Dim As string s
	Dim As Integer n, max = 0, char_count
	Restore symbols 
	Read s
	Do
		n += 1
		If n > max Then 
			max += 100
			ReDim Preserve this.symbols(1 To max)
		EndIf
		this.symbols(n) = s
		
		If s[0] <> char AndAlso (s[0] Or 32) <> char Then
			char = s[0]
			char_count += 1
			If char >= 65 AndAlso char <=90 Then
				char or= 32
			EndIf
			this.symbol_index(char) = n
			this.index_count = char_count 
		EndIf
		
		Read s
	Loop While Not s = ""
	ReDim Preserve this.symbols(1 To n)
	this.symbol_count = n
End Sub
Function  TSymbols.find_word(ByRef word As Const String) As UShort
	'
	Dim As Byte char
	Dim As UShort x
	char = word[0]
	char = IIf(char = 95, char, char Or 32)
	x = this.symbol_index(char)
	
	If x = 0 Then Return 0		' this letter is not in the index 
	 
	For n As UShort = x To this.symbol_count
		If LCase(this.symbols(n)) = LCase(word) Then
			Return n
		ElseIf (this.symbols(n)[0] Or 32) <> (word[0] Or 32) Then
			Exit For 
		EndIf 
	Next
	Return 0
End Function
Sub dump_index(ByVal kl As  TSymbols)
	'
	For x As Integer = 33 To 125
		? x, chr(x), kl.symbol_index(x)
	Next
End Sub
'------------------------------------------------------------------------------------------
Operators:	' (multi character) 
Data "&=", "+=", "-=", "*=", "/=", "\=", "^=", "##", "()", "[]", "[]"
Data "<>", "<=", ">=", "->" 
Data "And", "Andalso", "And=" 
Data "Delete" 
Data "Eqv", "Eqv=" 
Data "For" 
Data "Imp", "Imp=" 
Data "Let", "Let()" 
Data "Mod", "Mod="   
Data "Next", "New", "Not"
Data "Or", "Orelse", "Or="
Data "Procptr" 
Data "Shl", "Shr", "Shl=", "Shr=", "Step", "Strptr"  
Data "VarPtr"  
Data "Xor", "Xor=" 
'------------------------------------------------------------------------------------------
' 28 single char operators 
Operators1:
Data 28

Symbols:
'                                                 1    1    1    1    1    1    1   
'    1    2    3    4    5    6    7    8    9    0    1    2    3    4    5    6
Data "!", "@", "#", "$", "^", "&", "*", "(", ")", "-", "+", "=", "{", "}", "[", "]"
'    1    1    1     2    2    2    2    2    2    2    2    2
'    7    8    9     0    1    2    3    4    5    6    7    8
Data ":", ";", """", "'", "<", ",", ">", ".", "?", "/", "\", "_" 
Data "__DATE__", "__DATE_ISO__", "__FB_ARGC__", "__FB_ARGV__"
Data "__FB_BACKEND__", "__FB_BIGENDIAN__", "__FB_BUILD_DATE__", "__FB_CYGWIN__", "__FB_DARWIN__"
Data "__FB_DEBUG__", "__FB_DOS__", "__FB_ERR__", "__FB_FPMODE__", "__FB_FPU__"
Data "__FB_FREEBSD__", "__FB_GCC__", "__FB_LANG__", "__FB_LINUX__", "__FB_MAIN__"
Data "__FB_MIN_VERSION__", "__FB_MT__", "__FB_NETBSD__", "__FB_OPENBSD__", "__FB_OPTION_BYVAL__"
Data "__FB_OPTION_DYNAMIC__", "__FB_OPTION_ESCAPE__", "__FB_OPTION_EXPLICIT__", "__FB_OPTION_GOSUB__", "__FB_OPTION_PRIVATE__"
Data "__FB_OUT_DLL__", "__FB_OUT_EXE__", "__FB_OUT_LIB__", "__FB_OUT_OBJ__", "__FB_PCOS__"
Data "__FB_SIGNATURE__", "__FB_SSE__", "__FB_UNIX__", "__FB_VECTORIZE__", "__FB_VER_MAJOR__"
Data "__FB_VER_MINOR__", "__FB_VER_PATCH__", "__FB_VERSION__", "__FB_WIN32__", "__FB_XBOX__"
Data "__FILE__", "__FILE_NQ__", "__FUNCTION__", "__FUNCTION_NQ__", "__LINE__"
Data "__PATH__", "__TIME__", "#ASSERT", "#DEFINE"
Data "#ELSE", "#ELSEIF", "#ENDIF", "#ENDMACRO", "#ERROR"
Data "#IF", "#IFDEF", "#IFNDEF", "#INCLIB", "#INCLUDE"
Data "#LANG", "#LIBPATH", "#LINE", "#MACRO", "#PRAGMA"
Data "#PRINT", "#UNDEF", "$DYNAMIC", "$INCLUDE"
Data "$STATIC", "$LANG", "ABS", "ABSTRACT", "ACCESS"
Data "ACOS", "ADD", "ALIAS", "ALLOCATE", "ALPHA"
Data "AND", "ANDALSO", "AND", "ANY", "APPEND"
Data "AS", "ASSERT", "ASSERTWARN", "ASC", "ASIN"
Data "ASM", "ATAN2", "ATN", "BASE"
Data "BEEP", "BIN", "BINARY", "BIT", "BITRESET"
Data "BITSET", "BLOAD", "BOOLEAN", "BSAVE", "BYREF"
Data "BYTE", "BYVAL", "CALL", "CALLOCATE", "CASE"
Data "CAST", "CBOOL", "CBYTE", "CDBL", "CDECL", "CHAIN"
Data "CHDIR", "CHR", "CINT", "CIRCLE", "CLASS"
Data "CLEAR", "CLNG", "CLNGINT", "CLOSE", "CLS"
Data "COLOR", "COMMAND", "COMMON", "CONDBROADCAST", "CONDCREATE"
Data "CONDDESTROY", "CONDSIGNAL", "CONDWAIT"
Data "CONST", "CONSTRUCTOR", "CONTINUE", "COS"
Data "CPTR", "CSHORT", "CSIGN", "CSNG", "CSRLIN"
Data "CUBYTE", "CUINT", "CULNG", "CULNGINT", "CUNSG"
Data "CURDIR", "CUSHORT", "CUSTOM", "CVD", "CVI"
Data "CVL", "CVLONGINT", "CVS", "CVSHORT", "DATA"
Data "DATE", "DATEADD", "DATEDIFF", "DATEPART", "DATESERIAL"
Data "DATEVALUE", "DAY", "DEALLOCATE", "DECLARE", "DEFBYTE"
Data "DEFDBL", "DEFINED", "DEFINT", "DEFLNG", "DEFLONGINT"
Data "DEFSHORT", "DEFSNG", "DEFSTR", "DEFUBYTE", "DEFUINT"
Data "DEFULONGINT", "DEFUSHORT", "DELETE", "DESTRUCTOR", "DESTRUCTOR"
Data "DIM", "DIR", "DO", "DO", "DOUBLE"
Data "DRAW", "DRAW STRING", "DYLIBFREE", "DYLIBLOAD", "DYLIBSYMBOL"
Data "ELSE", "ELSEIF", "ENCODING", "END", "ENDIF"
Data "END IF", "ENUM", "ENVIRON", "EOF"
Data "EQV", "ERASE", "ERFN", "ERL", "ERMN"
Data "ERR", "ERROR", "EXEC", "EXEPATH", "EXIT"
Data "EXP", "EXPLICIT", "EXPORT", "EXTENDS", "EXTERN", "END EXTERN"
Data "FIELD", "FILEATTR", "FILECOPY", "FILEDATETIME", "FILEEXISTS"
Data "FILELEN", "FIX", "FLIP", "FOR", "FOR"
Data "FORMAT", "FRAC", "FRE", "FREEFILE"
Data "FUNCTION", "GET", "GET #", "GETJOYSTICK", "GETKEY"
Data "GETMOUSE", "GOSUB", "GOTO", "HEX", "HIBYTE"
Data "HIWORD", "HOUR", "IF", "IIF", "IMAGECONVERTROW"
Data "IMAGECREATE", "IMAGEDESTROY", "IMAGEINFO", "IMP", "IMPLEMENTS"
Data "IMPORT", "INKEY", "INP", "INPUT"
Data "INPUT #", "INPUT$", "INSTR", "INSTRREV", "INT"
Data "INTEGER", "IS", "ISDATE", "ISREDIRECTED"
Data "KILL", "LBOUND", "LCASE", "LEFT", "LEN"
Data "LET", "LIB", "LINE", "LINE INPUT", "LINE INPUT #"
Data "LOBYTE", "LOC", "LOCAL", "LOCATE", "LOCK"
Data "LOF", "LOG", "LONG", "LONGINT", "LOOP"
Data "LOWORD", "LPOS", "LPRINT", "LSET", "LTRIM"
Data "MID", "MINUTE", "MKD", "MKDIR"
Data "MKI", "MKL", "MKLONGINT", "MKS", "MKSHORT"
Data "MOD", "MONTH", "MONTHNAME", "MULTIKEY", "MUTEXCREATE"
Data "MUTEXDESTROY", "MUTEXLOCK", "MUTEXUNLOCK", "NAKED", "NAME"
Data "NAMESPACE", "NEXT", "NEW", "NEXT"
Data "NOT", "NOW", "OBJECT", "OCT", "OFFSETOF"
Data "ON ERROR", "ON", "ONCE", "OPEN"
Data "OPEN COM", "OPEN CONS", "OPEN ERR", "OPEN LPT", "OPEN PIPE"
Data "OPEN SCRN", "OPERATOR", "OPTION()", "OPTION BASE", "OPTION BYVAL"
Data "OPTION DYNAMIC", "OPTION ESCAPE", "OPTION EXPLICIT", "OPTION GOSUB", "OPTION NOGOSUB"
Data "OPTION NOKEYWORD", "OPTION PRIVATE", "OPTION STATIC", "OR"
Data "ORELSE", "OUT", "OUTPUT", "OVERLOAD", "OVERRIDE"
Data "PAINT", "PALETTE", "PASCAL", "PCOPY", "PEEK"
Data "PMAP", "POINT", "POINTCOORD", "POINTER", "POKE"
Data "POS", "PRESERVE", "PRESET", "PRINT", "PRINT #"
Data "PRINT USING", "PRIVATE", "PRIVATE:", "PROCPTR", "PROPERTY"
Data "PROTECTED:", "PSET", "PSET", "PTR", "PUBLIC"
Data "PUBLIC:", "PUT", "PUT #", "RANDOM", "RANDOMIZE"
Data "READ", "READ WRITE", "REALLOCATE", "REDIM"
Data "REM", "RESET", "RESTORE", "RESUME", "RESUME NEXT", "remon"
Data "RETURN", "RGB", "RGBA", "RIGHT", "RMDIR"
Data "RND", "RSET", "RTRIM", "RUN", "SADD"
Data "SCOPE", "SCREEN", "SCREENCOPY", "SCREENCONTROL"
Data "SCREENEVENT", "SCREENINFO", "SCREENGLPROC", "SCREENLIST", "SCREENLOCK"
Data "SCREENPTR", "SCREENRES", "SCREENSET", "SCREENSYNC", "SCREENUNLOCK"
Data "SECOND", "SEEK", "SELECT CASE", "SETDATE"
Data "SETENVIRON", "SETMOUSE", "SETTIME", "SGN", "SHARED"
Data "SHELL", "SHL", "SHR", "SHORT", "SIN"
Data "SINGLE", "SIZEOF", "SLEEP", "SPACE", "SPC"
Data "SQR", "STATIC", "STDCALL", "STEP"
Data "STICK", "STOP", "STR", "STRIG"
Data "STRING", "STRPTR", "SUB", "SWAP"
Data "SYSTEM", "TAB", "TAN", "THEN", "THIS"
Data "THREADCALL", "THREADCREATE", "THREADDETACH", "THREADWAIT", "TIME"
Data "TIMESERIAL", "TIMEVALUE", "TIMER", "TO", "TRANS"
Data "TRIM", "TYPE", "TYPEOF"
Data "UBOUND", "UBYTE", "UCASE", "UINTEGER", "ULONG"
Data "ULONGINT", "UNION", "UNLOCK", "UNSIGNED", "UNTIL"
Data "USHORT", "USING", "VA_ARG", "VA_FIRST"
Data "VA_NEXT", "VAL", "VALLNG", "VALINT", "VALUINT"
Data "VALULNG", "VAR", "VARPTR", "VIEW PRINT", "VIEW"
Data "VIRTUAL", "WAIT", "WBIN", "WCHR", "WEEKDAY"
Data "WEEKDAYNAME", "WEND", "WHILE", "WHEX"
Data "WIDTH", "WINDOW", "WINDOWTITLE", "WINPUT", "WITH"
Data "WOCT", "WRITE", "WRITE #", "WSPACE"
Data "WSTR", "WSTRING", "XOR"
Data "YEAR", "ZSTRING" 
