'----------------------------------------------------------------------------------------------------------------------
' TTokenizer.bas
' #include once "TTokenizer.bas"
'----------------------------------------------------------------------------------------------------------------------
Function get_low_case(ByRef s As Const String) As String 
	'
	Dim As Integer n = Len(s) - 1
	Dim As String k = Space(n + 1)	
	For x As Integer = 0 To n
		k[x] = s[x] Or 32
	Next 
	Return k
End Function
#Define __ERROR_INFO "File: " & __FILE__ & " Function: " & __FUNCTION__ " Line: " &  __LINE__
'----------------------------------------------------------------------------------------------------------------------
#Include Once "TList.bas"
#include once "SymbolList.bi"		
#Include once "token2.bi"
'----------------------------------------------------------------------------------------------------------------------
'----------------------------------------------------------------------------------------------------------------------
Type TCharString
	As String s
	Declare Operator &=(ByVal char As Const Byte)
	Declare Operator Cast() As String
	Declare Operator Let(ByRef rhs As Const String) 
End Type
Operator TCharString.Let(ByRef rhs As Const String)
	'
	this.s = rhs
End Operator
Operator TCharString.&=(ByVal char As Const Byte)
	'
	this.s &= " "
	this.s[Len(this.s) - 1] = char
End Operator
Operator TCharString.Cast() As String
	'
	Return this.s
End Operator

Type TTokenizer
	Public:
		Declare Constructor()
		Declare Constructor(ByRef source As Const String)
		Declare Destructor()
		
		Declare Function load_file(ByRef file_name As String) As boolean
		Declare Sub create_bas_file(ByRef file_name As String)
		Declare Function sub_text(ByVal first_char As ULongInt, ByVal last_char As ULongInt) As String
		Declare Function get_text_section(ByRef index As Const TLocation) As String  
	'Private:
		As zString Ptr _text
		'As ULongInt _token_count		' not using this - using node_count
		As ULongInt _char_index
		As ULongInt _char_line = 1

		As TlistNode ptr _enum_nodes(Any)
		As UShort _enum_count
		Declare Function add_enum(ByVal node As TListNode Ptr) As UShort  
		
		As TSymbols symbols
		As TTokenList tokens
		
		Declare Sub _tokenize_source()

		Declare Sub _tokenize_comment()
		Declare Sub _tokenize_multi_comment()
		Declare Sub _tokenize_decimal_number()
		Declare Sub _tokenize_identifier()
		Declare Sub _tokenize_line_cont()
		Declare Sub _tokenize_EOL(ByRef add_end_of_statement_token As boolean = TRUE)		
		
		Declare Sub _tokenize_sci_note(ByRef t_text As TCharString, ByRef t As TToken, ByRef this_char As Byte)
		Declare Sub _tokenize_int_suffix(ByRef t As TToken, ByRef this_char As Byte, ByRef is_letter As boolean)
		Declare Sub _tokenize_bin_suffix(ByRef t As TToken, ByRef this_char As Byte, ByRef is_letter As boolean)
		Declare Sub _tokenize_oct_suffix(ByRef t As TToken, ByRef this_char As Byte, ByRef is_letter As boolean) 
		Declare Sub _tokenize_suffix(ByRef t_text As TCharString, ByRef t As TToken, ByRef this_char As Byte, ByRef is_hex As Const boolean = FALSE)
		Declare Sub _get_token(ByVal char_index As Const Integer, ByRef token As TToken, ByRef token_type As TokenType)
		
		
		Declare Function _is_number_string(ByRef token As TToken) As boolean
		Declare Function _is_binary_string(ByRef t As TToken) As boolean
		Declare Function _is_hex_string(ByRef t As TToken) As boolean
		Declare Function _is_oct_string(ByRef t As TToken) As boolean  		
		Declare Function  _is_identifier_string(ByRef txt As String) As boolean
		
		Declare Sub _dump_tokens()
End Type
'----------------------------------------------------------------------------------------------------------------------
'----------------------------------------------------------------------------------------------------------------------
' this is out of wack and just this way for easy testing
#Include "Tokenizer.bi"
'----------------------------------------------------------------------------------------------------------------------
Function TTokenizer.get_text_section(ByRef index As Const TLocation) As String
	'
	Return Mid(*this._text, index.first_char + 1, index.last_char - index.first_char + 1)
End Function
Function TTokenizer.sub_text(ByVal first_char As ULongInt, ByVal last_char As ULongInt) As String
	'
	Return Mid(*this._text, first_char + 1, last_char - first_char + 1)
End Function
Destructor TTokenizer()
	'
	? "TTokenizer destructor called"
End Destructor
Function TTokenizer.add_enum(ByVal node As TListNode Ptr) As UShort
	'
	this._enum_count += 1
	ReDim Preserve this._enum_nodes(1 To this._enum_count) 
	this._enum_nodes(this._enum_count) = node
	Return this._enum_count
End Function
'-----------------------------------------------------------------------------------------------------------
' Main 
'-----------------------------------------------------------------------------------------------------------
Dim As TTokenizer tk
tk.load_file("code_test.bas")
tk._tokenize_source()
tk.tokens._dump_tokens_to_file("moomoo.bas")
Sleep
'-----------------------------------------------------------------------------------------------------------
' Main 
'-----------------------------------------------------------------------------------------------------------
