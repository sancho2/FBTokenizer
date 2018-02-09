'----------------------------------------------------------------------------------------------------------------------
' Tokenizer.bas Version 1.03
' #include once "Tokenizer.bas"
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

Dim Shared As String source_text		' source code text

'----------------------------------------------------------------------------------------------------------------------
#Include Once "TList.bas"
#include once "Symbols.bas"		
'----------------------------------------------------------------------------------------------------------------------
#Include once "token.bas"
'----------------------------------------------------------------------------------------------------------------------
'Common Shared As String source_text
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

Namespace Tokenizer
	
	Declare Function load_file(ByRef file_name As String) As boolean
	Declare Sub tokenize_source()
	Declare Sub _tokenize_EOL(ByRef add_end_of_statement_token As boolean = TRUE)		
	Declare Sub _tokenize_decimal_number()
	Declare Sub _tokenize_sci_note(ByRef t_text As TCharString, ByRef t As TToken, ByRef this_char As Byte)
	Declare Sub _tokenize_suffix(ByRef t_text As TCharString, ByRef t As TToken, ByRef this_char As Byte, ByRef is_hex As Const boolean = FALSE)
	Declare Function _is_number_string(ByRef token As TToken) As boolean
	Declare Function _is_binary_string(ByRef t As TToken) As boolean
	Declare Function _is_hex_string(ByRef t As TToken) As boolean
	Declare Function _is_oct_string(ByRef t As TToken) As boolean  		
	Declare Sub _tokenize_multi_comment()
	Declare Sub _tokenize_comment()
	Declare Sub _tokenize_identifier()
	Declare Sub _tokenize_line_cont()
	'	Declare Sub create_bas_file(ByRef file_name As String)

	Declare Function _add_enum(ByVal node As TListNode Ptr) As UShort  
	
	Static Shared As zString Ptr 		_text
	Static Shared As ULongInt    		_char_index
	Static Shared As ULongInt 			_char_line = 1
	Static Shared As UShort 			_char_col = 1
	Static Shared As UShort 			_enum_count
	Static Shared As TlistNode ptr 	_enum_nodes(Any)
	
	Static Shared As TTokenList 	tokens

End Namespace 
'----------------------------------------------------------------------------------------------------------------------
'#include "Enums.bas"	' this uses TTokenizer so it has to come after it
'----------------------------------------------------------------------------------------------------------------------

' this is out of wack and just this way for easy testing
#Include "Tokenizer.bi"
'----------------------------------------------------------------------------------------------------------------------
tokenizer.load_file("token.bas")
Tokenizer.tokenize_source()
Dim As TListNode Ptr node = tokenizer.tokens.get_item(19)
Dim As TToken Ptr tp = Cast(TToken Ptr, node->pData)
tokenizer.tokens._dump_tokens_to_file("moomoo.bas")
 'Sleep