'------------------------------------------------------------------------------------------
' Token.bi
' #include once "token.bi"
'------------------------------------------------------------------------------------------
Dim Shared As String source_text
Type TLocation
	As ULongInt first_char
	As ULongInt last_char
	Declare Operator Cast () As String
	Declare Operator Let(ByVal i As UByte)
End Type
Operator TLocation.Let(ByVal i As UByte)
	this.first_char = i
	this.last_char = i
End Operator
Operator TLocation.Cast() As String 
	Return Str(first_char) & ", " & Str(last_char)
End Operator
Operator Len(ByRef tl As Const TLocation ) As Integer
	Return tl.last_char - tl.first_char + 1
End Operator
'----------------------------------------------------------------------------------------------------------------------
Type TToken
	As TokenType token_type		' TokenType is an enum 
	As TLocation index
	As ULongInt token_index
	As ULongInt line_number 

	Declare Function get_length() As ULong		
	Declare Constructor
	Declare Constructor(ByVal tk_txt As Any Ptr, ByRef tk_type As Const TokenTYpe, _ 
						 ByRef tk_start As Const ULongInt, ByRef tk_end As Const ULongInt)  

	Declare Constructor(ByRef tk_txt As Const byte, ByRef tk_type As Const TokenTYpe, _ 
						 ByRef tk_start As Const ULongInt, ByRef tk_end As Const ULongInt)  

	Declare Operator Cast() As Byte 
	Declare Operator Cast() As String
	Declare Operator Let(ByVal i As Double)  
	Declare Operator Let(ByVal tt As TokenType)
End Type
Operator TToken.Cast() As Byte
	'
	Return source_text[this.index.first_char]
End Operator
Operator TToken.Cast() As String
	'
	Return Mid(source_text, this.index.first_char + 1, this.index.last_char - this.index.first_char + 1)
End Operator

Operator TToken.Let(ByVal tt As TokenType)
	'
	this.token_type = tt
End Operator
Operator TToken.Let(ByVal i As Double)
	'
	With This 
		If i = 0 Then
			.token_type = 0
			.index = 0		
		EndIf
	End With
	
End Operator 

Constructor TToken()
	'
	this.token_type = ttUnknown
End Constructor
Constructor TToken(ByRef tk_txt As Const Byte, ByRef tk_type As Const TokenType, _ 
						 ByRef tk_start As Const ULongInt, ByRef tk_end As Const ULongInt)
	'
	With This
		.token_type = tk_type
		.index = Type<TLocation>(tk_start, tk_end)
	End With

End Constructor
Constructor TToken(ByVal tk_txt As Any Ptr, ByRef tk_type As Const TokenType, _ 
						 ByRef tk_start As Const ULongInt, ByRef tk_end As Const ULongInt)
	'
	 With This
		.token_type = tk_type
		.index = Type<TLocation>(tk_start, tk_end)
	 End With
	 
End Constructor 						   

Function TToken.get_length() As ULong 
	'
	Return this.index.last_char - this.index.first_char + 1
End Function 
'----------------------------------------------------------------------------------------------------------------------
'----------------------------------------------------------------------------------------------------------------------
Type TTokenList extends TList
	'
	Declare Function _dump_tokens_to_file(ByRef file_name As String) As boolean
	Declare Function add_token(ByRef tkn As TToken Ptr, ByVal char_line As ULongInt) As ULongInt
	Declare Function get_text_section(ByRef index As Const TLocation) As String
	Declare Function get_char(ByRef index As Const TLocation) As Byte 
	Declare Constructor 
	Declare Destructor 
End Type
Constructor TTokenList()
	'
End Constructor
Function TTokenList.get_char(ByRef index As Const TLocation) As Byte
	'
	Return source_text[index.first_char]
End Function
Function TTokenList.add_token(ByRef tkn As TToken Ptr, ByVal char_line As ULongInt) As ULongInt 
	'
	Dim As TToken Ptr t 

	t = this.add_item(New TToken)
	*t = *tkn
	t->line_number = char_line
	t->token_index = this.node_count
	'	?"oooo "; t->token_index

	Return this.node_count

End Function    

Function TTokenList.get_text_section(ByRef index As Const TLocation) As String
	'
	Return Mid(source_text, index.first_char + 1, index.last_char - index.first_char + 1)
End Function
Function TTokenList._dump_tokens_to_file(ByRef file_name As String) As boolean
	' "C:\FbEdit1068\Projects\TAssist\forum.txt"
	' this should be moved to TTokenList
	Dim As Integer fnum
	fnum = FreeFile() 
	Dim As Tokentype  tt
	Dim As TToken Ptr t
	Dim As TListNode Ptr node
	
	If Open(file_name, For Output, As fnum) <> 0 Then
		Return FALSE
	EndIf
		
		node = this.get_first_item()
		While node <> 0
			't = Cast(TToken Ptr, this.get_data(node))
			t = Cast(TToken Ptr, node->pData)
			tt = t->token_type
			If tt = ttStatementEnd Then
				Print #fnum, "-:-", *tt
			Else
				Print #fnum, this.get_text_section(t->index), *tt	
			EndIf
			node = node->pNext
		Wend
 	
	Close fnum
	Return TRUE
End Function 

Destructor TTokenList()
	'
	Dim As TToken Ptr t
	Dim As TListNode Ptr n
	n = this.get_last_item
	While n <> 0 
			
		Delete Cast(TToken Ptr, n->pData)
		n->pData = 0
		n = n->pPrev
	Wend
	
	this.remove_all()
	?
	? "TTokenList destructor called"
End Destructor

