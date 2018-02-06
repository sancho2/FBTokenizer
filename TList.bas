Type TListNode
    As Any Ptr pData
    As TListNode Ptr pNext
    As TListNode Ptr pPrev
    Declare Property is_null_node () As boolean 
End Type
Property TListNode.is_null_node () As boolean
	'
	Return cbool(pPrev = 0)
End Property

Type TList 
	As TListNode Ptr null_node
	As Integer node_count
	
	Declare Constructor (ByRef node As TListNode Ptr = 0)
	Declare Destructor()
	Declare Function add_item(ByVal item As Any Ptr) As Any Ptr
	Declare Function add_head(ByVal item As Any Ptr) As Any Ptr
	Declare Function get_first_item() As TListNode Ptr
	Declare Function get_last_item() As TListNode Ptr
	Declare Function get_next_item(ByVal node As TListNode Ptr) As TListNode Ptr
	Declare Function get_prev_item(ByVal node As TListNode Ptr) As TListNode Ptr
	Declare Function get_data(ByVal node As TListNode Ptr) As Any Ptr
	Declare Function remove_item(ByVal node As TListNode Ptr) As TListNode Ptr
	Declare Sub remove_all()
	
End Type
Destructor TList()
	'
	Dim As TListNode Ptr node
	
	this.remove_all()
	DeAllocate(this.null_node)	
	this.null_node = 0
	?
	? "TList destructor called"
End Destructor
Function TList.add_head(ByVal item As Any Ptr) As Any Ptr
	Dim As TListNode Ptr pTemp
	
	If (this.null_node = 0) Then Return 0
	
	pTemp = this.null_node->pNext
	this.null_node->pNext = CAllocate(Len(TListNode))
	
	this.null_node->pNext->pPrev = this.null_node
	this.null_node->pNext->pData = item
	this.null_node->pNext->pNext = pTemp
	
	If (pTemp <> 0) Then
	  pTemp->pPrev = this.null_node->pNext
	End If
	
	Return item
End Function
Sub TList.remove_all()
	' does not remove null node
	Dim As TListNode Ptr node, pTemp
	 	
	node = this.get_last_item()
	While node <> 0 And node <> this.null_node
		pTemp = node->pPrev
		this.remove_item(node)
		node = pTemp
	Wend
End Sub

Function TList.remove_item(ByVal node As TListNode Ptr) As TListNode Ptr
	Dim As TListNode Ptr pPrev
	Dim As TListNode Ptr pNext
	
	If (node = 0) Then Return 0
	
	pPrev = node->pPrev
	pNext = node->pNext
	
	DeAllocate node
	node = 0
	
	If (pPrev <> 0) Then
	  pPrev->pNext = pNext
	End If
	If (pNext <> 0) Then
	  pNext->pPrev = pPrev
	End If
	
	this.node_count -= 1	
	Return pNext

End Function

Function TList.get_next_item(ByVal node As TListNode Ptr) As TListNode Ptr
    If (node = 0) Then Return 0

    Return node->pNext
End Function

Function TList.get_prev_item(ByVal node As TListNode Ptr) As TListNode Ptr
    ' can't do anything to a null list
    If (node = 0) Then Return 0
    ' this is needed for below
    If (node->pPrev = 0) Then Return 0
    '' since the list starts with a null node (pPrev and pData = 0),
    '' the first should be the one right after the real first.
    If (node->pPrev->pPrev = 0) Then Return 0

    Return node->pPrev
End Function
Function TList.get_first_item() As TListNode Ptr
    If (this.null_node = 0) Then Return 0

    Return this.null_node->pNext
End Function
Function TList.get_data(ByVal node As TListNode Ptr) As Any Ptr
    If (this.null_node = 0) Then Return 0

    Return node->pData
End Function
Function TList.get_last_item() As TListNode Ptr
    Dim As TListNode Ptr pTemp

    If (this.null_node = 0) Then Return 0

    pTemp = this.null_node
    While (pTemp->pNext <> 0)
        pTemp = pTemp->pNext
    Wend

    Return pTemp
End Function
Function TList.add_item(ByVal item As Any Ptr) As Any Ptr
	Dim As TListNode Ptr pTemp
	
	If (this.null_node = 0) Then Return 0 	'item
	
	pTemp = this.get_last_item()
	
	pTemp->pNext = CAllocate(Len(TListNode))
	pTemp->pNext->pPrev = pTemp
	pTemp->pNext->pData = item
	'? "wwwwwwwwww ";  pTemp->pNext->pData, item
	
	this.node_count += 1
	Return item
End Function
Constructor TList(ByRef node As TListNode Ptr = 0)
	' this doubles as default constructor
    Dim As TListNode Ptr pTemp
    this.null_node = CAllocate(Len(TListNode))
    node = this.null_node
End Constructor
'----------------------------------------------------------------------------------------------------------------
'----------------------------------------------------------------------------------------------------------------
'Sub dest_test()
'Dim As TList list
'Dim As String Ptr item ', test
'Dim As tlistnode Ptr test
'
'item = list.add_item(Callocate(Len(String)))
'*item = "hello"
'item = list.add_item(Callocate(Len(String)))
'*item = "world"
'item = list.add_item(Callocate(Len(String)))
'*item = "moo"
'
'test = list.get_first_item()
'item = list.get_data(test)
'? *item
'test = list.get_last_item()
'item = list.get_data(test)
'? *item
'test = list.get_prev_item(test)
'item = list.get_data(test)
'? *item
'
'test = list.get_prev_item(test)
'item = list.get_data(test)
'? *item
'
'test = list.get_prev_item(test)
'item = list.get_data(test)
'?"dd"; *item, item
'
'test = list.get_first_item()
'item = list.get_data(test)
'? "1 "; *item
'
'test = list.get_next_item(test)
'item = list.get_data(test)
'? "2 "; *item
'
'test = list.get_next_item(test)
'item = list.get_data(test)
'? "3 "; *item
'
'test = list.get_first_item()
'test = list.get_next_item(test)
'item = list.get_data(test)
'? "r "; *item
'
'? "c"; list.node_count
'list.remove_item(test)
'? "cc"; list.node_count
'
''? "rr "; *item
'test = list.get_first_item()
'item = list.get_data(test)
'? "1 "; *item
'
'test = list.get_next_item(test)
'item = list.get_data(test)
'? "2 "; *item
'list.remove_all(TRUE) 
'? "ccc"; list.node_count
''test = list.get_next_item(test)
''item = list.get_data(test)
''? "3 "; *item
'End Sub 
'
'dest_test()
'Sleep
'
''Dim As TListNode Ptr list, node
''Dim As Integer Ptr item
''Dim As String Ptr item
''list = ListCreate()
''item = ListAdd(list, CAllocate(Len(String)))
''*item = "4"
''item = ListAdd(list, CAllocate(Len(String)))
''*item = "123456789012345"
''item = 0 ' just to show it works
''node = ListGetFirst(list)
''
''While node <> 0
''    Print "found item"
''    item = ListGetData(node)
''    Print *item
''    node = ListRemove(node,1)
''Wend
''
''While Inkey$ = "" : Wend
''
'' CREATE
'
'' ADD, ADDHEAD
'
''
''
''' GETFIRST, GETLAST
''
''
''
''' GETNEXT, GETPREV
''
''
''' GETDATA
''
''
''' REMOVE, REMOVEALL
''
''
''
