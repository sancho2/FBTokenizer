# FBTokenizer
FreeBASIC Tokenizer
Read Me Notes:
-------------- 
This FreeBASIC project is an FB tokenizer. It steps through source code and creates tokens out of the various
symbols, identifiers, operators and keywords. 
It is for learning and testing purposes only. 
In order to try it out navigate to the bottom of Tokenizer.bas file and in the line:
tk.load_file("code_test.bas")
Change code_test.bas to a file containing the FreeBASIC code you want to tokenize. 
Below that line: 
tk.tokens._dump_tokens_to_file("moomoo.bas")
moomoo.bas should be changed to the name of an output file. Note that if the output file already exists it 
will be overwritten.  

- split into 5 modules
	- Tokenizer.bas	
		- main file 
		- contains declaration of TTokenizer class
	- Tokenizer.bi
		- included by Tokenizer.bas module
	`	- contains definition of TTokenizer class
	- TList.bas
		- list class included by Tokenizer.bas
		- used in more than one place
	- SymbolList.bi
		- FB specific symbol routines
		- included by Tokenizer.bas
	- Token2.bi
		- Contains declaration and definition of TToken class
		- included by TTokenizer class
- I started with TTokenizer as a class. I think I will move this to a namespace. There will 
  never be a case where more than one instance of TTokenizer need occur. This is a TODO
- Function get_low_case()converts the passed text string to lowercase. I found this to be faster 
  than using lcase() but only on WinXP. On Linux Ubuntu lcase() is faster. I may remove this 
  function.
- TCharString is a string type that I added. I began with token instances keeping a copy of the text. 
  This would have resulted in duplicating a large amount of text. I decided to keep only the source text
  in a shared variable source_text, and only keep the location of the first char. and last char. in a 
  token instance. 
  This change came as a revision. Previously I had overloaded the &=byte operator to append a char. byte to 
  the token instance's .text variable. I still need to know what the token text is as its being tokenized so
  I created TCharString type and overloaded that types &=byte operator. Whereever there is a variable name
  t_text that is the reworked system. In some places a t_text token string is created but never used. This 
  needs to be cleaned up. 

TTokenizer:
- get_text_section() is a function in more than one class. It simply gets a section of text from source_text.
  sub_text does the same thing. This will be removed. 
- create_bas_file() is a rudimentary method that writes the tokens to a bas file. Its definition is commented
  out due to latest revision. This will be fixed.
- load_file() is a method that loads source code file to be tokenized. This has no error checking and also
  needs to be cleaned up. 
- _enum_nodes keeps a list of pointers to any non-quoted, non-commented enum keywords
- _tokenize_source() is the main method. It steps through the source code text character by character 
  building tokens along the way. Instead of using strings I use the byte value of each character. Although
  this is more work it is faster. Whitespace, CRLF, and line continuations are not stored as tokens. There is 
  an 'end of statement' token added where CRLF or : create the end of a code statement.   
TToken and TTokenList:
- TToken is a class that defines a token 
- TTokenList is a class that defines a collection of TToken instances
- TTokenList extends the TList class. It uses its own add_token method to add to the TList node list. It
  uses the method _dump_tokens_to_file to output the token list to a file. Note that 'end of statement' is 
  using the symbol --:-- in this dump. 
TSymbols:
- This class is used to determine what kind of token we have found. 
- It has an alias __TS that is only used to shorten code typing (TSymbols had a long name previously).  
- This class uses 'string[0] or 32' in many places. This creates a lower case ascii letter out of the byte. 
- This module contains two enums: AccessModifiers and TokenType. TokenType has a large amount of enums. This
  enum is continually evovling and contains many symbols that are no longer used. Because of the constant 
  changes to this enum it became a problem to remember what value belonged to which enum symbol. I overloaded
  * (dereference) operator for both enums so that they would print the name of the enum symbol. This is just
  for debugging, but it is staying for now. 
- keywords and operators are read in from data statements. The list I am using at this point is incomplete. 
  This needs to be fixed. 
 
