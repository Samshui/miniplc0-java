# C0-java

这里是 c0 大作业的 Java 版本

    /**
     * @design
     *
     * E -> E B E 							|--> OE
     * 		E 'as' ty						|--> ASE
     * 		'-' E							|
     * 		'(' E ')'						|
     * 		IDENT							|
     * 		IDENT '(' W ')'					|
     * 		IDENT '=' E 					|
     * 		UINT | DOUBLE | STRING | CHAR
     *
     *
     * B -> 	 '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' | <= | >=
     * W -> 	 E {',' E}
     * T ->		 INT | VOID | DOUBLE
     *
     * 改写：
     * E -> C ( ==|!=|<|>|<=|>= C )?
     * C -> T { +|- T }
     * T -> F { *|/ F }
     * F -> A ( as int_ty | double_ty )?
     * A -> (-)? I
     * I -> IDENT | UINT | DOUBLE | func_call | '('E')'
     *
     * E' -> IDENT = E
     *
     */

	
    /**
	 * @design
	 *
	 * S -> E ';' 								|
	 * 		'let' IDENT ':' ty ('=' E)? ';' 	|
	 * 		'const' IDENT ':' ty '=' E ';'		|
	 * 		'if' E BS ('else' (BS | IFS))? 		|--> IFS
	 * 		'while' E BS 						|
	 * 		'return' E? ';' 					|
	 * 		'{' S* '}' 							|--> BS
	 * 		';'
	 */