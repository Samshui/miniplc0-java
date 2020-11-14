package c0.tokenizer;

public enum TokenType {
	/** 空 */
	None,

	/** 关键字 */
	FN_KW,      // fn
	LET_KW,     // let
	CONST_KW,   // const
	AS_KW,      // as
	WHILE_KW,   // while
	IF_KW,      // if
	ELSE_KW,    // else
	RETURN_KW,  // return
	BREAK_KW,   // break
	CONTINUE_KW,// continue

    /*
        digit -> [0-9]
        UINT_LITERAL -> digit+

        escape_sequence -> '\' [\\"'nrt]
        string_regular_char -> [^"\\]
        STRING_LITERAL -> '"' (string_regular_char | escape_sequence)* '"'

        // 扩展 c0
        FLOAT_LITERAL -> digit+ '.' digit+ ([eE] digit+)?

        char_regular_char -> [^'\\]
        CHAR_LITERAL -> '\'' (char_regular_char | escape_sequence)* '\''
     */
	/** 字面量 */
	UINT_LITERAL,   // digit+
	STRING_LITERAL, // '"' (string_regular_char | escape_sequence)* '"'
	FLOAT_LITERAL,  // digit+ '.' digit+ ([eE] digit+)?
	CHAR_LITERAL,   // '\'' (char_regular_char | escape_sequence)* '\''

	/** 真值 */
	// 所有非 0 的布尔值都被视为 true，而 0 被视为 false。

	/** 标识符（包括类型） */
	IDENT,      // [_a-zA-Z] [_a-zA-Z0-9]*

	/** 运算符 */
	PLUS,     // +
	MINUS,    // -
	MUL,      // *
	DIV,      // /
	ASSIGN,   // =
	EQ,       // ==
	NEQ,      // !=
	LT,       // <
	GT,       // >
	LE,       // <=
	GE,       // >=
	L_PAREN,  // (
	R_PAREN,  // )
	L_BRACE,  // {
	R_BRACE,  // }
	ARROW,    // ->
	COMMA,    // ,
	COLON,    // :
	SEMICOLON,// ;

	/** 注释 */
	COMMENT,  // '//' regex(.*) '\n'

	/** STRING */
	STR,

	/** 文件尾 */
	EOF;

	@Override
	public String toString() {
		switch (this) {
			case None:
				return "NullToken";
			case FN_KW:
				return "FN";
			case LET_KW:
				return "LET";
			case CONST_KW:
				return "CONST";
			case AS_KW:
				return "AS";
			case WHILE_KW:
				return "WHILE";
			case IF_KW:
				return "IF";
			case ELSE_KW:
				return "ELSE";
			case RETURN_KW:
				return "RETURN";
			case BREAK_KW:
				return "BREAK";
			case CONTINUE_KW:
				return "CONTINUE";
			case UINT_LITERAL:
				return "UINT";
			case STRING_LITERAL:
				return "STRING";
			case FLOAT_LITERAL:
				return "FLOAT";
			case CHAR_LITERAL:
				return "CHAR";
			case IDENT:
				return "IDENT";
			case PLUS:
				return "PLUS";
			case MINUS:
				return "MINUS";
			case MUL:
				return "MUL";
			case DIV:
				return "DIV";
			case ASSIGN:
				return "ASSIGN";
			case EQ:
				return "EQ";
			case NEQ:
				return "NEQ";
			case LT:
				return "LT";
			case GT:
				return "GT";
			case LE:
				return "LE";
			case GE:
				return "GE";
			case L_PAREN:
				return "LPAREN";
			case R_PAREN:
				return "RPAREN";
			case L_BRACE:
				return "LBRACE";
			case R_BRACE:
				return "RBRACE";
			case ARROW:
				return "ARROW";
			case COMMA:
				return "COMMA";
			case COLON:
				return "COLON";
			case SEMICOLON:
				return "SEMICOLON";
			case COMMENT:
				return "COMMENT";
			default:
				return "InvalidToken";
		}
	}
}
