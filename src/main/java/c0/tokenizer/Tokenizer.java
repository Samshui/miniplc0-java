package c0.tokenizer;

import c0.error.TokenizeError;
import c0.error.ErrorCode;
import c0.util.Pos;

import java.util.ArrayList;
import java.util.Arrays;

public class Tokenizer {

	private StringIter it;

	public Tokenizer(StringIter it) {
		this.it = it;
	}

	// 这里本来是想实现 Iterator<Token> 的，但是 Iterator 不允许抛异常，于是就这样了
	/**
	 * 获取下一个 Token
	 *
	 * @return
	 * @throws TokenizeError 如果解析有异常则抛出
	 */
	public Token nextToken() throws TokenizeError {
		it.readAll();

		// 跳过之前的所有空白字符
		skipSpaceCharacters();

		if (it.isEOF()) {
			return new Token(TokenType.EOF, "", it.currentPos(), it.currentPos());
		}

		char peek = it.peekChar();
		if (Character.isDigit(peek)) {
			return lexUIntorFloat();
		} else if (Character.isAlphabetic(peek) || peek == '_') {
			return lexIdentOrKeyword();
		} else if (peek == '\"') {
			return lexStringLiteral();
		} else if (peek == '\'') {
			return lexCharLiteral();
		} else {
			return lexOperatorOrUnknown();
		}
	}

	/**
	 * 读取数字
	 *
	 * @return
	 * @throws TokenizeError
	 */
	private Token lexUIntorFloat() throws TokenizeError {
		Pos startPos,endPos;
		String numStorage = new String();

		numStorage += it.peekChar();

		startPos = new Pos(it.currentPos().row,it.currentPos().col);
		it.nextChar();

		char nextCH;
		while (Character.isDigit(nextCH = it.peekChar())) {
			numStorage += nextCH;
			it.nextChar();
		}

		/* float */
		if (nextCH == '.') {
			numStorage += nextCH;
			it.nextChar();

			while (Character.isDigit(nextCH = it.peekChar()) ||
					nextCH == 'e' ||
					nextCH == 'E') {
				numStorage += nextCH;
				it.nextChar();
			}

			endPos = new Pos(it.currentPos().row,it.currentPos().col);
			Double float_num = new Double(numStorage);

			Token token = new Token(TokenType.FLOAT_LITERAL, float_num, startPos, endPos);
			return token;
		}
		/* uint */
		else {
			endPos = new Pos(it.currentPos().row,it.currentPos().col);
			Integer int_num = new Integer(numStorage);

			Token token = new Token(TokenType.UINT_LITERAL, int_num, startPos, endPos);
			return token;
		}
	}

	/*
	 * 如何解决分析Ident和KW：如果while读到的为字母，那么可能是Ident也可能是一个关键词，按照最大吞噬，对于
	 * "if_else ***"，最大吞噬会直接把if_else吞掉，而非只吞到if就结束了，所以比较可行的一条路是：
	 * 1. 如果开头就是'_'，那妥妥是Ident
	 * 2. 如果开头为字母，那按照最大吞噬往下读（读的时候接受'_'和digit）
	 * 3. 读到的有'_'和digit，那必然是Ident
	 * 4. 如果平安读完只有字母，那就查KW表区分
	 */

	/**
	 * 读Ident和KW
	 *
	 * @return
	 * @throws TokenizeError
	 */
	private Token lexIdentOrKeyword() throws TokenizeError {
		Pos startPos,endPos;
		String storage = new String();
		boolean isIdent = false;		// 是否为Ident

		if (it.peekChar() == '_') isIdent = true;
		storage += it.peekChar();

		startPos = new Pos(it.currentPos().row,it.currentPos().col);
		it.nextChar();

		char nextCH;
		// 最大吞噬
		while (true) {
			// 字母：正常吞噬
			if (Character.isLetter(nextCH = it.peekChar())) {
				storage += nextCH;
				it.nextChar();
			}
			// 吞噬后，Ident置true
			else if (Character.isDigit(nextCH = it.peekChar()) || (nextCH = it.peekChar()) == '_') {
				isIdent = true;
				storage += nextCH;
				it.nextChar();
			}
			else break;
		}

		endPos = new Pos(it.currentPos().row,it.currentPos().col);

		if (isIdent) {
			Token token = new Token(TokenType.IDENT, storage, startPos, endPos);
			return token;
		} else {
			TokenType tokenType = searchKeywordTable(storage.toString());
			if (tokenType == TokenType.STR) {
				Token token = new Token(TokenType.IDENT, storage, startPos, endPos);
				return token;
			} else if (tokenType != TokenType.None){
				Token token = new Token(tokenType, storage, startPos, endPos);
				return token;
			} else {
				// TODO: 错误处理
				Token token = new Token(TokenType.None, "-none-", startPos, endPos);
				return token;
			}
		}
	}

	/**
	 * 查找关键词表
	 *
	 * @param str
	 * @return
	 */
	private TokenType searchKeywordTable(String str) {
		ArrayList<TokenType> keywordArray = new ArrayList<TokenType>(Arrays.asList(
				TokenType.FN_KW,
				TokenType.LET_KW,
				TokenType.CONST_KW,
				TokenType.AS_KW,
				TokenType.WHILE_KW,
				TokenType.IF_KW,
				TokenType.ELSE_KW,
				TokenType.RETURN_KW,
				TokenType.BREAK_KW,
				TokenType.CONTINUE_KW,
				TokenType.STR
		));

		ArrayList<String> keyWordTable = new ArrayList<String>(Arrays.asList(
				"fn",
				"let",
				"const",
				"as",
				"while",
				"if",
				"else",
				"return",
				"break",
				"continue",
				str
		));

		for (String s:keyWordTable) {
			if (str.equals(s)) {
				return keywordArray.get(keyWordTable.indexOf(s));
			}
		}

		// 无法匹配报异常
		return TokenType.None;
	}

	/**
	 * 读符号或返回无法识别
	 *
	 * @return
	 * @throws TokenizeError
	 */
	private Token lexOperatorOrUnknown() throws TokenizeError {
		switch (it.nextChar()) {
			case '+':
				return new Token(TokenType.PLUS, "+", it.previousPos(), it.currentPos());
			case '-':
				if (it.peekChar() == '>') {
					return new Token(TokenType.ARROW, "->", it.previousPos(), it.currentPos());
				} else {
					return new Token(TokenType.MINUS, "-", it.previousPos(), it.currentPos());
				}
			case '*':
				return new Token(TokenType.MUL, "*", it.previousPos(), it.currentPos());
			case '/':
				// TODO
				if (it.peekChar() == '/') {
					skipComment();
				} else {
					return new Token(TokenType.DIV, "/", it.previousPos(), it.currentPos());
				}
			case '=':
				if (it.peekChar() == '=') {
					return new Token(TokenType.EQ, "==", it.previousPos(), it.currentPos());
				} else {
					return new Token(TokenType.ASSIGN, "=", it.previousPos(), it.currentPos());
				}
			case '!':
				if (it.peekChar() == '=') {
					return new Token(TokenType.NEQ, "!=", it.previousPos(), it.currentPos());
				} else {
					throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
				}
			case '<':
				if (it.peekChar() == '=') {
					return new Token(TokenType.LE, "<=", it.previousPos(), it.currentPos());
				} else {
					return new Token(TokenType.LT, "<", it.previousPos(), it.currentPos());
				}
			case '>':
				if (it.peekChar() == '=') {
					return new Token(TokenType.GE, ">=", it.previousPos(), it.currentPos());
				} else {
					return new Token(TokenType.GT, ">", it.previousPos(), it.currentPos());
				}
			case '(':
				return new Token(TokenType.L_PAREN, "(", it.previousPos(), it.currentPos());
			case ')':
				return new Token(TokenType.R_PAREN, ")", it.previousPos(), it.currentPos());
			case '{':
				return new Token(TokenType.L_BRACE, "{", it.previousPos(), it.currentPos());
			case '}':
				return new Token(TokenType.R_BRACE, "}", it.previousPos(), it.currentPos());
			case ',':
				return new Token(TokenType.COMMA, ",", it.previousPos(), it.currentPos());
			case ':':
				return new Token(TokenType.COLON, ":", it.previousPos(), it.currentPos());
			case ';':
				return new Token(TokenType.SEMICOLON, ";", it.previousPos(), it.currentPos());
			default:
				// 不认识这个输入，摸了
				throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
		}
	}

	/**
	 * 读字符串常量
	 *
	 * @return
	 * @throws TokenizeError
	 */
	private Token lexStringLiteral() throws TokenizeError {
		Pos startPos,endPos;
		startPos = new Pos(it.currentPos().row,it.currentPos().col);

		it.nextChar();

		char nextCH;
		String storage = new String();

		while ((nextCH = it.peekChar()) != '"' ) {
			// 字符串常量的两头引号对不上号
			if (it.isEOF()) {
				System.exit(1);
			}

			// 判断转义序列 escape_sequence -> '\' [\\"'nrt]
			if (nextCH == '\\') {
				it.nextChar();

				if ((nextCH = it.peekChar()) == '\\') storage += '\\';
				else if (nextCH == '\'') storage += '\'';
				else if (nextCH == '\"') storage += '\"';
				else if (nextCH == 'n') storage += '\n';
				else if (nextCH == 't') storage += '\t';
				else if (nextCH == 'r') storage += '\r';
				else System.exit(1);
			} else {
				storage += nextCH;
			}

			it.nextChar();
		}

		// 跳过尾部的“
		it.nextChar();
		endPos = new Pos(it.currentPos().row,it.currentPos().col);

		return new Token(TokenType.STRING_LITERAL, storage, startPos, endPos);
	}

	/**
	 * 读字符常量
	 *
	 * @return
	 * @throws TokenizeError
	 */
	private Token lexCharLiteral() throws TokenizeError {
		Pos startPos,endPos;
		startPos = new Pos(it.currentPos().row,it.currentPos().col);

		it.nextChar();
		Character storage = null;

		char nextCH;
		if (it.peekChar() == '\\') {
			it.nextChar();

			if ((nextCH = it.peekChar()) == '\\') storage = '\\';
			else if (nextCH == '\'') storage = '\'';
			else if (nextCH == '\"') storage = '\"';
			else if (nextCH == 'n') storage = '\n';
			else if (nextCH == 't') storage = '\t';
			else if (nextCH == 'r') storage = '\r';
			else System.exit(1);

			it.nextChar();
		} else if (it.peekChar() != '\''){
			storage = it.peekChar();
			it.nextChar();
		} else System.exit(1);

		if (it.peekChar() == '\'') {
			it.nextChar();
			endPos = new Pos(it.currentPos().row,it.currentPos().col);
			return new Token(TokenType.CHAR_LITERAL, storage, startPos, endPos);
		} else {
			System.exit(1);
			// 此处如果不返回会报错
			return null;
		}
	}

	/**
	 * 跳过空白符
	 */
	private void skipSpaceCharacters() {
		while (!it.isEOF() && Character.isWhitespace(it.peekChar())) {
			it.nextChar();
		}
	}

	/**
	 * 跳过注释
	 */
	private void skipComment() {
		while (it.peekChar() != '\n') {
			it.nextChar();
		}
		it.nextChar();
		skipSpaceCharacters();
	}
}