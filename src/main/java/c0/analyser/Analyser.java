package c0.analyser;

import c0.error.*;
import c0.instruction.Instruction;
import c0.table.FuncTable;
import c0.table.SymbolTable;
import c0.tokenizer.Token;
import c0.tokenizer.TokenType;
import c0.tokenizer.Tokenizer;
import c0.util.Pos;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public final class Analyser {

	Tokenizer tokenizer;
	ArrayList<Instruction> instructions;

	/**
	 * 当前偷看的 token
	 */
	Token peekedToken = null;

	/**
	 * 符号表
	 */
	HashMap<String, SymbolEntry> _symbolTable = new HashMap<>();
	SymbolTable symbolTable = new SymbolTable();
	FuncTable funcTable = new FuncTable();

	/**
	 * 下一个变量的栈偏移
	 */
	int nextOffset = 0;

	public Analyser(Tokenizer tokenizer) {
		this.tokenizer = tokenizer;
		this.instructions = new ArrayList<>();
	}

	public List<Instruction> analyse() throws CompileError {
		analyseProgramme();
		next();
		return instructions;
	}

	/**
	 * 查看下一个 Token
	 *
	 * @return
	 * @throws TokenizeError
	 */
	private Token peek() throws TokenizeError {
		if (peekedToken == null) {
			peekedToken = tokenizer.nextToken();
		}
		return peekedToken;
	}

	/**
	 * 获取下一个 Token
	 *
	 * @return
	 * @throws TokenizeError
	 */
	private Token next() throws TokenizeError {
		if (peekedToken != null) {
			var token = peekedToken;
			peekedToken = null;
			return token;
		} else {
			return tokenizer.nextToken();
		}
	}

	/**
	 * 如果下一个 token 的类型是 tt，则返回 true
	 *
	 * @param tt
	 * @return
	 * @throws TokenizeError
	 */
	private boolean check(TokenType tt) throws TokenizeError {
		var token = peek();
		return token.getTokenType() == tt;
	}

	/**
	 * 判断一个TokenType是否可能为Expr开头
	 *
	 * @param token
	 * @return
	 * @throws TokenizeError
	 */
	private boolean checkMayExpr(Token token) throws TokenizeError {
		if (check(TokenType.UINT_LITERAL) ||
				check(TokenType.STRING_LITERAL) ||
				check(TokenType.DOUBLE_LITERAL) ||
				check(TokenType.CHAR_LITERAL) ||
				check(TokenType.IDENT) ||
				check(TokenType.PLUS) ||
				check(TokenType.L_PAREN)) {
			return true;
		}
		return false;
	}

	/**
	 * 判断一个TokenType是否可能为STMT开头
	 *
	 * @param token
	 * @return
	 * @throws TokenizeError
	 */
	private boolean checkMayStmt(Token token) throws TokenizeError {
		if (check(TokenType.LET_KW) ||
				check(TokenType.CONST_KW) ||
				check(TokenType.IF_KW) ||
				check(TokenType.WHILE_KW) ||
				check(TokenType.RETURN_KW) ||
				check(TokenType.L_BRACE) ||
				check(TokenType.SEMICOLON) ||
				checkMayExpr(token)) {
			return true;
		}
		return false;
	}


	/**
	 * 如果下一个 token 的类型是 tt，则前进一个 token 并返回这个 token
	 *
	 * @param tt 类型
	 * @return 如果匹配则返回这个 token，否则返回 null
	 * @throws TokenizeError
	 */
	private Token nextIf(TokenType tt) throws TokenizeError {
		var token = peek();
		if (token.getTokenType() == tt) {
			return next();
		} else {
			return null;
		}
	}

	/**
	 * 如果下一个 token 的类型是 tt，则前进一个 token 并返回，否则抛出异常
	 *
	 * @param tt 类型
	 * @return 这个 token
	 * @throws CompileError 如果类型不匹配
	 */
	private Token expect(TokenType tt) throws CompileError {
		var token = peek();
		if (token.getTokenType() == tt) {
			return next();
		} else {
			throw new ExpectedTokenError(tt, token);
		}
	}

	/**
	 * 如果下一个 token 的类型是 tts中的任何一个，则前进一个 token 并返回，否则抛出异常
	 *
	 * @param tts 类型List
	 * @return 这个 token
	 * @throws CompileError 如果类型不匹配
	 */
	private Token expect(List<TokenType> tts) throws CompileError {
		var token = peek();
		for (TokenType tt : tts) {
			if (token.getTokenType() == tt) {
				return next();
			}
		}
		throw new ExpectedTokenError(tts, token);
	}

	/**
	 * 获取下一个变量的栈偏移
	 *
	 * @return
	 */
	private int getNextVariableOffset() {
		return this.nextOffset++;
	}

	/**
	 * 添加一个符号
	 *
	 * @param name          名字
	 * @param isInitialized 是否已赋值
	 * @param isConstant    是否是常量
	 * @param type          符合类型
	 * @param curPos        当前 token 的位置（报错用）
	 * @throws AnalyzeError 如果重复定义了则抛异常
	 */
	private void addSymbol(String name, boolean isInitialized, boolean isConstant, int type, Pos curPos) throws AnalyzeError {
		if (this._symbolTable.get(name) != null) {
			throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
		} else {
			this._symbolTable.put(name, new SymbolEntry(name, isConstant, isInitialized, type, getNextVariableOffset(), null));
		}
	}


	/**
	 * 设置符号为已赋值
	 *
	 * @param name   符号名称
	 * @param curPos 当前位置（报错用）
	 * @throws AnalyzeError 如果未定义则抛异常
	 */
	private void initializeSymbol(String name, Pos curPos) throws AnalyzeError {
		var entry = this._symbolTable.get(name);
		if (entry == null) {
			throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
		} else {
			entry.setInitialized(true);
		}
	}

	/**
	 * 获取变量在栈上的偏移
	 *
	 * @param name   符号名
	 * @param curPos 当前位置（报错用）
	 * @return 栈偏移
	 * @throws AnalyzeError
	 */
	private int getOffset(String name, Pos curPos) throws AnalyzeError {
		var entry = this._symbolTable.get(name);
		if (entry == null) {
			throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
		} else {
			return entry.getOffset();
		}
	}

	/**
	 * 获取变量是否是常量
	 *
	 * @param name   符号名
	 * @param curPos 当前位置（报错用）
	 * @return 是否为常量
	 * @throws AnalyzeError
	 */
	private boolean isConstant(String name, Pos curPos) throws AnalyzeError {
		var entry = this._symbolTable.get(name);
		if (entry == null) {
			throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
		} else {
			return entry.isConstant();
		}
	}

	/**
	 * 判断token是否为二元比较符
	 *
	 * @param token
	 * @return
	 */
	private TokenType isBinaryComparer(Token token) {
		if (token.getTokenType() == TokenType.EQ ||
				token.getTokenType() == TokenType.NEQ ||
				token.getTokenType() == TokenType.LT ||
				token.getTokenType() == TokenType.GT ||
				token.getTokenType() == TokenType.LE ||
				token.getTokenType() == TokenType.GE) {
			return token.getTokenType();
		}
		return null;
	}

	/**
	 * 二元比较
	 *
	 * @return
	 * @throws CompileError
	 */
	private Object analysisBinaryCompare() throws CompileError {
		Object ret = null;
		if (isBinaryComparer(peek()) != null) {
			Token binComparer = next();
			analysisC(peek());
		}
		return ret;
	}

	/**
	 * E -> C ( ==|!=|<|>|<=|>= C )?
	 * E' -> IDENT = E
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private Object analysisE(Token front) throws CompileError {
		Object ret = null;

		// accept assign expression exist
		if (front == null) {
			if (peek().getTokenType() == TokenType.IDENT) {
				front = next();

				// assign expr
				if (peek().getTokenType() == TokenType.ASSIGN) {
					expect(TokenType.ASSIGN);
					analysisE(peek());
				}
				// just expr
				else {
					analysisC(front);
					analysisBinaryCompare();
				}
			} else {
				analysisC(peek());
				analysisBinaryCompare();
			}
		}

		// not accept assign expression
		else {
			analysisC(front);
			analysisBinaryCompare();
		}
		return ret;
	}


	/**
	 * E -> C ( ==|!=|<|>|<=|>= C )?
	 * C -> T { +|- T }
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private Object analysisC(Token front) throws CompileError {
		Object ret = null;

		analysisT(front);

		while (peek().getTokenType() == TokenType.PLUS ||
				peek().getTokenType() == TokenType.MINUS) {
			Token op = next();
			analysisT(peek());
		}
		return ret;
	}

	/**
	 * C -> T { +|- T }
	 * T -> F { *|/ F }
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private Object analysisT(Token front) throws CompileError {
		Object ret = null;

		analysisF(front);

		while (peek().getTokenType() == TokenType.MUL ||
				peek().getTokenType() == TokenType.DIV) {
			Token op = next();
			analysisF(peek());
		}

		return ret;
	}

	/**
	 * T -> F { *|/ F }
	 * F -> A ( as int_ty | double_ty )?
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private Object analysisF(Token front) throws CompileError {
		Object ret = null;

		analysisA(front);

		if (peek().getTokenType() == TokenType.AS_KW) {
			expect(TokenType.AS_KW);
			if (peek().getTokenType() == TokenType.INT_TY) {
				next();
			} else if (peek().getTokenType() == TokenType.DOUBLE_TY) {
				next();
			}
		}

		return ret;
	}

	/**
	 * F -> A ( as int_ty | double_ty )?
	 * A -> (-)? I
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private Object analysisA(Token front) throws CompileError {
		Object ret = null;

		Boolean nFlag = false;
		if (front.getTokenType() == TokenType.MINUS) {
			nFlag = true;
			next();
			front = peek();
		}
		analysisI(front);

		return ret;
	}


	/**
	 * A -> (-)? I
	 * I -> IDENT | UINT | DOUBLE | func_call | '('E')'
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private Object analysisI(Token front) throws CompileError {
		Object ret = null;

		if (front.getTokenType() == TokenType.IDENT) {
			if (peek().getTokenType() != TokenType.L_PAREN) next();

			// func call
			if (peek().getTokenType() == TokenType.L_PAREN) {
				expect(TokenType.L_PAREN);

				if (peek().getTokenType() != TokenType.R_PAREN) {
					analysisE(peek());
					while (nextIf(TokenType.COMMA) != null) {
						analysisE(peek());
					}
					expect(TokenType.R_PAREN);
				} else {
					expect(TokenType.R_PAREN);
				}
			}

			// pass -> ident
		} else if (front.getTokenType() == TokenType.UINT_LITERAL) {
			expect(TokenType.UINT_LITERAL);
			// pass -> uint
		} else if (front.getTokenType() == TokenType.DOUBLE_LITERAL) {
			expect(TokenType.DOUBLE_LITERAL);
			// pass -> double
		} else if (front.getTokenType() == TokenType.L_PAREN) {
			// group expr
			expect(TokenType.L_PAREN);
			analysisE(peek());
			expect(TokenType.R_PAREN);
		} else {
			throw new AnalyzeError(ErrorCode.ExprERROR, peek().getEndPos());
		}
		return ret;
	}


	/**
	 * 语句
	 */
	private void analyseStmt() throws CompileError {
		while (checkMayStmt(peek())) {
			if (check(TokenType.LET_KW) || check(TokenType.CONST_KW)) analyseDeclStmt();
			else if (check(TokenType.IF_KW)) analyseIfStmt();
			else if (check(TokenType.WHILE_KW)) analyseWhileStmt();
			else if (check(TokenType.RETURN_KW)) analyseReturnStmt();
			else if (check(TokenType.L_BRACE)) analyseBlockStmt();
			else if (check(TokenType.SEMICOLON)) analyseEmptyStmt();
			else analyseExprStmt();
		}
	}

	/**
	 * 表达式语句
	 */
	private void analyseExprStmt() throws CompileError {
		analysisE(null);
		expect(TokenType.SEMICOLON);
	}

	/**
	 * 声明语句
	 */
	private void isLetDeclStmt() throws CompileError {
		expect(TokenType.LET_KW);
		expect(TokenType.IDENT);
		expect(TokenType.COLON);
		expect(List.of(TokenType.INT_TY, TokenType.DOUBLE_TY, TokenType.VOID_TY));
		if (!check(TokenType.SEMICOLON)) {
			expect(TokenType.ASSIGN);
			analysisE(peek());
		}
		expect(TokenType.SEMICOLON);
	}

	private void isConstDeclStmt() throws CompileError {
		expect(TokenType.CONST_KW);
		expect(TokenType.IDENT);
		expect(TokenType.COLON);
		expect(List.of(TokenType.INT_TY, TokenType.DOUBLE_TY, TokenType.VOID_TY));
		expect(TokenType.ASSIGN);
		analysisE(peek());
		expect(TokenType.SEMICOLON);
	}

	private void analyseDeclStmt() throws CompileError {
		if (check(TokenType.LET_KW)) isLetDeclStmt();
		else if (check(TokenType.CONST_KW)) isConstDeclStmt();
	}

	/**
	 * 控制流语句
	 */
	private void analyseIfStmt() throws CompileError {
		expect(TokenType.IF_KW);
		analysisE(peek());
		analyseBlockStmt();

		if (check(TokenType.ELSE_KW)) {
			expect(TokenType.ELSE_KW);
			if (check(TokenType.L_BRACE)) analyseBlockStmt();
			else if (check(TokenType.IF_KW)) analyseIfStmt();
			else throw new ExpectedTokenError(List.of(TokenType.L_BRACE, TokenType.IF_KW), peek());
		}
	}

	private void analyseWhileStmt() throws CompileError {
		expect(TokenType.WHILE_KW);
		analysisE(peek());
		analyseBlockStmt();
	}

	private void analyseReturnStmt() throws CompileError {
		expect(TokenType.RETURN_KW);
		if (!check(TokenType.SEMICOLON)) {
			analysisE(peek());
		}
		expect(TokenType.SEMICOLON);
	}

	/**
	 * 代码块
	 */
	private void analyseBlockStmt() throws CompileError {
		expect(TokenType.L_BRACE);
		while (!check(TokenType.R_BRACE)) {
			analyseStmt();
		}
		expect(TokenType.R_BRACE);
	}

	/**
	 * 空语句
	 */
	private void analyseEmptyStmt() throws CompileError {
		expect(TokenType.SEMICOLON);
	}

	/**
	 * 函数
	 */
	private void analyseFunctionParam() throws CompileError {
		if (check(TokenType.CONST_KW)) expect(TokenType.CONST_KW);
		expect(TokenType.IDENT);
		expect(TokenType.COLON);
		expect(List.of(TokenType.VOID_TY, TokenType.INT_TY, TokenType.DOUBLE_TY));
	}

	private void analyseFunctionParamList() throws CompileError {
		do {
			analyseFunctionParam();
		}
		while (nextIf(TokenType.COMMA) != null);
	}

	private void analyseFunction() throws CompileError {
		expect(TokenType.FN_KW);
		expect(TokenType.IDENT);
		expect(TokenType.L_PAREN);

		if (!check(TokenType.R_PAREN))
			analyseFunctionParamList();

		expect(TokenType.R_PAREN);
		expect(TokenType.ARROW);
		expect(List.of(TokenType.VOID_TY, TokenType.INT_TY, TokenType.DOUBLE_TY));
		analyseBlockStmt();
	}

	/**
	 * 程序
	 */
	private void analyseProgramme() throws CompileError {
		while (check(TokenType.LET_KW) || check(TokenType.CONST_KW) || check(TokenType.FN_KW)) {
			while (check(TokenType.LET_KW) || check(TokenType.CONST_KW)) {
				analyseDeclStmt();
			}

			while (check(TokenType.FN_KW)) {
				analyseFunction();
			}
		}
	}
}