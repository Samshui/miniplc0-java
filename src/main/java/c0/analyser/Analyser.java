package c0.analyser;

import c0.error.AnalyzeError;
import c0.error.CompileError;
import c0.error.ErrorCode;
import c0.error.ExpectedTokenError;
import c0.error.TokenizeError;
import c0.instruction.Instruction;
import c0.table.SymbolTable;
import c0.tokenizer.Token;
import c0.tokenizer.TokenType;
import c0.tokenizer.Tokenizer;
import c0.util.Pos;

import java.util.*;

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
	SymbolTable symbolTable = new SymbolTable(140000);

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
	 * @param type  		符合类型
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

	// 新的添加函数：不检查重定义的问题



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
	 * @design
	 *
	 * E -> E B E 							|--> OE
	 * 		E 'as' ty						|--> ASE
	 * 		'-' E							|
	 * 		'(' E ')'						|
	 * 		IDENT							|
	 * 		IDENT '(' C ')'					|
	 * 		IDENT '=' E 					|
	 * 		UINT | DOUBLE | STRING | CHAR
	 *
	 *
	 * B -> 	 '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' | <= | >=
	 * C -> 	 E {',' E}
	 * T ->		 INT | VOID | DOUBLE
	 */

	/**
	 * 表达式
	 */
	private void analyseExpr() throws CompileError {
		System.out.println(peek().toString());
		// 取反表达式
		if (check(TokenType.MINUS)) analyseNegateExpr();
		// 括号表达式
		else if (check(TokenType.L_PAREN)) analyseGroupExpr();
		else if (check(TokenType.IDENT)) {
			Token tokenStorage = next();

			// 函数调用表达式
			if (check(TokenType.L_PAREN)) analyseCallExpr(tokenStorage);
			// 赋值表达式
			else if (check(TokenType.ASSIGN)) analyseAssignExpr(tokenStorage);
			// 标识符表达式
			else analyseIdentExpr(tokenStorage);
		}
		else if (check(TokenType.UINT_LITERAL) ||
				check(TokenType.DOUBLE_LITERAL) ||
				check(TokenType.STRING_LITERAL) ||
				check(TokenType.CHAR_LITERAL)) {
			// 字面量表达式
			analyseLiteralExpr();
		}
		else throw new AnalyzeError(ErrorCode.ExprERROR, peek().getStartPos());

		if (isBinaryOperator(peek())) {
			Token opToken = next();
			System.out.println(opToken.toString());

			// 检查是否有右值
			if (!checkMayExpr(peek()))
				throw new AnalyzeError(ErrorCode.IncompleteExpression, opToken.getEndPos());

			analyseExpr();
			System.out.println("运算符表达式");
			// TODO do something
		} else if (check(TokenType.AS_KW)) {
			expect(TokenType.AS_KW);
			expect(List.of(TokenType.INT_TY, TokenType.VOID_TY, TokenType.DOUBLE_TY));
			// TODO do something
			System.out.println("类型转换表达式");
		}
	}

	/**
	 * 运算符表达式
	 */
	private boolean isBinaryOperator(Token token) {
		return token.getTokenType() == TokenType.PLUS || token.getTokenType() == TokenType.MINUS ||
				token.getTokenType() == TokenType.MUL || token.getTokenType() == TokenType.DIV ||
				token.getTokenType() == TokenType.EQ || token.getTokenType() == TokenType.NEQ ||
				token.getTokenType() == TokenType.LT || token.getTokenType() == TokenType.GT ||
				token.getTokenType() == TokenType.LE || token.getTokenType() == TokenType.GE;
	}

	/**
	 * 取反表达式
	 */
	private void analyseNegateExpr() throws CompileError {
		// TODO 是否需要在此处保存Expr的值并取反后填入地址
		expect(TokenType.MINUS);
		analyseExpr();

		System.out.println("取反表达式");
	}

	/**
	 * 赋值表达式
	 */
	private void analyseAssignExpr(Token name) throws CompileError {
		expect(TokenType.ASSIGN);
		analyseExpr();

		// TODO 查表，如果表里面有，就判断类型，如果类型也合适就加指令，否则报错
		System.out.println("赋值表达式");
	}

	/**
	 * 函数调用表达式
	 */
	private void callParamList() throws CompileError {
		// TODO 读到一个表达式应该作为函数的参数，需要进行一些操作
		do { analyseExpr(); }
		while (nextIf(TokenType.COMMA) != null);
	}

	private void analyseCallExpr(Token funcName) throws CompileError {
		// TODO 查表（函数名称）
		expect(TokenType.L_PAREN);

		// 当有参数时
		if (!check(TokenType.R_PAREN)) {
			// TODO 获取参数后操作，除了标准库函数，其他函数在使用前必先声明
			callParamList();
		}
		expect(TokenType.R_PAREN);

		System.out.println("函数调用表达式");
	}

	/**
	 * 字面量表达式
	 */
	private void analyseLiteralExpr() throws CompileError {
		Token name = peek();
		expect(List.of(
				TokenType.UINT_LITERAL,
				TokenType.DOUBLE_LITERAL,
				TokenType.STRING_LITERAL
		));

		System.out.println("字面量表达式");
		// TODO 返回值
	}

	/**
	 * 标识符表达式
	 */
	private void analyseIdentExpr(Token name) throws CompileError {
		// TODO 按照name查表，返回Ident对应的值
		System.out.println("标识符表达式");
	}

	/**
	 * 括号表达式
	 */
	private void analyseGroupExpr() throws CompileError {
		expect(TokenType.L_PAREN);
		analyseExpr();
		expect(TokenType.R_PAREN);
		System.out.println("括号表达式");
	}

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
		// TODO 表达式如果有值，则丢弃值
		System.out.println("\t\t[表达式语句]--begin");
		analyseExpr();
		expect(TokenType.SEMICOLON);
		System.out.println("\t\t[表达式语句]--end\n");
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
			analyseExpr();
		}
		expect(TokenType.SEMICOLON);
	}

	private void isConstDeclStmt() throws CompileError {
		expect(TokenType.CONST_KW);
		expect(TokenType.IDENT);
		expect(TokenType.COLON);
		expect(List.of(TokenType.INT_TY, TokenType.DOUBLE_TY, TokenType.VOID_TY));
		expect(TokenType.ASSIGN);
		analyseExpr();
		expect(TokenType.SEMICOLON);
	}

	private void analyseDeclStmt() throws CompileError {
		System.out.println("\t\t[声明语句]--begin");
		if (check(TokenType.LET_KW)) isLetDeclStmt();
		else if (check(TokenType.CONST_KW)) isConstDeclStmt();
//		else throw new ExpectedTokenError(List.of(TokenType.LET_KW, TokenType.CONST_KW), peek());
		System.out.println("\t\t[声明语句]--end\n");
	}

	/**
	 * 控制流语句
	 */
	private void analyseIfStmt() throws CompileError {
		System.out.println("\t\t[IF语句]--begin");
		expect(TokenType.IF_KW);
		analyseExpr();
		analyseBlockStmt();

		if (check(TokenType.ELSE_KW)) {
			expect(TokenType.ELSE_KW);
			if (check(TokenType.L_BRACE)) analyseBlockStmt();
			else if (check(TokenType.IF_KW)) analyseIfStmt();
			else throw new ExpectedTokenError(List.of(TokenType.L_BRACE, TokenType.IF_KW), peek());
		}
		System.out.println("\t\t[IF语句]--end\n");
	}

	private void analyseWhileStmt() throws CompileError {
		System.out.println("\t\t[WHILE语句]--begin");
		expect(TokenType.WHILE_KW);
		analyseExpr();
		analyseBlockStmt();
		System.out.println("\t\t[WHILE语句]--end\n");
	}

	private void analyseReturnStmt() throws CompileError {
		System.out.println("\t\t[RETURN语句]--begin");
		expect(TokenType.RETURN_KW);
		if (!check(TokenType.SEMICOLON)) {
			analyseExpr();
		}
		expect(TokenType.SEMICOLON);
		System.out.println("\t\t[RETURN语句]--end\n");
	}

	/**
	 * 代码块
	 */
	private void analyseBlockStmt() throws CompileError {
		System.out.println("\t\t[代码块]--begin");
		expect(TokenType.L_BRACE);
		while (!check(TokenType.R_BRACE)) {
			analyseStmt();
		}
		expect(TokenType.R_BRACE);
		System.out.println("\t\t[代码块]--end");
	}

	/**
	 * 空语句
	 */
	private void analyseEmptyStmt() throws CompileError {
		expect(TokenType.SEMICOLON);
		System.out.println("\t\t[空语句]");
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
		do { analyseFunctionParam(); }
		while (nextIf(TokenType.COMMA) != null);
	}

	private void analyseFunction() throws CompileError {
		System.out.println("\t\t-----<<函数>>-----begin");
		expect(TokenType.FN_KW);
		expect(TokenType.IDENT);
		expect(TokenType.L_PAREN);

		if (!check(TokenType.R_PAREN))
			analyseFunctionParamList();

		expect(TokenType.R_PAREN);
		expect(TokenType.ARROW);
		System.out.println(peek().toStringAlt());
		expect(List.of(TokenType.VOID_TY, TokenType.INT_TY, TokenType.DOUBLE_TY));
		analyseBlockStmt();
		System.out.println("\t\t-----<<函数>>-----end\n");
	}

	/**
	 * 程序
	 */
	private void analyseProgramme() throws CompileError {
		while (check(TokenType.LET_KW) || check(TokenType.CONST_KW) || check(TokenType.FN_KW)) {
			while (check(TokenType.LET_KW) || check(TokenType.CONST_KW)) {
				analyseDeclStmt();
			}
			System.out.println("<声明结束，进入函数模块>");
			while (check(TokenType.FN_KW)) {
				analyseFunction();
			}
		}
	}

}