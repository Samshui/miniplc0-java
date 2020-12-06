package c0.analyser;

import c0.Entry.SymbolEntry;
import c0.error.*;
import c0.instruction.Instruction;
import c0.instruction.Operation;
import c0.table.Table;
import c0.tokenizer.Token;
import c0.tokenizer.TokenType;
import c0.tokenizer.Tokenizer;
import c0.util.Pos;
import c0.util.SymbolType;

import java.util.ArrayList;
import java.util.List;

public final class Analyser {

	Tokenizer tokenizer;
	ArrayList<Instruction> tmpInstructions;
	ArrayList<Instruction> instructions;

	/**
	 * 当前偷看的 token
	 */
	Token peekedToken = null;

	/**
	 * 符号表
	 */
	Table table = new Table();

	/**
	 * 下一个变量的栈偏移
	 */
	int nextOffset = 0;

	/**
	 * 当前深度，受到程序的影响，进入块后deep+1，退出-1
	 */
	int deep = 1;

	public Analyser(Tokenizer tokenizer) {
		this.tokenizer = tokenizer;
		this.tmpInstructions = new ArrayList<>();
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
	private List<Instruction> analysisE(Token front) throws CompileError {
		List<Instruction> instructions = new ArrayList<>();

		/* 允许赋值表达式产生 */
		if (front == null) {
			if (peek().getTokenType() == TokenType.IDENT) {
				front = next();

				/* 赋值表达式 */
				if (peek().getTokenType() == TokenType.ASSIGN) {
					expect(TokenType.ASSIGN);
					analysisE(peek());

				}
				/* 非赋值表达式 */
				else {
					analysisC(front);
					analysisBinaryCompare();
				}
			} else {
				analysisC(peek());
				analysisBinaryCompare();
			}
		}

		/* 不允许产生赋值表达式 */
		else {
			analysisC(front);
			analysisBinaryCompare();
		}

		return instructions;
	}


	/**
	 * E -> C ( ==|!=|<|>|<=|>= C )?
	 * C -> T { +|- T }
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private List<Instruction> analysisC(Token front) throws CompileError {
		List<Instruction> instructions = new ArrayList<>();

		analysisT(front);

		while (peek().getTokenType() == TokenType.PLUS ||
				peek().getTokenType() == TokenType.MINUS) {
			Token op = next();
			analysisT(peek());
		}
		return instructions;
	}

	/**
	 * C -> T { +|- T }
	 * T -> F { *|/ F }
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private List<Instruction> analysisT(Token front) throws CompileError {
		List<Instruction> instructions = new ArrayList<>();

		analysisF(front);

		while (peek().getTokenType() == TokenType.MUL ||
				peek().getTokenType() == TokenType.DIV) {
			Token op = next();
			analysisF(peek());
		}

		return instructions;
	}

	/**
	 * T -> F { *|/ F }
	 * F -> A ( as int_ty | double_ty )?
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private List<Instruction> analysisF(Token front) throws CompileError {
		List<Instruction> instructions = new ArrayList<>();

		analysisA(front);

		if (peek().getTokenType() == TokenType.AS_KW) {
			expect(TokenType.AS_KW);
			if (peek().getTokenType() == TokenType.INT_TY) {
				next();
			} else if (peek().getTokenType() == TokenType.DOUBLE_TY) {
				next();
			}
		}

		return instructions;
	}

	/**
	 * F -> A ( as int_ty | double_ty )?
	 * A -> (-)? I
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private List<Instruction> analysisA(Token front) throws CompileError {
		List<Instruction> instructions = new ArrayList<>();

		Boolean nFlag = false;
		if (front.getTokenType() == TokenType.MINUS) {
			nFlag = true;
			next();
			front = peek();
		}
		analysisI(front);

		return instructions;
	}


	/**
	 * A -> (-)? I
	 * I -> IDENT | UINT | DOUBLE | STRING | func_call | '('E')'
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 *
	 * I是终结符等的分析，调用到此处时，
	 */
	private List<Instruction> analysisI(Token front) throws CompileError {
		List<Instruction> instructions = new ArrayList<>();

		if (front.getTokenType() == TokenType.IDENT) {
			if (peek().getTokenType() != TokenType.L_PAREN) {
				// 单句修改变量类型
				if (peek().getTokenType() == TokenType.AS_KW) {
					next();
					expect(List.of(TokenType.INT_TY, TokenType.DOUBLE_TY));
				} else {
					next();
				}
			}

			// func call
			if (peek().getTokenType() == TokenType.L_PAREN) {
				expect(TokenType.L_PAREN);

				if (peek().getTokenType() != TokenType.R_PAREN) {
					analysisE(peek());
					while (nextIf(TokenType.COMMA) != null) analysisE(peek());
					expect(TokenType.R_PAREN);
				} else {
					expect(TokenType.R_PAREN);
				}
			}

		} else if (front.getTokenType() == TokenType.UINT_LITERAL) {
			Token getUint = expect(TokenType.UINT_LITERAL);
			Integer i = (Integer) getUint.getValue();
			long iTOl = i;

			// add PUSH uint
			instructions.add(new Instruction(Operation.PUSH, iTOl));
		} else if (front.getTokenType() == TokenType.DOUBLE_LITERAL) {
			Token getDouble = expect(TokenType.DOUBLE_LITERAL);

			// double -> long
			Double d = (Double) getDouble.getValue();
			long dTOl = Double.doubleToLongBits(d);

			// add PUSH double
			instructions.add(new Instruction(Operation.PUSH,dTOl));
		} else if(front.getTokenType() == TokenType.STRING_LITERAL) {
			// 字符串字面量只会在putstr调用中出现，语义是对应的全局常量的编号
			Token getString = expect(TokenType.STRING_LITERAL);

			// add to global symtable
			table.addGlobalSymbol((String) getString.getValue(), TokenType.STRING_LITERAL, SymbolType.VAR, 1, getString.getStartPos(), true, true);

		} else if (front.getTokenType() == TokenType.CHAR_LITERAL) {
			// add push char -> long
			instructions.add(new Instruction(Operation.PUSH, (long) ((char) front.getValue())));
		} else if (front.getTokenType() == TokenType.L_PAREN) {
			expect(TokenType.L_PAREN);
			Object groupRet = analysisE(peek());
			expect(TokenType.R_PAREN);

			// group expr

		} else {
			throw new AnalyzeError(ErrorCode.ExprERROR, peek().getEndPos());
		}

		return instructions;
	}


	/**
	 * 语句
	 */
	private void analyseStmt() throws CompileError {
		while (checkMayStmt(peek())) {
			if (check(TokenType.LET_KW) || check(TokenType.CONST_KW)) analyseDeclStmt(false);
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
	private void isLetDeclStmt(boolean isGlobal) throws CompileError {
		expect(TokenType.LET_KW);
		Token nameToken = expect(TokenType.IDENT);
		expect(TokenType.COLON);
		Token typeToken = expect(List.of(TokenType.INT_TY, TokenType.DOUBLE_TY, TokenType.VOID_TY));

		boolean isInit = false;

		if (!check(TokenType.SEMICOLON)) {
			expect(TokenType.ASSIGN);

			// 语法分析拿到该赋值语句的右值
			// todo deal with -- generate instructions
			analysisE(peek());
			isInit = true;
		}

		// 如果是一个全局的let语句，就放入全局符号表
		// 反之就放入当前函数的符号表中（当前函数为函数表最后一个函数实体）
		if (isGlobal) {
			table.addGlobalSymbol(
					(String) nameToken.getValue(),
					typeToken.getTokenType(),
					SymbolType.VAR,
					this.deep, nameToken.getStartPos(), false, isInit);
		} else {
			table.addFuncSymbol((String) nameToken.getValue(),
					typeToken.getTokenType(),
					SymbolType.VAR,
					this.deep, false, isInit,
					nameToken.getStartPos());
		}

		expect(TokenType.SEMICOLON);
	}

	private void isConstDeclStmt(boolean isGlobal) throws CompileError {
		expect(TokenType.CONST_KW);
		Token nameToken = expect(TokenType.IDENT);
		expect(TokenType.COLON);
		Token typeToken = expect(List.of(TokenType.INT_TY, TokenType.DOUBLE_TY, TokenType.VOID_TY));
		expect(TokenType.ASSIGN);

		// todo deal with -- generate instructions
		analysisE(peek());

		// 如果是一个全局的const语句，就放入全局符号表
		// 反之就放入当前函数的符号表中（当前函数为函数表最后一个函数实体）
		if (isGlobal) {
			table.addGlobalSymbol(
					(String) nameToken.getValue(),
					typeToken.getTokenType(),
					SymbolType.VAR,
					this.deep, nameToken.getStartPos(), true, true);
		} else {
			table.addFuncSymbol((String) nameToken.getValue(),
					typeToken.getTokenType(),
					SymbolType.VAR,
					this.deep, true, true,
					nameToken.getStartPos());
		}

		expect(TokenType.SEMICOLON);
	}

	private void analyseDeclStmt(boolean isGlobal) throws CompileError {
		if (check(TokenType.LET_KW)) isLetDeclStmt(isGlobal);
		else if (check(TokenType.CONST_KW)) isConstDeclStmt(isGlobal);
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

		// 进入块后，deep + 1
		this.deep ++;

		while (!check(TokenType.R_BRACE)) {
			analyseStmt();
		}
		expect(TokenType.R_BRACE);

		// 退出块后，deep - 1
		this.deep --;
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
		boolean isConstant = false;

		// 判断形参是否为const
		if (check(TokenType.CONST_KW)) {
			isConstant = true;
			expect(TokenType.CONST_KW);
		}

		Token paramName = expect(TokenType.IDENT);
		expect(TokenType.COLON);
		Token paramType = expect(List.of(TokenType.VOID_TY, TokenType.INT_TY, TokenType.DOUBLE_TY));

		// 向函数实体加入形参信息
		table.addParam((String) paramName.getValue(), paramType, paramName.getStartPos());
	}

	private void analyseFunctionParamList() throws CompileError {
		do {
			analyseFunctionParam();
		}
		while (nextIf(TokenType.COMMA) != null);
	}

	private void analyseFunction() throws CompileError {
		expect(TokenType.FN_KW);
		Token funcName = expect(TokenType.IDENT);
		expect(TokenType.L_PAREN);

		// 在函数表中加入该函数（仅占位）
		table.addFuncEntry((String) funcName.getValue(), funcName.getEndPos());
		// 同时在符号表中加入该函数的名字和返回类型
		table.addGlobalSymbol((String) funcName.getValue(), TokenType.VOID_TY, SymbolType.FUNC, this.deep, funcName.getStartPos(), true, true);

		// 发现该函数有形参，进入形参处理，此时加入形参时直接加入到函数表倒着的第一个函数
		if (!check(TokenType.R_PAREN)) analyseFunctionParamList();

		expect(TokenType.R_PAREN);
		expect(TokenType.ARROW);

		// 获取该函数的返回类型，并填入倒数第一个函数实体以及对应的全局符号
		Token funcType = expect(List.of(TokenType.VOID_TY, TokenType.INT_TY, TokenType.DOUBLE_TY));
		if (funcType.getTokenType() != TokenType.VOID_TY) {
			table.addGlobalType(funcName, funcType.getTokenType());
			table.addFuncType(funcType.getTokenType());
		}

		// 进入该函数实体中
		analyseBlockStmt();
	}

	/**
	 * 程序
	 */
	private void analyseProgramme() throws CompileError {
		while (check(TokenType.LET_KW) || check(TokenType.CONST_KW) || check(TokenType.FN_KW)) {
			// 全局定义
			while (check(TokenType.LET_KW) || check(TokenType.CONST_KW)) {
				analyseDeclStmt(true);
			}

			// 函数定义
			while (check(TokenType.FN_KW)) {
				analyseFunction();
			}
		}
	}

	// 指令生成帮助类方法

	/**
	 * 获取var或者param的（绝对）偏移量
	 *
	 * @param nameToken
	 * @return
	 * @throws AnalyzeError
	 */
	public Instruction getAddressAboutVarOrParam(Token nameToken) throws AnalyzeError {
		SymbolEntry symbolEntry;
		Pos currentPos = nameToken.getStartPos();

		if (this.deep == 1) {
			symbolEntry = table.getGlobalVar((String) nameToken.getValue());
		} else {
			symbolEntry = table.getFuncVarOrParam((String) nameToken.getValue());
		}

		// 全局或者局部查找不到则报错
		if (symbolEntry == null) throw new AnalyzeError(ErrorCode.NotDeclared, currentPos);

		// 变量或者常量，通过deep判断
		if (symbolEntry.getSymbolType() == SymbolType.VAR) {
			if (symbolEntry.getDeep() == 1) return new Instruction(Operation.GLOBA, symbolEntry.getOff());
			else return new Instruction(Operation.LOCA, symbolEntry.getOff());
		} else if (symbolEntry.getSymbolType() == SymbolType.PARAM) {
			return new Instruction(Operation.ARGA, symbolEntry.getOff());
		} else {
			throw new AnalyzeError(ErrorCode.CannotGetOff, currentPos);
		}
	}
}