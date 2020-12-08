package c0.analyser;

import c0.Entry.SymbolEntry;
import c0.error.*;
import c0.instruction.Instruction;
import c0.instruction.Operation;
import c0.table.Table;
import c0.tokenizer.Token;
import c0.tokenizer.TokenType;
import c0.tokenizer.Tokenizer;
import c0.util.MyPair;
import c0.util.Pos;
import c0.util.SymbolType;

import java.util.ArrayList;
import java.util.List;

public final class Analyser {

	Tokenizer tokenizer;
	ArrayList<Instruction> tmpInstructions;
	ArrayList<Instruction> instructions;

	/**
	 * 当前函数是否有return
	 */
	boolean funcReturn = false;

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
		System.out.println(table.toString());
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

	// todo - list
	/*
	1. 调取到一个ident时指令操作
	2. 二值运算的指令操作
	3. 二值比较的指令操作
	4. 赋值时type是否对应的处理
	5. 调用函数时的指令操作
	6. if和while的跳转处理
	7. 调用函数的参数
	 */

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

					// 获取front对应的符号
					MyPair myPair = table.searchOneSymbolFromLocalToGlobal((String) front.getValue(), this.deep, front.getStartPos());
					SymbolEntry getSymbol = (SymbolEntry) myPair.getFirst();
					Boolean isGlobal = (Boolean) myPair.getSecond();

					if (isGlobal) instructions.add(new Instruction(Operation.GLOBA, getSymbol.getOff()));
					else {
						if (getSymbol.getSymbolType() == SymbolType.PARAM)
							instructions.add(new Instruction(Operation.ARGA, getSymbol.getOff()));
						else
							instructions.add(new Instruction(Operation.LOCA, getSymbol.getOff()));
					}

					// 加入赋值语句右值生成指令集
					instructions.addAll(analysisE(peek()));
					instructions.add(new Instruction(Operation.STORE_64));
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

		// 添加置负指令
		// todo 检查当前的类型以判断用neg.i还是neg.f
		instructions.add(new Instruction(Operation.NEG_I));
		return instructions;
	}


	/**
	 * A -> (-)? I
	 * I -> IDENT | UINT | DOUBLE | STRING | CHAR | func_call | '('E')'
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
			// 首先在当前深度查找是否在当前函数中有定义
			// 如果当前深度没有，就逐次向上找
			// 找到deep = 2时还没有就去全局找
			MyPair myPair = table.searchOneSymbolFromLocalToGlobal((String) front.getValue(), this.deep, front.getStartPos());

			SymbolEntry getSymbol = (SymbolEntry) myPair.getFirst();
			Boolean isGlobal = (Boolean) myPair.getSecond();

			// 还找不到就报错
			if (getSymbol == null) throw new AnalyzeError(ErrorCode.NotDeclared, front.getStartPos());

			// 非函数调用：类型转换 | 单纯调用符号
			if (peek().getTokenType() != TokenType.L_PAREN) {
				// 加载局部或全局符号地址
				if (isGlobal) instructions.add(new Instruction(Operation.GLOBA, getSymbol.getOff()));
				else {
					if (getSymbol.getSymbolType() == SymbolType.PARAM)
						instructions.add(new Instruction(Operation.ARGA, getSymbol.getOff()));
					else
						instructions.add(new Instruction(Operation.LOCA, getSymbol.getOff()));
				}
				instructions.add(new Instruction(Operation.LOAD_64));

				if (peek().getTokenType() == TokenType.AS_KW) {
					// 单句修改变量类型
					// todo generate instructions
					next();
					expect(List.of(TokenType.INT_TY, TokenType.DOUBLE_TY));
				} else {
					next();
				}
			}

			// 函数调用
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
		}
		else if (front.getTokenType() == TokenType.UINT_LITERAL) {
			// int字面量
			Token getUint = expect(TokenType.UINT_LITERAL);
			Integer i = (Integer) getUint.getValue();
			long iTOl = i;
			instructions.add(new Instruction(Operation.PUSH, iTOl));
		}
		else if (front.getTokenType() == TokenType.DOUBLE_LITERAL) {
			// double字面量
			Token getDouble = expect(TokenType.DOUBLE_LITERAL);
			Double d = (Double) getDouble.getValue();
			long dTOl = Double.doubleToLongBits(d);
			instructions.add(new Instruction(Operation.PUSH,dTOl));
		}
		else if(front.getTokenType() == TokenType.STRING_LITERAL) {
			// 字符串字面量只会在putstr调用中出现，语义是对应的全局常量的编号
			Token getString = expect(TokenType.STRING_LITERAL);
			table.addGlobalSymbol((String) getString.getValue(), TokenType.STRING_LITERAL, SymbolType.VAR, 1, getString.getStartPos(), true, true);
		}
		else if (front.getTokenType() == TokenType.CHAR_LITERAL) {
			// char字面量
			instructions.add(new Instruction(Operation.PUSH, (long) ((char) front.getValue())));
		}
		else if (front.getTokenType() == TokenType.L_PAREN) {
			// 括号表达式
			expect(TokenType.L_PAREN);
			instructions.addAll(analysisE(peek()));
			expect(TokenType.R_PAREN);
		}
		else {
			throw new AnalyzeError(ErrorCode.ExprERROR, peek().getEndPos());
		}

		return instructions;
	}


	/**
	 * 语句
	 */
	private List<Instruction> analyseStmt() throws CompileError {
		List<Instruction> instructions = new ArrayList<>();

		// 打补丁 -- 针对else前无if匹配的情况
		if (check(TokenType.ELSE_KW)) {
			throw new AnalyzeError(ErrorCode.IfElseNotMatch, peek().getStartPos());
		}

		while (checkMayStmt(peek())) {
			if (check(TokenType.LET_KW) || check(TokenType.CONST_KW)) instructions.addAll(analyseDeclStmt(false));
			else if (check(TokenType.IF_KW)) analyseIfStmt();
			else if (check(TokenType.WHILE_KW)) analyseWhileStmt();
			else if (check(TokenType.RETURN_KW)) instructions.addAll(analyseReturnStmt());
			else if (check(TokenType.L_BRACE)) instructions.addAll(analyseBlockStmt());
			else if (check(TokenType.SEMICOLON)) analyseEmptyStmt();
			else instructions.addAll(analyseExprStmt());
		}

		return instructions;
	}

	/**
	 * 表达式语句
	 */
	private List<Instruction> analyseExprStmt() throws CompileError {
		List<Instruction> instructions = new ArrayList<>();

		// 分析Expr并生成指令集
		instructions.addAll(analysisE(null));
		expect(TokenType.SEMICOLON);

		return instructions;
	}

	/**
	 * 声明语句
	 *
	 * @param isGlobal
	 * @return
	 * @throws CompileError
	 */
	private List<Instruction> isLetDeclStmt(boolean isGlobal) throws CompileError {
		expect(TokenType.LET_KW);
		Token nameToken = expect(TokenType.IDENT);
		expect(TokenType.COLON);

		List<Instruction> instructions = new ArrayList<>();
		List<Instruction> analyseEIns = new ArrayList<>();

		// 对变量置类型只允许int和double
		Token typeToken = expect(List.of(TokenType.INT_TY, TokenType.DOUBLE_TY));

		boolean isInit = false;

		if (!check(TokenType.SEMICOLON)) {
			expect(TokenType.ASSIGN);

			// 语法分析拿到该赋值语句的右值
			analyseEIns.addAll(analysisE(peek()));
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

			// 当全局变量被初始化时，加入赋值生成的指令集
			if (isInit) {
				// 获取全局符号的偏移量
				long globalSymbolOff = table.getGlobalSymbolOff((String) nameToken.getValue());

				table.getGlobalInstructions().add(new Instruction(Operation.GLOBA, globalSymbolOff));
				table.getGlobalInstructions().addAll(analyseEIns);
				table.getGlobalInstructions().add(new Instruction(Operation.STORE_64));
			}

		} else {
			table.addFuncSymbol((String) nameToken.getValue(),
					typeToken.getTokenType(),
					SymbolType.VAR,
					this.deep, false, isInit,
					nameToken.getStartPos());

			// 当局部变量被初始化时，向函数内加入赋值生成的指令集
			if (isInit) {
				// 获取局部符号的偏移值
				long localSymbolOff = table.getFuncTable().get(table.getFuncTable().size() - 1).getLocalSymbolOff((String) nameToken.getValue());

				instructions.add(new Instruction(Operation.LOCA, localSymbolOff));
				instructions.addAll(analyseEIns);
				instructions.add(new Instruction(Operation.STORE_64));
			}
		}

		expect(TokenType.SEMICOLON);

		return instructions;
	}

	/**
	 * const 声明语句
	 *
	 * @param isGlobal
	 * @return
	 * @throws CompileError
	 */
	private List<Instruction> isConstDeclStmt(boolean isGlobal) throws CompileError {
		expect(TokenType.CONST_KW);
		Token nameToken = expect(TokenType.IDENT);
		expect(TokenType.COLON);
		Token typeToken = expect(List.of(TokenType.INT_TY, TokenType.DOUBLE_TY));
		expect(TokenType.ASSIGN);

		List<Instruction> instructions = new ArrayList<>();
		List<Instruction> analyseEIns = new ArrayList<>();

		analyseEIns.addAll(analysisE(peek()));

		// 如果是一个全局的const语句，就放入全局符号表
		// 反之就放入当前函数的符号表中（当前函数为函数表最后一个函数实体）
		if (isGlobal) {
			table.addGlobalSymbol(
					(String) nameToken.getValue(),
					typeToken.getTokenType(),
					SymbolType.VAR,
					this.deep, nameToken.getStartPos(), true, true);

			// 获取全局符号的偏移量
			long globalSymbolOff = table.getGlobalSymbolOff((String) nameToken.getValue());

			table.getGlobalInstructions().add(new Instruction(Operation.GLOBA, globalSymbolOff));
			table.getGlobalInstructions().addAll(analyseEIns);
			table.getGlobalInstructions().add(new Instruction(Operation.STORE_64));
		} else {
			table.addFuncSymbol((String) nameToken.getValue(),
					typeToken.getTokenType(),
					SymbolType.VAR,
					this.deep, true, true,
					nameToken.getStartPos());

			// 获取局部符号的偏移值
			long localSymbolOff = table.getFuncTable().get(table.getFuncTable().size() - 1).getLocalSymbolOff((String) nameToken.getValue());

			instructions.add(new Instruction(Operation.LOCA, localSymbolOff));
			instructions.addAll(analyseEIns);
			instructions.add(new Instruction(Operation.STORE_64));
		}

		expect(TokenType.SEMICOLON);

		return instructions;
	}

	private List<Instruction> analyseDeclStmt(boolean isGlobal) throws CompileError {
		List<Instruction> instructions = new ArrayList<>();

		if (check(TokenType.LET_KW))
			instructions.addAll(isLetDeclStmt(isGlobal));
		else if (check(TokenType.CONST_KW))
			instructions.addAll(isConstDeclStmt(isGlobal));

		return instructions;
	}

	/**
	 * 控制流语句
	 */
	private void analyseIfStmt() throws CompileError {
		expect(TokenType.IF_KW);
		analysisE(peek());
		analyseBlockStmt();

		// while to get else_kw
		while (check(TokenType.ELSE_KW)) {
			expect(TokenType.ELSE_KW);

			// if bool {} else if bool {}
			if (check(TokenType.IF_KW)) {
				expect(TokenType.IF_KW);
				analysisE(peek());
				analyseBlockStmt();
			}
			// if bool {} else {}
			else {
				analyseBlockStmt();
				break;
			}
		}
	}

	private void analyseWhileStmt() throws CompileError {
		expect(TokenType.WHILE_KW);
		analysisE(peek());
		analyseBlockStmt();
	}

	private List<Instruction> analyseReturnStmt() throws CompileError {
		expect(TokenType.RETURN_KW);

		// 此时函数有返回值
		funcReturn = true;

		List<Instruction> instructions = new ArrayList<>();

		// 如果该函数为void类型，则不可以接返回值
		if (table.getFuncTable().get(table.getFuncTable().size() - 1).getFuncType() == TokenType.VOID_TY) {
			if (peek().getTokenType() != TokenType.SEMICOLON) {
				throw new AnalyzeError(ErrorCode.ShouldNotReturn, peek().getStartPos());
			}
		}
		// 如果函数类型不为void，就需要返回值
		else {
			if (peek().getTokenType() == TokenType.SEMICOLON) {
				throw new AnalyzeError(ErrorCode.ShouldReturn, peek().getStartPos());
			}

			// 加入 return Expr; 右部生成的指令集
			instructions.addAll(analysisE(peek()));
		}
		expect(TokenType.SEMICOLON);

		// 加入ret指令
		instructions.add(new Instruction(Operation.RET));
		return instructions;
	}

	/**
	 * 代码块
	 */
	private List<Instruction> analyseBlockStmt() throws CompileError {
		expect(TokenType.L_BRACE);

		// 打个补丁 - 读到下一个token为空时
		if (peek().getTokenType().ordinal() == 40)
			throw new AnalyzeError(ErrorCode.NotComplete, peek().getStartPos());

		List<Instruction> instructions = new ArrayList<>();

		// 进入块后，deep + 1
		this.deep ++;

		while (!check(TokenType.R_BRACE)) {
			instructions.addAll(analyseStmt());
		}
		expect(TokenType.R_BRACE);

		// 当前deep == 2时，表示此时处于函数内部的最外层，有返回则funcReturn应该为true
		if (deep == 2)
			if (table.getFuncTable().get(table.getFuncTable().size() - 1).getFuncType() != TokenType.VOID_TY && !funcReturn)
				throw new AnalyzeError(ErrorCode.ShouldReturn, peek().getStartPos());

		// 退出块后，deep - 1
		this.deep --;
		return instructions;
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
		// 函数的指令集
		List<Instruction> instructions = new ArrayList<>();

		expect(TokenType.FN_KW);
		Token funcName = expect(TokenType.IDENT);
		expect(TokenType.L_PAREN);

		// 在函数表中加入该函数（仅占位）
		table.addFuncEntry((String) funcName.getValue(), funcName.getEndPos());
		// 同时在符号表中加入该函数的名字和返回类型
		// table.addGlobalSymbol((String) funcName.getValue(), TokenType.VOID_TY, SymbolType.FUNC, this.deep, funcName.getStartPos(), true, true);

		// 发现该函数有形参，进入形参处理，此时加入形参时直接加入到函数表倒着的第一个函数
		if (!check(TokenType.R_PAREN)) analyseFunctionParamList();

		expect(TokenType.R_PAREN);
		expect(TokenType.ARROW);

		// 获取该函数的返回类型，并填入倒数第一个函数实体以及对应的全局符号
		Token funcType = expect(List.of(TokenType.VOID_TY, TokenType.INT_TY, TokenType.DOUBLE_TY));
		if (funcType.getTokenType() != TokenType.VOID_TY) {
			// table.addGlobalType(funcName, funcType.getTokenType());
			table.addFuncType(funcType.getTokenType());
		}

		// 进入该函数实体中
		instructions.addAll(analyseBlockStmt());

		// 退出函数体后，重置funcRet
		funcReturn = false;

		// 同时加入ret指令
		instructions.add(new Instruction(Operation.RET));

		// 将该函数的指令集加入到函数内指令集中
		table.addInstructionsToFunc(instructions);
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