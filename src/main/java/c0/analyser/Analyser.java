package c0.analyser;

import c0.Entry.FuncEntry;
import c0.Entry.SymbolEntry;
import c0.error.*;
import c0.instruction.Instruction;
import c0.instruction.Operation;
import c0.table.Table;
import c0.tokenizer.Token;
import c0.tokenizer.TokenType;
import c0.tokenizer.Tokenizer;
import c0.util.*;

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
	boolean finalFuncReturn = false;

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

	/**
	 * E -> C ( ==|!=|<|>|<=|>= C )?
	 * E' -> IDENT = E
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private LoadUp analysisE(Token front) throws CompileError {
		LoadUp loadUp = new LoadUp();
		List<Instruction> instructions = new ArrayList<>();

		/* 允许赋值表达式产生 */
		if (front == null) {
			if (peek().getTokenType() == TokenType.IDENT) {
				front = next();

				/* 赋值表达式 */
				if (peek().getTokenType() == TokenType.ASSIGN) {
					expect(TokenType.ASSIGN);

					// 获取front对应的符号
					MyPair myPair = table.searchOneSymbolFromLocalToGlobal((String) front.getValue(), this.deep, peek().getStartPos());
					SymbolEntry getSymbol = (SymbolEntry) myPair.getFirst();
					Boolean isGlobal = (Boolean) myPair.getSecond();

					// 如果符合为const，则不可被二次赋值
					if (getSymbol.isConstant()) throw new AnalyzeError(ErrorCode.AssignToConstant, front.getStartPos());

					// 如果front为全局变量，调用全局加载指令
					if (isGlobal)
						instructions.add(new Instruction(Operation.GLOBA, getSymbol.getOff()));
					// 局部加载指令（参数加载、变量加载）
					else {
						if (getSymbol.getSymbolType() == SymbolType.PARAM)
							instructions.add(new Instruction(Operation.ARGA, getSymbol.getOff()));
						else
							instructions.add(new Instruction(Operation.LOCA, getSymbol.getOff()));
					}

					// 加入赋值语句右值生成指令集
					LoadUp rightLoad = analysisE(peek());

					// 判断类型是否契合
					if (getSymbol.getType() != rightLoad.type) throw new AnalyzeError(ErrorCode.TypeMisMatch, peek().getStartPos());

					instructions.addAll(rightLoad.instructions);
					instructions.add(new Instruction(Operation.STORE_64));

					// 上载：赋值语句无返回类型
					loadUp.setType(TokenType.VOID_TY);
					loadUp.setInstructions(instructions);
					loadUp.setConstant(false);
				}
				/* 非赋值表达式 */
				else {
					LoadUp left = analysisC(front);
					LoadUp right = analysisBinaryCompare(left);

					instructions.addAll(left.instructions);

					if (right != null) {
						instructions.addAll(right.instructions);
					}

					// 上载
					loadUp.setType(left.type);
					loadUp.setInstructions(instructions);
					loadUp.setConstant(true);
				}
			} else {
				LoadUp left = analysisC(peek());
				LoadUp right = analysisBinaryCompare(left);

				instructions.addAll(left.instructions);

				if (right != null) {
					instructions.addAll(right.instructions);
				}

				// 上载
				loadUp.setType(left.type);
				loadUp.setInstructions(instructions);
				loadUp.setConstant(true);
			}
		}

		/* 不允许产生赋值表达式 */
		else {
			LoadUp left = analysisC(front);
			LoadUp right = analysisBinaryCompare(left);

			instructions.addAll(left.instructions);

			if (right != null) {
				instructions.addAll(right.instructions);
			}

			// 上载
			loadUp.setType(left.type);
			loadUp.setInstructions(instructions);
			loadUp.setConstant(true);
		}

		return loadUp;
	}

	/**
	 * 二元比较
	 *
	 * @return
	 * @throws CompileError
	 */
	private LoadUp analysisBinaryCompare(LoadUp front) throws CompileError {
		LoadUp loadUp = new LoadUp();
		List<Instruction> instructions = new ArrayList<>();

		if (isBinaryComparer(peek()) != null) {
			Token op = next();

			// 二元比较 -- 后值 -- 指令集
			LoadUp back = analysisC(peek());
			instructions.addAll(back.instructions);

			// 判断前后是否匹配
			if (front.type != back.type || front.type == TokenType.VOID_TY || back.type == TokenType.VOID_TY)
				throw new AnalyzeError(ErrorCode.TypeMisMatch, peek().getStartPos());

			// 二元比较 -- 比较符 -- 指令集
			if (front.type == TokenType.INT_TY)
				instructions.add(new Instruction(Operation.CMP_I));
			else if (front.type == TokenType.DOUBLE_TY)
				instructions.add(new Instruction(Operation.CMP_F));
			else {
				throw new AnalyzeError(ErrorCode.ShouldNotBeExist, peek().getStartPos());
			}

			// ==
			if (op.getTokenType() == TokenType.EQ) {
				instructions.add(new Instruction(Operation.NOT));
			}
			// !=
			else if (op.getTokenType() == TokenType.NEQ) {
				// do nothing
			}
			// <
			else if (op.getTokenType() == TokenType.LT) {
				instructions.add(new Instruction(Operation.SET_LT));
			}
			// >
			else if (op.getTokenType() == TokenType.GT) {
				instructions.add(new Instruction(Operation.SET_GT));
			}
			// <=
			else if (op.getTokenType() == TokenType.LE) {
				instructions.add(new Instruction(Operation.SET_GT));
				instructions.add(new Instruction(Operation.NOT));
			}
			// >=
			else if (op.getTokenType() == TokenType.GE) {
				instructions.add(new Instruction(Operation.SET_LT));
				instructions.add(new Instruction(Operation.NOT));
			}

			// 上载
			loadUp.setType(TokenType.BOOL_TY);
			loadUp.setInstructions(instructions);
			loadUp.setConstant(true);

			return loadUp;
		}
		return null;
	}

	/**
	 * E -> C ( ==|!=|<|>|<=|>= C )?
	 * C -> T { +|- T }
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private LoadUp analysisC(Token front) throws CompileError {
		LoadUp loadUp = new LoadUp();
		List<Instruction> instructions = new ArrayList<>();

		// 左值 -- 指令集
		LoadUp left = analysisT(front);
		instructions.addAll(left.instructions);

		while (peek().getTokenType() == TokenType.PLUS || peek().getTokenType() == TokenType.MINUS) {
			Token op = next();

			// 右值 -- 指令集
			LoadUp right = analysisT(peek());
			instructions.addAll(right.instructions);

			// 判断左右值是否匹配
			if (left.type != right.type || left.type == TokenType.VOID_TY || right.type == TokenType.VOID_TY)
				throw new AnalyzeError(ErrorCode.TypeMisMatch, peek().getStartPos());

			// 二元运算 -- 指令集
			if (op.getTokenType() == TokenType.PLUS) {
				if (left.type == TokenType.INT_TY)
					instructions.add(new Instruction(Operation.ADD_I));
				else if (left.type == TokenType.DOUBLE_TY)
					instructions.add(new Instruction(Operation.ADD_F));
				else
					throw new AnalyzeError(ErrorCode.ShouldNotBeExist, peek().getStartPos());
			}
			else {
				if (left.type == TokenType.INT_TY)
					instructions.add(new Instruction(Operation.SUB_I));
				else if (left.type == TokenType.DOUBLE_TY)
					instructions.add(new Instruction(Operation.SUB_F));
				else
					throw new AnalyzeError(ErrorCode.ShouldNotBeExist, peek().getStartPos());
			}
		}

		// 上载
		loadUp.setType(left.type);
		loadUp.setInstructions(instructions);
		loadUp.setConstant(true);
		
		return loadUp;
	}

	/**
	 * C -> T { +|- T }
	 * T -> F { *|/ F }
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private LoadUp analysisT(Token front) throws CompileError {
		LoadUp loadUp = new LoadUp();
		List<Instruction> instructions = new ArrayList<>();

		// 左值 -- 指令集
		LoadUp left = analysisF(front);
		instructions.addAll(left.instructions);

		while (peek().getTokenType() == TokenType.MUL || peek().getTokenType() == TokenType.DIV) {
			Token op = next();

			// 右值 -- 指令集
			LoadUp right = analysisF(peek());
			instructions.addAll(right.instructions);

			// 判断左右值是否匹配
			if (left.type != right.type || left.type == TokenType.VOID_TY || right.type == TokenType.VOID_TY)
				throw new AnalyzeError(ErrorCode.TypeMisMatch, peek().getStartPos());

			// 二元运算 -- 指令集
			if (op.getTokenType() == TokenType.MUL) {
				if (left.type == TokenType.INT_TY)
					instructions.add(new Instruction(Operation.MUL_I));
				else if (left.type == TokenType.DOUBLE_TY)
					instructions.add(new Instruction(Operation.MUL_F));
				else
					throw new AnalyzeError(ErrorCode.ShouldNotBeExist, peek().getStartPos());
			}
			else {
				if (left.type == TokenType.INT_TY)
					instructions.add(new Instruction(Operation.DIV_I));
				else if (left.type == TokenType.DOUBLE_TY)
					instructions.add(new Instruction(Operation.DIV_F));
				else
					throw new AnalyzeError(ErrorCode.ShouldNotBeExist, peek().getStartPos());
			}
		}

		// 上载
		loadUp.setType(left.type);
		loadUp.setInstructions(instructions);
		loadUp.setConstant(true);

		return loadUp;
	}

	/**
	 * T -> F { *|/ F }
	 * F -> A ( as int_ty | double_ty )?
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private LoadUp analysisF(Token front) throws CompileError {
		LoadUp loadUp = new LoadUp();
		List<Instruction> instructions = new ArrayList<>();

		LoadUp left = analysisA(front);
		instructions.addAll(left.instructions);

		// 当前类型
		TokenType currentTy = left.type;

		// 函数调用类型转换
		if (peek().getTokenType() == TokenType.AS_KW) {
			expect(TokenType.AS_KW);
			if (peek().getTokenType() == TokenType.INT_TY) {
				if (currentTy == TokenType.DOUBLE_TY) {
					currentTy = TokenType.INT_TY;
					instructions.add(new Instruction(Operation.FTOI));
				}
				next();
			} else if (peek().getTokenType() == TokenType.DOUBLE_TY) {
				if (currentTy == TokenType.INT_TY) {
					currentTy = TokenType.DOUBLE_TY;
					instructions.add(new Instruction(Operation.ITOF));
				}
				next();
			}
		}

		// 上载
		loadUp.setType(currentTy);
		loadUp.setInstructions(instructions);
		loadUp.setConstant(true);

		return loadUp;
	}

	/**
	 * F -> A ( as int_ty | double_ty )?
	 * A -> (-)? I
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private LoadUp analysisA(Token front) throws CompileError {
		LoadUp loadUp = new LoadUp();
		List<Instruction> instructions = new ArrayList<>();

		Boolean nFlag = false;
		if (front.getTokenType() == TokenType.MINUS) {
			nFlag = true;
			next();
			front = peek();
		}

		LoadUp upI = analysisI(front);
		instructions.addAll(upI.instructions);

		// 添加置负指令
		if (nFlag) {
			if (upI.type == TokenType.VOID_TY)
				throw new AnalyzeError(ErrorCode.TypeMisMatch, peekedToken.getStartPos());
			else {
				if (upI.type == TokenType.INT_TY || upI.type == TokenType.CHAR_LITERAL || upI.type == TokenType.STRING_LITERAL)
					instructions.add(new Instruction(Operation.NEG_I));
				else if (upI.type == TokenType.DOUBLE_TY)
					instructions.add(new Instruction(Operation.NEG_F));
				else
					throw new AnalyzeError(ErrorCode.ShouldNotBeExist, peek().getStartPos());
			}
		}

		// 上载
		loadUp.setType(upI.type);
		loadUp.setInstructions(instructions);
		loadUp.setConstant(true);

		return loadUp;
	}

	/**
	 * A -> (-)? I
	 * I -> IDENT | UINT | DOUBLE | STRING | CHAR | func_call | '('E')'
	 *
	 * @param front
	 * @return
	 * @throws CompileError
	 */
	private LoadUp analysisI(Token front) throws CompileError {
		LoadUp loadUp = new LoadUp();
		List<Instruction> instructions = new ArrayList<>();

		if (front.getTokenType() == TokenType.IDENT) {
			Token getPeek = next();

			// 非函数调用：类型转换 | 单纯调用符号
			if (peek().getTokenType() != TokenType.L_PAREN) {
				// 首先在当前深度查找是否在当前函数中有定义
				// 如果当前深度没有，就逐次向上找
				// 找到deep = 2时还没有就去全局找
				MyPair myPair = table.searchOneSymbolFromLocalToGlobal((String) front.getValue(), this.deep, front.getStartPos());

				SymbolEntry getSymbol = (SymbolEntry) myPair.getFirst();
				Boolean isGlobal = (Boolean) myPair.getSecond();

				// 还找不到就报错
				if (getSymbol == null) {
					System.out.println("not call func");
					System.out.println(front);
					throw new AnalyzeError(ErrorCode.NotDeclared, front.getStartPos());
				}

				// 找到了先看看是否初始化
				if (!getSymbol.isInitialized()) throw new AnalyzeError(ErrorCode.NotInitialized, front.getStartPos());

				// 加载局部或全局符号地址
				if (isGlobal) instructions.add(new Instruction(Operation.GLOBA, getSymbol.getOff()));
				else {
					// 符号为函数的形参
					if (getSymbol.getSymbolType() == SymbolType.PARAM) instructions.add(new Instruction(Operation.ARGA, getSymbol.getOff()));
					// 符号为函数的局部变量
					else instructions.add(new Instruction(Operation.LOCA, getSymbol.getOff()));
				}
				instructions.add(new Instruction(Operation.LOAD_64));

				// 当前类型
				TokenType currentTy = getSymbol.getType();

				if (peek().getTokenType() == TokenType.AS_KW) {
					// 单句修改变量类型
					next();

					Token ty = expect(List.of(TokenType.INT_TY, TokenType.DOUBLE_TY));
					if (getSymbol.getType() == TokenType.INT_TY && ty.getTokenType() == TokenType.DOUBLE_TY) {
						currentTy = TokenType.DOUBLE_TY;
						instructions.add(new Instruction(Operation.ITOF));
					}
					if (getSymbol.getType() == TokenType.DOUBLE_TY && ty.getTokenType() == TokenType.INT_TY) {
						currentTy = TokenType.INT_TY;
						instructions.add(new Instruction(Operation.FTOI));
					}
				}

				// 上载
				loadUp.setInstructions(instructions);
				loadUp.setType(currentTy);
				loadUp.setConstant(getSymbol.isConstant());
			}

			// 函数调用
			if (peek().getTokenType() == TokenType.L_PAREN) {
				expect(TokenType.L_PAREN);

				// 查找并调用函数
				FuncEntry funcEntry = table.searchFuncInTable((String) front.getValue());

				// 如果函数未定义 -- 报错
				if (funcEntry == null) {
					System.out.println("call func");
					throw new AnalyzeError(ErrorCode.NotDeclared, front.getStartPos());
				}

				// 如果函数有返回值，分配空间
				if (funcEntry.getFuncType() != TokenType.VOID_TY)
					instructions.add(new Instruction(Operation.STACKALLOC, (long) 1));

				// 函数调用有参数输入
				if (peek().getTokenType() != TokenType.R_PAREN) {
					// 在此处生成 -- 参数压入指令集
					List<LoadUp> paramUpLoads = new ArrayList<>();

					do{
						LoadUp paramLoad = analysisE(peek());
						paramUpLoads.add(paramLoad);
						instructions.addAll(paramLoad.instructions);
					}
					while (nextIf(TokenType.COMMA) != null);

					// 检查参数数量和类型
					if (funcEntry.getParamSlotCount() != paramUpLoads.size())
						throw new AnalyzeError(ErrorCode.FuncParamsMisMatch, peek().getStartPos());

					int checkParamStartPos = (funcEntry.getFuncType() == TokenType.VOID_TY)? 0 : 1;
					for (int i = 0; i < paramUpLoads.size(); i++) {
						if (paramUpLoads.get(i).type != funcEntry.getSymbolTable().get(i + checkParamStartPos).getType())
							throw new AnalyzeError(ErrorCode.FuncParamsMisMatch, peek().getStartPos());
					}

					expect(TokenType.R_PAREN);
				} else {
					expect(TokenType.R_PAREN);
				}

				// call func by funcID
				int funcID = table.getFuncID((String) front.getValue());
				instructions.add(new Instruction(Operation.CALL, (long) funcID));

				// 上载
				loadUp.setType(funcEntry.getFuncType());
				loadUp.setInstructions(instructions);
				// 函数不允许 fun() = x;的操作
				loadUp.setConstant(true);
			}
		}
		else if (front.getTokenType() == TokenType.UINT_LITERAL) {
			// int字面量
			Token getUint = expect(TokenType.UINT_LITERAL);
			Integer i = (Integer) getUint.getValue();
			long iTOl = i;
			instructions.add(new Instruction(Operation.PUSH, iTOl));

			// 上载
			loadUp.setType(TokenType.INT_TY);
			loadUp.setInstructions(instructions);
			// 字面量不允许 1 = x;的操作
			loadUp.setConstant(true);
		}
		else if (front.getTokenType() == TokenType.DOUBLE_LITERAL) {
			// double字面量
			Token getDouble = expect(TokenType.DOUBLE_LITERAL);
			Double d = (Double) getDouble.getValue();
			long dTOl = Double.doubleToLongBits(d);
			instructions.add(new Instruction(Operation.PUSH,dTOl));

			// 上载
			loadUp.setType(TokenType.DOUBLE_TY);
			loadUp.setInstructions(instructions);
			// 字面量不允许 1.0 = x;的操作
			loadUp.setConstant(true);
		}
		else if(front.getTokenType() == TokenType.STRING_LITERAL) {
			// 字符串字面量只会在putstr调用中出现，语义是对应的全局常量的编号
			Token getString = expect(TokenType.STRING_LITERAL);
			table.addGlobalSymbol((String) getString.getValue(), TokenType.STRING_LITERAL, SymbolType.VAR, 1, getString.getStartPos(), true, true);

			long strOff = table.getGlobalSymbolOff((String) front.getValue());
			instructions.add(new Instruction(Operation.PUSH, strOff));

			// 上载 -- 字符串是作为全局量出现，且push其序号
			loadUp.setType(TokenType.INT_TY);
			loadUp.setInstructions(instructions);
			loadUp.setConstant(true);
		}
		else if (front.getTokenType() == TokenType.CHAR_LITERAL) {
			// char字面量
			instructions.add(new Instruction(Operation.PUSH, (long) ((char) front.getValue())));

			// 上载
			loadUp.setType(TokenType.CHAR_LITERAL);
			loadUp.setInstructions(instructions);
			loadUp.setConstant(true);
		}
		else if (front.getTokenType() == TokenType.L_PAREN) {
			// 括号表达式
			expect(TokenType.L_PAREN);
			loadUp = analysisE(peek());
			expect(TokenType.R_PAREN);
		}
		else {
			throw new AnalyzeError(ErrorCode.ExprERROR, peek().getEndPos());
		}
		return loadUp;
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
			else if (check(TokenType.IF_KW)) instructions.addAll(analyseIfStmt());
			else if (check(TokenType.WHILE_KW)) instructions.addAll(analyseWhileStmt());
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
		instructions.addAll(analysisE(null).instructions);
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
			LoadUp right = analysisE(peek());

			analyseEIns.addAll(right.instructions);

			// 类型匹配判断
			if (typeToken.getTokenType() != right.type) {
				throw new AnalyzeError(ErrorCode.TypeMisMatch, peek().getStartPos());
			}

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

		// 获取右值
		LoadUp right = analysisE(peek());
		analyseEIns.addAll(right.instructions);

		// 判断类型匹配
		if (typeToken.getTokenType() != right.type)
			throw new AnalyzeError(ErrorCode.TypeMisMatch, peek().getStartPos());

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
	private List<Instruction> analyseIfStmt() throws CompileError {
		List<Instruction> instructions  = new ArrayList<>();
		IfElse ifElse = new IfElse();

		expect(TokenType.IF_KW);
		LoadUp condition = analysisE(peek());

		// 判断语句接受类型
		if (condition.type != TokenType.BOOL_TY &&
				condition.type != TokenType.INT_TY &&
				condition.type != TokenType.DOUBLE_TY)
			throw new AnalyzeError(ErrorCode.TypeMisMatch, peek().getStartPos());

		List<Instruction> handle = analyseBlockStmt();

		boolean isRet = false;
		if (handle.get(handle.size() - 1).getOpt() == Operation.RET) {
			isRet = true;
		}

		// 加入if分支
		ifElse.addOneIfBranch(condition.instructions, handle, isRet);

		isRet = false;

		// while to get else_kw
		while (check(TokenType.ELSE_KW)) {
			expect(TokenType.ELSE_KW);

			// if bool {} else if bool {}
			if (check(TokenType.IF_KW)) {
				expect(TokenType.IF_KW);

				condition = analysisE(peek());
				handle = analyseBlockStmt();

				if (handle.get(handle.size() - 1).getOpt() == Operation.RET) isRet = true;

				// 加入if分支
				ifElse.addOneIfBranch(condition.instructions, handle, isRet);

				isRet = false;
			}
			// if bool {} else {}
			else {
				handle = analyseBlockStmt();

				if (handle.get(handle.size() - 1).getOpt() == Operation.RET) {
					isRet = true;
					finalFuncReturn = true;
				}

				// 加入else分支
				ifElse.addOneElseBranch(handle, isRet);

				isRet = false;
				break;
			}
		}

		instructions = ifElse.generate();
		return instructions;
	}

	private List<Instruction> analyseWhileStmt() throws CompileError {
		List<Instruction> instructions = new ArrayList<>();

		expect(TokenType.WHILE_KW);
		analysisE(peek());
		analyseBlockStmt();

		return instructions;
	}

	private List<Instruction> analyseReturnStmt() throws CompileError {
		expect(TokenType.RETURN_KW);

		// 此时函数有返回值
		funcReturn = true;
		if (this.deep == 2) finalFuncReturn = true;

		List<Instruction> instructions = new ArrayList<>();

		// 获取函数返回类型
		TokenType funcType = table.getFuncTable().get(table.getFuncTable().size() - 1).getFuncType();

		// 如果该函数为void类型，则不可以接返回值
		if (funcType == TokenType.VOID_TY) {
			if (peek().getTokenType() != TokenType.SEMICOLON) {
				throw new AnalyzeError(ErrorCode.ShouldNotReturn, peek().getStartPos());
			}
		}
		// 如果函数类型不为void，就需要返回值
		else {
			instructions.add(new Instruction(Operation.ARGA, (long) 0));
			if (peek().getTokenType() == TokenType.SEMICOLON) {
				throw new AnalyzeError(ErrorCode.ShouldReturn, peek().getStartPos());
			}

			// 加入 return Expr; 右部生成的指令集
			LoadUp right = analysisE(peek());

			// 判断类型匹配
			if (funcType != right.type)
				throw new AnalyzeError(ErrorCode.TypeMisMatch, peek().getStartPos());

			instructions.addAll(right.instructions);
			instructions.add(new Instruction(Operation.STORE_64));
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
			if (table.getFuncTable().get(table.getFuncTable().size() - 1).getFuncType() != TokenType.VOID_TY && !finalFuncReturn)
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

			// 预留return slot -- 在其他的函数调用时实现
			// instructions.add(new Instruction(Operation.ARGA, (long) 0));
		}

		// 进入该函数实体中
		instructions.addAll(analyseBlockStmt());

		// 退出函数体后，重置funcRet
		funcReturn = false;
		finalFuncReturn = false;

		// 同时加入ret指令
		if (funcType.getTokenType() == TokenType.VOID_TY)
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
}