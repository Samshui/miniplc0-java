package c0.analyser;

import c0.error.AnalyzeError;
import c0.error.CompileError;
import c0.error.ErrorCode;
import c0.error.ExpectedTokenError;
import c0.error.TokenizeError;
import c0.instruction.Instruction;
import c0.tokenizer.Token;
import c0.tokenizer.TokenType;
import c0.tokenizer.Tokenizer;
import c0.util.Pos;
import org.checkerframework.checker.units.qual.C;

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
	HashMap<String, SymbolEntry> symbolTable = new HashMap<>();

	/**
	 * 下一个变量的栈偏移
	 */
	int nextOffset = 0;

	public Analyser(Tokenizer tokenizer) {
		this.tokenizer = tokenizer;
		this.instructions = new ArrayList<>();
	}

	public List<Instruction> analyse() throws CompileError {
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
	 * @param curPos        当前 token 的位置（报错用）
	 * @throws AnalyzeError 如果重复定义了则抛异常
	 */
	private void addSymbol(String name, boolean isInitialized, boolean isConstant, Pos curPos) throws AnalyzeError {
		if (this.symbolTable.get(name) != null) {
			throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
		} else {
			this.symbolTable.put(name, new SymbolEntry(isConstant, isInitialized, getNextVariableOffset()));
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
		var entry = this.symbolTable.get(name);
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
		var entry = this.symbolTable.get(name);
		if (entry == null) {
			throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
		} else {
			return entry.getStackOffset();
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
		var entry = this.symbolTable.get(name);
		if (entry == null) {
			throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
		} else {
			return entry.isConstant();
		}
	}

	/**
	 * @design
	 *
	 * E -> 	 E B E
	 * 		   | E 'as' T
	 *
	 * 		   | '-' E
	 * 		   | '(' E ')'
	 *
	 * 		   | IDENT
	 * 		   | IDENT '(' C ')'
	 * 		   | IDENT '=' E
	 *
	 * 		   | UINT | DOUBLE | STRING | CHAR
	 *
	 * B -> 	 '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' | <= | >=
	 * C -> 	 E {',' E}
	 * T ->		 INT | VOID | DOUBLE
	 */

	/*
	 * EXAMPLE:
	 * fn fib(x: int) -> int {
	 *     if x<=1 {
	 *         return 1;
	 *     }
	 *     let result: int = fib(x - 1);
	 *     result = result + fib(x - 2);
	 *     return result;
	 * }
	 *
	 * fn main() -> int {
	 *     let i: int = 0;
	 *     let j: int;
	 *     j = getint();
	 *     while i < j {
	 *         putint(i);
	 *         putchar(32);
	 *         putint(fib(i));
	 *         putln();
	 *         i = i + 1;
	 *     }
	 *     return 0;
	 * }
	 */

	/**
	 * 表达式
	 */
	private void analyseExpr() throws CompileError {}

	/**
	 * 语句
	 */
	private void analyseStmt() throws CompileError {}

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

	private void analyseOperatorExpr() throws CompileError {
		// TODO 局部opg分析
	}

	/**
	 * 取反表达式
	 */
	private void analyseNegateExpr() throws CompileError {
		// TODO 是否需要在此处保存Expr的值并取反后填入地址
		expect(TokenType.MINUS);
		analyseExpr();
	}

	/**
	 * 赋值表达式
	 */
	private void analyseAssignExpr() throws CompileError {
		Token name = peek();
		expect(TokenType.IDENT);
		expect(TokenType.EQ);

		// TODO 查表，如果表里面有，就判断类型，如果类型也合适就加指令，否则报错
	}

	/**
	 * 类型转换表达式
	 */
	private void analyseAsExpr() throws CompileError {
		// TODO 局部opg分析
	}

	/**
	 * 函数调用表达式
	 */
	private void callParamList() throws CompileError {
		// TODO 读到一个表达式应该作为函数的参数，需要进行一些操作
		analyseExpr();
		while (nextIf(TokenType.COMMA) != null) {
			analyseExpr();
		}
	}

	private void analyseCallExpr() throws CompileError {
		Token funcName = peek();

		// TODO 查表（函数名称）

		expect(TokenType.IDENT);
		expect(TokenType.L_PAREN);

		// 当有参数时
		if (!check(TokenType.R_PAREN)) {
			// TODO 获取参数后操作，除了标准库函数，其他函数在使用前必先声明
			callParamList();
		}
		expect(TokenType.R_PAREN);
	}

	/**
	 * 字面量表达式
	 */
	private void analyseLiteralExpr() throws CompileError {
		Token name = peek();
		expect(List.of(TokenType.UINT_LITERAL, TokenType.DOUBLE_LITERAL, TokenType.STRING_LITERAL));
		// TODO 返回值
	}

	/**
	 * 标识符表达式
	 */
	private void analyseIdentExpr() throws CompileError {
		Token name = peek();
		expect(TokenType.IDENT);
		// TODO 按照name查表，返回Ident对应的值
	}

	/**
	 * 表达式语句
	 */
	private void analyseExprStmt() throws CompileError {
		// TODO 表达式如果有值，则丢弃值
		analyseExpr();
		expect(TokenType.SEMICOLON);
	}

	/**
	 * 声明语句
	 */
	private void letDeclStmt() throws CompileError {
		expect(TokenType.LET_KW);
		expect(TokenType.IDENT);
		expect(TokenType.COLON);
		expect(List.of(TokenType.INT_TY, TokenType.DOUBLE_TY, TokenType.VOID_TY));
		if (!check(TokenType.SEMICOLON)) {
			expect(TokenType.EQ);
			analyseExpr();
		}
	}

	private void constDeclStmt() throws CompileError {

	}

	private void analyseDeclStmt() {

	}


//    private void analyseProgram() throws CompileError {
//        // 程序 -> 'begin' 主过程 'end'
//        // 示例函数，示例如何调用子程序
//        // 'begin'
//        expect(TokenType.Begin);
//
//        analyseMain();
//
//        // 'end'
//        expect(TokenType.End);
//        expect(TokenType.EOF);
//    }
//
//    private void analyseMain() throws CompileError {
//        // 主过程 -> 常量声明 变量声明 语句序列
//        analyseConstantDeclaration();
//        analyseVariableDeclaration();
//        analyseStatementSequence();
//    }
//
//    private void analyseConstantDeclaration() throws CompileError {
//        // 示例函数，示例如何解析常量声明
//        // 常量声明 -> 常量声明语句*
//
//        // 如果下一个 token 是 const 就继续
//        while (nextIf(TokenType.Const) != null) {
//            // 常量声明语句 -> 'const' 变量名 '=' 常表达式 ';'
//
//            // 变量名
//            var nameToken = expect(TokenType.Ident);
//
//            // 加入符号表
//            String name = (String) nameToken.getValue();
//            addSymbol(name, true, true, nameToken.getStartPos());
//
//            // 等于号
//            expect(TokenType.Equal);
//
//            // 常表达式
//            var value = analyseConstantExpression();
//
//            // 分号
//            expect(TokenType.Semicolon);
//
//            // 这里把常量值直接放进栈里，位置和符号表记录的一样。
//            // 更高级的程序还可以把常量的值记录下来，遇到相应的变量直接替换成这个常数值，
//            // 我们这里就先不这么干了。
//            instructions.add(new Instruction(Operation.LIT, value));
//        }
//    }
//
//    private void analyseVariableDeclaration() throws CompileError {
//        // 变量声明 -> 变量声明语句*
//
//        // 如果下一个 token 是 var 就继续
//        while (nextIf(TokenType.Var) != null) {
//            // 变量声明语句 -> 'var' 变量名 ('=' 表达式)? ';'
//
//            // 变量名
//            var nameToken = expect(TokenType.Ident);
//
//            // 变量初始化了吗
//            boolean initialized = false;
//
//            // 下个 token 是等于号吗？如果是的话分析初始化
//            // 分析初始化的表达式
//            if (nextIf(TokenType.Equal) != null) {
//                initialized = true;
//                analyseExpression();
//            }
//
//            // 分号
//            expect(TokenType.Semicolon);
//
//            // 加入符号表，请填写名字和当前位置（报错用）
//            String name = (String) nameToken.getValue();
//            addSymbol(name, initialized, false, nameToken.getStartPos());
//
//            // 如果没有初始化的话在栈里推入一个初始值
//            if (!initialized) {
//                instructions.add(new Instruction(Operation.LIT, 0));
//            }
//        }
//    }
//
//    private void analyseStatementSequence() throws CompileError {
//        // 语句序列 -> 语句*
//        // 语句 -> 赋值语句 | 输出语句 | 空语句
//
//        while (true) {
//            // 如果下一个 token 是……
//            var peeked = peek();
//            if (peeked.getTokenType() == TokenType.Ident) {
//                // 调用相应的分析函数
//                // 如果遇到其他非终结符的 FIRST 集呢？
//                /* 赋值语句 -> 标识符 '=' 表达式 ';' */
//                analyseAssignmentStatement();
//            } else if (peeked.getTokenType() == TokenType.Print) {
//                /* 输出语句 -> 'print' '(' 表达式 ')' ';' */
//                analyseOutputStatement();
//            } else if (peeked.getTokenType() == TokenType.Semicolon) {
//                next();
//            } else {
//                // 都不是，摸了
//                break;
//            }
//        }
//    }
//
//    private int analyseConstantExpression() throws CompileError {
//        // 常表达式 -> 符号? 无符号整数
//        boolean negative = false;
//        if (nextIf(TokenType.Plus) != null) {
//            negative = false;
//        } else if (nextIf(TokenType.Minus) != null) {
//            negative = true;
//        }
//
//        var token = expect(TokenType.Uint);
//
//        int value = (int) token.getValue();
//        if (negative) {
//            value = -value;
//        }
//
//        return value;
//    }
//
//    private void analyseExpression() throws CompileError {
//        // 表达式 -> 项 (加法运算符 项)*
//        // 项
//        analyseItem();
//
//        while (true) {
//            // 预读可能是运算符的 token
//            var op = peek();
//            if (op.getTokenType() != TokenType.Plus && op.getTokenType() != TokenType.Minus) {
//                break;
//            }
//
//            // 运算符
//            next();
//
//            // 项
//            analyseItem();
//
//            // 生成代码
//            if (op.getTokenType() == TokenType.Plus) {
//                instructions.add(new Instruction(Operation.ADD));
//            } else if (op.getTokenType() == TokenType.Minus) {
//                instructions.add(new Instruction(Operation.SUB));
//            }
//        }
//    }
//
//    private void analyseAssignmentStatement() throws CompileError {
//        // 赋值语句 -> 标识符 '=' 表达式 ';'
//
//        // 分析这个语句
//
//        // 标识符是什么？
//        var nameToken = expect(TokenType.Ident);
//        String name = (String) nameToken.getValue();
//        var symbol = symbolTable.get(name);
//        if (symbol == null) {
//            // 没有这个标识符
//            throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
//        } else if (symbol.isConstant) {
//            // 标识符是常量
//            throw new AnalyzeError(ErrorCode.AssignToConstant, nameToken.getStartPos());
//        }
//        // 设置符号已初始化
//        initializeSymbol(name, nameToken.getStartPos());
//
//        // 把结果保存
//        var offset = getOffset(name, nameToken.getStartPos());
//        instructions.add(new Instruction(Operation.STO, offset));
//
//        expect(TokenType.Equal);
//        analyseExpression();
//        expect(TokenType.Semicolon);
//    }
//
//    private void analyseOutputStatement() throws CompileError {
//        // 输出语句 -> 'print' '(' 表达式 ')' ';'
//
//        expect(TokenType.Print);
//        expect(TokenType.LParen);
//
//        analyseExpression();
//
//        expect(TokenType.RParen);
//        expect(TokenType.Semicolon);
//
//        instructions.add(new Instruction(Operation.WRT));
//    }
//
//    private void analyseItem() throws CompileError {
//        // 项 -> 因子 (乘法运算符 因子)*
//
//        // 因子
//        analyseFactor();
//
//        while (true) {
//            // 预读可能是运算符的 token
//            Token op = peek();
//            if (op.getTokenType() != TokenType.Mult && op.getTokenType() != TokenType.Div) {
//                break;
//            }
//
//            // 运算符
//            next();
//
//            // 因子
//            analyseFactor();
//
//            // 生成代码
//            if (op.getTokenType() == TokenType.Mult) {
//                instructions.add(new Instruction(Operation.MUL));
//            } else if (op.getTokenType() == TokenType.Div) {
//                instructions.add(new Instruction(Operation.DIV));
//            }
//        }
//    }
//
//    private void analyseFactor() throws CompileError {
//        // 因子 -> 符号? (标识符 | 无符号整数 | '(' 表达式 ')')
//
//        boolean negate;
//        if (nextIf(TokenType.Minus) != null) {
//            negate = true;
//            // 计算结果需要被 0 减
//            instructions.add(new Instruction(Operation.LIT, 0));
//        } else {
//            nextIf(TokenType.Plus);
//            negate = false;
//        }
//
//        if (check(TokenType.Ident)) {
//            // 是标识符
//            /* next */
//            var nameToken = next();
//
//            // 加载标识符的值
//            String name = (String) nameToken.getValue();
//            var symbol = symbolTable.get(name);
//            if (symbol == null) {
//                // 没有这个标识符
//                throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
//            } else if (!symbol.isInitialized) {
//                // 标识符没初始化
//                throw new AnalyzeError(ErrorCode.NotInitialized, nameToken.getStartPos());
//            }
//            var offset = getOffset(name, nameToken.getStartPos());
//            instructions.add(new Instruction(Operation.LOD, offset));
//        } else if (check(TokenType.Uint)) {
//            // 是整数
//            // 加载整数值
//            /* next */
//            int value = (int) next().getValue();
//            instructions.add(new Instruction(Operation.LIT, value));
//        } else if (check(TokenType.LParen)) {
//            // 是表达式
//            // 调用相应的处理函数
//            next();
//            analyseExpression();
//            expect(TokenType.RParen);
//        } else {
//            // 都不是，摸了
//            throw new ExpectedTokenError(List.of(TokenType.Ident, TokenType.Uint, TokenType.LParen), next());
//        }
//
//        if (negate) {
//            instructions.add(new Instruction(Operation.SUB));
//        }
//    }
}