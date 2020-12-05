package c0.table;

import c0.analyser.FuncEntry;
import c0.analyser.SymbolEntry;
import c0.error.AnalyzeError;
import c0.instruction.Instruction;
import c0.tokenizer.Token;
import c0.tokenizer.TokenType;
import c0.util.Pos;
import c0.util.SymbolType;

import java.util.ArrayList;

public class Table {
	private ArrayList<FuncEntry> funcTable;
	private ArrayList<SymbolEntry> symTable;

	/**
	 * 构造函数
	 */
	public Table() {
		this.funcTable = new ArrayList<>();
		this.symTable = new ArrayList<>();
	}

	/**
	 * 添加函数
	 *
	 * @param name
	 * @param type
	 */
	public void addFuncEntry(String name) {
		this.funcTable.add(new FuncEntry(name));
	}

	/**
	 * 设置函数属性
	 *
	 * @param type
	 */
	public void addFuncType(TokenType type) {
		this.funcTable.get(this.funcTable.size() - 1).setFuncType(type);
	}

	public void addParam(String name, Token paramToken, Pos currentPos) throws AnalyzeError {
		this.funcTable.get(this.funcTable.size() - 1).addParam(name, paramToken.getTokenType(), currentPos);
	}

	/**
	 * 添加全局符号
	 *
	 * @param name
	 * @param type
	 * @param symbolType
	 * @param offset
	 * @param deep
	 * @param value
	 * @param isConstant
	 * @param isInitialized
	 * @throws AnalyzeError
	 */
	public void addSymbolEntry(String name, TokenType type, SymbolType symbolType,
								int offset, int deep, Object value,
								boolean isConstant, boolean isInitialized) throws AnalyzeError {
		// todo 查表防止重复
		this.symTable.add(new SymbolEntry(name, type, symbolType, offset, deep, value, isConstant, isInitialized));
	}

	/**
	 * 添加函数参数
	 *
	 * @param name
	 * @param type
	 * @param pos
	 * @throws AnalyzeError
	 */
	private void addParam(String name, TokenType type, Pos pos) throws AnalyzeError {
		this.funcTable.get(this.funcTable.size() - 1).addParam(name, type, pos);
	}


}
