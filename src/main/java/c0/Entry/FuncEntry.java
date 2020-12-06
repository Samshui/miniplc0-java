package c0.Entry;

import c0.error.AnalyzeError;
import c0.error.ErrorCode;
import c0.instruction.Instruction;
import c0.tokenizer.TokenType;
import c0.util.Pos;
import c0.util.SymbolType;

import java.util.ArrayList;

public class FuncEntry {
	private String funcName;
	private TokenType funcType;
	private ArrayList<Instruction> instructions;
	private ArrayList<SymbolEntry> symbolTable;
	private ArrayList<Param> params;
	private int paramSlotCount = 0;
	private int varSlotCount = 0;

	public FuncEntry(String funcName) {
		this.funcName = funcName;
		this.funcType = TokenType.VOID_TY;
		this.instructions = new ArrayList<>();
		this.params = new ArrayList<>();
		this.symbolTable = new ArrayList<>();
	}

	// getter & setter
	public String getFuncName() {
		return funcName;
	}

	public void setFuncName(String funcName) {
		this.funcName = funcName;
	}

	public ArrayList<SymbolEntry> getSymbolTable() {
		return symbolTable;
	}

	public void setSymbolTable(ArrayList<SymbolEntry> symbolTable) {
		this.symbolTable = symbolTable;
	}

	public TokenType getFuncType() {
		return funcType;
	}

	public void setFuncType(TokenType funcType) {
		this.funcType = funcType;
	}

	public ArrayList<Instruction> getInstructions() {
		return instructions;
	}

	public void setInstructions(ArrayList<Instruction> instructions) {
		this.instructions = instructions;
	}

	public int getParamSlotCount() {
		return paramSlotCount;
	}

	public void setParamSlotCount(int paramSlotCount) {
		this.paramSlotCount = paramSlotCount;
	}

	public int getVarSlotCount() {
		return varSlotCount;
	}

	public void setVarSlotCount(int varSlotCount) {
		this.varSlotCount = varSlotCount;
	}

	/**
	 * 增加参数
	 *
	 * @param name
	 * @param type
	 * @throws AnalyzeError 如果参数重名，则会报错
	 */
	public void addParam(String name, TokenType type, Pos currentPos) throws AnalyzeError {
		if (searchParam(name) != -1) {
			throw new AnalyzeError(ErrorCode.DuplicateParamName, currentPos);
		}
		this.params.add(new Param(name, type, paramSlotCount++));
	}

	/**
	 * 查找函数参数
	 *
	 * @param name
	 * @return
	 */
	public int searchParam(String name) {
		for (int i = 0; i < this.params.size(); i++)
			if (this.params.get(i).getParamName().equals(name))
				return i;
		return -1;
	}

	/**
	 * 增加局部变量
	 *
	 * @param name
	 * @param type
	 * @param symbolType
	 * @param deep
	 * @param isConstant
	 * @param isInitialized
	 * @param currentPos
	 * @throws AnalyzeError
	 */
	public void addSymbol(String name,
						  TokenType type, SymbolType symbolType,
						  int deep, boolean isConstant, boolean isInitialized,
						  Pos currentPos) throws AnalyzeError {
		if (searchSymbol(name) != null) {
			throw new AnalyzeError(ErrorCode.DuplicateName, currentPos);
		}
		SymbolEntry symbolEntry = new SymbolEntry(name, type, symbolType, deep, isConstant, isInitialized);

		// 函数的局部变量偏移
		symbolEntry.setOff(this.varSlotCount);
		this.symbolTable.add(symbolEntry);

		// 局部偏移增加
		this.varSlotCount++;
	}

	/**
	 * 查找函数中的符号
	 *
	 * @param name
	 * @return
	 */
	public SymbolEntry searchSymbol(String name) {
		for (SymbolEntry s : this.symbolTable) {
			if (s.getName().equals(name)) return s;
		}
		return null;
	}
}

