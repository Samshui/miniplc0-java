package c0.analyser;

import c0.error.AnalyzeError;
import c0.error.ErrorCode;
import c0.instruction.Instruction;
import c0.tokenizer.Token;
import c0.tokenizer.TokenType;
import c0.util.Pos;

import java.util.ArrayList;

public class FuncEntry {
	private String funcName;
	private TokenType funcType;
	private ArrayList<Instruction> instructions;
	private ArrayList<SymbolEntry> symbolTable;
	private ArrayList<Param> params;
	private int paramSlotCount;
	private int varSlotCount;

	public FuncEntry(String funcName) {
		this.funcName = funcName;
		this.funcType = null;
		this.instructions = new ArrayList<>();
		this.params = new ArrayList<>();
		this.symbolTable = new ArrayList<>();
		this.paramSlotCount = 0;
		this.varSlotCount = 0;
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
		this.params.add(new Param(name, type));
	}

	/**
	 * 查找函数参数
	 *
	 * @param name
	 * @return
	 */
	public int searchParam(String name) {
		for (int i = 0; i < this.params.size(); i++)
			if (this.params.get(i).paramName.equals(name))
				return i;
		return -1;
	}
}

/**
 * 参数类
 */
class Param {
	public String paramName;
	public TokenType paramType;

	public Param(String paramName, TokenType paramType) {
		this.paramName = paramName;
		this.paramType = paramType;
	}
}
