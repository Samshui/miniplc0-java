package c0.Entry;

import c0.error.AnalyzeError;
import c0.error.ErrorCode;
import c0.instruction.Instruction;
import c0.tokenizer.TokenType;
import c0.util.Pos;
import c0.util.SymbolType;

import java.util.ArrayList;
import java.util.List;

public class FuncEntry {
	private String funcName;
	private TokenType funcType;
	private List<Instruction> instructions;
	private List<SymbolEntry> symbolTable;
	private int paramSlotCount = 0;
	private int varSlotCount = 0;

	public FuncEntry(String funcName) {
		this.funcName = funcName;
		this.funcType = TokenType.VOID_TY;
		this.instructions = new ArrayList<>();
		this.symbolTable = new ArrayList<>();
	}

	public FuncEntry(String funcName, TokenType funcType, int paramSlotCount, int varSlotCount) {
		this.funcName = funcName;
		this.funcType = funcType;
		this.paramSlotCount = paramSlotCount;
		this.varSlotCount = varSlotCount;
		this.instructions = new ArrayList<>();
		this.symbolTable = new ArrayList<>();
	}

	// getter & setter
	public String getFuncName() {
		return funcName;
	}

	public void setFuncName(String funcName) {
		this.funcName = funcName;
	}

	public List<SymbolEntry> getSymbolTable() {
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

	public List<Instruction> getInstructions() {
		return instructions;
	}

	public void setInstructions(List<Instruction> instructions) {
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
	 * 插入返回值占位符
	 */
	public void pushTypeSlot(TokenType type) {
		this.symbolTable.add(0, new SymbolEntry(type));

		for (int i = 1; i <= this.paramSlotCount; i++) {
			this.symbolTable.get(i).off += 1;
		}
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
		this.symbolTable.add(new SymbolEntry(name, type, SymbolType.PARAM, 2, paramSlotCount++, false, true));
	}

	/**
	 * 查找函数参数
	 *
	 * @param name
	 * @return
	 */
	public int searchParam(String name) {
		for (int i = 0; i < this.paramSlotCount; i++)
			if (this.symbolTable.get(i).getName().equals(name))
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
		// 如果深度不一样，可以允许重名
		if (searchSymbol(name) != null && searchSymbol(name).deep == deep) {
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

	/**
	 * 查询函数体当前深度的符号
	 *
	 * @param name
	 * @param deep
	 * @return
	 */
	public SymbolEntry searchSymbolOnlySameDeep(String name, int deep) {
		for (SymbolEntry s : this.symbolTable) {
			if (s.getName().equals(name) && s.getDeep() == deep) return s;
		}
		return null;
	}

	/**
	 * 在函数实体中以深度迭代查找一个符号（仅限在函数内的符号表）
	 *
	 * @param name
	 * @param deep
	 * @return
	 */
	public Object searchSymbolByDeepIterate(String name, int deep, Pos pos) throws AnalyzeError {
		if (deep > 1) {
			for (int i = deep; i > 1; i--) {
				SymbolEntry s = searchSymbolOnlySameDeep(name, i);
				if (s != null) return s;
			}
			return null;
		} else {
			// 此时是全局的符号，不可以在函数内查询
			throw new AnalyzeError(ErrorCode.SymbolShouldInGlobal, pos);
		}
	}

	/**
	 * 获取当前函数中局部变量的偏移值
	 *
	 * @param name
	 * @return
	 */
	public long getLocalSymbolOff(String name) {
		for (SymbolEntry s : this.symbolTable) {
			if (s.getName().equals(name)) return s.getOff();
		}
		return (long) -1;
	}

	/**
	 * 增加函数体的指令集
	 *
	 * @param instructions
	 */
	public void addAllInstructions(List<Instruction> instructions) {
		this.instructions.addAll(instructions);
	}

	public void addOneInstruction(Instruction instruction) {
		this.instructions.add(instruction);
	}

	public void setSymbolInitState(int off) {
		symbolTable.get(off).setInitialized(true);
	}

	@Override
	public String toString() {
		String sSymbol = new String("");
		String sInstruc = new String("");

		for (SymbolEntry s: symbolTable) {
			sSymbol += s.toString();
		}

		for (int i = 0; i < instructions.size(); i++) {
			sInstruc += (i + ": " + instructions.get(i).toString());
		}

		return new StringBuilder()
				.append("funcName:[" + funcName + "]")
				.append("\tfuncType:" + funcType.toString() + "")
				.append("\tvarSlotCount:" + varSlotCount + "")
				.append("\tparamSlotCount:" + paramSlotCount + "\n")
				.append("funcSymbol (with params):\n").append(sSymbol)
				.append("funcInstructions:\n").append(sInstruc)
				.append("\n")
				.toString();
	}
}

