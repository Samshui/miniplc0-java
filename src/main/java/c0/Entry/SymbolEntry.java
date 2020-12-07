package c0.Entry;

import c0.tokenizer.TokenType;
import c0.util.SymbolType;

public class SymbolEntry {
	String name;
	TokenType type;
	SymbolType symbolType;
	long off = -1;
	int deep;
	boolean isConstant;
	boolean isInitialized;

	/**
	 * 符号实体
	 *
	 * @param name 符号名
	 * @param type 符号类型
	 * @param isConstant 符号是否为const
	 * @param isInitialized 符号是否被初始化
	 */
	public SymbolEntry(String name, TokenType type, SymbolType symbolType, int deep, boolean isConstant, boolean isInitialized) {
		this.name = name;
		this.type = type;
		this.symbolType = symbolType;
		this.deep = deep;
		this.isConstant = isConstant;
		this.isInitialized = isInitialized;
	}

	/**
	 *
	 * @param name
	 * @param type
	 * @param symbolType
	 * @param deep
	 * @param off
	 * @param isConstant
	 * @param isInitialized
	 */
	public SymbolEntry(String name, TokenType type, SymbolType symbolType, int deep, long off, boolean isConstant, boolean isInitialized) {
		this.name = name;
		this.type = type;
		this.symbolType = symbolType;
		this.deep = deep;
		this.off = off;
		this.isConstant = isConstant;
		this.isInitialized = isInitialized;
	}

	/* getter & setter */
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public TokenType getType() {
		return type;
	}

	public void setType(TokenType type) {
		this.type = type;
	}

	public boolean isConstant() {
		return isConstant;
	}

	public void setConstant(boolean constant) {
		isConstant = constant;
	}

	public boolean isInitialized() {
		return isInitialized;
	}

	public void setInitialized(boolean initialized) {
		isInitialized = initialized;
	}

	public SymbolType getSymbolType() {
		return symbolType;
	}

	public void setSymbolType(SymbolType symbolType) {
		this.symbolType = symbolType;
	}

	public long getOff() {
		return off;
	}

	public void setOff(long off) {
		this.off = off;
	}

	public int getDeep() {
		return deep;
	}

	public void setDeep(int deep) {
		this.deep = deep;
	}

	@Override
	public String toString() {
		return new StringBuilder()
				.append("["+ name + "]:")
				.append("\ttype:" + type.toString())
				.append("\tsymType:" + symbolType.toString())
				.append("\tis_const:" + isConstant)
				.append("\tis_init:" + isInitialized)
				.append("\toff:" + off)
				.append("\tdeep:" + deep + "\n")
				.toString();
	}
}
