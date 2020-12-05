package c0.analyser;

import c0.tokenizer.TokenType;
import c0.util.SymbolType;

public class SymbolEntry {
	String name;
	TokenType type;
	SymbolType symbolType;
	int offset;
	int deep;
	Object value;
	boolean isConstant;
	boolean isInitialized;

	/**
	 * 符号实体
	 *
	 * @param name 符号名
	 * @param type 符号类型
	 * @param offset 符号在栈中的偏移
	 * @param value 符号的值
	 * @param isConstant 符号是否为const
	 * @param isInitialized 符号是否被初始化
	 */
	public SymbolEntry(String name, TokenType type, SymbolType symbolType, int offset, int deep, Object value, boolean isConstant, boolean isInitialized) {
		this.name = name;
		this.type = type;
		this.symbolType = symbolType;
		this.offset = offset;
		this.deep = deep;
		this.value = value;
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

	public int getOffset() {
		return offset;
	}

	public void setOffset(int offset) {
		this.offset = offset;
	}

	public Object getValue() {
		return value;
	}

	public void setValue(Object value) {
		this.value = value;
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

	@Override
	public String toString() {
		return new StringBuilder()
				.append("["+ name + "]:")
				.append("\ttype:" + type)
				.append("\tis_const:" + isConstant)
				.append("\tis_init:" + isInitialized)
				.append("\toffset:" + offset)
				.toString();
	}
}
