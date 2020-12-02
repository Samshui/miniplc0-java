package c0.analyser;

public class SymbolEntry {
	String name;
	boolean isConstant;
	boolean isInitialized;
	int type;
	int offset;
	int funcIndex;
	Object value;

	/**
	 * @param name 符号名称
	 * @param isConstant 符号对应变量、函数是否为const
	 * @param isInitialized 符号对应变量、函数是否被声明（被定义）
	 * @param type 符号对应类型（变量、函数...）
	 * @param offset 符号在伪栈中的偏移量
	 */
	public SymbolEntry(String name, boolean isConstant, boolean isInitialized, int type, int offset, Object value) {
		this.name = name;
		this.isConstant = isConstant;
		this.isInitialized = isInitialized;
		this.type = type;
		this.offset = offset;
		this.funcIndex = -1;
		this.value = value;
	}

	/**
	 * @param name
	 * @param isConstant
	 * @param isInitialized
	 * @param type
	 * @param offset
	 * @param funcIndex 在funcTable中的index映射
	 * @param value
	 */
	public SymbolEntry(String name, boolean isConstant, boolean isInitialized, int type, int offset, int funcIndex, Object value) {
		this.name = name;
		this.isConstant = isConstant;
		this.isInitialized = isInitialized;
		this.type = type;
		this.offset = offset;
		this.funcIndex = funcIndex;
		this.value = value;
	}

	/* setter & getter */
	public int getOffset() {
		return offset;
	}

	public void setOffset(int offset) {
		this.offset = offset;
	}

	public boolean isConstant() {
		return isConstant;
	}

	public void setConstant(boolean isConstant) {
		this.isConstant = isConstant;
	}

	public boolean isInitialized() {
		return isInitialized;
	}

	public void setInitialized(boolean isInitialized) {
		this.isInitialized = isInitialized;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Object getValue() {
		return value;
	}

	public void setValue(Object value) {
		this.value = value;
	}

	public int getFuncIndex() {
		return funcIndex;
	}

	public void setFuncIndex(int funcIndex) {
		this.funcIndex = funcIndex;
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
