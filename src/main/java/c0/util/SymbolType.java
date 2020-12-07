package c0.util;

public enum SymbolType {
	FUNC,
	VAR,
	PARAM,
	STRING;

	@Override
	public String toString() {
		switch (this) {
			case FUNC: return "func";
			case VAR: return "var";
			case PARAM: return "param";
			case STRING: return "String";
			default: return "unknown symType";
		}
	}
}


