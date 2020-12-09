package c0.instruction;

public enum Operation {
	NOP,

	PUSH,
	POP,
	POPN,

	DUP,

	LOCA,
	ARGA,
	GLOBA,
	LOAD_8,
	LOAD_16,
	LOAD_32,
	LOAD_64,

	STORE_8,
	STORE_16,
	STORE_32,
	STORE_64,

	ALLOC,
	FREE,
	STACKALLOC,

	ADD_I,
	SUB_I,
	MUL_I,
	DIV_I,
	ADD_F,
	SUB_F,
	MUL_F,
	DIV_F,
	DIV_U,

	SHL,
	SHR,

	AND,
	OR,
	XOR,
	NOT,

	CMP_I,
	CMP_U,
	CMP_F,

	NEG_I,
	NEG_F,

	ITOF,
	FTOI,

	SHRL,

	SET_LT,
	SET_GT,

	BR,
	BR_FALSE,
	BR_TRUE,

	CALL,
	RET,
	CALLNAME,

	SCAN_I,
	SCAN_C,
	SCAN_F,

	PRINT_I,
	PRINT_C,
	PRINT_F,
	PRINT_S,
	PRINTLN,

	PANIC;

	@Override
	public String toString() {
		switch (this) {
			case NOP: return "nothing";
			case PUSH: return "push";
			case POP: return "pop";
			case POPN: return "popN";
			case LOCA: return "loca";
			case ARGA: return "arga";
			case GLOBA: return "globa";
			case LOAD_64: return "load64";
			case STORE_64: return "store64";
			case STACKALLOC: return "stackAlloc";
			case ADD_I: return "addI";
			case ADD_F: return "addF";
			case SUB_I: return "subI";
			case SUB_F: return "subF";
			case MUL_I: return "mulI";
			case MUL_F: return "mulF";
			case DIV_I: return "divI";
			case DIV_F: return "divF";
			case NOT: return "not";
			case CMP_I: return "cmpI";
			case CMP_F: return "cmpF";
			case NEG_I: return "negI";
			case NEG_F: return "negF";
			case SET_LT: return "setLT";
			case SET_GT: return "setGT";
			case CALL: return "call";
			case RET: return "ret";
			case FTOI: return "FToI";
			case ITOF: return "IToF";
			case BR: return "br";
			case BR_TRUE: return "brTrue";
			default: return "others";
		}
	}
}
