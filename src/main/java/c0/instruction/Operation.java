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
			case NOP: return "Nothing";
			case PUSH: return "Push";
			case POP: return "Pop";
			case POPN: return "PopN";
			case LOCA: return "LocA";
			case ARGA: return "ArgA";
			case GLOBA: return "GlobA";
			case LOAD_64: return "Load64";
			case STORE_64: return "Store64";
			case STACKALLOC: return "StackAlloc";
			case ADD_I: return "AddI";
			case ADD_F: return "AddF";
			case SUB_I: return "SubI";
			case SUB_F: return "SubF";
			case MUL_I: return "MulI";
			case MUL_F: return "MulF";
			case DIV_I: return "DivI";
			case DIV_F: return "DivF";
			case NOT: return "Not";
			case CMP_I: return "CmpI";
			case CMP_F: return "CmpF";
			case NEG_I: return "NegI";
			case NEG_F: return "NegF";
			case SET_LT: return "SetLt";
			case SET_GT: return "SetGt";
			case CALL: return "Call";
			case RET: return "Ret";
			case FTOI: return "FToI";
			case ITOF: return "IToF";
			case BR: return "Br";
			case BR_TRUE: return "BrTrue";
			case CALLNAME: return "CallName";
			default: return "Others";
		}
	}
}
