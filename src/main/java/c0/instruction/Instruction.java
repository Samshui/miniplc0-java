package c0.instruction;

import java.util.Objects;

public class Instruction {
	long off;
	int len;
	private Operation opt;

	public Instruction(Operation opt) {
		this.opt = opt;
		this.len = 1;
	}

	public Instruction(Operation opt, long off) {
		this.off = off;
		this.opt = opt;
		this.len = 2;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;
		Instruction that = (Instruction) o;
		return opt == that.opt && Objects.equals(off, that.off);
	}

	@Override
	public int hashCode() {
		return Objects.hash(opt, off);
	}

	public long getOff() {
		return off;
	}

	public void setOff(long off) {
		this.off = off;
	}

	public Operation getOpt() {
		return opt;
	}

	public void setOpt(Operation opt) {
		this.opt = opt;
	}

	public int getLen() {
		return len;
	}

	public void setLen(int len) {
		this.len = len;
	}

	@Override
	public String toString() {
		if (this.len == 2) return this.opt.toString() + "(" + off + ")\n";
		else return this.opt.toString() + "\n";
	}

	public int getType() {
		switch (this.opt) {
			case LOCA:
				return 0xa;
			case ARGA:
				return 0xb;
			case GLOBA:
				return 0xc;
			case ADD_I:
				return 0x20;
			case SUB_I:
				return 0x21;
			case MUL_I:
				return 0x22;
			case DIV_I:
				return 0x23;
			case STORE_64:
				return 0x17;
			case CMP_I:
				return 0x30;
			case PUSH:
				return 0x1;
			case SET_LT:
				return 0x39;
			case BR_FALSE:
				return 0x42;
			case BR_TRUE:
				return 0x43;
			case SET_GT:
				return 0x3a;
			case BR:
				return 0x41;
			case RET:
				return 0x49;
			case CALLNAME:
				return 0x4a;
			case LOAD_64:
				return 0x13;
			case STACKALLOC:
				return 0x1a;
			case CALL:
				return 0x48;
			case NOT:
				return 0x2e;
			case NEG_I:
				return 0x34;
			default:
				return 0xFF;
		}
	}
}
