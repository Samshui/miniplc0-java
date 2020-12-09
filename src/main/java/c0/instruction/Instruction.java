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

	@Override
	public String toString() {
		if (this.len == 2) return this.opt.toString() + "(" + off + ")\n";
		else return this.opt.toString() + "\n";
	}
}
