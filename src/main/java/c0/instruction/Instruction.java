package c0.instruction;

import java.util.Objects;

public class Instruction {
	Number x;
	private Operation opt;

	public Instruction(Operation opt) {
		this.opt = opt;
		this.x = null;
	}

	public Instruction(Operation opt, Integer x) {
		this.opt = opt;
		this.x = x;
	}

	public Instruction(Operation opt, Double x) {
		this.opt = opt;
		this.x = x;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;
		Instruction that = (Instruction) o;
		return opt == that.opt && Objects.equals(x, that.x);
	}

	@Override
	public int hashCode() {
		return Objects.hash(opt, x);
	}

	public Operation getOpt() {
		return opt;
	}

	public void setOpt(Operation opt) {
		this.opt = opt;
	}

	public Number getX() {
		return x;
	}

	public void setX(Integer x) {
		this.x = x;
	}

	@Override
	public String toString() {
		return "this is a instruction";
	}
}
