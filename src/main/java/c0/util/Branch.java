package c0.util;

import c0.instruction.Instruction;
import c0.instruction.Operation;

import java.util.ArrayList;
import java.util.List;

public class Branch {
	public List<Instruction> condition = new ArrayList<>();
	public List<Instruction> brTrueAndBr = new ArrayList<>();
	public List<Instruction> handle = new ArrayList<>();
	public Instruction jump = new Instruction(Operation.BR, (long) 0);
	boolean isRet = false;

	public Branch() { }

	public Branch(List<Instruction> condition, Instruction jump, List<Instruction> handle) {
		this.condition = condition;
		this.handle = handle;
	}

	public List<Instruction> getCondition() {
		return condition;
	}

	public void setCondition(List<Instruction> condition) {
		this.condition = condition;
	}

	public List<Instruction> getBrTrueAndBr() {
		return brTrueAndBr;
	}

	public void setBrTrueAndBr(List<Instruction> brTrueAndBr) {
		this.brTrueAndBr = brTrueAndBr;
	}

	public Instruction getJump() {
		return jump;
	}

	public void setJump(Instruction jump) {
		this.jump = jump;
	}

	public List<Instruction> getHandle() {
		return handle;
	}

	public void setHandle(List<Instruction> handle) {
		this.handle = handle;
	}

	public boolean isRet() {
		return isRet;
	}

	public void setRet(boolean ret) {
		isRet = ret;
	}

	public int getBranchLen() {
		return condition.size() + brTrueAndBr.size()  + handle.size() + (isRet ? 0 : 1);
	}

	/**
	 * 设置跳数
	 *
	 * @param jumpStep
	 */
	public void setJumpStep(int jumpStep) {
		this.jump.setOff(jumpStep);
	}

	public List<Instruction> getBranchFullInstruction() {
		List<Instruction> instructions = new ArrayList<>();
		instructions.addAll(condition);
		if (brTrueAndBr.size() > 0) instructions.addAll(brTrueAndBr);
		instructions.addAll(handle);
		if (!isRet) instructions.add(jump);

		return instructions;
	}
}
