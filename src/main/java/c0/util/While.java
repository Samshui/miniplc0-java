package c0.util;

import c0.instruction.Instruction;
import c0.instruction.Operation;

import java.util.ArrayList;
import java.util.List;

public class While {
	private Instruction whileBegin = new Instruction(Operation.BR, (long) 0);
	private List<Instruction> condition = new ArrayList<>();
	private Instruction brTrue = new Instruction(Operation.BR_TRUE, (long) 1);
	private Instruction br = new Instruction(Operation.BR, (long) 0); // need give value
	private List<Instruction> handle = new ArrayList<>();
	private Instruction jumpBack = new Instruction(Operation.BR, (long) 0); // need judge or give value
	private boolean isRet = false;

	public While() { }

	public Instruction getWhileBegin() {
		return whileBegin;
	}

	public List<Instruction> getCondition() {
		return condition;
	}

	public void setCondition(List<Instruction> condition) {
		this.condition = condition;
	}

	public Instruction getBrTrue() {
		return brTrue;
	}

	public Instruction getBr() {
		return br;
	}

	public void setBr(Instruction br) {
		this.br = br;
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

	public Instruction getJumpBack() {
		return jumpBack;
	}

	public void setJumpBack(Instruction jumpBack) {
		this.jumpBack = jumpBack;
	}

	/**
	 * 设置下跳次数（跳出while）
	 *
	 * @param jumpBack
	 */
	public void setBrStep(int brStep) {
		this.br.setOff(brStep);
	}

	/**
	 * 设置回跳次数
	 *
	 * @param brBackStep
	 */
	public void setBrBackStep(int brBackStep) {
		this.jumpBack.setOff(brBackStep);
	}

	/**
	 * 生成完整while指令集
	 *
	 * @return
	 */
	public List<Instruction> generate() {
		List<Instruction> instructions = new ArrayList<>();

		instructions.add(whileBegin);
		instructions.addAll(condition);
		instructions.add(brTrue);

		if (isRet) {
			this.setBrStep(handle.size());
			instructions.add(br);
			instructions.addAll(handle);
		} else {
			this.setBrStep(handle.size() + 1);
			instructions.add(br);
			instructions.addAll(handle);
			this.setBrBackStep(-(condition.size() + handle.size() + 3));
			instructions.add(jumpBack);
		}

		return instructions;
	}
}
