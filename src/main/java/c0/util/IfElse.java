package c0.util;

import c0.instruction.Instruction;
import c0.instruction.Operation;

import java.util.ArrayList;
import java.util.List;

public class IfElse {
	private List<Branch> branches = new ArrayList<>();
	private int ifBranchesCount = 0;
	private int elseBranchesCount = 0;

	public IfElse() { }

	public IfElse(List<Branch> branches) {
		this.branches = branches;
	}

	public List<Branch> getBranches() {
		return branches;
	}

	public void setBranches(List<Branch> branches) {
		this.branches = branches;
	}

	public int getIfBranchesCount() {
		return ifBranchesCount;
	}

	public void setIfBranchesCount(int ifBranchesCount) {
		this.ifBranchesCount = ifBranchesCount;
	}

	public int getElseBranchesCount() {
		return elseBranchesCount;
	}

	public void setElseBranchesCount(int elseBranchesCount) {
		this.elseBranchesCount = elseBranchesCount;
	}

	/**
	 * 向ifElse分支组中加入if分支
	 *
	 * @param conditions
	 * @param handles
	 */
	public void addOneIfBranch(List<Instruction> conditions, List<Instruction> handles, boolean isRet) {
		Branch ifBranch = new Branch();

		ifBranch.setCondition(conditions);

		ifBranch.setHandle(handles);

		System.out.println(handles.size());

		ifBranch.brTrueAndBr.add(new Instruction(Operation.BR_TRUE, (long) 1));
		ifBranch.brTrueAndBr.add(new Instruction(Operation.BR, (long) handles.size() + (isRet ? 0 : 1)));

		ifBranch.setRet(isRet);
		this.branches.add(ifBranch);
		this.ifBranchesCount ++;
	}

	/**
	 * 向ifElse分支中加入else分支
	 *
	 * @param handles
	 */
	public void addOneElseBranch(List<Instruction> handles, boolean isRet) {
		Branch elseBranch = new Branch();
		elseBranch.setHandle(handles);
		elseBranch.setRet(isRet);
		// elseBranch.brTrueAndBr.add(new Instruction(Operation.BR, (long) 0));

		this.branches.add(elseBranch);
		this.elseBranchesCount ++;
	}

	/**
	 * 生成完整分支指令集
	 *
	 * @return
	 */
	public List<Instruction> generate() {
		int len = 0;
		// 由于采取的是 拉链 + 回填，所以处理的方法应该是从最后一个branch往前推长度（推到整数第二个）
		// 然后顺序生成指令集
		for (int i = this.branches.size() - 1; i > 0; i--) {
			int lastBranchLen = this.branches.get(i).getBranchLen();
			len += lastBranchLen;

			// 回填入上一个，需要保证该分支无ret语句
			if (!this.branches.get(i - 1).isRet)
				this.branches.get(i - 1).setJumpStep(len);
		}

		// 顺序生成
		List<Instruction> instructions = new ArrayList<>();
		for (int i = 0; i < this.branches.size(); i++)
			instructions.addAll(this.branches.get(i).getBranchFullInstruction());

		return instructions;
	}
}
