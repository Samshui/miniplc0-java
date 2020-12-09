package c0.util;

import c0.instruction.Instruction;
import c0.tokenizer.TokenType;

import java.util.ArrayList;
import java.util.List;

public class LoadUp {
	public List<Instruction> instructions = new ArrayList<>();
	public TokenType type;
	public boolean isConstant;

	public LoadUp(List<Instruction> instructions, TokenType type, boolean isConstant) {
		this.instructions = instructions;
		this.type = type;
		this.isConstant = isConstant;
	}

	public LoadUp() { }

	public List<Instruction> getInstructions() {
		return instructions;
	}

	public void setInstructions(List<Instruction> instructions) {
		this.instructions = instructions;
	}

	public TokenType getType() {
		return type;
	}

	public void setType(TokenType type) {
		this.type = type;
	}

	public boolean isConstant() {
		return isConstant;
	}

	public void setConstant(boolean constant) {
		isConstant = constant;
	}
}
