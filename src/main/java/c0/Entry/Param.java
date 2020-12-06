package c0.Entry;

import c0.tokenizer.TokenType;

public class Param {
	private String paramName;
	private TokenType paramType;
	private int paramOff = 0;

	public Param(String paramName, TokenType paramType, int paramOff) {
		this.paramName = paramName;
		this.paramType = paramType;
		this.paramOff = paramOff;
	}

	public String getParamName() {
		return paramName;
	}

	public void setParamName(String paramName) {
		this.paramName = paramName;
	}

	public TokenType getParamType() {
		return paramType;
	}

	public void setParamType(TokenType paramType) {
		this.paramType = paramType;
	}

	public int getParamOff() {
		return paramOff;
	}

	public void setParamOff(int paramOff) {
		this.paramOff = paramOff;
	}
}