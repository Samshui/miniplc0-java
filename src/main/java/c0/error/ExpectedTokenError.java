package c0.error;

import java.util.ArrayList;
import java.util.List;

import c0.tokenizer.Token;
import c0.tokenizer.TokenType;
import c0.util.Pos;

public class ExpectedTokenError extends CompileError {
	private static final long serialVersionUID = 1L;

	List<TokenType> expecTokenType;
	Token token;

	/**
	 * @param expectedTokenType
	 * @param token
	 * @param code
	 * @param pos
	 */
	public ExpectedTokenError(TokenType expectedTokenType, Token token) {
		this.expecTokenType = new ArrayList<>();
		this.expecTokenType.add(expectedTokenType);
		this.token = token;
	}

	/**
	 * @param expectedTokenType
	 * @param token
	 * @param code
	 * @param pos
	 */
	public ExpectedTokenError(List<TokenType> expectedTokenType, Token token) {
		this.expecTokenType = expectedTokenType;
		this.token = token;
	}

	@Override
	public ErrorCode getErr() {
		return ErrorCode.ExpectedToken;
	}

	@Override
	public Pos getPos() {
		return token.getStartPos();
	}

	@Override
	public String toString() {
		return new StringBuilder()
				.append("\nAnalyse error. Expected ")
				.append(expecTokenType)
				.append(" at ")
				.append(token.getStartPos())
				.append("\ngot: ")
				.append(token.toStringAlt())
				.append("\n")
				.toString();
	}
}
