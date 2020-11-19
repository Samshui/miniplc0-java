package c0.error;

public enum ErrorCode {
	NoError, // Should be only used internally.

	AssignToConstant,
	ConstantNeedValue,
	DuplicateDeclaration,            	// 重复定义					[语法分析]
	EOF,
	InvalidInput,                    	// 无效输入					[词法分析]
	InvalidIdentifier,               	// 无效标识符					[词法分析]
	InvalidEscapeSequence,           	// 无效转义序列				[词法分析]
	InvalidDouble,						// 无效浮点数					[词法分析]
	InvalidChar,                    	// 无效字符常量				[词法分析]
	InvalidVariableDeclaration,
	IntegerOverflow,
	IncompleteExpression,
	IncompleteString,                	// 字符串常量左右引号无法对应	[词法分析]
	IncompleteChar,                    	// 字符常量左右引号无法对应		[词法分析]
	StreamError,
	NeedIdentifier,
	NoSemicolon,
	NotDeclared,
	NotInitialized,
	InvalidAssignment,
	InvalidPrint,
	ExpectedToken
}
