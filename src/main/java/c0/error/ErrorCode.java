package c0.error;

public enum ErrorCode {
	NoError, // Should be only used internally.

	AssignToConstant,
	ConstantNeedValue,
	CannotGetOff,						// 无法获取偏移量				[语义分析]
	DuplicateGlobalVar,            		// 变量重复定义				[语义分析]
	DuplicateParamName,					// 参数命名重复				[语义分析]
	DuplicateFuncName,					// 函数命名重复				[语义分析]
	DuplicateName,						// 符号命名重复
	EOF,
	InvalidInput,                    	// 无效输入					[词法分析]
	InvalidIdentifier,               	// 无效标识符					[词法分析]
	InvalidEscapeSequence,           	// 无效转义序列				[词法分析]
	InvalidDouble,						// 无效浮点数					[词法分析]
	InvalidChar,                    	// 无效字符常量				[词法分析]
	InvalidVariableDeclaration,
	IntegerOverflow,
	IncompleteExpression,				// 表达不完整					[语法分析]
	IncompleteString,                	// 字符串常量左右引号无法对应	[词法分析]
	IncompleteChar,                    	// 字符常量左右引号无法对应		[词法分析]
	IfElseNotMatch,						// if-else不匹配				[语法分析]
	StreamError,
	ShouldReturn,						// 函数需要返回值				[语义分析]
	ShouldNotReturn,					// 函数不需要返回值			[语义分析]
	NeedIdentifier,
	NoSemicolon,
	NotComplete, 						// 不完整					[语义分析]
	NotDeclared,						// 符号未声明					[语义分析]
	NotInitialized,
	InvalidAssignment,
	InvalidPrint,
	InvalidIndent,						// 标识符无效					[语法分析]
	ExpectedToken,
	ExprERROR
}
