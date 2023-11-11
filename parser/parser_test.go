package parser

import (
	"fmt"
	"mogottsch/interpreter-go/ast"
	"mogottsch/interpreter-go/lexer"
	"testing"
)

func TestLetStatements(t *testing.T) {
	input := `
let x = 5;
let y = 10;
let foobar = 838383;
`

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)
	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 3 {
		t.Fatalf(
			"program.Statements does not contain 3 statements. got=%d",
			len(program.Statements),
		)
	}

	tests := []struct {
		expectedIdentifier string
	}{
		{"x"},
		{"y"},
		{"foobar"},
	}

	for i, tt := range tests {
		stmt := program.Statements[i]

		if !testLetStatement(t, stmt, tt.expectedIdentifier) {
			return
		}
	}
}

func TestReturnStatements(t *testing.T) {
	input := `
return 5;
return 10;
return 993322;
`

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 3 {
		t.Fatalf(
			"program.Statements does not contain 3 statements. got=%d",
			len(program.Statements),
		)
	}
	for _, statement := range program.Statements {
		returnStatement, ok := statement.(*ast.ReturnStatement)
		if !ok {
			t.Errorf(
				"statement not *ast.ReturnStatement. got=%T",
				statement,
			)
		}
		if returnStatement.TokenLiteral() != "return" {
			t.Errorf(
				"returnStatement.TokenLiteral not 'return', got %q",
				returnStatement.TokenLiteral(),
			)
		}
	}

}

func TestIdentifierExpression(t *testing.T) {
	input := "foobar;"

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 1 {
		t.Fatalf(
			"program.Statements does not contain 1 statements. got=%d",
			len(program.Statements),
		)
	}

	statement, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf(
			"program.Statements[0] is not ast.ExpressionStatement. got=%T",
			program.Statements[0],
		)
	}

	identifier, ok := statement.Expression.(*ast.Identifier)
	if !ok {
		t.Fatalf(
			"expression not *ast.Identifier. got=%T",
			statement.Expression,
		)
	}

	expected := "foobar"
	if identifier.Value != expected {
		t.Fatalf(
			"identifier.Value not %s. got=%s",
			expected,
			identifier.Value,
		)
	}

	if identifier.TokenLiteral() != expected {
		t.Fatalf(
			"identifier.TokenLiteral() not %s. got=%s",
			expected,
			identifier.TokenLiteral(),
		)
	}
}

func TestIntegerLiteralExpression(t *testing.T) {
	input := "5;"

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 1 {
		t.Fatalf(
			"program.Statements does not contain 1 statements. got=%d",
			len(program.Statements),
		)
	}

	statement, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf(
			"program.Statements[0] is not ast.ExpressionStatement. got=%T",
			program.Statements[0],
		)
	}

	integerLiteral, ok := statement.Expression.(*ast.IntegerLiteral)
	if !ok {
		t.Fatalf(
			"expression not *ast.IntegerLiteral. got=%T",
			statement.Expression,
		)
	}

	expected := int64(5)
	if integerLiteral.Value != expected {
		t.Fatalf(
			"integerLiteral.Value not %d. got=%d",
			expected,
			integerLiteral.Value,
		)
	}

	if integerLiteral.TokenLiteral() != "5" {
		t.Fatalf(
			"integerLiteral.TokenLiteral() not %d. got=%s",
			expected,
			integerLiteral.TokenLiteral(),
		)
	}
}

func TestParsingPrefixExpressions(t *testing.T) {
	prefixTests := []struct {
		input    string
		operator string
		value    int64
	}{
		{"!5;", "!", 5},
		{"-15;", "-", 15},
	}

	for _, tt := range prefixTests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		if len(program.Statements) != 1 {
			t.Fatalf(
				"program.Statements does not contain 1 statements. got=%d",
				len(program.Statements),
			)
		}

		statement, ok := program.Statements[0].(*ast.ExpressionStatement)
		if !ok {
			t.Fatalf(
				"program.Statements[0] is not ast.ExpressionStatement. got=%T",
				program.Statements[0],
			)
		}

		expression, ok := statement.Expression.(*ast.PrefixExpression)
		if !ok {
			t.Fatalf(
				"expression not *ast.PrefixExpression. got=%T",
				statement.Expression,
			)
		}

		if expression.Operator != tt.operator {
			t.Fatalf(
				"expression.Operator not %s. got=%s",
				tt.operator,
				expression.Operator,
			)
		}

		if !testIntegerLiteral(t, expression.Right, tt.value) {
			return
		}
	}
}

func TestParsingInfixExpressions(t *testing.T) {
	infixTests := []struct {
		input    string
		left     int64
		operator string
		right    int64
	}{
		{"5 + 5;", 5, "+", 5},
		{"5 - 5;", 5, "-", 5},
		{"5 * 5;", 5, "*", 5},
		{"5 / 5;", 5, "/", 5},
		{"5 > 5;", 5, ">", 5},
		{"5 < 5;", 5, "<", 5},
		{"5 == 5;", 5, "==", 5},
		{"5 != 5;", 5, "!=", 5},
	}

	for _, tt := range infixTests {
		l := lexer.New(tt.input)
		p := New(l)

		program := p.ParseProgram()
		checkParserErrors(t, p)

		if len(program.Statements) != 1 {
			t.Fatalf(
				"program.Statements does not contain 1 statements. got=%d",
				len(program.Statements),
			)
		}

		statement, ok := program.Statements[0].(*ast.ExpressionStatement)
		if !ok {
			t.Fatalf(
				"program.Statements[0] is not ast.ExpressionStatement. got=%T",
				program.Statements[0],
			)
		}

		expression, ok := statement.Expression.(*ast.InfixExpression)
		if !ok {
			t.Fatalf(
				"expression not *ast.InfixExpression. got=%T",
				statement.Expression,
			)
		}

		if !testIntegerLiteral(t, expression.Left, tt.left) {
			return
		}

		if expression.Operator != tt.operator {
			t.Fatalf(
				"expression.Operator not %s. got=%s",
				tt.operator,
				expression.Operator,
			)
		}

		if !testIntegerLiteral(t, expression.Right, tt.right) {
			return
		}
	}
}

func TestOperatorPrecedenceParsing(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"-a * b", "((-a) * b)"},
		{"!-a", "(!(-a))"},
		{"a + b + c", "((a + b) + c)"},
		{"a + b - c", "((a + b) - c)"},
		{"a * b * c", "((a * b) * c)"},
		{"a * b / c", "((a * b) / c)"},
		{"a + b / c", "(a + (b / c))"},
		{"a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"},
		{"5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"},
		{"5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"},
		{"3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"},
		{"3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"},
		{"3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"},
		{"true", "true"},
		{"false", "false"},
		{"3 > 5 == false", "((3 > 5) == false)"},
		{"3 < 5 == true", "((3 < 5) == true)"},
		{"1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"},
		{"(5 + 5) * 2", "((5 + 5) * 2)"},
		{"2 / (5 + 5)", "(2 / (5 + 5))"},
		{"-(5 + 5)", "(-(5 + 5))"},
		{"!(true == true)", "(!(true == true))"},
		{"a + add(b * c) + d", "((a + add((b * c))) + d)"},
		{
			"add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
			"add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
		},
		{"add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"},
	}

	for i, tt := range tests {
		t.Run(fmt.Sprintf("Test %d: %s", i, tt.input), func(t *testing.T) {
			l := lexer.New(tt.input)
			p := New(l)

			program := p.ParseProgram()
			checkParserErrors(t, p)

			actual := program.String()
			if actual != tt.expected {
				t.Errorf(
					"expected=%q, got=%q",
					tt.expected,
					actual,
				)
			}
		})
	}
}

func testLetStatement(t *testing.T, s ast.Statement, name string) bool {
	if s.TokenLiteral() != "let" {
		t.Errorf(
			"stmt.TokenLiteral not 'let'. got=%q",
			s.TokenLiteral(),
		)
		return false
	}

	letStmt, ok := s.(*ast.LetStatement)

	if !ok {
		t.Errorf("s not *ast.LetStatement. got=%T", s)
	}

	if letStmt.Name.Value != name {
		t.Errorf(
			"letStmt.Name.Value not '%s'. got=%s",
			name,
			letStmt.Name.Value,
		)
		return false
	}

	if letStmt.Name.TokenLiteral() != name {
		t.Errorf(
			"letStmt.Name.TokenLiteral() not '%s'. got=%s",
			name,
			letStmt.Name.TokenLiteral(),
		)
		return false
	}
	return true

}

func checkParserErrors(t *testing.T, p *Parser) {
	errors := p.Errors()

	if len(errors) == 0 {
		return
	}

	t.Errorf(
		"parser has %d errors",
		len(errors),
	)

	for _, msg := range errors {
		t.Errorf("parser error: %q", msg)
	}
	t.FailNow()
}

func testIntegerLiteral(t *testing.T, il ast.Expression, value int64) bool {
	integer, ok := il.(*ast.IntegerLiteral)

	if !ok {
		t.Errorf("expression not *ast.IntegerLiteral. got=%T", il)
		return false
	}

	if integer.Value != value {
		t.Errorf(
			"integer.Value not %d. got=%d",
			value,
			integer.Value,
		)
		return false
	}

	if integer.TokenLiteral() != fmt.Sprintf("%d", value) {
		t.Errorf(
			"integer.TokenLiteral() not %d. got=%s",
			value,
			integer.TokenLiteral(),
		)
		return false
	}

	return true
}

func TestBooleanExpressions(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"true;", true},
		{"false;", false},
	}

	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)

		program := p.ParseProgram()
		checkParserErrors(t, p)

		if len(program.Statements) != 1 {
			t.Fatalf(
				"program.Statements does not contain 1 statements. got=%d",
				len(program.Statements),
			)
		}

		statement, ok := program.Statements[0].(*ast.ExpressionStatement)
		if !ok {
			t.Fatalf(
				"program.Statements[0] is not ast.ExpressionStatement. got=%T",
				program.Statements[0],
			)
		}

		boolean, ok := statement.Expression.(*ast.Boolean)
		if !ok {
			t.Fatalf(
				"expression not *ast.Boolean. got=%T",
				statement.Expression,
			)
		}

		if boolean.Value != tt.expected {
			t.Errorf(
				"boolean.Value not %t. got=%t",
				tt.expected,
				boolean.Value,
			)
		}
	}
}

func TestIfExpressions(t *testing.T) {
	input := `if (x < y) { x }`

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf(
			"program.Statements does not contain 1 statements. got=%d",
			len(program.Statements),
		)
	}

	statement, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf(
			"program.Statements[0] is not ast.ExpressionStatement. got=%T",
			program.Statements[0],
		)
	}

	expression, ok := statement.Expression.(*ast.IfExpression)
	if !ok {
		t.Fatalf(
			"expression not *ast.IfExpression. got=%T",
			statement.Expression,
		)
	}

	if !testInfixExpression(
		t,
		expression.Condition,
		"x",
		"<",
		"y",
	) {
		return
	}

	if len(expression.Consequence.Statements) != 1 {
		t.Errorf(
			"consequence is not 1 statements. got=%d\n",
			len(expression.Consequence.Statements),
		)
	}

	consequence, ok := expression.Consequence.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf(
			"Statements[0] is not ast.ExpressionStatement. got=%T",
			expression.Consequence.Statements[0],
		)
	}

	if !testIdentifier(t, consequence.Expression, "x") {
		return
	}

	if expression.Alternative != nil {
		t.Errorf(
			"expression.Alternative.Statements was not nil. got=%+v",
			expression.Alternative.Statements,
		)
	}
}

func testInfixExpression(
	t *testing.T,
	expression ast.Expression,
	left interface{},
	operator string,
	right interface{},
) bool {
	opExpression, ok := expression.(*ast.InfixExpression)
	if !ok {
		t.Errorf(
			"expression is not ast.InfixExpression. got=%T(%s)",
			expression,
			expression,
		)
		return false
	}

	if !testLiteralExpression(t, opExpression.Left, left) {
		return false
	}

	if opExpression.Operator != operator {
		t.Errorf(
			"expression.Operator is not '%s'. got=%q",
			operator,
			opExpression.Operator,
		)
		return false
	}

	if !testLiteralExpression(t, opExpression.Right, right) {
		return false
	}

	return true
}

func testIdentifier(t *testing.T, expression ast.Expression, value string) bool {
	identifier, ok := expression.(*ast.Identifier)
	if !ok {
		t.Errorf(
			"expression not *ast.Identifier. got=%T",
			expression,
		)
		return false
	}

	if identifier.Value != value {
		t.Errorf(
			"identifier.Value not %s. got=%s",
			value,
			identifier.Value,
		)
		return false
	}

	if identifier.TokenLiteral() != value {
		t.Errorf(
			"identifier.TokenLiteral() not %s. got=%s",
			value,
			identifier.TokenLiteral(),
		)
		return false
	}

	return true
}

func testLiteralExpression(
	t *testing.T,
	expression ast.Expression,
	expected interface{},
) bool {
	switch v := expected.(type) {
	case int:
		return testIntegerLiteral(t, expression, int64(v))
	case int64:
		return testIntegerLiteral(t, expression, v)
	case string:
		return testIdentifier(t, expression, v)
	case bool:
		return testBooleanLiteral(t, expression, v)
	}

	t.Errorf(
		"type of expression not handled. got=%T",
		expression,
	)
	return false
}

func testBooleanLiteral(t *testing.T, expression ast.Expression, value bool) bool {
	boolean, ok := expression.(*ast.Boolean)
	if !ok {
		t.Errorf(
			"expression not *ast.Boolean. got=%T",
			expression,
		)
		return false
	}

	if boolean.Value != value {
		t.Errorf(
			"boolean.Value not %t. got=%t",
			value,
			boolean.Value,
		)
		return false
	}

	if boolean.TokenLiteral() != fmt.Sprintf("%t", value) {
		t.Errorf(
			"boolean.TokenLiteral() not %t. got=%s",
			value,
			boolean.TokenLiteral(),
		)
		return false
	}
	return true
}

func TestIfElseExpressions(t *testing.T) {
	input := `if (x < y) { x } else { y }`

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf(
			"program.Statements does not contain 1 statements. got=%d",
			len(program.Statements),
		)
	}

	statement, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf(
			"program.Statements[0] is not ast.ExpressionStatement. got=%T",
			program.Statements[0],
		)
	}

	expression, ok := statement.Expression.(*ast.IfExpression)
	if !ok {
		t.Fatalf(
			"expression not *ast.IfExpression. got=%T",
			statement.Expression,
		)
	}

	if !testInfixExpression(
		t,
		expression.Condition,
		"x",
		"<",
		"y",
	) {
		return
	}

	if len(expression.Consequence.Statements) != 1 {
		t.Errorf(
			"consequence is not 1 statements. got=%d\n",
			len(expression.Consequence.Statements),
		)
	}

	consequence, ok := expression.Consequence.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf(
			"Statements[0] is not ast.ExpressionStatement. got=%T",
			expression.Consequence.Statements[0],
		)
	}

	if !testIdentifier(t, consequence.Expression, "x") {
		return
	}

	if len(expression.Alternative.Statements) != 1 {
		t.Errorf(
			"consequence is not 1 statements. got=%d\n",
			len(expression.Alternative.Statements),
		)
	}

	alternative, ok := expression.Alternative.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf(
			"Statements[0] is not ast.ExpressionStatement. got=%T",
			expression.Alternative.Statements[0],
		)
	}

	if !testIdentifier(t, alternative.Expression, "y") {
		return
	}
}

func TestFunctionLiteralParsing(t *testing.T) {
	input := `fn(x, y) { x + y; }`

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf(
			"program.Statements does not contain 1 statements. got=%d",
			len(program.Statements),
		)
	}

	statement, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf(
			"program.Statements[0] is not ast.ExpressionStatement. got=%T",
			program.Statements[0],
		)
	}

	function, ok := statement.Expression.(*ast.FunctionLiteral)
	if !ok {
		t.Fatalf(
			"expression not *ast.FunctionLiteral. got=%T",
			statement.Expression,
		)
	}

	if len(function.Parameters) != 2 {
		t.Fatalf(
			"function literal parameters wrong. want 2, got=%d",
			len(function.Parameters),
		)
	}

	testLiteralExpression(t, function.Parameters[0], "x")
	testLiteralExpression(t, function.Parameters[1], "y")

	if len(function.Body.Statements) != 1 {
		t.Fatalf(
			"function.Body.Statements has not 1 statements. got=%d",
			len(function.Body.Statements),
		)
	}

	bodyStatement, ok := function.Body.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf(
			"function body statement is not ast.ExpressionStatement. got=%T",
			function.Body.Statements[0],
		)
	}

	testInfixExpression(t, bodyStatement.Expression, "x", "+", "y")
}

func TestFunctionParameterParsing(t *testing.T) {
	tests := []struct {
		input          string
		expectedParams []string
	}{
		{input: "fn() {};", expectedParams: []string{}},
		{input: "fn(x) {};", expectedParams: []string{"x"}},
		{input: "fn(x, y, z) {};", expectedParams: []string{"x", "y", "z"}},
	}

	for _, tt := range tests {
		t.Run(fmt.Sprintf("Test %s", tt.input), func(t *testing.T) {
			l := lexer.New(tt.input)
			p := New(l)

			program := p.ParseProgram()
			checkParserErrors(t, p)

			statement := program.Statements[0].(*ast.ExpressionStatement)
			function := statement.Expression.(*ast.FunctionLiteral)

			if len(function.Parameters) != len(tt.expectedParams) {
				t.Errorf(
					"length parameters wrong. want %d, got=%d\n",
					len(tt.expectedParams),
					len(function.Parameters),
				)
			}

			for i, identifier := range tt.expectedParams {
				testLiteralExpression(t, function.Parameters[i], identifier)
			}
		})
	}
}

func TestCallExpressionParsing(t *testing.T) {
	input := `add(1, 2 * 3, 4 + 5);`

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf(
			"program.Statements does not contain 1 statements. got=%d",
			len(program.Statements),
		)
	}

	statement, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf(
			"program.Statements[0] is not ast.ExpressionStatement. got=%T",
			program.Statements[0],
		)
	}

	expression, ok := statement.Expression.(*ast.CallExpression)
	if !ok {
		t.Fatalf(
			"expression not *ast.CallExpression. got=%T",
			statement.Expression,
		)
	}

	if !testIdentifier(t, expression.Function, "add") {
		return
	}

	if len(expression.Arguments) != 3 {
		t.Fatalf(
			"wrong length of arguments. got=%d, want=3",
			len(expression.Arguments),
		)
	}

	testLiteralExpression(t, expression.Arguments[0], 1)
	testInfixExpression(t, expression.Arguments[1], 2, "*", 3)
	testInfixExpression(t, expression.Arguments[2], 4, "+", 5)
}

func TestCallExpressionParameterParsing(t *testing.T) {
	tests := []struct {
		input             string
		expectedIdent     string
		nExpectedLiterals uint
	}{
		{input: "add();", expectedIdent: "add"},
		{input: "add(1);", expectedIdent: "add", nExpectedLiterals: 1},
		{
			input:             "add(1, 2 * 3, 4 + 5);",
			expectedIdent:     "add",
			nExpectedLiterals: 3,
		},
	}

	for _, tt := range tests {
		t.Run(fmt.Sprintf("Test %s", tt.input), func(t *testing.T) {
			l := lexer.New(tt.input)
			p := New(l)

			program := p.ParseProgram()
			checkParserErrors(t, p)

			statement := program.Statements[0].(*ast.ExpressionStatement)
			expression := statement.Expression.(*ast.CallExpression)

			if !testIdentifier(t, expression.Function, tt.expectedIdent) {
				return
			}

			if len(expression.Arguments) != int(tt.nExpectedLiterals) {
				t.Fatalf(
					"wrong length of arguments. got=%d want=1",
					len(expression.Arguments),
				)
			}
		})
	}
}
