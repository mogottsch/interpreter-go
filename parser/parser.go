package parser

import (
	"fmt"
	"mogottsch/interpreter-go/ast"
	"mogottsch/interpreter-go/lexer"
	"mogottsch/interpreter-go/token"
	"strconv"
)

const (
	_ int = iota
	LOWEST
	EQUALS
	LESSGREATER
	SUM
	PRODUCT
	PREFIX
	CALL
)

var precedences = map[token.TokenType]int{
	token.EQ:       EQUALS,
	token.NOT_EQ:   EQUALS,
	token.LT:       LESSGREATER,
	token.GT:       LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.SLASH:    PRODUCT,
	token.ASTERISK: PRODUCT,
	token.LPAREN:   CALL,
}

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

type Parser struct {
	l *lexer.Lexer

	errors []string

	curToken  token.Token
	peekToken token.Token

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l, errors: []string{}}

	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.IDENT, p.parseIdentifierExpression)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)
	p.registerPrefix(token.TRUE, p.parseBooleanExpression)
	p.registerPrefix(token.FALSE, p.parseBooleanExpression)
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(token.IF, p.parseIfExpression)
	p.registerPrefix(token.FUNCTION, p.parseFunctionLiteral)

	p.infixParseFns = make(map[token.TokenType]infixParseFn)
	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.MINUS, p.parseInfixExpression)
	p.registerInfix(token.SLASH, p.parseInfixExpression)
	p.registerInfix(token.ASTERISK, p.parseInfixExpression)
	p.registerInfix(token.EQ, p.parseInfixExpression)
	p.registerInfix(token.NOT_EQ, p.parseInfixExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)
	p.registerInfix(token.LPAREN, p.parseCallExpression)

	// Read two tokens, so curToken and peekToken are both set

	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) addTokenTypeError(t token.TokenType) {
	p.errors = append(
		p.errors,
		fmt.Sprintf("expected next token to be %s, got %s", t, p.curToken.Type),
	)
}

func (p *Parser) addNoPrefixParseFnError(t token.TokenType) {
	p.errors = append(
		p.errors,
		fmt.Sprintf("no prefix parse function for %s found", t),
	)
}

func (p *Parser) addInvalidIntegerError() {
	p.errors = append(
		p.errors,
		fmt.Sprintf("could not parse %q as integer", p.curToken.Literal),
	)
}

func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for p.curToken.Type != token.EOF {
		statement, ok := p.parseStatement()
		if ok {
			program.Statements = append(program.Statements, statement)
		}
		p.nextToken()

	}

	return program
}
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) parseStatement() (ast.Statement, bool) {
	switch p.curToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()

	default:
		return p.parseExpressionStatement(), true

	}
}

func (p *Parser) parseLetStatement() (*ast.LetStatement, bool) {
	if !p.expectCurTokenType(token.LET) {
		return nil, false
	}
	letStatement := &ast.LetStatement{Token: p.curToken}
	p.nextToken()

	letStatement.Name = p.parseIdentifier()
	p.nextToken()

	if !p.expectCurTokenType(token.ASSIGN) {
		return nil, false
	}
	p.nextToken()

	expression := p.parseExpression(LOWEST)
	letStatement.Value = expression
	p.nextToken()

	if !p.expectCurTokenType(token.SEMICOLON) {
		return nil, false
	}

	return letStatement, true

}

func (p *Parser) parseReturnStatement() (*ast.ReturnStatement, bool) {
	if !p.expectCurTokenType(token.RETURN) {
		return nil, false
	}

	returnStatement := &ast.ReturnStatement{Token: p.curToken}
	p.nextToken()

	expression := p.parseExpression(LOWEST)
	returnStatement.Value = expression
	p.nextToken()

	if !p.expectCurTokenType(token.SEMICOLON) {
		return nil, false
	}
	return returnStatement, true

}

func (p *Parser) parseIdentifier() *ast.Identifier {
	p.expectCurTokenType(token.IDENT)
	return &ast.Identifier{Value: p.curToken.Literal, Token: p.curToken}
}

func (p *Parser) parseIdentifierExpression() ast.Expression {
	return p.parseIdentifier()
}

func (p *Parser) expectCurTokenType(tt token.TokenType) bool {
	if p.curToken.Type != tt {
		p.addTokenTypeError(tt)
		return false
	}
	return true

}

func (p *Parser) expectPeekTokenType(tt token.TokenType) bool {
	if p.peekToken.Type != tt {
		p.addTokenTypeError(tt)
		return false
	}
	p.nextToken()
	return true
}

func (p *Parser) peekTokenIs(tt token.TokenType) bool {
	return p.peekToken.Type == tt
}

func (p *Parser) curTokenIs(tt token.TokenType) bool {
	return p.curToken.Type == tt
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	statement := &ast.ExpressionStatement{Token: p.curToken}

	statement.Expression = p.parseExpression(LOWEST)

	if p.peekToken.Type == token.SEMICOLON {
		p.nextToken()
	}

	return statement
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.addNoPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()

	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		p.nextToken()

		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	integerLiteral := &ast.IntegerLiteral{Token: p.curToken}
	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		p.addInvalidIntegerError()
		return nil
	}

	integerLiteral.Value = value

	return integerLiteral
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{Token: p.curToken, Operator: p.curToken.Literal}
	p.nextToken()
	expression.Right = p.parseExpression(PREFIX)

	return expression
}

func (p *Parser) peekPrecedence() int {
	if precedence, ok := precedences[p.peekToken.Type]; ok {
		return precedence
	}
	return LOWEST
}

func (p *Parser) curPrecedence() int {
	if precedence, ok := precedences[p.curToken.Type]; ok {
		return precedence
	}
	return LOWEST
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{Token: p.curToken, Operator: p.curToken.Literal, Left: left}

	precedence := p.curPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence)

	return expression
}

func (p *Parser) parseBooleanExpression() ast.Expression {
	return &ast.Boolean{Token: p.curToken, Value: p.curTokenIs(token.TRUE)}
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()

	expression := p.parseExpression(LOWEST)

	if !p.expectPeekTokenType(token.RPAREN) {
		return nil
	}

	return expression
}

func (p *Parser) parseIfExpression() ast.Expression {
	expression := &ast.IfExpression{Token: p.curToken}

	p.nextToken()
	if !p.expectCurTokenType(token.LPAREN) {
		return nil
	}

	p.nextToken()
	expression.Condition = p.parseExpression(LOWEST)

	if !p.expectPeekTokenType(token.RPAREN) {
		return nil
	}

	if !p.expectPeekTokenType(token.LBRACE) {
		return nil
	}

	expression.Consequence = p.parseBlockStatement()

	if p.peekTokenIs(token.ELSE) {
		p.nextToken()

		if !p.expectPeekTokenType(token.LBRACE) {
			return nil
		}

		expression.Alternative = p.parseBlockStatement()
	}

	return expression
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.curToken}

	block.Statements = []ast.Statement{}

	p.nextToken()

	for !p.curTokenIs(token.RBRACE) && !p.curTokenIs(token.EOF) {
		statement, ok := p.parseStatement()
		if ok {
			block.Statements = append(block.Statements, statement)
		}
		p.nextToken()
	}

	return block
}

func (p *Parser) parseFunctionLiteral() ast.Expression {
	function := &ast.FunctionLiteral{Token: p.curToken}
	p.nextToken()

	if !p.expectCurTokenType(token.LPAREN) {
		return nil
	}

	function.Parameters = p.parseFunctionParameters()
	if !p.expectCurTokenType(token.LBRACE) {
		return nil
	}

	function.Body = p.parseBlockStatement()

	return function
}

func (p *Parser) parseFunctionParameters() []*ast.Identifier {
	identifiers := []*ast.Identifier{}

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		p.nextToken()
		return identifiers
	}

	p.nextToken()

	identifier := p.parseIdentifier()
	identifiers = append(identifiers, identifier)

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		identifier := p.parseIdentifier()
		identifiers = append(identifiers, identifier)
	}

	if !p.expectPeekTokenType(token.RPAREN) {
		return nil
	}
	p.nextToken()

	return identifiers
}

func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	expression := &ast.CallExpression{Token: p.curToken, Function: function}
	expression.Arguments = p.parseCallArguments()

	return expression
}

func (p *Parser) parseCallArguments() []ast.Expression {
	arguments := []ast.Expression{}

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		p.nextToken()
		return arguments
	}

	p.nextToken()

	argument := p.parseExpression(LOWEST)
	arguments = append(arguments, argument)

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		argument = p.parseExpression(LOWEST)
		arguments = append(arguments, argument)
	}

	if !p.expectPeekTokenType(token.RPAREN) {
		return nil
	}

	return arguments
}
