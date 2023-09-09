package parser

import (
	"fmt"
	"mogottsch/interpreter-go/ast"
	"mogottsch/interpreter-go/lexer"
	"mogottsch/interpreter-go/token"
)

type Parser struct {
	l *lexer.Lexer

	errors []string

	curToken  token.Token
	peekToken token.Token
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l, errors: []string{}}

	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) addTokenTypeError(t token.TokenType) {
	if p.curToken.Type == t {
		panic(fmt.Sprintf("addTokenTypeError called with correct TokenType %s", t))
	}

	p.errors = append(
		p.errors,
		fmt.Sprintf("expected next token to be %s, got %s", t, p.curToken.Type),
	)
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
		return nil, false

	}
}

func (p *Parser) parseLetStatement() (*ast.LetStatement, bool) {
	if !p.expectCurTokenType(token.LET) {
		return nil, false
	}
	letStatement := &ast.LetStatement{Token: p.curToken}
	p.nextToken()

	identifier := p.parseIdentifier()
	letStatement.Name = &identifier
	p.nextToken()

	if !p.expectCurTokenType(token.ASSIGN) {
		return nil, false
	}
	p.nextToken()

	// expression := p.parseExpression()
	for p.curToken.Type != token.SEMICOLON {
		p.nextToken()
	}
	return letStatement, true

}

func (p *Parser) parseReturnStatement() (*ast.ReturnStatement, bool) {
	if !p.expectCurTokenType(token.RETURN) {
		return nil, false
	}

	returnStatement := &ast.ReturnStatement{Token: p.curToken}
	p.nextToken()

	// expression := p.parseExpression()
	for p.curToken.Type != token.SEMICOLON {
		p.nextToken()
	}
	return returnStatement, true

}

func (p *Parser) parseIdentifier() ast.Identifier {
	p.expectCurTokenType(token.IDENT)
	return ast.Identifier{Value: p.curToken.Literal, Token: p.curToken}
}

func (p *Parser) expectCurTokenType(tt token.TokenType) bool {
	if p.curToken.Type != tt {
		p.addTokenTypeError(tt)
		return false
	}
	return true

}

// func (p *Parser) parseExpression() ast.Expression {
//
// }
