package lexer

import (
	"mogottsch/interpreter-go/token"
	"unicode"
)

type Lexer struct {
	input          string
	position       int
	readPositition int
	ch             byte
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.readPositition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPositition]
	}

	l.position = l.readPositition
	l.readPositition++
}
func (l *Lexer) PeekChar() byte {
	if l.readPositition >= len(l.input) {
		return 0
	} else {
		return l.input[l.readPositition]
	}
}

func (l *Lexer) NextToken() token.Token {
	var tok token.Token
	for unicode.IsSpace(rune(l.ch)) {
		l.readChar()
	}

	switch l.ch {
	case '=':
		tok = newToken(token.ASSIGN, l.ch)
	case ';':
		tok = newToken(token.SEMICOLON, l.ch)
	case '(':
		tok = newToken(token.LPAREN, l.ch)
	case ')':
		tok = newToken(token.RPAREN, l.ch)
	case ',':
		tok = newToken(token.COMMA, l.ch)
	case '+':
		tok = newToken(token.PLUS, l.ch)
	case '{':
		tok = newToken(token.LBRACE, l.ch)
	case '}':
		tok = newToken(token.RBRACE, l.ch)
	case 0:
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isLetter(l.ch) {
			word := l.readChars(isLetter)

			tok = token.FromWord(word)

		} else if isDigit(l.ch) {
			digits := l.readChars(isDigit)

			tok = token.FromDigits(digits)
		} else {
			tok = newToken(token.ILLEGAL, l.ch)
		}
	}

	l.readChar()

	return tok
}

func newToken(tokenType token.TokenType, ch byte) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}

func isLetter(ch byte) bool {
	return unicode.IsLetter(rune(ch)) || ch == '_'
}

func isDigit(ch byte) bool {
	return unicode.IsDigit(rune(ch))
}

func (l *Lexer) readChars(isPartOfWord func(byte) bool) string {
	word := ""

	if !isPartOfWord(l.ch) {
		return word
	}

	for isPartOfWord(l.PeekChar()) {
		word += string(l.ch)
		l.readChar()
	}

	word += string(l.ch)

	return word
}
