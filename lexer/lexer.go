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
	l.skipSpaces()

	tok, ok := token.FromTwoChars(l.ch, l.PeekChar())
	if ok {
		l.readChar()
		l.readChar()
		return tok
	}
	tok, ok = token.FromChar(l.ch)
	if ok {
		l.readChar()
		return tok
	}
	if l.ch == 0 {
		return token.Token{Type: token.EOF, Literal: ""}
	}

	if isLetter(l.ch) {
		word := l.readChars(isLetter)

		tok = token.FromWord(word)

	} else if isDigit(l.ch) {
		digits := l.readChars(isDigit)

		tok = token.FromDigits(digits)
	} else {
		tok = newToken(token.ILLEGAL, l.ch)
	}

	l.readChar()

	return tok
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

func (l *Lexer) skipSpaces() {
	for unicode.IsSpace(rune(l.ch)) {
		l.readChar()
	}
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
