package token

type TokenType string

const (
	ILLEGAL = "ILLEGAL"
	EOF     = "EOF"

	// Identifiers + literals
	IDENT = "IDENT" // add, foobar, x, y, ...
	INT   = "INT"   // 1343456

	// Operators
	ASSIGN   = "="
	PLUS     = "+"
	MINUS    = "-"
	BANG     = "!"
	ASTERISK = "*"
	SLASH    = "/"

	LT = "<"
	GT = ">"

	EQ     = "=="
	NOT_EQ = "!="

	// Delimiters
	COMMA     = ","
	SEMICOLON = ";"

	LPAREN = "("
	RPAREN = ")"
	LBRACE = "{"
	RBRACE = "}"

	// Keywords
	FUNCTION = "FUNCTION"
	LET      = "LET"
	TRUE     = "TRUE"
	FALSE    = "FALSE"
	IF       = "IF"
	ELSE     = "ELSE"
	RETURN   = "RETURN"
)

type Token struct {
	Type    TokenType
	Literal string
}

var singleCharTokens = map[byte]TokenType{
	'=': ASSIGN,
	'+': PLUS,
	'-': MINUS,
	'!': BANG,
	'*': ASTERISK,
	'/': SLASH,
	'<': LT,
	'>': GT,
	'(': LPAREN,
	')': RPAREN,
	'{': LBRACE,
	'}': RBRACE,
	',': COMMA,
	';': SEMICOLON,
}

func FromChar(char byte) (Token, bool) {
	if tokenType, ok := singleCharTokens[char]; ok {
		return Token{Type: tokenType, Literal: string(char)}, true
	}
	return Token{}, false
}

var twoCharTokens = map[string]TokenType{
	"==": EQ,
	"!=": NOT_EQ,
}

func FromTwoChars(a, b byte) (Token, bool) {
	if tokenType, ok := twoCharTokens[string(a)+string(b)]; ok {
		return Token{Type: tokenType, Literal: string(a) + string(b)}, true
	}
	return Token{}, false
}

var keywords = map[string]TokenType{
	"fn":     FUNCTION,
	"let":    LET,
	"true":   TRUE,
	"false":  FALSE,
	"if":     IF,
	"else":   ELSE,
	"return": RETURN,
}

func FromWord(word string) Token {
	var tokenType TokenType = IDENT
	for k, v := range keywords {
		if k == word {
			tokenType = v
		}
	}
	return Token{Type: tokenType, Literal: word}
}

func FromDigits(digits string) Token {
	return Token{Type: INT, Literal: digits}
}
