package token

type TokenType string

type Token struct {
	Type    TokenType
	Literal string
}

func FromWord(word string) Token {
	var tokenType TokenType
	switch word {
	case "fn":
		tokenType = FUNCTION
	case "let":
		tokenType = LET

	default:
		tokenType = IDENT
	}
	return Token{Type: tokenType, Literal: word}
}

func FromDigits(digits string) Token {
	return Token{Type: INT, Literal: digits}
}

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
