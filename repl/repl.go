package repl

import (
	"bufio"
	"fmt"
	"io"
	"mogottsch/interpreter-go/lexer"
	"mogottsch/interpreter-go/token"
)

const PROMPT = ">> "

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)
	for {
		fmt.Fprint(out, PROMPT)
		if !scanner.Scan() {
			return
		}

		line := scanner.Text()

		l := lexer.New(line)

		tok := l.NextToken()
		for tok.Type != token.EOF {
			fmt.Fprintf(out, "%+v\n", tok)
			tok = l.NextToken()
		}
	}
}
