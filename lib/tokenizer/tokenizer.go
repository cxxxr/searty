package tokenizer

type Tokenizer struct{}

func New() *Tokenizer {
	return &Tokenizer{}
}

func ngram(text string, n int) []string {
	tokens := make([]string, 0)
	for i := 0; i < len(text)-(n-1); i++ {
		tokens = append(tokens, text[i:i+n])
	}
	return tokens
}

func (t *Tokenizer) Tokenize(text string) []string {
	boundingText := make([]byte, len(text)+2)
	copy(boundingText[1:], text)
	boundingText[0] = byte(0)
	boundingText[len(boundingText)-1] = byte(0)
	return ngram(string(boundingText), 3)
}
