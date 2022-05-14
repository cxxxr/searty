package tokenizer

type Tokenizer struct{}

func New() *Tokenizer {
	return &Tokenizer{}
}

func ngram(text string, n int) []string {
	tokens := make([]string, 0)

	for i := range text {
		token := make([]rune, 0, n)
		limit := n
		for _, c := range text[i:] {
			token = append(token, c)
			limit--
			if limit == 0 {
				break
			}
		}
		if len(token) < n {
			break
		}
		tokens = append(tokens, string(token))
	}

	return tokens
}

func (t *Tokenizer) Tokenize(text string) []string {
	return ngram(string(text), 3)
}
