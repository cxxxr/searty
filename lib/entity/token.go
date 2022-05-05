package entity

type TokenId string

const EmptyTokenId = TokenId("")

type Token struct {
	Id   TokenId
	Term string
}

func NewToken(uuid, term string) *Token {
	return &Token{TokenId(uuid), term}
}
