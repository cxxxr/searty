package primitive

type TokenId string

const EmptyTokenId = TokenId("")

type DocumentId int

type Document struct {
	Id       DocumentId
	Filename string
}
