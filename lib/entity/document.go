package entity

type DocumentId int

var EmptyDocumentId = DocumentId(0)

type Document struct {
	Id       DocumentId
	Filename string
}

func NewDocument(id DocumentId, filename string) *Document {
	return &Document{id, filename}
}
