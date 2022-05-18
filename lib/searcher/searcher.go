package searcher

type Searcher interface {
	Search(query string) ([]*Result, error)
}
