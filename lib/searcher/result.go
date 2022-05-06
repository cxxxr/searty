package searcher

type Result struct {
	Filename string
	start    int
	end      int
}

func newResult(filename string, start, end int) *Result {
	return &Result{filename, start, end}
}
