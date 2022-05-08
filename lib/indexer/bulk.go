package indexer

import (
	"log"
	"sync"
)

type BulkIndexer struct {
	goNum int
}

func NewBulkIndexer(goNum int) *BulkIndexer {
	return &BulkIndexer{goNum: goNum}
}

func (b *BulkIndexer) index(c <-chan string, outputDir string) {
	for specFile := range c {
		err := NewIndexer().Index(specFile, GetDatabaseFile(specFile, outputDir))
		if err != nil {
			log.Printf("error: %s\n", specFile)
		}
	}
}

func (b *BulkIndexer) Index(specFiles []string, outputDir string) {
	c := make(chan string, len(specFiles))

	var wg sync.WaitGroup
	wg.Add(b.goNum)

	for _, specFile := range specFiles {
		c <- specFile
	}
	close(c)

	for i := 0; i < b.goNum; i++ {
		go func() {
			b.index(c, outputDir)
			wg.Done()
		}()
	}

	wg.Wait()
}
