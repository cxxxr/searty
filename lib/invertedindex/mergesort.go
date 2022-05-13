package invertedindex

func splitFrontBack(node *Posting) (*Posting, *Posting) {
	return nil, nil
}

func sortedMerge(a, b *Posting) *Posting {
	return nil
}

func mergeSort(headPtr **Posting) {
	head := *headPtr
	if head == nil || head.next == nil {
		return
	}

	a, b := splitFrontBack(head)

	mergeSort(&a)
	mergeSort(&b)

	*headPtr = sortedMerge(a, b)
}
