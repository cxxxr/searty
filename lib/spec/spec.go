package spec

import (
	"encoding/json"
	"os"

	"github.com/pkg/errors"
)

type Identifier struct {
	Type    string `json:"type"`
	Name    string `json:"name"`
	Package string `json:"package"`
}

type Location struct {
	Specifier string `json:"specifier"`
	File      string `json:"file"`
	Position  int    `json:"position"`
}

type Definition struct {
	Identifier Identifier `json:"identifier"`
	Locations  []Location `json:"locations"`
}

type Spec struct {
	SystemName    string       `json:"system_name"`
	AsdFile       string       `json:"asd_file"`
	RootDirectory string       `json:"root_directory"`
	Files         []string     `json:"files"`
	Definitions   []Definition `json:"definitions"`
	Time          float64      `json:"time"`
}

func Read(filename string) (*Spec, error) {
	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, errors.WithStack(err)
	}

	var spec Spec
	if err := json.Unmarshal(data, &spec); err != nil {
		return nil, errors.WithStack(err)
	}

	return &spec, nil
}
