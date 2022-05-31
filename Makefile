.PHONY: help index version download extract json sqlite3 merge clean

VERSION ?= $(shell grep 'version: ' quicklisp.txt | cut -d ' ' -f 2)

help:
	@echo usage of this Makefile.
	@echo $(MAKE) index "	#" switch quicklisp dist version to target. if VERSION environment are set. it will target the version. example \"VERSION=2022-04-01 $(MAKE) index\" #"
	@echo $(MAKE) version "	#" show current quicklisp version.
	@echo $(MAKE) download "	#" download all lisp source archives from quicklisp.
	@echo $(MAKE) extract "	#" extract all lisp source which downloaded by $(MAKE) download
	@echo $(MAKE) json "	#" extract info from source and make json files.
	@echo $(MAKE) sqlite3 "	#" convert json files to sqlite3 files.
	@echo $(MAKE) merge "	#" merge sqlite3 files into a merged db file.
	@echo $(MAKE) help  "	#" show this message.

version: quicklisp.txt
	@echo $(VERSION)

index:
	@# force redownload index file
	rm -f quicklisp.txt; $(MAKE) quicklisp.txt
	@$(MAKE) version

quicklisp.txt:
	if [ 'x$(VERSION)' != 'x' ]; then \
		curl -o $@ http://beta.quicklisp.org/dist/quicklisp/$(VERSION)/distinfo.txt; \
	else \
		curl -o $@ http://beta.quicklisp.org/dist/quicklisp.txt; \
	fi

$(VERSION)/systems.txt: quicklisp.txt
	curl --create-dirs -o $@ `grep 'system-index-url: ' $< | cut -d ' ' -f 2`

$(VERSION)/releases.txt: quicklisp.txt
	curl --create-dirs -o $@ `grep 'release-index-url: ' $< | cut -d ' ' -f 2`

download: $(VERSION)/releases.txt
	cat $< | \
	grep -v '^#' | \
	awk -v 'OFS= ' '{print $$2}'| \
	sed -E "s#(^http://[^/]+/)(.+)\$$#\2#g" | \
	xargs -P3 -I {} echo "echo $(MAKE) {};$(MAKE) {}"|sh

%.tgz:
	echo $@ | sed -E "s#archive/([^/]+)/([^/]+)/(.+)([-_])([^-_]+)\$$#curl -L --create-dirs --output $@ http://beta.quicklisp.org/archive/\1/\2/\3\4\5#g" | sh

extract: $(VERSION)/releases.txt
	cat $< | \
	grep -v '^#' | \
	awk -v 'OFS= ' '{print $$2}'| \
	sed -E "s#(^http://[^/]+/)(.+)\$$#\2#g" | \
	xargs -P3 -I {} echo "tar xf {} -C $(VERSION)/"|sh

$(VERSION)/Makefile: $(VERSION)/systems.txt
	SCRIPTDIR="../scripts/" scripts/gen-makefile.ros $< ./ "" $@

json: $(VERSION)/Makefile
	cd $(VERSION);$(MAKE) -k

cmd/searty-index/searty-index:
	cd cmd/searty-index;go build

cmd/searty-merge/searty-merge:
	cd cmd/searty-merge;go build

JSON = $(wildcard $(VERSION)/*.json)

sqlite3: $(JSON:%.json=%.sqlite3)

%.sqlite3: %.json cmd/searty-index/searty-index
	./cmd/searty-index/searty-index -o $(shell dirname $@) $<

merge: cmd/searty-merge/searty-merge
	./cmd/searty-merge/searty-merge -o $(VERSION)/quicklisp.db $(VERSION)/*.sqlite3

clean:
	rm -rf $(VERSION)

alpine:
	docker run -v `pwd`:/tmp2 -it alpine:3.14 /bin/ash
install-roswell-musl:
	apk add alpine-sdk autoconf automake curl-dev
	git clone https://github.com/roswell/roswell
	cd roswell;./bootstrap
	cd roswell;./configure --with-sbcl-bin-variant=-musl
	cd roswell;make; make install
	ros
