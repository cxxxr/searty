## 手順

```bash
$ ./download-index.sh ~/quicklisp-dist    # ~/quicklisp-dist/以下にquicklisp.txt, releases.txt, systems.txtをダウンロードする
$ ./download-repos.sh ~/quicklisp-dist    # ~/quicklisp-dist/<version>/以下に各リポジトリをダウンロードする
```

```bash
$ ./gen-makefile.ros ~/quicklisp-dist/systems.txt ~/quicklisp-dist/2022-04-01/ /tmp/searty/ Makefile
$ make -j24 -k
```
