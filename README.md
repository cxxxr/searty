# searty
Common Lispのソースコードを対象にした全文検索エンジン

## Features
現状では、quicklispに登録されている全てのリポジトリを対象にインデックスを作成し、それを元にフレーズ検索を行うことができます。  
ソースコード中のシンボルをtrigram単位でインデックス化し、シンボルの完全一致/部分一致する箇所を検索します。

## Requirement
環境はUbuntu 20.04以降を想定しています。  
追加でインストールが必要なソフトウェアは主に以下のとおりです。  

- curl
- roswell
- sqlite3 (Ubuntu 20.04で入るsqlite3は古いため、手動で新しいバージョンを入れる必要があるかもしれません)

## Usage

### インデックスの作成
quicklispに登録されている全てのリポジトリを対象にしたインデックスを作成します。

```bash
$ ./download-index.sh ~/quicklisp-dist    # ~/quicklisp-dist/にquicklisp.txt, releases.txt, systems.txtをダウンロードする
$ ./download-repos.sh ~/quicklisp-dist    # ~/quicklisp-dist/<version>/に全てのリポジトリをダウンロードする
$ ./index.sh
```

インデックスの作成には数十分〜数時間ほどかかり、デフォルトでは24並列でインデックスを作成します。  
並列化数などは適時index.shなどを書き換える必要があるかもしれません。

これによって作られたデータベースは `db/quicklisp.db` に事前に用意してあり、検索時にはそれを参照します。

### 検索

作成したインデックスからシンボルを検索をします。

```
CL-USER> (ql:quickload :searty)
CL-USER> (searty:search-quicklisp "define-command" :start-boundary t :end-boundary t)

/home/user/quicklisp-dist/2022-04-01/cl-twitter-20180228-git/api/commands.lisp:31:10:24:(defmacro define-command (command (method return-type) base-url description &body args)
/home/user/quicklisp-dist/2022-04-01/cl-twitter-20180228-git/api/twitter-user.lisp:138:1:15:(define-command users/show (:get :twitter-user)
/home/user/quicklisp-dist/2022-04-01/cl-twitter-20180228-git/api/twitter-user.lisp:146:1:15:(define-command users/lookup (:get (:twitter-user) )
/home/user/quicklisp-dist/2022-04-01/cl-twitter-20180228-git/api/twitter-user.lisp:153:1:15:(define-command users/search (:get (:twitter-user) )
/home/user/quicklisp-dist/2022-04-01/cl-twitter-20180228-git/api/twitter-user.lisp:161:1:15:(define-command users/profile-banner (:get (:identity))
...
```

`search-quicklisp`に `start-boundary`, `end-boundary` をbooleanで渡すことで部分一致/完全一致を選べます。(両方`T`で完全一致, `start-boundary`のみ`T`で前方一致)

### License
MIT
