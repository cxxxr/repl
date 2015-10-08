# repl
common lispのrepl

## 特徴
readlineを使って式単位の入力履歴やシェルコマンドの呼び出し  
シンボルやファイル名の補完

## 対応している処理系
sbcl

## インストール
`make.sh`でshell-command.soを生成した後にasdfでreplをロードする

## 使い方
`(repl:repl)`でreplが起動してプロンプトが表示される

* 先頭が!だとその行の後ろの文字列はシェルに渡されて実行される
* 先頭がシンボルでそのシンボルの関数が定義されていたらそれを呼び出す
* カレントディレクトリの`replrc.lisp`または`$HOME/.replrc.lisp`を初期化ファイルとして読み込む

## ライセンス
[MIT](https://github.com/cxxxr/repl/blob/master/LICENSE)
