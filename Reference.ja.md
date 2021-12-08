# ClipboardMonitor拡張マニュアル

本アプリケーションはPythonスクリプトを`%LOCALAPPDATA%\ClipboardMonitor`に格納しています。

このフォルダはアプリケーションのコンテキストメニューからも開くことができますので、必要に応じてスクリプトを追加・編集してください。

ここでは、このスクリプトでできることや、入出力の方法をまとめています。

なお、ファイル名の先頭がアンダーバー(_)ではじまっているファイルはClipboardMonitorシステム予約とします。

今後スクリプトを追加する場合**必ずファイル名の先頭にアンダーバーが入ります**ので、**利用者が独自にスクリプトを作成する場合はアンダーバーからはじまらない名前をつけて**ください。

## すべてに共通の事項

コピー時スクリプトにも、変換スクリプトにも共通の事項は以下の通りです。

* クリップボードに格納されている文字列が、標準入力に書き込まれた状態でスクリプトは実行される。
* 標準出力に書き込まれた文字列がスクリプトの出力とされる。
* エラーが発生した場合、標準エラー出力の内容はアプリケーションから参照できる。
* 標準入力・標準出力・標準エラー出力すべてUTF-8エンコードのテキストとして扱われる。
* スクリプトファイル上部にスクリプトの表示名や表示順を規定する設定領域を作成できる(任意)。

### 設定領域とは

スクリプトがアプリケーションのメニュー上でどのように表示されるかや、メニュー上の並び順を定義する値です。
INIファイルの形式となっており、基本的には前後を三つのダブルクオートで囲みます(Python上で実行しないようにするため)。
v1.0時点で対応しているセクションおよびキーは以下の通り

* captionセクション: メニュー上に表示される名前を規定します。なかった場合はファイル名が表示されます。
  * def: 言語設定がdef(英語)の時に表示される名称
  * ja: 言語設定がja(日本語)の時に表示される名称
* generalセクション
  * order: スクリプトのメニュー上の順序。0に近い方が上位に並びます。指定がなかった場合100が設定されたものとみなします。

#### 例文(_charcount.pyより)

```python
"""
[caption]
def=&Counting characters
ja=文字数のカウント(&C)
[general]
order=100
"""

```
空行が入るとそこでIniファイルの定義は終わりです。

## コピー時スクリプト(Runフォルダ)

コピー時スクリプトは、クリップボードにテキストが追加されたときに呼び出されます。

標準出力に書き込んだ内容が、アプリケーションの下部ステータスビューに表示されます。

なお、ここにはごく簡単なHTMLタグが使用可能です。使用可能なタグは以下の通り。

* p
* a
* b
* i
* pre
* ul
* ol
* li
* dl
* dt
* dd
* br
* span
* div

CSSスタイルも色指定など簡易的なもののみ使用可能です。

### スクリプト記載例(_colorcode.pyより)

```python
import sys, io, re

sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

text = sys.stdin.read()
m =  re.match(r"^#([0-9a-f]{6}|[0-9a-f]{3})$", text, re.IGNORECASE)
if m: print(f'<p>Color:<span style="color:#{m[1]};">#{m[1]}</span></p>')
```

## 変換スクリプト

変換スクリプトは、変換スクリプトメニューより選択することで、クリップボードの内容を書き換え可能です。

標準出力に書き込んだ内容が、クリップボードに貼り付けされます(最後に改行があれば、それは削除されます)。

### スクリプト記載例(_bullet.pyより)

```python
import sys, io, re

sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

text = sys.stdin.read()

if text != "":
  print(re.sub(r"^.+$", "* \g<0>", text, flags=re.M))
```
