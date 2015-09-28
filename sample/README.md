サンプルプログラム
=====

* `toy.py`
  * NII-LCフォーマットのXMLファイル（../data/nii-lc/sample.xml など）を読み込み、正解（<answers>タグの中身）をそのまま解答するトイプログラム。
  * このプログラムの出力を（eval.py などで）評価すれば、正答率100%になるはず。

* `eval.py`
  * 解答（<responses>タグ）付きのNII-LCフォーマットXMLファイルを読み込み、正答率を出力する。

## 実行例

```bash
python toy.py ../data/nii-lc/sample.xml sample.toy.xml
python eval.py sample.toy.xml
```
