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

## Eval Result Log    
test1
evalInsur.py

whatprecision:0.241304347826
whatcount:460
whoprecision:0.202127659574
whocount:94
whenprecision:0.0694444444444
whencount:72
whereprecision:0.2
wherecount:40
whyprecision:0.174418604651
whycount:86
howprecision:0.119897959184
howcount:392
whichprecision:0.114285714286
whichcount:35
otherprecision:0.258221680877
othercount:821
overall precision:0.2105
overall count2000
