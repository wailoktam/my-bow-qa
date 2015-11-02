# トップレベルの Makefile。各サブディレクトリで make を実行するだけ
# NOTE: 各サブディレクトリは、all, test, clean, distclean を必ず定義すること

SUBDIRS = data sample

# プログラムをビルドする
all:
	@for subdir in $(SUBDIRS); do (cd $$subdir && $(MAKE) $@); done

# テストプログラムを走らせる
test:
	@for subdir in $(SUBDIRS); do (cd $$subdir && $(MAKE) $@); done

# ビルドしたファイルを消す
clean:
	@for subdir in $(SUBDIRS); do (cd $$subdir && $(MAKE) $@); done

# 配布パッケージに含めないファイルを全て消す
distclean:
	@for subdir in $(SUBDIRS); do (cd $$subdir && $(MAKE) $@); done
