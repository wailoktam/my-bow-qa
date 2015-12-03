#SBTURL := https://dl.bintray.com/sbt/native-packages/sbt/0.13.9/
#SBT := sbt-0.13.9.tgz
JAWIKIURL := https://dumps.wikimedia.org/jawiki/latest
JAWIKI1FILE := jawiki-latest-pages-articles1.xml.bz2
JAWIKI2FILE := jawiki-latest-pages-articles2.xml.bz2
JAWIKI3FILE := jawiki-latest-pages-articles3.xml.bz2
JAWIKI4FILE := jawiki-latest-pages-articles4.xml.bz2
JUMANPATH := /share/tools/juman-7.0/bin/juman
KNPPATH := /share/tools/knp-4.11/bin/knp
PACKED := packed
JIGGURL := https://github.com/mynlp/jigg/raw/master/jar
JIGGFILE := jigg-0.4.jar
JIGGPATH := qa/jigg/jar
JAWIKI1RAW := qa/data/downloaded/jawiki/1
JAWIKI2RAW := qa/data/downloaded/jawiki/2
JAWIKI3RAW := qa/data/downloaded/jawiki/3
JAWIKI4RAW := qa/data/downloaded/jawiki/4
JAWIKICLN := qa/data/clean/jawiki
1CLN := 1
2CLN := 2
3CLN := 3
4CLN := 4
#WIKITOOLURL := http://medialab.di.unipi.it/Project/SemaWiki/Tools
WIKITOOLFILE := WikiExtractor.py
WIKITOOL := qa/tool
JAR := target/scala-2.11/qa-assembly-1.0.jar
INDEX := qa/index
PARSERPATH := ~/jigg/jar/*
INPUT2PARSE := input/questions/NIILC-ECQA2015_dev.xml
INPUT2CLUEX := input/questions/qa-sampleParsed.xml
INPUT2SEARCH := input/questions/qa-sampleClueExtracted.xml
INPUT2ANSWER := input/questions/qa-sampleDocRetrieved.xml
OUTPUT := input/questions/qa-sampleAnswered.xml

.PHONY: all
all: ${HOME}/${JAWIKI1RAW} ${HOME}/${JAWIKI2RAW} ${HOME}/${JAWIKI3RAW} ${HOME}/${JAWIKI4RAW} ${HOME}/${JIGGPATH}
#ifeq  "$(wildcard ${HOME}/${JAWIKI4RAW})" ""
${HOME}/${JAWIKI1RAW}:
	wget ${JAWIKIURL}/${JAWIKI1FILE} --directory-prefix=${HOME}/${JAWIKI1RAW}
${HOME}/${JAWIKI2RAW}:
	wget ${JAWIKIURL}/${JAWIKI2FILE} --directory-prefix=${HOME}/${JAWIKI2RAW}
${HOME}/${JAWIKI3RAW}:
	wget ${JAWIKIURL}/${JAWIKI3FILE} --directory-prefix=${HOME}/${JAWIKI3RAW}
${HOME}/${JAWIKI4RAW}:
	wget ${JAWIKIURL}/${JAWIKI4FILE} --directory-prefix=${HOME}/${JAWIKI4RAW}
#endif
#	./sbt/bin/sbt compile
#	./sbt/bin/sbt assembly
${HOME}/${JIGGPATH}:
	mkdir -p jigg
	cd jigg
	mkdir -p jar
	cd ..
	wget ${JIGGURL}/${JIGGFILE} --directory-prefix=${HOME}/${JIGGPATH}
#endif

.PHONY: run-preprocessing 
run-preprocessing: ${HOME}/${JAWIKICLN}/${1CLN} ${HOME}/${JAWIKICLN}/${2CLN} ${HOME}/${JAWIKICLN}/${3CLN} ${HOME}/${JAWIKICLN}/${4CLN} ${HOME}/${INDEX}
#	java -cp $(JAR) qa.main.ja.Indexing $(JAWIKI1) $(INDEX)
#	java -cp $(JAR) qa.main.ja.Indexing $(JAWIKI2) $(INDEX)
#	java -cp $(JAR) qa.main.ja.Indexing $(JAWIKI3) $(INDEX)
#ifeq "$(wildcard ${HOME}/${JAWIKI4CLN})" ""
${HOME}/${JAWIKICLN}/${1CLN}:
	python ${HOME}/${WIKITOOL}/${WIKITOOLFILE} -s --output ${HOME}/${JAWIKICLN}/${1CLN} ${HOME}/${JAWIKI1RAW}/${JAWIKI1FILE}
${HOME}/${JAWIKICLN}/${2CLN}:
	python ${HOME}/${WIKITOOL}/${WIKITOOLFILE} -s --output ${HOME}/${JAWIKICLN}/${2CLN} ${HOME}/${JAWIKI2RAW}/${JAWIKI2FILE}
${HOME}/${JAWIKICLN}/${3CLN}:
	python ${HOME}/${WIKITOOL}/${WIKITOOLFILE} -s --output ${HOME}/${JAWIKICLN}/${3CLN} ${HOME}/${JAWIKI3RAW}/${JAWIKI3FILE}
${HOME}/${JAWIKICLN}/${4CLN}:
	python ${HOME}/${WIKITOOL}/${WIKITOOLFILE} -s --output ${HOME}/${JAWIKICLN}/${4CLN} ${HOME}/${JAWIKI4RAW}/${JAWIKI4FILE}
#endif
#ifeq "$(wildcard ${HOME}/${INDEX})" ""
${HOME}/${INDEX}:
	java -cp $(JAR) qa.main.ja.Indexing ${HOME}/${JAWIKICLN} ${HOME}/${INDEX}
#endif



.PHONY: run-parsing-question
run-parsing-question:
	java -cp $(JAR) qa.main.ja.JiggParser ${HOME}/${JIGGPATH}/${JIGGFILE} $(JUMANPATH) $(KNPPATH) $(INPUT2PARSE) $(INPUT2CLUEX)

.PHONY: run-clue-extraction 
run-clue-extraction :
	java -cp $(JAR) qa.main.ja.ClueExtractor $(INPUT2CLUEX) $(INPUT2SEARCH)

.PHONY: run-search 
run-search :
	java -cp $(JAR) qa.main.ja.SearchDocument ${HOME}/${INDEX} $(INPUT2SEARCH) $(INPUT2ANSWER) 1 1.0 1.0

.PHONY: run-answer 
run-answer :
	java -cp $(JAR) qa.main.ja.AnswerTitle $(INPUT2ANSWER) $(OUTPUT)


.PHONY: run
run: run-preprocessing run-parsing-question run-clue-extraction run-search run-answer
