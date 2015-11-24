#SBTURL := https://dl.bintray.com/sbt/native-packages/sbt/0.13.9/
#SBT := sbt-0.13.9.tgz
JAWIKI4URL := https://dumps.wikimedia.org/jawiki/latest
JAWIKI4FILE := jawiki-latest-pages-articles4.xml.bz2
PACKED := packed
JAWIKI1 := ~/data/jawiki/1
JAWIKI2 := ~/data/jawiki/2
JAWIKI3 := ~/data/jawiki/3
JAWIKI4 := data/jawiki/4
WIKITOOLURL := http://medialab.di.unipi.it/Project/SemaWiki/Tools
WIKITOOLFILE := WikiExtractor.py
WIKITOOL := tool
JAR := target/scala-2.11/qa-assembly-1.0.jar
INDEX := index
PARSERPATH := ~/jigg/jar/*
INPUT2PARSE := input/questions/NIILC-ECQA2015_dev.xml
INPUT2CLUEX := input/questions/qa-sampleParsed.xml
INPUT2SEARCH := input/questions/qa-sampleClueExtracted.xml
INPUT2ANSWER := input/questions/qa-sampleDocRetrieved.xml
OUTPUT := input/questions/qa-sampleAnswered.xml

.PHONY: all
all:
	wget ${JAWIKI4URL}/${JAWIKI4FILE} --directory-prefix=${HOME}/${JAWIKI4}
	wget ${WIKITOOLURL}/${WIKITOOLFILE} --directory-prefix=${HOME}/${WIKITOOL}
	./sbt/bin/sbt compile
	./sbt/bin/sbt assembly

.PHONY: run-preprocessing 
run-preprocessing:
#	java -cp $(JAR) qa.main.ja.Indexing $(JAWIKI1) $(INDEX)
#	java -cp $(JAR) qa.main.ja.Indexing $(JAWIKI2) $(INDEX)
#	java -cp $(JAR) qa.main.ja.Indexing $(JAWIKI3) $(INDEX)
	python ${HOME}/${WIKITOOL}/${WIKITOOLFILE} -s --output ${HOME}/${JAWIKI4} ${HOME}/${JAWIKI4}/${JAWIKI4FILE}
	java -cp $(JAR) qa.main.ja.Indexing ${HOME}/${JAWIKI4} $(INDEX)

.PHONY: run-parsing-question
run-parsing-question:
	java -cp $(JAR) qa.main.ja.JiggParser $(PARSERPATH) $(INPUT2PARSE) $(INPUT2CLUEX)

.PHONY: run-clue-extraction 
run-clue-extraction :
	java -cp $(JAR) qa.main.ja.ClueExtractor $(INPUT2CLUEX) $(INPUT2SEARCH)

.PHONY: run-search 
run-search :
	java -cp $(JAR) qa.main.ja.SearchDocument $(INDEX) $(INPUT2SEARCH) $(INPUT2ANSWER)

.PHONY: run-answer 
run-answer :
	java -cp $(JAR) qa.main.ja.AnswerTitle $(INPUT2ANSWER) $(OUTPUT)


.PHONY: run
run: run-parsing-question run-clue-extraction run-search run-answer
