SBTURL := https://dl.bintray.com/sbt/native-packages/sbt/0.13.9/
SBT := sbt-0.13.9.tgz
PACKED := packed
JAWIKI1 := ~/data/jawiki/1
JAWIKI2 := ~/data/jawiki/2
JAWIKI3 := ~/data/jawiki/3
JAWIKI4 := ~/data/jawiki/4
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
	wget ${SBTURL}${SBT}
	tar -xvzf ${SBT}
	./sbt/bin/sbt compile

.PHONY: run-indexing 
run-indexing:
#	java -cp $(JAR) qa.main.ja.Indexing $(JAWIKI1) $(INDEX)
#	java -cp $(JAR) qa.main.ja.Indexing $(JAWIKI2) $(INDEX)
#	java -cp $(JAR) qa.main.ja.Indexing $(JAWIKI3) $(INDEX)
	java -cp $(JAR) qa.main.ja.Indexing $(JAWIKI4) $(INDEX)

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
