QA_DATA_DIR = ../data/nii-lc

all:

test: sample.toy.eval.txt

clean:
	rm -f sample.*

distclean: clean

%.toy.xml: $(QA_DATA_DIR)/%.xml toy.py
	python toy.py $< $@

%.eval.txt: %.xml eval.py
	python eval.py $< > $@

.PRECIOUS: %.toy.xml %.eval.txt
	
