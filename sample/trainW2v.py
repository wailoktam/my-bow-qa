# !/usr/bin/env python
# -*- coding:utf-8 -*-
import gensim
import os
import re, pprint
import sys
reload(sys)
sys.setdefaultencoding("utf-8")
import site
import numpy
import unicodedata
import re
import string
import codecs
import kanjinums
from py4j.java_gateway import JavaGateway
from operator import is_not
from functools import partial

gateway = JavaGateway()

from xml.etree import ElementTree as etree

from gensim.models import word2vec

def myNormalize(inputStr):
    KuroTokenizer = gateway.jvm.org.atilika.kuromoji.Tokenizer
    tokenizer = KuroTokenizer.builder().build()
    result = tokenizer.tokenize(inputStr)
#    for token in result:
#        print "token"+token.getSurfaceForm()
#    inds = []
    print ("inputStr")
    print inputStr
    print ("result")
    print result
    normalizedTokens = []
    for token in result:
        try:
            normalizedToken = kanjinums.kanji2num(token.getBaseForm())
        except KeyError:
            normalizedToken = token.getBaseForm()
        except TypeError:
            normalizedToken = token.getSurfaceForm()
        normalizedTokens.append(unicodedata.normalize('NFKC', unicode(normalizedToken)))
    return(" ".join(normalizedTokens))




def pp(obj):
    pp = pprint.PrettyPrinter(indent=4, width=160)
    str = pp.pformat(obj)
    return re.sub(r"\\u([0-9a-f]{4})", lambda x: unichr(int("0x"+x.group(1), 16)), str)




if __name__ == '__main__':
#    window = sys.argv[1]
    wikiOnePPPath = "/home/wailoktam/qa/mylab/onePagePerFile"
    sentenceList = []
    corpusFileName = '/mnt/Works/wailoktam/segmentedS'
    corpusFile  = codecs.open(corpusFileName, 'w', 'utf-8')
    for subPath in os.listdir(wikiOnePPPath):
        fullPath = os.path.join(wikiOnePPPath,subPath)
#        print ("path"+fullPath+"\n")
        if os.path.isfile(fullPath):
            parsedXml = etree.parse(fullPath)
#            print(parsedXml.getroot().tag)
#            print(parsedXml.getroot().text)
#            pageText = parsedXml.find("./id").text
#            print ("pageText"+pageText+"\n")
            paras = parsedXml.findall(".//para")
            for para in paras:
                paraText = para.find(".//text").text
#                print ("para:"+para.text+"\n")
#                ''.join(unicode(paraText, 'utf-8').splitlines())
                for sent in paraText.strip().split("。"):
                    print ("sent",sent+"\n")
                    corpusFile.write(myNormalize(sent))
#                    print ("mynormalize output")
#                    print (myNormalize(sent))
    corpusFile.close()


    sentences = word2vec.Text8Corpus(corpusFileName)
    model = word2vec.Word2Vec(sentences, size=100)
    model.save('/home/wailoktam/model')



#    savedModel = word2vec.Word2Vec.load('/home/wailoktam/model')
#model = Word2Vec.load_word2vec_format('/home/wailoktam/model', unicode_errors='ignore')
#    out = savedModel.most_similar(positive=[u'マニアックネタ'])
#    vector = savedModel[u'マニアックネタ']
#    print(pp(out[1]))
#    print (vector)