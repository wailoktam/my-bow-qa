#! /usr/bin/python -S
#_*_ coding: utf-8 _*_
# Compute correct answer ratios for question answering output
import sys
reload(sys)
sys.setdefaultencoding("utf-8")
import site

import unicodedata
import re
import string
import codecs
import kanjinums
#from pyknp import Juman
from py4j.java_gateway import JavaGateway
from operator import is_not
from functools import partial

gateway = JavaGateway()

from xml.etree import ElementTree as etree


MY_NAME = __file__
bugcheck  = codecs.open('bug.csv', 'w', 'utf-8')
bugcheck.write("%s,%s,%s,%s,%s,%s\n" % ("question text", "answer", "found in sentence #", "found after what percentage", "sentence including answer", "doc including answer"))



def myNormalize(inputStr):
    KuroTokenizer = gateway.jvm.org.atilika.kuromoji.Tokenizer
    tokenizer = KuroTokenizer.builder().build();
    result = tokenizer.tokenize(inputStr)
    normalized = ""
    for token in result:
        try:
            normalizedToken = kanjinums.kanji2num(token.getSurfaceForm())
        except KeyError:
            normalizedToken = token.getSurfaceForm()
        try:
            normalized += normalizedToken
        except Exception:
            normalized += str(normalizedToken)
    normalized = unicodedata.normalize('NFKC',unicode(normalized))
    return(normalized)


if len(sys.argv) != 2:
    print("Usage: python {} INPUT_XML".format(MY_NAME))
    print("This program accepts QA XML file")
    sys.exit(1)

INPUT_XML = sys.argv[1]
#parser = etree.XMLParser(encoding="utf-8-sig")
xml = etree.parse(INPUT_XML)
questions = xml.findall(".//question")
if len(questions) == 0:
    print("Error: no <question> elements found.  Maybe a wrong file is specified.")
    sys.exit(1)

# 各 question の precision/recall を計算し、平均を取る
sum_precision = 0
sum_recall = 0
sum_found = 0
sum_found_in_doc = 0
total_answers = 0
sum_precision_nz = 0
sum_recall_nz = 0
questions_nz = 0
answerable_count = 0
same_count = 0
by_type_sum_precision = {}
by_type_sum_recall = {}
by_type_sum_found = {}
by_type_sum_found_in_doc = {}
type_mem_count = {}
questionTypes = set()
zeroTypes = ["いくら","どれくらい","なに＋アルファベット","どのくらい","なに＋カタカナ","なに＋その他","いくつ","いつ","どんな","どの","どれ","どちら","どう"]

print("Evaluating %s" % INPUT_XML)
print("-----")

for question in questions:
    # 各 question について、answer, response を全て取り出す
    questionType = question.find(".//B1").text
    questionText = question.find(".//text").text
    answerable = question.find(".//E.1").text
    title = question.find(".//E.3").text
    if questionType not in questionTypes:
        by_type_sum_precision[questionType] = 0
        by_type_sum_recall[questionType] = 0
        by_type_sum_found[questionType] = 0
        by_type_sum_found_in_doc[questionType] = 0
        type_mem_count[questionType] = 0
        questionTypes.add(questionType)
    type_mem_count[questionType] = type_mem_count[questionType] + 1
    answers = map(lambda a: a.text, question.findall(".//answer"))
    responses = map(lambda r: r.text, question.findall(".//response"))
    docs = question.findall(".//doc")
    if title in answers: same_count =  same_count+1
    if answerable == "Y": answerable_count = answerable_count+1
    # TODO: question のタイプによって、スコアの計算方法を変える
    # ここではとりあえず precision/recall を計算
    question_id = question.get("id")
    answers = filter(partial(is_not, None), answers)
    responses = filter(partial(is_not, None), responses)
    #if len(responses) == 0: print ("no response %s" % (question_id))
    num_correct_answers = float(len(set(map(lambda a: myNormalize(a), answers)) & set(map(lambda a: myNormalize(a), responses))))
    precision = float(num_correct_answers / len(responses))
    recall = float(num_correct_answers / len(answers))
    if num_correct_answers:
        print ("found in title: questionId %s docTitle %s" % (question_id,doc_title.strip()))
        sum_found = sum_found + 1
        by_type_sum_found[questionType] = by_type_sum_found[questionType]+1
    for answer in answers:
        found_once = 0

        for doc in docs:
            doc_title = doc.find(".//dtitle").text
            doc_txt = doc.find(".//dtext").text
            if answer in doc_txt and found_once==0:
                found_once = 1
                sentences = doc_txt.split('。')
                sentenceCounter = 0
                foundCounter = 0
                for sentence in sentences:
                    sentenceCounter = sentenceCounter + 1
                    if answer in sentence and foundCounter==0:
                        foundCounter = sentenceCounter
                        bracketedSent =sentence.replace(answer,"{"+answer+"}")
                bracketedDoc = doc_txt.replace(answer, "{"+answer+"}")
                bugcheck.write("%s,%s,%f,%f, %s,%s\n" % (questionText.strip(), answer.strip(), foundCounter, float(float(foundCounter)/float(sentenceCounter)*100), bracketedSent.strip(), bracketedDoc.strip()))
                print ("found in doc: questionId %s docTitle %s" % (question_id,doc_title.strip()))
                sum_found_in_doc = sum_found_in_doc + 1
                by_type_sum_found_in_doc[questionType] = by_type_sum_found_in_doc[questionType] + 1
    total_answers = total_answers + len(set(answers))
    sum_precision = sum_precision + precision
    sum_recall = sum_recall + recall
    if questionType.strip() not in zeroTypes:
        sum_precision_nz = sum_precision_nz + precision
        sum_recall_nz = sum_recall_nz + recall
        questions_nz = questions_nz + 1
    by_type_sum_precision[questionType] = by_type_sum_precision[questionType] + precision
    by_type_sum_recall[questionType] = by_type_sum_recall[questionType] + recall
#    bugcheck.write("%s: %f %f %f %f %f" % (question_id, precision, recall, num_correct_answers, len(answers),len(responses)))
#    for answer in answers:
#        try:
#            bugcheck.write("%s " % answer)
#        except TypeError:
#            bugcheck.write("error")
#    bugcheck.write("\n")
#    for response in responses: bugcheck.write("%s|" % response)
#    bugcheck.write("\n")

print("-----")
print("questions: %d" % len(questions))
print("precision: %f" % (sum_precision / len(questions)))
print("recall: %f" % (sum_recall / len(questions)))
print("found in titles: %f" % (sum_found))
print("found in docs: %f" % (sum_found_in_doc))
print("total_answers: %f" % total_answers)
print("average answers per question: %f" % (float(total_answers)/800))
print("non-zero type questions: %d" % questions_nz)
print("non-zero type precision: %f" % (sum_precision_nz / questions_nz))
print("non-zero type recall: %f" % (sum_recall_nz / questions_nz))
print("answerable count: %f" % (answerable_count))
print("answer same as title: %f" % (same_count))
#for key in by_type_sum_precision.keys():
#    bugcheck.write("type: %s\n" % key)
#    bugcheck.write("type member count: %s\n" % type_mem_count[key])
#    bugcheck.write("precision by type: %f\n" % by_type_sum_precision[key])
#    bugcheck.write("recall by type: %f\n" % by_type_sum_recall[key])
#    bugcheck.write("found in all titles by type: %f\n" % by_type_sum_found[key])
#    bugcheck.write("found in all docs by type: %f\n" % by_type_sum_found_in_doc[key])
