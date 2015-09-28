# coding: utf-8
# Compute correct answer ratios for question answering output

import sys
from xml.etree import ElementTree as etree

MY_NAME = __file__

if len(sys.argv) != 2:
    print("Usage: python {} INPUT_XML".format(MY_NAME))
    print("This program accepts QA XML file")
    sys.exit(1)

INPUT_XML = sys.argv[1]

xml = etree.parse(INPUT_XML)
questions = xml.findall(".//question")
if len(questions) == 0:
    print("Error: no <question> elements found.  Maybe a wrong file is specified.")
    sys.exit(1)

# 各 question の precision/recall を計算し、平均を取る
sum_precision = 0
sum_recall = 0

print("Evaluating %s" % INPUT_XML)
print("-----")

for question in questions:
    # 各 question について、answer, response を全て取り出す
    answers = map(lambda a: a.text, question.findall(".//answer"))
    responses = map(lambda r: r.text, question.findall(".//response"))
    # TODO: question のタイプによって、スコアの計算方法を変える
    # ここではとりあえず precision/recall を計算
    question_id = question.get("id")
    num_correct_answers = len(set(answers) & set(responses))
    precision = num_correct_answers / len(responses)
    recall = num_correct_answers / len(answers)
    sum_precision = sum_precision + precision
    sum_recall = sum_recall + recall
    print("%s: %f %f" % (question_id, precision, recall))

print("-----")
print("questions: %d" % len(questions))
print("precision: %f" % (sum_precision / len(questions)))
print("recall: %f" % (sum_recall / len(questions)))
