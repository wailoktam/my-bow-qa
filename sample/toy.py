# coding: utf-8
# Toy (or cheating) program of question answering
# This simply copies given correct answers to output

import sys
from xml.etree import ElementTree as etree

MY_NAME = __file__

if len(sys.argv) != 3:
    print("Usage: python {} INPUT_XML OUTPUT_XML".format(MY_NAME))
    print("This program accepts QA XML file")
    sys.exit(1)

INPUT_XML = sys.argv[1]
OUTPUT_XML = sys.argv[2]

xml = etree.parse(INPUT_XML)
questions = xml.findall(".//question")
if len(questions) == 0:
    print("Error: no <question> elements found.  Maybe a wrong file is specified.")
    sys.exit(1)

for question in questions:
    # 各 question について、answer を全て取り出す
    answers = question.findall(".//answer")
    # answer に書いてあるテキストをそのまま response として返す
    responses = map(lambda a: a.text, answers)
    # responses エレメントを作る
    responses_xml = etree.Element("responses", attrib={"annotator": MY_NAME})
    for response in responses:
        response_xml = etree.Element(tag="response")
        response_xml.text = response
        responses_xml.append(response_xml)
    # responses を入力 XML に追加する
    question.append(responses_xml)

# 処理のログを <log> エレメントに出力
log = etree.Element("log", attrib={"annotator": MY_NAME})
log.text = "%d questions processed" % len(questions)
xml.getroot().append(log)

xml.write(OUTPUT_XML, encoding="utf-8")
