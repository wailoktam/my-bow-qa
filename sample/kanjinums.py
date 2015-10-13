#coding: UTF8
"""
Converts kanji numbers into integers

Can covert numbers up to 9,999,999,999,999,999
(九千九百九十九兆九千九百九十九億九千九百九十九万九千九百九十九)

Released under MIT license.
"""
__version__ = "0.1"
__author__  = "Ryan Ginstrom"
__license__ = "MIT"
__description__ = "A module to convert kanji numbers into Python integers"

NUMS = ((1, u"一"),
        (2, u"二"),
        (3, u"三"),
        (4, u"四"),
        (5, u"五"),
        (6, u"六"),
        (7, u"七"),
        (8, u"八"),
        (9, u"九"),
        (10, u"十"),
        (100, u"百"),
        (1000, u"千"),
        (10000, u"万"),
        (100000000, u"億"),
        (1000000000000, u"兆"))

KANJIS = dict((kanji, num) for (num, kanji) in NUMS)

def _break_down_nums(nums):
    first, second, third, rest = nums[0], nums[1], nums[2], nums[3:]
    if first < third or third < second:
        return [first+second, third] + rest
    else:
        return [first, second*third] + rest

def kanji2num(kanji, enc="utf-8"):
    """
    Convert the kanji number to a Python integer.
    Supply `kanji` as a unicode string, or a byte string
    with the encoding specified in `enc`.
    """
    if not isinstance(kanji, unicode):
        kanji = unicode(kanji, enc)

    # get the string as list of numbers
    nums = [KANJIS[x] for x in kanji]

    num = 0
    while len(nums) > 1:
        first, second, rest = nums[0], nums[1], nums[2:]
        if second < first: # e.g. [10, 3, ...]
            if any(x > first for x in rest): # e.g. [500, 3, 10000, ...]
                nums = _break_down_nums(nums)
            else: # e.g. [500, 3, 10, ...]
                num += first
                nums = [second] + rest
        else: # e.g. [3, 10, ...]
            nums = [first*second] + rest

    return num + sum(nums)
