__author__ = 'wailoktam'
from keras.models import Sequential
from keras.datasets import cifar10
from keras.utils import np_utils
from keras.models import model_from_json
from keras.layers import Merge
from scipy import linalg, mat, dot
from keras.layers.core import Dense, Dropout, Activation, Flatten
from keras.layers.convolutional import Convolution2D, Convolution1D,MaxPooling2D
from keras.optimizers import SGD
import numpy as np
import scipy.misc
import numpy
import theano
import theano.tensor as T
import kanjinums
import re
import gensim
import os
from xml.etree import ElementTree as etree
from gensim.models import Word2Vec
from py4j.java_gateway import JavaGateway
from operator import is_not
from functools import partial

gateway = JavaGateway()



#model.add_node(Similarity(1, activation='sigmoid', input_shapes=[model.nodes['input1'].output_shape, model.nodes['input2'].output_shape]), name='sim', inputs=['input1','input2'], merge_mode='join')
#model.add_node(Similarity(), name='similarity',  input=['inp1repr', 'inp2repr'], merge_mode='join')

def myNormalize(inputStr):
    KuroTokenizer = gateway.jvm.org.atilika.kuromoji.Tokenizer
    tokenizer = KuroTokenizer.builder().build();
#    print ("inputStr"+inputStr+"\n")
    result = tokenizer.tokenize(inputStr)
    normalized = []
    for token in result:
        print ("token"+"["+token.getSurfaceForm()+"]"+"\n")
        try:
            normalizedToken = kanjinums.kanji2num(token.getSurfaceForm())
        except KeyError:
            normalizedToken = token.getSurfaceForm()
        try:
            normalized.append(str(normalizedToken))
        except Exception:
            normalized.append(normalizedToken)
#    normalized = unicodedata.normalize('NFKC',unicode(normalized))
    return(normalized)

def rmvSpecChar(inputLst):
    specCharRe = re.compile('[*\s]')
    outputLst = []
    for inputEle in inputLst:
        if not re.match(specCharRe,inputEle):outputLst.append(inputEle)
    return(outputLst)


def get_output(self, train):
    X = self.get_input() # returns OrderedDict {'inp1repr': tensor1, 'inp2repr': tensor2}
    X = list(X.items())
    return T.dot(T.dot(X[0], self.W), X[1])



def compile_cos_sim_theano():
    v1 = theano.tensor.vector(dtype=theano.config.floatX)
    v2 = theano.tensor.vector(dtype=theano.config.floatX)
    numerator = theano.tensor.sum(v1*v2)
    denominator = theano.tensor.sqrt(theano.tensor.sum(v1**2)*theano.tensor.sum(v2**2))
    return theano.function([v1, v2], numerator/denominator)

cos_sim_theano_fn = compile_cos_sim_theano()

def custom_objective(y_true, y_pred):
    if (y_true == 1):
        result = T.maximum(0.09 - y_pred, 0.)
    else:
        result = T.maximum(0.09 + y_pred, 0.)
    return result


def make_network():
   leftKerasModel = Sequential()
   leftKerasModel.add(Convolution2D(10, 3, 3, border_mode='same',
                           input_shape=(100, 100,8)))
   leftKerasModel.add(Activation('relu'))
   leftKerasModel.add(MaxPooling2D(pool_size=(2, 2)))

   rightKerasModel = Sequential()
   rightKerasModel.add(Convolution2D(10, 3, 3, border_mode='same',
                           input_shape=(100, 100,8)))
   rightKerasModel.add(Activation('relu'))
   rightKerasModel.add(MaxPooling2D(pool_size=(2, 2)))

   mergedKerasModel = Sequential()
   mergedKerasModel.add(Merge([leftKerasModel,rightKerasModel], mode= lambda l,r: dot(l,r.T)/linalg.norm(l).linalg.norm(r)))

   mergedKerasModel.add(Dense(10),activation='softmax')
   return mergedKerasModel

def train_model(model, leftData, rightData, labels):

   sgd = SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)
   model.compile(loss='custom_objectivity', optimizer=sgd)
   model.fit([rightData, leftData], labels, nb_epoch=10, batch_size=32)

#   print('Testing...')
#   res = model.evaluate(X_test, Y_test,
#                        batch_size=32, verbose=1, show_accuracy=True)
#   print('Test accuracy: {0}'.format(res[1]))

def save_model(model):

   model_json = model.to_json()
   open('trainKeras', 'w').write(model_json)
   model.save_weights('trainKeras_weights.h5', overwrite=True)


def load_and_scale_imgs():
   img_names = ['standing-cat.jpg', 'dog-face.jpg']

   imgs = [np.transpose(scipy.misc.imresize(scipy.misc.imread(img_name), (32, 32)),
                        (2, 0, 1)).astype('float32')
           for img_name in img_names]
   return np.array(imgs) / 255


if __name__ == '__main__':
   w2vModel= Word2Vec.load('/home/ubuntu/model')
   xml = etree.parse("/home/ubuntu/qa/mylab/input/questions/qa-sampleDocRetrievedBySect.xml")
   questions = xml.findall(".//question")
   labels = numpy.array([])
   qMatrix = numpy.array([])
   aMatrix = numpy.array([])
   qFile = open('qFile', 'w')
   aFile = open('aFile', 'w')
   lFile = open('lFile', 'w')


   zeroFilledVector = numpy.array([])
   for question in questions:
        questionText = question.find(".//text").text
        qCounter = 0
        aCounter = 0

        qSkip = False
        answers = map(lambda a: a.text, question.findall(".//answer"))
        answers = filter(partial(is_not, None), answers)
        for doc in question.findall(".//doc"):

            for sent in doc.findall(".//stext"):
                answerFoundFlag = False
                normalizedSentence = myNormalize(sent.text.strip())
                sentenceWoSc = rmvSpecChar(normalizedSentence)
#max question length is 33. Loop thru each word. If lenght less than 36, add all-zeroes vectors to the result matrix
                for word in myNormalize(questionText.strip()):
                    qCounter = qCounter + 1
#                    wvLength = len(w2vModel[word])
#                    print("wvLength %s/n"%wvLength)
                    try:
                        qMatrix = numpy.append(qMatrix, w2vModel[word])
                    except KeyError:
                        qSkip = True
                        qMatrix = numpy.append(qMatrix,zeroFilledVector)
                for i in range (1, 100):
                    zeroFilledVector = numpy.append(zeroFilledVector,0)
                for i in range (qCounter, 36):
                    qCounter = qCounter + 1
                    qMatrix = numpy.append(qMatrix, zeroFilledVector)

                aSkip = False
                for word in sentenceWoSc[:36]:
                    print ("normalizedSentence %s\n"%("/".join(normalizedSentence)))
                    print ("word in normalizedSentence %s\n"%(word))
                    aCounter = aCounter + 1
                    print("acounter in 1st loop %s/n"%aCounter)
#                    wvLength = len(w2vModel[word])
                    if qSkip: aSkip = True
                    try:
                        aMatrix = numpy.append(aMatrix, w2vModel[word])
                    except KeyError:
                        aSkip = True
                        aMatrix = numpy.append(aMatrix,zeroFilledVector)
                for i in range (aCounter, 36):
                    aCounter = aCounter + 1
                    print("acounter in 2nd loop %s/n"%aCounter)
                    aMatrix = numpy.append(aMatrix, zeroFilledVector)

                for answer in map(lambda a: myNormalize(a), answers):
                    joinedAnswer = "".join(answer).strip()
                    joinedSentence = "".join(sentenceWoSc).strip()
                    if joinedAnswer in joinedSentence:
                        answerFoundFlag = True
                if aSkip==False:
                    if answerFoundFlag:
                        labels = numpy.append(labels,1)
                    else:
                        labels = numpy.append(labels,0)


   numpy.save(qFile,qMatrix)
   numpy.save(aFile,aMatrix)
   numpy.save(lFile,labels)
   km = make_network()
   train_model(km,numpy.load(qFile), numpy.load(aFile),numpy.load(lFile))
   save_model(km)
   qFile.close()
   aFile.close()
   lFile.close()
   os.system('sudo shutdown now -P')



