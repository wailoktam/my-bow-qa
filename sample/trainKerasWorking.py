__author__ = 'wailoktam'
from keras.models import Sequential,Model
from keras.utils import np_utils
from keras.models import model_from_json
from keras.layers import Merge, Input, LSTM, Embedding
from scipy import linalg, mat, dot
from keras.layers.core import Dense, Dropout, Activation, Flatten, Reshape, Lambda
from keras.layers.convolutional import Convolution2D, Convolution1D,MaxPooling2D,MaxPooling1D
from keras.optimizers import SGD
import numpy as np
import scipy.misc
import numpy
import theano
import theano.tensor as T
import codecs
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
#        print ("token"+"["+token.getSurfaceForm()+"]"+"\n")
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



def make_network():
   leftKerasModel = Sequential()
   leftKerasModel.add(Dense((200), input_shape=(100, 100)))
   leftKerasModel.add(Reshape((1,100,100),  input_shape=(100, 100)))
   leftKerasModel.add(Convolution2D(10, 3, 3, border_mode='same'))
   leftKerasModel.add(Activation('relu'))
   leftKerasModel.add(MaxPooling2D(pool_size=(2, 2)))
#   leftKerasModel.add(Flatten())
   rightKerasModel = Sequential()
   rightKerasModel.add(Reshape((1,100,100), input_shape=(100,100)))
#   rightKerasModel.add(Convolution2D(10,1,3,3))
   rightKerasModel.add(Convolution2D(10, 3, 3, border_mode='same'))
   rightKerasModel.add(Activation('relu'))
   rightKerasModel.add(MaxPooling2D(pool_size=(2, 2)))
#   rightKerasModel.add(Flatten())
   mergedKerasModel = Sequential()
   merged = Merge([leftKerasModel, rightKerasModel], mode='cos')
#   merged = Merge([leftKerasModel, rightKerasModel], mode='cos')
   mergedKerasModel.add(merged)
#   mergedKerasModel.add(Lambda(lambda x: 1 - x))

   mergedKerasModel.add(Dense(2))
   mergedKerasModel.add(Activation('softmax'))
#   mergedKerasModel.add(Dense(2), activation='sigmoid'))
   print ("make network input shape")
   mergedKerasModel.summary()
   print mergedKerasModel.get_config()
   print (mergedKerasModel.input_shape)
   return mergedKerasModel


def make_arch1g_network():
    leftKerasModel = Sequential()
    leftKerasModel.add(Reshape((1, 100, 100), input_shape=(100, 100)))
    leftKerasModel.add(Convolution2D(10, 3, 3, border_mode='same'))
    leftKerasModel.add(Activation('relu'))
    leftKerasModel.add(MaxPooling2D(pool_size=(2, 2)))

    rightKerasModel = Sequential()
    rightKerasModel.add(Reshape((1, 100, 100), input_shape=(100, 100)))
    rightKerasModel.add(Convolution2D(10, 3, 3, border_mode='same'))
    rightKerasModel.add(Activation('relu'))
    rightKerasModel.add(MaxPooling2D(pool_size=(2, 2)))

    mergedKerasModel = Sequential()
    merged = Merge([leftKerasModel, rightKerasModel], mode='cos', dot_axes=1)  # dot_axes
    mergedKerasModel.add(merged)
    mergedKerasModel.add(Activation('relu'))  # add this line won't give me any error
    return mergedKerasModel



def make_arch1_network():
   leftKerasModel = Sequential()
   dense_1 = Dense(200, activation='tanh', input_dim=10000)
   leftKerasModel.add(dense_1)
   print ("d1 output shape")
   print dense_1.output_shape
   #   leftKerasModel.add(Flatten())
   leftKerasModel.add(Reshape((1,200)))
   convolution1d_1 = Convolution1D(10, 3, border_mode='same')
   leftKerasModel.add(convolution1d_1)
   print ("c1d1 output shape")
   print convolution1d_1.output_shape

#   leftKerasModel.add(Activation('relu'))
#   leftKerasModel.add(MaxPooling1D(pool_length=2, stride=None, border_mode='valid'))
#   leftKerasModel.add(Flatten())
   rightKerasModel = Sequential()
#   rightKerasModel.add(Dense(200, activation= 'tanh', input_dim=10000))
#   rightKerasModel.add(Flatten())
#   leftKerasModel.add(Dense((200)))
   rightKerasModel.add(Reshape((1,200)))
   rightKerasModel.add(Convolution1D(10, 3, border_mode='same'))
#   rightKerasModel.add(Activation('relu'))
#   rightKerasModel.add(MaxPooling1D(pool_length=2, stride=None, border_mode='valid'))
#   rightKerasModel.add(Flatten())
   mergedKerasModel = Sequential()
   mergedKerasModel.add(Merge([leftKerasModel,rightKerasModel], mode='cos', dot_axes=1))
   mergedKerasModel.add(Lambda(lambda x: 1-x))
#   mergedKerasModel.add(Flatten())
#   mergedKerasModel.add(Dense(2))
#   mergedKerasModel.add(Activation('softmax'))
   print ("make network input shape")
   mergedKerasModel.summary()
   print (mergedKerasModel.input_shape)
   return mergedKerasModel

def train_arch1g_model(km, leftData, rightData, labels):
#   print ("testData shape during training b4reshaping")
#   print testData.shape
#   testData = numpy.reshape(testData, (127,1,100,100)).astype(theano.config.floatX)
   sgd = SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)
#   print ("testData shape during training after reshaping")
#   print testData.shape
#   print ("model input shape during training")
#   print km.input_shape
   km.compile(loss='custom_objective', optimizer=sgd)
   km.fit([leftData, rightData], labels, nb_epoch=10, batch_size=32)

def train_arch1_model(km, leftData, rightData, labels):
#   print ("testData shape during training b4reshaping")
#   print testData.shape
#   testData = numpy.reshape(testData, (127,1,100,100)).astype(theano.config.floatX)
   sgd = SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)
#   print ("testData shape during training after reshaping")
#   print testData.shape
#   print ("model input shape during training")
#   print km.input_shape
   km.compile(loss='custom_objective', optimizer=sgd)
   km.fit([leftData, rightData], labels, nb_epoch=10, batch_size=32)


def train_model(model, leftData, rightData, labels):

   print('\nleft shape:', leftData.shape)
   print('\nright shape:', rightData.shape)
#   print('\nlabels type:', labels)
#   print('\nlabels shape:', labels.shape)
   sgd = SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)
   model.compile(loss='custom_objective', optimizer=sgd)
   model.fit([leftData, rightData], labels, nb_epoch=10, batch_size=32)

#get error: Error when checking model input: expected convolution2d_input_2 to have 4 dimensions, but got array with shape (127, 100, 100)


#   print('Testing...')
#   res = model.evaluate(X_test, Y_test,
#                        batch_size=32, verbose=1, show_accuracy=True)
#   print('Test accuracy: {0}'.format(res[1]))

def save_model(model):

   model_json = model.to_json()
   open('trainKeras', 'w').write(model_json)
   model.save_weights('trainKeras_weights.h5', overwrite=True)



if __name__ == '__main__':
#   bugcheck  = codecs.open('bug.csv', 'w', 'utf-8')
   w2vModel= Word2Vec.load('/home/ubuntu/model')
   xml = etree.parse("/home/ubuntu/qa/mylab/input/questions/qa-sampleDocRetrievedBySect.xml")
   questions = xml.findall(".//question")
   labels = numpy.array([])
   q3dInit = False
   a3dInit = False
   qFile = open('qFile', 'w')
   aFile = open('aFile', 'w')
   lFile = open('lFile', 'w')


   zeroFilledVector = numpy.array([])
   for i in range (0, 100):
                    zeroFilledVector = numpy.append(zeroFilledVector,0)
   for question in questions:
        questionText = question.find(".//text").text
#        qCounter = 0

#        qMatrixInit = False
        qSkip = False
        answers = map(lambda a: a.text, question.findall(".//answer"))
        answers = filter(partial(is_not, None), answers)
        for doc in question.findall(".//doc"):

            for sent in doc.findall(".//stext"):
                aMatrixInit = False
                qMatrixInit = False
                qCounter = 0
                aCounter = 0
                answerFoundFlag = False
                normalizedSentence = myNormalize(sent.text.strip())
                sentenceWoSc = rmvSpecChar(normalizedSentence)
#                print ("loop adv")
#max question length is 33. Loop thru each word. If lenght less than 36, add all-zeroes vectors to the result matrix
                for word in myNormalize(questionText.strip()):
                    qCounter = qCounter + 1

#                    wvLength = len(w2vModel[word])
#                    print("w2vModel type %s/n"%w2vModel[word])
#                    print('qMatrix shape: %s/n', qMatrix.shape)
#                    print('word vector shape: %s/n', numpy.array([w2vModel[word]]).shape)
#                    print('concated 1 time qMatrix shae: %s/n', numpy.concatenate((qMatrix, numpy.array([w2vModel[word]]))))
#                    print('zero vector shape: %s/n', numpy.array([zeroFilledVector]).shape)
                    try:
                        if qMatrixInit == False:
                            qMatrix = numpy.array([w2vModel[word]])
                            qMatrixInit = True
                        else:
#                        qMatrix = numpy.concatenate((qMatrix, numpy.array([w2vModel[word]])), axis=0)
                            qMatrix = numpy.concatenate((qMatrix, numpy.array([w2vModel[word]])), axis=0)
                    except KeyError:
                        qSkip = True
                        qMatrix = numpy.concatenate((qMatrix,numpy.array([zeroFilledVector])), axis= 0)

                for i in range (qCounter, 100):
                    qCounter = qCounter + 1
                    qMatrix = numpy.concatenate((qMatrix, numpy.array([zeroFilledVector])), axis=0)
#                    print('early qMatrix shape:', numpy.array([qMatrix]).shape)
                aSkip = False
                for word in sentenceWoSc[:36]:
#                    print ("normalizedSentence %s\n"%("/".join(normalizedSentence)))
#                    print ("word in normalizedSentence %s\n"%(word))
                    aCounter = aCounter + 1
#                    print("acounter in 1st loop %s/n"%aCounter)
#                    wvLength = len(w2vModel[word])
                    if qSkip: aSkip = True
                    try:
                        if aMatrixInit == False:
                            aMatrix = numpy.array([w2vModel[word]])
                            aMatrixInit = True
                        else:
                            aMatrix = numpy.concatenate((aMatrix, numpy.array([w2vModel[word]])), axis=0)
                    except KeyError:
                        aSkip = True
                        aMatrix = numpy.concatenate((aMatrix,[zeroFilledVector]), axis=0)
                for i in range (aCounter, 100):
                    aCounter = aCounter + 1
#                    print("acounter in 2nd loop %s/n"%aCounter)
                    aMatrix = numpy.concatenate((aMatrix, numpy.array([zeroFilledVector])), axis=0)

                for answer in map(lambda a: myNormalize(a), answers):
                    joinedAnswer = "".join(answer).strip()
                    joinedSentence = "".join(sentenceWoSc).strip()
                    if joinedAnswer in joinedSentence:
                        answerFoundFlag = True
                if aSkip==False:
                    if q3dInit == True:
#                        bugcheck.write("q3dArray shape:%s\n" % (q3dArray.shape))

                        q3dArray = numpy.concatenate((q3dArray,numpy.array([qMatrix])), axis=0)
                        print('\nq3dArray shape:', q3dArray.shape)
                        print('\nqMatrix shape:', numpy.array([qMatrix]).shape)
                    else:
                        q3dArray = numpy.array([qMatrix])
#                        bugcheck.write("q3dArray shape:%s\n" % (q3dArray.shape))
                        print('\nnot init q3dArray shape:', q3dArray.shape)
                        q3dInit = True
                    if a3dInit == True:
#                        bugcheck.write("a3dArray shape:%s\n" % (a3dArray.shape))

                        a3dArray = numpy.concatenate((a3dArray,numpy.array([aMatrix])), axis=0)
                        print('\na3dArray shape:', a3dArray.shape)
                        print('\naMatrix shape:', numpy.array([aMatrix]).shape)
                    else:
                        a3dArray = numpy.array([aMatrix])
#                        bugcheck.write("a3dArray shape:%s\n" % (a3dArray.shape))
                        print('\nnot init a3dArray shape:', a3dArray.shape)
                        a3dInit = True

                    if answerFoundFlag:
                        labels = numpy.append(labels,1)
#                        bugcheck.write("labels shape %s\n" % (labels.shape))
                    else:
                        labels = numpy.append(labels,0)
#                        bugcheck.write("labels shape %s\n" % (labels.shape))
                    print('\nlabels shape:', labels.shape)





   numpy.save(qFile,q3dArray)
   numpy.save(aFile,a3dArray)
   numpy.save(lFile,labels)
   labels = np_utils.to_categorical(labels, 2)

#   km = make_arch1g_network()
#   km = make_network()
#   train_test_model(km,q3dArray, a3dArray, labels)
#   test2dLArray = numpy.random.random((127, 10000))
#   test2dRArray = numpy.random.random((127, 10000))
#   testLabels = numpy.random.randint(2, size=127)
#   testLabels = np_utils.to_categorical(testLabels, 2)
#   q2dArray = numpy.reshape(q3dArray, (127,10000))
#   a2dArray = numpy.reshape(a3dArray, (127,10000))
#   train_arch1g_model(km,q3dArray, a3dArray, labels)
#   save_model(km)
   qFile.close()
   aFile.close()
   lFile.close()
   test3dLArray = numpy.random.random((1, 10,100))

   test3dRArray = numpy.random.random((1, 10,100))

   testLabels = numpy.random.randint(2, size=1)

   testLabels = np_utils.to_categorical(testLabels, 2)


   leftKerasModel = Sequential()

   leftKerasModel.add(Reshape((1000,), input_shape=(10,100)))

   leftKerasModel.add(Dense(200))


   rightKerasModel = Sequential()

   rightKerasModel.add(Reshape((1000,), input_shape=(10,100)))

   rightKerasModel.add(Dense(200))


   mergeLayer = Sequential()

   mergeLayer.add(Merge([leftKerasModel, rightKerasModel], mode='cos', dot_axes=1))
   mergeLayer.compile(loss='mse', optimizer='sgd')
   mergeLayer.fit([test3dLArray, test3dRArray], testLabels, nb_epoch=10, batch_size=32)
   mergeLayer.compile(loss='mse', optimizer='sgd')

   result = mergeLayer.predict([test3dLArray, test3dRArray], verbose=1)



#   bugcheck.close()
#   os.system('sudo shutdown now -P')



