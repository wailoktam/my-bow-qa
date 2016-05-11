__author__ = 'wailoktam'
from keras.models import Sequential
from keras.datasets import cifar10
from keras.utils import np_utils
from keras.models import model_from_json
from keras.layers.core import Dense, Dropout, Activation, Flatten
from keras.layers.convolutional import Convolution2D, MaxPooling2D
from keras.optimizers import SGD
import numpy as np
import scipy.misc
import theano
import theano.tensor as T

def compile_cos_sim_theano():
    v1 = theano.tensor.vector(dtype=theano.config.floatX)
    v2 = theano.tensor.vector(dtype=theano.config.floatX)
    numerator = theano.tensor.sum(v1*v2)
    denominator = theano.tensor.sqrt(theano.tensor.sum(v1**2)*theano.tensor.sum(v2**2))
    return theano.function([v1, v2], numerator/denominator)

cos_sim_theano_fn = compile_cos_sim_theano()


epsilon = 1.0e-9
def custom_objective(y_true, y_pred):
    '''Just another crossentropy'''
    y_pred = T.clip(y_pred, epsilon, 1.0 - epsilon)
    y_pred /= y_pred.sum(axis=-1, keepdims=True)
    cce = T.nnet.categorical_crossentropy(y_pred, y_true)
    return cce


img_channels = 3
img_rows = 32
img_cols = 32

def load_and_scale_imgs():
   img_names = ['standing-cat.jpg', 'dog-face.jpg']

   imgs = [np.transpose(scipy.misc.imresize(scipy.misc.imread(img_name), (32, 32)),
                        (2, 0, 1)).astype('float32')
           for img_name in img_names]
   return np.array(imgs) / 255

nb_classes = 10

def load_dataset():
   # the data, shuffled and split between train and test sets
   (X_train, y_train), (X_test, y_test) = cifar10.load_data()
   xFile = open('xFile', 'w')
   yFile = open('yFile', 'w')
   print(X_train.shape, 'train data when load')
   print(y_train.shape, 'labels when load')
   np.save(xFile, X_train)
   np.save(yFile, y_train)
   # convert class vectors to binary class matrices
   Y_train = np_utils.to_categorical(y_train, nb_classes)
   Y_test = np_utils.to_categorical(y_test, nb_classes)
   X_train = X_train.astype('float32')
   X_test = X_test.astype('float32')
   X_train /= 255
   X_test /= 255
   xFile.close()
   yFile.close()
   return X_train, Y_train, X_test, Y_test

def make_network():
   model = Sequential()

   model.add(Convolution2D(32, 3, 3, border_mode='same',
                           input_shape=(img_channels, img_rows, img_cols)))
   model.add(Activation('relu'))
   model.add(Convolution2D(32, 3, 3))
   model.add(Activation('relu'))
   model.add(MaxPooling2D(pool_size=(2, 2)))
   model.add(Dropout(0.25))

   model.add(Convolution2D(64, 3, 3, border_mode='same'))
   model.add(Activation('relu'))
   model.add(Convolution2D(64, 3, 3))
   model.add(Activation('relu'))
   model.add(MaxPooling2D(pool_size=(2, 2)))
   model.add(Dropout(0.25))

   model.add(Flatten())
   model.add(Dense(512))
   model.add(Activation('relu'))
   model.add(Dropout(0.5))
   model.add(Dense(nb_classes))
   model.add(Activation('softmax'))
   print model.input_shape

   return model

def train_model(model, X_train, Y_train, X_test, Y_test):

   sgd = SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)
   model.compile(loss='categorical_crossentropy', optimizer=sgd)
   print('X_train in train model:',X_train.shape)
   model.fit(X_train, Y_train, nb_epoch=5, batch_size=32,
             validation_split=0.1, show_accuracy=True, verbose=1)


#   res = model.evaluate(X_test, Y_test,
#                        batch_size=32, verbose=1, show_accuracy=True)
#   print('Test accuracy: {0}'.format(res[1]))

def save_model(model):

   model_json = model.to_json()
   open('cifar10_architecture.json', 'w').write(model_json)
   model.save_weights('cifar10_weights.h5', overwrite=True)

def load_model(model_def_fname, model_weight_fname):
   model = model_from_json(open(model_def_fname).read())
   model.load_weights(model_weight_fname)
   return model

def load_and_scale_imgs():
   img_names = ['standing-cat.jpg', 'dog-face.jpg']

   imgs = [np.transpose(scipy.misc.imresize(scipy.misc.imread(img_name), (32, 32)),
                        (2, 0, 1)).astype('float32')
           for img_name in img_names]
   return np.array(imgs) / 255

if __name__ == '__main__':
   train_model(make_network(),load_dataset()[0],load_dataset()[1],load_dataset()[2],load_dataset()[3])
#   imgs = load_and_scale_imgs()
#   model = load_model('cifar10_architecture.json', 'cifar10_weights.h5')
#   predictions = model.predict_classes(imgs)
#   print(predictions)


