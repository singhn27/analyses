#!usr/bin/env python

# SMS Spam Detector with Optimized Support Vector Machine Model
# Code adapted from http://radimrehurek.com/data_science_python/
# Data provided by  https://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection
# Input:            Labeled Training Data (SMS texts labeled as spam or 'ham' i.e. not spam)
# Output:           Support Vector Machine model trained on dataset, as .pkl file

# Libraries #

import cPickle
import csv
import pandas
import sklearn

from sklearn.cross_validation        import StratifiedKFold, train_test_split 
from sklearn.feature_extraction.text import CountVectorizer, TfidfTransformer
from sklearn.grid_search             import GridSearchCV
from sklearn.pipeline                import Pipeline
from sklearn.svm                     import SVC
from textblob                        import TextBlob

# Functions #

def lemmatize(message):
    message = unicode(message, 'utf8').lower()
    words   = TextBlob(message).words
    return    [word.lemma for word in words]

# Analysis #

messages = [line.rstrip() for line in open('./data/SMSSpamCollection')]

messages = pandas.read_csv( './data/SMSSpamCollection'
                          , sep     = '\t'
                          , quoting = csv.QUOTE_NONE
                          , names   = ["label", "message"]
                          )

msg_train, msg_test,        \
    label_train, label_test = train_test_split( messages['message']
                                              , messages['label']
                                              , test_size = 0.2
                                              )
'''
from sklearn.naive_bayes import MultinomialNB
pipeline_nb = Pipeline([('bow', CountVectorizer(analyzer = lemmatize))          # strings        -> token integer counts
                      , ('tfidf', TfidfTransformer())                           # integer counts -> weighted TF-IDF scores
                      , ('classifier', MultinomialNB())                         # train on TF-IDF vectors w/ Naive Bayes
                      ])
'''
pipeline_svm = Pipeline([('bow', CountVectorizer(analyzer = lemmatize))
                       , ('tfidf', TfidfTransformer())
                       , ('classifier', SVC())
                       ])

param_svm = [ {'classifier__C'     : [1, 10]
            , 'classifier__kernel' : ['linear']}
            ]

grid_svm = GridSearchCV( pipeline_svm                                           # SVM pipeline
                       , param_grid = param_svm                                 # parameters to tune via cross validation
                       , refit      = True                                      # fit using all data on best classifier
                       , n_jobs     = -1                                        # number of cores to use for parallelization
                       , scoring    = 'accuracy'                                # score on which to optimize
                       , cv         = StratifiedKFold(label_train, n_folds = 5) # type of cross validation
                       )

if __name__ == '__main__':
    detector_svm = grid_svm.fit(msg_train, label_train)
    with open('sms_spam_detector.pkl', 'wb') as fout:
        cPickle.dump(detector_svm, fout)