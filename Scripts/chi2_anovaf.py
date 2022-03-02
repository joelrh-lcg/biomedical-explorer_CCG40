#import argparse
from sklearn.feature_selection import SelectPercentile,chi2, f_classif
from scipy.sparse import csr_matrix
import itertools
import time 
import sys
import datetime
from pathlib import Path
import os

#class Logger(object):
#    "this class is for print the output of the script to both stdout and log file"
#    def __init__(self):
#        self.terminal = sys.stdout
#        self.log = open(os.path.join(os.path.join(Path(output).parent.absolute(), 'chi2_log.txt')), "a")
#
#    def write(self, message):
#        self.terminal.write(message)
#        self.log.write(message)
#
#    def flush(self):
#        #this flush method is needed for python 3 compatibility.
#        #this handles the flush command by doing nothing.
#        #you might want to specify some extra behavior here.
#        pass

def selectf(percentile, file_matrix, file_classes, output, test):

    start_time = time.time()

    #parser=argparse.ArgumentParser(description=
    #    'This script reduces the TF-IDF matrix through the selection of the best percentil based on chi2 test.')
#
    #parser.add_argument('-p', '--percentile',
    #    required=False, type=int, default=50, 
    #    help='Percentile selection.')
    #parser.add_argument('-m', '--matrix', dest='file_matrix', required=True, type=str,
    #    help='Vectorization matrix file path (tsv).')
    #parser.add_argument('-c', '--class', dest='file_classes', required=True, type=str,
    #    help='List of corresponding matrix vectorization classes.')
    #parser.add_argument('-o', '--output', required=True, type=str,
    #    help='Output matrix reduced.')
#
    #args=parser.parse_args()
#
    #sys.stdout = Logger()

    print("\n\n**********************************************************************\n")

    print("Run started at: ", datetime.datetime.now(), '\n')

    print("Saving results in file: ", output)

    print("\nReading input files...")
    X=[]
    with open((file_matrix), mode='r') as file:
        for line in file:
            X.append(line.strip('\n\r').split("\t"))

    nc = len(X) #number of components
    X = csr_matrix(X, dtype='double')
    y = []
    with open(file_classes, 'r') as file:
        for line in file.readlines():
            y.append(line.strip("\n[]'").split(','))
    y = list(itertools.chain.from_iterable(y))
    print("Done!\n")

    print(f"Components: {nc}, {X.get_shape()}")
    y_lSelect = []

    print("Extracting the best percentile: ", percentile)

    if(test == "CHI2"):
        print("Performing CHI2 feature selection...")
        selector = SelectPercentile(chi2, percentile=percentile)
    elif(test == "ANOVA-F"):
        print("Performing ANOVA-F feature selection...")
        selector = SelectPercentile(f_classif, percentile=percentile)
    
    X_reduced = selector.fit_transform(X, list(itertools.repeat(0, nc)))
    for i in selector.get_support(True): y_lSelect.append(y[i])
    print(f'Number of features after feature selection: {X_reduced.shape}')

    print(f"TF-IDF matrix reduced to dimensions: {X_reduced.shape}")

    with open(output, 'w') as file:
        for i in X_reduced.toarray().tolist():
            for j in i:
                file.write("{}\t".format(j))
            file.write("\n") 

    print("Total time: %s seconds." % (time.time() - start_time))  
    print("\n**********************************************************************\n")

    return [str(time.time() - start_time)]