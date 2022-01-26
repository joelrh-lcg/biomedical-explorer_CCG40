from http.client import ImproperConnectionState
from sklearn.decomposition import TruncatedSVD
import argparse
from scipy.sparse import csr_matrix
import sys
import os
from pathlib import Path
import datetime
import time

class Logger(object):
    "this class is for print the output of the script to both stdout and log file"
    def __init__(self):
        self.terminal = sys.stdout
        self.log = open(os.path.join(Path(args.output).parent.absolute(), 'svd_log.txt'), "w")

    def write(self, message):
        self.terminal.write(message)
        self.log.write(message)

    def flush(self):
        #this flush method is needed for python 3 compatibility.
        #this handles the flush command by doing nothing.
        #you might want to specify some extra behavior here.
        pass

if __name__ == '__main__':

    start_time = time.time()

    parser=argparse.ArgumentParser(description= 'This script reduces the TF-IDF matrix through SVD descompostion.')

    parser.add_argument('-m', '--matrix', dest='file_matrix', required=True, type=str,
        help='Vectorization matrix file path (tsv).')
    parser.add_argument('-o', '--output', required=True, type=str,
        help='Output matrix reduced.')

    args=parser.parse_args()

    sys.stdout = Logger()

    print("\n\n**********************************************************************\n")
    print("Run started at: ", datetime.datetime.now(), '\n')

    print("Saving results in file: ", args.output)

    print("\nReading input files...")
    X=[]
    with open((args.file_matrix), mode='r') as file:
        for line in file:
            X.append(line.strip('\n\r').split("\t"))

    nc = len(X) #number of components
    X = csr_matrix(X, dtype='double')
    y = []
    print("Done!\n")

    print(f"Components: {nc}")
    print(X.get_shape())

    print("Performing SVD reduction...")
    svd = TruncatedSVD(n_components=nc, random_state=42, n_iter=10)
    X_reduced = svd.fit_transform(X, y)
    exp_var = svd.explained_variance_ratio_.cumsum()
    params = svd.get_params()

    cont = 1
    for i in exp_var:
        if(i >= 0.7):
            break
        cont += 1
    X_reduced = X_reduced[0:nc,0:cont]

    print("Done!\n")

    print(f"TF-IDF matrix reduced to dimensions: {X_reduced.shape}")

    with open(args.output, 'w') as file:
        for i in X_reduced:
            for j in i:
                file.write("{}\t".format(j))
            file.write("\n")

print("Total time: %s seconds." % (time.time() - start_time))
print("\n**********************************************************************\n")