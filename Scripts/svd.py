from sklearn.decomposition import TruncatedSVD
import argparse
from scipy.sparse import csr_matrix

if __name__ == '__main__':

    parser=argparse.ArgumentParser(description= 'This script reduces the TF-IDF matrix through SVD descompostion.')

    parser.add_argument('-m', '--matrix', dest='file_matrix', required=True, type=str,
        help='Vectorization matrix file path (tsv).')
    parser.add_argument('-c', '--class', dest='file_classes', required=True, type=str,
        help='List of corresponding matrix vectorization classes.')
    parser.add_argument('-o', '--output', required=True, type=str,
        help='Output matrix reduced.')

    args=parser.parse_args()

    print("\nReading input files...")
    X=[]
    with open((args.file_matrix), mode='r') as file:
        for line in file:
            X.append(line.strip('\n\r').split("\t"))

    nc = len(X) #number of components
    X = csr_matrix(X, dtype='double')
    y = []
    with open(args.file_classes, 'r') as file:
        for line in file.readlines():
            y.append(line.strip("\n[]'").split(','))
    print("Done!\n")

    print(f"Components: {nc}")
    print(X.get_shape())

    print("Performing SVD reduction...")
    svd = TruncatedSVD(n_components=nc, random_state=42)
    X_reduced = svd.fit_transform(X, y)
    exp_var = svd.explained_variance_ratio_.cumsum()
    params = svd.get_params()

    print(exp_var)
    print(params)

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