import argparse
import os
from sklearn.decomposition import PCA
from sklearn.decomposition import TruncatedSVD
from sklearn.cluster import KMeans
from sklearn.manifold import TSNE
from scipy.sparse import csr_matrix


#Se llama al Parser
parser=argparse.ArgumentParser\
    (description='Este script genera la matriz tf-idf')


#Se agregan los argumentos para correr el codigo. Estos son, la ruta donde se encuentran los archivos y la ruta de salida.   
parser.add_argument('--Abstract', dest='Abstract', required=True, help='Ingrese la ruta del archivo con los abstracts.')
parser.add_argument('--outputpath', dest='outputpath', required=True, help='Ingrese la ruta del archivo de salida junto con su nombre y extension.')
parser.add_argument('--k', dest='k', required=True, help='Ingrese la k.')
parser.add_argument('--reduction', dest='reduction', required=True, help='Ingrese el metodo de reduccion de dimensionalidad.', choices=('PCA', 'SVD'), default=None)
parser.add_argument('--perplexity', dest='perplexity', required=True, help='Ingrese la perplejidad.')
args=parser.parse_args()
#Example: 
#python3 /Users/elisulvaran/Desktop/LCG/Laboratorios/Carlos_Mendez/article-classification/Clustering/Clustering.py --Abstract /Users/elisulvaran/Desktop/LCG/Laboratorios/Carlos_Mendez/article-classification/Clustering/Abstracts_vect.txt --outputpath /Users/elisulvaran/Desktop/LCG/Laboratorios/Carlos_Mendez/article-classification/Clustering/Resultados --outputfile a.png --odatafile data.txt --k 10 --perplexity 30


filename = "k"+args.k+"_"+args.reduction+"_perp"+args.perplexity+".txt"


print("\nReading input files...")
X=[]
with open((args.Abstract), mode='r') as file:
	for line in file:
		X.append(line.strip('\n\r').split("\t"))

nc = len(X) #number of components
X = csr_matrix(X, dtype='double')
print("Done!\n")


print("Performing {} dimensionality reduction...".format(args.reduction))
if args.reduction == 'PCA':
	pca = PCA(n_components=nc, random_state=42)
	X_reduced = pca.fit_transform(X.toarray())
	exp_var = pca.explained_variance_ratio_.cumsum()

elif args.reduction == 'SVD':
	svd = TruncatedSVD(n_components=nc, random_state=42)
	X_reduced = svd.fit_transform(X)
	exp_var = svd.explained_variance_ratio_.cumsum()

cont = 1
for i in exp_var:
	if(i >= 0.7):
		break
	cont += 1
X_reduced = X_reduced[0:nc,0:cont]

print("Done!\n")


#Clusterizacion
print("Perfoming k-means clusterization...")
kmeans = KMeans(n_clusters=int(args.k), random_state=42, n_jobs=-1)
y_pred = kmeans.fit_predict(X_reduced)
print("Done!\n")


#t-SNE
print("Performing t-SNE dimensionality reduction...")
tsne = TSNE(verbose=1, perplexity=int(args.perplexity), random_state=42)
X_embedded = tsne.fit_transform(X.toarray())
print("Done!\n")


#Archivo de salida
print("Saving output data file...")
with open(os.path.join(args.outputpath, filename), mode='w', encoding='utf8') as oFile:
	oFile.write("dim1\tdim2\tCluster\n")
	for i in range(len(y_pred)):
		oFile.write("{}\t{}\t{}\n".format(X_embedded[i,0], X_embedded[i,1], y_pred[i]))

print("Done! Shutting down.\n")
































