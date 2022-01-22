import argparse
import os
from sklearn.feature_extraction.text import TfidfVectorizer
from time import time
from nltk.corpus import stopwords
import string


# Tomar el tiempo:
t0 = time()

# correr el programa estando parado en la carpeta de article clasification: 
# python Scripts/MultilabelBinarizer_vectorizacion.py --TF Data-sets/division/TFs.txt --Abstract Data-sets/division/Abstracts.txt --outputpath Data-sets/division/ 

#Se llama al Parser
parser=argparse.ArgumentParser\
    (description='Este script genera la matriz tf-idf')
 
#Se agregan los argumentos para correr el codigo. Estos son, la ruta donde se encuentran los archivos y la ruta de salida.   
parser.add_argument('--Abstract', dest='Abstract', required=True, help='Ingrese la ruta del archivo con los abstracts.')
parser.add_argument('--outputpath', dest='outputPath', required=True, help='Ingrese la ruta del archivo de salida junto con su nombre y extension.')
args=parser.parse_args()

#crea la lista de los objetos necesarios y los almacena en dic. 
print("\nReading input file...")
dic_abs=[]
with open((args.Abstract), mode='r') as file:
	for line in file:
		line = line.strip("\n\r")
		#print(line)
		line = line.split("\t")[2]
		#print(line)
		dic_abs.append(line)
print("Done!\n")


print("Vectorizing input file...")
pf = stopwords.words('english')
vectorizer = TfidfVectorizer(stop_words=pf)
X = vectorizer.fit_transform(dic_abs).toarray().tolist()
names = vectorizer.get_feature_names()
print("Done!\n")


print("Writing output files...")
with open(os.path.join(args.outputPath,"clases_Abstracts.txt"), mode="w") as oFile:
	oFile.write(str(names))
	oFile.write('\n')

with open(os.path.join(args.outputPath,"Abstracts_vect.txt"), mode="w") as oFile:
	for i in X:
		for j in i:
			oFile.write("{}\t".format(j))
		oFile.write("\n")

print("Done! Shutting down...\n")








