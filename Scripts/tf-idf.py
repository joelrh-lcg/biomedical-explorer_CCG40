import os
from sklearn.feature_extraction.text import TfidfVectorizer
from time import time
from nltk.corpus import stopwords

def tfidf(Abstract, outputPath):

	# Tomar el tiempo:
	t0 = time()

	print("\nReading input file...")
	dic_abs=[]
	with open(Abstract, mode='r') as file:
		for line in file:
			line = line.strip("\n\r")
			#print(line)
			line = line.split("\t")[2]
			#print(line)
			dic_abs.append(line)
	print("Done!\n")
	print(f"{len(dic_abs)} abstracts read!")


	print("Vectorizing input file...")
	pf = stopwords.words('english')
	vectorizer = TfidfVectorizer(stop_words=pf)
	X = vectorizer.fit_transform(dic_abs).toarray().tolist()
	names = vectorizer.get_feature_names()
	print("Found features:", len(str(names)))
	print("Done!\n")


	print("Writing output files...")
	with open(os.path.join(outputPath,"clases_Abstracts.txt"), mode="w") as oFile:
		oFile.write(str(names))
		oFile.write('\n')

	with open(os.path.join(outputPath,"Abstracts_vect.tsv"), mode="w") as oFile:
		for i in X:
			for j in i:
				oFile.write("{}\t".format(j))
			oFile.write("\n")

	print("Done! Shutting down...\n")
