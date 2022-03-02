#from pickle import FALSE
from stanfordcorenlp import StanfordCoreNLP
import json, string
import os
import string
from time import time

#Para correr este codigo, deben correrse las siguientes lineas:
#cd /Users/elisulvaran/Desktop/stanford-corenlp-full-2018-10-05 
#java -mx4g -cp "*" edu.stanford.nlp.pipeline.StanfordCoreNLPServer -annotators "tokenize,ssplit,pos,lemma,parse,sentiment" -port 9000 -timeout 500000


def lemmatize_corenlp(conn_nlp, sentence):
    props = {
        'annotators': 'pos,lemma',
        'pipelineLanguage': 'en',
        'outputFormat': 'json'
    }

    #Se tokeniza en palabras
    sents = conn_nlp.word_tokenize(sentence)
    #Se eliminan signos de puntuacion
    sents_no_punct = [s for s in sents if s not in string.punctuation]
    #Se crea la oracion nuevamente
    sentence2 = " ".join(sents_no_punct)
    #Se anota para obtener los lemmas
    parsed_str = conn_nlp.annotate(sentence2, properties=props)
    parsed_dict = json.loads(parsed_str)
    #Se extraen los lemmas de cada palabra
    lemma_list = [v for d in parsed_dict['sentences'][0]['tokens'] for k,v in d.items() if k == 'lemma']
    #Se forma la oracion y se devuelve
    return(" ".join(lemma_list))


def corenlp_annotate(myFile):

	# Tomar el tiempo:
	t0 = time()

	#Se realiza la conexion y se llama al lematizador de corenlp
	print("\n")
	print("Connecting with StanfordCoreNLP...")
	nlp = StanfordCoreNLP('http://localhost', port=9000, timeout=10000, quiet=True)
	print("Done!\n")


	print("Reading file and lemmatizing...")
	lemmas = []      #Se crea una lista donde se guardaran los lemmas
	with open(myFile, mode='r') as iFile:      #Se abre el archivo de abstracts
		for line in iFile:     #Se lee cada linea del archivo 
			line = line.strip("\r\n")     #Se eliminan los saltos de linea y retornos de carro
			line = line.split("\t")    #Se divide cada linea por tabulador
			line = line[2]    #Se selecciona la posicion 2 de cada linea, que corresponde a la posicion de los abstracts
			ln = line.split(" ")     #Se separan las palabras
			table = str.maketrans('', '', string.punctuation)     #Para eliminar los signos de puntuacion
			abstract = [l.translate(table) for l in ln]     #Se eliminan los signos de puntuacion
			abstract = " ".join(abstract)     #Se añade un espacio donde se encontraban los signos de puntuacion
			lemma = lemmatize_corenlp(conn_nlp = nlp, sentence = abstract)     #Se llama al lematizador por cada linea del archivo, es decir, por cada abstract
			lemmas.append(lemma)      #Se almacena el abstract lematizado en la lista de lemmas
	print("Done!\n")


	print("Creating output file...")
	final_file = []      #Se crea una lista donde se guardara el archivo final a escribir
	with open(myFile, mode='r') as iFile:      #Se abre nuevamente el archivo de abstracts para crear el nuevo
		cont = 0     #Se inicializa un contador que indicara las posiciones de los abstracts lematizados en la lista de lemmas	
		for line in iFile:     #Se lee cada linea del archivo 
			line = line.strip("\r\n")     #Se eliminan los saltos de linea y retornos de carro
			line = line.split("\t")    #Se divide cada linea por tabulador
			line[2] = lemmas[cont]      #Se coloca el abstract lematizado en la posicion 2 de la linea
			print(lemmas[cont])
			final_file.append(line)      #Se fguarda la linea en el archivo final
			cont += 1     #Se aumenta el contador
	print("Done!\n")



	print("Saving output file...")
	with open(os.path.splitext(myFile)[0]+"_lemmatized.txt", mode="w") as oFile:  #Se abre el archivo de salida 
		for i in final_file:
			for j in i:
				oFile.write("{}\t".format(j))     #Se escribe el nuevo archivo por lineas
			oFile.write("\n")
	#p.terminate()
	return(time()-t0)

















