import argparse
import os
from time import time
from nltk.corpus import stopwords
import gensim.corpora as corpora
import gensim
from gensim import models
from gensim.utils import simple_preprocess
from gensim.models import CoherenceModel, TfidfModel
from gensim.parsing.preprocessing import preprocess_string, strip_punctuation, strip_numeric 
import collections

# ejemplo para correr el codigo 
# cd Documentos/Genomicas/Laboratorios/Carlos_Mendez/biomedical-literature-explorer/
# python Scripts/LDA.py --abstracts Data/abs_unicos_lemmatized.txt --clusters Results/Dimensionality_reduction/PCA_perp30.txt --outputpath Results/LDA/


# In the next section are all the parameters that we will be needing to run this script. 
t0 = time()
stop_words = stopwords.words('english')

parser=argparse.ArgumentParser\
    (description='Este script es para el metodo de Latent Dirichlet Allocation')
parser.add_argument('--abstracts', dest='abs', required=True, help='Ingrese la ruta del archivo con todos los abstracts')
parser.add_argument('--clusters', dest='Clusters', required=True, help='Ingrese la ruta de los archivos del clustering')
parser.add_argument('--outputpath', dest='outputpath', required=True, help='Ingrese la ruta del archivo de salida junto con su nombre y extension.')
args=parser.parse_args()

# We will create some functions to make make the code more efficient. 

# This function calculates the coherence value
def compute_coherence_values(dictionary, corpus, texts, limit, start = 2, step = 3):
    coherence_values = []
    model_list = []
    for num_topics in range(start, limit, step):
        model = gensim.models.wrappers.LdaMallet(mallet_path, corpus = corpus, num_topics = num_topics, id2word = id2word)
        model_list.append(model)
        coherencemodel = CoherenceModel(model = model, texts = texts, dictionary = dictionary, coherence = 'c_v')
        coherence_values.append(coherencemodel.get_coherence())
    return model_list, coherence_values

### CODE START ###

# First we will create a list with all the abstracts and a diccionary with all the words. 
documents = []
wordcount = {}
with open(args.abs, encoding='utf8', mode='r') as abstracts_tot:
    for line in abstracts_tot:
        ln=line.split("\t")
        documents.append(ln[2])
        for word in ln[2].split():
            if word not in stop_words:
                if word not in wordcount:
                    wordcount[word] = 1
                else:
                    wordcount[word] +=1

# Removing the words that are at least in half of the abstracts
for word in wordcount:
    if wordcount[word] > round(len(documents)/2):
        stop_words.append(word)

# We will notify the user that the model is already starting. 
print("\nPERFORMING LATENT DIRICHLET ALLOCATION MODEL")
 
# Since we have multiple clusters, we will make a list with the corresponding cluster each abstract has. 
clusters = []
with open(args.Clusters, encoding='utf8', mode='r') as clusters_tot:
    cluster_tot = iter(clusters_tot)
    next(clusters_tot)
    for line in clusters_tot:
        ln=line.split()
        clusters.append(int(ln[2]))
    k = max(clusters)+1 # this variable will determine the number the clusters that are in our data, so it will tell us how many models we need to do.

# Now we need to create a dictionary were each cluster has their corresponding abstracts. 
abstracts = {}
for i in range(k):
    abstracts_cl = []
    for j in range(len(documents)):   
        if clusters[j] == i:
            abstracts_cl.append(documents[j])
    abstracts[i] = abstracts_cl


# During the performance of the model I will create a dictionary with the results from each cluster. 
results = {}
for i in range(k):
    print("Processing cluster {} de {}".format(i+1, k))
    result = []
    bigram = gensim.models.Phrases(abstracts[i], min_count = 5, threshold = 100) 
    bigram_mod = gensim.models.phrases.Phraser(bigram) 
    data_words_nostops = [[word for word in simple_preprocess(str(doc)) if word not in stop_words] for doc in abstracts[i]]
    data_words_bigrams = [bigram_mod[doc] for doc in data_words_nostops]
    id2word = gensim.corpora.Dictionary(data_words_bigrams)
    texts = data_words_bigrams
    corpus = [id2word.doc2bow(text) for text in texts] 
    tfidf = models.TfidfModel(corpus)
    lda_model = gensim.models.LdaMulticore(tfidf[corpus], num_topics=1, id2word=id2word, passes=10, workers=4)
    lda_topics = lda_model.show_topics(num_words=15)
    filters = [lambda x: x.lower(), strip_punctuation, strip_numeric] 
    result.append(lda_topics)
    result.append(lda_model.log_perplexity(corpus))
    coherence_model_lda = CoherenceModel(model=lda_model, texts=data_words_bigrams, dictionary=id2word, coherence='c_v')
    result.append(coherence_model_lda.get_coherence())
    results[i] = result     
name = args.Clusters
name = name.split("/")
filename = "LDA_" + name[len(name)-1]

with open(os.path.join(args.outputpath, filename), mode='w', encoding='utf8') as oFile:
    for j in range(len(documents)):  
        for i in range(k):
            if clusters[j] == i:
                topics = results[i][0]
                topics = str(topics)[1:-1]
                topics = str(topics)[1:-1]
                topics = topics.replace('0',"")
                topics = topics.replace("+", "\t")
                topics = topics.replace('.1*', "")
                topics = topics.replace('.2*', "")
                topics = topics.replace('.3*', "")
                topics = topics.replace('.4*', "")
                topics = topics.replace('.5*', "")
                topics = topics.replace('.6*', "")
                topics = topics.replace('.7*', "")
                topics = topics.replace('.8*', "")
                topics = topics.replace('.9*', "")
                topics = topics.replace('"', "")
                topics = topics.replace(',', "")
                topics = topics.replace("'", "")
                oFile.write("{}\n" .format(topics))



oFile.close()
print("\nTime used for all the clusters: %fs \n" % (time() - t0))