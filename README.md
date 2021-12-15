# biomedical-explorer_CCG40

1. notebook.Rmd
- input: ./source xlsx archives with all CCG ids since 1980 to 2021
    - 20211208_Publicaciones_CIFN-CCG_1980-2021_BD_Fundanet.xlsx
    - 20211208_Publicaciones_CIFN-CCG_1980-2021_WOS.xlsx 
- output: ./source/pubmed_ids.txt


# Retrieve de los pubmed ids con entrez
2. abstracts_ext.py
- input: ./souce/pumbed_ids.txt which is a list with PUBMED IDs separated by \n
- output: abs_unicos.txt
    - ID \t Title \t abstract \t year \t authors \t journtal

# Lematizacion
3. corenlp.py
- input:  abs_unicos.txt
-  output: abs_unicos_lemmatized.txt

# Vectorization 
4. ./scripts/tf-idf.py
- input: abs_unicos_lemmatizet.txt 
- output: ./abstracts_vect.txt

# Clustering 
5. ./scripts/clustering_clean.py 
- input: ./abstracts_vect.txt
- output: k50_PCA_perp30.txt

