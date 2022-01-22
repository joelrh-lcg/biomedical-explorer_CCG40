# biomedical-explorer_CCG40

1. unique_ids.Rmd
- input: ./source xlsx archives with all CCG ids since 1980 to 2021
    - 20211208_Publicaciones_CIFN-CCG_1980-2021_BD_Fundanet.xlsx
    - 20211208_Publicaciones_CIFN-CCG_1980-2021_WOS.xlsx 
- output: ../source/pubmed_ids.txt


# Retrieve de los pubmed ids con entrez
2. abstracts_ext.py
- input: ./souce/pumbed_ids.txt which is a list with PUBMED IDs separated by \n
- output: ../temp/abs_unicos.txt
    - ID \t Title \t abstract \t year \t authors \t journtal

# Lematizacion
3. corenlp.py
- input:  ../temp/abs_unicos.txt
-  output: ../temp/abs_unicos_lemmatized.txt

# Vectorization 
4. tf-idf.py
- input: ../temp/abs_unicos_lemmatizet.txt 
- output: ../temp/Abstracts_vect.txt

# Reduccion de dimensionalidad
5. chi2.py o svd.py
- input: ../temp/Abstracts_vect.txt
- output: ../temp/Abstracts_vect_chi2.tsv o ./temp/Abstracts_vect_svd.tsv

# Clustering 
5. ./scripts/clustering_clean.py 
- input: ../temp/Abstracts_vect_chi2.txt o ../temp/Abstracts_vect_svd.txt
- output: k50_PCA_perp30.txt

