# biomedical-explorer_CCG40

---
How to run the visualizer

For run the ShinyApp, be sure to have the ShinyApp directory and the sessions/CCG40/session.Rdata

Go to the ShinyApp directory and run:

```
r -e "shiny::runApp(port = 1208)"
```

Go to your web browser and access to the following direction:
```
http://127.0.0.1:1208
```

---


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

# Busqueda de K con el metodo de la silueta
6. silhouette.py
- input: ../temp/Abstracts_vect.txt
- output: ../results/directory/

# Clustering 
5. ./scripts/clustering_clean.py 
- input: ../temp/Abstracts_vect_chi2.txt o ../temp/Abstracts_vect_svd.txt
- output: k50_PCA_perp30.txt

# ejemplo para correr silhouette.py
nohup python3 silhouette.py -m '../Source/Abstracts_vect_svd.txt' -j '../temp/scores_5a.joblib' -sm 'l' -s 5 --li 10 --ls 100 -c 10 -p '../temp/plots_5a/' &

