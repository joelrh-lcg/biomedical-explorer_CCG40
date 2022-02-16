#### upload procedure
#create archive only with lemma and pmid
#cat ../temp/abs_lemmatized.txt | cut -f1,3 > ../results/entities/abs_lemmas.txt
#turn archive into UTf8
#iconv -f UTF-8 -t UTF-8//TRANSLIT ../results/entities/abs_lemmas.txt -o ../results/entities/abs_lemmas.txt
##annotate with oger
while IFS=$'\t' read pmid lemma; do curl --data "$lemma" https://oger.nlp.idsia.ch/upload/txt/tsv/"$pmid"; done < ../results/entities/abs_lemmas.txt > ../temp/oger_annot_lemma.txt
##clean and preprocess
##create annotation text
cat ../temp/oger_annot_lemma.txt | grep '^[0-9]' > ../temp/oger_utf8_annot.txt
##create not annotation text
cat ../temp/oger_annot_lemma.txt | grep -v '^[0-9]' > ../temp/oger_utf8_notannot.txt