from Bio import Entrez
import argparse

parser=argparse.ArgumentParser(description= 'This script reduces the TF-IDF matrix through SVD descompostion.')

parser.add_argument('-p', '--pmids', required=True, type=str, help='Vectorization matrix file path (tsv).')
parser.add_argument('-o', '--out', required=True, type=str, help='Vectorization matrix file path (tsv).')
args=parser.parse_args()

#read archive for pmids
with open (args.pmids, 'r') as arc:
    pmids=[line.split('\n')[0] for line in arc]
print(f'{len(pmids)} Pubmed ids read...')


print('Entrez efecth...')
#email for contact in case of excesive demand prior blocking
Entrez.email = "joelrh@lcg.unam.mx"

handle = Entrez.efetch(db="pubmed", id=','.join(map(str, pmids)),
                       rettype="xml", retmode="text")
records = Entrez.read(handle)

#save pmids without abstract for future report
withoutabs = []

for pmid in records['PubmedArticle']:
  if 'Abstract' not in pmid['MedlineCitation']['Article']:
    withoutabs.append(str(pmid['MedlineCitation']['PMID']))
print(f'Articles without abstract: {len(withoutabs)}')

#create string for all the authors as a dict key
for pmid in records['PubmedArticle']:
  if 'Abstract' and 'AuthorList' in pmid['MedlineCitation']['Article']:
    #print(pmid['MedlineCitation']['PMID'])
    #print(pmid['MedlineCitation']['Article']['AuthorList'][-1].keys())
    s=''
    for autor in pmid['MedlineCitation']['Article']['AuthorList']:
      if 'ForeName' in autor.keys():
        s += (autor['LastName']+', '+autor['ForeName']+'; ')
      if 'Initials' in autor.keys():
        s += (autor['LastName']+', '+autor['Initials']+'; ')
      if 'CollectiveName' in autor.keys():
        s += (autor['CollectiveName']+'; ')
      else:
        s += (autor['LastName']+'; ')
    s = s[:-2]
  else:
    s = 'No authors listed'
  pmid['Autors'] = s

############################
#     Escribir archivo 'abs_unicos.txt'
# Formato:
# pmid  \t  titulo  \t  abstract  \t  fecha  \t  autores  \t  journal  \n
# pmid - pmid['MedlineCitation']['PMID']
# titulo - pmid['MedlineCitation']['Article']['ArticleTitle']
# abstract - pmid['MedlineCitation']['Article']['Abstract']['AbstractText'][0]
# fecha - pmid['MedlineCitation']['Article']['Journal']['JournalIssue']['PubDate']
# autores - pmid['MedlineCitation']['Article']['AuthorList'][i]['LastName'] +  pmid['MedlineCitation']['Article']['AuthorList'][i]['ForeName']
# journal - pmid['MedlineCitation']['Article']['Journal']['ISOAbbreviation']

f = open(args.out,  'w')

for pmid in records['PubmedArticle']: 
  keys = pmid['MedlineCitation']['Article']['Journal']['JournalIssue']['PubDate'].keys()
  if 'Month' in keys and 'Abstract' in pmid['MedlineCitation']['Article']:
    f.write(pmid['MedlineCitation']['PMID'] + '\t' + 
            pmid['MedlineCitation']['Article']['ArticleTitle'] + '\t' +
            str(pmid['MedlineCitation']['Article']['Abstract']['AbstractText'][0]).replace("\n", " ") + '\t' +
            pmid['MedlineCitation']['Article']['Journal']['JournalIssue']['PubDate']['Year'] + ' ' + pmid['MedlineCitation']['Article']['Journal']['JournalIssue']['PubDate']['Month'] + '\t' + 
            pmid['Autors'] + '\t' +
            pmid['MedlineCitation']['Article']['Journal']['ISOAbbreviation'] +
            '\n' )
    continue
  elif 'MedlineDate' in keys and 'Abstract' in pmid['MedlineCitation']['Article']: 
    f.write(pmid['MedlineCitation']['PMID'] + '\t' + 
            pmid['MedlineCitation']['Article']['ArticleTitle'] + '\t' +
            str(pmid['MedlineCitation']['Article']['Abstract']['AbstractText'][0]).replace("\n", " ") + '\t' +
            pmid['MedlineCitation']['Article']['Journal']['JournalIssue']['PubDate']['MedlineDate'] +  '\t' + 
            pmid['Autors'] + '\t' +
            pmid['MedlineCitation']['Article']['Journal']['ISOAbbreviation'] +
            '\n' )
    continue
  elif 'Month' not in keys and 'Abstract' in pmid['MedlineCitation']['Article']: 
    f.write(pmid['MedlineCitation']['PMID'] + '\t' + 
            pmid['MedlineCitation']['Article']['ArticleTitle'] + '\t' +
            str(pmid['MedlineCitation']['Article']['Abstract']['AbstractText'][0]).replace("\n", " ") + '\t' +
            pmid['MedlineCitation']['Article']['Journal']['JournalIssue']['PubDate']['Year'] +  '\t' + 
            pmid['Autors'] + '\t' +
            pmid['MedlineCitation']['Article']['Journal']['ISOAbbreviation'] +
            '\n' )


f.close()

f = open(args.out,  'r')

pmids_retrieved= [line.split('\t')[0] for line in f]

not_found = []

for id in pmids:
  if id not in pmids_retrieved:
    if id not in withoutabs:
      not_found.append(id)

f.close()

g = open('info_download.txt', 'w')

g.write('total_pmids recieved\tpmids_retrieved\tpmids_without_abstracts\tpmids_not_found\n'+
        str(len(pmids))+'\t'+str(len(pmids_retrieved))+'\t'+str(len(withoutabs))+'\t'+str(len(not_found)))