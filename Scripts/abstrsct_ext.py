# -*- coding: utf-8 -*-
#############################################
#       NOTAS 

from Bio import Entrez
import os

def download_abstracts(file, session, mail):

    ##read archive for pmids
    with open (file, "r") as arc:
        pmids=[line.split('\n')[0] for line in arc]

    #email for contact in case of excesive demand prior blocking
    Entrez.email = mail

    handle = Entrez.efetch(db="pubmed", id=','.join(map(str, pmids)),
                        rettype="xml", retmode="text")
    records = Entrez.read(handle)

    #save pmids without abstract for future report
    withoutabs = []

    for pmid in records['PubmedArticle']:
        if 'Abstract' not in pmid['MedlineCitation']['Article']:
            withoutabs.append(str(pmid['MedlineCitation']['PMID']))
        #print(len(withoutabs))

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

    f = open(os.path.join('../ShinyApp/sessions', session, 'source/abs.txt'),  'w')

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

    f = open(os.path.join('../ShinyApp/sessions', session, 'source/abs.txt'),  'r')

    pmids_retrieved= [line.split('\t')[0] for line in f]

    not_found = []

    for id in pmids:
        if id not in pmids_retrieved:
            if id not in withoutabs:
                not_found.append(id)

    f.close()

    g = open(os.path.join('../ShinyApp/sessions', session, 'reports/abs_downloaded.txt'), 'w')

    g.write('total_pmids recieved\tpmids_retrieved\tpmids_without_abstracts\tpmids_not_found\n'+
            str(len(pmids))+'\t'+str(len(pmids_retrieved))+'\t'+str(len(withoutabs))+'\t'+str(len(not_found)))

    g.close()

    return [str(len(pmids)), str(len(pmids_retrieved)), str(len(withoutabs)), str(len(not_found))]
