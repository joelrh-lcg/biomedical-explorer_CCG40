library(shiny)
library(shinydashboardPlus)
library(highcharter)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(gridExtra)
library(argparser, quietly=TRUE)
library(shinydashboard)
library(tidyverse)
library(matlab)
library(dplyr)
library(devtools)
library(tm)
library(wordcloud2)
library(shinyalert)
library(shinyWidgets)
library(reticulate)
library(shinyDirectoryInput)
library(ps)
library(gridExtra)
library(png)
library(grid)



readAbs <- function(){
  
  data_df <- read.table("../temp/abs.txt", sep = "\t", fill = T, header = F, quote="")
  data_df <- data_df[c("V1", "V2", "V4", "V5", "V6")]
  colnames(data_df) <- c("PMID", "Title", "Date", "Author", "Journal")
  data_df$Title <- stringr::str_wrap(factor(data_df$Title, levels = unique(data_df$Title)), 30)
  data_df$Author <- stringr::str_wrap(factor(data_df$Author, levels = unique(data_df$Author)), 30)
  
  return(data_df)
}

readClust <- function(abs_df, enrichment){
  
  dataframe <- read.table("../results/clustering/k60_SVD_perp30.txt", header = T, sep = "\t")
  dataframe$enrichment <- sapply(1:nrow(enrichment), function(x){paste(enrichment[x,], collapse = ",")})
  enrichment$Cluster <- (as.numeric(dataframe$Cluster)+1)
  dataframe <- cbind.data.frame(abs_df, dataframe)
  dataframe$dim1 <- round(dataframe$dim1, 5)
  dataframe$dim2 <- round(dataframe$dim2, 5)
  dataframe$Cluster <- dataframe$Cluster + 1
  
  return(dataframe)
}


# LDA barplot

wfreq <- function(abstracts){
  Word_freq <- as.data.frame(table(unlist(str_split(abstracts, " ")))) %>% arrange(desc(Freq)) %>% filter(! Var1 %in% stopwords("english"))
  return(Word_freq)
}

ldaAbs <- function(dataframe, abstracts){
  
  colnames(abstracts) <- c("PMID", "Abstract")
  abstracts <- merge(abstracts, dplyr::select(dataframe, c("PMID", "Cluster")), by = "PMID")
  
  return(abstracts)  
}

LDA <- function(abstracts, enrichment, Word_freq, clus){
  
  abs_filter <- filter(abstracts, Cluster == clus)
  abs_filter_freq <- as.data.frame(table(unlist(str_split(abs_filter, " ")))) %>% arrange(desc(Freq)) %>% filter(! Var1 %in% stopwords("english"))
  words <- filter(enrichment, Cluster == clus)
  words <- sapply(1:(length(words)-1), function(x){return(str_remove(unlist(str_remove(words[x], " ")), " "))})
  all_data <- sapply(1:length(words), function(x){return(filter(Word_freq, Var1 == words[x])$Freq)})
  cluster_data <- sapply(1:length(words), function(x){return(filter(abs_filter_freq, Var1 == words[x])$Freq)})
  remove_data <- which(cluster_data == "integer(0)")
  if (length(remove_data) == 0){
    cluster_terms <- data.frame(Terms = rep(unlist(words), times = 2), freq  = unlist(c(all_data, cluster_data)), data = c(rep("All clusters", times = length(all_data)), rep("Selected cluster", times = length(all_data))))
  }
  
  if (length(remove_data) != 0) {
    all_data <- all_data[-remove_data]
    cluster_terms <- data.frame(Terms = rep(unlist(words[-remove_data]), times = 2), freq  = unlist(c(all_data, cluster_data)), data = c(rep("All clusters", times = length(all_data)), rep("Selected cluster", times = length(all_data))))
  }
  
  plot <- ggplot(data = cluster_terms, aes(x = reorder(Terms, +freq), y = freq, fill = data)) + 
    geom_bar(stat="identity") +
    labs( x = "Topic", y = "Frequency") + 
    labs(fill = "Topic weight") +
    coord_flip() +
    theme(legend.position="bottom")
  
  return(plot)
}


wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}

# WORD CLOUD FUNCTION 

WordCloudFun <- function(Word_freq, abstracts, clus){
  
  WordsToRemove <- Word_freq %>% dplyr::select(Var1) %>% dplyr::slice(1:20)
  
  abs_filter <- filter(abstracts, Cluster == clus)
  Corpus_data <- Corpus(VectorSource(abs_filter[,2])) # first change our data to a Corpus
  data_filter <- Corpus_data %>% # in all this lines we transform our data and delate all that we will not be needing
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, WordsToRemove$Var1) 
  dtm <- as.matrix(TermDocumentMatrix(data_filter))  # we create a data frame with the words and their frequency
  df <- data.frame(word = names(sort(rowSums(dtm),decreasing=TRUE)), freq = sort(rowSums(dtm),decreasing=TRUE)) %>% slice(1:100)
  
  return(wordcloud2a(data = df, size = 0.8, color = 'random-light', backgroundColor = "white", minRotation = 0, maxRotation = 0))
}

getTfidterms <- function(){
  
  tfid1<-read.table("../temp/oger/by_cluster/matched_k60_SVD_perp30_10_tfidf.txt", header = F, sep = '\t', quote = '', comment.char = "")
  colnames(tfid1)<-c("clusters", "entity","word","frec","cluster_app","tfidf")
  tfid1$term<-"m_term"
  
  tfid2<-read.table("../temp/oger/by_cluster/preferred_k60_SVD_perp30_10_tfidf.txt", header = F, sep = '\t', quote = '', comment.char = "")
  colnames(tfid2)<-c("clusters", "entity","word","frec","cluster_app","tfidf")
  tfid2$term<-"p_term"
  
  tfid<-rbind(tfid1,tfid2)
  tfid$clusters <- as.integer(tfid$clusters + 1)
  
  return(tfid)
}

ogerClust <- function(tfid){

  df1 <- group_by(tfid, clusters) %>% mutate (percent = frec/sum(frec))
  df1 <-aggregate(df1$percent~df1$clusters+df1$entity, FUN = sum)
  colnames(df1)<-c("clusters", "entity","percent")
  
  return(df1)
}

matOger <- function(){
  
  matched_pmids <- data.frame(PMID=as.integer(),
                              Entity=character(), 
                              Term=character(),
                              Frecuency=as.integer(),
                              stringsAsFactors=FALSE)
  
  files <- list.files(path="../temp/oger/by_pmid", pattern="matched_", full.names=TRUE, recursive=FALSE)
  
  for(file in 1:length(files)){
    t <- read.table(files[[file]], header = F, sep = '\t', quote = '')
    matched_pmids <- rbind(matched_pmids,t)
  }
  
  colnames(matched_pmids) <- c("PMID", "Entity","Term", "Frecuence")
  
  return(matched_pmids)
}

prefOger <- function(){
  
  prefered_pmids <- data.frame(PMID=as.integer(),
                               Entity=character(), 
                               Term=character(),
                               Frecuency=as.integer(),
                               stringsAsFactors=FALSE) 
  
  files <- list.files(path="../temp/oger/by_pmid", pattern="preferred_", full.names=TRUE, recursive=FALSE)
  
  for(file in 1:length(files)){
    t <- try(read.table(files[[file]], header = F, sep = '\t', quote = '', comment.char = "", blank.lines.skip = T))
    if(!inherits(t, 'try-error')) prefered_pmids <- rbind(prefered_pmids,t)
  }
  
  colnames(prefered_pmids) <- c("PMID", "Entity","Term", "Frecuence")
  
  return(prefered_pmids)
}

mycolors <-c("biological_process"="#a6cee3", "cell"="#1f78b4", "cellular_component"="#b2df8a", "cell_line"="#33a02c", "chemical"="#fb9a99", "clinical_drug"="#e31a1c", "entity_type"="#fdbf6f", "disease"="#ff7f00", "gene/protein"="#cab2d6", "molecular_function"="#6a3d9a", "molecular_process"="#FE16F4", "organ/tissue"="#E3C60B", "organism"="#b15928", "sequence"="#999999")


loadEnv <- function(rdata_path){
  load(rdata_path)
}

shinyServer(function(input, output, session) {
  
  observeEvent(input$newSession, {
    
    if(input$newSession %% 2 == 1){
      sendSweetAlert(session, 
                     title = "Create a collection with a PMID list", 
                     type = "info", text = "Type or upload a PMID list separated by breaklines. 
                     A valid e-mail is needed in order to identify the abstracts download request and prevent excesive requests.
                     Finally, select a python interpreter that you prefer to use for run modules of TF-IDF calculation, dimensionality reduction and clustering.", 
                     btn_labels = "OK")
    updateActionButton(session = session, inputId = "newSession", label = "Cancel")}
    if(input$newSession %% 2 == 0){
      updateActionButton(session = session, inputId = "newSession", label = "Create new collection")}
  })
  
  observeEvent(input$processPMIDs, {
    output$runStart <- renderText({"Processing collection..."})
    withProgress(message = "Downloading abstracts...", max = 5, expr = {
    dir.create(paste0("sessions/", input$sessionName))
    dir.create(paste0("sessions/", input$sessionName, "/source"))
    dir.create(paste0("sessions/", input$sessionName, "/temp"))
    dir.create(paste0("sessions/", input$sessionName, "/results"))
    dir.create(paste0("sessions/", input$sessionName, "/reports"))
    pmids_path <- paste0("sessions/", input$sessionName, "/source/pmids.txt")
    
    
    if(length(input$upPMIDs$name) == 1) file.copy(input$upPMIDs$datapath, pmids_path)
    else writeLines(input$pmidsType, con = pmids_path)
    
    t <- try(use_python(input$pythonV))
    if(!inherits(t, 'try-error')){
    
      source_python("../Scripts/abstrsct_ext.py")
      download_result <- download_abstracts(pmids_path, input$sessionName, input$entrez_mail)
      download_result <- data.frame("PMIDs received" = download_result[1], 
                                    "PMIDs retrieved" = download_result[2],
                                    "PMIDS without abstract" = download_result[3],
                                    "PMIDs not found" = download_result[4])
      
      incProgress(1, "Abstracts downloaded. Running lemmatization with CoreNLP...")
      
      output$runInfo <- renderUI({
        str1 <- paste("<br>Name of collection:", input$sessionName)
        str2 <- paste("Number of articles entered:", download_result[1])
        str3 <- paste("Number of articles processed in collection:", download_result[2])
        str4 <- paste("Feature selection method:", input$feature_selection)
        str5 <- paste("Iteration method for found best number (k) of clusters:", input$searchm)
        str6 <- paste("Number of iterations: ", input$iterK)
        str7 <- paste("Step length:", input$stepsK)
        str8 <- paste("Range for K search:", input$lims_bestk[1], "-", input$lims_bestk[2])
        return(HTML(paste(str1,str2,str3,str4,str5,str6,str7,str8, sep = "<br/>")))
      })
      
      output$downloadInfo <- renderTable({download_result})
      
      
      path = readDirectoryInput(session, 'corenlp')
      system(paste0(
        "cd ", path, ' ; java -mx4g -cp "*" edu.stanford.nlp.pipeline.StanfordCoreNLPServer -annotators "tokenize,ssplit,pos,lemma,parse,sentiment" -port 9000 -timeout 10000 -quiet true'),
        wait = F, ignore.stdout = T)
      Sys.sleep(5)
      source_python("../Scripts/corenlp.py")
      corenlp_result <- corenlp_annotate(paste0("sessions/", input$sessionName, "/source/abs.txt"))
      pid <- ps() %>% filter(name == "java") %>% pull(pid)
      system(paste0("kill -9 ", pid))
      output$corenlpTime <- renderText({paste("CoreNLP lemmatization performed in", corenlp_result, "seconds.")})
      incProgress(2, "Lemmatization done. Building TF-IDF matrix...")
      
      source_python("../Scripts/tf-idf.py", convert = F)
      tfidf_result <- tfidf(paste0("sessions/", input$sessionName, "/source/abs_lemmatized.txt"), paste0("sessions/", input$sessionName, "/temp/"))
      incProgress(3, "TF-IDF done. Performing feature selection...")
      
      if(input$feature_selection == "SVD"){
        source_python("../Scripts/svd.py")
        selectf_result <- svd(paste0("sessions/", input$sessionName, "/temp/Abstracts_vect.tsv"), paste0("sessions/", input$sessionName, "/temp/Abstracts_vect_reduced.tsv"))
      } else {
        source_python("../Scripts/chi2_anovaf.py")
        selectf_result <- selectf(input$percentil, 
                                  paste0("sessions/", input$sessionName, "/temp/Abstracts_vect.tsv"),
                                  paste0("sessions/", input$sessionName, "/temp/clases_Abstracts.txt"),
                                  paste0("sessions/", input$sessionName, "/temp/Abstracts_vect_reduced.tsv"),
                                  input$feature_selection)}
      
      output$tfidf_result <- renderText({paste("TF-IDF vectorization created", selectf_result[2], "features.")})
      output$Feature_sel <- renderText({paste("Features selected", selectf_result[3], "in", selectf_result[1], "seconds.")})
      incProgress(3, "Best features selected. Evaluation K number of clusters...")
      
      source_python("../Scripts/silhouette.py")
      eval_sil_result <- eval_sil(input$searchm, input$lims_bestk[1], input$lims_bestk[2], 
                                  file_matrix = paste0("sessions/", input$sessionName, "/temp/Abstracts_vect_reduced.tsv"),
                                  plots = paste0("sessions/", input$sessionName, "/results/"),
                                  threads = input$cores,
                                  iter = input$iterK, step = input$stepsK)
      
      
      sil_result <- data.frame("K_cluster" = names(eval_sil_result),
                 "Silhouette Score" = unlist(eval_sil_result))
      
      output$silScores <- renderTable({
        sil_result
      })
      
      plotsK_files <- list.files(path = paste0("sessions/", input$sessionName, "/results/"), pattern = "plot_k", full.names = T)
      
      output$silEval <- renderPlot({
        imag <- readPNG(paste0("sessions/", input$sessionName, "/results/scores.png"))
        plot.new()
        grid::grid.raster(imag)
      })
      
      lapply(1:5, function(i){
        print(paste0("K=", sil_result$K_cluster[i]))
        output[[paste0("silPlots_title",i)]] <- renderText({paste0("K=", sil_result$K_cluster[i])})
        output[[paste0("silPlots",i)]] <- renderPlot({
          imag <- readPNG(paste0("sessions/", input$sessionName, "/results/plot_k", sil_result$K_cluster[i], ".png"))
          plot.new()
          grid::grid.raster(imag)
        })
      })
      
    }else{
        print("error")
    }
    incProgress(4, "Finished...")
    })
  })
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$corenlp
    },
    handlerExpr = {
      if (input$corenlp > 0) {
        # condition prevents handler execution on initial app launch
        
        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = readDirectoryInput(session, 'corenlp'))
        
        # update the widget value
        updateDirectoryInput(session, 'corenlp', value = path)
      }
    }
  )

  
  observeEvent(input$demo,{
    withProgress(message="Loading files and packages...", {
      
      abs <- read.csv("../temp/abs_lemmatized.txt", sep = "\t", header = F)[,c(1,3)]
      colnames(abs) <- c("PMID", "Abstract")
      
      data_df <- readAbs()
      incProgress(1/6,"Abstracts charged...")
      
      enrichment <- read.table("../results/LDA/LDA_k60_SVD_perp30.txt", sep = "\t")
      dataframe <- readClust(data_df, enrichment)
      enrichment$Cluster <- (as.numeric(dataframe$Cluster)+1)
      enrichment <- unique(enrichment)
      incProgress(2/6,"enrichment charged...")
      
      abstracts <- ldaAbs(dataframe, abs)
      Word_freq <- wfreq(abstracts)
      size <- length(unique(dataframe$Cluster))
      palette <- colorRampPalette(brewer.pal(8, "Set2"))(size)
      incProgress(3/6,"Words frequency calculated...")
      
      tfid <- getTfidterms()
      df1 <- ogerClust(tfid)
      matched_pmids <- matOger()
      prefered_pmids <- prefOger()
      incProgress(4/6,"Oger terms charged...")
      
      ###plots
      p_stacked <- ggplot(data=df1, aes(factor(clusters), percent, fill = entity)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        geom_bar(stat="identity", position="stack") +
        scale_fill_manual(values = mycolors) +
        labs(fill = "Entities") +
        ylab("Entity percentage") +
        xlab("Clusters")
      
      p_pie <- ggplot(df1, aes(x="", y=percent, fill=entity)) +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid  = element_blank()) +
        geom_bar(stat="identity", width=1, color="white") + 
        coord_polar("y", start=0)+
        scale_fill_manual(values = mycolors) +
        facet_wrap(~clusters)  +
        labs(fill = "Entities") +
        ylab("Clusters")
      
      
      p_dodged <- ggplot(data=df1, aes(factor(clusters), percent, fill=entity)) +
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90)) +
        geom_bar(stat="identity", position=position_dodge())+
        scale_fill_manual(values = mycolors)  +
        labs(fill = "Entities") +
        ylab("Entity percentage") +
        xlab("Clusters")
      
      incProgress(5/6,"Plotting...")
      
      stacked_plots<-list()
      for(i in 1:max(df1$clusters))
      {
        data <- df1[df1$clusters == i,]
        data <- transform(data, clusters = as.character(clusters))
        pstacked <- ggplot(data, aes(x=clusters, y=percent, fill = entity)) +
          geom_col() +
          theme_bw() +
          scale_fill_manual(values = mycolors) +
          labs(fill = "Entities") +
          ylab("Entity percentage") +
          xlab("Clusters") + 
          theme(axis.text.x = element_text(angle = 0), legend.position = "bottom")
        stacked_plots[[i]] <- pstacked
      }
      
      pie_plots<-list()
      for(i in 1:max(df1$clusters))
      {
        data <- df1[df1$clusters == i,]
        data <- transform(data, clusters = as.character(clusters))
        data$clusters <- with(data, paste("Cluster", clusters, sep = " "))
        ppie <-ggplot(data, aes(x="", y=percent, fill=entity)) +
          theme(axis.text = element_blank(),
                axis.ticks = element_blank(),
                panel.grid  = element_blank(),
                legend.position = "bottom") +
          geom_bar(stat="identity", width=1, color="white") + 
          coord_polar("y", start=0)+
          scale_fill_manual(values = mycolors) +
          facet_wrap(~clusters)+
          ylab("Cluster") +
          labs(fill = "Entities") + theme(legend.position="bottom")       
        pie_plots[[i]] <- ppie
      }
      
      dodged_plots<-list()
      for(i in 1:max(df1$clusters))
      {  
        data <- df1[df1$clusters == i,]
        data <- transform(data, clusters = as.character(clusters))
        pstacked <- ggplot(data, aes(x=clusters, y=percent, fill = entity)) +
          geom_bar(stat="identity", position="dodge") +
          theme_bw() +
          scale_fill_manual(values = mycolors) +
          labs(fill = "Entities") +
          ylab("Entity percentage") +
          xlab("Cluster") + 
          theme(axis.text.x = element_text(angle = 0), legend.position="bottom")
        dodged_plots[[i]] <- pstacked
      }
      
      Sys.sleep(5)
      
      save(file = "sessions/CCG40/session.Rdata", 
           abs, data_df, enrichment, dataframe, abstracts, 
                    Word_freq, size, palette, tfid, df1, matched_pmids, prefered_pmids, p_stacked,
                    p_pie, p_dodged, stacked_plots, pie_plots, dodged_plots)
      incProgress(1,"Plotting...")
    })## withprogress
  })
  
  observeEvent(input$GoVis, {
    load(paste0("sessions/", input$rSessions, "/session.Rdata"))
    output$wine_corr <- renderHighchart({
      withProgress(message = "Plotting clusters...",{
        dataframe %>%
          hchart('scatter', hcaes(x = dim1, y = dim2, group = Cluster), color = palette) %>%
          hc_tooltip(lineHeight= '15em', useHTML=TRUE, pointFormat = 
                       "<b>Cluster: {point.Cluster} </b><br>
           <b>PMID:</b> {point.PMID} <br>
           <b>Title:</b> {point.Title} <br>
           <b>Author(s):</b> {point.Author} <br>
           <b>Date:</b> {point.Date} <br>
           <b>Journal:</b> {point.Journal} <br>
           <b>LDA TF-IDF topics:</b> {point.enrichment} <br>
           ") %>%
          
          hc_plotOptions(series = list(boderWidth = 1,
                                       dataLabels = list(enabled = FALSE),
                                       events = list(click = JS("function(event) {
          var xstr = event.point.x;
          var ystr = event.point.y;
          var data = {x: xstr, y: ystr, nonce: Math.random()};
          Shiny.onInputChange('matrix_click', data);
          }"))))
        
      })
    })
    
    output$checkClst <- reactive({
      isTruthy(input$matrix_click)
    })
    outputOptions(output, 'checkClst', suspendWhenHidden=FALSE)
    
    observeEvent(input$matrix_click, {
      
      withProgress(message = "Loading cluster...", {
        
        xvals <- dataframe$dim1 %in% input$matrix_click$x
        yvals <- dataframe$dim2 %in% input$matrix_click$y
        art_data <- reactiveValues(
          clst = dataframe[xvals & yvals, 8], #corresponde al cluster
          art = dataframe[xvals & yvals, 1], #corresponde al PMID
          aut = dataframe[xvals & yvals, 4], #corresponde al autores
          date = dataframe[xvals & yvals, 3], #corresponde a la fecha
          jrnl = dataframe[xvals & yvals, 5], #corresponde al journal
          ttl = dataframe[xvals & yvals, 2] #corresponde al titulo
        )
        
        output$tableCluster <- renderDataTable({
          if(input$t_type == "m_term"){
            n_cluster <- tfid[tfid$entity == input$entities & tfid$clusters == art_data$clst & tfid$term == "m_term", 3:6]
            colnames(n_cluster)<-c("Term", "Frecuency","Clusters", "TF-IDF")
            n_cluster
          }
          else if(input$t_type == "p_term"){
            n_cluster <- tfid[tfid$entity == input$entities & tfid$clusters == art_data$clst & tfid$term == "p_term", 3:6]
            colnames(n_cluster)<-c("Term", "Frecuency","Clusters", "TF-IDF")
            n_cluster
          }
        }, options = list(
          pageLength = 10))
        
        output$cluster_selected <- renderUI({
          srt0 <- paste("<strong>Cluster:</strong>", art_data$clst)
          str1 <- paste("X:", input$matrix_click$x)
          str2 <- paste("Y:", input$matrix_click$y)
          HTML(paste(srt0,"<strong>Coordinates:</strong>", str1, str2, sep = "<br/>"))
        })
        
        output$mp_title <- renderText({  
          art_data$ttl
        }) 
        
        incProgress(1/2, "Rendering plots...")
        
        output$tablePMID <- renderDataTable({
          
          if(input$entities_a != "all"){
            if (input$nwords_a == 'm_term'){
              mt<- matched_pmids[matched_pmids$PMID == art_data$art & matched_pmids$Entity == input$entities_a,]
              colnames(mt)<-c("PMID", "Entity","Term", "Frecuence")
              mt[,c("Entity","Term", "Frecuence")]}
            else if (input$nwords_a == 'p_term'){
              pt<-prefered_pmids[prefered_pmids$PMID == art_data$art & prefered_pmids$Entity == input$entities_a,]
              colnames(pt)<-c("PMID", "Entity","Term", "Frecuence")
              pt[,c("Entity","Term", "Frecuence")]}
          }else{
            if (input$nwords_a == 'm_term'){
              mt<- matched_pmids[matched_pmids$PMID == art_data$art,]
              colnames(mt)<-c("PMID", "Entity","Term", "Frecuence")
              mt[,c("Entity","Term", "Frecuence")]}
            else if (input$nwords_a == 'p_term'){
              pt<-prefered_pmids[prefered_pmids$PMID == art_data$art,]
              colnames(pt)<-c("PMID", "Entity","Term", "Frecuence")
              pt[,c("Entity","Term", "Frecuence")]}
          }
        }, options = list(
          pageLength = 10)) 
        
        output$infoPMID <- renderUI({
          str1 <- paste('<br><strong>PMID:</strong> ', art_data$art)
          str2 <- paste("<strong>Title</strong>:", art_data$ttl)
          str3 <- paste("<strong>Autors:</strong>", art_data$aut)
          str4 <- paste("<strong>Date:</strong>", art_data$date)
          str5 <- paste("<strong>Journal:</strong>", art_data$jrnl)
          HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
        })
        
        output$plotAllClusters <- renderPlot(
          if (input$tplot == "dodged") {
            grid.arrange(dodged_plots[[art_data$clst]], p_dodged, widths = c(2,3), nrow = 1)}
          else if (input$tplot == "pie") {
            grid.arrange(pie_plots[[art_data$clst]], p_pie, widths = c(2,3), nrow = 1)} 
          else if (input$tplot == "stack") {
            grid.arrange(stacked_plots[[art_data$clst]], p_stacked, widths = c(2,3), nrow = 1)} 
        )
        
        output$LDAPlot <- renderPlot({
          LDA(abstracts = abstracts, enrichment = enrichment, clus = art_data$clst, Word_freq = Word_freq)
        })
        
        output$wordcloud2 <- renderWordcloud2({
          WordCloudFun(clus = art_data$clst, Word_freq = Word_freq, abstracts = abstracts)
        })
        
        Sys.sleep(2)
        incProgress(1, "Completed.")
        
      })
    })##matrixclick
    
  })##govis
  
})##server