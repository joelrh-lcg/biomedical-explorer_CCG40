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
library(shinyDirectoryInput)
library(parallel)

listSessions <- function(){
  #sessionsDirs <- list.dirs("sessions", recursive = F, full.names = F)
  sessionsDirs <- list(system("cd sessions/; ls -t", intern = T))[[1]]
  return(sessionsDirs)
}

shinyUI(
  navbarPage("Article collection visualizer",
             tabPanel("Load collection",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("rSessions", label = "Recent Sessions", 
                                    choices = listSessions()),
                        fluidRow(
                          column(3, actionButton("GoVis", label = "Explore!")),
                          column(3, actionButton("newSession", label = "Create new collection"))
                        ),
                        actionButton("demo", label = "Demo"),
                        
                        conditionalPanel("input.newSession%2==1", hr(),
                                         textInput("sessionName", label = "Enter a name for your collection:"),
                                         selectInput("pythonV", label = "Python interpreter:", multiple = F, 
                                                     choices = list(system("which -a python3", intern = T))[[1]]),
                                         directoryInput('corenlp', label = 'Path of CoreNLP directory:', value = '~'),
                                         textInput("entrez_mail", label="E-mail:", placeholder = "Used for entrez efecth"),
                                         textAreaInput("pmidsType", label = "Type your PMIDs:", height = 300, resize = "vertical"),
                                         fileInput("upPMIDs", label = "Upload your PMIDs:", multiple = F, accept = c("txt"), placeholder = "Plain text files"),
                                         radioButtons('feature_selection', label = "Feature selection method:", choices = c("SVD", "CHI2", "ANOVA-F")),
                                         conditionalPanel("input.feature_selection != 'SVD'",{
                                           sliderInput("percentil", label="Best percentil to select:", min=5, max=100, step=5, round= T, value=10)
                                         }),
                                         radioButtons("searchm", label = "Method for best K silhouette score evaluation:",
                                                      choices = c("Linear", "Binary")),
                                         sliderInput("lims_bestk", label = "Limits of evaluation for K", 
                                                     min = 2, max = 200, value = c(5, 100), step = 1, pre = "K=", round = T),
                                         conditionalPanel("input.searchm == 'Linear'",
                                                        sliderInput("stepsK", label = "Step:", min = 1, max = 10, value = 1, round = T)),
                                         conditionalPanel("input.searchm == 'Binary'",
                                                          sliderInput("iterK", label = "Number of iterations:", min = 2, max = 20, value = 10, round = T)),
                                         selectInput("cores", label = "Threads to run search K on parallel:",
                                                     choices = 1:detectCores()),
                                         checkboxInput('autoK', label="Automatically select best K (number of clusters) based on silhouettes scores.", value=T),
                                         hr(), actionButton("processPMIDs", label = "Process collection")
                                         )
                      ),
                      mainPanel(
                        conditionalPanel("input.newSession%2==1",
                          textOutput("runStart"), hr(),
                          uiOutput("runInfo"), hr(),
                          tableOutput("downloadInfo"), hr(),
                          textOutput("corenlpTime"),
                          textOutput("tfidf_result"),
                          textOutput("Feature_sel"), hr(),
                          tableOutput("silScores"),
                          plotOutput("silEval"),
                          tabsetPanel(
                            tabPanel(title = uiOutput("silPlots_title1"),plotOutput("silPlots1")),
                            tabPanel(title = uiOutput("silPlots_title2"),plotOutput("silPlots2")),
                            tabPanel(title = uiOutput("silPlots_title3"),plotOutput("silPlots3")),
                            tabPanel(title = uiOutput("silPlots_title4"),plotOutput("silPlots4")),
                            tabPanel(title = uiOutput("silPlots_title5"),plotOutput("silPlots5")),
                            )
                          )
                        )
                    )  
                      ),
              tabPanel("Explorer",
  fluidPage(                  
  box(width = 12, title = "Clusters collection",
      wellPanel(style='background-color:white',
      highchartOutput('wine_corr', height = "600px"))),
  
  conditionalPanel("output.checkClst",
                   box(title = "Article selected", width = 12,
                       wellPanel(style='background-color:white',
    textOutput("mp_title"),
    tags$head(tags$style("#mp_title{
    font-size: 20px;
    font-style: italic;
    }"
    )), hr(),
    sidebarLayout(
    sidebarPanel(
      radioButtons("entities_a", h3("Entities"),
                   choices = list("All" = "all",
                                  "Biological process"="biological_process",
                                  "Cell"="cell",
                                  "Cellular component" = "cellular_component",
                                  "Cellular lines"= "cell_line",
                                  "Chemicals" = "chemical",
                                  "Clinical drugs"="clinical_drug",
                                  "Entity type"="entity_type",
                                  "Disease"="disease",
                                  "Gene/protein" = "gene/protein",
                                  "Molecular function"="molecular_function",
                                  "Molecular process"="molecular_process",
                                  "Organ/tissue"="organ/tissue",
                                  "Organisms"="organism",
                                  "Sequence"="sequence"
                   ), selected = "all"),
      radioButtons("nwords_a", h3("Select of type term"),
                   choices = list("Matched term (original)" = "m_term",
                                  "Prefered term (OGER)" = "p_term"),
                   selected = "p_term"), width = 2),
    mainPanel(width = 10,
              box(width = 8, title = "Main terms",
                   wellPanel(style='background-color:white', dataTableOutput(outputId = "tablePMID"))),
              box(width = 4, title = "Data sheet", 
                  uiOutput(outputId = "infoPMID")) 
      )
    ))),
  box(title="Cluster selected", width = 12, wellPanel(style='background-color:white',
  sidebarLayout(
    sidebarPanel(
      fluidRow(
      wellPanel(style='background-color:white', uiOutput("cluster_selected"))),
      hr(),
      radioButtons("entities", h3("Entities"),
                   choices = list("Biological process"="biological_process",
                                  "Cell"="cell",
                                  "Cellular component" = "cellular_component",
                                  "Cellular lines"= "cell_line",
                                  "Chemicals" = "chemical",
                                  "Clinical drugs"="clinical_drug",
                                  "Entity type"="entity_type",
                                  "Disease"="disease",
                                  "Gene/protein" = "gene/protein",
                                  "Molecular function"="molecular_function",
                                  "Molecular process"="molecular_process",
                                  "Organ/tissue"="organ/tissue",
                                  "Organisms"="organism",
                                  "Sequence"="sequence"), 
                   selected = "biological_process"),
      radioButtons("t_type", h3("Select of type term"),
                   choices = list("Matched term (original)" = "m_term",
                                  "Prefered term (OGER)" = "p_term"
                   ), selected = "p_term"),
      radioButtons("tplot", h3("Select plot type"),
                   choices = list("Pie chart"="pie",
                                  "Bar plot stacked" = "stack",
                                  "Bar plot dodged"  = "dodged"
                   ), selected = "pie"), width = 2
    ),
    mainPanel(width = 10,
      fluidRow(
        box(width = 5, title = "Main LDA TF-IDF topics",
            wellPanel(style='background-color:white', plotOutput(outputId = "LDAPlot"))),
        box(width = 7, title = "Entity percentage for selected cluster vs all clusters",
            wellPanel(style='background-color:white', plotOutput("plotAllClusters")))
        ),
      fluidRow(
        box(width = 6,title = "Principal terms", 
            wellPanel(style='background-color:white',
                      div(dataTableOutput("tableCluster"), style = "font-size:90%"))),
        box(width = 6, title = "WordCloud",
            wellPanel(style='background-color:white', wordcloud2Output('wordcloud2'),align="center")), 
      ))))))
  ))))