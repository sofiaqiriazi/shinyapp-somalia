# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(rCharts)
library(shiny)
library(datasets)
library(magrittr)
library(XML)
library(reshape)
library(gsheet)
library(ggplot2)
library(scales)
library(zoo)
library(pracma)
library(psych)
#options(RCHART_LIB = 'morris')
options(RCHART_LIB = 'Highcharts')


jetson <- "https://docs.google.com/spreadsheets/d/1oPTPmoJ9phtMOkp-nMB7WHnPESomLzqUj9t0gcE9bYA"
conflicts <- gsheet2text(jetson, sheetid = 819472314)
conflicts.long <- read.csv(text=conflicts)

Dates <- sapply(conflicts.long[,1],as.character.Date)
conflicts.long$Date <- as.Date(conflicts.long$Date, format="%m/%d/%Y")

odd_indexes<-seq(2,19,1)
regions <- colnames(conflicts.long[odd_indexes])
list_regs <- rep(NA,18)
# for (i in 1:18){
#   list_regs[i] <- strsplit(regions[i],  "_(?=[^_]+$)",perl=TRUE)[[1]][1]
# }

list_regs <- c("Bay","Bakool","Banadir","Gedo", "Middle Juba", "Lower Juba", "Middle Shabelle", "Lower Shabelle")

shinyUI(
  # Use a fluid Bootstrap layout
  fluidPage(  
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src="main.js")
    ),
    
    # Give the page a title
    titlePanel("Predictive Engine: Project JETSON"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      # Define the sidebar with one input
      sidebarPanel(
        
        radioButtons("region", "Choose region", list_regs, selected = NULL, inline = FALSE),
        #sliderInput("slider", "Slider", 1, 100, 50),
        downloadButton("downloadData", "Generate report"),
        downloadButton("downloadCsv", "Generate csv")
      ),
      
      # Create a spot for the barplot
      mainPanel(
        #showOutput("graph1",lib="morris"),
        showOutput("graph1",lib="highcharts")
      )
      
    )
  )
)
