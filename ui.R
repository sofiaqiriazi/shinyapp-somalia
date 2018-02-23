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

list_regs <- c("Bay","Bakool","Banadir","Gedo", "Middle Juba", "Lower Juba",
               "Middle Shabelle", "Lower Shabelle", "Hiiraan", "Galgaduud", "Mudug","Nugaal", "Bari")

list_camps <- c("Dollo Ado")

shinyUI(
  # Use a fluid Bootstrap layout
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src="main.js")
    ),

    # Give the page a title
    titlePanel("Predictive Engine: Project JETSON"),
    p("Jetson is a project aimed at providing better data
      analytics to make better decisions to adequately prepare
      for contingencies in forced displacement situations.
      The Predictive Analytics Engine (Jetson) is an applied
      predictive analytics experiment taking concrete steps
      to provide insights on the future of displacement.",tags$br(),
      "The data behind the engine is anonymized, aggregated
      per month and per region. Project Jetson uses machine-learning
      for building a nonparametric algorithm (model) for each region.
      The models used for each region represent the best 'fit' that
      can explains the behaviour of seven years of historical data."),
    # Generate a row with a sidebar
    tabsetPanel(
      tabPanel("Refugees", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   radioButtons("region", "Choose region", list_regs, selected = NULL, inline = FALSE),
                   downloadButton("downloadData", "Generate report"),
                   downloadButton("downloadCsv", "Generate csv")
                  ),
                 mainPanel(
                   showOutput("graph1",lib="highcharts"),
                   p("Click on the arrivals/algorithm name, to select or unselect the data on the graph",align="center")

                 )
               )
      ),
      tabPanel("IDPs", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                  radioButtons("camp", "Choose camp", list_camps, selected = NULL, inline = FALSE),
                  downloadButton("downloadData", "Generate report"),
                  downloadButton("downloadCsv", "Generate csv")
                  ),
                 mainPanel(
                  showOutput("graph2",lib="highcharts"),
                  p("Click on the arrivals/algorithm name, to select or unselect the data on the graph",align="center")

                 )
               )
      )
    )
  )
)
