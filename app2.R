library(shiny)
library(survival)
library(ranger)    
library(ggplot2)
library(dplyr)
library(ggfortify)
install.packages('survminer')
source("https://bioconductor.org/biocLite.R")

library(survminer)

ui <- fluidPage(
  titlePanel("SURVIVAL ANALYSIS TCGA-LUSC DATA"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(7, offset=2, selectInput(inputId = 'gene', label = "Please select a gene", Gen_name$Hugo_Symbol, selected = "All"))),
      fluidRow(
        column(7,  offset=2,selectInput(inputId = 'endpoint', label = 'Please select the endpoint to display', c('OS', 'DSS', 'PFI', 'DFI'), selected = 'OS')))
      ),
    mainPanel(
      plotOutput("OS")
    )
  )
)

server <- function(input, output) {
  output$OS <- renderPlot(
    {
      km_fit <- surv_fit(as.formula(paste("Surv(" , paste(input$endpoint), paste("_time,"), paste(input$endpoint), paste(") ~"), paste(input$gene), sep ="")), data = fourendpints)
      ggsurvplot(km_fit,  data= fourendpints,  risk.table = TRUE, pval = TRUE, conf.int = TRUE, risk.table.y.text.col = T)
    },
    width = 1000, height = 800
  )
}

shinyApp(ui = ui, server = server)

km_fit <- surv_fit(Surv(OS_time,OS) ~TTN, data = fourendpints)
ggsurvplot(km_fit,  data= fourendpints,  risk.table = TRUE)

rm(km_fit)
