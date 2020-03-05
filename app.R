library(shiny)
library(survival)
library(ranger)    
library(ggplot2)
library(dplyr)
library(ggfortify)
install.packages('survminer')
source("https://bioconductor.org/biocLite.R")


ui <- fluidPage(
    titlePanel("SURVIVAL ANALYSIS TCGA-LUSC DATA"),
    fluidRow(
      column(4, offset = 1, selectInput(inputId = 'gene', label = "Please select a gene", Gen_name$Hugo_Symbol, selected = "All")),
      column(4, offset = 1, sliderInput(inputId = 'xvalue', 'Survival Days = ',value=10, min=1, max=max(fourendpints$OS_time, na.rm = TRUE)))
    ),
    #selectInput(inputId = 'endpoint', label = "Please select one of th outputs", c("OS", "DSS", "DFI", "PFI"), selected = "OS"),
    fluidRow(
      column(6, plotOutput("OS")),
      column(6, plotOutput("DSS")),
    ),
    fluidRow(
      column(4, offset = 1, tableOutput("c_OS")),
      column(4, offset = 2, tableOutput("c_DSS"))
    ),
    fluidRow(
      column(6, plotOutput("DFI")),
      column(6, plotOutput("PFI")),
    ),
    fluidRow(
      column(4, offset = 1, tableOutput("c_DFI")),
      column(4, offset = 2, tableOutput("c_PFI"))
    ),
)

server <- function(input, output) {
  output$OS <- renderPlot(
  {
  #km_fit <- survfit(as.formula(paste("Surv(", paste(input$endpoint), paste("_time, "), paste(input$endpoint), paste(") ~"), paste(input$gene), sep= "")), data = fourendpints)
  km_fit_OS <- survfit(as.formula(paste("Surv(OS_time, OS) ~", paste(input$gene), sep= "")), data = fourendpints)
  plot(km_fit_OS, 
    col=c("red","sky blue","green","purple","orange","yellow", "black", "pink", "brown"), xlab="Days", ylab="S(t)", main = "Overall Survival")
    legend("bottomleft", cex=0.9, levels(fourendpints[,c(input$gene)]), fill= c("red","sky blue","green","purple","orange","yellow", "black", "pink", "brown"))
    abline(v=input$xvalue,col=1,lty=2)
  })
  output$DSS <- renderPlot(
  {#km_fit <- survfit(as.formula(paste("Surv(", paste(input$endpoint), paste("_time, "), paste(input$endpoint), paste(") ~"), paste(input$gene), sep= "")), data = fourendpints)
    km_fit <- survfit(as.formula(paste("Surv(DSS_time, DSS) ~", paste(input$gene), sep= "")), data = fourendpints)
    plot(km_fit, 
         col=c("red","sky blue","green","purple","orange","yellow", "black", "pink", "brown"), xlab="Days", ylab="S(t)", main = "Disease Specific Survival")
    legend("bottomleft", cex=0.9, levels(fourendpints[,c(input$gene)]), fill= c("red","sky blue","green","purple","orange","yellow", "black", "pink", "brown"))
    abline(v=input$xvalue,col=1,lty=2)
  })
  output$DFI <- renderPlot(
  {#km_fit <- survfit(as.formula(paste("Surv(", paste(input$endpoint), paste("_time, "), paste(input$endpoint), paste(") ~"), paste(input$gene), sep= "")), data = fourendpints)
  km_fit <- survfit(as.formula(paste("Surv(DFI_time, DFI) ~", paste(input$gene), sep= "")), data = fourendpints)
  plot(km_fit, 
       col=c("red","sky blue","green","purple","orange","yellow", "black", "pink", "brown"), xlab="Days", ylab="S(t)", main = "Disease Free Interval")
  legend("bottomleft", cex=0.9, levels(fourendpints[,c(input$gene)]), fill= c("red","sky blue","green","purple","orange","yellow", "black", "pink", "brown"))
  abline(v=input$xvalue,col=1,lty=2)
  })
  output$PFI <- renderPlot(
  {#km_fit <- survfit(as.formula(paste("Surv(", paste(input$endpoint), paste("_time, "), paste(input$endpoint), paste(") ~"), paste(input$gene), sep= "")), data = fourendpints)
  km_fit <- survfit(as.formula(paste("Surv(PFI_time, PFI) ~", paste(input$gene), sep= "")), data = fourendpints)
  plot(km_fit, 
       col=c("red","sky blue","green","purple","orange","yellow", "black", "pink", "brown"), xlab="Days", ylab="S(t)", main = "Progression Free Survival")
  legend("bottomleft", cex=0.9, levels(fourendpints[,c(input$gene)]), fill= c("red","sky blue","green","purple","orange","yellow", "black", "pink", "brown"))
  abline(v=input$xvalue,col=1,lty=2)
  })
  output$c_OS <- renderTable({
    km_fit_OS <- survfit(as.formula(paste("Surv(OS_time, OS) ~", paste(input$gene), sep= "")), data = fourendpints)
    as.data.frame(summary(km_fit_OS, times=input$xvalue)[c("surv", "time", "strata")])
  })
  output$c_DSS <- renderTable({
    km_fit_DSS <- survfit(as.formula(paste("Surv(DSS_time, DSS) ~", paste(input$gene), sep= "")), data = fourendpints)
    as.data.frame(summary(km_fit_DSS, times=input$xvalue)[c("surv", "time", "strata")])
  })
  output$c_DFI <- renderTable({
    km_fit_DFI <- survfit(as.formula(paste("Surv(DFI_time, DFI) ~", paste(input$gene), sep= "")), data = fourendpints)
    as.data.frame(summary(km_fit_DFI, times=input$xvalue)[c("surv", "time", "strata")])
  })
  output$c_PFI <- renderTable({
    km_fit_PFI <- survfit(as.formula(paste("Surv(PFI_time, PFI) ~", paste(input$gene), sep= "")), data = fourendpints)
    as.data.frame(summary(km_fit_PFI, times=input$xvalue)[c("surv", "time", "strata")])
  })
}

shinyApp(ui = ui, server = server)

