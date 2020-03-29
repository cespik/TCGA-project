options(repos = BiocManager::repositories())
library(shinydashboard)
library(DBI)
library(reshape2)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(rlist)
library(DT)
library(GenVisR)
library(survival)
library(ranger)
library(ggfortify)
library(survminer)
library(shiny)
library(BiocManager)
#setRepositories(addURLs = c(BioC = "https://bioconductor.org/packages/3.10/bioc"))
#options(repos = BiocInstaller::biocinstallRepos())
#getOption("repos")

Draw_barplot <- function(input_table,title_name = NULL, input_select = NULL){
  input_table <- input_table[order(-input_table$Freq),]
  if(is.element("Total",input_select) || is.null(input_select)){
    bpTable <- input_table
  }else{
    bpTable <- input_table[input_table$Var1%in%input_select,]
  }
  bp <- barplot(bpTable$Freq, xaxt="n", main = title_name,
                xlab="Sequence Alteration", ylab = 'Fraction of Mutation Type', angle = 45)
  text(x=bp, y=-0.1, bpTable$Var1, xpd=TRUE, srt=45)
  
  return(bp)
}

Draw_pieplot<-function(table){
  slices <<- table$Freq
  lbls <- as.character(table$Var1)
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) # add percents to labels
  lbls <<- paste(lbls,"%",sep="") # add % to labels
  return(0)
}

Data_path <- read.csv("path to the csv file named Data_path.csv")
Data_drug <- read.csv("path to the csv file named Data_drug.csv")
Data_intE <- read.csv("path to the csv file named Data_inte.csv")
var_data <- read.csv("path to the csv file named var_data.csv")
fourendpints <- read.csv('path to the csv file named 4endpoints.csv')
fourendpints$All <- 'All'

db <- 'TCGA_GENOMIC_CDM'
host_db <- "localhost"
db_port <- '5432'
db_user <- 'postgres' 
db_password <- 'Pillol@1'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)

Data_path <- dbGetQuery(con, 'SELECT P1.pathway_source_value, G.target_gene_source_value, V.hgvs_p, V.variant_feature, V.sequence_alteration, V.variant_occurrence_id, v.specimen_id
                              FROM pathway_occurrence P 
                              LEFT OUTER JOIN pathway_annotation P1 ON P.pathway_source_id = P1.pathway_source_id
                              LEFT OUTER JOIN target_gene G ON G.target_gene_id = P.target_gene_id
                              INNER JOIN variant_occurrence V on V.target_gene_id = G.target_gene_id')

Data_drug <- dbGetQuery(con, 'SELECT G.target_gene_source_value, G.druggability_source_value, VO.variant_feature, VO.sequence_alteration
                              FROM variant_occurrence VO
                              INNER JOIN target_gene G on VO.target_gene_id = G.target_gene_id')

parr = "'[NO INTERACTIONS AVAILABLE]'"
Data_intE <- dbGetQuery(con, paste0("SELECT i.target_gene_source_value, i.drug_source_value, i.interaction_source_value
                              FROM interaction_occurrence i
                              WHERE interaction_source_value <> ", parr))

var_data <-  dbGetQuery(con, 'SELECT *
                              FROM variant_occurrence')

Data_drug_inte <- merge(Data_drug[!duplicated(Data_drug$target_gene_source_value),], Data_intE, by = 'target_gene_source_value', all.x =TRUE)
Data_drug_inte$variant_feature <- NULL
Data_drug_inte$sequence_alteration <- NULL
colnames(Data_drug_inte) <- c('Hugo Symbol', 'Druggability', 'Drug Name', 'interaction Type')

list_path <- as.list(Data_path[!duplicated(Data_path$pathway_source_value)&!is.na(Data_path$pathway_source_value), 'pathway_source_value'])
list_path <- list.prepend(list_path, 'All')

list_gene_all <- as.list(Data_drug[!duplicated(Data_drug$target_gene_source_value), 'target_gene_source_value'])
list_gene_all <- list.prepend(list_gene_all, 'All')

list_gene <-  as.list(Data_path[!duplicated(Data_path$target_gene_source_value)&!is.na(Data_path$target_gene_source_value)&!is.na(Data_path$pathway_source_value), 'target_gene_source_value'])
list_gene <- list.prepend(list_gene, 'All')

list_gene_drug <- as.list(unique(Data_drug[which(Data_drug$druggability_source_value == 'DRUGGABLE GENOME'), 'target_gene_source_value']))
list_gene_drug <- list.prepend(list_gene_drug, 'All')

list_gene_int <- as.list(Data_drug_inte[!duplicated(Data_drug_inte$`Hugo Symbol`),'Hugo Symbol'])
list_gene_int <- list.prepend(list_gene_int, 'All')

list_gene_surv <- as.list(colnames(fourendpints))
list_gene_surv <- list_gene_surv[24:length(list_gene_surv)-1]
list_gene_surv <- list.prepend(list_gene_surv, "All")

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = 'TCGA LUSC DASHBOARD'),
    dashboardSidebar(sidebarMenu(
      menuItem("TUMOR BURDEN", tabName = "TUMOR", icon = icon("truck-loading")),
      menuItem("PATHWAY", tabName = "PATHWAY", icon = icon("project-diagram")),
      menuItem("DRUGGABILITY", tabName = "DRUGG", icon = icon("capsules")),
      menuItem("SURVIVAL ANALYSIS", tabName = "SURVIVAL", icon = icon("chart-line"))
    )),
    dashboardBody(
      tabItems(
               
        tabItem(tabName = "SURVIVAL",
                column(width = 2, (box(width = 16, heigth = 300, background = "yellow", selectInput(inputId = 'endpoint', label = 'Please select the endpoint to display', c('OS', 'DSS', 'PFI', 'DFI'), selected = 'OS'),
                selectInput(inputId = 'surv_gene', label = "Please select a gene", list_gene_surv, selected = "All")))),
                column(width = 10, plotOutput('survival'))),
        
        tabItem(tabName = "DRUGG", fluidRow(
                column(width = 6, (
                  box(height = 370, width = 12, plotOutput("drugpie"))),
                  box(height = 500, width = 12, plotOutput("genburd"))),
                column(width = 6,
                  box(height = 100,width = 12, background = "blue", selectInput("gene2",  'Select a gene', choices = list_gene_int, selected = 'All')), 
                  box(title = '', height = 770,width = 12, DT::dataTableOutput('table')))
                )),
        
        tabItem(tabName = "PATHWAY",
          fluidRow(column(width = 4, 
                          valueBoxOutput("pathway", width = 12)),
                   column(width = 4 , 
                           box(height = 100, width = 12, background = "green", selectInput('pathway', 'Select a pathway', choices = list_path))),
                   column(width = 4, 
                          box(height = 100, width = 12, background = "blue", selectInput('gene', 'Select a gene', choices = list_gene)))),
                  
          
          fluidRow(box(plotOutput('path'), width = 8, height = 700),
                          box(plotOutput('mut_gen'), height = 700, width = 4), plotOutput('gen_b'))
          ),
        
        tabItem(tabName = "TUMOR",
                fluidRow(column(width = 4, valueBoxOutput("patients", width = 12)),
                         column(width = 4, valueBoxOutput("genes", width = 12)),
                         column(width = 4, valueBoxOutput("variants", width = 12))), 
                fluidRow(column(width = 9, plotOutput('waterfall')), 
                         column(width = 3, selectInput('ggene', choices = list_gene_all, 'Select a gene', selected = 'All'), plotOutput('gene_b'))))
        
      )
    )
   )
 )

server <- function(input, output, session) {
  
  
  
  output$path <- renderPlot({
    
    Tbl4pathpie1 <- as.data.frame(table(Data_path[!duplicated(Data_path[c('pathway_source_value', 'target_gene_source_value')]),
                                                 'pathway_source_value']))
    tot <- sum(Tbl4pathpie1$Freq)
    
    if (input$pathway == 'All') {
      Tbl4pathpie1 <- Tbl4pathpie1[order(-Tbl4pathpie1$Freq),]
      Tbl4pathpie <- Tbl4pathpie1[1:30,]
    } else {
      Tbl4pathpie <- Tbl4pathpie1[which(Tbl4pathpie1$Var1 == input$pathway),]
    }
    if (input$gene != 'All') {
      to_filt <- as.list(unique(Data_path[which(Data_path$target_gene_source_value == input$gene), 'pathway_source_value']))
      Tbl4pathpie <- Tbl4pathpie1[Tbl4pathpie1$Var1 %in% to_filt,]
      if (length(Tbl4pathpie$Var1) > 30) {
        Tbl4pathpie <- Tbl4pathpie[order(-Tbl4pathpie$Freq),]
        Tbl4pathpie <- Tbl4pathpie[1:30,]
      }
    }
    parttot <- sum(Tbl4pathpie$Freq)
    difff <- tot - parttot
    ccc <- data.frame('Var1' = 'Others', 'Freq' = difff)
    Tbl4pathpie <- rbind(Tbl4pathpie, ccc)
    pct <- round(Tbl4pathpie$Freq/tot*100, digits = 2)
    Tbl4pathpie$Var1 <- paste(Tbl4pathpie$Var1, pct)
    Tbl4pathpie$Var1 <- paste(Tbl4pathpie$Var1,"%",sep="")
    col = rainbow(length(Tbl4pathpie$Var1))
    Tbl4pathpie <- Tbl4pathpie[order(-Tbl4pathpie$Freq),]
    ggplot(Tbl4pathpie, aes(x = "", y = Freq, fill = Var1))+
      geom_bar(stat="identity",width=1)+
      coord_polar("y",start = 0) +
      ggtitle("Gene Pathway")+
      theme_minimal() + theme(panel.grid=element_blank()) + 
      theme(axis.ticks = element_blank()) +
      theme(axis.text.x=element_blank()) +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank()) +
      geom_col(color = 'black', width = 1000)+
      theme(plot.title = element_text(hjust = 0.5, size = 30),legend.position="right") +
      theme(legend.title = element_text(size = 17)) +
      theme(legend.text = element_text(size = 12)) +
      labs(fill = "PATHWAY NAME") + 
      theme(legend.box.spacing = unit(0.4, "cm")) +
      guides(fill = guide_legend(ncol = 1))
    }, width = 1000, height = 650
  )
  
  output$pathway <- renderValueBox({
    valueBox(
      nrow(Data_path[!duplicated(Data_path$pathway_source_value),]), "pathways", icon = icon("bezier-curve"),
      color = "yellow"
    )
  })
  
  output$mut_gen <- renderPlot({
    
    Tbl4genebar1 <- as.data.frame(table(as.data.frame(Data_path[!duplicated(Data_path$variant_occurrence_id), 'target_gene_source_value'])))
    
    if (input$gene == 'All') {
      Tbl4genebar <- Tbl4genebar1[order(-Tbl4genebar1$Freq),]
      Tbl4genebar <- Tbl4genebar[1:30,]
    } else {
      Tbl4genebar <- Tbl4genebar1[which(Tbl4genebar1$Var1 == input$gene),]
    }
    if (input$pathway != 'All') {
      to_filt <- as.list(unique(Data_path[which(Data_path$pathway_source_value == input$pathway), 'target_gene_source_value']))
      Tbl4genebar <- Tbl4genebar1[Tbl4genebar1$Var1 %in% to_filt,]
      if (length(Tbl4genebar$Var1) > 30) {
        Tbl4genebar <- Tbl4genebar[order(-Tbl4genebar$Freq),]
        Tbl4genebar <- Tbl4genebar[1:30,]
      }
    }
    Tbl4genebar <- arrange(Tbl4genebar , Freq)
    Tbl4genebar$Var1<- factor(Tbl4genebar$Var1, levels = Tbl4genebar$Var1)
    ggplot(data=Tbl4genebar, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity") + coord_flip() + labs(x="Number of occurrences", y='Hugo Symbol') +
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 12))
    }, width = 500, height = 650
  )
  
  output$drugpie <- renderPlot({
    tbl4drugpie <- as.data.frame(table(Data_drug$druggability_source_value))
    tot <- sum(tbl4drugpie$Freq)
    tbl4drugpie$Freq <- tbl4drugpie$Freq
    pct <- round(tbl4drugpie$Freq/tot*100, digits = 2)
    tbl4drugpie$Var1 <- paste(tbl4drugpie$Var1, pct)
    tbl4drugpie$Var1 <- paste(tbl4drugpie$Var1,"%",sep="")
    col = rainbow(length(tbl4drugpie$Var1))
    tbl4drugpie <- tbl4drugpie[order(-tbl4drugpie$Freq),]
    ggplot(tbl4drugpie, aes(x = "", y = Freq, fill = Var1))+
      geom_bar(stat="identity",width=1)+
      coord_polar("y",start = 0) +
      ggtitle("Gene Druggability")+
      scale_fill_brewer() + theme_minimal() + theme(panel.grid=element_blank()) + 
      theme(axis.ticks = element_blank()) +
      theme(axis.text.x=element_blank()) +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank()) +
      geom_col(color = 'black', width = 1000)+
      theme(plot.title = element_text(hjust = 0.5, size = 30),legend.position="right") +
      theme(legend.text = element_text(size = 15)) +
      theme(legend.title = element_text(size = 20)) +
      labs(fill = "FEATURES") + 
      theme(legend.box.spacing = unit(0.4, "cm")) +
      guides(fill = guide_legend(ncol = 1) +
      theme(plot.title = element_text())
    )
  }, height = 350, width = 750)
  
  output$genburd <- renderPlot({
    Tbl4genebar2 <- as.data.frame(table(as.data.frame(Data_path[!duplicated(Data_path$variant_occurrence_id), 'target_gene_source_value'])))
    if (input$gene2 == 'All') {
      Tbl4genebar <- Tbl4genebar2[order(-Tbl4genebar2$Freq),]
      Tbl4genebar <- Tbl4genebar[1:20,]
    } else {
      Tbl4genebar <- Tbl4genebar2[which(Tbl4genebar2$Var1 == input$gene2),]}
    Tbl4genebar <- arrange(Tbl4genebar , Freq)
    Tbl4genebar$Var1<- factor(Tbl4genebar$Var1, levels = Tbl4genebar$Var1)
    ggplot(data=Tbl4genebar, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity") + coord_flip() + labs(x="Number of variants", y='Hugo Symbol') +
    ggtitle("Variant distribution") +
    theme(plot.title = element_text(size = 25, hjust = 0.5)) +
    theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
  },  height = 450)
  
  target <- reactive({if (input$gene2 == "All") {
    to_be_returned <- Data_drug_inte
  } else {
    to_be_returned <- Data_drug_inte[which(Data_drug_inte$'Hugo Symbol' == input$gene2),]
    }
  return(to_be_returned)})
  
  output$table <- DT::renderDataTable({target()}, options = list(scrollX = TRUE, pageLength = 15))
  
  output$patients <- renderValueBox({
    valueBox(
      '504', "patients", icon = icon("hospital"),
      color = "blue"
    )
  })
  output$genes <- renderValueBox({
    valueBox(
      nrow(Data_path[!duplicated(Data_path$target_gene_source_value),]), "genes", icon = icon("dna"),
      color = "green"
    )
  })
  output$variants <- renderValueBox({
    valueBox(
      nrow(Data_path[!duplicated(Data_path$hgvs_p),]), "variants", icon = icon("align-justify"),
      color = "yellow"
    )
  })
  
  output$waterfall <- renderPlot({
    inputData <- data.frame("sample" = Data_path$specimen_id,
                            "gene" = Data_path$target_gene_source_value,
                            "variant_class" = Data_path$variant_feature)
    colnames(inputData) <- c('sample', 'gene', 'variant_class')
    
    table <- as.data.frame(table(inputData$variant_class))
    table <- table[order(-table$Freq), ]
    most_deleterious <- as.character(table$Var1)
    
    waterfall(inputData, fileType="Custom", variant_class_order = most_deleterious, plotMutBurden = TRUE,
              mainXlabel = FALSE, maxGenes=30, mainGrid = TRUE, mainLabelSize = 1,
              plot_proportions = FALSE)
  }, height = 750, width = 1210)
  
  output$gene_b <- renderPlot({
    if (input$ggene != 'All') {
      tbl <- as.data.frame(table(var_data[which(var_data$target_gene_symbol == input$ggene), 'specimen_id']))
    } else {
    tbl <- as.data.frame(table(var_data$specimen_id))}
    tbl <- tbl[order(-tbl$Freq),]
    tbl$Freq <- tbl$Freq
    ggplot(data = tbl, aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat="identity", fill='blue') +  theme_bw() +
          coord_flip() + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
          labs(y ="Number of mutations") + ggtitle("Mutational Burden") + theme(plot.title = element_text(size = 15, hjust = 0.5)) +
          theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), panel.grid.major.x = element_line(size =0.5, colour = 'black'),
                panel.grid.minor.x = element_line(size =0.5, colour = 'black')) + theme(panel.border = element_blank())
  },  height = 670, width = 360)
  
  output$survival <- renderPlot({
      km_fit <- surv_fit(as.formula(paste0("Surv(" , input$endpoint, "_time,", input$endpoint, ") ~", input$surv_gene)), data = fourendpints)
      ggsurv <- ggsurvplot(km_fit,  data= fourendpints,  risk.table = TRUE, pval = TRUE, conf.int = TRUE, risk.table.y.text.col = TRUE, risk.table.col = 'black', font.main = c(18, "plain", "black"),
                           font.x = c(18, "plain", "black"), font.y = c(18, "plain", "black"), font.tickslab = c(16, "plain", "black"), risk.table.fontsize = 6, surv.median.line = "hv", font.legend = c(16,'plain'),
                           risk.table.title = "Number at risk by time")
                                                                                                                                                                                                        
      ggsurv$table <- ggsurv$table + theme_cleantable()
      ggsurv$table <- ggsurv$table + theme(axis.text.y = element_text(size = 16)) + theme(plot.title = element_text(size = 20))
      ggsurv
    }, height = 850)
}

shinyApp(ui, server)

