
library(maftools)

laml = read.maf(maf = '\\Users\\mellil\\Desktop\\TGCA data\\TCGA.LUSC.mutect.95258183-63ea-4c97-ae29-1bae9ed06334.DR-10.0.somatic.maf.gz', clinicalData = "\\Users\\mellil\\Desktop\\TGCA data\\clinical.cart.2020-01-09.tar\\Clinical3.tsv")
getSampleSummary(laml)
getGeneSummary(laml)
getFields(laml)

mafSurvival(maf = laml, genes = "SYNE1", time = 'days_to_last_follow_up', Status = 'censored_staus', isTCGA = FALSE)
prog_geneset = survGroup(maf = laml, top = 10, geneSetSize = 3, time = "days_to_last_follow_up", Status = "censored_staus", verbose = FALSE)
print(prog_geneset)
mafSurvGroup(maf = laml, geneSet = c("TTN", "TP53"), time = "days_to_last_follow_up", Status = "censored_staus")


plotmafSummary(maf = laml, rmOutlier = TRUE, addStat = 'median', dashboard = TRUE, titvRaw = FALSE)
oncoplot(maf = laml, top = 100)
oncostrip(maf = laml, genes = c('TTN','TP53', 'CSMD3'))

Clin = read.table("\\Users\\mellil\\Desktop\\TGCA data\\clinical.cart.2020-01-09.tar\\clinical.tsv", sep='\t')
Exposure = read.table("\\Users\\mellil\\Desktop\\TGCA data\\clinical.cart.2020-01-09.tar\\exposure.tsv", sep='\t')
Familyhist = read.table("\\Users\\mellil\\Desktop\\TGCA data\\clinical.cart.2020-01-09.tar\\family_history.tsv", sep='\t')
Sample = read.table("\\Users\\mellil\\Desktop\\TGCA data\\biospecimen.cart.2020-01-09.tar\\sample.tsv", sep='\t')

d = laml@data

table(d$Tumor_Sample_Barcode)
oncoplot(maf = laml, top = 10)
geneCloud(input = laml, minMut = 3)



Clin <- read.delim("\\Users\\mellil\\Desktop\\clinical.cases_selection.2020-02-22.tar\\clinical.tsv", header = TRUE, sep = "\t")
Clin2 <- Clin[unique(Clin$case_id),]

r <- as.data.frame(Clin_pat$bcr_patient_barcode)
colnames(r) <- c("submitter_id")
rr <- as.data.frame(Clin2$submitter_id)
colnames(rr) <- "submitter_id"
rr$date <- Clin2$days_to_birth
rr$ddd <- Clin2$submitter_id
rrr <- merge(x=rr, y=r, by="submitter_id", all.x=TRUE)
length(unique(rrr$submitter_id))
rrr <- rrr[unique(rrr$submitter_id),]


write.csv(rr, "\\Users\\mellil\\Desktop\\Clinical.csv")
write.csv(r, "\\Users\\mellil\\Desktop\\Clin_pat.csv")

day <- as.numeric(as.character(Clin$V24))
print(sum(is.na(day)))
day <- round(day/30/12)
median(day[which(!is.na(day))])        
table(Clin$V13[-1])
prop.table(table(Clin$V13[-1]))
sum(is.na(Clin$V13))

install.packages("devtools")
devtools::install_github("https://github.com/ABMI/GeneProfiler.git")
genomic::genomic()

Npat <- as.numeric(length(Clin$V1[-1]))
MedAgeDiagnosis <- median(day[which(!is.na(day))])
RangeAgeDia <- range(day[which(!is.na(day))])
Fem <- as.numeric(table(Clin$V13)[1])

Male <- as.numeric(table(Clin$V13)[3])
FemPer <- round(as.numeric(prop.table(table(Clin$V13[-1]))[1]*100))
MalPer <- round(as.numeric(prop.table(table(Clin$V13[-1]))[3]*100))
NoHistSmoke <- as.numeric(sum(Exposure$V30 == "--"))
HistSmoke <- as.numeric(sum(Exposure$V30 != "--"))
HistSmokePer <- HistSmoke/Npat*100
tt <- table(Clin$V16)
Asian <- as.numeric(tt[1])
AsianPer <- round(Asian/Npat*100)
BlackorAfro <- as.numeric(tt[2])
BlackorAfroPer <- round(BlackorAfro/Npat*100)
NotRep <- as.numeric(tt[3])
NotRepPer <- round(NotRep/Npat*100)
white <- as.numeric(tt[5])
whitePer <- round(white/Npat*100)
VitStatus <- as.nuemric(sum(Clin$V23 != "--"))
VitStatusPer <- VitStatus/Npat*100

library(tidyverse)

duplicated(Clin)
table(Sample$V14)
table(d$Variant_Type)
table(d$Hugo_Symbol[which (d$Hugo_Symbol == "TTN")])

namm <- Clin[1,]
names(Clin) <- lapply(namm, as.character)
Clin <- Clin[-1,]
Clin$Gender_concept_id[Clin$gender == "male"] <- 8507
Clin$Gender_concept_id[Clin$gender == "female"] <- 8532
table(Clin$Gender_concept_id)

namm <- Exposure[1,]
names(Exposure) <- lapply(namm, as.character)
Exposure <- Exposure[-1,]

table(Clin$race)
table(Clin$Race_concept_id)
Clin$Race_concept_id[Clin$race == "black or african american"] <- 8516
Clin$Race_concept_id[Clin$race == "asian"] <- 8515
Clin$Race_concept_id[Clin$race == "white"] <- 8527
Clin$Race_concept_id[Clin$race == "not reported"] <- "NA"

table(Clin$ethnicity)
Clin$Ethnicity_concept_id[Clin$ethnicity == "not reported"] <- "NA"
Clin$Ethnicity_concept_id[Clin$ethnicity == "hispanic or latino"] <- 38003563
Clin$Ethnicity_concept_id[Clin$ethnicity == "not hispanic or latino"] <- 38003564
table(Clin$Ethnicity_concept_id)

Clin$censored_staus[Clin$vital_status == "Alive"] <- 0
Clin$censored_staus[Clin$vital_status == "Dead"] <- 1
table(Clin$vital_status)
as.numeric(as.character(Clin$days_to_death))

library(survival)

Clin$FU_staus[Clin$vital_status == "Dead"] <- as.numeric(as.character(Clin$days_to_death[Clin$vital_status == "Dead"]))
Clin$FU_staus[Clin$vital_status == "Alive"] <- as.numeric(as.character(Clin$days_to_last_follow_up[Clin$vital_status == "Alive"]))

surv_object <- Surv(time = Clin$FU_staus, event = Clin$censored_staus)
km_fit <- survfit(surv_object  ~ 1, data = Clin)

summary(km_fit, times = c(1,30,60,90*(1:10)))
plot(km_fit, xlab="Days", main = 'Kaplan Meyer Plot')
hist(as.numeric(Clin$days_to_death))

Clin2 <- Clin[duplicated(Clin$submitter_id),]
table(Clin2$vital_status)

Clin2$FU_staus[Clin2$vital_status == "Dead"] <- as.numeric(as.character(Clin2$days_to_death[Clin2$vital_status == "Dead"]))
Clin2$FU_staus[Clin2$vital_status == "Alive"] <- as.numeric(as.character(Clin2$days_to_last_follow_up[Clin2$vital_status == "Alive"]))

surv_object <- Surv(time = Clin2$FU_staus, event = Clin2$censored_staus)
km_fit <- survfit(Surv(FU_staus, censored_staus) ~ race, data = Clin2)

summary(km_fit, times = c(1,30,60,90*(1:100)))
plot(km_fit, xlab="Days", main = 'Kaplan MeyerPlot', col=c(1,3))

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

autoplot(km_fit)


redd <- data.frame(d$case_id, d$Tumor_Sample_Barcode)
colnames(redd) <- c("case_id", "Tumor_Sample_Barcode")
total <- merge(x = Clin2, y = redd, by = "case_id")

dd <- d
tsb <- Clin2$Tumor_Sample_Barcode
sr <- Clin2$site_of_resection_or_biopsy

redClin <- data.frame(matrix(nrow = 497))
redClin$Tumor_Sample_Barcode <- tsb
redClin$site_of_resection_or_biopsy <- sr
redClin$matrix.nrow...497. <- NULL
dd <- merge(x = redClin, y = dd, by = "Tumor_Sample_Barcode")

genes <- unique(d$Hugo_Symbol)
tab <- table(dd$site_of_resection_or_biopsy)
tab
tot_num <- list()
lower <- list()
lungNOS <- list()
main <- list()
middle <- list()
over <- list()
upper <- list()
clow <- tab[1]
cNOS <- tab[2]
cmain <- tab[3]
cmid <- tab[4]
cover <- tab[5]
cup <- tab[7]

for (g in genes) {
  tot_num <- append(tot_num, length(which (dd$Hugo_Symbol == g)))
  lower <- append(lower, length(which (dd$Hugo_Symbol == g & dd$site_of_resection_or_biopsy == "Lower lobe, lung")))
  lungNOS <- append(lungNOS, length(which (dd$Hugo_Symbol == g & dd$site_of_resection_or_biopsy == "Lung, NOS")))
  main <- append(main, length(which (dd$Hugo_Symbol == g & dd$site_of_resection_or_biopsy == "Main bronchus")))
  middle <- append(middle, length(which (dd$Hugo_Symbol == g & dd$site_of_resection_or_biopsy == "Middle lobe, lung")))
  over <- append(over, length(which (dd$Hugo_Symbol == g & dd$site_of_resection_or_biopsy == "Overlapping lesion of lung")))
  upper <- append(upper, length(which (dd$Hugo_Symbol == g & dd$site_of_resection_or_biopsy == "Upper lobe, lung")))
}

mysummary <- data.frame(matrix(nrow = 16303))
mysummary$Hugo_symbol <- genes
mysummary$Total <- tot_num
mysummary$Lower_lob <- lower
mysummary$Lung_NOS <- lungNOS
mysummary$Main_bronchus <- main
mysummary$Middle_lob <- middle
mysummary$Overlapping_lesion <- over
mysummary$Upper_lob <- upper
mysummary$matrix.nrow...16303. <- NULL
mysummary <-mysummary[order(as.numeric(mysummary$Total), decreasing = TRUE),]

generep <-rep(genes, 6)
dattt <- c(lower, lungNOS, main, middle, over, upper)
type <- c(rep("Lower_lob", length(genes)), rep("Lung_NOS", length(genes)), rep("Main_bronchus", length(genes)), rep("Middle_lob", length(genes)), rep("Overlapping_lesion", length(genes)), rep("Upper_lobo", length(genes)))
mysummary2 <- data.frame(generep, dattt, type)

library(ggplot2)
p <-ggplot(mysummary2, aes(Hugo_symbol, Total))
p +geom_bar(stat = "identity", aes(fill = Upper_lob))

rm(Clin22)
rm(adult)
rm(a)
rm(aaa)
rm(numeric)
rm(data2)
rm(ggcount)
rm(cut)
rm(spam)
rm(laml)
rm(pp)

p <- (d[which(d$Hugo_Symbol == "TTN"),])
nrow(p[which(p$Variant_Classification == "Missense_Mutation")])

Clin3 <- merge(x = Clin2, y = rrrr, by.x = 'case_id', by.y = 'd$case_id')
rrrr <- as.data.frame(d$case_id)
colnames(rrrr) <- c("case_id")
rrrr$Tumor_Sample_Barcode <- d$Tumor_Sample_Barcode


library(dplyr) 
dd <- d  %>% distinct(case_id, .keep_all = TRUE)

Clin3 <- merge(x = Clin2, y = rrrr2, by = "case_id", all.x = TRUE)
rm(Clin3)

length(unique(p$Tumor_Sample_Barcode))
p[which(p$sample_submitter_id == "TCGA-63-A5"), Tumor_Sample_Barcode]

write.table(Cli3, "\\Users\\mellil\\Desktop\\TGCA data\\clinical.cart.2020-01-09.tar\\Clinical3.tsv")
follow_up <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\gdc_download_20200220_164943.604058.tar\\950030cb-9959-438a-8da6-dc32cc2571e6\\nationwidechildrens.org_clinical_follow_up_v1.0_lusc.txt", header = TRUE, sep = "\t")
