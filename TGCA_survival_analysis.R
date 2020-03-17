Clin_drug <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\clinical_detailed\\nationwidechildrens.org_clinical_drug_lusc.txt", header = TRUE, sep = "\t")
Clin_drug <- Clin_drug[3:nrow(Clin_drug),]

Clin_followup <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\clinical_detailed\\nationwidechildrens.org_clinical_follow_up_v1.0_lusc.txt", header = TRUE, sep = "\t")
Clin_followup <- Clin_followup[3:nrow(Clin_followup),]

Clin_nte <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\clinical_detailed\\nationwidechildrens.org_clinical_nte_lusc.txt", header = TRUE, sep = "\t")
Clin_nte <- Clin_nte[3:nrow(Clin_nte),]

Clin_omf <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\clinical_detailed\\nationwidechildrens.org_clinical_omf_v4.0_lusc.txt", header = TRUE, sep = "\t")
Clin_omf <- Clin_omf[3:nrow(Clin_omf),]

Clin_pat <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\clinical_detailed\\nationwidechildrens.org_clinical_patient_lusc.txt", header = TRUE, sep = "\t")
Clin_pat <- Clin_pat[3:nrow(Clin_pat),]

Clin_rad <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\clinical_detailed\\nationwidechildrens.org_clinical_radiation_lusc.txt", header = TRUE, sep = "\t")
Clin_rad <- Clin_rad[3:nrow(Clin_rad),]

Clin <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\Clinical_all\\clinical.tsv", header = TRUE, sep = "\t")
Clin <- Clin[!duplicated(Clin$case_id),]

Aliquot <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_aliquot_lusc.txt", header = TRUE, sep = "\t")
Aliquot <- Aliquot[-1,]

Analyte <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_analyte_lusc.txt", header = TRUE, sep = "\t")
Analyte <- Analyte[-1,]

Diagnostic.slides <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_diagnostic_slides_lusc.txt", header = TRUE, sep = "\t")
Diagnostic.slides <- Diagnostic.slides[-1,]

Portion <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_portion_lusc.txt", header = TRUE, sep = "\t")
Portion <- Portion[-1,]

Protocol <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_Protocol_lusc.txt", header = TRUE, sep = "\t")
Protocol <- Protocol[-1,]

Samples <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_sample_lusc.txt", header = TRUE, sep = "\t")
Samples <- Samples[-1,]

Shipment.portion <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_Shipment_portion_lusc.txt", header = TRUE, sep = "\t")
Shipment.portion <- Shipment.portion[-1,]

Slide <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_slide_lusc.txt", header = TRUE, sep = "\t")
Slide <- Slide[-1,]

Control <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_ssf_normal_controls_lusc.txt", header = TRUE, sep = "\t")
Control <- Control[-1,]

Tumor.sample <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_ssf_tumor_samples_lusc.txt", header = TRUE, sep = "\t")
Tumor.sample <- Control[-1,]


Col.remover <- function(df.name, null.name) {
  
  cn <- colnames(df.name)
  nr <- nrow(df.name)
  
  for (item in cn) {
    c <- as.integer(length(df.name[which (df.name[,item] == null.name), item]))
    if (c == nr) {
      df.name[,item] = NULL
    }
  }
  return(df.name)
}


Clin_drug <- Col.remover(Clin_drug, "[Not Available]")
Clin_followup <- Col.remover(Clin_followup, "[Not Available]")
Clin_nte <- Col.remover(Clin_nte, "[Not Available]")
Clin_omf <- Col.remover(Clin_omf, "[Not Available]")
Clin_pat <- Col.remover(Clin_pat, "[Not Available]")
Clin_rad <- Col.remover(Clin_rad, "[Not Available]")
Clin <- Col.remover(Clin, "--")
Aliquot <- Col.remover(Aliquot, "[Not Available]")
Analyte <- Col.remover(Analyte, "[Not Available]") 
Diagnostic.slides <- Col.remover(Diagnostic.slides, "[Not Available]")
Portion <- Col.remover(Portion, "[Not Available]")
Protocol <- Col.remover(Protocol, "[Not Available]")
Samples <- Col.remover(Samples, "[Not Available]")
Shipment.portion <- Col.remover(Shipment.portion, "[Not Available]")
Slide <- Col.remover(Slide, "[Not Available]")
Control <- Col.remover(Control, "[Not Available]")
Tumor.sample <- Col.remover(Tumor.sample, "[Not Available]")


write.csv(Clin_drug, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Clin_drug.csv")
write.csv(Clin_followup, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Clin_FU.csv")
write.csv(Clin_nte, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Clin_nte.csv")
write.csv(Clin_omf, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Clin_omf.csv")
write.csv(Clin_pat, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Clin_pat.csv")
write.csv(Clin_rad, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Clin_rad.csv")
write.csv(Clin, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Clin.csv")
write.csv(Aliquot, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Aliquot.csv")
write.csv(Analyte, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Analyte.csv")
write.csv(Diagnostic.slides, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Diagnostic_slides.csv")
write.csv(Portion, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Portion.csv")
write.csv(Protocol, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Protocol.csv")
write.csv(Samples, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Samples.csv")
write.csv(Shipment.portion, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Shipment_Portion.csv")
write.csv(Slide, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Slide.csv")
write.csv(Control, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Control.csv")
write.csv(Tumor.sample, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Tumor_samples.csv")



##################Survival Analysis##########################
  
fourendpints <- as.data.frame(Clin$submitter_id)
colnames(fourendpints) <- c("Submitter_id")
fourendpints$Age_at_dianosis <- as.numeric(as.character(Clin$age_at_diagnosis))
fourendpints$Age_at_dianosis <- round(as.integer(as.character(fourendpints$Age_at_dianosis)) / 365, digits = 0)
fourendpints$Gender <- toupper(Clin$gender)
fourendpints$Race <- toupper(Clin$race)
fourendpints$ajcc_pathologic_tumor_stage <- Clin$ajcc_pathologic_stage
fourendpints$Histological_type <- Clin$primary_diagnosis
fourendpints$Year_of_diagnosis <- as.numeric(as.character(Clin$year_of_diagnosis))
fourendpints$Vital_status <- Clin$vital_status
fourendpints$Death_days_to <- as.numeric(as.character(Clin$days_to_death))

sss <- as.data.frame(Clin_followup$bcr_patient_barcode)
colnames(sss) = c("Submitter_id")
sss$tumor_status <- Clin_followup$tumor_status
sss <- sss[!duplicated(Clin_followup$bcr_patient_barcode),]
sss1 <- as.data.frame(Clin_pat$bcr_patient_barcode)
colnames(sss1) = c("Submitter_id")
sss1$tumor_status <- Clin_pat$tumor_status
ss <- merge(sss1, sss, by = "Submitter_id", all.x = TRUE)
colnames(ss) <- c("Submitter_id", "Clin_pat", "Clin_followup")
ss$Tumor_status <- ifelse((ss$Clin_pat == "[Not Available]"|ss$Clin_pat == "[Unknown]"|ss$Clin_pat == "[Discrepancy]") & ss$Clin_followup != "[Unknown]" & ss$Clin_followup != "[Not Available]", as.character(ss$Clin_followup),as.character(ss$Clin_pat))
ss$Tumor_status <- ifelse(ss$Tumor_status == "[Not Available]"|ss$Tumor_status == "[Unknown]", NA, as.character(ss$Tumor_status))
ss$Clin_pat <- NULL
ss$Clin_followup <- NULL
fourendpints <- merge(x = fourendpints, y = ss, by="Submitter_id", all.x = TRUE)

sss <- as.data.frame(Clin_followup$bcr_patient_barcode)
colnames(sss) = c("Submitter_id")
sss$last_contact_days_to <- as.numeric(as.character(Clin_followup$last_contact_days_to))
sss <- sss[!duplicated(Clin_followup$bcr_patient_barcode),]
fourendpints <- merge(x = fourendpints, y = sss, by="Submitter_id", all.x = TRUE)

sss <- as.data.frame(Clin_followup$bcr_patient_barcode)
colnames(sss) = c("Submitter_id")
sss$new_tumor_event_type <- Clin_followup$new_tumor_event_type
sss <- sss[!duplicated(Clin_followup$bcr_patient_barcode),]
fourendpints <- merge(x = fourendpints, y = sss, by="Submitter_id", all.x = TRUE)

sss <- as.data.frame(Clin_followup$bcr_patient_barcode)
colnames(sss) = c("Submitter_id")
sss$new_tumor_event_dx_days_to <- Clin_followup$new_tumor_event_dx_days_to
sss <- sss[!duplicated(Clin_followup$bcr_patient_barcode),]
fourendpints <- merge(x = fourendpints, y = sss, by="Submitter_id", all.x = TRUE)

sss <- as.data.frame(Clin_followup$bcr_patient_barcode)
colnames(sss) = c("Submitter_id")
sss$treatment_outcome_first_course <- Clin_followup$treatment_outcome_first_course
sss <- sss[!duplicated(Clin_followup$bcr_patient_barcode),]
sss1 <- as.data.frame(Clin_pat$bcr_patient_barcode)
colnames(sss1) = c("Submitter_id")
sss1$tumor_status <- Clin_pat$treatment_outcome_first_course
ss <- merge(sss1, sss, by = "Submitter_id", all.x = TRUE)
colnames(ss) <- c("Submitter_id", "Clin_pat", "Clin_followup")
ss$treatment_outcome_first_course <- ifelse((ss$Clin_pat == "[Not Available]"|ss$Clin_pat == "[Unknown]"|ss$Clin_pat == "[Discrepancy]"|ss$Clin_pat == "[Not Applicable]"|ss$Clin_pat == "Progressive Disease") & ss$Clin_followup != "[Unknown]" & ss$Clin_followup != "[Not Available]", as.character(ss$Clin_followup),as.character(ss$Clin_pat))
ss$treatment_outcome_first_course <- ifelse(ss$treatment_outcome_first_course == "[Not Available]"|ss$treatment_outcome_first_course == "[Unknown]", NA, as.character(ss$treatment_outcome_first_course))
ss$Clin_pat <- NULL
ss$Clin_followup <- NULL
fourendpints <- merge(x = fourendpints, y = ss, by="Submitter_id", all.x = TRUE)

rm(ss)
rm(sss1)
rm(sss)

fourendpints$OS <- ifelse(fourendpints$Vital_status == "Dead", 1, 0)
fourendpints$OS_time <- ifelse(is.na(fourendpints$Death_days_to) == TRUE, fourendpints$last_contact_days_to, fourendpints$Death_days_to)

fourendpints$DSS <- ifelse(fourendpints$Tumor_status == "TUMOR FREE", 0, 1)
fourendpints$DSS_time <- fourendpints$OS_time

fourendpints$new_tumor_event_dx_days_to <- ifelse(fourendpints$new_tumor_event_dx_days_to == "[Not Available]"|fourendpints$new_tumor_event_dx_days_to == "[Not Applicable]", NA, as.character(fourendpints$new_tumor_event_dx_days_to))
fourendpints$DFI <- ifelse(fourendpints$treatment_outcome_first_course != "Complete Remission/Response", NA, ifelse(is.na(fourendpints$new_tumor_event_dx_days_to)==FALSE&fourendpints$new_tumor_event_type!="New Primary Tumor",1,0))
fourendpints$DFI_time <- ifelse(fourendpints$DFI == 1, as.numeric(as.character(fourendpints$new_tumor_event_dx_days_to)), ifelse(fourendpints$Vital_status=="Dead", as.numeric(as.character(fourendpints$Death_days_to)), as.numeric(as.character(fourendpints$last_contact_days_to))))

fourendpints$PFI <- ifelse(is.na(fourendpints$new_tumor_event_dx_days_to)==FALSE|(fourendpints$Vital_status=="WITH TUMOR"&fourendpints$Vital_status=="Dead"), 1,0)
fourendpints$PFI_time <- ifelse(fourendpints$PFI == 1, ifelse(is.na(fourendpints$new_tumor_event_dx_days_to)==FALSE, as.numeric(as.character(fourendpints$new_tumor_event_dx_days_to)), as.numeric(as.character(fourendpints$Death_days_to))),0)
fourendpints$PFI_time <- ifelse(fourendpints$PFI == 0, ifelse(fourendpints$Vital_status=="Dead", as.numeric(as.character(fourendpints$Death_days_to)), as.numeric(as.character(fourendpints$last_contact_days_to))), as.numeric(as.character(fourendpints$PFI_time)))

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

#surv_object <- Surv(time = fourendpints$PFI_time, event = fourendpints$PFI)
#km_fit <- survfit(surv_object  ~ 1, data = fourendpints)

#summary(km_fit, times = c(1,30,60,90*(1:10)))
#autoplot(km_fit)

#plot(km_fit, xlab="Days", main = 'Kaplan Meyer Plot')

#km_fit <- survfit(Surv(fourendpints$PFI_time, fourendpints$PFI)  ~TTN, data = fourendpints)
#autoplot(km_fit)

library(progress)
library(maftools)

laml = read.maf(maf = '\\Users\\mellil\\Desktop\\TGCA data\\TCGA.LUSC.mutect.95258183-63ea-4c97-ae29-1bae9ed06334.DR-10.0.somatic.maf.gz')
Gen_data <-  laml@data
rm(laml)

to_be_merged <- as.data.frame(Clin$case_id)
to_be_merged$Submitter_id <- Clin$submitter_id
colnames(to_be_merged) <- c("case_id", "Submitter_id")
Gen_data <- merge(x=Gen_data, y=to_be_merged, by="case_id", all.x = TRUE)
Gen_name <- Gen_data[!duplicated(Gen_data$Hugo_Symbol), "Hugo_Symbol"]

pb <- progress_bar$new(
  format = " PROGRESS [:bar] :percent ETA: :eta",
  total = nrow(Gen_name), clear = FALSE, width= 60)

for (item in Gen_name$Hugo_Symbol) {
  pb$tick()
  Gen_name[which(Gen_name$Hugo_Symbol == item), "Cunt"] <- nrow(Gen_data[which(Gen_data$Hugo_Symbol == item),])
}
Gen_name <- Gen_name[order(-Cunt),]
Gen_name <- Gen_name[1:200,]

pb <- progress_bar$new(
  format = " PROGRESS [:bar] :percent ETA: :eta",
  total = nrow(Gen_name), clear = FALSE, width= 60)

for (item in Gen_name$Hugo_Symbol){
  pb$tick()
  for (pat in fourendpints$Submitter_id) {
    fourendpints[which(fourendpints$Submitter_id == pat), item] <- ifelse(nrow(Gen_data[which(Gen_data$Submitter_id == pat),]) == 0, NA, ifelse(nrow(Gen_data[which(Gen_data$Submitter_id == pat & Gen_data$Hugo_Symbol == item),]) !=0, 'mut', 'wt'))
  }
}

d <- data.frame("All", 504)
colnames(d) <- c("Hugo_Symbol", "Cunt")
Gen_name <- rbind(d, Gen_name)
fourendpints$All <- "All"

for (item in Gen_name$Hugo_Symbol) {
  fourendpints[,item] <- as.factor(fourendpints[,item])
}

fourendpints[,"Gender"] <- as.factor(fourendpints[,"Gender"])
fourendpints[,"ajcc_pathologic_tumor_stage"] <- as.factor(fourendpints[,"ajcc_pathologic_tumor_stage"])
fourendpints[, "All"] <- as.factor(fourendpints[, "All"])

g <- data.frame("Gender", 504)
colnames(g) <- c("Hugo_Symbol", "Cunt")
Gen_name <- rbind(g, Gen_name)

g <- data.frame("ajcc_pathologic_tumor_stage", 504)
colnames(g) <- c("Hugo_Symbol", "Cunt")
Gen_name <- rbind(g, Gen_name)

fourendpints$OS_time <- as.numeric(as.character(fourendpints$OS_time))






