library(httr)
library(jsonlite)
library(xml2)
library(KEGGREST)
#BiocManager::install("KEGGREST")
#BiocManager::install("ReactomePA")
#library(ReactomePA)
#BiocManager::install("KEGGprofile")
library(KEGGprofile)
library(KEGGREST)
#install.packages('stringr')
library(progress)
#install.packages('rDGIdb')
library(DBI)
library(ggplot2)
library(ggrepel)
library(maftools)
library(progress)
library(zoo)

######DATA UPLOADER######

Clin <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\Clinical_all\\clinical.tsv", header = TRUE, sep = "\t")
Clin <- Clin[!duplicated(Clin$case_id),]
Clin <- Clin[order(Clin$submitter_id),]

Clin_pat <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\clinical_detailed\\nationwidechildrens.org_clinical_patient_lusc.txt", header = TRUE, sep = "\t")
Clin_pat <- Clin_pat[3:nrow(Clin_pat),]

Clin_followup <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\clinical_detailed\\nationwidechildrens.org_clinical_follow_up_v1.0_lusc.txt", header = TRUE, sep = "\t")
Clin_followup <- Clin_followup[3:nrow(Clin_followup),]
Clin_followup <- Clin_followup[order(Clin_followup$bcr_patient_barcode),]

Clin_nte <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\clinical_detailed\\nationwidechildrens.org_clinical_nte_lusc.txt", header = TRUE, sep = "\t")
Clin_nte <- Clin_nte[3:nrow(Clin_nte),]

Clin_omf <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\clinical_detailed\\nationwidechildrens.org_clinical_omf_v4.0_lusc.txt", header = TRUE, sep = "\t")
Clin_omf <- Clin_omf[3:nrow(Clin_omf),]

Clin_drug <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\clinical_detailed\\nationwidechildrens.org_clinical_drug_lusc.txt", header = TRUE, sep = "\t")
Clin_drug <- Clin_drug[3:nrow(Clin_drug),]

Clin_rad <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\clinical_detailed\\nationwidechildrens.org_clinical_radiation_lusc.txt", header = TRUE, sep = "\t")
Clin_rad <- Clin_rad[3:nrow(Clin_rad),]

Control <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_ssf_normal_controls_lusc.txt", header = TRUE, sep = "\t")
Control <- Control[3:nrow(Control),]
Control <- Control[!duplicated(Control$bcr_patient_barcode),]

Tumor_sample <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_ssf_tumor_samples_lusc.txt", header = TRUE, sep = "\t")
Tumor_sample <- Tumor_sample[3:nrow(Tumor_sample),]

Samples <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_sample_lusc.txt", header = TRUE, sep = "\t")
Samples <- Samples[-1,]

#voc <- read.csv("\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\CONCEPT.csv", header = TRUE)

######DIAGNOSIS TABLE############

DIA <- as.data.frame(Clin$submitter_id)
colnames(DIA) <- c('person_source_value')
DIA$year_of_diagnosis <- Clin$year_of_diagnosis
DIA$Date_of_diagnosis <- as.Date(as.Date(paste(DIA$year_of_diagnosis, paste('-01-01'), sep = '')) + as.numeric(sample(seq(1,365), 504, replace = TRUE)))

######PERSON TABLE######

Person <- as.data.frame(Clin$submitter_id)
colnames(Person) <- c('person_source_value')
Person$person_id <- as.numeric(sample(seq(1000000, 1009999), 504, replace = FALSE))
Person$gender_source_value <- Clin$gender
Person$race_source_value <- Clin$race
Person$ethnicity_source_value <- Clin$ethnicity
Person$birth_datetime <- as.Date(DIA$Date_of_diagnosis + as.numeric(as.character(Clin$days_to_birth)), "%Y-%m-%d")
Person$death_datetime <- as.Date(DIA$Date_of_diagnosis + as.numeric(as.character(Clin$days_to_death)), "%Y-%m-%d")
#Person$birth_datetime <- format(Person$birth_datetime, "%d-%m%-%Y")
#Person$death_datetime <- format(Person$death_datetime, "%d-%m%-%Y")
Person$year_of_birth <- as.numeric(format(Person$birth_datetime, "%Y"))
Person$month_of_birth <- as.numeric(format(Person$birth_datetime, "%m"))
Person$day_of_birth <- as.numeric(format(Person$birth_datetime, "%d"))
#Person$gender_source_concept_id <- Clin$gender
#Person$race_source_concept_id <- Clin$race
#Person$ethnicity_source_concept_id <- Clin$ethnicity
#Person$death_datetime <- ifelse(is.na(Person$death_datetime) == TRUE, "NULL", Person$death_datetime)
Person$year_of_birth <- ifelse(is.na(Person$year_of_birth)==TRUE, 0, Person$year_of_birth)

Person$gender_concept_id[Person$gender_source_value == "male"] <- 8507
Person$gender_concept_id[Person$gender_source_value == "female"] <- 8532
table(Person$gender_concept_id)

Person$race_concept_id[Person$race_source_value == "black or african american"] <- 8516
Person$race_concept_id[Person$race_source_value == "asian"] <- 8515
Person$race_concept_id[Person$race_source_value == "white"] <- 8527
Person$race_concept_id[Person$race_source_value == "not reported"] <- 0
table(Person$race_concept_id)

Person$ethnicity_concept_id[Person$ethnicity_source_value == "not reported"] <- 0
Person$ethnicity_concept_id[Person$ethnicity_source_value == "hispanic or latino"] <- 38003563
Person$ethnicity_concept_id[Person$ethnicity_source_value == "not hispanic or latino"] <- 38003564
table(Person$ethnicity_concept_id)

Person <- Person[order(Person$person_source_value),]

Person <- Person[c('person_id', 'gender_concept_id', 'year_of_birth', 'month_of_birth', 'day_of_birth', 'birth_datetime', 'death_datetime',
                   'race_concept_id', 'ethnicity_concept_id', "person_source_value", "gender_source_value", "race_source_value", 
                   "ethnicity_source_value")]

write.csv(Person, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Person.csv", row.names = FALSE)

######OBSERVATION PERIOD TABLE######

Observation_period <- as.data.frame(Clin[which(Clin$year_of_diagnosis != "--"), "submitter_id"])
colnames(Observation_period) <- c('person_source_value')
Observation_period <- merge(Observation_period, DIA[,c('person_source_value', 'Date_of_diagnosis')], by = 'person_source_value')
colnames(Observation_period) <- c('person_source_value', 'observation_period_start_date')

Observation_period <- merge(Observation_period, Clin_followup[!duplicated(Clin_followup$bcr_patient_barcode),c('bcr_patient_barcode', 'last_contact_days_to')], by.x = "person_source_value", by.y = 'bcr_patient_barcode', all.x = TRUE)
Observation_period <- merge(Observation_period, Clin[,c('submitter_id' , 'days_to_death')], by.x = "person_source_value", by.y = 'submitter_id')
Observation_period$observation_period_end_date <- ifelse(is.na(Observation_period$last_contact_days_to)==TRUE|Observation_period$last_contact_days_to == '[Not Available]'|Observation_period$last_contact_days_to == '[Discrepancy]', as.numeric(as.character(Observation_period$days_to_death)), as.numeric(as.character(Observation_period$last_contact_days_to)))
Observation_period$observation_period_end_date <- as.Date(Observation_period$observation_period_start_date + Observation_period$observation_period_end_date)
Observation_period$days_to_death <- NULL
Observation_period$last_contact_days_to <- NULL

Observation_period$observation_period_id <- as.numeric(sample(seq(1010000, 1019999), nrow(Observation_period), replace = FALSE))
Observation_period <- merge(Observation_period, Person[,c("person_source_value", "person_id")], by="person_source_value")
Observation_period$period_type_concept_id <- 44814724
Observation_period <- Observation_period[c('observation_period_id', 'person_id', 'observation_period_start_date', 'observation_period_end_date', 'period_type_concept_id')]

write.csv(Observation_period, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Observation_period.csv", row.names = FALSE)

###########VISIT OCCURENCE##############

visit_occurrence_dia <- as.data.frame(Person$person_id)
colnames(visit_occurrence_dia) <- c("person_id")
visit_occurrence_dia <- merge(visit_occurrence_dia, Observation_period[,c('person_id', 'observation_period_start_date')], by = "person_id")
colnames(visit_occurrence_dia) <- c("person_id", "visit_start_datetime")
visit_occurrence_dia$visit_start_datetime <- as.Date(visit_occurrence_dia$visit_start_datetime)
visit_occurrence_dia$visit_concept_id <- 32194
visit_occurrence_dia$visit_type_concept_id <- 44818517
visit_occurrence_dia$visit_source_value <- "DIAGNOSIS"
visit_occurrence_dia <- visit_occurrence_dia[which(is.na(visit_occurrence_dia$visit_start_datetime)==FALSE),]

visit_occurrence_FU <- as.data.frame(Clin_followup[which(is.na(Clin_followup$form_completion_date) == FALSE), 'bcr_patient_barcode'])
colnames(visit_occurrence_FU) <- c("person_source_value")
visit_occurrence_FU <- merge(visit_occurrence_FU, Person[,c('person_source_value', 'person_id')], by = "person_source_value")
visit_occurrence_FU$person_source_value <- NULL
visit_occurrence_FU$visit_start_datetime <- as.Date(Clin_followup$form_completion_date)
visit_occurrence_FU <- visit_occurrence_FU[which(is.na(visit_occurrence_FU$visit_start_datetime)==FALSE),]
visit_occurrence_FU$visit_concept_id <- 32194
visit_occurrence_FU$visit_type_concept_id <- 44818517
visit_occurrence_FU$visit_source_value <- "FU"

visit_occurrence_OMF <- as.data.frame(Clin_omf[which(Clin_omf$other_malignancy_dx_days_to != '[Not Available]'),'bcr_patient_barcode'])
colnames(visit_occurrence_OMF) <- c("person_source_value")
visit_occurrence_OMF <- merge(visit_occurrence_OMF, Person[,c('person_source_value', 'person_id')], by = "person_source_value")
#visit_occurrence_OMF$person_source_value <- NULL
visit_occurrence_OMF$days <- Clin_omf[which(Clin_omf$other_malignancy_dx_days_to != '[Not Available]'),'other_malignancy_dx_days_to']
visit_occurrence_OMF <- merge(visit_occurrence_OMF, Observation_period[,c("person_id", "observation_period_start_date")], by = 'person_id')
visit_occurrence_OMF$visit_start_datetime <- as.Date(visit_occurrence_OMF$observation_period_start_date) + as.numeric(as.character(visit_occurrence_OMF$days))
visit_occurrence_OMF$visit_concept_id <- 32194
visit_occurrence_OMF$visit_type_concept_id <- 44818517
visit_occurrence_OMF$visit_source_value <- "OMF"
visit_occurrence_OMF$days <- NULL
visit_occurrence_OMF$observation_period_start_date <- NULL
visit_occurrence_OMF$person_source_value <- NULL
visit_occurrence_OMF <- visit_occurrence_OMF[which(is.na(visit_occurrence_OMF$visit_start_datetime)==FALSE),]


visit_occurrence_NTE <-as.data.frame(Clin_followup[which(Clin_followup$new_tumor_event_dx_days_to != '[Not Applicable]'& Clin_followup$new_tumor_event_dx_days_to != '[Not Available]'),"bcr_patient_barcode"])
colnames(visit_occurrence_NTE) <- c("person_source_value")
visit_occurrence_NTE <- as.data.frame(visit_occurrence_NTE[!duplicated(visit_occurrence_NTE$person_source_value),])
colnames(visit_occurrence_NTE) <- c("person_source_value")
visit_occurrence_NTE <- merge(visit_occurrence_NTE, Person[,c('person_source_value', 'person_id')], by = "person_source_value")
visit_occurrence_NTE <- merge(visit_occurrence_NTE, Clin_followup[!duplicated(Clin_followup$bcr_patient_barcode),c('bcr_patient_barcode', 'new_tumor_event_dx_days_to')], by.x = "person_source_value", by.y = 'bcr_patient_barcode')
visit_occurrence_NTE <- merge(visit_occurrence_NTE, Observation_period[,c("person_id", "observation_period_start_date")], by = 'person_id', all.x = TRUE)
visit_occurrence_NTE$visit_start_datetime <- as.Date(visit_occurrence_NTE$observation_period_start_date) + as.numeric(as.character(visit_occurrence_NTE$new_tumor_event_dx_days_to))
visit_occurrence_NTE$visit_concept_id <- 32194
visit_occurrence_NTE$visit_type_concept_id <- 44818517
visit_occurrence_NTE$visit_source_value <- "NTE"
visit_occurrence_NTE$new_tumor_event_dx_days_to <- NULL
visit_occurrence_NTE$observation_period_start_date <- NULL
visit_occurrence_NTE$person_source_value <- NULL
visit_occurrence_NTE <- visit_occurrence_NTE[which(is.na(visit_occurrence_NTE$visit_start_datetime)==FALSE),]


visit_control <- as.data.frame(Control[which(Control$normal_control_procurement_days_to != '[Not Available]' & Control$normal_control_procurement_days_to != '[Completed]'),  'bcr_patient_barcode'])
colnames(visit_control) <- c("person_source_value")
visit_control$bcr_sample_uuid <- Control[which(Control$normal_control_procurement_days_to != '[Not Available]' & 
                                     Control$normal_control_procurement_days_to != '[Completed]'),  'bcr_sample_uuid']
visit_control <- merge(visit_control, Person[,c('person_source_value', 'person_id')], by = "person_source_value")
visit_control <- merge(visit_control, Control[which(Control$normal_control_procurement_days_to != '[Not Available]' 
                                                    & Control$normal_control_procurement_days_to != '[Completed]'), 
                                                    c('bcr_sample_uuid', 'normal_control_procurement_days_to')], 
                                                    by = 'bcr_sample_uuid')
visit_control <- merge(visit_control, Observation_period[, c('person_id', 'observation_period_start_date')], by = 'person_id')
visit_control$visit_start_datetime <- as.Date(visit_control$observation_period_start_date) + 
  as.numeric(as.character(visit_control$normal_control_procurement_days_to))
visit_control$visit_concept_id <- 32194
visit_control$visit_type_concept_id <- 44818517
visit_control$visit_source_value <- "CONTROL"
visit_control$normal_control_procurement_days_to <- NULL
visit_control$person_source_value <- NULL
visit_control$observation_period_start_date <- NULL
visit_control$bcr_sample_uuid <- NULL
visit_control <- visit_control[which(is.na(visit_control$visit_start_datetime)==FALSE),]


visit_tumor <- as.data.frame(Tumor_sample$bcr_patient_barcode)
colnames(visit_tumor) <- c("person_source_value")
visit_tumor <- merge(visit_tumor, Person[,c('person_source_value', 'person_id')], by = "person_source_value")
visit_tumor <- merge(visit_tumor, Tumor_sample[, c('bcr_patient_barcode', 'tumor_sample_procurement_days_to'),], by.x = 'person_source_value', by.y = 'bcr_patient_barcode')
visit_tumor <- merge(visit_tumor, Observation_period[, c('person_id', 'observation_period_start_date')], by = 'person_id')
visit_tumor$visit_start_datetime <- as.Date(visit_tumor$observation_period_start_date) + 
  as.numeric(as.character(visit_tumor$tumor_sample_procurement_days_to))
visit_tumor$visit_concept_id <- 32194
visit_tumor$visit_type_concept_id <- 44818517
visit_tumor$visit_source_value <- "TUMOR"
visit_tumor$tumor_sample_procurement_days_to <- NULL
visit_tumor$person_source_value <- NULL
visit_tumor$observation_period_start_date <- NULL
visit_tumor <- visit_tumor[which(is.na(visit_tumor$visit_start_datetime)==FALSE),]

visit_vs <- as.data.frame(Person$person_id)
colnames(visit_vs) <- c("person_id")
visit_vs <- merge(visit_vs, Observation_period[,c('person_id', 'observation_period_start_date')], by = "person_id", all.x = TRUE)
colnames(visit_vs) <- c("person_id", "dia")
visit_vs <- merge(visit_vs, Person[,c('person_id', 'person_source_value')], by = 'person_id')
visit_vs <- merge(visit_vs, Clin[, c("submitter_id", "days_to_last_follow_up")], by.x = 'person_source_value', by.y = 'submitter_id')
visit_vs <- merge(visit_vs, Clin[, c("submitter_id", "days_to_death")], by.x = 'person_source_value', by.y = 'submitter_id')
visit_vs <- merge(visit_vs, Clin[, c("submitter_id", "vital_status")], by.x = 'person_source_value', by.y = 'submitter_id')
visit_vs$days <- ifelse(visit_vs$vital_status == 'Dead', as.numeric(as.character(visit_vs$days_to_death)), as.numeric(as.character(visit_vs$days_to_last_follow_up)))
visit_vs$visit_start_datetime <- as.Date(visit_vs$dia) + 
  as.numeric(as.character(visit_vs$days))
#visit_vs <- merge(visit_vs, Clin_followup_last[,c("bcr_patient_barcode", "form_completion_date")], by.x = 'person_source_value', by.y = 'bcr_patient_barcode', all.x =TRUE)
#visit_vs$date <- ifelse(visit_vs$form_completion_date) == TRUE, as.Date(as.character(visit_vs$visit_start_datetime)), as.Date(as.character(visit_vs$form_completion_date)))
#visit_vs$date <- as.Date(visit_vs$date)
visit_vs$person_source_value <- NULL
visit_vs$dia <- NULL
visit_vs$days_to_last_follow_up <- NULL
visit_vs$days_to_death <- NULL
visit_vs$days <- NULL
visit_vs$form_completion_date <- NULL
visit_vs$vital_status <- NULL
colnames(visit_vs) <- c('person_id', 'visit_start_datetime')
visit_vs$visit_concept_id <- 32194
visit_vs$visit_type_concept_id <- 44818517
visit_vs$visit_source_value <- "VITAL_STATUS"

visit_occurrence <- as.data.frame(rbind(visit_occurrence_dia, visit_occurrence_FU, visit_occurrence_NTE, visit_occurrence_OMF, visit_control, visit_tumor, visit_vs))
dup <- visit_occurrence[duplicated(visit_occurrence[c('person_id', 'visit_start_datetime')]),]
dup <- dup[!duplicated(dup[c('person_id', 'visit_start_datetime', 'visit_source_value')]),]
visit_occurrence <- visit_occurrence[!duplicated(visit_occurrence[c('person_id', 'visit_start_datetime')]),]

v <- c('FU', 'OMF', 'NTE', 'CONTROL', 'TUMOR', 'VITAL_STATUS')
for (item in v) {
  visit_occurrence <- merge(visit_occurrence, dup[which(dup$visit_source_value == item), c('person_id', "visit_start_datetime", 'visit_source_value')], by = c('person_id', "visit_start_datetime"), all.x = TRUE)
}

colnames(visit_occurrence) <- c('person_id', 'visit_start_datetime', 'visit_concept_id', 'visit_type_concept_id', 'or', 'FU', "OMF", "NTE", 'CONTROL', 'TUMOR', "VITAL_STATUS")
visit_occurrence$visit_source_value <- paste(visit_occurrence$or , ifelse(is.na(visit_occurrence$FU)==FALSE, visit_occurrence$FU, ''), 
                                             ifelse(is.na(visit_occurrence$OMF)==FALSE, visit_occurrence$OMF, ''), 
                                             ifelse(is.na(visit_occurrence$NTE)==FALSE, visit_occurrence$NTE, ''),
                                             ifelse(is.na(visit_occurrence$CONTROL)==FALSE, visit_occurrence$CONTROL, ''),
                                             ifelse(is.na(visit_occurrence$TUMOR)==FALSE, visit_occurrence$TUMOR, ''),
                                             ifelse(is.na(visit_occurrence$VITAL_STATUS)==FALSE, visit_occurrence$VITAL_STATUS, ''))
for (item in v){
  visit_occurrence[item] <- NULL
}
visit_occurrence$or <- NULL
visit_occurrence$visit_occurrence_id <- as.numeric(sample(seq(1020000, 1029999), nrow(visit_occurrence), replace = FALSE))
visit_occurrence$visit_end_datetime <- visit_occurrence$visit_start_datetime
visit_occurrence <- visit_occurrence[c("visit_occurrence_id", "person_id", "visit_concept_id", "visit_start_datetime", "visit_end_datetime", "visit_type_concept_id", "visit_source_value")]

write.csv(visit_occurrence, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\tables\\Visit_occurrence.csv", row.names = FALSE)

##############CONDITION OCCURRENCE#############

Condition_occurrence <- as.data.frame(Clin[which(Clin$year_of_diagnosis!="--"), "submitter_id"])
colnames(Condition_occurrence) <- c("person_source_value")
Condition_occurrence <- merge(Condition_occurrence, Person[,c('person_source_value', 'person_id')], by = "person_source_value")
Condition_occurrence <- merge(Condition_occurrence, Clin[,c("submitter_id", "primary_diagnosis")], by.x = 'person_source_value' , by.y = 'submitter_id')
Condition_occurrence <- merge(Condition_occurrence, Clin[,c("submitter_id", "icd_10_code")], by.x = 'person_source_value' , by.y = 'submitter_id')
Condition_occurrence <- merge(Condition_occurrence, Observation_period[,c("person_id", "observation_period_start_date")], by  = 'person_id')
Condition_occurrence$condition_type_concept_id <- 44786627
Condition_occurrence$observation_period_start_date <- as.Date(Condition_occurrence$observation_period_start_date)
visit_occurrence$visit_start_datetime <- as.Date(visit_occurrence$visit_start_datetime)
Condition_occurrence <- merge(Condition_occurrence, visit_occurrence[grep("DIAGNOSIS", visit_occurrence$visit_source_value),c("visit_occurrence_id", "person_id", "visit_start_datetime")],
                             by.x = c("person_id", "observation_period_start_date"), by.y = c("person_id", "visit_start_datetime"))

Condition_occurrence$condition_concept_id <- ifelse(Condition_occurrence$icd_10_code == "C34.0", 1567487,
                                                    ifelse(Condition_occurrence$icd_10_code == "C34.1", 1567488,
                                                          ifelse(Condition_occurrence$icd_10_code == "C34.10", 45571493,
                                                                 ifelse(Condition_occurrence$icd_10_code == "C34.2", 35206184,
                                                                        ifelse(Condition_occurrence$icd_10_code == "C34.3", 1567489,
                                                                               ifelse(Condition_occurrence$icd_10_code == "C34.8", 1567490,
                                                                                      ifelse(Condition_occurrence$icd_10_code == "C34.9", 1567491, 0)))))))

Condition_occurrence <- merge(Condition_occurrence, Clin[,c("submitter_id", "ajcc_pathologic_stage")], by.x = "person_source_value", by.y = "submitter_id")

Condition_occurrence$condition_status_concept_id <- ifelse(Condition_occurrence$ajcc_pathologic_stage == "Stage I", 45880980,
                                                    ifelse(Condition_occurrence$ajcc_pathologic_stage == "Stage IA", 45878383,
                                                           ifelse(Condition_occurrence$ajcc_pathologic_stage == "Stage IB", 45876317,
                                                                  ifelse(Condition_occurrence$ajcc_pathologic_stage == "Stage II", 45880979,
                                                                         ifelse(Condition_occurrence$ajcc_pathologic_stage == "Stage IIA", 45876316,
                                                                                ifelse(Condition_occurrence$ajcc_pathologic_stage == "Stage IIB", 45882497,
                                                                                       ifelse(Condition_occurrence$ajcc_pathologic_stage == "Stage III", 45878643, 
                                                                                              ifelse(Condition_occurrence$ajcc_pathologic_stage == "Stage IIIA", 45880978,
                                                                                                     ifelse(Condition_occurrence$ajcc_pathologic_stage == "Stage IIIB", 45884285,
                                                                                                            ifelse(Condition_occurrence$ajcc_pathologic_stage == "Stage IV", 45880109,0))))))))))

Condition_occurrence$person_source_value <- NULL
colnames(Condition_occurrence) <- c('person_id', 'condition_start_datetime', 'condition_source_value','condition_source_concept_id', 'condition_type_concept_id', 
                                    'visit_occurrence_id', 'condition_concept_id', 'condition_status_source_value', 'condition_status_concept_id')
Condition_occurrence <- Condition_occurrence[, c('person_id', 'condition_concept_id', "condition_start_datetime", "condition_type_concept_id",
                                                 "condition_status_concept_id", "visit_occurrence_id", "condition_source_value", "condition_source_concept_id", "condition_status_source_value")]

Condition_nte <- as.data.frame(Clin_followup[which(Clin_followup$new_tumor_event_dx_days_to != '[Not Applicable]'& Clin_followup$new_tumor_event_dx_days_to != '[Not Available]'),"bcr_patient_barcode"])
colnames(Condition_nte) <- c("person_source_value")
Condition_nte <- as.data.frame(Condition_nte[!duplicated(Condition_nte$person_source_value),])
colnames(Condition_nte) <- c("person_source_value")
Condition_nte <- merge(Condition_nte, Person[,c('person_source_value', 'person_id')], by = "person_source_value")
Condition_nte <- merge(Condition_nte,Clin_followup[!duplicated(Clin_followup$bcr_patient_barcode),c("bcr_patient_barcode", "new_tumor_event_type")], by.x = 'person_source_value' , by.y = 'bcr_patient_barcode')
Condition_nte$new_tumor_event_type <- paste0(Condition_nte$new_tumor_event_type, "_nte")
Condition_nte <- merge(Condition_nte, visit_occurrence[grep("NTE", visit_occurrence$visit_source_value),c('person_id', "visit_occurrence_id", "visit_start_datetime")], by='person_id')
#Condition_nte[which(Condition_nte$person_id == 13760), "new_tumor_event_type"] <- "Locoregional Recurrence"  
#Condition_nte[which(Condition_nte$person_id == 14614), "new_tumor_event_type"] <- "Locoregional Recurrence"
#tobeappend <- c(13760,"TCGA-58-A46K", "Distant Metastasis", 30466, '2010-07-06')
#Condition_nte <- rbind(Condition_nte, tobeappend)
#tobeappend <- c(14614,"TCGA-58-A46K", "Distant Metastasis", 37621, '2011-10-28')
#Condition_nte <- rbind(Condition_nte, tobeappend)
Condition_nte$condition_concept_id <- ifelse(Condition_nte$new_tumor_event_type == "Distant Metastasis" | Condition_nte$new_tumor_event_type == "Locoregional Recurrence|Distant Metastasis", 4163446,
                                                    ifelse(Condition_nte$new_tumor_event_type == "New Primary Tumor" | Condition_nte$new_tumor_event_type == "Distant Metastasis|New Primary Tumor", 439392,
                                                           ifelse(Condition_nte$new_tumor_event_type == "Locoregional Recurrence", 4198434, 0)))
Condition_nte$condition_type_concept_id <- 44786629
Condition_nte$condition_status_concept_id <- 0
Condition_nte$condition_status_source_value <- 0
Condition_nte$condition_status_source_concept_id <- 0

Condition_nte$person_source_value <- NULL
colnames(Condition_nte) <- c('person_id', 'condition_source_value', 'visit_occurrence_id','condition_start_datetime','condition_concept_id', 'condition_type_concept_id', 
                                    'condition_status_concept_id','condition_status_source_value','condition_source_concept_id')
Condition_nte <- Condition_nte[, c('person_id', 'condition_concept_id', "condition_start_datetime", "condition_type_concept_id",
                                                 "condition_status_concept_id", "visit_occurrence_id", "condition_source_value", "condition_source_concept_id", "condition_status_source_value")]

Condition_omf <- as.data.frame(Clin_omf$bcr_patient_barcode)
colnames(Condition_omf) <- c("person_source_value")
Condition_omf$bcr_omf_barcode <- Clin_omf$bcr_omf_barcode
Condition_omf$days <- Clin_omf$other_malignancy_dx_days_to
Condition_omf$days <- ifelse(Condition_omf$days == "[Not Available]", NA, as.numeric(as.character(Condition_omf$days)))
Condition_omf <- merge(Condition_omf, Person[,c('person_source_value', 'person_id')], by = "person_source_value")
Condition_omf <- merge(Condition_omf, Observation_period[,c("person_id", "observation_period_start_date")], by='person_id', all.x = TRUE)
Condition_omf$date <- as.numeric(as.character(Condition_omf$days)) + as.Date(Condition_omf$observation_period_start_date)
Condition_omf$days <- NULL
Condition_omf$observation_period_start_date <- NULL
Condition_omf <- merge(Condition_omf, Clin_omf[,c("bcr_patient_barcode", "other_malignancy_histological_type", "bcr_omf_barcode")], by.x = c('person_source_value', 'bcr_omf_barcode') , by.y = c('bcr_patient_barcode', 'bcr_omf_barcode'))
Condition_omf$other_malignancy_histological_type <- paste0(Condition_omf$other_malignancy_histological_type, "_omf")
Condition_omf <- merge(Condition_omf, visit_occurrence[grep("OMF", visit_occurrence$visit_source_value),c('person_id', "visit_occurrence_id", "visit_start_datetime")], by.x = c('person_id', 'date'), by.y = c('person_id', 'visit_start_datetime') , all.x = TRUE)


Condition_omf$condition_concept_id <- ifelse(Condition_omf$other_malignancy_histological_type == "Basaloid Squamous Cell", 4112752,
                                             ifelse(Condition_omf$other_malignancy_histological_type == "Colon Adenocarcinoma", 197500,
                                                    ifelse(Condition_omf$other_malignancy_histological_type == "Hepatocellular Carcinoma", 4001171,
                                                           ifelse(Condition_omf$other_malignancy_histological_type == "Lung Adenocarcinoma Mixed Subtype"|
                                                                    Condition_omf$other_malignancy_histological_type == "Lung Clear Cell Squamous Cell Carcinoma"|
                                                                    Condition_omf$other_malignancy_histological_type == "Lung Adenocarcinoma- Not Otherwise Specified (NOS)", 4311499,
                                                                  ifelse(Condition_omf$other_malignancy_histological_type == "Clear Cell Squamous Cell"|
                                                                           Condition_omf$other_malignancy_histological_type == "Small Cell Squamous Cell"|
                                                                           Condition_omf$other_malignancy_histological_type == "Squamous Cell Carcinoma, Not Otherwise Specified", 4111921,
                                                                         ifelse(Condition_omf$other_malignancy_histological_type == "Adenosquamous", 4247238,0))))))
Condition_omf$condition_type_concept_id <- 44786629
Condition_omf$condition_status_source_value <- Clin_omf$ajcc_pathologic_tumor_stage
Condition_omf$condition_status_concept_id <- ifelse(Condition_omf$condition_status_source_value == "Stage I", 45880980,
                                                           ifelse(Condition_omf$condition_status_source_value == "Stage IA", 45878383,
                                                                  ifelse(Condition_omf$condition_status_source_value == "Stage IB", 45876317,
                                                                         ifelse(Condition_omf$condition_status_source_value == "Stage II", 45880979,
                                                                                ifelse(Condition_omf$condition_status_source_value == "Stage IIA", 45876316,
                                                                                       ifelse(Condition_omf$condition_status_source_value == "Stage IIB", 45882497,
                                                                                              ifelse(Condition_omf$condition_status_source_value == "Stage III", 45878643, 
                                                                                                     ifelse(Condition_omf$condition_status_source_value == "Stage IIIA", 45880978,
                                                                                                            ifelse(Condition_omf$condition_status_source_value == "Stage IVA", 45880108,
                                                                                                                   ifelse(Condition_omf$condition_status_source_value == "Stage IV", 45880109, 0))))))))))
Condition_omf$condition_source_concept_id <- 0
Condition_omf$person_source_value <- NULL
Condition_omf$bcr_omf_barcode <- NULL
colnames(Condition_omf) <- c('person_id', 'condition_start_datetime', 'condition_source_value' , 'visit_occurrence_id' ,'condition_concept_id', 'condition_type_concept_id', 'condition_status_source_value',
                             'condition_status_concept_id','condition_source_concept_id')
Condition_omf <- Condition_omf[, c('person_id', 'condition_concept_id', "condition_start_datetime", "condition_type_concept_id",
                                   "condition_status_concept_id", "visit_occurrence_id", "condition_source_value", "condition_source_concept_id", "condition_status_source_value")]

Condition_occurrence <- rbind(Condition_occurrence, Condition_nte, Condition_omf)
Condition_occurrence$condition_occurrence_id <- as.numeric(sample(seq(1030000, 1039999), nrow(Condition_occurrence), replace = FALSE))
Condition_occurrence <- Condition_occurrence[, c('condition_occurrence_id', 'person_id', 'condition_concept_id', "condition_start_datetime", "condition_type_concept_id",
                                   "condition_status_concept_id", "visit_occurrence_id", "condition_source_value", "condition_source_concept_id", "condition_status_source_value")]

write.csv(Condition_occurrence, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Condition_occurrence.csv", row.names = FALSE)

###############SPECIMEN TABLE###################

Samples <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_sample_lusc.txt", header = TRUE, sep = "\t")
Samples <- Samples[-1,]

Aliquot <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_aliquot_lusc.txt", header = TRUE, sep = "\t")
Aliquot <- Aliquot[-1,]

Slide <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_slide_lusc.txt", header = TRUE, sep = "\t")
Slide <- Slide[-1,]

Diagnostic.slides <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_diagnostic_slides_lusc.txt", header = TRUE, sep = "\t")
Diagnostic.slides <- Diagnostic.slides[-1,]

Portion <- read.delim("\\Users\\mellil\\Desktop\\TGCA data\\specimen\\nationwidechildrens.org_biospecimen_portion_lusc.txt", header = TRUE, sep = "\t")
Portion <- Portion[-1,]

laml = read.maf(maf = '\\Users\\mellil\\Desktop\\TGCA data\\TCGA.LUSC.mutect.95258183-63ea-4c97-ae29-1bae9ed06334.DR-10.0.somatic.maf.gz')
gen_data <- laml@data
rm(laml)

Specimen_control <- as.data.frame(Control$bcr_patient_barcode)
colnames(Specimen_control) <- c('person_source_value')
Specimen_control$sample_uuid <- Control$bcr_sample_uuid
Specimen_control <- merge(Specimen_control, Person[,c('person_source_value', 'person_id')], by = "person_source_value")
Specimen_control <- merge(Specimen_control, Clin[, c('submitter_id', 'case_id')], by.x = 'person_source_value', by.y = 'submitter_id')
Specimen_control <- merge(Specimen_control, gen_data[which(!duplicated(gen_data$case_id)), c('case_id', 'Matched_Norm_Sample_Barcode')], by = 'case_id', all.x = TRUE)
Specimen_control <- merge(Specimen_control, Samples[,c('bcr_sample_uuid', "sample_type")], by.y = 'bcr_sample_uuid', by.x = 'sample_uuid')
Specimen_control$specimen_concept_id <- ifelse(Specimen_control$sample_type == 'Blood Derived Normal', 4001225, 4122248)
Specimen_control$specimen_type_concept_id <- 581378
Specimen_control <- merge(Specimen_control, Observation_period[,c('person_id', "observation_period_start_date")], by = 'person_id', all.x = TRUE)
Specimen_control <- merge(Specimen_control, Control[,c('bcr_sample_uuid', "normal_control_procurement_days_to")], by.y = 'bcr_sample_uuid', by.x = 'sample_uuid')
Specimen_control$specimen_datetime <- as.Date(as.numeric(as.character(Specimen_control$normal_control_procurement_days_to)) + Specimen_control$observation_period_start_date)
Specimen_control$disease_status_concept_id <- 4069590
Specimen_control <- merge(Specimen_control, Control[,c('bcr_sample_uuid','normal_tissue_anatomic_site', "normal_control_site_other")], by.y = 'bcr_sample_uuid', by.x = 'sample_uuid')
Specimen_control$anatomic_site_concept_id <- ifelse(Specimen_control$normal_tissue_anatomic_site == "Lung" | Specimen_control$normal_control_site_other == 'Lung'|
                                                      (Specimen_control$normal_tissue_anatomic_site == "Other" & Specimen_control$normal_control_site_other == 'Distal Lung'), 44516275, 
                                                        ifelse(Specimen_control$normal_tissue_anatomic_site == "Other" & (Specimen_control$normal_control_site_other == 'Distal portion of lobe'|
                                                                                                                            grepl("Inferior lobe", Specimen_control$normal_control_site_other)==TRUE), 4155435, 
                                                               ifelse(Specimen_control$normal_tissue_anatomic_site == "Other" & Specimen_control$normal_control_site_other == 'Left upper and lower',  4155435,
                                                                      ifelse(Specimen_control$normal_tissue_anatomic_site == "Other" & Specimen_control$normal_control_site_other == 'L unknown', 4195613,
                                                                             ifelse(Specimen_control$normal_tissue_anatomic_site == "Other" & Specimen_control$normal_control_site_other == 'R unknown', 4141610, 
                                                                                    ifelse(Specimen_control$normal_tissue_anatomic_site == "Other" & Specimen_control$normal_control_site_other == 'Right - upper/middle lobes', 4075420,
                                                                                           ifelse(Specimen_control$normal_tissue_anatomic_site == "Other" & Specimen_control$normal_control_site_other == 'Right' , 4141610, 
                                                                                                  ifelse(Specimen_control$normal_tissue_anatomic_site == "Other" & grepl("Superior lobe", Specimen_control$normal_control_site_other)==TRUE, 4155435, NA))))))))

Specimen_control$normal_tissue_anatomic_site <- ifelse(Specimen_control$normal_tissue_anatomic_site == "Lung", as.character(Specimen_control$normal_tissue_anatomic_site), as.character(Specimen_control$normal_control_site_other))
Specimen_control$normal_tissue_anatomic_site <- ifelse(Specimen_control$normal_tissue_anatomic_site == '[Not Available]'|Specimen_control$normal_tissue_anatomic_site == '[Not Applicable]', NA, as.character(Specimen_control$normal_tissue_anatomic_site))
Specimen_control$normal_control_site_other <- NULL
Specimen_control$person_source_value <- NULL
Specimen_control$sample_uuid <- NULL
Specimen_control$case_id <- NULL
Specimen_control$normal_control_procurement_days_to <- NULL
Specimen_control$observation_period_start_date <- NULL

colnames(Specimen_control) <- c('person_id', 'specimen_source_id', 'specimen_source_value', 'specimen_concept_id', 'specimen_type_concept_id',
                                'specimen_datetime', 'disease_status_concept_id', 'anatomic_site_source_value', 'anatomic_site_concept_id')
Specimen_control <- Specimen_control[,c('person_id', 'specimen_concept_id', "specimen_type_concept_id", "specimen_datetime", "anatomic_site_concept_id",
                                        "disease_status_concept_id", "specimen_source_id", "specimen_source_value", "anatomic_site_source_value")]


Specimen_tumor <- as.data.frame(Tumor_sample$bcr_patient_barcode)
colnames(Specimen_tumor) <- c('person_source_value')
Specimen_tumor$sample_uuid <- Tumor_sample$bcr_sample_uuid
Specimen_tumor <- merge(Specimen_tumor, Person[,c('person_source_value', 'person_id')], by = "person_source_value")
Specimen_tumor <- merge(Specimen_tumor, Clin[, c('submitter_id', 'case_id')], by.x = 'person_source_value', by.y = 'submitter_id')
Specimen_tumor <- merge(Specimen_tumor, gen_data[which(!duplicated(gen_data$case_id)), c('case_id', 'Tumor_Sample_Barcode')], by = 'case_id', all.x = TRUE)
Specimen_tumor <- merge(Specimen_tumor, Samples[,c('bcr_sample_uuid', "sample_type")], by.y = 'bcr_sample_uuid', by.x = 'sample_uuid')
Specimen_tumor$specimen_concept_id <- 4122248
Specimen_tumor$specimen_type_concept_id <- 581378
Specimen_tumor <- merge(Specimen_tumor, Observation_period[,c('person_id', "observation_period_start_date")], by = 'person_id', all.x = TRUE)
Specimen_tumor <- merge(Specimen_tumor, Tumor_sample[,c('bcr_sample_uuid', "tumor_sample_procurement_days_to")], by.y = 'bcr_sample_uuid', by.x = 'sample_uuid')
Specimen_tumor$specimen_datetime <- as.Date(as.numeric(as.character(Specimen_tumor$tumor_sample_procurement_days_to)) + Specimen_tumor$observation_period_start_date)
Specimen_tumor$disease_status_concept_id <- 4066212
Specimen_tumor <- merge(Specimen_tumor, Tumor_sample[,c('bcr_sample_uuid','cancer_procurement_method_other', "tumor_sample_procurement_method")], by.y = 'bcr_sample_uuid', by.x = 'sample_uuid')
Specimen_tumor$anatomic_site <- ifelse(Specimen_tumor$tumor_sample_procurement_method == 'Tumor Resection', as.character(Specimen_tumor$tumor_sample_procurement_method), as.character(Specimen_tumor$cancer_procurement_method_other))
Specimen_tumor <- merge(Specimen_tumor, Tumor_sample[,c('bcr_sample_uuid','laterality', "site_of_disease_description")], by.y = 'bcr_sample_uuid', by.x = 'sample_uuid')
Specimen_tumor$cancer_procurement_method_other <- NULL
Specimen_tumor$tumor_sample_procurement_method <- NULL
Specimen_tumor$tumor_sample_procurement_days_to <- NULL
Specimen_tumor$sample_type <- NULL
Specimen_tumor$observation_period_start_date <- NULL
Specimen_tumor$anatomic_site_concept_id <- ifelse(Specimen_tumor$anatomic_site == 'lobectomy' | Specimen_tumor$anatomic_site == 'Lobectomy' | Specimen_tumor$anatomic_site == 'LOBECTOMY' | Specimen_tumor$anatomic_site == 'Lobectomy with mediastinal lymph node dissection', 4075096 ,
                                                  ifelse(Specimen_tumor$anatomic_site == 'L Pneumonectomy' | Specimen_tumor$anatomic_site == 'Left pneumonectomy ', 4176336,
                                                         ifelse(Specimen_tumor$anatomic_site == 'L lower lobectomy' | Specimen_tumor$anatomic_site == 'Left Lower Lobectomy and LN Dissection' | 
                                                                  Specimen_tumor$anatomic_site == 'R lower lobectomy' | Specimen_tumor$anatomic_site == 'right lower lobectomy' |
                                                                  Specimen_tumor$anatomic_site == 'Right Lower Lobectomy' | Specimen_tumor$anatomic_site == 'Right lower lobectomy and lypmh node dissection', 4122795,
                                                                ifelse(Specimen_tumor$anatomic_site == 'Completion Lt. Pneumonectomy'| Specimen_tumor$anatomic_site == 'Pnemonectomy' |  Specimen_tumor$anatomic_site == 'Pneumonectomy', 4111459, 
                                                                       ifelse(Specimen_tumor$anatomic_site == 'R Pneumonectomy', 4179408, 
                                                                              ifelse(Specimen_tumor$anatomic_site == 'L upper lobectomy'| Specimen_tumor$anatomic_site == 'left upper lobectomy'| Specimen_tumor$anatomic_site == 'Left Upper Lobectomy'| Specimen_tumor$anatomic_site == 'R-upper lobe removed'| Specimen_tumor$anatomic_site == 'R upper lobectomy'| Specimen_tumor$anatomic_site == 'right upper lobectomy'|
                                                                                       Specimen_tumor$anatomic_site == 'Right upper lobectomy and rib resection and LN dissection', 4117609,
                                                                                     ifelse(Specimen_tumor$anatomic_site == 'Bilobectomy' | Specimen_tumor$anatomic_site == 'Resection of lobe or bilobectomy, but less than the whole lung (partial pneumonectomy)' | Specimen_tumor$anatomic_site == 'Segmental Resection' | Specimen_tumor$anatomic_site == 'Segmentectomy' |  Specimen_tumor$anatomic_site == 'SEGMENTECTOMY' | 
                                                                                            Specimen_tumor$anatomic_site == 'Video-Assisted Thoracoscopy exploration with subsequent left thoracotomy and left lower lobectomy' | Specimen_tumor$anatomic_site == 'wedge resection', 44516275, NA)))))))

Specimen_tumor$anatomic_site_concept_id <- ifelse(Specimen_tumor$anatomic_site != 'Tumor Resection', Specimen_tumor$anatomic_site_concept_id, 
                                            ifelse(Specimen_tumor$site_of_disease_description == 'Other', 44516275, 
                                              ifelse((Specimen_tumor$site_of_disease_description == 'Lower' | Specimen_tumor$site_of_disease_description == 'Lower') & Specimen_tumor$laterality == 'Left', 4165555,
                                                      ifelse((Specimen_tumor$site_of_disease_description == 'Lower' | Specimen_tumor$site_of_disease_description == 'Lower|Lower') & Specimen_tumor$laterality == 'Right', 4131818,
                                                             ifelse(Specimen_tumor$site_of_disease_description == 'Middle' & Specimen_tumor$laterality == 'Right', 4247919,
                                                                    ifelse(Specimen_tumor$site_of_disease_description == 'Middle' & Specimen_tumor$laterality == 'Left', 4179862,
                                                                           ifelse((Specimen_tumor$site_of_disease_description == 'Upper' | Specimen_tumor$site_of_disease_description == 'Upper|Upper') & Specimen_tumor$laterality == 'Left', 4259129,
                                                                              ifelse((Specimen_tumor$site_of_disease_description == 'Upper' | Specimen_tumor$site_of_disease_description == 'Upper|Upper') & Specimen_tumor$laterality == 'Right', 4174862,NA))))))))
Specimen_tumor$anatomic_site_source_value <- tolower(Specimen_tumor$anatomic_site)
Specimen_tumor$specimen_source_value <- ifelse(grepl('resection', Specimen_tumor$anatomic_site_source_value), "tumor resection", 
                                                ifelse(grepl('lobectomy', Specimen_tumor$anatomic_site_source_value) | grepl('lobe', Specimen_tumor$anatomic_site_source_value) | grepl('segmentectomy', Specimen_tumor$anatomic_site_source_value) , 'lobectomy',
                                                       ifelse(grepl('pneumonectomy', Specimen_tumor$anatomic_site_source_value) | grepl('pnemonectomy', Specimen_tumor$anatomic_site_source_value),'pneumonectomy', 
                                                              ifelse(grepl('biopsy', Specimen_tumor$anatomic_site_source_value), 'biopsy',
                                                                     ifelse(grepl('bilobectomy', Specimen_tumor$anatomic_site_source_value), 'lobectomy', NA)))))
Specimen_tumor$person_source_value <- NULL
Specimen_tumor$sample_uuid <- NULL
Specimen_tumor$case_id <- NULL
Specimen_tumor$laterality <- NULL
Specimen_tumor$site_of_disease_description <- NULL
Specimen_tumor$anatomic_site <- NULL

colnames(Specimen_tumor) <- c('person_id', 'specimen_source_id', 'specimen_concept_id', 'specimen_type_concept_id',
                                'specimen_datetime', 'disease_status_concept_id',  'anatomic_site_concept_id', 'anatomic_site_source_value', 'specimen_source_value')
Specimen_tumor <- Specimen_tumor[,c('person_id', 'specimen_concept_id', "specimen_type_concept_id", "specimen_datetime", "anatomic_site_concept_id",
                                        "disease_status_concept_id", "specimen_source_id", 'specimen_source_value', "anatomic_site_source_value")]

Specimen <- rbind(Specimen_control, Specimen_tumor)
Specimen$specimen_id <- as.numeric(sample(seq(1040000, 1049999), nrow(Specimen)))

Specimen <- Specimen[,c('specimen_id', 'person_id', 'specimen_concept_id', "specimen_type_concept_id", "specimen_datetime", "anatomic_site_concept_id",
                                    "disease_status_concept_id", "specimen_source_id", 'specimen_source_value', "anatomic_site_source_value")]

write.csv(Specimen, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Specimen.csv", row.names = FALSE)

################PROCEDURE OCCURENCE TABLE######################        

Procedure_collection <- as.data.frame(Specimen$specimen_source_id)
colnames(Procedure_collection) <- c('specimen_source_id')
Procedure_collection$person_id <- Specimen$person_id
Procedure_collection$specimen_source_value <- Specimen$specimen_source_value
Procedure_collection$procedure_concept_id <- ifelse(Procedure_collection$specimen_source_value == "Blood Derived Normal", 44782057,
                                                    ifelse(Procedure_collection$specimen_source_value == 'Solid Tissue Normal' | Procedure_collection$specimen_source_value == 'biopsy', 4311405,
                                                           ifelse(Procedure_collection$specimen_source_value == 'tumor resection', 4119250,
                                                                  ifelse(Procedure_collection$specimen_source_value == 'pneumonectomy', 4172438,
                                                                         ifelse(Procedure_collection$specimen_source_value == 'lobectomy',4054037, 0)))))
Procedure_collection <- merge(Procedure_collection, Specimen[,c('person_id', 'specimen_source_value', 'specimen_datetime')], by = c('person_id', 'specimen_source_value'))
Procedure_collection$procedure_type_concept_id <- 44786630
Procedure_collection$specimen_source_id <- NULL
colnames(Procedure_collection) <- c('person_id', 'procedure_source_value', 'procedure_concept_id', 'procedure_datetime', 'procedure_type_concept_id')
Procedure_collection <- Procedure_collection[c('person_id', 'procedure_concept_id', 'procedure_datetime', 'procedure_type_concept_id', 'procedure_source_value')]

Procedure_seq <- as.data.frame(gen_data[!duplicated(Tumor_Sample_Barcode), 'Tumor_Sample_Barcode'])
colnames(Procedure_seq) <- c('specimen_source_id')
Procedure_seq <- merge(Procedure_seq, Specimen[,c('person_id', 'specimen_source_id')], by = 'specimen_source_id')
Procedure_seq$procedure_concept_id <- 37208194
Procedure_seq <- merge(Procedure_seq, Specimen[,c('specimen_source_id', 'specimen_datetime')], by = 'specimen_source_id')
Procedure_seq$procedure_type_concept_id <- 44786631
Procedure_seq$procedure_source_value <- NA
Procedure_seq$specimen_source_id <- NULL
colnames(Procedure_seq) <- c('person_id', 'procedure_concept_id', 'procedure_datetime', 'procedure_type_concept_id', 'procedure_source_value')
Procedure_seq <- Procedure_seq[c('person_id', 'procedure_concept_id', 'procedure_datetime', 'procedure_type_concept_id', 'procedure_source_value')]

Procedure_occurrence <- rbind(Procedure_collection, Procedure_seq)
antani <- visit_occurrence[, c('visit_occurrence_id', 'visit_start_datetime')]
antani$visit_start_datetime <- as.Date(antani$visit_start_datetime)
antani <- antani[!duplicated(antani$visit_start_datetime),]
Procedure_occurrence$procedure_datetime <- as.Date(Procedure_occurrence$procedure_datetime)
Procedure_occurrence <- merge(Procedure_occurrence, antani, 
                              by.x = c('procedure_datetime'), by.y = c('visit_start_datetime'), all.x = TRUE)

Procedure_occurrence$procedure_occurrence_id <- as.numeric(sample(seq(1050000, 1059999), nrow(Procedure_occurrence)))
Procedure_occurrence <- Procedure_occurrence[c('procedure_occurrence_id', 'person_id', 'procedure_concept_id', 'procedure_datetime', 'procedure_type_concept_id', 'visit_occurrence_id', 'procedure_source_value')]

write.csv(Procedure_occurrence, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Procedure_occurrence.csv", row.names = FALSE)

##################OBSERVATIONS TABLE####################

Observation <- as.data.frame(Person$person_id)
colnames(Observation) <- c('person_id')
Observation$person_source_value <- Person$person_source_value
#Measurement <- merge(Measurement, Observation_period[,c('person_id', "observation_period_start_date")], by = 'person_id')
Observation <- merge(Observation, visit_occurrence[which(grepl('VITAL_STATUS', visit_occurrence$visit_source_value)), c('person_id', 'visit_occurrence_id', 'visit_start_datetime')], by = 'person_id')
Observation <- merge(Observation, Clin[!duplicated(Clin$submitter_id), c("submitter_id", "vital_status")], by.x = 'person_source_value', by.y = 'submitter_id')
Observation$Observation_type_concept_id <- 38000280
Observation$observation_concept_id <- 3009506
Observation$value_as_string <- Observation$vital_status
Observation$value_as_concept_id <- ifelse(Observation$value_as_string == 'Alive', 45884343, 45878547)
Observation$person_source_value <- NULL
colnames(Observation) <- c('person_id', 'visit_occurrence_id', 'observation_date', 'observation_source_value', 'observation_type_concept_id',
                           'observation_concept_id', 'value_as_string', 'value_as_concept_id')

Observation_tumor <- as.data.frame(Person$person_id)
colnames(Observation_tumor) <- c('person_id')
Observation_tumor$person_source_value <- Person$person_source_value
Observation_tumor <- merge(Observation_tumor, visit_occurrence[which(grepl('VITAL_STATUS', visit_occurrence$visit_source_value)), c('person_id', 'visit_occurrence_id', 'visit_start_datetime')], by = 'person_id')
Observation_tumor <- merge(Observation_tumor, Clin_followup[!duplicated(Clin_followup$bcr_patient_barcode), c("bcr_patient_barcode", "tumor_status")], by.x = 'person_source_value', by.y = 'bcr_patient_barcode', all.x = TRUE)
Observation_tumor$Observation_type_concept_id <- 38000280
Observation_tumor$observation_concept_id <- 4174610
Observation_tumor$value_as_string <- Observation_tumor$tumor_status
Observation_tumor$value_as_concept_id <- 0
Observation_tumor$person_source_value <- NULL

colnames(Observation_tumor) <- c('person_id', 'visit_occurrence_id', 'observation_date', 'observation_source_value', 'observation_type_concept_id',
                           'observation_concept_id', 'value_as_string', 'value_as_concept_id')

Observation_out <- as.data.frame(Person$person_id)
colnames(Observation_out) <- c('person_id')
Observation_out$person_source_value <- Person$person_source_value
Observation_out <- merge(Observation_out, visit_occurrence[which(grepl('VITAL_STATUS', visit_occurrence$visit_source_value)), c('person_id', 'visit_occurrence_id', 'visit_start_datetime')], by = 'person_id')  
Observation_out <- merge(Observation_out, Clin_followup[!duplicated(Clin_followup$bcr_patient_barcode), c("bcr_patient_barcode", "treatment_outcome_first_course")], by.x = 'person_source_value', by.y = 'bcr_patient_barcode', all.x = TRUE)
Observation_out$Observation_type_concept_id <- 38000280
Observation_out$observation_concept_id <- 37397701
Observation_out$value_as_string <- Observation_out$treatment_outcome_first_course
Observation_out$value_as_concept_id <- ifelse(Observation_out$treatment_outcome_first_course == "Complete Remission/Response", 4014046,
                                              ifelse(Observation_out$treatment_outcome_first_course == "Partial Remission/Response", 4014045,
                                                     ifelse(Observation_out$treatment_outcome_first_course == "Progressive Disease", 4190937, 0)))
Observation_out$person_source_value <- NULL

colnames(Observation_out) <- c('person_id', 'visit_occurrence_id', 'observation_date', 'observation_source_value', 'observation_type_concept_id',
                           'observation_concept_id', 'value_as_string', 'value_as_concept_id')

Observation <- rbind(Observation, Observation_tumor, Observation_out)

Observation$observation_id <- as.numeric(sample(seq(7000000, 7009999), nrow(Observation)))
Observation <- Observation[c('observation_id', 'person_id', 'observation_concept_id', 'observation_date', 'observation_type_concept_id',
                             'value_as_string', 'visit_occurrence_id', 'observation_source_value')]

write.csv(Observation, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Observations.csv", row.names = FALSE)

##################CARE SITE TABLE####################

Care_site <- as.data.frame(matrix(nrow = 1, ncol = 1))
Care_site$care_site_name <- "TCGA LUSC"
Care_site$place_of_service <- "TGCA Portal"
Care_site$location_ID <-80001
Care_site$care_site_ID <- 1060001
Care_site$V1 <- NULL
colnames(Care_site) <- c('care_site_name', 'place_of_service', 'location_id', 'care_site_id')
Care_site <- Care_site[c('care_site_id', 'care_site_name', 'place_of_service', 'location_id' )]

write.csv(Care_site, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Care_site.csv", row.names = FALSE)

##################GENOMIC_TEST TABLE#################

Genomic_test <- as.data.frame(matrix(nrow = 1, ncol=1))
Genomic_test$sequencing_device <- "Illumina HiSeq 2000"
Genomic_test$V1 <- NULL
Genomic_test$genomic_test_id <- 1070001
Genomic_test$care_site_id <- 1060001
Genomic_test$genomic_test_name <- "WGS"
Genomic_test$genomic_test_version <- NA
Genomic_test$reference_genome <- 'GRCh38'
Genomic_test$target_capture <- NA
Genomic_test$read_type <- NA
Genomic_test$read_length <- NA
Genomic_test$alignment_tool <- NA
Genomic_test$variant_calling_tool <- "mutect"
Genomic_test$chromosome_coordinate <- NA
Genomic_test$annotation_tool <- NA
Genomic_test$annotation_database <- NA

Genomic_test <- Genomic_test[,c("genomic_test_id", "care_site_id", "genomic_test_name", "genomic_test_version", "reference_genome", "sequencing_device", "target_capture",
                             "read_type", "read_length", "alignment_tool", "variant_calling_tool", "chromosome_coordinate", "annotation_tool", "annotation_database")]

write.csv(Genomic_test, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Genomic_test.csv", row.names = FALSE)

##################TARGET_GENE TABLE####################

actionable <- function(hugo_symbol) {
  
  pb <- progress_bar$new(
    format = " PROGRESS [:bar] :percent ETA: :eta",
    total = length(hugo_symbol), clear = FALSE, width= 60)
  
  druggable <- list()
  
  for (item in hugo_symbol) {
    pb$tick()
    server <- "http://dgidb.org/api/v2"
    ext <- paste("/interactions.json?genes=",item, sep = '')
    r <- GET(paste(server, ext, sep = ""), content_type("application/json"))
    r <- fromJSON(toJSON(content(r)))
    if (length(r$ambiguousTerms) != 0) {
      druggable <- append(druggable, 'Ambiguous term')
    } else {
      if (length(r$matchedTerms) == 0 ) {
        druggable <- append(druggable, '[Not Available]')
      } else {
          resp <- 'NOT DRUGGABLE GENOME'
          r <- r$matchedTerms$geneCategories
          if("DRUGGABLE GENOME" %in% r[[1]]$name) {resp <- "DRUGGABLE GENOME"}
          druggable <- append(druggable, resp)
        }
    }
  }
  return(druggable)
} 

Target_gene <- gen_data[!duplicated(gen_data$Hugo_Symbol), 'Hugo_Symbol']
colnames(Target_gene) <- c('hgnc_id')
Target_gene$Genomic_test_id <- 1070001
Target_gene <- merge(Target_gene, gen_data[which(!duplicated(gen_data$Hugo_Symbol)), 
                                           c('Hugo_Symbol', 'Entrez_Gene_Id', 'Chromosome', 'Start_Position', 'End_Position')], 
                                                by.x = 'hgnc_id', by.y = 'Hugo_Symbol')
Target_gene$Entrez_Gene_Id <- paste('hsa:', Target_gene$Entrez_Gene_Id, sep='')
Target_gene$target_gene_id <- as.numeric(sample(seq(1100000, 1199999), nrow(Target_gene)))
Target_gene$druggability <- as.character(actionable(Target_gene$hgnc_id))
colnames(Target_gene) <- c('Target_gene_source_value', 'genomic_test_id', 'target_gene_source_id', 'chromosome_id',
                           'start_position', 'end_position', 'target_gene_id', 'druggability_source_value')
Target_gene <- Target_gene[,c('target_gene_id', 'genomic_test_id', 'Target_gene_source_value', 'chromosome_id', 
                              'start_position', 'end_position', 'target_gene_source_id',  'druggability_source_value')]


write.csv(Target_gene, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Target_gene.csv", row.names = FALSE)


##################VARIANT_OCCURENCE TABLE#################

gen_data$variant_occurrence_id <- as.numeric(sample(seq(2000000, 2999999), nrow(gen_data), replace = FALSE))

Variant_occurrence <- as.data.frame(gen_data$Hugo_Symbol)
colnames(Variant_occurrence) <- c('Hugo_Symbol')
Variant_occurrence$variant_occurrence_id <- gen_data$variant_occurrence_id
Variant_occurrence$Tumor_Sample_Barcode <- gen_data$Tumor_Sample_Barcode
Variant_occurrence$RefSeq <- gen_data$RefSeq
Variant_occurrence$rs_id <- gen_data$dbSNP_RS
Variant_occurrence$hgvsc <- gen_data$HGVSc
Variant_occurrence$hgvsp <- gen_data$HGVSp
Variant_occurrence$variant_read_depth <- gen_data$n_depth
Variant_occurrence$total_read_depth <- gen_data$t_depth
Variant_occurrence$variant_exon_number <- gen_data$Exon_Number
Variant_occurrence$sequence_alteration <- gen_data$Variant_Type
Variant_occurrence$variant_classification <- gen_data$Variant_Classification
Variant_occurrence$reference_allele <- gen_data$Reference_Allele
Variant_occurrence$alternate_allele1 <- gen_data$Tumor_Seq_Allele1
Variant_occurrence$alternate_allele2 <- gen_data$Tumor_Seq_Allele2
Variant_occurrence$genetic_origin <- gen_data$Mutation_Status
Variant_occurrence$Matched_Norm_Sample_Barcode <- gen_data$Matched_Norm_Sample_Barcode
Variant_occurrence$genotype <- gen_data$'Allele'
Variant_occurrence <- merge(Variant_occurrence, Target_gene[,c('Target_gene_source_value', 'target_gene_id')], by.x = 'Hugo_Symbol', by.y = 'Target_gene_source_value')
Variant_occurrence <- merge(Variant_occurrence, Specimen[, c('specimen_source_id', 'specimen_id')], by.x = 'Tumor_Sample_Barcode', by.y = 'specimen_source_id')
Variant_occurrence <- merge(Variant_occurrence, Specimen[, c('specimen_source_id', 'specimen_id')], by.x = 'Matched_Norm_Sample_Barcode', by.y = 'specimen_source_id')
Variant_occurrence <- merge(Variant_occurrence, Specimen[, c('specimen_source_id', 'person_id')], by.x = 'Tumor_Sample_Barcode', by.y = 'specimen_source_id')
Variant_occurrence <- merge(Variant_occurrence, Procedure_occurrence[which(Procedure_occurrence$procedure_concept_id == 	37208194),c('person_id', 'procedure_occurrence_id')], by = 'person_id')
Variant_occurrence$target_gene2_id <- NA

Variant_occurrence$person_id <- NULL
Variant_occurrence$Tumor_Sample_Barcode <- NULL
Variant_occurrence$Matched_Norm_Sample_Barcode <- NULL
colnames(Variant_occurrence) <- c('target_gene1_symbol', 'variant_occurrence_id', 'reference_sequence', 'rs_id', 'hgvs_c', 'hgvs_p', 'variant_read_depth', 'total_read_depth', 'variant_exon',
                                'sequence_alteration', 'variant_feature', 'reference_allele', 'alternate_allele1', 'alternate_allele2', 'genetic_origin', 'genotype',
                                'target_gene1_id', 'specimen_id', 'reference_specimen_id', 'procedure_occurrence_id', 'target_gene2_id')
Variant_occurrence <- Variant_occurrence[,c('variant_occurrence_id', 'procedure_occurrence_id', 'specimen_id', 'reference_specimen_id', 'target_gene1_id', 'target_gene1_symbol', 'target_gene2_id',
                                             'reference_sequence', 'rs_id', 'reference_allele', 'alternate_allele1', 'alternate_allele2', 'hgvs_c', 'hgvs_p', 'variant_read_depth',
                                             'total_read_depth', 'variant_exon', 'sequence_alteration', 'variant_feature', 'genetic_origin', 'genotype')]


write.csv(Variant_occurrence, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Variant_occurrence.csv", row.names = FALSE)


##################VARIANT_ANNOTATION TABLE##################

Variant_annotation_ccd5 <- as.data.frame(gen_data[which(gen_data$CCDS != ''), 'variant_occurrence_id'])
colnames(Variant_annotation_ccd5) <- c('variant_occurrence_id')
Variant_annotation_ccd5 <- merge(Variant_annotation_ccd5, gen_data[,c('variant_occurrence_id', 'CCDS')], by = 'variant_occurrence_id')
Variant_annotation_ccd5$annotation_field <- 'CCDS'
Variant_annotation_ccd5$value_as_number <- NA
Variant_annotation_ccd5 <- merge(Variant_annotation_ccd5, gen_data[,c('variant_occurrence_id', 'PolyPhen')], by = 'variant_occurrence_id')
Variant_annotation_ccd5$pathogenic <- substr(Variant_annotation_ccd5$PolyPhen, 1, regexpr('0', as.character(Variant_annotation_ccd5$PolyPhen))-2)
Variant_annotation_ccd5$pathogenic <- ifelse(Variant_annotation_ccd5$pathogenic == "", NA, as.character(Variant_annotation_ccd5$pathogenic))
Variant_annotation_ccd5$PolyPhen <- NULL
colnames(Variant_annotation_ccd5) <- c('variant_occurrence_id', 'value_as_string', 'annotation_field', 'value_as_number', 'pathogenic')
Variant_annotation_ccd5 <- Variant_annotation_ccd5[,c('variant_occurrence_id', 'annotation_field', 'value_as_string', 'value_as_number', 'pathogenic')]

Variant_annotation_ensp <- as.data.frame(gen_data[which(gen_data$ENSP != ''), 'variant_occurrence_id'])
colnames(Variant_annotation_ensp) <- c('variant_occurrence_id')
Variant_annotation_ensp <- merge(Variant_annotation_ensp, gen_data[,c('variant_occurrence_id', 'ENSP')], by = 'variant_occurrence_id')
Variant_annotation_ensp$annotation_field <- 'ENSP'
Variant_annotation_ensp$value_as_number <- NA
Variant_annotation_ensp <- merge(Variant_annotation_ensp, gen_data[,c('variant_occurrence_id', 'PolyPhen')], by = 'variant_occurrence_id')
Variant_annotation_ensp$pathogenic <- substr(Variant_annotation_ensp$PolyPhen, 1, regexpr('0', as.character(Variant_annotation_ensp$PolyPhen))-2)
Variant_annotation_ensp$pathogenic <- ifelse(Variant_annotation_ensp$pathogenic == "", NA, as.character(Variant_annotation_ensp$pathogenic))
Variant_annotation_ensp$PolyPhen <- NULL
colnames(Variant_annotation_ensp) <- c('variant_occurrence_id', 'value_as_string', 'annotation_field', 'value_as_number', 'pathogenic')
Variant_annotation_ensp <- Variant_annotation_ensp[,c('variant_occurrence_id', 'annotation_field', 'value_as_string', 'value_as_number', 'pathogenic')]

Variant_annotation_swissprot <- as.data.frame(gen_data[which(gen_data$SWISSPROT != ''), 'variant_occurrence_id'])
colnames(Variant_annotation_swissprot) <- c('variant_occurrence_id')
Variant_annotation_swissprot <- merge(Variant_annotation_swissprot, gen_data[,c('variant_occurrence_id', 'SWISSPROT')], by = 'variant_occurrence_id')
Variant_annotation_swissprot$annotation_field <- 'SWISSPROT'
Variant_annotation_swissprot$value_as_number <- NA
Variant_annotation_swissprot <- merge(Variant_annotation_swissprot, gen_data[,c('variant_occurrence_id', 'PolyPhen')], by = 'variant_occurrence_id')
Variant_annotation_swissprot$pathogenic <- substr(Variant_annotation_swissprot$PolyPhen, 1, regexpr('0', as.character(Variant_annotation_swissprot$PolyPhen))-2)
Variant_annotation_swissprot$pathogenic <- ifelse(Variant_annotation_swissprot$pathogenic == "", NA, as.character(Variant_annotation_swissprot$pathogenic))
Variant_annotation_swissprot$PolyPhen <- NULL
colnames(Variant_annotation_swissprot) <- c('variant_occurrence_id', 'value_as_string', 'annotation_field', 'value_as_number', 'pathogenic')
Variant_annotation_swissprot <- Variant_annotation_swissprot[,c('variant_occurrence_id', 'annotation_field', 'value_as_string', 'value_as_number', 'pathogenic')]


Variant_annotation_trembl <- as.data.frame(gen_data[which(gen_data$TREMBL != ''), 'variant_occurrence_id'])
colnames(Variant_annotation_trembl) <- c('variant_occurrence_id')
Variant_annotation_trembl <- merge(Variant_annotation_trembl, gen_data[,c('variant_occurrence_id', 'TREMBL')], by = 'variant_occurrence_id')
Variant_annotation_trembl$annotation_field <- 'TREMBL'
Variant_annotation_trembl$value_as_number <- NA
Variant_annotation_trembl <- merge(Variant_annotation_trembl, gen_data[,c('variant_occurrence_id', 'PolyPhen')], by = 'variant_occurrence_id')
Variant_annotation_trembl$pathogenic <- substr(Variant_annotation_trembl$PolyPhen, 1, regexpr('0', as.character(Variant_annotation_trembl$PolyPhen))-2)
Variant_annotation_trembl$pathogenic <- ifelse(Variant_annotation_trembl$pathogenic == "", NA, as.character(Variant_annotation_trembl$pathogenic))
Variant_annotation_trembl$PolyPhen <- NULL
colnames(Variant_annotation_trembl) <- c('variant_occurrence_id', 'value_as_string', 'annotation_field', 'value_as_number', 'pathogenic')
Variant_annotation_trembl <- Variant_annotation_trembl[,c('variant_occurrence_id', 'annotation_field', 'value_as_string', 'value_as_number', 'pathogenic')]


Variant_annotation_uniparc <- as.data.frame(gen_data[which(gen_data$UNIPARC != ''), 'variant_occurrence_id'])
colnames(Variant_annotation_uniparc) <- c('variant_occurrence_id')
Variant_annotation_uniparc <- merge(Variant_annotation_uniparc, gen_data[,c('variant_occurrence_id', 'UNIPARC')], by = 'variant_occurrence_id')
Variant_annotation_uniparc$annotation_field <- 'UNIPARC'
Variant_annotation_uniparc$value_as_number <- NA
Variant_annotation_uniparc <- merge(Variant_annotation_uniparc, gen_data[,c('variant_occurrence_id', 'PolyPhen')], by = 'variant_occurrence_id')
Variant_annotation_uniparc$pathogenic <- substr(Variant_annotation_uniparc$PolyPhen, 1, regexpr('0', as.character(Variant_annotation_uniparc$PolyPhen))-2)
Variant_annotation_uniparc$pathogenic <- ifelse(Variant_annotation_uniparc$pathogenic == "", NA, as.character(Variant_annotation_uniparc$pathogenic))
Variant_annotation_uniparc$PolyPhen <- NULL
colnames(Variant_annotation_uniparc) <- c('variant_occurrence_id', 'value_as_string', 'annotation_field', 'value_as_number', 'pathogenic')
Variant_annotation_uniparc <- Variant_annotation_uniparc[,c('variant_occurrence_id', 'annotation_field', 'value_as_string', 'value_as_number', 'pathogenic')]

Variant_annotation <- rbind(Variant_annotation_ccd5, Variant_annotation_ensp, Variant_annotation_swissprot, Variant_annotation_trembl, Variant_annotation_uniparc)
Variant_annotation$variant_annotation_id <- as.numeric(sample(seq(3000000, 3999999), nrow(Variant_annotation), replace = FALSE))
colnames(Variant_annotation) <- c('variant_occurrence_id', 'annotation_field', 'value_as_string', 'value_as_number', 'pathogenic', 'variant_annotation_id')
Variant_annotation <- Variant_annotation[,c('variant_annotation_id', 'variant_occurrence_id', 'annotation_field', 'value_as_string', 'value_as_number', 'pathogenic')]

write.csv(Variant_annotation, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Variant_annotation.csv", row.names = FALSE)

##################PATHWAY_OCCURRENCE TABLE##################


pathway <- function(genes) {

  pb <- progress_bar$new(
    format = " PROGRESS [:bar] :percent ETA: :eta",
    total = length(genes), clear = FALSE, width= 60)
  
  path <- list()
  gene_id <- list ()
  
  for (item in genes) {
    pb$tick()
    if (length(as.list(keggLink("pathway", item)))>0) {
      path <- append(path, as.list(keggLink("pathway", item)))
      for (i in 1:length(as.list(keggLink("pathway", item)))) {
        gene_id <- append(gene_id, item)
      }
    } else {
      gene_id <- append(gene_id, item)
      path <- append(path, '[Not Available]')
    }
  }
  to_be_returned <- as.data.frame(matrix(nrow=length(path),ncol=0))
  to_be_returned$target_gene_source_id <- gene_id
  to_be_returned$pathway_source_id <- path
  return(to_be_returned)
}

pathway_occurrence <- pathway(Target_gene$target_gene_source_id)
pathway_occurrence$target_gene_source_id <- as.character(pathway_occurrence$target_gene_source_id)
pathway_occurrence$pathway_source_id <- as.character(pathway_occurrence$pathway_source_id)
pathway_occurrence <- merge(pathway_occurrence, Target_gene[!duplicated(Target_gene$target_gene_source_id),c('target_gene_source_id', 'target_gene_id')], by = 'target_gene_source_id')
pathway_occurrence$pathway_occurrence_id <- as.numeric(sample(seq(4000000, 4999999), nrow(pathway_occurrence), replace = FALSE))

##################PATHWAY_ANNOTATION TABLE##################

Pathway_annotation <- as.data.frame(pathway_occurrence[!duplicated(pathway_occurrence$pathway_source_id),'pathway_source_id'])
Pathway_annotation <- as.data.frame(Pathway_annotation[which(Pathway_annotation!= '[Not Available]'),])
colnames(Pathway_annotation) <- c('pathway_source_id')
Pathway_annotation$pathway_source_value <- lapply(Pathway_annotation$pathway_source_id, keggList)
Pathway_annotation$pathway_source_value <- trimws(substr(Pathway_annotation$pathway_source_value, 1, gregexpr(pattern = ' - H', Pathway_annotation$pathway_source_value)))
Pathway_annotation$pathway_annotation_id <- as.numeric(sample(seq(5000000, 5999999), nrow(Pathway_annotation), replace = FALSE))

pathway_occurrence <- merge(pathway_occurrence, Pathway_annotation[, c('pathway_source_id', 'pathway_annotation_id')], by = 'pathway_source_id', all.x = TRUE)
pathway_occurrence <- pathway_occurrence[, c("pathway_occurrence_id", 'pathway_annotation_id', "target_gene_id", 'pathway_source_id', "target_gene_source_id")]
Pathway_annotation <- Pathway_annotation[, c("pathway_annotation_id", "pathway_source_id", "pathway_source_value")]

write.csv(Pathway_annotation, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Pathway_annotation.csv", row.names = FALSE)
write.csv(pathway_occurrence, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Pathway_occurrence.csv", row.names = FALSE)

##################INTERACTION_OCCURRENCE TABLE##################

interaction_finder <- function(hugo_symbol) {
  
  pb <- progress_bar$new(
    format = " PROGRESS [:bar] :percent ETA: :eta",
    total = length(hugo_symbol), clear = FALSE, width= 60)
  
  drug_name <- list()
  gene_name <- list()
  interaction_type <- list()
  score <- list()
  for (item in hugo_symbol) {
    pb$tick()
    server <- "http://dgidb.org/api/v2"
    ext <- paste("/interactions.json?genes=", item, sep = '')
    r <- GET(paste(server, ext, sep = ""), content_type("application/json"))
    r <- fromJSON(toJSON(content(r)))
    r <- r$matchedTerms
    r <- r$interactions
    r <- r[[1]]
    if (length(r) != 0) {
      for (i in 1:nrow(r)) {
        gene_name <- append(gene_name, item)
        drug_name <- append(drug_name, r$drugName[i])
        interaction_type <- append(interaction_type, r$interactionType[i])
        score <- append(score, r$score[[i]])
      } 
    } else {
      gene_name <- append(gene_name, item)
      drug_name <- append(drug_name, '[NO INTERACTIONS AVAILABLE]')
      interaction_type <- append(interaction_type, '[NO INTERACTIONS AVAILABLE]')
      score <- append(score, '[NO INTERACTIONS AVAILABLE]')
    } 
  }
  to_be_returned <- as.data.frame(matrix(ncol=0, nrow=length(gene_name)))
  to_be_returned$gene_source_value <- gene_name
  to_be_returned$drug_source_value <- drug_name
  to_be_returned$interaction_source_value <- interaction_type
  to_be_returned$score_as_number <- score
  return(to_be_returned)
}

interaction_occurrence <- interaction_finder(Target_gene$Target_gene_source_value)
interaction_occurrence$gene_source_value <- as.character(interaction_occurrence$gene_source_value)
interaction_occurrence$drug_source_value <- as.character(interaction_occurrence$drug_source_value)
interaction_occurrence$interaction_source_value <- as.character(interaction_occurrence$interaction_source_value)
interaction_occurrence$score_as_number <- as.numeric(as.character(interaction_occurrence$score_as_number))
interaction_occurrence$score_as_number <- ifelse(is.na(interaction_occurrence$score_as_number), '[Not Available]', interaction_occurrence$score_as_number)
interaction_occurrence$interaction_source_value <- ifelse(as.character(interaction_occurrence$interaction_source_value) == 'list()', '[Not Available]', as.character(interaction_occurrence$interaction_source_value))
interaction_occurrence$interaction_occurrence_id <- as.numeric(sample(seq(6000000, 6999999), nrow(interaction_occurrence), replace = FALSE))
interaction_occurrence <- merge(interaction_occurrence, Target_gene[,c("Target_gene_source_value", "target_gene_id")], by.x = 'gene_source_value', by.y = 'Target_gene_source_value', all.x = TRUE)

colnames(interaction_occurrence) <- c("Target_gene_source_value", "drug_source_value", "interaction_source_value", "score_as_number", "interaction_occurrence_id", 'target_gene_id')
interaction_occurrence <- interaction_occurrence[, c("interaction_occurrence_id", 'target_gene_id', "Target_gene_source_value", "drug_source_value", "interaction_source_value", "score_as_number")]

write.csv(interaction_occurrence, "\\Users\\mellil\\Desktop\\TGCA data\\Data_to be used_ETL\\Tables\\interaction_occurrence.csv", row.names = FALSE)
