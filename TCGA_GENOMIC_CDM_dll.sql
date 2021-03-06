--HINT DISTRIBUTE ON RANDOM
CREATE TABLE concept (
  concept_id			INTEGER			NOT NULL ,
  concept_name			VARCHAR(255)	NOT NULL ,
  domain_id				VARCHAR(20)		NOT NULL ,
  vocabulary_id			VARCHAR(20)		NOT NULL ,
  concept_class_id		VARCHAR(20)		NOT NULL ,
  standard_concept		VARCHAR(1)		NULL ,
  concept_code			VARCHAR(50)		NOT NULL ,
  valid_start_date		DATE			NOT NULL ,
  valid_end_date		DATE			NOT NULL ,
  invalid_reason		VARCHAR(1)		NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE vocabulary (
  vocabulary_id			VARCHAR(20)		NOT NULL,
  vocabulary_name		VARCHAR(255)	NOT NULL,
  vocabulary_reference	VARCHAR(255)	NOT NULL,
  vocabulary_version	VARCHAR(255)	NULL,
  vocabulary_concept_id	INTEGER			NOT NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE domain (
  domain_id			    VARCHAR(20)		NOT NULL,
  domain_name		    VARCHAR(255)	NOT NULL,
  domain_concept_id		INTEGER			NOT NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE concept_class (
  concept_class_id			VARCHAR(20)		NOT NULL,
  concept_class_name		VARCHAR(255)	NOT NULL,
  concept_class_concept_id	INTEGER			NOT NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE concept_relationship (
  concept_id_1			INTEGER			NOT NULL,
  concept_id_2			INTEGER			NOT NULL,
  relationship_id		VARCHAR(20)		NOT NULL,
  valid_start_date		DATE			NOT NULL,
  valid_end_date		DATE			NOT NULL,
  invalid_reason		VARCHAR(1)		NULL
  )
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE relationship (
  relationship_id			VARCHAR(20)		NOT NULL,
  relationship_name			VARCHAR(255)	NOT NULL,
  is_hierarchical			VARCHAR(1)		NOT NULL,
  defines_ancestry			VARCHAR(1)		NOT NULL,
  reverse_relationship_id	VARCHAR(20)		NOT NULL,
  relationship_concept_id	INTEGER			NOT NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE concept_synonym (
  concept_id			INTEGER			NOT NULL,
  concept_synonym_name	VARCHAR(1000)	NOT NULL,
  language_concept_id	INTEGER			NOT NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE concept_ancestor (
  ancestor_concept_id		INTEGER		NOT NULL,
  descendant_concept_id		INTEGER		NOT NULL,
  min_levels_of_separation	INTEGER		NOT NULL,
  max_levels_of_separation	INTEGER		NOT NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE source_to_concept_map (
  source_code				VARCHAR(50)		NOT NULL,
  source_concept_id			INTEGER			NOT NULL,
  source_vocabulary_id		VARCHAR(20)		NOT NULL,
  source_code_description	VARCHAR(255)	NULL,
  target_concept_id			INTEGER			NOT NULL,
  target_vocabulary_id		VARCHAR(20)		NOT NULL,
  valid_start_date			DATE			NOT NULL,
  valid_end_date			DATE			NOT NULL,
  invalid_reason			VARCHAR(1)		NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE drug_strength (
  drug_concept_id				INTEGER		  	NOT NULL,
  ingredient_concept_id			INTEGER		  	NOT NULL,
  amount_value					NUMERIC		    NULL,
  amount_unit_concept_id		INTEGER		  	NULL,
  numerator_value				NUMERIC		    NULL,
  numerator_unit_concept_id		INTEGER		  	NULL,
  denominator_value				NUMERIC		    NULL,
  denominator_unit_concept_id	INTEGER		  	NULL,
  box_size						INTEGER		 	NULL,
  valid_start_date				DATE		    NOT NULL,
  valid_end_date				DATE		    NOT NULL,
  invalid_reason				VARCHAR(1)  	NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE attribute_definition (
  attribute_definition_id		  INTEGER			  NOT NULL,
  attribute_name				      VARCHAR(255)	NOT NULL,
  attribute_description			  TEXT	NULL,
  attribute_type_concept_id		INTEGER			  NOT NULL,
  attribute_syntax				    TEXT	NULL
)
;


/**************************

Standardized meta-data

***************************/


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE cdm_source
(
  cdm_source_name					VARCHAR(255)	NOT NULL ,
  cdm_source_abbreviation			VARCHAR(25)		NULL ,
  cdm_holder						VARCHAR(255)	NULL ,
  source_description				TEXT			NULL ,
  source_documentation_reference	VARCHAR(255)	NULL ,
  cdm_etl_reference					VARCHAR(255)	NULL ,
  source_release_date				DATE			NULL ,
  cdm_release_date					DATE			NULL ,
  cdm_version						VARCHAR(10)		NULL ,
  vocabulary_version				VARCHAR(20)		NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE metadata
(
  metadata_concept_id       INTEGER       	NOT NULL ,
  metadata_type_concept_id  INTEGER       	NOT NULL ,
  name                      VARCHAR(250)  	NOT NULL ,
  value_as_string           TEXT  			NULL ,
  value_as_concept_id       INTEGER       	NULL ,
  metadata_date             DATE          	NULL ,
  metadata_datetime         TIMESTAMP      	NULL
)
;

--INSERT INTO metadata (metadata_concept_id, metadata_type_concept_id, name, value_as_string, value_as_concept_id, metadata_date, metadata_datetime)
--VALUES (0, 0, 'CDM Version', '6.0', 0, NULL, NULL)
--;


/************************

Standardized clinical data

************************/


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE person
(
  person_id						BIGINT	  	NOT NULL , 
  gender_concept_id				INTEGER	  	NOT NULL ,
  year_of_birth					INTEGER	  	NOT NULL ,
  month_of_birth				INTEGER	  	NULL,
  day_of_birth					INTEGER	  	NULL,
  birth_datetime				TIMESTAMP	NULL,
  death_datetime				TIMESTAMP	NULL,
  race_concept_id				INTEGER		NOT NULL,
  ethnicity_concept_id			INTEGER	  	NOT NULL,
  location_id					BIGINT		NULL,
  provider_id					BIGINT		NULL,
  care_site_id					BIGINT		NULL,
  person_source_value			VARCHAR(50)	NULL,
  gender_source_value			VARCHAR(50) NULL,
  gender_source_concept_id	  	INTEGER		NULL,
  race_source_value				VARCHAR(50) NULL,
  race_source_concept_id		INTEGER		NULL,
  ethnicity_source_value		VARCHAR(50) NULL,
  ethnicity_source_concept_id	INTEGER		NULL
)
;


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE observation_period
(
  observation_period_id				BIGINT		NOT NULL ,
  person_id							BIGINT		NOT NULL ,
  observation_period_start_date		DATE		NOT NULL ,
  observation_period_end_date		DATE		NULL ,
  period_type_concept_id			INTEGER		NULL
)
;


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE specimen
(
  specimen_id					BIGINT			NOT NULL ,
  person_id						BIGINT			NOT NULL ,
  specimen_concept_id			INTEGER			NOT NULL ,
  specimen_type_concept_id		INTEGER			NOT NULL ,
  specimen_date					DATE			NULL ,
  specimen_datetime				TIMESTAMP		NULL ,
  quantity						NUMERIC			NULL ,
  unit_concept_id				INTEGER			NULL ,
  anatomic_site_concept_id		INTEGER			NULL ,
  disease_status_concept_id		INTEGER			NOT NULL ,
  specimen_source_id			VARCHAR(50)		NULL ,
  specimen_source_value			VARCHAR(50)		NULL ,
  unit_source_value				VARCHAR(50)		NULL ,
  anatomic_site_source_value	VARCHAR(1000)	NULL ,
  disease_status_source_value	VARCHAR(50)		NULL
)
;


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE visit_occurrence
(
  visit_occurrence_id			BIGINT			NOT NULL ,
  person_id						BIGINT			NOT NULL ,
  visit_concept_id				INTEGER			NOT NULL ,
  visit_start_date				DATE			NULL ,
  visit_start_datetime			TIMESTAMP		NULL ,
  visit_end_date				DATE			NULL ,
  visit_end_datetime			TIMESTAMP		NULL ,
  visit_type_concept_id			INTEGER			NOT NULL ,
  provider_id					BIGINT			NULL,
  care_site_id					BIGINT			NULL,
  visit_source_value			VARCHAR(50)		NULL,
  visit_source_concept_id		INTEGER			NULL ,
  admitted_from_concept_id      INTEGER     	NULL ,   
  admitted_from_source_value    VARCHAR(50) 	NULL ,
  discharge_to_source_value		VARCHAR(50)		NULL ,
  discharge_to_concept_id		INTEGER   		NULL ,
  preceding_visit_occurrence_id	BIGINT 			NULL
)
;


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE visit_detail
(
  visit_detail_id                    BIGINT      NOT NULL ,
  person_id                          BIGINT      NOT NULL ,
  visit_detail_concept_id            INTEGER     NOT NULL ,
  visit_detail_start_date            DATE        NULL ,
  visit_detail_start_datetime        TIMESTAMP   NOT NULL ,
  visit_detail_end_date              DATE        NULL ,
  visit_detail_end_datetime          TIMESTAMP   NOT NULL ,
  visit_detail_type_concept_id       INTEGER     NOT NULL ,
  provider_id                        BIGINT      NULL ,
  care_site_id                       BIGINT      NULL ,
  discharge_to_concept_id            INTEGER     NOT NULL ,
  admitted_from_concept_id           INTEGER     NOT NULL , 
  admitted_from_source_value         VARCHAR(50) NULL ,
  visit_detail_source_value          VARCHAR(50) NULL ,
  visit_detail_source_concept_id     INTEGER     NOT NULL ,
  discharge_to_source_value          VARCHAR(50) NULL ,
  preceding_visit_detail_id          BIGINT      NULL ,
  visit_detail_parent_id             BIGINT      NULL ,
  visit_occurrence_id                BIGINT      NOT NULL
)
;


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE procedure_occurrence
(
  procedure_occurrence_id		BIGINT			NOT NULL ,
  person_id						BIGINT			NOT NULL ,
  procedure_concept_id			INTEGER			NULL ,
  procedure_date				DATE			NULL ,
  procedure_datetime			TIMESTAMP		NULL ,
  procedure_type_concept_id		INTEGER			NOT NULL ,
  modifier_concept_id			INTEGER			NULL ,
  quantity						INTEGER			NULL ,
  provider_id					BIGINT			NULL ,
  visit_occurrence_id			BIGINT			NULL ,
  visit_detail_id             	BIGINT      	NULL ,
  procedure_source_value		VARCHAR(50)		NULL ,
  procedure_source_concept_id	INTEGER			NULL ,
  modifier_source_value		    VARCHAR(50)		NULL 
)
;


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE drug_exposure
(
  drug_exposure_id				BIGINT			 	NOT NULL ,
  person_id						BIGINT			 	NOT NULL ,
  drug_concept_id				INTEGER			  	NOT NULL ,
  drug_exposure_start_date		DATE			    NULL ,
  drug_exposure_start_datetime	TIMESTAMP		 	NOT NULL ,
  drug_exposure_end_date		DATE			    NULL ,
  drug_exposure_end_datetime	TIMESTAMP		  	NOT NULL ,
  verbatim_end_date				DATE			    NULL ,
  drug_type_concept_id			INTEGER			  	NOT NULL ,
  stop_reason					VARCHAR(20)			NULL ,
  refills						INTEGER		  		NULL ,
  quantity						NUMERIC			    NULL ,
  days_supply					INTEGER		  		NULL ,
  sig							TEXT				NULL ,
  route_concept_id				INTEGER				NOT NULL ,
  lot_number					VARCHAR(50)	 		NULL ,
  provider_id					BIGINT			  	NULL ,
  visit_occurrence_id			BIGINT			  	NULL ,
  visit_detail_id               BIGINT       		NULL ,
  drug_source_value				VARCHAR(50)	  		NULL ,
  drug_source_concept_id		INTEGER			  	NOT NULL ,
  route_source_value			VARCHAR(50)	  		NULL ,
  dose_unit_source_value		VARCHAR(50)	  		NULL
)
;


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE device_exposure
(
  device_exposure_id			    BIGINT		  	NOT NULL ,
  person_id						    BIGINT			NOT NULL ,
  device_concept_id			        INTEGER			NOT NULL ,
  device_exposure_start_date	    DATE			NULL ,
  device_exposure_start_datetime	TIMESTAMP		NOT NULL ,
  device_exposure_end_date		    DATE			NULL ,
  device_exposure_end_datetime    	TIMESTAMP		NULL ,
  device_type_concept_id		    INTEGER			NOT NULL ,
  unique_device_id			        VARCHAR(50)		NULL ,
  quantity						    INTEGER			NULL ,
  provider_id					    BIGINT			NULL ,
  visit_occurrence_id			    BIGINT			NULL ,
  visit_detail_id                 	BIGINT       	NULL ,
  device_source_value			    VARCHAR(100)	NULL ,
  device_source_concept_id		    INTEGER			NOT NULL
)
;


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE condition_occurrence
(
  condition_occurrence_id		BIGINT			NOT NULL ,
  person_id						BIGINT			NOT NULL ,
  condition_concept_id			INTEGER			NOT NULL ,
  condition_start_date			DATE			NULL ,
  condition_start_datetime		TIMESTAMP		NULL ,
  condition_end_date			DATE			NULL ,
  condition_end_datetime		TIMESTAMP		NULL ,
  condition_type_concept_id		INTEGER			NOT NULL ,
  condition_status_concept_id	INTEGER			NOT NULL ,
  stop_reason					VARCHAR(20)		NULL ,
  provider_id					BIGINT			NULL ,
  visit_occurrence_id			BIGINT			NULL ,
  visit_detail_id               BIGINT	     	NULL ,
  condition_source_value		VARCHAR(1000)	NULL ,
  condition_source_concept_id	VARCHAR(50)		NULL ,
  condition_status_source_value	VARCHAR(1000)	NULL
)
;


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE measurement
(
  measurement_id				BIGINT			NOT NULL ,
  person_id						BIGINT			NOT NULL ,
  measurement_concept_id		INTEGER			NOT NULL ,
  measurement_date				DATE			NULL ,
  measurement_datetime			TIMESTAMP		NOT NULL ,
  measurement_time              VARCHAR(10) 	NULL,
  measurement_type_concept_id	INTEGER			NOT NULL ,
  operator_concept_id			INTEGER			NULL ,
  value_as_number				NUMERIC			NULL ,
  value_as_concept_id			INTEGER			NULL ,
  unit_concept_id				INTEGER			NULL ,
  range_low					    NUMERIC			NULL ,
  range_high					NUMERIC			NULL ,
  provider_id					BIGINT			NULL ,
  visit_occurrence_id			BIGINT			NULL ,
  visit_detail_id               BIGINT	     	NULL ,
  measurement_source_value		VARCHAR(50)		NULL ,
  measurement_source_concept_id	INTEGER			NOT NULL ,
  unit_source_value				VARCHAR(50)		NULL ,
  value_source_value			VARCHAR(50)		NULL
)
;


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE note
(
  note_id						BIGINT			NOT NULL ,
  person_id						BIGINT			NOT NULL ,
  note_event_id         		BIGINT      	NULL , 
  note_event_field_concept_id	INTEGER 		NOT NULL , 
  note_date						DATE			NULL ,
  note_datetime					TIMESTAMP		NOT NULL ,
  note_type_concept_id			INTEGER			NOT NULL ,
  note_class_concept_id 		INTEGER			NOT NULL ,
  note_title					VARCHAR(250)	NULL ,
  note_text						TEXT  			NULL ,
  encoding_concept_id			INTEGER			NOT NULL ,
  language_concept_id			INTEGER			NOT NULL ,
  provider_id					BIGINT			NULL ,
  visit_occurrence_id			BIGINT			NULL ,
  visit_detail_id       		BIGINT       	NULL ,
  note_source_value				VARCHAR(50)		NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE note_nlp
(
  note_nlp_id					BIGINT			NOT NULL ,
  note_id						BIGINT			NOT NULL ,
  section_concept_id			INTEGER			NOT NULL ,
  snippet						VARCHAR(250)	NULL ,
  "offset"					    VARCHAR(250)	NULL ,
  lexical_variant				VARCHAR(250)	NOT NULL ,
  note_nlp_concept_id			INTEGER			NOT NULL ,
  nlp_system					VARCHAR(250)	NULL ,
  nlp_date						DATE			NOT NULL ,
  nlp_datetime					TIMESTAMP		NULL ,
  term_exists					VARCHAR(1)		NULL ,
  term_temporal					VARCHAR(50)		NULL ,
  term_modifiers				VARCHAR(2000)	NULL ,
  note_nlp_source_concept_id  	INTEGER			NOT NULL
)
;


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE observation
(
  observation_id					BIGINT			NOT NULL ,
  person_id						    BIGINT			NOT NULL ,
  observation_concept_id			INTEGER			NOT NULL ,
  observation_date				    DATE			NULL ,
  observation_datetime				TIMESTAMP		NULL ,
  observation_type_concept_id	    INTEGER			NOT NULL ,
  value_as_number				    NUMERIC			NULL ,
  value_as_string				    VARCHAR(60)		NULL ,
  value_as_concept_id			    INTEGER			NULL ,
  qualifier_concept_id			    INTEGER			NULL ,
  unit_concept_id				    INTEGER			NULL ,
  provider_id					    BIGINT			NULL ,
  visit_occurrence_id			    BIGINT			NULL ,
  visit_detail_id               	BIGINT      	NULL ,
  observation_source_value		  	VARCHAR(50)		NULL ,
  observation_source_concept_id		INTEGER			NULL ,
  unit_source_value				    VARCHAR(50)		NULL ,
  qualifier_source_value			VARCHAR(50)		NULL ,
  observation_event_id				BIGINT			NULL , 
  obs_event_field_concept_id		INTEGER			NULL , 
  value_as_datetime					TIMESTAMP		NULL
)
;


--HINT DISTRIBUTE ON KEY(person_id)
CREATE TABLE survey_conduct 
(
  survey_conduct_id					BIGINT			NOT NULL ,
  person_id						    BIGINT			NOT NULL ,
  survey_concept_id			  		INTEGER			NOT NULL ,
  survey_start_date				    DATE			NULL ,
  survey_start_datetime				TIMESTAMP		NULL ,
  survey_end_date					DATE			NULL ,
  survey_end_datetime				TIMESTAMP		NOT NULL ,
  provider_id						BIGINT			NULL ,
  assisted_concept_id	  			INTEGER			NOT NULL ,
  respondent_type_concept_id		INTEGER			NOT NULL ,
  timing_concept_id					INTEGER			NOT NULL ,
  collection_method_concept_id		INTEGER			NOT NULL ,
  assisted_source_value		  		VARCHAR(50)		NULL ,
  respondent_type_source_value		VARCHAR(100)  	NULL ,
  timing_source_value				VARCHAR(100)	NULL ,
  collection_method_source_value	VARCHAR(100)	NULL ,
  survey_source_value				VARCHAR(100)	NULL ,
  survey_source_concept_id			INTEGER			NOT NULL ,
  survey_source_identifier			VARCHAR(100)	NULL ,
  validated_survey_concept_id		INTEGER			NOT NULL ,
  validated_survey_source_value		VARCHAR(100)	NULL ,
  survey_version_number				VARCHAR(20)		NULL ,
  visit_occurrence_id				BIGINT			NULL ,
  visit_detail_id					BIGINT			NULL ,
  response_visit_occurrence_id	BIGINT			NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE fact_relationship
(
  domain_concept_id_1		INTEGER			NOT NULL ,
  fact_id_1					BIGINT			NOT NULL ,
  domain_concept_id_2		INTEGER			NOT NULL ,
  fact_id_2					BIGINT			NOT NULL ,
  relationship_concept_id	INTEGER			NOT NULL
)
;



/************************

Standardized health system data

************************/


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE location
(
  location_id			BIGINT			NOT NULL ,
  address_1				VARCHAR(50)		NULL ,
  address_2				VARCHAR(50)		NULL ,
  city					VARCHAR(50)		NULL ,
  state					VARCHAR(2)		NULL ,
  zip					VARCHAR(9)		NULL ,
  county				VARCHAR(20)		NULL ,
  country				VARCHAR(100)	NULL ,
  location_source_value VARCHAR(50)		NULL ,
  latitude				NUMERIC			NULL ,
  longitude				NUMERIC			NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE location_history --Table added
(
  location_history_id           BIGINT      NOT NULL ,
  location_id			        BIGINT		NOT NULL ,
  relationship_type_concept_id	INTEGER		NOT NULL , 
  domain_id				        VARCHAR(50)	NOT NULL ,
  entity_id				        BIGINT		NOT NULL ,
  start_date			        DATE		NOT NULL ,
  end_date				        DATE		NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE care_site
(
  care_site_id					BIGINT			NOT NULL ,
  care_site_name				VARCHAR(255)	NULL ,
  place_of_service_concept_id	INTEGER			NULL ,
  location_id					BIGINT			NULL ,
  care_site_source_value		VARCHAR(50)		NULL ,
  place_of_service_source_value	VARCHAR(50)		NULL
)
;


--HINT DISTRIBUTE ON RANDOM
CREATE TABLE provider
(
  provider_id					BIGINT			NOT NULL ,
  provider_name					VARCHAR(255)	NULL ,
  NPI							VARCHAR(20)		NULL ,
  DEA							VARCHAR(20)		NULL ,
  specialty_concept_id			INTEGER			NOT NULL ,
  care_site_id					BIGINT			NULL ,
  year_of_birth					INTEGER			NULL ,
  gender_concept_id				INTEGER			NOT NULL ,
  provider_source_value			VARCHAR(50)		NULL ,
  specialty_source_value		VARCHAR(50)		NULL ,
  specialty_source_concept_id	INTEGER			NULL ,
  gender_source_value			VARCHAR(50)		NULL ,
  gender_source_concept_id		INTEGER			NOT NULL
)
;


/************************

Standardized health economics

************************/


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE payer_plan_period
(
  payer_plan_period_id			BIGINT			NOT NULL ,
  person_id						BIGINT			NOT NULL ,
  contract_person_id            BIGINT        	NULL ,
  payer_plan_period_start_date  DATE			NOT NULL ,
  payer_plan_period_end_date	DATE			NOT NULL ,
  payer_concept_id              INTEGER       	NOT NULL ,
  plan_concept_id               INTEGER       	NOT NULL ,
  contract_concept_id           INTEGER       	NOT NULL ,
  sponsor_concept_id            INTEGER       	NOT NULL ,
  stop_reason_concept_id        INTEGER       	NOT NULL ,
  payer_source_value			VARCHAR(50)	  	NULL ,
  payer_source_concept_id       INTEGER       	NOT NULL ,
  plan_source_value				VARCHAR(50)	  	NULL ,
  plan_source_concept_id        INTEGER       	NOT NULL ,
  contract_source_value         VARCHAR(50)   	NULL ,
  contract_source_concept_id    INTEGER       	NOT NULL ,
  sponsor_source_value          VARCHAR(50)   	NULL ,
  sponsor_source_concept_id     INTEGER       	NOT NULL ,
  family_source_value			VARCHAR(50)	  	NULL ,
  stop_reason_source_value      VARCHAR(50)   	NULL ,
  stop_reason_source_concept_id INTEGER       	NOT NULL
)
;




--HINT DISTRIBUTE ON KEY(person_id)
CREATE TABLE cost
(
  cost_id						BIGINT		NOT NULL ,
  person_id						BIGINT		NOT NULL,
  cost_event_id					BIGINT      NOT NULL ,
  cost_event_field_concept_id	INTEGER		NOT NULL , 
  cost_concept_id				INTEGER		NOT NULL ,
  cost_type_concept_id		  	INTEGER     NOT NULL ,
  currency_concept_id			INTEGER		NOT NULL ,
  cost							NUMERIC		NULL ,
  incurred_date					DATE		NOT NULL ,
  billed_date					DATE		NULL ,
  paid_date						DATE		NULL ,
  revenue_code_concept_id		INTEGER		NOT NULL ,
  drg_concept_id			    INTEGER		NOT NULL ,
  cost_source_value				VARCHAR(50)	NULL ,
  cost_source_concept_id	  	INTEGER		NOT NULL ,
  revenue_code_source_value 	VARCHAR(50) NULL ,
  drg_source_value			    VARCHAR(3)	NULL ,
  payer_plan_period_id			BIGINT		NULL
)
;


/************************

Standardized derived elements

************************/


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE drug_era
(
  drug_era_id				BIGINT		NOT NULL ,
  person_id					BIGINT		NOT NULL ,
  drug_concept_id			INTEGER		NOT NULL ,
  drug_era_start_datetime	TIMESTAMP		NOT NULL ,
  drug_era_end_datetime		TIMESTAMP		NOT NULL ,
  drug_exposure_count		INTEGER		NULL ,
  gap_days					INTEGER		NULL
)
;


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE dose_era
(
  dose_era_id				BIGINT			NOT NULL ,
  person_id					BIGINT			NOT NULL ,
  drug_concept_id			INTEGER			NOT NULL ,
  unit_concept_id			INTEGER			NOT NULL ,
  dose_value				NUMERIC			NOT NULL ,
  dose_era_start_datetime	TIMESTAMP		NOT NULL ,
  dose_era_end_datetime		TIMESTAMP		NOT NULL
)
;


--HINT DISTRIBUTE_ON_KEY(person_id)
CREATE TABLE condition_era
(
  condition_era_id					BIGINT			NOT NULL ,
  person_id							BIGINT			NOT NULL ,
  condition_concept_id				INTEGER			NOT NULL ,
  condition_era_start_datetime		TIMESTAMP		NOT NULL ,
  condition_era_end_datetime		TIMESTAMP		NOT NULL ,
  condition_occurrence_count		INTEGER			NULL
)
;

/*******************************
 
 Genomic Standardized elements
 
 *******************************/


CREATE TABLE genomic_test
(
  genomic_test_id					BIGINT			NOT NULL ,
  care_site_id						BIGINT			NOT NULL ,
  genomic_test_name 				VARCHAR(50)		NOT NULL ,
  genomic_test_version				VARCHAR(100)	NULL ,
  reference_genome					VARCHAR(50)		NULL ,
  sequencing_device					VARCHAR(50)		NULL ,
  target_capture					INTEGER			NULL,
  read_type							VARCHAR(50)		NULL,
  read_length						INTEGER 		NULL,
  alignment_tool					VARCHAR(50)		NULL,
  variant_callinf_tool				VARCHAR(50)		NULL,
  chromosome_coordinate				VARCHAR(50)		NULL,
  annotation_tool					VARCHAR(50)		NULL,
  annotation_database				VARCHAR(50)		NULL
)
;


CREATE TABLE target_gene
(
  target_gene_id					BIGINT			NOT NULL ,
  genomic_test_id					BIGINT			NOT NULL ,
  target_gene_source_value 			VARCHAR(50)		NOT NULL ,
  chromosome_id						VARCHAR(50)		NULL ,
  start_position					BIGINT			NULL,
  end_position						BIGINT			NULL,
  target_gene_source_id				VARCHAR(1000)	NULL,
  druggability_source_value			VARCHAR(1000)   NULL
)
;




CREATE TABLE variant_annotation
(
  variant_annotation_id				BIGINT			NOT NULL ,
  variant_occurrence_id				BIGINT			NOT NULL ,
  annotation_field 					VARCHAR(50)		NOT NULL ,
  value_as_string					VARCHAR(50)		NULL ,
  value_as_number					INTEGER 		NULL ,
  pathogenic						VARCHAR(100)	NULL
)
;



CREATE TABLE variant_occurrence
(
  variant_occurrence_id				BIGINT			NOT NULL ,
  procedure_occurrence_id 			BIGINT			NOT NULL ,
  specimen_id						BIGINT			NOT NULL ,
  reference_specimen_id				BIGINT			NOT NULL ,
  target_gene_id					BIGINT 				NOT NULL ,
  target_gene_symbol				VARCHAR(50)		NULL,
  target_gene_id2					BIGINT			NULL,
  reference_sequence				VARCHAR(1000)	NULL,
  rs_id								VARCHAR(500)		NULL,
  reference_allele					VARCHAR(1000)		NULL,
  alternate_allele					VARCHAR(1000)		NULL,
  alternate_allele2					VARCHAR(1000)		NULL,
  hgvs_c							VARCHAR(1000)		NULL,
  hgvs_p							VARCHAR(1000)		NULL,
  variant_read_depth				VARCHAR(50)			NULL,
  total_read_depth					VARCHAR(50)			NULL,
  variant_exon						VARCHAR(1000)		NULL,
  sequence_alteration				VARCHAR(1000)		NULL,
  variant_feature					VARCHAR(1000)		NULL,
  genetic_origin					VARCHAR(1000)		NULL,
  genotype							VARCHAR(1000)		NULL
)
;



CREATE TABLE pathway_occurrence
(
  pathway_occurrence_id				BIGINT			NOT NULL ,
  pathway_annotation_id 			BIGINT			NULL ,
  target_gene_id					BIGINT			NOT NULL ,
  pathway_source_id					VARCHAR(1000)	NOT NULL ,
  target_gene_source_id				VARCHAR(1000) 	NOT NULL 
) 
;



 CREATE TABLE pathway_annotation
(
  pathway_annotation_id				BIGINT			NOT NULL ,
  pathway_source_id					VARCHAR(1000)	NOT NULL ,
  pathway_source_value				VARCHAR(1000)	NOT NULL 
 ) 
 ;
 
CREATE TABLE interaction_occurrence
(
  interaction_occurrence_id				BIGINT			NOT NULL ,
  target_gene_id						BIGINT			NOT NULL ,
  target_gene_source_value				VARCHAR(1000)	NOT null,
  drug_source_value						VARCHAR(1000)   NOT null,
  interaction_source_value				VARCHAR(1000)	not null,
  score_as_numer						VARCHAR(1000)	not null	
 ) 
 ;

\*****************************

Vocabulary upload

******************************\

COPY CONCEPT FROM 'C:\Users\mellil\Desktop\TGCA data\Vocabulary\CONCEPT.csv' WITH DELIMITER E'\t' CSV HEADER QUOTE E'\b' ;

COPY DRUG_STRENGTH FROM 'C:\Users\mellil\Desktop\TGCA data\Vocabulary\DRUG_STRENGTH.csv' WITH DELIMITER E'\t' CSV HEADER QUOTE E'\b' ; 

COPY CONCEPT_RELATIONSHIP FROM 'C:\Users\mellil\Desktop\TGCA data\Vocabulary\CONCEPT_RELATIONSHIP.csv' WITH DELIMITER E'\t' CSV HEADER QUOTE E'\b' ;

COPY CONCEPT_ANCESTOR FROM 'C:\Users\mellil\Desktop\TGCA data\Vocabulary\CONCEPT_ANCESTOR.csv' WITH DELIMITER E'\t' CSV HEADER QUOTE E'\b' ;

COPY CONCEPT_SYNONYM FROM 'C:\Users\mellil\Desktop\TGCA data\Vocabulary\CONCEPT_SYNONYM.csv' WITH DELIMITER E'\t' CSV HEADER QUOTE E'\b' ;

COPY VOCABULARY FROM 'C:\Users\mellil\Desktop\TGCA data\Vocabulary\VOCABULARY.csv' WITH DELIMITER E'\t' CSV HEADER QUOTE E'\b' ;

COPY RELATIONSHIP FROM 'C:\Users\mellil\Desktop\TGCA data\Vocabulary\RELATIONSHIP.csv' WITH DELIMITER E'\t' CSV HEADER QUOTE E'\b' ;

COPY CONCEPT_CLASS FROM 'C:\Users\mellil\Desktop\TGCA data\Vocabulary\CONCEPT_CLASS.csv' WITH DELIMITER E'\t' CSV HEADER QUOTE E'\b' ;

COPY DOMAIN FROM 'C:\Users\mellil\Desktop\TGCA data\Vocabulary\DOMAIN.csv' WITH DELIMITER E'\t' CSV HEADER QUOTE E'\b' ;


\*****************************

TCGA data upload

******************************\


COPY person(person_id, gender_concept_id, year_of_birth, month_of_birth, day_of_birth, birth_datetime, death_datetime, race_concept_id, ethnicity_concept_id, person_source_value, gender_source_value, race_source_value, ethnicity_source_value) FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Person.csv' CSV HEADER DELIMITER',' NULL 'NA';

COPY care_site(care_site_id, care_site_name, place_of_service_source_value, location_id) FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Care_site.csv' CSV HEADER DELIMITER',' NULL 'NA';

COPY condition_occurrence(condition_occurrence_id, person_id, condition_concept_id, condition_start_datetime, condition_type_concept_id, condition_status_concept_id, visit_occurrence_id, condition_source_value, condition_source_concept_id, condition_status_source_value) FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Condition_occurrence.csv' CSV HEADER DELIMITER ',' NULL 'NA';

COPY observation_period(observation_period_id, person_id, observation_period_start_date, observation_period_end_date, period_type_concept_id) FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Observation_period.csv' CSV HEADER DELIMITER ',' NULL 'NA';

COPY procedure_occurrence(procedure_occurrence_id, person_id, procedure_concept_id, procedure_datetime, procedure_type_concept_id, visit_occurrence_id, procedure_source_value) FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Procedure_occurrence.csv' CSV HEADER DELIMITER ',' NULL 'NA';

COPY specimen(specimen_id, person_id, specimen_concept_id, specimen_type_concept_id, specimen_datetime, anatomic_site_concept_id, disease_status_concept_id, specimen_source_id, specimen_source_value, anatomic_site_source_value) FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Specimen.csv' CSV HEADER DELIMITER ',' NULL 'NA';

COPY visit_occurrence(visit_occurrence_id, person_id, visit_concept_id, visit_start_datetime, visit_end_datetime, visit_type_concept_id, visit_source_value) FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Visit_occurrence.csv' CSV HEADER DELIMITER ',' NULL 'NA';

COPY observation(observation_id, person_id, observation_concept_id, observation_date, observation_type_concept_id, value_as_string, visit_occurrence_id, observation_source_value) FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Observations.csv' CSV HEADER DELIMITER ',' NULL 'NA';

COPY genomic_test FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Genomic_test.csv' CSV HEADER DELIMITER ',' NULL 'NA';

COPY target_gene FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Target_gene.csv' CSV HEADER DELIMITER ',' NULL 'NA';

COPY variant_annotation FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Variant_annotation.csv' CSV HEADER DELIMITER ',' NULL 'NA';

COPY variant_occurrence FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Variant_occurrence.csv' CSV HEADER DELIMITER ',' NULL 'NA';

COPY pathway_occurrence FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Pathway_occurrence.csv' CSV HEADER DELIMITER ',' NULL 'NA';

COPY pathway_annotation FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\Pathway_annotation.csv' CSV HEADER DELIMITER ',' NULL 'NA';

COPY interaction_occurrence FROM 'C:\Users\mellil\Desktop\TGCA data\Data_to be used_ETL\Tables\interaction_occurrence.csv' CSV HEADER DELIMITER ',' NULL 'NA';

/************************************************************************************************************************************************************************************************************************

/************************
*************************
*************************
*************************

Primary key constraints

*************************
*************************
*************************
************************/



/************************

Standardized vocabulary

************************/

ALTER TABLE concept ADD CONSTRAINT xpk_concept PRIMARY KEY (concept_id);

ALTER TABLE vocabulary ADD CONSTRAINT xpk_vocabulary PRIMARY KEY (vocabulary_id);

ALTER TABLE domain ADD CONSTRAINT xpk_domain PRIMARY KEY (domain_id);

ALTER TABLE concept_class ADD CONSTRAINT xpk_concept_class PRIMARY KEY (concept_class_id);

ALTER TABLE concept_relationship ADD CONSTRAINT xpk_concept_relationship PRIMARY KEY (concept_id_1,concept_id_2,relationship_id);

ALTER TABLE relationship ADD CONSTRAINT xpk_relationship PRIMARY KEY (relationship_id);

ALTER TABLE concept_ancestor ADD CONSTRAINT xpk_concept_ancestor PRIMARY KEY (ancestor_concept_id,descendant_concept_id);

ALTER TABLE source_to_concept_map ADD CONSTRAINT xpk_source_to_concept_map PRIMARY KEY (source_vocabulary_id,target_concept_id,source_code,valid_end_date);

ALTER TABLE drug_strength ADD CONSTRAINT xpk_drug_strength PRIMARY KEY (drug_concept_id, ingredient_concept_id);


/************************

Standardized clinical data

************************/


/**PRIMARY KEY NONCLUSTERED constraints**/

ALTER TABLE person ADD CONSTRAINT xpk_person PRIMARY KEY ( person_id ) ;

ALTER TABLE observation_period ADD CONSTRAINT xpk_observation_period PRIMARY KEY ( observation_period_id ) ;

ALTER TABLE specimen ADD CONSTRAINT xpk_specimen PRIMARY KEY ( specimen_id ) ;

ALTER TABLE visit_occurrence ADD CONSTRAINT xpk_visit_occurrence PRIMARY KEY ( visit_occurrence_id ) ;

ALTER TABLE visit_detail ADD CONSTRAINT xpk_visit_detail PRIMARY KEY ( visit_detail_id ) ;

ALTER TABLE procedure_occurrence ADD CONSTRAINT xpk_procedure_occurrence PRIMARY KEY ( procedure_occurrence_id ) ;

ALTER TABLE drug_exposure ADD CONSTRAINT xpk_drug_exposure PRIMARY KEY ( drug_exposure_id ) ;

ALTER TABLE device_exposure ADD CONSTRAINT xpk_device_exposure PRIMARY KEY ( device_exposure_id ) ;

ALTER TABLE condition_occurrence ADD CONSTRAINT xpk_condition_occurrence PRIMARY KEY ( condition_occurrence_id ) ;

ALTER TABLE measurement ADD CONSTRAINT xpk_measurement PRIMARY KEY ( measurement_id ) ;

ALTER TABLE note ADD CONSTRAINT xpk_note PRIMARY KEY ( note_id ) ;

ALTER TABLE note_nlp ADD CONSTRAINT xpk_note_nlp PRIMARY KEY ( note_nlp_id ) ;

ALTER TABLE observation  ADD CONSTRAINT xpk_observation PRIMARY KEY ( observation_id ) ;

ALTER TABLE survey_conduct ADD CONSTRAINT xpk_survey PRIMARY KEY ( survey_conduct_id ) ;

/************************

Genomic data 

************************/


ALTER TABLE genomic_test ADD CONSTRAINT xpk_test PRIMARY KEY ( genomic_test_id ) ;

ALTER TABLE target_gene ADD CONSTRAINT xpk_target_gene PRIMARY KEY ( target_gene_id ) ;

ALTER TABLE variant_annotation ADD CONSTRAINT xpk_variant_annotation PRIMARY KEY ( variant_annotation_id ) ;

ALTER TABLE variant_occurrence ADD CONSTRAINT xpk_variant_occurrence PRIMARY KEY ( variant_occurrence_id ) ;

ALTER TABLE pathway_annotation ADD CONSTRAINT xpk_pathway_annotation PRIMARY KEY ( pathway_annotation_id ) ;

ALTER TABLE pathway_occurrence ADD CONSTRAINT xpk_pathway_occurrence PRIMARY KEY ( pathway_occurrence_id ) ;

ALTER TABLE interaction_occurrence ADD CONSTRAINT xpk_interaction_occurrence PRIMARY KEY (interaction_occurrence_id ) ;

ALTER TABLE care_site  ADD CONSTRAINT xpk_care_site PRIMARY KEY (care_site_id ) ;

/************************
*************************
*************************
*************************

Indices

*************************
*************************
*************************
************************/

/************************

Standardized vocabulary

************************/

CREATE UNIQUE INDEX idx_concept_concept_id  ON concept  (concept_id ASC);
CLUSTER concept  USING idx_concept_concept_id ;
CREATE INDEX idx_concept_code ON concept (concept_code ASC);
CREATE INDEX idx_concept_vocabluary_id ON concept (vocabulary_id ASC);
CREATE INDEX idx_concept_domain_id ON concept (domain_id ASC);
CREATE INDEX idx_concept_class_id ON concept (concept_class_id ASC);
CREATE INDEX idx_concept_id_varchar ON concept (CAST(concept_id AS VARCHAR));

CREATE UNIQUE INDEX idx_vocabulary_vocabulary_id  ON vocabulary  (vocabulary_id ASC);
CLUSTER vocabulary  USING idx_vocabulary_vocabulary_id ;

CREATE UNIQUE INDEX idx_domain_domain_id  ON domain  (domain_id ASC);
CLUSTER domain  USING idx_domain_domain_id ;

CREATE UNIQUE INDEX idx_concept_class_class_id  ON concept_class  (concept_class_id ASC);
CLUSTER concept_class  USING idx_concept_class_class_id ;

CREATE INDEX idx_concept_relationship_id_1 ON concept_relationship (concept_id_1 ASC);
CREATE INDEX idx_concept_relationship_id_2 ON concept_relationship (concept_id_2 ASC);
CREATE INDEX idx_concept_relationship_id_3 ON concept_relationship (relationship_id ASC);

CREATE UNIQUE INDEX idx_relationship_rel_id  ON relationship  (relationship_id ASC);
CLUSTER relationship  USING idx_relationship_rel_id ;

CREATE INDEX idx_concept_synonym_id  ON concept_synonym  (concept_id ASC);
CLUSTER concept_synonym  USING idx_concept_synonym_id ;

CREATE INDEX idx_concept_ancestor_id_1  ON concept_ancestor  (ancestor_concept_id ASC);
CLUSTER concept_ancestor  USING idx_concept_ancestor_id_1 ;
CREATE INDEX idx_concept_ancestor_id_2 ON concept_ancestor (descendant_concept_id ASC);

CREATE INDEX idx_source_to_concept_map_id_3  ON source_to_concept_map  (target_concept_id ASC);
CLUSTER source_to_concept_map  USING idx_source_to_concept_map_id_3 ;
CREATE INDEX idx_source_to_concept_map_id_1 ON source_to_concept_map (source_vocabulary_id ASC);
CREATE INDEX idx_source_to_concept_map_id_2 ON source_to_concept_map (target_vocabulary_id ASC);
CREATE INDEX idx_source_to_concept_map_code ON source_to_concept_map (source_code ASC);

CREATE INDEX idx_drug_strength_id_1  ON drug_strength  (drug_concept_id ASC);
CLUSTER drug_strength  USING idx_drug_strength_id_1 ;
CREATE INDEX idx_drug_strength_id_2 ON drug_strength (ingredient_concept_id ASC);


/************************

Standardized clinical data

************************/

CREATE UNIQUE INDEX idx_person_id  ON person  (person_id ASC);
CLUSTER person  USING idx_person_id ;

CREATE INDEX idx_observation_period_id  ON observation_period  (person_id ASC);
CLUSTER observation_period  USING idx_observation_period_id ;

CREATE INDEX idx_specimen_person_id  ON specimen  (person_id ASC);
CLUSTER specimen  USING idx_specimen_person_id ;
CREATE INDEX idx_specimen_concept_id ON specimen (specimen_concept_id ASC);

CREATE INDEX idx_visit_person_id  ON visit_occurrence  (person_id ASC);
CLUSTER visit_occurrence  USING idx_visit_person_id ;
CREATE INDEX idx_visit_concept_id ON visit_occurrence (visit_concept_id ASC);

CREATE INDEX idx_visit_detail_person_id  ON visit_detail  (person_id ASC);
CLUSTER visit_detail  USING idx_visit_detail_person_id ;
CREATE INDEX idx_visit_detail_concept_id ON visit_detail (visit_detail_concept_id ASC);

CREATE INDEX idx_procedure_person_id  ON procedure_occurrence  (person_id ASC);
CLUSTER procedure_occurrence  USING idx_procedure_person_id ;
CREATE INDEX idx_procedure_concept_id ON procedure_occurrence (procedure_concept_id ASC);
CREATE INDEX idx_procedure_visit_id ON procedure_occurrence (visit_occurrence_id ASC);

CREATE INDEX idx_drug_person_id  ON drug_exposure  (person_id ASC);
CLUSTER drug_exposure  USING idx_drug_person_id ;
CREATE INDEX idx_drug_concept_id ON drug_exposure (drug_concept_id ASC);
CREATE INDEX idx_drug_visit_id ON drug_exposure (visit_occurrence_id ASC);

CREATE INDEX idx_device_person_id  ON device_exposure  (person_id ASC);
CLUSTER device_exposure  USING idx_device_person_id ;
CREATE INDEX idx_device_concept_id ON device_exposure (device_concept_id ASC);
CREATE INDEX idx_device_visit_id ON device_exposure (visit_occurrence_id ASC);

CREATE INDEX idx_condition_person_id  ON condition_occurrence  (person_id ASC);
CLUSTER condition_occurrence  USING idx_condition_person_id ;
CREATE INDEX idx_condition_concept_id ON condition_occurrence (condition_concept_id ASC);
CREATE INDEX idx_condition_visit_id ON condition_occurrence (visit_occurrence_id ASC);

CREATE INDEX idx_measurement_person_id  ON measurement  (person_id ASC);
CLUSTER measurement  USING idx_measurement_person_id ;
CREATE INDEX idx_measurement_concept_id ON measurement (measurement_concept_id ASC);
CREATE INDEX idx_measurement_visit_id ON measurement (visit_occurrence_id ASC);

CREATE INDEX idx_note_person_id  ON note  (person_id ASC);
CLUSTER note  USING idx_note_person_id ;
CREATE INDEX idx_note_concept_id ON note (note_type_concept_id ASC);
CREATE INDEX idx_note_visit_id ON note (visit_occurrence_id ASC);

CREATE INDEX idx_note_nlp_note_id  ON note_nlp  (note_id ASC);
CLUSTER note_nlp  USING idx_note_nlp_note_id ;
CREATE INDEX idx_note_nlp_concept_id ON note_nlp (note_nlp_concept_id ASC);

CREATE INDEX idx_observation_person_id  ON observation  (person_id ASC);
CLUSTER observation  USING idx_observation_person_id ;
CREATE INDEX idx_observation_concept_id ON observation (observation_concept_id ASC);
CREATE INDEX idx_observation_visit_id ON observation (visit_occurrence_id ASC);

CREATE INDEX idx_survey_person_id  ON survey_conduct  (person_id ASC);
CLUSTER survey_conduct  USING idx_survey_person_id ;

CREATE INDEX idx_fact_relationship_id_1 ON fact_relationship (domain_concept_id_1 ASC);
CREATE INDEX idx_fact_relationship_id_2 ON fact_relationship (domain_concept_id_2 ASC);
CREATE INDEX idx_fact_relationship_id_3 ON fact_relationship (relationship_concept_id ASC);

CREATE INDEX idx_genomic_test_id ON genomic_test (genomic_test_id ASC);

CREATE INDEX idx_target_gene_genomic_test_id  ON target_gene (genomic_test_id ASC);
CLUSTER target_gene USING idx_target_gene_genomic_test_id;
CREATE INDEX idx_target_gene_id ON target_gene (target_gene_id ASC);

CREATE INDEX idx_specimen_id  ON variant_occurrence (specimen_id ASC);
CLUSTER variant_occurrence USING idx_specimen_id;
CREATE INDEX idx_target_gene1_id  ON variant_occurrence (target_gene_id ASC);
CREATE INDEX idx_target_gene2_id  ON variant_occurrence (target_gene_id2 ASC);
CREATE INDEX idx_variant_occurrence_id  ON variant_occurrence (variant_occurrence_id ASC);

create index idx_variant_occ_variant_annotation on variant_annotation (variant_occurrence_id ASC);
cluster variant_annotation using idx_variant_occ_variant_annotation ;
CREATE INDEX idx_variant_annotation_id ON variant_annotation (variant_annotation_id ASC);

create index idx_pathway_occurrence_target_gene_id on pathway_occurrence (target_gene_id ASC);
cluster pathway_occurrence using idx_pathway_occurrence_target_gene_id;
create index idx_pathway_occurrence_pathway_annotation_id on pathway_occurrence (pathway_annotation_id ASC);
create index idx_pathway_occurrence_pathway_occurrence_id on pathway_occurrence (pathway_occurrence_id ASC);

create index idx_pathway_annotation_pathway_annotation_id on pathway_annotation (pathway_annotation_id ASC);

create index idx_target_gene_id_int_occ on interaction_occurrence (target_gene_id ASC);
cluster interaction_occurrence using idx_target_gene_id_int_occ;
create index idx_interaction_occ_id on interaction_occurrence (interaction_occurrence_id ASC);

drop table attribute_definition ;
drop table cdm_source ;
drop table condition_era ;
drop table "cost";
drop table device_exposure;
drop table dose_era;
drop table drug_era;
drop table drug_exposure;
drop table drug_strength;
drop table "location" ;
drop table location_history ;
drop table metadata;
drop table note ;
drop table note_nlp;
drop table payer_plan_period ;
drop table provider ;
drop table survey_conduct ;
drop table visit_detail ;
drop table fact_relationship ;
