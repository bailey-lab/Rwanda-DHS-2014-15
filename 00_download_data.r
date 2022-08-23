#pull rwanda dhs data from website

#download rdhs package
library(rdhs) 

#obtain survey microdatasets
surveys <- dhs_surveys(indicatorIds = c("CH_DIAR_C_DIA", "ML_PMAL_C_RDT", "ML_FEVR_C_FEV","CH_ARIS_C_ARI"),
                       countryIds =  "RW", #rwanda
                       surveyType = c("DHS"),
                       surveyYearStart = 2013) #return the desired surveys


datasets = dhs_datasets(surveyIds = surveys$SurveyId, fileFormat = "FL", fileType = c("PR", "AR")) #household member recode, hiv

#need to have DHS account, project, and permissions
set_rdhs_config(email = "insert email here",
                project = "insert project here",
                global=FALSE)

downloaded_data <- get_datasets(datasets$FileName, clear_cache = TRUE)

#read datasets
#pull certain variables of interest
vars <- search_variable_labels(
  datasets$FileName,
  search_terms = c("malaria rapid test",
                   "diarrhea recently",
                   "fever .* two weeks",
                   "breaths",
                   "cluster", "household", "urban", "rural", "Ease of Access to Treatment",
                   "Province", "region", "Sample Weight", "child's age", "weight",
                   "wealth", "education", "net", "mother's age", "vaccination",
                   "toilet", "water source", "treatment", "housing type", "cooking fuel", 
                   "blood sample ID number")) #pull variables related to these terms


#can also pull specific DHS variables if the search terms doesn't work-- can find on DHS website 
vars1 <- search_variables(datasets$FileName, variables = c("ha62", "hb62", "hml32","sh312", 
                                                           "hv103", "ha70","ha63", "hb63", "hb70", "hml34", "hiv03", "hiv01", "hiv02", "hivline", "hvidx"))#specify specific variables you want to pull


vars <- rbind(vars, vars1) #make full list 

extract <- extract_dhs(vars, add_geo = TRUE) #extract the data

rwanda_data = rbind_labelled(extract[2], variables = vars) #only include household member recode but with barcodes

rwanda_hiv_data <- rbind_labelled(extract[1], variables = vars)

#clean it up a bit, get rid of duplicate variables
rwanda_hiv_data$hiv01.1 <- NULL
rwanda_hiv_data$hiv03.1 <- NULL
rwanda_hiv_data$hiv03.2 <- NULL
rwanda_hiv_data$hiv03.3 <- NULL
rwanda_hiv_data$hiv01.2 <- NULL
rwanda_hiv_data$hiv02.1 <- NULL
rwanda_hiv_data$hiv01.3 <- NULL
rwanda_hiv_data$hiv02.2 <- NULL




