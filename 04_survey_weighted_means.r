#survey weighted malaria and mutation means

# load libraries
library(foreign)
library(plyr)
library(dplyr)
library(srvyr)

#read in base dataset 
rwanda_data4 <- readRDS(rwanda_data4, file = "/path_to_file/rwanda_data4.rds")

#organize weighted dataset
#by individual
weighted_individual_malaria <- filter(rwanda_data4, !is.na(hml35),!is.na(hml32))
weighted_individual_malaria$weight<- weighted_individual_malaria$hv005/1000000

#this gives us number tested at each study site in malaria arm
anemia_arm_weighted_cluster_malaria <- group_by(weighted_individual_malaria, hv001) %>% summarize(n_pos_rdt = sum(hml35, na.rm = TRUE), n_pos_mic = sum(hml32, na.rm = TRUE),
                                                                                                  n_pos_total = sum(malaria, na.rm=TRUE), prev_malaria_rdt = (mean(hml35, na.rm = TRUE) * 100),
                                                                                                  prev_malaria_mic = (mean(hml32, na.rm = TRUE) * 100),prev_malaria_total = (mean(malaria, na.rm = TRUE) * 100),
                                                                                                  long = mean(LONGNUM.x), lat = mean(LATNUM.x), n_malaria_tested = n(), hv005 = mean(hv005),
                                                                                                  region = first(DHSREGNA.x)) #removes NAs to be consistent w DHS 

#saveRDS(anemia_arm_weighted_cluster_malaria, file= "/path_to_file/anemia_arm_weighted_cluster_malaria.RDS")
#write.csv(anemia_arm_weighted_cluster_malaria, file= "/path_to_file/anemia_arm_weighted_cluster_malaria.csv")

##weighted malaria prevalence by regions based on DHS anemia/malaria arm####
dstrata <- weighted_individual_malaria %>%
  as_survey_design(strata = DHSREGNA.x, weights = weight)

anemia_regions_weighted <- dstrata %>% 
  group_by(DHSREGNA.x) %>%
  summarise(malaria = survey_mean(malaria) *100)

#saveRDS(anemia_regions_weighted, file= "/path_to_file/anemia_regions_weighted.RDS")
#write.csv(anemia_regions_weighted, file= "/path_to_file/anemia_regions_weighted.csv")


###hiv arm weighted mutation prevalence###
#sequenced_samples_data <- readRDS(file = "/path_to_file/sequenced_samples_data.RDS") #read in file-- subset of high_prev_barcode_subset only including samples were successfully sequenced, adding in mutation variable based on haplotype analysis 

#manually enter the samples with mutations based on SeekDeep results
sequenced_samples_data$WildType <- 1
for(i in 1:351){
  if(sequenced_samples_data$mutation[i] != "Wild Type"){
    sequenced_samples_data$WildType[i] = 0
  }
}

sequenced_samples_data$C532W <- 1
for(i in 1:351){
  if(sequenced_samples_data$mutation[i] != "C532W"){
    sequenced_samples_data$C532W[i] = 0
  }
}

sequenced_samples_data$G533A <- 1
for(i in 1:351){
  if(sequenced_samples_data$mutation[i] != "G533A"){
    sequenced_samples_data$G533A[i] = 0
  }
}


sequenced_samples_data$G533G <- 1
for(i in 1:351){
  if(sequenced_samples_data$mutation[i] != "G533G"){
    sequenced_samples_data$G533G[i] = 0
  }
}


sequenced_samples_data$V555A <- 1
for(i in 1:351){
  if(sequenced_samples_data$mutation[i] != "V555A"){
    sequenced_samples_data$V555A[i] = 0
  }
}

sequenced_samples_data$R561H <- 1
for(i in 1:351){
  if(sequenced_samples_data$mutation[i] != "R561H"){
    sequenced_samples_data$R561H[i] = 0
  }
}

#saveRDS(sequenced_samples_data, file= "/path_to_file/sequenced_samples_data.RDS")

##hiv sample weight
sequenced_samples_data$hiv_weight <- sequenced_samples_data$hiv05/1000000 #add weight variable

dstrata3 <- sequenced_samples_data %>%
  as_survey_design(strata = DHSREGNA.x, weights = hiv_weight)

#percents of mutations by region
hiv_mutations_regions_weighted2 <- dstrata3 %>% 
  group_by(DHSREGNA.x) %>%
  summarise(R561H = survey_mean(R561H) *100, C532W = survey_mean(C532W) *100, 
            G533A = survey_mean(G533A) *100, G533G= survey_mean(G533G) *100, 
            V555A = survey_mean(V555A) *100, WildType = survey_mean(WildType)*100) 

#saveRDS(hiv_mutations_regions_weighted2, file= "/path_to_file/rwanda_dhs/hiv_mutations_regions_weighted_final.RDS")
#write.csv(hiv_mutations_regions_weighted2, file= "/path_to_file/hiv_mutations_regions_weighted_final.csv")

#overall percents, not divided by region/cluster
hiv_mutations_overall_weighted <- dstrata3 %>% 
  summarise(R561H = survey_mean(R561H) *100, C532W = survey_mean(C532W) *100,
            G533A = survey_mean(G533A) *100, G533G= survey_mean(G533G) *100,
            V555A = survey_mean(V555A) *100, WildType = survey_mean(WildType)*100)

#write.csv(hiv_mutations_overall_weighted, file= "/path_to_file/hiv_mutations_overall_weighted.csv")

#adding in mutation counts

#read in file-- subset of rwanda_data4 only including successfully sequenced samples and their mutation
#sequenced_samples_data <- readRDS(file= "/path_to_file/sequenced_samples_data.rds")

district_mutation_count <- as.data.frame.matrix(table(sequenced_samples_data$DHSREGNA.x, sequenced_samples_data$mutation))
#write.csv(district_mutation_count,"/path_to_file/district_mutation_count.csv")

#I merged district mutation count, hiv_mutations_overall_weighted2, and manually entered approximate lat and long of each district in excel to create district_mutation_count_percent_weighted used for plots

