#comparing malaria prevalence by cluster calculated by DHS (RDT, microscopy) and us (PCR) 

#calculating expected value based on cluster malaria compared to actual pcr1 value 

dhs_cluster_malaria <- readRDS(file = "/path_to_file/anemia_arm_weighted_cluster_malaria.RDS") #file created in 04_survey_weighted_means
pcr1_positive <- read.csv(file = "/path_to_file/sample_names_pcr2.csv") #file including sample names of PCR step 1 positive samples
rwanda_high_prev_barcode_subset <- readRDS(file = "/path_to_file/high_prev_barcode_subset.rds") #file created in 02_sorting_samples.r
plate_samples <- read.csv(file = "/path_to_file/rwanda_plates2.csv") #file with plate information such as rwanda_plates2 created in 02_sorting_samples.r

pcr_plate_samples <- plate_samples[plate_samples$sample.found == 1,]
pcr_plate_samples <- pcr_plate_samples[!is.na(pcr_plate_samples$sample.found),]

plate_samples_barcodes <- unique(plate_samples$hiv01)
dhs_data_plate_samples <- rwanda_high_prev_barcode_subset[(rwanda_high_prev_barcode_subset$hiv01) %in% plate_samples_barcodes,]


cluster_malaria <- readRDS(file = "path_to_file/cluster_malaria.rds") #cluster_malaria is available in repository 
high_prev <- data.frame(filter(cluster_malaria, prev_malaria_total > 15)) #> 15% malaria prevalence 

high_prev_weighted_individual_malaria <- weighted_individual_malaria[which(weighted_individual_malaria$hv001 %in% high_prev),] 

samples <- pcr1_positive$sample_name
combined <- rwanda_high_prev_barcode_subset[(plate_samples$sample_id) %in% samples,]

plate_samples$positive_pcr1 <- 0
plate_samples$positive_pcr1[which(plate_samples$sample_id %in% samples)] <- 1
experimental_cluster_malaria <- group_by(plate_samples, hv001..cluster.) %>% summarize(n_pos_pcr = sum(positive_pcr1), prev_malaria_pcr = (mean(positive_pcr1)*100))
names(experimental_cluster_malaria)[1] <- "hv001"
cluster_malaria_combined <- merge(dhs_cluster_malaria, experimental_cluster_malaria, by="hv001") #creates file with malaria prevalence by cluster for both pcr and rdt

cor(cluster_malaria_combined$prev_malaria_total, cluster_malaria_combined$prev_malaria_pcr) #Pearson coefficient of correlation

