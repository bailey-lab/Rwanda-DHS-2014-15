library(dplyr)

 #read in rwanda_data4 dataframe from last script
rwanda_data4 <- readRDS(rwanda_data4, file = "/path_to_file/rwanda_data4.rds") 

#summarize cluster malaria prevalence
cluster_malaria <- group_by(rwanda_data4, hv001) %>% summarize(n_pos_rdt = sum(hml35, na.rm = TRUE), n_pos_mic = sum(hml32, na.rm = TRUE),
                                                               n_pos_total = sum(malaria, na.rm = TRUE), prev_malaria_rdt = (mean(hml35, na.rm = TRUE) * 100), 
                                                               prev_malaria_mic = (mean(hml32, na.rm = TRUE) * 100),prev_malaria_total = (mean(malaria, na.rm = TRUE) * 100),
                                                               long = mean(LONGNUM.x), lat = mean(LATNUM.x), observations = n() )
mid_high_prev <- data.frame(filter(cluster_malaria, prev_malaria_total > 15)) #select clusters with > 15% 
mid_high_prev_clusters <- mid_high_prev$hv001 

#quick map to see distribution
#mapping it out
library(rgeos)
library(ggplot2)
library(sf)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)

world <- ne_countries(scale = "medium", returnclass = "sf") 
rwanda <-  subset(world, admin == "Rwanda")
#15% or higher
ggplot(data = rwanda)+
  geom_sf(fill = "blanchedalmond")+
  geom_point(data = cluster_malaria, aes(x = long, y = lat, fill = as.factor(mid_high_prev), color = as.factor(mid_high_prev)), size = 1, shape = 21)+
  labs(title = paste("mid_high prevalence (>15%) malaria clusters"))



barcodes <- as.vector(na.omit(rwanda_data4$hiv01))

rwanda_barcode_subset <- rwanda_data4[which(rwanda_data4$hiv01 %in% barcodes),]
#Q-- should this only include > 15 y/os? I left them in for now

#set the 99996, etc to NA

num <- c(99992, 99993, 99994,99995,99996,"'?    '","?    ") #dont know or missing etc
for (i in 1:nrow(rwanda_barcode_subset)){
  if(rwanda_barcode_subset$barcode[i] %in% num){
    rwanda_barcode_subset$barcode[i] <- NA
  } 
}

#saveRDS(rwanda_barcode_subset, file = "/path_to_file/rwanda_barcode_subset.rds") #save file

rwanda_high_prev_barcode_subset <- rwanda_barcode_subset[which(rwanda_barcode_subset$hv001 %in% mid_high_prev_clusters),] #select samples with barcodes from high prev clusters for subsample

#make a sheet with the samples and sample information to use in wet lab, pulling samples
rwanda_plates <- rwanda_high_prev_barcode_subset[,c("hiv02", "hv001", "hv002", "hvidx", "hiv01")]
rwanda_plates1 <- grouped_df(rwanda_plates, "hv001", drop = group_by_drop_default(rwanda_plates))
rwanda_plates2 <- arrange(rwanda_plates1, hiv02, .by_group = T)

#write.csv(rwanda_plates2, file = "/path_to_file/rwanda_plates2.csv")



