#cluster map
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(rnaturalearth) 
library(rnaturalearthdata)
library(sf) #see ubuntu issues here: https://rtask.thinkr.fr/installation-of-r-4-0-on-ubuntu-20-04-lts-and-tips-for-spatial-packages/
library(ggspatial)
library(scatterpie)
library(sp)
library(scales)


###GPS data has been removed from datasets, needed to make maps###

#cluster_malaria <- readRDS(file = "path_to_file/cluster_malaria.rds") #cluster_malaria is available in repository 

#add in mutation variable based on sequencing results 
cluster_malaria$mutation <- "Wild Type"
cluster_malaria$mutation[which(cluster_malaria$hv001 == 486)] <- "C532W"
cluster_malaria$mutation[which(cluster_malaria$hv001 == 388)] <- "G533A"
cluster_malaria$mutation[which(cluster_malaria$hv001 == 266)] <- "G533G"
cluster_malaria$mutation[which(cluster_malaria$hv001 == 203 | cluster_malaria$hv001 == 102 |cluster_malaria$hv001 ==237)] <- "V555A"
cluster_malaria$mutation[which(cluster_malaria$hv001 == 65 | cluster_malaria$hv001 == 210 |cluster_malaria$hv001 ==84)] <- "R561H"


#saveRDS(cluster_malaria, file = "/path_to_file/cluster_malaria.rds")

high_prev <- data.frame(filter(cluster_malaria, prev_malaria_total > 15)) #> 15% malaria prevalence 

high_prev <- high_prev %>% 
  mutate(mutation = fct_relevel(mutation, 
                                "R561H", "C532W", "G533A", 
                                "G533G", "V555A", "Wild Type"))
d <- dhs_data(countryIds = "RW",
              surveyYearStart = 2014,
              breakdown = "subnational")
sp <- download_boundaries(surveyId = d$SurveyId[1], method = "sf")

m <- d$Value[match(sp$sdr_subnational_boundaries$REG_ID, d$RegionId)]
sp$sdr_subnational_boundaries2$Value <- m
districts <- sp$sdr_subnational_boundaries2

admin10 <- ne_download(scale="large", type = "admin_1_states_provinces_lines",
                       category = "cultural", returnclass = "sf")
rivers10 <- ne_download(scale = 10, type = 'rivers_lake_centerlines', 
                        category = 'physical', returnclass = "sf")
lakes10 <- ne_download(scale = "large", type = 'lakes', 
                       category = 'physical', returnclass = "sf")
sov110 <- ne_download(scale="medium", type = "sovereignty",
                      category = "cultural", returnclass = "sf")
admin110 <- ne_download(scale="large", type = "populated_places",
                        category = "cultural", returnclass = "sf")
roads10 <- ne_download(scale="large", type = "roads",
                       category = "cultural", returnclass = "sf")

sov10 <- ne_download(scale="large", type = "sovereignty",
                     category = "cultural", returnclass = "sf")

#Rwanda Map with mutations by cluster -- supplementary figure and inset of figure 2
ggplot()+
  geom_sf(data=rivers10, color="cyan4", size=0.5, alpha=0.5) +
  geom_sf(data=lakes10, color="grey40", fill ="aliceblue", size= 0.8) +
  geom_sf(data=admin10, color="grey80", size= 0.4) +
  geom_sf(data=admin110, color="grey80", size= 0.4) +
  geom_sf(data=districts, color="grey80", size= 0.4, fill = "ivory") +
  geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.85, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanz -\nania.", 
           color="grey60", size=5 , fontface="italic") +
  geom_point(data = high_prev, aes(x = long, y = lat, fill = mutation, color = mutation), size = 2, shape = 21)+
  scale_fill_manual(values = c("firebrick","coral","darkgoldenrod1", "lightblue","cyan3","grey80")) +
  scale_color_manual(values = c("firebrick","coral","darkgoldenrod1", "lightblue","cyan3","grey80")) +
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1,-2.9), expand = TRUE)+
  theme_void()




#Figure 2 base code--mutation prevalence by district/region

#district_mutation_count_percent_weighted <- read.csv(file = "/path_to_file/district_mutation_count_percent_weighted.csv") # made in 04_survey_weighted_means.r

###new with district percentages

district_mutation_count_percent_weighted$radius <- rescale(district_mutation_count_percent_weighted$Total, to = c(0.2, 0.7)) 

ggplot()+
  geom_sf(data=rivers10, color="cyan4", size=0.5, alpha=0.5) +
  geom_sf(data=lakes10, color="grey40", fill ="aliceblue", size= 0.8) +
  geom_sf(data=admin10, color="grey80", size= 0.4) +
  geom_sf(data=admin110, color="grey80", size= 0.4) +
  geom_sf(data=districts, color="grey80", size= 0.4, fill = "ivory") +
  geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.85, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanz -\nania.", 
           color="grey60", size=5 , fontface="italic") +
  geom_scatterpie(aes(x=long, y=lat, r = (radius/5)), 
                  data = district_mutation_count_percent_weighted, 
                  cols = c("R561H","C532W","G533A","G533G","V555A","Wild.Type"), 
                  color = "grey20",
                  alpha=.8)+
  scale_fill_manual(values = c("firebrick","coral","darkgoldenrod1", "lightblue","cyan3","grey80")) +
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1,-2.9), expand = TRUE)+
  theme_void()

#ggsave
