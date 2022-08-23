# study design plots-- Figure 1

library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(rnaturalearth) 
library(rnaturalearthdata)
library(sf) #see ubuntu issues here: https://rtask.thinkr.fr/installation-of-r-4-0-on-ubuntu-20-04-lts-and-tips-for-spatial-packages/
library(ggspatial)
library(scatterpie)
library(sp)
library(rdhs)
library(ggplot2)

# make request
d <- dhs_data(countryIds = "RW",
              surveyYearStart = 2014,
              breakdown = "subnational")

# get our related spatial data frame object
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


####study design maps#########
#Figure 1b-our study design
#GPS data has been removed from cluster_malaria due to DHS regulations

#cluster_malaria <- readRDS("path_to_file/cluster_malaria.r")

ggplot() +
  geom_sf(data=rivers10, color="cyan4", size=0.5, alpha=0.5) +
  geom_sf(data=lakes10, color="grey40", fill ="aliceblue", size= 0.8) +
  geom_sf(data=admin10, color="grey80", size= 0.4) +
  geom_sf(data=districts, color="grey80",fill= "ivory", size= 0.4) +
  geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanz -\nania.", 
           color="grey60", size=5 , fontface="italic") +
  geom_point(data = cluster_malaria, aes(x = long, y = lat, fill = prev_malaria_total, color = prev_malaria_total),
             size = 2, shape =ifelse(cluster_malaria$prev_malaria_total > 15, 17, 16))+
  scale_color_gradient(low = "lightblue", high = "darkblue")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")+
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1,-2.9), expand = TRUE)+
  theme_void()

#figure 1b
#studies done 2010-2015
uwimana_study_2020 <- data.frame(Site = c("Masaka", "Rukara","Ruhuha", "Bugarama","Kibirizi", "Nyarurema"), lat = c(-1.995556,-1.8008,-2.3125,-2.6988,-2.6505, -1.404), long = c(30.191667,30.5034,30.0589,29.0082,29.7824,30.1661), R561H =c(7.4,.7,0,0,0,0))
tacoli_2010_15 <- data.frame(Site = "Huye 2010-15",long = 29.7419, lat = -2.6005) 

#r561h sites
r561h_uwimana <- uwimana_study_2020[1:2,]

ggplot() +
  geom_sf(data=rivers10, color="cyan4", size=0.5, alpha=0.5) +
  geom_sf(data=lakes10, color="grey40", fill ="aliceblue", size= 0.8) +
  geom_sf(data=districts, color="grey80", fill = "ivory", size= 0.4) +
  geom_sf(data=sov110, color='black', size=0.8, fill = ifelse(sov110$ADMIN == "Rwanda", 'NA', 'grey90')) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  annotate("text", x = 29, y = -1.2, label = "Democratic\nRepublic\nof the\nCongo", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30, y = -1.1, label = "Uganda", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.1, y = -2.65, label = "Burundi", 
           color="grey60", size=5 , fontface="italic") +
  annotate("text", x = 30.8, y = -2.7, label = "Tanz -\nania.", 
           color="grey60", size=5 , fontface="italic") +
  geom_point(data = uwimana_study_2020, aes(x = long, y = lat), size = 5,
             shape = 17, fill = "red", color = "red")+
  geom_point(data = tacoli_2010_15, aes(x = long, y = lat), size = 5,
             shape = 17, fill = "blue", color = "blue")+
  geom_point(data = r561h_uwimana, aes(x = long, y = lat), size = 5,
             shape =3, fill = "black", color = "black")+
  coord_sf(xlim = c(28.8, 30.9), ylim = c(-1,-2.9), expand = TRUE)+
  theme_void()
