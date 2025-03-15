library(ggplot2)
library(tidyverse)


CCTD_table <- read.csv("C:\\Users\\tyler\\Documents\\Marine Data Science\\Final.csv", skip = 2)
colnames(CCTD_table) <- NULL
CCTD_colnames <- c("version", "set", "source", "pred_taxa", "affilation", "collab", 
                   "collab_aff.", "collab_2", "collab_aff_2", "collab_3", "collab_aff_3",
                   "prey_size", "collection_id", "date", "month", "day", "year", 
                   "lat", "long", "region", "depth", "temp", "bottom_temp", 
                   "surface_temp", "pred_id", "pred_name", "pred_sci_name", 
                   "pred_aphia_id", "pred_sex", "pred_age", "tot_length", "unknown_length",
                   "prey_cont", "pred_info", "prey_id", "prey_name", "prey_sci_name",
                   "prey_aphia_id", "prey_number", "prey_num_correct", "prey_comp")
colnames(CCTD_table) <- CCTD_colnames
View(CCTD_table)

unique(CCTD_table[,26]) # Look at all Predators
unique(CCTD_table[,36]) # Look at all Prey

View(CCTD_table[CCTD_table$pred_name == "California Sea Lion", ])
nrow(CCTD_table[CCTD_table$predator_common_name == "California Sea Lion", ])
CSL_table <- CCTD_table[
  CCTD_table$pred_name == "California Sea Lion", ]

# Look at all the prey of the Seal
unique(CCTD_table[CCTD_table$pred_name == "California Sea Lion", 
                  "prey_common_name"])

CSL_lat <- as.vector(CSL_table[, 18])
CSL_long <- as.vector(CSL_table[, 19])
CSL_region <- as.vector(CSL_table[, 20])

red_crab_table <- CSL_table[CSL_table$prey_name == "Pelagic Red Crab", ]
red_crab_lat <- as.vector(red_crab_table$lat)
mean(red_crab_lat) # 33.15
red_crab_long <- as.vector(red_crab_table$long)
mean(red_crab_long) # -119.08
red_crab_regions <- CSL_table[CSL_table$region, ]

CSL_palette <- c("#304C88", "#F58C66")

p <- ggplot() + 
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  guides(color = guide_legend(title = "Regions")) +
  geom_point(data=red_crab_table, aes(x=red_crab_long, y=red_crab_lat, color="Red Crab"), size=7) +
  geom_point(data=CSL_table, aes(x = CSL_long, y = CSL_lat, color="California Sea Lions"), size=2) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.28, 0.01),
        legend.justification = c(1, 0)) +
  scale_color_manual(values = CSL_palette)
plot(p)
  

# Determine the most common prey for California Sea Lions
CSL_table %>%
  group_by(prey_name) %>% 
  summarise(prey = n()) %>% 
  arrange(desc(prey)) %>% 
  slice(1:4)
# California Market Squid, Northern Anchovy, Pacific Hake, Shortbelly Rockfish

# Determine the least common prey for California Sea Lions
CSL_table %>%
  group_by(prey_name) %>% 
  summarise(prey = n()) %>% 
  arrange(prey)

squid_table <- CSL_table[CSL_table$prey_name == "California Market Squid", ]
squid_lat <- as.vector(squid_table$lat)
mean(squid_lat) # 33.21
squid_long <- as.vector(squid_table$long)
mean(squid_long) # -119.31

anchovy_table <- CSL_table[CSL_table$prey_name == "Northern Anchovy", ]
anchovy_lat <- as.vector(anchovy_table$lat)
mean(anchovy_lat) # 33.23
anchovy_long <- as.vector(anchovy_table$long)
mean(anchovy_long) # -119.27

hake_table <- CSL_table[CSL_table$prey_name == "Pacific Hake", ]
hake_lat <- as.vector(hake_table$lat)
mean(hake_lat) # 33.44
hake_long <- as.vector(hake_table$long)
mean(hake_long) # -119.59

rockfish_table <- CSL_table[CSL_table$prey_name =="Shortbelly Rockfish", ]
rockfish_lat <- as.vector(rockfish_table$lat)
mean(rockfish_lat) # 33.18
rockfish_long <- as.vector(rockfish_table$long)
mean(rockfish_long) # -119.26

