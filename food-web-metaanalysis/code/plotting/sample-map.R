library(tidyverse)
library(readxl)
library(ggmap)
library(cowplot)
library(ggthemes)
library(raster)


#### READ AND MANIPULATE DATA ####
### CA vector data ###
ca_data <- map_data("state","california")

### Meta-analysis data ###
outcome <- read_excel("~/Box Sync/Work/Code/soilc-ca-rangelands/food-web-metaanalysis/data/FINAL_DATA_SHEET_MARCH2019.xlsx", 
                      sheet = "Outcome_Data", skip = 2) %>%
  dplyr::select(control, treatment, ecosytem_type, soil_type, study_area, is_reserve, lat, long, 
         latlong.precision, experiment_type)
outcome$type <- rep("Conservation outcome",nrow(outcome))
outcome$latlong <- paste(outcome$lat, outcome$long)

outcome %>%
  group_by(latlong) %>%
  summarise(no_rows = length(latlong)) -> outcome.counts

outcome %>%
  unique() %>%
  full_join(outcome.counts) -> outcome

rm(outcome.counts)

soil <- read_excel("~/Box Sync/Work/Code/soilc-ca-rangelands/food-web-metaanalysis/data/FINAL_DATA_SHEET_MARCH2019.xlsx", 
                   sheet = "Soil_Data", skip = 2) %>%
  dplyr::select(control, treatment, ecosytem_type, soil_type, study_area, is_reserve, lat, long, 
         latlong.precision, experiment_type)
soil$type <- rep("Soil property",nrow(soil))
soil$latlong <- paste(soil$lat, soil$long)

soil %>%
  group_by(latlong) %>%
  summarise(no_rows = length(latlong)) -> soil.counts

soil %>%
  unique() %>%
  full_join(soil.counts) -> soil

rm(soil.counts)

all_data <- rbind(outcome, soil) %>%
  dplyr::select(-latlong) %>%
  drop_na()
all_data$lat <- all_data$lat %>% as.numeric()
all_data$long <- all_data$long %>% as.numeric()


#### PLOT
### Toner veg map
myMap <- get_map(location = "California",
                 source = "stamen",
                 maptype = "terrain", 
                 crop = FALSE, 
                 zoom=7)

# colors <- c("#67000d","#fb6a4a")
colors <- c("#018571","#a6611a")
# ggmap(myMap, darken = c(0.6, "white")) + 
#   # geom_polygon(data = ca_data, 
#   #              aes(x = long, y = lat, group=group), 
#   #              color ="white", 
#   #              fill ="orangered4", 
#   #              alpha=0.4, 
#   #              size =0.2) + 
#   geom_point(aes(x=long,y=lat,shape=type,color=experiment_type,size=no_rows),
#              data=all_data,
#              alpha = .8) +
#   scale_shape_manual(values=c(15,2)) +
#   scale_color_manual(values=colors) +
#   theme_bw() + xlab("") + ylab("") + 
#   theme(
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position=FALSE
#   )

ggmap(myMap, darken = c(0.5, "white")) + 
  geom_point(data=all_data,
             aes(x=long,y=lat,shape=type,color=experiment_type,size=no_rows),
             alpha = .8) +
  scale_shape_manual(values=c(15,2)) +
  scale_color_manual(values=colors) +
  theme_bw() + xlab("") + ylab("") + 
  guides(color="none",
         size = guide_legend("Observations",
                             title.position="top"), 
         shape = guide_legend("Measurement type",
                              title.position="top")) +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom"
  )





############


ggmap(myMap, darken = c(0.5, "white")) + 
  geom_point(aes(x=long,y=lat,shape=type,color=experiment_type,size=no_rows),
             data=all_data,
             alpha = .8) +
  scale_shape_manual(values=c(15,2)) +
  scale_color_manual(values=colors) +
  theme_bw() + xlab("") + ylab("") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position="none"
  ) -> map


ggplot(all_data, aes(no_rows,fill=type,color=type)) +
  geom_density(adjust=1.5, alpha=0.1) +
  ylab("Density") +
  xlab("Number of observations") +
  scale_fill_manual(values=colors,guide=FALSE) +
  scale_color_manual(values=colors,guide=FALSE) +
  theme_few() -> density
  

ggdraw() +
  draw_plot(map, -0.05, 0, 1, 1) +
  draw_plot(density, 0.4, 0.7, 0.6, 0.3) +
  draw_plot_label(c("A", "B"), c(0, 0.4), c(1, 1), size = 14)
