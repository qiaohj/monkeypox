
library(data.table)
library(ggplot2)
library(sf)
library(ggmap)
setwd("/media/huijieqiao/SSD_Fast/monkeypox/monkeypox")
world <- map_data('world')
map<-readRDS("../Data/background.rda")
imported_cases_se<-readRDS("../Tables/imported_cases_se.rda")
p<-ggplot()+#ggmap(map, darken = c(0.5, "white")) +
  #geom_map(data=world, map=world, aes(long, lat, map_id=region), fill=NA, color="grey", size=0.1)  +
  geom_curve(data=imported_cases_se, aes(x = Travel_LON, y = Travel_LAT, 
                                         xend = LON, yend = LAT, color=factor(N)), 
             curvature = .2, size=0.3,
             arrow = arrow(length = unit(0.02, "npc"))) +
  coord_sf()+
  scale_color_manual(
    breaks = c("1", "2", "3", "4"),
    labels = c("1", "2", "3", "4"),
    values = c("1" = "#E69F00",
               "2" = "#009E73",
               "3" = "#0072B2",
               "4" = "#CC79A7")
  )+
  labs(color="N cases") +
  theme_bw()+
  theme(legend.position = "none",
    panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
p
ggsave(p, filename="../Figures/Imported_Cases_Map/imported_cases_map_background.pdf", width=9, height=6)
ggsave(p, filename="../Figures/Imported_Cases_Map/imported_cases_map_pathonly.pdf", width=9, height=6)

ggsave(p, filename="../Figures/Imported_Cases_Map/imported_cases_map.png", width=9, height=6)
ggsave(p, filename="../Figures/Imported_Cases_Map/imported_cases_map.pdf", width=9, height=6)
