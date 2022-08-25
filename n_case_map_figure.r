library(data.table)
library(ggplot2)
library(sf)
setwd("/media/huijieqiao/SSD_Fast/monkeypox/monkeypox")
world <- map_data('world')
local_cases_figure<-readRDS("../Tables/local_cases_figure.rda")
sum(local_cases_figure$N)

#reported countries
local_cases_figure[NAME=="United Kingdom"]
local_cases_figure[grepl("India", NAME)]
local_cases_figure[which(Country_ISO3=="SLE")]


reported_countries<-c("United States", "Nigeria", "Sierra Leone",
                      "Liberia", "Congo", "Democratic Republic of the Congo",
                      "Central African Republic", "Cameroon", "Singapore",
                      "United Kingdom")
local_cases_figure$Reported<-F
local_cases_figure[NAME %in% reported_countries]$Reported<-T
local_cases_figure_se<-local_cases_figure[, .(N_Country=.N, N_cases=sum(N)),
                                          by=list(Reported)]

#world<-merge(world, local_cases_figure, by.x="region", by.y="NAME", all.x=T)
#world<-read_sf("../Shape/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
map<-readRDS("../Data/background.rda")
local_cases_se_month$Month_label<-""
local_cases_se_month[month==5]$Month_label<-"May"
local_cases_se_month[month==6]$Month_label<-"June"
local_cases_se_month[month==7]$Month_label<-"July"
local_cases_se_month[month==8]$Month_label<-"August"
local_cases_se_month$Month_label<-factor(local_cases_se_month$Month_label, levels=c("May", "June", "July", "August"))
p<-ggmap(map, darken = c(0.5, "white")) +
  geom_map(data=world, map=world, aes(long, lat, map_id=region), fill=NA, color="grey", size=0.1) +
  #geom_sf(data=world, aes(), fill=NA, color="black", size=0.1, inherit.aes = FALSE) +
  #coord_quickmap()+
  geom_point(aes(x=LON, y=LAT, size=log10(N_month)),
                  data=local_cases_se_month[month>=5], alpha=.5, color="#df536b", fill="#fccde5") +
  #scale_color_viridis()+
  #scale_fill_viridis()+
  #geom_scatterpie_legend(log10(local_cases_figure$N)*5+1, x=-160, y=-55)+
  theme_bw()+
  #coord_fixed() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_blank(),
    legend.position="none",
    #legend.key.width=unit(0.8,"in"),
    #strip.background.x = element_blank(),
    #strip.text.x = element_blank())
  )+
  facet_wrap(~Month_label, nrow=2, ncol=2)
p

p2<-ggmap(map, darken = c(0.5, "white")) +
  geom_map(data=world, map=world, aes(long, lat, map_id=region), fill=NA, color="grey", size=0.1) +
  #geom_sf(data=world, aes(), fill=NA, color="black", size=0.1, inherit.aes = FALSE) +
  #coord_quickmap()+
  geom_point(aes(x=LON, y=LAT, size=log10(N)),
             data=local_cases_se_month[month>=5], color="#df536b", fill="#fccde5", alpha=.2) +
  #scale_color_viridis()+
  #scale_fill_viridis()+
  #geom_scatterpie_legend(log10(local_cases_figure$N)*5+1, x=-160, y=-55)+
  theme_bw()+
  theme(
    
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_blank(),
    legend.position="none",
    #legend.key.width=unit(0.8,"in"),
    #strip.background.x = element_blank(),
    #strip.text.x = element_blank())
  )
p2

ppp<-ggarrange(p2, p, ncol=1, nrow=2)

ggsave(ppp, filename="../Figures/N_Cases_Map/n_cases_map.png", width=7, height=9)
ggsave(ppp, filename="../Figures/N_Cases_Map/n_cases_map.pdf", width=7, height=9)
