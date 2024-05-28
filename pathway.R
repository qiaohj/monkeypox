library(data.table)
library(raster)
library(sf)
library(ggplot2)
library(scatterpie)
library(ggrepel)
library(viridis)
sf_use_s2(FALSE)
setwd("/media/huijieqiao/WD10T_12/monkeypox/monkeypox")
if (F){
  #Fix ios3 columns
  regions_shp<-read_sf("../Shape/World/country.shp")
  world2<-read_sf("../Shape/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
  regions_shp$ISO3<-""
  regions_dt<-data.table(NAME=regions_shp$NAME, WB_CNTRY=regions_shp$WB_CNTRY, ID=c(1:nrow(regions_shp)))
  world2_dt<-data.table(NAME=world2$NAME, ISO3=world2$ISO3)
  regions_dt<-merge(regions_dt, world2_dt, by.x="NAME", by.y="NAME", all.x=T, all.y=F)
  setorder(regions_dt, ID)
  regions_dt[is.na(ISO3)]<-regions_dt[is.na(ISO3)]$WB_CNTRY
  regions_shp$ISO3<-regions_dt$ISO3
  
  regions_shp[which(regions_shp$WB_CNTRY!=regions_shp$ISO3),]
  lls<-data.table(st_coordinates(st_centroid(regions_shp)))
  regions_shp$LON<-lls$X
  regions_shp$LAT<-lls$Y
  write_sf(regions_shp, "../Shape/World/country_fixed.shp")
}
regions_shp<-read_sf("../Shape/World/country_fixed.shp")
iso3<-unique(regions_shp$ISO3)[1]
df<-fread("../Data/latest_20220824.csv")

cols<-c("ID", "Status", "Location", "City", "Country", "Country_ISO3", "Age", "Gender", "Date_onset",
        "Date_confirmation", "Travel_history_entry", "Travel_history_location", "Travel_history_country",
        "Genomics_Metadata", "Date_entry")

df<-df[, ..cols]

df$onset_date<-as.Date(df$Date_onset, format = "%Y-%m-%d")
df$confirmation_date<-as.Date(df$Date_confirmation, format = "%Y-%m-%d")
df$entry_date<-as.Date(df$Date_entry, format = "%Y-%m-%d")
unique(df$Status)
df_date<-df[Status=="confirmed"]
df_date[Travel_history_country=="UAE"]$Travel_history_country<-"United Arab Emirates"
df_date[Travel_history_country=="USA"]$Travel_history_country<-"United States"
df_date[Travel_history_country=="England"]$Travel_history_country<-"United Kingdom"
df_date[Travel_history_country=="Gran Canaria"]$Travel_history_country<-"Spain"
df_date[Travel_history_country=="Canary Islands"]$Travel_history_country<-"Spain"

item1<-df_date[Travel_history_country=="Spain; Germany"]
item1$Travel_history_country<-"Germany"
df_date[Travel_history_country=="Spain; Germany"]$Travel_history_country<-"Spain"
df_date<-rbind(df_date, item1)

item1<-df_date[Travel_history_country=="Spain, Singapore"]
item1$Travel_history_country<-"Singapore"
df_date[Travel_history_country=="Spain, Singapore"]$Travel_history_country<-"Spain"
df_date<-rbind(df_date, item1)

item1<-df_date[Travel_history_country=="Spain; Portugal"]
item1$Travel_history_country<-"Portugal"
df_date[Travel_history_country=="Spain; Portugal"]$Travel_history_country<-"Spain"
df_date<-rbind(df_date, item1)

item1<-df_date[Travel_history_country=="Spain, Portugal"]
item1$Travel_history_country<-"Portugal"
df_date[Travel_history_country=="Spain, Portugal"]$Travel_history_country<-"Spain"
df_date<-rbind(df_date, item1)

item1<-df_date[Travel_history_country=="Spain, Mexico"]
item1$Travel_history_country<-"Mexico"
df_date[Travel_history_country=="Spain, Mexico"]$Travel_history_country<-"Spain"
df_date<-rbind(df_date, item1)

item1<-df_date[Travel_history_country=="Italy, Spain"]
item1$Travel_history_country<-"Spain"
df_date[Travel_history_country=="Italy, Spain"]$Travel_history_country<-"Italy"
df_date<-rbind(df_date, item1)

item1<-df_date[Travel_history_country=="France, Spain"]
item1$Travel_history_country<-"Spain"
df_date[Travel_history_country=="France, Spain"]$Travel_history_country<-"France"
df_date<-rbind(df_date, item1)

item1<-df_date[Travel_history_country=="Netherlands and Spain"]
item1$Travel_history_country<-"Spain"
df_date[Travel_history_country=="Netherlands and Spain"]$Travel_history_country<-"Netherlands"
df_date<-rbind(df_date, item1)


regions_shp[which(regions_shp$NAME=="Taiwan"),]
df_date[(Travel_history_country!="")&(!(Travel_history_country %in% regions_shp$NAME))]
df_date[(Travel_history_country!="")&((Travel_history_country %in% regions_shp$NAME))]
df_date[Country=="Taiwan"]$Country_ISO3<-"CHN"
df_date[Country=="Taiwan"]$Country<-"China"

df_date[!(Country_ISO3 %in% regions_shp$ISO3)]

regions_shp[which(regions_shp$NAME=="Netherlands"),]

df_date$day_of_week<-wday(df_date$confirmation_date)
df_date$month<-month(df_date$confirmation_date)


m<-5
region_code<-unique(regions_shp[, c("NAME", "LON", "LAT", "ISO3")])
region_code$geometry<-NULL
region_code_travel<-region_code
colnames(region_code_travel)<-c("Travel_NAME", "Travel_LON", "Travel_LAT", "Travel_ISO3")
df_date_with_geo<-merge(df_date, region_code, 
                        by.x="Country_ISO3", by.y="ISO3")
df_date_with_geo<-merge(df_date_with_geo, region_code_travel,
                        by.x="Travel_history_country", by.y="Travel_NAME", all.x=T, all.y=F)

imported_cases<-df_date_with_geo[(Country_ISO3!=Travel_ISO3)&(!is.na(Travel_ISO3))]
local_cases<-df_date_with_geo[is.na(Travel_ISO3) | Country_ISO3==Travel_ISO3]
local_cases<-local_cases[Travel_history_country==""]
local_cases<-df_date_with_geo
local_cases_se<-local_cases[,.(N=.N), by=list(NAME, LON, LAT, Country_ISO3)]
saveRDS(local_cases_se, "../Tables/local_cases_se.rda")
saveRDS(local_cases, "../Tables/local_cases.rda")

local_cases_se_month<-local_cases[,.(N_month=.N), 
                                  by=list(NAME, LON, LAT, Country_ISO3, month)]
local_cases_se_month<-merge(local_cases_se_month, local_cases_se, by=c("NAME", "LON", "LAT", "Country_ISO3"))
saveRDS(local_cases_se_month, "../Tables/local_cases_se_month.rda")

cols<-c("NAME", "LON", "LAT", "Country_ISO3", "N")
local_cases_figure<-unique(local_cases_se_month[, ..cols])

local_cases_se_month_5<-local_cases_se_month[month==5]
local_cases_figure<-merge(local_cases_figure, local_cases_se_month_5, 
                          by=c("NAME", "LON", "LAT", "Country_ISO3", "N"), all=T)
local_cases_figure$month<-NULL
local_cases_figure[is.na(N_month)]$N_month<-0
colnames(local_cases_figure)[6]<-"May"

local_cases_se_month_6<-local_cases_se_month[month==6]
local_cases_figure<-merge(local_cases_figure, local_cases_se_month_6, 
                          by=c("NAME", "LON", "LAT", "Country_ISO3", "N"), all=T)
local_cases_figure$month<-NULL
local_cases_figure[is.na(N_month)]$N_month<-0
colnames(local_cases_figure)[7]<-"Jun"

local_cases_se_month_7<-local_cases_se_month[month==7]
local_cases_figure<-merge(local_cases_figure, local_cases_se_month_7, 
                          by=c("NAME", "LON", "LAT", "Country_ISO3", "N"), all=T)
local_cases_figure$month<-NULL
local_cases_figure[is.na(N_month)]$N_month<-0
colnames(local_cases_figure)[8]<-"Jul"

local_cases_se_month_8<-local_cases_se_month[month==8]
local_cases_figure<-merge(local_cases_figure, local_cases_se_month_8, 
                          by=c("NAME", "LON", "LAT", "Country_ISO3", "N"), all=T)
local_cases_figure$month<-NULL
local_cases_figure[is.na(N_month)]$N_month<-0
colnames(local_cases_figure)[9]<-"Aug"
saveRDS(local_cases_figure, "../Tables/local_cases_figure.rda")
world <- map_data('world')
world <- read_sf("../Shape/World/country.shp")
p<-ggplot() +
  geom_sf(data=world, fill=NA, color="black", linewidth=0.1) +
  #coord_quickmap()+
  geom_scatterpie(aes(x=LON, y=LAT, r=log10(N)),
                  data=local_cases_figure, cols=c("May", "Jun", "Jul", "Aug"), 
                  color=NA, alpha=.8) +
  #geom_scatterpie_legend(log10(local_cases_figure$N)*5+1, x=-160, y=-55)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = "Number of cases per country and region",
       fill = NULL) +
  coord_sf() +
  scale_fill_manual(
    breaks = c("May", "Jun", "Jul", "Aug"),
    labels = c("May", "June", "July", "August"),
    values = c("May" = "#E69F00",
               "Jun" = "#009E73",
               "Jul" = "#0072B2",
               "Aug" = "#CC79A7")
  )
p
#ggsave(p, filename="../Figures/N_Cases_Map/n_cases_map.png", width=9, height=6)
saveRDS(local_cases_figure, "../Figures/N_Cases_Map/local_cases_figure.rda")
local_cases_se_day<-local_cases[,.(N_day=.N), 
                                by=list(NAME, LON, LAT, Country_ISO3, confirmation_date)]
setorderv(local_cases_se_day, c("Country_ISO3", "confirmation_date"))
local_cases_se_day$cumulative <- ave(local_cases_se_day$N_day, 
                                     local_cases_se_day$Country_ISO3, 
                                     FUN=cumsum)
local_cases_se_day<-merge(local_cases_se_day, local_cases_se, 
                          by=c("NAME", "LON", "LAT", "Country_ISO3"))
setorderv(local_cases_se, "N", c(-1))

local_cases_se_day$Label<-""
local_cases_se_day[Country_ISO3 %in% local_cases_se[1:10]$Country_ISO3]$Label<-
  local_cases_se_day[Country_ISO3 %in% local_cases_se[1:10]$Country_ISO3]$Country_ISO3
p<-ggplot(local_cases_se_day[confirmation_date>="2022-05-01"])+
  geom_line(data=local_cases_se_day[confirmation_date>="2022-05-01"&Label==""], 
            aes(x=confirmation_date, y=cumulative, group=Country_ISO3), 
            color="grey")+
  geom_line(data=local_cases_se_day[confirmation_date>="2022-05-01" & Label!=""], 
            aes(x=confirmation_date, y=cumulative, group=Country_ISO3, 
                color=Label))+
  geom_text(data=local_cases_se_day[confirmation_date>="2022-05-01" & Label!="" & cumulative==N], 
            aes(x=Sys.Date(), y=N, label=Label, color=Label))+
  scale_y_log10()+
  theme_bw()+
  labs(x="", y="Cumulative cases (log-transformed)")+
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")+
  scale_color_viridis_d()+
  theme(legend.position="none")
p

itemxx<-local_cases_se_day[confirmation_date>="2022-05-01" & Label!="" & cumulative==N]
setorderv(itemxx, "N", c(-1))

ggsave(p, filename="../Figures/N_Cases_Map/daily_cases.pdf", width=8, height=6)
saveRDS(local_cases_se_day, "../Tables/local_cases_se_day.rda")


imported_cases_se<-imported_cases[, .(N=.N), by=list(LON, LAT, Travel_LON, Travel_LAT, 
                                                     Travel_history_country, Travel_ISO3,
                                                     NAME, Country_ISO3)]
imported_cases_se$N<-imported_cases_se$N
saveRDS(imported_cases_se, "../Tables/imported_cases_se.rda")

imported_cases_N<-imported_cases_se[, .(N=.N), by=list(Travel_history_country, Travel_LON, Travel_LAT)]
saveRDS(imported_cases_N, "../Tables/imported_cases_N.rda")

p<-ggplot() +
  geom_sf(data=world, fill=NA, color="black", size=0.1) +
  geom_curve(data=imported_cases_se, aes(x = Travel_LON, y = Travel_LAT, 
                                         xend = LON, yend = LAT, color=factor(N)), 
             curvature = .2, size=0.3,
             arrow = arrow(length = unit(0.02, "npc"))) +
  scale_color_manual(
    breaks = c("1", "2", "3", "4"),
    labels = c("1", "2", "3", "4"),
    values = c("1" = "#E69F00",
               "2" = "#009E73",
               "3" = "#0072B2",
               "4" = "#CC79A7")
  )+
  labs(title = "Imported pathways",
       color="N cases") +
  theme_bw()+
  coord_sf() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
p
ggsave(p, filename="../Figures/Imported_Cases_Map/imported_cases_map.png", width=9, height=6)

saveRDS(imported_cases_se, "../Figures/Imported_Cases_Map/imported_cases_se.rda")


p<-ggplot() +
  geom_sf(data=world, fill=NA, color="black", size=0.1) +
  geom_curve(data=imported_cases_se, aes(x = Travel_LON, y = Travel_LAT, 
                                         xend = LON, yend = LAT, color=factor(N)), 
             curvature = .2, size=0.2,
             arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data=imported_cases_N[N>=5],
             aes(x=Travel_LON, y=Travel_LAT, size=N), pch=21)+
  scale_color_manual(
    breaks = c("1", "2", "3", "4"),
    labels = c("1", "2", "3", "4"),
    values = c("1" = "#E69F00",
               "2" = "#009E73",
               "3" = "#0072B2",
               "4" = "#CC79A7")
  )+
  labs(title = "Imported pathways",
       color="N cases") +
  theme_bw()+
  coord_sf() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
p
ggsave(p, filename="../Figures/Imported_Cases_Map/imported_cases_map_big.png", width=20, height=15)

#Spain
imported_cases_se_spain<-imported_cases_se[Travel_history_country=="Spain"|NAME=="Spain"]
imported_cases_se_spain$in_out<-"Import"
imported_cases_se_spain[Travel_history_country=="Spain"]$in_out<-"Export"
p<-ggplot() +
  geom_sf(data=world, fill=NA, color="black", size=0.1) +
  geom_curve(data=imported_cases_se_spain, aes(x = Travel_LON, y = Travel_LAT, 
                                         xend = LON, yend = LAT, color=in_out), 
             curvature = .2, size=0.3,
             arrow = arrow(length = unit(0.02, "npc"))) +
  scale_color_manual(
    breaks = c("Import", "Export"),
    labels = c("Import", "Export"),
    values = c("Import" = "#0072B2",
               "Export" = "#CC79A7")
  )+
  labs(title = "Spain",
       color="") +
  theme_bw()+
  coord_sf() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
p
ggsave(p, filename="../Figures/Imported_Cases_Map/imported_cases_map_spain.png", width=9, height=6)

#United States
imported_cases_se_spain<-imported_cases_se[Travel_history_country=="United States"|NAME=="United States"]
imported_cases_se_spain$in_out<-"Import"
imported_cases_se_spain[Travel_history_country=="United States"]$in_out<-"Export"
p<-ggplot() +
  geom_sf(data=world, fill=NA, color="black", size=0.1) +
  geom_curve(data=imported_cases_se_spain, aes(x = Travel_LON, y = Travel_LAT, 
                                               xend = LON, yend = LAT, color=in_out), 
             curvature = .2, size=0.3,
             arrow = arrow(length = unit(0.02, "npc"))) +
  scale_color_manual(
    breaks = c("Import", "Export"),
    labels = c("Import", "Export"),
    values = c("Import" = "#0072B2",
               "Export" = "#CC79A7")
  )+
  labs(title = "United States",
       color="") +
  theme_bw()+
  coord_sf() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
p
ggsave(p, filename="../Figures/Imported_Cases_Map/imported_cases_map_usa.png", width=9, height=6)

#Germany
imported_cases_se_spain<-imported_cases_se[Travel_history_country=="Germany"|NAME=="Germany"]
imported_cases_se_spain$in_out<-"Import"
imported_cases_se_spain[Travel_history_country=="Germany"]$in_out<-"Export"
p<-ggplot() +
  geom_sf(data=world, fill=NA, color="black", size=0.1) +
  geom_curve(data=imported_cases_se_spain, aes(x = Travel_LON, y = Travel_LAT, 
                                               xend = LON, yend = LAT, color=in_out), 
             curvature = .2, size=0.3,
             arrow = arrow(length = unit(0.02, "npc"))) +
  scale_color_manual(
    breaks = c("Import", "Export"),
    labels = c("Import", "Export"),
    values = c("Import" = "#0072B2",
               "Export" = "#CC79A7")
  )+
  labs(title = "Germany",
       color="") +
  theme_bw()+
  coord_sf() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
p
ggsave(p, filename="../Figures/Imported_Cases_Map/imported_cases_map_germany.png", width=9, height=6)


#Portugal
imported_cases_se_spain<-imported_cases_se[Travel_history_country=="Portugal"|NAME=="Portugal"]
imported_cases_se_spain$in_out<-"Import"
imported_cases_se_spain[Travel_history_country=="Portugal"]$in_out<-"Export"
p<-ggplot() +
  geom_sf(data=world, fill=NA, color="black", size=0.1) +
  geom_curve(data=imported_cases_se_spain, aes(x = Travel_LON, y = Travel_LAT, 
                                               xend = LON, yend = LAT, color=in_out), 
             curvature = .2, size=0.3,
             arrow = arrow(length = unit(0.02, "npc"))) +
  scale_color_manual(
    breaks = c("Import", "Export"),
    labels = c("Import", "Export"),
    values = c("Import" = "#0072B2",
               "Export" = "#CC79A7")
  )+
  labs(title = "Portugal",
       color="") +
  theme_bw()+
  coord_sf() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
p
ggsave(p, filename="../Figures/Imported_Cases_Map/imported_cases_map_portugal.png", width=9, height=6)

