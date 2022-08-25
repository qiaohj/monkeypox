library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/SSD_Fast/monkeypox/monkeypox")
flight_data_se<-readRDS("../Tables/flight_data_se.rda")
flight_data_se_all<-flight_data_se$flight_data_se_all
flight_data_se<-flight_data_se$flight_data_se
setorderv(flight_data_se_all, "N", c(-1))
setorderv(flight_data_se, "N", c(-1))

flight_data_se_2019<-flight_data_se[year==2019]
flight_data_se_2022<-flight_data_se[year==2022]
colnames(flight_data_se_2019)[c(3,4)]<-c("y_2019", "N_2019")
colnames(flight_data_se_2022)[c(3,4)]<-c("y_2022", "N_2022")
flight_data_se_merge<-merge(flight_data_se_2019, flight_data_se_2022, 
                            by=c("destination_country", "destination_country_iso3"),
                            all=T)
flight_data_se_merge[is.na(y_2019)]$y_2019<-2019
flight_data_se_merge[is.na(y_2022)]$y_2022<-2022
flight_data_se_merge[is.na(N_2019)]$N_2019<-0
flight_data_se_merge[is.na(N_2022)]$N_2022<-0
flight_data_se_merge$change<-(flight_data_se_merge$N_2022-flight_data_se_merge$N_2019)/flight_data_se_merge$N_2019
flight_data_se_merge$all_N<-flight_data_se_merge$N_2019+flight_data_se_merge$N_2022
setorderv(flight_data_se_merge, "all_N", c(1))
flight_data_se_merge[all_N>10000]
flight_data_se_merge$increase<-"T"
flight_data_se_merge[change<0]$increase<-"F"
flight_data_se_merge$destination_country_iso3_f<-factor(flight_data_se_merge$destination_country_iso3,
                                                        levels=flight_data_se_merge$destination_country_iso3)
local_cases_se<-readRDS("../Tables/local_cases_se.rda")
flight_data_se_merge$is_Mp<-"F"
flight_data_se_merge[destination_country_iso3 %in% local_cases_se[N>100]$Country_ISO3]$is_Mp<-"T"


dddd<-flight_data_se_merge[(nrow(flight_data_se_merge)-20):nrow(flight_data_se_merge)][all_N>10000|
                                                                                   (destination_country_iso3 %in% local_cases_se[N>0]$Country_ISO3)]
dddd[!(destination_country_iso3 %in% local_cases_se[N>0]$Country_ISO3)]

dddd_with_mp<-flight_data_se_merge[destination_country_iso3 %in% local_cases_se[N>0]$Country_ISO3]
dddd_without_mp<-flight_data_se_merge[!(destination_country_iso3 %in% local_cases_se[N>0]$Country_ISO3)]
setorderv(dddd_with_mp, "N_2022", c(-1))
setorderv(dddd_without_mp, "N_2022", c(-1))
dddd_with_mp$is_Mp<-"T"
dddd_without_mp$is_Mp<-"F"
#dddd<-rbindlist(list(dddd_with_mp[1:10], dddd_without_mp[1:10]))

local_cases_se[Country_ISO3=="RUS"]
write.csv(local_cases_se, "../Tables/local_cases_se.csv")
p<-ggplot(dddd)+
  geom_bar(aes(x=change, y=destination_country_iso3_f, fill=increase), stat = "identity", width=0.3)+
  geom_point(aes(x=change, y=destination_country_iso3_f, color=is_Mp), size=3)+
  geom_text(data=dddd[change<=0], 
            aes(x=change, y=destination_country_iso3_f, label=destination_country_iso3), hjust=1.35, vjust=0.5)+
  geom_text(data=dddd[change>0], 
            aes(x=change, y=destination_country_iso3_f, label=destination_country_iso3), hjust=-0.4, vjust=0.5)+
  scale_fill_manual(breaks=c("T", "F"), values=c("#FF3333", "#0072B2"))+
  scale_color_manual(breaks=c("T", "F"), values=c("#BC211F", "#1B499F"))+
  scale_x_continuous(labels = function(x) paste0(x*100, "%"))+
  #xlim(-0.8, 1)+
  theme_bw()+
  theme(axis.line = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(), 
        panel.background = element_blank(), 
        legend.background = element_rect(fill = "#FFFFFF", color = NA),
        #panel.border = element_blank(),
        legend.position="none",
        #legend.key.width=unit(0.8,"in"),
        #strip.background.x = element_blank(),
        #strip.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
p
ggsave(p, filename = "../Figures/Flights/Overview.pdf", width=5, height=6)
#ggsave(p, filename = "../Figures/Flights/Overview_with_legend.pdf", width=5, height=6)
