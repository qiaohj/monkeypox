library(ggplot2)
library(data.table)
setwd("/media/huijieqiao/SSD_Fast/monkeypox/monkeypox")
local_cases_se_day<-readRDS("../Tables/local_cases_se_day.rda")
first_day<-local_cases_se_day[, .(first_date=min(confirmation_date),
                                  last_date=max(confirmation_date)), 
                              by=list(NAME)]
local_cases_se_day<-merge(local_cases_se_day, first_day, by="NAME")
local_cases_se_day$days<-difftime(local_cases_se_day$confirmation_date, 
                                  local_cases_se_day$first_date,
                                  units = "days")
worldpop<-fread("../Data/worldpop/worldpop2022.csv")
worldpop[worldpop$name=="DR Congo"]$name<-"Democratic Republic of the Congo"
worldpop[worldpop$name=="Republic of the Congo"]$name<-"Congo"
worldpop[worldpop$name=="Iran"]$name<-"Iran (Islamic Republic of)"
worldpop[worldpop$name=="South Korea"]$name<-"Korea, Republic of"
worldpop[worldpop$name=="Moldova"]$name<-"Republic of Moldova"

worldpop[grepl("Moldova", worldpop$name)]
local_cases_se_day[!(NAME %in% worldpop$name)]
local_cases_se_day<-merge(local_cases_se_day, worldpop, by.x="NAME", by.y="name")
local_cases_se_day$case_density<-local_cases_se_day$cumulative * local_cases_se_day$Density
local_cases_se_day$case_by_population<-local_cases_se_day$cumulative / local_cases_se_day$pop2022

local_cases_se_day_last_day<-local_cases_se_day[confirmation_date==last_date]
p<-ggplot(local_cases_se_day)+geom_line(aes(x=days, y=case_density, group=Country_ISO3))+
  geom_text(data=local_cases_se_day_last_day, aes(x=days+8, y=case_density, label=NAME))+
  scale_y_sqrt()+
  theme_bw()
ggsave(p, filename="../Figures/N_Cases/N_by_pop_density.png", width=8, height=6)
p<-ggplot(local_cases_se_day)+geom_line(aes(x=days, y=case_by_population, group=Country_ISO3))+
  geom_text(data=local_cases_se_day_last_day, aes(x=days+8, y=case_by_population, label=Country_ISO3))+
  labs(x="Days from 1st reported case", y="Cases/1M (log)")+
  scale_y_log10()+
  #xlim(0, 100)+
  theme_bw()
p

ggsave(p, filename="../Figures/N_Cases/N_by_pop_with_label.pdf", width=6, height=5)

p<-ggplot(local_cases_se_day)+
  geom_line(aes(x=days, y=case_by_population, group=Country_ISO3), color="grey", alpha=0.5)+
  geom_line(data=local_cases_se_day[Country_ISO3 %in% local_cases_se[N>1000]$Country_ISO3], 
            aes(x=days, y=case_by_population, color=Country_ISO3))+
  geom_text(data=local_cases_se_day_last_day[Country_ISO3 %in% local_cases_se[N>1000]$Country_ISO3], 
            aes(x=days+8, y=case_by_population, label=Country_ISO3, color=Country_ISO3))+
  labs(x="Days from 1st reported case", y="Cases/1M (log-transformed)")+
  scale_y_log10()+
  #xlim(0, 100)+
  theme_bw()+
  scale_color_viridis_d()+
  theme(legend.position = "none")
  
p
itemxx<-local_cases_se_day_last_day[Country_ISO3 %in% local_cases_se[N>1000]$Country_ISO3]
setorderv(itemxx, "case_by_population", c(-1))
ggsave(p, filename="../Figures/N_Cases/N_by_pop_without_label.pdf", width=6, height=5)
