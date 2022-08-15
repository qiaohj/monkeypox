library(data.table)
df<-fread("../Data/latest_20220813.csv")

cols<-c("ID", "Status", "Location", "City", "Country", "Country_ISO3", "Age", "Gender", "Date_onset",
        "Date_confirmation", "Travel_history_entry", "Travel_history_location", "Travel_history_country",
        "Genomics_Metadata", "Date_entry")

df<-df[, ..cols]

