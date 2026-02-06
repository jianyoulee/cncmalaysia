#packages
library(tidyverse) 
library(data.table)
library(stringdist)
library(plyr)

#function to clean inaturalist data
clean_inat <- function(csv_file){
  #fast reading the csv file
  data <- fread(csv_file)
  #select data needed
  data <- select(data, 'id','time_observed_at','captive_cultivated','common_name','iconic_taxon_name','taxon_kingdom_name','taxon_phylum_name','taxon_class_name','taxon_order_name','taxon_family_name','scientific_name','latitude','longitude')
  #trimming
  data %>%
    mutate_if(is.character, str_trim)
  #change data type of date time
  data$date_time <- substring(data$time_observed_at, first = 1,last = 19)
  data$date_time <- as.POSIXlt(data$date_time, format = '%Y-%m-%d %H:%M:%OS') + 8*60*60
  data$date <- format(data$date_time, '%Y-%m-%d')
  data$time <- format(data$date_time, '%H:%M:%OS')
  #check na
  colSums(is.na(data))
  #change boolean values to more meaningful strings
  #change blank spaces into 'Unidentified'
  data$captive_cultivated <- mapvalues(data$captive_cultivated, 
                                         c(TRUE,FALSE),
                                         c('captive/cultivated','wildlife'))
  data$iconic_taxon_name <- mapvalues(data$iconic_taxon_name, 
                                        c('Actinopterygii', 'Amphibia', 'Animalia','Arachnida','Aves','Chromista','Insecta','Mammalia','Mollusca','Plantae','Reptilia', 'Protozoa', ''),
                                        c('Fishes','Amphibians','Animals','Spiders & Scorpions','Birds','Microorganisms','Insects','Mammals','Soft-bodied Invertebrates', 'Plants', 'Reptiles','Microorganisms', 'Unidentified'))
  data$common_name <- replace(data$common_name, data$common_name=='', 'Unidentified')
  data$taxon_kingdom_name <- replace(data$taxon_kingdom_name, data$taxon_kingdom_name=='', 'Unidentified')
  data$taxon_phylum_name <- replace(data$taxon_phylum_name, data$taxon_phylum_name=='', 'Unidentified')
  data$taxon_class_name <- replace(data$taxon_class_name, data$taxon_class_name=='', 'Unidentified')
  data$taxon_order_name <- replace(data$taxon_order_name, data$taxon_order_name=='', 'Unidentified')
  data$taxon_family_name <- replace(data$taxon_family_name, data$taxon_family_name=='', 'Unidentified')
  data$scientific_name <- replace(data$scientific_name, data$scientific_name=='', 'Unidentified')
  return(data)
}

#applying to the data 
inat2023 <- clean_inat('./observations-2023.csv')
count(inat2023$iconic_taxon_name)
inat2023[1,date_time]
inat2022 <- clean_inat('./observations-2022.csv')
count(inat2022$iconic_taxon_name)
inat2022[1,date_time]
inat2021 <- clean_inat('./observations-2021.csv')
count(inat2021$iconic_taxon_name)
inat2021[1,date_time]
inat2020 <- clean_inat('./observations-2020.csv')
count(inat2020$iconic_taxon_name)
inat2020[1,date_time]
inat2019 <- clean_inat('./observations-2019.csv')
count(inat2019$iconic_taxon_name)
inat2019[1,date_time]
inat2018 <- clean_inat('./observations-2018.csv')
count(inat2018$iconic_taxon_name)
inat2018[1,date_time]
inat2017 <- clean_inat('./observations-2017.csv')
count(inat2017$iconic_taxon_name)
inat2017[1,date_time]

#save
write.csv(inat2022, file = './cleaned_data/inat2022.csv', row.names = FALSE)
write.csv(inat2021, file = './cleaned_data/inat2021.csv', row.names = FALSE)
write.csv(inat2020, file = './cleaned_data/inat2020.csv', row.names = FALSE)
write.csv(inat2019, file = './cleaned_data/inat2019.csv', row.names = FALSE)
write.csv(inat2018, file = './cleaned_data/inat2018.csv', row.names = FALSE)
write.csv(inat2017, file = './cleaned_data/inat2017.csv', row.names = FALSE)

#combine all in one, and to reduce file size, further select less columns
inat_malaysia <- rbind(inat2017, inat2018, inat2019, inat2020, inat2021, inat2022, inat2023)
inat_malaysia <- select(inat_malaysia, 'id', 'date_time', 'date', 'time', 'common_name','iconic_taxon_name','taxon_kingdom_name','scientific_name','latitude','longitude')
write.csv(inat_malaysia, file = './cleaned_data/inat_malaysia.csv', row.names = FALSE)

#city nature challenge years and dates 
#2018 April 27-30
cnc2018 <- inat2018 %>% filter(between(date, as.Date('2018-04-27'),as.Date('2018-04-30')))
#2019 April 26-29
cnc2019 <- inat2019 %>% filter(between(date, as.Date('2019-04-26'),as.Date('2019-04-29')))
#2020 April 24-27
cnc2020 <- inat2020 %>% filter(between(date, as.Date('2020-04-24'),as.Date('2020-04-27')))
#2021 April 30 - May 3
cnc2021 <- inat2021 %>% filter(between(date, as.Date('2021-04-30'),as.Date('2021-05-03')))
#2022 April 29 - May 2
cnc2022 <- inat2022 %>% filter(between(date, as.Date('2022-04-29'),as.Date('2022-05-02')))
#2023 April 28 - May 1
cnc2023 <- inat2023 %>% filter(between(date, as.Date('2023-04-28'),as.Date('2023-05-01')))

cnc <- rbind(cnc2018, cnc2019, cnc2020, cnc2021, cnc2022, cnc2023)

write.csv(cnc, file = './cleaned_data/cnc.csv', row.names = FALSE)

klang <- lapply(list.files(path = '.', pattern = '_id.csv', full.names = TRUE), fread)
klang <- rbindlist(klang)
klang$region <- 'kl-selangor'

penang <- lapply(list.files(path = '.', pattern = '_idp.csv', full.names = TRUE), fread)
penang <- rbindlist(penang)
penang$region <- 'penang'

seremban <- fread('D:/R/inaturalist/cnc2021_ids.csv')
seremban$region <- 'seremban'

jb <- lapply(list.files(path = '.', pattern = '_idjb.csv', full.names = TRUE), fread)
jb <- rbindlist(jb)
jb$region <- 'johor-bahru'

cnc_id <- rbind(klang, penang, seremban, jb)
cnc_complete <- merge(x=cnc_id, y=cnc, by = 'id')
write.csv(cnc_complete, file = './cleaned_data/cnc_complete.csv', row.names = FALSE)
