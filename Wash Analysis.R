

library(dplyr)
library(readr)
library(hflights)


data(hflights)
head(hflights)

export <- read_csv("wash_beneficiary_data-extracted-from-2021-09-01-to-2021-09-30-extracted-2021-10-11T1200.csv")
dtm <- read_csv("dtm_population.csv")
pin <- read_csv("pin.csv")

hrp <- c("AAH","AGUF","CAID","CIDAR","CRS","CRUDAN","DRC","FHI-360","FREE",
        "GOALPRIME","GREENCODE","IMC","INTERSOS","IOM","IRC","JDF","KABHUDA","LINDII","MAL","MercyCorps",
        "NCA","NRC","PALRI","SHO","STSI","SCI","SI","TCDI","TDH","Tearfund","UNICEF","YIPDI","ZOA")

ind_1_filter = 'temporary_facilities_and_services'
ind_2_filter = c('construction_water_facilities', 'rehabilitation_water_facilities')
ind_3_filter = 'operations_maintenance'
ind_6_filter_list = c("Latrine Cleaning and Disinfection (Emergency)",
                     "Latrine Cleaning and Disinfection (VIP)",
                     "Latrine Construction (Emergency)",
                     "Latrine Construction (VIP)",
                     "Latrine Gender Marking (Emergency)",
                     "Latrine Gender Marking (VIP)",
                     "Latrine Lock Installation (Emergency)",
                     "Latrine Lock Installation (VIP)",
                     "Latrine Monitoring",
                     "Latrine Rehabilitation (Emergency)",
                     "Latrine Rehabilitation (VIP)",
                     "Latrine Sludge Dumping Site")
ind_7_filter_list = c("Latrine Construction (HH)",
                     "Latrine Rehabilitation (HH)")
ind_8_filter_list = c("Latrine Desludging (Mechanical)", "Latrine Desludging (Manual)")
ind_9_filter_list = "waste_management"
ind_12_filter_list = "House to House Visits"
ind_13_filter_list = c("Initial Hygiene Kit Distribution", "Replenishment Hygiene Kit Distribution")

desc_id = c(ind_1_filter, ind_2_filter, ind_3_filter, ind_9_filter_list)
det_name = c(ind_6_filter_list, ind_7_filter_list, ind_8_filter_list, ind_12_filter_list, ind_13_filter_list)

desc_id 
c <- desc_id %in% export_plus$activity_description_id

export_plus <- export %>% filter(activity_description_id %in% desc_id | activity_detail_name %in% det_name) %>% 
  left_join(dtm, by="site_id") %>% left_join(pin, by="admin2pcode") %>%
  mutate(population = ifelse(dtm_pop > 0, dtm_pop, site_population)) %>%
  mutate(total_reached = ifelse(total > population, population, total)) %>%
  mutate(girls = total_reached * 0.2862) %>% mutate(boys = total_reached * 0.2538) %>% 
  mutate(women = total_reached * 0.2067) %>% mutate(men = total_reached * 0.1833) %>%
  mutate(eld_women = total_reached * 0.0371) %>% mutate(eld_men = total_reached * 0.0329) %>%
  mutate()
  

head(export)
#flights <- tbl_df(hflights)

#flights

selected <- select(admin1name, admin2name, admin3name, site_name, beneficiary_type_name, population, reporting_period,
                   reporting_year, organization, implementing_partners, donor, project_code, project_title, project_start_date, 
                   project_end_date, activity_detail_name, activity_description_id, activity_type_name, households, girls,
                   boys, women, men, eld_women, eld_men, total_reached, admin1pcode, admin2pcode, admin3pcode, site_lng, site_lat,
                   )
