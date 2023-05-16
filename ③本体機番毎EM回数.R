# ファイル読込
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv")
Metis_MIF_2211 <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")
Metis_MF3_EM.count.ope <- read_tsv("./tsv_data/Metis_MF3_EM.count.ope.tsv")

D303A618292 <- 
  Metis_MF3_2211 %>% 
    filter(Machine_numbers == "303A618292") %>% 
    group_by(Machine_numbers, Maintenance_date, Phenomenon, Treatment_location, Peripheral_name) %>%
    # group_by(Machine_numbers, Model_abbreviation, Maintenance_date, Phenomenon, Treatment_location, Peripheral_name) %>%
    summarise(
      EM.count = n()
    ) %>% 
    arrange(-EM.count) %>%
    filter(is.na(Peripheral_name)) %>%
    ungroup()


EM.count <- 
  Metis_MF3_2211 %>% 
  group_by(Machine_numbers, Model_abbreviation, Maintenance_date, Phenomenon, Treatment_location, Peripheral_name) %>%
  summarise(
    EM.count = n()
  ) %>% 
  arrange(-EM.count) %>%
  filter(is.na(Peripheral_name)) %>%
  ungroup()

# 58,976台にてEM発生
EM.count %>% 
  distinct(Machine_numbers)

EM.count.sum <- 
  EM.count %>% 
    group_by(Machine_numbers, Model_abbreviation) %>% 
    summarise(
      EM.count = n()
    ) %>% 
    arrange(-EM.count) %>% 
    ungroup()

sum(EM.count.sum$EM.count) # 117935
max(EM.count.sum$EM.count) # 29

Metis_MF3_2211 %>% 
  names()

Phenomenon <- 
  Metis_MF3_2211 %>% 
    distinct(Phenomenon)
    
Treatment_location <- 
  Metis_MF3_2211 %>% 
  distinct(Treatment_location)  
