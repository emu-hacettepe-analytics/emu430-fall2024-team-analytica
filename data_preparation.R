# Turkish characters detection

Sys.setlocale("LC_ALL", "Turkish")

# Installing packages

install.packages("readxl")
install.packages("dplyr")
install.packages("stringr")
install.packages("lubridate")

# Activating packages

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

# Loading the data set

data_original <- read_excel("izbb-kaza-ariza-verileri.xlsx")

# General information

head(data_original)

str(data_original)

# How many NA in which column?

colSums(is.na(data_original))

# Delete NA ones in the ISTIKAMET column

data_new <- data_original[!is.na(data_original$ISTIKAMET), ]

colSums(is.na(data_new))

# See unique destination names

unique_istikamet <- sort(unique(data_new$ISTIKAMET))

print(unique_istikamet)

# String edits in destination names

data_new$ISTIKAMET <- gsub("Alsancak|Alsancak İstikameti|Alsancak Gar|alsancak", "Alsancak", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Buca|Buca Heykel", "Buca", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Konak|Konak Tüneli|Konak İstikameti", "Konak", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Kemalpaşa|kemalpaşa|Kemalpaş", "Kemalpaşa", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Havalimanı|havalimanı", "Havalimanı", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("İnciraltı|İnciraltı İstikameti", "İnciraltı", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Üçkuyular|Üçkuyular Meydan", "Üçkuyular", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Sarnıç|Sarniç", "Sarnıç", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Fahrettin Altay|F.Altay|Fahrettin altay", "Fahrettin Altay", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Tersane|Tersanesi", "Tersane", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Otogar|Otogar Meydanı", "Otogar", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Basmane|Basmane Gar", "Basmane", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Pınarbaşı|Pınarbaşo", "Pınarbaşı", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Üçyol|üçyol|Üçyo", "Üçyol", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Karabağlar|karabağlar", "Karabağlar", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Konak|konak", "Konak", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Gündoğdu|Gündoğdu Mahallesi", "Gündoğdu", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Mahvel|Mahvel Kavşağı", "Mahvel", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Beyazevler|Beyaz Evler", "Beyazevler", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Adliye|Adliye Kavşağı", "Adliye", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Harmandalı|Harmandali", "Harmandalı", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Yıkık Kemer|Yıkıkkemer|Yıkıkker", "Yıkıkkemer", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Karşıa", "Karşıyaka", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Liman D Kapısı", "Liman", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("otogar", "Otogar", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("bornova", "Bornova", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Mai", "Mustafa Kemal Sahil", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Borsa|Borsa Kavşağı", "Borsa", data_new$ISTIKAMET)
data_new$ISTIKAMET <- gsub("Çevre Yolu|Çevreyolu", "Çevreyolu", data_new$ISTIKAMET)

# Checking destination names

unique_istikamet_new <- sort(unique(data_new$ISTIKAMET))

print(unique_istikamet_new)

# See unique accident type names

unique_tur <- sort(unique(data_new$TUR))

print(unique_tur)

# String edits in accident type names

data_new$TUR <- gsub("maddi Hasarlı|Maddi Hasarlı|MAddi Hasarlı", "Maddi Hasarlı", data_new$TUR)
data_new$TUR <- gsub("Ölümlü", "Ölümlü Kaza", data_new$TUR)
data_new$TUR <- gsub("Yakıtı Biten", "Yakıt Bitimi", data_new$TUR)
data_new$TUR <- gsub("Yangın", "Yanan Araç", data_new$TUR)
data_new$TUR <- gsub("yaralanmalı Kaza", "Yaralanmalı Kaza", data_new$TUR)

# Checking accident type names

unique_tur_new <- sort(unique(data_new$TUR))

print(unique_tur_new)

# Calculate the number of data for each destination

counts <- data_new %>%
  group_by(ISTIKAMET) %>%
  summarise(count = n())

# Finding direction values with less than 5 data

istikamet_to_remove <- counts %>%
  filter(count < 5) %>%
  pull(ISTIKAMET)

# Delete them from the data set

data_new <- data_new %>%
  filter(!ISTIKAMET %in% istikamet_to_remove)

# Calculate the time between accident time and intervention time and add it as a new column

data_new$GECEN_SURE <- difftime(data_new$MUDAHALE_ZAMANI, data_new$KAZA_ZAMANI, units = "mins")

# Taking into account the day difference for interventions arriving after midnight

data_new <- data_new %>%
  mutate(
    GECEN_SURE = ifelse(
      MUDAHALE_ZAMANI < KAZA_ZAMANI, 
      difftime(MUDAHALE_ZAMANI + days(1), KAZA_ZAMANI, units = "mins"),
      difftime(MUDAHALE_ZAMANI, KAZA_ZAMANI, units = "mins")
    )
  )

# Calculate the average response time for each destination

data_new <- data_new %>%
  group_by(ISTIKAMET) %>%
  mutate(ORTALAMA_GECEN_SURE = mean(GECEN_SURE, na.rm = TRUE))

# Fill each NA value for GECEN_SURE with the average time of the direction it is connected to

data_new <- data_new %>%
  mutate(GECEN_SURE = ifelse(is.na(GECEN_SURE), ORTALAMA_GECEN_SURE, GECEN_SURE))

sum(is.na(data_new$GECEN_SURE))

# data_new is now avaible for analyses

str(data_new)

save(data_new, file = "data_new.RData")

