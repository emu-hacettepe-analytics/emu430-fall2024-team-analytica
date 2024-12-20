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


#burdan itibaren benim

library(dplyr)

class(counts)

#en yüksek kaza sayılı istikametler
count_sorted <- counts %>%
  arrange(desc(count))
top_10_accidents <- head(count_sorted, 10)             
print(top_10_accidents)



library(lubridate)

#2023 yılına göre filtreleme
data_2023 <- data_new %>%
  mutate(TARIH = as.Date(TARIH, format = "%Y-%m-%d")) %>%  # yyyy-aa-gg formatında tarih dönüşümü
  filter(year(TARIH) == 2023)

#istikamete göre gruplandırma
grouped_data <- data_2023 %>%
  group_by(ISTIKAMET) %>%  # İstikamete göre gruplama
  summarise("Total Number of Accidents" = n())

#en yüksek sayılı 10 istikamet
top_10_accidents <- grouped_data %>%
  arrange(desc("Total Number of Accidents")) %>%
  head(top_10_accidents,10)


#bunu kullan

data_2023 <- data_2023 %>%
  mutate(ISTIKAMET_SAYISI = as.numeric(factor(ISTIKAMET)))
grouped_data_2 <- data_2023 %>%
  group_by(ISTIKAMET) %>%  # İstikamete göre gruplama
  summarise(Total_Accidents = n())
sorted_grouped_data <- grouped_data_2 %>%
  arrange(desc(Total_Accidents))

top_10_destination <- sorted_grouped_data %>%
  head(10)

print(top_10_destination)


#İstikamete göre kaza dağılımları bar plot
library(ggplot2)

ggplot(sorted_grouped_data, aes(x = reorder(ISTIKAMET, Total_Accidents), y = Total_Accidents)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  coord_flip() +  
  labs(title = "Distribution of Accidents According to Directions (2023)", x = "Destination", y = "Frequency") +
  theme_minimal()


#hangi tarihte kaç kaza olmuş
library(dplyr)
accidents_date <- data_2023 %>%
  group_by(TARIH) %>%  # TARIH sütununa göre gruplama
  summarise(Accident_Number = n())
accidents_date

sorted_accidents_date <-accidents_date %>%
  arrange(desc(Accident_Number))

head(sorted_accidents_date,10)

#Günlük toplam kaza sayısını gösteren line plot
library(ggplot2)

ggplot(accidents_date, aes(x = TARIH, y = Accident_Number)) +
  geom_line(color = "blue", size = 1) +  # Çizgi grafiği
  geom_point(color = "pink", size = 2) +  # Veri noktalarını ekleme
  labs(title = "Number of Accidents by Date (2023)", x = "Date", y = "Number of Accidents") +
  theme_minimal()

#En çok kaza yapılan tarih
 library(dplyr)
most_accidents_date <- accidents_date %>%
  filter(Accident_Number == max(Accident_Number)) %>%  # En çok kaza yapılan tarih(ler)
  pull(TARIH)

#En çok kaza yapıln gün ilçelere göre dağılım
destination_accidents <- data_2023 %>%
  filter(TARIH %in% most_accidents_date) %>%  # En çok kaza yapılan tarihleri seç
  group_by(ISTIKAMET) %>%  # İstikamete göre gruplama
  summarise(Accident_Number = n()) %>%  # Her istikamet için kaza sayısını hesaplama
  arrange(desc(Accident_Number)) 
destination_accidents

#en çok kaza yapılan gün ilçelere göre bar plot
library(ggplot2)

ggplot(destination_accidents, aes(x = reorder(ISTIKAMET, Accident_Number), y = Accident_Number)) +
  geom_bar(stat = "identity", fill = "brown", color = "black") +
  labs(title = "Accidents According to Directions on the Dates with the Most Accidents", x = "Destination", y = "Number of Accidents") +
  theme_minimal()

#en çok kaza yapılan beş gün
library(dplyr)
top_5_most_accident_dates <- accidents_date %>%
  arrange(desc(Accident_Number)) %>%  # Kaza sayılarına göre sıralama
  slice(1:5) %>%  # İlk 5 satırı seçme
  pull(TARIH)
top_5_most_accident_dates

#en çok kaza yapılan beş günün istikamete göre dağılımı
destination_accidents_top_5 <- data_2023 %>%
  filter(TARIH %in% top_5_most_accident_dates) %>%  # İlk 5 tarihi seçme
  group_by(ISTIKAMET) %>%  # İstikamete göre gruplama
  summarise(Accident_Number = n()) %>%  # Her istikamet için kaza sayısını hesaplama
  arrange(desc(Accident_Number))


# En çok kaza yapılan 5 tarihteki kazaları tarihe ve istikamete göre gruplama
accidents_date_destination <- data_2023 %>%
  filter(TARIH %in% top_5_most_accident_dates) %>%  # İlk 5 tarihi seçme
  group_by(TARIH, ISTIKAMET) %>%  # Tarihe ve istikamete göre gruplama
  summarise(Accident_Number = n()) %>%  # Her grup için kaza sayısını hesaplama
  arrange(TARIH, desc(Accident_Number))  # Tarih ve kaza sayısına göre sıralama
# Sonuçları görüntüleme
accidents_date_destination

#ilk beş tarihi tablo olarak ayırıyor istikamete göre
date_list <- accidents_date_destination %>%
  group_split(TARIH)
date_list 
# İlk tabloyu görüntüleme
date_list[[1]]

#üsttekiyle aynı
for (i in seq_along(date_list)) {
  cat("Tarih:", unique(date_list[[i]]$TARIH), "\n")
  print(date_list[[i]])
  cat("\n")
}

#bunu kesin kullan ---
install.packages("DT")
library(DT)

# Her tarih için etkileşimli tabloyu görüntüleme
for (i in seq_along(date_list)) {
  cat("Tarih:", unique(date_list[[i]]$TARIH), "\n")
  datatable(date_list[[i]], options = list(pageLength = 5, scrollX = TRUE))
}

dates_together <- bind_rows(date_list)

# Tabloyu görüntüleme bu süper bi şey oldu
datatable(dates_together, options = list(pageLength = 10, scrollX = TRUE))

#---

