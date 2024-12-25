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
data_new$TUR <- gsub("Ölümlü|Ölümlü Kaza|Ölümlü Kaza Kaza" , "Ölümlü Kaza", data_new$TUR)
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

#en yüksek kaza sayılı istikametler bunu kullanmasak da olabilir
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

#en yüksek sayılı 10 istikamet bunun yerine accidents descending oluşturdum
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
  theme_minimal() +
  theme(axis.text.y = element_text(size = 2))


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
most_accidents_date

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
destination_accidents_top_5

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



library(DT)
date_list <- accidents_date_destination %>%
  group_split(TARIH)
dates_together <- bind_rows(date_list)
datatable(dates_together, options = list(pageLength = 10, scrollX = TRUE))

#---

#aysunun
library(dplyr)

data_2023 <- data_2023 %>%
  mutate(TYPE_NUMBER = as.numeric(factor(TUR)))
grouped_data_type <- data_2023 %>%
  group_by(TUR) %>%  # Ture göre gruplama
  summarise(Accident_Type = n())
sorted_grouped_type_data <- grouped_data_type %>%
  arrange(desc(Accident_Type))


library(dplyr)

data_2023 <- data_2023 %>%
  mutate(TYPE_NUMBER = as.numeric(factor(TUR)))
grouped_data_type <- data_2023 %>%
  group_by(TUR) %>%  # Ture göre gruplama
  summarise(Accident_Type = n())
sorted_grouped_type_data <- grouped_data_type %>%
  arrange(desc(Accident_Type))

library(ggplot2)

ggplot(sorted_grouped_type_data, aes(x = reorder(TUR, Accident_Type), y = Accident_Type)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  coord_flip() +  
  labs(title = "Distribution of Accidents According to Types (2023)", x = "Accident Types", y = "Frequency") +
  theme_minimal()


# İzmir haritasını indir ve yükle
install.packages("sf")  # Eğer yüklü değilse
library(sf)

izmir_map <- st_read("https://paintmaps.com/tr/bos-haritalar/41c/ornekler#google_vignette")

# Harita üzerinde veri göstermek için (örnek: kaza noktaları)
data <- data.frame(
  lon = c(27.1, 28.9, 29.0),
  lat = c(38.4, 39.9, 40.1),
  kazalar = c(10, 15, 20)
)

library(ggplot2)
ggplot() +
  geom_sf(data = izmir_map, fill = "lightblue", color = "white") +
  geom_point(data = data, aes(x = lon, y = lat, size = kazalar), color = "red") +
  labs(title = "Kazaların Yoğunluk Haritası", x = "Boylam", y = "Enlem") +
  theme_minimal()

str(data_new$TARIH)
head(data_new$TARIH)

data_new <- data_new %>%
  mutate(TARIH = as.Date(TARIH, format = "%Y-%m-%d"))
str(data_new$TARIH)

rlang::last_trace()


library(ggplot2)
library(dplyr)

# 1. Eksik veya hatalı verileri temizleyin
data_2023 <- data_2023 %>%
  filter(!is.na(ISTIKAMET) & !is.na(TUR) & ISTIKAMET != "")

# 2. Facet data oluşturun
facet_data <- data_2023 %>%
  group_by(ISTIKAMET, TUR) %>%
  summarise(count = n(), .groups = "drop")

print(facet_data)

# Facet data kontrol
print(facet_data)

# 3. En fazla kazanın olduğu ilk 10 istikameti belirleyin
top_istikamets <- facet_data %>%
  group_by(ISTIKAMET) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  slice(1:10) %>%
  pull(ISTIKAMET)

# İlk 10 istikamet kontrol
print(top_istikamets)

# 4. Sadece bu istikametleri içeren veri kümesini oluşturun
filtered_data <- facet_data %>% filter(ISTIKAMET %in% top_istikamets)

# Filtrelenmiş veri kontrol
print(filtered_data)

# 5. Facet Grid ile grafiği oluşturun
ggplot(filtered_data, aes(x = ISTIKAMET, y = count, fill = TUR)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(TUR ~ ., scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0, face = "bold")
  ) +
  labs(
    title = "Top 10 Directions with the Most Accidents and Accident Type Distribution",
    x = "Direction",
    y = "Number of Accidents",
    fill = "Type of Accidents"
  )


data_2023 <- data_2023 %>%
  mutate(AY = format(TARIH, "%B"))  # Ay isimlerini çıkarma

# Ay sıralamasını düzgün hale getirmek için bir faktör olarak tanımlama
data_2023$AY <- factor(data_2023$AY, 
                       levels = c("January", "February", "March", "April", 
                                  "May", "June", "July", "August", 
                                  "September", "October", "November", "December"))

# Aylık dağılımı gruplandırma
monthly_data <- data_2023 %>%
  group_by(AY, TUR) %>%
  summarise(count = n(), .groups = "drop")

# Grafik oluşturma
ggplot(monthly_data, aes(x = AY, y = count, fill = TUR)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(TUR ~ ., scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0, face = "bold")
  ) +
  labs(
    title = "Accident Distribution Monthly",
    x = "Month",
    y = "Number of Accident",
    fill = "Type of Accident"
  )



data_original <- read_excel("izbb-kaza-ariza-verileri.xlsx")

data_new <- data_original[!is.na(data_original$ISTIKAMET), ]

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

data_new$TUR <- gsub("maddi Hasarlı|Maddi Hasarlı|MAddi Hasarlı", "Maddi Hasarlı", data_new$TUR)
data_new$TUR <- gsub("Ölümlü|Ölümlü Kaza Kaza", "Ölümlü Kaza", data_new$TUR)
data_new$TUR <- gsub("Yakıtı Biten", "Yakıt Bitimi", data_new$TUR)
data_new$TUR <- gsub("Yangın", "Yanan Araç", data_new$TUR)
data_new$TUR <- gsub("yaralanmalı Kaza", "Yaralanmalı Kaza", data_new$TUR)

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

str(data_new)

library(dplyr)

data_2023 <- data_2023 %>%
  mutate(TYPE_NUMBER = as.numeric(factor(TUR)))
grouped_data_type <- data_2023 %>%
  group_by(TUR) %>%  # Ture göre gruplama
  summarise(Accident_Type = n())
sorted_grouped_type_data <- grouped_data_type %>%
  arrange(desc(Accident_Type))
sorted_grouped_type_data
datatable(sorted_grouped_type_data, 
          options = list(pageLength = 10, scrollX = TRUE), 
          caption = "Sequential Distribution of Accident Types (2023)") %>%
  DT::formatStyle(
    columns = colnames(sorted_grouped_type_data), 
    fontSize = '10px'   
  ) %>%
  htmlwidgets::onRender(
    "function(el, x) {
        $(el).css({'width': '70%', 'height': '300px'});
        $(el).find('th').css({'font-size': '10px'}); 
        $(el).find('.dataTables_length').css({'font-size': '10px'});
        $(el).find('.dataTables_filter').css({'font-size': '10px'});
        $(el).find('.dataTables_paginate').css({'font-size': '10px'});
        $(el).find('.dataTables_info').css({'font-size': '10px'});
    }"
  )


library(ggplot2)
library(dplyr)

# 1. Eksik veya hatalı verileri temizleyin
data_2023 <- data_2023 %>%
  filter(!is.na(ISTIKAMET) & !is.na(TUR) & ISTIKAMET != "")

# 2. Facet data oluşturun
facet_data <- data_2023 %>%
  group_by(ISTIKAMET, TUR) %>%
  summarise(count = n(), .groups = "drop")

datatable(facet_data, 
          options = list(pageLength = 10, scrollX = TRUE), 
          caption = "Facet Data (ISTIKAMET and TUR Distribution)") %>%
  DT::formatStyle(
    columns = colnames(facet_data), 
    fontSize = '10px'   
  ) %>%
  htmlwidgets::onRender(
    "function(el, x) {
        $(el).css({'width': '70%', 'height': '300px'});
        $(el).find('th').css({'font-size': '10px'}); 
        $(el).find('.dataTables_length').css({'font-size': '10px'});
        $(el).find('.dataTables_filter').css({'font-size': '10px'});
        $(el).find('.dataTables_paginate').css({'font-size': '10px'});
        $(el).find('.dataTables_info').css({'font-size': '10px'});
    }"
  )

# Facet data kontrol


# 3. En fazla kazanın olduğu ilk 10 istikameti belirleyin
top_istikamets <- facet_data %>%
  group_by(ISTIKAMET) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  slice(1:10) %>%
  pull(ISTIKAMET)

# İlk 10 istikamet kontrol
datatable(top_istikamets, 
          options = list(pageLength = 10, scrollX = TRUE), 
          caption = "Top 10 Directions with the Most Accidents") %>%
  DT::formatStyle(
    columns = colnames(top_istikamets), 
    fontSize = '10px'   
  ) %>%
  htmlwidgets::onRender(
    "function(el, x) {
        $(el).css({'width': '70%', 'height': '300px'});
        $(el).find('th').css({'font-size': '10px'}); 
        $(el).find('.dataTables_length').css({'font-size': '10px'});
        $(el).find('.dataTables_filter').css({'font-size': '10px'});
        $(el).find('.dataTables_paginate').css({'font-size': '10px'});
        $(el).find('.dataTables_info').css({'font-size': '10px'});
    }"
  )


# 4. Sadece bu istikametleri içeren veri kümesini oluşturun
filtered_data <- facet_data %>% filter(ISTIKAMET %in% top_istikamets)

# Filtrelenmiş veri kontrol
datatable(filtered_data, 
          options = list(pageLength = 10, scrollX = TRUE), 
          caption = "Filtered Data for Top 10 Directions") %>%
  DT::formatStyle(
    columns = colnames(filtered_data), 
    fontSize = '10px'   
  ) %>%
  htmlwidgets::onRender(
    "function(el, x) {
        $(el).css({'width': '70%', 'height': '300px'});
        $(el).find('th').css({'font-size': '10px'}); 
        $(el).find('.dataTables_length').css({'font-size': '10px'});
        $(el).find('.dataTables_filter').css({'font-size': '10px'});
        $(el).find('.dataTables_paginate').css({'font-size': '10px'});
        $(el).find('.dataTables_info').css({'font-size': '10px'});
    }"
  )

# 5. Facet Grid ile grafiği oluşturun
ggplot(filtered_data, aes(x = ISTIKAMET, y = count, fill = TUR)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(TUR ~ ., scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0, face = "bold")
  ) +
  labs(
    title = "Top 10 Directions with the Most Accidents and Accident Type Distribution",
    x = "Direction",
    y = "Number of Accidents",
    fill = "Type of Accidents"
  )


library(ggplot2)
library(dplyr)
library(DT)

# 1. Eksik veya hatalı verileri temizleyin
data_2023 <- data_2023 %>%
  filter(!is.na(ISTIKAMET) & !is.na(TUR) & ISTIKAMET != "")

# 2. Facet data oluşturun
facet_data <- data_2023 %>%
  group_by(ISTIKAMET, TUR) %>%
  summarise(count = n(), .groups = "drop")

# Facet data etkileşimli tablo
datatable(facet_data, 
          options = list(pageLength = 10, scrollX = TRUE), 
          caption = "Facet Data (ISTIKAMET and TUR Distribution)") %>%
  DT::formatStyle(
    columns = colnames(facet_data), 
    fontSize = '10px'   
  ) %>%
  htmlwidgets::onRender(
    "function(el, x) {
        $(el).css({'width': '70%', 'height': '300px'});
        $(el).find('th').css({'font-size': '10px'}); 
        $(el).find('.dataTables_length').css({'font-size': '10px'});
        $(el).find('.dataTables_filter').css({'font-size': '10px'});
        $(el).find('.dataTables_paginate').css({'font-size': '10px'});
        $(el).find('.dataTables_info').css({'font-size': '10px'});
    }"
  )

# 3. En fazla kazanın olduğu ilk 10 istikameti belirleyin
top_istikamets <- facet_data %>%
  group_by(ISTIKAMET) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  slice(1:10)

# İlk 10 istikamet etkileşimli tablo
datatable(top_istikamets, 
          options = list(pageLength = 10, scrollX = TRUE), 
          caption = "Top 10 Directions with the Most Accidents") %>%
  DT::formatStyle(
    columns = colnames(top_istikamets), 
    fontSize = '10px'   
  ) %>%
  htmlwidgets::onRender(
    "function(el, x) {
        $(el).css({'width': '70%', 'height': '300px'});
        $(el).find('th').css({'font-size': '10px'}); 
        $(el).find('.dataTables_length').css({'font-size': '10px'});
        $(el).find('.dataTables_filter').css({'font-size': '10px'});
        $(el).find('.dataTables_paginate').css({'font-size': '10px'});
        $(el).find('.dataTables_info').css({'font-size': '10px'});
    }"
  )

# 4. Sadece bu istikametleri içeren veri kümesini oluşturun
filtered_data <- facet_data %>% filter(ISTIKAMET %in% top_istikamets$ISTIKAMET)

# Filtrelenmiş veri etkileşimli tablo
datatable(filtered_data, 
          options = list(pageLength = 10, scrollX = TRUE), 
          caption = "Filtered Data for Top 10 Directions") %>%
  DT::formatStyle(
    columns = colnames(filtered_data), 
    fontSize = '10px'   
  ) %>%
  htmlwidgets::onRender(
    "function(el, x) {
        $(el).css({'width': '70%', 'height': '300px'});
        $(el).find('th').css({'font-size': '10px'}); 
        $(el).find('.dataTables_length').css({'font-size': '10px'});
        $(el).find('.dataTables_filter').css({'font-size': '10px'});
        $(el).find('.dataTables_paginate').css({'font-size': '10px'});
        $(el).find('.dataTables_info').css({'font-size': '10px'});
    }"
  )

# 5. Grafik oluşturma
ggplot(filtered_data, aes(x = ISTIKAMET, y = count, fill = TUR)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(TUR ~ ., scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0, face = "bold")
  ) +
  labs(
    title = "Top 10 Directions with the Most Accidents and Accident Type Distribution",
    x = "Direction",
    y = "Number of Accidents",
    fill = "Type of Accidents"
  )

#orijinal tubanın
library(ggplot2)
library(dplyr)

# 1. Eksik veya hatalı verileri temizleyin
data_2023 <- data_2023 %>%
  filter(!is.na(ISTIKAMET) & !is.na(TUR) & ISTIKAMET != "")

# 2. Facet data oluşturun
facet_data <- data_2023 %>%
  group_by(ISTIKAMET, TUR) %>%
  summarise(count = n(), .groups = "drop")

print(facet_data)

# Facet data kontrol
print(facet_data)

# 3. En fazla kazanın olduğu ilk 10 istikameti belirleyin
top_istikamets <- facet_data %>%
  group_by(ISTIKAMET) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  slice(1:10) %>%
  pull(ISTIKAMET)

# İlk 10 istikamet kontrol
print(top_istikamets)

# 4. Sadece bu istikametleri içeren veri kümesini oluşturun
filtered_data <- facet_data %>% filter(ISTIKAMET %in% top_istikamets)

# Filtrelenmiş veri kontrol
print(filtered_data)

# 5. Facet Grid ile grafiği oluşturun
ggplot(filtered_data, aes(x = ISTIKAMET, y = count, fill = TUR)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(TUR ~ ., scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0, face = "bold")
  ) +
  labs(
    title = "Top 10 Directions with the Most Accidents and Accident Type Distribution",
    x = "Direction",
    y = "Number of Accidents",
    fill = "Type of Accidents"
  )

#berfinin grafiğindeki hatayı gidermeye çalışma
library(ggplot2)
library(dplyr)

# Her bir kaza türü için en az iki veri noktası sağlamaya yönelik doldurma
data_2023 <- data_2023 %>%
  group_by(TUR) %>%
  mutate(dummy = ifelse(n() < 2, TRUE, FALSE)) %>%
  ungroup()

# Eksik gruplar için bir satır ekleme (sahte veri)
data_2023 <- data_2023 %>%
  bind_rows(
    data_2023 %>%
      filter(dummy) %>%
      mutate(ORTALAMA_GECEN_SURE = mean(data_2023$ORTALAMA_GECEN_SURE, na.rm = TRUE)) %>%
      slice(1)
  )

# Grafik
ggplot(data_2023, aes(x = TUR, y = ORTALAMA_GECEN_SURE)) +
  geom_violin(fill = "lightgreen", color = "darkgreen") +
  labs(
    title = "Average Response Time By Accident Types",
    x = "Accident Type",
    y = "Average Response Time"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

library(ggplot2)
library(dplyr)

# Her bir kaza türü için en az iki veri noktası sağlamaya yönelik doldurma
data_2023 <- data_2023 %>%
  group_by(TUR) %>%
  mutate(dummy = ifelse(n() < 2, TRUE, FALSE)) %>%
  ungroup()

# Eksik gruplar için bir satır ekleme (sahte veri)
data_2023 <- data_2023 %>%
  bind_rows(
    data_2023 %>%
      filter(dummy) %>%
      mutate(ORTALAMA_GECEN_SURE = mean(data_2023$ORTALAMA_GECEN_SURE, na.rm = TRUE)) %>%
      slice(1)
  )

# Grafik
ggplot(data_2023, aes(x = TUR, y = ORTALAMA_GECEN_SURE)) +
  geom_violin(fill = "lightgreen", color = "darkgreen") +
  labs(
    title = "Average Response Time By Accident Types",
    x = "Accident Type",
    y = "Average Response Time"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )
