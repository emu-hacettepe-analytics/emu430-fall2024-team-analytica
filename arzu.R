#checking column names
colnames(data_new)

str(data_new)

#If the column name is 'Date' and the format is "%Y-%m-%d":
data_new$tarih <- as.Date(data_new$TARIH, format = "%Y-%m-%d")

# Filtering 2023 data:
data_2023 <- subset(data_new, format(data_new$tarih, "%Y") == "2023")

data_2023$ay <- format(as.Date(data_2023$tarih), "%m") 
# Month information (01, 02, ...)

str(data_2023$ay)

data_2023$ay <- as.numeric(data_2023$ay)

# Storing monthly totals in a vector
months <- 1:12
total_values <- c(
  sum(data_2023$ay == "1"),
  sum(data_2023$ay == "2"),
  sum(data_2023$ay == "3"),
  sum(data_2023$ay == "4"),
  sum(data_2023$ay == "5"),
  sum(data_2023$ay == "6"),
  sum(data_2023$ay == "7"),
  sum(data_2023$ay == "8"),
  sum(data_2023$ay == "9"),
  sum(data_2023$ay == "10"),
  sum(data_2023$ay == "11"),
  sum(data_2023$ay == "12")
)

# Veriyi bir veri çerçevesine dönüştür
monthly_data <- data.frame(month = months, Total = total_values)

install.packages("ggplot2")
library(ggplot2)
ggplot(monthly_data, aes(x = factor(month), y =Total)) +
  geom_bar(stat = "identity",width = 0.7, fill = "green") +
  labs(title = "Monthly Accident Numbers ", x = "Months", y = "Number of Accidents") +
  theme_minimal()

library(dplyr)
daily_accident <- data_2023 %>%
  group_by(tarih) %>%
  summarise(daily_accident = n())

head(daily_accident)

daily_accident <- daily_accident %>%
  mutate(month = format(as.Date(tarih), "%B"))

colnames(daily_accident)
daily_accident <- daily_accident %>%
  mutate(Gun = as.numeric(format(as.Date(tarih), "%d")))  

daily_accident$month <- factor(daily_accident$month, levels = month.name)  # Ayları Ocak'tan Aralık'a sırala
zirve_noktalar <- daily_accident %>%
  group_by(month) %>%
  filter(Kaza_Sayisi == max(Kaza_Sayisi, na.rm = TRUE))

ggplot(gunluk_kaza, aes(x = day, y = Kaza_Sayisi)) +
  geom_line(color = "blue", size = 0.5) +  # Çizgi grafiği
  geom_point(color = "red", size = 1) +  # Günlük noktalar
  geom_point(data = zirve_noktalar, aes(x = Gun, y = Kaza_Sayisi), color = "green", size = 2) +  # Zirve noktaları
  facet_wrap(~ Ay, scales = "free_x", ncol = 3) +  # Facet grid
  labs(title = "Gunluk Kaza Sayilari",
       x = "Gun",
       y = "Kaza Sayisi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # X ekseni yazılarını eğik yap
        strip.text = element_text(size = 10, face = "bold"))  # Ay başlıklarını kalın yap