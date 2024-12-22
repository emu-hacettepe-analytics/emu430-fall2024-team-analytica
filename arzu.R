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
  geom_bar(stat = "identity",width = 0.7, fill = "darkgreen") +
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

daily_accident$month <- factor(daily_accident$month, levels = month.name)

library(dplyr)
peak_points <- daily_accident %>%
  group_by(month) %>%
  filter(daily_accident == max(daily_accident, na.rm = TRUE))

library(ggplot2)
ggplot(daily_accident, aes(x = Gun, y = daily_accident)) +
  geom_line(color = "black", size = 0.5) +
  geom_point(color = "red", size = 1) +
  geom_point(data = peak_points, aes(x = Gun, y = daily_accident), 
             color = "purple", size = 2.5) +  # Zirve noktalarını yeşil yap
  facet_wrap(~ month, scales = "free_y", ncol = 3) +
  labs(title = "Annual Accident Number)",
       x = "Day",
       y = "Number of Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10, face = "bold"))


# Zirve noktalarının tarihlerini listele
peak_dates <- peak_points$tarih

# Listeyi ekranda göster
print(peak_dates)

