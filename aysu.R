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