#summarize outer bays from ocular count
library(tidyverse)

occular <- readxl::read_xlsx(path="Izembek-data-from-Heather/Annual-IZF-Izembek-Fall-Segment-Data.xlsx",
                          range = "A2:Q82") %>%
  pivot_longer(cols=starts_with("Sum"), names_to="Segment", values_to="Count") %>%
  mutate(Segment = str_sub(Segment, -2, -1))
names(occular) <- str_to_sentence(names(occular))
df <- data.frame(Lagoon = c(rep("Main", 6), rep("Bechevin",2), "Morzhovi", "Kinzarof"),
            Segment = as.character(c(60:65, 67:68, 80, 85)))
occular <- occular %>% left_join(df)

totals <- occular %>% drop_na(Lagoon) %>% group_by(Year, Month, Day, Lagoon) %>%
  summarise(Total = sum(Count)) %>%
  mutate(Date = paste(Year, Month, Day, sep="-"))

ggplot(data = totals, aes(x=Year, y=Total, col=Lagoon)) + 
  geom_point()

ggplot(data = filter(totals, Lagoon != "Main"), aes(x=Year, y=Total, col=Lagoon)) + 
  geom_point()

#sum bays and calculate percentage for each year, month, and day
is_impute <- totals %>% filter(Lagoon != "Main") %>%
  group_by(Year, Lagoon) %>%
  summarise(sdCount = sd(Total)) %>%
  filter(sdCount == 0)
#years 2011, 2012, 2013, there were only one rep in outer bays, but other obs were imputed
bay_tot <- totals %>% filter(Lagoon != "Main") %>%
  group_by(Year, Month, Day) %>%
  summarise(Bay_Total = sum(Total), n = n())
ize <- totals %>% filter(Lagoon == "Main") %>%
  group_by(Year, Month, Day) %>%
  summarise(Main_Total = sum(Total))
df <- left_join(bay_tot, ize) %>%
  mutate(Percent_bay = Bay_Total/(Bay_Total = Main_Total))
ggplot(data = df) + geom_histogram(aes(x=Percent_bay))  
mean(df$Percent_bay, na.rm = T)
sd(df$Percent_bay, na.rm = T)
mean(df$Bay_Total, na.rm = T)
sd(df$Bay_Total, na.rm = T)
