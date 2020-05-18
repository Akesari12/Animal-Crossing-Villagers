library(tidyverse)
library(ggthemes)
library(cluster)
library(caret)
library(factoextra)
library(gridExtra)

# Load data

Animal_Crossing_New_Horizons_Villagers <- read_csv("../data/Animal Crossing New Horizons Villagers.csv")

Animal_Crossing_New_Horizons_Villagers <- Animal_Crossing_New_Horizons_Villagers %>%
  rename(Birthday = Birhtday) %>%
  separate(Birthday, c("Birth_Month", "Birth_Day")) %>%
  select(-Birth_Day)

# Barplot of Species

species_plot <- Animal_Crossing_New_Horizons_Villagers %>%
  ggplot() +
  geom_bar(aes(Species)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Species") +
  ylab("Count") +
  ggtitle("Barplot of Species in Animal Crossing")

# Barplot of Species and Personality

species_personality_plot <- Animal_Crossing_New_Horizons_Villagers %>%
  ggplot() +
  geom_bar(aes(Species, fill = Personality)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text()) +
  xlab("Species") +
  ylab("Count") +
  ggtitle("Barplot of Species/Personality in Animal Crossing") 


# Barplot of Species and Gender

species_gender_plot <- Animal_Crossing_New_Horizons_Villagers %>%
  mutate(Gender = ifelse(Personality %in% c("♀ Peppy", "♀ Sisterly",
                                           "♀ Normal",  "♀ Snooty"),
                         "Female",
                         "Male")) %>% 
  ggplot() +
  geom_bar(aes(Species, fill = Gender)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -.05),
        axis.title = element_text()) +
  xlab("Species") +
  ylab("Count") +
  ggtitle("Barplot of Species/Gender in Animal Crossing") 

# Barplot of birthdays

birthday_plot <- Animal_Crossing_New_Horizons_Villagers %>%
  ggplot() +
  geom_bar(aes(Birth_Month)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -.05),
        axis.title = element_text()) +
  xlab("Species") +
  ylab("Count") +
  ggtitle("Barplot of Birth Months in Animal Crossing") 

grid.arrange(species_plot, birthday_plot,
             species_personality_plot,
             species_gender_plot, nrow = 2)

# Clustering

ac_ohe <- Animal_Crossing_New_Horizons_Villagers %>%
  select(Name, Personality, Species, Birth_Month) 

rownames(ac_ohe) <- ac_ohe$Name
ac_ohe <- ac_ohe %>% select(-Name)

dmy <- dummyVars(" ~ .", data = ac_ohe)
ac_ohe <- data.frame(predict(dmy, newdata = ac_ohe))
ac_ohe

ac_clusters <- kmeans(ac_ohe, centers = 2, nstart = 25)

fviz_cluster(ac_clusters, data = ac_ohe)
  