library(tidyverse)

mpg %>%
  ggplot(aes(x = manufacturer, y = cty)) + 
  geom_point()

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = manufacturer, y= cty)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust =  0.5, hjust = .5))

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = manufacturer, y = cty)) +
  geom_jitter(width = .2, alpha = .75, size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = .5)) +
  theme(text = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Car Manufacturer",
       x = "Manufacturer",
       y = "City Fuel Economy (mpg)")

library(Hmisc)

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = manufacturer, y = cty)) + 
  stat_summary(fun.data = mean_cl_boot, colour = "black", size = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = .5)) +
  theme(text = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Car Manufacturer",
       x = "Manufacturer", 
       y = "City Fuel Economy (mpg)")

#innnetol kell atnezni
mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = fct_reorder(manufacturer, .fun = mean, cty), y = cty, colour = manufacturer)) +
  stat_summary(fun.data = mean_cl_boot, size = 1) +
  geom_jitter(alpha = .25) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "Manufacturer by City Fuel Economy",
       x = "Manufacturer", 
       y = "City Fuel Economy (mpg)") +
  guides(colour = 'none') +
  coord_flip()

mpg %>%
  filter(class != "suv") %>%
  mutate(class = str_to_title(class)) %>%
  ggplot(aes(x = displ, y = cty, colour = class)) + 
  geom_jitter(width = .2) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Engine Displacement",
       x = "Engine Displacement (litres)", 
       y = "City Fuel Economy (mpg)") +
  guides(colour = 'none') +
  facet_wrap(~ class)

mpg %>%
  mutate(class = str_to_upper(class)) %>%
  ggplot(aes(x = cty, y = displ)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  theme(text = element_text(size = 13)) +
  theme_minimal() + 
  labs(x = "City Fuel Economy (mpg)",
       y = "Engine Displacement (litres)", 
       colour = "Vehicle Class")

mpg %>%
  ggplot(aes(x = displ)) +
  geom_histogram(binwidth = .5, fill = "grey") +
  labs(title = "Histogram of Engine Displacement",
       x = "Engine Displacement (litres)",
       y = "Count")

library(ggridges)

mpg %>%
  mutate(class = str_to_title(class)) %>%
  ggplot(aes(x = displ, y = fct_reorder(class, .fun = mean, displ))) +
  geom_density_ridges(height = .5, aes(fill = class)) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  guides(fill = 'none') + 
  labs(x = "Engine Displacement (litres)",
       y = NULL)

library(NHANES)

ncol(NHANES)

nrow(NHANES)

head(NHANES)

NHANES %>% 
  select(ID) %>% 
  n_distinct()

NHANES_tidied <- NHANES %>% 
  distinct(ID, .keep_all = TRUE)

ncol(NHANES_tidied)

nrow(NHANES_tidied)

NHANES_tidied %>%
  ggplot(aes(x = BMI)) + 
  geom_histogram(bins = 100, na.rm = TRUE)

NHANES_tidied %>% 
  group_by(Education) %>% 
  summarise(median = median(BMI, na.rm = TRUE))

NHANES_tidied %>% 
  filter(!is.na(Education) & !is.na(BMI)) %>%
  ggplot(aes(x = Education, y = BMI, colour = Education)) + 
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  geom_boxplot(alpha = .5) +
  guides(colour = 'none') + 
  labs(title = "Examining the effect of education level on BMI", 
       x = "Education Level", 
       y = "BMI")

NHANES_tidied %>% 
  filter(!is.na(Education) & !is.na(BMI) & !is.na(Diabetes)) %>%
  ggplot(aes(x = Education:Diabetes, y = BMI, colour = Education)) + 
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  geom_boxplot(alpha = .5) +
  guides(colour = 'none') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(title = "Examining the effect of education level and diabetes on BMI", 
       x = "Education Level x Diabetes", 
       y = "BMI")

NHANES_tidied %>% 
  filter(!is.na(Education) & !is.na(BMI)) %>%
  group_by(Education) %>% 
  ggplot(aes(x = BMI, fill = Education)) +
  geom_histogram() +
  guides(fill = 'none') + 
  labs(title = "Examining the effect of education level on BMI",
       x = "BMI", 
       y = "Number of cases") + 
  facet_wrap(~ Education)

NHANES_tidied %>% 
  filter(!is.na(Education) & !is.na(BMI)) %>%
  group_by(Education) %>% 
  ggplot(aes(x = BMI, fill = Education)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(aes(y = ..density..)) +
  guides(fill = 'none') + 
  labs( title = "Examining the effect of education level on BMI", 
        x = "BMI", 
        y = "Density") + 
  facet_wrap(~ Education)
