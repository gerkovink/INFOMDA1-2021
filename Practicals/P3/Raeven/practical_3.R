# Practical 3
# Raeven van den Acker
# 21-09-2021

# Load packages ----
library(tidyverse)
library(magrittr)
library(mice)
library(DAAG)

# 1. Create age histogram
age_hist <- 
  ggplot(boys, aes(age)) +
  geom_histogram(binwidth = 0.4) +
  theme_minimal() +
  labs(x = "age [years]", title = "Distribution of Dutch boys' ages")
age_hist

# 2. Create gen bar chart
gen_bar <- 
  ggplot(boys, aes(gen)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Genital Tanner Stage", title = "Distribution of Dutch boys' genital tanner stage")
gen_bar

# 3. Create a missingness indicator for gen, phb & tv

R.boys <- 
  boys %>% 
  mutate(R.gen = is.na(gen), 
         R.phb = is.na(phb), 
         R.tv = is.na(tv))

# 4. Assess whether the missingness is related to age

plot(boys$age, R.gen)
plot(boys$age, R.phb)
plot(boys$age, R.tv)

# all three plots show that there are only data for gen, phb and tv starting from a certain age
# younger than this age, puberty has not started yet so there is not much point measuring these 

# 5. Create a histogram for age faceted by whether or not they have a missing value for gen

R.boys %>%   
  ggplot(aes(x=age)) +
  geom_histogram() +
  facet_wrap(~ R.gen) +
  theme_minimal() +
  labs(x = "age [years]", title = "Distribution of Dutch boys' ages")

# 6. Create a scatter plot of age v. BMI

R.boys %>% 
  ggplot(aes(x = age, y = bmi, col = R.gen)) +
  geom_point() +
  theme_minimal() + 
  labs(title = "Boys' BMI depending on their age")

# 8. Create a boxplot of region v. age

R.boys %>% 
  ggplot(aes(x=reg, y=age)) +
  geom_boxplot(fill = "orange") + 
  theme_minimal() +
  labs(title = "boxplot of boys' age per region")

# 9. Create a density plot of age, splitting by gen
 
R.boys %>% 
   ggplot(aes(x=age, fill = gen)) +
   geom_density(alpha = 0.7) +
   theme_minimal() + 
   labs(title = "density of boys' ages by Genital Tanner Stage")

# 10. Create a diverging bar chart for boys' heights

boys %>% 
  mutate(age = cut(age, 0:22, labels = paste0(0:21, " years")),
         height = hgt - mean(hgt, na.rm = TRUE)) %>% 
  group_by(age) %>% 
  summarize(height = mean(height, na.rm = TRUE)) %>% 
  mutate(color = ifelse(height > 0, "above average", "below average")) %>% 
  
  ggplot(aes(x = height, y = age, fill = color)) +
  geom_bar(stat = "identity") +
  theme_minimal()
  
# 11. Load elastic1 and elastic2 and combine them

elastics <- 
  bind_rows("elastic1" = elastic1, "elastic2" = elastic2, .id = "set")
  
# 12. & 13. Create a scatter plot of the elastics data with a regression line

elastics %>% 
  ggplot(aes(x = stretch, y = distance, color = set)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "stretch v. distance of elastic bands", 
       y = "distance band traveled on release")

# 14. Fit a linear regression model to elastic1 and elastic2

model1 <- lm(distance ~ stretch, elastic1)
model2 <- lm(distance ~ stretch, elastic2) 
  
# 15. For the models, determine the fitted values, their standard errors and their R^2

model1 %>% predict(se.fit = TRUE)
model2 %>% predict(se.fit = TRUE)

# 16. Study the residual v. leverage plots for both models

model1 %>% plot(which = 5)
model2 %>% plot(which = 5)

# 17. & 18. 

prediction <- predict(model1, newdata = elastic2)

predicted_data <-
  data.frame(distance = prediction, 
              stretch  = elastic2$stretch) %>%
  bind_rows(Predicted = .,
            Observed  = elastic2, 
            .id = "Predicted")

predicted_data %>%
  ggplot(aes(stretch, distance, col = Predicted)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Predicted and observed distances")
