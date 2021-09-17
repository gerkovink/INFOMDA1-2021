# Practical 2
# Raeven van den Acker
# 13-09-2021

# Load packages ----
library(ISLR)
library(tidyverse)
library(magrittr)

# 1. Name aesthetics, geoms, scales, and facets of homeruns_plot ----
  # aesthetics: x = hits, y = homeruns
  # geoms: point for the scatter plot, density 2D for the blue contours
  # scales: ?
  # facets: one facet
  # coordinate system: Cartesian

# 2. students data ----

# generate data
set.seed(1234)
student_grade  <- rnorm(32, 7)
student_number <- round(runif(32) * 2e6 + 5e6)
programme      <- sample(c("Science", "Social Science"), 32, replace = TRUE)

# put data in tibble
gg_students <- tibble(student_grade, student_number, programme)

# 3. & 4. Replot homeruns_plot ----
homeruns_plot <- 
  ggplot(Hitters, aes(x = HmRun, y = Hits, color = League, size = Salary)) +
  geom_point() +
  labs(x = "Home runs", y = "Hits", title = "Replotted homeruns_plot")
 homeruns_plot

# 6. Create a histogram of students' grades
grades_hist <-
  ggplot(gg_students, aes(student_grade)) +
  geom_histogram(binwidth = 0.1) +
  labs(x = "student grade", y = "frequency", title = "Histogram of students' grades")
grades_hist

# 7. & 8. & 9. Create a density plot of students' grades & add rugg marks & increase data:ink
grades_density <-
  ggplot(gg_students, aes(student_grade)) +
  geom_density(fill = "light seagreen", color = NA) +
  geom_rug(color = "orange", size = 1) +
  labs(x = "student grade", y = " ", title = "Density plot of students' grades") +
  theme_minimal() +
  xlim(0,10)
grades_density

# 10. Create boxplot of students' grades
grades_boxplot <-
  ggplot(gg_students, aes(x = programme, y = student_grade, fill = programme)) +
  geom_boxplot() +
  labs(x = "programme", y = "student grade", title = "Box plot of students' grades")
 grades_boxplot

 # 11. What do the lines in the boxplot mean?
  # thick horizontal line: median of data
  # upper horizontal: median of the upper half of data -> third quartile
  # lower horizontal: median of the lower half of data -> first quartile
  # upper vertical: reaches until the maximum data value (excluding outliers)
  # lower vertical: reaches until the minimum data value (excluding outliers)

# 12. Comparing distributions
 grades_density_comp <-
   ggplot(gg_students, aes(student_grade, fill = programme)) +
   geom_density(color = NA, alpha = 0.7) +
   labs(x = "student grade", y = " ", title = "Density plot of students' grades") +
   theme_minimal() +
   xlim(0,10)
 grades_density_comp

# 13. Bar plot of Hitters$Years
years_barplot <-
  ggplot(Hitters, aes(Years))+
  geom_bar() +
  labs(title = "Bar plot of number of years of players in the major league")
years_barplot

# 14. - 17. Create a line plot of the number of trades/day

trades_lineplot <-
  mutate(Smarket[1:200, ], Day = 1:200) %>% 
  
  ggplot(aes(x = Day, y = Volume)) +
  geom_line(color = "orange", size = 0.7) +
  geom_point(color = "orange") +
  geom_label(aes(x = which.max(Volume), y = max(Volume)+0.07, label = "Peak volume"))
  labs(title = "Number of trades made per day")

trades_lineplot

# 18. Create the baseball data frame
  
filt_Hitters <- filter(Hitters, Salary != "NA")

baseball <-
  cut(filt_Hitters$Salary, breaks = 3, labels = c("low","med","high")) %>% 
  mutate(filt_Hitters, Salary_bracket = .) %>% 
  mutate(Prop_HmRun = CHmRun/CHits)

# 19. & 20. Create the baseball_plot

baseball_plot <-
  ggplot(baseball, aes(x = CWalks, y = Prop_HmRun)) +
  geom_point() +
  xlim(0, 1600) + ylim(0, 0.4) +
  labs(title = "How career walks affect the proportion of home runs, by salary bracket", 
       x = "Number of career walks", 
       y = "Proportion of career hits that were a home run") +
  facet_wrap(salary_bracket)

baseball_plot

# 21. Carseats data

car_seats <-
  cut(Carseats$Advertising, breaks = 3, labels = c("low","med","high")) %>% 
  mutate(Carseats, Ad_bracket = .)

carseats_plot <-
  ggplot(car_seats, aes(x = Education , y = Sales)) +
  geom_point() +
  #xlim(0, 1600) + ylim(0, 0.4) +
  labs(title = "How education affects car seat sales depending on advertising budget", 
       x = "Education", 
       y = "Unit sales (x 1000)") +
  facet_wrap(car_seats$Ad_bracket)

carseats_plot
