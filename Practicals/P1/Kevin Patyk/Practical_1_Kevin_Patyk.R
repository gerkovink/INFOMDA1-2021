#setting the working directory
getwd()
setwd(dir = "C:\\Users\\kvnpa\\Desktop\\UU\\Utrecht Semester 3\\Statistical Learning and Visualization\\Practical 1")

#installing & loading required packages
packages <- c("ISLR", "tidyverse", "haven", "readxl")
install.packages(packages)
lapply(X = packages, FUN = library, character.only = T)

#saving the work space image in case needed for later
save.image(file = "Practical_1_KP.RData")
load("Practical_1_KP.RData")

#QUESTION NUMBER 1 ----:
object_1 <- 1:5
object_2 <- 1L:5L
object_3 <- "-123.456"
object_4 <- as.numeric(object_2)
object_5 <- letters[object_1]
object_6 <- as.factor(rep(object_5, 2))
object_7 <- c(1, 2, 3, "4", "5", "6")

lapply(X = mget(ls())[-8], FUN = class)
#or you can run the class() function over each object separately, but this was way more time efficient and reduces the lines of code I needed to write.

#QUESTION NUMBER 2 ----:
object_8 <- as.numeric(object_7)
class(object_8)

#QUESTION NUMBER 3 ----:
object_list <- mget(ls())[-9]
#or
object_list_alt <- list(object_1, object_2, object_3, object_4, object_5, object_6, object_7, object_8)

#accessing contents of a list
object_list$object_1
object_list$object_1[1]
#or
object_list[[1]]
object_list[[1]][1]

#QUESTION NUMBER 4 ----:
object_df <- data.frame(object_1, object_2, object_5)

#accessing contents of a data frame
object_df$object_1 #entire first column
#or
object_df[, 1] #entire first column

object_df[1, ] #entire first row
object_df[1, 3] #first row, third column

#QUESTION NUMBER 5 ----:
ncol(object_df) #3 columns/variables
nrow(object_df) #5 rows/observations

#QUESTION NUMBER 6 ----:
list.files()
df <- read_csv(file = "googleplaystore.csv")
#or
df_alt <- read.csv(file = "googleplaystore.csv")

#QUESTION NUMBER 7 ----:
#Did any column get a variable type you did not expect?

#ANSWER: Well, I guess I maybe expected the Size and Price variables to be numeric, but it makes sense they are not because they include non-numeric vlaues (M and $), so R is coercing them into character vectors. It makes sense that the number of installs is character, even though it is generally numeric, since we do not have the EXACT number of installs. Lastly, the Last Updated variable could be converted to a date. All of this can be fixed during pre-processing though.

#QUESTION NUMBER 8 ----:
head(df) #shows the first 6 rows of a data frame, unless otherwise specified

#QUESTION NUMBER 9 ----:

#Encountering an issue with a specific package, so I have to install and load it apparently.
install.packages('Rcpp')
library(Rcpp)

list.files()
students <- read_excel(file.choose()) #was encountering some issues with the path, so I just decided to choose the file manually. 

tail(students) #shows the last 6 rows of a data frame, unless otherwise specified
View(students) #this will allow us to visually inspect the data in a new window. It will show us the entire data set, even if it is very large. 

#QUESTION NUMBER 10 ----:
summary(students)
#The range of grades achieves by students is 4.84 - 9.29. 

#QUESTION NUMBER 11 ----:
?filter
#show the students with a grade lower than 5.5
students %>% 
  filter(grade < 5.5)

#QUESTION NUMBER 12 ----:
#show only the students with a grade higher than 8 from programme A
students %>% 
  filter(grade < 5.5 & programme == "A")

#QUESTION NUMBER 13 ----:
#sort the students dataset such that the students from programme A are on top of the data frame and within the programmes the highest grades come first.
students %>%
  arrange(programme, desc(grade))
#or
students %>%
  arrange(programme, -grade)

#QUESTION NUMBER 14 ----:
#show only the student_number and programme columns from the students dataset
students %>%
  select(student_number, programme)
#or
students %>% 
  select(-grade)

#QUESTION NUMBER 15 ----:
#use mutate() and recode() to change the codes in the programme column of the students dataset to their names. Store the result in a variable called students_recoded.
students_recoded <- students %>%
  mutate(programme_recoded = recode(programme, "A" = "Science", "B" = "Social Science"))
#or
students_recoded_alt <- students %>%
  mutate(programme_recoded = ifelse(programme == "A", "Science", "Social Science"))

#QUESTION NUMBER 16 ----:
#create a data processing pipeline that (a) loads the apps dataset, (b) parses the number of installs as 'Downloads' variable using mutate() and parse_number(), (c) shows only apps with more than 500 000 000 downloads, (d) orders them by rating (best on top), and (e) shows only the relevant columns (you can choose which are relevant, but select at least the Rating and Category variables). Save the result under the name popular_apps.
popular_apps <- read_csv(file = "googleplaystore.csv") %>%
  mutate(Downloads = parse_number(Installs)) %>%
  filter(Downloads > 500000000) %>%
  arrange(desc(Rating)) %>%
  select(App, Downloads, Rating, Category, Reviews, Genres, Type) %>%
  distinct(App, .keep_all = T)
  
#QUESTION NUMBER 17 ----:
#show the median, minimum, and maximum for the popular apps dataset you made in the previous assignment.
popular_apps %>%
  summarise("median" = median(Rating, na.rm = T), "maximum" = max(Rating, na.rm = T), "minimum" = min(Rating, na.rm = T))

#QUESTION NUMBER 18 ----:
mad <- function(x) {
  median(abs(x - median(x)))
}
#Add the median absolute deviation to the summaries you made before
popular_apps %>%
  summarise("median" = median(Rating, na.rm = T), "maximum" = max(Rating, na.rm = T), "minimum" = min(Rating, na.rm = T), "MAD" = mad(Rating))

#QUESTION NUMBER 19 ----:
popular_apps %>%
  group_by(Category) %>%
  summarise("mean_rating" = mean(Rating, na.rm = T))

#QUESTION NUMBER 20 ----:
#create an interesting summary based on the Google play store apps dataset. An example could be "do games get higher ratings than communication apps?"

#For this one, I just examined which categories have the highest average ratings and then removed any average ratings below 4, then arranged them with the highest ratings first, and kept only the top 5. The Games and Photography Categories have the highest average rating (both 4.5), followed by Productivity, Tools, and finally Entertainment. 
popular_apps %>%
  group_by(Category) %>%
  summarise("mean_rating" = mean(Rating, na.rm = T)) %>%
  filter(mean_rating > 4) %>%
  arrange(-mean_rating) %>%
  top_n(n = 5)

#I then wanted do this for Genres, but it turned out no different. Apparently the Arcade genre  is synonymous with the Games category. 
popular_apps %>%
  group_by(Genres) %>%
  summarise("mean_rating" = mean(Rating, na.rm = T)) %>%
  filter(mean_rating > 4) %>%
  arrange(-mean_rating) %>%
  top_n(n = 5)

#I then decided to try one more where I examined whether Free or Paid games receive higher ratings. This worked and free apps have a mean rating of 4.22. However, it was interesting that only Free apps popped up, when there is a distinction between Free and Paid in the original data set. This made me realize that all of the popular apps, which we specified with Downloads > 500000000, are Free. There are no paid ones which exceed our threshold. 
popular_apps %>% 
  group_by(Type) %>%
  summarise("mean_rating" = mean(Rating, na.rm = T))
