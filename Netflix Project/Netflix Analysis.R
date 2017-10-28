library(ggplot2)
library(dplyr)
library(tidyverse)
library(forcats)
library(stringr)
library(corrgram)

setwd("C:/Users/yiqin/Dropbox/UCD/17Fall/431 Data Visualization/Week2")

movie = read.csv("movie.csv")
#convert factor into numeric
movie$Adjusted_Gross = as.numeric(gsub(",","",as.character(movie$Adjusted_Gross)))
movie$Profit_num = as.numeric(gsub(",","",as.character(movie$Profit)))
movie$Overseas_rev = as.numeric(gsub(",","",as.character(movie$Overseas_rev)))

#correlation map
corrgram(movie, order=NULL, panel=panel.shade, text.panel=panel.txt,
         main="Correlogram")

#Does higher rating movie also have a higher adjusted gross revenue?
ggplot(movie, aes(x = MovieLens_Rating, y = Adjusted_Gross)) +   
  geom_point() + geom_smooth(method=lm, se = TRUE) + ylim(0,2000)


ggplot(movie, aes(x = IMDb_Rating, y = Adjusted_Gross)) +   
  geom_point() + geom_smooth(method=lm, se = TRUE) + ylim(0,2000)

cor(movie$MovieLens_Rating,movie$Adjusted_Gross)
cor(movie$IMDb_Rating,movie$Adjusted_Gross)

#Shall we make a short movie or a long movie?
pairs(movie[,c("Runtime_min","Profit_num","Adjusted_Gross","MovieLens_Rating","IMDb_Rating")])
cor(movie$Runtime_min,movie[,c("Profit_num","Adjusted_Gross","MovieLens_Rating","IMDb_Rating")])


#If a movie does well in US, does it also usually do well overseas? 
ggplot(movie, aes(x = US_rev, y = Overseas_rev)) +   
  geom_point() + geom_smooth(se = TRUE) 
cor(movie$US_rev,movie$Overseas_rev)


#Find top 10 Directors that makes movies that have highest average profit
movie %>%
  group_by(Director) %>%
  summarise(avg_profit = mean(Profit_num)) %>%
  arrange(desc(avg_profit)) %>%
  slice(1:10) %>% 
  ggplot(aes(x = fct_reorder(Director,avg_profit), y = avg_profit)) +
  geom_col() + coord_flip()
#fct_reorder -> Reorder Factor Levels By Sorting Along Another Variable

#profit by genre and studio
df = movie %>%
  group_by(Studio, Genre) %>%
  summarise(avg_profit = mean(Profit_num)) 
 
df = as.data.frame(df)

df = df %>% group_by(Genre) %>%
  arrange(Genre,desc(avg_profit)) %>%
  mutate(rank = row_number()) %>%
  filter(rank %in% c(1,2,3))
  
#revenue by genere and studio
df = movie %>%
  group_by(Studio, Genre) %>%
  summarise(avg_revenue = mean(Adjusted_Gross)) 

df = as.data.frame(df)

df = df %>% group_by(Genre) %>%
  arrange(Genre,desc(avg_revenue)) %>%
  mutate(rank = row_number()) %>%
  filter(rank %in% c(1,2,3))


#profit by weekday
ggplot(movie, aes(x = Profit_num, color = Day_of_Week)) + geom_freqpoly()
ggplot(movie, aes(x = Adjusted_Gross, color = Day_of_Week)) + geom_freqpoly() + xlim(0,1000)
ggplot(movie, aes(x = Day_of_Week, y = Profit_num)) + geom_boxplot() + ylim(0,400)
ggplot(movie, aes(x = Day_of_Week, y = Adjusted_Gross)) + geom_boxplot() + ylim(0,1000)

#you have explored many variables and how they affect revenue
#please recommend a strategy for Netflix's next investment in a movie. 
