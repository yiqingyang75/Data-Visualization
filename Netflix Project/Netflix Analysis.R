library(ggplot2)
library(dplyr)
library(tidyverse)
library(forcats)
library(stringr)
library(corrgram)

setwd("C:/Users/yiqin/Dropbox/UCD/17Fall/431 Data Visualization/Week2")
#setwd("C:/Users/Caroline/Dropbox/MSBA/BAX 431")

movie = read.csv("movie.csv")
#convert variables into correct type
movie$Adjusted_Gross = as.numeric(gsub(",","",as.character(movie$Adjusted_Gross)))
movie$Gross_rev = as.numeric(gsub(",","",as.character(movie$Gross_rev)))
movie$Profit = as.numeric(gsub(",","",as.character(movie$Profit)))
movie$Overseas_rev = as.numeric(gsub(",","",as.character(movie$Overseas_rev)))
movie$Release_Date <- as.POSIXct(movie$Release_Date, format = "%d/%m/%Y")

summary(movie)


#deal with outliers
Q1 = quantile(movie$Adjusted_Gross, 0.25)
Q3 = quantile(movie$Adjusted_Gross, 0.75)
IQR = IQR(movie$Adjusted_Gross, na.rm = TRUE)
movie = movie[movie$Adjusted_Gross<Q3+3*IQR,]
movie = movie[movie$Runtime_min>60 & movie$Runtime_min<180,]
movie[movie$Movie_Title == "Gone with the Wind",]

movie_2000 = movie[movie$Release_Date >= "2000-1-1",]

#Distribution
ggplot(movie,aes(x=Adjusted_Gross))+geom_histogram()
ggplot(movie,aes(x=Profit))+geom_histogram()
boxplot(movie$Adjusted_Gross)
boxplot(movie$Profit)
boxplot(movie$Runtime_min)

#by time
#thought: Is the way to adjust the inflation too dramatic? 
ggplot(movie,aes(x=Release_Date,y=Adjusted_Gross)) +geom_point()
ggplot(movie,aes(x=Release_Date,y=Gross_rev)) +geom_point()


#correlation map
corrgram(movie, order=NULL, panel=panel.shade, text.panel=panel.txt,
         main="Correlogram")

####revenue by genere and studio####
#which genere?
movie_2000 %>% group_by(Genre) %>%
  summarise(avg_revenue = mean(Adjusted_Gross),
            freq = n()) %>%
  arrange(desc(avg_revenue)) 
  
Genre_ave = aggregate(Adjusted_Gross~Genre,movie_2000,mean)
Genre_ave$Genre<- factor(Genre_ave$Genre, levels = Genre_ave$Genre[order(-Genre_ave$Adjusted_Gross)])
ggplot(Genre_ave,aes(x = Genre, y = Adjusted_Gross)) + geom_bar(stat = "identity") + 
  ggtitle("Movies' Average Adjusted Gross Revenue by Genre") +
  xlab("Genre") + ylab("Adjusted Gross Revenue in millions") + 
  theme_economist()

filter1 = movie_2000$Genre %in% c("action","animation","adventure","sci-fi")
movie1_2000 = movie_2000[filter1,]

#which studio?
df = movie_2000 %>%
  group_by(Studio, Genre) %>%
  summarise(avg_revenue = mean(Adjusted_Gross),
            freq = n()) 
df = as.data.frame(df)
df = df %>% group_by(Genre) %>%
  arrange(Genre,desc(avg_revenue)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 5)
# WB,DreamWorks,Paramount Pictures, Buena Vista Studios,DreamWorks,SONY, Fox, Universal
sort(table(df$Studio),decreasing = TRUE)

  

####Movie time####
cor(movie1_2000$Runtime_min,movie1_2000[,c("Profit","Adjusted_Gross","MovieLens_Rating","IMDb_Rating")])
#action & animation -> longer movie time
ggplot(movie1_2000, aes(x = Runtime_min, y = Adjusted_Gross)) +   
  geom_point() + geom_smooth(method=lm, se = TRUE) + facet_grid(Genre ~ .) 


####Director####
movie1_2000 %>%
  group_by(Director) %>%
  summarise(avg_profit = mean(Profit)) %>%
  arrange(desc(avg_profit)) %>%
  slice(1:10) %>% 
  ggplot(aes(x = fct_reorder(Director,avg_profit), y = avg_profit)) +
  geom_col() + coord_flip()
#fct_reorder -> Reorder Factor Levels By Sorting Along Another Variable



####Rating####
cor(movie1_2000$Adjusted_Gross,movie1_2000[,c("MovieLens_Rating","IMDb_Rating")])
cor(movie1_2000$Profit,movie1_2000[,c("MovieLens_Rating","IMDb_Rating")])

ggplot(movie, aes(x = MovieLens_Rating, y = Adjusted_Gross)) +   
  geom_point() + geom_smooth(method=lm, se = TRUE) 

ggplot(movie, aes(x = MovieLens_Rating, y = Profit)) +   
  geom_point() + geom_smooth(method=lm, se = TRUE) 

ggplot(movie, aes(x = IMDb_Rating, y = Adjusted_Gross)) +   
  geom_point() + geom_smooth(method=lm, se = TRUE) 




#If a movie does well in US, does it also usually do well overseas? 
ggplot(movie, aes(x = US_rev, y = Overseas_rev)) +   
  geom_point() + geom_smooth(se = TRUE) + geom_abline(slope = -1, intercept = 200)
#CT comment -- I added the ab line to demonstrate the $200m mininum on these plots. 

cor(movie$US_rev,movie$Overseas_rev)






#profit by genre and studio
df = movie %>%
  group_by(Studio, Genre) %>%
  summarise(avg_profit = mean(Profit_num)) 
 
df = as.data.frame(df)

df = df %>% group_by(Genre) %>%
  arrange(Genre,desc(avg_profit)) %>%
  mutate(rank = row_number()) %>%
  filter(rank %in% c(1,2,3))
  



#profit by weekday
ggplot(movie, aes(x = Profit_num, color = Day_of_Week)) + geom_freqpoly()
ggplot(movie, aes(x = Adjusted_Gross, color = Day_of_Week)) + geom_freqpoly() + xlim(0,1000)
ggplot(movie, aes(x = Day_of_Week, y = Profit_num)) + geom_boxplot() + ylim(0,400)
ggplot(movie, aes(x = Day_of_Week, y = Adjusted_Gross)) + geom_boxplot() + ylim(0,1000)

#Creates a variable where we cap the maximum.
#Although now I think some more about this, it's possible this doesn't help -- works well if you're 
#trying to calculate the average effect, but it may create other problems.  We can discuss.
movie_2000$CapAdjGross <- replace(movie_2000$Adjusted_Gross, movie_2000$Adjusted_Gross>1500, 1500)
ggplot(movie_2000, aes(x = CapAdjGross)) + geom_histogram(binwidth = 100)
filter(movie_2000, Adjusted_Gross > 1500)


#you have explored many variables and how they affect revenue
#please recommend a strategy for Netflix's next investment in a movie. 
