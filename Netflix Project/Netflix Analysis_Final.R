library(ggplot2)
library(dplyr)
library(tidyverse)
library(forcats)
library(stringr)
library(corrgram)
library(ggthemes)
library(lubridate)

setwd("C:/Users/yiqin/Dropbox/UCD/17Fall/431 Data Visualization/Week2")

movie = read.csv("movie.csv")

#convert variables
movie$Adjusted_Gross = as.numeric(gsub(",","",as.character(movie$Adjusted_Gross)))
movie$Gross_rev = as.numeric(gsub(",","",as.character(movie$Gross_rev)))
movie$Profit = as.numeric(gsub(",","",as.character(movie$Profit)))
movie$Overseas_rev = as.numeric(gsub(",","",as.character(movie$Overseas_rev)))
movie$Release_Date <- as.POSIXct(movie$Release_Date, format = "%d/%m/%Y")
movie$Release_Month = month(movie$Release_Date)
movie$Release_Year = year(movie$Release_Date)
summary(movie)
summary(movie[,c("Adjusted_Gross","Gross_rev","Profit")])
str(movie)

#check the distribution, very long right tail -> outliers
ggplot(movie,aes(x=Adjusted_Gross))+geom_histogram() + 
  xlab("Ajusted Gross Revenue") + ggtitle("Distribution of Ajusted_Gross")
ggplot(movie,aes(x=Profit))+geom_histogram() + ggtitle("Distribution of Profit")
ggplot(movie,aes(x=Profit_perc))+geom_histogram() +
  xlab("ROI") + ggtitle("Distribution of ROI")


#define value > Q3 + 3*IQR as outliers, change the value of outliers into Q3 + 3*IQR
modify_outlier = function (df, variable) {
  Q3 = quantile(df[[variable]], 0.75)
  IQR = IQR(df[[variable]], na.rm = TRUE)
  df[df[[variable]]>Q3+3*IQR,variable] = Q3+3*IQR
  return(df)
}

movie = modify_outlier(movie,"Adjusted_Gross")
movie = modify_outlier(movie,"Profit")
movie = modify_outlier(movie,"Profit_perc")

#adjust revenue and roi into normal distributin for further ANOVA testing
movie$adj_rev = movie$Adjusted_Gross ^ (1/10)
ggplot(movie,aes(x=adj_rev))+geom_histogram(binwidth = 0.005) + 
  xlab("Ajusted Gross Revenue") + ggtitle("Distribution of Ajusted_Gross")
movie$adj_roi = movie$Profit_perc ^ (1/6)
ggplot(movie,aes(x=adj_roi))+geom_histogram(binwidth = 0.02) + 
  xlab("Ajusted Gross Revenue") + ggtitle("Distribution of Ajusted_Gross")

#inflation? preference? limit our time range to >2000
movie_2000 = movie[movie$Release_Date >= "2000-1-1",]
#inflation
ggplot(movie,aes(x=Release_Date,y=Adjusted_Gross)) +geom_point() +
  ylab("Ajuested Gross Revenue") + ggtitle ("Ajusted Gross Revenue by time") + theme_economist()


ggplot(movie_2000,aes(x=Release_Date,y=Adjusted_Gross)) +geom_point() +
  ylab("Ajuested Gross Revenue") + ggtitle ("Ajusted Gross Revenue by time") + theme_economist()

#preference
ggplot(movieC,aes(Release_Year, Genre,fill = Adjusted_Gross)) + geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") + 
  geom_vline(aes(xintercept=2000), colour='red', linetype='dashed', lwd=1) + 
  xlab("Year") + ggtitle("Preference of genre over year")

ggplot(movie_2000,aes(Release_Year, Genre,fill = Adjusted_Gross)) + geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") + 
  geom_vline(aes(xintercept=2000), colour='red', linetype='dashed', lwd=1) + 
  xlab("Year") + ggtitle("Preference of genre over year")


#Release time vs revenue
ggplot(movie_2000,aes(Day_of_Week, Genre,fill = Adjusted_Gross)) + geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") + xlab("Weekday") + ggtitle("Which day to release")
ggplot(movie_2000,aes(Release_Month, Genre,fill = Adjusted_Gross)) + geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") + xlab("Weekday") + ggtitle("Which month to release")


#relationship between revenue and ranking -> maybe some, cor > 0.3
ggplot(movie_2000, aes(x = MovieLens_Rating, y = Adjusted_Gross)) +   
  geom_point() + geom_smooth(se = TRUE) + 
  ylab ("Ajusted Gross Revenue") + ggtitle("MovieLens Rating vs Revenue")  + theme_economist()

ggplot(movie, aes(x = IMDb_Rating, y = Adjusted_Gross)) +   
  geom_point() + geom_smooth( se = TRUE) + 
  ylab ("Ajusted Gross Revenue") + ggtitle("IMDb Rating vs Revenue") + theme_economist()

cor(movie_2000$Adjusted_Gross,movie_2000[,c("MovieLens_Rating","IMDb_Rating")])


#relationship between ROI and ranking -> not much. cor < 0.2
ggplot(movie_2000, aes(x = MovieLens_Rating, y = Profit_perc)) +   
  geom_point() + geom_smooth(se = TRUE) + 
  ylab("ROI") + ggtitle ("MovieLens Rating VS ROI") + theme_economist()

ggplot(movie, aes(x = IMDb_Rating, y = Adjusted_Gross)) +   
  geom_point() + geom_smooth(se = TRUE) +
  ylab("ROI") + ggtitle ("IMDb Rating VS ROI") + theme_economist()
cor(movie_2000$Profit_perc,movie_2000[,c("MovieLens_Rating","IMDb_Rating")])


#relationship between budget and revenue
ggplot(data = movie_2000, aes(x = Budget, y = Adjusted_Gross)) + geom_point() + 
  geom_smooth(se=TRUE) + ylab("Revenue") +  ggtitle("Budget vs Revenue") + theme_economist()

#relationship between genre and revenue/profit
movie_2000 %>%
  group_by(Genre) %>%
  summarise(Avg_Revenue = mean(Adjusted_Gross)) %>%
  arrange(desc(Avg_Revenue)) %>%
  ggplot(aes(x = reorder(Genre,Avg_Revenue), y = Avg_Revenue)) + 
  geom_bar(stat = "identity",fill = "#56B4E9") + 
  ylab("Average Revenue") + xlab("Genre") + ggtitle("Genre vs Revenue")+ 
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) 

movie_2000 %>%
  group_by(Genre) %>%
  summarise(Avg_profit = mean(Profit)) %>%
  arrange(desc(Avg_profit)) %>%
  ggplot(aes(x = reorder(Genre,Avg_profit), y = Avg_profit)) + 
  geom_bar(stat = "identity",fill = "#56B4E9") + 
  ylab("Average Profit") + xlab("Genre") + ggtitle("Genre vs Profit")+ 
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

#running time vs revenue?
ggplot(movie_2000,aes(Runtime_min, Genre,fill = Adjusted_Gross)) + geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") + 
  xlab("Running Time") + ggtitle("Running time vs Revenue by Genre")

#If a movie does well in US, does it also usually do well overseas? 
ggplot(movie_2000, aes(x = US_rev, y = Overseas_rev)) +   
  geom_point() + geom_smooth(se = TRUE) + 
  xlab("US Revenue") + ylab("Overseas Revenue") + ggtitle("US Revenue vs Overseas Revenue") + 
  theme_economist()
cor(movie$US_rev,movie$Overseas_rev)

#####which genere?####
by_genre = movie_2000 %>% group_by(Genre) %>%
  summarise(avg_revenue = mean(Adjusted_Gross),
            avg_Roi = mean(Profit_perc),
            avg_rate = mean(IMDb_Rating),
            freq = n()) %>%
  arrange(desc(avg_revenue))

target_rate = quantile(movie_2000$IMDb_Rating, 0.5)
target_revenue = quantile(movie_2000$Adjusted_Gross, 0.5)
target_ROI = quantile(movie_2000$Profit_perc, 0.5)

#if focus on Revenue, chose adventure/sci-fi
ggplot(by_genre,aes(x = avg_revenue, y = avg_rate, colour = Genre, size = freq)) + geom_point() +
  geom_hline(aes(yintercept=target_rate), colour='red', linetype='dashed', lwd=1) +
  geom_vline(aes(xintercept=target_revenue), colour='red', linetype='dashed', lwd=1) + 
  geom_text(aes(label = Genre)) + 
  xlab("Revenue") + ylab("Rate") + ggtitle ("Select a genre with high revenue and rating")

#if focus on ROI, chose drama
ggplot(by_genre,aes(x = avg_Roi, y = avg_rate, colour = Genre, size = freq)) + geom_point() +
  geom_hline(aes(yintercept=target_rate), colour='red', linetype='dashed', lwd=1) +
  geom_vline(aes(xintercept=target_ROI), colour='red', linetype='dashed', lwd=1) +
  geom_text(aes(label = Genre)) + xlab("ROI") + ylab("Rate") + ggtitle ("Select a genre with high ROI and rating")

#subset target genre 
filter = movie_2000$Genre %in% c("drama","adventure","sci-fi")
movie2000_target = movie_2000[filter,]
movie2000_drama = movie_2000[movie_2000$Genre == "drama",]
movie2000_adventure= movie_2000[movie_2000$Genre == "adventure",]
movie2000_sci = movie_2000[movie_2000$Genre == "sci-fi",]


####which director?####
#Just focus on the top 30 ranking directors
movie2000_target30 = as.data.frame(
  movie2000_target %>%
  group_by(Director,Genre) %>%
  summarise(avg_revenue = mean(Adjusted_Gross),
            avg_ROI = mean(Profit_perc),
            avg_rate = mean(IMDb_Rating),
            freq = n()))
movie2000_target30 = as.data.frame(movie2000_target30 %>% arrange(desc(avg_rate)) %>% slice(1:30) ) 

#if based on revenue
ggplot(movie2000_target30[movie2000_target30$Genre %in% c("adventure", "sci-fi"),], 
       aes(x = fct_reorder(Director,avg_revenue), y = avg_revenue, fill = Genre)) +
  geom_col(position = "dodge") + coord_flip() +
  xlab("Director") + ylab("Revenue") + ggtitle("Select Director with high Revenue")

#if based on ROI
ggplot(movie2000_target30[movie2000_target30$Genre %in% c("drama"),], 
       aes(x = fct_reorder(Director,avg_ROI), y = avg_ROI, fill = Genre)) +
  geom_col(position = "dodge") + coord_flip() +
  xlab("Director") + ylab("Revenue") + ggtitle("Select Director with high ROI")


####which studio?####
#Just focus on the top 30 ranking directors
movie2000_target_Studio = as.data.frame(
  movie2000_target %>%
    group_by(Studio,Genre) %>%
    summarise(avg_revenue = mean(Adjusted_Gross),
              avg_ROI = mean(Profit_perc),
              avg_rate = mean(IMDb_Rating),
              freq = n()))
movie2000_target_Studio = as.data.frame(movie2000_target_Studio %>% arrange(desc(avg_rate)) %>% slice(1:10) ) 

#if based on revenue
ggplot(movie2000_target_Studio[movie2000_target_Studio$Genre %in% c("adventure", "sci-fi"),], 
       aes(x = fct_reorder(Studio,avg_revenue), y = avg_revenue, fill = Genre)) +
  geom_col(position = "dodge") + coord_flip() +
  xlab("Studio") + ylab("Revenue") + ggtitle("Select Studio with high Revenue")

#if based on ROI
ggplot(movie2000_target_Studio[movie2000_target_Studio$Genre %in% c("drama"),], 
       aes(x = fct_reorder(Studio,avg_ROI), y = avg_ROI, fill = Genre)) +
  geom_col(position = "dodge") + coord_flip() +
  xlab("Studio") + ylab("ROI") + ggtitle("Select Studio with high ROI")

####running time?####
#action & animation -> longer movie time
ggplot(movie2000_target, aes(x = Runtime_min, y = Adjusted_Gross)) +   
  geom_point() + geom_smooth(method=lm, se = TRUE) + facet_grid(Genre ~ .) +
  xlab ("Movie Time") + ylab("Revenue") + ggtitle("Movie Time vs Revenue")

ggplot(movie2000_target, aes(x = Runtime_min, y = Profit_perc)) +   
  geom_point() + geom_smooth(method=lm, se = TRUE) + facet_grid(Genre ~ .) +
  xlab ("Movie Time") + ylab("ROI") + ggtitle ("Movie Time vs ROI")


####Release Month?####
movie2000_target$Release_Month = as.factor(movie2000_target$Release_Month)
movie2000_adventure$Release_Month = as.factor(movie2000_adventure$Release_Month)
movie2000_sci$Release_Month = as.factor(movie2000_sci$Release_Month)
movie2000_drama$Release_Month = as.factor(movie2000_drama$Release_Month)
movie_2000$Release_Month = as.factor(movie_2000$Release_Month)

ggplot(movie2000_target[movie2000_target$Genre %in% c("adventure","sci-fi"),],
       aes(y = Adjusted_Gross, x = Release_Month, fill = Genre)) + geom_boxplot() +
  xlab("Release Month") + ylab("Revenue") + ggtitle ("Revenue by Month")

ggplot(movie2000_drama,
       aes(y = Profit_perc, x = Release_Month, fill = Genre)) + geom_boxplot() + 
  xlab("Release Month") + ylab("ROI") + ggtitle ("ROI by Month")

#ANOVA test acroos month is not significant
adventure.aov = aov(adj_rev ~ Release_Month, data = movie2000_adventure)
summary(adventure.aov)

sci.aov = aov(adj_rev ~ Release_Month, data = movie2000_sci)
summary(sci.aov)
TukeyHSD(sci.aov)

drama.aov = aov(adj_roi ~ Release_Month, data = movie2000_drama)
summary(drama.aov)


#ANOVA test for overall Revenue acroos month
ggplot(movie_2000, aes(y = Adjusted_Gross, x = Release_Month)) + geom_boxplot(fill = "#56B4E9") + 
  xlab("Release Month") + ylab("Revenue") + ggtitle ("Overall Revenue by Month")

month_aov = aov(adj_rev ~ Release_Month, movie_2000)
summary(month_aov)
month_aov = as.data.frame(TukeyHSD(month_aov)[1])
month_aov[month_aov$Release_Month.p.adj<0.05,]

#ANOVA test for overall ROI acroos month
ggplot(movie_2000, aes(y = adj_roi, x = Release_Month)) + geom_boxplot(fill = "#56B4E9") + 
  xlab("Release Month") + ylab("ROI") + ggtitle ("Overall ROI by Month")
month_roi_aov =  aov(Profit_perc ~ Release_Month, movie_2000)
summary(month_roi_aov)
month_roi_aov = as.data.frame(TukeyHSD(month_roi_aov)[1])
month_roi_aov[month_roi_aov$Release_Month.p.adj<0.05,]


#scenario analysis
#Success is Money!
#revenue>1000, revenue, post-2000
movie_successful_revenue = movie_2000[movie_2000$Adjusted_Gross>= 1000,]
stat_mean <- aggregate(Adjusted_Gross~Genre, movie_successful_revenue, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Genre,Adjusted_Gross), y = Adjusted_Gross, fill = Genre)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Genre, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Genre")+ylab("Adjusted Gross Revenue")+ggtitle("Successful (revenue>1000) by Genre")

#successful director!! money post-2000
movie_successful_revenue = movie_2000[movie_2000$Adjusted_Gross>= 1000,]
stat_mean <- aggregate(Adjusted_Gross~Director, movie_successful_revenue, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Director,Adjusted_Gross), y = Adjusted_Gross, fill = Director)) + 
  geom_bar(stat = "identity") + geom_text(
    aes(x = Director, y = Adjusted_Gross, label = Adjusted_Gross),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2) + coord_flip() +
  xlab("Director")+ylab("Adjusted Gross Revenue")+ggtitle("Successful (revenue>1000) by Director")

#successful Studio money &reputation post-2000
movie_successful_revenue = movie_2000[movie_2000$Adjusted_Gross>= 1000,]
stat_mean <- aggregate(Adjusted_Gross~Studio, movie_successful_revenue, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Studio,Adjusted_Gross), y = Adjusted_Gross, fill = Studio)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Studio, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2) + coord_flip() + 
  xlab("Studio")+ylab("Adjusted Gross Revenue")+ggtitle("Successful (revenue > 1000) by Studio")

#Success is Money + Reputation
#rev>1000 &rating >=8
movie_successful_revenue = movie_2000[movie_2000$Adjusted_Gross>= 1000,]
movie_successful_revandrat = movie_successful_revenue[movie_successful_revenue$IMDb_Rating>=8.0,]
stat_mean <- aggregate(Adjusted_Gross~Genre, movie_successful_revandrat, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Genre,Adjusted_Gross), y = Adjusted_Gross, fill = Genre)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Genre, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ 
  xlab("Genre")+ylab("Adjusted Gross Revenue")+
  ggtitle("Successful (revenue>1000 & rate > 8) by Genre")

#successful director!! money&reputation post-2000
movie_successful_revenue = movie_2000[movie_2000$Adjusted_Gross>= 1000,]
movie_successful_revandrat = movie_successful_revenue[movie_successful_revenue$IMDb_Rating>=8.0,]
stat_mean <- aggregate(Adjusted_Gross~Director, movie_successful_revandrat, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Director,Adjusted_Gross), y = Adjusted_Gross, fill = Director)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Director, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ coord_flip() +
  xlab("Director")+ylab("Adjusted Gross Revenue")+ggtitle("Successful (revenue>1000 & rate>8 by Director")

#successful Studio money &reputation post-2000
movie_successful_revenue = movie_2000[movie_2000$Adjusted_Gross>= 1000,]
movie_successful_revandrat = movie_successful_revenue[movie_successful_revenue$IMDb_Rating>=8.0,]
stat_mean <- aggregate(Adjusted_Gross~Studio, movie_successful_revandrat, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Studio,Adjusted_Gross), y = Adjusted_Gross, fill = Studio)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Studio, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ coord_flip() +
  xlab("Studio")+ylab("Adjusted Gross Revenue")+ggtitle("Successful (revenue>1000 & rate>8) by Studio")
