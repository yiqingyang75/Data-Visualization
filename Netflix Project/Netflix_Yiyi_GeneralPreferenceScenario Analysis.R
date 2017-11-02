setwd("~/Desktop/UCD MSBA/Data")

movie = read.csv("movie.csv")
#convert factor into numeric
movie$Adjusted_Gross = as.numeric(gsub(",","",as.character(movie$Adjusted_Gross)))
movie$Profit_num = as.numeric(gsub(",","",as.character(movie$Profit)))
movie$Overseas_rev = as.numeric(gsub(",","",as.character(movie$Overseas_rev)))

library(ggplot2)
##by time not needed ##
movie$Release_Date <- as.POSIXct(movie$Release_Date, format = "%d/%m/%Y")
movie_2010 = movie[movie$Release_Date >= "2010-1-1",]
movie_2000 = movie[movie$Release_Date >= "2000-1-1",]
movie_1985 = movie[movie$Release_Date >= "1985-1-1",]

#general analysis post 2000
stat_mean <- aggregate(Adjusted_Gross~Genre, movie_200, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Genre,Adjusted_Gross), y = Adjusted_Gross, fill = Genre)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Genre, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Genre")+ylab("Adjusted_Gross")+ggtitle("Adjusted_Gross by Genre post 2000")
#Studio, 2000
stat_mean <- aggregate(Adjusted_Gross~Studio, movie_2000, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Studio,Adjusted_Gross), y = Adjusted_Gross, fill = Studio)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Studio, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Studio")+ylab("Adjusted_Gross")+ggtitle("Adjusted_Gross by Studio")
#ROI studio 
stat_mean <- aggregate(Profit_perc~Studio, movie_2000, mean)
stat_mean$Profit_perc <- round(stat_mean$Profit_perc, 2)
ggplot(data = stat_mean, aes(x=reorder(Studio,Profit_perc), y = Profit_perc, fill = Studio)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Studio, y = Profit_perc, label = Profit_perc),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Studio")+ylab("ROI")+ggtitle("ROI by Studio post 2000")
#ROI GENRE
stat_mean <- aggregate(Profit_perc~Genre, movie_2000, mean)
stat_mean$Profit_perc <- round(stat_mean$Profit_perc, 2)
ggplot(data = stat_mean, aes(x=reorder(Genre,Profit_perc), y = Profit_perc, fill = Genre)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Genre, y = Profit_perc, label = Profit_perc),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Genre")+ylab("ROI")+ggtitle("ROI by Genre post 2000")

#Preference Shift Analysis post 2010
#general analysis post 2010
stat_mean <- aggregate(Adjusted_Gross~Genre, movie_2010, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Genre,Adjusted_Gross), y = Adjusted_Gross, fill = Genre)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Genre, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Genre")+ylab("Adjusted_Gross")+ggtitle("Adjusted_Gross by Genre post 2010")
# studio ROI
stat_mean <- aggregate(Profit_perc~Studio, movie_2010, mean)
stat_mean$Profit_perc <- round(stat_mean$Profit_perc, 2)
ggplot(data = stat_mean, aes(x=reorder(Studio,Profit_perc), y = Profit_perc, fill = Studio)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Studio, y = Profit_perc, label = Profit_perc),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Studio")+ylab("ROI")+ggtitle("ROI by Studio post 2010")
#ROI GENRE
stat_mean <- aggregate(Profit_perc~Genre, movie_2010, mean)
stat_mean$Profit_perc <- round(stat_mean$Profit_perc, 2)
ggplot(data = stat_mean, aes(x=reorder(Genre,Profit_perc), y = Profit_perc, fill = Genre)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Genre, y = Profit_perc, label = Profit_perc),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Genre")+ylab("ROI")+ggtitle("ROI by Genre post 2010")


#scenario analysis
#Success is Money!
#revenue>1000, revenue, post-2000
movie_successful_revenue = movie_2000[movie_2000$Adjusted_Gross>= 1000,]
stat_mean <- aggregate(Adjusted_Gross~Genre, movie_successful_revenue, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Genre,Adjusted_Gross), y = Adjusted_Gross, fill = Genre)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Genre, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Genre")+ylab("Adjusted Gross Revenue")+ggtitle("Successful (money) by Genre post 2000")

#successful director!! money post-2000
movie_successful_revenue = movie_2000[movie_2000$Adjusted_Gross>= 1000,]
stat_mean <- aggregate(Adjusted_Gross~Director, movie_successful_revenue, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Director,Adjusted_Gross), y = Adjusted_Gross, fill = Director)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Director, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Director")+ylab("Adjusted Gross Revenue")+ggtitle("Successful (money) by Director post 2000")

#successful Studio money &reputation post-2000
movie_successful_revenue = movie_2000[movie_2000$Adjusted_Gross>= 1000,]
stat_mean <- aggregate(Adjusted_Gross~Studio, movie_successful_revenue, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Studio,Adjusted_Gross), y = Adjusted_Gross, fill = Studio)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Studio, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Studio")+ylab("Adjusted Gross Revenue")+ggtitle("Successful (money) by Studio post 2000")

#Success is Money + Reputation
#rev>1000 &rating >=8
movie_successful_revenue = movie_2000[movie_2000$Adjusted_Gross>= 1000,]
movie_successful_revandrat = movie_successful_revenue[movie_successful_revenue$IMDb_Rating>=8.0,]
stat_mean <- aggregate(Adjusted_Gross~Genre, movie_successful_revandrat, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Genre,Adjusted_Gross), y = Adjusted_Gross, fill = Genre)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Genre, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Genre")+ylab("Adjusted Gross Revenue")+ggtitle("Successful (money&reputation) by Genre post 2000")

#successful director!! money&reputation post-2000
movie_successful_revenue = movie_2000[movie_2000$Adjusted_Gross>= 1000,]
movie_successful_revandrat = movie_successful_revenue[movie_successful_revenue$IMDb_Rating>=8.0,]
stat_mean <- aggregate(Adjusted_Gross~Director, movie_successful_revandrat, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Director,Adjusted_Gross), y = Adjusted_Gross, fill = Director)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Director, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Director")+ylab("Adjusted Gross Revenue")+ggtitle("Successful (money&reputation) by Director post 2000")

#successful Studio money &reputation post-2000
movie_successful_revenue = movie_2000[movie_2000$Adjusted_Gross>= 1000,]
movie_successful_revandrat = movie_successful_revenue[movie_successful_revenue$IMDb_Rating>=8.0,]
stat_mean <- aggregate(Adjusted_Gross~Studio, movie_successful_revandrat, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Studio,Adjusted_Gross), y = Adjusted_Gross, fill = Studio)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Studio, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Studio")+ylab("Adjusted Gross Revenue")+ggtitle("Successful (money&reputation) by Studio post 2000")


#Adjusted_Gross_Revenue, 2000
stat_mean <- aggregate(Adjusted_Gross~Genre, movie_2000, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Genre,Adjusted_Gross), y = Adjusted_Gross, fill = Genre)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Genre, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Genre")+ylab("Adjusted_Gross")+ggtitle("Adjusted_Gross by Genre")




#2010 scenario analysis --- not used
#revenue
movie_2010 = movie[movie$Release_Date >= "2010-1-1",]
stat_mean <- aggregate(Adjusted_Gross~Genre, movie_2010, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Genre,Adjusted_Gross), y = Adjusted_Gross, fill = Genre)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Genre, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Genre")+ylab("Adjusted_Gross")+ggtitle("Adjusted_Gross by Genre post 2010")

#ROI 
stat_mean <- aggregate(Profit_perc~Genre, movie_2010, mean)
stat_mean$Profit_perc <- round(stat_mean$Profit_perc, 2)
ggplot(data = stat_mean, aes(x=reorder(Genre,Profit_perc), y = Profit_perc)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Genre, y = Profit_perc, label = Profit_perc),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Genre")+ylab("Profit_perc")+ggtitle("Profit_perc by Genre")

#revenue>1000, revenue, post-2000
movie_successful_revenue = movie_2010[movie_2010$Adjusted_Gross>= 1000,]
stat_mean <- aggregate(Adjusted_Gross~Genre, movie_successful_revenue, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Genre,Adjusted_Gross), y = Adjusted_Gross)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Genre, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Genre")+ylab("Adjusted Gross Revenue")+ggtitle("Adjusted Gross Revenue by Genre")

#rev>1000 &rating >=8
movie_successful_revenue = movie_2010[movie_2010$Adjusted_Gross>= 1000,]
movie_successful_revandrat = movie_successful_revenue[movie_successful_revenue$IMDb_Rating>=8.0,]
stat_mean <- aggregate(Adjusted_Gross~Genre, movie_successful_revandrat, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Genre,Adjusted_Gross), y = Adjusted_Gross)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Genre, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Genre")+ylab("Adjusted Gross Revenue")+ggtitle("Adjusted Gross Revenue by Genre")

#successful director!! money&reputation post-2010
movie_successful_revenue = movie_2010[movie_2010$Adjusted_Gross>= 1000,]
movie_successful_revandrat = movie_successful_revenue[movie_successful_revenue$IMDb_Rating>=8.0,]
stat_mean <- aggregate(Adjusted_Gross~Director, movie_successful_revandrat, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Director,Adjusted_Gross), y = Adjusted_Gross)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Director, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Director")+ylab("Adjusted Gross Revenue")+ggtitle("Adjusted Gross Revenue by Director")

#succesful ROI director!! post-2010
movie_successful_revenue = movie_2010[movie_2010$Profit_perc>= 1000,]
movie_successful_revandrat = movie_successful_revenue[movie_successful_revenue$IMDb_Rating>=8.0,]
stat_mean <- aggregate(Profit_perc~Director, movie_successful_revandrat, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Profit_perc, 2)
ggplot(data = stat_mean, aes(x=reorder(Director,Profit_perc), y = Profit_perc)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Director, y = Profit_perc, label = Profit_perc),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Director")+ylab("Adjusted Gross Revenue")+ggtitle("Adjusted Gross Revenue by Director")

#successful Studio  post2010
movie_successful_revenue = movie_2010[movie_2010$Adjusted_Gross>= 1000,]
movie_successful_revandrat = movie_successful_revenue[movie_successful_revenue$IMDb_Rating>=8.0,]
stat_mean <- aggregate(Adjusted_Gross~Studio, movie_successful_revandrat, mean)
stat_mean$Adjusted_Gross <- round(stat_mean$Adjusted_Gross, 2)
ggplot(data = stat_mean, aes(x=reorder(Studio,Adjusted_Gross), y = Adjusted_Gross)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Studio, y = Adjusted_Gross, label = Adjusted_Gross),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Studio")+ylab("Adjusted Gross Revenue")+ggtitle("Adjusted Gross Revenue by Studio")

#successful studio ROI
stat_mean <- aggregate(Profit_perc~Studio, movie_2010, mean)
stat_mean$Profit_perc <- round(stat_mean$Profit_perc, 2)
ggplot(data = stat_mean, aes(x=reorder(Studio,Profit_perc), y = Profit_perc)) + geom_bar(stat = "identity") + geom_text(
  aes(x = Studio, y = Profit_perc, label = Profit_perc),
  position = position_dodge(width = 1),
  vjust = -0.5, size = 2)+ xlab("Studio")+ylab("Profit_perc")+ggtitle("Profit_perc by Studio")
 