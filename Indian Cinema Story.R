setwd('C:\\Analytics\\MachineLearning\\Bollywood Movie Dataset')
library(ggplot2)
library(tidytext)
library(tidyverse)
library(highcharter)
library(janitor)
library(dplyr)
library(ggrepel)
library(plotly)
library(highcharter)
library(wordcloud)

Actor <- read_csv('BollywoodActorRanking.csv')
director <- read_csv('BollywoodDirectorRanking.csv')
movies <- read_csv('BollywoodMovieDetail.csv')

summary(Actor)
glimpse(Actor)
summary(director)
glimpse(director)
summary(movies)
glimpse(movies)


sum(is.na(Actor))
sum(is.na(director))
sum(is.na(movies))

# Actor
head(Actor)

ggplotly(Actor %>% group_by(actorName,movieCount)%>% arrange(desc(movieCount))%>% head(20) %>%ungroup()%>%mutate(actorName=reorder(actorName,-movieCount))%>%
           ggplot(aes(x=actorName,y=movieCount))+geom_bar(stat = "identity",color="black",fill="pink")+ 
           theme(axis.text.x = element_text(angle=45,hjust = 1,vjust = 0.5)) +labs(x="Actor name",y="Number of movies"))


ggplotly(Actor %>% group_by(actorName,ratingSum)%>% arrange(desc(ratingSum))%>% head(20) %>%ungroup()%>% mutate(actorName=reorder(actorName,-ratingSum))%>%
           ggplot(aes(x=actorName,y=ratingSum))+geom_bar(stat = "identity",color="black",fill="violet")+
           theme(axis.text.x = element_text(angle=45,hjust = 1,vjust = 0.5)) +labs(x="Actor name",y="Number of ratings"))


ggplotly(Actor %>% group_by(actorName,googleHits)%>% arrange(desc(googleHits))%>% head(20) %>%ungroup()%>% mutate(actorName=reorder(actorName,-googleHits))%>%
           ggplot(aes(x=actorName,y=googleHits))+geom_bar(stat = "identity",color="black",fill="brown")+
           theme(axis.text.x = element_text(angle=45,hjust = 1,vjust = 0.5)) +labs(x="Actor name",y="Number of googlehits"))


ggplotly(director %>% group_by(directorName,movieCount)%>%arrange(desc(movieCount))%>%head(20)%>%ungroup()%>%mutate(directorName=reorder(directorName,-movieCount))%>%
           ggplot(aes(x=directorName, y=movieCount))+geom_bar(stat = 'identity',color='black',fill='violet')+
           theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 0.5))+labs(x='Director name', y='Number of movies'))

ggplotly(director %>% group_by(directorName,ratingSum)%>%arrange(desc(ratingSum))%>%head(20)%>%ungroup()%>%mutate(directorName=reorder(directorName,-ratingSum))%>%
           ggplot(aes(x=directorName, y=ratingSum))+geom_bar(stat = 'identity',color='black',fill='orange')+
           theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 0.5))+labs(x='Director name', y='Number of ratings'))


ggplotly(director %>% group_by(directorName,googleHits)%>% arrange(desc(googleHits))%>% head(20) %>%ungroup()%>% mutate(directorName=reorder(directorName,-googleHits))%>%
           ggplot(aes(x=directorName,y=googleHits))+geom_bar(stat = "identity",color="black",fill="sky blue")+
           theme(axis.text.x = element_text(angle=45,hjust = 1,vjust = 0.5)) +labs(x="Director name",y="Number of google hits"))



# movies
# which genre is more popular in indian cinemas
movies %>% group_by(genre) %>% count()%>%hchart('treemap',
                                                hcaes(x='genre', value=n,color=n))


# hit movies
ggplotly(movies %>% group_by(releaseYear) %>% count()%>% arrange(desc(n))%>% ungroup()%>% mutate(releaseYear=reorder(releaseYear,-n))%>% head(15)%>%
           ggplot(aes(releaseYear,n,fill=releaseYear))+geom_bar(stat = "identity",color="black")+
           theme(axis.text.x = element_text(angle=45,hjust = 1,vjust = 0.5))+ggtitle("Hit movies"))


ggplotly(movies %>% group_by(title,hitFlop) %>% arrange(desc(hitFlop))%>% ungroup()%>% head(25)%>% mutate(title=reorder(title,-hitFlop))%>%
           ggplot(aes(title,y=hitFlop))+geom_bar(stat = "identity",color="black",fill="yellow")+theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 0.5))+ggtitle('Hit movies'))


ggplotly(movies %>% group_by(title,hitFlop) %>% arrange(desc(-hitFlop))%>% ungroup()%>% head(25)%>% mutate(title=reorder(title,-hitFlop))%>%
           ggplot(aes(title,y=hitFlop))+geom_bar(stat = "identity",color="black",fill="yellow")+theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 0.5))+ggtitle("Flop movies"))


# lets see the word cloud
movies %>% unnest_tokens(word,genre) %>% count(word) %>%
  with(wordcloud(word,n,color=brewer.pal(8,'Dark2')))

# top actors in bollywood
movies %>%  unnest_tokens(word,actors)%>% count(word) %>%
  with(wordcloud(word,n,color=brewer.pal(8,"Dark2")))
