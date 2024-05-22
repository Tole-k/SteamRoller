library(dplyr)
library(tidyr)
library(stringr)
data<-read.csv("data/steam.csv")
data<-data%>%
    drop_na()%>%
    filter(average_playtime>0&median_playtime>0)%>%
    mutate(DollarsPerHour=round(price/average_playtime,digits=4))%>%
    mutate(NegativeRatio=round(negative_ratings/(positive_ratings+negative_ratings),digits=3))%>%
    mutate(PositiveRatio=round(positive_ratings/(positive_ratings+negative_ratings),digits=3))%>%
    mutate(release_date=as.Date(release_date))%>%
    mutate(year=format(release_date,"%Y"))%>%
    separate(owners,sep ="-",into=c("min_owners","max_owners"))
dataD<-data%>%
    filter(price>0)%>%
    separate_rows(platforms,sep=";")%>%
    separate_rows(genres,sep=";")
dataP<-data%>%
    filter(positive_ratings+negative_ratings>0)%>%
    separate_rows(platforms,sep=";")%>%
    separate_rows(genres,sep=";")
dataH<-data%>%
    filter(min_owners > 10000)%>%
    group_by(year)%>%
    summarise(PositiveRatio=sum(positive_ratings)/(sum(positive_ratings)+sum(negative_ratings)))
data_desc<-read.csv("data/steam_description_data.csv")%>%drop_na()
data_desc<-merge(data,data_desc,by.x = "appid",by.y = "steam_appid")