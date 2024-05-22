library(dplyr)
library(tidyr)
data<-read.csv("Assignment4/data/steam.csv")
data%>%separate_rows(genres)%>%distinct(genres)
data[1,]