# Analysis of my Instagram Account

#### Problem Statement :
I have downloaded data from my Instagram Account. This data 5-6 files. I'll use the below 3 files for my furtehr analysis:
#### 1. media.json - Contains data about my uploads (stories,pics and videos)
#### 2. connections.json - Contains data about followers/following
#### 3. likes.json - Contains about about who I liked

Instagram does not provide data

I have also created a Shiny App of the Exploratory Data Analysis I have done on the data for better understanding

[Shiny App for my Instagram Analysis](https://yatinkode.shinyapps.io/instaproject/)

```
Note : Since Instagram does not provide any information about usage statistics, we cannot really forecast the
usage of my Instagram in the future. I have considered the number of likes I do every moment as a usage 
statistics and forecast the likes done by me on any post/video etc. in the future 6 months. I have the data of
my account from May, 2014 to December, 2018. I will forecast the values of  my usage(likes done) for the 
months of January 2019 to June 2019
```
### Lets start with the code

#### Loading required libraries
```R
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(tseries)
library(data.table)
library(scales)
library(reshape2)
```

#### Loading Data
```R
media <- fromJSON("media.json", flatten=TRUE)              #Loading media json file
connections <- fromJSON("connections.json", flatten=TRUE)  #Loading connection data
likes <- fromJSON("likes.json", flatten=TRUE)              #Loading likes data
```
#### Performing EDA on media data
```R
str(media)

#-----------------getting the pics upload info out of media--------------------------
pics<-media$photos

pics<-separate(pics,taken_at,c("upload_date","upload_time"),sep="T")

pics$type="photo"    #Assigning the photo type to pics needed for further analysis

#Removing Location since it is not included in stories
pics<-pics[,-5]

#------------------getting the videos upload info out of media-----------------------
videos<-media$videos
videos<-separate(videos,taken_at,c("upload_date","upload_time"),sep="T")

videos$type="video" #Assigning the video type to video needed for further analysis

#Removing Location since it is not included in stories
videos<-videos[,-4]

#--------------------getting the stories upload info out of media---------------------
stories<-media$stories

#Removing data of 2019
stories<-stories[-which(grepl("2019-", stories$taken_at, fixed=TRUE)),]

stories<-separate(stories,taken_at,c("upload_date","upload_time"),sep="T")

stories$type="story"  #Assigning the story type to stories needed for further analysis

#combining pics ,video and stories into sngle dataframe
uploads<-rbind(pics,videos,stories)
nrow(uploads)

table(uploads$type)
#photo story video 
#183    57     7 

#Create a pie chart for getting proportion of pics,videos and stories
```

#### Create proportion pie chart for uploaded media types
```R
# Create a basic bar
pie <- ggplot(as.data.frame(table(uploads$type)), aes(x="", y=Freq, fill=Var1)) + geom_bar(stat="identity", width=100)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) +
      geom_text(aes(label = paste0(round((Freq*100)/sumfreq), "%")), position = position_stack(vjust = 0.5))

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Distribution of Media Uploads")

# Tidy up the theme
pie + theme_classic() + theme(axis.line = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              plot.title = element_text(hjust = 0.5, color = "#666666"))
```
![data](https://github.com/yatinkode/Personal-DS-and-ML-Projects/blob/master/Instagram%20Analysis/images/mediapie.png)

```R
#get monthly uploads
uploads$upload_date<-as.Date(uploads$upload_date,"%Y-%m-%d")

#Uploads per month
uploads$month<-format(uploads$upload_date,"%Y-%m")

#Uploads per year
uploads$Year<-format(uploads$upload_date,"%Y")
```

