######################################################################################################
#Self Instagram EDA with Shiny App
#Author: Yatin Kode
######################################################################################################

############################## Loading required Libraries ############################################
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(tseries)
library(data.table)
library(scales)
library(reshape2)
#####################################################################################################

############################# Loading theme for ggplot ##############################################
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

#####################################################################################################

##################################### Expolratory Data Analysis #####################################

#---------------------------------- Data about what I uploaded -------------------------------------#

#Loading media json file
media <- fromJSON("media.json", flatten=TRUE)
str(media)

#getting the pics upload info out of media
pics<-media$photos

pics<-separate(pics,taken_at,c("upload_date","upload_time"),sep="T")

pics$type="photo"    #Assigning the photo type to pics needed for further analysis

#Removing Location since it is not included in stories
pics<-pics[,-5]

#getting the videos upload info out of media
videos<-media$videos
videos<-separate(videos,taken_at,c("upload_date","upload_time"),sep="T")

videos$type="video" #Assigning the video type to video needed for further analysis

#Removing Location since it is not included in stories
videos<-videos[,-4]

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

sumfreq<-sum(as.data.frame(table(uploads$type))$Freq)

#Create a pie chart for getting proportion of pics,videos and stories

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


#get monthly uploads
uploads$upload_date<-as.Date(uploads$upload_date,"%Y-%m-%d")

#Uploads per month
uploads$month<-format(uploads$upload_date,"%Y-%m")

ggplot(uploads,aes(x=as.factor(uploads$month),y=1,fill=as.factor(uploads$type)))+
  geom_bar(stat = "identity")+xlab("Month-Year")+ylab("Number of uploads")+
  ggtitle("Uploads per Month")+guides(fill=guide_legend(title="Post Type"))+
  theme(axis.text.x=element_text(angle=60, hjust=1))


#Uploads per year
uploads$Year<-format(uploads$upload_date,"%Y")

ggplot(uploads,aes(x=as.factor(uploads$Year),y=1,fill=as.factor(uploads$type)))+
  geom_bar(stat = "identity")+xlab("Year")+ylab("Number of uploads")+
  ggtitle("Uploads per year")+guides(fill=guide_legend(title="Post Type"))


#For timeseries on uploads
monthlyuploads<-aggregate(uploads$caption, list(uploads$month),FUN=length)

#----------------------------------------------------------------------------------------------------#

#----------------------------- Data about my connections --------------------------------------------#

connections <- fromJSON("connections.json", flatten=TRUE)
str(connections)

#followers 

#tidying up the followers data frame
followers<-as.data.frame(connections$followers,header=F)
followers<-gather(followers,follower_id,time_followed , -1:-2)
followers[nrow(followers)+1,3]<-colnames(followers[1])
followers[nrow(followers),4]<-as.character(followers[1,1])
followers[nrow(followers)+1,3]<-colnames(followers[2])
followers[nrow(followers),4]<-as.character(followers[1,2])
followers<-followers[,c(3,4)]

#Removing data to 2019
followers<-followers[-which(grepl("2019-", followers$time_followed, fixed=TRUE)),]

#Convert followed time to date format
followers$time_followed<-as.Date(followers$time_followed,"%Y-%m-%d")


#followers per month
followers$month<-format(followers$time_followed,"%Y-%m")

ggplot(followers,aes(x=as.factor(followers$month),y=1))+
  geom_bar(stat = "identity",fill="steelblue")+xlab("Month-Year")+ylab("Number of Followers")+
  ggtitle("Followers per Month")+theme(axis.text.x=element_text(angle=60, hjust=1))


#followers per year
followers$Year<-format(followers$time_followed,"%Y")

ggplot(followers,aes(x=as.factor(followers$Year),y=1))+
  geom_bar(stat = "identity",fill="steelblue")+xlab("Year")+ylab("Number of Followers")+
  ggtitle("Followers per Year")



#Following

following<-as.data.frame(connections$following)

#tidying up following data frame to get correct data format
following<-gather(following,following_id,time_following , -1:-2)
following[nrow(following)+1,3]<-colnames(following[1])
following[nrow(following),4]<-as.character(following[1,1])
following[nrow(following)+1,3]<-colnames(following[2])
following[nrow(following),4]<-as.character(following[1,2])
following<-following[,c(3,4)]

#Removing data to 2019
following<-following[-which(grepl("2019-", following$time_following, fixed=TRUE)),]

#Convert following time to date format
following$time_following<-as.Date(following$time_following,"%Y-%m-%d")

#following per month
following$Month<-format(following$time_following,"%Y-%m")

ggplot(following,aes(x=following$Month,y=1))+
  geom_bar(stat = "identity",fill="steelblue")+xlab("Month-Year")+ylab("Number of Following")+
  ggtitle("Following per Month")+theme(axis.text.x=element_text(angle=60, hjust=1))


#following per year
following$Year<-format(following$time_following,"%Y")

ggplot(following,aes(x=as.factor(following$Year),y=1))+
  geom_bar(stat = "identity",fill="steelblue")+xlab("Year")+ylab("Number of Following")+
  ggtitle("Following per Year")



#------------------------------ Data about what I liked ---------------------------------------------#

likes <- fromJSON("likes.json", flatten=TRUE)
likes_df<-likes$media_likes

likes_df<-as.data.frame(likes_df)

#giving column names to the dataframe
names(likes_df)<-c("like_time","like_uid")

#Removing records of year 2019
likes_df<-likes_df[-which(grepl("2019-", likes_df$like_time, fixed=TRUE)),]

#Getting top 5 userids I liked
most_liked<-head(arrange(aggregate(likes_df$like_time, list(likes_df$like_uid), length),desc(x)),5)
most_liked$Group.1<-as.character(c("U1","U2","U3","U4","U5"))


#top most liked people/pages by me
ggplot(most_liked,aes(x=as.factor(most_liked$Group.1),y=most_liked$x))+geom_bar(stat = "identity",fill="steelblue")+theme_bw()+labs(x="Most Liked Users",y="Number of Likes")

#separate date and time
likes_df<-separate(likes_df,like_time,into=c("date","time"),sep="T")

#Separate hours out of time to further get hourly usage (likes done)
likes_df<-separate(likes_df,time,into=c("hour"),sep=":",remove=T)
#hourlyseries<-aggregate(likes_df$like_uid, list(likes_df$hour),FUN=length)

#ggplot(hourlyseries,aes(x=as.factor(hourlyseries$Group.1),y=hourlyseries$x))+geom_bar(stat = "identity",fill="steelblue")+theme_bw()+labs(x="Hours in a Day",y="Number of Likes",title = "Usage(Likes per hour)")



likes_df$date<-as.Date(likes_df$date,"%Y-%m-%d")

#Likes per month
likes_df$month<-format(likes_df$date,"%Y-%m")

#Converted to monthly series since it is lot of data to plot in Shiny apps make it unresponsive
monthlyseries<-likes_df[,c(4,3)]

monthlyseries<-aggregate(monthlyseries$like_uid, list(monthlyseries$month),FUN=length)

ggplot(monthlyseries,aes(x=as.factor(monthlyseries$Group.1),y=monthlyseries$x))+
  geom_bar(stat = "identity",fill="steelblue")+xlab("Month-Year")+ylab("Number of Likes Given")+
  ggtitle("Likes Given per Month")+theme(axis.text.x=element_text(angle=60, hjust=1))


#Likes per year
likes_df$Year<-format(likes_df$date,"%Y")

yearlyseries<-likes_df[,c(5,3)]

yearlyseries<-aggregate(yearlyseries$like_uid, list(yearlyseries$Year),FUN=length)

ggplot(yearlyseries,aes(x=as.factor(yearlyseries$Group.1),y=yearlyseries$x))+
  geom_bar(stat = "identity",fill="steelblue")+xlab("Year")+ylab("Number of Likes Given")+
  ggtitle("Likes Given per Year")


#Since Instagram does not provide any information about usage statistics, 
#we cannot really forcast the usage of my Instagram in the future. I have considered the number of 
#likes I do every moment as a usage statistics and forecast the likes done by me on any post/video
#etc. in the future 6 months

#So, likes = usage here

#Agregate monthly likes for time series forecasting

names(monthlyseries)[1]<-paste("Month")
names(monthlyseries)

monthlyseries$timeseq<-seq(1:nrow(monthlyseries))

total_timeser <- ts(monthlyseries$x,frequency=12,start=2014)
#ts(births, frequency = 12, start = c(1946, 1))

plot(decompose(total_timeser))

total_timeser <- ts(monthlyseries$x)

indata <- monthlyseries[1:50,-1]
timeser <- ts(indata$x)
plot(timeser,main="Plot for Timeseries of likes" ,xlab="Month",ylab="Likes")

#smoothing
w <-1
smoothedseries <- stats::filter(timeser,filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series
lines(smoothedseries, col="red", lwd=2)


############## Model Classical ##################

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

timevals_in <- indata$timeseq
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Likes')

#Now, let's fit a Multplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

#Since multplicative model fits the smoothehned series more appropriately than additive model we will choose multiplicative model
#Formula obtained after multiple trial and errors


outdata <- monthlyseries[51:57,-1]
#timevals_out <- outdata$Year.Month


timevals_out <-  data.frame(Month = outdata$timeseq)

#getting the best lm formula with lowest MAPE value
minMAPE <- 30
finalLmFormula <- ''
for(i in seq(0.1,1,0.1)){
  for(j in seq(1,10,1)){
    for(k in seq(0.1,1,0.1)){
      for(l in seq(1,4)){
        if(l>2 & k>0.1){
          next()
        }
        if(l==1)
          eval(parse(text = paste("lmformula<-as.formula(Likes ~ sin(Month*",i,") * poly(Month,",j,") * cos(Month*",k,"))")))
        if(l==2)
          eval(parse(text = paste("lmformula<-as.formula(Likes ~ sin(Month*",i,") * poly(Month,",j,") + cos(Month*",k,")* poly(Month,",j,"))")))
        if(l==3)
          eval(parse(text = paste("lmformula<-as.formula(Likes ~ sin(Month*",i,") * poly(Month,",j,"))")))
        if(l==4)
          eval(parse(text = paste("lmformula<-as.formula(Likes ~ cos(Month*",i,") * poly(Month,",j,"))")))
        lmfit <- lm(lmformula, data = smootheddf)
        global_pred <- predict(lmfit, Month=timevals_in)
        #lines(timevals_in, global_pred, col='blue', lwd=2)
        local_pred <- timeser-global_pred
        armafit <- auto.arima(local_pred)
        resi<-local_pred-fitted(armafit)
        fcast_arima <- predict(lmfit, timevals_out)
        MAPE_arima <- accuracy(fcast_arima, outdata$x)[5]
        global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
        fcast <- global_pred_out
        print(lmformula)
        print(accuracy(fcast,outdata$x)[5])
        if(accuracy(fcast,outdata$x)[5] < minMAPE){
          minMAPE <- accuracy(fcast,outdata$x)[5]
          finalLmFormula <- lmformula
        }
      }
    }
  }
}

finalLmFormula  #Likes ~ sin(Month * 0.4) * poly(Month, 2) * cos(Month * 0.1)
minMAPE  #10.66694


lmformula<-as.formula(Likes ~ sin(Month * 0.4) * poly(Month, 2) * cos(Month * 0.1))

lmfit <- lm(lmformula, data = smootheddf)


#lmfit <- lm(Sales ~ sin(0.6*Month) * poly(Month,3) * cos(0.6*Month) * poly(Month,3), data=smootheddf)


global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)

#Drawing the global prediction
lines(timevals_in, global_pred, col='blue', lwd=2)

legend("topleft", legend = c("Original","Smooth Series", "Regression Line"),
       text.width = strwidth("1,000,00000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red","blue"),
       title = "Line Types")

#Now, let's look at the locally predictable series. We will remove the trend and seasonality from the series and get local series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l",main="Local series of Insta Likes",xlab="Month",ylab="Likes")  # We have found out the local series
#Lets verify whether local series is white noise or not

#ACF test
acf(local_pred,main="ACF plot for local series of Insta Likes")   #AR(0)

#PACF test
pacf(local_pred,main="PACF plot for local series of Insta Likes")    #MA(0)

print(adf.test(local_pred,alternative = "stationary"))   #p-value = 0.01 Series is stationary since p-value below 0.05 in ADF test

print(kpss.test(local_pred))                              #p-value = 0.1 Series is stationary since p-value above 0.05 in KPSS test

#Lets see if the stationary series is weak or strong
armafit <- auto.arima(local_pred)
armafit                              #ARIMA(0,0,0) with zero mean 

tsdiag(armafit)


#Now we will get the residual
resi<-local_pred-fitted(armafit)

plot(resi,main="Residual series for Insta Likes")

acf(resi,main="ACF plot for residual series in Insta Likes")      #Almost all points are below cutoff value in ACF
pacf(resi,main="PACF plot for residual series in Insta Likes")    #Almost all points are below cutoff value in PACF

#Now we check whether the residual is white noise

adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -4.8972, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
#Since p-value for Augmented Dickey-Fuller Test is less than threshold 0.05 it is stationary

kpss.test(resi)
#KPSS Level = 0.041582, Truncation lag parameter = 3, p-value = 0.1
#Since p-value for KPSS test is greater than threshold 0.05 it is stationary

####################### Model Evaluation #############

fcast_arima <- predict(lmfit, timevals_out)
print(fcast_arima)
#       1        2        3        4        5        6          7
#1154.1455 1008.7896  897.4289  827.3084  791.8214  770.5264  734.4728

#MAPE (mean absolute percentage error) for finding out the error in evaluating our model
MAPE_arima <- accuracy(fcast_arima, outdata$x)[5]

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$x)[5]
MAPE_class_dec                                       #10.66694

#The error is very less so our model is good to go

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black" ,main="Forecasting Insta Likes by Classical Decomposition",xlab="Month",ylab="Likes")
lines(class_dec_pred, col = "red")
abline(v = 50, col="blue", lwd=2, lty=2)

legend("topleft", legend = c("Original","Forecasted"),
       text.width = strwidth("1,000,0000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red"),
       title = "Line Types")

#Future pred till June 2019
LikesFeb19toJune19 <- predict(lmfit,data.frame(Month=seq(1:63)))

LikesFeb19toJune19[59:63]
# 59        60        61        62        63 
#521.94769 341.98497 155.03805  23.32891  17.58068

plot(ts(total_timeser), col = "black" ,main="Forecasting Insta Likes by Classical Decomposition",xlab="Month",ylab="Likes")
lines(ts(LikesFeb19toJune19), col = "red")
abline(v = 50, col="blue", lwd=2, lty=2)
rect(c(57,0), -1e6, c(63,0), 1e6, col = rgb(0.5,0.5,0.5,1/3), border=NA)
legend("topleft", legend = c("Original","Predicted","Forecasted band"),
       text.width = strwidth("1,000,000000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red"),
       title = "Line Types")





########## Forecasting using Auto ARIMA ############################

autoarima <- auto.arima(timeser)
autoarima                        #ARIMA(0,1,1) 
#ARIMA method predicts that the series is of AR(2) and needed 1 level of differencing

plot(autoarima$x, col="black",main="Plotting Insta Likes using Auto-ARIMA",xlab="Month",ylab="Likes")
lines(fitted(autoarima), col="red")
legend("topleft", legend = c("Original","Forecasted"),
       text.width = strwidth("1,000,000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red"),
       title = "Line Types")

tsdiag(autoarima)               #Plotting diagnostics of time series


#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#Dickey-Fuller = -2.7296, Lag order = 3, p-value = 0.2816
#Hence residual is Stationary or white noise since p-value of ADF test is less than 0.05

kpss.test(resi_auto_arima)
#KPSS Level = 0.15451, Truncation lag parameter = 3, p-value = 0.1
#Hence residual is white noise since p-value of KPSS test greater tha 0.05

#Also, let's evaluate the model using MAPE and forecast the future values
fcast_auto_arima <- predict(autoarima, n.ahead = 12)
fcast_auto_arima$pred
#1375.24 1375.24 1375.24 1375.24 1375.24 1375.24 1375.24 1375.24 1375.24 1375.24 1375.24 1375.24

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$x)[5]
MAPE_auto_arima               #48.51751
#MAPE value obtained is pretty good in Auto ARIMA test

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black",main="Forecasting Insta Likes using Auto-ARIMA",xlab="Month",ylab="Sales")
lines(auto_arima_pred, col = "red")
abline(v = 42, col="blue", lwd=2, lty=2)
rect(c(57,0), -1e6, c(62,0), 1e6, col = rgb(0.5,0.5,0.5,1/3), border=NA)
legend("topleft", legend = c("Original","Predicted","Forecasted band"),
       text.width = strwidth("1,000,000000000"),
       lty = 1, xjust = 1, yjust = 1,
       col = c("black","red","grey"),
       title = "Line Types")




library(shiny)
library(plotly)



ui <- fluidPage(
  
  headerPanel("Instagram Explorer"),
  sidebarPanel(width="3",
    selectInput('y', 'Type', choices = c("Followers","Following","Likes Given"), selected = "Followers",width = "140px"),
    selectInput('x', 'Time Period', choices = c("Monthly","Yearly"), selected = "Month",width = "140px"),
    radioButtons("view", "View Type:",
                 c("Count", "Percent"), 
                 selected = "Count"),
  selectInput('z', 'Media Based Proportion', choices = c("Uploads","Pie Dist."), selected = "Uploads",width = "140px")),
  mainPanel(
    plotlyOutput('trendPlot',width="980px"),
    plotlyOutput('propPlot',width="980px")
  )
)

server <- function(input, output) {
  
  output$trendPlot <- renderPlotly({
    
    if(input$x=="Monthly" && input$y=="Followers"){
   p<- ggplot(followers,aes(x=followers$month,y=1))+
      geom_bar(stat = "identity",fill="steelblue")+xlab("Month-Year")+ylab("Number of Followers")+
      ggtitle("Followers per Month")+theme(axis.text.x=element_text(angle=60, hjust=2))
    
    }
    else if(input$x=="Yearly" && input$y=="Followers"){
      
      p<- ggplot(followers,aes(x=followers$Year,y=1))+
        geom_bar(stat = "identity",fill="steelblue")+xlab("Year")+ylab("Number of Followers")+
        ggtitle("Followers per Year")
      
    }
    else if(input$x=="Monthly" && input$y=="Following"){
      
      p<- ggplot(following,aes(x=following$Month,y=1))+
        geom_bar(stat = "identity",fill="steelblue")+xlab("Month-Year")+ylab("Number of Following")+
        ggtitle("Following per Month")+theme(axis.text.x=element_text(angle=60, hjust=1))
      
    }
    else if(input$x=="Yearly" && input$y=="Following"){
      p<- ggplot(following,aes(x=as.factor(following$Year),y=1))+
        geom_bar(stat = "identity",fill="steelblue")+xlab("Year")+ylab("Number of Following")+
        ggtitle("Following per Year")
      
    }
    else if(input$x=="Monthly" && input$y=="Likes Given"){
      p<-ggplot(likes_df,aes(x=as.factor(likes_df$month),y=1))+
        geom_bar(stat = "identity",fill="steelblue")+xlab("Month-Year")+ylab("Number of Likes Given")+
        ggtitle("Likes Given per Month")+theme(axis.text.x=element_text(angle=60, hjust=1))
    }  
    else{
      p<-ggplot(likes_df,aes(x=as.factor(likes_df$Year),y=1))+
        geom_bar(stat = "identity",fill="steelblue")+xlab("Year")+ylab("Number of Likes Given")+
        ggtitle("Likes Given per Year")
    }
    
    ggplotly(p)
    
  })
  
  
  output$propPlot <- renderPlotly({
    
    if(input$x=="Monthly" && input$z=="Uploads"){
      q<- ggplot(uploads,aes(x=as.factor(uploads$month),y=1,fill=as.factor(uploads$type)))+
        geom_bar(stat = "identity")+xlab("Month-Year")+ylab("Number of uploads")+
        ggtitle("Uploads per Month")+guides(fill=guide_legend(title="Post Type"))+
        theme(axis.text.x=element_text(angle=60, hjust=1))
    }
  else{
    q<-ggplot(uploads,aes(x=as.factor(uploads$Year),y=1,fill=as.factor(uploads$type)))+
      geom_bar(stat = "identity")+xlab("Year")+ylab("Number of uploads")+
      ggtitle("Uploads per year")+guides(fill=guide_legend(title="Post Type"))
  }
    else{
      q<-pie
    }
    ggplotly(q)
    
  })
  
}

shinyApp(ui, server)
