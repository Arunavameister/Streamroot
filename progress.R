library(ggplot2)
library(dplyr)
rm(list=ls())

###############reading in data####################

data <- read.csv("D:/streamroot/data.csv", sep = ",", header = TRUE)
data

#######data usage by consumption for p2p##########

con<-data$p2p<160000
p2p_con<-ifelse(con, 'low data usage', 'medium data usage')
p2p_con1<-factor(p2p_con)
con<-data$p2p>190000
p2p_con2<-ifelse(con, 'high data usage', p2p_con)
p2p_con3<-factor(p2p_con2)


streams<-table(data$stream)
streams

#######number of people using low/medium/high amount of P2P data######

count_of_p2p_vol<-table(p2p_con3)

count_of_p2p_vol

barplot(count_of_p2p_vol,ylim=c(20000,400000),xlab="Data Consumption",ylab="Numbr of people",space = 0.5,main = "P2P Network", font.main = 4)


#######data usage by consumption for CDN##########

con4<-data$cdn<100000
cdn_con<-ifelse(con4, 'low data usage', 'medium data usage')
cdn_con1<-factor(cdn_con)
con4<-data$cdn>200000
cdn_con2<-ifelse(con4, 'high data usage', cdn_con)
cdn_con3<-factor(cdn_con2)
cdn_con3

#######number of people using low/medium/high amount of CDN data######

count_of_cdn_vol<-table(cdn_con3)

count_of_cdn_vol

barplot(count_of_cdn_vol,ylim=c(20000,200000),xlab="Data Consumption",ylab="Number of people",space = 0.5,main = "CDN Network", font.main = 4)

#########Analysis of false connected users################

count_of_users<-table(data$connected)
count_of_users

data_2<-data[(which(data$connected=="false")),]
data_2

#########CDN data consumption of false connected Users####

con_false<-data_2$cdn<100000
cdn_con_false<-ifelse(con_false, 'low data usage', 'medium data usage')
cdn_con1<-factor(cdn_con_false)
con_false<-data_2$cdn>200000
cdn_con2_false<-ifelse(con_false, 'high data usage', cdn_con)
cdn_con3_false<-factor(cdn_con2_false)
cdn_con3_false

count_of_cdn_vol_false<-table(cdn_con3_false)
count_of_cdn_vol_false

barplot(count_of_cdn_vol_false,ylim=c(0,30000),xlab="Data Consumption",ylab="Number of people",space = 0.5,main = "CDN Network", font.main = 4)




#######Grouping the data and finding out the percentage of network traffic(p2p/cdn) used########

byisp <- group_by(data,isp)
byisp

tt <- summarise(byisp, 
                p2p = sum(p2p, na.rm = TRUE), 
                cdn = sum(cdn, na.rm = TRUE),
                tot = sum(p2p, na.rm = TRUE) + sum(cdn, na.rm = TRUE)
)

tt$percent_p2p <- (tt$p2p/tt$tot)*100
tt$percent_cdn <- (tt$cdn/tt$tot)*100

tt$percent_p2p

tt$percent_cdn

percent_p2p<-tt$percent_p2p
percent_cdn<-tt$percent_cdn


isp_names<-c('Arange', 'BTP', 'Datch_Telcom', 'Fro', 'Olga')

isp_names

##########using factor variable to decribe the data#########

cond<-tt$percent_p2p<40
p2p_cond<-ifelse(cond, 'very small, not good', 'good distribution of traffic')
p2p_cond1<-factor(p2p_cond)
p2p_cond1


cond1<-tt$percent_cdn>60
cdn_cond<-ifelse(cond, 'very high, not good', 'good distribution of traffic')
cdn_cond1<-factor(cdn_cond)
cdn_cond1


#######see the data in a tabular form################

percentage_p2p<-cbind(isp_names,percent_p2p, p2p_cond)
percentage_cdn<-cbind(isp_names,percent_cdn,cdn_cond)
percentage_p2p
percentage_cdn

barplot(percent_p2p,names.arg=isp_names,xlab="different ISPs",ylab="Percentage usage",ylim=c(0,60),space = 1,main = "P2P Network", font.main = 4)
barplot(percent_cdn,names.arg=isp_names,xlab="different ISPs",ylab="Percentage usage",ylim=c(0,100),space = 1,main = "CDN Network", font.main = 4)



#############Grouping by stream###################

bystream <- group_by(data,stream)
bystream

tstream <- summarise(bystream, 
                p2p = sum(p2p, na.rm = TRUE), 
                cdn = sum(cdn, na.rm = TRUE),
                tot = sum(p2p, na.rm = TRUE) + sum(cdn, na.rm = TRUE)
)
stream_percentage_p2p <- (tstream$p2p/tstream$tot)*100
stream_percentage_cdn <- (tstream$cdn/tstream$tot)*100

stream_percentage_p2p

stream_percentage_cdn

stream_names<-c(1:10)
stream_names

stream_percent_P2P<-cbind(stream_names,stream_percentage_p2p)
stream_percent_P2P

stream_percent_CDN<-cbind(stream_names,stream_percentage_cdn)
stream_percent_CDN

barplot(stream_percentage_p2p,names.arg=stream_names,xlab="Different streams",ylab="Percentage usage",ylim=c(0,70),space = 1,main = "P2P Network", font.main = 4)
barplot(stream_percentage_cdn,names.arg=stream_names,xlab="Different streams",ylab="Percentage usage",ylim=c(0,100),space = 1,main = "CDN Network", font.main = 4)
