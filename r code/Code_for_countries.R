library(plyr)
library(dplyr)
library(ggplot2)
library(lattice)
library(data.table)
library(grid)
library(foreign)

setwd("C://Users//uqjalla1//Dropbox//WCS_WORK//Analyses_nov2018")

Strong <- read.csv("C://Users//uqjalla1//Dropbox//WCS_WORK//Analyses_nov2018//R_input_countries.csv")
head(Strong)

dt <- data.table(Strong)

countries_HF_93 <- data.frame(dt[,list(avHF93=mean(MeanHF93)),by=list(Country)])
write.csv(countries_HF_93,file="wcs_count_hf93.csv",row.names = FALSE)

countries_HF_09 <- data.frame(dt[,list(avHF09=mean(MeanHF09)),by=list(Country)])
write.csv(countries_HF_09,file="wcs_count_hf09.csv",row.names = FALSE)

countries_sp_imp <- data.frame(dt[,list(avspim=mean(MeanSpImp)),by=list(Country)])
write.csv(countries_sp_imp,file="wcs_count_spimp.csv",row.names = FALSE)

countries_area <- data.frame(dt[,list(carea=sum(Area)),by=list(Country)])
write.csv(countries_area,file="wcs_count_area.csv",row.names = FALSE)

countries_area4 <- data.frame(dt[,list(carea4=sum(Area4HF)),by=list(Country)])
write.csv(countries_area4,file="wcs_count_area4.csv",row.names = FALSE)

