library(plyr)
library(dplyr)
library(ggplot2)
library(lattice)
library(data.table)
library(grid)
library(foreign)
library(reshape2)
library(tidyr)
library(broom)
library(gtable)
library(gridExtra)
library(ggrepel)

setwd("C://Users//James Allan//Dropbox//WCS_WORK//Analyses_nov2018")

Strong <- read.csv("C://Users//James Allan//Dropbox//WCS_WORK//Analyses_nov2018//R_input_all.csv")
Strong2 <- na.omit(Strong)
head(Strong2)
summary(Strong2)

#use for country t-tests
Strong <- read.csv("C://Users//James Allan//Dropbox//WCS_WORK//Analyses_nov2018//R_input_country_t_tests.csv")
Strong2 <- na.omit(Strong)
head(Strong2)
summary(Strong2)

write.csv(Strong2,file="Database_no_na.csv",row.names = FALSE)

########## CALCULATE GLOBAL SUMMARY STATISTICS (WELCH TWO SAMPLE T-TEST) ##########################

# T tests of the global mean footprints for 09 between WCS and Other PA's
t.test(Strong2$Mean_HF_09 ~ Strong2$Category)

class(Strong$Mean_HF_93)
Strong2$Mean_HF_93 <- as.numeric(as.character(Strong2$Mean_HF_93)) #Make numeric not factor
t.test(Strong2$Mean_HF_93 ~ Strong2$Category)

t.test(Strong2$impact ~ Strong2$Category)

class(Strong$perc_4_th)
Strong2$perc_4_th <- as.numeric(as.character(Strong2$perc_4_th)) #Make numeric not factor
t.test(Strong2$perc_4_th ~ Strong2$Category)

class(Strong2$area_4_th)
Strong2$area_4_th <- as.numeric(as.character(Strong2$area_4_th)) #Make numeric not factor
t.test(Strong2$area_4_th ~ Strong2$Category)

# Standard Errors

sem<-sd(Strong2$Mean_HF_09)/sqrt(length(Strong2$Mean_HF_09))
sem

sem<-sd(Strong2$Mean_HF_93)/sqrt(length(Strong2$Mean_HF_93))
sem

sem<-sd(Strong2$impact)/sqrt(length(Strong2$impact))
sem

Strong3 <- na.omit(Strong2)
summary(Strong3)

sem<-sd(Strong3$perc_4_th)/sqrt(length(Strong3$perc_4_th))
sem

######### COUNTRY SUMMARY ##################################################################################

## Country Tables ###########################################################################################

### for mean human footprint 2009
dt <- data.table(Strong3)

C1 <- data.frame(dt[,list(Mean_HF_09 =mean(Mean_HF_09)),by=list(Country2,Category)])

C2 <- dcast(C1, Country2 ~ Category)



C2$Pvalue <- rep(0, nrow(C2))
C2$StError <- rep(0, nrow(C2))
C2$index <- seq(1,nrow(C2), by = 1)
dt <- merge(dt, C2, by = "Country2")

for(pu in 1:nrow(C2)){
    C3 <- subset(dt, index == pu, select=c(WDPA_PID, Country2, Mean_HF_09, impact, perc_4_th, Category, NAME))
    result <- t.test(C3$Mean_HF_09 ~ C3$Category)
    pval <- result$p.value
    sem<-sd(C3$Mean_HF_09)/sqrt(length(C3$Mean_HF_09))
    C2[pu,4] <- pval
    C2[pu,5] <- sem
}
 
write.csv(C2,file="CountriesHF09.csv",row.names = FALSE)
   
### for perc four threshold###########################################################

dt <- data.table(Strong3)

C1 <- data.frame(dt[,list(perc_4_th =mean(perc_4_th)),by=list(Country2,Category)])

C4 <- dcast(C1, Country2 ~ Category)

C4$Pvalue <- rep(0, nrow(C4))
C4$StError <- rep(0, nrow(C4))
C4$index <- seq(1,nrow(C4), by = 1)
dt <- merge(dt, C4, by = "Country2")

for(pu in 1:nrow(C4)){
  C5 <- subset(dt, index == pu, select=c(WDPA_PID, Country2, Mean_HF_09, impact, perc_4_th, Category, NAME))
  result <- t.test(C5$perc_4_th ~ C5$Category)
  pval <- result$p.value
  sem<-sd(C3$perc_4_th)/sqrt(length(C5$perc_4_th))
  C4[pu,4] <- pval
  C4[pu,5] <- sem
}

write.csv(C4,file="Countries4th.csv",row.names = FALSE)
    
### for impact ###########################################################
    
    dt <- data.table(Strong3)
    
    C1 <- data.frame(dt[,list(impact =mean(impact)),by=list(Country2,Category)])
    
    C6 <- dcast(C1, Country2 ~ Category)
    
    C6$Pvalue <- rep(0, nrow(C6))
    C6$StError <- rep(0, nrow(C6))
    C6$index <- seq(1,nrow(C6), by = 1)
    dt <- merge(dt, C6, by = "Country2")
    
    for(pu in 1:nrow(C6)){
      C7 <- subset(dt, index == pu, select=c(WDPA_PID, Country2, Mean_HF_09, impact, perc_4_th, Category, NAME))
      result <- t.test(C7$impact ~ C7$Category)
      pval <- result$p.value
      sem<-sd(C7$impact)/sqrt(length(C7$impact))
      C6[pu,4] <- pval
      C6[pu,5] <- sem
    }
    
    write.csv(C6,file="CountriesImpact.csv",row.names = FALSE)
    
######### if want just one country use this code###
    
    Argentina <- subset(dt, Country2 == "Argentina", select=c(WDPA_PID, Country2, Mean_HF_09, impact, perc_4_th, Category, NAME))
    write.csv(dt,file="Argentina.csv",row.names = FALSE)
    t.test(Argentina$Mean_HF_09 ~ Argentina$Category)
    sem<-sd(Argentina$Mean_HF_09)/sqrt(length(Argentina$Mean_HF_09))
    sem
    
###################### scatter plots #########

        
plot1 <- ggplot(C2, aes(WCS, WDPA))+
    geom_point(colour = 'darkgreen')+
    xlab("Average human footprint in WCS supported protected areas")+
    ylab("Average human footprint in Non-WCS supported protected areas")+
    xlim(0, 15)+
    ylim(0, 15)+
    geom_abline(intercept = 0, slope = 1, colour = 'black', linetype="dotted")+
    #geom_text(aes(label=Country2),hjust=0, vjust=0, size = 3)+
    geom_label_repel(aes(label = Country2),
                       box.padding   = 0.35,
                       point.padding = 0.5,
                       label.size = NA,
                       segment.color = 'grey50', size = 3)+
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                       axis.text=element_text(size=10),
                       axis.title=element_text(size=10))
    plot1
    
plot2 <- ggplot(C4, aes(WCS, WDPA))+
    geom_point(colour = 'orange')+
      xlab("% habitat converted in WCS supported protected areas")+
      ylab("% habitat converted in non-WCS supported protected areas")+
      xlim(0, 100)+
      ylim(0, 100)+
      geom_abline(intercept = 0, slope = 1, colour = 'black', linetype="dotted")+
  geom_label_repel(aes(label = Country2),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   label.size = NA,
                   segment.color = 'grey50', size = 3)+
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                         axis.text=element_text(size=10),
                         axis.title=element_text(size=10))
plot2
    
plot3 <- ggplot(C6, aes(WCS, WDPA))+
     geom_point(colour = 'purple')+
      xlab("% species impacted in WCS Supported protected areas (%)")+
      ylab("% species impacted in Non-WCS supported protected areas (%)")+
      xlim(20, 80)+
      ylim(20, 80)+
      geom_abline(intercept = 0, slope = 1, colour = 'black', linetype="dotted")+
  geom_label_repel(aes(label = Country2),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   label.size = NA,
                   segment.color = 'grey50', size = 3)+
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                         axis.text=element_text(size=10),
                         axis.title=element_text(size=10))
plot3
  
grid.arrange(plot1, plot2, plot3, nrow = 1)    

