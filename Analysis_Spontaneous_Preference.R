
#Required packages
library(tidyverse)
library(colorRamps)
library(ggplus)
library(XML)
library(colorRamps)
library(viridis)
library(dplyr)
library(hexbin)
library(ggpubr)
library(patchwork)
library(rstatix)
library(stringr)
library(corrplot)


#Set working directory

setwd("Add here the directory to your data")


######################
##  LOAD XML FILES ##
######################

# create list of files
list.filenames<-list.files(".", pattern='.xml')


#Create an empty data frame

dff_xml<-data.frame()

for (i in list.filenames) {
  
  xmlfile <- xmlTreeParse(i)  
  
  #access the top node
  topxml <- xmlRoot(xmlfile)
  
  #To put the data in a data frame,you first need to extract the XML values. 
  #You can use the xmlSApply() function to do this:
  topxml <- xmlSApply(topxml,
                      function(x) xmlSApply(x, xmlValue))
  
  #Generate the data frame for each i
  xml_df <- data.frame(t(topxml),row.names=NULL)
  
  #Bind the data frames in dff 
  if (i==1) { dff_xml<- xml_df} else { dff_xml<-rbind (dff_xml,xml_df)}
}

######################
##  LOAD DAT FILES ##
######################


# create list of files
list.dat.files<-list.files(".", pattern='.dat')


#Create an empty data frame
dff_dat<-data.frame()


for (i in list.dat.files) {
  #Generate the data frame for each i
  dat.file <- read.table(i,header=T)
  #Add a colum with the ID
  dat.file$DATAFILE.text<-as.factor(i)
  #Bind all the data frames 
  if (i==1) { dff_dat<- dat.file} else { dff_dat<-rbind (dff_dat,dat.file)}
}


#Join the two data frames

z<-full_join(dff_dat,dff_xml,by='DATAFILE.text')%>%
  group_by(DATAFILE.text)


library(dplyr)
z$Sex<-z$FLY.text


#Center the data

z <- z %>%
  mutate(xcenter= x - as.numeric(as.character(ARENA_CENTER_X.text)), 
         ycenter= y - as.numeric(as.character(ARENA_CENTER_Y.text)),
         arena_r =  as.numeric(as.character(ARENA_RADIUS.text)),
         strip_angles = as.character(STRIPE_POS.text),
         d = sqrt((abs(xcenter)-0)^2 + (abs(ycenter)-0)^2))%>%
  	 filter(! d > arena_r) 

#Normalize arena to 1
R<-z %>% 
   mutate(xnorm=(xcenter/arena_r),
   ynorm=(ycenter/arena_r))


################################
#####  TRAJECTORY PLOT  ########
################################


#To draw the edge of the platform

xx<-0
yy<-0
r<-1

# Trajectory plots by ID 

gg1<-ggplot(R, aes(x=xnorm, y=ynorm)) +
  geom_count(aes(color = ..n..,size = ..n..))+
  scale_color_viridis(option = "D",limits=c(1, 1100),direction=-1)+
  coord_fixed()+
  theme_classic()+
  annotate("path",
           x=xx+r*cos(seq(0,2*pi,length.out=100)),
           y=yy+r*sin(seq(0,2*pi,length.out=100)),
           colour = "black", size = 1) 

gg1

#Save the plots in PDF file
#devtools::install_github("guiastrennec/ggplus")

library(ggplus)
pdf("individual trajectories_same scale_norm.pdf")
gg10 <- facet_multiple(plot=gg1, facets="DATAFILE.text", ncol = 1, nrow = 1, scales = "fixed")
dev.off()


#Plot Y position density plot

ypos <-  ggplot(R, aes(ynorm))+
	  geom_density(fill='gray')+
	  coord_flip()+
	  geom_vline(xintercept=0,linetype="dashed")+
	  labs(x= 'Y Position')+
	  theme_classic()+
	  theme(legend.position = 'none')+
	  theme(axis.text = element_text(color='black', size=14))
ypos       



################################
#####   OCCUPANCY PLOT  ########
################################


#Individual transition plot


# weights for smoothing
weights = c(21,16,4,1) #numeric vector of length 3 for relative weights of the center, the six neighbor cells, and twelve second neighbors.

#Data frame that will contain all the smbin data
dff_smbin<-data.frame()

pdf("Individual Transition Plots Norm.pdf")

for (i in list.dat.files) {
  zz<-filter(R,DATAFILE.text==i)
  
  x=c(zz$xnorm,r,-r)
  y=c(zz$ynorm,r,-r)
  
  hbin = hexbin(x,y,xbins=60) #the number of bins partitioning the range of data
  smbin = hsmooth(hbin,wts=weights) # generate smooth data to plot
  
  df<-data.frame(sapply(c("cell","count","xcm","ycm"), function(x) slot(hbin, x)))
  df$ID<-i
  
  if (i==1) { dff_smbin<- df} else { dff_smbin<-rbind (dff_smbin,df)}  
  
  
  
  #calculate the color-cuts for the plot
  maxval = quantile(smbin@count,0.95) # 95% quantil
  maxcut = maxval/max(smbin@count)
  cuts = seq(0,maxcut,length=255)
  cuts = c(cuts,1)
  
  #PLOT
  
  par(mfrow=c(2,3))
  plot(smbin, ,colorcut=cuts,colramp=blue2red,clip="off",
       main=paste("Transition plot",i),legend=FALSE,newpage=T)
  
  
}

dev.off()

###################
#Transition plot
###################

pdf("Transition_plot Norm.pdf")

x= c(R$xnorm,r,-r)
y= c(R$ynorm,r,-r)


hbin = hexbin(x,y,xbins=60,IDs=TRUE) #the number of bins partitioning the range of data, set IDs=TRUE to be able to go back to the original rows
smbin = hsmooth(hbin,wts=weights) # generate smooth data to plot

#get the bin number for each observation
smbin_count<-smbin@cID

#get the count of observations in the cell populated by a particular observation
smbin@count[match(smbin@cID, smbin@cell)]
library(plyr)
CountDF<-count(smbin@cID)


#calculate the color-cuts for the plot
maxval = quantile(smbin@count,0.95) # 95% quantil
maxcut = maxval/max(smbin@count)
cuts = seq(0,maxcut,length=255)
cuts = c(cuts,1)

#PLOT
par(mfrow=c(2,2))
plot(smbin, ,colorcut=cuts,colramp=blue2red,clip="off",
     main=paste("Transition plot"),legend=FALSE,newpage=T)


dev.off()



############################
# Transition plot by Sex    
############################


R$Sex<-as.factor(R$Sex)
Sex=levels(R$Sex)


pdf("Transition_plots_bySex.pdf")

for (i in Sex) {
  
  zzz<- R %>%
    filter(Sex==i) 
  
  
  x= c(zzz$xnorm,r,-r)
  y= c(zzz$ynorm,r,-r)
  
  hbin = hexbin(x,y,xbins=60,IDs=TRUE) #the number of bins partitioning the range of data, set IDs=TRUE to be able to go back to the original rows
  smbin = hsmooth(hbin,wts=weights) # generate smooth data to plot
  
  #get the bin number for each observation
  smbin_count<-smbin@cID
  
  #get the count of observations in the cell populated by a particular observation
  smbin@count[match(smbin@cID, smbin@cell)]
  library(plyr)
  CountDF<-count(smbin@cID)
  
  
  #calculate the color-cuts for the plot
  maxval = quantile(smbin@count,0.95) # 95% quantil
  maxcut = maxval/max(smbin@count)
  cuts = seq(0,maxcut,length=255)
  cuts = c(cuts,1)
  
  #PLOT
  
  par(mfrow=c(2,2))
  plot(smbin, ,colorcut=cuts,colramp=blue2red,clip="off",
       main=paste("Transition plot",i),legend=FALSE,newpage=T)
  
}

dev.off()


#Plot y position over time for each Sex

Sex <-   ggplot(R, aes(ynorm,color=Sex))+
	  geom_density(alpha = 0.3, aes(fill=Sex))+
	  facet_wrap(~Sex,scale = "free_x")+
	  theme_classic()+
	  coord_flip()+
	  theme(legend.position = 'none')+
	  geom_vline(xintercept=0,linetype="dashed")+
	  theme(axis.text = element_text(color='black', size=14))
Sex

ggsave(Sex,filename='SexDensity.pdf')


##############################################################################################################################


# Substract specific data for statistical analysis

sup <-    dff_smbin  %>%
	  filter(ycm>(0.5))%>%
	  mutate("Arena"="Top")

inf <-  dff_smbin %>%
  	filter(ycm<(-0.5))%>%
 	mutate("Arena"="Bottom")

data<-rbind(sup,inf)
data$Arena<-as.factor(data$Arena)
data$ID<-as.factor(data$ID)


results <-  data %>%
	    group_by(ID,Arena) %>%
	    dplyr::summarise(sum= sum(count)) %>% 
	    mutate(sum2=sum/1000)

ID <-   results %>%
	group_by(Arena)%>%
	dplyr::summarise(n=n())



#Check normality for paired samples

# compute the difference
d <- with(results, 
          sum [Arena == "Bottom"] - sum[Arena == "Top"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # =>if the p-value is greater than the significance level 0.05 we can assume the normality.

#t test
t.test(sum ~ Arena, data=results,paired=TRUE)


# To calculate the difference between the two areas of the platform - PREFERENCE INDEX - 

Difference<- results %>%
	     spread(Arena,sum)%>%
	     group_by(ID)%>%
	     dplyr::mutate(PI=(Top-Bottom)/(Top + Bottom)) 


#Save data
#Extract data as csv
write.csv(Difference,file="PI.csv", sep = ",", row.names = TRUE, col.names = TRUE)



#Plot population boxplot

boxplot <- ggboxplot(Difference, y='PI',  add.params = list(size=3,alpha=0.3),
                    add='jitter')+ 
  ylim(-1,1)+
  geom_hline(yintercept=0, linetype="dashed",color='black')+
  theme(legend.position = 'none')+
  theme_classic()+
  theme(legend.position = 'none')+
  stat_summary(fun.data = function(x) data.frame(y=-0.5, label = paste("Mean=",mean(x), "+/-", sd=sd(x))), geom="text") +
  theme(axis.text = element_text(color='black', size=14),
        axis.title = element_text(color='black',size=16))

boxplot

Fig1 <- ypos + boxplot
Fig1
ggsave(Fig1,filename = "YPos_PI.pdf",w=4,h=4)


#Statistical one sample test (PI different from 0)

#Assumptions
shapiro.test(Difference$PI) # if p-value is greater than the significance level 0.05 then we can assume the normality.
ggqqplot(Difference$PI) # Check normal distribution
t.test(Difference$PI)


stat.test <- wilcox.test(Difference$PI) %>% add_significance()
stat.test







