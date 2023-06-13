
#Required packages
library(colorRamps) # Return functions that interpolate a set of given colors to create new color palettes
library(ggplus) #devtools::install_github("guiastrennec/ggplus")
library(XML) # for xmlTreeParse function
library(viridis)
library(hexbin) #for binning into hexagon cells
library(tidyverse) # data manipulation and analysis
library(car)

#Set working directory
setwd("write here the directory where your files are saved")

######################
##  LOAD XML FILES ##
######################

# create list of files
list.filenames<-list.files("/write here the directory where your files are saved", pattern='.xml')

#Create an empty data frame
df_xml<-data.frame()

#Load data
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

              #Bind the data frames in df 
              if (i==1) { df_xml<- xml_df} else { df_xml<-rbind (df_xml,xml_df)}
                           }

######################
##  LOAD DAT FILES ##
######################


# create list of files
list.dat.files<-list.files("/write here the directory where your files are saved", pattern='.dat')


#Create an empty data frame
df_dat<-data.frame()

#Load data
for (i in list.dat.files) {
        #Generate the data frame for each i
        dat.file <- read.table(i,header=T)
        #Add a colum with the ID
        dat.file$DATAFILE.text<-as.factor(i)
        #Bind all the data frames 
        if (i==1) { df_dat<- dat.file} else { df_dat<-rbind (df_dat,dat.file)}
}


#Join data frames
z<-full_join(df_dat,df_xml,by='DATAFILE.text')%>%
group_by(DATAFILE.text)

#Generate new columns with Group and Sex information
z <- z %>%
separate(FLY.text,c('Group','Sex'), sep='_')

#Center the data
z <- z %>%
  mutate(xcenter= x - as.numeric(as.character(ARENA_CENTER_X.text)), 
         ycenter= y - as.numeric(as.character(ARENA_CENTER_Y.text)),
         arena_r =  as.numeric(as.character(ARENA_RADIUS.text)),
         strip_angles = as.character(STRIPE_POS.text),
         d = sqrt((abs(xcenter)-0)^2 + (abs(ycenter)-0)^2))%>%
         filter(! d > arena_r) # Remove out of platform data
 
#Exctract data as csv
write.csv(z,file="write the group and orientation of the stimuli (e.g. PI_Training_3Sq_Bottom).csv", sep = ",", row.names = TRUE, col.names = TRUE)



################################
#####   OCCUPANCY PLOT  ########
################################


#Individual transition plots
# weights for smoothing
weights = c(21,16,4,1) #numeric vector of length 3 for relative weights of the center, the six neighbor cells, and twelve second neighbors.

#Data frame that will contain all the smbin data
df_smbin<-data.frame()

#To draw the platform
xx<-0
yy<-0
r<-1

pdf("Individual Transition Plots.pdf")

for (i in list.dat.files) {
  
                          zz<-filter(z,DATAFILE.text==i)
                          
                          x=c(zz$xcenter,r,-r)
                          y=c(zz$ycenter,r,-r)
                          
                          hbin = hexbin(x,y,xbins=60) #the number of bins partitioning the range of data
                          smbin = hsmooth(hbin,wts=weights) # generate smooth data to plot
                          
                          df<-data.frame(sapply(c("cell","count","xcm","ycm"), function(x) slot(hbin, x)))
                          df$ID<-i
                          
                          if (i==1) { df_smbin<- df} else { df_smbin<-rbind (df_smbin,df)}  
                          
                          
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

###########################
# Transition plot by Group  
###########################


z$Group<-as.factor(z$Group)
group_names = levels(z$Group)


pdf("Transition_plots by Group.pdf")

for (i in group_names) {
  
                          zzz<- z %>%
                          filter(Group==i) 
                          
                          x= c(zzz$xcenter,r,-r)
                          y= c(zzz$ycenter,r,-r)
                         
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



#####################################
# Transition plot by Group and Sex   
#####################################


z$Sex<-as.factor(z$Sex)
Sex=levels(z$Sex)

pdf("Transition_plots by Group and Sex.pdf")

for (i in group_names) {
  
  for(j in Sex) {
    
    zzz<- z %>%
      filter(Group==i,Sex==j) 
    
    x= c(zzz$xcenter,r,-r)
    y= c(zzz$ycenter,r,-r)
    
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
         main=paste("Transition plot",i,j),legend=FALSE,newpage=T)
    
  }}

dev.off()


##############################
#Quantification and statistics
##############################

df_smbin <- df_smbin %>%
separate(ID,c('Group','ID'), sep='_')

#Subtract specific data 
sup <- df_smbin  %>%
        filter(ycm>(100))%>%
        mutate("Arena"="Superior")

inf<- df_smbin %>%
      filter(ycm<(-100))%>%
      mutate("Arena"="Inferior")

data<-rbind(sup,inf)
data$Arena<-as.factor(data$Arena)
data$ID<-as.factor(data$ID)
data$Group<-as.factor(data$Group)

results <- data %>%
           group_by(ID,Group,Arena) %>%
           dplyr::summarise(sum= sum(count))

ID <- results %>%
      group_by(Group,Arena)%>%
      dplyr::summarise(n=n())

ggplot (results,aes(x=Arena , y=sum)) +
        scale_x_discrete() +
        geom_violin(mapping = NULL, data = NULL, stat = "ydensity",fill="grey50",
                    position = "dodge", draw_quantiles = T, trim = TRUE,colour='black',
                    scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,alpha=0.2)+
        geom_jitter(aes(colour = ID), 
                    position = position_jitter(width = .05), alpha = 0.5,size=2) +
        theme_minimal() +
        theme (axis.text.x=element_text(size=10), axis.text.y=element_text(size=10),axis.title.x= element_text(size=15),axis.title.y= element_text(size=15)) +
        facet_grid(.~Group)+
        labs(y= 'Occupancy',x='Arena', title='Boxplot - Mean Ocuppancy') +
        theme(legend.position="none")


##Statistical analysis by group

#CT Group

CT <- results%>%
      filter(Group=='CT')

#Test for homogeneity of variances
bartlett.test(sum ~ Arena, data = CT) #p-value < 0.05 we can assume equal variances
#Test for normality
qqPlot(CT$sum)
shapiro.test(CT$sum) #p-value > 0.05 we can assume normality
# if normal, Student's t-test
t.test(sum ~ Arena, data =CT,paired=T)
#if not normal, Wilcoxon test
boxplot(sum ~ Arena, data=CT)
wilcox.test(sum ~ Arena, data=CT,  paired = TRUE)


#TR Group

TR <- results%>%
      filter(Group=='TR')


#Test for homogeneity of variances
bartlett.test(sum ~ Arena, data = TR) #p-value < 0.05 we can assume equal variances
#Test for normality
qqPlot(TR$sum)
shapiro.test(TR$sum) #p-value > 0.05 we can assume normality
#If normal,Student's t-test
t.test(sum ~ Arena, data =TR,paired=T)
#If not normal,Wilcoxon test
str(TR)
boxplot(sum ~ Arena, data=TR)
wilcox.test(sum ~ Arena, data=TR,  paired = TRUE)


# Compare TR and CT Group

#First calculate the PI for each group
Difference <- results %>%
              spread(Arena, sum)%>%
              mutate(PI=(Superior-Inferior)/(Superior+Inferior))

#Boxplot
ggplot  (Difference,aes(x=Group , y=PI)) +
        scale_x_discrete() +
        geom_jitter(aes(colour = ID), 
                    position = position_jitter(width = .05), alpha = 0.8,size=3) +
        geom_boxplot(aes(colour = Group), outlier.colour = NA, position = 'dodge', alpha=0.4)+
        theme_classic()+
        ylim(-1,1)+
        theme (axis.text.x=element_text(size=10), axis.text.y=element_text(size=10),axis.title.x= element_text(size=15),axis.title.y= element_text(size=15)) +
        theme(legend.position="none")


#Statistical analysis

#Test for homogeneity of variances
bartlett.test(PI ~ Group, data = Difference) #p-value < 0.05 we can assume equal variances

#Test for normality
qqPlot(Difference$PI)
shapiro.test(Difference$PI) #p-value > 0.05 we can assume normality
#Wilcoxon test
str(Difference)
boxplot(PI ~ Group, data=Difference)
wilcox.test(PI ~ Group, data=Difference,  paired = F)


############################################################################################################################




