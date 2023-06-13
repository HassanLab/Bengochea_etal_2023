#Set working directory
setwd("write here the directory where your files are saved")

# Generate a data frame with all the .dat files in the folder
filenames <- list.files(pattern="*.dat")

dataset <- data.frame()
for (i in seq_along(filenames)) {

  d1 <- read.table(filenames[i], header=T)
  d1$DATAFILE.text<- as.factor(filenames[i])
  assign (paste("Rec", i, sep = "."),d1)          # a cada uno de los archivos le asigna un numero de registro
  if(i==1){ dataset <- d1 } else {dataset <- rbind(dataset,d1)}    # pega todos los data frame en un solo data frame
}


#Generate a data frame with all the .xml files
library(XML)
# Parse the XML file
xml_filenames <- list.files(pattern="*.xml")

xml_dataset<-data.frame()

for (i in xml_filenames) {
    xml.df <- xmlTreeParse(i)
    xmltop <- xmlRoot(xml.df)
    assign (paste("xml",i,sep="."),xml.df)
    xmltop <- xmlSApply(xmltop,
                    function(x) xmlSApply(x, xmlValue))
    #put the values in a dataframe
    xml_df <- data.frame(t(xmltop),row.names=NULL)
    if (i==1){xml_df <-xml_dataset} else {xml_dataset<-rbind(xml_dataset,xml_df)}
    }


# Join the two data frames
library(dplyr)
all.data<-full_join(dataset, xml_dataset, by = "DATAFILE.text")

str(all.data)

all.data$ARENA_CENTER_X.text<-as.numeric(as.character(all.data$ARENA_CENTER_X.text))
all.data$ARENA_CENTER_Y.text<-as.numeric(as.character(all.data$ARENA_CENTER_Y.text))


attach(all.data)

inverted.xy <- all.data %>%
mutate(inv.x=(ifelse( x>ARENA_CENTER_X.text, -abs(x-ARENA_CENTER_X.text) + ARENA_CENTER_X.text,
                      (ifelse( x<ARENA_CENTER_X.text, abs(ARENA_CENTER_X.text-x) + ARENA_CENTER_X.text,
                               x)))),
       inv.y=(ifelse( y>ARENA_CENTER_Y.text, -abs(y-ARENA_CENTER_Y.text) + ARENA_CENTER_Y.text,
                      (ifelse( y<ARENA_CENTER_Y.text, abs(ARENA_CENTER_Y.text-y) + ARENA_CENTER_Y.text,
                               y)))))
                      
customFun  = function(DF) {
  write.table(DF,paste0("",unique(DF$DATAFILE.text),""))
  return(DF)
}

data_inverted_xy <- inverted.xy %>%
  select(frame,time, inv.x, inv.y, burst, DATAFILE.text)%>%
  dplyr::rename(x=inv.x, y=inv.y)%>%
  group_by(DATAFILE.text)%>%
  do(customFun(.))

data_inverted_xy_split <- split.data.frame(data_inverted_xy, data_inverted_xy$DATAFILE.text)




