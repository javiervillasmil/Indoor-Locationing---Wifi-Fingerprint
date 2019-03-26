
libraries_function <- function(){
  if(require("pacman")=="FALSE"){
    install.packages('pacman')
    library('pacman')
    pacman::p_install(tidyverse,lubridate,dplyr,anytime,plyr,plotly,cowplot,reshape2,expss,caret,lattice,ggplot2,rpart,RColorBrewer,randomForest,data.table,liquidSVM,DMwR,kknn,C50,e1071,doParallel)
    pacman::p_update(tidyverse,lubridate,dplyr,anytime,plyr,plotly,cowplot,reshape2,expss,caret,lattice,ggplot2,rpart,RColorBrewer,randomForest,data.table,liquidSVM,DMwR,kknn,C50,e1071,doParallel)
  }
}







#import the fingerprint WiFi dataset
wifi <- read.csv("C:/Users/Javier Villasmil/Desktop/Ubiqum/Task 010 - WiFi Location/UJIndoorLoc/trainingData.csv")

test <- read.csv("C:/Users/Javier Villasmil/Desktop/Ubiqum/Task 010 - WiFi Location/UJIndoorLoc/validationData.csv")

#First look to the train dataset
str(wifi)
summary(wifi)
attributes(wifi)
#First look to the test dataset
str(test)
summary(test)

#Converting BUILDINGID into factor with three (3) Levels
wifi$BUILDINGID <- as.factor(wifi$BUILDINGID)

test$BUILDINGID <- as.factor(test$BUILDINGID)

#Renaming BUILDINGID factor with Location Name
wifi$BUILDINGID <- revalue(wifi$BUILDINGID, c("0" = "TI", 
                                              "1" = "TD", 
                                              "2" = "TC"))

test$BUILDINGID <- revalue(test$BUILDINGID, c("0" = "TI", 
                                              "1" = "TD", 
                                              "2" = "TC"))
#Converting FLOOR into factor with five (5) Levels
wifi$FLOOR <- as.factor(wifi$FLOOR)

test$FLOOR <- as.factor(test$FLOOR)
#Renaming FLOOR factor with Location Name
wifi$FLOOR <- revalue(wifi$FLOOR, c("0" = "1st Floor", 
                                    "1" = "2nd Floor", 
                                    "2" = "3rd Floor",
                                    "3" = "4th Floor",
                                    "4" = "5th Floor"))

test$FLOOR <- revalue(test$FLOOR, c("0" = "1st Floor", 
                                    "1" = "2nd Floor", 
                                    "2" = "3rd Floor",
                                    "3" = "4th Floor",
                                    "4" = "5th Floor"))

levels(wifi$BUILDINGID)
levels(wifi$FLOOR)

levels(test$BUILDINGID)
levels(test$FLOOR)

#Removing repeated rows
wifi <- wifi[!duplicated(wifi),]
test <- test[!duplicated(test),]

#Converting UNIX TIMESTAMP to DateTime
wifi$DateTime <-anytime(wifi$TIMESTAMP, tz = "CET")
class(wifi$DateTime)

test$DateTime <-anytime(test$TIMESTAMP, tz = "CET")
class(test$DateTime)

#pasting two columns together
pasted <- within(wifi, x <- paste(BUILDINGID,FLOOR,sep='_'))
wifi$bf <- pasted$x

pasted2 <- within(test, x <- paste(BUILDINGID,FLOOR,sep='_'))
test$bf <- pasted2$x

#detecting Access Points with no measurements (columns)
colindex <- c()
for (i in 1:520){
  if (all(wifi[,i] == 100)){
    colindex = c(colindex,i)
    }
}

colindextest <- c()
for (i in 1:520){
  if (all(test[,i] == 100)){
    colindextest = c(colindextest,i)
  }
}
#list of access points with no measurements
colindex
colindextest

#check any AP's without values in both datasets 
which(colindextest %in% colindex) 

#list of access points with no measurements for both datasets
colremove <- c(colindex,colindextest)

#remove the columns by index in both datasets
wifi <- wifi[,-colremove]
test <- test[,-colremove]

#changing values from 100 (no measurement) to -105dBm (low Signal)
head(wifi)
head(test)
wifi[,1:312] <- as.data.frame(apply(wifi[,1:312], 2, function(x) {ifelse(x == 100,-105,x)}))
test[,1:312] <- as.data.frame(apply(test[,1:312], 2, function(x) {ifelse(x == 100,-105,x)}))
head(wifi)
head(test)

# Remove registers (rows) where all the values (signals) are low
wifi$new <- apply(wifi[,1:312], 1, function(x) {sum(x)})
rowremovewifi  <- which(wifi$new == (312*(-105)))
wifi <- wifi[-rowremovewifi,]
wifi$new <- NULL

test$new <- apply(test[,1:312], 1, function(x) {sum(x)})
rowremovetest <- which(test$new == (312*(-105))) #empty
test$new <- NULL

# A cleaner code to remove los signal registers

#keepwifi <- apply(wifi[,1:312], 1, function(x) length(unique(x[!is.na(x))) != 1) #<-HELP READING THIS ONE 
#wifi <- wifi[keepwifi, ] 
#
#keeptest <- apply(test[,1:312], 1, function(x) length(unique(x[!is.na(x)])) != 1) #<-HELP READING THIS ONE 
#test[keeptest,]

#####################################################################################

#Checking variance in columns
wifizerovar <- nearZeroVar(wifi, saveMetrics = TRUE)
#order WAP's by decreasing FreqRatio
wifizerovar <- wifizerovar[order(wifizerovar$freqRatio, decreasing = TRUE ),]
# WAP's to remove - 10.000 is the freqRatio choosed as a threeshold
nozerocolumns <- rownames(wifizerovar[which(wifizerovar$freqRatio > 10000),])
#checks that the index of the columns corresponds to the WAP that we want to remove
colnames(wifi)[which(names(wifi) %in% nozerocolumns)]

#removes the WAP with "NearZeroVar" by index.
#wifi <- wifi[,-which(names(wifi) %in% nozerocolumns)] - Decided not to remove these columns because the values can be interesting for fingerprints

#same procedure agsint the test set
#colnames(test)[which(names(test) %in% nozerocolumns)] - Decided not to remove these columns because the values can be interesting for fingerprints
#test <- test[,-which(names(test) %in% nozerocolumns)]

######################################################################################

#compare the two datasets WIFI and TEST to check if both are equal (column wise)
columncompar <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Names are identical')
    }
  }
}

columncompar(wifi,test) #check!


######################################################################################
WAPS<-grep("WAP", names(wifi), value=T) #value = TRUE returns the values instead of the vectors

#creating a column with the highest WAP and highest Signal detected.
wifi <- wifi %>% 
  mutate(High_WAP=NA, High_RSSI=NA)

test <- test %>% 
  mutate(High_WAP=NA, High_RSSI=NA)

#gets which columns has the highest value for a fingerprint
wifi<-wifi %>% 
  mutate(High_WAP=colnames(wifi[WAPS])[apply(wifi[WAPS],1,which.max)])

test<-test %>% 
  mutate(High_WAP=colnames(test[WAPS])[apply(test[WAPS],1,which.max)])

wifi$High_WAP <- as.factor(wifi$High_WAP)
test$High_WAP <- as.factor(test$High_WAP)


#gets which is the highest value for a fingerprint
wifi<-wifi %>% 
  mutate(High_RSSI=apply(wifi[WAPS], 1, max))

test<-test %>% 
  mutate(High_RSSI=apply(test[WAPS], 1, max))

wifi$High_RSSI
test$High_RSSI


# Are there same HighWAP in different Buildings?
WAPS_Recoloc<- wifi %>%
  select(High_WAP, BUILDINGID) %>%
  distinct(High_WAP, BUILDINGID)

RepWAPS<-sort(WAPS_Recoloc$High_WAP[duplicated(WAPS_Recoloc$High_WAP)]) 
RepWAPS       # WAP 248 is highwap in the 3 buildings!!

# Examine WAP 248
WAP248<-wifi[wifi$High_WAP=="WAP248",313:324]
plot(LATITUDE ~ LONGITUDE, data = wifi, pch = 20, col = "grey")
points(LATITUDE ~ LONGITUDE, data=WAP248, pch=20, col="blue")

wifi %>%
  group_by(High_WAP)


###############################################
#################### PLOTS ####################
#################### PLOTS ####################
#################### PLOTS #################### 
#################### PLOTS ####################
###############################################

############# NUMBER OF SIGNALS BY ACCESS POINT ###########

#Creates a column with the count of Number of Signals per WAP
wifi$WAP_num <- apply(wifi[,1:312], 1, function (x) length(x[x != -105]))
test$WAP_num <- apply(test[,1:312], 1, function (x) length(x[x != -105]))
#creeates a bar plot using ggplot with the distribution of recieved signal by WAP.
ggplot() + geom_bar(data = wifi, aes(x = WAP_num),color="red") + 
           geom_bar(data = test, aes(x = WAP_num),color="yellow")

############# DISTRIBUTION OF MEASUREMENTS BY ACCESS POINT ###########

#applies a fuction that counts valuves different that -105 (non-empty), then creates a data.frame
freq <- as.data.frame(apply(wifi[,1:312], 2, function(x) {sum(x != -105)}))
#renames the column
colnames(freq) <- c("FREQUENCY")
#creates a new column with factors for each Access Point (WAP)
freq[ "WAP" ] <- rownames(freq)
#reorder de columns
freq <- freq[,c(2,1)]
#reorder de rows so the most frequent Access Point in on top
freq <- freq[order(freq$FREQUENCY, decreasing = TRUE ),]

#applies a fuction that counts valuves different that -105 (non-empty), then creates a data.frame
freq2 <- as.data.frame(apply(test[,1:312], 2, function(x) {sum(x != -105)}))
#renames the column
colnames(freq2) <- c("FREQUENCY")
#creates a new column with factors for each Access Point (WAP)
freq2[ "WAP" ] <- rownames(freq2)
#reorder de columns
freq2 <- freq2[,c(2,1)]
#reorder de rows so the most frequent Access Point in on top
freq2 <- freq2[order(freq2$FREQUENCY, decreasing = TRUE ),]

#creeates a bar plot using ggplot, for the stats = "identity" meaning that the plot is taking the values of each row instead the count.
hh <- ggplot() + geom_bar(data = freq, aes(x = reorder(WAP,-FREQUENCY) , y = FREQUENCY, color=FREQUENCY), stat="identity") + coord_flip() + theme(axis.text.y = element_text(size = 1))
ii <- ggplot() + geom_bar(data = freq2, aes(x = reorder(WAP,-FREQUENCY) , y = FREQUENCY, color=FREQUENCY), stat="identity") + coord_flip()
  
plot_grid(hh,ii)

############# DISTRIBUTION OF SIGNALS BY OVERALL ###########
#Train
wifiRSSI <- wifi[,1:312]
wifiRSSI <- melt(wifiRSSI)
wifiRSSI <- wifiRSSI %>%
  filter(value != -105)

#validation
testRSSI <- test[,1:312]
testRSSI <- melt(testRSSI)
testRSSI <- testRSSI %>%
  filter(value != -105)

ll <- ggplot() + geom_histogram(data = wifiRSSI, aes(x = value),binwidth = 1,colour="black",fill="white") + xlab("RSSI - TRAIN") + xlim(-105, 0)
  
ww <- ggplot() + geom_histogram(data = testRSSI, aes(x = value),binwidth = 1,colour="black",fill="lightblue") + xlab("RSSI - VALIDATION") + ylab("") + xlim(-105, 0)

plot_grid(ll,ww)

#Plotly
#######

colnames(wifi)
#FONTS FOR PLOTLY
font1 <- list(
  family = "Arial, sans-serif",
  size = 18,
  color = "grey50")
#FONTS FOR PLOTLY
font2 <- list(
  family = "Arial, sans-serif",
  size = 14,
  color = "Black")  

#Wifi Measurements - TRAIN SET
p1 <- plot_ly(wifi, x= ~LONGITUDE,
              y= ~LATITUDE,
              z= ~FLOOR, 
              type="scatter3d", 
              mode="markers",
              color = ~BUILDINGID, colors = c('#BF382A', '#0C4B8E',"#0c8e0c")) %>%
  layout(title = 'Wifi Measurements - TRAIN SET',
         scene = list(
                      xaxis = list(title = 'LONGITUDE',
                                   titlefont = font1,
                                   tickangle = 180,
                                   gridcolor = 'grey50',
                                   zerolinewidth = 1,
                                   range = c(-7692, -7301),
                                   zeroline = FALSE,
                                   showline = FALSE,
                                   showticklabels = FALSE),
                      yaxis = list(title = 'LATITUDE',
                                   titlefont = font1,
                                   tickangle = 180,
                                   gridcolor = 'grey50',
                                   zerolinewidth = 1,
                                   range = c(4864747,4865018),
                                   zeroline = FALSE,
                                   showline = FALSE,
                                   showticklabels = FALSE),
                      zaxis = list(title = '',
                                   tickfont = font2,
                                   gridcolor = 'grey50',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwith = 2,
                                   categoryorder = "array",
                                   categoryarray = c("1st Floor", 
                                                     "2nd Floor", 
                                                     "3rd Floor",
                                                     "4th Floor",
                                                     "5th Floor"))),
        
          paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)')

#Wifi Measurements - TEST SET
p2 <- plot_ly(test, x= ~LONGITUDE,
              y= ~LATITUDE,
              z= ~FLOOR, 
              type="scatter3d", 
              mode="markers",
              color = ~BUILDINGID, colors = c('#BF382A', '#0C4B8E',"#0c8e0c")) %>%
  layout(title = 'Wifi Measurements - TEST SET',
         scene = list(
           xaxis = list(title = 'LONGITUDE',
                        titlefont = font1,
                        tickangle = 180,
                        gridcolor = 'grey50',
                        zerolinewidth = 1,
                        range = c(-7692, -7301),
                        zeroline = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE),
           yaxis = list(title = 'LATITUDE',
                        titlefont = font1,
                        tickangle = 180,
                        gridcolor = 'grey50',
                        zerolinewidth = 1,
                        range = c(4864747,4865018),
                        zeroline = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE),
           zaxis = list(title = '',
                        tickfont = font2,
                        gridcolor = 'grey50',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwith = 2,
                        categoryorder = "array",
                        categoryarray = c("1st Floor", 
                                          "2nd Floor", 
                                          "3rd Floor",
                                          "4th Floor",
                                          "5th Floor"))),
         
         paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)')
#
#
#
p1
p2
#
#
#
levels(wifi$BUILDINGID)
levels(wifi$FLOOR)

#Subsets for each building and floor

####### BUILDING 0 - TI ##################################################################################

#TRAIN SET
wifi_b0 <- wifi %>%
  filter(BUILDINGID == "TI")

    #BUILDING 0 - FLOOR 1
    wifi_b0_1 <- wifi_b0 %>%
      filter(FLOOR == "1st Floor")
    #BUILDING 0 - FLOOR 2
    wifi_b0_2 <- wifi_b0 %>%
      filter(FLOOR == "2nd Floor")
    #BUILDING 0 - FLOOR 3
    wifi_b0_3 <- wifi_b0 %>%
      filter(FLOOR == "3rd Floor")
    #BUILDING 0 - FLOOR 4
    wifi_b0_4 <- wifi_b0 %>%
      filter(FLOOR == "4th Floor")
    
#TEST SET
test_b0 <- test %>%
  filter(BUILDINGID == "TI")
    
    #BUILDING 0 - FLOOR 1
    test_b0_1 <- test_b0 %>%
      filter(FLOOR == "1st Floor")
    #BUILDING 0 - FLOOR 2
    test_b0_2 <- test_b0 %>%
      filter(FLOOR == "2nd Floor")
    #BUILDING 0 - FLOOR 3
    test_b0_3 <- test_b0 %>%
      filter(FLOOR == "3rd Floor")
    #BUILDING 0 - FLOOR 4
    test_b0_4 <- test_b0 %>%
      filter(FLOOR == "4th Floor")
    
  
####### BUILDING 1 -TD ##################################################################################
#TRAIN SET
wifi_b1 <- wifi %>%
  filter(BUILDINGID == "TD")
    
    #BUILDING 1 - FLOOR 1
    wifi_b1_1 <- wifi_b1 %>%
      filter(FLOOR == "1st Floor")
    #BUILDING 1 - FLOOR 2
    wifi_b1_2 <- wifi_b1 %>%
      filter(FLOOR == "2nd Floor")
    #BUILDING 1 - FLOOR 3
    wifi_b1_3 <- wifi_b1 %>%
      filter(FLOOR == "3rd Floor")
    #BUILDING 1 - FLOOR 4
    wifi_b1_4 <- wifi_b1 %>%
      filter(FLOOR == "4th Floor")
    
#TEST SET
test_b1 <- test %>%
  filter(BUILDINGID == "TD")
    
    #BUILDING 1 - FLOOR 1
    test_b1_1 <- test_b1 %>%
      filter(FLOOR == "1st Floor")
    #BUILDING 1 - FLOOR 2
    test_b1_2 <- test_b1 %>%
      filter(FLOOR == "2nd Floor")
    #BUILDING 1 - FLOOR 3
    test_b1_3 <- test_b1 %>%
      filter(FLOOR == "3rd Floor")
    #BUILDING 1 - FLOOR 4
    test_b1_4 <- test_b1 %>%
      filter(FLOOR == "4th Floor")
    
####### BUILDING 2 -TC ##################################################################################
#TRAIN SET
wifi_b2 <- wifi %>%
  filter(BUILDINGID == "TC")
    
    #BUILDING 2 - FLOOR 1
    wifi_b2_1 <- wifi_b2 %>%
      filter(FLOOR == "1st Floor")
    #BUILDING 2 - FLOOR 2
    wifi_b2_2 <- wifi_b2 %>%
      filter(FLOOR == "2nd Floor")
    #BUILDING 2 - FLOOR 3
    wifi_b2_3 <- wifi_b2 %>%
      filter(FLOOR == "3rd Floor")
    #BUILDING 2 - FLOOR 4
    wifi_b2_4 <- wifi_b2 %>%
      filter(FLOOR == "4th Floor")
    #BUILDING 2 - FLOOR 5
    wifi_b2_5 <- wifi_b2 %>%
      filter(FLOOR == "5th Floor")   
    
#TEST SET
test_b2 <- test %>%
  filter(BUILDINGID == "TC")
    
    #BUILDING 2 - FLOOR 1
    test_b2_1 <- test_b2 %>%
      filter(FLOOR == "1st Floor")
    #BUILDING 2 - FLOOR 2
    test_b2_2 <- test_b2 %>%
      filter(FLOOR == "2nd Floor")
    #BUILDING 2 - FLOOR 3
    test_b2_3 <- test_b2 %>%
      filter(FLOOR == "3rd Floor")
    #BUILDING 2 - FLOOR 4
    test_b2_4 <- test_b2 %>%
      filter(FLOOR == "4th Floor")
    #BUILDING 2 - FLOOR 5
    test_b2_5 <- test_b2 %>%
      filter(FLOOR == "5th Floor")  
    
#####################################################################################
    
###############################################
#################### PLOTS #################### 
#################### PLOTS ####################
#################### PLOTS #################### #More Plots!
#################### PLOTS ####################
###############################################

# BUILDING 0 - TI    
p3 <- ggplot() + 
  geom_point(data=wifi_b0_1, aes(x = LONGITUDE, y = LATITUDE,color = "red"),size = 2,shape = 4) + 
  geom_point(data=test_b0_1, aes(x = LONGITUDE, y = LATITUDE,color = "black"),size = 2,shape = 1) +
  xlab("LONGITUDE") + ylab("LATITUDE") + ggtitle("BUILDING TI - FLOOR 1") + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = '', values =c('red'='red','black'='black'), labels = c('Wifi Validation Set','WiFi Train Set'))
  
    
p4 <- ggplot() + 
  geom_point(data=wifi_b0_2, aes(x = LONGITUDE, y = LATITUDE,color = "red"),size = 2,shape = 4) + 
  geom_point(data=test_b0_2, aes(x = LONGITUDE, y = LATITUDE,color = "black"),size = 2,shape = 1) +
  xlab("LONGITUDE") + ylab("LATITUDE") + ggtitle("BUILDING TI - FLOOR 2") + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = '', values =c('red'='red','black'='black'), labels = c('Wifi Validation Set','WiFi Train Set'))

p5 <- ggplot() + 
  geom_point(data=wifi_b0_3, aes(x = LONGITUDE, y = LATITUDE,color = "red"),size = 2,shape = 4) + 
  geom_point(data=test_b0_3, aes(x = LONGITUDE, y = LATITUDE,color = "black"),size = 2,shape = 1) +
  xlab("LONGITUDE") + ylab("LATITUDE") + ggtitle("BUILDING TI - FLOOR 3") + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = '', values =c('red'='red','black'='black'), labels = c('Wifi Validation Set','WiFi Train Set'))

p6 <- ggplot() + 
  geom_point(data=wifi_b0_4, aes(x = LONGITUDE, y = LATITUDE,color = "red"),size = 2,shape = 4) + 
  geom_point(data=test_b0_4, aes(x = LONGITUDE, y = LATITUDE,color = "black"),size = 2,shape = 1) +
  xlab("LONGITUDE") + ylab("LATITUDE") + ggtitle("BUILDING TI - FLOOR 4") + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = '', values =c('red'='red','black'='black'), labels = c('Wifi Validation Set','WiFi Train Set'))

p3_6 <- plot_grid(p3, p4, p5, p6)

# BUILDING 1 - TD
p7 <- ggplot() + 
  geom_point(data=wifi_b1_1, aes(x = LONGITUDE, y = LATITUDE,color = "blue"),size = 2,shape = 4) + 
  geom_point(data=test_b1_1, aes(x = LONGITUDE, y = LATITUDE,color = "black"),size = 2,shape = 1) +
  xlab("LONGITUDE") + ylab("LATITUDE") + ggtitle("BUILDING TD - FLOOR 1") + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = '', values =c('blue'='blue','black'='black'), labels = c('Wifi Validation Set','WiFi Train Set'))

p8 <- ggplot() + 
  geom_point(data=wifi_b1_2, aes(x = LONGITUDE, y = LATITUDE,color = "blue"),size = 2,shape = 4) + 
  geom_point(data=test_b1_2, aes(x = LONGITUDE, y = LATITUDE,color = "black"),size = 2,shape = 1) +
  xlab("LONGITUDE") + ylab("LATITUDE") + ggtitle("BUILDING TD - FLOOR 2") + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = '', values =c('blue'='blue','black'='black'), labels = c('Wifi Validation Set','WiFi Train Set'))

p9 <- ggplot() + 
  geom_point(data=wifi_b1_3, aes(x = LONGITUDE, y = LATITUDE,color = "blue"),size = 2,shape = 4) + 
  geom_point(data=test_b1_3, aes(x = LONGITUDE, y = LATITUDE,color = "black"),size = 2,shape = 1) +
  xlab("LONGITUDE") + ylab("LATITUDE") + ggtitle("BUILDING TD - FLOOR 3") + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = '', values =c('blue'='blue','black'='black'), labels = c('Wifi Validation Set','WiFi Train Set'))

p10 <- ggplot() + 
  geom_point(data=wifi_b1_4, aes(x = LONGITUDE, y = LATITUDE,color = "blue"),size = 2,shape = 4) + 
  geom_point(data=test_b1_4, aes(x = LONGITUDE, y = LATITUDE,color = "black"),size = 2,shape = 1) +
  xlab("LONGITUDE") + ylab("LATITUDE") + ggtitle("BUILDING TD - FLOOR 4") + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = '', values =c('blue'='blue','black'='black'), labels = c('Wifi Validation Set','WiFi Train Set'))

p7_10 <- plot_grid(p7, p8, p9, p10)

# BUILDING 2 - TC
p11 <- ggplot() + 
  geom_point(data=wifi_b2_1, aes(x = LONGITUDE, y = LATITUDE,color = "dark green"),size = 2,shape = 4) + 
  geom_point(data=test_b2_1, aes(x = LONGITUDE, y = LATITUDE,color = "black"),size = 2,shape = 1) +
  xlab("LONGITUDE") + ylab("LATITUDE") + ggtitle("BUILDING TC - FLOOR 1") + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = '', values =c('dark green'='dark green','black'='black'), labels = c('Wifi Validation Set','WiFi Train Set'))

p12 <- ggplot() + 
  geom_point(data=wifi_b2_2, aes(x = LONGITUDE, y = LATITUDE,color = "dark green"),size = 2,shape = 4) + 
  geom_point(data=test_b2_2, aes(x = LONGITUDE, y = LATITUDE,color = "black"),size = 2,shape = 1) +
  xlab("LONGITUDE") + ylab("LATITUDE") + ggtitle("BUILDING TC - FLOOR 2") + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = '', values =c('dark green'='dark green','black'='black'), labels = c('Wifi Validation Set','WiFi Train Set'))

p13 <- ggplot() + 
  geom_point(data=wifi_b2_3, aes(x = LONGITUDE, y = LATITUDE, color = "dark green"),size = 2,shape = 4) + 
  geom_point(data=test_b2_3, aes(x = LONGITUDE, y = LATITUDE,color = "black"),size = 2,shape = 1) +
  xlab("LONGITUDE") + ylab("LATITUDE") + ggtitle("BUILDING TC - FLOOR 3") + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = '', values =c('dark green'='dark green','black'='black'), labels = c('Wifi Validation Set','WiFi Train Set'))

p14 <- ggplot() + 
  geom_point(data=wifi_b2_4, aes(x = LONGITUDE, y = LATITUDE,color = "dark green"),size = 2,shape = 4) + 
  geom_point(data=test_b2_4, aes(x = LONGITUDE, y = LATITUDE,color = "black"),size = 2,shape = 1) +
  xlab("LONGITUDE") + ylab("LATITUDE") + ggtitle("BUILDING TC - FLOOR 4") + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = '', values =c('dark green'='dark green','black'='black'), labels = c('Wifi Validation Set','WiFi Train Set'))

p15 <- ggplot() + 
  geom_point(data=wifi_b2_5, aes(x = LONGITUDE, y = LATITUDE,color = "dark green"),size = 2,shape = 4) + 
  geom_point(data=test_b2_5, aes(x = LONGITUDE, y = LATITUDE,color = "black"),size = 2,shape = 1) +
  xlab("LONGITUDE") + ylab("LATITUDE") + ggtitle("BUILDING TC - FLOOR 5") + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = '', values =c('dark green'='dark green','black'='black'), labels = c('Wifi Validation Set','WiFi Train Set'))


p11_15 <- plot_grid(p11, p12, p13, p14, p15)

######################################################################## 
plot_grid(p3,p4,p5,p6,p7,p8,p9,p10,p11, p12, p13, p14, p15, cols=4)
########################################################################

###################### DISTRIBUTION OF POINTS BY BUILDING ##############
p16 <- ggplot(wifi, aes(x = BUILDINGID, fill = wifi$FLOOR)) + 
  geom_bar(position = "dodge", width = 0.8) +
  scale_fill_brewer(type = "qual", palette = "Dark2", name="") + xlab("") + ylab("FREQUENCY") +
  scale_x_discrete(labels=c("TI" = "TI\nBUILDING 0","TD" = "TD\nBUILDING 1","TC" = "TC\nBUILDING 2"))


p17 <- ggplot(wifi, aes(x = BUILDINGID,fill = BUILDINGID)) + 
  geom_bar(position = "dodge", width = 0.8) + xlab("") + ylab("FREQUENCY") +
  scale_x_discrete(labels=c("TI" = "TI\nBUILDING 0","TD" = "TD\nBUILDING 1","TC" = "TC\nBUILDING 2")) +
  scale_fill_manual(values=c("red", "blue", "dark green"))

plot_grid(p17,p16)


#Check longitude and latitude range
max(wifi$LONGITUDE) - min(wifi$LONGITUDE)
max(wifi$LATITUDE) - min(wifi$LATITUDE)

l <- ggplot()+geom_boxplot(aes(y=wifi$LATITUDE),outlier.colour="red", outlier.shape=8,outlier.size=4)
l2 <- ggplot()+geom_boxplot(aes(y=wifi$LONGITUDE),outlier.colour="red", outlier.shape=8,outlier.size=4) 

plot_grid(l,l2)
######################################################################

# BUILDINGID PREDICTION

# Prepare Parallel Process
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)


#apply a random forest to predict bulding - 148sec
# Accuracy     Kappa 
#0.9972997 0.9957309 
system.time(rf <- randomForest(BUILDINGID ~ .,data = wifi[,c(1:312,316)],  ntree = 100, do.trace = TRUE, importance = TRUE))
prediction1 <- predict(rf,test)
performance1 <- postResample(prediction1,test$BUILDINGID)
confusionMatrix(prediction1, test$BUILDINGID)

#apply a SVM to predict bulding - 48sec
# Accuracy     Kappa 
#0.9990999 0.9985778
system.time(svm_predict_building <- train(BUILDINGID~., data = wifi[,c(1:312,316)], method = "svmLinear", trControl = trainControl(verboseIter = TRUE)))
building_prediction <- predict(svm_predict_building,test)
performance2 <- postResample(building_prediction,test$BUILDINGID)
building_confusion_matrix <- confusionMatrix(building_prediction, test$BUILDINGID)

#apply a KNN to predict bulding - 64seg
#Accuracy     Kappa 
#0.9936994 0.9900587 
system.time(knn_predict_building <- train.kknn(BUILDINGID~.,data =  wifi[,c(1:312,316)],ks = c(3,5,7,9),  kmax = 9,scale = FALSE, kernel = 'optimal'))
prediction3 <- predict(knn_predict_building,test)
performance3 <- postResample(prediction3,test$BUILDINGID)
confusionMatrix(prediction3, test$BUILDINGID)

###################################################################
#MODEL SELECTED SVM WITH .99 ACURRACY AND KAPPA TO PREDICT BUILDING
save(svm_predict_building, file = "svm_predict_building.rda")
###################################################################


#CREATING DATASETS FOR FLOOR PREDICTION
###################################################################

Test_pBuilding <- test
#replaces my the buildingID predictions
Test_pBuilding$BUILDINGID <- prediction2
#check if the overwriting is correct
test$BUILDINGID[which(!(Test_pBuilding$BUILDINGID == test$BUILDINGID))] #real value is a TI
Test_pBuilding$BUILDINGID[which(!(Test_pBuilding$BUILDINGID == test$BUILDINGID))] #predicted value is a TD

#creates a new training dataset with the bulding and floor included
Model_Building_Floor <- wifi[,c(1:312,315,316)]

Model_Building_Latitude <- wifi[,c(1:312,316,314)]

#creates a new test dataset with the bulding (predicted) and floor included
test_b0_pBuilding <- Test_pBuilding %>%
  filter(BUILDINGID == "TI")

test_b1_pBuilding <- Test_pBuilding %>%
  filter(BUILDINGID == "TD")

test_b2_pBuilding <- Test_pBuilding %>%
  filter(BUILDINGID == "TC")

###################################################################

#FLOOR PREDICTION

#apply a random forest to predict floor using the predicted building - 187seg
#Accuracy     Kappa 
#0.9108911 0.8752347 
system.time(rf_predict_floor <- randomForest(FLOOR ~ .,data = Model_Building_Floor,  ntree = 100, do.trace = TRUE, importance = TRUE))
prediction_rf_floor_full <- predict(rf_predict_floor,Test_pBuilding)
performance_rf_floor_full <- postResample(prediction_rf_floor_full,Test_pBuilding$FLOOR)
floor_confusion_matrix <- confusionMatrix(prediction_rf_floor_full, Test_pBuilding$FLOOR)

#apply a SVM to predict floor using the predicted building
#Accuracy     Kappa 
#0.8775878 0.8300252 
system.time(svm_predict_floor <- train(FLOOR~., data = Model_Building_Floor, method = "svmLinear", trControl = trainControl(verboseIter = TRUE)))
prediction_svm_floor_full <- predict(svm_predict_floor,Test_pBuilding)
performance_svm_floor_full <- postResample(prediction_svm_floor_full,Test_pBuilding$FLOOR)
confusionMatrix(prediction_svm_floor_full, Test_pBuilding$FLOOR)

#apply a kNN to predict floor using the predicted building - 0.90 / 0.87
#Accuracy     Kappa 
#0.8118812 0.7394158
system.time(knn_predict_floor <- train.kknn(FLOOR~.,data =  Model_Building_Floor,ks = c(3,5,7,9),  kmax = 9,scale = FALSE, kernel = 'optimal'))
prediction_knn_floor_full <- predict(knn_predict_floor,Test_pBuilding)
performance_knn_floor_full <- postResample(prediction_knn_floor_full,Test_pBuilding$FLOOR)
confusionMatrix(prediction_knn_floor_full, Test_pBuilding$FLOOR)
####################

#apply a kNN to predict floor WITHOUT using the predicted building - #0.91 0.87 (without normalizing)
#k_Wb <- kNN(FLOOR ~ .,wifi[,c(1:312,315)],test[,c(1:312,315)],norm=FALSE,k=5) 
#postResample(k_Wb,test$FLOOR)
#confusionMatrix(k_Wb, test$FLOOR)

#apply a kNN to predict floor WITHOUT using the predicted building - #0.80 0.73 (normalization)
#k_b_WNORM <- kNN(FLOOR ~ .,wifi[,c(1:312,315)],test[,c(1:312,315)],norm=TRUE,k=5) 
#postResample(k_b_WNORM,test$FLOOR)
#confusionMatrix(k_b_WNORM, test$FLOOR)

#performance agaisnt each building

#kNN for building 0 - #<- FOR BUILDING 0 - 97%/95%
#k_b0 <- kNN(FLOOR ~ .,wifi_b0[,c(1:312,315)],test_b0[,c(1:312,315)],norm=FALSE,k=5)  
#postResample(k_b0,test_b0$FLOOR)
#confusionMatrix(k_b0, test_b0$FLOOR)

#randomForest building 0 #<- FOR BUILDING 0 - 98%/96%
#system.time(rf00 <- randomForest(FLOOR ~ .,data = Model_Building_Floor,  ntree = 100, do.trace = TRUE, importance = TRUE))
prediction_FLoor_Building00 <- predict(rf_predict_floor,test_b0_pBuilding)
performancerf00 <- postResample(prediction_FLoor_Building00,test_b0_pBuilding$FLOOR)
CM_FLOOR_B00 <- confusionMatrix(prediction_FLoor_Building00, test_b0_pBuilding$FLOOR)

#SVM building 0 #<- FOR BUILDING 0 - 0.91/0.87
#system.time(svm_predict_floor00 <- train(FLOOR~., data = Model_Building_Floor, method = "svmLinear", trControl = trainControl(verboseIter = TRUE)))
#predictionSVM00 <- predict(svm_predict_floor00,test_b0_pBuilding)
#performanceSVM00 <- postResample(predictionSVM00,test_b0_pBuilding$FLOOR)
#confusionMatrix(predictionSVM00, test_b0_pBuilding$FLOOR)

#################
#kNN for building 1 #<- FOR BUILDING 1 - 78%/68%
#k_b1 <- kNN(FLOOR ~ .,wifi_b1[,c(1:312,315)],test_b1[,c(1:312,315)],norm=FALSE,k=5)  #<- FOR BUILDING 1 - 78%/0.68%
#postResample(k_b1,test_b1$FLOOR)
#confusionMatrix(k_b1, test_b1$FLOOR)

#randomForest building 1 #<- FOR BUILDING 1 - 80%/71%
#system.time(rf01 <- randomForest(FLOOR ~ .,data = Model_Building_Floor,  ntree = 100, do.trace = TRUE, importance = TRUE))
prediction_FLoor_Building01 <- predict(rf_predict_floor,test_b1_pBuilding)
performancerf01 <- postResample(prediction_FLoor_Building01,test_b1_pBuilding$FLOOR)
CM_FLOOR_B01 <- confusionMatrix(prediction_FLoor_Building01, test_b1_pBuilding$FLOOR)

#SVM building 1 #<- FOR BUILDING 1 - 78%/69%
#system.time(svm_predict_floor01 <- train(FLOOR~., data = Model_Building_Floor, method = "svmLinear", trControl = trainControl(verboseIter = TRUE)))
#predictionSVM01 <- predict(svm_predict_floor01,test_b1_pBuilding)
#performanceSVM01 <- postResample(predictionSVM01,test_b1_pBuilding$FLOOR)
#confusionMatrix(predictionSVM01, test_b1_pBuilding$FLOOR)


##################################
#kNN for building 2
#k_b2 <- kNN(FLOOR ~ .,wifi_b2[,c(1:312,315)],test_b2[,c(1:312,315)],norm=FALSE,k=3)  #<- FOR BUILDING 1 - 0.93%/0.90%
#postResample(k_b2,test_b2$FLOOR)
#confusionMatrix(k_b2, test_b2$FLOOR)

#randomForest building 2 #<- FOR BUILDING 2 - 92%/89%
#system.time(rf02 <- randomForest(FLOOR ~ .,data = Model_Building_Floor,  ntree = 150, do.trace = TRUE, importance = TRUE))
prediction_FLoor_Building02 <- predict(rf_predict_floor,test_b2_pBuilding)
performancerf02 <- postResample(prediction_FLoor_Building02,test_b2_pBuilding$FLOOR)
CM_FLOOR_B02 <- confusionMatrix(prediction_FLoor_Building02, test_b2_pBuilding$FLOOR)

#SVM building 2 #<- FOR BUILDING 2 - 92/89
#system.time(svm_predict_floor02 <- train(FLOOR~., data = Model_Building_Floor, method = "svmLinear", trControl = trainControl(verboseIter = TRUE)))
#predictionSVM02 <- predict(svm_predict_floor02,test_b2_pBuilding)
#performanceSVM02 <- postResample(predictionSVM02,test_b2_pBuilding$FLOOR)
#confusionMatrix(predictionSVM02, test_b2_pBuilding$FLOOR)
####################

system.time(rf_floor_00 <- randomForest(FLOOR ~ .,data = droplevels(wifi_b0[,c(1:312,315)]),  ntree = 100, do.trace = TRUE, importance = TRUE))
system.time(rf_floor_01 <- randomForest(FLOOR ~ .,data = droplevels(wifi_b1[,c(1:312,315)]),  ntree = 100, do.trace = TRUE, importance = TRUE))
system.time(rf_floor_02 <- randomForest(FLOOR ~ .,data = droplevels(wifi_b2[,c(1:312,315)]),  ntree = 100, do.trace = TRUE, importance = TRUE))

#Accuracy     Kappa 
#0.9813433 0.9736119
prediction_rf_floor_00 <- predict(rf_floor_00,test_b0_pBuilding) 
performancerf_rf_floor_00 <- postResample(prediction_rf_floor_00,test_b0_pBuilding$FLOOR)

#Accuracy     Kappa 
#0.8045603 0.7170159
prediction_rf_floor_01 <- predict(rf_floor_01,test_b1_pBuilding) 
performancerf_rf_floor_01 <- postResample(prediction_rf_floor_01,test_b1_pBuilding$FLOOR)

#Accuracy     Kappa 
#0.9141791 0.8827959
prediction_rf_floor_02 <- predict(rf_floor_02,test_b2_pBuilding) 
performancerf_rf_floor_02 <- postResample(prediction_rf_floor_02,test_b2_pBuilding$FLOOR)


###################################################################
#MODEL SELECTED RF WITH OVERALL PERFOMANCE OF 0.91 ACURRACY AND 0.88 KAPPA TO PREDICT FLOOR USING THE WHOLE DATASET FOR TRAINING
save(rf_predict_floor, file = "rf_predict_floor.rda")
load("C:\\Users\\Javier Villasmil\\rf_predict_floor.rda")
###################################################################



# LATITUDE PREDICTION

################################################# RF LATITUDE FOR ALL
system.time(rf_LAT00 <- randomForest(LATITUDE ~ .,data = Model_Building_Latitude,  ntree = 100, do.trace = TRUE, importance = TRUE))
rf_LAT <- rf_LAT00

#RMSE  Rsquared       MAE 
#5.7370065 0.9682254 3.9836514 
prediction_RF_LAT_00 <- predict(rf_LAT,test_b0_pBuilding)
performancerf_RF_LAT_00 <- postResample(prediction_RF_LAT_00,test_b0_pBuilding$LATITUDE)

#RMSE   Rsquared        MAE 
#10.6645483  0.9094193  7.4606523
prediction_RF_LAT_01 <- predict(rf_LAT,test_b1_pBuilding)
performancerf_RF_LAT_01 <- postResample(prediction_RF_LAT_01,test_b1_pBuilding$LATITUDE)

#     RMSE  Rsquared       MAE 
#9.2883254 0.8989664 6.4047942
prediction_RF_LAT_02 <- predict(rf_LAT,test_b2_pBuilding)
performancerf_RF_LAT_02 <- postResample(prediction_RF_LAT_02,test_b2_pBuilding$LATITUDE)

################################################# KNN LATITUDE FOR ALL
system.time(knn_latitude_full <- train.kknn(LATITUDE ~ .,data = wifi[,c(1:312,314,316)],ks = c(3,5,7,9),  kmax = 9,scale = FALSE, kernel = 'optimal'))

#RMSE   Rsquared        MAE 
#19.8373711  0.6634594  8.3471577 
prediction_knn_wholeLAT_00 <- predict(knn_latitude_full,test_b0_pBuilding) 
performance_knn_wholeLAT_00 <- postResample(prediction_knn_wholeLAT_00,test_b0_pBuilding$LATITUDE)

#RMSE   Rsquared        MAE 
#11.7997492  0.8881734  7.1583637
prediction_knn_wholeLAT_01 <- predict(knn_latitude_full,test_b1_pBuilding) 
performance_knn_wholeLAT_01 <- postResample(prediction_knn_wholeLAT_01,test_b1_pBuilding$LATITUDE)

#RMSE  Rsquared       MAE 
#9.5841593 0.8918045 6.4464977
prediction_knn_wholeLAT_02 <- predict(knn_latitude_full,test_b2_pBuilding) 
performance_knn_wholeLAT_02 <- postResample(prediction_knn_wholeLAT_02,test_b2_pBuilding$LATITUDE)


########################################
########## full values resume ##########
############# lATITUDE ################
########################################

#RMSE   Rsquared        MAE 
#15.8267429  0.9496581  7.5601760 
prediction_knn_latitude_full <- predict(knn_latitude_full,Test_pBuilding) 
performance_knn_latitude_full <- postResample(prediction_knn_latitude_full,Test_pBuilding$LATITUDE)

#RMSE  Rsquared       MAE 
#8.2533338 0.9865351 5.5284809
prediction_rf_latitude_full <- predict(rf_LAT,Test_pBuilding) 
performance_rf_latitude_full <- postResample(prediction_rf_latitude_full,Test_pBuilding$LATITUDE)
##########################################


###################################################################
#MODEL SELECTED RF WITH OVERALL PERFOMANCE OF RMSE: 8.253, Rsqared: 0.986, MAE: 5.528 PREDICT LATITUDE USING THE WHOLE DATASET FOR TRAINING
save(rf_LAT, file = "rf_predict_latitude.rda")
load("C:\\Users\\Javier Villasmil\\rf_predict_latitude.rda")
###################################################################



#LATITUDE - ONE MODEL FOR EACH BUILDING# - 3 MODELS TOTAL
###################################### RANDOM FOREST
system.time(rf_latitude_b00 <- randomForest(LATITUDE ~ .,data = wifi_b0[,c(1:312,314)],  ntree = 100, do.trace = TRUE, importance = TRUE))
system.time(rf_latitude_b01 <- randomForest(LATITUDE ~ .,data = wifi_b1[,c(1:312,314)],  ntree = 100, do.trace = TRUE, importance = TRUE))
system.time(rf_latitude_b02 <- randomForest(LATITUDE ~ .,data = wifi_b2[,c(1:312,314)],  ntree = 100, do.trace = TRUE, importance = TRUE))

#     RMSE  Rsquared       MAE 
#5.4970395 0.9709228 3.8464546 
prediction_rf_latitude_00 <- predict(rf_latitude_b00,test_b0_pBuilding) 
performancerf_rf_latitude_00 <- postResample(prediction_rf_latitude_00,test_b0_pBuilding$LATITUDE)

#RMSE   Rsquared        MAE 
#10.7112475  0.9087618  7.4699716
prediction_rf_latitude_01 <- predict(rf_latitude_b01,test_b1_pBuilding)
performancerf_rf_latitude_01 <- postResample(prediction_rf_latitude_01,test_b1_pBuilding$LATITUDE)

#     RMSE  Rsquared       MAE 
#9.4272902 0.8936006 6.4497858
prediction_rf_latitude_02 <- predict(rf_latitude_b02,test_b2_pBuilding)
performancerf_rf_latitude_02 <- postResample(prediction_rf_latitude_02,test_b2_pBuilding$LATITUDE)

###################################### KNN
system.time(knn_latitude_b00 <- train.kknn(LATITUDE ~ .,data = wifi_b0[,c(1:312,314)],ks = c(3,5,7,9),  kmax = 9,scale = FALSE, kernel = 'optimal'))
system.time(knn_latitude_b01 <- train.kknn(LATITUDE ~ .,data = wifi_b1[,c(1:312,314)],ks = c(3,5,7,9),  kmax = 9,scale = FALSE, kernel = 'optimal'))
system.time(knn_latitude_b02 <- train.kknn(LATITUDE ~ .,data = wifi_b2[,c(1:312,314)],ks = c(3,5,7,9),  kmax = 9,scale = FALSE, kernel = 'optimal'))

#RMSE   Rsquared        MAE 
#14.0841305  0.8111094  7.0077541  
prediction_knn_latitude_00 <- predict(knn_latitude_b00,test_b0_pBuilding) 
performance_knn_latitude_00 <- postResample(prediction_knn_latitude_00,test_b0_pBuilding$LATITUDE)

#RMSE   Rsquared        MAE 
#12.8035584  0.8696193  7.4698966
prediction_knn_latitude_01 <- predict(knn_latitude_b01,test_b1_pBuilding) 
performance_knn_latitude_01 <- postResample(prediction_knn_latitude_01,test_b1_pBuilding$LATITUDE)

#RMSE  Rsquared       MAE 
#11.777949  0.840482  7.426439 
prediction_knn_latitude_02 <- predict(knn_latitude_b02,test_b2_pBuilding) 
performance_knn_latitude_02 <- postResample(prediction_knn_latitude_02,test_b2_pBuilding$LATITUDE)
######################################


# LONGITUDE PREDICTION

###################################### RANDOM FOREST
system.time(rf_longitude_b00 <- randomForest(LONGITUDE ~ .,data = wifi_b0[,c(1:312,313)],  ntree = 100, do.trace = TRUE, importance = TRUE))
system.time(rf_longitude_b01 <- randomForest(LONGITUDE ~ .,data = wifi_b1[,c(1:312,313)],  ntree = 100, do.trace = TRUE, importance = TRUE))
system.time(rf_longitude_b02 <- randomForest(LONGITUDE ~ .,data = wifi_b2[,c(1:312,313)],  ntree = 100, do.trace = TRUE, importance = TRUE))

#RMSE  Rsquared       MAE 
#6.9760240 0.9322754 4.6875748
prediction_rf_longitude_00 <- predict(rf_longitude_b00,test_b0_pBuilding) 
performancerf_rf_longitude_00 <- postResample(prediction_rf_longitude_00,test_b0_pBuilding$LONGITUDE)

#RMSE  Rsquared       MAE 
#9.3016293 0.9601056 6.6165643 
prediction_rf_longitude_01 <- predict(rf_longitude_b01,test_b1_pBuilding) 
performancerf_rf_longitude_01 <- postResample(prediction_rf_longitude_01,test_b1_pBuilding$LONGITUDE)

#RMSE   Rsquared        MAE 
#10.5149479  0.8904811  6.8873554
prediction_rf_longitude_02 <- predict(rf_longitude_b02,test_b2_pBuilding) 
performancerf_rf_longitude_02 <- postResample(prediction_rf_longitude_02,test_b2_pBuilding$LONGITUDE)

###################################### KNN
system.time(knn_longitude_b00 <- train.kknn(LONGITUDE ~ .,data = wifi_b0[,c(1:312,313)],ks = c(3,5,7,9),  kmax = 9,scale = FALSE, kernel = 'optimal'))
system.time(knn_longitude_b01 <- train.kknn(LONGITUDE ~ .,data = wifi_b1[,c(1:312,313)],ks = c(3,5,7,9),  kmax = 9,scale = FALSE, kernel = 'optimal'))
system.time(knn_longitude_b02 <- train.kknn(LONGITUDE ~ .,data = wifi_b2[,c(1:312,313)],ks = c(3,5,7,9),  kmax = 9,scale = FALSE, kernel = 'optimal'))

#RMSE  Rsquared       MAE 
#9.5236841 0.8692662 5.5508266
prediction_knn_longitude_00 <- predict(knn_longitude_b00,test_b0_pBuilding) 
performance_knn_longitude_00 <- postResample(prediction_knn_longitude_00,test_b0_pBuilding$LONGITUDE)

#RMSE   Rsquared        MAE 
#11.2137887  0.9417551  7.2819478 
prediction_knn_longitude_01 <- predict(knn_longitude_b01,test_b1_pBuilding) 
performance_knn_longitude_01 <- postResample(prediction_knn_longitude_01,test_b1_pBuilding$LONGITUDE)

#RMSE   Rsquared        MAE 
#13.1795532  0.8295386  8.3052541
prediction_knn_longitude_02 <- predict(knn_longitude_b02,test_b2_pBuilding) 
performance_knn_longitude_02 <- postResample(prediction_knn_longitude_02,test_b2_pBuilding$LONGITUDE)
#######################################

#LONGITUDE - ONE MODEL FOR ALL THE BUILDINGS# ONE MODEL TOTAL

system.time(rf_LONGITUDE_full <- randomForest(LONGITUDE ~ .,data = wifi[,c(1:312,316,313)],  ntree = 100, do.trace = TRUE, importance = TRUE))
system.time(knn_LONGITUDE_full <- train.kknn(LONGITUDE ~ .,data = wifi[,c(1:312,316,313)],ks = c(3,5,7,9),  kmax = 9,scale = FALSE, kernel = 'optimal'))

################################ RANDOM FOREST
#RMSE  Rsquared       MAE 
#6.9903554 0.9341325 4.7491671
prediction_rf_longitude_00_full <- predict(rf_LONGITUDE_full,test_b0_pBuilding) 
performance_rf_longitude_00_full <- postResample(prediction_rf_longitude_00_full,test_b0_pBuilding$LONGITUDE)

#RMSE  Rsquared       MAE 
#9.1123832 0.9613591 6.4410962
prediction_rf_longitude_01_full <- predict(rf_LONGITUDE_full,test_b1_pBuilding) 
performance_rf_longitude_01_full <- postResample(prediction_rf_longitude_01_full,test_b1_pBuilding$LONGITUDE)

#RMSE   Rsquared        MAE 
#10.7119519  0.8872783  7.2347318
prediction_rf_longitude_02_full <- predict(rf_LONGITUDE_full,test_b2_pBuilding) 
performance_rf_longitude_02_full <- postResample(prediction_rf_longitude_02_full,test_b2_pBuilding$LONGITUDE)

################################ KNN
#RMSE   Rsquared        MAE 
#25.0901793  0.4891539  8.0327811
prediction_knn_longitude_00_full <- predict(knn_LONGITUDE_full,test_b0_pBuilding) 
performance_knn_longitude_00_full <- postResample(prediction_knn_longitude_00_full,test_b0_pBuilding$LONGITUDE)

#RMSE   Rsquared        MAE 
#12.6176716  0.9278076  7.5740051
prediction_knn_longitude_01_full <- predict(knn_LONGITUDE_full,test_b1_pBuilding) 
performance_knn_longitude_01_full <- postResample(prediction_knn_longitude_01_full,test_b1_pBuilding$LONGITUDE)

#RMSE   Rsquared        MAE 
#13.0310677  0.8315194  7.9547070
prediction_knn_longitude_02_full <- predict(knn_LONGITUDE_full,test_b2_pBuilding) 
performance_knn_longitude_02_full <- postResample(prediction_knn_longitude_02_full,test_b2_pBuilding$LONGITUDE)


########################################
########## full values resume ##########
############# LONGITUDE ################
########################################

#RMSE   Rsquared        MAE 
#19.7145706  0.9735296  7.8871752 
prediction_knn_longitude_full <- predict(knn_LONGITUDE_full,Test_pBuilding) 
performance_knn_longitude_full <- postResample(prediction_knn_longitude_full,Test_pBuilding$LONGITUDE)

#     RMSE  Rsquared       MAE 
#8.6139050 0.9949634 5.8162720
prediction_rf_longitude_full <- predict(rf_LONGITUDE_full,Test_pBuilding) 
performance_rf_longitude_full <- postResample(prediction_rf_longitude_full,Test_pBuilding$LONGITUDE)
##########################################

###################################################################
#MODEL SELECTED RF WITH OVERALL PERFOMANCE OF RMSE: 8.613, Rsqared: 0.994, MAE: 5.816 PREDICT LATITUDE USING THE WHOLE DATASET FOR TRAINING.
save(rf_LONGITUDE_full, file = "rf_predict_longitude.rda")
load("C:\\Users\\Javier Villasmil\\rf_predict_longitude.rda")
###################################################################

#Models selected:
load("C:\\Users\\Javier Villasmil\\svm_predict_building.rda")
load("C:\\Users\\Javier Villasmil\\rf_predict_floor.rda")
load("C:\\Users\\Javier Villasmil\\rf_predict_latitude.rda")
load("C:\\Users\\Javier Villasmil\\rf_predict_longitude.rda")

###################################################################
#################### ERROR ANALYSIS ###############################

test_errors <- test[,c("BUILDINGID","FLOOR","LATITUDE","LONGITUDE","PHONEID")]

building_confusion_matrix
floor_confusion_matrix
performance_rf_latitude_full
performance_rf_longitude_full

###########################################################
test_errors$BUILD_PRED <- building_prediction
test_errors$FLOOR_PRED <- prediction_rf_floor_full
test_errors$LATITUDE_PRED <- prediction_rf_latitude_full
test_errors$LONGITUDE_PRED <- prediction_rf_longitude_full
###########################################################

#Bulding Confusion Matrix
df_building <- as.data.frame(building_confusion_matrix$table) #turns the CM into a dataframe
df_building <- df_building %>%
  mutate(Reference = factor(Reference), # alphabetical order by default
         Prediction = factor(Prediction, levels = rev(unique(Prediction)))) # force reverse alphabetical order
CM_Building_Plot <- ggplot(df_building, aes(x=Reference, y=Prediction, fill=Freq,)) + geom_tile(color="grey") + 
  labs(title = "Confusion Matrix: BUILDING Prediction") +
  scale_x_discrete(position = "top") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_equal() +
  scale_fill_distiller(palette="Blues", direction=1) +
  guides(fill=F) + # removing legend for `fill`
  geom_text(aes(label=Freq), color="black", size=4) # printing values

#Floor Confusion Matrix
df_floor<- as.data.frame(floor_confusion_matrix$table) #turns the CM into a dataframe
df_floor <- df_floor %>%
  mutate(Reference = factor(Reference), # alphabetical order by default
         Prediction = factor(Prediction, levels = rev(unique(Prediction)))) # force reverse alphabetical order
CM_Floor_Plot <- ggplot(df_floor, aes(x=Reference, y=Prediction, fill=Freq,)) + geom_tile(color="grey") +
  labs(title = "Confusion Matrix: FLOOR Prediction") +
  scale_x_discrete(position = "top") +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5)) +
  coord_equal() +
  scale_fill_distiller(palette="Blues", direction=1) +
  guides(fill=F) + # removing legend for `fill`
  geom_text(aes(label=Freq), color="black", size=4) # printing values

#Floor Confusion Matrix Building 0
df_floor_b00<- as.data.frame(CM_FLOOR_B00$table) #turns the CM into a dataframe
df_floor_b00 <- df_floor_b00 %>%
  mutate(Reference = factor(Reference), # alphabetical order by default
         Prediction = factor(Prediction, levels = rev(unique(Prediction)))) # force reverse alphabetical order
CM_Floor_Plot_b0 <- ggplot(df_floor_b00, aes(x=Reference, y=Prediction, fill=Freq,)) + geom_tile(color="grey") + xlab("") +
  scale_x_discrete(position = "top") +
  theme_bw() + 
  coord_equal() +
  scale_fill_distiller(palette="Reds", direction=1) +
  guides(fill=F) + # removing legend for `fill`
  geom_text(aes(label=Freq), color="black", size=4) # printing values

#Floor Confusion Matrix Building 1
df_floor_b01<- as.data.frame(CM_FLOOR_B01$table) #turns the CM into a dataframe
df_floor_b01 <- df_floor_b01 %>%
  mutate(Reference = factor(Reference), # alphabetical order by default
         Prediction = factor(Prediction, levels = rev(unique(Prediction)))) # force reverse alphabetical order
CM_Floor_Plot_b1 <- ggplot(df_floor_b01, aes(x=Reference, y=Prediction, fill=Freq,)) + geom_tile(color="grey") + xlab("") +
  scale_x_discrete(position = "top") +
  theme_bw() + 
  coord_equal() +
  scale_fill_distiller(palette="BuPu", direction=1) +
  guides(fill=F) + # removing legend for `fill`
  geom_text(aes(label=Freq), color="black", size=4) # printing values

#Floor Confusion Matrix Building 2
df_floor_b02<- as.data.frame(CM_FLOOR_B02$table) #turns the CM into a dataframe
df_floor_b02 <- df_floor_b02 %>%
  mutate(Reference = factor(Reference), # alphabetical order by default
         Prediction = factor(Prediction, levels = rev(unique(Prediction)))) # force reverse alphabetical order
CM_Floor_Plot_b2 <- ggplot(df_floor_b02, aes(x=Reference, y=Prediction, fill=Freq,)) + geom_tile(color="grey") + xlab("") +
  scale_x_discrete(position = "top") +
  theme_bw() + 
  coord_equal() +
  scale_fill_distiller(palette="Greens", direction=1) +
  guides(fill=F) + # removing legend for `fill`
  geom_text(aes(label=Freq), color="black", size=4) # printing values

plot_grid_floors <- plot_grid(CM_Floor_Plot_b0,CM_Floor_Plot_b1,CM_Floor_Plot_b2, ncol = 1, labels = c('BUILDING TI-00','BUILDING TD-01',"BUILDING TC-02"), label_size = 10)
plot_grid_final_floor <- plot_grid(CM_Floor_Plot,plot_grid_floors, ncol = 2)

####################################################################
####################################################################

#subsetting errors
test_errors <- test_errors %>%
  mutate(LATITUDE_RES = LATITUDE - LATITUDE_PRED, 
         LONGITUDE_RES = LONGITUDE - LONGITUDE_PRED, 
         DISTANCE_ERROR = sqrt((LATITUDE_RES^2) + (LONGITUDE_RES^2)),
         MANHATAN_ERROR = abs(LATITUDE_RES)+abs(LONGITUDE_RES))

test_errors_TD_01 <- test_errors %>%
  filter(BUILDINGID == "TD")
test_errors_TD_00 <- test_errors %>%
  filter(BUILDINGID == "TI")
test_errors_TD_02 <- test_errors %>%
  filter(BUILDINGID == "TC")
  
test_errors_TD_01_f02 <- test_errors_TD_01 %>%
  filter(FLOOR=="2nd Floor")

test_errors_TD_01_f03 <- test_errors_TD_01 %>%
  filter(FLOOR=="3rd Floor")

################### USING ONLY ONE FLOOR
ggplot() + 
  geom_point(data=test_errors, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") +
  geom_point(data=test_errors, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, color=FLOOR_PRED),size = 2)

################### ONE BUILDING ALL FLOORS (FACET_WRAP)
ggplot() + 
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, color=FLOOR_PRED),size = 2) + facet_wrap("FLOOR")

ggplot() + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, color=FLOOR_PRED),size = 2) + facet_wrap("FLOOR")

ggplot() + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, color=FLOOR_PRED),size = 2) + facet_wrap("FLOOR")

################### ALL BUILDINGS ALL FLOORS (FACER GRID)
ggplot() + 
  #geom_point(data=test_errors, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, color=FLOOR_PRED),size = 2) +
  labs(color = "") + xlab("LATITUDE") + ylab("LONGITUDE") +
  facet_grid(FLOOR~BUILDINGID)+
  theme_bw()+
  theme(strip.text = element_text(face="bold", size=10,lineheight=4.0),
        strip.background = element_rect(fill="seashell2"),
        axis.text.x  = element_text(angle=90,size=8),
        axis.text.y  = element_text(size=8))+
  scale_color_brewer(palette = "Set1")

#########################################################
#########################################################

#DISTANCE ERRORS
###########

#BUILDING OO - GENERAL        
ggplot() + 
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = DISTANCE_ERROR),size = 2)+
  scale_color_distiller(palette = "PuRd", direction = 1, limits=c(0,40), na.value = "yellow")
#BUILDING OO - PER FLOOR
ggplot() + 
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = DISTANCE_ERROR),size = 2) + facet_wrap("FLOOR")+
  scale_color_distiller(palette = "PuRd", direction = 1, limits=c(0,40),na.value = "yellow")

###########  
#BUILDING O1 - GENERAL        
ggplot() + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = DISTANCE_ERROR),size = 2)+
  scale_color_distiller(palette = "PuRd", direction = 1, limits=c(0,40),na.value = "yellow")
#BUILDING O1 - PER FLOOR
ggplot() + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = DISTANCE_ERROR),size = 2) + facet_wrap("FLOOR")+
  scale_color_distiller(palette = "PuRd", direction = 1, limits=c(0,40),na.value = "yellow")

###########  
#BUILDING O2 - GENERAL        
ggplot() + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = DISTANCE_ERROR),size = 2)+
  scale_color_distiller(palette = "PuRd", direction = 1, limits=c(0,40),na.value = "yellow")
#BUILDING O2 - PER FLOOR
ggplot() + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = DISTANCE_ERROR),size = 2) + facet_wrap("FLOOR")+
  scale_color_distiller(palette = "PuRd", direction = 1, limits=c(0,40),na.value = "yellow")
########################################################################

#HISTOGRAMS
# Histogram Latitude Error
general_lat_hist <- ggplot(test_errors, aes(x=LATITUDE_RES)) + 
  geom_histogram(colour="black", fill="lightblue",bins = 100) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) + 
  xlab("Latitude Residuals - All Buildings \n (m)") +
  ylab("Frequency") +
  xlim(-100,100)

bybuilding_lat_hist <- ggplot(test_errors, aes(x=LATITUDE_RES,fill=BUILDINGID)) + 
  geom_histogram(colour="black",bins = 100) +
  labs(fill = "") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) + 
  xlim(-100,100)  +
  xlab("Latitude Residuals \n (m)") +
  ylab("Frequency") +
  facet_wrap("BUILDINGID",dir="v")+
  theme(strip.text = element_text(face="bold", size=10,lineheight=4.0),
        strip.background = element_rect(fill="seashell2"))

grid_latitude <- plot_grid(bybuilding_lat_hist,ncol = 1)
grid_latitude <- plot_grid(general_lat_hist,grid_latitude)
grid_latitude

# Histogram Longitude Error
general_lon_hist <- ggplot(test_errors, aes(x=LONGITUDE_RES)) + 
  geom_histogram(colour="black", fill="lightblue",bins = 100) +
  xlim(-100,100) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  xlab("Longitude Residuals - All Buildings \n (m)") +
  ylab("Frequency")
  
bybuilding_lon_hist <- ggplot(test_errors, aes(x=LONGITUDE_RES,fill=BUILDINGID)) + 
  geom_histogram(colour="black",bins = 100) +
  labs(fill = "") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) + 
  xlim(-100,100) +
  xlab("Longitude Residuals \n (m)") +
  ylab("Frequency") +
  facet_wrap("BUILDINGID",dir="v")+
  theme(strip.text = element_text(face="bold", size=10,lineheight=4.0),
        strip.background = element_rect(fill="seashell2"))

grid_longitude <- plot_grid(bybuilding_lon_hist,ncol = 1)
grid_longitude <- plot_grid(general_lon_hist,grid_longitude)
grid_longitude

# Histogram Distance Error
general_dist_hist <- ggplot(test_errors, aes(x=DISTANCE_ERROR)) + 
  geom_histogram(colour="black", fill="lightblue",bins = 100) +
  xlim(0,135) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  xlab("Distance Errors - All Buildings \n (m)") +
  ylab("Frequency")


bybuilding_dist_hist <- ggplot(test_errors, aes(x=DISTANCE_ERROR,fill=BUILDINGID)) + 
  geom_histogram(colour="black",bins = 100) +
  labs(fill = "") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) + 
  xlim(0,135) +
  xlab("Distance Errors \n (m)") +
  ylab("Frequency") +
  facet_wrap("BUILDINGID",dir="v")+
  theme(strip.text = element_text(face="bold", size=10,lineheight=4.0),
        strip.background = element_rect(fill="seashell2"))

grid_distance <- plot_grid(bybuilding_dist_hist,ncol = 1)
grid_distance <- plot_grid(general_dist_hist,grid_distance)
grid_distance

# Boxplot Distance Error
ggplot(test_errors, aes(y=DISTANCE_ERROR,x=BUILDINGID, fill=BUILDINGID)) + 
  geom_boxplot(outlier.colour="red") +
  ylab("Distance Errors \n (m)") +
  labs(fill = "")+
  coord_flip()+
  geom_jitter(shape=1, position=position_jitter(0.1))
  

#########################################
shapiro.test(test_errors$LONGITUDE_RES)
shapiro.test(test_errors$LATITUDE_RES)
shapiro.test(test_errors$DISTANCE_ERROR)
##########################################

#LATITUDE RESIDUAL ERRORS
ggplot() + 
  geom_point(data=test_errors, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = LATITUDE_RES),size = 2)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color ="Latitude Error \n         (m)")+
  #scale_color_distiller(palette = "RdBu", direction = -1, limits = c(-10,10),guide = "colourbar",na.value="yellow",name = "Latitude Error \n         (m)")+
  scale_color_gradient2(midpoint=0, low="red", mid="green",high="red", space ="Lab",limits = c(-10,10),na.value="red")+
  facet_grid(FLOOR~BUILDINGID)+
  theme_bw()+
  theme(strip.text = element_text(face="bold", size=10,lineheight=4.0),
        strip.background = element_rect(fill="seashell2"),
        axis.text.x  = element_text(angle=90,size=8),
        axis.text.y  = element_text(size=8))


#BUILDING OO - GENERAL - LATITUDE        
ggplot() + 
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = LATITUDE_RES),size = 2)+
  scale_color_distiller(palette = "RdBu", direction = -1, limits = c(-10,10),guide = "colourbar",na.value="yellow",name = "Latitude Error \n         (m)")

#BUILDING OO - PER FLOOR  LATITUDE
ggplot() + 
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = LATITUDE_RES),size = 2)+
  scale_color_distiller(palette = "RdBu", direction = -1, limits = c(-10,10),guide = "colourbar",na.value="yellow",name = "Latitude Error \n         (m)")+
  facet_wrap("FLOOR")
  
#BUILDING O1 - GENERAL - LATITUDE        
ggplot() + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = LATITUDE_RES),size = 2)+
  scale_color_distiller(palette = "RdBu", direction = -1, limits = c(-10,10),guide = "colourbar",na.value="yellow",name = "Latitude Error \n         (m)")

#BUILDING O1 - PER FLOOR LATITUDE
ggplot() + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = LATITUDE_RES),size = 2)+
  scale_color_distiller(palette = "RdBu", direction = -1, limits = c(-10,10),guide = "colourbar",na.value="yellow",name = "Latitude Error \n         (m)")+
  facet_wrap("FLOOR")

#BUILDING O2 - GENERAL - LATITUDE        
ggplot() + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = LATITUDE_RES),size = 2)+
  scale_color_distiller(palette = "RdBu", direction = -1, limits = c(-10,10),guide = "colourbar",na.value="yellow",name = "Latitude Error \n         (m)")

#BUILDING O2 - PER FLOOR LATITUDE
ggplot() + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = LATITUDE_RES),size = 2)+
  scale_color_distiller(palette = "RdBu", direction = -1, limits = c(-10,10),guide = "colourbar",na.value="yellow",name = "Latitude Error \n         (m)")+
  facet_wrap("FLOOR")

#################################################################################
#################################################################################

#LATITUDE RESIDUAL ERRORS
ggplot() + 
  geom_point(data=test_errors, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = LONGITUDE_RES),size = 2)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color ="Longitude Error \n         (m)")+
  #scale_color_distiller(palette = "RdBu", limits = c(-10,10),direction = -1,guide = "colourbar",na.value="yellow",name = "Longitude Error \n           (m)")
  scale_color_gradient2(midpoint=0, low="red", mid="green",high="red", space ="Lab",limits = c(-10,10),na.value="red")+
  facet_grid(FLOOR~BUILDINGID)+
  theme_bw()+
  theme(strip.text = element_text(face="bold", size=10,lineheight=4.0),
        strip.background = element_rect(fill="seashell2"),
        axis.text.x  = element_text(angle=90,size=8),
        axis.text.y  = element_text(size=8))

#BUILDING OO - PER FLOOR - LONGITUDE
ggplot() + 
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_00, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = LONGITUDE_RES),size = 2)+
  scale_color_distiller(palette = "RdBu", limits = c(-10,10),direction = -1,guide = "colourbar",na.value="yellow",name = "Longitude Error \n           (m)")+
  facet_wrap("FLOOR")

#BUILDING O1 - GENERAL - LONGITUDE 
ggplot() + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = LONGITUDE_RES),size = 2)+
  scale_color_distiller(palette = "RdBu", limits = c(-10,10),direction = -1,guide = "colourbar",na.value="yellow",name = "Longitude Error \n           (m)")

#BUILDING O1 - PER FLOOR - LONGITUDE
ggplot() + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_01, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = LONGITUDE_RES),size = 2)+
  scale_color_distiller(palette = "RdBu", limits = c(-10,10),direction = -1,guide = "colourbar",na.value="yellow",name = "Longitude Error \n           (m)")+
  facet_wrap("FLOOR")

#BUILDING O2 - GENERAL - LONGITUDE 
longitude_building_error_02 <- ggplot() + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = LONGITUDE_RES),size = 2)+
  scale_color_gradient2(midpoint=0, low="red", mid="green",high="red", space ="Lab",limits = c(-10,10),na.value="red")
  #scale_color_distiller(palette = "RdBu", limits = c(-10,10),direction = -1,guide = "colourbar",na.value="yellow",name = "Longitude Error \n           (m)")

#BUILDING O2 - PER FLOOR - LONGITUDE
longitude_floor_errorb02 <- ggplot() + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") + 
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED),size = 3,shape=1,color = "black")+
  geom_point(data=test_errors_TD_02, aes(x = LONGITUDE_PRED, y = LATITUDE_PRED, colour = LONGITUDE_RES),size = 2) +
  #scale_color_distiller(palette = "RdBu", limits = c(-10,10),direction = -1,guide = "colourbar",na.value="yellow",name = "Longitude Error \n           (m)")+
  facet_wrap("FLOOR",dir="v",ncol=1)+
  theme(strip.text = element_text(face="bold", size=20,lineheight=5.0),strip.background = element_blank())

plot_grid(longitude_building_error_02,longitude_floor_errorb02)
#################################################################################
#################################################################################

test_big_distance_errors <- test_errors %>%
  filter(DISTANCE_ERROR>0)

## BUILDING 00
aa <- ggplot(data=test_big_distance_errors[test_big_distance_errors$BUILDINGID %in% c("TI"),], aes(x = LONGITUDE, y = LATITUDE,colour = DISTANCE_ERROR)) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE),size = 1,shape = 4,color="black") +
  labs(color="Distance (m)")+
  xlab("Longitude \n Building TI-00")+
  ylab("Latitude")+
  #geom_point(aes(x = LONGITUDE_PRED, y = LATITUDE_PRED,),size = 2, shape = 1, colour = "red")+
  geom_segment(aes(x = LONGITUDE, y = LATITUDE, xend = LONGITUDE_PRED, yend = LATITUDE_PRED),
               arrow = arrow(length = unit(0.01, "npc")))+
  scale_color_gradient(low="green", high="red", space ="Lab", limit=c(0,20), na.value = "blue")

bb <- ggplot(data=test_big_distance_errors[test_big_distance_errors$BUILDINGID %in% c("TI"),], aes(x = LONGITUDE, y = LATITUDE,colour = DISTANCE_ERROR)) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE),size = 1,shape = 4,color="black") +
  labs(color="Distance (m)")+
  xlab("Longitude \n Building TI-00")+
  ylab("Latitude")+
  #geom_point(aes(x = LONGITUDE_PRED, y = LATITUDE_PRED,),size = 2, shape = 1, colour = "red")+
  geom_segment(aes(x = LONGITUDE, y = LATITUDE, xend = LONGITUDE_PRED, yend = LATITUDE_PRED),
               arrow = arrow(length = unit(0.05, "npc")))+
  scale_color_gradient(low="green", high="red", space ="Lab", limit=c(0,20), na.value = "blue") +
  facet_wrap("FLOOR",dir="v",ncol=1)+
  theme(strip.text = element_text(face="bold", size=10,lineheight=4.0),
        strip.background = element_rect(fill="seashell2"))
  
plot_grid(aa,bb)  

## BUILDING 01
cc <- ggplot(data=test_big_distance_errors[test_big_distance_errors$BUILDINGID %in% c("TD"),], aes(x = LONGITUDE, y = LATITUDE,colour = DISTANCE_ERROR)) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE),size = 1,shape = 4,color="black") +
  labs(color="Distance (m)")+
  xlab("Longitude \n Building TD-01")+
  ylab("Latitude")+
  #geom_point(aes(x = LONGITUDE_PRED, y = LATITUDE_PRED,),size = 2, shape = 1, colour = "red")+
  geom_segment(aes(x = LONGITUDE, y = LATITUDE, xend = LONGITUDE_PRED, yend = LATITUDE_PRED),
               arrow = arrow(length = unit(0.01, "npc")))+
  scale_color_gradient(low="green", high="red", space ="Lab", limit=c(0,20), na.value = "blue")

dd <- ggplot(data=test_big_distance_errors[test_big_distance_errors$BUILDINGID %in% c("TD"),], aes(x = LONGITUDE, y = LATITUDE,colour = DISTANCE_ERROR)) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE),size = 1,shape = 4,color="black") +
  labs(color="Distance (m)")+
  xlab("Longitude \n Building TD-01")+
  ylab("Latitude")+
  #geom_point(aes(x = LONGITUDE_PRED, y = LATITUDE_PRED,),size = 2, shape = 1, colour = "red")+
  geom_segment(aes(x = LONGITUDE, y = LATITUDE, xend = LONGITUDE_PRED, yend = LATITUDE_PRED),
               arrow = arrow(length = unit(0.05, "npc")))+
  scale_color_gradient(low="green", high="red", space ="Lab", limit=c(0,20), na.value = "blue") +
  facet_wrap("FLOOR",dir="v",ncol=1)+
  theme(strip.text = element_text(face="bold", size=10,lineheight=4.0),
        strip.background = element_rect(fill="seashell2"))

plot_grid(cc,dd)  


## BUILDING 02
ee <- ggplot(data=test_big_distance_errors[test_big_distance_errors$BUILDINGID %in% c("TC"),], aes(x = LONGITUDE, y = LATITUDE,colour = DISTANCE_ERROR)) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE),size = 1,shape = 4,color="black") +
  labs(color="Distance (m)")+
  xlab("Longitude \n Building TC-02")+
  ylab("Latitude")+
  #geom_point(aes(x = LONGITUDE_PRED, y = LATITUDE_PRED,),size = 2, shape = 1, colour = "red")+
  geom_segment(aes(x = LONGITUDE, y = LATITUDE, xend = LONGITUDE_PRED, yend = LATITUDE_PRED),
               arrow = arrow(length = unit(0.01, "npc")))+
  scale_color_gradient(low="green", high="red", space ="Lab", limit=c(0,20), na.value = "blue")

ff <- ggplot(data=test_big_distance_errors[test_big_distance_errors$BUILDINGID %in% c("TC"),], aes(x = LONGITUDE, y = LATITUDE,colour = DISTANCE_ERROR)) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE),size = 1,shape = 4,color="black") +
  labs(color="Distance (m)")+
  xlab("Longitude \n Building TC-02")+
  ylab("Latitude")+
  #geom_point(aes(x = LONGITUDE_PRED, y = LATITUDE_PRED,),size = 2, shape = 1, colour = "red")+
  geom_segment(aes(x = LONGITUDE, y = LATITUDE, xend = LONGITUDE_PRED, yend = LATITUDE_PRED),
               arrow = arrow(length = unit(0.05, "npc")))+
  scale_color_gradient(low="green", high="red", space ="Lab", limit=c(0,20), na.value = "blue") +
  facet_wrap("FLOOR",dir="v",ncol=1)+
  theme(strip.text = element_text(face="bold", size=10,lineheight=4.0),
        strip.background = element_rect(fill="seashell2"))

plot_grid(ee,ff)  

#########################################
mean(test_errors$DISTANCE_ERROR)
median(test_errors$DISTANCE_ERROR)

mean(abs(test_errors$LONGITUDE_RES))
median(abs(test_errors$LONGITUDE_RES))

mean(abs(test_errors$LATITUDE_RES))
median(abs(test_errors$LATITUDE_RES))
     
#########################################
#########################################
ggplot(test_errors, aes(x=LONGITUDE_RES)) + 
  geom_histogram(colour="black", fill="darkblue",bins = 100) +
  xlim(-100,100) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  xlab("") +
  ylab("")

ggplot(test_errors, aes(x=DISTANCE_ERROR)) + 
  geom_histogram(colour="black", fill="darkred",bins = 100) +
  xlim(0,135) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
  xlab("") +
  ylab("")

ggplot(test_errors, aes(x=DISTANCE_ERROR,fill=BUILDINGID)) + 
  geom_histogram(colour="black",bins = 100) +
  labs(fill = "") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) + 
  xlim(0,135) +
  xlab("") +
  ylab("") +
  facet_wrap("BUILDINGID",dir="v")+
  theme(strip.text = element_text(face="bold", size=10,lineheight=4.0),
        strip.background = element_blank())

## BUILDING 00
ggplot(data=test_big_distance_errors[test_big_distance_errors$BUILDINGID %in% c("TI"),], aes(x = LONGITUDE, y = LATITUDE,colour = DISTANCE_ERROR)) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE),size = 1,shape = 4,color="black") +
  labs(color="")+
  xlab("")+
  ylab("")+
  #geom_point(aes(x = LONGITUDE_PRED, y = LATITUDE_PRED,),size = 2, shape = 1, colour = "red")+
  geom_segment(aes(x = LONGITUDE, y = LATITUDE, xend = LONGITUDE_PRED, yend = LATITUDE_PRED),
               arrow = arrow(length = unit(0.01, "npc")))+
  scale_color_gradient(low="green", high="red", space ="Lab", limit=c(0,20), na.value = "blue")

## BUILDING 01
ggplot(data=test_big_distance_errors[test_big_distance_errors$BUILDINGID %in% c("TD"),], aes(x = LONGITUDE, y = LATITUDE,colour = DISTANCE_ERROR)) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE),size = 1,shape = 4,color="black") +
  labs(color="")+
  xlab("")+
  ylab("")+
  #geom_point(aes(x = LONGITUDE_PRED, y = LATITUDE_PRED,),size = 2, shape = 1, colour = "red")+
  geom_segment(aes(x = LONGITUDE, y = LATITUDE, xend = LONGITUDE_PRED, yend = LATITUDE_PRED),
               arrow = arrow(length = unit(0.01, "npc")))+
  scale_color_gradient(low="green", high="red", space ="Lab", limit=c(0,20), na.value = "blue")

## BUILDING 02
ggplot(data=test_big_distance_errors[test_big_distance_errors$BUILDINGID %in% c("TC"),], aes(x = LONGITUDE, y = LATITUDE,colour = DISTANCE_ERROR)) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE),size = 1,shape = 4,color="black") +
  labs(color="")+
  xlab("")+
  ylab("")+
  #geom_point(aes(x = LONGITUDE_PRED, y = LATITUDE_PRED,),size = 2, shape = 1, colour = "red")+
  geom_segment(aes(x = LONGITUDE, y = LATITUDE, xend = LONGITUDE_PRED, yend = LATITUDE_PRED),
               arrow = arrow(length = unit(0.01, "npc")))+
  scale_color_gradient(low="green", high="red", space ="Lab", limit=c(0,20), na.value = "blue")

#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
