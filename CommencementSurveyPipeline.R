packages = c('likert', 
             'installr', 
             'HH', 
             'tidyverse', 
             'sjPlot' ,
             'plotly', 
             'ggplot2', 
             'ggthemes', 
             'plyr', 
             'scales', 
             'reshape2', 
             'waffle', 
             'hrbrthemes', 
             'viridis')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

#------Pipeline for auto-cleanup if it hasn't been done manually-----------

test <- read.csv("C:/Users/Adam/Desktop/Commencement Pipeline/commencement.csv")
dat_dirty <- data.frame(test[3:nrow(test),c(18:31,35)])
dat <- dat_dirty[!(is.na(dat_dirty$Virtual) | dat_dirty$Virtual==""), ]
dat <-  dat[!(dat$Status.1=="Not listed, please list"),]
dat$Status <- dat$Status.1
dat$Status.1 <- NULL
rownames(dat) <- NULL
rownames(dat) <- seq(length=nrow(dat))
test <- NULL
write.csv(dat, "C:/Users/Adam/Desktop/commencement_clean.csv", row.names = FALSE)
dat<- read.csv("C:/Users/Adam/Desktop/commencement_clean.csv")
#-----------Method for making those bar plots --------------

barplotThing <- function(dat, x, y, cat, colors) {
  
  tab1 <- dat[1:nrow(dat),c(x,y,cat)] %>% melt(id.vars = colnames(dat)[cat]) %>% filter(!(value=="" | Status==""))
  tab1 <- as.data.frame(table(tab1[1:nrow(tab1),c(1,3)]))[order(tab1[,1]),]
  tab1<- tab1[!(tab1[,1]==""),]
  colnames(tab1) <- c("cat","value","Freq")
  
  
  count <- 0
  for(i in unique(tab1$cat)) {
    sum <- sum(tab1$Freq[tab1$cat == i])
    revpos <- c(sum(tab1$cat == i):1)
    for(j in 1:sum(tab1$cat == i)) {
      tab1$percent[count + j] <- round( 100* (as.numeric(tab1$Freq[count + j]) / sum))
      tab1$labelpercent[count + revpos[j]] <- round( 100* (as.numeric(tab1$Freq[count + revpos[j]]) / sum))
    }
    count = count + length(unique(tab1$cat))
  }
  
  count <- 0
  for(i in unique(tab1$cat)) {
    sum <- 0
    for(j in sum(tab1$cat == i):1) {
      tab1$pos[j + count] = sum + (tab1$labelpercent[j+count] / 2) 
      sum = sum + tab1$percent[j+count]
    }
    count = count + length(unique(tab1$cat))
  }
  
  
  tab1Plot <- ggplot() + theme_bw() + geom_bar(aes(y = percent, x = cat, fill = value), data = tab1,
                                               stat="identity")
  
  tab1Plot <- tab1Plot + geom_text(data=tab1, aes(x = cat, y = pos, 
                                                  label = paste0(percent,"%")), size=4)
  tab1Plot <- tab1Plot + theme(legend.position="bottom", legend.direction="horizontal",
                               legend.title = element_blank())
  
  tab1Plot <- tab1Plot + scale_fill_manual(values=colors) +
    
    scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) 
  
  return(tab1Plot)
  
}


#of those who are interested in a virtual ceremony, how many are also interested
#in an in person ceremony?


VN_IP_VYes_table <- as.data.frame(table(dat$VYInPerson[dat$Virtual == "Yes"]))

VN_IP_VUnsure_table <- as.data.frame(table(dat$VYInPerson[dat$Virtual == "Unsure"]))

VN_IP_VNo_table <- as.data.frame(table(dat$VN_InPerson))

dat_VYes <- filter(dat, Virtual=="Yes")
dat_VUnsure <- filter(dat, Virtual=="Unsure")
dat_VNo <- filter(dat, Virtual=="No")

#Aggregate yes/no responses for an All column
dat_VAll <- dat
for (i in 1:nrow(dat)) {
  if (dat_VAll$VYInPerson[i] == "") {
    dat_VAll$VYInPerson[i] <- dat$VN_InPerson[i]
  }
}

#three filtered lists for undergrads, master's and doctoral students

dat_undergrad <- filter(dat, Status=="Undergraduate")
dat_masters <- filter(dat, Status=="Master's")
dat_phd <- filter(dat, Status=="Doctoral")


#-------------------------SAMPLE DESCRIPTION---------------------------

#----------------academic level waffle plot----------------------
pie1 <- dat[!(dat$Status==""),]
pie <- as.data.frame(table(pie1$Status)[2:4])
pie2 <- pie$Freq / 2
val_names <- sprintf("%s (%s)", c("Doctoral", "Master's", "Undergraduate"), scales::percent(round(pie2/sum(pie2), 2)))
names(pie2) <- val_names
StatusWaffle <- waffle(pie2, rows=10, colors = c("#81A5D8","#F18775","#E9BA65"), title = "Academic Level of Survey Respondents", xlab = "1 square = 2 respondents")
StatusWaffle

#--------------Waffle Plot for Virtual---------------------
pie1 <- dat[!(dat$Virtual==""),]
pie <- as.data.frame(table(pie1$Virtual)[1:3])
pie2 <- pie$Freq / 2
val_names <- sprintf("%s (%s)", c("No", "Unsure", "Yes"), scales::percent(round(pie2/sum(pie2), 2)))
names(pie2) <- val_names
VirtualWaffle <- waffle(pie2, rows=10, colors = c("#F18775","#E9BA65","#81A5D8"), title = "Interest in Virtual Ceremony", xlab = "1 square = 2 respondents")
VirtualWaffle
#----------------Waffle Plot for Physical-----------------
pie1 <- dat_VAll[!(dat_VAll$VYInPerson==""),]
pie <- as.data.frame(table(pie1$VYInPerson)[2:4])
pie2 <- pie$Freq / 2
val_names <- sprintf("%s (%s)", c("No", "Unsure", "Yes"), scales::percent(round(pie2/sum(pie2), 2)))
names(pie2) <- val_names
PhysicalWaffle <- waffle(pie2, rows=10, colors = c("#F18775","#E9BA65","#81A5D8"), title = "Interest in In-Person Ceremony", xlab = "1 square = 2 respondents")
PhysicalWaffle

pie1 <- c(95,538,1706)
pie1 <- pie1 / 6
val_names <- sprintf("%s - %i", c("Doctoral", "Master's", "Undergraduate"), round(pie1 * 6))
names(pie1) <- val_names
PhysicalWaffle <- waffle(pie1, rows=10, colors = c("#81A5D8","#F18775","#E9BA65"), title = "Total Potential Survey Participants", xlab = "1 square = 6 respondents")
PhysicalWaffle
#----------- First Bar Chart - Virtual--------------

vPlot_fill <- c("#FF695F", "#CDCDCD", "#A0D7FF")

VYgrad <- dat[1:nrow(dat),c(1,15)] %>% melt(id.vars = "Status") %>% filter(!(value=="" | Status==""))
VYgrad <- as.data.frame(table(VYgrad[1:nrow(VYgrad),c(1,3)])) %>% filter(!(Status==""))
VYgrad <- VYgrad[order(VYgrad$Status),]

count <- 0
for(i in unique(VYgrad$Status)) {
  sum <- sum(VYgrad$Freq[VYgrad$Status == i])
  revpos <- c(sum(VYgrad$Status == i):1)
  for(j in 1:sum(VYgrad$Status == i)) {
    VYgrad$percent[count + j] <- round( 100* (as.numeric(VYgrad$Freq[count + j]) / sum))
    VYgrad$labelpercent[count + revpos[j]] <- round( 100* (as.numeric(VYgrad$Freq[count + revpos[j]]) / sum))
  }
  count = count + length(unique(VYgrad$Status))
}
count <- 0
for(i in unique(VYgrad$Status)) {
  sum <- 0
  for(j in sum(VYgrad$Status == i):1) {
    VYgrad$pos[j + count] = sum + (VYgrad$labelpercent[j+count] / 2) 
    sum = sum + VYgrad$percent[j+count]
  }
  count = count + length(unique(VYgrad$Status))
}


VYgradPlot <- ggplot() + theme_bw() + geom_bar(aes(y = percent, x = Status, fill = value), data = VYgrad,
                                               stat="identity")

VYgradPlot <- VYgradPlot + geom_text(data=VYgrad, aes(x = Status, y = pos, 
                                                      label = paste0(percent,"%")), size=4)
VYgradPlot <- VYgradPlot + theme(legend.position="bottom", legend.direction="horizontal",
                                 legend.title = element_blank())

VYgradPlot <- VYgradPlot + labs(x="Academic level", y="Percent of Respondants") +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  ggtitle("Expressed Interest in a Virtual Ceremony")

VYgradPlot <- VYgradPlot + scale_fill_manual(values=vPlot_fill)

VYgradPlot 


#-------------------------Second bar chart thing-------------

VYes_VYperson_cumu <- data.frame(round(table(dat_VYes$VYInPerson) * 100/ sum(table(dat_VYes$VYInPerson))))
VYes_VYperson_cumu$Status <- c("Yes", "Yes", "Yes", "Yes")
VUnsure_VYperson_cumu <- data.frame(round(table(dat_VUnsure$VYInPerson) * 100 / sum(table(dat_VUnsure$VYInPerson))))
VUnsure_VYperson_cumu$Status <- c("Unsure", "Unsure", "Unsure", "Unsure")
VNo_VNperson_cumu <- data.frame(round(table(dat_VNo$VN_InPerson) * 100 / sum(table(dat_VNo$VN_InPerson))))
VNo_VNperson_cumu$Status <- c("No", "No", "No", "No")
everyone_Vperson_cumu <- rev(data.frame(round(table(dat_VAll$VYInPerson) * 100 / sum(table(dat_VAll$VYInPerson)))))
everyone_Vperson_cumu$Status <- c("All", "All", "All", "All")

VYes_VYperson_cumu <- VYes_VYperson_cumu[order(VYes_VYperson_cumu$Status),]
VUnsure_VYperson_cumu <- VUnsure_VYperson_cumu[order(VUnsure_VYperson_cumu$Status),]
VNo_VNperson_cumu <- VNo_VNperson_cumu[order(VNo_VNperson_cumu$Status),]
everyone_Vperson_cumu <- everyone_Vperson_cumu[order(everyone_Vperson_cumu$Status),]

VYperson_final <- as.data.frame(rbind(VYes_VYperson_cumu[2:4,], 
                                      VUnsure_VYperson_cumu[2:4,], 
                                      VNo_VNperson_cumu[2:4,],
                                      everyone_Vperson_cumu[2:4,]))

colnames(VYperson_final) <- factor(c("Response", "Freq", "Status"))

x <- 0
i <- 1
while (i < 11) {
  x <- VYperson_final$Freq[i+2]
  VYperson_final$pos[i] <- x/2
  VYperson_final$FreqAdjusted[i] <- VYperson_final$Freq[i+2]
  y = x + (VYperson_final$Freq[i + 1])/2
  VYperson_final$pos[i + 1] <- y
  VYperson_final$FreqAdjusted[i+1] <- VYperson_final$Freq[i+1]
  z = y + (VYperson_final$Freq[i + 1])/2 + (VYperson_final$Freq[i])/2
  VYperson_final$pos[i + 2] <- z
  VYperson_final$FreqAdjusted[i+2] <- VYperson_final$Freq[i]
  i = i + 3
}

vYesPlot <- ggplot() + theme_bw() + geom_bar(aes(y = Freq, x = Status, fill = Response), data = VYperson_final,
                                             stat="identity")

vYesPlot <- vYesPlot + geom_text(data=VYperson_final, aes(x = Status, y = pos, 
                                                          label = paste0(FreqAdjusted,"%")), size=4)
vYesPlot <- vYesPlot + theme(legend.position="bottom", legend.direction="horizontal",
                             legend.title = element_blank())

vYesPlot <- vYesPlot + labs(x="Total Student Interest in Virtual Ceremony", y="Percent of Respondants") +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  ggtitle("Expressed Interest in an In-Person Ceremony")

vYesPlot <- vYesPlot + scale_fill_manual(values=vPlot_fill)

vYesPlot

#-----------THIRD BAR PLOT THING MOST EFFICIENT ITERATION --------------

plot <- barplotThing(dat, 2, 3, 15, vPlot_fill)
plot <- plot + labs(x="Academic level", y="Percent of Respondants")+
  ggtitle("Expressed Interest in an In-Person Ceremony")
plot

#----------------------IN PERSON CEREMONY-------------------------

VYperson_final <- as.data.frame(rbind(VYes_VYperson_cumu[2:4,], 
                                      VUnsure_VYperson_cumu[2:4,], 
                                      VNo_VNperson_cumu[2:4,],
                                      everyone_Vperson_cumu[2:4,]))

colnames(VYperson_final) <- factor(c("Response", "Freq", "Status"))

x <- 0
i <- 1
while (i < 11) {
  x <- VYperson_final$Freq[i+2]
  VYperson_final$pos[i] <- x/2
  VYperson_final$FreqAdjusted[i] <- VYperson_final$Freq[i+2]
  y = x + (VYperson_final$Freq[i + 1])/2
  VYperson_final$pos[i + 1] <- y
  VYperson_final$FreqAdjusted[i+1] <- VYperson_final$Freq[i+1]
  z = y + (VYperson_final$Freq[i + 1])/2 + (VYperson_final$Freq[i])/2
  VYperson_final$pos[i + 2] <- z
  VYperson_final$FreqAdjusted[i+2] <- VYperson_final$Freq[i]
  i = i + 3
}

vYesPlot <- ggplot() + theme_bw() + geom_bar(aes(y = Freq, x = Status, fill = Response), data = VYperson_final,
                                             stat="identity")
vYesPlot <- vYesPlot + geom_text(data=VYperson_final, aes(x = Status, y = pos, 
                                                          label = paste0(FreqAdjusted,"%")), size=4)
vYesPlot <- vYesPlot + theme(legend.position="bottom", legend.direction="horizontal",
                             legend.title = element_blank())
vYesPlot <- vYesPlot + labs(x="Total Student Interest in Virtual Ceremony", y="Percent of Respondants") +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  ggtitle("Expressed Interest in an In-Person Ceremony")
vYesPlot <- vYesPlot + scale_fill_manual(values=vPlot_fill)

#----------------------LIKERT STUFF-------------------------

likertLevels <- c("Extremely unlikely", "Somewhat unlikely", "Neither likely nor unlikely",
                  "Somewhat likely", "Extremely likely")


#turn the total likelihoods into factors. Doing this before filtering saves lines of code

dat$Inperson.likely_1 <- factor(dat$Inperson.likely_1, levels = likertLevels)
dat$Inperson.likely_2 <- factor(dat$Inperson.likely_2, levels = likertLevels)
dat$Inperson.likely_3 <- factor(dat$Inperson.likely_3, levels = likertLevels)

#makes frequency tables of the different response categories 

undergrad_df_1 <- as.data.frame(table(
  dat_undergrad$Inperson.likely_1))
undergrad_df_2 <- as.data.frame(table(
  dat_undergrad$Inperson.likely_2))
undergrad_df_3 <- as.data.frame(table(
  dat_undergrad$Inperson.likely_3))

masters_df_1 <- as.data.frame(table(
  dat_masters$Inperson.likely_1))
masters_df_2 <- as.data.frame(table(
  dat_masters$Inperson.likely_2))
masters_df_3 <- as.data.frame(table(
  dat_masters$Inperson.likely_3))

phd_df_1 <- as.data.frame(table(
  dat_phd$Inperson.likely_1))
phd_df_2 <- as.data.frame(table(
  dat_phd$Inperson.likely_2))
phd_df_3 <- as.data.frame(table(
  dat_phd$Inperson.likely_3))

#combines undergrad likert frequencies into single data frame
undergrad_combined <- as.data.frame.matrix(rbind(undergrad_df_1[2:6,2], 
                                                 undergrad_df_2[2:6,2], 
                                                 undergrad_df_3[2:6,2]))

masters_combined <- as.data.frame.matrix(rbind(masters_df_1[2:6,2], 
                                               masters_df_2[2:6,2], 
                                               masters_df_3[2:6,2]))

phd_combined <- as.data.frame.matrix(rbind(phd_df_1[2:6,2], 
                                           phd_df_2[2:6,2], 
                                           phd_df_3[2:6,2]))

#ok now let's name some ROWS AND COLUMNS BRO

colnames(undergrad_combined) <- likertLevels #sets rownames of combined table into likert lvls
colnames(masters_combined) <- likertLevels #sets rownames of combined table into likert lvls
colnames(phd_combined) <- likertLevels #sets rownames of combined table into likert lvls

rownames(undergrad_combined) <- c("August 2020", "December 2020", "May 2021") #sets colnames to categorical colnames
rownames(masters_combined) <- c("August 2020", "December 2020", "May 2021") #sets colnames to categorical colnames
rownames(phd_combined) <- c("August 2020", "December 2020", "May 2021") #sets colnames to categorical colnames

#binds rownames set 

undergrad_final <- rev(tibble::rownames_to_column(undergrad_combined, var="Measure"))
masters_final <-rev(tibble::rownames_to_column(masters_combined, var="Measure"))
phd_final <- rev(tibble::rownames_to_column(phd_combined, var="Measure")) 

#merge the tables together and find the sum of their respective month values
#ALSO I actually am using tibbles here which is something I should have done all along 
#tbh
#I also had to do all the row and col name binding separately for the last frame here 
#which is something I would probably fix if I had another go at this pipeline


everyone_final <- rbind(undergrad_final, masters_final, phd_final) %>%
  group_by(Measure = tolower(Measure)) %>%
  summarise_each(list(mean=mean)) %>%
  as.data.frame()

everyone_final <- round(everyone_final[,2:6]) * 3


colnames(everyone_final) <- (likertLevels) #sets rownames of combined table into likert lvls
rownames(everyone_final) <- c("August 2020", "December 2020", "May 2021") #sets colnames to categorical colnames
everyone_final <- rev(tibble::rownames_to_column(everyone_final, var="Measure"))

View(everyone_final)


#plot for undergrads

likert(Measure ~ ., data = undergrad_final, ylab=NULL,
       ReferenceZero=3, as.percent=TRUE,
       positive.order=TRUE,
       main = list("Undergraduate Student Interest By Date",x=unit(.5, "npc")),
       sub= list("Likelihood Rating",x=unit(.5, "npc")),
       xlim=c(-60,-40,-20,0,20,40,60,80),
       strip=FALSE,
       par.strip.text=list(cex=.2))

#plot for master's students

likert(Measure ~ ., data = masters_final, ylab=NULL,
       ReferenceZero=3, as.percent=TRUE,
       positive.order=TRUE,
       main = list("Master's Student Interest By Date",x=unit(.5, "npc")),
       sub= list("Likelihood Rating",x=unit(.5, "npc")),
       xlim=c(-60,-40,-20,0,20,40,60,80),
       strip=FALSE,
       par.strip.text=list(cex=.2))

#plot for phd students

likert(Measure ~ ., data = phd_final, ylab=NULL,
       ReferenceZero=3, as.percent=TRUE,
       positive.order=TRUE,
       main = list("Doctoral Student Interest By Date",x=unit(.5, "npc")),
       sub= list("Likelihood Rating",x=unit(.5, "npc")),
       xlim=c(-60,-40,-20,0,20,40,60,80),
       strip=FALSE,
       par.strip.text=list(cex=.2))

#plot for everyone

likert(Measure ~ ., data = rev(everyone_final), ylab=NULL,
       ReferenceZero=3, as.percent=TRUE,
       positive.order=TRUE,
       main = list("All Interest By Date",x=unit(.5, "npc")),
       sub= list("Likelihood Rating",x=unit(.5, "npc")),
       xlim=c(-60,-40,-20,0,20,40,60,80),
       strip=FALSE,
       par.strip.text=list(cex=.2))

#Spineplot replacement

#Aggregate yes/no responses for an All column
dat_VAll <- dat
for (i in 1:nrow(dat)) {
  if (dat_VAll$VYInPerson[i] == "") {
    dat_VAll$VYInPerson[i] <- dat$VN_InPerson[i]
  }
}


ranks_final <- as.data.frame(rbind(VYes_VYperson_cumu[2:4,], 
                                   VUnsure_VYperson_cumu[2:4,], 
                                   VNo_VNperson_cumu[2:4,],
                                   everyone_Vperson_cumu[2:4,]))

colnames(ranks_final) <- factor(c("Response", "Freq", "Status"))

x <- 0
i <- 1
while (i < 11) {
  x <- VYperson_final$Freq[i+2]
  VYperson_final$pos[i] <- x/2
  VYperson_final$FreqAdjusted[i] <- VYperson_final$Freq[i+2]
  y = x + (VYperson_final$Freq[i + 1])/2
  VYperson_final$pos[i + 1] <- y
  VYperson_final$FreqAdjusted[i+1] <- VYperson_final$Freq[i+1]
  z = y + (VYperson_final$Freq[i + 1])/2 + (VYperson_final$Freq[i])/2
  VYperson_final$pos[i + 2] <- z
  VYperson_final$FreqAdjusted[i+2] <- VYperson_final$Freq[i]
  i = i + 3
}

vYesPlot <- ggplot() + theme_bw() + geom_bar(aes(y = Freq, x = Status, fill = Response), data = VYperson_final,
                                             stat="identity")

vYesPlot <- vYesPlot + geom_text(data=VYperson_final, aes(x = Status, y = pos, 
                                                          label = paste0(FreqAdjusted,"%")), size=4)
vYesPlot <- vYesPlot + theme(legend.position="bottom", legend.direction="horizontal",
                             legend.title = element_blank())

vYesPlot <- vYesPlot + labs(x="Total Student Interest in Virtual Ceremony", y="Percent of Respondants") +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  ggtitle("Expressed Interest in an In-Person Ceremony")

vYesPlot <- vYesPlot + scale_fill_manual(values=vPlot_fill)

vYesPlot

everyone_Virtual_cumu <- rev(data.frame(round(table(dat$Virtual) * 100 / sum(table(dat$Virtual)))))
everyone_Virtual_cumu$Status <- c("All", "All", "All")

#------------Ranked Choice Stuff------------------

test <- select(dat, c(Status, Rank.All_1:Rank.All_5))
test <- filter(test,!(Status==""))
test <- melt(test, id.vars = c("Status"))
testtable <- as.data.frame(table(test))
output <- data.frame()
k <- 1
for (i in 1:100) {
  if ((i-1)%%4!=0) {
    output[k,1:ncol(testtable)] <- testtable[i,1:ncol(testtable)]
    k=k+1
  }
}
output$value <- as.factor(output$value)
output$percent <- output$Freq
test <- arrange(output, Status, variable)

for (i in 0:(((nrow(test))/5)-1)) {
  i = 5*i
  sum <- 0
  for (j in 1:5){
    sum <- sum + test$Freq[i+j]
  }
  for (k in 1:5) {
    pSum <- 0
    test$percent[i+k] <- round((test$Freq[i+k] / sum) * 100)
    pSum = pSum + test$percent[i+k]
  }
}

for (i in 0:14) {
  i = 5*i
  v <- test$percent[i+1]/2
  test$pos[i + 1] <- v/2
  
  w = v + (test$percent[i + 2]/4)
  test$pos[i + 2] <- w
  
  x = w + (test$percent[i + 3])/4 + (test$percent[i + 2]/4)
  test$pos[i + 3] <- x
  
  y = x + (test$percent[i + 4])/4 + (test$percent[i + 3]/4)
  test$pos[i + 4] <- y
  
  z = y + (test$percent[i + 5])/4 + (test$percent[i + 4]/4)
  test$pos[i + 5] <- z
}

test$pos = 100.5 - (test$pos * 2)

test$value <- factor(test$value, levels = c("1","2","3","4","5"),
                     labels = c("Ranked First",
                                "Ranked Second",
                                "Ranked Third",
                                "Ranked Fourth",
                                "Ranked Last"))

test$variable <- factor(test$variable, levels = c("Rank.All_1", 
                                                  "Rank.All_2", 
                                                  "Rank.All_4", 
                                                  "Rank.All_5", 
                                                  "Rank.All_3"), 
                        labels = c("Q1",
                                   "Q2",
                                   "Q5",
                                   "Q4",
                                   "Q3"))


test2 <- select(dat, c(Rank.All_1:Rank.All_5))
test2$Status[1:nrow(test2)] <- "All"
test2 <- melt(test2, id.vars = c("Status"))
test2table <- as.data.frame(table(test2))
output2 <- test2table
output2$value <- as.factor(output2$value)
output2$percent <- output2$Freq
test2 <- arrange(output2, variable)

for (i in 0:(((nrow(test2))/5)-1)) {
  i = 5*i
  sum <- 0
  for (j in 1:5){
    sum <- sum + test2$Freq[i+j]
  }
  for (k in 1:5) {
    pSum <- 0
    test2$percent[i+k] <- round((test2$Freq[i+k] / sum) * 100)
    pSum = pSum + test2$percent[i+k]
  }
}
test2$pos <- test2$percent
for (i in 0:4) {
  i = 5*i
  v <- test2$percent[i+1]/2
  test2$pos[i + 1] <- v/2
  print(v)
  
  w = v + (test2$percent[i + 2]/4)
  test2$pos[i + 2] <- w
  
  x = w + (test2$percent[i + 3])/4 + (test2$percent[i + 2]/4)
  test2$pos[i + 3] <- x
  
  y = x + (test2$percent[i + 4])/4 + (test2$percent[i + 3]/4)
  test2$pos[i + 4] <- y
  
  z = y + (test2$percent[i + 5])/4 + (test2$percent[i + 4]/4)
  test2$pos[i + 5] <- z
}

test2$pos = 100.5 - (test2$pos * 2)

test2$value <- factor(test2$value, levels = c("1","2","3","4","5"),
                      labels = c("Ranked First",
                                 "Ranked Second",
                                 "Ranked Third",
                                 "Ranked Fourth",
                                 "Ranked Last"))

test2$variable <- factor(test2$variable, levels = c("Rank.All_1", 
                                                    "Rank.All_2", 
                                                    "Rank.All_4", 
                                                    "Rank.All_5", 
                                                    "Rank.All_3"), 
                         labels = c("Q1",
                                    "Q2",
                                    "Q5",
                                    "Q4",
                                    "Q3"))

test3 <- rbind(test,test2)

fiveColor <- c("#64fac5", "#a7fa64", "#e8df5a", "#de8540", "#c95140")

plot <- ggplot() +
  geom_bar(data=test3, aes(y = percent, x = variable, fill = value), stat="identity",
           position='stack') +
  theme_bw() +
  facet_grid( ~ Status)+
  geom_text(data=test3, aes(x = variable, y = pos,
                           label = paste0(percent,"%")), size=3) +
  labs(x="Option Categorization", y="Percent Per Rank") +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  ggtitle("Rankings of Various Graduation Dates (%)") +
  theme(legend.position="right", legend.direction="vertical",
        legend.title = element_blank()) +
  scale_fill_manual(values=fiveColor)

plot

#-------------Waffle Plot for Guest Respondents v Nonrespondents-----------------

test <- (sum(is.na(dat$X820guests))) + (sum(is.na(dat$X1220guests))) + (sum(is.na(dat$X521guests)))
test = round(test / 3)

guest_totals <- c(test,nrow(dat) - test) / 2

  val_names <- sprintf("%s (%s)", c("Did not respond","Did respond"), scales::percent(round(guest_totals/sum(guest_totals), 2)))
names(guest_totals) <- val_names
guestWaffle <- waffle(guest_totals, rows=10, colors = c("#F18775", "#81A5D8"), title = "Respondents for Guest Number Predictions", xlab = "1 square = two people")
guestWaffle

#------------------------GUEST NUMBER PREDS----------------

#What would people prefer as grads, undergrads, etc? THe sum of projected guests plus the 

GuestsAug20 <- sum(dat$X820guests,na.rm = TRUE) + nrow(dat[!((dat$X820guests == "") | is.na(dat$X820guests)),])
GuestsDec20 <- sum(dat$X1220guests,na.rm = TRUE) + nrow(dat[!((dat$X1220guests == "") | is.na(dat$X1220guests)),])
GuestsMay21 <- sum(dat$X521guests,na.rm = TRUE) + nrow(dat[!((dat$X521guests == "") | is.na(dat$X521guests)),])

#combine them into a vector (might save time later)
guestVector <- c(GuestsAug20,GuestsDec20,GuestsMay21)
guestVector  

#-----------------BOX PLOT FOR GUEST PREDICTIONS-----------------------------
dat_guest <- dat
dat_guest <- dat_guest[!(dat_guest$Status==""),]
dat_guest <-select(dat_guest, Status, X820guests, X1220guests, X521guests)
dat_guest <- melt(data = dat_guest, id.vars = c("Status"))

dat_guest_respondents <- as.data.frame(table(dat_guest, useNA = "ifany"))
dat_guest_respondents <- dat_guest_respondents[!(dat_guest_respondents$Status==""),]

dgrn <- data.frame(as.numeric(dat_guest_respondents$value),
                   as.numeric(dat_guest_respondents$Freq))

dgrn_na <- sum(dgrn$as.numeric.dat_guest_respondents.Freq.[
  is.na(dgrn$as.numeric.dat_guest_respondents.value)])

dgrn_rest <- sum(dgrn$as.numeric.dat_guest_respondents.Freq.) - dgrn_na


guest_totals <- c('Did Not Respond' = dgrn_na, 'Did Respond' = dgrn_rest)

dat_guest_noNA <- dat_guest[!is.na(dat_guest$value),]

levels(dat_guest_noNA$variable) <- c("Aug 2020", "Dec 2020", "May 2021")

ggplot(dat_guest_noNA, aes(x=value, y=variable)) + 
  geom_boxplot(
    
    # custom boxes
    color="#4F4F4F",
    fill=c("#81A5D8","#F18775","#E9BA65"),
    alpha=0.8,
    
    # Notch?
    notch=TRUE,
    notchwidth = 0.6,
    
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size= 1,
    outlier.shape = 19,
    
    show.legend = TRUE,
    orientation = "y"
    
    
  ) + theme_bw() +
  
  xlab("Reported Number of Guests") + 
  ylab("")




