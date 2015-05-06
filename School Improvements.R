setwd("~/Desktop/Public Data Sets/School Improvement Grants")
library(ggplot2)
library(gridExtra)
library(dplyr)


grants <- read_csv("School Improvement Grants.csv") 
blank <- which(grants$`Model Selected` == "") #removing the NA transformations. Also has NA for award amount
grants <- grants[-blank,]
grants <- na.omit(grants) #some cities did not report any award amount so removing them for computation

grants <- grants %>% rename(`Award Amount` = `2010/11/Award Amount`)
grants[which(is.na(grants$`Award Amount`)),] %>% select(`City`, `Award Amount`)

#---------------Finding top 10 cities who were granted most money


cities <- grants %>% group_by(City,State) %>% summarise(Total_Award = sum(`Award Amount`)) %>% ungroup() %>% arrange(desc(Total_Award))
top.ten <- cities[1:10,]
  
top.ten$Total_Award <- top.ten$Total_Award / 1000000  #dividing by 1 mil for better display on graph
top.ten$Total_Award <- round(top.ten$Total_Award,2)

#-----------------------


#-----------Importing census recorded data for top 10 cities

#for this data set, I manually entered data into excel for the top 10 cities using data from census.gov
census <- read_csv("Top 10 city census.csv") 

#--------------


#-----------Bar graph of total award money by city
  
p1 <- ggplot(data = top.ten, aes(x = reorder(City,Total_Award), y = Total_Award, fill = State)) + geom_bar(stat = "identity", position = "dodge", color = "black") + 
  xlab("City") + ylab("Amount Awarded (in Millions)") + ggtitle("Top 10 cities in the U.S Awarded the Most Amount of Grant Money for Schools in 2010") +
  geom_text(aes(label = Total_Award), vjust = -.2) + 
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, color = "black", size = 10),
        axis.title.x = element_text(face = "italic", size = 16),
        axis.title.y = element_text(face = "italic", size = 16),
        plot.title = element_text(face = "bold.italic", size = 18, color = "black")) + 
  scale_fill_manual(values = c(MN = "#99CCFF", CO = "#9999CC", DC = "#99CCCC", FL = "#3366CC", PA = "#003399", MO = "#006699", OH = "#6699CC"))
p1 <- arrangeGrob(p1, sub = textGrob(paste("Data taken from Data.gov"), x = 0, hjust = -0.1, vjust = 0.1, gp = gpar(fontsize = 14, col = "#336699")))
p1 <- arrangeGrob(p1, sub = textGrob("Note: missing data in data set", x = 1, hjust = 1.1, vjust = -1, gp = gpar(fontsize = 14, col = "#336699")))
p1
#---------------------

#--------Using U.S census data to look at poverty level by cities from above


#14.5% is below poverty level in US
p2 <- ggplot(data = census, aes(x = City, y = `Poverty Level`, fill = State)) + geom_bar(stat = "identity", color = "black") + 
  scale_x_discrete(limits = census$City) + xlab("City") + ylab("Percent") + ggtitle("Poverty Level by City") + 
  geom_hline(yintercept = 14.5, size = 1) + scale_y_continuous(breaks = c(0,10,14.5,20,30)) + annotate("text", x = 0, y = 32, label = ("Percentage of People in  US Below Poverty Level: 14.5%"), size = 6,hjust = 0) + 
  geom_text(aes(label = `Poverty Level`), vjust = -.2) + 
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, color = "black", size = 10), 
        axis.title.x = element_text(face = "italic", size = 16), 
        axis.title.y = element_text(face = "italic", size = 16), 
        plot.title = element_text(face = "bold.italic", size = 18, color = "black")) + 
  scale_fill_manual(values = c(MN = "#99CCFF", CO = "#9999CC", DC = "#99CCCC", FL = "#3366CC", PA = "#003399", MO = "#006699", OH = "#6699CC"))
p2 <- arrangeGrob(p2, sub = textGrob(paste("Data taken from Census.gov"), x = 0, hjust = -0.1, vjust = 0.1, gp = gpar(fontsize = 14, col = "#336699")))
  
p2
#------------------------


#---------Using census data to look at percentage bachelor's degree holders by cities above



#percent with bachelor's degrees of higher in US is 28.8%

p3 <- ggplot(data = census, aes(x = City, y = `Bach.City`, fill = State)) + geom_bar(stat = "identity", color = "black") + 
  scale_x_discrete(limits = census$City) + xlab("City") + ylab("Percent") + ggtitle("Percentage Bachelor's Degree Holders by City") + 
  geom_hline(yintercept = 28.8, size = 1) + scale_y_continuous(breaks = c(0,10,20,28.8, 30, 40, 50)) + annotate("text", x = 5, y = 50, label = ("Percentage of People in  US with Bachelor's Degrees: 28.8%"), size = 6,hjust = 0) + 
  geom_text(aes(label = `Bach.City`), vjust = -.2) + 
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, color = "black", size = 10), 
        axis.title.x = element_text(face = "italic", size = 16), 
        axis.title.y = element_text(face = "italic", size = 16), 
        plot.title = element_text(face = "bold.italic", size = 18, color = "black")) + 
  scale_fill_manual(values = c(MN = "#99CCFF", CO = "#9999CC", DC = "#99CCCC", FL = "#3366CC", PA = "#003399", MO = "#006699", OH = "#6699CC"))
p3 <- arrangeGrob(p3, sub = textGrob(paste("Data taken from Census.gov"), x = 0, hjust = -0.1, vjust = 0.1, gp = gpar(fontsize = 14, col = "#336699")))

p3

#------------------------
