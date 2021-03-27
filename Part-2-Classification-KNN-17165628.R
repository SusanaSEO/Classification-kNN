#Part 2 - Classification - kNN
#Susana Reche Rodriguez
#Student Number: 17165628
#Setting the seed
#
#
#
set.seed(333)
#
#Importing needed libraries
library(caret)
library(class)
library(psych)
library(fastDummies)
library(dummies)
library(naniar)
library(cowplot)
library(ggpubr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
require(caTools)
library(ipred) 


#IMPORT DATA SET 
################################################################################################
################################################################################################
#Dataset downloaded from http://archive.ics.uci.edu/ml/datasets/Online+Shoppers+Purchasing+Intention+Dataset
#Import data set
online_shoppers_intention <- read.csv("online_shoppers_intention.csv", header = TRUE)








#FIRST INSPECTION AND CHECK OF NA VALUES
################################################################################################
################################################################################################
#Check structure of the file
str(online_shoppers_intention)

#Check first rows
head(online_shoppers_intention)

#Checking the summary
summary(online_shoppers_intention) #the variables have very different scales so will need to be normalized

#Check if there is any na values
sapply(online_shoppers_intention, function(x) sum(is.na(x))) #Doesn't seem to be any NA

#Check again for na values
sum(is.na(online_shoppers_intention))

#Check again with different function the missing values
online_shoppers_intention[!complete.cases(online_shoppers_intention), ] #no missing values


#Visualising if there is any null value #https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
vis_miss(online_shoppers_intention)







#FIRST DATA TRANSFORMATION AND EXPLORATION CATEGORICAL VARIABLES
################################################################################################
################################################################################################

#Transforming as factor Revenue
online_shoppers_intention$Revenue <- as.factor(online_shoppers_intention$Revenue)

#Checking number of records with and without purchase
online_shoppers_intention %>%
  group_by(Revenue) %>%
  summarize(n = n())

#Visualizing Revenue
revenue <- ggplot(online_shoppers_intention, aes(x=Revenue)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Revenue", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))
  


#Transforming weekend from FALSE/TRUE to 0/1
online_shoppers_intention$Weekend <- ifelse(online_shoppers_intention$Weekend == "TRUE", 1, 0)

#Transforming as factor Weekend
#online_shoppers_intention$Weekend <- as.factor(online_shoppers_intention$Weekend, levels = c("0","1"))

#Check new structure of Weekend
str(online_shoppers_intention$Weekend)

#Visualizing Weekend
weekend <- ggplot(online_shoppers_intention, aes(x=Weekend)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Weekend", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))



#Transforming as factor Operating Systems
online_shoppers_intention$OperatingSystems <- as.factor(online_shoppers_intention$OperatingSystems)

#Checking unique Operating Systems
unique(online_shoppers_intention$OperatingSystems)

#Visualizing Operating System
operatingsystems <- ggplot(online_shoppers_intention, aes(x=OperatingSystems)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Operating Systems", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))



#Transforming as factor Browser
online_shoppers_intention$Browser <- as.factor(online_shoppers_intention$Browser)

#Checking unique Browsers
unique(online_shoppers_intention$Browser)

#Visualizing browser
browser <- ggplot(online_shoppers_intention, aes(x=Browser)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Browser", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))



#Transforming as factor Region
online_shoppers_intention$Region <- as.factor(online_shoppers_intention$Region)

#Checking unique Regions
unique(online_shoppers_intention$Region)

#Visualizing Region
region <- ggplot(online_shoppers_intention, aes(x=Region)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Region", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))



#Transforming as factor Traffic Type
online_shoppers_intention$TrafficType <- as.factor(online_shoppers_intention$TrafficType)

#Checking unique Traffic Type
unique(online_shoppers_intention$TrafficType)

#Visualizing Traffic Type
traffictype <- ggplot(online_shoppers_intention, aes(x=TrafficType)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Traffic Type", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))



#Transforming as factor Visitor Type
online_shoppers_intention$VisitorType <- as.factor(online_shoppers_intention$VisitorType)

#Checking unique Visitor Type
unique(online_shoppers_intention$VisitorType)

#Visualizing Visitor Type
visitortype <- ggplot(online_shoppers_intention, aes(x=VisitorType)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Visitor Type", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))


#Check different Months
unique(online_shoppers_intention$Month)

#Rename June into Jun
online_shoppers_intention$Month[online_shoppers_intention$Month == "June"] <- "Jun"

#Check different Months
unique(online_shoppers_intention$Month)

#Converting to factor Month
online_shoppers_intention$Month <- factor(online_shoppers_intention$Month,
                                             levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

#Check different Months
unique(online_shoppers_intention$Month)

#Checking possible empty values
sum(is.na(online_shoppers_intention$Month))

#Visualizing Month
month <- ggplot(online_shoppers_intention, aes(x=Month)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Month", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))



#Checking new structure
str(online_shoppers_intention)


#Combine all plots together
ggarrange(month,operatingsystems,browser,region,traffictype,visitortype,weekend,revenue)


#DATA EXPLORATION NUMERICAL VARIABLES
################################################################################################
################################################################################################


administrative <- ggplot(online_shoppers_intention, aes(x= "Administrative",y=Administrative)) +
  geom_jitter(alpha = 0.2,color="#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Administrative")+
  theme(plot.title = element_text(hjust = 0.5))
  

administrative_duration <- ggplot(online_shoppers_intention, aes(x= "Administrative_Duration",y=Administrative_Duration)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Administrative Duration")+
  theme(plot.title = element_text(hjust = 0.5))


informational <- ggplot(online_shoppers_intention, aes(x= "Informational",y=Informational)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Informational")+
  theme(plot.title = element_text(hjust = 0.5))


informational_duration <- ggplot(online_shoppers_intention, aes(x= "Informational_Duration",y=Informational_Duration)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Informational Duration")+
  theme(plot.title = element_text(hjust = 0.5))


productrelated <- ggplot(online_shoppers_intention, aes(x= "ProductRelated",y=ProductRelated)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Product Related")+
  theme(plot.title = element_text(hjust = 0.5))

productrelated_duration <- ggplot(online_shoppers_intention, aes(x= "ProductRelated_Duration",y=ProductRelated_Duration)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Product Related Duration")+
  theme(plot.title = element_text(hjust = 0.5))


bouncerates <- ggplot(online_shoppers_intention, aes(x= "BounceRates",y=BounceRates)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Bounce Rates")+
  theme(plot.title = element_text(hjust = 0.5))

exitrates <- ggplot(online_shoppers_intention, aes(x= "ExitRates",y=ExitRates)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Exit Rates")+
  theme(plot.title = element_text(hjust = 0.5))

pagevalues <- ggplot(online_shoppers_intention, aes(x= "PageValues",y=PageValues)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Page Values")+
  theme(plot.title = element_text(hjust = 0.5))


specialday <- ggplot(online_shoppers_intention, aes(x= "SpecialDay",y=SpecialDay)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Special Day")+
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(administrative,administrative_duration,informational,informational_duration,productrelated,productrelated_duration,
          bouncerates,exitrates,pagevalues,specialday)

str(online_shoppers_intention)

#ONLY WITH PURCHASE
################################################################################################
################################################################################################
online_shoppers_intention_revenue <- subset(online_shoppers_intention, online_shoppers_intention$Revenue == "TRUE")



weekend_revenue <- ggplot(online_shoppers_intention_revenue, aes(x=Weekend)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Weekend", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))


operatingsystems_revenue <- ggplot(online_shoppers_intention_revenue, aes(x=OperatingSystems)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Operating Systems", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))


browser_revenue <- ggplot(online_shoppers_intention_revenue, aes(x=Browser)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Browser", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))

region_revenue <- ggplot(online_shoppers_intention_revenue, aes(x=Region)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Region", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))


traffictype_revenue <- ggplot(online_shoppers_intention_revenue, aes(x=TrafficType)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Traffic Type", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))


visitortype_revenue <- ggplot(online_shoppers_intention_revenue, aes(x=VisitorType)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Visitor Type", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))


month_revenue <- ggplot(online_shoppers_intention_revenue, aes(x=Month)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Month", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))



#Checking new structure
str(online_shoppers_intention_revenue)


#Combine all plots together
ggarrange(month_revenue,operatingsystems_revenue,browser_revenue,region_revenue,traffictype_revenue,visitortype_revenue,weekend_revenue)




administrative_revenue <- ggplot(online_shoppers_intention_revenue, aes(x= "Administrative",y=Administrative)) +
  geom_jitter(alpha = 0.2,color="#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Administrative")+
  theme(plot.title = element_text(hjust = 0.5))


administrative_duration_revenue <- ggplot(online_shoppers_intention_revenue, aes(x= "Administrative_Duration",y=Administrative_Duration)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Administrative Duration")+
  theme(plot.title = element_text(hjust = 0.5))


informational_revenue <- ggplot(online_shoppers_intention_revenue, aes(x= "Informational",y=Informational)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Informational")+
  theme(plot.title = element_text(hjust = 0.5))


informational_duration_revenue <- ggplot(online_shoppers_intention_revenue, aes(x= "Informational_Duration",y=Informational_Duration)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Informational Duration")+
  theme(plot.title = element_text(hjust = 0.5))


productrelated_revenue <- ggplot(online_shoppers_intention_revenue, aes(x= "ProductRelated",y=ProductRelated)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Product Related")+
  theme(plot.title = element_text(hjust = 0.5))

productrelated_duration_revenue <- ggplot(online_shoppers_intention_revenue, aes(x= "ProductRelated_Duration",y=ProductRelated_Duration)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Product Related Duration")+
  theme(plot.title = element_text(hjust = 0.5))


bouncerates_revenue <- ggplot(online_shoppers_intention_revenue, aes(x= "BounceRates",y=BounceRates)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Bounce Rates")+
  theme(plot.title = element_text(hjust = 0.5))

exitrates_revenue <- ggplot(online_shoppers_intention_revenue, aes(x= "ExitRates",y=ExitRates)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Exit Rates")+
  theme(plot.title = element_text(hjust = 0.5))

pagevalues_revenue <- ggplot(online_shoppers_intention_revenue, aes(x= "PageValues",y=PageValues)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Page Values")+
  theme(plot.title = element_text(hjust = 0.5))


specialday_revenue <- ggplot(online_shoppers_intention_revenue, aes(x= "SpecialDay",y=SpecialDay)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Special Day")+
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(administrative_revenue,administrative_duration_revenue,informational_revenue,informational_duration_revenue,productrelated_revenue,
          productrelated_duration_revenue,bouncerates_revenue,exitrates_revenue,pagevalues_revenue,specialday_revenue)





#ONLY WITHOUT PURCHASE
################################################################################################
################################################################################################
online_shoppers_intention_no_revenue <- subset(online_shoppers_intention, online_shoppers_intention$Revenue == "FALSE")



weekend_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x=Weekend)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Weekend", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))


operatingsystems_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x=OperatingSystems)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Operating Systems", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))


browser_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x=Browser)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Browser", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))

region_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x=Region)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Region", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))


traffictype_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x=TrafficType)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Traffic Type", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))


visitortype_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x=VisitorType)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Visitor Type", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))


month_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x=Month)) +
  geom_bar(alpha = 0.5, fill = "#008081")+
  labs(title = "Month", x = element_blank(), y = "Count")+
  theme(plot.title = element_text(hjust = 0.5))



#Checking new structure
str(online_shoppers_intention_no_revenue)


#Combine all plots together
ggarrange(month_no_revenue,operatingsystems_no_revenue,browser_no_revenue,region_no_revenue,traffictype_no_revenue,visitortype_no_revenue,weekend_no_revenue)




administrative_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x= "Administrative",y=Administrative)) +
  geom_jitter(alpha = 0.2,color="#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Administrative")+
  theme(plot.title = element_text(hjust = 0.5))


administrative_duration_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x= "Administrative_Duration",y=Administrative_Duration)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Administrative Duration")+
  theme(plot.title = element_text(hjust = 0.5))


informational_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x= "Informational",y=Informational)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Informational")+
  theme(plot.title = element_text(hjust = 0.5))


informational_duration_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x= "Informational_Duration",y=Informational_Duration)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Informational Duration")+
  theme(plot.title = element_text(hjust = 0.5))


productrelated_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x= "ProductRelated",y=ProductRelated)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Product Related")+
  theme(plot.title = element_text(hjust = 0.5))

productrelated_duration_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x= "ProductRelated_Duration",y=ProductRelated_Duration)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Product Related Duration")+
  theme(plot.title = element_text(hjust = 0.5))


bouncerates_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x= "BounceRates",y=BounceRates)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Bounce Rates")+
  theme(plot.title = element_text(hjust = 0.5))

exitrates_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x= "ExitRates",y=ExitRates)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Exit Rates")+
  theme(plot.title = element_text(hjust = 0.5))

pagevalues_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x= "PageValues",y=PageValues)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Page Values")+
  theme(plot.title = element_text(hjust = 0.5))


specialday_no_revenue <- ggplot(online_shoppers_intention_no_revenue, aes(x= "SpecialDay",y=SpecialDay)) +
  geom_jitter(alpha = 0.2, color = "#008081") +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(),y = element_blank(), title ="Special Day")+
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(administrative_no_revenue,administrative_duration_no_revenue,informational_no_revenue,informational_duration_no_revenue,productrelated_no_revenue,
          productrelated_duration_no_revenue,bouncerates_no_revenue,exitrates_no_revenue,pagevalues_no_revenue,specialday_no_revenue)



#PREPARING DATA FOR kNN
################################################################################################
################################################################################################
str(online_shoppers_intention)
summary(online_shoppers_intention)


#NORMALIZING NUMERICAL VARIABLES
################################################################################################
################################################################################################
#Creating a fucntion to be able to normalize the data
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }


#Creating a new data set to use for normalizing the data
online_shoppers_intention_norm<- online_shoppers_intention

#Normalizing the numerical variables
online_shoppers_intention_norm[,1:10]<- as.data.frame(lapply(online_shoppers_intention_norm[,1:10], normalize)) 

#Ensuring all the numerical variables were normalized
summary(online_shoppers_intention_norm)
str(online_shoppers_intention_norm)







#TRANSFORMING CATEGORICAL VARIABLES
################################################################################################
################################################################################################
#Generate dummy code for Month (dummy code https://quantdev.ssri.psu.edu/sites/qdev/files/kNN_tutorial.html)
Month <- as.data.frame(dummy.code(online_shoppers_intention_norm$Month))

#Check resulting new names
names(Month)

#Generate dummy code for Visitor
VisitorType <- as.data.frame(dummy.code(online_shoppers_intention_norm$VisitorType))

#Check resulting new names
names(VisitorType)

#Rename "other" with "Other Visitor"
VisitorType <- VisitorType %>% dplyr::rename("Other_Visitor" = "Other")

#Check resulting new names
names(VisitorType)

#Generate dummy code for Traffic Type
TrafficType <- as.data.frame(dummy.code(online_shoppers_intention_norm$TrafficType))

#Check resulting new names
names(TrafficType)

#Add prefix to column names
colnames(TrafficType) <- paste("TrafficType", colnames(TrafficType), sep = "_")

#Check resulting new names
names(TrafficType)

#Generate dummy code for Region
Region <- as.data.frame(dummy.code(online_shoppers_intention_norm$Region))

#Check resulting new names
names(Region)

#Add prefix to column names
colnames(Region) <- paste("Region", colnames(Region), sep = "_")

#Check resulting new names
names(Region)

#Generate dummy code for Browser
Browser <- as.data.frame(dummy.code(online_shoppers_intention_norm$Browser))

#Check resulting new names
names(Browser)

#Add prefix to column names
colnames(Browser) <- paste("Browser", colnames(Browser), sep = "_")

#Check resulting new names
names(Browser)

#Generate dummy code for Operating Systems
OperatingSystems <- as.data.frame(dummy.code(online_shoppers_intention_norm$OperatingSystems))

#Check resulting new names
names(OperatingSystems)

#Add prefix to column names
colnames(OperatingSystems) <- paste("OperatingSystems", colnames(OperatingSystems), sep = "_")

#Check resulting new names
names(OperatingSystems)

#Converting Weekend to numerical
online_shoppers_intention_norm$Weekend <- as.numeric(online_shoppers_intention_norm$Weekend)
online_shoppers_intention_norm$Weekend <- ifelse(online_shoppers_intention_norm$Weekend == "2", 1, 0)
#Checking new structure
str(online_shoppers_intention_norm)

#Remove old variables
online_shoppers_intention_norm <- online_shoppers_intention_norm %>% select(-one_of(c("Month","VisitorType","TrafficType","Region","Browser","OperatingSystems")))

#Checking new structure
str(online_shoppers_intention_norm)

#Combine new variables
online_shoppers_intention_norm <- cbind(online_shoppers_intention_norm,Month,VisitorType,TrafficType,Region,Browser,OperatingSystems)

#Checking new structure
str(online_shoppers_intention_norm)

#Checking Summary
summary(online_shoppers_intention_norm)

#Check NAs
sapply(online_shoppers_intention_norm, function(x) sum(is.na(x))) 




#DIVIDING THE DATA SET INTO TRAINING AND VALIDATION
################################################################################################
################################################################################################
#Setting the seed again to confirm
set.seed(333)
#Create data partition to be able to train the model as kNN is supervised
validation_index <- createDataPartition(online_shoppers_intention_norm$Revenue, p=0.80, list=FALSE)
# 80% of data for training purposes
training <- online_shoppers_intention_norm[validation_index,-12]
# 20% of the data for validation
validation <- online_shoppers_intention_norm[-validation_index,-12]

#Extract output label
training_label <- online_shoppers_intention_norm[validation_index,12] 
#Extract output label
validation_label <- online_shoppers_intention_norm[-validation_index,12]

#Checking structure of label vectors
str(training_label)
str(validation_label)

#Checking the number of 0 and 1 in each of them
length(grep("TRUE",training_label))
length(grep("TRUE",validation_label))

length(grep("FALSE",training_label))
length(grep("FALSE",validation_label))

#OPTIMAL K
################################################################################################
################################################################################################

i = 1                          # declaration to initiate for loop

k.optm = 1                     # declaration to initiate for loop

for (i in 1:100){ 
  knn.mod <-  knn(train=training, test=validation, cl=training_label, k=i)
  k.optm[i] <- 100 * sum(validation_label == knn.mod)/NROW(training_label)
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}

max(k.optm)

#Accuracy plot
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level", main = "Accuracy Plot", col = "#008081")



#CLASSIFICATION - kNN
################################################################################################
################################################################################################
#With class
online_shoppers_intention_pred <- knn(training, validation, cl= training_label, k = 13)



#MODEL EVALUATION
################################################################################################
################################################################################################
#confusion matrix

confusionMatrix(online_shoppers_intention_pred, validation_label,positive = "TRUE")



#DECISION TREE
################################################################################################
################################################################################################
#Setting the seed again to confirm
set.seed(333)
#To work with the decision tree all the data needs to be together with the label.
#A new partition is created
# 80% of data for training purposes
training_tree <- online_shoppers_intention_norm[validation_index,]

# 20% of the data for validation
validation_tree <- online_shoppers_intention_norm[-validation_index,]

str(training_tree)

#Creating the decision tree model
online_shoppers_intention_tree  <- rpart(Revenue ~ .,
                     data = training_tree,
                     method = "class")

#Checking the decision tree
online_shoppers_intention_tree

#Visualizing the decision tree
prp(online_shoppers_intention_tree)

#Visualizing the decision tree
rpart.plot(online_shoppers_intention_tree, extra = 106)

#Using the decision tree to predict the class of the validation segment
predicted = predict(online_shoppers_intention_tree, validation_tree, type = "class")

#Confusion matrix decision tree
confusionMatrix(predicted, validation_tree$Revenue,positive = "TRUE")




#RANDOM FOREST
################################################################################################
################################################################################################
#The partition used will be the same as the decision tree
#Creating the ramdom forest model
online_shoppers_intention_random_forest <- randomForest(Revenue~.,data = training_tree)

#Using the random forest to predict the class of the validation segment
predicted_random_forest <- predict(online_shoppers_intention_random_forest, validation_tree)

#Confusion matrix random forest
confusionMatrix(predicted_random_forest, validation_tree$Revenue,positive = "TRUE")

#Undertanding the variables that contrinuted the most to the correct prediction
varImpPlot(online_shoppers_intention_random_forest, bg = "#008081", main="Contribution of variables to the Model")


