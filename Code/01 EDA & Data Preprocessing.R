getwd()
setwd("/Users/lu/Desktop/S3/ST5188/Customer/Data")

library(sf)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(lattice)
library(leafpop)
library(vapoRwave)
library(viridis)
library(dplyr)
library(htmlwidgets)
library(readxl)
library(tidyverse)
library(ggpubr)
library(treemapify)
library(networkD3)
library(outliers)
library(writexl)

# Load / import data
IBM <- read_excel('IBM_Telco_customer_churn.xlsx')

# Drop irrelevant columns
str(IBM)
IBMM = IBM[,-which(colnames(IBM) %in% c("CustomerID","Count","Country","State","City","Lat Long","Churn Value","Churn Score","CLTV","Churn Reason"))]
write.csv(IBMM,"IBM new.csv")

# Rename columns: No need

# Drop duplicate rows
IBMM[duplicated(IBMM)]

# Drop missing or NULL values
summary(is.na(IBMM))
which(is.na(IBMM$`Total.Charges`))
IBMMM<-IBMM[!(is.na(IBMM$`Total.Charges`)),]


# Univariate analysis: only small group of customers enjoyed the service
ggarrange(b1, b3, b4, b5, b6, b7, b8, b2, b9, labels = LETTERS[1:9],ncol = 3, nrow = 3)
b1<-ggplot(data = IBMMM) +geom_bar(mapping = aes(x = `Multiple.Lines`))
b2<-ggplot(data = IBMMM) +geom_bar(mapping = aes(x = `Internet.Service`))
b3<-ggplot(data = IBMMM) +geom_bar(mapping = aes(x = `Online.Security`))
b4<-ggplot(data = IBMMM) +geom_bar(mapping = aes(x = `Online.Backup`))
b5<-ggplot(data = IBMMM) +geom_bar(mapping = aes(x = `Device.Protection`))
b6<-ggplot(data = IBMMM) +geom_bar(mapping = aes(x = `Tech.Support`))
b7<-ggplot(data = IBMMM) +geom_bar(mapping = aes(x = `Streaming.TV`))
b8<-ggplot(data = IBMMM) +geom_bar(mapping = aes(x = `Streaming.Movies`))
b9<-ggplot(data = IBMMM) +geom_bar(mapping = aes(x = `Phone.Service`))

#inbalanced: slight 20-40; medium 1-20; high 0-1
bl1<-IBMMM %>% count(Gender)
bl1$n/7032
bl2<-IBMMM %>% count(`Senior.Citizen`)
bl2$n/7032
bl3<-IBMMM %>% count(`Partner`)
bl3$n/7032
bl4<-IBMMM %>% count(`Paperless.Billing`)
bl4$n/7032
bl4<-IBM %>% count(`Churn Reason`)
bl4<-bl4[c(1:20),]

self<-rbind(bl4[c(7:8),],bl4[14,])
competitor<-rbind(bl4[c(3:6),])
CurrentService<-rbind(bl4[c(1:2),],bl4[9,],bl4[c(10:13),],bl4[c(15:20),])

bl4$n/7032
pie(bl4$n/7032)
bl5<-IBMMM %>% count(`Churn.Label`)
bl5$n/7032
bl5
pie(bl5$n/7032)

#when we look into the churn reasons, 
#27.30% of customers terminated as they dissatisfied with attitude of support person, 
#represented as tag 2 in pie chart below. 
#strategy 3rd is better than 1st

# categorical
# create a treemap of marriage officials
plotdata <- IBMMM %>%count(Internet.Service )
plotdata <- IBMMM %>%count(Payment.Method)
plotdata <- IBMMM %>%count(Contract)
plotdata <- IBM %>%count(`Churn Reason`)
plotdata <- CurrentService %>%count(`Churn Reason`)
plotdata <- competitor %>%count(`Churn Reason`)

ggplot(plotdata, 
       aes(fill =Payment.Method, 
           area = n, 
           label = Payment.Method)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "Payment.Method") +
  theme(legend.position = "none")

ggplot(plotdata, 
       aes(fill =`Churn Reason`, 
           area = n, 
           label = `Churn Reason`)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "Churn Reason") +
  theme(legend.position = "none")

ggplot(plotdata, 
       aes(fill =`Churn Reason`, 
           area = n, 
           label = `Churn Reason`)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "Churn Reason") +
  theme(legend.position = "none")


# Determine / correct data types of columns
str(IBMMM)
head(IBMMM)
asNumeric <- function(x) as.numeric(factor(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.character)], asNumeric))
IBMMM_num <- factorsNumeric(IBMMM)

# Bivariate analysis: scatter(nonsense)/cor/slr
cor(IBMMM_num)

# Multi-variate analysis
m<-lm(Churn.Label~., data = IBMMM_num)
summary(m)


# sankey: Dependents ~ Contract ~ Payment Method～churn: most importance three factors
# A connection data frame is a list of flows with intensity for each flow
s1<-sum(IBMMM$Dependents == 'Yes'& IBMMM$Contract == 'Month-to-month')
s2<-sum(IBMMM$Dependents == 'Yes'& IBMMM$Contract == 'Two year')
s3<-sum(IBMMM$Dependents == 'Yes'& IBMMM$Contract == 'One year')
s4<-sum(IBMMM$Dependents == 'No'& IBMMM$Contract == 'Month-to-month')
s5<-sum(IBMMM$Dependents == 'No'& IBMMM$Contract == 'Two year')
s6<-sum(IBMMM$Dependents == 'No'& IBMMM$Contract == 'One year')
s7<-sum(IBMMM$Contract == 'Month-to-month'& IBMMM$`Payment.Method` == 'Electronic check')
s8<-sum(IBMMM$Contract == 'Month-to-month'& IBMMM$`Payment.Method` == 'Mailed check')
s9<-sum(IBMMM$Contract == 'Month-to-month'& IBMMM$`Payment.Method` == 'Credit card (automatic)')
s10<-sum(IBMMM$Contract == 'Month-to-month'& IBMMM$`Payment.Method` == 'Bank transfer (automatic)')
s11<-sum(IBMMM$Contract == 'Two year'& IBMMM$`Payment.Method` == 'Electronic check')
s12<-sum(IBMMM$Contract == 'Two year'& IBMMM$`Payment.Method` == 'Mailed check')
s13<-sum(IBMMM$Contract == 'Two year'& IBMMM$`Payment.Method` == 'Credit card (automatic)')
s14<-sum(IBMMM$Contract == 'Two year'& IBMMM$`Payment.Method` == 'Bank transfer (automatic)')
s15<-sum(IBMMM$Contract == 'One year'& IBMMM$`Payment.Method` == 'Electronic check')
s16<-sum(IBMMM$Contract == 'One year'& IBMMM$`Payment.Method` == 'Mailed check')
s17<-sum(IBMMM$Contract == 'One year'& IBMMM$`Payment.Method` == 'Credit card (automatic)')
s18<-sum(IBMMM$Contract == 'One year'& IBMMM$`Payment.Method` == 'Bank transfer (automatic)')
s19<-sum(IBMMM$Payment.Method=='Electronic check'&IBMMM$Churn.Label == 'Yes')
s20<-sum(IBMMM$Payment.Method=='Electronic check'&IBMMM$Churn.Label == 'No')
s21<-sum(IBMMM$Payment.Method=='Mailed check'&IBMMM$Churn.Label == 'Yes')
s22<-sum(IBMMM$Payment.Method=='Mailed check'&IBMMM$Churn.Label == 'No')
s23<-sum(IBMMM$Payment.Method=='Credit card (automatic)'&IBMMM$Churn.Label == 'Yes')
s24<-sum(IBMMM$Payment.Method=='Credit card (automatic)'&IBMMM$Churn.Label == 'No')
s25<-sum(IBMMM$Payment.Method=='Bank transfer (automatic)'&IBMMM$Churn.Label == 'Yes')
s26<-sum(IBMMM$Payment.Method=='Bank transfer (automatic)'&IBMMM$Churn.Label == 'No')
s<-c(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26)


links <- data.frame(
  source=c("Yes","Yes","Yes","No", "No", "No", "Month to month", "Month to month", "Month to month", "Month to month", "Two year", "Two year",  "Two year","Two year", "One year", "One year", "One year", "One year",'Electronic check','Electronic check','Mailed check','Mailed check', "Credit card", "Credit card", "Bank transfer", "Bank transfer"), 
  target=c("Month to month","Two year", "One year", "Month to month","Two year", "One year", "Electronic check", "Mailed check", "Credit card", "Bank transfer", "Electronic check", "Mailed check", "Credit card", "Bank transfer", "Electronic check", "Mailed check", "Credit card", "Bank transfer","Yes","No","Yes","No","Yes","No","Yes","No"), 
  value=s
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
#nodes <- data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())

nodes<-data.frame(name=c(c(as.character(links$source), as.character(links$target)) %>% unique(),"Yes","No"))

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
target1<-match(links$target, nodes$name)-1
links$IDtarget <- c(target1[1:18],9,10,9,10,9,10,9,10)

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes, fontSize = 16,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

# Detect outliers: https://www.statology.org/how-to-identify-influential-data-points-using-cooks-distance/
#http://r-statistics.co/Outlier-Treatment-With-R.html
#Cook’s Distance is an estimate of the influence of a data point. 
#It takes into account both the leverage and residual of each observation. 
#Cook’s Distance is a summary of how much a regression model changes when the ith observation is removed.

par(mfrow = c(2, 2))
plot(m)
summary(m)
cooksd <- cooks.distance(m)
influential <- cooksd[(cooksd > (4 * mean(cooksd, na.rm = TRUE)))]
influential
gap<-as.numeric(cooksd)-4*mean(cooksd, na.rm = TRUE)
within<-gap[gap<0]
outlier_order<-which(gap>0)
threshold<-which.min(abs(within))#vector of flooring and capping

IBMMM[outlier_order, ] <- IBMMM[threshold,]
write_xlsx(IBMMM, "IBMMM_replace_outliers.xlsx")

names_of_influential <- names(influential)
outliers <- IBMMM_num[names_of_influential,]
str(outliers)
IBMMM_num_without_outliers <- IBMMM_num %>% anti_join(outliers)
write_xlsx(IBMMM_num_without_outliers, "C:\\Users\\lu\\Desktop\\S3\\ST5188\\Customer\\Data\\IBM_without_outliers.xlsx")

par(mfrow = c(1, 1))
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")


IBMMM_num_replace_outliers<-factorsNumeric(IBMMM)
model2 <- lm(Churn.Label ~ ., data = IBMMM_num_replace_outliers)
summary(model2)
par(mfrow = c(2, 2))
plot(model2)
#0.4234



#The earth package implements variable importance based on Generalized cross validation (GCV), 
#number of subset models the variable occurs (nsubsets) and residual sum of squares (RSS).
library(earth)
regressor <- earth(Churn.Label~., data= IBMMM_num) # build model
ev <- evimp (regressor) # estimate variable importance
plot(ev)
print(ev)



# To examine the distribution of a continuous variable, use a histogram:
ggplot(data = IBMMM) +geom_histogram(mapping = aes(x = `Tenure Months`), binwidth = 0.5)

## set filter
smaller <- IBMMM %>% filter( MonthlyCharges > 28)
ggplot(data = smaller, mapping = aes(MonthlyCharges)) +geom_histogram(binwidth = 0.1)

# multiple histograms, x is cont while color is categorical
ggplot(data = IBMMM, mapping = aes(x = `Monthly Charges`, colour = Dependents)) +geom_freqpoly(binwidth = 0.1)

# unusual values
ggplot(data = IBMMM, mapping = aes(x = `Tenure Months`, y = `Monthly Charges`)) + geom_point()

ggplot(data = IBMMM, mapping = aes(x = `Internet Service`, y = `Monthly Charges`)) +geom_boxplot()

# Two categorical variables
ggplot(data = IBMMM) +geom_count(mapping = aes(x = TechSupport, y = Churn))
ggplot(data = IBMMM) +geom_count(mapping = aes(x = Dependents, y = Churn))
ggplot(data = IBMMM) +geom_count(mapping = aes(x = PhoneService, y = Churn))
ggplot(data = IBMMM) +geom_count(mapping = aes(x = SeniorCitizen, y = Churn))









