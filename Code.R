#### Libraries ####
library("readr")
library("dplyr")
library("corrplot")
library("car")
library("pROC")
library("PresenceAbsence")
library("coop")
library("stringr")
library("rpart")
library("rpart.plot")
library("patchwork")
library("ggplot2")

#####SEPTEMBER DATA####
#reading data
sept <- read.csv("RptStockMovAmount2.csv")
dim(sept)
colnames(sept)
sept <- sept[-c(2,4,7,9,10,11,13,14,16,20,22,25,27,30,31)]

#changing column names
for (i in 1:length(sept[9,])){
  colnames(sept)[i] <- as.character(sept[9,i])
}
colnames(sept)

#Dealing with empty cells
str(sept)
sept$Type <- as.character(sept$Type)
unique(sept$Type)
sept <- subset(sept,(Type=='POS')) #since all "clean transactions"  have POS as type
dim(sept)

#removing unneccesarry columns
sept <- sept[-c(3,6,9,14,15,16)]
sept$Month <- "September"
head(sept,1)
colnames(sept)[3] <- "Customer"
colnames(sept)
dim(sept)

#Changing column types
sept$Date <-as.numeric(gsub('/','',str_sub(as.character(sept$Date),1,2)))
sept$Total <- as.numeric(gsub(',','',as.character(sept$Total)))
sept$Customer <- as.character(sept$Customer)
sept$Item <- as.character(sept$Item)
sept$Qty. <- as.numeric(gsub(',','',as.character(sept$Qty.)))
sept$C.Qty <- as.numeric(gsub(',','',as.character(sept$C.Qty)))
sept$U.Price <- as.numeric(gsub(',','',as.character(sept$U.Price)))
sept$T.Price <- as.numeric(gsub(',','',as.character(sept$T.Price)))
sept$Discount <- as.numeric(gsub(',','',as.character(sept$Discount)))
sept$T.Nbr. <- as.numeric(gsub(',','',as.character(sept$T.Nbr.)))

hist(sept$Date,xlab="Day",main="Month Days", col='blue')

#####OCTOBER DATA####
#cleaning data
oct <- read.csv("RptStockMovAmount.csv")
dim(oct)
head(oct)
oct <- na.omit(oct)
dim(oct)
oct <- oct[,-3]
str(oct)
colnames(oct)[3]<-"Customer"
colnames(oct)

#fixing column type
oct$Date <-as.numeric(gsub('/','',str_sub(as.character(oct$Date),1,2)))
oct$Total <- as.numeric(gsub(',','',as.character(oct$Total)))
oct$Customer <- as.character(oct$Customer)
oct$Item <- as.character(oct$Item)
oct$Qty. <- as.numeric(gsub(',','',as.character(oct$Qty.)))
oct$C.Qty <- as.numeric(gsub(',','',as.character(oct$C.Qty)))
oct$U.Price <- as.numeric(gsub(',','',as.character(oct$U.Price)))
oct$T.Price <- as.numeric(gsub(',','',as.character(oct$T.Price)))
oct$Discount <- as.numeric(gsub(',','',as.character(oct$Discount)))
oct$Month <- "October"
oct$Date <- oct$Date +30 # to take into consideration the september 30 days
summary(oct$Date)

hist(oct$Date,xlab="Day",main="Month Days", col='blue')


#####JOINING SEPT AND OCT####
str(sept)
str(oct)
colnames(sept)
colnames(oct)
data<- bind_rows(sept,oct)
data <- data[,-6]
dim(data)
colnames(data)

hist(data$Date,xlab="Day",main="Sales per month days", col='blue')

#####CREATING CUSTOMER DATA FOR BOTH MONTHS####
dim(data)
colnames(data)
convert_to_customer_data <- function(data){
  #days last visit
  recency <- aggregate(Date~Customer, data = data, max)
  recency[,2]<- max(data[,1])-recency[,2]
  
  #first visit
  first_visit <- aggregate(Date~Customer, data = data, min)
  first_visit[,2]<- max(data[,1])-first_visit[,2]
  
  #number of times the customer came per month
  frequency <- aggregate(Date~Customer, data = data, unique)
  for ( i in 1:nrow(frequency)){
    frequency$freq[i] <- length(frequency$Date[[i]]) 
  }
  
  #If the customer benefits from a discount or not
  cust_discount <-aggregate(Discount~Customer, data = data, sum)
  cust_discount$discount <- ifelse(cust_discount[,2]>0,1,0)
  
  #Total spent per customer
  cust_totalpurch <-aggregate(Total~Customer, data = data, sum)
  
  #number of products bought by customer
  cust_number_of_items<- aggregate(Item~Customer, data = data, length)
  
  #Get the minimum and maximum amount amount
  df <- summarise(group_by(data,Date,Customer),total=sum(Total))
  min_amount <- aggregate(df$total~as.factor(df$Customer), data = df,min)
  max_amount <- aggregate(df$total~as.factor(df$Customer), data = df,max)
  
  #forming customer data dataframe
  customer_data <-data.frame(recency,first_visit[,2],frequency[,3],cust_discount[,3],cust_number_of_items[,2],cust_totalpurch[,2],min_amount[,2],max_amount[,2])
  customer_data$Average_purchase <- customer_data$cust_totalpurch...2./customer_data$frequency...3.
  colnames(customer_data) <- c("CustomerID","Last_visit","First_visit","Freqeuncy","Discount","Number_of_products","Total_spent","Min_spent","Max_spent","Average_spent")
  return(customer_data)
}
customer_data <- convert_to_customer_data(data)
colnames(customer_data)
head(customer_data)

#####CHECKING FOR OUTLIERS CUSTOMER DATA####
#CHECKING FOR OUTLIERS
#total spent
boxplot(customer_data$Total_spent, col = "red", main = "Boxplot of Total Amount Spent")
summary(customer_data$Total_spent)
dim(customer_data)
customer_data <- subset(customer_data, Total_spent < 2000000)
summary(customer_data$Total_spent)
dim(customer_data)
boxplot(customer_data$Total_spent, col = "red", main = "Boxplot of Total Amount Spent")

#Frequency
boxplot(customer_data$Freqeuncy,col = "red", main = "Boxplot of Frequency of Visits")
summary(customer_data$Freqeuncy)

#####Classifying customers for the training set ####
cluster_data <- customer_data[,c(1,4,7,10)]
head(cluster_data)

frequencyRank <- rank(cluster_data[,2], ties.method="first")
totalSpentRank <- rank(cluster_data[,3], ties.method="first")
avgSpentRank <- rank(cluster_data[,4], ties.method="first")

ranks <- data.frame(customer_data$CustomerID, totalSpentRank, frequencyRank, avgSpentRank)
head(ranks)

norm <- scale(ranks[,-1])
summary(norm) #mean 0
apply(norm, 2, sd) # var is 1 

#k-means clustering
head(norm)
cl <- kmeans(norm, 2, iter.max = 100, nstart=10)
cl$centers
table(cl$cluster)
# cluster 1 is light and 2 is heavy 
aggregate(cluster_data[,-1], by=list(cl$cluster), sum)
aggregate(cluster_data[,-1], by=list(cl$cluster), min)
aggregate(cluster_data[,-1], by=list(cl$cluster), max)
aggregate(cluster_data[,-1], by=list(cl$cluster), mean)

customer_labels <- data.frame(cluster_data$CustomerID, cl$cluster)
colnames(customer_labels) <- c("CustomerID", "Label")
customer_labels[,2] <- ifelse(customer_labels[,2]==1,0,1)
head(customer_labels)
table(customer_labels[,2])

#####CREATING CUSTOMER DATA FOR ONLY SEPTEMBER####
dim(sept)
colnames(sept)

customer_data_sept <- convert_to_customer_data(sept)
head(customer_data_sept)
dim(customer_data_sept)
colnames(customer_data_sept)


#####CHECKING FOR OUTLIERS IN CUSTOMER SEPTEMBER DATA####
#total spent
boxplot(customer_data_sept$Total_spent, col = "red", main = "Boxplot of Total Amount Spent")
summary(customer_data_sept$Total_spent)
dim(customer_data_sept)
customer_data_sept <- subset(customer_data_sept, Total_spent < 2000000)
summary(customer_data_sept$Total_spent)
dim(customer_data_sept)
boxplot(customer_data_sept$Total_spent, col = "red", main = "Boxplot of Total Amount Spent")

#Frequency
boxplot(customer_data_sept$Freqeuncy,col = "red", main = "Boxplot of Frequency of Visits")
summary(customer_data_sept$Freqeuncy)


#####LEFT JOINING CUSTOMER SEPTEMBER AND LABELS DATA####
data_sept_labels <-  merge(x = customer_data_sept, y = customer_labels, by = "CustomerID", all.x = TRUE)
dim(data_sept_labels)
head(data_sept_labels)
colnames(data_sept_labels)
data_sept_labels <- na.omit(data_sept_labels)
dim(data_sept_labels)

data_sept_labels <- data_sept_labels[,-1] #deleting customer ID model

#####Visualization####
cor_data = data.frame(data_sept_labels[,c(1,2,3,5,6,7,8,9)])
correlation = cor(cor_data)
corrplot(correlation, method = "color")

x1 = ggplot(data_labels, 
            aes(x = Label, 
                y = Average_spent)) +
  geom_boxplot(color = "darkorange4", fill = "darkorange2") +
  labs(title = "Average Spent by Customer Segment", x = "Customer Segment",
       y = "Average Spending")

x2 = ggplot(data_labels, 
            aes(x = Label, 
                y = Freqeuncy)) +
  geom_boxplot(color = "mediumorchid4", fill = "mediumorchid2") +
  labs(title = "Number of Visits by Customer Segment", x = "Customer Segment",
       y = "Number of Visits")

x3 = ggplot(data_labels, 
            aes(x = Label, 
                y = Number_of_products)) +
  geom_boxplot(color = "seagreen4", fill = "seagreen2") +
  labs(title = "Number of Products Bought by Customer Segment", x = "Customer Segment",
       y = "Number of Products Bought")

x4 = ggplot(data_labels, 
            aes(x = Label, 
                y = Max_spent)) +
  geom_boxplot(color = "tomato4", fill = "tomato2") +
  labs(title = "Maximum Spent by Customer Segment", x = "Customer Segment",
       y = "Maximum Spent")

(x1 | x2) / (x3 | x4)
#####SPLITTING DATA####
str(data_sept_labels)
data_sept_labels$Discount <- as.factor(data_sept_labels$Discount)
data_sept_labels$Label <- as.factor(data_sept_labels$Label)
set.seed(1000)
split <- sample(1:2, nrow(data_sept_labels), replace = TRUE, prob=c(0.7, 0.3))
training_data <- data_sept_labels[split==1, ]
validation_data <- data_sept_labels[split==2, ]


#####LOGISTIC MODEL#####

colnames(training_data)

lc <- glm(Label~Number_of_products+Last_visit+First_visit+Average_spent
          +Discount+Min_spent+Max_spent+Freqeuncy, data = training_data,
          family = "binomial")
summary(lc)

lc_pred <- predict(lc, validation_data,type = "response")

#Check for multi-collinearity 

vif(lc)


lc2 <- glm(Label~Number_of_products+Last_visit+First_visit
           +Discount+Min_spent+Max_spent+Freqeuncy, data = training_data,
           family = "binomial")
summary(lc2)

lc2_pred <- predict(lc2, validation_data,type = "response")

vif(lc2)

#####REGRESSION TREE####

class_tree <- rpart(Label ~ Last_visit + First_visit + Freqeuncy + Discount + Number_of_products + Min_spent + Max_spent + Average_spent, data = training_data, control = rpart.control(cp = 0.0001))
printcp(class_tree)

rpart.plot(class_tree,type=4,extra=2,
           main="Customer Segmentation")

rpart(formula = Label ~ ., data = training_data, control = rpart.control(cp = 0.0001))
class_tree$cptable

bestcp=class_tree$cptable[which.min(class_tree$cptable[,"xerror"]),"CP"]

tree.pruned=prune(class_tree, cp = bestcp)

x11()
rpart.plot(tree.pruned,type=4,extra=2,
           main="Customer Segmentation")


tree_pred=predict(tree.pruned, newdata=validation_data,type="prob")

tree_pred=tree_pred[,2]


#####Evaluating Models ####

acc_measures <- function(p) {
  act_pred=data.frame(ID=1:nrow(validation_data),1*(validation_data$Label== 1),p)
  # create data frame for actual and predicted but ID must be there
  conf_mat=cmx(act_pred)#  creates a confusion matrix  
  total_acc= pcc(conf_mat) # Overall accuracy
  
  sens=sensitivity(conf_mat)# to obtain snesitivty 
  
  spec=specificity(conf_mat)# to obtain specificity
  auc <- auc(act_pred)
  x=c(total_acc,sens,spec,auc) 
  
  #names(vec)= c("Total Accuracy", "Sensitivity", "Specificity","AUC")
  return(x)
}


acc_measures(lc_pred)
acc_measures(lc2_pred)
acc_measures(tree_pred)

act_pred_mult=data.frame(ID=1:nrow(validation_data),1*(validation_data$Label== 1),lc_pred,lc2_pred,tree_pred)

x11()
auc.roc.plot(act_pred_mult,col=c("navyblue","darkorange1","green4"),line.type = 1, lwd = 2,
             threshold = 1001,  main="ROC Curves",legend.text=c("Logistic classifier", "Logistic classifier modified", "Classification Tree"), 

             )

#####Customer Recommendations####

cid <- sort(customer_data$CustomerID)
head(cid)
length(cid)

item <- sort(unique(data$Item))
head(item)
length(item)

user_item <- data.frame(cid)
#code below takes few mins 
for (j in 1:length(item)){
  spec_cid <- select(filter(data, Item == item[j] ), Customer)
  col1 <- rep(0, length(cid))
  for (i in 1:nrow(spec_cid)){
    if (spec_cid[i,] %in% cid) {
      col1[match(spec_cid[i,],cid)] = 1
    }
  }
  user_item <- data.frame(user_item,col1)
  colnames(user_item)[j+1] <- item[j]
}
dim(user_item)

#Collaborative filtering
#User-based Collaborative Filtering
user_item <-user_item[,-1]# to delete id column
#User-User Similarity Matrix
cos_matrix <- cosine(t(user_item))
dim(cos_matrix) # 3218 x 3218 matrix ==> checks
cos_matrix <- data.frame(cos_matrix)
colnames(cos_matrix) <- 1:length(cid)

#heavy customer ids
xx <- customer_labels
xx$CustomerID <- 1:nrow(customer_labels)
heavy_customers_ids <- subset(xx, Label==1)$CustomerID
length(heavy_customers_ids)
head(heavy_customers_ids)
heavy_customers_ids[6]

#Making recommendations for Customer 23
sort(cos_matrix[23,], decreasing = TRUE)[1:6]

#customers if IDs:741, 70, 760 2531, 1961 are the most similar to customerID = 23

#suggestion products for customer of ID 23 with that of ID 741

recommender <- function(customer_id,similar_customer_id){
  items_of_customer <-unique(subset(data, Customer==sort(unique(data$Customer))[customer_id])$Item)
  potential_suggestions <- unique(subset(data, Customer==sort(unique(data$Customer))[similar_customer_id])$Item)
  
  recommedations <- c()
  for (i in 1:length(potential_suggestions)){
    if (!(potential_suggestions[i] %in% items_of_customer)) {
      recommedations <- c(recommedations, potential_suggestions[i])
    }
  }
  return(recommedations)
}
unique(subset(data, Customer==sort(unique(data$Customer))[23])$Item)
unique(subset(data, Customer==sort(unique(data$Customer))
              [741])$Item)
recommender(23,741)





