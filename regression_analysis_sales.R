path<- "C:\\Users\\CHIRAG\\Downloads\\ACADgILd\\bigmart"

setwd(path)

train<-read.csv("train.csv")
test<-read.csv("test.csv")

dim(train)
dim(test)

str(train)

table(is.na(train))

colSums(is.na(train))

summary(train)

#Some plots to underdtand our data better

library(ggplot2)
ggplot(data = train,aes(x= Item_Visibility, y = Item_Outlet_Sales))+
  geom_point(size = 2.5, color="navy")+xlab("Item_visibility")+ylab("item_outlet")+
               ggtitle("item_visibility_vs_itemoutlet")
             
ggplot(data = train,aes(x= Outlet_Identifier, y = Item_Outlet_Sales))+
  geom_bar(stat = "identity",color="blue")+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))
  xlab("Outlet_Identifier")+ylab("item_outlet")+
  ggtitle("Outlet_Identifier_vs_outlet_sales")

ggplot(data = train,aes(x= Item_Type, y = Item_Outlet_Sales))+
    geom_bar(stat = "identity",color="blue")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.50, color = "black"))+
  xlab("Item_type")+ylab("item_outlet")

ggplot(train, aes(Item_Type,Item_MRP)) + geom_boxplot()+ ggtitle("boxplot")+
  theme(axis.text.x = element_text(angle = 80,vjust =.05,color = "black"))+
  xlab("Item_type")+ylab("Item_MRP")

#adding column in test data

test$Item_Outlet_Sales <-1
View(test)
dim(test)
combi<- rbind(train,test)

#Treating Null and 0 values

combi$Item_Weight[is.na(combi$Item_Weight)] <-median(combi$Item_Weight,na.rm = T)
table(is.na(combi$Item_Weight))

combi$Item_Visibility<- ifelse(combi$Item_Visibility==0, 
                               median(combi$Item_Visibility),combi$Item_Visibility)

levels(combi$Outlet_Size)[1] <- "Other"

levels(combi$Item_Fat_Content)

library(plyr)

combi$Item_Fat_Content<- revalue(combi$Item_Fat_Content, c("low fat"= "Low Fat",
                                                          "reg" = "Regular"))

combi$Item_Fat_Content<- revalue(combi$Item_Fat_Content, c("LF"= "Low Fat"))
                                                          
table(combi$Item_Fat_Content)
 
library(dplyr)
q <- combi%>%
 group_by(Outlet_Identifier)%>%
  tally()
head(q)

names(q)[2] <- "Outlet_Count"
combi<- full_join(q,combi,"Outlet_Identifier")
View(combi)
r<- combi %>% group_by(Item_Identifier)  %>% tally()
head(r)
names(r)[2]<- "Item_Count"

combi<- merge(r,combi,"Item_Identifier")

head(combi)

s<- combi %>% select(Outlet_Establishment_Year) %>%
  mutate("Outlet_year" = 2013- Outlet_Establishment_Year)
head(s)

combi <- full_join(s,combi)

split<- substr(combi$Item_Identifier,1,2)
split<- gsub("FD","Food",split)
split<- gsub("NC","Non Consumable",split)
split<- gsub("DR","Drinks",split)
table(split)
combi$item_type_new<- split
View(combi)

#lable and One hot encoding

combi$Item_Fat_Content<- ifelse(combi$Item_Fat_Content=="Regular",1,0)

library(dummies)
install.packages("dummies")

combi<-dummy.data.frame(combi,names = c("Outlet_Size","Outlet_Location_Type","Outlet_Type","item_type_new",sep="_"))

#Droping unwanted variables which we have encoded 
 
combi<- select(combi, -c(Item_Identifier, Outlet_Identifier, Item_Fat_Content,Item_Type))
combi<- select(combi, -c(Item_Type))

str(combi)


new_train<- combi[1:nrow(train),]
new_test<- combi[-(1:nrow(train)),]
str(new_train)
View(new_train)
linear_model<- lm(Item_Outlet_Sales ~.,data = new_train)
summary(linear_model)
library("Metrics")
rmse(new_train$Item_Outlet_Sales,linear_model$fitted.values)

#finding correlation

cor(new_train)

#only 20% of variation in Y is explained by variables in the data we cannot have good predictive model with this
#let me try to improve my model and reduce correlation in this data

path<- "C:\\Users\\CHIRAG\\Downloads\\ACADgILd\\bigmart"

setwd(path)

train<-read.csv("train.csv")
test<-read.csv("test.csv")

test$Item_Outlet_Sales <-1
View(combi)
combi<- rbind(train,test)

#NA value treatment

table(is.na(combi))
colSums(is.na(combi))

combi$Item_Weight[is.na(combi$Item_Weight)]<- median(combi$Item_Weight, na.rm = T)

#0 value treatment

combi$Item_Visibility<- ifelse(combi$Item_Visibility==0,median(combi$Item_Visibility),combi$Item_Visibility)

#rename level in outlet size

levels(combi$Outlet_Size)[1]<- "Other"

#rename levels in Item fat content 

library("plyr")
combi$Item_Fat_Content<- revalue(combi$Item_Fat_Content,c("LF"="Low Fat","reg"="Regular","low fat"="Low Fat"))

combi$Year<- 2013 - combi$Outlet_Establishment_Year

#drop unwanted variables
library("dplyr")
combi<- select(combi,-c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year))

new_train<- combi[1:nrow(train),]
new_test<- combi[-(1:nrow(train)),]


linear_model<- lm(Item_Outlet_Sales~.,new_train)
summary(linear_model)

par(mfrow=c(2,2))
plot(linear_model)

#we can see for the plot that its suffering from heteroskedasticity common practice to tackle this by
#taking log of response variable

linear_model<- lm(log(Item_Outlet_Sales)~.,new_train)
summary(linear_model)

predlm<- predict(linear_model, new_test)

#we can see that R-squared is also significantly improved while taking care of heteroskedasticity

library("Metrics")
install.packages("Metrics")
rmse(new_train$Item_Outlet_Sales,exp(linear_model$fitted.values))

#Now Ill apply decision tree to further reduce rmse value
#imp. packages
library('rpart')
library("rpart.plot")

#tree model
tree_model<- rpart(Item_Outlet_Sales~.,data= new_train)
prp(tree_model)

pre_score<- predict(tree_model)
rmse(new_train$Item_Outlet_Sales, pre_score)
summary(pre_score)


head(tree_model)


summary(pre_score)

main_predict <- predict(tree_model, newdata = new_test, type = "vector")

sub_file <- data.frame(Item_Identifier = test$Item_Identifier, Outlet_Identifier = test$Outlet_Identifier,       Item_Outlet_Sales = main_predict)
View(sub_file)
write.csv(sub_file,"Decision_tree_sales.csv")

#from this we can see that RMSE value has iproved from 1524 to 1102 and that is significant improvement 
#for this data and for predicting closest reuslt