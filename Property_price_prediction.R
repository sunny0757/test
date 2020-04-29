install.packages("dummies")
library(ggpubr)
library(moments)
library(car)
library(dummies)
library(corrplot)

train <- read.csv("Property_Price_Train.csv")
test<- read.csv("Property_Price_Test.csv")

str(train)

typeof(train$Sale_Type)

View(train)
var(train$Pool_Area)

apply(train,2, function(x) (sum(is.na(x))*100/nrow(train)))

#more than 80% missing in cat vars()

write.csv(apply(train,2, function(x) (sum(is.na(x))*100/nrow(train))),"missing.csv")


ncol(train)

missing <- c('Id','Lane_Type','Pool_Quality','Fence_Quality','Miscellaneous_Feature')


#removing variables with >80% missing



train1 <- train[,!(names(train) %in% missing)]



ncol(train1)

names(train1)

cat_var <- names(train1)[which(sapply(train1, is.factor))]
summary(train1[c(cat_var)])
#43

num_var <- names(train1)[which(sapply(train1, is.numeric))]
#37
int_var <- names(train1)[which(sapply(train1, is.integer))]


con_var <- union(num_var,int_var)

con_var1 <- con_var[!con_var %in% c('Sale_Price','Id')]

unique(train$Building_Class)

#missing value imputation

for( i in con_var1){
  train1[[i]][is.na(train1[[i]])] <- median(train1[[i]],na.rm=T)
}

for( i in cat_var)
  {
  train1[[i]][is.na(train1[[i]])] <- names(sort(-table(train1[[i]])))[1]
}



#outlier treatment

for(i in con_var1){
  
  
  qntozone <- quantile(train1[[i]], probs=c(0.05,0.95))
  
  print(i)
  print(qntozone[1])
  print(qntozone[2])
  
  z_values = scale(train1[[i]])
  
 
  train1[[i]][z_values > 3] = qntozone[2]
  train1[[i]][z_values < -3] = qntozone[1]
  
}

#calculate variance and remove variables with 0 variance

write.csv(apply(train1[c(con_var)],2, function(x) (var(x,na.rm = T))),"variance.csv")

write.csv(apply(train1[c(cat_var)],2, function(x) (length(unique(x)))),"unique.csv")

variance <- c('Underground_Half_Bathroom','Kitchen_Above_Grade','Pool_Area')


train2 <- train1[,!(names(train1) %in% variance)]

View(train2$Garage_Area)
View(train1)
View(train2)

names(train2)

num_var <- names(train2)[which(sapply(train2, is.numeric))]
#37
int_var <- names(train2)[which(sapply(train2, is.integer))]


con_var <- union(num_var,int_var)

con_var2 <- con_var[!con_var %in% c('Sale_Price','Id')]


train3 <- train2

for(i in con_var2){
  
  print(i)
  k = skewness(train2[[i]])
  print(k)
  
  if(k > 0) {
    
    train3[[i]] = log10(abs(train2[[i]]+1))
    
  } else{
    train3[[i]] = log10(max(train2[[i]]+1)-train2[[i]])
  }
}

ncol(train3)


train4 <- dummy.data.frame(train3, names = cat_var , sep = "_")

ncol(train4)

names(train4)

Model_lm1=lm(Sale_Price~.,data=train4)
summary(Model_lm1) 
plot(Model_lm1)


vif(Model_lm1)

alias(Model_lm1)


#the linearly dependent variables
ld.vars <- attributes(alias(Model_lm1)$Complete)$dimnames[[1]]

typeof(ld.vars)

train5 = train4[,!(names(train4) %in% ld.vars)]

names(train4)

train5$`Exterior2nd_Wd Shng`<-NULL

correlations <- cor(train5)

View(train5$Garage_Area)

# row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
# correlations<- correlations[row_indic ,row_indic ]
# corrplot(correlations, method="square")

Model_lm1=lm(Sale_Price~.,data=train5)

summary(Model_lm1) 
plot(Model_lm1)

write.csv(vif(Model_lm1),"vif.csv")
alias(Model_lm1)


# #remove the linearly dependent variables variables
# formula.new <- as.formula(
#   paste(
#     paste(deparse(formula), collapse=""), 
#     paste(ld.vars, collapse="-"),
#     sep="-"
#   )
# )


#run stepwise 



null<- lm(formula =Sale_Price~1, data=train5)

full<- lm(formula =Sale_Price~., data=train5)

stepwise_model<-step(null, scope = list(upper=full), data=train5, direction="both")

summary(stepwise_model)

vif(stepwise_model)


  