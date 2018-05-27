
#------------------------------------------
# defining data to make decision tree
#------------------------------------------
ds<- data.frame(
  Age=          c("<=30","<=30","31..40",">40",">40",">40","31..40",
                  "<=30","<=30",">40","<=30","31..40","31..40",">40"),
  
  student=      c("no","no","no","no","yes","yes","yes",
                  "no","yes","yes","yes","no","yes","no"),
  
  income=       c("high","high","high","medium","low","low","low",
                  "medium","low","medium","medium","medium","high","medium"),
  
  credit_rating=c("fair","excellent","fair","fair","fair","excellent","excellent",
                  "fair","fair","fair","excellent","excellent","fair","excellent"),
  
  buys_computer=c("no","no","yes","yes","yes","no","yes",
                  "no","yes","yes","yes","yes","yes","no")
)

str(ds)
set.seed(1234)

# random shuffling------------------------------------
pd <- sample(2,nrow(ds),replace = TRUE,prob = c(0.8,0.2))
train <- ds[pd==1,]

# using Rpart to make decision tree-------------------
library(rpart)
library(rpart.plot)

jpeg(file = "tree.jpg")

# creating tree from data----------------------------
tree <- rpart(buys_computer~ Age+ income+ student+ credit_rating, 
              data=train, 
              parms=list(split="information"),
              control=rpart.control(minsplit=1, minbucket=0, cp=0.1) )

# plotting the tree----------------------------------
rpart.plot(tree)

dev.off()
