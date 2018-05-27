
#1 reding the data---------------------------------
mydata <- read.csv("uci_dataset_a.csv", header=TRUE)

#2 creating data frame of two columns only---------
a<- data.frame(x=mydata$ATT1[1:length(mydata$ATT1)-1],
               y=mydata$ATT2[1:length(mydata$ATT2)-1]
    )

#3  creating a function for regression-------------
# assuming y=f(x)
reg_y_on_x<- function(a,item){
  byx=cov(a$x,a$y)/sd(a$x)^2
  result=mean(a$y)+byx*(item-mean(a$x))
  result
}

#4 calling the function-----------------------------
reg_y_on_x(a,mydata$ATT1[length(mydata$ATT1)])
