
#---------------------------------------------------
# Pearson correlation
#---------------------------------------------------
# correlation = summ((x-~x)(y-~y))/n*sigma(x)sigma(y)

a<-data.frame(x=c(1,2,3,4,5,6,7),y=c(4,3,1,6,2,7,5))

# Loading the data from CSV file
mydata <- read.csv("uci_dataset_a.csv", header=TRUE)

a<- data.frame(x=mydata$ATT1,y=mydata$ATT2)

corr<- function(a){
  meanx=mean(a$x)
  meany=mean(a$y)
  x_sub_mean=a$x-meanx
  y_sub_mean=a$y-meany
  
  n=length(a$x)
  
  standx=sqrt(sum(x_sub_mean^2)/n)
  standy=sqrt(sum(y_sub_mean^2)/n)
  
  cor=sum(x_sub_mean*y_sub_mean)/(n*standx*standy)
  cor
}

corr(a)

# Built in method to find correlation 
cor(mydata$ATT1,mydata$ATT2)

#---------------------------------------------
# Spearman's correlation
#---------------------------------------------
#-------giving ranks manually-----------------

data <- data.frame(att_x =c(4,7,8,2,9,11,6),att_y =c(12,23,51,23,67,52,15))
corelation_spearman<-function(data){
  data$x1 = c(6,4,3,7,2,1,5)
  data$y1 = c(7,4.5,3,4.5,1,2,6)
  data$att_z = (data$x1 - data$y1)
  print(data$att_z)
  n = nrow(data)
  sum_d2 = sum((data$att_z)**2)
  r_n = 6*sum_d2
  r_d = n*((n*n)-1)
  r = 1-(r_n/r_d)
  cat("Spearman's Rank Correlation(r) = ",r,"\n")
}
corelation_spearman(data)

#---------------------------------------------
# assumed mean method correlation
#---------------------------------------------

data <- data.frame(att_x =c(4,7,8,2,9,11,6),att_y =c(12,23,51,23,67,52,15))
corelation_assume_mean<-function(data,m_x,m_y){
  data$x1 = (data$att_x - m_x)
  data$y1 = (data$att_y - m_y)
  print(data$x1)
  print(data$y1)
  n = nrow(data)
  sum_x = sum(data$x1)
  sum_y = sum(data$y1)
  sum_xy = sum(data$x1 * data$y1)
  sum_x2 = sum((data$x1)**2)
  sum_x_2 = sum_x**2
  sum_y2 = sum((data$y1)**2)
  sum_y_2 = sum_y**2
  r_n = (n*sum_xy)-(sum_x*sum_y)
  r_d1 = ((n*sum_x2)-(sum_x_2))*((n*sum_y2)-(sum_y_2))
  r_d = sqrt(r_d1)
  r = r_n/r_d
  cat("Assume Mean Meathod Correlation(r) = ",r,"\n")
}
corelation_assume_mean(data,7,35)

