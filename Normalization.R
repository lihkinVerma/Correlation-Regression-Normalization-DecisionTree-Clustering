
#---------------------------------------
# collection of an attribute values
#---------------------------------------
a<- c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25,
      30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70)

#---------------------------------------
# min max normalization technique
#---------------------------------------
min_max_norm<- function(a){
  min<- min(a)
  max<- max(a)
  for(i in a){
    m=(i-min)/(max-min)
    cat(m," ")
  }
}

#---------------------------------------
# z-score normalization technique
#---------------------------------------
z_score_norm<- function(a){
  mean=mean(a)
  sd=sd(a)
  for(i in a){
    m=(i-mean)/sd
    cat(m," ")
  }
}

#---------------------------------------
# decimal scaling normalization technique
#---------------------------------------
decimal_scaling_norm<- function(a){
  nc=nchar(a)
  for(i in a){
    m=i/(10^max(nc))
    cat(m," ")
  }
}

#---------------------------------------
# calling of functions
#---------------------------------------
min_max_norm(a)
z_score_norm(a)
decimal_scaling_norm(a)