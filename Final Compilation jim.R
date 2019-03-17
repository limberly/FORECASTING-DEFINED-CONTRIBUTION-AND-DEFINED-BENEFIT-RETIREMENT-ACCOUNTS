#contribution is used for age 25 to 74
#contribution_2 is used for graded contribution and 50,000 salary at each start age
#yes is lumpsum
#final consists of simulated lumpsum values

  
  #Used for bootstrapping
  #rtn consists of random yearly returns
  #yas the lumpsum
  boot_45 <- function(a,b) {
  final <- rep(x=0, 1000)
  for (j in 1:1000) {
    
    rtn <- sample(sam$return, size = a, replace = TRUE)
    rtn <- as.data.frame(rtn)
    yas <- rep(x=0, a)
  for (i in 1:a) {
    yas[i] <- Contribution_2$Salary_45[i+b]*prod(rtn$rtn[i:a])
    
  }
  final[j] <- sum(yas)
  }
  print(mean(final))
  print(sd(final))
  }
  

#bootstrapping using geometric average
  geometric <- function(a,b) {
    for (j in 1:1000) {
      
      rtn <- sample(GeoAvg$`10yravg`, size = a, replace = TRUE)
      rtn <- as.data.frame(rtn)
      yas <- rep(x=0, a)
      for (i in 1:a) {
        yas[i] <- contribution$Contribution[i+b]*prod(rtn$rtn[i:a])
        
      }
      final[j] <- sum(yas)
    }
    print(mean(final))
    print(sd(final))
  }
#Using fixed rate
  
  
  fxrate_35 <- function(a,b) {
    for (j in 1:1000) {
    yas <- rep(x=0, a)
    for (i in 1:a) {
      yas[i] <- contribution_2$Salary_25[i+b]*1.0776^(a-i+1)
    }
    final[j] <- sum(yas)
    }
    print(mean(final))
    print(sd(final))
  }
  
#Using Binomial Expectation
# a is how many years to project
# b is what age to start. contribution age starts from 25, so if you want 35, put 10 for b value
  BinExp_45 <- function(a,b){
  final <- rep(x=0, 1000)
  for (k in 1:1000) {
    yes <- 0
    
    for (j in 1:a) { 
      random <- sample(1:68, 1, replace = TRUE)
      up <- 0; dn <- 0; prob <- 0
      up <- 1+((sam$return_2[random] - sam$div[random]) + sam$vol_3[random])
      dn <- 1+((sam$return_2[random] - sam$div[random]) - sam$vol_3[random])
      prob <- ((1+(sam$return_2[random] - sam$div[random])) - dn) / (up - dn)
      
      #calculating tree. yes is initially 0, so adding contribution from the beginning is ok.
      yes = (yes + contribution_2$Salary_45[j+b]) * up * prob + (yes+ contribution_2$Salary_45[j+b]) * dn * (1 - prob)
      
      
    }
    final[k] <- yes
  }
  print(mean(final))
  print(sd(final))
  }