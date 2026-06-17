
# functions to make calculations

# survival correction for stages in the model
# calculated from the annual survival rates estimated in the field

sup_correction <- function(sup,d){
  P<-((1-(sup^(d-1)))/(1-sup^d))*sup  #ver Bruce&Shernock
  G<- ((P^d)*(1-sup))/(1-P^d)  #ver Bruce&Shernock
  result<-list(P=P,G=G)
  return(result)
  }

# create function for a truncated normal distribution constrained between 0 and 1

rnormt <- function(n, mean, sd) {
  
  range<-c(0,1)
  F.a <- pnorm(min(range), mean = mean, sd = sd)
  F.b <- pnorm(max(range), mean = mean, sd = sd)
  
  u <- runif(n, min = F.a, max = F.b)
  
  qnorm(u, mean = mean, sd = sd)
  
}
