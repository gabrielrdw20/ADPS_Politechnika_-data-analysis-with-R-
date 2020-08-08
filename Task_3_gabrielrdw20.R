# github: gabrielrdw20

library('dplyr')
library('scales')
library('expm')
#library('markovchain') 


sequence <- readRDS('~/ADPS/R/wig_changes.rds')
seq <- na.omit(sequence); seq

omega <- as.numeric(length(sequence)) # przestrzeñ probabilityczna

pluspluscounter <- 0
plusminuscounter <- 0
minuspluscounter <- 0
minusminuscounter <- 0



for(i in 1:omega)
{
  a <- seq[i]
  b <- seq[i+1]
  
  if(a == '+' & b == '+')
  {
    m1[i] <- 1
    pluspluscounter <- pluspluscounter + 1
    
  } else if (a == '+' & b == '-')
  {
    m2[i] <- 1
    plusminuscounter <- plusminuscounter + 1
    
  } else if (a == '-' & b == '+')
  {
    m3[i] <- 1
    minuspluscounter <- minuspluscounter + 1
    
  } else if (a == '-' & b == '-')
  {
    m4[i] <- 1
    minusminuscounter <- minusminuscounter + 1
  } 
  
}



prawdpp <- (pluspluscounter)/(omega-1)
prawdpm <- (plusminuscounter)/(omega-1)
prawdmp <- (minuspluscounter)/(omega-1)
prawdmm <-(minusminuscounter)/(omega-1)
# prawd_check <- prawdpp + prawdpm + prawdmp + prawdmm; prawd_check # wynik 1 - ok (100%)


colnames <- c('+','-')
rownames <- c('+','-')
finalMatrix <- noquote(matrix(c(percent(prawdpp),percent(prawdmp),percent(prawdpm),percent(prawdmm)),nrow=2, ncol=2, dimnames=list(rownames, colnames)))
poweredMatrix <- noquote(matrix(c(prawdpp, prawdmp,prawdpm,prawdmm), ncol=2,nrow=2,dimnames=list(rownames,colnames)))

finalMatrix # ---------- WYNIK ---------- 
round(poweredMatrix%^%3, digits=3) # ---------- macierz do potêgi 3-ciej ----------




