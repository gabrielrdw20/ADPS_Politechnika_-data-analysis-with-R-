# github: gabrielrdw20

# ------------------ 4 a -----------------------------

N <- 100000
K <- 250
FF <- 1500
TT <- 40
t <- 1

St <- function(t)
{
  if(t == 1){
    return(St <- K * N)
    
    
  } else if(t>1){
    return(St(t-1) + K * N)
  }
}

exit <- function(){.Internal(.invokeRestart(list(NULL, NULL), NULL))}

for(i in 1:TT)
{  
  if(t<=TT)
  {
    St_1 <- St(t)
    
    if(St_1 >= 0)
    {
      cn <- rt(N, df=2);cn
      a <- length(cn[which(cn>=qt(0.9999,2))])
      n <- sample(0:100,1,replace=T)
      o <- sample(0:90,1, replace=T)
      N <- N + n - o - a
      odszkodowanie <- a * FF
      St_1 <- St_1 - odszkodowanie
      
    } else if(St_1 < 0)
    {
      print('Dzia³alnoœæ zbankrutowa³a.')
      exit();
    }
    
    t <- t+1
    
  } else  exit();
  
  print(paste0('Próba nr: ', i))
  print(paste0('Rezerwa: ', St_1))
  print(paste0('Liczba wyp³at: ', a))
  print(paste0('Odszkodowanie: ', odszkodowanie))
  cat('\n')
}

# ------------------ 4 b -----------------------------


symulacja <- function(N,K,FF,TT)
{
  
  t<-1
  
  St <- function(t)
  {
    if(t == 1){
      return(St <- K * N)
      
      
    } else if(t>1){
      return(St(t-1) + K * N)
    }
  }
  
  for(i in 1:TT)
  {  
    if(t<=TT)
    {
      St_1 <- St(t)
      
      if(St_1 >= 0)
      {
        cn <- rt(N, df=2)
        a <- length(cn[which(cn>=qt(0.9999,2))])
        n <- sample(0:100,1,replace=T)
        o <- sample(0:90,1, replace=T)
        N <- N + n - o - a
        odszkodowanie <- a * FF
        St_1 <- St_1 - odszkodowanie
        print(St_1)
        cat('\n')
        
        
      } else if(St_1 < 0)
      {
        St_1 <- 'NA'
        print(St_1)
        cat('\n')
      }
      
      t <- t+1
      
    }
  }
}


symulacja(N,K,FF,TT)


# ----------- 4b AD. 1 -------------------------------------------

N <- 3
K <- 0.10
FF <- 1000
TT <- 10
t <- 1
M <- 5a
z <- matrix(nrow=TT)
ab <- c()
tmp <- data.frame()

St <- function(t)
{
  if(t == 1){
    return(St <- K * N)
    
    
  } else if(t>1){
    return(St(t-1) + K * N)
  }
}

for(i in 1:TT)
{
  if(t<=TT)
  {
    St_1 <- St(t)
    
    if(St_1 >= 0)
    {
      cn <- rt(N, df=2)
      a <- length(cn[which(cn>=qt(0.9999,2))])
      n <- sample(0:100,1,replace=T)
      o <- sample(0:90,1, replace=T)
      N <- N + n - o - a
      odszkodowanie <- a * FF
      St_1 <- (St_1 - odszkodowanie)
      
      z[i] <- St_1
      ab[i] <-z[i]
      
    } else if(St_1 < 0)
    { z[i] <- 'NA'
    ab[i]<-z[i]
    }
    tmp <- rbind(tmp,data.frame(ab[i]))
    t <- t+1
    
  }
}

print(as.vector(unlist(tmp)))


# ----------- 4b AD. 2 -------------------------------------------

N <- 5
K <- 2
FF <- 1000
TT <- 10
t <- 1 
M <- 5

St <- function(t)
{
  if(t == 1){
    return(St <- K * N)
    
    
  } else if(t>1){
    return(St(t-1) + K * N)
  }
}

z <- matrix(nrow=TT)
ab <- c()
cd <- matrix(nrow=M,ncol=TT)
tmp <- data.frame()

for(z in 1:TT)
{
  
  for(j in 1:M)
  { 
    
    
    if(t<=TT)
    {
      
      if(St_1 >= 0)
      {
        cn <- rt(N, df=2)
        a <- length(cn[which(cn>=qt(0.9999,2))])
        n <- sample(0:100,1,replace=T)
        o <- sample(0:90,1, replace=T)
        N <- N + n - o - a
        odszkodowanie <- a * FF
        St_1 <- (St_1 - odszkodowanie)
        
        z[j] <- St_1
        ab[j] <-z[j]
        
      } else if(St_1 < 0)
      { z[j] <- 'NA'
      ab[j]<-z[j]
      }
      tmp <- rbind(tmp,data.frame(ab[j]))
      t <- t+1 
    }
  }
}

final <-unlist(tmp)
SIM

# ----------- 4b AD. 3 -------------------------------------------

N <- 200
K <- 10
FF <- 1000
TT <- 10
t <- 1 

for(j in 1:M)
{ 
  
  if(t<=TT)
  {
    St_1 <- St(t)
    
    if(St_1 >= 0)
    {
      cn <- rt(N, df=2)
      a <- length(cn[which(cn>=qt(0.9999,2))])
      n <- sample(0:100,1,replace=T)
      o <- sample(0:90,1, replace=T)
      N <- N + n - o - a
      odszkodowanie <- a * FF
      St_1 <- (St_1 - odszkodowanie)
      
      z[j] <- St_1
      ab[j] <-z[j]
      
    } else if(St_1 < 0)
    { z[j] <- NA
    ab[j]<-z[j]
    }
    tmp <- rbind(tmp,data.frame(ab[j]))
    t <- t+1
    
  }
  
  
}


final <-unlist(tmp)
SIM <- noquote(matrix(final, ncol=TT,nrow=M))
SIM



# AD. 3.1 ------------------------------------------------------------
testMatrix <- matrix(ncol= TT, nrow=M)

for(i in 1:TT)
{
  for(j in 1:M)
  {
    !is.na(SIM[i,j]) -> testMatrix[i,j]
  }
}

# Sprawdzam prawdopodobieñstwo bankructwa, gdzie 1 - nie, NA - tak:
probability <- colSums(testMatrix)/M; probability


# AD. 3.2
months <- 0
monthsAll <- c()
tv<- c()
checkSum <- 0
checkVal <- c()
SIM2 <- matrix(0,ncol=TT, nrow=M)

for(i in 1:TT)
{
  
  for(j in 1:M)
  {
    if(SIM[j,i] != 'NA') 
    {
      checkSum <- as.numeric(SIM[j,i]) + checkSum
      SIM2[j,i] <- as.numeric(SIM[j,i])
      months <- months + 1
    } else if (SIM[j,i] == 'NA'){
      checkSum <- 0
      SIM2[j,i] <- 0
      months <- months + 0
      next()
    }
  }
  monthsAll[i] <- months
  checkVal[i] <- checkSum
}

monthsAll
# œredni poziom rezerw, jeœli jest 0 oznacza to, ¿e spó³ka zbankrutowa³a
aver <- checkVal/M; aver


# AD. 3.3 ------------------------------------------------------------
tmp2 <- monthsAll/TT; tmp2








