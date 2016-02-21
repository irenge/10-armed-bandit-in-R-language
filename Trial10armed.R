n <- 10

R<-array(0,dim=c(2000,2000))
#O<-array(0,dim=c(100,100))
epsilon <- 0.01
for (i in 1:2000) {
  print(i)
  qstar <- rnorm(n,mean = 0, sd = 1)
  opt <- max(qstar)
  astar <- which(Qstar==opt)
  q <- array(0,c(1,10))
  Ka <- array(0,c(1,10))
  for(j in 1:1000){
    if(runif(1) < epsilon){
      a <- ceiling(runif(1)*n)
    }
    else{
      qm <- q/(max(Ka,array(1,c(1,n))))
      K <- which(qm == max(qm))
      sk <- ceiling(runif(1) * length(K))
      a <- K[sk]
      
    }
    
    reward <- qstar[a] + runif(1)
    q[a] = q[a] + reward
    Ka[a] = Ka[a] + 1
    R[i,j] = reward
    
   
   # O[i,j]<-(astar==a)
  }
  
}
