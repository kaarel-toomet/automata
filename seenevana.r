seenevana <- function(sz=100,tm=50,pmd=TRUE,dqm=0.5,dqp=0.02) {
  nx <- sz
  ny <- sz
  x <- seq(1,nx)
  y <- seq(1,nx)
  xy <- expand.grid(x = x, y = y)
  xv <- c(0,nx)
  yv <- c(0,ny)
  F0 <- matrix(0,nx,ny)
  Q <- matrix(1,nx,ny)
  breks <- 0:100/100
  qpal <- colorRampPalette(c("black","green","white"))(100)
  fpal <- colorRampPalette(c("blue","cyan"))(100)
  #x11()
  ## -----w----y---y-----t---js--8----k-c-----d---l---
  i <- nx %/% 2 + 1
  j <- ny %/% 2 + 1
  F0[i,j] <- 1
  
  vneighbors <- function(X) {
     X1 <- rbind(0, cbind(0, X, 0), 0)
     iX <- as.matrix(expand.grid(1 + seq(length=nrow(X)), 1 + seq(length=ncol(X))))
     it <- cbind(iX[,1] - 1, iX[,2])
     itr <- cbind(iX[,1] - 1, iX[,2] + 1)
     ir <- cbind(iX[,1], iX[,2] + 1)
     ibr <- cbind(iX[,1] + 1, iX[,2] + 1)
     ib <- cbind(iX[,1] + 1, iX[,2])
     ibl <- cbind(iX[,1] + 1, iX[,2] - 1)
     il <- cbind(iX[,1], iX[,2] - 1)
     itl <- cbind(iX[,1] - 1, iX[,2] - 1)
     n <- X1[it] + X1[itr] + X1[ir] + X1[ibr] + X1[ib] + X1[ibl] + X1[il] + X1[itl]
   }
  vneighbors(F0)
  for(t in 1:tm) {  #keeretab seenevana 100 keeru
    F1 <- F0
    i <- F0==1
    Q[i] <- Q[i]-dqm
    Q[!i] <- Q[!i]+dqp
    Q[Q < 0] <- 0
    Q[Q > 1] <- 1
    n <- vneighbors(F0)
    p <- 1-(1-Q)^n
    k <- runif(length(p))<p
    F1[k] <- 1
  F1[runif(length(F1)) > Q] <- 0
  F0 <- F1
  F2 <- Q
  F2[F0==0] <- NA
  if(pmd) {
    image(seq(length=nx), seq(length=ny),z=Q,breaks = breks,col = qpal,
         main=paste0(t, "/", tm, "   ", sum(F0)))
    image(seq(length=nx), seq(length=ny),z=F2,breaks = breks,col = fpal,
         add = TRUE)
    Sys.sleep(0.1)
  }
  if(sum(F0)==0) {
    break
  }
  }
  sum(F0)
} 
seenevana2 <- function(sz=200,tm=200,dqm=0.95,dqp=0.95) {
  pmd <- TRUE
  nx <- sz
  ny <- sz
  x <- seq(1,nx)
  y <- seq(1,nx)
  xy <- expand.grid(x = x, y = y)
  xv <- c(0,nx)
  yv <- c(0,ny)
  G0 <- F0 <- matrix(0,nx,ny)
  Q <- matrix(0.5,nx,ny)
  breks <- 0:100/100
  qpal <- colorRampPalette(c("black","green","white"))(100)
  fpal <- colorRampPalette(c("blue","cyan"))(100)
  gpal <- colorRampPalette(c("red","yellow"))(100)
  #x11()
  ## -----w----y---y-----t---js--8----k-c-----d---l---
  i <- nx %/% 2 + 1
  j <- ny %/% 2 + 1
  F0[i,j] <- 1
  G0[i+2,j+2] <- 1
  vneighbors <- function(X) {
                                  #kaeb naabresid
    X1 <- rbind(0, cbind(0, X, 0), 0)
    iX <- as.matrix(expand.grid(1 + seq(length=nrow(X)), 1 + seq(length=ncol(X))))
    it <- cbind(iX[,1] - 1, iX[,2])
    itr <- cbind(iX[,1] - 1, iX[,2] + 1)
    ir <- cbind(iX[,1], iX[,2] + 1)
    ibr <- cbind(iX[,1] + 1, iX[,2] + 1)
    ib <- cbind(iX[,1] + 1, iX[,2])
    ibl <- cbind(iX[,1] + 1, iX[,2] - 1)
    il <- cbind(iX[,1], iX[,2] - 1)
    itl <- cbind(iX[,1] - 1, iX[,2] - 1)
    n <- X1[it] + X1[itr] + X1[ir] + X1[ibr] + X1[ib] + X1[ibl] + X1[il] + X1[itl]
  }
  
  for(t in 1:tm) {  #keeretab seenevana 100 keeru
    F1 <- F0
    i <- F0 == 1
    Q[i] <- Q[i] - dqm
    
    G1 <- G0
    i <- G0 == 1
    Q[i] <- Q[i] + dqp
    
    Q[Q < 0] <- 0
    Q[Q > 1] <- 1
    ###################################sinine seenevana
    n <- vneighbors(F0)
    p <- 1-(1-Q)^n               #kaeb iga kandi tgen8osfst et seenevanariseeruks
    k <- runif(length(p))<p       #kaeb kas p on suurem kui lambivana
    F1[k] <- 1
    F1[runif(length(F1)) > Q] <- 0      
    F0 <- F1
    ##############################punane seenevana
    n <- vneighbors(G0)
    p <- 1-(Q)^n
    k <- runif(length(p))<p 
    G1[k] <- 1
    G1[runif(length(G1)) > 1-Q] <- 0      
    G0 <- G1
    ################## alusta kriibeldamist
    if(pmd) {  
      F2 <- Q
      F2[F0==0] <- NA
      G2 <- Q
      G2[G0==0] <- NA
      image(seq(length=nx), seq(length=ny),z=Q,breaks = breks,col = qpal,
            main=paste0(t, "/", tm, "   ", sum(F0),"   ",sum(G0)))
      image(seq(length=nx), seq(length=ny),z=F2,breaks = breks,col = fpal,
            add = TRUE)
      image(seq(length=nx), seq(length=ny),z=G2,breaks = breks,col = gpal,
            add = TRUE)
      Sys.sleep(0.1)
    }
    if(sum(F0)+sum(G0)==0) {
      break
    }
  }
  sum(F0)
} 
svstats <- function(n=10, sz=100, tm = sz/2 + 20,dqm=0.5,dqp=0.02) {
  library(doParallel)
  registerDoParallel(cores = 3)
  k <- foreach(x = 1:n, .combine = c) %dopar% {
    seenevanak <- seenevana(sz,tm,FALSE,dqm,dqp)
  }
  print(k)
  cat("vanariseerund:", sum(k > 0), "/", length(k), "\n")
}