library(ggplot2)
library(doParallel)

meow <- function(f) {
  x <- runif(100)
  x <- x-mean(x)
  bx <- x
  for(t in 1:100) {
    x <- bx
    print(range(x))
    p <- ggplot(data.frame(i=seq(along=x), x=x, y=1)) +
      geom_col(aes(i, y, fill=x)) +
      scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high = "#FF0000", limits=c(-1,1))
    print(p)
    Sys.sleep(1)
    for(i in seq(along=x)) {
      j <- seq(i-1, i+1)
      j[j<1] <- length(x)
      j[j>length(x)] <- 1
      bx[i] <- mean(x[j])*f
    }
  }
}

meow2 <- function(f = 2, size = 10, T = 200, torus = TRUE) {
  ps = 200
  pal <- colorRampPalette(c("blue","white","red"))(ps)
  breks <- seq(-1, 1, length.out = ps+1)
  nx <- ny <- size
  x <- matrix(runif(nx*ny), nx, ny)
  #x[] <- 0
  #x[3,2] <- 1
  #x[3,4] <- -1
  x <- x-mean(x)
  bx <- x
  ##
  registerDoParallel(cores=2)
  for(t in 1:T) {
    x <- bx
    image(seq(length=nx), seq(length=ny),z=t(sign(x)),breaks = c(-2, 0, 2 ),col = c("#000080", "#800000"),
          main=paste0(t, ": [", paste(range(x), collapse=", "), "]"), useRaster = TRUE)
    image(seq(length=nx), seq(length=ny),z=t(x),breaks = breks,col = pal, add = TRUE, useRaster = TRUE)
    Sys.sleep(0.1)
    bx <- foreach(i = seq(length=nx), .combine=rbind) %dopar% {
       ni <- seq(i-1, i+1)
       if(torus) {
          ni[ni<1] <- nx
          ni[ni>nx] <- 1
       } else {
          ni[ni<1] <- NA
          ni[ni>nx] <- NA
       }
       bx <- double(ny)
      for(j in 1:ny) {
        nj <- seq(j-1, j+1)
        if(torus) {
          nj[nj<1] <- ny
          nj[nj>ny] <- 1
        }
        else {
          nj[nj<1] <- NA
          nj[nj>ny] <- NA
        }
        bx[j] <- mean(x[ni, nj], na.rm = TRUE)*f
      }
       bx
    }
  }
}
