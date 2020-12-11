# Functions

# To combine after parallel run
comb <- function(...) {
  mapply('rbind', ..., SIMPLIFY=FALSE)
}

## Minimum spanning tree ranks 
mst.rank <- function (x) {
  l.mst <- NULL
  for(f in 1:(dim(x)[2])) {
    euc.dist <- fields::rdist(t(x[,-f]))
    l.mst <- c(l.mst,sum(vegan::spantree(euc.dist)$dist))
  }
  x.rank <- rank(l.mst,ties="random")
  return(x.rank)
}

## Minimum spanning tree rank histograms
mst.rhist <- function(B,M)
{
  reps <- length(B)
  x <- rep(0,reps) 
  for(i in 1:reps){
    x[i] <- mst.rank(t(B[[i]]))[1]
  }
  # hist(x,main="",xlab=hist_xlab,ylab=hist_ylab,axes=FALSE,col="gray40",border="white",ylim=hist_ylim)
  # hist(x,breaks=seq(0,M+1,1),main="",xlab=hist_xlab,ylab=hist_ylab,axes=FALSE,col="gray40",border="white",ylim=hist_ylim)
  hist(x,breaks=seq(0,M+1,1),plot=FALSE)
}


avg.rhist <- function(B,M)
{
  reps <- length(B)
  x <- rep(0,reps) 
  for(i in 1:reps){
    B.ranks <- apply(B[[i]],2,rank)
    B.preranks <- apply(B.ranks,1,mean)
    x[i] <- rank(B.preranks,ties="random")[1]
    
  }
  # hist(x,breaks=seq(0,M+1,1),main="",xlab=hist_xlab,ylab=hist_ylab,axes=FALSE,col="gray40",border="white",ylim=hist_ylim)
  hist(x,breaks=seq(0,M+1,1),plot=FALSE)
}

## Band depth ranks
bd.rank <- function(x)
{
  d <- dim(x)
  x.prerank <- array(NA,dim=d)
  for(i in 1:d[1]) {
    tmp.ranks <- rank(x[i,])
    x.prerank[i,] <- (d[2] - tmp.ranks) * (tmp.ranks - 1)
  }
  x.rank <- apply(x.prerank,2,mean) + d[2] - 1
  x.rank <- rank(x.rank,ties="random")
  return(x.rank)
} 

## Band depth rank histograms
bd.rhist <- function(B,M)
{
  reps <- length(B)
  x <- rep(0,reps) 
  for(i in 1:reps){
    tmp <- B[[i]]
    x[i] <- bd.rank(t(tmp))[1]
  }
  # hist(x,breaks=seq(0,M+1,1),main="",xlab=hist_xlab,ylab=hist_ylab,axes=FALSE,col="gray40",border="white",ylim=hist_ylim)
  # hist(x,main="",xlab=hist_xlab,ylab=hist_ylab,axes=FALSE,col="gray40",border="white",ylim=hist_ylim)
  hist(x,breaks=seq(0,M+1,1),plot=FALSE)
}
