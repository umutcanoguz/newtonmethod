f <- function(x){
  cos(x)-x
}
ft <- function(x){
  -sin(x)-1
}
p <- 0
p0 <- pi/2 
p_seq <- c()
newtonmethod <- function(f,ft,tol=1e-3,maxiter=100){
  #step1
  i <- 1
  #step2
  while(i<=maxiter){
    #step3
    p <- p0-f(p0)/ft(p0)
    p_seq [i]<-p
    #step4
    if(abs(p-p0)<tol){
      cat("algoritma başarılı",p,"\n")
      return(data.frame(p_seq))
    }
    #step5
    i <- i+1
    p0 <- p
  }
  return(p)
}
newtonmethod(f,p0)
