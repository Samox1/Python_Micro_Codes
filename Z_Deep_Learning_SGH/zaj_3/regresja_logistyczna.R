regresja_logistyczna <- function(x,y){
   X=as.matrix(cbind(matrix(1, nrow(x), 1),x))   
   w<- matrix(0, ncol(X), 1)
   w_old=w+10E9
   while ( norm(w-w_old, type="2")>0.00001 ){
	w_old=w
	p=exp(X%*%w)/(1+exp(X%*%w))
	V=diag(c(p*(1-p)))
	w=w+solve(t(X)%*%V%*%X,t(X)%*%(y-p))
   }
   rownames(w)<-NULL
   return(w)
}