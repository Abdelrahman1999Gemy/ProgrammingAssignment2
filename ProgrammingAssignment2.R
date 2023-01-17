makeCacheMatrix<- function( x=matrix() ){
  invers<-NULL
  set<-function(y){
    x<<-y
    invers<<-NULL
  }
  get<-function(){x}
  setInvers<-function(inversCalculate){invers<<-inversCalculate}
  getInvers<-function(){invers}
  list(set=set,get=get,setInvers=setInvers,getInvers=getInvers)

}
cacheSolve<-function (x,...){
 invers<-x$getInvers()
 if(!is.null(invers)){
   message("Getting the cached data")
   return (invers)
 }
 data<-x$get()
 invers<-solve(data,...)
 x$setInvers(invers)
 invers
}