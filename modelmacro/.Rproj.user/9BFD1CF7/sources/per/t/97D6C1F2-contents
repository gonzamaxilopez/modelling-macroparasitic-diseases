
library(rootSolve)

funF <- function(x,R0){
  gama=0.08
  z=exp(-gama)
  k= 0.7
  (1+(1-z)*x/k)^(-k-1)-(1+(1-z/2)*x/k)^(-k-1)-(1/R0)
}

funG <- function(x,R0){
  alpha=0.5
  gama=0.08
  z=exp(-gama)
  #k= 0.7
  (exp(-x*(1-z))-exp(-x*(1-alpha*z)))-(1/R0)
}



#R0<-0:0.1:
R0<-seq(0,20, by=0.0001) 

#fileConn<-file("output.txt")

output <- matrix(ncol=4, nrow=length(R0))

for (i in (1:length(R0))) {
#R0=4
#fun <- funF(x,R0=4) 
#curve(funF(x),0, 10)
All <- uniroot.all(function (x) funF(x,R0[i]),c(0,50))
#if All = numeric(0)
#print(All[1])
output[i,1] <- R0[i]
output[i,2] <- All[1]
output[i,3] <- All[2]
output[i,4] <- 0

#print(output)
#outdata<-data.frame(R0[i],All[1],All[2])
#write.table(outdata, file = "outdatabifurcation.txt", sep = "\t",row.names = F, col.names = T)
#write(c(R0[i],All[1]), fileConn)
}
output <- data.frame(output)
#write.table(output)
#close(fileConn)
write.table(output, file = "outdatabifurcation.txt", sep = "\t",row.names = F, col.names = T)

