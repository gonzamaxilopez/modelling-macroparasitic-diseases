library(knitr)
library(phaseR)
library(deSolve)
library(graphics)
library(captioner)
#library(latex2exp)


funF <- function(x){
  gama=0.08
  z=exp(-gama)
  k= 0.7
  (1+(1-z)*x/k)^(-k-1)-(1+(1-z/2)*x/k)^(-k-1)
}

model1<- function(t,M,parameters) {
  R0<- 6
  muH<-1/70
  muP<-1/1
  
  m <- M
  dM <- (muH+muP)*(R0*funF(m)-1)*m
  list(dM)
}



example2_flowField  <- flowField(model1,
                                 xlim = c(0, 20),
                                 ylim = c(-1, 20),
                                 system ="one.dim",
                                 add = FALSE,
                                 xlab = "t", ylab ="M")
                            

grid()

example2_nullclines <- nullclines(model1,
                                  xlim   = c(0, 20),
                                  ylim   = c(0, 20),
                                  system = "one.dim",points = 101, col = c("red","blue" ), add = T,
                                  add.legend = F)
#matplot(example2_nullclines, type = c("l"), lty = 1, col = "black", lwd = 1:2)

example2_trajectory <- trajectory(model1,
                                  y0 = c(0.5,0.95,1,2,3,5),
                                  tlim = c(0, 20),
                                  system = "one.dim")

#ggplot(, aes(gp, y))

#title(main = "My title", sub = "My subtitle")
# Add X-axis
#axis(1,2,G)
#axis(1, at=x,labels=H, col.axis="red", las=2)
#axis(1, 1:7, LETTERS[1:7], col.axis = "blue")
# Add Y-axis
#axis(2)
#axis(2, col = "violet", col.axis = "dark violet", lwd = 2)
#aes(x = mpg, y = wt)














# lotkaVolterra_flowField    <- flowField(lotkaVolterra,
#                                         xlim       = c(0, 5), 
#                                         ylim       = c(0, 5),
#                                         parameters = c(1, 1, 1, 1),
#                                         add        = F)
# lotkaVolterra_trajectories <- trajectory(lotkaVolterra,
#                                          y0     = rbind(c(2, 2),
#                                                         c(0.5, 0.5),
#                                                         c(0.5, 1.5),
#                                                         c(1.5, 0.5),
#                                                         c(3, 3)),
#                                          parameters = c(1, 1, 1, 1),
#                                          col    = rep("black", 5),
#                                          tlim   = c(0, 100))
# 
# 
# xample4_flowField  <- flowField(WilsonCowan2,
#                                 xlim = c(-0.01, .425),
#                                 ylim = c(0, .5),
#                                 add  = FALSE,
#                                 ylab = TeX("$E$"),
#                                 xlab= TeX("$I$"),
#                                 frac=1)
# grid()
# example4_nullclines <- nullclines(WilsonCowan2,
#                                   xlim = c(-0.01, .425),
#                                   ylim = c(0, .5),
#                                   lty = 2, lwd = 2,
#                                   col=c("lightseagreen","aquamarine4"))
