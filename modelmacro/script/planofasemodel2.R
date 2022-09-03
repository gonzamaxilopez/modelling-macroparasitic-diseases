library(knitr)
library(phaseR)
library(deSolve)
library(graphics)
library(captioner)
#library(latex2exp)

# Plot the velocity field, nullclines and several trajectories

funF <- function(x){
  gama=0.08
  z=exp(-gama)
  k= 0.7
  alpha=0.5
  (1+(1-z)*x/k)^(-k-1)-(1+(1-alpha*z)*x/k)^(-k-1)
}

gama=0.08;
z=exp(-gama);
k= 0.7;
alpha=0.5;



mtilde=(k*((1-alpha*z)/(1-z))^(1/(k+2))-k)/((z-1)*((1-alpha*z)/(1-z))^(1/(k+2))+(1-alpha*z));

R0tilde=1/funF(mtilde);

# parameters <- c(
R0c= 1;
R0a= 3;
#                 muH= 1/70,
#                 muP= 1/1,
betac= 1;
betaa= 0.5;
pic= 0.3;
pia= 0.7;
# )

R0=(pic*R0c*betac+pia*R0a*betaa)/(pic*betac+pia*betaa);




example111 <- function(t, y, parameters) {
  C<-y[1]
  A<-y[2]
  #R0c= R0c
  #R0a= R0a
  muH= 1/70
  muP= 1
  #betac= 1
  #betaa= 0.5
  #pic= 0.3
  #pia= 0.7
  
  #list(c(y[1]*(3 - y[1] - 2*betac*y[2]), y[2]*(2 - y[1] - y[2])))
  #dC <- betac*(muH+muP)*(pic*R0c*funF(C)*C+pia*R0a*funF(A)*A)/(pic*betac+pia*betaa)-(muH+muP)*C
  #dA <- betaa*(muH+muP)*(pic*R0c*funF(C)*C+pia*R0a*funF(A)*A)/(pic*betac+pia*betaa)-(muH+muP)*A
  #list(c(y[1]*(3 - y[1] - 2*y[2]), y[1]*(2 - y[1] - y[2])))
  list(c(betac*(muH+muP)*(pic*R0c*funF(C)*C+pia*R0a*funF(A)*A)/(pic*betac+pia*betaa)-(muH+muP)*C,betaa*(muH+muP)*(pic*R0c*funF(C)*C+pia*R0a*funF(A)*A)/(pic*betac+pia*betaa)-(muH+muP)*A))
}





example11_flowField <- flowField(example111,
                                 xlim = c(-1, 10),
                                 ylim = c(-1, 10),
                                 points = 21,
                                 add = FALSE)

example11_nullclines <- nullclines(example111,
                                   xlim = c(-1, 10),
                                   ylim = c(-1, 10),
                                   points = 200)
y0 <- matrix(c(1.5, .85, 1.5, 1,
               1.75, 1,1.75,0.5,1,0,2,0), 6, 2,
             byrow = TRUE)

example11_trajectory <- trajectory(example111,
                                   y0 = y0,
                                   tlim = c(0, 10))
# Determine the stability of the equilibrium point
example9_stability <- stability(example111,
                                ystar = c(0, 0))



# Determine the stability of the equilibrium points
example11_stability_1 <- stability(example111, ystar = c(0, 0))
example11_stability_2 <- stability(example111, ystar = c(0, 2))
example11_stability_3 <- stability(example111, ystar = c(1, 1))
example11_stability_4 <- stability(example111, ystar = c(3, 0))



# Plot the velocity field, nullclines and several trajectories


f <- function(t,u,) {
  p1=0.6
  p2=0.1
  du1 = p1*(u[2]-u[1])
  du2 = u[1]*(p2-u[1]) - u[2]
  return(c(du1,du2))
}

example11_flowField <- flowField(f,
                                 xlim = c(1, 5),
                                 ylim = c(1, 5),
                                 points = 21,
                                 add = FALSE)

















funF <- function(x){
  gama=0.08
  z=exp(-gama)
  k= 0.7
  (1+(1-z)*x/k)^(-k-1)-(1+(1-z/2)*x/k)^(-k-1)
}

model2<- function(t,y, parms){
  
  R0c<- 4
  R0a<- 3
  muH<-1/70
  muP<-1/1
  betac<- 1
  betaa<- .5
  pic<-.3
  pia<-.7
  
  
  
    C=y[1]
    A=y[2]
    
  dC <- betac*(muH+muP)*(pic*R0c*funF(C)*C+pia*R0a*funF(A)*A)/(pic*betac+pia*betaa)-(muH+muP)*C
  dA <- betaa*(muH+muP)*(pic*R0c*funF(C)*C+pia*R0a*funF(A)*A)/(pic*betac+pia*betaa)-(muH+muP)*A
  return(list(c(dC, dA)))
  
}
#model <- function(t, state, parms) {
#  with(as.list(c(state,parms)), {
#    dR <- r*R*(1 - R/K) - a*R*N
#    dN <- c*a*R*N - delta*N
#    return(list(c(dR, dN)))
#  })
#}


example2_flowField  <- flowField(model2,
                                 xlim = c(-5, 5),
                                 ylim = c(-5, 5),
                                 points = 200,
                                 add = FALSE)
                                 #xlim = c(0, 20),
                                 #ylim = c(0, 5),
                                 #system ="one.dim",
                                 #add = FALSE,
                                 #xlab = "t", ylab ="M")


grid()

# example2_nullclines <- nullclines(model1,
#                                   xlim   = c(0, 20),
#                                   ylim   = c(0, 20),
#                                   system = "one.dim",points = 101, col = c("red","blue" ), add = T,
#                                   add.legend = F)
#matplot(example2_nullclines, type = c("l"), lty = 1, col = "black", lwd = 1:2)

# example2_trajectory <- trajectory(model1,
#                                   y0 = c(0.5,0.95,1,2,3,5),
#                                   tlim = c(0, 20),
#                                   system = "one.dim")

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
