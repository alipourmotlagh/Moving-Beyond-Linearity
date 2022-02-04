
#-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
#                                                               _
#                                                               _
#                                                               _
#                                                               _
#                                                               _
#-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-



#install.packages('ISLR')


library(ISLR)


str(Wage)



fit=lm(wage~poly(age,4),data=Wage)
summary(fit)


agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
pred=predict(fit,newdata = list(age=age.grid),se=TRUE)
se.bands=cbind(pred$fit+2*pred$se.fit,pred$fit-2*pred$se.fit)
plot(age,wage, col='darkgrey')
lines(age.grid,pred$fit,lwd=2,col='blue')
matlines(age.grid,se.bands,col='blue',lty=2)








fita=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
summary(fit)


agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preda=predict(fita,newdata = list(age=age.grid),se=TRUE)
se.bandss=cbind(preda$fit+2*preda$se.fit,preda$fit-2*preda$se.fit)
plot(age,wage, col='darkgrey')
lines(age.grid,preda$fit,lwd=2,col='blue')
matlines(age.grid,se.bandss,col='blue',lty=2)


plot(fitted(fit),fitted(fita))





fitla=lm(wage~education,data=Wage)
fitlb=lm(wage~education+age,data=Wage)
fitlc=lm(wage~education+poly(age,2),data=Wage)
fitld=lm(wage~education+poly(age,3),data=Wage)
anova(fitla,fitlb,fitlc,fitld)

#----------------------------------
#nonlinear polynomial

fitglm=glm(I(wage>250)~poly(age,3),data=Wage,family=binomial)
summary(fitglm)
predglm=predict(fitglm,list(age=age.grid),se=T)
se.bandsglm=predglm$fit+cbind(fitglm=0,lower=-2*predglm$se.fit,upper=2*predglm$se.fit)
se.bandsglm[1:5,]




prob.bands=exp(se.bandsglm)/(1+exp(se.bandsglm))
matplot(age.grid,prob.bands,col='blue',lwd=c(2,1,1),lty=c(1,1,2),type='l',ylim=c(0,0.1))
points(jitter(age),I(wage>250)/10,pch='|',cex=0.5)



#-----------------
require(splines)


fitsp=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
plot(age, wage,col='darkgray')
lines(age.grid,predict(fitsp,list(age=age.grid)),col='darkgreen',lwd=2)
abline(v=c(25,40,60),lty=2,col='darkgray')




fitsps=smooth.spline(age,wage,df=16)
lines(fitsps,col='red',lwd=2)
fitsps

fitspscv=smooth.spline(age,wage,cv=TRUE)
lines(fitspscv,col='blue',lwd=2)
fitspscv



#------------------------------------
# Additive Model
#gam package

