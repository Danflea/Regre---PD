#a) Diagrama de Dispersión
plot(PD1$X,PD1$Y,main="Diagrama de Dispersión",
     col="blue",xlab="Temperatura",ylab="latidos")

#b) Coeficiente de correlación
cor(PD1$X,PD1$Y,method="p")

#c)
cor.test(PD1$X,PD1$Y,method="p",alternative = "t")
qt(0.02/2,18)
qt(c(0.02/2,1-0.02/2),18)

#d)
cor.test(PD1$X,PD1$Y,method="p",alternative = "t",
         conf.level = 0.98)$conf

#e)
modelo<-lm(Y~X,data=PD1)

#f y h)
anova(modelo)
qf(0.02,1,18,lower.tail=F)
pf(309.39,1,18,lower.tail=F)

summary(modelo)
qt(c(0.02/2,1-0.02/2),18)

#g)
confint(modelo,level = 0.98)

#i)
sig<-summary(modelo)$sigma
n<-nrow(PD1)
LI<-(n-2)*sig^2/qchisq(1-0.02/2,n-2)
LS<-(n-2)*sig^2/qchisq(0.02/2,n-2)
c(LI,LS)

#j)
summary(modelo)$r.sq*100
