x=read.table('data_mat_stk_project.txt',header=T)
attach(x)
#plot(x)

names(x)
incident_number=x$SkadeNr
year=x$SkadeAr
delay=x$avvikling
number_of_incident=x$antallskader
estimated_compensation=x$SkadeEstimat
regress=x$Regress
paid=x$UtbetaltBelop*(-1)
Reserve=reserve


##################################

estimated_compensation=x$SkadeEstimat
estimated_compensation_square=estimated_compensation^2
estimated_compensation=sqrt(estimated_compensation_square)

A=mean(estimated_compensation)
B=var(estimated_compensation)


df=data.frame(year,estimated_compensation)
aggregate(.~year,data=df,var)

####################################
x.sub=subset(x,year>=2003)


max(year=x$SkadeAr)



df=data.frame(year,delay)
aggregate(.~year,data=df,mean)


df=data.frame(year,Reserve)
aggregate(.~year,data=df,mean)


df=data.frame(year,paid)
aggregate(.~year,data=df,sd)


y=1:8
for (i in y){y[i]=nrow(subset(x,avvikling==(i-1)))}
plot(0:7,Y)








ddply(x,.(year),summarise,delay=sum(delay), paid= sum(paid),Reserve=sum(reserve))

ddply(x,.(delay),summarise, paid=sum(paid) )


x=[order(incident_number","year","delay","number_of_incident","estimated_compensation","regress","paid","reserve")]


reserve=x$RBNS
reserve_square =reserve^2
reserve=sqrt(reserve_square)


install.packages("plyr")

library(plyr)

rbind(incident_numbe,year,delay,number_of_incident,estimated_compensation,regress,paid,reserve)



delay=x$avvikling
#delay=as.factor(delay)
n=length(delay)
plot(delay)
qqnorm(x$avvikling)# The plot of the sample quantiles of the delay
mean(x$avvikling)
var(x$avvikling)
sd(x$avvikling)
max(delay)
ppois(7,0.9341546)# The Distribution of the delay with lamda=mean
plot(density(delay),main="Delay",type="p",xlab="year",ylab="f(x)")

paid=x$UtbetaltBelop*(-1)
#paid=as.factor(paid)
Mean=mean(paid)
Var=var(paid)
#sd(x$UtbetaltBelop)
Sd=sd(paid)
n=length(paid)
plnorm(n, 29449.85, 140109)
plot(n, plnorm(n, 29449.85, 140109), type ="l", col ="blue", lwd = 2)
f1 = 1/(sqrt(2*pi)*Sd)*exp^(-((log(paid)) - Mean)^2 / (2*Sd^2))

min(x$UtbetaltBelop)
x.norm<-rnorm(n=6000,m=-29449.85,sd=140109)
hist(paid,main="Histogram of what was eventually paid")
plot(density(paid),main="what was eventually paid")


head(x)
reserve=x$RBNS*(-1)
mean(reserve)
var(reserve)
sd(reserve)
max(reserve)
ppois(8300000,28322.12)
plot(density(reserve),main="The RBNS reserve put aside by the company")
head(cbind(delay,paid))
plot(delay,paid,xlab="delay",ylab="paid")

q=-reserve
plot(density(log(q[q>0])))
q[q>0]
log(q[q>0])

###################################

head(cbind(paid,reserve))
plot(reserve,paid,xlab="reserve",ylab="paid")

plot(rep(mean(paid),length(paid)),delay,xlab="(mean(paid)",ylab="delay")


####################################


#ppois(2135000,29449.85) #The Distribution of what was paid with lamda=mean

qqnorm(x$UtbetaltBelop) #The plot of the sample quantiles of what was paid

head(cbind(avvikling,UtbetaltBelop))# we set the delay and what was paid togheter.
plot(UtbetaltBelop,avvikling,xlab="UtbetaltBelop",ylab="avvikling") # The scatter plot of the delay and what 
#was eventually paid  reveals a positive linear relationship between them
abline(lm(UtbetaltBelop ~avvikling ))# we can generate a linear regression model of the two variables and draw a line with abline  
