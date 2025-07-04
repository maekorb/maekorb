study1 <- read.table("study1.txt",header=T)
study1
as.Date(study1$date_dead, format="%Y/%m/%d")
x <- c("92/27/2", "03/3/8", "02/14/1", "97/28/6")
as.Date(x, "%y/%d/%m")
x <- c("02/27/1992", "08/3/1992", "01/14/1992", "06/28/1993")
as.Date(x, "%m/%d/%Y")
x <- c("27/2/2001", "3/8/2003", "14/1/2003", "28/6/2007")
as.Date(x, "%d/%m/%Y")

attach(study1)
# 死亡日-外科手術日
as.Date(date_dead,"%Y/%m/%d")-as.Date(date_surg,"%Y/%m/%d")

# 最終観測日-外科手術日
as.Date(date_final,"%Y/%m/%d")-as.Date(date_surg,"%Y/%m/%d")

time_event <-as.Date(date_dead,"%Y/%m/%d")-as.Date(date_surg,"%Y/%m/%d")
time_cens  <-as.Date(date_final,"%Y/%m/%d")-as.Date(date_surg,"%Y/%m/%d")
survtime <- ifelse(is.na(time_event)==TRUE,time_cens,time_event)
survtime
cens <- ifelse(is.na(time_event)==TRUE,0,1)   # 中途打ち切り指標の生成
cens
is.na(time_event)


cbind(survtime,cens)

library(survival)
km <- survfit(Surv(survtime, cens)~1)
plot(km,xlab="days",ylab="Survival")

summary(km)

print(km, show.rmean=TRUE)

km1 <- survfit(Surv(survtime,cens)~1,conf.type="plain")
summary(km1)

plot(km,col="blue")
par(new=T)
plot(km1,col="red")

km2 <- survfit(Surv(survtime,cens)~1,conf.type="log-log")
summary(km2)

km <- survfit(Surv(survtime,cens)~1,conf.type="log")
km1 <- survfit(Surv(survtime,cens)~1,conf.type="plain")
km2 <- survfit(Surv(survtime,cens)~1,conf.type="log-log")

time <- rlnorm(30,meanlog=0,sdlog=1)
cens <- rep(1,30)  # とりあえず中途打ち切りを考えないので

ns <- 100
#---------------------------------------------------- begin:真の生存関数の描画
curve(1-plnorm(x, meanlog=0, sdlog=1),0,5,type='l',ylim=c(0,1),col="red")
#---------------------------------------------------- end:真の生存関数の描画
for(i in 1:ns){
        time <- rlnorm(30,meanlog=0,sdlog=1)
        cens <- rep(1,30)
        km <- survfit(Surv(time,cens)~1)
        par(new=T)
        plot(km, xlim=c(0,5), conf.int=FALSE)
}

