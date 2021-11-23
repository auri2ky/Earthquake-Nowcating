library(readxl)
library(fitdistrplus)
library(actuar)
data <- read_excel("C:\Users\bhatt\Downloads\data_excel.xlsx", 
                         col_types = c("skip", "skip", "date", 
                                       "skip", "numeric", "numeric", "numeric", 
                                       "skip", "skip", "skip", "numeric"))


data$inter_event_count <- ifelse(data[,5]>=6, 1 ,0)

x <- 0
y <- 0

print(x)

for(row in 1:nrow(data)){
    x <- ifelse(data[row,5]>=6, 1, 0)
    data[row,6] <- ifelse(x==1, y, -1)
    
    y <- ifelse(x==1, 0, y+1)
}

data1 <- subset(data, data[,6] >= 1)
#data1[1,6] <-  0

arr <- data1[,6]

data2 = unname(unlist(arr))   #to convert data type to vector

print(data2)

A<-fitdist(data2, 'weibull')
B<-fitdist(data2, 'lnorm')
C<-fitdist(data2, 'gamma',method = "mle")
D<-fitdist(data2, 'exp',method = "mle")
C1<-fitdist(data2, 'gamma',method = "mme")
D1<-fitdist(data2, 'exp',method = "mme")
E<-fitdist(data2, 'invgauss',start = list(mean = 5, shape = 1))
X<-fitdist(data2,'invweibull')
# histogram of the data
hist(data2)
# Summary of the results
summary(A)
summary(B)
summary(C)
summary(D)
summary(C1)
summary(D1)
summary(E)
summary(X)
# give a chart name
png(file= "A.png")
plot(A)
# save the file
dev.off()
#plot the data
par(mfrow=c(1,1.5))
a<-c("Weibull","lognormal","gamma", "exponential" , "invgauss")
denscomp(list(A,B,C,D),legendtext = a)
cdfcomp(list(A,B,C,D),legendtext=a)
qqcomp(list(A,B,C,D),legendtext=a)
qqcomp(C)
ks.test(data2,"pweibull",0.8207141 ,104.3661258)
ks.test(data2,"plnorm", 3.955964 , 0.11959939)
ks.test(data2,"pgamma",0.747461176,  0.006422185 )
ks.test(data2,"pexp", 0.008588478)
ks.test(data2,"pinvgauss",116.42648,16.24071)
ks.test(data2,"pinvweibull",0.6126891,23.8432039)
gfMLE<-gofstat(list(A, B, C, D, E, X))
print(gfMLE)
eps_275 <- pweibull(275, shape = 0.8207141 , scale = 104.3661258)
print(eps_275)

eps_400 <- pweibull(400, shape = 0.8207141 , scale = 104.3661258)
print(eps_400)
