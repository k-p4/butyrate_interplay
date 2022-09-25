# Generating data
set.seed(24601)
d <- as.data.frame(cbind(rnorm(1:20, 500, 50), c(rep(0, 10), rep(1, 10))))
View(d)
treatment <- d$V2
outcome <- d$V1

#Difference in means
original <- diff(tapply(outcome, treatment, mean))
mean(outcome[treatment==1])-mean(outcome[treatment==0])

#Permutation test
permutation.test <- function(treatment, outcome, n){
        distribution=c()
        result=0
        for(i in 1:n){
                distribution[i]=diff(by(outcome, sample(treatment, length(treatment), FALSE), mean))
        }
        result=sum(abs(distribution) >= abs(original))/(n)
        return(list(result, distribution))
}

test1 <- permutation.test(treatment, outcome, 100000)
hist(test1[[2]], breaks=25, col='grey', main="Permutation Distribution", las=1, xlab='')
abline(v=original, lwd=3, col="red")

test1[[1]]


#Compare to t-test
t.test(outcome~treatment)