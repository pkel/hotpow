blocks <- read.csv('blocks.csv')
blocks$interval <- c(NA, diff(blocks$time))
blocks <- blocks[-1, ]

summary(blocks)

hist(blocks$interval, freq=F)

xrange <- range(blocks$interval)
x <- seq(xrange[1], xrange[2], 0.01)
d.should <- dgamma(x, shape=128, rate=128)
d.is <- density(blocks$interval)

plot(xrange, c(0, max(d.should, d.is$y)), type="n", xlab = "time", ylab="probability density")
lines(x, d.should, col="gray")
abline(v=1, col="gray", lty=2)
lines(d.is)
abline(v=mean(blocks$interval), lty=2)
