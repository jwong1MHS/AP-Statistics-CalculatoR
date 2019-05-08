# Take 400 random samples from a binomial population of size 40
# (with n = 10 and p=0.15).
# Then calculate the mean of each sample, and plot these 400 sample means.
# this will provide a good insight as the Central Limit Theorem (CLT).


sample.means <- vector(mode = "numeric", length = 400)
sample       <- vector(mode = "numeric", length = 40)

length(sample.means)

length(sample)

head(sample.means)
## [1] 0 0 0 0 0 0 ## obviously we have not assigned values to sample.means yet

## set.seed(101)

## rbinom(N,n,p)

for(i in 1:length(sample.means)) {
  for(j in 1:length(sample)) {
  sample[j] <- rbinom(1, 10, 0.15)
  }
  sample.means[i] <- mean(sample)
}

## Plot just one sample (The only one available is the last)

hist(sample,probability=TRUE,col=gray(.9),main="Central Limit Theorem")

## Now plot the sample means for 400 samples to verify the Central Limit Theorem.

hist(sample.means,probability=TRUE,col=gray(.9),main="Central Limit Theorem")
hist(sample.means, col=gray(.9), main="Central Limit Theorem")

sample.means