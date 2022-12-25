setwd("/Users/susheel/Documents/GitHub/statistics-for-data-scientists/data")

# TASK: demonstrate Central Limit Theorem from sample of 5 and 20 with histograms
library(ggplot2)

df <- read.csv("loans_income.csv", header = TRUE)

#samp_data <- data.frame(
#  income = sample(df, 1000), 
#  type = 'data_dist')
samp_data <- data.frame(
  income = df[sample(nrow(df), 1000, replace = TRUE), ], 
  type = 'data_dist'
)
#simple random sample of 1000 values from dataset, this is just to show the DATA DISTRIBUTION and how its DISTRIBUTION is different from the SAMPLING DISTRIBUTION
#f'n from book did not work (according to SO: need to sample from numbers, not from the data frame. then use the results to get the sampled rows. Commented out code from book)

#samp_mean_5 <- data.frame(
#  income = tapply(sample(df, 1000 * 5), 
#                  rep(1:1000, rep(5, 1000)), FUN = mean), 
#  type = 'mean_of_5'
#)
samp_mean_5 <- data.frame(
  income = tapply(df[sample(nrow(df), 1000 * 5, replace = TRUE), ],
                  rep(1:1000, rep(5, 1000)), FUN = mean),
  type = 'mean_of_5')
#sample of means of 5 values
#f'n from book did not work (according to SO: need to sample from numbers, not from the data frame. then use the results to get the sampled rows. Commented out code from book)
# tapply f'n arguments: X and INDEX and FUN

samp_mean_5

samp_mean_20 <- data.frame(
  income = tapply(df[sample(nrow(df), 1000 * 20, replace = TRUE), ],  
                  rep(1:1000, rep(20, 1000)), FUN = mean), 
  type = 'mean_of_20')
#sample of means of 20 values
#f'n from book did not work (according to SO: need to sample from numbers, not from the data frame. then use the results to get the sampled rows.

income <- rbind(samp_data, samp_mean_5, samp_mean_20)

income$type = factor(income$type, levels = c('data_dist', 'mean_of_5', 'mean_of_20'), labels = c('Data', 'Mean of 5', 'Mean of 20'))

ggplot(income, aes(x = income)) + geom_histogram(bins = 40) + facet_grid(type ~ .)
#as the sample size of the sample statistic (mean) increases, it follows the central limit theorem. as the number of samples in the sample statistic increases, it follows the normal distribution -- even if the data itself does not follow the normal distribution bell curve. 

#TASK:  compare median of income from original loans dataset vs a bootstrapped sample
median(df$x)
#62000

library(boot)
median_fn <- function(x, idx) median(x[idx])
boot_obj <- boot(df$x, R = 10000, statistic = median_fn)
boot_obj
#bias of -76.91895 and standard error of 222.3674

#TASK:  plot QQ graph for 100 random numbers from a normal distribution
norm_samp <- rnorm(100)
qqnorm(norm_samp)
abline(a = 0, b = 1, col = 'grey')

#TASK: plot QQ graph for random, possibly asymmetric distribution, values
rand_samp <- floor(runif(100, min = 0, max = 101))
rand_samp <- diff(log(rand_samp[rand_samp > 0]))
qqnorm(rand_samp)
abline(a = 0, b = 1, col = 'grey')

#TASK: use binomial probabilities to answer the following: if the probability of a click converting to a sale in 0.02, what is the probability of observing 0 sales in 200 clicks? 
dbinom(x = 0, p = 0.02, size = 200)
#probability of observing 0 sales in 200 clicks when the probability of converting a click to a sale is 0.02, is 1.7%

#TASK: use binomial probabilities to answer the following: probability of observing two or fewer successes in 5 trials, where the probability of success for each trial is 0.1
pbinom(q = 2, prob = 0.1, size = 5)
#book is so outdated... the probability of observing 2 or fewer successes in 5 trials where the probability of success for each trial is 0.1, is 99%

#TASK: poisson distribution with rate of 2
rpois(100, lambda = 2)

#TASK: generate 100 random numbers from exponential distribution where mean number of events per time period is 2
rexp(n = 100, rate = 0.2)
15
#TASK: generate 100 random numbers from Weibull distribution with shape of 1.5 (probability of event increases over time) and characteristic life of 5000 (time to failure)
rweibull(n = 100, shape = 1.5, scale = 5000)
