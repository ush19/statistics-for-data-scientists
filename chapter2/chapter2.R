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

income
