# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Title: Basic Biostatistic course, AU, spring 2016
# Author: Andreas Halgreen Eiset
# System: x86_64; Linux 4.2.0-35-generic; R version: 3.2.2
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# Data can be loaded using the excel files with the "gdata" package or the .dta
# (Stata) or SPSS files with the "foregin" package. Alternatively, data can be
# convertet to .csv files and loaded using the base function read.csv().

# The .log (Stata) files provided at
# http://www.biostat.au.dk/teaching/basicbiostat/exercises.html
# has been used as template for the answers below.

# Libraries will be called as they are used. The can all be installed with
# the install.packages() command.

# Below is a couple of convenience functions that I will use throughout the
# exercises:

# A matrix with many of the common exploratory stats including 95%CI and 95%PI
Explore <- function(d, N) {
        matrix(data = c(N,
                        summary(d),
                        sd(d),
                        sd(d)/sqrt(N),
                        ConfInt(d, c(-1, 1), .05, N),
                        qnorm(c(.025, .975), mean(d), sd(d))
        ),
        dimnames = list(c("N", "Min", "1stQu", "Median", "Mean", "3rdQu", "Max",
                          "sigma", "Std.Err", "CIlow", "CIhigh", "PIlow",
                          "PIhigh"),
                        "Value")
        )
}
# Series of plots to check normality assumption:
CheckNorm <- function(d) {
        par(mfrow = c(1, 2))
        qqnorm(d)
        qqline(d)
        hist(d, prob = TRUE)
        curve(dnorm(x, mean = mean(d), sd = sd(d)), col = "red", add = TRUE)
        par(mfrow = c(1, 1))
}
# CI for t-distribution (95%CI is part of the Explore function):
ConfInt <- function(d, limit, alpha, N) {
        mean(d) + limit * qt(1 - (alpha/2), N-1) * (sd(d)/sqrt(N))
}
# Summary stats from regression models
SumFit <- function(model) {
        print(summary(model))
        print(confint(model)) # uses log-likelihood; "confint.default" uses se
}

# I have not made a point of keeping the enviroment tidy. rm() should be called
# as appropriate and at least between sections (e.g. after exercise 1.5).

# The entire code is available at https://github.com/eiset/BasicBiostat_S2016.git

# exercise 1.1 -----------------------------------------------------------

# load the data as a data frame to "dta"
dta <- read.csv('./data/bmiwomen.csv')

# check dimensions and format of data
str(dta)
# we've got 85 observations and two variables the id variable is loaded as
# integer and bmi as numerical. This seems reasonable


## Q1
db <- dta$bmi
hist(db, prob = TRUE, xlim = c(14, 35), breaks = 9, main = "Histogram\
     density with theoretical normal overlay")
# theoretical normal distribution with the same mean and sd as in bmi
curve(dnorm(x, mean = mean(db), sd = sd(db)), col = "red", add = TRUE)

## Q2
# summary of data with base R
nrow(dta)
summary(db)
sd(db)
var(db)
quantile(db, probs = c(.01, .05, .10, .25, .50, .75, .90, .95, .99))

#  -*- -*- -*-
# The results of quantile is not exactly the same as what is found with Stata.
# If preferred another alogorithm for calculating the percentiles can be
# chosen with the parameter "type". See "?quantile".
#  -*- -*- -*-

library(moments)
skewness(db)
kurtosis(db)

## Q3
# Base R was used in the answers above. In the following the dplyr package will
# be used to run similar commands with more compact code

ConfInt(bmi, c(-1, 1), .05, nrow(bmi))

## Q4
t.test(dta$bmi, mu = 24.1, alternative = c("two.sided"))

## Q5
n <- 85
muhat <- 24.99
sigma <- 3.58
se <- sigma / sqrt(n)

# CIlow
muhat + -1 * qt(1 - (.05 / 2), n - 1) * se
# CIhigh
muhat + 1 * qt(1 - (.05 / 2), n - 1) * se

# t-test
mu0 <- 24.1 # the H0 hypothesis
t <- (muhat - mu0) / (sigma/sqrt(n)) # t statistic
2 * pt(t, n - 1, lower = FALSE)

# exercise 1.2 -----------------------------------------------------------

dta <- read.csv("./data/trigly.csv")
dim(dta)
str(dta)

library(dplyr)
# For strata (sample)
group_by(dta, sample) %>%
  summarise(N = n(),
            mean(trigly),
            median(trigly),
            sd(trigly)
            )

# For total
dta %>%
  summarise(N = n(),
            mean(trigly),
            median(trigly),
            sd(trigly)
            )

## Q1
# CI calculated assuming t-distribution
dta %>%
  filter(sample == 5) %>%
  summarise(obs = n(),
            mu = mean(trigly),
            se = sd(trigly) / sqrt(obs),
            CIlow = mu + -1 * qt(1 - (.05 / 2), obs - 1) * se,
            CIhigh = mu + 1 * qt(1 - (.05 / 2), obs - 1) * se
            )

## Q2
# Calculate prediction interval
mu <- mean(dta$trigly)
sigma <- sd(dta$trigly)
qnorm(c(.025, .975), mu, sigma)

## Q3
# Explore how good the calculated PI fits the data.

matrix(data = c(nrow(dta[dta$trigly < (qnorm(.025, mu, sigma)), ]),
                nrow(dta[dta$trigly > (qnorm(.975, mu, sigma)), ]),
                nrow(data.frame(dta$trigly, na.rm = TRUE))
                ),
       dimnames = list(c("below", "above", "data excl. NAs"), "n")
       )

#  -*- -*- -*-
# There is 427 observations when excluding obersvations with NA
# (i.e. no observations of NA). There is zero observations below the lower PI
# limit. There is 18/427 ~ 4% of the observations above the upper PI limit.
# A good 95%PI expects 2.5% below and 2.5% above, so it is not good.
# To count the number of observation below and above the PI limits is in general
# not the most effecient way of checking normality (here we prefere the QQ-plot).
#  -*- -*- -*-

## Q4
# Skewed distribution: Assumption of normality not satisfied! Show this on
# histogram

hist(dta$trigly, prob = TRUE)
abline(v = c(.08363665, .91542655), col = "red")
curve(dnorm(x, mean = mu, sd = sigma), col = "blue", add = TRUE)

# rm(list = ls()) # !!!This command clears everything in the workspace!!!

# exercise 1.3 -----------------------------------------------------------

# From the stata log: "A more detailed solution is given in the file:
# "solution1_3.pdf".

dta <- read.csv("./data/siblings.csv")
str(dta)

apply(dta, 2, function(x) c(summary(x),
                            sigma = sd(x),
                            quantile(x, c(.10, .25, .50, .75, .90))
                            )
      )
# summary () will always indicate any NAs in the data. There is no indication of
# the coding of sex in the two first variables. From the stata log file: 1 = boy.

# Create new variable "wdif"
dta$wdif <- dta$weight2nd - dta$weight1st

# First only boy-boy (bb); Then boy-girl (bg)
bb <- dta[dta$sex1st == 1 & dta$sex2nd == 1, ]

## Q1bb
# Dotplot, boxplot, QQ-plot and histogram of the differences in birth weight
bd <- bb$wdif
plot(bd); boxplot(bd);

CheckNorm(bd)

# -*- -*- -*-
# Data reasonably well normally distributed. One observation sticks out, one can
# try to analyze the data without to see if it changes the results.
# -*- -*- -*-

summary(bb$wdif) # The one that "sticks out" is at -2440

# Histogram without the "outlier"
xo <- bb[bb$wdif > -2000, "wdif"]
hist(xo, prob = TRUE); curve(dnorm(x, mean = mean(xo), sd = sd(xo)), col = "red",
                             add = TRUE)
rm(xo)

## Q2bb
n <- length(bd); mu <- mean(bd); sigma <- sd(bd); se <- sigma / sqrt(n)

mu + -1 * qt(1 - (.05 / 2), n - 1) * se; mu + 1 * qt(1 - (.05 / 2), n - 1) * se

# -*- -*- -*-
# Difference on AVERAGE birth weight: the second born expected to be 112-250g
# heavier. 0 is not inside the CI, which means that p-value<5%
# -*- -*- -*-

## Q3bb
qnorm(c(.025, .975), mu, sigma)

# -*- -*- -*-
# The width of PI (-866 ; 1228) shows that, under the assumption of normality,
# in more than 50% of the cases the second born will be larger than the first born.
# -*- -*- -*-

## Q4bb
# t-test
t.test(bd, mu = 0, alternative = c("two.sided"))

# -*- -*- -*-
# If there is no difference in birth weight of boys, it would be unlikely to
# observe such a large difference as we have seen
# -*- -*- -*-

## Q5bb
# The assumption of normality is supported by the data. There is only one
# observation that stands out. In general there is strong evidence that the second
# born is larger than the first born with an estimated mean difference of 181
# (95%CI = 112; 250, p < 0.0000). We would expect (in 95% of the cases) the
# second born to be between -866 and 1228 larger than the first born, thus, in
# more than 50% of the cases the second born would be largest.

rm(bb, mu, n, se, sigma, bd)

## Q1bg
# Repeat all for boy-girl (bg)
bg <- dta[dta$sex1st == 1 & dta$sex2nd == 2, ]

# Dotplot, boxplot, QQ-plot and histogram of the differences in birth weight
bgd <- bg$wdif
plot(bgd); boxplot(bgd); qqnorm(bgd); qqline(bgd)
hist(bgd, prob = TRUE); curve(dnorm(x, mean = mean(bgd), sd = sd(bgd)), col = "red",
                              add = TRUE)

# -*- -*- -*-
# Data reasonably well normally distributed. There does not seem to be observations
# that "stands out".
# -*- -*- -*-

## Q2bg
n <- length(bgd); mu <- mean(bgd); sigma <- sd(bgd); se <- sigma / sqrt(n)
mu + -1 * qt(1 - (.05 / 2), n - 1) * se; mu + 1 * qt(1 - (.05 / 2), n - 1) * se

## Q3bg
qnorm(c(.025, .975), mu, sigma)

## Q4bg
t.test(bgd, mu = 0, alternative = c("two.sided"))

## Q7
# From the stata log file: Maybe a little bit nicer plots. More symmetric around
# 0 - probably no significant differences. In both cases PIs are wider than CIs.
# For boy-boy the mean is over zero, for boy-girl around 0, but we can't leave
# out that the mean birth weight of the girl is 135g greater.

# rm(list = ls()) # !!!This command clears everything in the workspace!!!

# exercise 1.4 -----------------------------------------------------------
dta <- read.csv("./data/trigly.csv")

dta$lntri <- log(dta$trigly)

## Q1
qqnorm(dta$lntri); qqline(dta$lntri)

## Q2
mu <- mean(dta$lntri); sigma <- sd(dta$lntri); qnorm(c(.025, .975), mu, sigma)

## Q3
matrix(data = c(nrow(dta[dta$lntri < (qnorm(.025, mu, sigma)), ]),
                nrow(dta[dta$lntri > (qnorm(.975, mu, sigma)), ]),
                nrow(data.frame(dta$trigly, na.rm = TRUE)),
                nrow(dta[dta$lntri < (qnorm(.025, mu, sigma)), ])/
                  nrow(data.frame(dta$trigly, na.rm = TRUE)),
                nrow(dta[dta$lntri > (qnorm(.975, mu, sigma)), ])/
                  nrow(data.frame(dta$trigly, na.rm = TRUE))
                ),
       dimnames = list(c("below", "above", "data excl. NAs", "pct.below",
                         "pct.above"), "n")
       )
# There is (almost) exactly 2.5% of the observations below the lower limit of
# the 95%PI and 3% of the observations are above. The calculated PI seems reasonable.

# rm(list = ls()) # !!!This command clears everything in the workspace!!!

# exercise 1.5 -----------------------------------------------------------

dta <- read.csv("./data/siblings.csv")

library(ggplot2)
dta$sex1 <- factor(dta$sex1st, labels = c("boy", "girl"))
dta$sex2 <- factor(dta$sex2nd, labels = c("boy", "girl"))

g <- ggplot(dta, aes(weight1st, weight2nd)) +
  geom_point(alpha = 0.6) +
  facet_grid(sex1 ~ sex2) +
  labs(x = "weight in grams, first child", y = "weight in grams, second child")
g

## Q1
# Discussion about independence: If the first child is big/small, the same is
# expected for the secondThis is probably due to a mixture of heritage and common
# environment.

# The interpretation can be improved by adding a "guideline":

g + geom_smooth(method = "lm")

# We also observe regression towards the mean: the mean for the second child
# was a bit higher when the first child was "short" and vice versa.


# exercise 2.1 -----------------------------------------------------------

dta <- read.csv("./data/fishoil.csv")
# Difference in blood pressure in 430 pregnant women in two diet groups.

str(dta)
# There are four (integer) variables of 430 observations. From the standard
# answers group "1" is controls and group "2" is labelled as "Fish Oil". For ease
# of translation the group variable is recoded as factor giving the same names
dta$grp <- ifelse(dta$group == 1, "control", "fish oil")
dta$grp <- factor(dta$grp)
str(dta)

## Q1
# In base R
# qqnorm(dta[dta$group == "control", "systol"])
# qqline(dta[dta$group == "control, "systol"])

# With ggplot both broups can easily be plotted side by side
library(ggplot2)
g <- ggplot(dta, aes(sample = systol)) +
  facet_grid(. ~ group) +
  stat_qq(alpha = 0.5) +
  geom_abline(intercept = mean(dta$systol), slope = sd(dta$systol), colour = "blue")
g
# From the stata log file: "Except the outliers, it looks like the normal
# distribution is a good approximation. The data are spread more or less equally
# in the 2 groups."

## Q2
library(dplyr)
library(tidyr)
# For strata (sample)
SummaLikeStata <- function(dataframe) {
        group_by(dataframe, grp) %>%
                summarise("1%" = quantile(systol, probs = .01),
                          "5%" = quantile(systol, probs = .05),
                          "10%" = quantile(systol, probs = .10),
                          "25%" = quantile(systol, probs = .25),
                          "50%" = quantile(systol, probs = .50),
                          "75%" = quantile(systol, probs = .75),
                          "90%" = quantile(systol, probs = .90),
                          "95%" = quantile(systol, probs = .95),
                          "99%" = quantile(systol, probs = .99),
                          min = min(systol),
                          max = max(systol),
                          N = n(),
                          mean = mean(systol),
                          median = median(systol),
                          sigma = sd(systol),
                          s2 = var(systol),
                          CI_low = mean + -1 * qt(1 - (.05 / 2), N - 1) * sigma / sqrt(N),
                          CI_up = mean + 1 * qt(1 - (.05 / 2), N - 1) * sigma / sqrt(N)
                ) %>%
                gather(type, value, -1) %>%
                spread(grp, value) # to adjust the layout of the print
}
SummaLikeStata(dta)

t.test(dta[dta$grp == "control", "systol"],
       dta[dta$grp == "fish oil", "systol"],
       alternative = c("two.sided"), paired = FALSE)
# The last line gives the default for R and more parameteres may be set as
# appropriate(see ?t.test).

# From the stata log file: We see again the 2 outliers in the Control group with
# values 56 og 84 mmHg. Medians and means are nor very far from each other. The
# standard deviations are quite similar. The "Fish oil" group does not contain
# number 0 in the CI. They are quite narrow thanks to the amount of information.

## Q3
var.test(dta[dta$grp == "control", "systol"],
         dta[dta$grp == "fish oil", "systol"])
# We can accept the hypothesis of equal variances: Variance differs only by 2%
# (F = 0.9838)

## Q4
sd(dta$systol)

# To get each df for each group:
df1 <- as.numeric(t.test(dta[dta$grp == "control", "systol"])$parameter)
df2 <- as.numeric(t.test(dta[dta$grp == "fish oil", "systol"])$parameter)
# Another more simplistic approach would be to just use lenght()
# or nrow()-1 for each subset.

# Calculation of the common std. deviation
sqrt(
  sum(
  var(dta[dta$grp == "control", "systol"]) * df1 +
    var(dta[dta$grp == "fish oil", "systol"]) * df2
  ) /
    sum(df1 + df2)
  )

## Q5
t.test(dta[dta$group == "control", "systol"], dta[dta$group == "fish oil", "systol"])
# From the stata log file:  It's important to remark here the CI and comment on
# it. Comment on the lower and upper limit and not only about the 0 being in it.
# Remember that there is a difference in the increase in blood pressure (in the
# Fish oil group). A comment about the outliers would be appropriate.


# -*- -*- -*- -*-
# Methods: Mean systolic blood pressure (SBP) were compared between the fish oil
# group and controls using Welch t-test of two independt samples. The assumption
# of normality were examined above.

# Results and conclusion: The SBP were approximately normal although the control
# group contained two outliers at 56 and 84 mmHg. I assume the two groups are
# different people (though from the same population) and are thus independent.
# The mean SBP in the control group was 1.40 (95%CI: -0.56; 3.37) mmHg and for
# the fish oil group 2.72 (95%CI: 0.76; 4.69). The difference in means between
# the control and the fish oil group was 1.32 (95%CI: -4.09; 1.45) and was not
# statistically significant (p = 0.35).
# -*- -*- -*- -*-

# exercise 2.2 -----------------------------------------------------------

dta <- read.csv("./data/fishoil.csv")
dta$grp <- ifelse(dta$group == 1, "control", "fish oil"); dta$grp <- factor(dta$grp)

## Q1
# Drop the two outliers in the control group: E.g. create new data frame.

dta2 <- filter(dta, systol >= -50)

# From the stata log file: Data is now nicer in the control group
ggplot(dta2, aes(grp, systol)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.4)

## Q2
# there are several ways of creating histogram (or density plot) with an overlayed
# normal curve. A simplistic method - with base R - is the one used previously
# after subsetting:

cntrl <- dta2[dta2$grp == "control", "systol"]
fish <- dta2[dta2$grp == "fish oil", "systol"]
par(mfrow = c(1, 2)) # To get both plot in same graph
hist(cntrl, prob = TRUE, ylim = c(0, 0.04))
curve(dnorm(x, mean = mean(cntrl), sd = sd(cntrl)), col = "red", add = TRUE)
hist(fish, prob = TRUE, ylim = c(0, 0.04))
curve(dnorm(x, mean = mean(fish), sd = sd(fish)), col = "red", add = TRUE)
boxplot(bd); qqnorm(bd); qqline(bd)

## Q3
SummaLikeStata(dta2)

t.test(dta2[dta2$grp == "control", "systol"],
       dta2[dta2$grp == "fish oil", "systol"],
       alternative = c("two.sided"), paired = FALSE)
## Q4
var.test(dta2[dta2$grp == "control", "systol"],
         dta2[dta2$grp == "fish oil", "systol"])

# Calculation of the common std. deviation
df1 <- as.numeric(t.test(dta2[dta2$grp == "control", "systol"])$parameter)
df2 <- as.numeric(t.test(dta2[dta2$grp == "fish oil", "systol"])$parameter)

sqrt(
  sum(
    var(dta2[dta2$grp == "control", "systol"]) * df1 +
      var(dta2[dta2$grp == "fish oil", "systol"]) * df2
  ) /
    sum(df1 + df2)
)

# From the stata log file: Small changes in the control group and the variance is
# now smaller.

# exercise 2.3 ------------------------------------------------------------

dta <- read.csv("./data/hp.csv")
str(dta)
# Since we are going to perform numerus calculations on the "day" variable it
# would be logical to code it as a numeric variable:
dta$day <- as.numeric(dta$day)
str(dta)

# 1 = passive, 2 = active
dta$grp <- ifelse(dta$group == 1, "passive", "active"); dta$grp <- factor(dta$grp)

## Q1
summary(dta)

psiv <- dta[dta$grp == "passive", "day"]
actv <- dta[dta$grp == "active", "day"]

par(mfrow = c(1, 2)) # To get both plot in same graph
hist(psiv, prob = TRUE, ylim = c(0, 0.006))
curve(dnorm(x, mean = mean(psiv), sd = sd(psiv)), col = "red", add = TRUE)
hist(actv, prob = TRUE, ylim = c(0, 0.006))
curve(dnorm(x, mean = mean(actv), sd = sd(actv)), col = "red", add = TRUE)

# From the stata log file: A slightly long tail of the passive.

## Q2
qnorm(c(.025, .975), mean(psiv), sd(psiv))

# Below is a very long code all just to give a nice output. It can however be
# wrapped in a function to use for later

Compare <- function(d) {
        d %>% group_by(grp) %>%
                mutate(mu = mean(day),
                       sigma = sd(day),
                       PIlow = qnorm(.025, mu, sigma),
                       PIhigh = qnorm(.975, mu, sigma)) %>%
                filter(day < PIlow | day > PIhigh) %>%
                mutate(blw = as.numeric(ifelse(day < PIlow, day, NA)),
                       abv = as.numeric(ifelse(day > PIhigh, day, NA)),
                       npil = as.numeric(ifelse(day < PIlow, 1, 0)),
                       npih = as.numeric(ifelse(day > PIhigh, 1, 0))
                ) %>%
                summarise(PIlow = qnorm(.025, mu, sigma)[1],
                          PIhigh = qnorm(.975, mu, sigma)[1],
                          N.below = sum(npil),
                          mu.below = mean(blw, na.rm = TRUE),
                          sd.below = sd(blw),
                          min.below = min(blw),
                          max.below = max(blw),
                          N.above = sum(npih),
                          mu.above = mean(abv, na.rm = TRUE),
                          sd.above = sd(abv),
                          min.above = min(abv),
                          max.above = max(abv)
                ) %>%
                gather(type, value, -1) %>%
                arrange(desc(grp))
}
head(Compare(dta), 15)

# From the stata log file: The slightly long tail of the passive matters to the
# validity of the prediction interval; the PI are less accuarate for the passive
# as compared to the active.

## Q3
# From the stat log file: Active have a longer heart period (ie lower heart rate)
# than the passive (the mean difference somewhere between 33 and 120ms). Although
# the slightly long tail of the passive is expected to have an impact of the
# validity of the prediction intervals, the mean comparison of the two groups is
# expected to be robust to this potential small departure of normalisty. There are
# no problems due to variance homogenity. The non-parametric test gives the same
# conclusion.

t.test(dta[dta$grp == "passive", "day"],
       dta[dta$grp == "active", "day"],
       alternative = c("two.sided"), paired = FALSE)

var.test(dta[dta$grp == "passive", "day"],
         dta[dta$grp == "active", "day"])

wilcox.test(dta[dta$grp == "passive", "day"],
            dta[dta$grp == "active", "day"],
            alternative = c("two.sided"),
            correct = TRUE, conf.int = TRUE, paired = FALSE)

# exercise 2.4 ------------------------------------------------------------

## Q1: ≈55

## Q2
power.t.test(sd = 8, delta = 5, power = .90,
             type = "two.sample", alternative = "two.sided")

## Q3
power.t.test(n = 200, sd = 25, delta = 5,
             type = "two.sample", alternative = "two.sided")

# exercise 2.5 ------------------------------------------------------------

dtalog <- read.csv("./data/hp.csv")
dtalog$day <- as.numeric(dtalog$day)
dtalog$grp <- ifelse(dtalog$group == 1, "passive", "active")
dtalog$grp <- factor(dtalog$grp)

# Logarithmic transformation of the day variable
dtalog$day <- log(dtalog$day)

## Q1
summary(dtalog)

ggplot(dtalog, aes(grp, day)) +
        geom_boxplot() +
        geom_jitter(alpha = 0.4)

psivlog <- dtalog[dtalog$grp == "passive", "day"]
actvlog <- dtalog[dtalog$grp == "active", "day"]

par(mfrow = c(1, 2)) # To get both plot in same graph
hist(psivlog, prob = TRUE, ylim = c(0, 4))
curve(dnorm(x, mean = mean(psivlog), sd = sd(psivlog)), col = "red", add = TRUE)
hist(actvlog, prob = TRUE, ylim = c(0, 4))
curve(dnorm(x, mean = mean(actvlog), sd = sd(actvlog)), col = "red", add = TRUE)

# -*- -*- -*- -*-
# If anything the log transformed data fit the normality assumption better
# -*- -*- -*- -*-

## Q2
qnorm(c(.025, .975), mean(psivlog), sd(psivlog))
# Now the previous function come in handy
log_comp <- Compare(dtalog)

# To get the exponentiated values - e.g. the 95%PI compared with the geometric
# and arithmetic mean for the passive above their PI:
matrix(data = c(exp(log_comp[1, "value"]),
                exp(log_comp[2, "value"]),
                exp(log_comp[9, "value"]),
                mean(psiv[psiv > qnorm(.975, mean(psiv), sd(psiv))])
                ),
       dimnames = list(c("exp.PIlow", "exp.PIhigh", "mean.geom", "mean.arit"),
                       "value")
       )
# As expected the geometric mean is smaller than the arithmetric mean.
# To get an (almost) generic code the expression  'log_comp$grp == "passive" &
# log_comp$type == "PIlow"' can be used in place of the rownumbers.

## Q3
# From the stata log file: There is a difference between 0.04 and 0.14

t.test(dtalog[dtalog$grp == "passive", "day"],
       dtalog[dtalog$grp == "active", "day"])

var.test(psivlog, actvlog)

wilcox.test(psivlog, actvlog,
            alternative = c("two.sided"),
            correct = TRUE, conf.int = TRUE, paired = FALSE)

# exercise 2.6 ------------------------------------------------------------
dta <- read.csv("./data/siblings.csv")
str(dta)

dta <- dta[!dta$sex1st == 2, ]
# We are now down to 503 observations
# Long format. Pair variable is introduced to keep the pairs
dta <- dta %>%
        mutate(pair = 1:nrow(dta),
               sex1st = ifelse(.$sex1st == 1, "boy", "girl"),
               sex2nd = ifelse(.$sex2nd == 1, "boy", "girl")) %>%
        gather(nbrwt, weight, -c(pair, sex1st, sex2nd)) %>%
        gather(nbrsx, sex, -c(nbrwt, weight, pair)) %>%
        mutate(nbrwt = ifelse(.$nbrwt == "weight1st", 1, 2),
               nbrsx = ifelse(.$nbrsx == "sex1st", 1, 2)) %>%
        filter(nbrwt == nbrsx) %>%
        select(- nbrsx) %>%
        rename(bth.odr = nbrwt) %>%
        mutate(pair = factor(pair),
               bth.odr = factor(bth.odr),
               weight = as.numeric(.$weight),
               sex = factor(sex))

## Q1
boy.scnd <- dta[dta$sex == "boy" & dta$bth.odr == 2, "weight"]
grl.scnd <- dta[dta$sex == "girl" & dta$bth.odr == 2, "weight"]
CheckNorm(boy.scnd)
CheckNorm(grl.scnd)

# From stata log file: Birth weights follows approximately a normal distribution.

filter(dta, bth.odr == 2) %>%
        ggplot(aes(weight, fill = sex)) +
        geom_histogram(position = "identity", alpha = 0.5)
# alternatively geom_density could be used:
filter(dta, bth.odr == 2) %>%
        ggplot(aes(weight, fill = sex)) +
        geom_density(alpha = 0.5)

var.test(boy.scnd, grl.scnd)

t.test(boy.scnd, grl.scnd)

# From stata log file: On average boys heavier than girls.

## Q2
dta <- dta %>% group_by(pair) %>%
        mutate(wdif = sum(weight - weight[bth.odr == 1])) %>%
        data.frame()

# Similar to above
wdif.boy.scnd <- dta[dta$sex == "boy" & dta$bth.odr == 2, "wdif"]
wdif.grl.scnd <- dta[dta$sex == "girl" & dta$bth.odr == 2, "wdif"]
CheckNorm(wdif.boy.scnd)
CheckNorm(wdif.grl.scnd)
# It seems that the normal condition is fulfilled.

# We only have data where the first born is a boy.

var.test(wdif.boy.scnd, wdif.grl.scnd)
# -*- -*- -*-
# We fail to reject the hypothesis of different variances of the weight difference
# of the 1st and the 2nd child depending of the sex of the 2nd child. This
# condition for the parametric statistics is fulfilled.
# For the purpose of this assignment I assume independence of the sex of the
# second child.
# -*- -*- -*-

t.test(wdif.boy.scnd, wdif.grl.scnd)
# We reject the hypothesis of no weight difference of the 1st and the 2nd child.

t.test(wdif.boy.scnd)
t.test(wdif.grl.scnd)
# From stata log file: Second boy is heavier than 1st one, but 2nd girl is not
# statistical signifikant heavier than the 1st one.


# exercise 3.1 ------------------------------------------------------------

## Q1
dta <- read.csv("./data/energy.csv")
str(dta)

dta <- dta %>% mutate(dif = post - pre,
                      ave = (post + pre) / 2)

summary(dta$dif); nrow(dta); sd(dta$dif)

# Check same distribution:
# 1. Scatterplot with guideline (blue, including 95%CI) and
ggplot(dta, aes(pre, post)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_function(fun = function(x) x, geom = "line") +
        labs(x = "Pre-menstrual intake",
             y = "Post-menstrual intake")

# 2. Bland-Altman plot including line at 0, mean and 2 standard diviations above
# and below the mean (dashed).
mu <- mean(dta$dif); sigma <- sd(dta$dif)

ggplot(dta, aes(ave, dif)) +
        geom_point() +
        geom_hline(yintercept = c(mu, 0)) +
        geom_hline(yintercept = c(ConfInt(dta$dif, 1, .05, nrow(dta)),
                                  ConfInt(dta$dif, -1, .05, nrow(dta))
                                  ),
                   linetype = 2) +
        geom_hline(yintercept = c(qnorm(.975, mu, sigma),
                                  qnorm(.025, mu, sigma)
                                  ),
                   linetype = 1, colour = "darkgrey")

# Check normality:
CheckNorm(dta$dif)

## Q2
t.test(dta$pre, dta$post, paired = TRUE)

# 95%PI:
qnorm(c(.025, .975), mu, sigma)

## Q3
# see Q1 Bland-Altman plot

# exercise 3.2 ------------------------------------------------------------
dta <- read.csv("./data/alcohol.csv")
str(dta)

dta$dif <- dta$leg - dta$heart
dta$ave <- (dta$leg + dta$heart) / 2

## Q1
ggplot(dta, aes(heart, leg)) +
        geom_point() +
        geom_smooth(method = "lm") +
        stat_function(fun = function(x) x, geom = "line")
CheckNorm(dta$dif)

# From the stata log: Heart values are on average bigger. But for the actual 
# values we can also see the opposite as seen from the scatter plot below.

## Q2 + Q3 + Q4
Explore(dta$dif, length(dta$dif))
mu <- mean(dta$dif); sigma <- sd(dta$dif)
ggplot(dta, aes(ave, dif)) +
        geom_point() +
        geom_hline(yintercept = mu) +
        geom_hline(yintercept = 0, colour = "red") +
        geom_hline(yintercept = c(ConfInt(dta$dif, 1, .05, nrow(dta)),
                                  ConfInt(dta$dif, -1, .05, nrow(dta))
                                  ),
                   linetype = 2) +
        geom_hline(yintercept = c(qnorm(.975, mu, sigma),
                                  qnorm(.025, mu, sigma)
                                  ),
                   linetype = 1, colour = "blue")
CheckNorm(dta$dif)

t.test(dta$heart, dta$leg, paired = TRUE)

# exercise 3.3 ------------------------------------------------------------
# From the stata log file: The important point is that the authors do not compare
# the treatment groups, but only the 2 groups separately. A statistical
# non-significant finding can be due to either no true difference between the
# baseline and after 1 week, but it can also be due to a too small sample to
# statistical identfy the true difference.

dta <- read.csv("./data/captopril.csv")
# From the stata log file: RCTof diabetics dependent on insulin with nephropathy
# (Hommel et al., BMJ, 1986)
str(dta)
dta$grp <- ifelse(dta$group == 1, "captopril", "placebo")
dta$grp <- factor(dta$grp)

Explore(dta$baseline, length(dta$baseline)); Explore(dta$oneweek, length(dta$oneweek))
summary(dta$grp)

## Q1
t.test(dta[dta$group == 1, "baseline"], dta[dta$group == 1, "oneweek"], paired = TRUE)

t.test(dta[dta$group == 2, "baseline"], dta[dta$group == 2, "oneweek"], paired = TRUE)

# Q2
dta$dif <- with(dta, oneweek - baseline)
dta$ave <- with(dta, (oneweek + baseline) / 2)

ggplot(dta, aes(baseline, oneweek)) +
        geom_point() +
        geom_smooth(method = "lm", aes(colour = "black")) +
        stat_function(fun = function(x) x, geom = "line", aes(colour = "blue")) +
        scale_colour_manual(name = "legend",
                            values = c("black", "blue"),
                            labels = c("captopril", "y = x"))

t.test(dta[dta$grp == "captopril", "dif"], dta[dta$grp == "placebo", "dif"])
t.test(dta[dta$grp == "captopril", "dif"], mu = 0)
t.test(dta[dta$grp == "placebo", "dif"], mu = 0)
var.test(dta[dta$grp == "captopril", "dif"], dta[dta$grp == "placebo", "dif"])
wilcox.test(dta[dta$grp == "captopril", "dif"],
            dta[dta$grp == "placebo", "dif"],
            alternative = c("two.sided"),
            correct = TRUE, conf.int = TRUE, paired = FALSE)
# R gives a warning since the data contains ties. With base R functon wilcox.test()
# it defaults to normal approximation with ties. Alternatively use wilcox_test()
# function in the coin package.

# From the stata log file: Note, all though the p-values of the t-test and the
# Wilcoxon rank-sum test are similar (eidt: not so in base R), only the latter is
# statistical significant. In general the non-parametric test will have slight
# lower power than the t-test, resulting in general in a slightly higher p-values.
# This example is however opposite. Both tests are valid. We should of course present
# the test specified in the study protocol.

# exercise 3.4 ------------------------------------------------------------
# Fron the stata log file: See the detailed solution in Solution3-4.pdf. We will
# see that it is reasonable to look at relative changes between day and night
# since difference from night to day depends on starting. The relative decline is
# almost identical

dta <- read.csv("./data/hp.csv")
str(dta)
dta$grp <- ifelse(dta$group == 1, "passive", "active")
dta$grp <- factor(dta$grp)

## Q1
ggplot(dta, aes(day, night)) +
        geom_point(aes(shape = grp)) +
        geom_smooth(method = "lm", aes(colour = "black")) +
        stat_function(fun = function(x) x, geom = "line", aes(colour = "blue")) +
        scale_colour_manual(name = "legend",
                            values = c("black", "blue"),
                            labels = c("heart period", "y = x"))

dta$dif <- with(dta, night - day); dta$ave <- with(dta, (day + night) / 2)
var.test(dta[dta$group == 1, "dif"], dta[dta$group == 2, "dif"])
t.test(dta[dta$group == 1, "dif"], dta[dta$group == 2, "dif"])


# The result of the t test is not exactly the same as the one given in the log
# file. The default in R is assuming unequal variance (Welch's t test). The power
# of Welch's t test comes close to that of Student's even with equal variance.
# If the Student's t test is perferred the option "var.equal = TRUE" can be added
# to the call - this will give the same results as in Stata.

mu.psv <- mean(dta[dta$group == 1, "dif"])
sigma.psv <- sd(dta[dta$group == 1, "dif"])
mu.actv <- mean(dta[dta$group == 2, "dif"])
sigma.actv <- sd(dta[dta$group == 2, "dif"])

g1 <- filter(dta, group == 1) %>%
        ggplot(aes(ave, dif)) +
        geom_point() +
        geom_hline(yintercept = mu.psv) +
        geom_hline(yintercept = 0, colour = "red") +
        geom_hline(yintercept = c(ConfInt(dta[dta$group == 1, "dif"], 1, .05,
                                          length(dta[dta$group == 1, "dif"])),
                                  ConfInt(dta[dta$group == 1, "dif"], -1, .05,
                                          length(dta[dta$group == 1, "dif"]))
        ),
        linetype = 2) +
        geom_hline(yintercept = c(qnorm(.975, mu.psv, sigma.psv),
                                  qnorm(.025, mu.psv, sigma.psv)
        ),
        linetype = 1, colour = "blue") +
        scale_y_continuous(limits = c(-10, 450)) +
        labs(title = "Passive\nBland-Altman plot\nwith lines mean, 95CI, 95PI, and 0")

g2 <- filter(dta, group == 2) %>%
        ggplot(aes(ave, dif)) +
        geom_point() +
        geom_hline(yintercept = mu.actv) +
        geom_hline(yintercept = 0, colour = "red") +
        geom_hline(yintercept = c(ConfInt(dta[dta$group == 2, "dif"], 1, .05,
                                          length(dta[dta$group == 2, "dif"])),
                                  ConfInt(dta[dta$group == 2, "dif"], -1, .05,
                                          length(dta[dta$group == 2, "dif"]))
        ),
        linetype = 2) +
        geom_hline(yintercept = c(qnorm(.975, mu.actv, sigma.actv),
                                  qnorm(.025, mu.actv, sigma.actv)
        ),
        linetype = 1, colour = "blue") +
        scale_y_continuous(limits = c(-10, 450)) +
        labs(title = "Active\nBland-Altman plot\nwith lines mean, 95CI, 95PI, and 0")

library(gridExtra)
grid.arrange(g1, g2, ncol = 2)

CheckNorm(dta[dta$group == 1, "dif"]); CheckNorm(dta[dta$group == 2, "dif"])

## Q2
dta$ldif <- with(dta, log(night/day)); dta$lave <- with(dta, log(night * day))

ggplot(dta, aes(log(day), log(night))) +
        geom_point() +
        geom_smooth(method = "lm", aes(colour = "black")) +
        stat_function(fun = function(x) x, geom = "line", aes(colour = "blue")) +
        scale_colour_manual(name = "legend",
                            values = c("black", "blue"),
                            labels = c("heart period", "y = x")) +
        facet_grid(. ~ grp)

var.test(dta[dta$group == 1, "ldif"], dta[dta$group == 2, "ldif"])
t.test(dta[dta$group == 1, "ldif"], dta[dta$group == 2, "ldif"])

mu.psv <- mean(dta[dta$group == 1, "ldif"])
sigma.psv <- sd(dta[dta$group == 1, "ldif"])
mu.actv <- mean(dta[dta$group == 2, "ldif"])
sigma.actv <- sd(dta[dta$group == 2, "ldif"])

g1 <- filter(dta, group == 1) %>%
        ggplot(aes(lave, ldif)) +
        geom_point() +
        geom_hline(yintercept = mu.psv) +
        geom_hline(yintercept = 0, colour = "red") +
        geom_hline(yintercept = c(ConfInt(dta[dta$group == 1, "ldif"], 1, .05,
                                          length(dta[dta$group == 1, "ldif"])),
                                  ConfInt(dta[dta$group == 1, "ldif"], -1, .05,
                                          length(dta[dta$group == 1, "ldif"]))
        ),
        linetype = 2) +
        geom_hline(yintercept = c(qnorm(.975, mu.psv, sigma.psv),
                                  qnorm(.025, mu.psv, sigma.psv)
        ),
        linetype = 1, colour = "blue") +
        scale_y_continuous(limits = c(-0.01, 0.41)) +
        labs(title = "Passive, log transf.\nBland-Altman plot\nwith lines mean, 95CI, 95PI, and 0")

g2 <- filter(dta, group == 2) %>%
        ggplot(aes(lave, ldif)) +
        geom_point() +
        geom_hline(yintercept = mu.actv) +
        geom_hline(yintercept = 0, colour = "red") +
        geom_hline(yintercept = c(ConfInt(dta[dta$group == 2, "ldif"], 1, .05,
                                          length(dta[dta$group == 2, "ldif"])),
                                  ConfInt(dta[dta$group == 2, "ldif"], -1, .05,
                                          length(dta[dta$group == 2, "ldif"]))
        ),
        linetype = 2) +
        geom_hline(yintercept = c(qnorm(.975, mu.actv, sigma.actv),
                                  qnorm(.025, mu.actv, sigma.actv)
        ),
        linetype = 1, colour = "blue") +
        scale_y_continuous(limits = c(-0.01, 0.41)) +
        labs(title = "Active, log transf.\nBland-Altman plot\nwith lines mean, 95CI, 95PI, and 0")

library(gridExtra)
grid.arrange(g1, g2, ncol = 2)

CheckNorm(dta[dta$group == 1, "ldif"]); CheckNorm(dta[dta$group == 2, "ldif"])

# exercise 3.5 ------------------------------------------------------------
dta <- read.csv("./data/siblings.csv")

# From stata log file: Probably looks good on both scales, so easier the original
# scale. Birth weight of 1000 sib pairs.

dta$wdif <- with(dta, weight2nd - weight1st)
dta$wave <- with(dta, (weight2nd - weight1st) / 2)

filter(dta, sex1st == 1 & sex2nd == 1) %>%
        ggplot(aes(weight1st, weight2nd)) +
        geom_point() +
        geom_smooth(method = "lm", aes(colour = "black")) +
        stat_function(fun = function(x) x, geom = "line", aes(colour = "blue")) +
        scale_colour_manual(name = "legend",
                            values = c("black", "blue"),
                            labels = c("weight", "y = x"))

Explore(dta[dta$sex1st == 1 & dta$sex2nd == 1, "wdif"],
        length(dta[dta$sex1st == 1 & dta$sex2nd == 1, "wdif"]))

mu <- mean(dta[dta$sex1st == 1 & dta$sex2nd == 1, "wdif"])
sigma <- sd(dta[dta$sex1st == 1 & dta$sex2nd == 1, "wdif"])
filter(dta, sex1st == 1 & sex2nd == 1) %>%
        ggplot(aes(wave, wdif)) +
        geom_point() +
        geom_hline(yintercept = mu) +
        geom_hline(yintercept = 0, colour = "red") +
        geom_hline(yintercept = c(ConfInt(dta[dta$sex1st == 1 &
                                                      dta$sex2nd == 1, "wdif"],
                                          1, .05, length(dta[
                                                  dta$sex1st == 1 &
                                                          dta$sex2nd == 1, "wdif"
                                                  ])),
                                  ConfInt(dta[dta$sex1st == 1 &
                                                      dta$sex2nd == 1, "wdif"],
                                          -1, .05, length(dta[
                                                  dta$sex1st == 1 &
                                                          dta$sex2nd == 1, "wdif"
                                                  ]))
                                  ),
                   linetype = 2) +
        geom_hline(yintercept = c(qnorm(.975, mu, sigma),
                                  qnorm(.025, mu, sigma)
                                  ),
                   linetype = 1, colour = "blue") +
        labs(title = "Bland-Altman plot\nwith lines mean, 95CI, 95PI, and 0")

CheckNorm(dta[dta$sex1st == 1 & dta$sex2nd == 1, "wdif"])
dta$wldif <- with(dta, log(weight2nd / weight1st))
dta$wlave <- with(dta, (log(weight2nd) + log(weight1st)) / 2)

Explore(dta[dta$sex1st == 1 & dta$sex2nd == 1, "wldif"],
        length(dta[dta$sex1st == 1 & dta$sex2nd == 1, "wldif"]))

mu <- mean(dta[dta$sex1st == 1 & dta$sex2nd == 1, "wldif"])
sigma <- sd(dta[dta$sex1st == 1 & dta$sex2nd == 1, "wldif"])
filter(dta, sex1st == 1 & sex2nd == 1) %>%
        ggplot(aes(wlave, wldif)) +
        geom_point() +
        geom_hline(yintercept = mu) +
        geom_hline(yintercept = 0, colour = "red") +
        geom_hline(yintercept = c(ConfInt(dta[dta$sex1st == 1 &
                                                      dta$sex2nd == 1, "wldif"],
                                          1, .05, length(dta[
                                                  dta$sex1st == 1 &
                                                          dta$sex2nd == 1, "wldif"
                                                  ])),
                                  ConfInt(dta[dta$sex1st == 1 &
                                                      dta$sex2nd == 1, "wldif"],
                                          -1, .05, length(dta[
                                                  dta$sex1st == 1 &
                                                          dta$sex2nd == 1, "wldif"
                                                  ]))
        ),
        linetype = 2) +
        geom_hline(yintercept = c(qnorm(.975, mu, sigma),
                                  qnorm(.025, mu, sigma)
        ),
        linetype = 1, colour = "blue") +
        labs(title = "Bland-Altman plot\nwith lines mean, 95CI, 95PI, and 0")

CheckNorm(dta[dta$sex1st == 1 & dta$sex2nd == 1, "wldif"])

# exercise 3.6 ------------------------------------------------------------
dta <- read.csv("./data/temp3.csv")
str(dta)

## Q1
dta$dif <- with(dta, hgoral - craft); dta$ave <- with(dta, (hgoral + craft) / 2)

# Y-X scatter plot
plot(dta$craft, dta$hgoral) # base R
# with ggplot2 - and a bit of line work as above:
ggplot(dta, aes(craft, hgoral)) +
        geom_point() +
        geom_smooth(method = "lm", aes(colour = "black")) +
        stat_function(fun = function(x) x, geom = "line", aes(colour = "blue")) +
        scale_colour_manual(name = "legend",
                            values = c("black", "blue"),
                            labels = c("value", "y = x")) +
        labs(x = "Craft oral",
             y = "Hg oral")

Explore(dta$dif, nrow(dta))

# Blamd-Altman plot
mu <- mean(dta$dif); sigma <- sd(dta$dif)

ggplot(dta, aes(ave, dif)) +
        geom_point() +
        geom_hline(yintercept = mu) +
        geom_hline(yintercept = 0, colour = "red") +
        geom_hline(yintercept = c(ConfInt(dta$dif, 1, .05, nrow(dta)),
                                  ConfInt(dta$dif, -1, .05, nrow(dta))
        ),
        linetype = 2) +
        geom_hline(yintercept = c(qnorm(.975, mu, sigma),
                                  qnorm(.025, mu, sigma)
        ),
        linetype = 1, colour = "blue") +
        labs(title = "Bland-Altman plot\nwith lines mean, 95CI, 95PI, and 0")

CheckNorm(dta$dif)

t.test(dta$craft, dta$hgoral, paired = TRUE)

# We fail to reject the hypothesis of no difference in mean.

## Q2
# From the stata log file: The data is consistent with the hypothesis of no
# systematic difference. 95% of the individual differences are between -0.6 and
# 0.6, in general small differences between the two thermometers.

## Q3
dta$difreccraft <- with(dta, hgrectal - craft)
dta$difrecorralt <- with(dta, hgrectal - hgoral)
summary(dta) # reveals two missing in the newly generated variables

# quick and dirty:
par(mfrow = c(1, 2))
plot(dta$difreccraft, dta$dif); plot(dta$difrecorralt, dta$dif)
par(mfrow = c(1, 1)) # back to normal view

# There are numerous ways in R to subset. This is perhaps the most basic:
dta[abs(dta$dif) > 0.8, c("id", "hgoral", "craft", "hgrectal")]


# exercise 4.1 ------------------------------------------------------------
dta <- read.csv("./data/vaccine.csv")
str(dta)
dta$vac <- ifelse(dta$vaccine == 1, "yes", "no"); dta$vac <- factor(dta$vac)
dta$inf <- ifelse(dta$influenza == 1, "yes", "no"); dta$inf <- factor(dta$inf)

summary(dta)

## Q1
# bionom.test takes numbers of succeses and number of trials (or a vector of 2).
# See ?binom.test for details
binom.test(length(dta[dta$vac == "no" & dta$influenza == 1, "influenza"]),
           length(dta[dta$vac == "no", "influenza"]),
           p = .5, alternative = "two.sided", conf.level = .95)
# the last line is the default so no need to include it.

binom.test(length(dta[dta$vac == "yes" & dta$influenza == 1, "influenza"]),
           length(dta[dta$vac == "yes", "influenza"]))

# Q2 + Q3
t <- table(dta$inf, dta$vac)
addmargins(t); prop.table(t)
sum(diag(t) / sum(t))

chisq.test(dta$inf, dta$vac)
# The result differ from the one given with Stata cs function due to the default
# continuity correction in R. This can be set to "correct = FALSE" giving the
# same result as Stata.

# Many different packages have a function to calculate the basic measures of
# association. Here I use epitools but I also like the Epi and epiR packages
# that both gives nicer outputs but have other drawbacks. Alternatively,
# one could easily write a function to calculate the measures, for instance
# based on the formulas given in Kirkwood and Sterne 2. ed. chapter 16.

library(epitools)
epitab(dta$vac, dta$inf,
       method = c("riskratio"),
       verbose = TRUE)
epitab(dta$vac, dta$inf,
       method = c("oddsratio"))

# Notice that this function (and the equvivalent in Epi package) gives another
# structure of the data than e.g. Stata.

# Risk difference
# Strangely this measure is not included in any of the packages. It is easily
# computed though (all of this could be wrapped in a function):
d1 <- length(dta[dta$vaccine == 1 & dta$influenza == 1, "id"])
d0 <- length(dta[dta$vaccine == 0 & dta$influenza == 1, "id"])
h1 <- length(dta[dta$vaccine == 1 & dta$influenza == 0, "id"])
h0 <- length(dta[dta$vaccine == 0 & dta$influenza == 0, "id"])
n1 <- length(dta[dta$vaccine == 1, "id"])
n0 <- length(dta[dta$vaccine == 0, "id"])
p1 <- d1 / n1
p0 <- d0 / n0

rd <- p1 - p0
se.rd <- sqrt((p1 * (1 - p1) / n1) + (p0 * (1 - p0) / n0))
conf.int <- rd + c(-1,1) * 1.96 * se.rd
rd; se.rd; conf.int

## Q4
cs <- chisq.test(dta$vaccine, dta$influenza)
cs
# To get the same information as automatically printed in Stata:
cs$observed
cs$expected
cs$residuals
cs$stdres

# exercise 4.2 ------------------------------------------------------------
dta <- read.csv("./data/strepto.csv")
str(dta)

## Q1
binom.test(length(dta[dta$treatment == "0" & dta$survival == "1", "survival"]),
           length(dta[dta$treatment == "0", "survival"]))

binom.test(length(dta[dta$treatment == "1" & dta$survival == "1", "survival"]),
           length(dta[dta$treatment == "1", "survival"]))

## Q2+Q3
epitab(dta$treatment, dta$survival,
       method = c("riskratio"))
chisq.test(dta$treatment, dta$survival)

# exercise 4.3 ------------------------------------------------------------
dta <- read.csv("./data/siblings.csv")
str(dta)

table(dta$sex1st, dta$sex2nd); prop.table(table(dta$sex1st, dta$sex2nd))

binom.test(length(dta[dta$sex1st == "2" & dta$sex2nd == "1", "sex2nd"]),
           length(dta[dta$sex1st == "2", "sex2nd"]))

binom.test(length(dta[dta$sex1st == "1" & dta$sex2nd == "1", "sex2nd"]),
           length(dta[dta$sex1st == "1", "sex2nd"]))

## Q2
# From the stata log file: We see here the hypothesis of independence

epitab(dta$sex1st, dta$sex2nd,
       method = "riskratio",
       pvalue = "chi2")

prop.test(table(dta$sex1st, dta$sex2nd), correct = FALSE)

## Q3
binom.test(length(dta[dta$sex1st == "1", "sex1st"]),
           length(dta[, "sex1st"]))

binom.test(length(dta[dta$sex2nd == "1", "sex2nd"]),
           length(dta[, "sex2nd"]))

## Q4
# From the stata log: Note that there is no reason now to assume that the gender
# are independent. So we are in the case of paired data

mcnemar.test(dta$sex1st, dta$sex2nd)

# From the stata log file - also applies to R prop.test:

#. *                      Watch out!
#        prtest makes an unpaired comparison of paired data
#. *                      Watch out!

table(dta$sex1st, dta$sex2nd)
prop.test(x = c(234 + 269, 262 + 235), c(1000, 1000), correct = FALSE)


## Q5
# From the stata log file: Central to understand the difference between:
# Same probability and independence between siblings

# exercise 4.4 ------------------------------------------------------------
dta <- read.csv("./data/schistosoma.csv")
str(dta)

# From stata log file: There is a significantly greater change of positive
# response by Bell's methods: Dif 13 (7;18)% Since we don't know the truth, we
# don't know which one is best.

table(dta)

mcnemar.test(dta$KatoKatz, dta$Bell, correct = FALSE)

# exercise 4.5 ------------------------------------------------------------
power.prop.test(p1 = .30, p2 = .20, power = .90)
power.prop.test(p1 = .30, p2 = .20, power = .80)

## Q2
# From the stata log: The answer is probably around 0.185

power.prop.test(p1 = .30, p2 = .20, n = 300)

power.prop.test(p1 = .30, p2 = .15, n = 300)

power.prop.test(p1 = .30, p2 = .17, n = 300)

power.prop.test(p1 = .30, p2 = .19, n = 300)

power.prop.test(p1 = .30, p2 = .185, n = 300)

# exercise 4.6 ------------------------------------------------------------

binom.test(114, 1896)

binom.test(47, 719)

binom.test(965, 10770)

binom.test(1087, 14510)

# Can be done in several ways. Directly calculate X^2:
prop.test(c(114, 47, 965, 1087), c(1896, 719, 10770, 14510))

# If more analysis are to be done this is a more flexible solution:
smokekills <- matrix(data = c(114, 1782, 47, 672, 965, 9805, 1087, 13423),
                     nrow = 2, ncol = 4,
                     dimnames = list(c("dead", "alive")))
t <- as.table(smokekills)
margin.table(t)
margin.table(t, c(1))
prop.table(t)
summary(t)

## Q2
# From the stata log file: There is an association but the test does not say what
# kind of association. It looks like cigarettes is the important risk factor.

# exercise 4.7 ------------------------------------------------------------
dta <- read.csv("./data/siblings.csv")

dta$gender1 <- ifelse(!dta$sex1st %in% NA, 2 - dta$sex1st, NA)
dta$gender1 <- ifelse(dta$gender1 == 0, "girl", "boy")
dta$lbw1 <- with(dta, ifelse(!weight1st %in% NA,
                             ifelse(weight1st < 2500, 1, 0),
                             NA))

epitab(dta$gender1, dta$lbw1, rev = "rows", method = c("riskratio"))
table(dta$lbw1, dta$gender1)
prop.test(c(16, 18), c(487 + 16, 479 + 18))


# exercise 5.1 ------------------------------------------------------------
dta <- read.csv("./data/pefr.csv")
str(dta)
dta$sex <- factor(ifelse(dta$sex == "1", "female", "male"))
summary(dta)

## Q1
library(tidyr)
library(dplyr)
library(ggplot2)

ggplot(dta, aes(height, PEFR)) +
        geom_point(aes(colour = sex)) +
        geom_smooth(aes(group = sex, colour = sex), method = "lm")

# men only!
dtamen <- filter(dta, sex == "male")

ggplot(dtamen, aes(height, PEFR)) +
        geom_point() +
        geom_smooth(method = "lm")

## Q2
m1 <- glm(PEFR ~ height, data = dtamen, family = "gaussian")
SumFit(m1)

## Q3
dtamen$height170 <- dtamen$height - 170
m2 <- glm(PEFR ~ height170, data = dtamen, family = "gaussian")
SumFit(m2)

## Q4 + Q5
plot(m2)

## Q6
# Returning to all sexes
dta$height170 <- dta$height - 170

m3 <- glm(PEFR ~ sex + height170 + sex * height170, data = dta,
          family = "gaussian")
SumFit(m3)

dta$sex2 <- relevel(dta$sex, ref = 2)
m4 <- glm(PEFR ~ sex2 + height170 + sex2 * height170, data = dta,
          family = "gaussian")
SumFit(m4)

## Q7
dta$sex3 <- as.integer(dta$sex)
m5 <- glm(PEFR ~ sex + height170, data = dta, family = "gaussian")
summary(m5)

# From the stata log file: _cons ("(Intercept)" in R) is the intersept for the
# females. 2.sex is difference in intercepte for males as compared to females.
# Thus _cons + 2.sex is the intercept for the males, i.e. the mean PEFR for males
# at hight 170 cm.

d <- data.frame(height170 = 0, sex = "male")
pred <- predict(m5, newdata = d, type = "link", se.fit = TRUE)
pred

ggplot(dta, aes(height170, PEFR)) +
        geom_point(aes(colour = sex)) +
        stat_function(fun = function(x) (488.4078 + 50.0129) + x, geom = "line",
                colour = "blue") +
        stat_function(fun = function(x) coef(m5)[1] + x, geom = "line",
                colour = "red") +
        scale_colour_manual(name = "legend",
                            values = c("red", "blue", "red", "blue"))

# For comparison: This gives the automatically generated regression lines
# (ggplot2 calls the base R glm function)
x <- dta$height170
y <- dta$PEFR
ggplot(dta, aes(x = height170, y = PEFR)) +
        geom_point(aes(colour = sex)) +
        geom_smooth(method = "glm", formula = y ~ x, aes(colour = sex))

# This can be simplified to
ggplot(dta, aes(height170, PEFR)) +
        geom_point(aes(colour = sex)) +
        geom_smooth(method = "glm", aes(colour = sex))

# exercise 5.2 ------------------------------------------------------------
dta <- read.csv("./data/fdr.csv")
str(dta)
summary(dta)

## Q1
ggplot(dta, aes(vo2, rd)) +
        geom_point() +
        geom_smooth(method = "glm")

## Q2
dta$vo2_30 <- dta$vo2 - 30

m1 <- glm(rd ~ vo2_30, data = dta, family = "gaussian")
SumFit(m1)

m2 <- glm(rd ~ vo2, data = dta, family = "gaussian")
SumFit(m2)


# From the stata log file: Estimating the mean rd for a relative with vo2=30
# based on the last regression analysis.

d <- data.frame(vo2 = 30)
pred <- predict(m2, newdata = d, type = "link", se.fit = TRUE)
pred

# To get the 95%CI for the FITTED value
matrix(data = c(pred$fit,
                (pred$fit - (qnorm(.975) * pred$se.fit)),
                (pred$fit + (qnorm(.975) * pred$se.fit))
                ), ncol = 4,
       dimnames = list("fitted", c("estimate", "CIlow", "CIhigh"))
       )

# The inverse link function could be applied to the fitted values
fit2 <- m2$family$linkinv(fit)
CIhigh2 <- m2$family$linkinv(CIhigh)
CIlow2 <- m2$family$linkinv(CIlow)

## Q3
par(mfrow = c(2, 2))
plot(m2)

## Q4
cor.test(dta$rd, dta$vo2)
# Pearson prod.-moment correlation coefficient is neither distributionally
# robust or outlier resistant.

cor.test(dta$rd, dta$vo2, method = "spearman")

## Q5
dta$lrd <- log(dta$rd)
dta$lvo2 <- log(dta$vo2)

ggplot(dta, aes(lvo2, lrd)) +
        geom_point() +
        geom_smooth(method = "lm")

dta$lvo2_30 <- log(dta$vo2 / 30)

m3 <- glm(lrd ~ lvo2_30, data = dta, family = "gaussian")
SumFit(m3)

m4 <- glm(lrd ~ lvo2, data = dta, family = "gaussian")
SumFit(m4)

d <- data.frame(lvo2 = 3.4011974)
pred <- predict(m4, newdata = d, type = "link", se.fit = TRUE)

# To get the 95%CI for fit
matrix(data = c(pred$fit,
                pred$se.fit,
                (pred$fit - (qnorm(.975) * pred$se.fit)),
                (pred$fit + (qnorm(.975) * pred$se.fit))
                ), ncol = 4,
       dimnames = list("fitted", c("estimate", "se", "CIlow", "CIhigh"))
       )

plot(m4)

# From the stata log: The model here is mean log(Rd)=beta0+beta1*log(VO2) or
# median Rd=exp(beta0)*VO2^beta1 (^=raised to the power of)
#
# So provided the model is acceptable, exp(beta0) is the median Rd if VO2=1,
# which likely does not have a biological interpreation.
# comparing an person with doubled the VO2 than another person then the median
# ratio of Rd between the two individs is 2^beta1.
# The estimate of beta1 and CI should be transformed. Both regression model of
# rd on vo2 and log(rd) on log(vo2) seem appropriate form a statistical
# prespective. The choise should then depend on which seem most reasonable from
# a biological perspective.

## Q6
# model with rd on vo2: m2
summary(m2)

d <- data.frame(vo2 = 30)
pred <- predict(m2, newdata = d, type = "link", se.fit = TRUE)

# To get the 95%CI for fit
matrix(data = c(pred$fit,
                pred$se.fit,
                (pred$fit - (qnorm(.975) * pred$se.fit)),
                (pred$fit + (qnorm(.975) * pred$se.fit))
                ), ncol = 4,
       dimnames = list("fitted", c("estimate", "se", "CIlow", "CIhigh"))
       )

# exercise 5.3 ------------------------------------------------------------
dta <- read.csv("./data/tewl.csv")
str(dta)
dta$grp <- factor(ifelse(dta$group == 0, "reference", "fish processing"))
summary(dta)
Explore(dta$skintemp, nrow(dta))

## Q1
CheckNorm(dta[dta$group == 0, "log10tewl"])
CheckNorm(dta[dta$group == 1, "log10tewl"])

var.test(dta[dta$grp == "reference", "log10tewl"],
         dta[dta$grp == "fish processing", "log10tewl"])

tt <- t.test(dta[dta$grp == "reference", "log10tewl"],
       dta[dta$grp == "fish processing", "log10tewl"],
       var.equal = TRUE)
tt

# From the stata log file: We can backtranform the results to the original scale.
# (Note we compare fish proccessing compared to reference persones).
10^(tt$estimate[2] - tt$estimate[1])
10^(-tt$conf.int[2]); 10^(-tt$conf.int[1])

# The reduction in medians for fish proccessing compared to reference persones
1 - 10^(tt$estimate[2] - tt$estimate[1])
1 - 10^(-tt$conf.int[1])
1 - 10^(-tt$conf.int[2])

# From the stata log: The assumptions underlying the t-test seem to be satified.
# Just for comparison we supplemnt with a non-parametric test.

wilcox.test(dta[dta$grp == "reference", "log10tewl"],
            dta[dta$grp == "fish processing", "log10tewl"],
            conf.int = TRUE,
            correct = FALSE)

## Q2
CheckNorm(dta[dta$group == 0, "skintemp"])
CheckNorm(dta[dta$group == 1, "skintemp"])

var.test(dta[dta$grp == "reference", "skintemp"],
         dta[dta$grp == "fish processing", "skintemp"])

t.test(dta[dta$grp == "reference", "skintemp"],
       dta[dta$grp == "fish processing", "skintemp"],
       var.equal = TRUE)

wilcox.test(dta[dta$grp == "reference", "skintemp"],
            dta[dta$grp == "fish processing", "skintemp"],
            conf.int = TRUE,
            correct = FALSE)

## Q3
dta$temp25 <- dta$skintemp - 25

m1 <- lm(log10tewl ~ temp25, data = dta[dta$grp == "fish processing", ])
SumFit(m1)

library(dplyr); library(ggplot2)
filter(dta, group == 1) %>%
  ggplot(aes(log10tewl, skintemp)) +
  geom_point() +
  geom_smooth(method = "lm")

# The residuals and the fitted values (obtained by transfoming the linear
# predictors by the inverse of the link function) of the model:
resid(m1)
fitted(m1)

# Create a new df with the variables of interest
temp <- data.frame(res = resid(m1),
                   s.temp = dta[dta$grp == "fish processing", "skintemp"])
# Now plot with ggplot2
ggplot(temp, aes(res, s.temp)) +
  geom_point()

# Or without creating a new df: in base R
plot(resid(m1), dta[dta$grp == "fish processing", "skintemp"])

CheckNorm(resid(m1))

# the same for the reference group (i.e. "group 0")
m2 <- lm(log10tewl ~ temp25, data = dta, grp == "reference")
SumFit(m2)

filter(dta, group == 0) %>%
  ggplot(aes(log10tewl, skintemp)) +
  geom_point() +
  geom_smooth(method = "lm")

plot(resid(m2), dta[dta$group == 0, "skintemp"])

CheckNorm(resid(m2))

mean(dta[dta$grp == "reference", "log10tewl"])

## Q4
ggplot(dta, aes(skintemp, log10tewl)) +
  geom_point(aes(colour = grp)) +
  geom_smooth(aes(colour = grp), method = "lm")

# From the stata log file: A linear regression with different slopes in the two
# groups.

dta$grp2 <- relevel(dta$grp, ref = "reference")
m3 <- lm(log10tewl ~ grp2 + temp25 + grp2 * temp25, data = dta)
SumFit(m3)

## Q5
m4 <- lm(log10tewl ~ grp2 + temp25, data = dta)
SumFit(m4)

# Backtransformation
10^(m4$coef[2])
10^(confint(m4)[2]); 10^(confint(m4)[5])

# The reduction in medians for fish proccessing compared to reference persones
1 - 10^(m4$coef[2])
1 - 10^(confint(m4)[2])
10^(confint(m4)[5])

# From the stata log file: The two means are shown in the graphs. The difference
# (of the horisontal lines) is the unadjusted mean difference. The vertical
# difference between the two (regression) lines is the adjusted mean difference.

ggplot(dta, aes(skintemp, log10tewl)) +
  geom_point(aes(colour = grp)) +
  geom_smooth(aes(colour = grp), method = "glm") +
  geom_hline(yintercept = mean(dta[dta$grp == "fish processing", "log10tewl"]),
             colour = "red") +
  geom_hline(yintercept = mean(dta[dta$grp == "reference", "log10tewl"]),
             colour = "blue") +
  scale_colour_manual(name = "legend",
                      values = c(rep(c("red", "blue"), 2)))

## Q6

# From the stata log file: We can accept the hypothesis that an increase in skin
# temperature of 5C corresponds to a increase in log10(TEWL) of 0.175.

m4$coef[3] * 5
confint(m4)[3] * 5; confint(m4)[6] * 5

## Q7
# From the stata log file: The log10(TEWL) is 0.24 (CI: 0.15-0.33) higher on the
# log-scale for reference persons as compared to fish processing persons. On the
# original scale, this correspond to a 42% (29%-53%) lower median TEWL for Fish
# processing as compared to reference persons, which is statistical significant
# (p<0.001). The difference in log10(TEWL) were adjusted for skin temperature
# in an analysis for covariance (multiple linear regression model). The model fit
# were accessed using residual plots. The adjusted difference showed a slightly
# lower median for Fish processing persons (4% lower median, CI: 24% lower median
# to 22% higher median) as compared to reference person, which is not statistical
# significant (p=0.75).



# exercise 6.1 ------------------------------------------------------------
dta <- read.csv("./data/haemoglob.csv")
str(dta)

dta$tp <- factor(ifelse(dta$type == 1, "HbSS", ifelse(dta$type == 2, "HbSB", "HbSC")))


## Q1
Explore(dta$haemo, length(dta$haemo))

## Q2
CheckNorm(dta[dta$type == 1, "haemo"])
CheckNorm(dta[dta$type == 2, "haemo"])
CheckNorm(dta[dta$type == 3, "haemo"])

## Q3
m1 <- lm(haemo ~ tp, data = dta)
SumFit(m1)
anova(m1)

bartlett.test(haemo ~ tp, data = dta)

# From the stata log: Bartlett's test accept the hypothesis of equal variantion
# in the three groups. One-way-anova reject the hypothesis of equal means.

## Q4
dta$tp2 <- relevel(dta$tp, ref = "HbSS")
m2 <- lm(haemo ~ tp2, data = dta)
SumFit(m2)

# Again several ways to do it. One approach is to alter the contrasts. It is
# important to note that the default reference level for character variables
# is determined by alphabetical order (thus when creating a factor 1 = "lowest
# alphebetically"):
m3 <- lm(haemo ~ C(tp, contr.treatment(3, base = 2)), data = dta)
# Look at the first explanatory variable
SumFit(m3)

# exercise 6.2 ------------------------------------------------------------
dta <- read.csv("./data/folate.csv")
str(dta)
summary(dta)
dta$grp <- factor(ifelse(dta$group == 1, "NOandO2",
                         ifelse(dta$group == 2, "NO", "nihil")))

Explore(dta$folate, length(dta$folate))

ggplot(dta, aes(grp, folate)) +
  geom_point(aes(colour = grp))

CheckNorm(dta[dta$grp == "NOandO2", "folate"])
CheckNorm(dta[dta$grp == "NO", "folate"])
CheckNorm(dta[dta$grp == "nihil", "folate"])

# From the stata log file: Evalueating the overall hypothesis of the same means
# in the three group by both the one-way-anova and linear regression. Note the
# output of the regression analysis is more detailed, but only the one-way-anova
# includes the Bartlett test for equal variation (this is not the case in R)

m1 <- lm(folate ~ grp, data = dta)
anova(m1)
bartlett.test(folate ~ grp, data = dta)
SumFit(m1)

m2 <- lm(folate ~ 0 + grp, data = dta)
SumFit(m2)
plot(m2)

# exercise 6.3 ------------------------------------------------------------

dta <- read.csv("./data/cancer.csv")
str(dta)
summary(dta)

# Since the type variable is of 5 levels I don't want to recode. I merely convert
# to factor and leve it for specific need to name the levels.
dta$type <- factor(dta$type)

## Q1
# Plots - does not look normal to me
plot(dta$surv)
boxplot(dta$surv)

# function because I don't want exhaust my fingers. BEWARE: this will print five
# plots without warning
for(i in 1:5) {
  print(CheckNorm(dta[dta$type == i, "surv"]))
}
# Still doesn't look normal!
# From the stata log file: We need to work in the logscale, where both the scatter
# plot and QQ plots show that the normal model is a good approximation to the
# log-survival times.

dta$logsurv <- log(dta$surv)
Explore(dta$surv, length(dta$surv))
# Again: automatically prints five plots
for(i in 1:5) {
  print(CheckNorm(dta[dta$type == i, "logsurv"]))
}
# Much nicer (if normal is nice)

m1 <- lm(logsurv ~ type, data = dta)
anova(m1)
bartlett.test(logsurv ~ type, data = dta)
SumFit(m1)

# To compare geometric means
m2 <- lm(logsurv ~ 0 + type, data = dta)
SumFit(m2)

# This function will immediately print three regressions with base set to 3, 4,
# and 5 respectively. From this we get all the possible combinations
for(i in 3:5) {
  m <- lm(logsurv ~ C(type, contr.treatment(5, base = i)), data = dta)
  print(SumFit(m))
}

plot(m1)

## Q2
SumFit(m2)

# Geometric mean and 95%CI
# Breast cancer:
exp(m2$coef[5]); exp(confint(m2)[5]); exp(confint(m2)[10])
# Stomach:
exp(m2$coef[1]); exp(confint(m2)[1]); exp(confint(m2)[6])

## Q3
# Breast cancer:
exp(m1$coef[5]); exp(confint(m1)[5]); exp(confint(m1)[10])

## Q4
# From the stata log file: The analysis in question 1 show that the normal model
# with same standard deviation fits the data nicely for the log-survival times.
# Thus the only difference between the survival times between to cancer
# forms is a shift in the mean. We can formulate this by
#
#   log-survival in group 2 = meandiff + log-survival in group 1     (1)
#
# where meandiff is a constant describing the difference in means on the log-scale.
# Taking the exponential on both side in (1), the equationbecomes
#
#   survival in group 2 = exp(meandiff) * survival in group 1        (2)
#
# The distribution in survival times in two cancer group are identical, beside
# a constant (const=exp(meandiff)) that is multiplied. This is the accelerated
# waiting time model


# exercise 7.1 ------------------------------------------------------------
dta <- read.csv("./data/postterm.csv")
str(dta)
summary(dta)

dta$ptd2 <- factor(ifelse(dta$ptd == 0, "not p.term", "p.term"))
dta$prty <- factor(ifelse(dta$parity == 0, "no prev.deliver", "1 or more"))

## Q1
table(dta$prty, dta$ptd2)

# Exposed
1677/4696
# Not exposed:
1722/4216

## Q2
library(epitools)
epitab(dta$ptd2, dta$prty,
       method = c("oddsratio"),
       rev = "columns",
       verbose = TRUE)

## Q3
dta$prty2 <- relevel(dta$prty, ref = "no prev.deliver")
m1 <- glm(ptd2 ~ prty2, data = dta, family = "binomial")
SumFit(m1)

## Q4
# From the stata log: The hypothesis H: OR=0.9 can be written in equivalent form
# H: log(OR)=log(0.9) or H: log(OR)-log(0.9)=0. The last hypothesis can be
# evaluated in the lincom command.

# A nice way to diaplay the exponentiated data
exp(cbind(OR = coef(m1), confint(m1)))

## Q5
dta$age30 <- dta$age - 30
dta2 <- filter(dta, parity == 0)

m2 <- glm(ptd2 ~ age30, data = dta2, family = "binomial")
SumFit(m2)
exp(cbind(OR = coef(m2), confint(m2)))

## Q6
cbind(OR = 1.0209 ^ 10, CIlow = 1.0069^10, CIhigh = 1.0351^10)

exp(cbind(OR = coef(m2), confint(m2)))[2, ] ^10

## Q7
m3 <- glm(ptd2 ~ prty2 + age30 + prty2 * age30, data = dta, family = "binomial")
SumFit(m3)

m3 <- glm(ptd2 ~ prty + age30 + prty * age30, data = dta, family = "binomial")
SumFit(m3)

## Q8
m4 <- glm(ptd2 ~ prty2 + age30, data = dta, family = "binomial")
SumFit(m4)
exp(cbind(OR = coef(m4), confint(m4)))

# exercise 7.2 ------------------------------------------------------------
dta <- read.csv("./data/tatsoib.csv")
str(dta)
summary(dta)
dta$grp <- factor(ifelse(dta$group == 0, "placebo", "drug"))
names(dta)[3] <- "ch.tats"

## Q1
Explore(dta$age, length(dta$age))
CheckNorm(dta[dta$group == 0, "age"]); CheckNorm(dta[dta$group == 1, "age"])

var.test(dta[dta$group == 0, "age"], dta[dta$group == 1, "age"])
t.test(dta[dta$group == 0, "age"], dta[dta$group == 1, "age"], var.equal = TRUE)


## Q2 + Q3
CheckNorm(dta[dta$group == 0, "ch.tats"]); CheckNorm(dta[dta$group == 1, "ch.tats"])
var.test(dta[dta$group == 0, "ch.tats"], dta[dta$group == 1, "ch.tats"])
t.test(dta[dta$group == 0, "ch.tats"], dta[dta$group == 1, "ch.tats"], var.equal = TRUE)

# From the stata log: A two-sample t-test is identical to a linear regression
# analysis using a categorical independent variable with two levels, here group.
# Note, that the common standard deviation can be read off the output of the
# linear regression.

dta$grp2 <- relevel(dta$grp, ref = "placebo")
m1 <- lm(ch.tats ~ grp2, data = dta)
SumFit(m1)

## Q4
dta$ch.pos <- factor(ifelse(dta$ch.tats > 0, 1, 0))

# *** NOTICE *** NOTICE *** NOTICE ***
# I think the following is incorrect...but I cannot figure out what I do wrong
summary(glm(ch.pos ~ 1, data = dta, subset = group == 0,
            family = "binomial"))
summary(glm(ch.pos ~ 1, data = dta, subset = group == 1,
            family = "binomial"))
# *** NOTICE *** NOTICE *** NOTICE ***

epitab(dta$grp, dta$ch.pos,
       method = c("oddsratio"),
       rev = "columns",
       verbose = TRUE)

## Q5
#-
## Q6
dta$age45 <- dta$age - 45

ggplot(dta, aes(ch.tats, age)) +
        geom_point(aes(colour = grp)) +
        geom_smooth(aes(colour = grp), method = "lm")

m2 <- lm(ch.tats ~ age45, data = dta, group == 0)
SumFit(m2)
plot(m2)

## Q7
m3 <- lm(ch.tats ~ age45, data = dta, group == 1)
SumFit(m3)
plot(m3)

## Q8
m4 <- lm(ch.tats ~ age45 * grp2, data = dta)
SumFit(m4)

## Q9
m5 <- lm(ch.tats ~ age45 + grp2, dta)
SumFit(m5)
plot(m5)

rbind(coef(summary(m1)), coef(summary(m5)))
rbind(confint(m1), confint(m5))

anova(m1, m5)

## 10
# -
## 11
m6 <- glm(ch.pos ~ grp2, data = dta, family =  "binomial")
SumFit(m6)
exp(cbind(OR = coef(m6), confint(m6)))

m7 <- glm(ch.pos ~ grp2 * age45, data = dta, family =  "binomial")
SumFit(m7)
exp(cbind(OR = coef(m7), confint(m7)))

m8 <- glm(ch.pos ~ grp2 + age45, data = dta, family =  "binomial")
exp(cbind(OR = coef(m8), confint(m8)))

rbind(coef(summary(m6)), coef(summary(m8)))
rbind(confint(m6), confint(m8))


# exercise 8.1 ------------------------------------------------------------
dta <- read.csv("./data/meldates.csv")
str(dta)
summary(dta)

dta$sex.fc <- factor(ifelse(dta$sex == 0, "female", "male"))

# Convert the start- and end date variables to date format in R
dta$start.dte <- as.Date(dta$startdate, format = "%d-%b-%Y")
dta$end.dte <- as.Date(dta$enddate, format = "%d-%b-%Y")

# This is pretty easy in base R. For more complicated time manupulations the
# package lubridate is highly recomended. With lubridate the above would be
# R> library(lubridate) R> dmy(dta$startdate)

# We also create a "time" variable indicting the time of each event. This is
# to be used in the further analysis. I divide by 365.25 to get years.
dta$time <- as.numeric(dta$end.dte - dta$start.dte) / 365.25 

str(dta)
summary(dta)

## Q1
library(survival)
library(survMisc)
dta.surv <- Surv(dta$time, dta$status)
head(summary(surv.dta))

m1 <- survfit(dta.surv ~ 1, type = "kaplan")
m1

# I find both of these plots has advantages and disadvantages
plot(m1, mark.time = TRUE, xlab = "Time", ylab = "Survival Prob.")
autoplot(m1, xlab = "Time", ylab = "Survival Prob.")

# To get the cummulative hazard function (f(y) = -log(y))
plot(m1, fun = "cumhaz", xlab = "Time", ylab = "Survival Prob.", ylim = c(0, .60))
abline(h = c(.25, .50), col = "grey")

summary(m1, times = c(0, 7))
quantile(m1, probs = .25)

## Q2
m2 <- survfit(surv.dta ~ sex.fc, data = dta, type = "kaplan")
m2
plot(m2, mark.time = TRUE, lty = c(1, 4), conf.int = TRUE)
legend(10, 1.0, c("female", "male"), lty = c(1, 4))

quantile(m2, probs = .25)

survdiff(dta.surv ~ sex.fc, data = dta)

## Q3 + Q4
# complementary log-log survival plot (f(y) = log(-log(y)). This also log 
# transforms the x axis
plot(m2, fun = "cloglog", col = c("red", "blue"), xlim = c(0.4, 20))
legend(10, -3.5, c("female", "male"), lty = c(1, 1), lwd=c(2, 2), col = c("red", "blue"))

# To get only the log-log transformation on the t-axis
Clly <- function(p) {return(log(-log(p)))}
plot(m2, fun = Clly, col = c("red", "blue"))

# The Cox proportional hazard function
m3 <- coxph(dta.surv ~ sex.fc, data = dta)
m3
summary(m3)

# To plot the cox prop. haz. model we specify a new model based on the above
m4 <- survfit(m3, newdata = data.frame(sex.fc = c("female", "male")))
plot(m4, lty = c(1, 4), mark = 20)
legend(11, 0.1, c("female", "male"), lty = c(1, 4))

# exercise 8.2 ------------------------------------------------------------
dta <- read.csv("./data/azat.csv")
# Explore the raw data
str(dta)
summary(dta)

## Q1
# Explore the basic time to event data
dta.surv <- Surv(dta$time, dta$death)
summary(dta.surv)
str(dta.surv)
survfit(dta.surv ~ 1, type = "kaplan")

# Answer the question!
m1 <- survfit(dta.surv ~ cenc0, data = dta, type = "kaplan")
m1
autoplot(m1, xlab = "Time", ylab = "Survival Prob.")

## Q2
survdiff(dta.surv ~ cenc0, data = dta)

## Q3
m2 <- coxph(dta.surv ~ cenc0, data = dta)
m2
summary(m2)

## Q4
plot(m1, fun = "cloglog", col = c("red", "blue"), xaxs = "S")
# Or only cloglog of y transformation (function from above)
plot(m1, fun = Clly, col = c("red", "blue"))

plot(survfit(m2, newdata = data.frame(cenc0 = c(0, 1))))

## Q5
dta.cc0 <- dta[dta$cenc0 == 0, ]
dta.surv2 <- Surv(dta.cc0$time, dta.cc0$death)

m3 <- survfit(dta.surv2 ~ treat, data = dta.cc0, type = "kaplan")
plot(m3, main = "Cenc neg.")

m4 <- coxph(dta.surv2 ~ treat, data = dta.cc0)
summary(m4)
plot(m3, fun = "cloglog", xaxs = "S")

# exercise 8.3 ------------------------------------------------------------
dta <- read.csv("./data/toxin.csv")
str(dta)
summary(dta)
dta$grp <- factor(dta$group)

## Q1 + Q2
ggplot(dta, aes(highdose, lowdose)) +
  geom_point(aes(colour = grp))

dta$dif <- with(dta, highdose - lowdose)
dta$avg <- with(dta, (highdose + lowdose) / 2)

ggplot(dta, aes(dif, avg)) +
  geom_point(aes(colour = grp))

CheckNorm(dta[dta$grp == 2, "dif"]); CheckNorm(dta[dta$grp == 1, "dif"])

var.test(dta[dta$grp == 2, "dif"], dta[dta$grp == 1, "dif"])
t.test(dta[dta$grp == 2, "dif"], dta[dta$grp == 1, "dif"], var.equal = TRUE)
wilcox.test(dta[dta$grp == 2, "dif"], dta[dta$grp == 1, "dif"], correct = FALSE)

## Q3
# From the stata log: If belive that there is no effect of progsterone on the 
# difference then we can look at the averages.

CheckNorm(dta[dta$grp == 2, "avg"]); CheckNorm(dta[dta$grp == 1, "avg"])

var.test(dta[dta$grp == 2, "avg"], dta[dta$grp == 1, "avg"])

# -*- NOTICE -*- NOTICE -*- NOTICE
# This is not valid - the variance is not equal in the two groups
t.test(dta[dta$grp == 2, "avg"], dta[dta$grp == 1, "avg"], var.equal = TRUE)
# -*- NOTICE -*- NOTICE -*- NOTICE

t.test(dta[dta$grp == 2, "avg"], dta[dta$grp == 1, "avg"])
wilcox.test(dta[dta$grp == 2, "avg"], dta[dta$grp == 1, "avg"], correct = FALSE)

# lowdose (or highdose instead)or highdose) in the two groups instead
t.test(dta[dta$grp == 2, "lowdose"], dta[dta$grp == 1, "lowdose"], var.equal = TRUE)
t.test(dta[dta$grp == 2, "highdose"], dta[dta$grp == 1, "highdose"], var.equal = TRUE)


