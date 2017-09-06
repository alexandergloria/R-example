# Harald Baayen - Analyzing Linguistic Data
## 1.2 Getting data into and out of R
library(languageR)
head(verbs, n = 10)
help(verbs)
example(verbs)

## 1.3 Accessing information in data frames
rs <- c(638, 799, 390, 569, 567) #shorthand for rows
verbs[rs, ]
verbs[verbs$AnimacyOfTheme == "animate", ]
subset(verbs, AnimacyOfTheme == "animate")
verbs.rs = verbs[rs, ]

## 1.4 Operations on data frames
## 1.4.1 Sorting a data frame by one or more columns
verbs.rs[order(verbs.rs$RealizationOfRec), ]
verbs.rs[order(verbs.rs$Verb, verbs.rs$LengthOfTheme), ]


### 2 Graphical data exploration
## 2.1 Random variables
## 2.2 Visualizing single random variables

par(mfrow = c(3, 2)) # plots arranged in 3 rows and 2 columns
colnames(ratings)
barplot(xtabs( ~ ratings$Length), xlab = "word length", col = "grey")
mean(ratings$Length)
median(ratings$Length)
range(ratings$Length)

library(MASS)
truehist(ratings$Length, xlab="word length", col="grey")
truehist(ratings$Frequency, xlab = "log word frequency", col = "grey")
truehist(ratings$SynsetCount, xlab = "log synset count", col = "grey")
truehist(ratings$SynsetCount,  xlab = "log family size", col = "grey")
truehist(ratings$DerivEntropy, xlab = "derivational entropy", col = "grey")
par(mfrow = c(1, 1))

truehist(lexdec$RT, col = "lightgrey", xlab = "log RT")
h <- hist(lexdec$RT, 
          freq = FALSE, 
          plot = FALSE)
d <- density(lexdec$RT)
xlimit = range(h$breaks, d$x)
ylimit = range(0, h$density, d$y)
hist(lexdec$RT, 
     freq=FALSE, 
     xlim=xlimit, 
     ylim=ylimit, 
     main="",
     xlab="log RT", 
     ylab="", 
     col="lightgrey", 
     border="darkgrey",
     breaks = seq(5.8, 7.6, by = 0.1))
lines(d$x, d$y)
lines(d)
plot(h)
plot(d)
plot(sort(lexdec$RT), ylab = "log RT")
plot(quantile(lexdec$RT), 
     xaxt = "n",
     xlab = "Quartiles", 
     ylab = "log RT")
mtext(c("0%", "25%", "50%", "75%", "100%"),
      side = 1, 
      at = 1:5, 
      line = 1, 
      cex = 0.7)
plot(quantile(lexdec$RT, seq(0, 1, 0.1)),
       + xaxt = "n", xlab = "Deciles", ylab = "log RT")
mtext(paste(seq(0, 100, 10), rep("%", 11), sep = ""),
        + side = 1, at = 1:11, line = 1, cex = 0.7, las = 2)
quantile(lexdec$RT, seq(0, 1, 0.1))
paste(seq(0, 100, 10), rep("%", 11), sep = "")
boxplot(exp(lexdec$RT)) # upper panel
boxplot(lexdec$RT) # lower panel

## 2.3 Visualizing two or more variables

verbs.xtabs <- xtabs( ~ AnimacyOfRec + RealizationOfRec, data = verbs[verbs$AnimacyOfTheme != "animate", ])
verbs.xtabs
par(mfrow = c(1, 2))
barplot(verbs.xtabs, legend.text=c("anim", "inanim"))
barplot(verbs.xtabs, beside = T, legend.text = rownames(verbs.xtabs))
par(mfrow = c(1, 1))


verbs.xtabs <- xtabs( ~ AnimacyOfRec + AccessOfRec + RealizationOfRecipient,
                      data = dative)
verbs.xtabs

mosaicplot(verbs.xtabs, main = "dative")

plot(ratings$Frequency, ratings$FamilySize)
#scatterplot with words
plot(ratings$Frequency, ratings$FamilySize, 
     type = "n", xlab = "Frequency", ylab = "Family Size")
text(ratings$Frequency, ratings$FamilySize,
     as.character(ratings$Word), cex = 0.7)

lines(lowess(ratings$Frequency, ratings$FamilySize), col="darkgrey")
pairs(ratings[ , -c(1, 6:8, 10:14)])

## 2.4 Trellis graphics
# A trellis is a wooden grid for growing roses and other flowers that
# need vertical support. Trellis graphics are graphs in which data are 
# visualized by many systematically organized graphs simultaneously.

library(lattice)
# grouped boxplot
bwplot(RT ~ Correct | NativeLanguage, data = lexdec)
weightRatings[1:5, ]
xylowess.fnc(Rating ~ Frequency | Subject, data = weightRatings, xlab = "log Frequency", ylab = "Weight Rating")
xyplot(Rating ~  Frequency | Subject, data = weightRatings, xlab = "log Frequency", ylab = "Weight Rating")
english <- english[english$AgeSubject == "young", ]
nrow(english)
xylowess.fnc(FamilySize ~ NumberComplexSynsets | equal.count(WrittenFrequency), data = english)

#Excercises


### 3 Probability distributions
## 3.1 Distributions
## 3.2 Discrete distributions
dbinom(59000, 1000000, 0.05885575)
dbinom(1, size = 1000000, prob = 0.0000082)
dbinom(0, size = 1000000, prob = 0.0000082) + dbinom(1, size = 1000000, prob = 0.0000082)
sum(dbinom(0:1, size = 1000000, prob = 0.0000082))
pbinom(1, size = 1000000, prob = 0.0000082)
1 - pbinom(381, size = 1000000, prob = 0.00013288)
n <- 1000
p <- 0.05885575
frequencies <- seq(25, 95, by = 1) # 25, 26, 27, ..., 94, 95
probabilities <- dbinom(frequencies, n, p)
plot(frequencies, probabilities, type = "h", xlab = "frequency", ylab = "probability of frequency")
s <- 500 # the number of random numbers
n <- 1000000 # number of trials in one experiment
p <- 0.0000082 # probability of success
x <- xtabs( ~ rbinom(s, n, p) ) / s
x
plot(as.numeric(names(x)), 
     x, 
     type = "h", 
     xlim = c(0, 30),
     xlab = "frequency", 
     ylab = "sample probability of frequency")

pbinom(4, size = 10, prob = 0.5) # from count to cumulative probability
qbinom(0.3769531, size = 10, prob = 0.5) # from cumulative probability to count
havelaar$Frequency
n <- 1000
p <- mean(havelaar$Frequency / n)
qnts <- seq(0.005, 0.995, by=0.01)
plot(qbinom(qnts, n, p), 
     quantile(havelaar$Frequency,qnts),
     xlab = paste("quantiles of (", n, ",", round(p, 4), ")-binomial", sep=""), 
     ylab = "frequencies")
havelaar.tab <- xtabs( ~ havelaar$Frequency)
havelaar.tab
havelaar.probs = xtabs( ~ havelaar$Frequency)/nrow(havelaar)
round(havelaar.probs, 3)
sum(havelaar.probs)
plot(as.numeric(names(havelaar.probs)), havelaar.probs,
     xlim=c(0, 40), 
     type="h", 
     xlab="counts", 
     ylab="relative frequency")
mtext("observed", 3, 1)
n = 1000
p = mean(havelaar$Frequency / n)
p

counts = 0:40
plot(counts, 
     dbinom(counts, n, p),
     type = "h", 
     xlab = "counts", 
     ylab = "probability")
mtext("binomial (1000, 0.013)", 3, 1)
lambda = n * p
plot(counts, 
     dpois(counts, lambda),
     type = "h", 
     xlab="counts", 
     ylab="probability")
mtext("Poisson (13.4)", 3, 1)
sum(dpois(0:80, 100)) # sum of individual probabilities
ppois(80, 100) # joint probability of first 80


## 3.3 Continuous distributions
# 3.3.1 The normal distribution
x = seq(-4, 4, 0.1)
y = dnorm(x)
plot(x, y, xlab = "x", ylab = "density", ylim = c(0, 0.8), type = "l") # line type: the quoted character is lower case L
mtext("normal(0, 1)", 3, 1)
abline(v = 0, lty = 2) # the vertical dashed line
lines(c(-1, 0), rep(dnorm(-1), 2), lty = 2)
x = seq(0, 8, 0.1)
y = dnorm(x, mean = 4, sd = 0.5)
pnorm(-1.96)
qnorm(0.02499790)
pnorm(0) - pnorm(-1)
x = rnorm(10, 3, 0.1)
x
x - mean(x)
(x - mean(x)) / sd(x)
scale(x)
attr(,"scaled:center")
attr(,"scaled:scale")
mean(x) == attr(x, "scaled:center")
sd(x) == attr(x1, "scaled:scale")
pnorm(0, 1, 3) - pnorm(-1, 1, 3)
v = rnorm(20, 4, 2) # repeating this command
# will result in a different vector
# of random numbers
sd(v) # sd of sample
sqrt(var(v)) # square root of variance
mean(v - mean(v)) #zero
var(v)
sum( (v - mean(v))^2)/(length(v) - 1)

# 3.3.2 The t, F, and ?? 2 distributions
pnorm(-3, 0, 1)
pt(-3, 2)
1 - pf(6, 1, 1)
1 - pf(6, 20, 8)
1 - pchisq(4, 1)
1 - pchisq(4, 5)
1 - pchisq(4, 10)


