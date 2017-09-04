# http://www.ats.ucla.edu/stat/r/pages/looping_strings.htm
hsb2 <- read.csv("http://www.ats.ucla.edu/stat/data/hsb2.csv")
the_url <- "https://stats.idre.ucla.edu/wp-content/uploads/2016/02/hsb2-2.csv"
hsb2 <- read.csv(the_url)
# hsb2 <- read.table(the_url, sep=",", header=T)
names(hsb2)

varlist <- names(hsb2)[8:11]

models <- lapply(varlist, function(x) {
  lm(substitute(read ~ i, list(i = as.name(x))), data = hsb2)
})

## look at the first element of the list, model 1
models[[1]]

## apply summary to each model stored in the list, models
lapply(models, summary)

# par(mfrow = c(2, 2), ask = TRUE) #set ask = TRUE: R will ask before changing graphs 
par(mfrow = c(2, 2), ask = FALSE)
invisible(lapply(models, plot))
# http://stackoverflow.com/questions/25987367/substitute-in-r-together-with-anova
# alternative 1
models <- lapply(varlist, function(x) {
  lm(as.formula(paste("read ~",x)), data = hsb2)
})
lapply(models, summary)

# alternative 2
models <- lapply(varlist, function(x) {
  aov(substitute(read ~ i, list(i = as.name(x))), data = hsb2)
})
lapply(models, summary)
#Error message:
#Error in terms.default(formula, "Error", data = data) : 
#no terms component nor attribute 
#problem: substitute() returns an expression, not a formula
#Neither substitute nor bquote typically return an expression. Typically they return a call
#Expression objects (as returned by expression()) are rarely
#There is an is.expression function and an 'expression' mode. That should serve as an operational definition. 

#solution1: evaluate the expression to get the formula
models <- lapply(varlist, function(x) {
  aov(eval(substitute(read ~ i, list(i = as.name(x)))), data = hsb2)
})
lapply(models, summary)

#solution2: evaluate the expression but with a "cleaner" call property for each of the result
models <- lapply(varlist, function(x) {
  eval(bquote(aov(read ~ .(as.name(x)), data = hsb2)))
})
lapply(models, summary)

#solution3:
models <- sapply(varlist, function(x) {
  lm(read ~ .,  data = hsb2[, c("read", x) ])
}, simplify=FALSE)
summary(models[[1]])  # The first model. Note the use of "[["
lapply(models, summary)
# setNames( sapply(...), varlist) to name the result

#solution4: do.call puts the variables into the call output so it reads properly
doModel <- function(col1, col2, data = hsb2, FUNC = "lm") 
{
  form <- as.formula(paste(col1, "~", col2))
  do.call(FUNC, list(form, substitute(data)))
}     
lapply(varlist, doModel, col1 = "read")
sapply(varlist, doModel, col1 = "read", simplify = FALSE) #keep the names in the list and also allow list$name subsetting

###
# http://stats.stackexchange.com/questions/11079/problem-with-anova-repeated-measures-error-model-is-singular
Nj        <- 10                               # number of subjects per sex
P         <- 2                                # number of levels for IV sex
Q         <- 3                                # number of levels for IV stimulus
R         <- 2                                # number of levels for IV condition
subject   <- factor(rep(1:(P*Nj), times=Q*R)) # subject id
sex       <- factor(rep(1:P, times=Q*R*Nj), labels=c("F", "M")) # IV sex
stimulus  <- factor(rep(1:Q, each=P*R*Nj))    # IV stimulus
condition <- factor(rep(rep(1:R, each=P*Nj), times=Q), labels=c("EXP1", "EXP2"))
DV_t11    <- round(rnorm(P*Nj,  8, 2), 2)     # responses for stimulus=1 and condition=1
DV_t21    <- round(rnorm(P*Nj, 13, 2), 2)     # responses for stimulus=2 and condition=1
DV_t31    <- round(rnorm(P*Nj, 13, 2), 2)
DV_t12    <- round(rnorm(P*Nj, 10, 2), 2)
DV_t22    <- round(rnorm(P*Nj, 15, 2), 2)
DV_t32    <- round(rnorm(P*Nj, 15, 2), 2)
response  <- c(DV_t11, DV_t12, DV_t21, DV_t22, DV_t31, DV_t32)       # all responses
dfL       <- data.frame(subject, sex, stimulus, condition, response) # long format

#use aov(), but you won't get the error corrections for the within-effects
summary(aov(response ~ sex*stimulus*condition + Error(subject/(stimulus*condition)), data=dfL))

#use the Anova() function from the car package, which gives you the error corrections
#it requires your data to be in wide format
#use multivariate notation for your model formula
sexW  <- factor(rep(1:P, Nj), labels=c("F", "M"))     # factor sex for wide format
dfW   <- data.frame(sexW, DV_t11, DV_t21, DV_t31, DV_t12, DV_t22, DV_t32) # wide format
#between-model in multivariate notation
fit   <- lm(cbind(DV_t11, DV_t21, DV_t31, DV_t12, DV_t22, DV_t32) ~ sexW, data=dfW)
#dataframe describing the columns of the data matrix
intra <- expand.grid(stimulus=gl(Q, 1), condition=gl(R, 1))
library(car)                    # for Anova()
summary(Anova(fit, idata=intra, idesign=~stimulus*condition), multivariate=FALSE, univariate=TRUE)

library(ez)              # for ezANOVA()
ezANOVA(data=dfL,
        wid=.(subject), 
        dv=.(response), 
        within=.(stimulus, condition), 
        between=.(sex), 
        observed=.(sex))

#Finally, if the main effect for sex is really all you're interested in, 
#it's equivalent to just average for each person across all the conditions 
#created by the combinations of stimulus and condition, 
#and then run a between-subjects ANOVA for the aggregated data.

#average per subject across all repeated measures
mDf <- aggregate(response ~ subject + sex, data=dfL, FUN=mean)
summary(aov(response ~ sex, data=mDf))     # ANOVA with just the between-effect

aov(response ~ stimulus * sex * condition + Error(subject/(stimulus * condition)))
a <- aggregate(response ~ stimulus + sex + subject, dfL, mean)
aov(response ~ stimulus * sex + Error(subject/stimulus), a)

###
# http://stackoverflow.com/questions/8121542/r-specifying-variable-name-in-function-parameter-for-a-function-of-general-uni
dataf <- data.frame (A= 1:10, B= 21:30, C= 51:60, D = 71:80)

myfun <- function (dataframe, varA, varB) {
  daf2 <- data.frame (A = dataframe$A*dataframe$B, 
                      B= dataframe$C*dataframe$D)
  anv1 <- lm(varA ~ varB, daf2)
  print(anova(anv1)) 
}             

# myfun (dataframe = dataf, varA = A, varB = B)
myfun (dataframe = dataf, varA = dataf$A, varB = dataf$B)

myfun2 <- function (dataframe, varA, varB) {
  attach(dataframe)
  daf2 <- data.frame (A = A*B, B= C*D)
  anv1 <- lm(varA ~ varB, daf2)
  return(anova(anv1))
}             

myfun2(dataframe = dataf, varA = A, varB = B)

#The essence of your problems is that you should always pass column names as characters, and use them as such.
myfun3 <- function (dataframe, varA, varB) {
  #on this next line, you use A and B. But this should be what is
  #passed in as varA and varB, no?
  daf2 <- data.frame (A = dataframe$A*dataframe$B, B=dataframe$C*dataframe$D)
  #so, as a correction, we need:
  colnames(daf2)<-c(varA, varB)
  #the first argument to lm is a formula. If you use it like this,
  #it refers to columns with _names_ varA and varB, not as names
  #the _contents_ of varA and varB!!
  anv1 <- lm(varA ~ varB, daf2)
  #so, what we really want, is to build a formula with the contents
  #of varA and varB: we have to this by building up a character string:
  frm<-paste(varA, varB, sep="~")
  anv1 <- lm(formula(frm), daf2)
  print(anova(anv1)) 
}             
#here, you pass A and B, because you are used to being able to do that in a formula
#(like in lm). But in a formula, there is a great deal of work done to make that
#happen, that doesn't work for most of the rest of R, so you need to pass the names
#again as character strings:
myfun3(dataframe = dataf, varA = A, varB = B)
#becomes:
myfun3(dataframe = dataf, varA = "A", varB = "B")

#alternative: the only place the variable names are actually used, are in the formula. 
myfun4 <- function (dataframe) {
  daf2 <- data.frame (A = dataframe$A*dataframe$B, 
                      B=dataframe$C*dataframe$D)
  #now we know that columns A and B simply exist in data.frame daf2!!
  anv1 <- lm(A ~ B, daf2)
  print(anova(anv1))
}  
myfun4(dataf)   

#Cleaned Function with trial:
myfun5 <- function (dataframe, varA, varB) {
  frm<-paste(varA, varB, sep="~")
  anv1 <- lm(formula(frm), dataframe)
  anova(anv1)
}
myfun5(dataframe = dataf, varA = "A", varB = "B")
myfun5(dataframe = dataf, varA = "A", varB = "D")
myfun5(dataframe = dataf, varA = "B", varB = "C")

foo<- data.frame(one=1:5,two=6:10)
bar <- function(y) eval(parse(text=paste('foo$',y,sep='')))

myfun6 <- function(formula, dataframe) {
  daf2 <- data.frame(A=dataframe$A*dataframe$B, B=dataframe$C*dataframe$D)
  anv1 <- lm(formula=formula, data=daf2)
  print(anova(anv1))
}

myfun6(formula=A~B, dataframe=dataf)

myfun7 <- function(dataframe, varA, varB) {
  daf2 <- data.frame(A=dataframe$A*dataframe$B, B=dataframe$C*dataframe$D)
  frm = as.formula(sprintf("%s~%s", varA, varB))
  anv1 <- lm(frm, daf2)
  print(anova(anv1))
}

myfun7(dataframe=dataf, varA="A", varB="B") 


###
# http://stackoverflow.com/questions/25659510/why-does-as-formula-only-work-inside-lm-inside-with
f_string <- 'Sepal.Length ~ Sepal.Width'
l <- with(iris, lm(as.formula(f_string))) # works fine
f_formula <- as.formula(f_string)
str(f_formula)
ls(envir=environment(f_formula))
l <- with(iris, lm(f_formula))
f_formula2 <- with(iris, as.formula(f_string))
lm(f_formula2)
ls(envir=environment(f_formula2))
# you shouldn't use with here, use the argument data= instead
lm(f_formula, iris)


###
# http://stackoverflow.com/questions/32442525/r-function-to-apply-anova-over-different-subsets-of-ones-dataset-and-collect-o
data(iris)
library(car)
data=data.frame()
for (i in 1:10) {data=rbind(data,cbind(replicate=i,iris))}
library(devtools)
# install_github("dgrtwo/broom")
library(broom)
library(dplyr)

group_by(data, replicate) %>% do(tidy(Anova(aov(Sepal.Length ~ Species, data = .),type="III"))) %>% filter(term=="Species")

anova_wrapper <- function(data, model_expression_as_string, grouping_variable,...) {
  f_wrap <- paste0('function(.) {',model_expression_as_string,'}') %>%    parse(text=.) %>% eval
  data %>% group_by_(grouping_variable) %>% 
    do(f_wrap(.) %>% Anova(...=...) %>% tidy) %>% return
}

aov_model_expression_as_string = 'aov(Sepal.Length ~ Species, data = .)'
lm_model_expression_as_string = 'lm(Sepal.Length ~ Sepal.Width + Petal.Length , data = .)'
grouping_variable = 'replicate'


data %>% 
  anova_wrapper(model_expression_as_string = aov_model_expression_as_string,
                grouping_variable = grouping_variable,type="III")

#using a lm instead of aov and different argument for Anova:

data %>% 
  anova_wrapper(model_expression_as_string = lm_model_expression_as_string,
                grouping_variable = grouping_variable,type="III")
#normally create a seperate function for each sort of analysis.

###
# http://stackoverflow.com/questions/27274523/dplyr-version-of-grouping-a-dataframe-then-creating-regression-model-on-each-gro
library(dplyr)
models <- iris %>% group_by(Species) %>% do(mod = lm(Sepal.Length ~ Sepal.Width, data = .))
models %>% rowwise %>% do(anova(.$mod))
models2 <- iris %>% group_by(Species) %>% do(mod = summary(lm(Sepal.Length ~ Sepal.Width, data = .)))
models2 %>% rowwise %>% do(anova(.$mod))

# to store the whole result in a list
plyr::dlply(iris, "Species", function(d) anova(lm(Sepal.Length ~ Sepal.Width, data = iris)))

library(nlme)
lmList(Sepal.Length ~ Sepal.Width | Species, iris)

mydf <- 
structure(list(country = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 
                                     2L, 2L, 2L, 2L, 2L, 2L), 
                                   .Label = c("Aruba", "Brazil"), 
                                   class = "factor"), 
               date = c(2011L, 2010L, 2009L, 2008L, 2007L, 2006L, 2011L, 
                        2010L, 2009L, 2008L, 2007L, 2006L), 
               BirthRate = c(10.584, 10.804, 11.06, 11.346, 11.653, 11.977, 
                             10.584, 10.804, 11.06, 11.346, 11.653, 11.977), 
               US. = c(25354.8, 24289.1, 24639.9, 27549.3, 25921.3, 24015.4, 
                       25354.8, 24289.1, 24639.9, 27549.3, 25921.3, 24015.4)), 
          .Names = c("country", "date", "BirthRate", "US."), 
          class = "data.frame", 
          row.names = c("4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"))
group_by(mydf, country) %>%
  do({model = lm(BirthRate ~ US., data = .);
  data.frame(int = coef(model)[1], slope = coef(model)[2])})


###
# http://stats.stackexchange.com/questions/59192/r-greenhouse-geisser-in-anova-from-car-package
phase <- factor(rep(c("pretest", "posttest", "followup"), c(5, 5, 5)),
                levels=c("pretest", "posttest", "followup"))
hour <- ordered(rep(1:5, 3))
idata <- data.frame(phase, hour)
idata

mod.ok <- lm(cbind(pre.1, pre.2, pre.3, pre.4, pre.5, 
                   post.1, post.2, post.3, post.4, post.5, 
                   fup.1, fup.2, fup.3, fup.4, fup.5) ~  1, 
             data=OBrienKaiser)

(av.ok <- Anova(mod.ok, idata=idata, idesign=~phase*hour) )
b<-summary(av.ok)

h2 <-
  structure(list(A1neg = c(-8.427556992, 1.20452559, -14.331842422, -10.428559303, 
                           1.750265002, 9.388166428, 0.790130436, -1.592002392, 
                           0.539065838, -3.758603573, 8.391399384), 
                 B1neg = c(-12.188085556, -1.964554906, -12.247328758, -7.326891422, 
                           -0.961694896, -1.048453212, -4.225459576, 0.173920691, 
                           1.876976371, -9.11947155, -1.706287026 ), 
                 A1pos = c(-0.660317183, 3.498036146, 22.003242493, 19.905063629,
                           -3.124288321, 11.968006134, 5.838645935, 5.140467644, 
                           5.154311657, 2.298083067, 1.164232969), 
                 B1pos = c(-12.805168152, -1.550003886, 45.990013123, 15.915545464, 
                           -1.67797184, 7.565258026, 10.635170937, 12.769438744, 
                           11.738276482, 4.544145107, 0.230011433)), 
            .Names = c("A1neg", "B1neg", "A1pos", "B1pos"), 
            class = "data.frame", row.names = c("1", "11", "21", "31", "41", "51", "61", "71", "81", "91", "101"))

condition <- ordered(rep(c("A", "B"), c(2)),
                     levels=c("A", "B"))
reg <- factor(rep(c("neg", "pos"), c(2,2)),
              levels=c("neg", "pos"))
idata<-data.frame(condition, reg)
idata

mod.ok<-lm(cbind( A1neg,B1neg,A1pos,B1pos) ~ 1, data=h2)
(av.ok<-Anova(mod.ok, idata=idata, idesign=~condition*reg))
summary(av.ok)

h3 <- structure(list(A1neg = c(-8.427556992, 1.20452559, -14.331842422, 
                               -10.428559303, 1.750265002, 9.388166428, 0.790130436, -1.592002392, 
                               0.539065838, -3.758603573, 8.391399384),
                     B1neg = c(-12.188085556, 
                               -1.964554906, -12.247328758, -7.326891422, -0.961694896, -1.048453212, 
                               -4.225459576, 0.173920691, 1.876976371, -9.11947155, -1.706287026),
                     C1neg = c(1.750265002, 0.539065838, 1.20452559, 8.391399384, -3.758603573, 
                               -7.326891422, 0.790130436, -9.11947155, -1.592002392, -12.188085556, 
                               -10.428559303),
                     A1pos = c(-0.660317183, 3.498036146, 22.003242493, 19.905063629, 
                               -3.124288321, 11.968006134, 5.838645935, 5.140467644, 5.154311657, 
                               2.298083067, 1.164232969),
                     B1pos = c(-12.805168152, -1.550003886, 
                               45.990013123, 15.915545464, -1.67797184, 7.565258026, 10.635170937, 
                               12.769438744, 11.738276482, 4.544145107, 0.230011433),
                     C1pos= c(-1.550003886, 1.164232969, 11.738276482, 5.838645935, -12.805168152, 
                              -0.660317183, 22.003242493, 19.905063629, 0.230011433, 7.565258026, 
                              5.154311657)),
                
                .Names = c("A1neg", 
                           "B1neg",
                           "C1neg",
                           "A1pos",
                           "B1pos",
                           "C1pos"),
                class = "data.frame", row.names = c("1", "11", "21", "31", "41", "51", "61", "71", "81", "91", "101"))

condition <- ordered(rep(c("A", "B", "C"), c(2)),
                     levels=c("A", "B", "C"))
reg <- factor(rep(c("neg", "pos"), c(3,3)),
              levels=c("neg", "pos"))
idata<-data.frame(condition, reg)
idata

mod.ok<-lm(cbind(A1neg,B1neg,C1neg, A1pos,B1pos,C1pos) ~ 1, data=h3)
(av.ok<-Anova(mod.ok, idata=idata, idesign=~condition*reg))
summary(av.ok)

# http://stats.stackexchange.com/questions/51826/anova-from-r-output-interpretation
# http://stats.stackexchange.com/questions/12398/how-to-interpret-f-and-p-value-in-anova
# http://stats.stackexchange.com/questions/5135/interpretation-of-rs-lm-output
# http://www.princeton.edu/~otorres/NiceOutputR.pdf #using stargazer for non-latex-user

utils::data(npk, package="MASS")
npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
summary(npk.aovE) ## THIS IS THE TABLE I WANT AS A TABLE OBJECT
library(xtable)
xtable(summary(npk.aovE))
xtable(npk.aovE)
lapply(summary(npk.aovE), xtable)
coef( summary(npk.aovE) )  # which returns NULL
summary(npk.aovE)[[2]]
?summary.aovlist

#http://stats.stackexchange.com/questions/67704/outputting-only-p-value-for-anova-in-r
models3 <- lm(yield ~  N*P*K + block, npk )
str(anova(models3))
anova(models3)$"Pr(>F)" #vector with all the p-values

anova(models3)[[5]] #vector with all the p-values
anova(models3)$"Pr(>F)"[1] # first Element of the vector
anova(models3)[[5]][1] # first Element of the vector 
anova(models3)[1,5] # because it's a data frame
aa <- anova(models3)
aa[1,5]


#http://stats.stackexchange.com/questions/35590/linear-regression-with-repeated-measures-in-r
#http://stats.stackexchange.com/questions/82387/how-to-specify-mixed-anova-with-multiple-repeated-measures-and-covariate-in-r

# This data comes from an experiment designed to investigate effects of different types of instruction on learning. 
# Instruction is manipulated between subjects using factorial combinations of "pretraining" (3 levels) 
# and "training" (2 levels). Learning is assessed as change in performance from a pretest, 
# administered before pretraining & training, to a posttest, administered after. 
# Test accuracy is my dv and test section (pre vs post) is treated as a within-subjects factor. 
# I also have an additional within-subjects factor called "problem type", 
# which is used to classify different types of problems on the pretest and posttest. 
# Some problem types have more problems than others, but the number of problems of a given type is 
# the same for pretest and posttest.

# Sample data:
nsubj = 215; nsec  = 2; nprob = 6
D = data.frame(
  subjid   = rep( 1:nsubj, each=nsec*nprob ),
  pretrain = rep( sample( c('a','b','c'), nsubj, replace=TRUE ), each=nsec*nprob ),
  training = rep( sample( c('j','k'), nsubj, replace=TRUE ), each=nsec*nprob ),
  study    = rep( sample( 1:6, nsubj, replace=TRUE ), each=nsec*nprob ),
  section  = rep( rep( c( 'pretest', 'posttest' ), each=nprob ), nsubj ),
  probtype = rep( c( 'v', 'w', 'x', 'y', 'z', 'z' ), nsec*nsubj ),
  accuracy = sample( c( 0.0, 0.5, 1.0 ), nsubj * nsec * nprob, replace=TRUE ) )

# Model:
library( ez )
options( contrasts=c("contr.sum","contr.poly") )
ezANOVA( data=D, wid=.(subjid), dv=.(accuracy), within=.(section,probtype), between=.(pretrain,training), type=3 )

# nlme version:
library( nlme )
fit = lme( accuracy ~ section*probtype*pretrain*training, random=~1|subjid, method='REML', data=D )
anova.lme( fit, type='marginal' )
# results in similar but not identical F and p values to the original model. Denominator dfs are quite different.

# Hint: Miller, G. A., & Chapman, J. P. (2001). Misunderstanding analysis of covariance. 
# Journal of Abnormal Psychology, 110(1), 40-48. doi:10.1037/0021-843X.110.1.40
library(afex)
#ANOVA
ez.glm("subjid", "accuracy", D, within=c("section","probtype"), 
       between= c("pretrain","training"))

#ANCOVA
ez.glm("subjid", "accuracy", D, within=c("section","probtype"), 
       between= c("pretrain","training"), covariate= "study",
       factorize = FALSE)
###
#https://www.ilri.org/biometrics/Publication/Full%20Text/Stephen_Mbunai_Sonal_Nagda.pdf
# http://goanna.cs.rmit.edu.au/~fscholer/anova.php
# http://stats.stackexchange.com/questions/60362/choice-between-type-i-type-ii-or-type-iii-anova

###

# http://www.uni-kiel.de/psychologie/rexrepos/posts/anovaCRp.html
## One-way ANOVA (CR-p design)

# Install required packages
wants <- c("car", "DescTools", "multcomp")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

# Simulate data
set.seed(123)
P     <- 4
Nj    <- c(41, 37, 42, 40)
muJ   <- rep(c(-1, 0, 1, 2), Nj)
dfCRp <- data.frame(IV=factor(rep(LETTERS[1:P], Nj)),
                    DV=rnorm(sum(Nj), muJ, 5))
# plot of chunk rerAnovaCRp01
plot.design(DV ~ IV, fun=mean, data=dfCRp, main="Group means")

# Using oneway.test()
# Assuming variance homogeneity
oneway.test(DV ~ IV, data=dfCRp, var.equal=TRUE)

# Generalized Welch-test without assumption of variance homogeneity
oneway.test(DV ~ IV, data=dfCRp, var.equal=FALSE)


# Using aov()
aovCRp <- aov(DV ~ IV, data=dfCRp)
summary(aovCRp)
model.tables(aovCRp, type="means")

# Model comparisons using anova(lm())
(anovaCRp <- anova(lm(DV ~ IV, data=dfCRp)))
anova(lm(DV ~ 1, data=dfCRp), lm(DV ~ IV, data=dfCRp))
anovaCRp["Residuals", "Sum Sq"]

# https://de.wikipedia.org/wiki/Effektst?rke
# https://de.wikipedia.org/wiki/Bestimmtheitsma%C3%9F
# https://www.inwt-statistics.de/blog-artikel-lesen/Bestimmtheitsmass_R2-Teil1.html
# Effect size estimates
dfSSb <- anovaCRp["IV",        "Df"]
SSb   <- anovaCRp["IV",        "Sum Sq"]
MSb   <- anovaCRp["IV",        "Mean Sq"]
SSw   <- anovaCRp["Residuals", "Sum Sq"]
MSw   <- anovaCRp["Residuals", "Mean Sq"]

(etaSq <- SSb / (SSb + SSw)) # Effektst?rke
library(DescTools)                     # for EtaSq()
EtaSq(aovCRp, type=1) 

(omegaSq <- dfSSb * (MSb-MSw) / (SSb + SSw + MSw)) # Populationseffektsch?ter
(f <- sqrt(etaSq / (1-etaSq))) # Effektst?rke

# Planned comparisons - a-priori
# General contrasts using glht() from package multcomp
cntrMat <- rbind("A-D"          =c(  1,   0,   0,  -1),
                 "1/3*(A+B+C)-D"=c(1/3, 1/3, 1/3,  -1),
                 "B-C"          =c(  0,   1,  -1,   0))
library(multcomp)                      # for glht()
summary(glht(aovCRp, linfct=mcp(IV=cntrMat), alternative="less"),
        test=adjusted("none"))

# Pairwise tt-tests
pairwise.t.test(dfCRp$DV, dfCRp$IV, p.adjust.method="bonferroni")


# Planned comparisons - post-hoc
# Scheffe tests
library(DescTools)                  # for ScheffeTest()
ScheffeTest(aovCRp, which="IV", contrasts=t(cntrMat))

# Tukey's simultaneous confidence intervals
(tHSD <- TukeyHSD(aovCRp))

# plot of chunk rerAnovaCRp02
plot(tHSD)

# Using glht() from package multcomp
library(multcomp)                      # for glht()
tukey <- glht(aovCRp, linfct=mcp(IV="Tukey"))
summary(tukey)
confint(tukey)


# Assess test assumptions
# Normality
Estud <- rstudent(aovCRp)
#plot of chunk rerAnovaCRp03
qqnorm(Estud, pch=20, cex=2)
qqline(Estud, col="gray60", lwd=2)

shapiro.test(Estud)

# Variance homogeneity
# plot of chunk rerAnovaCRp04
plot(Estud ~ dfCRp$IV, main="Residuals per group")

library(car)
leveneTest(aovCRp)

try(detach(package:car))
try(detach(package:multcomp))
try(detach(package:mvtnorm))
try(detach(package:splines))
try(detach(package:TH.data))
try(detach(package:survival))
try(detach(package:DescTools))

###
# http://www.uni-kiel.de/psychologie/rexrepos/posts/anovaCRFpq.html
## Two-way ANOVA (CRF-pq design)

# Install required packages
wants <- c("car", "DescTools", "multcomp", "phia")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

# CRF-pqpq ANOVA
# Using aov() (SS type I)
set.seed(123)
Njk  <- 8
P    <- 2
Q    <- 3
muJK <- c(rep(c(1, -1), Njk), rep(c(2, 1), Njk), rep(c(3, 3), Njk))
dfCRFpq <- data.frame(IV1=factor(rep(1:P, times=Njk*Q)),
                      IV2=factor(rep(1:Q,  each=Njk*P)),
                      DV =rnorm(Njk*P*Q, muJK, 2))
dfCRFpq$IVcomb <- interaction(dfCRFpq$IV1, dfCRFpq$IV2)
aovCRFpq <- aov(DV ~ IV1*IV2, data=dfCRFpq)
summary(aovCRFpq)

# Using Anova() from package car (SS type II or III)
# Since this design has equal cell sizes, all SS types give the same result.

# change contrasts for SS type III
fitIII <- lm(DV ~ IV1 + IV2 + IV1:IV2, data=dfCRFpq,
             contrasts=list(IV1=contr.sum, IV2=contr.sum))
library(car)                           # for Anova()
Anova(fitIII, type="III")

# Plot marginal and cell means
plot.design(DV ~ IV1*IV2, data=dfCRFpq, main="Marginal means")
interaction.plot(dfCRFpq$IV1, dfCRFpq$IV2, dfCRFpq$DV,
                 main="Cell means", col=c("red", "blue", "green"), lwd=2)

# Effect size estimate
library(DescTools)
EtaSq(aovCRFpq, type=1)

# Simple effects
library(phia)
testInteractions(aovCRFpq, fixed="IV2", across="IV1", adjustment="none")
testInteractions(aovCRFpq, fixed="IV1", across="IV2", adjustment="none")

# Planned comparisons
# Main effects only
# Free comparisons of marginal means
cMat <- rbind("c1"=c( 1/2, 1/2, -1),
              "c2"=c(  -1,   0,  1))

library(multcomp)
summary(glht(aovCRFpq, linfct=mcp(IV2=cMat), alternative="two.sided"),
        test=adjusted("bonferroni"))

# Tukey simultaneous confidence intervals
# Fit model without interaction that is ignored by Tukey's HSD.
aovCRF <- aov(DV ~ IV1 + IV2, data=dfCRFpq)
TukeyHSD(aovCRF, which="IV2")

# Using glht() from package multcomp.
library(multcomp)
tukey <- glht(aovCRF, linfct=mcp(IV2="Tukey"))
summary(tukey)
confint(tukey)

# Cell comparisons using the associated one-way ANOVA
(aovCRFpqA <- aov(DV ~ IVcomb, data=dfCRFpq))
cntrMat <- rbind("c1"=c(-1/2,  1/4, -1/2, 1/4, 1/4, 1/4),
                 "c2"=c(   0,    0,   -1,   0,   1,   0),
                 "c3"=c(-1/2, -1/2,  1/4, 1/4, 1/4, 1/4))
library(multcomp)
summary(glht(aovCRFpqA, linfct=mcp(IVcomb=cntrMat), alternative="greater"),
        test=adjusted("none"))

# Post-hoc Scheffe tests using the associated one-way ANOVA
library(DescTools)
ScheffeTest(aovCRFpqA, which="IVcomb", contrasts=t(cntrMat))

# Post-hoc Scheffe tests for marginal means
library(DescTools)
ScheffeTest(aovCRFpq, which="IV2", contrasts=c(-1, 1/2, 1/2))

# Assess test assumptions
# Normality
Estud <- rstudent(aovCRFpq)
qqnorm(Estud, pch=20, cex=2)
qqline(Estud, col="gray60", lwd=2)

shapiro.test(Estud)

# Variance homogeneity
plot(Estud ~ dfCRFpq$IVcomb, main="Residuals per group")
library(car)
leveneTest(aovCRFpq)

try(detach(package:phia))
try(detach(package:car))
try(detach(package:multcomp))
try(detach(package:mvtnorm))
try(detach(package:splines))
try(detach(package:TH.data))
try(detach(package:survival))
try(detach(package:DescTools))

###
# http://www.uni-kiel.de/psychologie/rexrepos/posts/ancova.html
## Analysis of covariance (ANCOVA)

# Install required packages
wants <- c("car", "effects", "multcomp")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

# Test the effects of group membership and of covariate
# Visually assess the data
SSRIpre  <- c(18, 16, 16, 15, 14, 20, 14, 21, 25, 11)
SSRIpost <- c(12,  0, 10,  9,  0, 11,  2,  4, 15, 10)
PlacPre  <- c(18, 16, 15, 14, 20, 25, 11, 25, 11, 22)
PlacPost <- c(11,  4, 19, 15,  3, 14, 10, 16, 10, 20)
WLpre    <- c(15, 19, 10, 29, 24, 15,  9, 18, 22, 13)
WLpost   <- c(17, 25, 10, 22, 23, 10,  2, 10, 14,  7)
P     <- 3
Nj    <- rep(length(SSRIpre), times=P)
dfAnc <- data.frame(IV=factor(rep(1:P, Nj), labels=c("SSRI", "Placebo", "WL")),
                    DVpre=c(SSRIpre,   PlacPre,  WLpre),
                    DVpost=c(SSRIpost, PlacPost, WLpost))
plot(DVpre  ~ IV, data=dfAnc, main="Pre-scores per group")
plot(DVpost ~ IV, data=dfAnc, main="Post-Scores per group")

# Type I sum of squares
fitFull <- lm(DVpost ~ IV + DVpre, data=dfAnc)
fitGrp  <- lm(DVpost ~ IV,         data=dfAnc)
fitRegr <- lm(DVpost ~      DVpre, data=dfAnc)
anova(fitFull)

# Type II/III sum of squares
# Since no interaction is present in the model, SS type II and III are equivalent here.
# Using Anova() from package car
library(car)                       # for Anova()
fitFiii <- lm(DVpost ~ IV + DVpre,
              contrasts=list(IV=contr.sum), data=dfAnc)
car::Anova(fitFiii, type="III")

#Using model comparisons for SS type II
anova(fitRegr, fitFull)
anova(fitGrp,  fitFull)

# Test individual regression coefficients
(sumRes <- summary(fitFull))
confint(fitFull)

# Vsisualize ANCOVA coefficients
coeffs    <- coef(sumRes)
iCeptSSRI <- coeffs[1, 1]
iCeptPlac <- coeffs[2, 1] + iCeptSSRI
iCeptWL   <- coeffs[3, 1] + iCeptSSRI
slopeAll  <- coeffs[4, 1]
xLims <- c(0, max(dfAnc$DVpre))
yLims <- c(min(iCeptSSRI, iCeptPlac, iCeptWL), max(dfAnc$DVpost))

plot(DVpost ~ DVpre, data=dfAnc, xlim=xLims, ylim=yLims,
     pch=rep(c(3, 17, 19), Nj), col=rep(c("red", "green", "blue"), Nj),
     main="Data and group-wise regression lines")
legend(x="topleft", legend=levels(dfAnc$IV), pch=c(3, 17, 19),
       col=c("red", "green", "blue"))
abline(iCeptSSRI, slopeAll, col="red")
abline(iCeptPlac, slopeAll, col="green")
abline(iCeptWL,   slopeAll, col="blue")

# Effect size estimate
# omega for the group effect
# Using SS type II
anRes <- anova(fitRegr, fitFull)
dfGrp <- anRes[2, "Df"]
dfE   <- anRes[2, "Res.Df"]
MSgrp <- anRes[2, "Sum of Sq"] / dfGrp
MSE   <- anRes[2, "RSS"] / dfE
SST   <- sum(anova(fitFull)[ , "Sum Sq"])
(omegaSqHat <- dfGrp*(MSgrp - MSE) / (SST + MSE))

# Planned comparisons between groups
# Adjusted group means
aovAncova <- aov(DVpost ~ IV + DVpre, data=dfAnc)
library(effects)                    # for effect()
YMjAdj <- effect("IV", aovAncova)
summary(YMjAdj)

# Planned comparisons
cMat <- rbind("SSRI-Placebo"  = c(-1,  1, 0),
              "SSRI-WL"       = c(-1,  0, 1),
              "SSRI-0.5(P+WL)"= c(-2,  1, 1))
library(multcomp)                    # for glht()
aovAncova <- aov(DVpost ~ IV + DVpre, data=dfAnc)
summary(glht(aovAncova, linfct=mcp(IV=cMat), alternative="greater"),
        test=adjusted("none"))

try(detach(package:effects))
try(detach(package:colorspace))
try(detach(package:lattice))
try(detach(package:grid))
try(detach(package:car))
try(detach(package:multcomp))
try(detach(package:mvtnorm))
try(detach(package:splines))
try(detach(package:TH.data))
try(detach(package:survival))


###

responseList <- names(iris)[-5]
modelList    <- lapply(responseList, function(resp) {
  mF <- formula(paste(resp, " ~ Species"))
  aov(mF, data = iris)
})
lapply(modelList, summary)

formula <- as.formula(paste0("cbind(", paste(names(iris)[-5], collapse = ","), ") ~ Species"))
fit <- aov(formula, data=iris)
summary(fit)

###
# http://stats.idre.ucla.edu/r/codefragments/spf2_22/
# SPLIT-PLOT FACTORIAL SPF2-22

library(foreign)  # to read the Stata file
library(lattice)  # for the graphics
library(lme4)  # to fit the model

spf <- read.dta("http://www.ats.ucla.edu/stat/data/spf2-22.dta")
head(spf,10)
histogram(~y | interaction(a, b, c), data = spf)

m1 <- lmer(y ~ factor(a) * factor(b) * factor(c) + (1 | s), data = spf)
anova(m1)

##


##

# oft test
# responseList <- vars.test
# modelList1 <- lapply(responseList, function(resp) {
#   mF <- formula(paste(resp, " ~ gtyp"))
#   aov(mF, data = df.data)
# })
# lapply(modelList1, summary)
# 
# modelList2 <- lapply(responseList, function(resp) {
#   mF <- formula(paste(resp, " ~ gtyp"))
#   oneway.test(mF, data=df.data, na.action=na.omit, var.equal=FALSE)
# })
# modelList2
# as.data.frame(modelList2)
# 
# formula <- as.formula(paste0("cbind(", paste(responseList, collapse = ","), ") ~ gtyp"))
# fit <- aov(formula, data=df.data)
# summary(fit)


# responseList <- vars.test
# modelList1 <- lapply(responseList, function(resp) {
#   mF <- formula(paste(resp, " ~ gtyp"))
#   aov(mF, data = df.data)
# })
# modelList1
# lapply(modelList1, summary)
# 
# modelList2 <- lapply(responseList, function(resp) {
#   mF <- formula(paste(resp, " ~ gtyp"))
#   anova(lm(mF, data = df.data))
# })
# modelList2
# lapply(modelList2, summary)
# 
# modelList3 <- lapply(responseList, function(resp) {
#   mF <- formula(paste(resp, " ~ gtyp"))
#   oneway.test(mF, data=df.data, na.action=na.omit, var.equal=FALSE)
# })
# modelList3
# 
# formula <- as.formula(paste0("cbind(", paste(responseList, collapse = ","), ") ~ gtyp"))
# fit <- aov(formula, data=df.data)
# summary(fit)

# model = lm(IMPC_OFD_001_001a ~ gtyp , data=df.data, na.action=na.omit)
# library(car)
# Anova(model, type="III") 
# Anova(model, type="II")[1,4] 
# Anova(model, type="II")
# anova(model)
# summary(model)