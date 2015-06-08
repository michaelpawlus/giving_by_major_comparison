## set the working directory
setwd("C://users/pawlusm/Desktop")

## create a data frame from a report with the following four columns
#### ID, engagement score, major, fiscal year giving total
###### save this report as a csv file for importing into the R environment
ad <- read.csv("alum_donor_factors_no_names.csv")

## if you want run some quick analysis functions
summary(ad)     ## summary of the data
str(ad)         ## structure of the data
names(ad)       ## names of the columns

## remove outlier engagement score to make the data frame
## a more manageable size (16 columns instead of 100+)
ad2 <- ad[ad$aff_total<16,]

## quick check the new data frame (we still have a lot of rows)
summary(ad2)

## load the ggplot2 package for plotting charts
## if this packages is not installed then -> install.packages("ggplot2")
library(ggplot2)

## look at the data in  a box and whisker plot
## here we convert the engagement to a factor 
## treated like text rather than a number
## we also take the base10 log for giving to correct for extreme skewness
ggplot(ad2, aes(as.factor(aff_total), log10(fy14))) + geom_boxplot() +
        ylab("logFY14") +
        xlab("Affinity Score")

## we can view the data similarly as a jitter plot
## much like a b&w but density shown through dot concentration
ggplot(ad2, aes(as.factor(aff_total), log10(fy14))) + geom_jitter() +
        ylab("logFY14") +
        xlab("Affinity Score") 

## we can view the data as a scatterplot with a linear regression line
## for this plot we treat engagement scores as numbers again (not as factors)
ggplot(ad2, aes(aff_total, log10(fy14))) +
        geom_point(shape=1) +   # use hollow circles
        geom_smooth(method=lm)  # add a linear regression line

## lets try to add in a layer for comparing majors
## let's look at a histogram for majors
qplot(ad2$Major)
## there are way too many majors available

## we can create a table with the number of IDs for each major
maj_tbl <- aggregate(coreid ~ Major, data = ad2, length)

## then select the top 10 most represented majors in the dataset
head(maj_tbl[order(-maj_tbl$coreid),], n= 10)

## let's narrow this down to five
ad3 <- ad2[ad2$Major %in% c("Accounting","General Education","Business General","Nursing","Psychology","Management"),]

## take a quick check to make sure our subset worked
summary(ad3)

## let's try a side by side b&w chart
qplot(as.factor(aff_total), log10(fy14), data = ad3, color = Major, geom = c("jitter", "boxplot"))
## this is a little too busy

## let's go down to three
ad4 <- ad2[ad2$Major %in% c("Accounting","Nursing","Psychology"),]

## let's try the same b&w chart
qplot(as.factor(aff_total), log10(fy14), data = ad4, color = Major, geom = c("jitter", "boxplot"))
## that's better but it is still hard to compare

## what about side by side panels
qplot(aff_total, log10(fy14), data = ad4, color = Major, facets=~Major) + stat_smooth(method="lm")

## the panels show some interesting trends
## at lower affinity we get more buy-in from nursing
## but extra engagement doesn't boost giving
## accounting give less at a low affinity
## but extra engagement really matters and boosts giving significantly
## psych majors give at very low levels
## extra engagement results in a large boost
## however, they still end up below average
