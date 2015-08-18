georgia = read.csv("~/GitHub/STA380/data/georgia2000.csv")
attach(georgia)

georgia$percent = votes/ballots
attach(georgia)
plot(percent)
outlier0 = identify(percent, n=3)  # index 3, 9, 120
outlier1 = identify(percent, n = 2) # index 15, 53
georgia[outlier0,]
georgia[outlier1,]


## issue #1
hist(percent)
plot(percent~equip, data = georgia)
fix(georgia)
summary(equip)
LeverTable = georgia[georgia$equip == "LEVER",]
OpticalTable = georgia[georgia$equip == "OPTICAL",]
PaperTable = georgia[georgia$equip == "PAPER",]
PunchTable = georgia[georgia$equip == "PUNCH",]
par(mfrow = c(2,2))
hist(LeverTable$percent, main = "Lever Equipment", xlab = "% of vote recorded")
hist(OpticalTable$percent, main = "Optical Equipment", xlab = "% of vote recorded")
hist(PaperTable$percent, main = "Paper Equipment", xlab = "% of vote recorded", 4)
hist(PunchTable$percent, main = "Punch Equipment", xlab = "% of vote recorded")
summary(LeverTable$percent)
mean(LeverTable$percent)
mean(OpticalTable$percent)
mean(PaperTable$percent)
mean(PunchTable$percent)

boxplot(LeverTable$percent)
boxplot(OpticalTable$percent)
boxplot(PaperTable$percent)
boxplot(PunchTable$percent)

# final visual
library(ggplot2)
equipviolin = ggplot(georgia, aes(y=percent, x=equip)) + geom_violin() + geom_point()
summary(equip)
equipviolin 

## issue #2
## impact on poor and minority groups 
## poor
poorviolin = ggplot(georgia, aes(y=poor, x=equip)) + geom_violin() + geom_point()
poorviolin
# minority
minorityviolin = ggplot(georgia, aes(y=perAA, x=equip)) + geom_violin() + geom_point()
minorityviolin 
