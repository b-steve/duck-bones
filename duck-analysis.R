library(RColorBrewer)
## Loading in data.
duck.df <- read.csv("duck-data.csv")
duck.df$locationn <- duck.df$location
duck.df$location <- paste0("L", duck.df$location)
duck.df$bone.id <- paste0(duck.df$duck.id, duck.df$side)

## Splitting into femur and tibiotarsus.
tibio.df <- duck.df[duck.df$bone == "tibiotarsus", ]
femur.df <- duck.df[duck.df$bone == "femur", ]

## Looking at some plots.

## No obvious differences between screw types. Bone types are similar
## in mean, but vary different in terms of variance.
boxplot(tibio.df$force[tibio.df$screwtype == "cortex"],
        tibio.df$force[tibio.df$screwtype == "locking"],
        femur.df$force[femur.df$screwtype == "cortex"],
        femur.df$force[femur.df$screwtype == "locking"],
        names = rep("", 4))
mtext(c("Tibiotarsus\nCortex", "Tibiotarsus\nLocking", "Femur\nLocking", "Femur\nCortex"),
      side = 1, line = 0.5, at = 1:4, padj = 1)

## No obvious differences between screw types. Bone types are similar
## in mean, but vary different in terms of variance.
boxplot(tibio.df$force[tibio.df$sex == "F"],
        tibio.df$force[tibio.df$sex == "M"],
        femur.df$force[femur.df$sex == "F"],
        femur.df$force[femur.df$sex == "M"],
        names = rep("", 4))
mtext(c("Tibiotarsus\nFemale", "Tibiotarsus\nMale", "Femur\nFemale", "Femur\nMale"),
      side = 1, line = 0.5, at = 1:4, padj = 1)

## Just for a sanity check, looking at left vs right.
boxplot(tibio.df$force[tibio.df$side == "L"],
        tibio.df$force[tibio.df$side == "R"],
        femur.df$force[femur.df$side == "L"],
        femur.df$force[femur.df$side == "R"],
        names = rep("", 4))
mtext(c("Tibiotarsus\nLeft", "Tibiotarsus\nRight", "Femur\nLeft", "Femur\nRight"),
      side = 1, line = 0.5, at = 1:4, padj = 1)

## Looking at individual ducks.
boxplot(force ~ duck.id, data = tibio.df)
boxplot(force ~ bone.id, data = tibio.df)
## And individual bones.
boxplot(force ~ duck.id, data = femur.df)
boxplot(force ~ bone.id, data = femur.df)

## Location for tibiotarsus.
boxplot(tibio.df$force ~ tibio.df$location, xlab = "Tibiotarsus location", ylab = "Force")
## Location for femur.
boxplot(femur.df$force ~ femur.df$location, xlab = "Femur location", ylab = "Force")

## Looking at individual duck and location.
plot.new()
plot.window(xlim = c(1, 5), ylim = range(tibio.df$force))
box()
axis(1)
axis(2)
title(xlab = "Location", ylab = "Force (N)")
k <- 1
for (i in unique(tibio.df$duck.id)){
    if (i != "s11"){
        lines(1:5, tibio.df$force[tibio.df$duck.id == i & tibio.df$side == "L"], col = k)
        if (i != "s19"){
            lines(1:5, tibio.df$force[tibio.df$duck.id == i & tibio.df$side == "R"], col = k)
        }
        k <- k + 1
    }
}


## Same again for the femur.
plot.new()
plot.window(xlim = c(1, 5), ylim = range(femur.df$force))
box()
axis(1)
axis(2)
title(xlab = "Location", ylab = "Force (N)")
k <- 1
for (i in unique(femur.df$duck.id)){
    if (i != "s3"){
        lines(1:5, femur.df$force[femur.df$duck.id == i & femur.df$side == "L"], col = k)
    }
    lines(1:5, femur.df$force[femur.df$duck.id == i & femur.df$side == "R"], col = k)
    k <- k + 1
}

## Fitting some models.
library(lme4)
library(boot)
library(boot.pval)
library(RLRsim)

## Tibiotarus data.

## Full model. Looks like no variance between bones.
tibio1.fit <- lmer(force ~ sex + location + screwtype + (1 | duck.id / bone.id), data = tibio.df)
summary(tibio1.fit)

## Taking out the bone random effect.
tibio2.fit <- lmer(force ~ sex + location + screwtype + (1 | duck.id), data = tibio.df)
summary(tibio2.fit)
confint(tibio2.fit)
## Test for significant duck effect.
exactRLRT(tibio2.fit)

## Putting in a sex/screw type interaction. Not really required.
tibio3.fit <- lmer(force ~ sex * screwtype + location + (1 | duck.id / bone.id), data = tibio.df)
summary(tibio3.fit)
anova(tibio3.fit)

## Taking out the location variable. Strongly significant.
tibio4.fit <- lmer(force ~ sex + screwtype + (1 | duck.id), data = tibio.df)
summary(tibio4.fit)
anova(tibio4.fit, tibio2.fit)

## Trying out a quadratic.
tibio5.fit <- lmer(force ~ sex + locationn + I(locationn^2) + screwtype + (1 | duck.id), data = tibio.df)
summary(tibio5.fit)

## Trying out a screw with location interaction.
tibio6.fit <- lmer(force ~ sex + location * screwtype + (1 | duck.id), data = tibio.df)
summary(tibio6.fit)
anova(tibio6.fit, tibio2.fit)

## Trying out a screw with location interaction.
tibio7.fit <- lmer(force ~ sex * location + screwtype + (1 | duck.id), data = tibio.df)
summary(tibio7.fit)
anova(tibio7.fit, tibio2.fit)

## CIs for sex and screw type.
boot <- bootMer(tibio2.fit, function(x) x@beta[c(2, 7)], nsim = 1000)
boot.ci(boot, type = "perc", index = 1)
boot.ci(boot, type = "perc", index = 2)


## Femur data.
femur1.fit <- lmer(force ~ sex + location + screwtype + (1 | duck.id / bone.id), data = femur.df)
summary(femur1.fit)

## Taking out the bone random effect.
femur2.fit <- lmer(force ~ sex + location + screwtype + (1 | duck.id), data = femur.df)
summary(femur1.fit)

## Putting in a sex/screw type interaction. Not really required.
femur3.fit <- lmer(force ~ sex * screwtype + location + (1 | duck.id), data = femur.df)
summary(femur3.fit)
anova(femur3.fit, femur2.fit)

## Trying out a screw with location interaction. Not really required.
femur6.fit <- lmer(force ~ sex + location * screwtype + (1 | duck.id), data = femur.df)
summary(femur6.fit)
anova(femur6.fit, femur2.fit)

## Trying out a screw with location interaction.
femur7.fit <- lmer(force ~ sex * location + screwtype + (1 | duck.id), data = femur.df)
summary(femur7.fit)
anova(femur7.fit, femur2.fit)

## CIs for sex and screw type.
boot <- bootMer(femur2.fit, function(x) x@beta[c(2, 7)], nsim = 1000)
boot.ci(boot, type = "perc", index = 1)
boot.ci(boot, type = "perc", index = 2)

## Making some predictions for the tibio data.
newdata <- expand.grid(sex = c("M", "F"), location = paste0("L", 1:5), screwtype = c("locking", "cortex"))
preds <- predict(tibio2.fit, newdata = newdata, re.form = ~ 0, se.fit = TRUE)
newdata$locationn <- as.numeric(substr(newdata$location, 2, 2))
preds.est <- preds$fit
preds.se <- preds$se.fit
preds.lower <- preds.est - qnorm(0.975)*preds.se
preds.upper <- preds.est + qnorm(0.975)*preds.se

plot.new()
plot.window(xlim = c(0.75, 5.25), ylim = c(min(preds.lower), max(preds.upper)))
box()
axis(1)
axis(2)
title(xlab = "Location", ylab = "Force (N)")
cols <- brewer.pal(n = 4, name = "Dark2")
k <- 1
for (i in c("F", "M")){
    for (j in c("cortex", "locking")){
        points(newdata$locationn[newdata$sex == i & newdata$screwtype == j] + (k - 2.5)/10,
               preds.est[newdata$sex == i & newdata$screwtype == j], col = cols[k], pch = 16)
        segments(newdata$locationn[newdata$sex == i & newdata$screwtype == j] + (k - 2.5)/10,
                 preds.lower[newdata$sex == i & newdata$screwtype == j],
                 newdata$locationn[newdata$sex == i & newdata$screwtype == j] + (k - 2.5)/10,
                 preds.upper[newdata$sex == i & newdata$screwtype == j], col = cols[k])
        k <- k + 1
    }
}
legend("topright", legend = c("Sex: F; Screw: Coretex", "Sex: F; Screw: Locking",
                            "Sex: M; Screw: Coretex", "Sex: M; Screw: Locking"),
       lty = 1, pch = 16, col = cols)

## Same for the femur data.
newdata <- expand.grid(sex = c("M", "F"), location = paste0("L", 1:5), screwtype = c("locking", "cortex"))
preds <- predict(femur2.fit, newdata = newdata, re.form = ~ 0, se.fit = TRUE)
newdata$locationn <- as.numeric(substr(newdata$location, 2, 2))
preds.est <- preds$fit
preds.se <- preds$se.fit
preds.lower <- preds.est - qnorm(0.975)*preds.se
preds.upper <- preds.est + qnorm(0.975)*preds.se

plot.new()
plot.window(xlim = c(0.75, 5.25), ylim = c(min(preds.lower), max(preds.upper)))
box()
axis(1)
axis(2)
title(xlab = "Location", ylab = "Force (N)")
cols <- brewer.pal(n = 4, name = "Dark2")
k <- 1
for (i in c("F", "M")){
    for (j in c("cortex", "locking")){
        points(newdata$locationn[newdata$sex == i & newdata$screwtype == j] + (k - 2.5)/10,
               preds.est[newdata$sex == i & newdata$screwtype == j], col = cols[k], pch = 16)
        segments(newdata$locationn[newdata$sex == i & newdata$screwtype == j] + (k - 2.5)/10,
                 preds.lower[newdata$sex == i & newdata$screwtype == j],
                 newdata$locationn[newdata$sex == i & newdata$screwtype == j] + (k - 2.5)/10,
                 preds.upper[newdata$sex == i & newdata$screwtype == j], col = cols[k])
        k <- k + 1
    }
}
legend("topright", legend = c("Sex: F; Screw: Coretex", "Sex: F; Screw: Locking",
                            "Sex: M; Screw: Coretex", "Sex: M; Screw: Locking"),
       lty = 1, pch = 16, col = cols)
    
