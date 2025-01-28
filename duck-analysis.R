library(RColorBrewer)
## Loading in data.
duck.df <- read.csv("duck-data.csv")
duck.df$locationn <- duck.df$location
duck.df$location <- paste0("L", duck.df$location)
duck.df$bone.id <- paste0(duck.df$duck.id, duck.df$side, substr(duck.df$bone, 1, 1))

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

## Breaking things down by screw type and location.
boxplot(tibio.df$force ~ tibio.df$screwtype + tibio.df$location)
abline(v = 2*(1:4) + 0.5)

## Same for femur.
boxplot(femur.df$force ~ femur.df$screwtype + femur.df$location)
abline(v = 2*(1:4) + 0.5)

t.test(tibio.df$force[tibio.df$screwtype == "cortex" & tibio.df$location == "L1"],
       tibio.df$force[tibio.df$screwtype == "locking" & tibio.df$location == "L1"])



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

## Trying out a sex with location interaction.
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
    
## Same thing in nlme.
library(nlme)
library(MuMIn)
full.fit <- lme(force ~ (sex + location + screwtype + bone)^3, random = ~ 1 | duck.id / bone.id,
                weights = varIdent(form = ~ 1 | bone), data = duck.df, method = "ML")
d <- dredge(full.fit)
fit <- get.models(d, 1)[[1]]
summary(fit)

fitx <- lme(force ~ bone + location + sex + bone:location + bone:sex, random = ~ 1 | duck.id / bone.id,
           weights = varIdent(form = ~ 1 | bone), data = duck.df, method = "ML")
summary(fitx)

summary(duck.fit)
anova(duck.fit)
summary(duck.fit)


## Same but with glmmTMB.
library(glmmTMB)
fity <- glmmTMB(force ~ bone + location + sex + bone:location + bone:sex + (1 | duck.id / bone.id),
                dispformula = ~ bone, data = duck.df)
summary(fity)

## Dispersion model
sim1 <- function(nfac=40, nt=100, facsd=0.1, tsd=0.15, mu=0, residsd=1)
{
  dat <- expand.grid(fac=factor(letters[1:nfac]), t=1:nt)
  n <- nrow(dat)
  dat$REfac <- rnorm(nfac, sd=facsd)[dat$fac]
  dat$REt <- rnorm(nt, sd=tsd)[dat$t]
  dat$x <- rnorm(n, mean=mu, sd=residsd) + dat$REfac + dat$REt
  dat
}
set.seed(101)
d1 <- sim1(mu=100, residsd=10)
d2 <- sim1(mu=200, residsd=5)
d1$sd <- "ten"
d2$sd <- "five"
dat <- rbind(d1, d2)
m0 <- glmmTMB(x ~ sd + (1|t), dispformula=~sd, data=dat)
fixef(m0)$disp
c(log(5), log(10)-log(5)) # expected dispersion model coefficients
