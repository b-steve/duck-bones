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

## If we conduct a single t-test, we do have a significant difference
## between coretex and locking in the tibiotarsus location L1. But we
## shouldn't really be picking individual comparisons to conduct
## standalone hypothesis tests.
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

## Fitting some models with glmmTMB.
library(glmmTMB)
library(MuMIn)
fit.full <- glmmTMB(force ~ (sex + location + screwtype + bone)^3 + (1 | duck.id / bone.id),
                    dispformula = ~ bone,
                    data = duck.df, na.action = "na.fail")
d <- dredge(fit.full)
best.fit <- get.models(d, 1)[[1]]
summary(best.fit)
## Fitting the best model from scratch.
fit <- glmmTMB(force ~ bone + location + sex + bone:location + bone:sex +
                   (1 | duck.id / bone.id),
               dispformula = ~ bone, data = duck.df)
summary(fit)
## Re-levelling bone type.
duck.df$bone.relevel <- factor(duck.df$bone, levels = c("tibiotarsus", "femur"))
fit.bone.relevel <- glmmTMB(force ~ bone.relevel + location + sex + bone.relevel:location +
                                bone.relevel:sex + (1 | duck.id / bone.id),
                            dispformula = ~ bone, data = duck.df)
summary(fit.bone.relevel)
## Re-levelling sex.
duck.df$sex.relevel <- factor(duck.df$sex, levels = c("M", "F"))
fit.sex.relevel <- glmmTMB(force ~ bone + location + sex.relevel + bone:location +
                                bone:sex.relevel + (1 | duck.id / bone.id),
                           dispformula = ~ bone, data = duck.df)
summary(fit.sex.relevel)

## Re-levelling location a bunch of times. First to Location 2.
duck.df$location.relevel2 <- factor(duck.df$location, levels = paste0("L", 1:5)[c(2, 1, 3, 4, 5)])
fit.location2.relevel <- glmmTMB(force ~ bone + location.relevel2 + sex +
                                     bone:location.relevel2 + bone:sex +
                                     (1 | duck.id / bone.id),
                                 dispformula = ~ bone, data = duck.df)
summary(fit.location2.relevel)
confint(fit.location2.relevel)

## Location 3.
duck.df$location.relevel3 <- factor(duck.df$location, levels = paste0("L", 1:5)[c(3, 1, 2, 4, 5)])
fit.location3.relevel <- glmmTMB(force ~ bone + location.relevel3 + sex +
                                     bone:location.relevel3 + bone:sex +
                                     (1 | duck.id / bone.id),
                                 dispformula = ~ bone, data = duck.df)
summary(fit.location3.relevel)
confint(fit.location3.relevel)
## Location 4.
duck.df$location.relevel4 <- factor(duck.df$location, levels = paste0("L", 1:5)[c(4, 1, 2, 3, 5)])
fit.location4.relevel <- glmmTMB(force ~ bone + location.relevel4 + sex +
                                     bone:location.relevel4 + bone:sex +
                                     (1 | duck.id / bone.id),
                                 dispformula = ~ bone, data = duck.df)
summary(fit.location4.relevel)
confint(fit.location4.relevel)
## Location 5.
duck.df$location.relevel5 <- factor(duck.df$location, levels = paste0("L", 1:5)[c(5, 1, 2, 3, 4)])
fit.location5.relevel <- glmmTMB(force ~ bone + location.relevel5 + sex +
                                     bone:location.relevel5 + bone:sex +
                                     (1 | duck.id / bone.id),
                                 dispformula = ~ bone, data = duck.df)
summary(fit.location5.relevel)
confint(fit.location5.relevel)

## Now relevelling location with males as the baseline.
fit.sex.location2.relevel <-  glmmTMB(force ~ bone + location.relevel2 + sex.relevel +
                                          bone:location.relevel2 + bone:sex.relevel +
                                          (1 | duck.id / bone.id),
                                      dispformula = ~ bone, data = duck.df)
fit.sex.location3.relevel <-  glmmTMB(force ~ bone + location.relevel3 + sex.relevel +
                                          bone:location.relevel3 + bone:sex.relevel +
                                          (1 | duck.id / bone.id),
                                      dispformula = ~ bone, data = duck.df)
fit.sex.location4.relevel <-  glmmTMB(force ~ bone + location.relevel4 + sex.relevel +
                                          bone:location.relevel4 + bone:sex.relevel +
                                          (1 | duck.id / bone.id),
                                      dispformula = ~ bone, data = duck.df)
fit.sex.location5.relevel <-  glmmTMB(force ~ bone + location.relevel5 + sex.relevel +
                                          bone:location.relevel5 + bone:sex.relevel +
                                          (1 | duck.id / bone.id),
                                      dispformula = ~ bone, data = duck.df)

## Now relevelling location with tibiotarsus as the baseline.
fit.bone.location2.relevel <-  glmmTMB(force ~ bone.relevel + location.relevel2 + sex.relevel +
                                          bone.relevel:location.relevel2 + bone.relevel:sex.relevel +
                                          (1 | duck.id / bone.id),
                                      dispformula = ~ bone, data = duck.df)
fit.bone.location3.relevel <-  glmmTMB(force ~ bone.relevel + location.relevel3 + sex.relevel +
                                          bone.relevel:location.relevel3 + bone.relevel:sex.relevel +
                                          (1 | duck.id / bone.id),
                                      dispformula = ~ bone, data = duck.df)
fit.bone.location4.relevel <-  glmmTMB(force ~ bone.relevel + location.relevel4 + sex.relevel +
                                          bone.relevel:location.relevel4 + bone.relevel:sex.relevel +
                                          (1 | duck.id / bone.id),
                                      dispformula = ~ bone, data = duck.df)
fit.bone.location5.relevel <-  glmmTMB(force ~ bone.relevel + location.relevel5 + sex.relevel +
                                          bone.relevel:location.relevel5 + bone.relevel:sex.relevel +
                                          (1 | duck.id / bone.id),
                                      dispformula = ~ bone, data = duck.df)


newdata <- expand.grid(location = paste0("L", 1:5),
                       sex = c("F", "M"),
                       bone = c("tibiotarsus", "femur"))
newdata$locationn <- as.numeric(substr(newdata$location, 2, 2))
newdata$duck.id <- NA
newdata$bone.id <- NA
preds <- predict(fit, newdata = newdata, allow.new.levels = TRUE, se.fit = TRUE)
newdata$locationn[newdata$sex == "F"] <- newdata$locationn[newdata$sex == "F"] - 0.1
newdata$locationn[newdata$sex == "M"] <- newdata$locationn[newdata$sex == "M"] + 0.1
orig.df <- duck.df
orig.df$locationn[orig.df$sex == "F"] <- orig.df$locationn[orig.df$sex == "F"] - 0.1
orig.df$locationn[orig.df$sex == "M"] <- orig.df$locationn[orig.df$sex == "M"] + 0.1

preds.est <- preds$fit
preds.se <- preds$se.fit
preds.lower <- preds.est - qnorm(0.975)*preds.se
preds.upper <- preds.est + qnorm(0.975)*preds.se

library(RColorBrewer)
library(tools)
cols <- brewer.pal(6, name = "Paired")[c(1, 2, 5, 6)]
par(mfrow = c(2, 1), mar = c(4, 4, 3, 0) + 0.1)
## A plot with data and estimates for the average bird and bone.
for (b in c("tibiotarsus", "femur")){
    odf <- orig.df[orig.df$bone == b, ]
    ndf <- newdata[newdata$bone == b, ]
    npe <- preds.est[newdata$bone == b]
    npl <- preds.lower[newdata$bone == b]
    npu <- preds.upper[newdata$bone == b]
    plot.new()
    plot.window(xlim = range(newdata$locationn),
                ylim = c(min(c(orig.df$force[orig.df$bone == b],
                               preds.lower[newdata$bone == b])),
                         max(c(orig.df$force[orig.df$bone == b],
                               preds.upper[newdata$bone == b]))))
    box()
    axis(1)
    axis(2)
    title(xlab = "Location", ylab = "Force (N)", main = toTitleCase(b))
    cols.est <- ifelse(ndf$sex == "F", cols[4], cols[2])
    cols.data <- ifelse(odf$sex == "F", cols[3], cols[1])
    points(odf$locationn, odf$force,
           col = cols.data)
    points(ndf$locationn, npe, pch = 16, col = cols.est, cex = 1.5)
    segments(x0 = ndf$locationn, y0 = npl,
             x1 = ndf$locationn, y1 = npu,
             col = cols.est, lwd = 2)
    lines(ndf$locationn[ndf$sex == "F"], npe[ndf$sex == "F"], col = cols[4])
    lines(ndf$locationn[ndf$sex == "M"], npe[ndf$sex == "M"], col = cols[2])
    if (b == "tibiotarsus"){
        legend("topright", legend = c("F", "M"), col = cols[c(4, 2)],
               lty = c(1, 1), pch = c(16, 16))
    }
}

## Making a plot to explore the differences between the tibiotasus and
## the femur.
cis.f <- cis.m <- matrix(0, nrow = 5, ncol = 3)
cis.f[1, ] <- confint(fit)[2, ]
cis.m[1, ] <- confint(fit.sex.relevel)[2, ]
for (i in 2:5){
    cis.f[i, ] <- confint(get(paste0("fit.location", i, ".relevel")))[2, ]
    cis.m[i, ] <- confint(get(paste0("fit.sex.location", i, ".relevel")))[2, ]
}
plot.new()
plot.window(xlim = c(1 - 0.1, 5 + 0.1), ylim = c(min(c(cis.f[, 1], cis.m[, 1])),
                                                 max(c(cis.f[, 2], cis.m[, 2]))))
box()
axis(1)
axis(2)
points(1:5 - 0.1, cis.f[, 3], pch = 16, col = cols[4])
points(1:5 + 0.1, cis.m[, 3], pch = 16, col = cols[2])
segments(x0 = 1:5 - 0.1, y0 = cis.f[, 1], x1 = 1:5 - 0.1, y1 = cis.f[, 2], col = cols[4])
segments(x0 = 1:5 + 0.1, y0 = cis.m[, 1], x1 = 1:5 + 0.1, y1 = cis.m[, 2], col = cols[2])
abline(h = 0, lty = "dotted")
title(xlab = "Location", ylab = "Estimated difference between tibiotarsus and femur")
legend("topright", legend = c("F", "M"), col = cols[c(4, 2)],
       lty = c(1, 1), pch = c(16, 16))

## Same again, but for differences between males and females.
cis.fem <- cis.tib <- matrix(0, nrow = 5, ncol = 3)
cis.fem[1, ] <- confint(fit)[7, ]
cis.tib[1, ] <- confint(fit.bone.relevel)[7, ]
for (i in 2:5){
    cis.fem[i, ] <- confint(get(paste0("fit.location", i, ".relevel")))[7, ]
    cis.tib[i, ] <- confint(get(paste0("fit.bone.location", i, ".relevel")))[7, ]
}
plot.new()
plot.window(xlim = c(1 - 0.1, 5 + 0.1), ylim = c(min(c(cis.fem[, 1], cis.tib[, 1])),
                                                 max(c(cis.fem[, 2], cis.tib[, 2]))))
box()
axis(1)
axis(2)
points(1:5 - 0.1, cis.fem[, 3], pch = 16, col = cols[4])
points(1:5 + 0.1, cis.tib[, 3], pch = 16, col = cols[2])
segments(x0 = 1:5 - 0.1, y0 = cis.fem[, 1], x1 = 1:5 - 0.1, y1 = cis.fem[, 2], col = cols[4])
segments(x0 = 1:5 + 0.1, y0 = cis.tib[, 1], x1 = 1:5 + 0.1, y1 = cis.tib[, 2], col = cols[2])
abline(h = 0, lty = "dotted")
title(xlab = "Location", ylab = "Estimated difference between males and females")
legend("topright", legend = c("Femur", "Tibiotarsus"), col = cols[c(4, 2)],
       lty = c(1, 1), pch = c(16, 16))

