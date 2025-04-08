## Tibiotarsus data.
tibio.df <- read.csv("tibio-data.csv")
## Femur ata.
femur.df <- read.csv("femur-data.csv")

## Loading in required packages.
library(car)
library(lme4)
library(RLRsim)

## For tibiotarsus.

## Full model.
fit.tibio1 <- lmer(force ~ location*screwtype*sex + (1 | duck.id / bone.id),
             data = tibio.df)
## No need for three-way interaction.
Anova(fit.tibio1)
## Dropping out three-way interaction.
fit.tibio2 <- lmer(force ~ (location + screwtype + sex)^2 + (1 | duck.id / bone.id),
             data = tibio.df)
Anova(fit.tibio2)
## No need for two-way interactions.
fit.tibio3 <- lmer(force ~ location + screwtype + sex + (1 | duck.id / bone.id),
             data = tibio.df)
summary(fit.tibio3)
## Only location is statistically significant.
Anova(fit.tibio3)

## Testing random effects.
m.duck <- lmer(force ~ location + screwtype + sex + (1 | duck.id), data = tibio.df)
m.bone <- lmer(force ~ location + screwtype + sex + (1 | bone.id), data = tibio.df)
## Testing for the presence of bone effects.
exactRLRT(m.bone, fit.tibio3, m.duck)
## Testing for the presence of duck effects.
exactRLRT(m.duck, fit.tibio3, m.bone)

## We can drop bone effects to avoid a singular fit and because
## there isn't evidence they're required, and sex because we don't
## have evidence for an effect. We leave in screw type because it's
## our variable of primary interest.
fit.tibio.final <- lmer(force ~ location + screwtype + (1 | duck.id), data = tibio.df)
summary(fit.tibio.final)
Anova(fit.tibio.final)
      
## For femur. Everything falls out in the same way as the tibiotarsus
## data.

## Full model.
fit.femur1 <- lmer(force ~ location*screwtype*sex + (1 | duck.id / bone.id),
                   data = femur.df)
summary(fit.femur1)
## No need for three-way interaction.
Anova(fit.femur1)
## Dropping out three-way interaction.
fit.femur2 <- lmer(force ~ (location + screwtype + sex)^2 + (1 | duck.id / bone.id),
             data = femur.df)
Anova(fit.femur2)
## No need for two-way interactions.
fit.femur3 <- lmer(force ~ location + screwtype + sex + (1 | duck.id / bone.id),
             data = femur.df)
summary(fit.femur3)
## Only location is statistically significant.
Anova(fit.femur3)

## Testing random effects.
m.duck <- lmer(force ~ location + screwtype + sex + (1 | duck.id), data = femur.df)
m.bone <- lmer(force ~ location + screwtype + sex + (1 | bone.id), data = femur.df)
## Testing for the presence of bone effects.
exactRLRT(m.bone, fit.femur3, m.duck)
## Testing for the presence of duck effects.
exactRLRT(m.duck, fit.femur3, m.bone)

## We can drop bone effects to avoid a singular fit and because
## there isn't evidence they're required, and sex because we don't
## have evidence for an effect. We leave in screw type because it's
## our variable of primary interest.
fit.femur.final <- lmer(force ~ location + screwtype + (1 | duck.id), data = femur.df)
summary(fit.femur.final)
Anova(fit.femur.final)

## Making plots of final model estimates and confidence intervals.
newdata <- expand.grid(location = paste0("L", 1:5),
                       screwtype = c("cortex", "locking"))
newdata$locationn <- as.numeric(substr(newdata$location, 2, 2))
newdata$duck.id <- NA
newdata$bone.id <- NA
preds.tibio <- predict(fit.tibio.final, newdata = newdata, re.form = NA, se.fit = TRUE)
preds.femur <- predict(fit.femur.final, newdata = newdata, re.form = NA, se.fit = TRUE)
newdata$locationn[newdata$screwtype == "cortex"] <- newdata$locationn[newdata$screwtype == "cortex"] - 0.1
newdata$locationn[newdata$screwtype == "locking"] <- newdata$locationn[newdata$screwtype == "locking"] + 0.1
preds.tibio <- predict(fit.tibio.final, newdata = newdata, re.form = NA, se.fit = TRUE)
orig.tibio.df <- tibio.df
orig.tibio.df$locationn[orig.tibio.df$screwtype == "cortex"] <-
    orig.tibio.df$locationn[orig.tibio.df$screwtype == "cortex"] - 0.1
orig.tibio.df$locationn[orig.tibio.df$screwtype == "locking"] <-
    orig.tibio.df$locationn[orig.tibio.df$screwtype == "locking"] + 0.1
orig.femur.df <- femur.df
orig.femur.df$locationn[orig.femur.df$screwtype == "cortex"] <-
    orig.femur.df$locationn[orig.femur.df$screwtype == "cortex"] - 0.1
orig.femur.df$locationn[orig.femur.df$screwtype == "locking"] <-
    orig.femur.df$locationn[orig.femur.df$screwtype == "locking"] + 0.1

preds.est.tibio <- preds.tibio$fit
preds.se.tibio <- preds.tibio$se.fit
preds.lower.tibio <- preds.est.tibio - qnorm(0.975)*preds.se.tibio
preds.upper.tibio <- preds.est.tibio + qnorm(0.975)*preds.se.tibio
preds.est.femur <- preds.femur$fit
preds.se.femur <- preds.femur$se.fit
preds.lower.femur <- preds.est.femur - qnorm(0.975)*preds.se.femur
preds.upper.femur <- preds.est.femur + qnorm(0.975)*preds.se.femur

library(RColorBrewer)
library(tools)
cols <- brewer.pal(6, name = "Paired")[c(1, 2, 5, 6)]
## A plot with data and estimates for the average bird and bone.
scale <- 10
pw <- 480*2*scale
ph <- 480*scale
pcex <- scale
png(width = pw, height = ph, file = "duck-plot.png")
opar <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 0) + 0.1, cex = pcex, lwd = scale)
for (b in c("tibiotarsus", "femur")){
    if (b == "tibiotarsus"){
        odf <- orig.tibio.df
        ndf <- newdata
        npe <- preds.est.tibio
        npl <- preds.lower.tibio
        npu <- preds.upper.tibio
    }
    if (b == "femur"){
        odf <- orig.femur.df
        ndf <- newdata
        npe <- preds.est.femur
        npl <- preds.lower.femur
        npu <- preds.upper.femur
    }
    plot.new()
    plot.window(xlim = range(newdata$locationn),
                ylim = c(min(c(odf$force,
                               npl)),
                         max(c(odf$force,
                               npu))))
    box()
    axis(1)
    axis(2)
    title(xlab = "Location", ylab = "Maximum force (N)", main = toTitleCase(b))
    cols.est <- ifelse(ndf$screwtype == "cortex", cols[4], cols[2])
    cols.data <- ifelse(odf$screwtype == "cortex", cols[3], cols[1])
    points(odf$locationn, odf$force,
           col = cols.data)
    points(ndf$locationn, npe, pch = 16, col = cols.est, cex = 1.5)
    segments(x0 = ndf$locationn, y0 = npl,
             x1 = ndf$locationn, y1 = npu,
             col = cols.est, lwd = 2*scale)
    lines(ndf$locationn[ndf$screwtype == "cortex"], npe[ndf$screwtype == "cortex"], col = cols[4], lwd = scale)
    lines(ndf$locationn[ndf$screwtype == "locking"], npe[ndf$screwtype == "locking"], col = cols[2], lwd = scale)
    legend("topright", legend = c("cortex", "locking"), col = cols[c(4, 2)],
           lty = c(1, 1), pch = c(16, 16))
}
par(opar)
dev.off()
