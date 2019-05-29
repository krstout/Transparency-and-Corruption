rm(list = ls())

setwd("C:\\Users\\Kevin\\Dropbox\\PSC 622 Political Behavior")

library(MASS)
library(texreg)

trans <- read.csv("Transparency Data Clean.csv")

# Transparency Data Clean is different from Transparency Data --> duplicate Germany and Russia observations removed
# in the Clean Data.

# Drop Cambodia

trans <- trans[trans$spine != "KH",]

### Reconstruct clarity

trans$inv.govfrac <- 1 - trans$govfrac

trans$inv.frac <- 1 - trans$frac

#trans$clarity <- ((trans$inv.frac + trans$inv.govfrac)/2) * 100
trans$clarity <- ((trans$inv.frac + trans$inv.govfrac + trans$allhouse)/3) * 100 

### Dichotomize clarity

trans$high.clarity <- ifelse(trans$clarity > 55.42, 1, 0)

### Invert Press Freedom

trans$press <- 100 - trans$Press.Score

### Average Press Freedom

trans$avg.press <- (trans$press + trans$obi)/2

### High Press

trans$high.press <- ifelse(trans$avg.press > 45.04, 1, 0)

### Invert Corruption

trans$cpi <- 100 - trans$CPI.Score

### Presidential System 

trans$presidential <- ifelse(trans$system == 0, 1, 0)

### Log GDP

trans$loggdp <- log(trans$GDPPC)

### Model

model1 <- lm(cpi ~ obi + clarity + press + presidential + housesys + mdmh + state + loggdp + 
               PR.Rating + obi*clarity, data = trans)
summary(model1)

#model2 <- rlm(cpi ~ obi + clarity + press + presidential + housesys + mdmh + state + loggdp + 
#               PR.Rating + press*clarity, data = trans)
#summary(model2)

model3 <- lm(cpi ~ avg.press + high.clarity + presidential + housesys + mdmh + state + loggdp +
               PR.Rating + avg.press*high.clarity, data = trans)
summary(model3)

#model4 <- lm(cpi ~ high.press + high.clarity + presidential + housesys + mdmh + state + loggdp +
#               PR.Rating + high.press*high.clarity, data = trans)
#summary(model4)

htmlreg(l = list(model1, model3), file = "Table 1.doc", digits = 3, 
        custom.model.names = c("Full Model", "High Clarity Model"), 
        custom.coef.names = c("Intercept", "Open Budget Index", "Clarity", "Press Freedom", "Presidential System",
                              "Plurality System", "Mean District Magnitude", "Federal System", "Logged GDP per Capita",
                              "Freedom House Democracy", "Budget X Clarity", "Average Transparency", "High Clarity",
                              "Transparency X High Clarity"),
        reorder.coef = c(1, 2, 4, 12, 3, 13, 5, 6, 7, 8, 9, 10, 11, 14),
        groups = list("Transparency and Clarity" = 2:6, "Political Context" = 7:10, "Controls" = 11:12, 
                      "Interactions" = 13:14))
