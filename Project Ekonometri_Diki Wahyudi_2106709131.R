# Model Persamaan Simultan
library(readr)
urlfile <- "https://raw.githubusercontent.com/dikiwahyudi11/Econometrics/main/jatim.csv" 
df <- read_delim(url(urlfile), delim = ";", escape_double = FALSE, trim_ws = TRUE)
df <- data.frame(df)
str(df)
colnames(df) <- c("kabupaten", "Y1", "Y2", "X2", "X1")
View(df)

# Visualisasi
library(ggpubr)
library(ggplot2)
p1 <- ggscatter(df, x = "Y2", y = "Y1", add = "reg.line", conf.int = TRUE,
                add.params = list(color = "blue", fill = "lightgray")) +
  xlab(bquote(Y[2])) + ylab(bquote(Y[1])) +
  stat_cor(method = "pearson") # Add correlation coefficient
p2 <- ggscatter(df, x = "X1", y = "Y1", add = "reg.line", conf.int = TRUE,
                add.params = list(color = "blue", fill = "lightgray")) +
  xlab(bquote(X[1])) + ylab(bquote(Y[1])) +
  stat_cor(method = "pearson") # Add correlation coefficient
p3 <- ggscatter(df, x = "Y1", y = "Y2", add = "reg.line", conf.int = TRUE,
                add.params = list(color = "blue", fill = "lightgray")) +
  xlab(bquote(Y[1])) + ylab(bquote(Y[2])) +
  stat_cor(method = "pearson", label.y = 90) # Add correlation coefficient
p4 <- ggscatter(df, x = "X2", y = "Y2", add = "reg.line", conf.int = TRUE,
                add.params = list(color = "blue", fill = "lightgray")) +
  xlab(bquote(X[2])) + ylab(bquote(Y[2])) +
  stat_cor(method = "pearson", label.y = 90) # Add correlation coefficient
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Cara Manual
# Stage 1 Regression
y1.lm <- lm(Y1 ~ X1 + X2, data = df)
y2.lm <- lm(Y2 ~ X1 + X2, data = df)
df$y1hat <- y1.lm$fitted.values
df$y2hat <- y2.lm$fitted.values
# Stage 2 Regression
m11 <- lm(Y1 ~ y2hat + X1, data = df)
summary(m11)
m21 <- lm(Y2 ~ y1hat + X2, data = df)
summary(m21)

# Cara Cepat
library(systemfit)
pdrb <- Y1 ~ Y2 + X1
ipm <- Y2 ~ Y1 + X2
sys <- list(pdrb, ipm)
instr <- ~ X1 + X2
model <- systemfit(sys, inst = instr, method = "2SLS", data = df)
summary(model)

library(ivreg)
pdrb <- ivreg(Y1 ~ Y2 + X1 | X1 + X2, data = df)
summary(pdrb)
ipm <- ivreg(Y2 ~ Y1 + X2 | X1 + X2, data = df)
summary(ipm)
crPlots(pdrb, smooth=list(span=1))
crPlots(ipm, smooth=list(span=1))
# Uji Asumsi
# Uji Normalitas
shapiro.test(resid(pdrb))
shapiro.test(resid(ipm))
# Uji Multikolinieritas
library(car)
vif(pdrb)
vif(ipm)
# Uji Homoskedastisitas
par(mfrow = c(1, 2))
plot(fitted(pdrb), rstudent(pdrb))
abline(h = 0)
plot(fitted(ipm), rstudent(ipm))
abline(h = 0)
par(mfrow = c(1, 2))
ncvTest(pdrb)
ncvTest(ipm)
# Uji Hausman untuk Simultanitas
df$vt <- y1.lm$residuals
hausman <- lm(Y2 ~ y1hat + vt, data = df)
summary(hausman)
