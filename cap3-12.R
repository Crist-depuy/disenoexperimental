df=read.csv("frijol.csv",sep=";")
df

str(df)

df$Tratamieto=as.factor(df$Tratamieto)
str(df)

boxplot(Y~Tratamieto,data=df)

modelo=aov(Y~Tratamieto,data=df)
summary(modelo)

tk=TukeyHSD(modelo)
tk

plot(tk)

1-pf(1559,3,24)

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

library("car")

leveneTest(Y~Tratamieto,data=df)

plot(modelo$residuals)
abline(h=0)
plot(df$Tratamieto,modelo$residuals)
abline(h=0)
plot(modelo$fitted.values,modelo$residuals)
abline(h=0)