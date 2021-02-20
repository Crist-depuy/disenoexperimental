Y=c(45,47,50,42,43,44,49,37,51,52,57,49)

df=expand.grid(LETTERS[1:4],1:3)
df$Y=Y
df

names(df)=c("Detergente","Lavadora","Y")
df

str(df)
df$Lavadora=factor(df$Lavadora)
str(df)

modelo=aov(Y~Detergente+Lavadora,data=df)
summary(modelo)

boxplot(Y~Detergente,data=df)
boxplot(Y~Lavadora,data=df)

boxplot(Y~Detergente*Lavadora,data=df)

tk=TukeyHSD(modelo)
tk

qqnorm(modelo$residuals)
qqline(modelo$residuals)

