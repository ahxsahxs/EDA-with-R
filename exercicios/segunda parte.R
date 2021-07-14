View(ToothGrowth)


plot(ToothGrowth$dose, ToothGrowth$len,
     xlim = c(0,2),
     xlab = 'Dose', ylab = 'Comprimento')


# Comprimento = MediaGlobal + Dose
lm1 = lm(len ~ dose, ToothGrowth)

intercepto = coef(lm1)[1]
angulacao = coef(lm1)[2]

# Y = 7.4225 + 9.763571 * X
# Y = a + b * X

abline(a = intercepto, b = angulacao)

plot(
  x = c(1,2,3,4,5,6,7,8),
  y = c(1,2,3,4,3,4,5,7),
  type = 's'
)

?base::plot


data('ToothGrowth')
?data
attach(ToothGrowth)

