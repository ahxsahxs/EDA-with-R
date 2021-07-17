data(mtcars)

df.carros = mtcars[, c('mpg', 'cyl', 'hp',
                       'wt', 'am', 'vs')]
colnames(df.carros) = c('Eficiencia', 'Cilindros',
                        'Potencia', 'Peso',
                        'TipoTransmissao',
                        'TipoMotor')

df.carros$TipoMotor = ifelse(
  df.carros$TipoMotor == 0,
  'Normal',
  'V-Shaped'
)
df.carros$TipoMotor = factor(df.carros$TipoMotor)

df.carros$TipoTransmissao = ifelse(
  df.carros$TipoTransmissao == 0,
  'Automática',
  'Manual'
)
df.carros$TipoTransmissao = factor(df.carros$TipoTransmissao)

df.carros$Cilindros = factor(df.carros$Cilindros)

df.carros$Carro = rownames(df.carros)

View(df.carros)


library(ggplot2)



ggplot(df.carros,
       mapping = aes(x = TipoMotor, y = Eficiencia)) +
  geom_boxplot() +
  labs(title = 'Dispersão da eficiência',
       subtitle = 'Agrupado pelo tipo de motor')


ggplot(df.carros,
       mapping = aes(x = Peso, y = Eficiencia,
                     colour = TipoMotor)) +
  geom_point() +
  geom_smooth()

ggplot(df.carros,
       mapping = aes(x = Peso, y = Eficiencia,
                     colour = Cilindros)) +
  geom_point() +
  geom_smooth()

ggplot(df.carros,
       mapping = aes(x = Peso, y = Eficiencia,
                     colour = TipoTransmissao)) +
  geom_point() +
  geom_smooth()

library(GGally)

df.analise = df.carros[, -7]
pairs(df.analise)

ggpairs(df.analise)
