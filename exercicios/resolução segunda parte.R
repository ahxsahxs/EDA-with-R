data("ToothGrowth")

#TipoAdministracao = 'Suco de Laranja', quando supp == 'OJ'
#TipoAdministracao = 'Ácido Ascórbico', quando supp == 'VC'
ToothGrowth$TipoAdministracao = ifelse(
  ToothGrowth$supp == 'OJ',
  'Suco de Laranja',
  'Ácido Ascórbico'
)

# converter_tipo_administracao = function (tipoDieta) {
#   if (tipoDieta == 'OJ') {
#     return('Suco de Laranja')
#   } else {
#     return('Ácido Ascórbico')
#   }
# }
# 
# ToothGrowth$TipoAdministracao = sapply(
#   ToothGrowth$supp,
#   converter_tipo_administracao
# )


library(ggplot2)

ggplot(ToothGrowth,
       mapping =  aes(x = TipoAdministracao,
                      y = len)) +
  geom_boxplot() +
  xlab('Tipo de Administração') +
  ylab('Comprimento do odontoblasto') +
  labs(title = 'Dispersão do comprimento do odontoblasto')


ToothGrowth$dose = factor(ToothGrowth$dose)

ggplot(ToothGrowth,
       mapping =  aes(x = dose,
                      y = len,
                      colour = TipoAdministracao)) +
  geom_boxplot() +
  # geom_point() +
  xlab('Dose administrada') +
  ylab('Comprimento do odontoblasto') +
  labs(title = 'Dispersão do comprimento do odontoblasto',
       subtitle = 'Agrupados pelo tipo de administração')



data(chickwts)
data(ChickWeight)


ggplot(chickwts,
       mapping = aes(x = feed,
                     y = weight)) +
  geom_boxplot() +
  xlab('Dieta utilizada') +
  ylab('Peso após 6 semanas') +
  labs(title = 'Dispersão do peso',
       subtitle = 'agrupado por tipo de dieta')



galinha1 = subset(ChickWeight, Chick == 1)
galinha1 = galinha1[, c('Time', 'weight')]
colnames(galinha1) = c('Tempo', 'Peso')

View(galinha1)

# Brackets

ggplot(galinha1,
       mapping = aes(y = Peso, colour = Tempo)) +
  geom_boxplot() +
  geom_point(x = 0, size = 2) +
  labs(title = 'Dispersão do peso',
       subtitle = 'Para galinha 1')
  

ggplot(galinha1,
       mapping = aes(x = Tempo,
                     y = Peso)) +
  geom_point() +
  geom_line() +
  labs(title = 'Dispersão do peso em relação ao tempo',
       subtitle = 'Para galinha 1')


obter_estatisticas_dieta = function (codigoDieta) {
  dados_dieta = subset(ChickWeight, Diet == codigoDieta)
  
  estatistiscas = c(mean(dados_dieta$weight),
                    sd(dados_dieta$weight))
  
  return(estatistiscas)
}


niveis.dieta = levels(ChickWeight$Diet)
matriz.estatisticas = sapply(niveis.dieta,
                             obter_estatisticas_dieta)

df.estatisticas = data.frame(t(matriz.estatisticas))
colnames(df.estatisticas) = c('Media', 'DesvioPadrao')

df.estatisticas$TipoDieta = paste('Dieta', 1:4)

df.estatisticas$LimiteInferior = df.estatisticas$Media - df.estatisticas$DesvioPadrao
df.estatisticas$LimiteSuperior = df.estatisticas$Media + df.estatisticas$DesvioPadrao

View(df.estatisticas)

ggplot(df.estatisticas,
       mapping = aes(x = TipoDieta,
                     y = Media)) +
  geom_col(alpha = 0.5)+
  geom_errorbar(mapping = aes(
    ymin = LimiteInferior,
    ymax = LimiteSuperior
  ))

ggplot(ToothGrowth,
       aes(x = len,
           fill = supp)) +
  geom_density(alpha = 0.75)


