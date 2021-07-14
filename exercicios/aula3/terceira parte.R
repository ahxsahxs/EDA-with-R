# Motivação

## Este script tem como propósito gerar alguns gráficos úteis na visualização dos
## dados. Diferentes formas de representar os dados foram utilizadas para facilitar
## a obtenção dos resultos pretendidos.

# Importação dos dados

## Lê o arquivo "mortes.csv" que se localiza na raiz do projeto.
df.mortes = read.csv('mortes.csv')

## Cria um vetor contendo a segunda coluna dos dados.
## Esta coluna contém o número mensal de mortes relacionadas a problemas
## pulmonares, desde janeiro de 1974 até dezembro de 1979.
vetor.mortes = df.mortes[, 2]

## Cria um gráfico de linhas para exibir a série temporal de variação dos dados.
## Pelo gráfico observamos uma tendência sazonal, sendo que ocorre um pico no
## número de mortes a cada intervalo de mais ou menos 12 meses.
plot(vetor.mortes, type = 'b',
     main = 'Variação mensal do número de mortes',
     sub = 'Mortes provocadas por problemas pulmonares no Reino Unido',
     xlab = 'Mês',
     ylab = 'Número de mortes')


## Cria um boxplot da variação total do número de mortes, isto é, considerando
## todas as observações disponíveis. De forma geral o número de mortes está
## concentrado entre 1500 e 2500, mas existem alguns casos atípicos que podem
## chegar até 4000 mortes no mês.
boxplot(vetor.mortes,
        main = 'Dispersão amostral do Número de mortes',
        ylab = 'Número de Mortes',
        sub = 'Mortes provocadas por problemas pulmonares no Reino Unido')


## Imprime novamente a série temporal, mas agora adicionando uma linha horizontal
## na mediana. Observamos que os dados são assimétricos, sendo que a variância
## dos valores abaixo da mediana é bem menor do que a variância dos valores acima
## da mediana. 
## Isto indica que os valores extremos acima do limite superior são bem distoantes
## do resto da amostra.
mediana = quantile(vetor.mortes, 0.5)
plot(vetor.mortes, type = 'b',
     main = 'Variação mensal do número de mortes',
     sub = 'Mortes provocadas por problemas pulmonares no Reino Unido',
     xlab = 'Mês',
     ylab = 'Número de mortes')
abline(h = mediana)



## Cria uma matriz contendo 12 colunas e 6 linhas, de forma que cada coluna
## seja composta por seis observações do número de mortes para um mesmo mês.
## Além disso cada linha é composta por 12 observações do número de mortes
## para um ano.
matrix.mortes = matrix(vetor.mortes,
                       ncol = 12,
                       nrow = 6,
                       byrow = TRUE)
colnames(matrix.mortes) = c(
  'Janeiro', 'Fevereiro', 'Março', 'Abril',
  'Maio', 'Junho','Julho', 'Agosto',
  'Setembro', 'Outubro', 'Novembro', 'Dezembro'
)
rownames(matrix.mortes) = 1974:1979
View(matrix.mortes)

## É possível também passar uma matriz ao comando boxplot, de forma que sejam
## gerados os diagramas para os 12 conjuntos de dados utilizados.
boxplot(matrix.mortes,
        main = 'Dispersão amostral do número de mortes',
        sub = 'Mortes provocadas por problemas pulmonares no Reino Unido',
        at = 1:12,
        names = colnames(matrix.mortes),
        col = '#c3e1eb',
        border = '#1a9c52'
)


# Vamos definir agora um data.frame de 72 linhas, uma para cada mês observado.
# As colunas serão o número de mortes, o mês e o ano da observação.

# 1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10,11,12,
# 1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10,11,12,
# 1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10,11,12
vetor.mes = rep(1:12, times = 6)

# 1974,1974,1974,1974,1974,1974,1974,1974,1974,1974,1974,
# 1975,1975,1975,1975,1975,1975,1975,1975,1975,1975,1975,
# 1976,1976,1976,1976,1976,1976,1976,1976,1976,1976,1976,
# 1977,1977,1977,1977,1977,1977,1977,1977,1977,1977,1977,
# 1978,1978,1978,1978,1978,1978,1978,1978,1978,1978,1978,
# 1979,1979,1979,1979,1979,1979,1979,1979,1979,1979,1979
vetor.ano = rep(1974:1979, each = 12)

novo.df.mortes = data.frame(
  Mortes = vetor.mortes,
  Mes = factor(vetor.mes, levels = 1:12),
  Ano = factor(vetor.ano, levels = 1974:1979)
)

converter_mes = function (indiceMes) {
  colnames(matrix.mortes)[indiceMes]
}

novo.df.mortes$MesDescritivo = sapply(novo.df.mortes$Mes, converter_mes)
novo.df.mortes$MesDescritivo = factor(novo.df.mortes$MesDescritivo,
                                      levels = colnames(matrix.mortes))


View(novo.df.mortes)


# ggplot2

## Para facilitar o processo de construção de imagens podemos utilizar algumas 
## bibliotecas do R. Uma das principais ferramentas disponíveis é a biblioteca ggplot2.

# Para instalar a biblioteca, descomente e execute o seguinte comando
# install.packages('ggplot2')

# Para importar a biblioteca é utilizado o comando *library*
library(ggplot2)
library(scales)

## Boxplot do número de mortes, separando os dados de cada mês
ggplot(data = novo.df.mortes,
       mapping = aes(x = MesDescritivo, y = Mortes)) +
  geom_boxplot() +
  labs(title = 'Dispersão amostral do número de mortes',
       subtitle = 'Mortes provocadas por problemas pulmonares no Reino Unido')

## Boxplot do número de mortes, separando os dados de cada ano
ggplot(data = novo.df.mortes,
       mapping = aes(x = Ano, y = Mortes)) +
  geom_boxplot() +
  labs(title = 'Dispersão amostral do número de mortes',
       subtitle = 'Mortes provocadas por problemas pulmonares no Reino Unido')

## Boxplot do número de mortes, utilizando o fator de agrupamento do ano
## para produzir mais de um diagrama.
ggplot(data = novo.df.mortes,
       mapping = aes(y = Mortes)) +
  geom_boxplot() +
  facet_wrap(~ Ano, nrow = 1) +
  labs(title = 'Dispersão amostral do número de mortes',
       subtitle = 'Mortes provocadas por problemas pulmonares no Reino Unido') +
  theme(axis.text.x = element_blank())



## Mapa de calor do número de mortes. Um mapa de calor é uma representação
## espacial de uma váriavel numérica. Neste caso temos duas dimensões de variação:
## o ano e o mês.
ggplot(data = novo.df.mortes,
       mapping = aes(x = MesDescritivo, y = Ano, fill = Mortes)) +
  geom_tile() +
  geom_text(mapping = aes(label = Mortes), colour = 'white') +
  labs(title = 'Mapa de calor do número de mortes por mês e ano',
       subtitle = 'Mortes provocadas por problemas pulmonares no Reino Unido') +
  scale_fill_gradient(low = muted('blue'), high = 'red')


## Vamos fazer agora algo mais elaborado. Vamos obter o desvio padrão do número
## de mortes para cada mês do ano. De posse desta informação vamos obter o 
## intervalo de confiança dado pela distância de 1.96 unidades de desvio padrão.

## Primeiro declaramos uma função que recebe o mês numérico, de 1 a 12, e
## retorna a média do número de mortes para aquele mês.
obter_media_do_mes = function (indiceMes) {
  dados.do.mes = subset(novo.df.mortes, Mes == indiceMes)
  
  return (mean(dados.do.mes$Mortes))
}

## Em seguida declaramos uma função que recebe o mês numérico, de 1 a 12, e
## retorna o desvio padrão do número de mortes para aquele mês.
obter_desvio_padrao_do_mes = function (indiceMes) {
  dados.do.mes = subset(novo.df.mortes, Mes == indiceMes)
  
  return (sd(dados.do.mes$Mortes))
}

## Utilizamos a função *sapply* para declarar dois vetores de 72 valores cada,
## o primeiro contém as médias do mês correspondente a cada observação do data.frame
## o segundo contém o desvio padrão do mês correspondente a cada observação do data.frame
vetor.medias = sapply(novo.df.mortes$Mes, obter_media_do_mes)
vetor.desvios.padroes = sapply(novo.df.mortes$Mes, obter_desvio_padrao_do_mes)
  

## Utilizamos os vetores definidos anteriormente para calcular os limites
## superiores e inferiores de confiança.
novo.df.mortes$LimiteSuperior = vetor.medias + 1.96 * vetor.desvios.padroes
novo.df.mortes$LimiteInferior = vetor.medias - 1.96 * vetor.desvios.padroes


## Geramos um gráfico de dispersão onde o mês está no eixo horizontal e o número
## de mortes no eixo vertical. Além dos pontos foi adicionada uma linha que liga
## as observações de um mesmo ano e também foi adicionada a região de confiança
## calculada utilizando os dados de todos os anos para um mesmo mês.
ggplot(data = novo.df.mortes,
       mapping = aes(x = MesDescritivo, y = Mortes, group = Ano)) +
  geom_ribbon(mapping = aes(ymin = LimiteInferior, ymax = LimiteSuperior),
              alpha = 0.20) +
  geom_line(mapping = aes(colour = Ano), size=2) +
  labs(title = 'Região de confiança para Variação mensal do número de mortes',
       subtitle = 'Mortes provocadas por problemas pulmonares no Reino Unido')


## Outra forma de visualização a evolução por ano seria utilizando o diagrama
## de área da biblioteca ggplot2. Nela os dados são sobrepostos de forma que o
## eixo horizontal continue sendo o mês, mas agora o eixo vertical é o total de
## mortes durante os 5 anos de estudo.
ggplot(data = novo.df.mortes,
       mapping = aes(x = MesDescritivo, y = Mortes,
                     group = Ano, fill = Ano)) +
  geom_area() +  
  labs(title = 'Número total de mortes',
       subtitle = 'Mortes provocadas por problemas pulmonares no Reino Unido')
  

## Outra questão que pode ser de interesse é verificar se o número de mortes de
## cada mês está apresentando tendência de queda ao longo do tempo. Para verificar
## tal informação podemos exibir a regressão linear do número de mortes sobre o ano
## das observações.


ggplot(data = novo.df.mortes,
       mapping = aes(x = Ano, y = Mortes,
                     group = MesDescritivo)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = 'lm') +
  facet_wrap( ~ MesDescritivo) +
  labs(title = 'Número total de mortes',
       subtitle = 'Mortes provocadas por problemas pulmonares no Reino Unido')

## Observe que em alguns meses, como janeiro e fevereiro, não é possível ver uma
## tendêcia de queda, enquanto para outros meses como novembro e dezembro a 
## redução do número de mortes é bastante expressiva.
