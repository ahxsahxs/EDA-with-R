#########################################################
################ EXERCICIOS AULA 1 ######################
#########################################################

# Importação dos dados
df.alunos = read.csv('exercicios/idade_sexo.csv')

# Visualização do data.frame
View(df.alunos)


# Abaixo está sendo definido um vetor com o sexo dos alunos
vetor.sexo = df.alunos$Sexo
# vetor.sexo = df.alunos[ , 'Sexo']


# Abaixo está sendo definido um vetor com a idade dos alunos
vetor.idade = df.alunos$Idade
# vetor.idade = df.alunos[ , 'Idade']

# Declaração da matriz de 10 linhas e duas colunas
matrix.alunos = matrix(
  c(vetor.sexo, vetor.idade),
  ncol = 2,
  nrow = 10,
  byrow = FALSE
)


# Visualização da matriz de 10 linhas e duas colunas
View(matrix.alunos)


# Visualização da matriz de duas linhas e 10 colunas
View(t(matrix.alunos))

# Declaração da matriz de duas linhas e 10 colunas
matrix.alunos.transposta = matrix(
  c(vetor.sexo, vetor.idade), 
  nrow = 2,
  ncol = 10,
  byrow = TRUE
)

# Visualização da matriz de duas linhas e 10 colunas
View(matrix.alunos.transposta)


# Declara uma função para converter o código do sexo
# em um texto. (0 = Masculino, 1 = Feminino)
converter_sexo = function (codigoSexo) {
  
  if (codigoSexo == 0) {
    return ('Masculino')
  } else {
    return ('Feminino') 
  }
  
}

# Cria um vetor contendo o sexos no formato de texto
vetor.sexo.texto = sapply(vetor.sexo, converter_sexo)

# Visualiza o vetor
View(vetor.sexo.texto)

# Declara um fator contendo os textos de sexo
fator.sexo = factor(vetor.sexo.texto)

# Declara um data frame de duas colunas, a idade e o sexo
novo.data.frame = data.frame(
  Idade = vetor.idade,
  Sexo = fator.sexo
)


# Visualiza o novo data.frame
View(novo.data.frame)

# Salva os dados em um arquivo .csv
write.csv(
  novo.data.frame,
  file = 'exercicios/dados_tratados.csv'
)


#########################################################
################ EXEMPLOS AULA 2 ########################
#########################################################


#### Algumas estatísticas
min(df.alunos$Idade)
max(df.alunos$Idade)
mean(df.alunos$Idade)

#### Exibe as idades de forma ordenada
df.alunos$Idade[order(df.alunos$Idade)]

#### quantil de 50% ou mediana
quantile(df.alunos$Idade, probs = 0.5)

#### quantil de 75%
quantile(df.alunos$Idade, probs = 0.75)

#### quantil de 90%
quantile(df.alunos$Idade, probs = 0.9)


#######################################################
################# Alguns gráficos #####################
#######################################################

# Visualização de dados
View(ToothGrowth)

# Diagrama de dispersão do comprimento pela dose
plot(ToothGrowth$dose, ToothGrowth$len,
     xlim = c(0,2),
     xlab = 'Dose', ylab = 'Comprimento')


# Estima um modelo de regressão linear para a amostra
# Comprimento = MediaGlobal + Dose
lm1 = lm(len ~ dose, ToothGrowth)

# Salva os coeficientes do modelo
intercepto = coef(lm1)[1]
angulacao = coef(lm1)[2]

# Y = 7.4225 + 9.763571 * X
# Y = a + b * X

# Adiciona a reta de regressão no gráfico
abline(a = intercepto, b = angulacao)



#### Exemplo extra: Gráfico de linhas
plot(
  x = c(1,2,3,4,5,6,7,8),
  y = c(1,2,3,4,3,4,5,7),
  type = 'b'
)