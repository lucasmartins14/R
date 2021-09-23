## Regressao Linear Simples e Multivariada

install.packages("tidyverse")

library(tidyverse)
library(readxl)
library(dplyr)

# Leitura dos dados
concrete <- read_excel("~/LUCAS MARTINS/Pós-graduação/Estatistica/Trilha 4/Concrete_Data.xls")

# Renomeando as variaveis Nomes muito extensos
names(concrete)

dim(concrete)

object.size(concrete)

names(concrete) <- c("Cement", "Blast", "Fly", "Water", "SupPlast", "CoarseAgg",
                     "FineAgg", "Age", "CCS")
names(concrete)                

str(concrete, strict.width = "wrap")

# Regressao simples, Cement como variavel preditora e a variavel CCS como alvo (dependente)

### -- Regressao Linear Simples
modreg1 <- lm(CCS ~ Cement, data = concrete)

# lm - Modelo Linear

summary(modreg1)

# Quando criamos um modelo linear com a função lm e rodamos o summary, devemos avaliar alguns pontos importantes:
#  1. Verificar se os parâmetros ajustados tem significância estatística; olhamos a coluna Pr(>|t|):
#   • Esta coluna nos dá o p-value, isto é, a estatística que testa a significância do parâmetro.
#   • Se os valores desta coluna forem menores do que alpha (nível de significância escolhido a priori),
#     então os parâmetros terão significância estatística, isto é, permanecem no modelo.
#   • Por exemplo, para α = 0.05, os parâmetros acima têm significância estatística.
# 2. Verificar se o modelo tem significância estatística; olhamos o p-value na última linha do sumário:
#   • Este valor deve ser menor do que o valor de alpha escolhido.
# No caso acima, partindo do valor default de α = 0.05, nosso modelo tem:
#   • os parâmetros têm significância estatística (p-value < 0.05);
#   • o modelo tem significância estatística (p-value < 0.05).
# Entretanto, nosso modelo consegue explicar muito pouco da variabilidade da variável alvo CCS. O valor do
# R-quadrado ajustado está em 0.2471, ou seja, explicamos menos de 25% da variabilidade de CCS.

# Depois do sumário, apesar de nosso modelo estar bem ruim, ou seja, explica pouco, ainda precisamos analisar
# se ele atende às premissas do método dos mínimos quadrados. Fazemos isso analisando os gráficos diagnósticos,
# que obtemos com o comando plot do objeto do modelo
par(mfrow = c(2,2))

plot(modreg1)

par(mfrow = c(1,1))

# Gráfico Residuals vs Fitted
#O gráfico Residuals vs Fitted mostra como os resíduos se comportam em relação aos valores ajustados. Um
#dos princípios do método dos mínimos quadrados é que a variância da variável alvo é constante, portanto,
#esperamos que os resíduos, isto é, a diferença entre o valor real da variável alvo e o valor predito pelo modelo,
#também tenha uma variância constante. Isso significa que, ao olharmos para este gráfico não devemos observar
#padrões, tendências, mas uma nuvem de resíduos dispersa aleatoriamente ao longo do eixo X. Quando o
#número de pontos é pequeno, nem sempre há uma nuvem de pontos bem definida; neste caso, devemos
#observar que a amplitude de variação dos resíduos é aproximadamente constante; ou seja, se os resíduos da
#esquerda estão com amplitudes parecidas com os resíduos da direita.

# Gráfico Normal Q-Q
#verifica se os resíduos seguem uma distribuição normal. No gráfico acima, os resíduos
#do nosso modelo não estão tão ruins. Há um pequeno desvio na parte inferior, mas na maior parte do gráfico,
#eles estão bem comportados, acompanhando a linha tracejada, inclusive na parte superior (só fogem um
#pouquinho no final, últimos pontos).

# Quando este gráfico apresenta padrões, como uma forma em S, ou uma forma de banana, então temos
# indício de que o modelo não está seguindo o padrão dos dados. Por exemplo, precisa de um termo quadrático
# para capturar uma forma de parábola nos dados, etc.

### -- Regressao Linear Multivariada
# Modelo com 2 variáveis preditoras (explicativas) para tentarmos melhorar o nosso R-quadrado ajustado.

modreg2 <- lm(CCS ~ Cement + SupPlast, data = concrete)

summary(modreg2)

# Fazemos as mesmas análises: significância dos parâmetros, significância do modelo e valor do R-quadrado ajustado.
# • Todos os parâmetros têm significância estatística, o que é bom!
# • O modelo também tem signfiicância estatística
# • O valor do R-quadrado ajustado melhorou um pouco, estamos agora com 0.35, ou seja, 35%.

# Agora os gráficos diagnósticos:

par(mfrow = c(2,2))
plot(modreg2)
par(mfrow = c(1,1))

# Os gráficos diagnósticos estão razoáveis. Não há padrão destacado no Residuals vs Fitted e o Normal Q-Q
# está muito parecido com o anterior.
# Podemos continuar então adicionando mais variáveis, ou fazemos o oposto – adicionamos todas e retiramos
# paulatinamente as que não tiverem signficiância estatística. Vou fazer deste último modo, adicionar tudo.


## Modelo completo – todas as preditoras

modregfull <- lm(CCS ~ ., data = concrete)

summary(modregfull)

# Analisando o sumário do modelo:
# • Temos duas variáveis com p-value maiores que 0.05: CoarseAgg e FineAgg. Todas as outras tem p-value menor que 0.05.
# • O R-quadrado ajustado está agora com 0.6125, ou seja, melhoramos a explicação da variância de CCS.
# • O modelo tem significância estatística a um nível de 0.05

par(mfrow = c(2, 2))
plot(modregfull)

# Tudo certo aqui: não temos padrão destacado no Residuals vs Fitted e o Normal Q-Q está bem comportado.

# Retirando variáveis sem significância estatística

modreg3 <- update(modregfull, . ~ . - CoarseAgg)

summary(modreg3)

# Após retirar a variável CoarseAgg ainda vemos que FineAgg permanece sem significância estatística. Então,
# ela é a próxima a ser removida.

modreg4 <- update(modreg3, . ~ . - FineAgg)

summary(modreg4)

# Vemos que todas as variáveis tem significância estatística a 0.05; o modelo também tem significância estatística
# e o nosso R-quadrado ajustado continua em 0.61.
# Para nos certificarmos de que o modelo segue as premissas do Método dos Mínimos Quadrados, rodamos os
# gráficos diagnósticos novamente

par(mfrow = c(2, 2))
plot(modreg4)
par(mfrow = c(1, 1))

# Novamente, tudo certo com os gráficos diagnósticos. Assim, temos nosso modelo com parâmetros com
#significância estatística e seguindo as premissas do método OLS. Podemos então passar para a interpretação do modelo.


## Interpretando o modelo

# A interpretação do modelo é feita através da análise dos parâmetros ajustados, e com um entendimento/consulta
# ao significado de cada variável

coef(modreg4)

#As variáveis estão todas na mesma unidade (kg in a mˆ3 of mixture), ou seja, kg do elemento em 1m3 de
#mistura de concreto, exceto a variável Age que é a idade (em dias) do concreto. Estas informações vêm do
#dicionário de dados, daí sua importância em uma análise de dados.
#Assim, entendemos que a cada dia de aumento na idade do concreto, temos um aumento de 0.1135 na força
#de compressão. Da mesma maneira, para cada kg de água adicionado (ou presente) no concreto, temos uma
#diminuição de 0.219 na força de compressão.
#A partir desta análise simples dos parâmetros, uma pergunta pode surgir: como se dá a perda de água do
#concreto? Isso porque, de modo inverso à análise acima, a cada kg de água retirado do concreto, temos um
#aumento de 0.219 na força de compressão.


# Então, podemos fazer um gráfico que relaciona a idade do concreto com a quantidade de água presente; de
#repente, temos algum insight do gráfico.

with(concrete, plot(Age, Water))

# Para esta nossa base de dados, não podemos concluir nada a partir deste gráfico.
# A análise deve continuar para cada parâmetro, buscando entender o modelo e despertar perguntas que possam
# levar a novas análises e melhorar o poder de explicação do modelo.

