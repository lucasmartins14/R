# Análise exploratória de dados - Diamantes

install.packages("ggplot2")
install.packages("ggpubr")

library(ggplot2)
library(ggpubr)


data(diamonds)

diamantes <- diamonds

# Qual e a estrutura do conjunto de dados dos diamantes?
str(diamantes, strict.width = "wrap")

# Explore a parte inicial e a final do conjunto de dados.

# Inicio do conjunto de dados
head(diamantes)

# Fim do conjunto de dados
tail(diamantes)

# Faca alguns sumarios estatisticos para entender melhor a base de dados.
summary(diamantes)
dim(diamantes)

# Saida da funcao summary()esta de acordo com a descricao mostrada anteriormente? Sim.

# Explore a variavel price, seguindo o modelo de exploracao
boxplot(diamantes$price, col="red")

ggplot(diamantes) + geom_boxplot(aes(x = "", y = price),
                                 width = 0.5, varwidth = F) + labs(y = "Price", x = "") +
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))



ggplot(diamantes) + geom_bar(aes(x = cut), stat = "count",
                             fill = "wheat", color = "black", bin = 10) +
                               labs(y = "Num") + labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))


# Veja a distribuicao da variavel (histograma); observe a faixa de valores da variavel tambem
hist(diamantes$price)

ggplot(diamantes) + geom_histogram(aes(price),
                                   bins = 15, fill = "green", color = "black") +
                                   geom_vline(xintercept = median(diamantes$price),
                                              color = "black", lwd = 1) +
                                  theme_pubr() + theme(plot.caption = element_text(hjust = 0))
                                   




#Explore tambem as variaveis carat, cut, color, clarity, x, y, z, depth e table, seguindo o modelo de exploratorios


ggplot(diamantes) + geom_bar(aes(x = cut), stat = "count",
                             fill = "wheat", color = "black", bin = 10) +
  labs(y = "Num") + labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))


# Crie boxplots para as variaveis numericas; veja se existem dados anormais (outliers).


# Utilize as variaveis categoricas para fazer o facetamento dos dados, mostrando alguns graficos com 2 ou mais variaveis continuas lado a lado.

