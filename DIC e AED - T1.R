### Dicionário de dados e Primeira análise exploratória de dados

install.packages("readr")
install.packages("dplyr")

library(readr)

PelicanStores <- read_csv("~/LUCAS MARTINS/Pós-graduação/Estatistica/Trilha 1/Exercício de Aprofundamento/PelicanStoresESSE.txt")

library(dplyr)


# Estrutura do conjunto de dados
str(PelicanStores)

# Processo de estatística básico
summary(PelicanStores)

# Acessar qualquer elemento da tabela de forma Numérica
PelicanStores[2,1:4]

# Acessar qualquer elemento da tabela Nominal
PelicanStores[ 1:5 , c("Itens", "Vendas líquidas", "Idade")]

# Médias
df_medias <- data.frame(Variaveis = c("Itens", "Vendas líquidas", "Idade"), Média = apply(PelicanStores[,c("Itens", "Vendas líquidas", "Idade")], 2, mean))

# Mediana
df_mediana <- data.frame(Variaveis = c("Itens", "Vendas líquidas", "Idade"), Mediana = apply(PelicanStores[,c("Itens", "Vendas líquidas", "Idade")], 2, median))

# Desvio Padrão
df_sd <- data.frame(Variaveis = c("Itens", "Vendas líquidas", "Idade"), DesvioPadrao = apply(PelicanStores[,c("Itens", "Vendas líquidas", "Idade")], 2, sd))

# Variância
df_var <- data.frame(Variaveis = c("Itens", "Vendas líquidas", "Idade"), Variancia = apply(PelicanStores[,c("Itens", "Vendas líquidas", "Idade")], 2, var))

# Juntar Média e Mediana no mesmo df
df_1 <- left_join(df_medias,df_mediana, by ="Variaveis")

# Juntar df1 com desvio padrão
df_2 <- left_join(df_1, df_sd, by = "Variaveis")

# Juntar df2 com variancia
df_3 <- left_join(df_2, df_var, by = "Variaveis")

# Printar final
df_3

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Moda
table(PelicanStores[, "Tipo de Cliente"])
table(PelicanStores[, "Método de Pagamento"])
table(PelicanStores[, "Gênero"])
table(PelicanStores[, "Estado Civil"])

# Moda

df_modacat <- data.frame(Variaveis = c("Tipo de Cliente", "Método de Pagamento", "Gênero", "Estado Civil"), Moda = apply(PelicanStores[, c("Tipo de Cliente", "Método de Pagamento", "Gênero", "Estado Civil")], 2, getmode), row.names = NULL)

# Printar final + Moda
df_3
df_modacat