#Exemplo 15. Suponha que o peso médio dos Pinguins Reis encontrados em uma colônia
#na Antártica no último ano foi 15.4 kg. Em uma amostra de 35 pinguins na mesma época
#neste ano, na mesma colônia, o peso médio foi 14.6kg. Assuma que o desvio padrão da
#população seja 2.5kg, podemos afirmar que o peso médio dos pinguins se manteve em
#relação ao último ano?


# Estatiística de teste

# média da amostra
xbar = 14.1

# valor da hipótese
mu0 = 15.4

# desvio padrão da população
sigma = 2

# tamanho da amostra
n = 35

# estatística de teste
z = (xbar-mu0)/(sigma/sqrt(n))

z

# Teste de Hipótese

alpha = 0.05

z.half.alpha = qnorm(1 - alpha/2)

c(-z.half.alpha,z.half.alpha)


