#baixando os pacotes que serão utilizados para a realização do projeto e os carregando
#Pacotes Utilizados
packages <- c("ggplot2", "corrplot", "ggtern", "readxl", "dplyr")

# Instalando e carregando pacotes
for (i in packages) {
  if (!require(i, character.only = TRUE)) {
    install.packages(i, dependencies = TRUE)
    library(i, character.only = TRUE)
  }
}

# Ler o arquivo com o banco de dados
dados <- read_excel("Concrete_Data.xls")


#renomeando as colunas para facilitar a modificação
colnames(dados) <- c(
  "cimento","escoria","cinza","agua",
  "plastificante","agregado_grosso",
  "agregado_fino","idade","resistencia")


#transformando em Experimento de Mistura:
#realizando a soma para podermos ter tudo em proporção 
dados$soma <- dados$cimento + dados$escoria + dados$cinza
#removendo linhas inválidas
dados <- dados[dados$soma > 0, ]
#criação das proporções
dados$x1 <- dados$cimento / dados$soma
dados$x2 <- dados$escoria / dados$soma
dados$x3 <- dados$cinza / dados$soma
#verificação
head(dados[, c("x1","x2","x3")])
#Conferindo se realmente a soma é igual a 1
all.equal(dados$x1 + dados$x2 + dados$x3, rep(1, nrow(dados)))


#definindo a resposta
y <- dados$resistencia


#Ajuste do Modelo Quadrático de Scheffé:
#O uso do '0 +' remove o intercepto, obrigatório para misturas
#Exibe R², Erro Padrão e p-valores dos coeficientes
#Verificação da significância individual de cada componente e interação.
modelo <- lm(resistencia ~ 0 + x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3,
             data = dados)
summary(modelo)


#Tabela de ANOVA (Análise de Variância)
# Testa a significância global do modelo.
tabela_anova <- anova(modelo)
tabela_anova


#Estatísticas básicas das variáveis:
summary(dados[, c("x1","x2","x3","resistencia")])
#desvio padrão:
sd(dados$x1)
sd(dados$x2)
sd(dados$x3)
sd(dados$resistencia)


#histogramas:
# Histograma Resistência (y)
h1 <- ggplot(dados, aes(x = y)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Histograma da Resistência", x = "Resistência (MPa)", 
       y = "Frequência") +
  theme_minimal()
# Histograma Cimento (x1)
h2 <- ggplot(dados, aes(x = x1)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 20) +
  labs(title = "Distribuição: Cimento (x1)", x = "Proporção de Cimento",
       y = "Frequência") +
  theme_minimal()
# Histograma Escória (x2)
h3 <- ggplot(dados, aes(x = x2)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 20) +
  labs(title = "Distribuição: Escória (x2)", x = "Proporção de Escória",
       y = "Frequência") +
  theme_minimal()
# Histograma Cinza (x3)
h4 <- ggplot(dados, aes(x = x3)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 20) +
  labs(title = "Distribuição: Cinza (x3)", x = "Proporção de Cinza", 
       y = "Frequência") +
  theme_minimal()

#Correlação:
# Calcule a matriz de correlação:
cor_matriz <- cor(dados[, c("x1", "x2", "x3", "resistencia")])
# 2. Criar o gráfico
corrplot(cor_matriz, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black", # Mostra os valores dentro
         tl.col = "black", 
         title = "Matriz de Correlação - Mistura HPC",
         mar = c(0,0,1,0))
#matriz de dispersão:
pairs(dados[, c("x1","x2","x3","resistencia")])

# Gráfico de Resíduos vs. Ajuste
# Verificar se a variância é constante e se o modelo é linear.
res <- plot(modelo, which = 1, main = "Resíduos vs. Ajuste")
res

# Gráfico de Normalidade (Q-Q Plot)
# Verificar se os erros seguem uma distribuição normal.
qq <- plot(modelo, which = 2, main = "Normal Q-Q Plot")

#Gráfico Triangular
#cor por resposta: azul = baixa resistência, vermelho = alta resistência
graph1 <- ggtern(data = dados, aes(x = x1, y = x2, z = x3, color = resistencia)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_distiller(palette = "RdYlBu", direction = -1) +
  labs(
    title = "Diagrama de Mistura - Resistência do Concreto",
    T = "Cimento (x1)", 
    L = "Escória (x2)", 
    R = "Cinza (x3)",
    color = "Resistência"
  ) +
  theme_bw() +
  theme_showarrows() +
  theme(
    plot.margin = margin(30, 80, 50, 80), 
    tern.axis.arrow.show = TRUE,
    tern.axis.arrow.sep = 0.12,
    tern.axis.arrow.text.B = element_text(vjust = 1.5), 
    tern.axis.title.L = element_text(hjust = 0.5, vjust = 0.5),
    tern.axis.title.R = element_text(hjust = 0.5, vjust = 0.5)
  )
graph1

#Visualizando os histogramas:
h1
h2
h3
h4
