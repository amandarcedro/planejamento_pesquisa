#Intruções de uso:
#Selecione todo o código (Ctrl+A) e rode (run)
#garanta que todos os pacotes foram devidamente instalados, caso tenha ocorrido algum erro, baixe-os manualmente e depois os selecione para carregar




#baixe os pacotes abaixo, caso não existam no seu computador e os carregue:
packages <- c("ggplot2", "corrplot", "tidyverse", "janitor", "skimr", "GGally", "psych", "dplyr", "reshape2", "FrF2", "car", "qcc", "lmtest")

# Instalando e carregando pacotes
for (i in packages) {
  if (!require(i, character.only = TRUE)) {
    install.packages(i, dependencies = TRUE)
    library(i, character.only = TRUE)
  }
}


#lendo os bancos de dados utilizados diretamente do github. Aqui foi utilizado o dataset "Wine Quality" com ambos os vinhos vermelho e branco:
red <- read.csv( "https://raw.githubusercontent.com/Amandacedro/Trabalho-2-PP/main/winequality-red.csv", sep=";")

white <- read.csv( "https://raw.githubusercontent.com/Amandacedro/Trabalho-2-PP/main/winequality-white.csv", sep=";" )



#criando a variável "tipo de vinho":
red$wine_type <- "Red"
white$wine_type <- "White"
#juntando os bancos de dados para formar um só:
wine <- bind_rows( red, white)
#limpando os nomes e transformando o tipo de vinho em fator:
wine <- wine %>% clean_names()
wine$wine_type <- as.factor( wine$wine_type)
#conferindo se há a existência de NA:
colSums(is.na(wine))


#estatística dos dados
summary(wine)



#criando subconjunto para o projeto
dados <- wine %>% select( quality, alcohol, volatile_acidity, sulphates, density, p_h, citric_acid, wine_type)
#codificação dos fatores
dados2 <- dados %>% mutate(
  A = ifelse( alcohol> median(alcohol), 1,-1), # A = variável álcool para o vinho analisado maior que a mediana de álcool atribui-se valor 1, caso contrário -1
  
  B = ifelse( volatile_acidity> median(volatile_acidity), 1,-1), # B = variável acidez volátil para o vinho analisado maior que a mediana de acidez volátil atribui-se valor 1, caso contrário -1
  
  C = ifelse( sulphates> median(sulphates), 1,-1), # C = variável sulfatos para o vinho analisado maior que a mediana de sulfatos atribui-se valor 1, caso contrário -1
  
  D = ifelse( density> median(density), 1,-1), # D = variável densidade para o vinho analisado maior que a mediana de densidade atribui-se valor 1, caso contrário -1
  
  E = ifelse( p_h> median(p_h), 1,-1), # E = variável ph para o vinho analisado maior que a mediana de ph atribui-se valor 1, caso contrário -1
  
  F = ifelse( citric_acid> median(citric_acid), 1,-1), # F = variável ácido cítrico para o vinho analisado maior que a mediana de ácido cítrico atribui-se valor 1, caso contrário -1
  
  G = ifelse( wine_type=="White", 1,-1) # G = variável tipo de vinho para o vinho analisado ser branco atribui-se valor 1, caso contrário -1
  
)



#estatísticas descritivas:
describe( wine %>% select(where(is.numeric)))
#distribuição da qualidade:
qualidade_vinhos <- ggplot(wine, aes(x = quality)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribuição da qualidade dos vinhos",
    x = "Qualidade",
    y = "Frequência"
  ) +
  theme_minimal()
qualidade_vinhos


#distribuição da qualidade por tipo de vinho
box <- ggplot(wine, aes(x = wine_type, y = quality, fill = wine_type)) +
  geom_boxplot( show.legend = FALSE) + 
  scale_fill_manual(values = c("Red" = "firebrick", "White" = "darkgray")) +
  labs(
    title = "Qualidade Segundo o Tipo do Vinho",
    x = "Tipo de Vinho",
    y = "Qualidade"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11)
  )
box

#histograma alcool por tipo:
dist_vinho <- ggplot( wine, aes( x = alcohol,fill = wine_type)) +
  geom_histogram(bins = 30, alpha = 0.9, position = "identity", color="white") +
  facet_wrap(~wine_type) +
  scale_fill_manual(values = c("Red" = "firebrick", "White" = "darkgray")) +
  labs(
    title = "Distribuição do teor alcoólico por tipo de vinho",
    x = "Álcool",
    y = "Contagem"
  ) +
  theme_minimal()+
  theme(legend.position = "none")
dist_vinho

#boxplot dos fatores:
boxplots <- wine %>%
  pivot_longer(
    cols = -c(quality, wine_type),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  ggplot(
    aes(
      x = wine_type,
      y = valor,
      fill = wine_type
    )
  ) +
  geom_boxplot() +
  facet_wrap(
    ~variavel,
    scales = "free"
  ) +
  scale_fill_manual(values = c("Red" = "firebrick", "White" = "darkgray")) +
  labs(title = "Boxplot dos fatores", x = "tipo de vinho",fill = "Tipo de vinho") +
  theme_minimal()
boxplots


#matriz de correlação:
correlacao <-
  wine %>%
  select(
    where(is.numeric)
  )%>%
  cor()
correlacao
melted <- melt(correlacao)

#correlação
corr <- ggplot(
  melted,
  aes(
    Var1,
    Var2,
    fill=value
  )
)+
  geom_tile(color="white")+
  geom_text(
    aes(
      label=round(value,2)
    ),
    size=3
  )+
  scale_fill_gradient2(
    low="firebrick",
    mid="white",
    high="navy",
    midpoint=0
  )+
  labs(
    title = "Matriz de Correlação das Variáveis",
    subtitle = "Valores próximos de 1 ou -1 indicam forte associação linear",
    x = NULL, 
    y = NULL,
    fill = "valores"
  )+
  theme_minimal()+
  theme(
    axis.text.x=
      element_text(
        angle=45,
        hjust=1
      )
  )
corr

#gráfico de dispersão:
#Álcool × qualidade:
disp1 <- ggplot(
  wine,
  aes(
    x = alcohol,
    y = quality,
    color = wine_type,
    fill = wine_type
  )
) +
  geom_jitter(
    alpha = 0.25,
    size = 1.5,
    width = 0.1,
    height = 0.15
  ) +
  geom_smooth(
    method = "lm",
    size = 1.2,
    alpha = 0.2
  ) +
  scale_color_manual(values = c("Red" = "firebrick", "White" = "darkgray")) +
  scale_fill_manual(values = c("Red" = "firebrick", "White" = "darkgray")) +
  labs(
    title = "Tendência da Qualidade do Vinho pelo Teor Alcoólico",
    x = "Álcool",
    y = "Qualidade",
    color = "Tipo de Vinho",
    fill = "Tipo de Vinho"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10)
  )
disp1


#Acidez volátil:
disp2 <- ggplot(
  wine,
  aes(
    x = volatile_acidity,
    y = quality,
    color = wine_type,
    fill = wine_type
  )
) +
  geom_jitter(
    alpha = 0.25,
    width = 0.01,
    height = 0.15
  ) +
  geom_smooth(
    method = "lm",
    size = 1.2,
    alpha = 0.2
  ) +
  scale_color_manual(values = c("Red" = "firebrick", "White" = "darkgray")) +
  scale_fill_manual(values = c("Red" = "firebrick", "White" = "darkgray")) +
  labs(
    title = "Qualidade versus Acidez Volátil",
    subtitle = "Tendência linear indica que maior acidez volátil reduz a qualidade do vinho",
    x = "Acidez Volátil",
    y = "Qualidade",
    color = "Tipo de Vinho", 
    fill = "Tipo de Vinho"   
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10)
  )
disp2



#matriz de dispersão:
matriz <- wine %>%
  select(
    quality,
    alcohol,
    volatile_acidity,
    sulphates,
    density
  )%>%
  ggpairs(
    title="Matriz de dispersão entre variáveis"
  )
matriz



#Construção do planejamento fracionário:
plano <- FrF2(
  nruns=16,
  nfactors=7,
  factor.names=c(
    "A",
    "B",
    "C",
    "D",
    "E=ABC",
    "F=BCD",
    "G=ACD"
  ),
  randomize=FALSE
)
plano



design.info(plano)
aliasprint(plano)
cat(
  "Planejamento:",
  nrow(plano),
  "corridas para",
  ncol(plano),
  "fatores"
)
corridas_completo <- 2^7
corridas_fracionario <- nrow(plano)
economia <- (corridas_completo-corridas_fracionario)/
  corridas_completo*100
cat(
  "\nPlanejamento completo:",
  corridas_completo,
  "\nPlanejamento utilizado:",
  corridas_fracionario,
  "\nEconomia experimental:",
  round(economia,2),
  "%"
)




#modelo inicial com fatores codificados:
mod=
  lm(
    quality~
      A+
      B+
      C+
      D+
      E+
      F+
      G+
      A:G+
      B:G+
      C:G,
    data=dados2
  )



#resumo do modelo:
summary(mod)



#anova
anova(mod)
pf(
  summary(mod)$fstatistic[1],
  summary(mod)$fstatistic[2],
  summary(mod)$fstatistic[3],
  lower.tail=FALSE
)



#teste t tipo de vinho:
t.test(
  quality~wine_type,
  data=wine
)



#comparação formal entre tipos de vinho
anova(
  lm(
    quality~wine_type,
    data=wine))



#gráfico de interação:
#Álcool × tipo:
interacao1 <- dados2 %>%
  group_by(G, A) %>%
  summarise(media = mean(quality), .groups = "drop") %>%
  ggplot(
    aes(
      x = factor(A, levels = c(-1, 1), labels = c("Baixo", "Alto")),
      y = media,
      group = factor(G),
      color = factor(G, levels = c(-1, 1), labels = c("Tinto", "Branco"))
    )
  ) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Tinto" = "firebrick", "Branco" = "darkgray")) +
  labs(
    title = "Gráfico de Interação: Qualidade Média",
    x = "Teor Alcoólico",
    y = "Qualidade Média",
    color = "Tipo de Vinho"
  ) +
  theme_minimal()
interacao1



#pareto dos efeitos
#pareto dos efeitos
effects <- 2*coef(mod)[-1]
dadospareto <- data.frame(
  efeito = names(effects),
  valor = abs(effects)
)



pareto <- ggplot(
  dadospareto,
  aes(
    x = reorder(efeito, valor), 
    y = valor
  )
) +
  geom_col(fill = "steelblue", alpha = 0.85, width = 0.7) +
  geom_text(
    aes(label = round(valor, 2)), 
    hjust = -0.2, 
    size = 3.5, 
    fontface = "bold",
    color = "black"
  ) +
  coord_flip() +
  labs(
    title = "Diagrama de Pareto dos Efeitos",
    subtitle = "Fatores ordenados por magnitude de impacto na resposta",
    x = "Efeitos / Fatores",
    y = "Valor Absoluto do Efeito"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )
pareto


#diagnóstico:
#Gráfico resíduos × ajustados:
res <- ggplot(
  data.frame(
    ajustado=fitted(mod),
    res=residuals(mod)
  ),
  aes(
    ajustado,
    res
  )
)+
  geom_point(alpha=.25)+
  geom_hline(
    yintercept=0,
    linetype=2
  )+
  labs(
    title="Resíduos versus valores ajustados",
    x="Valores ajustados",
    y="Resíduos"
  )+
  theme_minimal()
res


#qqplot:
qq <-  ggplot(
  data.frame(res = residuals(mod)),
  aes(sample = res)
) +
  stat_qq(color = "black", alpha = 0.8, size = 2) +
  stat_qq_line(color = "red", size = 1) +
  labs(
    title = "Gráfico de Probabilidade Normal dos Resíduos (Q-Q Plot)",
    x = "Quantis Teóricos",
    y = "Quantis Amostrais (Resíduos)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10)
  )
qq


#histograma resíduos:
res2 <- ggplot(data.frame(res = residuals(mod)), aes(res)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribuição de Resíduos do Modelo",
       x = "Resíduos",
       y = "Frequência") +
  theme_minimal()
res2

#teste de normalidade dos resíduos
set.seed(123)
shapiro.test(
  sample(
    residuals(mod),
    500
  )
)

#homocedasticidade
bptest(mod)

#colinearidade
vif(mod)

#Teste de Autocorrelação dos Resíduos
dwtest(mod)



#cook
cook <- cooks.distance(mod)
n <- length(cook)
limite <- 4 / n
cat(
  "Número de observações influentes:",
  sum(cook>limite)
)

cook_graph <- ggplot(
  data.frame(obs = 1:n, cook),
  aes(x = obs, y = cook)
) +
  geom_col(fill = "firebrick", alpha = 0.8) +
  geom_hline(yintercept = limite, color = "black", linetype = "dashed", size = 0.7) +
  labs(
    title = "Análise de Pontos Influentes (Distância de Cook)",
    subtitle = paste("Linha tracejada indica o limite teórico de", round(limite, 4)),
    x = "Índice da Observação (Linha no Banco de Dados)",
    y = "Distância de Cook"
  ) +
  theme_minimal()
cook_graph

