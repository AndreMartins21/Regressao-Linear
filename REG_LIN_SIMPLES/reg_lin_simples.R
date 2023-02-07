"""
# Objetivo: 
  Verificar se o gasto em publicidade é capaz de prever a venda de CDs

# Referência:
  Scripts baseados no vídeo =  https://www.youtube.com/watch?v=E2bYIb81q4A
"""

remove(list = ls())  # Removendo todas as variáveis pré-salvas do R


#-------------------------------------------------------------------------------
# 1°) Carregando os pacotes
#-------------------------------------------------------------------------------
if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr)

# P.S: 
# O método  `p_load` do pacote `pacman` irá baixar e carregar todos os 
# pacotes passados como parâmetro. Isso POUPA alguns IF's / LIBRARY


#-------------------------------------------------------------------------------
# 2°) Carregando o Banco de Dados  
#-------------------------------------------------------------------------------
dados <- read.csv2('Banco de Dados 11.csv')
View(dados)

# Ver os tipos de cada variável em `dados`
glimpse(dados)


#-------------------------------------------------------------------------------
# 3°) Verificar os pressupostos para a reg. lin.
#-------------------------------------------------------------------------------
"""
3.1 => Rel. linear entre a VD e VI (Gráfico de Dispersão)
3.2 => Construção do Modelo Linear
3.3 => Análise gráfica do modelo gerado no 3.2
3.4 => Normalidade dos resíduos            [Pode ser feito no 3.3)]
3.5 => Outliers nos resíduos               [Pode ser feito no 3.3)]
3.6 => Independência dos Resíduos (Durbin-Watson)
3.7 => Teste para a homocedasticidade (Breusch-Pagan)  [Pode ser feito no 3.3)]
"""


### 3.1: Rel. linear entre a VD e VI (Gráfico de Dispersão)
# VD = Y = dados$Vendas  
# VI = X = dados$Publicidade

plot(dados$Publicidade, dados$Vendas)
 
### 3.2: Construção do modelo linear
mod <- lm(Vendas ~ Publicidade, data = dados)
mod


### 3.3: Análise gráfica do modelo gerado no 3.2
par(mfrow=c(2, 2))
plot(mod)

""" 
Consulta => https://data.library.virginia.edu/diagnostic-plots/

Explicação de cada gráfico oriundo do lm:

# G11: Residuals vs Fitted (Resíduos vs Valores_Previstos)
  - Linearidade 
    . Se a linha vermelha estiver bem centralizada e na horizontal, há rel. linear
  - Homocedasticidade (homogeneidade de variância)
    . Caso os pontos estejam bem distribuidos (formando um retângulo aprox.), o
         pressuposto de homocedasticidade está atendido.
    . Caso forme aprox. um triângulo, os pontos não estão bem distribuídos, portanto,
         não há homocedasticidade e sim heterocedasticidade.

# G112: Normal Q-Q 
  - Permite ver se os resíduos tem distribuição Normal
  - Eixo Y: Resíduos padronizados
  - Eixo X: Resíduos teóricos esperados, caso a distribuição fosse Normal
  
  - Para q os resíduos sigam uma Normal, os pontos devem estar aprox. distribuídos 
    em cima da reta pontilhada demarcada

# G21: Scale-Location
  - Mais recomendado pra garantir a HOMOCEDASTICIDADE
  - Para isso, a linha vermelha tem q ser aprox. horizontal e pontos dispersos
    formando um retângulo

# G22: Residuals vs Leverage
  - Outliers
    . Caso existe outliers, haverá resíduos abaixo de -3 OU acima de -3
        (R < -3) OU (R > 3)
  
  - Ponto de Alavancagem (PA)
    . Um sujeito experimental que está tão distante que influencia na estimação 
        do modelo. Seria um outlier considerável
    .  Caso tenha algum ponto fora da linha pontilhada vermelha, será um PA 

"""


### 3.4: Normalidade dos resíduos

# P.S: É a mesma análise feita no gráfico: "G12: Normal Q-Q" do plot(lm)
shapiro.test(mod$residuals)

# H0: Dist. dos Resíduos = Normal (p-value > 0.05)
# H1: Dist. dos Resíduos != Normal (p-value <= 0.05)


### 3.5: Outliers nos resíduos

# P.S: É a mesma análise feita no gráfico: "G22: Residuals vs Leverage"
res.padron <-  rstandard(mod)

# NÃO há outliers caso n esteja fugindo do intervalo 3 e -3
summary(res.padron)



### 3.6: Independência dos Resíduos (Durbin-Watson)

# P.S: Vem da library `car`
durbinWatsonTest(mod)

# Análises para que haja independência:
# 1°: A Statistic de DW deve estar prox. de 2 (entre 1 e 3)
# 2°: p-valor > 0.05



### 3.7: Teste para a homocedasticidade (Breusch-Pagan)

# P.S: É a mesma análise feita nos gráficos: "G11: Residuals vs Fitted" e "G21: Scale-Location"
bptest(mod)

# H0: Há homocedasticidade (p-value > 0.05)
# H1: NÃO há homocedasticidade (p-value <= 0.05) 


#-------------------------------------------------------------------------------
# 4°) Análise do modelo
#-------------------------------------------------------------------------------
summary(mod)

"""
<> Interpretações:

# Estimative (Intercepto):
  - Valor: 125.1795
  - Se eu NÃO investir em publicidade, o valor esperado da venda é de R$ 125.17
  
# Estimative (Publicidade):
  - Valor: 0.10495
  - A cada R$ 1 gasto em publicidade, aumenta R$ 0.10495 em vendas (média)

# Pr(>|t|) OU p-value:
  - Baseado no teste t, e tem como hipóteses:
    . H0: Coeficiente = 0 (p-value > 0.05)   => X não tem impacto em Y
    . H1: Coeficiente != 0 (p-value <= 0.05) 
  
# R²:
 - Valor: 0.3634
 - O investimento em Publicidade explica 36,34% das vendas

# F-statistic:
  - Compara o valor estimado com um modelo nulo | sem previsor algum (venda sempre na média)
    . H0: O modelo criado prevê tão bem quanto esse modelo nulo   (p-value > 0.05)
    . H1: Há diferença entre esses modelos (p-value <= 0.05)

"""

#-------------------------------------------------------------------------------
# 5°) Gráfico de dispersão com GGPLOT
#-------------------------------------------------------------------------------
par(mfrow=c(1, 1))


# Gráfico de dispersão sem muitos detalhes:
g_normal = ggplot(data = dados, mapping = aes(x = Publicidade, y = Vendas)) + 
  geom_point() + 
  theme_classic()

g_normal


# Adicionando a linha da Regressão
g_line = g_normal + 
  ggplot2::geom_smooth(method = "lm", col = "red") 

g_line


# Adicionando fórmula da Regressão Linear e R²:
graph <- g_line +
  ggpubr::stat_regline_equation(
    aes(
      label = paste(..eq.label.., ..adj.rr.label..,
      sep = "*plain(\",\")~~")
    ),
    label.x = 0.05, label.y = 400,
    parse = TRUE, coef.digits = 5
  ) 

graph  