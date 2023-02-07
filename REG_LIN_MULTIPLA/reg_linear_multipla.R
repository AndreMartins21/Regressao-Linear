"""
O `Banco de dados 12.csv` contém as notas de 200 alunos. 
# Objetivo:
  - Checar se o tempo de revisão e o tempo de sono pré-prova são capazes de 
  prever a nota do aluno
"""

# 1°: Carregando os pacotes necessários
if(!require(pacman)) install.packages("pacman")

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr, 
               QuantPsyc, psych, scatterplot3d)

# 2°: Carregando o banco de dados
setwd("/home/andre_pessoal/Documents/PROGRAMMING/R/Regressao_Linear/REG_LIN_MULTIPLA/Exemplo I")

dados <- read.csv2('Banco de Dados 12.csv')
View(dados)           
dplyr::glimpse(dados)  # Resumo dos dados

# 3°: Construindo o MODELO DE REG. LINEAR
mod <- lm(Notas~Tempo_Rev + Tempo_Sono, data = dados)
mod

# 4°: Análise gráfica
par(mfrow=c(2, 2))
plot(mod)

"""
P.S:
   - Há linearidade (Residulas vs Fitted)
   - Os resíduos seguem uma distribuição Normal (Normal Q-Q)
   - Há homocedasticidade (Scale-Location)
   - Não há outliers e nem pontos influentes (Residuals vs Leverage)
       . Os pontos estão indo de -3 a 3
"""

par(mfrow=c(1, 1))


# 4°: Fazendo testes dos pressupostos por código (sem gráfico)
# P.S: 
#   "4.5: Ausência de Multicolinearidade" NÃO HÁ em Reg. Lin. Simples


## 4.1: Normalidade dos resíduos
shapiro.test(mod$residuals)  # p > 0.05 => Distrib. Normal

## 4.2: Outliers nos resíduos
summary(stats::rstandard(mod))

## 4.3: Independência dos resíduos
car::durbinWatsonTest(mod)  # p > 0.05 => Não HÁ autocorrelação

## 4.4: Teste de homocedasticidade (Breusch-Pagan)
lmtest::bptest(mod)  # p > 0.05 => Há homocedasticidade

## 4.5: Ausência de Multicolinearidade
"""
Checa se há uma correlação alta entre as variáveis independentes.
- Caso tenha, haverá um impacto na estimação do coeficiente, gerando um coef. 
incoerente, dado que há mt correção entre as var. indep.
 
Multicolinearidade: r > 0.9 (ou 0.8)
"""
psych::pairs.panels(dados)  # Ignorar Notas (analisar somente VAR. DEP.)

"""
Outra forma de analisar a multicolinearidade é o método `vif` do pacote `car`:
# Multicolinearidade:  VIF > 10
"""
car::vif(mod)



#-----------------------------------
# Criação do 2° Modelo
#-----------------------------------
mod2 <- lm(Notas ~ Tempo_Rev, data = dados)
mod2


# 5°: Análise do 2° modelo e comparando-o com o 1°
summary(mod)   # R²adj = 0.401
summary(mod2)  # R²adj = 0.355






