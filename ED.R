x <- read.csv("leite.txt", 
              sep = "\t", 
              fileEncoding = "latin1")
head(x)
View(x)
str(x)
names(x)
summary(x)

# Remoção de colunas
x <- x[, c("Tempo", "Total")]
View(x)

# Medidas de tendência central
mean(x$Total)
median(x$Total)

# Identificação da assimetria dos dados

simetria_def <- function(dados){
  media <- mean(dados)
  mediana <- median(dados)
  
  if (abs(media - mediana) < 0.01) {
    cat("A distribuição é SIMÉTRICA\n")
    return("simétrica")
  } else {
    cat("A distribuição é ASSIMÉTRICA\n")
    return("assimétrica")
  }
}
simetria_def(x$Total)

# Cálculo dos quantis
quantile(x$Total)

quantile(x$Total, probs = c(0.10, 0.25, 0.75, 0.90))

C10 = quantile(x$Total, 0.10)
Q1  = quantile(x$Total, 0.25)
Q3  = quantile(x$Total, 0.75)
C90 = quantile(x$Total, 0.90)

# Medidas de dispersão

# Amplitude total
AT <- max(x$Total) - min(x$Total)

# Variância

var <- var(x$Total)
var

# Desvio padrão

DesvPad <- sqrt(var(x$Total))
DesvPad
# ou 
sd(x$Total)

# Desvio interquartílico
DQ <- Q3 - Q1
DQ
# ou
IQR(x$Total)

# Coeficiente de variação

cv <- (sd(x$Total) / mean(x$Total)) * 100
cv

# Classificação da dispersão

classcv <- function(dados){
  cv <- (sd(dados) / mean(dados)) * 100
  if (cv < 15) {
    cat("Baixa dispersão\n")
    return("baixa dispersão")
  } else if (cv >= 15 && cv <= 30) {
    cat("Média dispersão\n")
    return("média dispersão")
  } else {
    cat("Alta dispersão\n")
    return("alta dispersão")
  }
}
classcv(x$Total)

# Propriedades da distribuição

# Coeficiente de assimetria de Pearson

ass_pearson <- 3 * (mean(x$Total) - median(x$Total)) / sd(x$Total)
ass_pearson


class_ass_pearson <- function(dados){
  ass <- 3 * (mean(dados) - median(dados)) / sd(dados)
  
  if (ass <= 0.15) {
    return("pode ser considerada simétrica")
  } else if (ass > 0.15 && ass <= 1) {
    return("moderadamente assimétrica")
  } else {
    return("acentuadamente assimétrica")
  }
}
class_ass_pearson(x$Total)


# Coeficiente de assimetria de Yule

ass_yule <-  (Q3 + Q1 - 2 * median(x$Total)) / (Q3 - Q1)
ass_yule


class_yule <- function(dados){
  Q1 <- quantile(dados, 0.25)
  Md <- median(dados)
  Q3 <- quantile(dados, 0.75)
  
  ayule <- (Q3 + Q1 - 2 * Md) / (Q3 - Q1)
  
  if (abs(ayule) < 0.05) {
    return("aproximadamente simétrica")
  } else if (ayule > 0) {
    return("assimetria positiva")
  } else {
    return("assimetria negativa")
  }
}
class_yule(x$Total)

# Coeficiente de assimetria de Kelley

ass_kelley <- (C90 + C10 - 2 * median(x$Total)) / (C90 - C10)
ass_kelley

class_kelley <- function(dados){
  ak <- (C90 + C10 - 2 * median(dados)) / (C90 - C10)
  
  if (abs(ak) < 0.05) {
    return("aproximadamente simétrica")
  } else if (abs(ak) > 0.95) {
    return("fortemente assimétrica")
  } else if (ak > 0) {
    return("assimetria positiva")
  } else {
    return("assimetria negativa")
  }
}
class_kelley(x$Total)

# Coeficiente percentílico de curtose
ak <- (Q3 - Q1) / (2 * (C90 - C10))
ak


class_curtose <- function(dados){
  ak <- (Q3 - Q1) / (2 * (C90 - C10))
  if (abs(ak - 0.263) < 0.001) {
    return("mesocúrtica")
  } else if (ak < 0.263) {
    return("leptocúrtica")
  } else {
    return("platicúrtica")
  }
}
class_curtose(x$Total)

# Gráficos 
boxplot(x$Total, 
        col="lightblue")

hist(x$Total, 
     probability = TRUE, 
     col = "lightgray")

plot(density(x$Total), 
     col="lightblue")

