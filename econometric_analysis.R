getwd()
dane0 <- read.csv("insurance.csv")
head(dane0)
summary(dane0)
#View(dane)
library(tidyverse)
library(ggplot2)
library(lmtest)
library(moments)
library(fastDummies)
library(corrplot)
library(car)
#library(xtable)

# Statystyki opisowe dla zmiennych numerycznych
num_vars <- sapply(dane0, is.numeric)
opisowe <- data.frame(
  Zmienna = names(dane0)[num_vars],
  Srednia = sapply(dane0[, num_vars], mean),
  Mediana = sapply(dane0[, num_vars], median),
  SD = sapply(dane0[, num_vars], sd),
  Min = sapply(dane0[, num_vars], min),
  Max = sapply(dane0[, num_vars], max),
  Skosnosc = sapply(dane0[, num_vars], skewness),
  Kurtoza = sapply(dane0[, num_vars], kurtosis)
)

print(opisowe)

# Tworzenie histogramów
hist(dane0$age, breaks = seq(15, 65, by = 5))
hist(dane0$bmi)
hist(dane0$children, breaks = seq(-0.5, max(dane0$children) + 0.5, by = 1))
hist(dane0$charges)

plot(dane0$bmi, dane0$charges)


#zmodyfikowana funkcja hellwiga
hellwig <- function(data) {  # zakładamy, że y w pierwszej kolumnie
  m <- ncol(data) - 1
  cor_mat <- cor(data)
  R0 <- cor_mat[1, -1]
  R <- abs(cor_mat[-1, -1])
  comb <- expand.grid(rep(list(c(TRUE, FALSE)), m))
  comb <- comb[rowSums(comb) > 0, ]
  
  results <- data.frame(H = numeric(nrow(comb)), variables = character(nrow(comb)), stringsAsFactors = FALSE)
  var_names <- names(R0)
  
  for(i in 1:nrow(comb)){
    k <- which(unlist(comb[i, ]))
    H <- 0
    for(j in k){
      H <- H + R0[j]^2 / sum(R[j, k])
    }
    results$H[i] <- H
    results$variables[i] <- paste(var_names[k], collapse = ", ")
  }
  
  return(results[order(-results$H), ])  # wyniki posortowane malejąco według H
}



#przekształcenie zmiennych kategorycznych na 0 i 1
dane0 <- dummy_cols(dane0, select_columns = c("sex", "smoker", "region"),
                         remove_first_dummy = TRUE,
                         remove_selected_columns = TRUE)

#charges jako pierwsza kolummna
if ("charges" %in% names(dane0) && names(dane0)[1] != "charges") {
  dane0 <- dane0[, c("charges", setdiff(names(dane0), "charges"))]
}

corr <- cor(dane0)
corrplot(corr)

dane0$obesity <- ifelse(dane0$bmi>30,1,0)

#wybor zmiennych hellwig
head(hellwig(dane0))
dane <- dane0[, c("charges", "children", "age", "smoker_yes", "obesity")]


#podział na zbiór testowy i treningowy
set.seed(123)
train <- dane |>
  slice_sample(prop = 0.8)
test <- anti_join(dane, train)

#pierwsza postac modelu
model <- lm(charges ~ children + age + obesity + smoker_yes, data = train)

summary(model)
plot(model)


#test Chowa
#H0: brak zmian strukturalnych
#H1: występują zmiany strukturalne

train <- train |>
  arrange(train$smoker_yes)
#sum(train1$smoker_yes)
n <- sum(train$smoker_yes==0)

train1 <- train[1:n,]
train2 <- train[(n+1):1070,]


train1 <- train1 |>
  select(-smoker_yes)

train2 <- train2 |>
  select(-smoker_yes)


model1 <- lm(charges ~ ., data=train1)
model2 <- lm(charges ~ ., data=train2)

SC <- sum((model$residuals)^2)
S1 <- sum((model1$residuals)^2)
S2 <- sum((model2$residuals)^2)
k <- 4
N1 <- 841
N2 <- 229


F <- ((SC-(S1+S2))*(N1+N2-2*k))/((S1+S2)*k)
F

1-pf(F, k, (N1+N2-2*k)) #pvalue ok 0 -> dokladniej bedzie podzielic 

#dobor zmiennych do nowych modeli (po podzielu)
head(hellwig(train1))
head(hellwig(train2))

#niepalacy
model1 <- lm(charges ~ age + children, data=train1)
resettest(model1)
model1 <- lm(charges ~ poly(age, 2)  + children, data=train1)
resettest(model1)
summary(model1)
plot(model1)



#palacy
model2 <- lm(charges ~ age + obesity, data = train2)
resettest(model2)
summary(model2)
plot(model2)


#vif - współliniowość

vif(model)
#1 < VIF ≤ 5	Umiarkowana korelacja (umiarkowana współliniowość)
vif(model1)
vif(model2)

#autokorelacja - chcemy aby korelacja miedzy zmiennymi wynosila 0
library(lmtest)

#test durbin watson
#H0: r=0, H1: r>0
dwtest(model)
dwtest(model1)
dwtest(model2)

#brak autokorelacji

#heteroskedastyczność
#test Breuscha-Pagana - H0 - mamy homoskedastyczność, h1 - heteroskedastycznosc
bptest(model)
bptest(model1)
bptest(model2)



mean(model1$residuals)
mean(model2$residuals)


#normalnosc reszt
plot(density(model$residuals))
shapiro.test(model$residuals) #nasze reszty nie maja rozkladu normalnego
shapiro.test(model1$residuals) #nasze reszty nie maja rozkladu normalnego
shapiro.test(model2$residuals) #nasze reszty nie maja rozkladu normalnego
plot(density(model1$residuals))
plot(density(model2$residuals))




#bootstrap na podstawie błędów losowych (dla niepalących)
R <- 10000
e1 <- model1$residuals

X <- model.matrix(model1)
beta_est <- coef(model1)
beta_gw <- matrix(NA, nrow = R, ncol = length(beta_est))
colnames(beta_gw) <- names(beta_est)

for(i in 1:R){
  e_gw <- sample(e1, length(e1), replace = TRUE)
  y_gw <- X %*% beta_est + e_gw
  model_gw <- lm(y_gw ~ X - 1)    
  beta_gw[i, ] <- coef(model_gw)
}

df1 <- data.frame(coef(model1), apply(beta_gw, 2, mean),confint(model1), t(apply(beta_gw, 2, quantile, probs = c(0.025, 0.975))))
colnames(df1) <- c("Beta_est", "Beta_est_gw" ,"Klasyczny PU 2,5%", "Klasyczny PU 97,5%", "Bootstrap PU 2,5%", "Bootstrap PU 97,5%")
#xtable(df1, digits = 2)
print(df1)


#bootstrap na podstawie błędów losowych (dla palących)

R <- 10000
e2 <- model2$residuals


shapiro.test(e2)
X <- model.matrix(model2)
beta_est <- coef(model2)
beta_gw <- matrix(NA, nrow = R, ncol = length(beta_est))
colnames(beta_gw) <- names(beta_est)

for(i in 1:R){
  e_gw <- sample(e2, length(e2), replace = TRUE)
  y_gw <- X %*% beta_est + e_gw
  model_gw <- lm(y_gw ~ X - 1)    
  beta_gw[i, ] <- coef(model_gw)
}

df2 <- data.frame(coef(model2), apply(beta_gw, 2, mean),confint(model2), t(apply(beta_gw, 2, quantile, probs = c(0.025, 0.975))))
colnames(df2) <- c("Beta_est", "Beta_est_gw" ,"Klasyczny PU 2,5%", "Klasyczny PU 97,5%", "Bootstrap PU 2,5%", "Bootstrap PU 97,5%")
#xtable(df2, digits = 2)
print(df2)


#R^2 treningowe dla całości

train2$pred <- predict(model2, train2)
train1$pred <- predict(model1, train1)

charges <- c(train1$charges, train2$charges)
pred <- c(train1$pred, train2$pred)

SSE <- sum((charges - pred)^2)
SST <- sum((charges - mean(charges))^2)

R2_test <- 1 - SSE / SST
print(R2_test)



#predykcje na zbiorze testowym
test <- test |>
  arrange(smoker_yes) 

test1 <- test[1:222,]
test2 <- test[223:267,]
#summary(test2)
#sum(test$smoker_yes)
#test2$obesity <- ifelse(test2$bmi>30, 1, 0)

test2$pred <- predict(model2, test2)

test1$pred <- predict(model1, test1)

#R^2 dla niepalących
SSE <- sum((test1$charges - test1$pred)^2) 
SST <- sum((test1$charges - mean(test1$charges))^2)
R2_test <- 1 - SSE/SST
print(R2_test)

#R^2 dla palących
SSE <- sum((test2$charges - test2$pred)^2)
SST <- sum((test2$charges - mean(test2$charges))^2)
R2_test <- 1 - SSE/SST
print(R2_test)

#R^2 dla całości
charges <- c(test1$charges, test2$charges)
pred <- c(test1$pred, test2$pred)

SSE <- sum((charges - pred)^2)
SST <- sum((charges - mean(charges))^2)

R2_test <- 1 - SSE / SST
print(R2_test)


#wykresy 
ggplot(test1, aes(x = pred, y = charges)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred), color = "red") +
  labs(title = "Predykcja vs Rzeczywiste charges")

ggplot(test2, aes(x = pred, y = charges)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred), color = "red") +
  labs(title = "Predykcja vs Rzeczywiste charges")




#błędy
error1 <- test1$charges - test1$pred
MAE = mean(abs(error1)) 
MAPE = mean(abs(error1/test1$charges))
RMSE = sqrt(mean(error1^2))
MAE
MAPE
RMSE

error2 <- test2$charges - test2$pred
MAE = mean(abs(error2)) 
MAPE = mean(abs(error2/test2$charges))
RMSE = sqrt(mean(error2^2))
MAE
MAPE
RMSE


error <- charges - pred
MAE <- mean(abs(error))
MAPE <- mean(abs(error / charges))
RMSE <- sqrt(mean(error^2))
MAE
MAPE
RMSE


