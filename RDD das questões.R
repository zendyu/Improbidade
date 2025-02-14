#Aqui iremos rodar as regressões lineares do problema:

rm(list = ls())
gc()

library(dplyr)
library(tidyr)
library(rdrobust)
library(ggplot2)
library(readxl)
library(readr)
library(stringr)
library(rdd)
library(lubridate)
library(rddtools)
library(stargazer)
library(flextable)
library(rddensity)

#Carregando as 3 bases de dados já anonimizadas:

BaseTJSP <- read_excel("C:/Users/José Luiz/OneDrive/Documentos/BMAC/TCC/Base de Dados/BDacórdão.xlsx")

BaseTJSP2 <- read_excel("C:/Users/José Luiz/OneDrive/Documentos/BMAC/TCC/Base de Dados/BDSegGeral.xlsx")

BasePrimeira <- read_excel("C:/Users/José Luiz/OneDrive/Documentos/BMAC/TCC/Base de Dados/BDprimeira_instância.xlsx")



## 1. Julgamento de Recursos:


# Particionamento e cálculo das médias
#a base e a grande dúvida é saber o tamanho dos bins, para ter estatísticas relevantes
#
#Aqui ao invés de chutarmos os valores, fizemos algumas iterações e armazenamos na tabela para saber qual seria
#o valor mais significativo estatisticamente, e dentre esses o que possui uma melhor explicação atrelada ao 
#comportamento dos tribunais

#Criando um subset para o rodar o modelo: (Somente os casos mais simples 1 recurso e 1 decisão)

novo_df <- BaseTJSP

novo_df <- novo_df %>%
  filter(`Situação do Provimento` %in% c('Provimento', 'Não-Provimento', 
                                         'Provimento em Parte'))

novo_df <- novo_df %>%
  mutate(`Situação do Provimento` = ifelse(`Situação do Provimento` == "Provimento em Parte", "Provimento", `Situação do Provimento`))

novo_df <- novo_df %>%
  filter(novo_df$`Ação Classe do PG` %in% c('64 - Ação Civil de Improbidade Administrativa', '65 - Ação Civil Pública Cível'))

novo_df <- novo_df %>%
  filter(`Assunto Principal` %in% c('10011-Improbidade Administrativa'))

novo_df$Ano <- format(novo_df$`Data_Acórdão`, "%Y")
novo_df$Mes <- format(novo_df$`Data_Acórdão`, "%m")

#Ajustando as colunas para que possamos utilizar o RDD

novo_df$`Data_Acórdão` <- as.Date(novo_df$`Data_Acórdão`)
novo_df$`Data_Acórdão` <- floor_date(novo_df$`Data_Acórdão`, "day")
data_corte <- as.Date("2021-10-25")
novo_df$tempo <- as.numeric(novo_df$`Data_Acórdão` - data_corte)
novo_df$Entrada <- as.Date(novo_df$Entrada)
novo_df$Entrada <- floor_date(novo_df$Entrada, "day")
novo_df$temporecurso <- as.numeric(novo_df$Entrada- data_corte)



#Regressão logistica na base toda
novo_df$y <- ifelse(novo_df$`Situação do Provimento` == "Provimento", 1, 0) 
novo_df$pos_lei <- ifelse(novo_df$tempo > 0, 1, 0)
modelo_logit<- glm(novo_df$y ~ novo_df$tempo*novo_df$pos_lei + novo_df$Câmara_Julgadora + novo_df$Recorrente*novo_df$pos_lei, family = binomial)
summary(modelo_logit)

novo_df$pred_prob <- predict(modelo_logit, type = "response")


# Calcular a probabilidade predita em toda a base de dados
novo_df$pred_prob <- predict(modelo_logit, newdata = novo_df, type = "response")

# Calcular a média das probabilidades preditas agrupadas por tempo e pos_lei
media_prob <- novo_df %>%
  group_by(tempo, pos_lei) %>%
  summarise(media_pred_prob = mean(pred_prob))

# Plotar as médias das probabilidades preditas contra o tempo
ggplot(media_prob, aes(x = tempo, y = media_pred_prob, color = as.factor(pos_lei))) +
  geom_line() +
  labs(title = "Média das Probabilidades Preditas vs Tempo",
       x = "Tempo",
       y = "Média das Probabilidades Preditas") +
  theme_minimal()


ggplot(novo_df, aes(x = novo_df$tempo, y = novo_df$y)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_prob, color = as.factor(pos_lei))) +
  labs(title = "Regressão Logística Descontínua", x = "Variável de Corte", y = "Probabilidade Predita") +
  theme_minimal()



#### PARTICULARES #######

bin_size <- 30
novo_df$bin <- floor(novo_df$tempo / bin_size)

#tentando agrupar por câmara para efetuar o RDD com covariada

novo_df1 <- novo_df %>%
  filter(novo_df$`Data_Acórdão` >= '2014-10-25' & novo_df$`Data_Acórdão` < '2024-10-25')

dados_Percentual <- novo_df1 %>%
  group_by(`Câmara_Julgadora`, bin, `Recorrente`, `Situação do Provimento`) %>%
  summarise(contagem = n()) %>%
  mutate(Percentual = contagem / sum(contagem) * 100)



dados_4 <- dados_Percentual %>%
  filter(`Recorrente`== 'Particular')

ggplot(dados_4, aes(x = interaction(`Situação do Provimento`, bin), y = Percentual, fill=`Situação do Provimento`)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~`Câmara_Julgadora`) +
  labs(x = "Ano e Parte", y = "Percentual", fill = "Resultado Útil") +
  ggtitle("Gráficos de Resultados Úteis por Turma")


dados_particulares <- subset(dados_Percentual, dados_Percentual$Recorrente == "Particular")


dados_agrupados_part <- subset(dados_particulares, dados_particulares$`Situação do Provimento` == 'Provimento')
dados_agrupados_part$pos_lei <- ifelse(dados_agrupados_part$bin > 0, 1, 0)




#regressão básica:
modelo_part <- lm(dados_agrupados_part$Percentual ~ dados_agrupados_part$pos_lei + dados_agrupados_part$bin + 
                    dados_agrupados_part$pos_lei*dados_agrupados_part$bin + dados_agrupados_part$Câmara_Julgadora, data = dados_agrupados_part)
summary(modelo_part)
stargazer(modelo_part, type = 'text')

#Rodando as regressões descontínuas:


rdd_object <- rdd_data(y = dados_agrupados_part$Percentual, x = dados_agrupados_part$bin, cutpoint = 0, covar = 
                         dados_agrupados_part$Câmara_Julgadora)
summary(rdd_object)
resultado <- rdd_reg_lm(rdd_object)
stargazer(resultado, type = 'text')
stargazer(resultado, type = 'text', omit = c('x', 'x_right'),  column.labels = 'Percentual de provimentos', row.names = c('D = Pós-lei ; Constant = Média antes da lei'))
plot(rdd_object)

rdd_object <- rdd_data(y = dados_agrupados_part$Percentual, x = dados_agrupados_part$bin, cutpoint = 0)
resultado <- rdd_reg_lm(rdd_object)
stargazer(resultado, type = 'text')

summary(rdrobust(y=dados_agrupados_part$Percentual, x=dados_agrupados_part$bin, c=0, p=1,
                 masspoints = 'off'))

rd_plot1 <-  rdplot(y=dados_agrupados_part$Percentual, x=dados_agrupados_part$bin, c=0, p=1,
                    masspoints = 'off',
                    x.label = paste("Períodos de grupo de", bin_size, " dias"),
                    y.label = "Percentual de Provimentos nos recursos dos réus",
                    title = "Diferença percentual de provimentos - Particulares")

rdplot(y = dados_particulares$Percentual, x = dados_particulares$bin, p=1, c=0, masspoints = 'off',
       subset = dados_particulares$`Situação do Provimento` == 'Provimento',
       x.label = paste("Períodos de grupo de", bin_size, " dias"),
       y.label = "Percentual de Provimentos nas ações",
       title = "Diferença em percentual de provimentos em recursos de - Particulares")


png(filename = "rdplot_grafico.png", res = 500, width = 6, height = 5, units = "in") 
plot(rd_plot1$rdplot) 
dev.off()



DCdensity(dados_particulares$bin, cutpoint = 0)
density <- rddensity(dados_particulares$bin, c = 0, massPoints = FALSE)
rdplotdensity(density, dados_particulares$bin)
summary(density)


#Regressão Logística

novo_df4 <- subset(novo_df1, novo_df1$Recorrente == 'Particular')
novo_df4$y <- ifelse(novo_df4$`Situação do Provimento` == "Provimento", 1, 0) 
novo_df4$pos_lei <- ifelse(novo_df4$tempo > 0, 1, 0)
modelo_logit<- glm(novo_df4$y ~ novo_df4$tempo*novo_df4$pos_lei + novo_df4$Câmara_Julgadora, family = binomial)
summary(modelo_logit)

novo_df4$pred_prob <- predict(modelo_logit, type = "response")

ggplot(novo_df4, aes(x = novo_df4$tempo, y = novo_df4$y)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_prob, color = as.factor(pos_lei))) +
  labs(title = "Regressão Logística Descontínua", x = "Variável de Corte", y = "Probabilidade Predita") +
  theme_minimal()


####### MINISTÉRIO PÚBLICO #########

novo_df2 <- novo_df %>%
  filter(novo_df$`Data_Acórdão` >= '2014-10-25' & novo_df$`Data_Acórdão` < '2024-10-25')

bin_size <- 31
novo_df$bin <- floor(novo_df$tempo / bin_size)

#tentando agrupar por câmara para efetuar o RDD com covariada

novo_df2 <- novo_df %>%
  filter(novo_df$`Data_Acórdão` >= '2014-10-25' & novo_df$`Data_Acórdão` < '2024-10-25')

dados_Percentual <- novo_df %>%
  group_by(`Câmara_Julgadora`, bin, `Recorrente`, `Situação do Provimento`) %>%
  summarise(contagem = n()) %>%
  mutate(Percentual = contagem / sum(contagem) * 100)



dados_4 <- dados_Percentual %>%
  filter(`Recorrente`== 'Ministério Público')

ggplot(dados_4, aes(x = interaction(`Situação do Provimento`, bin), y = Percentual, fill=`Situação do Provimento`)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~`Câmara_Julgadora`) +
  labs(x = "Ano e Parte", y = "Percentual", fill = "Resultado Útil") +
  ggtitle("Gráficos de Resultados Úteis por Turma")


dados_MP <- subset(dados_Percentual, dados_Percentual$Recorrente == "Ministério Público")


dados_agrupados_MP <- subset(dados_MP, dados_MP$`Situação do Provimento` == 'Não-Provimento')
dados_agrupados_MP$pos_lei <- ifelse(dados_agrupados_MP$bin > 0, 1, 0)




#regressão básica:
modelo_part <- lm(dados_agrupados_MP$Percentual ~ dados_agrupados_MP$pos_lei + dados_agrupados_MP$bin + 
                    dados_agrupados_MP$pos_lei*dados_agrupados_MP$bin + dados_agrupados_MP$Câmara_Julgadora, data = dados_agrupados_MP)
stargazer(modelo_part, type = 'text')

#Rodando as regressões descontínuas:


rdd_object <- rdd_data(y = dados_agrupados_MP$Percentual, x = dados_agrupados_MP$bin, cutpoint = 0, covar = 
                         dados_agrupados_MP$Câmara_Julgadora)
resultado <- rdd_reg_lm(rdd_object, order = 2)
summary(resultado)
stargazer(resultado, type = 'text')
plot(rdd_object)


stargazer(resultado, type = 'text', omit = c('x', 'x_right'),  column.labels = 'Percentual de Não-provimentos', row.names = c('D = Pós-lei ; Constant = Média antes da lei'))

rdplot(y=dados_agrupados_MP$Percentual, x=dados_agrupados_MP$bin, c=0, p=2,
       masspoints = 'off')

summary(rdrobust(y=dados_agrupados_MP$Percentual, x=dados_agrupados_MP$bin, c=0, p=2,
                 masspoints = 'off'))

rd_plot2<-rdplot(y = dados_MP$Percentual, x = dados_MP$bin, p=2, c=0,
                 subset = dados_MP$`Situação do Provimento` == 'Não-Provimento', masspoints = 'off',
                 x.label = paste("Períodos de grupo de 31 dias"),
                 y.label = "% de Não-Provimentos nos Recursos do MP",
                 title = "Diferença em percentual de não-provimentos em recursos de - MP")

png(filename = "rdplotmp_grafico.png", res = 500, width = 6, height = 5, units = "in") 
plot(rd_plot2$rdplot) 
dev.off()


rdplot(y = dados_MP$Percentual, x = dados_MP$bin, p=2,
       subset = dados_MP$`Situação do Provimento` == 'Não-Provimento',
       x.label = paste("Períodos mensal de agrupamento"),
       y.label = "Percentual de Não-Provimentos nos recursos")


DCdensity(dados_MP$bin, cutpoint = 0)
density <- rddensity(dados_MP$bin, c = 0, p=2, massPoints = FALSE)
rdplotdensity(density, dados_MP$bin)
summary(density)



### Rodando um regressão logística

modelo_logit<- glm(dados_agrupados_MP$Percentual ~ dados_agrupados_MP$bin*dados_agrupados_MP$pos_lei + dados_agrupados_MP$Câmara_Julgadora)
summary(modelo_logit)

dados_agrupados_MP$pred_prob <- predict(modelo_logit, type = "response")

ggplot(dados_agrupados_MP, aes(x = dados_agrupados_MP$bin, y = dados_agrupados_MP$Percentual)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_prob, color = as.factor(pos_lei))) +
  labs(title = "Regressão Logística Descontínua", x = "Variável de Corte", y = "Probabilidade Predita") +
  theme_minimal()


novo_df3 <- subset(novo_df2, novo_df2$Recorrente == 'Ministério Público')
novo_df3$y <- ifelse(novo_df3$`Situação do Provimento` == "Não-Provimento", 1, 0) 
novo_df3$pos_lei <- ifelse(novo_df3$tempo > 0, 1, 0)
modelo_logit<- glm(novo_df3$y ~ novo_df3$tempo*novo_df3$pos_lei + novo_df3$Câmara_Julgadora, family = binomial())
summary(modelo_logit)

novo_df3$pred_prob <- predict(modelo_logit, type = "response")

ggplot(novo_df3, aes(x = novo_df3$tempo, y = novo_df3$y)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_prob, color = as.factor(pos_lei))) +
  labs(title = "Regressão Logística Descontínua", x = "Variável de Corte", y = "Probabilidade Predita") +
  theme_minimal()



#2.Analisando a base de dados da primeira instância:
  
  #Agora iniciaremos analisando a segunda base de dados na qual temos os processos
  #em primeira instância e iremos verificar quantas novas ações foram propostas após a 
  #alteração na LIA

primeira_df <- BasePrimeira

primeira_df <- BasePrimeira %>%
  filter(`Assunto` %in% c('Improbidade Administrativa'))

primeira_df <- primeira_df %>%
  filter(primeira_df$Entrada >= '2014-01-01')

primeira_df <- subset.data.frame(primeira_df, primeira_df$`Principal Parte Ativa`!= 'Particular')

primeira_df$Ano <- format(primeira_df$Entrada, "%Y")
primeira_df$Mes <- format(primeira_df$Entrada, "%m")

primeira_df <- primeira_df %>%
  filter(primeira_df$Entrada >= '2015-05-01' & primeira_df$Entrada < '2024-01-01')


#nesta segunda parte rodaremos inicialmente algumas regressões

primeira_df$Entrada <- as.Date(primeira_df$Entrada)
primeira_df$Entrada <- floor_date(primeira_df$Entrada, "day")
data_corte <- as.Date("2021-10-25")
primeira_df$tempo <- as.numeric(primeira_df$Entrada - data_corte)

primeira_df <- primeira_df %>%
  filter(primeira_df$Entrada >= '2015-05-01' & primeira_df$Entrada < '2024-01-01')


bin_size <- 15
primeira_df$bin <- floor(primeira_df$tempo / bin_size)

dados_primeira2 <- primeira_df %>%
  group_by(bin, `Principal Parte Ativa`) %>%
  summarise(Contagem = n())

total_por_bin <- dados_primeira2 %>%
  group_by(bin) %>%
  summarise(Total = sum(Contagem))

dados_MP_prim <- subset(dados_primeira2, dados_primeira2$`Principal Parte Ativa` == 'Ministério Público')

dados_MP_prim <- merge(dados_MP_prim, total_por_bin, by = c("bin"))

dados_MP_prim$Percentual <- (dados_MP_prim$Contagem / dados_MP_prim$Total) * 100


ggplot(dados_MP_prim, aes(x = interaction(bin, sep = "-"), y = Percentual,)) +
  geom_line() +
  geom_point() +
  labs(x = "Mês-Ano", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


dados_MP_prim$pos_lei <- ifelse(dados_MP_prim$bin > 0, 1, 0)

#Rodando regressão linear:

modelo_MP <- lm(dados_MP_prim$Percentual ~ dados_MP_prim$pos_lei, data = dados_MP_prim)
stargazer(modelo_MP, type = 'text')


#Rodando as regressões descontínuas:


rdd_object <- rdd_data(y = dados_MP_prim$Percentual, x = dados_MP_prim$bin, cutpoint = 0)
resultado <- rdd_reg_lm(rdd_object)

summary(resultado)
plot(rdd_object)

stargazer(resultado, type = 'text')
stargazer(resultado, type = 'text', omit = c('x', 'x_right'),  column.labels = 'Percentual de provimentos', row.names = c('D = Pós-lei ; Constant = Média antes da lei'))
plot(rdd_object)

rdd_object <- rdd_data(y = dados_MP_prim$Percentual, x = dados_MP_prim$bin, cutpoint = 0)
resultado <- rdd_reg_lm(rdd_object)
stargazer(resultado, type = 'text')

summary(rdrobust(y=dados_MP_prim$Percentual, x=dados_MP_prim$bin, c=0, p=1,
                 masspoints = 'off'))

rd_plot2 <-  rdplot(y=dados_MP_prim$Percentual, x=dados_MP_prim$bin, c=0, p=1,
                    x.label = paste("Períodos de grupo de", bin_size, " dias"),
                    y.label = "% de Ações propostas pelo Ministério Público",
                    title = "Diferença em percentual de ações do MP após a alteração legal")



png(filename = "rdplot2_grafico.png", res = 500, width = 6, height = 5, units = "in") 
plot(rd_plot2$rdplot) 
dev.off()


