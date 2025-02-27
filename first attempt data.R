rm(list = ls())
gc()

library(dplyr)
library(rdrobust)
library(ggplot2)
library(readxl)
library(readr)
library(stringr)
library(rdd)
library(lubridate)
library(rddtools)
library(flextable)
library(stargazer)
library(officer)
library(openxlsx)



BaseTJSP2 <- read_excel("C:/Users/José Luiz/OneDrive/Documentos/BMAC/TCC/Base de Dados/BDSegGeral.xlsx")
BaseTJSP <- read_excel("C:/Users/José Luiz/OneDrive/Documentos/BMAC/TCC/Base de Dados/BDacórdão.xlsx")
BasePrimeira <- read_excel("C:/Users/José Luiz/OneDrive/Documentos/BMAC/TCC/Base de Dados/BDprimeira_instância.xlsx")

#Ajuste das informações sensíveis:

BaseTJSP$`Entrada` <- as.Date(BaseTJSP$`Entrada`)
novo_df <- BaseTJSP %>%
  arrange(Entrada)

novo_df <- novo_df %>%
  mutate(Processo = sprintf("%07s-%s", row_number(), format(Entrada, "%Y")))

novo_df <- novo_df %>%
  mutate(`Principal Parte Ativa` = case_when(
    str_detect(`Principal Parte Ativa`, "Ministério") ~ "Ministério Público",
    str_detect(`Principal Parte Ativa`, "Prefeitura") | str_detect(`Principal Parte Ativa`, "Municipal") | str_detect(`Principal Parte Ativa`, "Município") | str_detect(`Principal Parte Ativa`, "Estadual") | str_detect(`Principal Parte Ativa`, "Estado") ~ "Ente Público",
    TRUE ~ "Particular"
  ))
novo_df <- novo_df %>%
  mutate(`Principal Parte Passiva` = case_when(
    str_detect(`Principal Parte Passiva`, "Ministério") ~ "Ministério Público",
    str_detect(`Principal Parte Ativa`, "Prefeitura") | str_detect(`Principal Parte Ativa`, "Municipal") | str_detect(`Principal Parte Ativa`, "Município") | str_detect(`Principal Parte Ativa`, "Estadual") | str_detect(`Principal Parte Ativa`, "Estado") ~ "Ente Público",
    TRUE ~ "Particular"
  ))

novo_df$`Data da Movimentação` <- as.Date(novo_df$`Data da Movimentação`)



exemplo <-novo_df %>%
  slice(100:104)


tabela<-flextable(exemplo)
tabela <- set_table_properties(tabela, width = 1, layout = "autofit")
tabela <- theme_vanilla(tabela)

print(tabela)

write.xlsx(tabela, file = "tabela.xlsx")

# Criação do documento Word
doc <- read_docx()

# Adiciona a tabela ao documento
doc <- body_add_flextable(doc, value = tabela)

# Salva o documento
print(doc, target = "tabela_apa.docx")


getwd()


#Criando um subset para o rodar o modelo: (Somente os casos mais simples 1 recurso e 1 decisão)

novo_df <- novo_df %>%
  filter(`Situação do Provimento` %in% c('Provimento', 'Não-Provimento', 
                                         'Provimento em Parte'))

novo_df <- novo_df %>%
  mutate(`Situação do Provimento` = ifelse(`Situação do Provimento` == "Provimento em Parte", "Provimento", `Situação do Provimento`))

novo_df <- novo_df %>%
  filter(novo_df$`Ação Classe do PG` %in% c('64 - Ação Civil de Improbidade Administrativa', '65 - Ação Civil Pública Cível'))

novo_df <- novo_df %>%
  filter(`Assunto Principal` %in% c('10011-Improbidade Administrativa'))


novo_df <- novo_df %>%
  filter(novo_df$`Data_Acórdão` >= '2014-01-01')

# Verificando os totais para ver como responder a primeira pergunta, já que poucas observações
# implicarão em dificuldade em rodar os testes estatísticos

totais_dplyr <- novo_df %>%
  group_by(novo_df$`Recorrente`) %>%
  count(novo_df$`Situação do Provimento`)
print(totais_dplyr)

totais_dplyr <- totais_dplyr %>%
  rename(Recorrente = `novo_df$\`Principal Parte Ativa\``, Situação_Provimento =`novo_df$\`Situação do Provimento\``, Totais = n)



novo_df$Ano <- format(novo_df$`Data_Acórdão`, "%Y")
novo_df$Mes <- format(novo_df$`Data_Acórdão`, "%m")
novo_df$Semestre <- as.numeric(cut(as.numeric(format(novo_df$`Data_Acórdão`, "%m")), breaks = c(0, 6, 12), labels = FALSE))
novo_df$Bimestre <- as.numeric(cut(as.numeric(format(novo_df$`Data_Acórdão`, "%m")), breaks = c(0, 2, 4, 6, 8, 10, 12), labels = FALSE))


#Criando gráficos de barras por câmaras:

dados_percentual <- novo_df %>%
  group_by(`Câmara_Julgadora`, Ano, `Recorrente`, `Situação do Provimento`) %>%
  summarise(contagem = n()) %>%
  mutate(percentual = contagem / sum(contagem) * 100)

dados_4 <- dados_percentual %>%
  filter(`Recorrente`== 'Particular')

ggplot(dados_4, aes(x = interaction(`Situação do Provimento`, Ano), y = percentual, fill=`Situação do Provimento`)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~`Câmara_Julgadora`) +
  labs(x = "Ano e Parte", y = "Percentual", fill = "Resultado Útil") +
  ggtitle("Gráficos de Resultados Úteis por Turma")



dados_câmara <- novo_df %>%
  group_by(Ano, novo_df$`Câmara_Julgadora`, novo_df$`Recorrente`) %>%
  summarise(Contagem = n())



#Total por ano, parte indiscriminada
dados_agrupados1 <- novo_df %>%
  group_by(Ano, `Recorrente`) %>%
  summarise(Contagem = n())

total_por_ano <- dados_agrupados1 %>%
  group_by(Ano) %>%
  summarise(Total = sum(Contagem))

dados_agrupados1 <- merge(dados_agrupados1, total_por_ano, by = c("Ano"))

dados_agrupados1$Percentual <- (dados_agrupados1$Contagem / dados_agrupados1$Total) * 100


ggplot(dados_agrupados1, aes(x = interaction(Ano, sep = "-"), y = Contagem, color = `Recorrente`)) +
  geom_line() +
  geom_point() +
  labs(x = "Mês-Ano", y = "Quantidade") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(dados_agrupados1, aes(x=Ano, y=Contagem, fill=`Recorrente`)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Contagem), position=position_dodge(width=0.9), vjust=-0.5) +
  labs(title="Quantidades de recursos por parte", x="Ano", y="Contagem") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

ggplot(dados_agrupados1, aes(x = interaction(Ano, sep = "-"), y = Percentual, fill = `Recorrente`)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=sprintf("%.1f%%", Percentual)), position=position_dodge(width=0.9), vjust=-0.5) +
  labs(title="Frequência Relativa de Recurso", x="Ano", y="Percentual") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

a<-ggplot(dados_agrupados1, aes(x = interaction(Ano, sep = "-"), y = Percentual, fill = `Recorrente`)) + geom_bar(stat="identity", position="dodge") + geom_text(aes(label=sprintf("%.1f%%", Percentual)), position=position_dodge(width=0.9), vjust=-0.5) + labs(title="Frequência Relativa de Recurso", x="Ano", y="Percentual") + theme_minimal() + theme(axis.text.x = element_text(angle=45, hjust=1)) + guides(fill = guide_legend(position = "bottom"))

ggsave(filename = "grafico.jpeg", plot = ggplot(dados_agrupados1, aes(x = interaction(Ano, sep = "-"), y = Percentual, fill = `Recorrente`)) + geom_bar(stat="identity", position="dodge") + geom_text(aes(label=sprintf("%.1f%%", Percentual)), position=position_dodge(width=0.9), vjust=-0.5) + labs(title="Frequência Relativa de Recurso", x="Ano", y="Percentual") + theme_minimal() + theme(axis.text.x = element_text(angle=45, hjust=1)) + guides(fill = guide_legend(position = "bottom")), dpi = 500, width = 6, height = 5)

  getwd()
  
  

#MP por ano
dados_agrupados2 <- subset(novo_df, novo_df$`Recorrente` == 'Ministério Público') %>%
  group_by(Ano, `Situação do Provimento`) %>%
  summarise(Contagem = n())

total_por_ano <- dados_agrupados2 %>%
  group_by(Ano) %>%
  summarise(Total = sum(Contagem))

dados_agrupados2 <- merge(dados_agrupados2, total_por_ano, by = c("Ano"))

dados_agrupados2$Percentual <- (dados_agrupados2$Contagem / dados_agrupados2$Total) * 100


ggplot(dados_agrupados2, aes(x = interaction(Ano, sep = "-"), y = Percentual, color = `Situação do Provimento`)) +
  geom_line() +
  geom_point() +
  labs(x = "Mês-Ano", y = "Quantidade") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Entes Públicos por ano
dados_agrupados3 <- subset(novo_df, novo_df$`Principal Parte Ativa` == 'Ente Público') %>%
  group_by(Ano, `Situação do Provimento`) %>%
  summarise(Contagem = n())

total_por_ano <- dados_agrupados3 %>%
  group_by(Ano) %>%
  summarise(Total = sum(Contagem))

dados_agrupados3 <- merge(dados_agrupados3, total_por_ano, by = c("Ano"))

dados_agrupados3$Percentual <- (dados_agrupados3$Contagem / dados_agrupados3$Total) * 100


ggplot(dados_agrupados3, aes(x = interaction(Ano, sep = "-"), y = Percentual, color = `Situação do Provimento`)) +
  geom_line() +
  geom_point() +
  labs(x = "Mês-Ano", y = "Quantidade") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#Particular por ano
dados_agrupados4 <- subset(novo_df, novo_df$`Recorrente` == 'Particular') %>%
  group_by(Ano, `Situação do Provimento`) %>%
  summarise(Contagem = n())

total_por_ano <- dados_agrupados4 %>%
  group_by(Ano) %>%
  summarise(Total = sum(Contagem))

dados_agrupados4 <- merge(dados_agrupados4, total_por_ano, by = c("Ano"))

dados_agrupados4$Percentual <- (dados_agrupados4$Contagem / dados_agrupados4$Total) * 100

dados_agrupados4$`Situação do Provimento` <- factor(dados_agrupados4$`Situação do Provimento`, levels = c("Não-Provimento", "Provimento"))

ggplot(dados_agrupados4, aes(x = interaction(Ano, sep = "-"), y = Percentual, color = `Situação do Provimento`)) +
  geom_line() +
  geom_point() +
  geom_path(lineend = "butt", 
            linejoin = "round", linemitre = 1) +
  labs(x = "Mês-Ano", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

dados_provimento <- subset(dados_agrupados4, `Situação do Provimento` == "Provimento")

ggplot(dados_provimento, aes(x = Ano, y = Percentual, color = `Situação do Provimento`)) +
  geom_line() +
  geom_point() +
  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) +
  labs(x = "Mês-Ano", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") 


#verificação em outros períodos de tempo:
#Bimestre:
#MP por Bimestre
dados_agrupados21 <- subset(novo_df, novo_df$`Recorrente` == 'Ministério Público') %>%
  group_by(Ano, Bimestre, `Situação do Provimento`) %>%
  summarise(Contagem = n())

total_por_bimestre <- dados_agrupados21 %>%
  group_by(Ano, Bimestre) %>%
  summarise(Total = sum(Contagem))

dados_agrupados21 <- merge(dados_agrupados21, total_por_bimestre, by = c("Ano", "Bimestre"))

dados_agrupados21$Percentual <- (dados_agrupados21$Contagem / dados_agrupados21$Total) * 100


ggplot(dados_agrupados21, aes(x = interaction(Bimestre, Ano, sep = "-"), y = Percentual, color = `Situação do Provimento`)) +
  geom_line() +
  geom_point() +
  labs(x = "Bimestre", y = "Quantidade") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Particular
dados_agrupados41 <- subset(novo_df, novo_df$`Recorrente` == 'Particular') %>%
  group_by(Ano, Bimestre, `Situação do Provimento`) %>%
  summarise(Contagem = n())

total_por_bimestre <- dados_agrupados41 %>%
  group_by(Ano, Bimestre) %>%
  summarise(Total = sum(Contagem))

dados_agrupados41 <- merge(dados_agrupados41, total_por_bimestre, by = c("Ano", "Bimestre"))

dados_agrupados41$Percentual <- (dados_agrupados41$Contagem / dados_agrupados41$Total) * 100


ggplot(dados_agrupados41, aes(x = interaction(Bimestre,Ano, sep = "-"), y = Percentual, color = `Situação do Provimento`)) +
  geom_line() +
  geom_point() +
  geom_path(lineend = "butt", 
            linejoin = "round", linemitre = 1) +
  labs(x = "Mês-Ano", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


#Semestre:
#MP por semestre
dados_agrupados22 <- subset(novo_df, novo_df$`Recorrente` == 'Ministério Público') %>%
  group_by(Ano, Semestre, `Situação do Provimento`) %>%
  summarise(Contagem = n())

total_por_semestre <- dados_agrupados22 %>%
  group_by(Ano, Semestre) %>%
  summarise(Total = sum(Contagem))

dados_agrupados22 <- merge(dados_agrupados22, total_por_semestre, by = c("Ano", "Semestre"))

dados_agrupados22$Percentual <- (dados_agrupados22$Contagem / dados_agrupados22$Total) * 100


ggplot(dados_agrupados22, aes(x = interaction(Semestre, Ano, sep = "-"), y = Percentual, color = `Situação do Provimento`)) +
  geom_line() +
  geom_point() +
  labs(x = "Semestre", y = "Quantidade") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#Particular por semestre
dados_agrupados42 <- subset(novo_df, novo_df$`Recorrente` == 'Particular') %>%
  group_by(Ano, Semestre, `Situação do Provimento`) %>%
  summarise(Contagem = n())

total_por_semestre <- dados_agrupados42 %>%
  group_by(Ano, Semestre) %>%
  summarise(Total = sum(Contagem))

dados_agrupados42 <- merge(dados_agrupados42, total_por_semestre, by = c("Ano", "Semestre"))

dados_agrupados42$Percentual <- (dados_agrupados42$Contagem / dados_agrupados42$Total) * 100


ggplot(dados_agrupados42, aes(x = interaction(Semestre,Ano, sep = "-"), y = Percentual, color = `Situação do Provimento`)) +
  geom_line() +
  geom_point() +
  geom_path(lineend = "butt", 
            linejoin = "round", linemitre = 1) +
  labs(x = "Mês-Ano", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 




#Agora iniciaremos analisando a segunda base de dados na qual temos os processos
#em primeira instância e iremos verificar quantas novas ações foram propostas após a 
#alteração na LIA'''

primeira_df <- BasePrimeira

primeira_df <- BasePrimeira %>%
  mutate(`Principal Parte Ativa` = case_when(
    str_detect(`Principal Parte Ativa`, "Ministério") ~ "Ministério Público",
    str_detect(`Principal Parte Ativa`, "Prefeitura") | 
      str_detect(`Principal Parte Ativa`, "Município") | 
      str_detect(`Principal Parte Ativa`, "Estado") | 
      str_detect(`Principal Parte Ativa`, "MUNICÍPIO") |
      str_detect(`Principal Parte Ativa`, "PREFEITURA") ~ "Ente Público",
    TRUE ~ "Particular"
  ))


primeira_df <- primeira_df %>%
  filter(`Assunto` %in% c('Improbidade Administrativa'))


primeira_df <- primeira_df %>%
  filter(primeira_df$Entrada >= '2014-01-01')

primeira_df <- subset.data.frame(primeira_df, primeira_df$`Principal Parte Ativa`!= 'Particular')


primeira_df$Ano <- format(primeira_df$Entrada, "%Y")
primeira_df$Mes <- format(primeira_df$Entrada, "%m")


#Total por ano, parte indiscriminada - Percentual aparentemente mostra que o MP avocou as
#ações para sua competência, dada a mudança de quem poderá entrar com ações agora.


dados_primeira1 <- primeira_df %>%
  group_by(Ano, `Principal Parte Ativa`) %>%
  summarise(Contagem = n())

total_por_ano <- dados_primeira1 %>%
  group_by(Ano) %>%
  summarise(Total = sum(Contagem))

dados_primeira1 <- merge(dados_primeira1, total_por_ano, by = c("Ano"))

dados_primeira1$Percentual <- (dados_primeira1$Contagem / dados_primeira1$Total) * 100


ggplot(dados_primeira1, aes(x = interaction(Ano, sep = "-"), y = Percentual, color = `Principal Parte Ativa`)) +
  geom_line() +
  geom_point() +
  labs(x = "Mês-Ano", y = "Quantidade") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


dados_primeira2 <- primeira_df %>%
  group_by(Ano, `Principal Parte Ativa`) %>%
  summarise(Contagem = n())

total_por_ano <- dados_primeira2 %>%
  group_by(Ano) %>%
  summarise(Total = sum(Contagem))



dados_primeira2 <- subset(dados_primeira2, dados_primeira2$Ano >2015)



ggplot(dados_primeira2, aes(x = interaction(Ano, sep = "-"), y = Contagem, color = `Principal Parte Ativa`)) +
  geom_line() +
  geom_point() +
  labs(x = "Mês-Ano", y = "Quantidade") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


summarise(dados_primeira2)



## terceira hipótese número de vezes que recorreu
#MP por ano
dados_agrupados333 <- novo_df %>%
  group_by(Ano, `Principal Parte Ativa`) %>%
  summarise(Contagem = n())


dados_câmara <- novo_df %>%
  group_by(Ano, novo_df$`Órgão Julgador Segunda Instancia`, novo_df$`Principal Parte Ativa`) %>%
  summarise(Contagem = n())

