# Instale o pacote ggplot2 se ainda não tiver
install.packages("ggplot2")

# Carregar o pacote
library(ggplot2)

# Criando dados simulados
x <- seq(-10, 10, by = 0.1) # Variável score (X)

# Funções para os resultados esperados (E[Y(0)|X] e E[Y(1)|X])
y0_controle <- ifelse(x < 0, -0.2 * x, NA)  # Controle antes do cutoff
y1_tratamento <- ifelse(x >= 0, 0.2 * x + 1, NA)  # Tratamento após o cutoff

# Criando um data frame com as linhas
data <- data.frame(
  X = x,
  Y0 = -0.2 * x,      # Linha contínua E[Y(0)|X]
  Y1 = 0.2 * x + 1    # Linha contínua E[Y(1)|X]
)

# Criando o gráfico
ggplot() +
  # Curva azul contínua antes do corte (E[Y(0)|X])
  geom_line(data = data[data$X < 0, ], aes(x = X, y = Y0), color = "blue", linetype = "solid", size = 1) +
  
  # Curva azul tracejada após o corte (E[Y(0)|X])
  geom_line(data = data[data$X >= 0, ], aes(x = X, y = Y0), color = "blue", linetype = "dashed", size = 1) +
  
  # Curva vermelha tracejada antes do corte (E[Y(1)|X])
  geom_line(data = data[data$X < 0, ], aes(x = X, y = Y1), color = "red", linetype = "dashed", size = 1) +
  
  # Curva vermelha contínua após o corte (E[Y(1)|X])
  geom_line(data = data[data$X >= 0, ], aes(x = X, y = Y1), color = "red", linetype = "solid", size = 1) +
  
  # Linha vertical no cutoff
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  
  # Anotação da diferença causal τ_SRD
  annotate("text", x = 1, y = 0.5, label = expression(tau[SRD]), size = 5, color = "black") +
  
  # Customizações do tema (sem fundo azul)
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "serif") # Mudança da fonte
  ) +
  
  # Labels e título
  labs(
    x = "Score (X)",
    y = "Outcome Esperado (Y)",
    title = "Sharp RDD",
    subtitle = "Ilustrando o Efeito do Tratamento no Cutoff"
  )



# Criando dados simulados
x <- seq(-10, 10, by = 0.1) # Variável score (X)

# Funções para os resultados esperados (E[Y(0)|X] e E[Y(1)|X])
y0_controle <- ifelse(x < 0, -0.2 * x, NA)  # Controle antes do cutoff
y1_tratamento <- ifelse(x >= 0, 0.2 * x + 1, NA)  # Tratamento após o cutoff

# Criando um data frame com as linhas
data <- data.frame(
  X = x,
  Y0 = -0.2 * x,      # Linha contínua E[Y(0)|X]
  Y1 = 0.2 * x + 1    # Linha contínua E[Y(1)|X]
)

# Criando o gráfico
ggplot() +
  # Curva azul contínua antes do corte (E[Y(0)|X])
  geom_line(data = data[data$X < 0, ], aes(x = X, y = Y0), color = "blue", linetype = "solid", size = 1) +
  
  # Curva azul tracejada após o corte (E[Y(0)|X])
  geom_line(data = data[data$X >= 0, ], aes(x = X, y = Y0), color = "blue", linetype = "dashed", size = 1) +
  
  # Curva vermelha tracejada antes do corte (E[Y(1)|X])
  geom_line(data = data[data$X < 0, ], aes(x = X, y = Y1), color = "red", linetype = "dashed", size = 1) +
  
  # Curva vermelha contínua após o corte (E[Y(1)|X])
  geom_line(data = data[data$X >= 0, ], aes(x = X, y = Y1), color = "red", linetype = "solid", size = 1) +
  
  # Segmento contínuo em verde no X=0 entre Y=0 e Y=1
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), color = "black", size = 1) +
  
  # Linha vertical no cutoff
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  
  # Anotação da diferença causal τ_SRD
  annotate("text", x = 1.3, y = 0.5, label = expression(delta[SRD]), size = 5, color = "black") +
  
  # Adicionando as legendas nas curvas
  annotate("text", x = -7.5, y = 2, label = '(E[Y(0)]|X])', color = "blue", size = 4, hjust = 0) +
  annotate("text", x = 5, y = 2.5, label = '(E[Y(1)]|X])', color = "red", size = 4, hjust = 1) +
  
  # Customizações do tema (sem fundo azul)
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "serif") # Mudança da fonte
  ) +
  
  # Labels e título
  labs(
    x = "Score (X)",
    y = "Outcome Esperado (Y)",
    title = "Sharp RDD",
    subtitle = "Ilustrando o Efeito do Tratamento no Cutoff"
  )


# Instale o pacote ggplot2 se ainda não tiver
install.packages("ggplot2")

# Carregar o pacote
library(ggplot2)

# Criando dados simulados
x <- seq(-10, 10, by = 0.1) # Variável score (X)

# Funções para os resultados esperados (E[Y(0)|X] e E[Y(1)|X])
y0_controle <- ifelse(x < 0, -0.2 * x, NA)  # Controle antes do cutoff
y1_tratamento <- ifelse(x >= 0, 0.2 * x + 1, NA)  # Tratamento após o cutoff

# Criando um data frame com as linhas
data <- data.frame(
  X = x,
  Y0 = -0.2 * x,      # Linha contínua E[Y(0)|X]
  Y1 = 0.2 * x + 1    # Linha contínua E[Y(1)|X]
)

# Criando o gráfico
ggplot() +
  # Curva azul contínua antes do corte (E[Y(0)|X])
  geom_line(data = data[data$X < 0, ], aes(x = X, y = Y0), color = "blue", linetype = "solid", size = 1) +
  
  # Curva azul tracejada após o corte (E[Y(0)|X])
  geom_line(data = data[data$X >= 0, ], aes(x = X, y = Y0), color = "blue", linetype = "dashed", size = 1) +
  
  # Curva vermelha tracejada antes do corte (E[Y(1)|X])
  geom_line(data = data[data$X < 0, ], aes(x = X, y = Y1), color = "red", linetype = "dashed", size = 1) +
  
  # Curva vermelha contínua após o corte (E[Y(1)|X])
  geom_line(data = data[data$X >= 0, ], aes(x = X, y = Y1), color = "red", linetype = "solid", size = 1) +
  
  # Segmento contínuo em verde no X=0 entre Y=0 e Y=1
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), color = "green", size = 1) +
  
  # Linha vertical no cutoff
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  
  # Anotação da diferença causal τ_SRD
  annotate("text", x = 0.5, y = 1.5, label = expression(delta[SRD]), size = 5, color = "black") +
  
  # Adicionando as legendas nas curvas
  annotate("text", x = -8, y = -1.5, label = expression(E[Y[i] == 0 | X[i]]), color = "blue", size = 4, hjust = 0) +
  annotate("text", x = 8, y = 3, label = expression(E[Y[i] == 1 | X[i]]), color = "red", size = 4, hjust = 1) +
  
  # Customizações do tema (sem fundo azul)
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "serif") # Mudança da fonte
  ) +
  
  # Labels e título
  labs(
    x = "Score (X)",
    y = "Expected Outcome (Y)",
    title = "Sharp Regression Discontinuity Design",
    subtitle = "Illustrating Treatment Effect at the Cutoff"
  )
