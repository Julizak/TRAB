## Técnica de Análise de Agrupamentos - definição "Average" (k=4)
# Objetivo: Identificação de padrões ESG no Vale do Paraíba e Litoral Norte 
#           de São Paulo
# Fonte: https://conteudo.clp.org.br/ranking-de-competitividade-2023-relatorios

pacotes <- c("readxl", # para ler planilha Excel 
             "plotly", # plataforma gráfica
             "ggplot2", # plataforma gráfica
             "tidyverse", # carregar outros pacotes do R
             "ggrepel", # geoms de texto e rótulo para 'ggplot2' que ajudam a
             # evitar sobreposição de textos
             "knitr", "kableExtra", # formatação de tabelas
             "reshape2", # função 'melt'
             "misc3d", # gráficos 3D
             "plot3D", # gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", # função 'fviz_dend' para construção de dendrogramas
             "ade4") # função 'ade4' para matriz de distâncias em var. binárias

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregamento da base de dados
ranking <- read_excel("Municipios_2023.xlsx")

# Estatísticas descritivas
summary(ranking)

## Selecionar os dados apenas de ESG  
dados_ESG <- subset(ranking, Tipo != "Geral" & Tipo != "ODS")

## Tratar dados: Selecionar apenas dados dos municipios da RMVPLN

# >> 1- Renomear colunas
dados_ESG <- rename(dados_ESG, Municipio = Município)

# >> 2- Extrair outras regiões, estados e outros anos
dados_ESG <- subset(dados_ESG, Região != "Norte" & Região != "Nordeste" & 
                      Região != "Centro-oeste" & Região != "Sul" & 
                      Região != "Centro-Oeste")
dados_ESG <- subset(dados_ESG, UF != "RJ" & UF != "MG" & UF != "ES")
dados_ESG <- subset(dados_ESG, Ano != "2021" & Ano != "2022")

# >> 3- Dados filtrados (apenas cidades da RMVPLN)
dados_ESG_filtrados <- subset (dados_ESG, Municipio %in% 
                                 c("São José dos Campos", "Taubaté", "Caçapava",
                                   "Jacareí", "Caraguatatuba","Pindamonhangaba",
                                   "Guaratinguetá","Lorena","Ubatuba",
                                   "São Sebastião"))

# >> 4- Ajustar a tabela
# a- retiradas colunas desnecessárias
vetor_colunas <- c("Tipo", "Capital", "G100", "Amazônia Legal", "UF", "Região", 
                   "Delta", "Ano", "População", "Nota Bruta", "Posição")
tabela_ESG <- dados_ESG_filtrados[,!names(dados_ESG_filtrados) %in% 
                                    vetor_colunas] 

# b- replicar 3 tabelas para exclusão das linhas desnecessárias
df_tab_amb <- tabela_ESG
df_tab_soc <- tabela_ESG
df_tab_gov <- tabela_ESG

# c- retirando as notas de cada ESG
df_tab_amb <- df_tab_amb[-seq(11, nrow(df_tab_amb)),]
df_tab_soc <- df_tab_soc[-c(1:20),]
df_tab_gov <- df_tab_gov[-c(1:10),]
df_tab_gov <- df_tab_gov[-c(11:20),]


# d- renomear a coluna nota pelo correspondente do ESG
df_tab_amb <- rename(df_tab_amb, Ambiental = Nota)
df_tab_soc <- rename(df_tab_soc, Social = Nota)
df_tab_gov <- rename(df_tab_gov, Governanca = Nota)


# e- excluir a coluna Dado das tabelas especificas
df_tab_amb <- df_tab_amb[,-which(names(df_tab_amb) == "Dado")]
df_tab_soc <- df_tab_soc[,-which(names(df_tab_soc) == "Dado")]
df_tab_gov <- df_tab_gov[,-which(names(df_tab_gov) == "Dado")]

# f- juntar as tabelas especificas
ESG_final <- df_tab_amb 
ESG_final <- cbind(ESG_final, Social = df_tab_soc$Social)
ESG_final <- cbind(ESG_final, Governanca = df_tab_gov$Governanca)

# g- Gráfico 3D com scatter
rownames(ESG_final) <- ESG_final$Municipio

scatter3D(x=ESG_final$Ambiental,
          y=ESG_final$Social,
          z=ESG_final$Governanca,
          phi = 0, bty = "g", pch = 20, cex = 2,
          xlab = "Ambiental",
          ylab = "Social",
          zlab = "Governança",
          main = "ESG dos Municípios",
          clab = "Nota")>
  text3D(x=ESG_final$Ambiental,
         y=ESG_final$Social,
         z=ESG_final$Governanca,
         labels = rownames(ESG_final),
         add = TRUE, cex = 1)

# Estatísticas descritivas
summary(ESG_final)

# Boxplots por variável
ggplotly(
  ESG_final %>%
    melt() %>%
    ggplot(aes(label = Municipio)) +
    geom_boxplot(aes(x = variable, y = value, fill = variable)) +
    geom_point(aes(x = variable, y = value), alpha = 0.5) +
    labs(x = "Diretrizes",
         y = "Nota") +
    scale_fill_manual("Legenda:",
                      values = c("orange", "purple", "bisque4")) +
    theme_bw()
)

#---------- Esquema de aglomeração hierárquico ---------------------------------

# Matriz de dissimilaridades
matriz_D <- ESG_final %>% 
  select(Ambiental, Social, Governanca) %>% 
  dist(method = "euclidean")

# Visualizando a matriz de dissimilaridades
data.matrix(matriz_D) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Elaboração da clusterização hierárquica
cluster_hier <- agnes(x = matriz_D, method = "average")

# As distâncias para as combinações em cada estágio
coeficientes <- sort(cluster_hier$height, decreasing = FALSE) 
coeficientes

# Tabela com o esquema de aglomeração
esquema <- as.data.frame(cbind(cluster_hier$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

# Visualização do esquema hierárquico de aglomeração
esquema %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Construção do dendrograma
dev.off()
fviz_dend(x = cluster_hier)

# Dendrograma com visualização dos clusters (definição de 4 clusters)
fviz_dend(x = cluster_hier,
          k = 4,
          k_colors = c("seagreen", "peru", "royalblue4", "violetred2"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())

# Criar variável categórica para indicação do cluster 
## O argumento 'k' indica a quantidade de agrupamentos definidos
ESG_final$cluster_H <- factor(cutree(tree = cluster_hier, k = 4))

# Visualização da base de dados com a alocação das observações nos clusters
ESG_final %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Estatística da variável 'Ambiental'
group_by(ESG_final, cluster_H) %>%
  summarise(
    mean = mean(Ambiental, na.rm = TRUE),
    sd = sd(Ambiental, na.rm = TRUE),
    min = min(Ambiental, na.rm = TRUE),
    max = max(Ambiental, na.rm = TRUE))

# Estatística da variável 'Social'
group_by(ESG_final, cluster_H) %>%
  summarise(
    mean = mean(Social, na.rm = TRUE),
    sd = sd(Social, na.rm = TRUE),
    min = min(Social, na.rm = TRUE),
    max = max(Social, na.rm = TRUE))

# Estatística da variável 'Governanca'
group_by(ESG_final, cluster_H) %>%
  summarise(
    mean = mean(Governanca, na.rm = TRUE),
    sd = sd(Governanca, na.rm = TRUE),
    min = min(Governanca, na.rm = TRUE),
    max = max(Governanca, na.rm = TRUE))

# ANOVA da variável 'Ambiental'
summary(anova_Ambiental <- aov(formula = Ambiental ~ cluster_H,
                               data = ESG_final))

# ANOVA da variável 'Social'
summary(anova_Social <- aov(formula = Social ~ cluster_H,
                            data = ESG_final))

# ANOVA da variável 'Governanca'
summary(anova_Governanca <- aov(formula = Governanca ~ cluster_H,
                                data = ESG_final))

## Gráficos do resultado de cada variável com o cluster_H:
# Ambiental:
ggplotly(
  ggplot(ESG_final) +
    geom_point(aes(x = Municipio, 
                   y = Ambiental, 
                   color = cluster_H)) + 
    labs(x = "Ambiental",
         y = "Nota") +
    theme(axis.text.x=element_text(angle=90,vjust=0,5,hjust=1)) +
    scale_color_manual(values = c("seagreen", "violetred2", "royalblue4", 
                                  "peru")) 
)

# Social:
ggplotly(
  ggplot(ESG_final) +
    geom_point(aes(x = Municipio, 
                   y = Social, 
                   color = cluster_H)) + 
    labs(x = "Social",
         y = "Nota") +
    theme(axis.text.x=element_text(angle=90,vjust=0,5,hjust=1)) +
    scale_color_manual(values = c("seagreen", "violetred2", "royalblue4", 
                                  "peru")) 
)

# Governança:
ggplotly(
  ggplot(ESG_final) +
    geom_point(aes(x = Municipio, 
                   y = Governanca, 
                   color = cluster_H)) + 
    labs(x = "Governança",
         y = "Nota") +
    theme(axis.text.x=element_text(angle=90,vjust=0,5,hjust=1)) +
    scale_color_manual(values = c("seagreen", "violetred2", 
                                  "royalblue4", "peru")) 
)

# Criando o gráfico 3D interativo
cores <- c("seagreen", "violetred2", "royalblue4", "peru") 

# Convertendo cluster_H em fator com níveis específicos
ESG_final$cluster_H <- factor(ESG_final$cluster_H, 
                              levels = unique(ESG_final$cluster_H))

# Criando o gráfico plot_ly
plot_ly(ESG_final, x = ~Ambiental, y = ~Social, z = ~Governanca, 
        color = ~cluster_H, 
        colors = cores, marker = list(size = 2)) %>%
  add_markers() %>%
  add_text(text = ~Municipio, textposition = "top")

##################################### FIM ######################################

