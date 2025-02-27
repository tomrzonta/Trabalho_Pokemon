# TRABALHO = BASE_POKEMON
renv::activate()
install.packages("summarytools")
install.packages("renv")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("readr")
install.packages("dplyr")
install.packages("summarytools")
install.packages("readxl")
install.packages("knitr")
install.packages("dlookr")

library(renv)
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(summarytools)
library(readxl)
library(knitr)
library(dlookr)

renv::init()
renv::snapshot()

## Importar Base

df <- read.csv("C:/Users/Pichau/Documents/RSTUDIO/Estatistica_para_Ciencia_Dados/pokemon.csv", stringsAsFactors = FALSE)

## Contagem de linhas

count(df)

## Início da tabela

kable(head(df))

## Tipo de dado

df %>% dlookr::diagnose()

## Frequência por tipo

df %>% dplyr::select(Type.1) %>% summarytools::freq()

## Primeira comparação de poder de ataque

stronger <- df %>% group_by(Generation) %>% summarise(Mais_Forte = Name[which.max(Attack)],max_atack = max(Attack, na.rm = TRUE),
                                                      Mais_Fraco = Name[which.min(Attack)], min_atack = min(Attack, na.rm = TRUE))

print(stronger)

## Buscando outliers

ggplot(data = stronger) +
  geom_point(mapping = aes(x = Mais_Forte,  y = max_atack)) +
  geom_point(mapping = aes(x = Mais_Fraco, y = min_atack))

Mais_forte_Tipo <- df %>% group_by(Type.1) %>% summarise(Mais_forte_tipo1 = Name[which.max(Attack)],
                                                         max_forte = max(Attack, na.rm = TRUE)) %>% arrange(desc(max_forte))
Mais_forte_Tipo

# Media de ataque calculada por geração

media_por_geracao <- df %>% group_by(Generation) %>% summarise(media = mean(Attack, na.rm = TRUE)) %>%
  arrange(Generation)

media_por_geracao

## Buscando outliers

df %>% dplyr::select(Generation, Attack) %>%
  ggplot(aes(group = Generation, x=Generation, y = Attack)) + geom_boxplot() +
  xlab('Geração') +
  ylab('Ataque') +
  theme_classic()

## Estatísticas de ataque

estatisticas_attack <- df %>% group_by(Generation) %>%
  summarise(
    media = mean(Attack, na.rm = TRUE),
    desvio = sd(Attack, na.rm = TRUE),
    prim_quartil = quantile(Attack, 0.25, na.rm = TRUE),
    terc_quartil = quantile(Attack, 0.75, na.rm = TRUE)
  )

print(estatisticas_attack)

## Comparação de tipo e geração com base no ataque

df %>% ggplot(aes(x= Type.1, y= Generation, fill = Attack)) +
  geom_tile() +
  xlab("Tipo") +
  ylab("Geração")

## Função DESC

df %>%  group_by(Generation) %>% dplyr::select(Attack) %>% summarytools::descr()

## Histograma de poder de ataque por quantidade

ggplot(df, aes(x = Attack)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Ataque", x = "Attack", y = "Quantidade") +
  theme_minimal()

## Distribuição de ataque separado por geração

ggplot(df, aes(x = as.factor(Generation), y = Attack, fill = as.factor(Generation))) +
  geom_violin(alpha = 0.5) +
  labs(title = "Distribuição de Ataque por Geração", x = "Geração", y = "Ataque") +
  theme_minimal() +
  theme(legend.position = "none")

## Scatterplot entre ataque e defesa


ggplot(df, aes(x = Attack, y = Defense, color = as.factor(Type.1))) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatterplot de Attack vs. Defense",
       x = "Attack", y = "Defense", color = "Tipo de Pokémon") +
  theme_minimal()


## Gráfico de linha com média por geração

df_summary <- df %>%
  group_by(Generation) %>%
  summarise(Media_Attack = mean(Attack, na.rm = TRUE))

ggplot(df_summary, aes(x = Generation, y = Media_Attack)) +
  geom_line(color = "blue", size = 1) +  
  geom_point(color = "red", size = 3) + 
  labs(title = "Media de Ataque por Geracao", x = "Geracao", y = "Media de Ataque") +
  theme_minimal()
