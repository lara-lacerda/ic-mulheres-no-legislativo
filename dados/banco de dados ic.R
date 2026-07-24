library(data.table)
library(tidyverse)
library(knitr)
library(dplyr)

#LEITURA DE DADOS
caminho_real <- "C:/Users/Lara/Downloads/dados_IC"

arquivos <- list.files(path = caminho_real, pattern = "\\.csv$", full.names = TRUE,recursive = TRUE, ignore.case = TRUE)

dados_unidos <- rbindlist(lapply(arquivos, fread, encoding = "Latin-1"), fill = TRUE)


###################DEPUTADOS FEDERAIS: 2002 A 2022
deputados_federais <- dados_unidos%>%
  select(ANO_ELEICAO, DS_CARGO, DS_GENERO, SG_UE, DS_SIT_TOT_TURNO, SG_PARTIDO)%>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR QP", "ELEITO POR MÉDIA", "MÉDIA"),
         DS_CARGO == "DEPUTADO FEDERAL")


partidos_df <- deputados_federais%>%
  group_by(ANO_ELEICAO, SG_UE) %>%
  summarise(PARTIDOS_ELEITOS = n_distinct(SG_PARTIDO), .groups = "drop")

mulheres_df <- deputados_federais %>%
  group_by(ANO_ELEICAO, SG_UE) %>%
  summarise(MULHERES_ELEITAS = sum(DS_GENERO == "FEMININO"), .groups = "drop")


#TRANSFORMANDO EM TABELA
tabela_partidos_df <- partidos_df %>%
  group_by(SG_UE) %>%
  summarise(MEDIA_PARTIDOS = mean(PARTIDOS_ELEITOS))

tabela_mulheres_df <- mulheres_df%>%
  group_by(SG_UE) %>%
  summarise(MEDIA = mean(MULHERES_ELEITAS))

#################ASSEMBLEIAS LEGISLATIVAS: 2002 A 2022

deputados_estaduais <- dados_unidos%>%
  select(ANO_ELEICAO, DS_CARGO, DS_GENERO, SG_UE, DS_SIT_TOT_TURNO, SG_PARTIDO)%>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR QP", "ELEITO POR MÉDIA", "MÉDIA"),
         DS_CARGO == "DEPUTADO ESTADUAL")

partidos_de <- deputados_estaduais%>%
  group_by(ANO_ELEICAO, SG_UE) %>%
  summarise(PARTIDOS_ELEITOS = n_distinct(SG_PARTIDO), .groups = "drop")

mulheres_de <- deputados_estaduais %>%
  group_by(ANO_ELEICAO, SG_UE) %>%
  summarise(MULHERES_ELEITAS = sum(DS_GENERO == "FEMININO"), .groups = "drop")


#TRANSFORMANDO EM TABELA
tabela_partidos_de <- partidos_de %>%
  group_by(SG_UE) %>%
  summarise(MEDIA_PARTIDOS = mean(PARTIDOS_ELEITOS))

tabela_mulheres_de <- mulheres_de%>%
  group_by(SG_UE) %>%
  summarise(MEDIA = mean(MULHERES_ELEITAS))