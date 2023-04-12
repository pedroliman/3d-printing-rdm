

# Selective Openness Code Repository --------------------------------------
# Code Repository for the "Selective Openness" paper
# Copyright (C) 2022 by Pedro Nascimento de Lima
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# Read the README.md file for usage instructions.
# -------------------------------------------------------------------------



# Settings ----------------------------------------------------------------

opcoes = list(
  VarResposta = "sNPVProfit1",
  VarCenarios = "Scenario",
  VarEstrategias = "Lever",
  N = 30,
  VarTempo = "time",
  VarCriterio = "RegretPercentil75",
  SentidoCriterio = "min",
  Paralelo = TRUE,
  ModoParalelo = "PSOCK", # PSOCK - Windows e Linux. FORK - Apenas UNIX
  SimularApenasCasoBase = TRUE,
  FullFactorialDesign = TRUE,
  FiltrarCasosPlausiveis = TRUE
)

START<-2018; FINISH <-2028; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
invisible(sapply(X = paste0(list.files(path = "./R/functions/", pattern = "*.R",full.names = T)),FUN = source, echo = F)) 
N_PLAYERS <- 4;

# Parâmetros para a Geração dos Gráficos
plots_width = 9
plots_heigh = 3.5

USAR_DADOS_SALVOS = FALSE
SIMULAR_HISTORICO_DIFERENTE = FALSE


library(dplyr)
library(tidyr)

# Load Previous Results ---------------------------------------------------

load("./outputs/results_final.rda")



# Table 1 -----------------------------------------------------------------

# Rank strategies by NPV Profit Regret 

s_ranking <- results$AnaliseRegret$ResumoEstrategias[,c("Lever", "sNPVProfit1RegretPercPercentil75", "sNPVProfit1RegretPercentil75")] %>%
  dplyr::inner_join(results$Inputs$Levers) %>%
  dplyr::arrange(sNPVProfit1RegretPercentil75) %>%
  mutate(MktShareStrat = case_when(aSwitchForCapacityStrategy1 == 1 ~ "Aggressive", 
                                   T ~ "Conservative")) %>%
  dplyr::select(Lever, MktShareStrat, aDesiredMarketShare1, aOrcamentoPeD1, aPercPeDAberto1, sNPVProfit1RegretPercentil75) %>%
  mutate(sNPVProfit1RegretPercentil75 = round(sNPVProfit1RegretPercentil75 / 1e6, 0))


writexl::write_xlsx(s_ranking, "./figures/table_1.xlsx")


# Figure 1 ----------------------------------------------------------------

# Whisker plot by strategy

fig1 = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1Regret", nome_amigavel_variavel = "NPV Regret")

fig1

ggsave("./figures/fig1.svg", width = 9,height = 5,bg = "white")


# Data for Figures 2 and 3 ------------------------------------------------

# Compute additional outputs
results$DadosUltimoPeriodo$sPriceAvg = rowSums(results$DadosUltimoPeriodo[,c("aOrderShare1", "aOrderShare2", "aOrderShare3", "aOrderShare4")] * results$DadosUltimoPeriodo[,c("sPrice1", "sPrice2", "sPrice3", "sPrice4")])

results$DadosUltimoPeriodo$aPerformanceAvg = rowSums(results$DadosUltimoPeriodo[,c("aOrderShare1", "aOrderShare2", "aOrderShare3", "aOrderShare4")] * results$DadosUltimoPeriodo[,c("aPerformance1", "aPerformance2", "aPerformance3", "aPerformance4")])

results$DadosUltimoPeriodo$sTotalInstalledBase = rowSums(results$DadosUltimoPeriodo[,c("sInstalledBase1", "sInstalledBase2", "sInstalledBase3", "sInstalledBase4")])


## analyse regret for several variables:
analise_regret_profit_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "sNPVProfit1", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)
analise_regret_profit = analise_regret_profit_completa$ResumoEstrategias

analise_regret_share_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "aOrderShare1", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)
analise_regret_share = analise_regret_share_completa$ResumoEstrategias

analise_regret_performance_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "aPerformance1", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)
analise_regret_performance = analise_regret_performance_completa$ResumoEstrategias

analise_regret_adopters_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "sCumulativeAdopters", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)
analise_regret_adopters  = analise_regret_adopters_completa$ResumoEstrategias

analise_regret_average_price_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "sPriceAvg", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias, sentido = "min")
analise_regret_average_price  = analise_regret_average_price_completa$ResumoEstrategias

analise_regret_average_performance_completa  = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "aPerformanceAvg", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)
analise_regret_average_performance  = analise_regret_average_performance_completa$ResumoEstrategias

analise_regret_total_installed_base_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "sTotalInstalledBase", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)
analise_regret_total_installed_base  = analise_regret_total_installed_base_completa$ResumoEstrategias



minimax <- function(x) (x-min(x))/(max(x)-min(x))

# join Datasets:
dataset_todas_as_metricas = analise_regret_profit %>%
  left_join(., analise_regret_share, by = "Lever") %>% 
  left_join(., analise_regret_performance, by = "Lever") %>%
  left_join(., analise_regret_adopters, by = "Lever") %>% 
  left_join(., analise_regret_average_price, by = "Lever") %>%
  left_join(., analise_regret_average_performance, by = "Lever") %>%
  left_join(., analise_regret_total_installed_base, by = "Lever")

# Filtrando Só o que eu quero usar e Unindo com a Descrição dos Levers:
dataset_todas_as_metricas  = dataset_todas_as_metricas %>%
  dplyr::select(., matches("RegretPercentil75|Lever")) %>%
  left_join(s_ranking %>% dplyr::select(-sNPVProfit1RegretPercentil75))

summary(dataset_todas_as_metricas)

normalized_data <- dataset_todas_as_metricas %>%
  mutate(across(contains("RegretPercentil"), ~minimax(.x)))

# Write out file
writexl::write_xlsx(normalized_data, "./outputs/output_regret.xlsx")

# Figure 2 ----------------------------------------------------------------


# Figure 3 ----------------------------------------------------------------






