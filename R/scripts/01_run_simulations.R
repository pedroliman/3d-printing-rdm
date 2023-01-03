

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



# Configuring simulations -------------------------------------------------

# Carregando Funções Úteis
list_tabelas_output = list()
START<-2007; FINISH <-2017; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;
N_PLAYERS <<- 4;

# Parâmetros para a Geração dos Gráficos
plots_width = 9
plots_heigh = 3.5

USAR_DADOS_SALVOS = FALSE
SIMULAR_HISTORICO_DIFERENTE = FALSE

# source necessary functions:
invisible(sapply(X = paste0(list.files(path = "./R/functions/", pattern = "*.R",full.names = T)),FUN = source, echo = F)) 


# Opções, mudando a Variável de Critério:
opcoes_iniciais = list(
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

opcoes = opcoes_iniciais

# Planilhas de Configuração das Simulação:
planilha_simulacao_calibracao_historico = "./inputs/params_calibracao_historico.xlsx"

planilha_simulacao_opcao1_futuro = "./inputs/params_calibracao_opcao1.xlsx"

planilha_opcao2.0_passado_e_futuro = planilha_simulacao_calibracao_historico

planilha_opcao2.1_futuro = planilha_simulacao_calibracao_historico

percentil_utilizado_como_criterio = c(PercentilCriterio = 0.5)

# Número de casos TOTAL a rodar (considerando todas as estratégias e todos os cenários).

n_casos_total = 54 * 2000

n_estrategias = nrow(carregar_inputs(arquivo_de_inputs = planilha_simulacao_opcao1_futuro, opcoes = opcoes)$Levers)

# Tamanho do Ensemble Adimitido (para simular todas as estratégias)
n_ensemble_total = round(n_casos_total / n_estrategias, 0) 

# Tamanho do ensemble para calibração.
n_ensemble_calibracao = round(n_ensemble_total / percentil_utilizado_como_criterio,0)

#### Simulate Strategies ####
opcoes$FiltrarCasosPlausiveis = FALSE
opcoes$SimularApenasCasoBase = FALSE
opcoes$N = n_ensemble_total
INICIALIZAR_ESTOQUES_COM_CASO_BASE = FALSE
SIMULAR_HISTORICO_DIFERENTE = FALSE
ANO_INICIO_AVALIACAO = 2018
planilha_inputs = planilha_simulacao_opcao1_futuro
START<-2018; FINISH <-2028; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;

# Source functions again:
invisible(sapply(X = paste0(list.files(path = "./R/functions/", pattern = "*.R",full.names = T)),FUN = source, echo = F)) 

# Simular
results = simularRDM_e_escolher_estrategia(inputs = planilha_inputs,
                                                 sdmodel = sdmodel, 
                                                 opcoes = opcoes)

dir.create("./outputs/", showWarnings=FALSE)

save(results, file = paste0("./outputs/results_final.rda"))