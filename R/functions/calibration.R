

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



# get cost for calibration
getCost<-function(p, modelo, dados_calibracao){
  
  output_modelo = solve_modelo_dissertacao(parametros = p, modelo = modelo, simtime = SIM_TIME)
  
  cost <- modCost(obs=dados_calibracao, model=output_modelo)
  
  return(cost)
  
}

# add error to ensemble
adicionar_erro_ao_ensemble = function(results, variavel_calibracao, planilha_calibracao, lever, opcoes = opcoes) {
  
  dados_calibracao <- as.data.frame(read_xlsx(path = planilha_calibracao, sheet = "Plan1"))
  
  variaveis_a_utilizar_modelo = c(opcoes$VarCenarios, opcoes$VarTempo, variavel_calibracao)
  
  variaveis_a_utilizar_dados = c(opcoes$VarTempo, variavel_calibracao)
  
  # Se um lever não foi informado, usar o primeiro do ensemble
  if(missing(lever)){
    # Usar o primeiro lever que eu achar
    lever = results$DadosSimulados$Lever[1]
  }
  
  dados_modelo = results$DadosSimulados[which(results$DadosSimulados$Lever == lever),variaveis_a_utilizar_modelo]
  
  cenarios = unique(dados_modelo$Scenario)
  
  # Função para Obter Estatísticas de Fit
  obter_estatisticas_fit = function(cenario, dados_modelo = dados_modelo , dados_calibracao = dados_calibracao, variaveis_a_utilizar_dados = variaveis_a_utilizar_dados) {
    modcost = FME::modCost(model = dados_modelo[which(dados_modelo$Scenario == cenario),],
                           obs = dados_calibracao[,variaveis_a_utilizar_dados])
    
    data_model = modcost$residuals$mod
    
    data_obs = modcost$residuals$obs
    
    n = modcost$var$N
    
    mean_model = mean(data_model, na.rm = TRUE)
    
    mean_data = mean(data_obs, na.rm = TRUE)
    
    sd_model = sqrt(sum((data_model - mean_model)^2)/n) 
    
    sd_data = sqrt(sum((data_obs - mean_data)^2)/n)
    
    correlation_coef = sum(((data_model - mean_model) / sd_model) * ((data_obs - mean_data) / sd_data)) / n
    
    # Como o objetivo não é predição não irei inserir R Squared.
    RSquared = correlation_coef ^ 2
    
    # Calcular os dados segundo sterman (para o R2 fechar, os índices também).
    correlacao = cor(x = modcost$residuals$obs, y = modcost$residuals$mod)
    
    # Soma dos Erros Quadrados
    SumOfSquareResiduals = modcost$model
    
    # Mean Square error
    MeanSquareError = SumOfSquareResiduals / modcost$var$N
    
    # Root Mean Square Error
    RootMeanSquareError = sqrt(MeanSquareError)
    
    # Mean Absolute Error
    MeanAbsoluteError = sum(abs(modcost$residuals$res)) / modcost$var$N
    
    # Mean Absolute Percent Error
    MeanAbsolutePercentError = (sum(abs(modcost$residuals$res) / abs(modcost$residuals$obs))) / modcost$var$N
    
    # Thiel Statistics: Morecroft (2007), pg. 399. 
    UM_ThielBiasDiffMeans =  ((mean_model - mean_data)^2) / MeanSquareError
    #V Sterman: UM_ThielBiasDiffMeans =  (mean_model^2 - mean_data^2) / MeanSquareError
    
    US_ThielUnequalVariation = ((sd_model - sd_data)^2) / MeanSquareError
    #V Sterman: US_ThielUnequalVariation = (sd_model^2 - sd_data^2) / MeanSquareError
    
    UC_ThielUnequalCovariation = (1 / MeanSquareError) * (sd_model * sd_data) * (2 * (1 - correlation_coef))
    
    # if(cenario == 140){
    #   browser()
    # }
    
    stats_fit = c(RSquared = RSquared,
                  r = correlation_coef,
                  RootMeanSquareError = RootMeanSquareError,
                  SumOfSquareResiduals = SumOfSquareResiduals,
                  MeanSquareError = MeanSquareError,
                  MeanAbsoluteError = MeanAbsoluteError,
                  MeanAbsolutePercentError = MeanAbsolutePercentError,
                  UM_ThielBiasDiffMeans = UM_ThielBiasDiffMeans,
                  US_ThielUnequalVariation = US_ThielUnequalVariation,
                  UC_ThielUnequalCovariation = UC_ThielUnequalCovariation)
    stats_fit
  }
  
  stats_fit = obter_estatisticas_fit(cenario = 1, dados_modelo = dados_modelo, dados_calibracao = dados_calibracao, variaveis_a_utilizar_dados = variaveis_a_utilizar_dados)
  
  stats_fit = lapply(1:length(cenarios), obter_estatisticas_fit, dados_modelo = dados_modelo, dados_calibracao = dados_calibracao, variaveis_a_utilizar_dados = variaveis_a_utilizar_dados)
  stats_fit = do.call(rbind, stats_fit)
  
  ensemble = cbind(results$Ensemble, stats_fit)
  
  return(ensemble)
  
}