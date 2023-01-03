

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



##### CONSTANTES #####
VAR_SCENARIO = "Scenario"
VAR_LEVER = "Lever"

##### MODELO #####

#' solve_modelo_dissertacao
#' Inicializa Parâmetros e Estoques do Modelo da Dissertação, e retorna os resultados do modelo como um dataframe.
#'
#' @param parametros Vetor de parâmetros a serem utilizados na simulação. Deve incluir parâmetros para a inicialização dos estoques.
#' @param modelo Modelo de Dinâmica de Sistemas  (conforme os moldes da biblioteca deSolve).
#' @param simtime Vetor de Tempo da Simulação.
#'
#' @return matriz com resultados da simulação.
#' @export
#' 
solve_modelo_dissertacao <- function(parametros, modelo, simtime){
  
  # Número de Players no modelo
  N_PLAYERS <<- 4
  
  # All the stocks are initialised here...
  
  n_tempo = length(simtime)
  
  ordem_vetores_players = order(names(parametros[grep("aSwitchForCapacityStrategy", x = names(parametros))]))
  
  ##### VARIÁVEIS DE ENTRADA - AUXILIARES #####
  auxs    <- list(
    # Variáveis informadas de modo Independente por Player:
    # Estratégia de Capacidade:
    # A ordem dos números a seguir obedece a ordem na planilha e a ordem na geração do ensemble.
    aSwitchForCapacityStrategy = unname(round(parametros[grep("aSwitchForCapacityStrategy", x = names(parametros))][c(4,1,2,3)], 0))
    ,aDesiredMarketShare = unname(parametros[grep("aDesiredMarketShare", x = names(parametros))][c(4,1,2,3)])
    # Variáveis de Decisão - Existem para o player analisado e para os outros Players:
    ,aOrcamentoPeD =  unname(parametros[grep("aOrcamentoPeD", x = names(parametros))][c(4,1,2,3)])
    ,aPercPeDAberto =  unname(parametros[grep("aPercPeDAberto", x = names(parametros))][c(4,1,2,3)])
    # A Initial Price
    ,aInitialPrice = unname(parametros[grep("aInitialPrice", x = names(parametros))])
    ,aPatentShare = unname(parametros[grep("aPatentShare", x = names(parametros))])
    ,aInitialSharePlayers = unname(parametros[grep("aInitialSharePlayers", x = names(parametros))])
    
    
    # Outras Variáveis.
    ,aDiscountRate = unname(parametros["aDiscountRate"])
    ,aNormalDeliveryDelay = unname(parametros["aNormalDeliveryDelay"])
    ,aSwitchForCapacity = unname(parametros["aSwitchForCapacity"])
    # Vamos testar apenas um parâmetro por enquanto
    ,aFractionalDiscardRate = unname(parametros["aFractionalDiscardRate"]) # unname(pars["aFractionalDiscardRate"]) # Original 0.1
    ,aInitialDiffusionFraction = unname(parametros["aInitialDiffusionFraction"])
    ,aReferencePrice = unname(parametros["aReferencePrice"])
    ,aReferenceIndustryDemandElasticity = unname(parametros["aReferenceIndustryDemandElasticity"])
    ,aReferencePopulation = unname(parametros["aReferencePopulation"])
    ,aInnovatorAdoptionFraction = unname(parametros["aInnovatorAdoptionFraction"])
    ,aWOMStrength = unname(parametros["aWOMStrength"]) # unname(pars["aWOMStrength"]) # Original 1
    ,aPopulation = unname(parametros["aPopulation"]) #100000000 # Original Sterman: 100000000
    ,aUnitsPerHousehold = unname(parametros["aUnitsPerHousehold"])
    ,aSwitchForShipmentsInForecast = unname(parametros["aSwitchForShipmentsInForecast"])
    ,aVolumeReportingDelay = unname(parametros["aVolumeReportingDelay"])
    ,aForecastHorizon = unname(parametros["aForecastHorizon"])
    ,aCapacityAcquisitionDelay = unname(parametros["aCapacityAcquisitionDelay"])
    ,aTimeForHistoricalVolume = unname(parametros["aTimeForHistoricalVolume"])
    # Market Sector
    ,aReferenceDeliveryDelay = unname(parametros["aReferenceDeliveryDelay"])
    ,aSensOfAttractToAvailability = unname(parametros["aSensOfAttractToAvailability"])
    ,aSensOfAttractToPrice = unname(parametros["aSensOfAttractToPrice"])
    # Learning Curve Params
    ,aLCStrength = unname(parametros["aLCStrength"])
    ,aInitialProductionExperience = rep(unname(parametros["aInitialProductionExperience"]), times = N_PLAYERS)
    ,aRatioOfFixedToVarCost = unname(parametros["aRatioOfFixedToVarCost"])
    ,aNormalProfitMargin = unname(parametros["aNormalProfitMargin"])
    ,aNormalCapacityUtilization = unname(parametros["aNormalCapacityUtilization"])
    #Target Capacity Sector
    ,aMinimumEfficientScale = unname(parametros["aMinimumEfficientScale"]) # Original 100000
    
    # Esta variavel é desdobrada por player.
    ,aWeightOnSupplyLine= unname(parametros["aWeightOnSupplyLine"])
    ,aTimeToPerceiveCompTargetCapacity = unname(parametros["aTimeToPerceiveCompTargetCapacity"])
    
    # Price Sector
    ,aPriceAdjustmentTime = unname(parametros["aPriceAdjustmentTime"])
    ,aSensOfPriceToCosts = unname(parametros["aSensOfPriceToCosts"])
    ,aSensOfPriceToDSBalance = unname(parametros["aSensOfPriceToDSBalance"])
    ,aSensOfPriceToShare = unname(parametros["aSensOfPriceToShare"])
    # Capacity Sector
    ,aSwitchForPerfectCapacity = unname(parametros["aSwitchForPerfectCapacity"])
    
    # Pesquisa e Desenvolvimento
    ,aPeDLigado = unname(parametros["aPeDLigado"])
    
    ,aTempoMedioRealizacaoPeD = unname(parametros["aTempoMedioRealizacaoPeD"])
    ,aCustoMedioPatente = unname(parametros["aCustoMedioPatente"])
    ,aTempoMedioAvaliacao = unname(parametros["aTempoMedioAvaliacao"])
    ,aTaxaRejeicao = unname(parametros["aTaxaRejeicao"])
    ,aTempoVencimentoPatentes = unname(parametros["aTempoVencimentoPatentes"])
    ,aTempodeInutilizacaoPatente = unname(parametros["aTempodeInutilizacaoPatente"])
    ,aPerfSlope = unname(parametros["aPerfSlope"])
    ,aPerfMin = unname(parametros["aPerfMin"])
    ,aPerfMax = unname(parametros["aPerfMax"])
    ,aSensOfAttractToPerformance = unname(parametros["aSensOfAttractToPerformance"])
    ,aReferencePerformance = unname(parametros["aReferencePerformance"])
    
    ,aInitialInvestimentoNaoRealizadoPeD = rep(unname(parametros["aInitialInvestimentoNaoRealizadoPeD"]), times = N_PLAYERS)
    ,aInitialPatentesRequisitadas = rep(unname(parametros["aInitialPatentesRequisitadas"]), times = N_PLAYERS)
    ,aInitialPatentesEmpresa = rep(unname(parametros["aInitialPatentesEmpresa"]), times = N_PLAYERS)
    ,aInitialsPatentesEmDominioPublicoUteis = unname(parametros["aInitialsPatentesEmDominioPublicoUteis"])
    ,aInitialsInvestimentoPeDDepreciar = rep(unname(parametros["aInitialsInvestimentoPeDDepreciar"]), times = N_PLAYERS)
    ,aInitialPatentLefts = unname(parametros["aInitialPatentLefts"])
    # Novas Variáveis de Condições Iniciais:
    
    ,aInitialReorderShare =unname(parametros["aInitialReorderShare"])
    ,aTotalInitialInstalledBase = unname(parametros["aTotalInitialInstalledBase"])
    ,aInitialIndustryShipments = unname(parametros["aInitialIndustryShipments"])
    ,aModoInitialCumulativeAdopters = unname(parametros["aModoInitialCumulativeAdopters"])
    
    
    # Variáveis Adicionais
    ,Scenario = unname(parametros["Scenario"])
    ,Lever = unname(parametros["Lever"])
  )
  
  
  ##### VARIÁVEIS DE ENTRADA - ESTOQUES INICIAIS, SEM AJUSTES #####
  
  # Informando Estoques Iniciais, sem ajustes, apenas para calcular o primeiro tempo.
  stocks_iniciais  <- c(
    sNPVProfit = rep(0, times = N_PLAYERS)
    ,sValueOfBacklog = rep(100000, times = N_PLAYERS)
    ,sBacklog = rep(100, times = N_PLAYERS) 
    ,sInstalledBase = rep(2000, times = N_PLAYERS)  # rep(30000, times = N_PLAYERS) # Este estoque possui uma fórmula, verificar como fazer aqui no R.
    ,sPrice = unname(auxs$aInitialPrice)
    ,sCumulativeAdopters = 2000 # Este estoque possui uma fórmula, verificar como fazer aqui no R.
    # Teste 28/12/10:05: Removendo a Replicação Inicial desta variável.
    # ,sReportedIndustryVolume = rep(101904, times = N_PLAYERS)
    ,sReportedIndustryVolume = 100
    ,sCumulativeProduction = rep(1e+007, times = N_PLAYERS) # Este estoque possui formula
    ,sPerceivedCompTargetCapacity = rep(1000, times = N_PLAYERS) # Este estoque possui formula
    ,sSmoothCapacity1 = rep(100, times = N_PLAYERS) # Este estoque possui formula
    ,sSmoothCapacity2 = rep(100, times = N_PLAYERS) # Este estoque possui formula
    ,sSmoothCapacity3 = rep(100, times = N_PLAYERS) # Este estoque possui formula
    
    ,sInvestimentoNaoRealizadoPeD = rep(1000, times = N_PLAYERS)
    ,sPatentesRequisitadas = rep(100, times = N_PLAYERS)
    ,sPatentesEmpresa = rep(100, times = N_PLAYERS)
    ,sPatentesEmDominioPublicoUteis = 200
    ,sInvestimentoPeDDepreciar = rep(1000, times = N_PLAYERS)
    ,sPatentLefts = 0
    
  ) 
  
  # Calculando estoques para o t0.
  iteracoes_aquecimento_estoques = 3
  
  for(i in 1:iteracoes_aquecimento_estoques){
    
    # Criando List com variaveis globais - Antes de Inicializar o Modelo.
    list.variaveis.globais <<- list(
      # sReportedIndustryVolume = matrix(NA, ncol = N_PLAYERS, nrow = n_tempo),
      sReportedIndustryVolume = matrix(NA, ncol = 1, nrow = n_tempo),
      aExpectedIndustryDemand = matrix(NA, ncol = 1, nrow = n_tempo),
      aIndustryShipments = matrix(NA, ncol = 1, nrow = n_tempo)
    )
    
    estoques_calculados = modelo(time = START, stocks = stocks_iniciais, auxs = auxs, modo = "inicial")
    
    stocks_iniciais  <- c(
      sNPVProfit = unname(stocks_iniciais[grep("sNPVProfit", x = names(stocks_iniciais))]) 
      ,sValueOfBacklog = unname(estoques_calculados$ValueOfBacklogIni)
      ,sBacklog = unname(estoques_calculados$BacklogIni)
      ,sInstalledBase = unname(estoques_calculados$InstalledBaseIni)
      ,sPrice = unname(stocks_iniciais[grep("sPrice", x = names(stocks_iniciais))])
      ,sCumulativeAdopters = unname(estoques_calculados$CumulativeAdoptersIni)
      ,sReportedIndustryVolume = unname(estoques_calculados$ReportedIndustryVolumeIni)
      ,sCumulativeProduction = unname(estoques_calculados$CumulativeProductionIni)
      ,sPerceivedCompTargetCapacity = unname(estoques_calculados$PerceivedCompTargetCapacityIni)
      ,sSmoothCapacity1 = unname(estoques_calculados$CapacityIni)
      ,sSmoothCapacity2 = unname(estoques_calculados$CapacityIni)
      ,sSmoothCapacity3 = unname(estoques_calculados$CapacityIni)
      ,sInvestimentoNaoRealizadoPeD = unname(estoques_calculados$InitialInvestimentoNaoRealizadoPeD)
      ,sPatentesRequisitadas = unname(estoques_calculados$InitialPatentesRequisitadas)
      ,sPatentesEmpresa = unname(estoques_calculados$InitialPatentesEmpresa)
      ,sPatentesEmDominioPublicoUteis = unname(estoques_calculados$InitialsPatentesEmDominioPublicoUteis)
      ,sInvestimentoPeDDepreciar = unname(estoques_calculados$InitialsInvestimentoPeDDepreciar)
      ,sPatentLefts = unname(estoques_calculados$IntialPatentLefts)
    ) 
    
  }
  
  stocks = stocks_iniciais
  
  # Criando List Novamente, para manter a list limpa.
  list.variaveis.globais <<- list(
    # sReportedIndustryVolume = matrix(NA, ncol = N_PLAYERS, nrow = n_tempo),
    sReportedIndustryVolume = matrix(NA, ncol = 1, nrow = n_tempo),
    aExpectedIndustryDemand = matrix(NA, ncol = 1, nrow = n_tempo),
    aIndustryShipments = matrix(NA, ncol = 1, nrow = n_tempo)
  )
  
  
  resultado_completo = data.frame(deSolve::ode(y=stocks, simtime, func = modelo, 
                                               parms=auxs, method="euler"))
  # Posso filtrar os resultados ou não:
  # resultado_completo[variaveis_calibracao]
  resultado_completo
}

##### MODELO ####
#' modelo
#'
#' Esta função contém o modelo de equações diferenciais empregado pelo trabalho. Este modelo específico foi baseado no modelo de Sterman (2007) (Getting Big Too Fast: Strategic Dynamics with Increasing Returns and Bounded Rationality) e possui diveras modificações para representar o caso da manufatura aditiva (destacando-se o setor de P&D).
#' @param time tempo a ser simulado (inteiro)
#' @param stocks objeto com estoques a serem computados pelo modelo
#' @param auxs objeto com vetor de parâmetros utilizados pelo modelo
#' @param modo "completo" (realiza a simulação completa) ou "inicial" (calcula valores iniciais de estoques).
#'
#' @return matriz com resultados das equações (de um dt).
#' @export
#'
modelo <- function(time, stocks, auxs, modo = "completo"){
  with(as.list(c(stocks, auxs)),{
    
    
    # Variáveis Necessárias para tratar período histórico de modo diferente.
    if(SIMULAR_HISTORICO_DIFERENTE){
      ano_futuro_inicial = ANO_INICIO_AVALIACAO  
    } else {
      ano_futuro_inicial = START
    }
    
    
    ## Browser: Se o Tempo for igual ao Start, verificar se existe algum estoque negativo.
    if(time == START){
      if(any(stocks<0)){
        browser ()
      }
    }
    
    variaveis_periodo_historico = list(
      # Todas as Estratégias no Cenário Base são agressivas
      aSwitchForCapacityStrategy = rep(1, times = N_PLAYERS),
      
      # O Lucro não é computado no período Histórico,
      fNPVProfitChange = rep(0, times = N_PLAYERS),
      
      # Nenhum Player possui política de PeD aberto, com excessão do player 4 (outros).
      aPercPeDAberto = c(rep(0, times = N_PLAYERS-1), 0.1),
      
      # Os players desejam o mesmo market share inicial que tem inicialmente.
      aDesiredMarketShare = unname(aInitialSharePlayers),
      
      # O Orçamento de P e D de todos os players é 0.06 (valor inicial da 3D Systems).
      aOrcamentoPeD = rep(0.6, N_PLAYERS)
    )
    
    # Variáveis que podem ser definidas no início (são parâmetros não calculados):
    # Se estou no período de histórico da simulação, usar parâmetros do período histórico.
    if(time < ano_futuro_inicial) {
      aSwitchForCapacityStrategy = variaveis_periodo_historico$aSwitchForCapacityStrategy
      aPercPeDAberto = variaveis_periodo_historico$aPercPeDAberto
      aDesiredMarketShare = variaveis_periodo_historico$aDesiredMarketShare
      aOrcamentoPeD = variaveis_periodo_historico$aOrcamentoPeD
    }
    
    
    
    # Criando uma variavel n_tempo local
    n_tempo = nrow(list.variaveis.globais$sReportedIndustryVolume)
    
    
    ##### VETORIZANDO ESTOQUES #####
    #Estoques Vetorizados = substituindo estoques pela forma vetorizada (pra que seja possivel formular equações de forma mais simples).
    # Esta implementação tem por objetivo não gerar a necessidade de referenciar os estoque spelo seu nome único
    sNPVProfit = stocks[grep("sNPVProfit", x = names(stocks))]
    sValueOfBacklog = stocks[grep("sValueOfBacklog", x = names(stocks))]
    sBacklog = stocks[grep("sBacklog", x = names(stocks))]
    sInstalledBase = stocks[grep("sInstalledBase", x = names(stocks))]
    sPrice = stocks[grep("sPrice", x = names(stocks))]
    sCumulativeAdopters = stocks[grep("sCumulativeAdopters", x = names(stocks))]
    sReportedIndustryVolume = stocks[grep("sReportedIndustryVolume", x = names(stocks))]
    sCumulativeProduction = stocks[grep("sCumulativeProduction", x = names(stocks))]
    sPerceivedCompTargetCapacity = stocks[grep("sPerceivedCompTargetCapacity", x = names(stocks))]
    sSmoothCapacity1 = stocks[grep("sSmoothCapacity1", x = names(stocks))]
    sSmoothCapacity2 = stocks[grep("sSmoothCapacity2", x = names(stocks))]
    sSmoothCapacity3 = stocks[grep("sSmoothCapacity3", x = names(stocks))]
    
    sInvestimentoNaoRealizadoPeD = stocks[grep("sInvestimentoNaoRealizadoPeD", x = names(stocks))]
    sPatentesRequisitadas = stocks[grep("sPatentesRequisitadas", x = names(stocks))]
    sPatentesEmpresa = stocks[grep("sPatentesEmpresa", x = names(stocks))]
    sPatentesEmDominioPublicoUteis = stocks[grep("sPatentesEmDominioPublicoUteis", x = names(stocks))]
    sInvestimentoPeDDepreciar = stocks[grep("sInvestimentoPeDDepreciar", x = names(stocks))]
    sPatentLefts = stocks[grep("sPatentLefts", x = names(stocks))]
    
    
    #Obtendo o número da linha no qual estou
    linha = ((time - START)* (n_tempo - 1)) / (FINISH - START) + 1
    
    # Gravando a Variável sReportedIndustryVolume no vetor global
    list.variaveis.globais$sReportedIndustryVolume[linha,] <<- sReportedIndustryVolume
    
    
    ##### DIFFUSION SECTOR  - PT 1 #####
    aDemandCurveSlope = - aReferenceIndustryDemandElasticity * (aReferencePopulation / aReferencePrice )
    
    aLowestPrice = min(sPrice)
    
    aIndustryDemand = min(
      aPopulation,
      aReferencePopulation * max(
        0,
        1 + aDemandCurveSlope * (aLowestPrice - aReferencePrice) / aReferencePopulation
      )
    )
    
    checkIndustryDemand = aIndustryDemand
    
    
    ##### CONDIÇÕES INICIAIS - CUMULATIVE ADOPTERS #####
    
    # Calculando Variáveis Necessárias para Definir os Cumulative Adopters
    
    aTotalInitialInstalledBase = min(aIndustryDemand,
                                     (aInitialReorderShare * aInitialIndustryShipments) / aFractionalDiscardRate) 
    
    aEstimatedAdopters = aTotalInitialInstalledBase / aUnitsPerHousehold
    
    aInitialNewAdoptersOrderRate = aInitialIndustryShipments*(1-aInitialReorderShare)
    
    aInitialAdoptionRate = aInitialNewAdoptersOrderRate / aUnitsPerHousehold
    
    
    
    # Initial Cumulative Adopters 1 - Opção Original
    aInitialCumulativeAdopters1 = aInitialDiffusionFraction * aIndustryDemand
    
    
    # Initial Cumulative Adopters 2 - Opção Calculada pelo Reorder Share
    aInitialCumulativeAdopters2 = aTotalInitialInstalledBase / aUnitsPerHousehold
    
    
    # Initial Cumulative Adopters 3 - Opção calculada pelo Initial Adoption Rate:
    aInitialCumulativeAdopters3 = (aInitialAdoptionRate/(aIndustryDemand-aEstimatedAdopters))*(aPopulation/aWOMStrength)
    
    
    aInitialCumulativeAdopters = if(aModoInitialCumulativeAdopters == 1) {
      aInitialCumulativeAdopters1
    } else if (aModoInitialCumulativeAdopters == 2) {
      aInitialCumulativeAdopters2
    } else {
      aInitialCumulativeAdopters3
    }
    
    ##### DIFFUSION SECTOR  - PT 2 #####
    
    aNonAdopters = aIndustryDemand - sCumulativeAdopters
    
    checkNonAdopters = aNonAdopters
    
    # Ajuste temporário: Colocar o adoption Rate como Fluxo apenas positivo.
    
    fAdoptionRate = max(0, 
                        aNonAdopters * (aInnovatorAdoptionFraction + aWOMStrength * sCumulativeAdopters/aPopulation)) 
    
    checkAdoptionRate = fAdoptionRate
    
    ##### ORDERS SECTOR - PT 1 #####
    
    fDiscardRate = sInstalledBase * aFractionalDiscardRate
    
    ##### INDUSTRY DEMAND SECTOR #####
    
    fReorderRate = sum(fDiscardRate)
    
    aInitialOrderRate = aUnitsPerHousehold * fAdoptionRate
    
    fIndustryOrderRate = fReorderRate + aInitialOrderRate
    
    # if(modo == "completo") {
    #   browser()
    # }
    
    
    checkIndustryOrderRate = fIndustryOrderRate
    
    ##### ORDERS SECTOR - PT 2 #####
    
    aDesiredShipments = sBacklog / aNormalDeliveryDelay
    
    ### CAPACITY SECTOR - PT 1 ####
    
    aCapacity = aSwitchForPerfectCapacity * (aDesiredShipments / aNormalCapacityUtilization) + (1-aSwitchForPerfectCapacity) * sSmoothCapacity3
    
    aNormalProduction = aCapacity * aNormalCapacityUtilization
    
    aIndustryNormalProduction = sum(aNormalProduction)
    
    ##### ORDERS SECTOR - PT 3 #####
    
    fShipments = aSwitchForCapacity * pmin(aDesiredShipments, aCapacity) + (1-aSwitchForCapacity) * aDesiredShipments
    
    aCapacityUtilization = fShipments / aCapacity
    
    aIndustryShipments = sum(fShipments)
    
    list.variaveis.globais$aIndustryShipments[linha,] <<- aIndustryShipments
    
    # Calculando a Variação no Industry Shipments - Ajuda a Calcular a Variação Percentual em Demanda para Avaliar a Plausibilidade do Modelo (principalmente em relação às condições iniciais).
    VariacaoDemanda = if(time == START){
      ((aIndustryShipments - aInitialIndustryShipments) / aInitialIndustryShipments)
    } else {
      ((aIndustryShipments - list.variaveis.globais$aIndustryShipments[linha-1,]) / list.variaveis.globais$aIndustryShipments[linha-1,])
    }
    
    # if(modo == "completo"){
    #   browser()
    # }
    
    aMarketShare = fShipments / aIndustryShipments
    
    aDeliveryDelay = sBacklog / fShipments
    
    checkIndustryShipments = aIndustryShipments
    
    ##### MARKET SECTOR #####
    
    # Patentes e Performance
    
    
    
    aPatentesEmpresaTemAcesso = sPatentesRequisitadas + sPatentesEmpresa + sPatentesEmDominioPublicoUteis + sPatentLefts
    
    aPerformanceCalculada = aPerfSlope * aPatentesEmpresaTemAcesso
    
    aPerformance = pmax(aPerfMin, pmin(aPerfMax, aPerformanceCalculada))
    
    checkPerformance = mean(aPerformance)
    
    aAttractivenessFromPerformance = aPeDLigado * exp(aSensOfAttractToPerformance*(aReferencePerformance/aPerformance)) + (1 - aPeDLigado)
    
    aAttractivenessFromAvailability = exp(aSensOfAttractToAvailability*(aDeliveryDelay/aReferenceDeliveryDelay))
    
    aAttractivenessFromPrice = exp(aSensOfAttractToPrice*(sPrice/aReferencePrice))
    
    aAttractiveness = aAttractivenessFromAvailability * aAttractivenessFromPrice * aAttractivenessFromPerformance
    
    aTotalAttractiveness = sum(aAttractiveness)
    
    aOrderShare = aAttractiveness / aTotalAttractiveness
    
    # if(time == FINISH){
    #   browser()  
    # }
    # 
    
    
    ##### ORDERS SECTOR - PT 3 #####
    
    fOrders = fIndustryOrderRate * aOrderShare
    
    checkOrders = sum(fOrders)
    
    ##### EXPECTED INDUSTRY DEMAND SECTOR #####
    
    aInitialDemandForecast = fReorderRate
    
    aIndustryVolume = pmax(aInitialDemandForecast,
                           aSwitchForShipmentsInForecast*aIndustryShipments+
                             (1-aSwitchForShipmentsInForecast)*fIndustryOrderRate)
    
    
    # Variavel com SMOOTH - Primeira Ordem: - Retirando o DT, o calculo funcionou corretamente!
    fsmooth_ReportedIndustryVolume = ((aIndustryVolume - sReportedIndustryVolume) / aVolumeReportingDelay) # * STEP # Multiplicando pelo step para ajustar o calculo.
    
    # Variavel com DELAY - A definição das constantes aqui devem ser alteradas se as condicoes iniciais do modelo mudarem
    # Esta implementacao considera que os delays sempre serao iguais. Se os delays nao forem iguais, deve-se encontrar outra forma de implementar os delays (talvez com a equacao multiplicativa 1*(time > tempodelay)
    if((time - START) > aTimeForHistoricalVolume) {
      nlinhas_delay = aTimeForHistoricalVolume / STEP
      aLaggedIndustryVolume = list.variaveis.globais$sReportedIndustryVolume[(linha - nlinhas_delay),]
    } else {
      aLaggedIndustryVolume = list.variaveis.globais$sReportedIndustryVolume[1,]
    }
    
    if(aLaggedIndustryVolume < 0) {
      browser()
    }
    
    aExpGrowthInVolume =  log(sReportedIndustryVolume/aLaggedIndustryVolume)/aTimeForHistoricalVolume
    
    aExpectedIndustryDemand = sReportedIndustryVolume*exp(aForecastHorizon*aCapacityAcquisitionDelay*aExpGrowthInVolume)
    
    list.variaveis.globais$aExpectedIndustryDemand[linha,] <<- aExpectedIndustryDemand
    
    
    # Mais uma variável com delay
    if((time - START) > aCapacityAcquisitionDelay) {
      nlinhas_delay = aCapacityAcquisitionDelay / STEP
      aLaggedVolumeForecast = list.variaveis.globais$aExpectedIndustryDemand[linha-nlinhas_delay,]
    } else {
      aLaggedVolumeForecast = list.variaveis.globais$aExpectedIndustryDemand[1,]
    }
    
    aForecastError = (aLaggedVolumeForecast - aIndustryVolume)/(1e-009+aIndustryVolume)
    
    checkLaggedVolumeForecast = mean(aLaggedVolumeForecast)
    
    ##### TARGET CAPACITY SECTOR #####
    
    aIndustryCapacity = sum(aCapacity)
    
    aCompetitorCapacity = aIndustryCapacity - aCapacity
    
    aExpectedCompCapacity = aNormalCapacityUtilization*(aWeightOnSupplyLine*sPerceivedCompTargetCapacity+(1-aWeightOnSupplyLine)*aCompetitorCapacity)
    
    aUncontestedDemand = pmax(0, aExpectedIndustryDemand - aExpectedCompCapacity)
    
    aUncontestedMarketShare = aUncontestedDemand / aExpectedIndustryDemand
    
    # Definindo Estratégia no Cenário Base
    
    
    
    aSwitchForCapacityStrategy1 = ifelse(aSwitchForCapacityStrategy == 1, 1, 0)
    aSwitchForCapacityStrategy2 = ifelse(aSwitchForCapacityStrategy == 2, 1, 0)
    aSwitchForCapacityStrategy3 = ifelse(aSwitchForCapacityStrategy == 3, 1, 0)
    aSwitchForCapacityStrategy4 = ifelse(aSwitchForCapacityStrategy == 4, 1, 0)
    
    aTargetMarketShare = {
      aSwitchForCapacityStrategy1*pmax(aDesiredMarketShare,aUncontestedMarketShare) +
        aSwitchForCapacityStrategy2*pmin(aDesiredMarketShare,aUncontestedMarketShare) +
        aSwitchForCapacityStrategy3*aDesiredMarketShare +
        aSwitchForCapacityStrategy4*aUncontestedMarketShare
    }
    
    
    
    aTargetCapacity = pmax(aMinimumEfficientScale,
                           aTargetMarketShare*aExpectedIndustryDemand/aNormalCapacityUtilization)
    
    
    aTargetNormalProduction = aTargetCapacity * aNormalCapacityUtilization
    
    aIndustryTotalTargetCapacity = sum(aTargetCapacity)
    
    aCompetitorTargetCapacity = aIndustryTotalTargetCapacity - aTargetCapacity
    
    fChangePerceivedCompTargetCapacity = (aCompetitorTargetCapacity - sPerceivedCompTargetCapacity) / aTimeToPerceiveCompTargetCapacity
    
    checkCompetitorTargetCapacity = mean(aCompetitorTargetCapacity)
    
    ##### CAPACITY SECTOR  - PT 2 - FLUXOS #####
    fchangeSmoothCapacity1 = (aTargetCapacity - sSmoothCapacity1) / (aCapacityAcquisitionDelay / 3)
    fchangeSmoothCapacity2 = (sSmoothCapacity1 - sSmoothCapacity2) / (aCapacityAcquisitionDelay / 3)
    fchangeSmoothCapacity3 = (sSmoothCapacity2 - sSmoothCapacity3) / (aCapacityAcquisitionDelay / 3)
    
    
    
    ##### Custo P e D ####
    aTempoDepreciacao = aTempoMedioAvaliacao + aTempoVencimentoPatentes + aTempoMedioRealizacaoPeD
    
    fDepreciacaoInvPeD = sInvestimentoPeDDepreciar / aTempoDepreciacao
    
    aPeDUnitCost = fDepreciacaoInvPeD / fShipments
    
    
    
    
    ##### LEARNING CURVE SECTOR #####
    fProduction = fShipments
    
    aLCExponent = log(aLCStrength)/log(2)
    
    aLearning = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent
    
    aInitialUnitFixedCost = (aInitialPrice/(1+aNormalProfitMargin))*aRatioOfFixedToVarCost*(1/(1+aRatioOfFixedToVarCost/aNormalCapacityUtilization))
    
    aInitialUnitVariableCost = (aInitialPrice/(1+aNormalProfitMargin))*(1/(1+aRatioOfFixedToVarCost/aNormalCapacityUtilization))
    
    aUnitFixedCost = aLearning * aInitialUnitFixedCost + aPeDUnitCost * aPeDLigado
    
    aUnitVariableCost = aLearning * aInitialUnitVariableCost
    
    checkUnitFixedCost = mean(aUnitFixedCost)
    
    checkUnitVariableCost = mean(aUnitVariableCost)
    
    ##### PRICE SECTOR #####
    
    aBasePrice = (1+aNormalProfitMargin)*(aUnitVariableCost+aUnitFixedCost/aNormalCapacityUtilization)
    
    aDemandSupplyBalance = aDesiredShipments/(aNormalCapacityUtilization*aCapacity)
    
    # Trava do Preço: Os players nunca precificarão acima de 2 vezes o preço inicial
    
    aTargetPrice = 
      pmin(aInitialPrice * 2,
           pmax(aUnitVariableCost,
                sPrice*
                  (1+aSensOfPriceToCosts*((aBasePrice/sPrice)-1))*
                  (1+aSensOfPriceToDSBalance*(aDemandSupplyBalance-1))*
                  (1+aSensOfPriceToShare*((aTargetMarketShare-aMarketShare))))
      )
    
    
    checkTargetPrice = mean(aTargetPrice)
    
    fChangeInPrice = (aTargetPrice - sPrice) / aPriceAdjustmentTime
    
    # if(time == FINISH){
    #   browser()  
    # }
    # 
    
    ##### NET INCOME SECTOR #####
    
    aDiscountFactor = exp(-aDiscountRate*(time - ano_futuro_inicial)) # 
    
    fValueOfNewOrders = fOrders * sPrice
    
    checkValueOfNewOrders1 = fValueOfNewOrders[1] #
    
    aAveragePriceOfOrderBook = sValueOfBacklog / sBacklog
    
    fRevenue = fShipments * aAveragePriceOfOrderBook #
    
    ##### P&D - Investimento #####
    
    fInvestimentoPeD = fRevenue * aOrcamentoPeD * aPeDLigado
    
    fInvestimentoPeDRealizado = sInvestimentoNaoRealizadoPeD / aTempoMedioRealizacaoPeD
    
    fPatentesSolicitadas = (fInvestimentoPeDRealizado) / aCustoMedioPatente
    
    fPatentesRejeitadas = (sPatentesRequisitadas/aTempoMedioAvaliacao) * aTaxaRejeicao
    
    fPatentesConcedidas = (sPatentesRequisitadas/aTempoMedioAvaliacao) * (1-aTaxaRejeicao) * (1-aPercPeDAberto)
    
    fPatentLeftsGeradas = (sPatentesRequisitadas/aTempoMedioAvaliacao) * (1-aTaxaRejeicao) * (aPercPeDAberto)
    
    fPatentLeftsVencidas = sPatentLefts / aTempoVencimentoPatentes
    
    # Estou somando as Patentes com investimenti (direto em domínio publico na equação abaixo, sem passar por outros estoques).
    # Eventualmente é possível modelar este comportamento passando por outros estoques.
    fPatentesVencidas = sPatentesEmpresa / aTempoVencimentoPatentes 
    
    #fPatentesAbertas = ((aPercPeDAberto * fInvestimentoPeDRealizado) / aCustoMedioPatente) * (1-aTaxaRejeicao)
    
    fPatentesUtilidadeExpirada = sPatentesEmDominioPublicoUteis / aTempodeInutilizacaoPatente
    
    ##### NET INCOME - PARTE 2 #####
    
    checkRevenue1 = fRevenue[1] #
    
    aVariableCost = fShipments * aUnitVariableCost #
    
    aFixedCost = aCapacity * (aUnitFixedCost - (aPeDUnitCost * aPeDLigado)) #
    
    fCost = aFixedCost + aVariableCost #
    
    fNetIncome = fRevenue - fCost - fInvestimentoPeD #
    
    fNPVProfitChange = fNetIncome * aDiscountFactor #
    
    # Considerar a Mudança do Lucro igual a zero enquanto a simulação estiver no período histórico.
    if(time < ano_futuro_inicial) {
      fNPVProfitChange = variaveis_periodo_historico$fNPVProfitChange
    }
    
    
    # if(modo=="completo" & time %in% c(2008, 2019)){
    #   browser()
    # }
    
    checkNPVProfitChange = mean(fNPVProfitChange) #
    
    checkNPVProfitChange1 = fNPVProfitChange[1]
    
    aNPVIndustryProfits = sum(sNPVProfit) #
    
    
    ##### ESTOQUES #####
    
    d_NPVProfit_dt = fNPVProfitChange
    
    d_ValueOfBacklog_dt = fValueOfNewOrders - fRevenue
    
    d_Backlog_dt = fOrders - fShipments
    
    d_InstalledBase_dt = fShipments - fDiscardRate
    
    d_Price_dt = fChangeInPrice
    
    d_CumulativeAdopters_dt = fAdoptionRate
    
    d_sReportedIndustryVolume_dt = fsmooth_ReportedIndustryVolume
    
    d_CumulativeProduction_dt = fProduction
    
    d_PerceivedCompTargetCapacity_dt = fChangePerceivedCompTargetCapacity
    
    d_SmoothCapacity1_dt = fchangeSmoothCapacity1
    
    d_SmoothCapacity2_dt = fchangeSmoothCapacity2
    
    d_SmoothCapacity3_dt = fchangeSmoothCapacity3
    
    #Estoques do Investimento em PeD
    
    d_InvestimentoNaoRealizadoPeD_dt = fInvestimentoPeD - fInvestimentoPeDRealizado
    
    d_PatentesRequisitadas_dt = fPatentesSolicitadas - fPatentesConcedidas - fPatentesRejeitadas - fPatentLeftsGeradas
    
    d_PatentesEmpresa_dt = fPatentesConcedidas - fPatentesVencidas
    
    d_PatentesEmDominioPublicoUteis_dt = sum(fPatentesVencidas) + fPatentLeftsVencidas - fPatentesUtilidadeExpirada
    
    d_InvestimentoPeDDepreciar_dt = fInvestimentoPeD - fDepreciacaoInvPeD
    
    d_PatentLefts_dt = sum(fPatentLeftsGeradas) - fPatentLeftsVencidas
    
    
    # Variaveis de Estoques Iniciais
    
    aInitialOrderRateCalibracao = if(aModoInitialCumulativeAdopters == 1){
      fIndustryOrderRate
    } else {
      aInitialIndustryShipments
    }
    
    # Alteração para Calibração de dados Iniciais: Calibrar o Backlog inicial com a Demanda Inicial Informada.
    # BacklogIni = aInitialSharePlayers * fIndustryOrderRate * aNormalDeliveryDelay
    BacklogIni = aInitialSharePlayers * aInitialOrderRateCalibracao * aNormalDeliveryDelay
    
    # Alteração Importante para a Calibração dos Dados Iniciais do Modelo!
    # A variável de Base de Usuários inicial deve partir da mesma estimativa
    
    # Antes da Alteração:
    # InstalledBaseIni = aInitialCumulativeAdopters * aInitialSharePlayers * aUnitsPerHousehold
    
    #Depois:
    # O Reorder Share deve depender do tempo de vida médio:
    # 
    InstalledBaseIni = aInitialCumulativeAdopters * aInitialSharePlayers
    
    CumulativeAdoptersIni = aInitialCumulativeAdopters
    
    # Alterando o Valor do Backlog para considerar a demanda inicial:
    ValueOfBacklogIni = aInitialSharePlayers * aInitialOrderRateCalibracao * aNormalDeliveryDelay * aInitialPrice
    
    ReportedIndustryVolumeIni = aIndustryVolume
    
    CumulativeProductionIni = aInitialProductionExperience
    
    PerceivedCompTargetCapacityIni = aCompetitorCapacity
    
    # Alterando a Capacidade para Considerar a Demanda Inicial
    CapacityIni = aInitialSharePlayers * aInitialOrderRateCalibracao / aNormalCapacityUtilization
    
    InitialInvestimentoNaoRealizadoPeD = aInitialInvestimentoNaoRealizadoPeD * aPatentShare
    
    InitialPatentesRequisitadas = aInitialPatentesRequisitadas * aPatentShare
    
    InitialPatentesEmpresa = aInitialPatentesEmpresa * aPatentShare
    
    InitialsPatentesEmDominioPublicoUteis =  aInitialsPatentesEmDominioPublicoUteis
    
    InitialsInvestimentoPeDDepreciar = aInitialsInvestimentoPeDDepreciar * aPatentShare
    
    IntialPatentLefts = aInitialPatentLefts
    
    
    ##### ESTOQUES - INICIAIS #####
    
    if(INICIALIZAR_ESTOQUES_COM_CASO_BASE){
      
      stocks_ini = list(
        BacklogIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sBacklog", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        InstalledBaseIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sInstalledBase", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        CumulativeAdoptersIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sCumulativeAdopters", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        ValueOfBacklogIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sValueOfBacklog", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        ReportedIndustryVolumeIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sReportedIndustryVolume", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        CumulativeProductionIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sCumulativeProduction", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        PerceivedCompTargetCapacityIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sPerceivedCompTargetCapacity", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        CapacityIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sSmoothCapacity3", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        
        InitialInvestimentoNaoRealizadoPeD = VARIAVEIS_FINAIS_CASO_BASE[grep("sInvestimentoNaoRealizadoPeD", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        InitialPatentesRequisitadas = VARIAVEIS_FINAIS_CASO_BASE[grep("sPatentesRequisitadas", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        InitialPatentesEmpresa = VARIAVEIS_FINAIS_CASO_BASE[grep("sPatentesEmpresa", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        InitialsPatentesEmDominioPublicoUteis = VARIAVEIS_FINAIS_CASO_BASE[grep("sPatentesEmDominioPublicoUteis", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        InitialsInvestimentoPeDDepreciar = VARIAVEIS_FINAIS_CASO_BASE[grep("sInvestimentoPeDDepreciar", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        IntialPatentLefts = VARIAVEIS_FINAIS_CASO_BASE[grep("sIntialPatentLefts", x = names(VARIAVEIS_FINAIS_CASO_BASE))]
      )
      
      # Estes valores vieram como colunas e devem se transformar em vetores:
      stocks_ini = lapply(stocks_ini, transf_colunas_em_vetor)
      
      
    } else {
      stocks_ini = list(
        BacklogIni = BacklogIni,
        InstalledBaseIni = InstalledBaseIni,
        CumulativeAdoptersIni = CumulativeAdoptersIni,
        ValueOfBacklogIni = ValueOfBacklogIni,
        ReportedIndustryVolumeIni = ReportedIndustryVolumeIni,
        CumulativeProductionIni = CumulativeProductionIni,
        PerceivedCompTargetCapacityIni = PerceivedCompTargetCapacityIni,
        CapacityIni = CapacityIni,
        
        InitialInvestimentoNaoRealizadoPeD = InitialInvestimentoNaoRealizadoPeD,
        InitialPatentesRequisitadas = InitialPatentesRequisitadas,
        InitialPatentesEmpresa = InitialPatentesEmpresa,
        InitialsPatentesEmDominioPublicoUteis = InitialsPatentesEmDominioPublicoUteis,
        InitialsInvestimentoPeDDepreciar = InitialsInvestimentoPeDDepreciar,
        IntialPatentLefts = IntialPatentLefts
      )  
    }
    
    if(time == START){
      if(any(unlist(stocks_ini)<0)){
        browser()  
      }
    }
    
    ##### COMPARAR RESULTADOS COM O ITHINK #####
    
    if(VERIFICAR_STOCKS & modo == "completo"){
      for (variavel in variaveis_ithink_stocks) {
        # Definir o tipo de variavel
        # Variavel é um estoque?
        variavel_ithink_alterada = gsub(pattern = "\\[", replacement = "", x = variavel, ignore.case = TRUE)
        variavel_ithink_alterada = gsub(pattern = "\\]", replacement = "", x = variavel_ithink_alterada, ignore.case = TRUE)
        
        # Verificar apenas Estoques:
        variavel_ithink_alterada = paste("s", variavel_ithink_alterada, sep = "")
        
        # Valor da Variavel Calculada
        valor_variavel_R = eval(parse(text = variavel_ithink_alterada))
        
        valor_variavel_ithink = dados_ithink_stocks[[linha,variavel]]
        
        diferenca = valor_variavel_R - valor_variavel_ithink
        
        if (abs(x = diferenca) > CHECK_PRECISION){
          message(paste("Estoque Diff:", time, linha, variavel, diferenca, sep = " - "))
          if(BROWSE_ON_DIFF){
            browser()  
          }
        }
      }  
    }
    
    
    if(VERIFICAR_CHECKS & modo == "completo"){
      for (variavel in variaveis_ithink_checks) {
        # Definir o tipo de variavel
        # Variavel é um estoque?
        variavel_ithink_alterada = gsub(pattern = "\\[", replacement = "", x = variavel, ignore.case = TRUE)
        variavel_ithink_alterada = gsub(pattern = "\\]", replacement = "", x = variavel_ithink_alterada, ignore.case = TRUE)
        
        # Verificar apenas Estoques:
        #variavel_ithink_alterada = paste("s", variavel_ithink_alterada, sep = "")
        
        # Valor da Variavel Calculada
        valor_variavel_R = eval(parse(text = variavel_ithink_alterada))
        
        valor_variavel_ithink = dados_ithink_checks[[linha,variavel]]
        
        diferenca = valor_variavel_R - valor_variavel_ithink
        
        if(!is.na(diferenca)){
          if (abs(x = diferenca) > CHECK_PRECISION){
            message(paste("Check Diff:", time, linha, variavel, diferenca, sep = " - "))
            if(BROWSE_ON_DIFF){
              browser()  
            }
          }  
        }
        
      }
    }
    
    # Colocar isso dentro do IF abaixo e verificar!
    if(VERIFICAR_GLOBAL & modo == "completo"){
      
      # Forma que usa todas as variaveis do ambiente:
      # variaveis_disponiveis_ambiente = ls()
      # variaveis_auxiliares = variaveis_disponiveis_ambiente[grep("^[aA].*", variaveis_disponiveis_ambiente)]
      # 
      # Forma que usa as variaveis globais definidas em um vetor
      variaveis_auxiliares = variaveis_globais_a_verificar
      
      
      seletor_players = paste("[",1:N_PLAYERS,"]", sep = "")
      
      variaveis_a_verificar = expand.grid(variaveis_auxiliares, seletor_players)
      
      variaveis_a_verificar = paste(variaveis_a_verificar[,1], variaveis_a_verificar[,2], sep = "")
      
      variaveis_a_verificar_no_ithink = substring(variaveis_a_verificar, 2)
      
      
      verificar_variaveis_globais = function(n_variavel){
        valor_variavel_R = eval(parse(text = variaveis_a_verificar[n_variavel]))
        
        valor_variavel_ithink = dados_ithink_global[[linha,variaveis_a_verificar_no_ithink[n_variavel]]]
        
        if((length(valor_variavel_ithink) > 0)) {
          decisao = !is.na(valor_variavel_ithink) & is.numeric(valor_variavel_ithink)
          if(decisao == FALSE) {
            valor_variavel_ithink = NA
          }
        } else {valor_variavel_ithink = NA}
        
        
        if((length(valor_variavel_R) > 0)) {
          decisao = !is.na(valor_variavel_R) & is.numeric(valor_variavel_R)
          if(decisao == FALSE) {
            valor_variavel_R = NA
          }
        } else {valor_variavel_R = NA}
        
        
        diferenca = unname(valor_variavel_R)  - unname(valor_variavel_ithink)
        
        diferenca
      }
      
      diferencas = lapply(X = 1:length(variaveis_a_verificar), FUN = verificar_variaveis_globais)
      
      diferencas = do.call(rbind, diferencas)
      
      matriz_diferencas = data.frame(
        variaveis_a_verificar,
        diferencas
      )
      
      diferencas_a_reportar = subset(matriz_diferencas, abs(diferencas) > CHECK_PRECISION)
      
      if(nrow(diferencas_a_reportar)>1) {
        message(paste("Check Diferenças Globais:", time, linha, sep = " - "))
        if(BROWSE_ON_DIFF){
          browser()  
        }
      }
      
    }
    
    # if(modo == "completo" & time == FINISH) {
    #   browser()
    # }
    ##### VARIÁVEIS RETORNADAS #####
    
    resultado_completo = list(c(
      d_NPVProfit_dt
      ,d_ValueOfBacklog_dt
      ,d_Backlog_dt
      ,d_InstalledBase_dt
      ,d_Price_dt
      ,d_CumulativeAdopters_dt
      ,d_sReportedIndustryVolume_dt
      ,d_CumulativeProduction_dt
      ,d_PerceivedCompTargetCapacity_dt
      ,d_SmoothCapacity1_dt
      ,d_SmoothCapacity2_dt
      ,d_SmoothCapacity3_dt
      ,d_InvestimentoNaoRealizadoPeD_dt
      ,d_PatentesRequisitadas_dt
      ,d_PatentesEmpresa_dt
      ,d_PatentesEmDominioPublicoUteis_dt
      ,d_InvestimentoPeDDepreciar_dt
      ,d_PatentLefts_dt
    )
    ,fIndustryOrderRate = unname(fIndustryOrderRate) 
    ,aOrderShare = unname(aOrderShare)
    ,aPerformance = unname(aPerformance)
    ,aPatentesEmpresaTemAcesso = unname(aPatentesEmpresaTemAcesso)
    ,aNonAdopters = unname(aNonAdopters)
    ,fReorderRate = unname(fReorderRate) 
    ,aIndustryShipments = unname(aIndustryShipments)
    ,aIndustryVolume = unname(aIndustryVolume) 
    ,fNPVProfitChange = unname(fNPVProfitChange) 
    ,fNetIncome = unname(fNetIncome) 
    ,aNPVIndustryProfits = unname(aNPVIndustryProfits)
    ,VariacaoDemanda = unname(VariacaoDemanda)
    ,fInvestimentoPeD = unname(fInvestimentoPeD)
    ,aAttractivenessFromPerformance = unname(aAttractivenessFromPerformance)
    ,aAttractivenessFromAvailability = unname(aAttractivenessFromAvailability)
    ,aAttractivenessFromPrice = unname(aAttractivenessFromPrice)
    )
    
    
    return (if(modo == "inicial"){
      stocks_ini
    } else {
      resultado_completo
    })   
  })
}


#### OBJETO SDMODEL #####

# Nomeando o Dataframe de Saída (este vetor não é mais utilizado)
nomes_variaveis = c("Tempo", "d_NPVProfit_dt", "aDiscountFactor", "aDiscountRate", "fNPVProfitChange", "fNetIncome", "aNPVIndustryProfits")

# Inicializando um list com Tudo o que é necessário para a Simulação.
# Este objeto contém informações necessárias sobre o modelo para a sua simulação.
sdmodel = list(
  Start = START,
  Finish = FINISH,
  Step = STEP,
  SimTime = SIM_TIME,
  # Auxs = auxs,
  # Stocks = stocks,
  Modelo = modelo,
  Variaveis = nomes_variaveis
)
