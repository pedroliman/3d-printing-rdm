

##### GRÁFICOS ####
gerar_grafico_superficie = function(dados_ultimo_ano,variaveis, estrategia) {
  dadosplot = subset.data.frame(dados_ultimo_ano, (Lever == estrategia))
  
  dadosplot = dadosplot[variaveis]
  
  dadosplot = as.matrix(dadosplot)
  
  names = colnames(dadosplot)
  
  s = interp(dadosplot[,1],dadosplot[,2],dadosplot[,3])
  
  names(s) = names
  
  # Plotando a População Final
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "Taxa Nascimento",
    titlefont = f
  )
  y <- list(
    title = "TaxaMorte",
    titlefont = f
  )
  z <- list(
    title = "Populacao",
    titlefont = f
  )
  
  plot_ly(x = s[[1]], y = s[[2]], z = s[[3]]) %>% add_surface() %>% layout(xaxis = x, yaxis = y)
  
}

plot_clientes_uma_estrategia = function(dados, estrategia) {
  gr2_dados = subset(dados, (Lever == estrategia))
  ggplot2::ggplot(gr2_dados,
                  aes(x=Tempo, y=Adopters, color=factor(Lever), group=Scenario)) + 
    geom_line() + 
    ylab("Adopters") + 
    xlab("Time") +
    labs(color = "Strategy")
}

plot_cash_uma_estrategia = function(dados, estrategia) {
  gr2_dados = subset(dados, (Lever == estrategia))
  ggplot2::ggplot(gr2_dados,
                  aes(x=Tempo, y=Cash, color=factor(Lever), group=Scenario)) + 
    geom_line() + 
    ylab("Net Present Value") + 
    xlab("Time") +
    labs(color = "Strategy")
}

plot_linha_uma_variavel_ensemble_uma_estrategia = function(dados, variavel, nome_amigavel_variavel, estrategia) {
  
  gr2_dados = subset(dados, (Lever %in% estrategia))
  
  call_grafico = substitute(
    expr = ggplot2::ggplot(gr2_dados, aes(x= time, y= Variavel, color=factor(Lever) , group= interaction(Lever, Scenario) 
    )),
    env = list(Variavel = as.name(variavel))
  )
  
  p <- eval(call_grafico)
  
  p + 
    geom_line() + 
    ylab(nome_amigavel_variavel) + 
    xlab("Time") + 
    theme(legend.position="bottom")  +
    labs(color = "Strategy") +
    scale_y_continuous(labels = format_for_humans)
}

plot_linha_uma_variavel_players_um_cenario = function(dados = results$DadosSimulados, estrategia = 1, cenario = 4, variavel = "sNPVProfit", nome_amigavel_variavel = "VPL", opcoes = opcoes){
  variaveis_players = paste(variavel, 1:N_PLAYERS, sep="")
  # Filtrando Dados de Interesse
  dados = dados[,c(opcoes$VarTempo, opcoes$VarCenarios, opcoes$VarEstrategias, variaveis_players)] %>% subset(., Lever == estrategia & Scenario == cenario)
  
  # Trazer Dados Simulados para o formato "Longo" para permitir que o gráfico seja realizado
  dados_longo = tidyr::gather(dados, player, variavel, 4:(3+N_PLAYERS))
  
  # Removendo Nome longo da Variável
  dados_longo$player =  gsub(variavel,"P", dados_longo$player,ignore.case=T)
  
  # Adicionar ruído à variável para o gráfico distinguir melhor os players
  
  p = ggplot2::ggplot(dados_longo, aes(x= time, y= variavel, color=player, group = player))
  p + geom_line() +
    ylab(nome_amigavel_variavel) + 
    xlab("Time") + 
    theme(legend.position="bottom")  +
    labs(color = "Player") +
    scale_y_continuous(labels = format_for_humans)
}



plot_linha_uma_variavel_ensemble = function(dados, variavel, nome_amigavel_variavel, estrategia) {
  
  levers_no_ensemble = unique(dados$Lever)
  
  # Caso exista mais de uma estratégia, usar somente a primeira.
  if(length(levers_no_ensemble) > 1) {
    dados = subset(dados, Lever == 1)
  }
  
  call_grafico = substitute(
    expr = ggplot2::ggplot(dados, aes(x= time, y= Variavel, color=Scenario, group= Scenario 
    )),
    env = list(Variavel = as.name(variavel))
  )
  
  p <- eval(call_grafico)
  
  p + 
    geom_line() + 
    ylab(nome_amigavel_variavel) + 
    xlab("Time") + 
    theme(legend.position="bottom")  +
    labs(color = "Case") +
    scale_y_continuous(labels = format_for_humans)
}



plot_linha_uma_variavel = function(dados, variavel, nome_amigavel_variavel) {
  
  call_grafico = substitute(
    expr = ggplot2::ggplot(dados, aes(x= time, y= Variavel)),
    env = list(Variavel = as.name(variavel))
  )
  
  p <- eval(call_grafico)
  
  p + 
    geom_line() + 
    ylab(nome_amigavel_variavel) + 
    xlab("Time") + 
    theme(legend.position="bottom") +
    scale_y_continuous(labels = format_for_humans)
}


plot_partial_plot = function(dados, variavel, nome_amigavel_variavel) {
  
  call_grafico = substitute(
    expr = ggplot2::ggplot(dados, aes(x= Variavel, y= VariavelYHAT)),
    env = list(Variavel = as.name(variavel), VariavelYHAT = as.name(paste0("yhat.",variavel)))
  )
  
  p <- eval(call_grafico)
  
  p + 
    geom_line() + 
    ylab("yhat") + 
    xlab(variavel) + 
    theme(legend.position="bottom") +
    scale_y_continuous(labels = format_for_humans) +
    scale_x_continuous(labels = format_for_humans) + 
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=8))
  #theme(axis.text.x = element_text(size=20)) # + ggtitle(paste0("Part. Dep.: ",v))
}


plot_partial_plot_n_variaveis = function(dados) {
  
  p <- ggplot2::ggplot(dados, aes(x= Incerteza, y= PartialDependence))
  
  p = p + 
    geom_line() + 
    ylab("Partial Dependence") + 
    xlab("Uncertain Variable") + 
    theme(legend.position="bottom") +
    scale_y_continuous(labels = format_for_humans) +
    scale_x_continuous(labels = format_for_humans) + 
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=8)) 
  
  p = p + facet_wrap(~Variavel, scales = "free_x", ncol = 3, strip.position = "bottom")
  p 
  #theme(axis.text.x = element_text(size=20)) # + ggtitle(paste0("Part. Dep.: ",v))
}




plot_linha_duas_variaveis = function(dados, variavel1, nome_amigavel_variavel1, variavel2, nome_amigavel_variavel2) {
  
  p <- ggplot2::ggplot(dados, aes(x = time))
  
  call_variavel1 = substitute(
    expr = p + geom_line(aes(y = Variavel, colour = NomeVariavel)),
    env = list(Variavel = as.name(variavel1), NomeVariavel = nome_amigavel_variavel1)
  ) 
  
  razaovariavel = (max(dados[,variavel1]) - min(dados[,variavel1])) / (max(dados[,variavel2]) - min(dados[,variavel2]))
  
  p <- eval(call_variavel1)
  
  call_variavel2 = substitute(
    expr = p + geom_line(aes(y = Variavel * Razao, colour = NomeVariavel)),
    env = list(Variavel = as.name(variavel2), NomeVariavel = nome_amigavel_variavel2, Razao = razaovariavel)
  ) 
  
  p <- eval(call_variavel2)
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./razaovariavel, name = nome_amigavel_variavel2, labels = format_for_humans), labels = format_for_humans)
  
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c("blue", "red"))
  p <- p + labs(y = nome_amigavel_variavel1,
                x = "Time",
                colour = "Variable")
  
  p <- p + theme(legend.position="bottom")
  
  p
  
}



plot_taxa_adocao_uma_estrategia = function(dados, estrategia) {
  gr2_dados = subset(dados, (Lever == estrategia))
  ggplot2::ggplot(gr2_dados,
                  aes(x=Tempo, y=Adoption_Rate, color=factor(Lever), group=Scenario)) + 
    geom_line() + 
    ylab("Adoption Rate") + 
    xlab("Time") +
    labs(color = "Strategy")
}


#' grafico_whisker_por_lever
#'
#' Este grafico 
#' 
#' @param dados_regret dados resultantes da análise de regret.
#' @param variavel nome da variável simulada a realizar o gráfico.
#' @param nome_amigavel_variavel nome amigável para a variável de resposta (plotada no eixo y)
#'
#' @return grafico whisker do ggplot2
#' @export
#'
grafico_whisker_por_lever = function(dados_regret, variavel, nome_amigavel_variavel) {
  dados_por_estrategia = dplyr::group_by(dados_regret, Lever)
  
  dados_por_estrategia$Lever = as.factor(dados_por_estrategia$Lever)
  
  # Gerando Grafico da Variável de Perda de Oportunidade
  call_grafico = substitute(
    expr = ggplot(dados_por_estrategia, aes(y = Variavel,x = Lever, group = Lever)),
    env = list(Variavel = as.name(variavel))
  )
  
  p <- eval(call_grafico)
  p + geom_boxplot() + 
    scale_y_continuous(labels = format_for_humans) + 
    ylab(nome_amigavel_variavel) + 
    theme(axis.text.x = element_text(size=7))
}

#' plot_fronteira_tradeoff_estrategia
#' 
#' Esta funcao ainda nao é completamente generalizada. estao dentro desta funcao a definicao do cenário a ser analisado.
#'
#' @param results list com os dados resultantes da funcao de simulacao e analise
#' @param opcoes list de opcoes do modelo (as opcoes sao padronizadas.)
#'
#' @return grafico plotly com a fronteira de tradeoffs conforme um determinado cenário.
#' @export
#'
plot_fronteira_tradeoff_estrategia = function(results, opcoes = opcoes) {
  
  ensemble = as.data.frame(results$Ensemble)
  
  # Na linha abaixo as variáveis devem ser definidas.
  # Resultados da Análise em 2/01
  # cenarios_escolhidos = subset(ensemble,
  #                              aReferenceIndustryDemandElasticity > 0.127 &
  #                                aReferenceIndustryDemandElasticity < 0.940 &
  #                                aFractionalDiscardRate > 0.143 &
  #                                aReferencePopulation > 5.3 * 10 ^ 4)
  
  # Resultados em 03/01:
  
  cenarios_escolhidos = subset(ensemble,
                               aReferencePopulation > 5.8 * 10 ^ 4 &
                                 aNormalCapacityUtilization > 0.626 &
                                 aNormalCapacityUtilization < 0.864 &
                                 aDesiredMarketShare2 > 0.325 &
                                 aDesiredMarketShare2 < 0.528 &
                                 aSwitchForCapacityStrategy4 < 2.14 &
                                 aSwitchForCapacityStrategy4 > 0.611 &
                                 aSensOfAttractToPrice > -11.3
  )
  
  
  
  numero_cenarios_escolhidos = cenarios_escolhidos$Scenario
  
  dados_cenario = subset(results$DadosUltimoPeriodo, Scenario %in% numero_cenarios_escolhidos)
  
  #dados_cenario = results$DadosUltimoPeriodo %>% dplyr::filter(AdvertisingCost  < 5.727e+04 & AverageTicket  >  1.789e+00 & AdoptionFraction  <  2.895e-02)
  
  analise_regret_cenario = calcular_e_resumir_regret(dados = dados_cenario, var_resposta = opcoes$VarResposta, var_cenarios = opcoes$VarCenarios, var_estrategias = opcoes$VarEstrategias)
  
  variavel_comparacao = paste(opcoes$VarResposta,opcoes$VarCriterio, sep = "")
  
  variaveis_grafico_regret = c(opcoes$VarEstrategias, variavel_comparacao)
  
  regret_todos_os_futuros = results$AnaliseRegret$ResumoEstrategias[variaveis_grafico_regret]
  
  regret_todos_os_futuros = as.data.frame(regret_todos_os_futuros)
  
  names(regret_todos_os_futuros) = c(opcoes$VarEstrategias, "PerdaOportunidadeTodosOsCenarios")
  
  # names(regret_todos_os_futuros[variaveis_grafico_regret]) = c(opcoes$VarEstrategias, paste(variavel_comparacao, "TodosOsCenarios", sep = ""))
  
  regret_cenario = analise_regret_cenario$ResumoEstrategias[variaveis_grafico_regret]
  
  regret_cenario = as.data.frame(regret_cenario)
  
  names(regret_cenario) = c(opcoes$VarEstrategias, "PerdaOportunidadeNoCenario")
  
  dados_join = dplyr::left_join(regret_todos_os_futuros, regret_cenario)
  
  dados_join = dplyr::inner_join(dados_join, results$Inputs$Levers)
  
  # Esta descrição é customizada para os cenários definidos aqui.
  dados_join$Descricao = paste("CS",dados_join$aSwitchForCapacityStrategy1,"MS",dados_join$aDesiredMarketShare1,"OR",dados_join$aOrcamentoPeD1,"AB",dados_join$aPercPeDAberto1, sep = ".")
  
  # Gerando Plot com Tradeoff de Estratégias.
  plot_tradeoff_estrategias = ggplot2::ggplot(dados_join, aes(x=PerdaOportunidadeTodosOsCenarios, y=PerdaOportunidadeNoCenario, fill=aPercPeDAberto1)) +
    geom_label(label=as.character(dados_join$Lever), color="white", size=3) +
    scale_y_continuous(labels = format_for_humans) + 
    scale_x_continuous(labels = format_for_humans) +
    ylab("Regret (75% Percentile) - Vunerable Scenario") +
    xlab("Regret (75% Percentile) - All other Scenarios")
  
  `%notin%` = function(x,y) !(x %in% y)
  
  # definindo estratégias a usar neste gráfico: as 8 mais robustas em geral
  dados_join = dados_join %>% dplyr::arrange(PerdaOportunidadeTodosOsCenarios)
  
  # Estratégias a entrar no gráfico: As 6 mais robustas em geral:
  estrategias_grafico = dados_join$Lever[1:6]
  
  regret_medio_cenario_falha = results$AnaliseRegret$Dados %>% dplyr::filter(Scenario %in% numero_cenarios_escolhidos, Lever %in% estrategias_grafico) %>% group_by(Lever) %>% summarise(MediaRegretCenarioFalha = mean(sNPVProfit1Regret))
  
  regret_medio_cenario_sucesso = results$AnaliseRegret$Dados %>% dplyr::filter(Scenario %notin% numero_cenarios_escolhidos, Lever %in% estrategias_grafico) %>% group_by(Lever) %>% summarise(MediaRegretCenarioSucesso = mean(sNPVProfit1Regret))
  
  ProbCenario = seq(from = 1/101, to = 100/101, length.out = 100)
  
  OddsCenario = ProbCenario / (1-ProbCenario)
  
  #ProbCenario = c(1/1001, 1/101, 1/11, 1/2, 10/11, 100/101, 1000/1001)
  #OddsCenario = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
  
  OddsTextoBreaks = c("1:100", "1:10", "1:1", "10:1", "100:1")
  OddsCenarioBreaks = c(0.01, 0.1, 1, 10, 100)
  
  probs_df = data.frame(ProbCenario, OddsCenario)
  
  odds_e_levers = data.frame(expand.grid(Lever = results$Inputs$Levers$Lever, OddsCenario = OddsCenario))
  
  odds_e_levers = dplyr::inner_join(odds_e_levers, probs_df)
  
  tabela_analise_tradeoff = dplyr::inner_join(regret_medio_cenario_falha, 
                                              regret_medio_cenario_sucesso)
  
  tabela_analise_tradeoff = dplyr::inner_join(tabela_analise_tradeoff, 
                                              odds_e_levers)
  
  tabela_analise_tradeoff$RegretEsperado = tabela_analise_tradeoff$ProbCenario * tabela_analise_tradeoff$MediaRegretCenarioFalha + (1-tabela_analise_tradeoff$ProbCenario) * tabela_analise_tradeoff$MediaRegretCenarioSucesso
  
  # Tabela Gerada, Gerar Plot
  
  plot_curva_tradeoff = ggplot2::ggplot(tabela_analise_tradeoff,
                                        aes(x=OddsCenario, y=RegretEsperado, color=factor(Lever), group=factor(Lever))) + 
    geom_line(size=1) + 
    ylab("Expected Regret ($)") + 
    xlab("Odds of Vulnerable Scenario") +
    labs(color = "Strategy") +
    scale_x_continuous(breaks = OddsCenarioBreaks, labels = OddsTextoBreaks, trans = "log10") + 
    scale_y_continuous(labels = format_for_humans)
  
  list(
    DadosTradeoffPontos = dados_join,
    PlotTradeoffDispersao = plot_tradeoff_estrategias,
    DadosTradeoffChances = tabela_analise_tradeoff,
    PlotTradeoffOdds = plot_curva_tradeoff
  )
  
}


#' plot_fronteira_tradeoff_estrategia
#' 
#' Esta funcao ainda nao é completamente generalizada. estao dentro desta funcao a definicao do cenário a ser analisado.
#'
#' @param results list com os dados resultantes da funcao de simulacao e analise
#' @param opcoes list de opcoes do modelo (as opcoes sao padronizadas.)
#'
#' @return grafico plotly com a fronteira de tradeoffs conforme um determinado cenário.
#' @export
#'
plot_tradeoff_regret_vpl = function(results, opcoes = opcoes) {
  
  ensemble = as.data.frame(results$Ensemble)
  
  # Na linha abaixo as variáveis devem ser definidas.
  # Resultados da Análise em 2/01
  # cenarios_escolhidos = subset(ensemble,
  #                              aReferenceIndustryDemandElasticity > 0.127 &
  #                                aReferenceIndustryDemandElasticity < 0.940 &
  #                                aFractionalDiscardRate > 0.143 &
  #                                aReferencePopulation > 5.3 * 10 ^ 4)
  
  # Resultados em 03/01:
  
  cenarios_escolhidos = subset(ensemble,
                               aReferencePopulation > 5.8 * 10 ^ 4 &
                                 aNormalCapacityUtilization > 0.626 &
                                 aNormalCapacityUtilization < 0.864 &
                                 aDesiredMarketShare2 > 0.325 &
                                 aDesiredMarketShare2 < 0.528 &
                                 aSwitchForCapacityStrategy4 < 2.14 &
                                 aSwitchForCapacityStrategy4 > 0.611 &
                                 aSensOfAttractToPrice > -11.3
  )
  
  
  
  numero_cenarios_escolhidos = cenarios_escolhidos$Scenario
  
  dados_cenario = subset(results$DadosUltimoPeriodo, Scenario %in% numero_cenarios_escolhidos)
  
  #dados_cenario = results$DadosUltimoPeriodo %>% dplyr::filter(AdvertisingCost  < 5.727e+04 & AverageTicket  >  1.789e+00 & AdoptionFraction  <  2.895e-02)
  
  analise_regret_cenario = calcular_e_resumir_regret(dados = dados_cenario, var_resposta = opcoes$VarResposta, var_cenarios = opcoes$VarCenarios, var_estrategias = opcoes$VarEstrategias)
  
  variavel_comparacao = paste(opcoes$VarResposta,opcoes$VarCriterio, sep = "")
  
  variaveis_grafico_regret = c(opcoes$VarEstrategias, variavel_comparacao, "sNPVProfit1Percentil75")
  
  regret_todos_os_futuros = results$AnaliseRegret$ResumoEstrategias[variaveis_grafico_regret]
  
  regret_todos_os_futuros = as.data.frame(regret_todos_os_futuros)
  
  names(regret_todos_os_futuros) = c(opcoes$VarEstrategias, "PerdaOportunidade75", "VPL75")
  
  dados_join = dplyr::inner_join(regret_todos_os_futuros, results$Inputs$Levers)
  
  # Esta descrição é customizada para os cenários definidos aqui.
  dados_join$Descricao = paste("CS",dados_join$aSwitchForCapacityStrategy1,"MS",dados_join$aDesiredMarketShare1,"OR",dados_join$aOrcamentoPeD1,"AB",dados_join$aPercPeDAberto1, sep = ".")
  
  ggplot2::ggplot(dados_join, aes(x=PerdaOportunidade75, y=VPL75, fill=aPercPeDAberto1)) +
    geom_label(label=as.character(dados_join$Lever), color="white", size=3) +
    scale_y_continuous(labels = format_for_humans) + 
    scale_x_continuous(labels = format_for_humans) +
    ylab("Net Present Value (75% Percentile)") +
    xlab("Regret (75% Percentile)")
  
  
}

#' plot_estrategias_versus_incertezas
#'
#' @param ensemble_analisado data.frame resultante da funcao analisar_ensemble_com_melhor_estrategia
#' @param incertezas vetor com nomes das variáveis de incerteza a incluir no gráfico, devem corresponder à variáveis no ensemble.
#' @param binario default \code{TRUE}, pode apresentar a divisão entre as outras estratégias ou não.
#'
#' @return grafico que mostra a que condições a estratégia candidata é mais sucetível a falhar.
#' @export
plot_estrategias_versus_incertezas = function(df_vulnerabilidade, incertezas, binario = TRUE) {
  
  df_vulnerabilidade$CasoInteresse = as.factor(df_vulnerabilidade$CasoInteresse)
  
  p = if(binario) {
    GGally::ggpairs(df_vulnerabilidade, columns = incertezas, aes(colour = CasoInteresse, alpha = 0.7))
  } else {
    GGally::ggpairs(df_vulnerabilidade, columns = incertezas, aes(colour = sNPVProfit1Regret, alpha = 0.7))
  }
  
  p
}


plot_landscape_futuros_plausiveis = function(results, estrategia, variavelresp, nomeamigavel_variavelresp, variavel1, n_variavel1, variavel2, n_variavel2) {
  
  ensemble_e_resultados = dplyr::inner_join(as.data.frame(results$Ensemble), results$AnaliseRegret$Dados, by = "Scenario")
  
  ensemble_e_resultados = ensemble_e_resultados[which(ensemble_e_resultados$Lever == estrategia),]
  
  var_x = ensemble_e_resultados[,variavel1]
  var_y = ensemble_e_resultados[,variavel2] 
  var_z = ensemble_e_resultados[,variavelresp]
  
  my.df.interp <- interp(x = var_x, y = var_y, z = var_z, nx = 30, ny = 30)
  my.df.interp.xyz <- as.data.frame(interp2xyz(my.df.interp))
  names(my.df.interp.xyz) <- c(variavel1, variavel2, variavelresp)
  
  my.df.interp.xyz = my.df.interp.xyz[complete.cases(my.df.interp.xyz),] 
  
  
  call_plot = substitute(
    expr = ggplot(data = my.df.interp.xyz, aes(x = Variavelx, y = Variavely)) + geom_tile(aes(fill = Variavelz)),
    env = list(Variavelx = as.name(variavel1), Variavely = as.name(variavel2), Variavelz = as.name(variavelresp))
  ) 
  
  p <- eval(call_plot)
  p <- p + xlab(n_variavel1) + 
    ylab(n_variavel2) + 
    labs(fill = nomeamigavel_variavelresp) + 
    ggtitle(label = paste("Estratégia", estrategia)) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "bottom") + 
    viridis::scale_fill_viridis()
  
  #scale_color_gradient()
  
  #scale_fill_gradient(low="green", high="red")
  
  p + scale_x_continuous(labels = format_for_humans) + scale_y_continuous(labels = format_for_humans)
}

plot_grid_estrategias_casos_vpl = function(results) {
  plot<-ggplot(results$DadosUltimoPeriodo, aes(Scenario, Lever, fill = sNPVProfit1)) + 
    geom_tile(colour="gray20", size=1.5, stat="identity") + 
    scale_fill_viridis(option="D") +
    scale_y_continuous(breaks=1:6)+
    xlab("Case") + 
    ylab("Strategy") +
    theme(
      #plot.title = element_text(color="white",hjust=0,vjust=1, size=rel(2)),
      plot.background = element_blank(),
      panel.background = element_blank(),
      #panel.border = element_rect(fill=NA,color="gray20", size=0.5, linetype="solid"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(), 
      #axis.text = element_text(color="white", size=rel(1.5)),
      axis.text.y  = element_text(hjust=1),
      #legend.text = element_text(color="white", size=rel(1.3)),
      #legend.background = element_rect(fill="gray20"),
      legend.position = "right"
      #legend.title=element_blank()
    )
  
  plot$labels$fill = "NPV"
  plot
}


sdrdm.pairs_plot = function(data, lever, variables) {
  dados_grafico = subset(data, Lever == lever)
  dados_grafico = dados_grafico[variables]
  
  pairs(dados_grafico)
}


#' salvar_plots_result
#'
#' Esta função gera uma série de gráficos a partir de um objeto de resultados
#' @param results  objeto de resultados retornado pela função simular_RDM
#' @param cenario_plot_players um cenário escolhido para exibir plots comparando os players.
#' @param estrategia_candidata o número de uma estratégia candidata a testar para filtrar os plots
#' @param opcoes variável global de opções.
#'
#' @return não gera nenhum retorno. Esta função salva os gráficos na pasta /imagem.
#' @export
#'
salvar_plots_result = function(results, cenario_plot_players, estrategia_candidata, opcoes = opcoes){
  # Nome Objeto
  nome_resultado = deparse(substitute(results))
  
  estrategia_plot_players = estrategia_candidata
  
  plots_linha_geral = list(
    plot_estrategia_candidata_vpl = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sNPVProfit1", nome_amigavel_variavel = "Net Present Value", estrategia = estrategia_candidata),
    plot_estrategia_candidata_preco = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sPrice1", nome_amigavel_variavel = "Player 1 Avg. Price", estrategia = estrategia_candidata),
    plot_estrategia_candidata_share = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "aOrderShare1", nome_amigavel_variavel = "Player 1 Market Share", estrategia = estrategia_candidata),
    plot_estrategia_candidata_demanda_global = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "aIndustryShipments", nome_amigavel_variavel = "Prof. 3D Printer Sales", estrategia = estrategia_candidata)
  )
  
  plots_whisker = list(
    plot_whisker_lever_perc_regret = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1RegretPerc", nome_amigavel_variavel = "Percent Regret"),
    plot_whisker_lever_regret = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1Regret", nome_amigavel_variavel = "Regret"),
    plot_whisker_lever_profit = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1", nome_amigavel_variavel = "Net Present Value"),
    plot_whisker_lever_share = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "aOrderShare1",  nome_amigavel_variavel = "Player 1 Market Share"),
    plot_whisker_lever_industry_order_rate = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "aIndustryShipments", nome_amigavel_variavel = "Prof. 3D Printer Sales"),
    plot_whisker_lever_price = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sPrice1", nome_amigavel_variavel = "Player 1 Avg. Price"),
    plot_whisker_lever_installed_base = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sInstalledBase1", nome_amigavel_variavel = "Player 1 Installed Base")
  )
  
  plots_players = list(
    plot_players_vpl = plot_linha_uma_variavel_players_um_cenario(dados = results$DadosSimulados, 
                                                                  estrategia = estrategia_plot_players, 
                                                                  cenario = cenario_plot_players, 
                                                                  variavel = "sNPVProfit", 
                                                                  nome_amigavel_variavel = "Net Present Value", 
                                                                  opcoes = opcoes)
    
    ,plot_players_vpl = plot_linha_uma_variavel_players_um_cenario(dados = results$DadosSimulados, 
                                                                   estrategia = estrategia_plot_players, 
                                                                   cenario = cenario_plot_players, 
                                                                   variavel = "aOrderShare", 
                                                                   nome_amigavel_variavel = "Market Share", 
                                                                   opcoes = opcoes)
    
    
    ,plot_players_net_income = plot_linha_uma_variavel_players_um_cenario(dados = results$DadosSimulados, 
                                                                          estrategia = estrategia_plot_players, 
                                                                          cenario = cenario_plot_players, 
                                                                          variavel = "fNetIncome", 
                                                                          nome_amigavel_variavel = "Net Profit", 
                                                                          opcoes = opcoes)
    
    
    ,plot_players_performance = plot_linha_uma_variavel_players_um_cenario(dados = results$DadosSimulados, 
                                                                           estrategia = estrategia_plot_players, 
                                                                           cenario = cenario_plot_players, 
                                                                           variavel = "aPerformance", 
                                                                           nome_amigavel_variavel = "Product Performance", 
                                                                           opcoes = opcoes)
    
    ,plot_players_pantes = plot_linha_uma_variavel_players_um_cenario(dados = results$DadosSimulados, 
                                                                      estrategia = estrategia_plot_players, 
                                                                      cenario = cenario_plot_players, 
                                                                      variavel = "aPatentesEmpresaTemAcesso", 
                                                                      nome_amigavel_variavel = "Patents Accessed by Player", 
                                                                      opcoes = opcoes)
  )
  
  
  # Salvando os Gráficos
  mapply(ggsave, file=paste0("./images/",nome_resultado,"-estrat",estrategia_candidata,"-",names(plots_linha_geral), ".png"), plot=plots_linha_geral, width = plots_width, height = plots_heigh)
  
  mapply(ggsave, file=paste0("./images/",nome_resultado,"-", names(plots_whisker), ".png"), plot=plots_whisker, width = plots_width, height = plots_heigh)
  
  mapply(ggsave, file=paste0("./images/",nome_resultado,"-cenario",cenario_plot_players,"-", names(plots_players), ".png"), plot=plots_players, width = plots_width, height = plots_heigh)
  
  # Retornar objeto com todos os plots:
  list(
    plots_players = plots_players,
    plots_whisker = plots_whisker,
    plots_linha_geral = plots_linha_geral
  )
}

#### DEMONSTRAÇÕES ####
#' gerar_grafico_curva_experiencia
#'
#' @return gráfico demonstrando como a curva de experiência do modelo funciona.
#' @export
gerar_grafico_curva_experiencia = function() {
  aLCStrength = c(0.5, 0.75, 0.85, 0.95, 1)
  
  aLCExponent = log(aLCStrength)/log(2)
  
  aInitialProductionExperience = 1000
  
  sCumulativeProduction = 1000:5000
  
  learning_df = data.frame(Learning0.5 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[1],
                           Learning0.75 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[2],
                           Learning0.85 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[3],
                           Learning0.95 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[4],
                           Learning1 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[5])
  
  aInitialUnitFixedCost = 2000
  
  aInitialUnitVariableCost = 1000
  
  # aUnitFixedCost = aLearning * aInitialUnitFixedCost
  
  aUnitVariableCost = data.frame(learning_df * aInitialUnitVariableCost, Producao = sCumulativeProduction) 
  
  ggplot2::ggplot(aUnitVariableCost, aes(Producao)) + 
    geom_line(aes(y = Learning1, colour = "1")) + 
    geom_line(aes(y = Learning0.95, colour = "0.95")) + 
    geom_line(aes(y = Learning0.85, colour = "0.85")) + 
    geom_line(aes(y = Learning0.75, colour = "0.75")) + 
    geom_line(aes(y = Learning0.5, colour = "0.5")) + 
    ylab("Custo Variável de Produção") + 
    xlab("Produção Acumulada") +
    labs(color = expression(Gamma))
}