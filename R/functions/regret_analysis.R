

##### CALCULO DO REGRET (PERDA DE OPORTUNIDADE) #####

#' calcular_regret
#'
#' @param dados dataframe de dados simulados para o calculo do Regret.
#' @param var_resposta variável de resposta a utilizar no calculo de regret (quanto mais, melhor)
#' @param var_group variável a agrupar (ex.: Cenários)
#'
#' @return mesmo dataframe de entrada com variáveis a mais.
calcular_regret = function(dados, var_resposta, var_group, sentido = "max") {
  var_maximo = paste("MaximoPor", var_group, sep = "")
  var_minimo = paste("MinimoPor", var_group, sep = "")
  var_regret = paste(var_resposta, "Regret", sep = "")
  var_regret_perc = paste(var_regret, "Perc", sep = "")
  
  dados[var_maximo] = calcular_maximo_por_variavel(var_resposta = var_resposta, var_group = var_group, dados = dados)
  
  dados[var_minimo] = calcular_minimo_por_variavel(var_resposta = var_resposta, var_group = var_group, dados = dados)
  
  if (sentido == "max") {
    dados[var_regret] = dados[var_maximo] - dados[var_resposta]  
  } else {
    dados[var_regret] = dados[var_resposta] - dados[var_minimo]
  }
  
  dados[var_regret_perc] = dados[var_regret] / (dados[var_maximo] - dados[var_minimo])
  
  dados  
}


##### RESUMIR VARIÁVEL DE RESPOSTA PARA A ANÁLISE DO REGRET #####
#' resumir_variavel_resposta
#'
#' @param dados dataframe com dados para analise do regret.
#' @param var_resposta variável de resposta para análise do RDM.
#' @param var_group 
#'
#' @return dataframe com resumo das variaveis por grupo definido.
resumir_variavel_resposta = function(dados = dados_ano_final, var_resposta = "Cash", var_group = "Lever") {
  var_regret = paste(var_resposta, "Regret", sep = "")
  var_regret_perc = paste(var_regret, "Perc", sep = "")
  
  call = substitute(
    expr =
      dplyr::group_by(dados, VarGroup) 
    %>% dplyr::select(VarGroup, VarResposta, VarRegret, VarRegretPerc)
    %>% dplyr::summarise(
      VarMedio = mean(VarResposta, na.rm = TRUE),
      VarDev = sd(VarResposta, na.rm = TRUE),
      VarMediaSobDesvio = mean(VarResposta, na.rm = TRUE) / sd(VarResposta, na.rm = TRUE),
      Percentil25Var = quantile(VarResposta, probs = c(0.25), na.rm = TRUE),
      Percentil75Var = quantile(VarResposta, probs = c(0.75), na.rm = TRUE),
      RegretMedio = mean(VarRegret, na.rm = TRUE),
      DesvioRegret = sd(VarRegret, na.rm = TRUE),
      Percentil25Regret = quantile(VarRegret, probs = c(0.25), na.rm = TRUE),
      Percentil75Regret = quantile(VarRegret, probs = c(0.75), na.rm = TRUE),
      RegretMedioPerc = mean(VarRegretPerc, na.rm = TRUE),
      DesvioRegretPerc = sd(VarRegretPerc, na.rm = TRUE),
      Percentil25RegretPerc = quantile(VarRegretPerc, probs = c(0.25), na.rm = TRUE),
      Percentil75RegretPerc = quantile(VarRegretPerc, probs = c(0.75), na.rm = TRUE)
    )
    ,
    env = list(VarGroup = as.name(var_group),
               VarResposta = as.name(var_resposta),
               VarRegret = as.name(var_regret),
               VarRegretPerc = as.name(var_regret_perc)
    )
  )
  
  resumo = eval(call)  
  
  colnames(resumo) = c(
    var_group,
    paste(var_resposta, "Medio", sep = ""),
    paste(var_resposta, "Desvio", sep = ""),
    paste(var_resposta, "MediaSobreDesvio", sep = ""),
    paste(var_resposta, "Percentil25", sep = ""),
    paste(var_resposta, "Percentil75", sep = ""),
    paste(var_regret, "Medio", sep = ""),
    paste(var_regret, "Desvio", sep = ""),
    paste(var_regret, "Percentil25", sep = ""),
    paste(var_regret, "Percentil75", sep = ""),
    paste(var_regret_perc, "Medio", sep = ""),
    paste(var_regret_perc, "Desvio", sep = ""),
    paste(var_regret_perc, "Percentil25", sep = ""),
    paste(var_regret_perc, "Percentil75", sep = "")
  )
  
  resumo
}


##### ESCOLHER ESTRATÉGIA CANDIDATA #####

#' escolher_estrategia_candidata
#'
#' Escolhe a estratégia candidata "mais robusta" (dentre as disponíveis) a partir de dados simulados e de um resumo das estratégias contendo índices de perda de oportunidade.
#' @param dados Dataframe com dados simulados
#' @param resumo_estrategias dataframe de resumo das estratégias (gerado pela função calcular_e_resumir_regret).
#' @param var_resposta VAriávei de Resposta a considerar (string).
#' @param var_criterio Variável a usar como critério (default é o Percentil 75 do Regret Relativo).
#' @param sentido Sentido a utilizar na análise (default é min para minimizar o regret).
#'
#' @return uma (ou mais) estratégias candidatas. (ID da estratégia).
escolher_estrategia_candidata = function(dados, resumo_estrategias, var_resposta, var_criterio = "RegretPercPercentil75", sentido = "min") {
  
  var_respota_criterio = paste(var_resposta, var_criterio, sep = "")
  
  
  # Esta lista de criterios deve ser mantida igual à lista que a funcao resumir_variavel_resposta()
  possiveis_var_criterios = c("Percentil25", "Percentil75", "Medio", "Desvio", "RegretMedio", "RegretDesvio", "RegretPercentil25", "RegretPercentil75", "RegretPercMedio", "RegretPercDesvio", "RegretPercPercentil25", "RegretPercPercentil75")
  
  # Conferindo alguns pressupostos basicos:
  possiveis_var_respota_e_criterios = paste(var_resposta, possiveis_var_criterios, sep = "")
  
  # Conferindo se a variável de resposta e variável de critério combinam corretamente:
  if (!all(possiveis_var_respota_e_criterios %in% names(resumo_estrategias))){
    stop("Existe algo errado com a sua variavel de resposta ou variavel de criterio (a combinacao das duas no existe no resumo de estrategias).")
  }
  
  # Conferindo se a Variavel de criterio está correta.
  if(!var_criterio %in% possiveis_var_criterios){
    stop(paste("Esta variavel de criterio esta incorreta. escolha entre:",possiveis_var_criterios))
  }
  
  
  # Agora sim, posso escolhenr a estratégia que tem o menor percentil percentual 75 (assim como Lempert):
  estrategias_candidatas = switch(sentido,
                                  "min" = escolher_estrategia_min(resumo_estrategias, var_respota_criterio),
                                  "max" = escolher_estrategia_max(resumo_estrategias, var_respota_criterio))
  
  estrategias_candidatas
}


##### CALCULAR E RESUMIR REGRET #####

calcular_e_resumir_regret = function(dados, var_resposta, var_cenarios, var_estrategias, sentido = "max") {
  
  if (sentido == "max") {
    dados = calcular_regret(dados = dados, var_resposta = var_resposta, var_group = var_cenarios, sentido = "max")  
  } else {
    dados = calcular_regret(dados = dados, var_resposta = var_resposta, var_group = var_cenarios, sentido = "min")
  }
  
  
  # Resumindo Variável de Resposta Cash:
  resumo_estrategias = resumir_variavel_resposta(dados = dados, var_resposta = var_resposta, var_group = var_estrategias)
  
  # Formar lista de outputs dessta análise
  output = list(
    Dados = dados,
    ResumoEstrategias = resumo_estrategias
  )
  
  output
}

##### ESCOLHER ESTRATÉGIA #####

escolher_estrategia_min = function(resumo_estrategias, criterio) {
  linha_estrategia = which(resumo_estrategias[criterio] == min(resumo_estrategias[criterio]))
  estrategia = resumo_estrategias[linha_estrategia, "Lever"]  
  estrategia
}


escolher_estrategia_max = function(resumo_estrategias, criterio) {
  linha_estrategia = which(resumo_estrategias[criterio] == max(resumo_estrategias[criterio]))
  estrategia = resumo_estrategias[linha_estrategia, "Lever"]  
  estrategia
}

##### ANALISAR ENSEMBLE DETERMINANDO A MELHOR ESTRATÉGIA #####
analisar_ensemble_com_melhor_estrategia = function(ensemble, dados_regret, var_cenarios, var_estrategias, var_resposta, estrategia_candidata) {
  
  
  ensemble = as.data.frame(ensemble)
  dados_regret = as.data.frame(dados_regret)
  
  
  dados_regret["MelhorEstrategia"] = dados_regret[var_resposta] == dados_regret$MaximoPorScenario
  
  linhas_melhores_estrategias = which(dados_regret[var_resposta] == dados_regret$MaximoPorScenario)
  
  variaveis = c(var_cenarios, var_estrategias, var_resposta)
  
  melhores_estrategias = as.data.frame(dados_regret[linhas_melhores_estrategias, variaveis])
  
  ensemble_com_melhor_estrategia = dplyr::inner_join(ensemble, melhores_estrategias)
  
  ensemble_com_melhor_estrategia["EstrategiaCandidata"] = ensemble_com_melhor_estrategia[var_estrategias] == estrategia_candidata
  
  #ensemble_com_melhor_estrategia = as.factor(ensemble_com_melhor_estrategia[var_estrategias])
  
  ensemble_com_melhor_estrategia
  
}


##### ANÁLISE DE VULNERABILIDADE #####

#' obter_df_analise_vulnerabilidade
#'
#' @param results results da função de simulação do RDM (contendo análise de regret, necessáriamente).
#' @param estrategia_candidata número da estratégia ser analisada.
#' @param variavel_resposta nome da variávei de respota analisada nesta estratégia  (que deverá estar acima ou abaixo do threshold).
#' @param threshold número acima ou abaixo do qual a variável de resposta estará para ser considerada "interessante" para a análise.
#' @param planilha_inputs planilha de inputs com as variáveis incertas, para filtrarmos somente as variáveis incertas.
#' @param sentido_vulnerabilidade pode ser ">=" ou "<=". Se ">=", os casos de interesse serão aqueles onde a variáveil de interesse seja >= ao threshold.
#'
#' @return dataframe com o ensemble, variável de resposta e indicação se cada caso é um caso de interesse ou não.
#' @export
obter_df_vulnerabilidade = function(results, estrategia_candidata, variavel_resposta = "sNPVProfit1RegretPerc" , threshold = 0.1, planilha_inputs, sentido_vulnerabilidade = ">=") {
  
  if(sentido_vulnerabilidade == ">=") {
    results$AnaliseRegret$Dados$CasoInteresse = as.numeric(results$AnaliseRegret$Dados[,variavel_resposta] >= threshold)  
  } else {
    results$AnaliseRegret$Dados$CasoInteresse = as.numeric(results$AnaliseRegret$Dados[,variavel_resposta] <= threshold)  
  }
  
  # Obter Ensemble com Dados Simulados:
  ensemble_e_resultados = dplyr::inner_join(as.data.frame(results$Ensemble), results$AnaliseRegret$Dados, by = "Scenario")
  
  ensemble_e_resultados = ensemble_e_resultados[which(ensemble_e_resultados$Lever == estrategia_candidata),]
  
  # Retirar NAs do Ensemble
  ensemble_e_resultados = na.omit(ensemble_e_resultados)
  
  parametros_completos = readxl::read_xlsx(planilha_inputs, sheet = "params")
  
  variaveis_incertas = parametros_completos$Variavel[which(parametros_completos$Tipo=="Incerto")]
  
  x = ensemble_e_resultados[,c(variavel_resposta,"Scenario", "Lever",variaveis_incertas)]
  y = as.numeric(ensemble_e_resultados$CasoInteresse)
  
  data.frame(CasoInteresse = y, x)
}


#' obter_df_diff_media
#' Esta função serve para listar as variáveis que potencialmente mais distinguem os Casos de Interesse dos demais casos.
#' Esta função compara a média de cada variável dos casos de interesse com a média de todo o ensemble.
#' @param df_vulnerabilidade data.frame com a análise de vulnerabilidade retornado pela função obter_df_vulnerabilidade.
#'
#' @return data.frame que é um ranking de variáveis.
#' @export
#'
obter_df_diff_media_casos_interesse = function(df_vulnerabilidade) {
  medias_interesse = df_vulnerabilidade %>% dplyr::filter(CasoInteresse == 1) %>% dplyr::select(-CasoInteresse, -Scenario, -Lever, -sNPVProfit1Regret) %>% summarise_all(mean)
  
  medias_global = df_vulnerabilidade %>% dplyr::filter(CasoInteresse == 0) %>% dplyr::select(-CasoInteresse, -Scenario, -Lever, -sNPVProfit1Regret)  %>% dplyr::summarise_all(mean)
  
  max_global = df_vulnerabilidade %>% dplyr::select(-CasoInteresse, -Scenario, -Lever, -sNPVProfit1Regret) %>% dplyr::summarise_all(max)
  
  min_global = df_vulnerabilidade %>% dplyr::select(-CasoInteresse, -Scenario, -Lever, -sNPVProfit1Regret) %>% dplyr::summarise_all(min)
  
  range_global = max_global - min_global
  
  medias_dif = (medias_interesse - medias_global) / range_global
  
  v_medias_analisadas = colnames(medias_dif)
  
  v_medias_dif = unname(t(medias_dif)[,1])
  
  v_medias_global = unname(t(medias_global)[,1])
  
  v_range_global = unname(t(range_global)[,1])
  
  v_medias_interesse = unname(t(medias_interesse)[,1])
  
  ordem = order(abs(v_medias_dif), decreasing = TRUE)
  
  df_analise_medias = data.frame(
    Ranking = 1:length(v_medias_analisadas),
    Variavel = v_medias_analisadas[ordem],
    DifMediaRelativa = v_medias_dif[ordem],
    MediaCasosInteresse = v_medias_interesse[ordem],
    MediaGlobal = v_medias_global[ordem],
    Range = v_range_global[ordem]
  )  
}


#' obter_df_teste_t_casos_interesse
#' Realiza um Teste T para cada variável de incerteza. 
#' @param df_vulnerabilidade data.frame com a análise de vulnerabilidade retornado pela função obter_df_vulnerabilidade.
#'
#' @return data.frame que é um ranking de variáveis.
#' @export
#'
obter_df_teste_t_casos_interesse = function(df_vulnerabilidade) {
  
  casos_para_teste = df_vulnerabilidade %>% dplyr::select(-Scenario, -Lever, -sNPVProfit1Regret)
  
  # Teste T para Diferença de Médias
  casos_para_teste$CasoInteresse = as.factor(casos_para_teste$CasoInteresse)
  resultados_teste_t = t(sapply(casos_para_teste[-1], function(x) 
    unlist(t.test(x~casos_para_teste$CasoInteresse)[c("estimate","p.value","statistic")])))
  
  resultados_teste_t = as.data.frame(resultados_teste_t)
  
  resultados_teste_t$RejeitaH0_95Conf = resultados_teste_t$p.value < 0.05
  
  resultados_teste_t$RejeitaH0_99Conf = resultados_teste_t$p.value < 0.01
  
  resultados_teste_t$Variavel = rownames(resultados_teste_t)
  
  rownames(resultados_teste_t) = NULL
  
  est = unique(df_vulnerabilidade$Lever)
  
  names(resultados_teste_t) = c(paste("Média Est.",est,"não Falha"), paste("Média Est.",est,"Falha"), "Valor_P", "Est. T", "Rej. H0 95%", "Rej. H0 99%", "Variável")
  
  resultados_teste_t = dplyr::arrange(.data = resultados_teste_t, Valor_P)
  
  resultados_teste_t$Rank = 1:nrow(resultados_teste_t)
  
  resultados_teste_t = resultados_teste_t[,c(8,7,3,4,5,6,1,2)]
  
  resultados_teste_t
}




#' plot_violino_casos_interesse_por_variavel
#'
#' @param df_vulnerabilidade data.frame retornado pela função obter_df_vulnerabilidade
#' @param variavel nome da variável incerta
#' @param nome_amigavel_var nome amigável da variável incerta
#'
#' @return plot "violino" exibindo densidade da variável nos demais casos.
#' @export
#'
plot_violino_casos_interesse_por_variavel  = function(df_vulnerabilidade, variavel, nome_amigavel_var) {
  call_grafico = substitute(
    expr = ggplot2::ggplot(df_vulnerabilidade, aes(factor(CasoInteresse), Variavel)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +  geom_jitter(height = 0, width = 0.1)
    ,env = list(Variavel = as.name(variavel))
  )
  
  p <- eval(call_grafico) + xlab("Estratégia Vulnerável") + ylab(nome_amigavel_var) + scale_y_continuous(labels = format_for_humans)
  p
}



#' plot_dispersao_casos_interesse_por_variavel
#'
#' @param df_vulnerabilidade data.frame retornado pela função obter_df_vulnerabilidade
#' @param variavel1 nome da variável incerta 1 a considerar
#' @param nome_amigavel_var1 nome amigável desta variável
#' @param variavel2 nome da variável incerta 2 a considera
#' @param nome_amigavel_var2 nome amigável desta variável
#'
#' @return plot de dispersão sinalizando os casos de interesse
#' @export
#'
#' @examples
plot_dispersao_casos_interesse_por_variavel  = function(df_vulnerabilidade, variavel1, nome_amigavel_var1,  variavel2, nome_amigavel_var2) {
  call_grafico = substitute(
    expr =  ggplot(as.data.frame(df_vulnerabilidade), aes(x=Variavel1, y=Variavel2, color = as.factor(CasoInteresse)))
    ,env = list(Variavel1 = as.name(variavel1), Variavel2 = as.name(variavel2))
  )
  
  p =  eval(call_grafico)
  
  p = p + geom_point() + scale_color_manual(values = c("blue", "red"), name = "Caso de Interesse") + theme(legend.position = "bottom")
  
  p = p + xlab(nome_amigavel_var1) + ylab(nome_amigavel_var2) + scale_y_continuous(labels = format_for_humans) + scale_x_continuous(labels = format_for_humans)
  
  p
  
}


#' plot_dispersao_duas_variaveis
#' Gera um gráfico de dispersão de duas variáveis agrupando por lever.
#' @param df_dados data.frame retornado pela função obter_df_vulnerabilidade
#' @param variavel1 nome da variável incerta 1 a considerar
#' @param nome_amigavel_var1 nome amigável desta variável
#' @param variavel2 nome da variável incerta 2 a considera
#' @param nome_amigavel_var2 nome amigável desta variável
#' @param method pode ser lm, glm ou loess (consultar geom_smooth)
#' @param se default é TRUE, mostra intervalo de confiança.
#'
#' @return plot de dispersão sinalizando os casos de interesse
#' @export
#'
#' @examples
plot_dispersao_duas_variaveis  = function(df_dados, variavel1, nome_amigavel_var1,  variavel2, nome_amigavel_var2, linha_regr = TRUE, method = lm, se = TRUE, facet = TRUE) {
  call_grafico = substitute(
    expr =  ggplot(as.data.frame(df_dados), aes(x=Variavel1, y=Variavel2, color = factor(Lever)))
    ,env = list(Variavel1 = as.name(variavel1), Variavel2 = as.name(variavel2))
  )
  
  p =  eval(call_grafico)
  
  p = p + geom_point() + theme(legend.position = "bottom") # + scale_color_manual(values = c("blue", "red"), name = "Caso de Interesse") 
  
  # Adicionando Linha de Regressão
  if(linha_regr == TRUE){
    p = p + geom_smooth(method=method, aes(fill=factor(Lever)), show.legend = F, se = se)  
  }
  
  p = p + xlab(nome_amigavel_var1) + ylab(nome_amigavel_var2) + scale_y_continuous(labels = format_for_humans) + scale_x_continuous(labels = format_for_humans)
  
  
  p$labels$colour <- "Strategy"
  
  
  if(facet == TRUE){
    p = p + facet_wrap(~factor(Lever))
  }
  
  p
  
}


plot_dispersao_duas_variaveis_cor  = function(df_dados, variavel1, nome_amigavel_var1,  variavel2, nome_amigavel_var2, variavel_cor, method = lm, se = TRUE, facet = TRUE) {
  call_grafico = substitute(
    expr =  ggplot(as.data.frame(df_dados), aes(x=Variavel1, y=Variavel2, color = VariavelCor))
    ,env = list(Variavel1 = as.name(variavel1), Variavel2 = as.name(variavel2), VariavelCor = as.name(variavel_cor))
  )
  
  p =  eval(call_grafico)
  
  p = p + geom_point() + theme(legend.position = "bottom") # + scale_color_manual(values = c("blue", "red"), name = "Caso de Interesse") 
  
  p = p + xlab(nome_amigavel_var1) + ylab(nome_amigavel_var2) + scale_y_continuous(labels = format_for_humans) + scale_x_continuous(labels = format_for_humans)
  
  p$labels$colour <- variavel_cor
  
  p = p + scale_colour_gradient(low="red", high="green")
  
  if(facet == TRUE){
    p = p + facet_wrap(~factor(Lever))
  }
  
  p
  
}