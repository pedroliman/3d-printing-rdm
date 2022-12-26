
##### SIMULAR RDM E ESCOLHER ESTRATEGIA #####

#' simularRDM_e_escolher_estrategia
#'
#' @param inputs caminho para o arquivo de inputs
#' @param sdmodel list com modelo e suas opções
#' @param opcoes list com opções para a simulação e analise de Regret.
#'
#' @return list com resultados da simulacao e uma estratégia candidata.
simularRDM_e_escolher_estrategia = function(inputs = "params.xlsx", sdmodel = sdmodel, opcoes = opcoes, ensemble) {
  
  output_simulacao = simular_RDM(arquivo_de_inputs=inputs ,sdmodel = sdmodel, n = opcoes$N, opcoes = opcoes, ensemble = ensemble)
  
  ## Simular
  dados_simulacao = output_simulacao$DadosSimulacao
  
  # Selecionando dados do último ano:
  dados = selecionar_ultimo_periodo(dados_simulacao = dados_simulacao, var_tempo = opcoes$VarTempo)
  
  # Analisar Regret
  analise_regret = calcular_e_resumir_regret(dados = dados, var_resposta = opcoes$VarResposta, var_cenarios = opcoes$VarCenarios, var_estrategias = opcoes$VarEstrategias)
  
  # Escolher a Estratégia Candidata, com base no critério de robustez dos percentis
  estrategia_candidata = escolher_estrategia_candidata(dados = analise_regret$Dados, resumo_estrategias = analise_regret$ResumoEstrategias, var_resposta = opcoes$VarResposta, var_criterio = opcoes$VarCriterio, sentido = opcoes$SentidoCriterio)
  
  message(paste("A Estrategia candidata é a ", estrategia_candidata$Lever))
  
  output = list(
    DadosSimulados = dados_simulacao,
    DadosUltimoPeriodo = dados,
    AnaliseRegret = analise_regret,
    Inputs = output_simulacao$Inputs,
    Ensemble = output_simulacao$Ensemble,
    EstrategiaCandidata =  estrategia_candidata[opcoes$VarEstrategias],
    Opcoes = opcoes,
    SdModel = sdmodel
  )
  
  output
  
}


filtrar_casos_plausiveis = function(dados_simulacao, ensemble, ranges, opcoes = opcoes){
  
  n_cenarios_simulados = length(unique(dados_simulacao[,opcoes$VarCenarios]))
  
  cenarios_fora_do_range = sapply(1:nrow(ranges), definir_cenarios_fora_range, ranges = ranges, dados_simulacao = dados_simulacao, opcoes = opcoes)
  
  cenarios_fora_do_range = unique(unlist(cenarios_fora_do_range))
  
  if(length(cenarios_fora_do_range > 0)){
    message(paste0("Filtrando ",length(cenarios_fora_do_range), " ( ", round(100 * length(cenarios_fora_do_range)/n_cenarios_simulados,2)," %) dos ", n_cenarios_simulados ," cenarios simulados."))
  }
  
  dados_simulacao_filtrados = subset(dados_simulacao, !(Scenario %in% cenarios_fora_do_range)) 
  ensemble_filtrado = subset(dados_simulacao, !(Scenario %in% cenarios_fora_do_range)) 
  
  list(
    dados_simulacao_filtrados = dados_simulacao_filtrados,
    ensemble_filtrado = ensemble_filtrado,
    cenarios_fora_do_range = cenarios_fora_do_range
  )
}


#' definir_cenarios_fora_range
#' #' Esta função retorna o número dos cenários fora dos ranges definidos. Isto pode ser usado para filtrar cenários plausíveis ou ainda definir se um caso está ou não em um cenário (obtido com o PRIM, por exemplo).
#' 
#' @param n_var índice da variavel no data.frame de ranges
#' @param ranges data.framde de ranges (com as colunas Variavel, Min, e Max)
#' @param dados_simulacao data.frame com colunas correspondendo às variáveis do range.
#' @param opcoes objeto de opções.
#'
#' @return
#' @export
#'
#' @examples
definir_cenarios_fora_range = function(n_var, ranges = ranges, dados_simulacao = dados_simulacao, opcoes = opcoes) {
  
  variavel = ranges$Variavel[n_var]
  minimo = ranges$Min[n_var]
  maximo = ranges$Max[n_var]
  
  # cenarios onde a variavel não está no range:
  cenarios_viola_maximo = unique(dados_simulacao[which(dados_simulacao[,variavel] > maximo),opcoes$VarCenarios])
  cenarios_viola_minimo = unique(dados_simulacao[which(dados_simulacao[,variavel] < minimo),opcoes$VarCenarios])
  
  cenarios_fora_range = c(cenarios_viola_maximo, cenarios_viola_minimo)
  
  # informar o que aconteceu:
  if(length(cenarios_viola_maximo)>0){
    message(paste("Variavel", variavel, "acima do máximo(",maximo,").",  length(cenarios_viola_maximo), "cenarios descartados."))
  }
  
  # informar o que aconteceu:
  if(length(cenarios_viola_minimo)>0){
    message(paste("Variavel", variavel, "abaixo do minimo(",minimo,").", length(cenarios_viola_minimo), "cenarios descartados."))
  }
  
  cenarios_fora_range
}


##### CARREGAR INPUTS #####

#' carregar_inputs
#'
#' @param arquivo_de_inputs caminho para o arquivo de inputs com estratégias e incertezas
#' @param abas_a_ler abas a ler do arquivo de inputs
#' @param nomes_inputs Nome a ser atribuido aos dataframes de input.
#'
#' @return list com inputs para a simulação.
carregar_inputs = function (arquivo_de_inputs="params.xlsx", abas_a_ler = c("params", "levers", "Levers_FullDesign", "RangesPlausiveis"), nomes_inputs = c("Parametros", "Levers", "LeversFull", "RangesPlausiveis"), opcoes = opcoes) {
  
  # Criando uma list para os inputs
  message(
    paste("01. funcoes.R/carregar_inputs: Iniciando Carregamento de Inputs (funcao carregar_inputs()",
          "arquivo_de_inputs = ", arquivo_de_inputs)
  )
  inputs = vector(mode = "list", length = length(nomes_inputs))
  names(inputs) = nomes_inputs
  
  # Preenchendo os Dados dos Inputs
  for (aba in abas_a_ler) {
    n_aba = which(aba == abas_a_ler)
    inputs[[n_aba]] = readxl::read_excel(arquivo_de_inputs,sheet = aba)
  }
  
  
  # Substituir Levers aqui mesmo:
  # Substituindo Levers por Proketo Fatorial Completo, se isto foi selecionado:
  # Gerar um Fatorial Completo das Variáveis, se for necessário
  if(opcoes$FullFactorialDesign){
    var_levers = na.omit(expand.grid(inputs$LeversFull))
    n_levers = nrow(var_levers)
    inputs$Levers = data.frame(Lever = 1:n_levers,
                               LeverCode = as.character(1:n_levers),
                               CasoBase = c(1, rep(0, n_levers-1)),
                               var_levers)
  }
  
  message("01. funcoes.R/carregar_inputs: Finalizando Carregamento de Inputs.")
  return(inputs)
  
  
  
}


##### OBTER ENSEMBLE - PARÂMETROS #####

#' obter_lhs_ensemble
#'
#' @param params dataframe de parâmetros a usar (no padrão pré-determinado)
#' @param n tamanho do ensemble a montar
#'
#' @return dataframe com ensemble montado (pronto para a simulação)
obter_lhs_ensemble = function (params, n=100, opcoes = opcoes) {
  message("01. funcoes.R/obter_lhs_ensemble: Iniciando Obtenção do Ensemble.")
  #Obtendo DataFrame de Parâmetros
  
  nvar = length(params$Variavel)
  pontos = n
  
  # Obtendo um Hypercubo com as Variáveis que eu quero
  randomLHS <- lhs::randomLHS(pontos, nvar)
  
  p = as.data.frame(randomLHS)
  min = as.vector(params$Min)
  max = as.vector(params$Max)
  variaveis = as.vector(params$Variavel)
  
  # Transformando o Hypercubo em variáveis
  # var <- matrix(nrow=pontos, ncol=variaveis)
  ensemble = matrix(nrow = pontos, ncol = nvar+1)
  
  # Montando o Ensemble
  for (var in variaveis) {
    i = which(x = variaveis == var)
    
    # Aqui o i é +1 porque a primeira coluna será o cenário.
    ensemble[,i+1] = qunif(p = randomLHS[,i], min = min[i], max = max[i])
  }
  
  # Adicionando A variável "Scenario"
  variaveis = c(c(opcoes$VarCenarios),variaveis)
  
  colnames(ensemble) = variaveis
  
  ensemble[,opcoes$VarCenarios] = 1:nrow(ensemble)
  
  ensemble
}

##### AMPLIAR ENSEMBLE COM ESTRATÉGIAS #####

#' ampliar_ensemble_com_levers
#'
#' @param ensemble conjunto de cenarios a simular
#' @param levers conjunto de estratégias a simular
#'
#' @return dataframe com a combinação de todas as estratégias em todos os cenários.
ampliar_ensemble_com_levers = function(ensemble, levers, levers_full, opcoes) {
  
  variaveis_adicionais = names(dplyr::select(levers, -LeverCode, -CasoBase))
  
  # Filtrar cenários a simular caso seja necessário apenas simular o Caso Base:
  if(opcoes$SimularApenasCasoBase){
    levers = subset(levers, as.logical(CasoBase))
  }
  
  linhas_ensemble_incial = nrow(ensemble)
  novo_ensemble = matrix(0, nrow = nrow(ensemble)*length(levers$Lever), ncol = ncol(ensemble) + length(variaveis_adicionais))
  
  names_old_ensemble = colnames(ensemble)
  names_novo_ensemble = c(names_old_ensemble, variaveis_adicionais)
  
  colnames(novo_ensemble) = names_novo_ensemble
  
  j = 1
  for (l in seq_along(levers$Lever)) {
    lini = j
    lfim = j + linhas_ensemble_incial-1
    matriz_var_adicionais = as.matrix(levers[l,variaveis_adicionais])
    novo_ensemble[lini:lfim,names_old_ensemble] = ensemble
    novo_ensemble[lini:lfim,variaveis_adicionais] = matrix(matriz_var_adicionais, nrow = linhas_ensemble_incial, ncol = ncol(matriz_var_adicionais), byrow = TRUE)
    j = j + linhas_ensemble_incial
  }
  
  novo_ensemble
  
}


##### SIMULAR #####

#' simular
#'
#' @param stocks integrais a serem resolvidas numéricamente. (numeric) 
#' @param simtime Tempo de simulação (numeric)
#' @param modelo Modelo de dinâmica de sistemas no padrão do deSolve (function)
#' @param ensemble ensemble montado (pronto para a simulação)
#' @param nomes_variaveis_final vetor com nomes de variáveis
#' @param paralelo TRUE ou false (roda no modo paralelo ou Falso)
#' @param modo_paralelo "FORK" ou "PSOCK". Usar "FORK" no Linux e Mac, e "PSOCK" no Windows.
#'
#' @return
#' @export
#'
#' @examples
simular = function(simtime, modelo, ensemble, nomes_variaveis_final, opcoes = opcoes) {
  message("01. funcoes.R/simular: Iniciando Simulação.")
  
  paralelo = opcoes$Paralelo
  
  modo_paralelo = opcoes$ModoParalelo
  
  # Rodando a Simulação (uma vez), com a primeira linha do ensemble - Ajuda a saber se funciona.
  # Esta função apenas funciona com o estoque inicial fixo, será necessário implementar de outra forma depois.
  t_inicio_teste = Sys.time()
  o = as.data.frame(solve_modelo_dissertacao(parametros = ensemble[1,], modelo = modelo, simtime = simtime)) 
  
  t_fim_teste = Sys.time()
  t_uma_rodada = as.numeric(difftime(time1 = t_fim_teste, time2 = t_inicio_teste, units = "secs"))
  
  # Esta funcao Executa a Resolução do Modelo para uma dada linha do Ensemble.
  # O Ideal para o modo paralelo seria executar "n" linhas do ensemble por vez.
  
  solve_modelo = function(n_linha_ensemble) {
    params = ensemble[n_linha_ensemble,]
    res = solve_modelo_dissertacao(parametros = params, modelo = modelo, simtime = simtime)
    # Gerar Matriz de Resultados
    cbind(res,
          Lever = ensemble[n_linha_ensemble,opcoes$VarEstrategias],
          Scenario = ensemble[n_linha_ensemble,opcoes$VarCenarios])
  }
  
  
  
  # Forma não elegante de resolver o problema:
  solve_modelo_batch = function(n_linhas_por_vez, linha_inicial) {
    
    first_result = solve_modelo(linha_inicial)
    
    for (i in (linha_inicial+1):(n_linhas_por_vez+linha_inicial-1)) {
      results_solve_modelo = solve_modelo(i)
      
      if (i == (linha_inicial + 1)) {
        results = rbind(first_result, results_solve_modelo)
      } else {
        results = rbind(results, results_solve_modelo)
      }
      
    }
    
    results
    
  }
  
  
  # Teste para o Azure - Passar tudo por parâmetro
  solve_modelo_batch_azure = function(n_linhas_por_vez, linha_inicial, ensemble, modelo, simtime, START, FINISH, VERIFICAR_STOCKS, VERIFICAR_CHECKS, VAR_LEVER, VAR_SCENARIO, INICIALIZAR_ESTOQUES_COM_CASO_BASE, SIMULAR_HISTORICO_DIFERENTE, ANO_INICIO_AVALIACAO, VERIFICAR_GLOBAL, opcoes, STEP, solve_modelo_dissertacao) {
    
    ensemble = ensemble
    modelo = modelo
    simtime = simtime
    START = START
    FINISH = FINISH
    VERIFICAR_STOCKS = VERIFICAR_STOCKS
    VERIFICAR_CHECKS = VERIFICAR_CHECKS
    VAR_LEVER = VAR_LEVER
    VAR_SCENARIO = VAR_SCENARIO
    INICIALIZAR_ESTOQUES_COM_CASO_BASE = INICIALIZAR_ESTOQUES_COM_CASO_BASE
    SIMULAR_HISTORICO_DIFERENTE = SIMULAR_HISTORICO_DIFERENTE
    ANO_INICIO_AVALIACAO = ANO_INICIO_AVALIACAO
    VERIFICAR_GLOBAL = VERIFICAR_GLOBAL
    opcoes = opcoes
    STEP = STEP
    solve_modelo_dissertacao = solve_modelo_dissertacao
    
    first_result = solve_modelo(linha_inicial)
    
    for (i in (linha_inicial+1):(n_linhas_por_vez+linha_inicial-1)) {
      results_solve_modelo = solve_modelo(i)
      
      if (i == (linha_inicial + 1)) {
        results = rbind(first_result, results_solve_modelo)
      } else {
        results = rbind(results, results_solve_modelo)
      }
      
    }
    
  }
  
  if(paralelo == TRUE) {
    
    t_inicio = Sys.time()
    message(t_inicio)
    
    if((modo_paralelo == "PSOCK") | (modo_paralelo == "FORK")){
      # Calculate the number of cores
      no_cores <- detectCores() - 1
      # Inicializar Cluster
      cl <- makeCluster(no_cores, type = modo_paralelo)
      
      # Carregando bibliotecas no cluster:
      #clusterEvalQ(cl, source("funcoes.R"))
      #clusterEvalQ(cl, eval(parse('funcoes.R')))
      # textConnection(animal.R)
      
      if(modo_paralelo  == "PSOCK") {
        # Se o modo paralelo é "PSOCK", é necessário definir explícitamente as variáveis que devem ir para o cluster
        # http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
        clusterEvalQ(cl, library(deSolve))
        
        # Exportando objetos que preciso ter nos clusters:
        clusterExport(cl, varlist = list("ensemble", 
                                         "modelo", 
                                         "simtime",
                                         "START",
                                         "FINISH", 
                                         "VERIFICAR_STOCKS", 
                                         "solve_modelo", 
                                         "VERIFICAR_CHECKS",
                                         "VAR_LEVER",
                                         "VAR_SCENARIO",
                                         "INICIALIZAR_ESTOQUES_COM_CASO_BASE",
                                         "SIMULAR_HISTORICO_DIFERENTE",
                                         "ANO_INICIO_AVALIACAO",
                                         "VERIFICAR_GLOBAL",
                                         "opcoes",
                                         "STEP",
                                         "solve_modelo_dissertacao"), envir = environment())  
      }
      
      
      message(paste("Iniciando Simulacao em modo paralelo. Usando", no_cores, "núcleos."))
      
      # t_uma_rodada = 0.583717
      
      t_estimado = t_uma_rodada * nrow(ensemble)/no_cores
      message(paste("Tempo estimado (segundos):"),t_estimado)
      
      # Aplicando Função paralela para a computação dos resultados
      dados_simulacao <- parLapply(cl, 1:nrow(ensemble), solve_modelo)
      stopCluster(cl)
      
      # Unindo Dados da Simulação, seja entregues pelo Azure seja pelo meu
      dados_simulacao = do.call(rbind, dados_simulacao)
      
    }
    
    if(modo_paralelo == "Azure") {
      
      browser()
      
      # 3. Set your credentials - you need to give the R session your credentials to interact with Azure
      setCredentials("credentials.json")
      
      # 4. Register the pool. This will create a new pool if your pool hasn't already been provisioned
      browser()
      
      cluster <- doAzureParallel::makeCluster("cluster.json")
      
      browser()
      
      # 5. Register the pool as your parallel backend
      registerDoAzureParallel(cluster)
      
      browser()
      
      # 6. Check that your parallel backend has been registered
      # getDoParWorkers()
      
      # GErando resultado
      # browser()
      
      # O Tamanho do Ensemble precisa ser multiplo do número de tarefas
      numero_de_tarefas = 10
      
      linhas_por_vez = nrow(ensemble) / numero_de_tarefas
      
      vetor_tarefas = seq(from = 1, by = linhas_por_vez, length.out = numero_de_tarefas)
      
      browser()
      
      # Primeira tentativa
      # dados_simulacao <- foreach(i = vetor_tarefas, .combine='rbind') %dopar% {
      #   solve_modelo_batch(n_linhas_por_vez = linhas_por_vez, linha_inicial = i)
      # }
      
      # Segunda Tentativa - Também Não funcionou. Parece que tudo precisa estar encapsulado na função.
      # dados_simulacao <- foreach(i = 1:nrow(ensemble), .combine='rbind') %dopar% {
      #    as.data.frame(solve_modelo(i))
      # }
      
      # Terceira tentativa -Tentando passar os parâmetros por "força bruta"
      dados_simulacao <- foreach(i = vetor_tarefas, .combine='rbind') %dopar% {
        solve_modelo_batch_azure(n_linhas_por_vez = linhas_por_vez, linha_inicial = i, ensemble, modelo, simtime, START, FINISH, VERIFICAR_STOCKS, VERIFICAR_CHECKS, VAR_LEVER, VAR_SCENARIO, INICIALIZAR_ESTOQUES_COM_CASO_BASE, SIMULAR_HISTORICO_DIFERENTE, ANO_INICIO_AVALIACAO, VERIFICAR_GLOBAL, opcoes, STEP, solve_modelo_dissertacao)
      }
      
      
      browser()
      
    }
    
    
    
    t_fim = Sys.time()
    
    # perf = t_estimado / as.numeric(difftime(time1 = t_fim, time2 = t_inicio, units = "secs")) / 0.75
    # message(t_fim)
    message("Finalizando Simulacao. Finalizando Cluster")
    # message(paste("Indice de Performance",perf))
    #resultados_paralelo = lapply(1:nrow(ensemble), solve_modelo)
    dados_simulacao = as.data.frame(dados_simulacao)
    
  }
  
  if(paralelo == FALSE){
    # J é o índice dos dados simulados
    j = 1
    # Rodando a Simulacao Em todo o Ensemble
    # Fazer os calculos da maneira anterior aqui.
    nomes_temporario = names(o)
    
    # o<-data.frame(ode(y=stocks, times=simtime, func = modelo, 
    #                   parms=ensemble[1,], method="euler"))
    pontos = nrow(ensemble)
    
    nlinhas = nrow(o)
    
    ncolunas = ncol(o)+2
    
    # Montando uma matriz com todos os dados para a simulação
    dados_simulacao = matrix(nrow = pontos*nlinhas, ncol = ncolunas)
    
    for (i in 1:nrow(ensemble)) {
      resultados_simulacao = as.data.frame(solve_modelo_dissertacao(parametros = ensemble[i,], modelo = modelo, simtime = simtime)) 
      
      resultados_simulacao = as.matrix(resultados_simulacao)
      
      linhas = nrow(resultados_simulacao)
      
      # Avançando a linha inicial e Final da Simulação
      l_inicial = j
      l_final = j + linhas-1
      
      # Adicionando o resultado ao ensemble
      dados_simulacao[l_inicial:l_final,1:(ncolunas-2)] = resultados_simulacao
      
      # Adicionando o Número do Lever
      dados_simulacao[l_inicial:l_final,(ncolunas-1)] = ensemble[i,opcoes$VarEstrategias]
      
      # Adicionando o Número do Cenário
      dados_simulacao[l_inicial:l_final,ncolunas] = ensemble[i,opcoes$VarCenarios]
      
      # Exibindo uma Mensagem de Status
      if (i %% 5 == 0) {
        message(paste(i, "simulações finalizadas."))
      }
      # Avançando o índice dos dados simulados
      j = j + linhas
    }
    # Usando nomes temporario
    colnames(dados_simulacao) = c(nomes_temporario, opcoes$VarEstrategias, opcoes$VarCenarios)
    # colnames(dados_simulacao) = nomes_variaveis_final
    
    dados_simulacao = as.data.frame(dados_simulacao)
    names(dados_simulacao) = c(nomes_temporario, opcoes$VarEstrategias, opcoes$VarCenarios)
    #names(dados_simulacao) = nomes_variaveis_final
  }
  
  message("01. funcoes.R/simular: Finalizando Simulacao.")
  
  dados_simulacao
}


##### SIMULAR RDM #####

#' simular_RDM
#'
#' @param arquivo_de_inputs Caminho para o arquivo de dados padronizado com Estrategias e Incertezas (character)
#' @param sdmodel Lista com variáveis para simulação de dinamica de sistemas
#' @param n Número de cenarios a gerar (numeric)
#'
#' @return data.frame com resultados da simulação
simular_RDM = function(arquivo_de_inputs="params.xlsx", sdmodel, n = opcoes$N, opcoes = opcoes, ensemble){
  t_inicio = Sys.time()
  message("Bem vindo ao SIMULADOR RDM! Pedro Lima.")
  message(paste("Iniciando Simulacao RDM: ", t_inicio))
  
  # Carregando Inputs
  inputs = carregar_inputs(arquivo_de_inputs = arquivo_de_inputs, opcoes = opcoes)
  
  # Substituindo Levers por Proketo Fatorial Completo, se isto foi selecionado:
  # Gerar um Fatorial Completo das Variáveis, se for necessário
  # if(opcoes$FullFactorialDesign){
  #   var_levers = na.omit(expand.grid(inputs$LeversFull))
  #   n_levers = nrow(var_levers)
  #   inputs$Levers = data.frame(Lever = 1:n_levers,
  #                       LeverCode = as.character(1:n_levers),
  #                       CasoBase = c(1, rep(0, n_levers-1)),
  #                       var_levers)
  # }
  # 
  
  # Obter Ensemble LHS (Sem Variáveis das Estratégias)
  
  # Se um ensemble não foi informado, gerar um ensemble.
  if(missing(ensemble)){
    # Usar o primeiro lever que eu achar
    ensemble = obter_lhs_ensemble(params = inputs$Parametros, n = n, opcoes = opcoes)
  }
  
  # Ampliar Ensemble com as variáveis das Estratégias
  novo_ensemble = ampliar_ensemble_com_levers(ensemble = ensemble, levers = inputs$Levers,levers_full = inputs$LeversFull, opcoes = opcoes)
  
  # Rodando a Simulação
  nestrategias = length(inputs$Levers$Lever)
  nfuturos = nrow(ensemble)
  ntempo = ((sdmodel$Finish - sdmodel$Start)/sdmodel$Step)
  
  message(paste("Esta rotina realizará", nestrategias * nfuturos, "Simulacoes.\n (", nestrategias, "estratégias x", nfuturos, "futuros, em", ntempo , "periodos de tempo."))
  
  # TODO: Esta Chamada vai precisar mudar para considerar a nova funcao
  dados_simulacao = simular(simtime = sdmodel$SimTime, modelo = sdmodel$Modelo, ensemble = novo_ensemble, nomes_variaveis_final = sdmodel$Variaveis, opcoes = opcoes)
  
  t_fim = Sys.time()
  
  message("Finalizando Simulacao. Tempo de Simulacao: ", t_fim - t_inicio)
  
  
  # Filtrando Cenarios que sao plausiveis
  if(opcoes$FiltrarCasosPlausiveis) {
    message("Filtrando Casos Plausiveis.")
    resultados_filtrados = filtrar_casos_plausiveis(dados_simulacao, ensemble = novo_ensemble, ranges = inputs$RangesPlausiveis, opcoes = opcoes)  
    dados_simulacao = resultados_filtrados$dados_simulacao_filtrados
    novo_ensemble = resultados_filtrados$ensemble_filtrado
    message(paste("Índices dos Cenarios Filtrados:", resultados_filtrados$cenarios_fora_do_range, collapse = ", "))
  }
  
  
  output = list(
    Inputs = inputs,
    Ensemble = ensemble,
    NovoEnsemble = novo_ensemble,
    DadosSimulacao = dados_simulacao
  )
  
  output
  
}