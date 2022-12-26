


##### FUNÇÕES AUXILIARES #####

# select last period
selecionar_ultimo_periodo = function(dados_simulacao, var_tempo) {
  call = substitute(
    expr = dados_simulacao %>% dplyr::filter(Tempo == max(Tempo)),
    env = list(Tempo = as.name(var_tempo)))
  eval(call)  
}


selecionar_ultimo_periodo = function(dados_simulacao, var_tempo) {
  call = substitute(
    expr = dados_simulacao %>% dplyr::filter(Tempo == max(Tempo)),
    env = list(Tempo = as.name(var_tempo)))
  eval(call)  
}

# max by variable
calcular_maximo_por_variavel = function(var_resposta, var_group, dados) {
  call = substitute(
    expr = {dplyr::group_by(dados, VarGroup) %>%
        dplyr::summarise(Maximo = max(VarResposta))
    }
    ,
    env = list(VarGroup = as.name(var_group), VarResposta = as.name(var_resposta)))
  
  max_variavel_resposta = eval(call)
  
  dados_join = dplyr::inner_join(dados, max_variavel_resposta)
  
  dados_join$Maximo
}

# min by variable
calcular_minimo_por_variavel = function(var_resposta, var_group, dados) {
  call = substitute(
    expr = {dplyr::group_by(dados, VarGroup) %>%
        dplyr::summarise(Minimo = min(VarResposta))
    }
    ,
    env = list(VarGroup = as.name(var_group), VarResposta = as.name(var_resposta)))
  
  max_variavel_resposta = eval(call)
  
  dados_join = dplyr::inner_join(dados, max_variavel_resposta)
  
  dados_join$Minimo
}

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


#### Funções Auxiliares ####
# Funcao para formatar números para humanos.
# https://stackoverflow.com/questions/46657442/understanding-vectorisation
format_for_humans <- function(x, digits = 3){
  grouping <- pmax(floor(log(abs(x), 1000)), 0)
  paste(signif(x / (1000 ^ grouping), digits = digits), 
        c('', 'K', 'M', 'B', 'T')[grouping + 1],sep = " ")
}


transf_colunas_em_vetor = function(x) {as.vector(t(x))}

format_percentage_for_humans = function(x, digits = 1){
  paste(round(100 * x, digits), "%", sep = "")
}

format_currency_for_humans = function(x, currency = "$", digits = 4) {
  paste(currency, format_for_humans(x, digits = digits), sep = " ")
}