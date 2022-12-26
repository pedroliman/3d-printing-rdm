


#### 4.1 Teste com Dados Históricos de Demanda ####
# Simulação 0: Simulando Histórico e Observando Fit do Modelo:
###
opcoes$SimularApenasCasoBase = TRUE
opcoes$N = 200
# Esta opção faz com que os estoques sejam inicializados com o valor inicial dos estoques no cenário base.
INICIALIZAR_ESTOQUES_COM_CASO_BASE = FALSE
SIMULAR_HISTORICO_DIFERENTE = FALSE
# Esta opção é utilizada para modificar o comportamento de variáveis selecionadas (ex.: Estratégia do player) durante o período histórico.
# Se ativado, o período de tempo anterior ao ANO_INICIO_AVALIACAO assume variáveis com um valor "default", e o NPV dos players não é
# modificado enquanto até o ANO_INICIO_AVALIACAO. Se não ativado, a simulação ocorre normalmente.
ANO_INICIO_AVALIACAO = 2018
planilha_inputs = planilha_simulacao_calibracao_historico
opcoes$FiltrarCasosPlausiveis = FALSE
# Rodar Simulação:
START<-2004; FINISH <-2014; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;
invisible(sapply(X = paste0(list.files(path = "./R/functions/", pattern = "*.R",full.names = T)),FUN = source, echo = F)) 


resultados_casos_plausiveis = simularRDM_e_escolher_estrategia(inputs = planilha_inputs,
                                                               sdmodel = sdmodel,
                                                               opcoes = opcoes)




list_tabelas_output[["ParametrosCalibracao"]] <- resultados_casos_plausiveis$Inputs$Parametros

# Salvar resultados com casos plausíveis:
#save(resultados_casos_plausiveis, file = "/home/pedro/Documents/dev/ms-rdm-dissertation-dados-temp/resultados_casos_plausiveis.rda")
save(resultados_casos_plausiveis, file = "resultados_casos_plausiveis.rda")

load("resultados_casos_plausiveis.rda")




# load(file = "/home/pedro/Documents/dev/ms-rdm-dissertation-dados-temp/resultados_casos_plausiveis.rda")


variavel_calibracao = "fIndustryOrderRate"
variavel_calibracao = "aIndustryShipments"
nome_amigavel_variavel_calibracao = "Demanda Global"

# Parâmetros Utilizados
resultados_casos_plausiveis$Inputs$Parametros

# Mostrar Ensemble.

head(resultados_casos_plausiveis$Ensemble, 10)
# Gerar 10.000 replicações e salvar.

# Mostrar Dados Simulados
head(resultados_casos_plausiveis$DadosSimulados, 20)

# Mostrar Variável de Demanda em Todos os Cenários (Sem Filtro)

cenarios_a_exibir_grafico = sample(1:opcoes$N,size = min(50,opcoes$N))


plot_demanda_pre_calibracao = plot_linha_uma_variavel_ensemble(dados = subset(resultados_casos_plausiveis$DadosSimulados, Scenario %in% cenarios_a_exibir_grafico), 
                                                               variavel = variavel_calibracao, 
                                                               nome_amigavel_variavel = nome_amigavel_variavel_calibracao) #+ geom_vline(xintercept = 2017)

plot_demanda_pre_calibracao = plot_demanda_pre_calibracao # + annotate("text", x = 2017.2, y = max(resultados_casos_plausiveis$DadosSimulados$fIndustryOrderRate), label=c("Hoje"),hjust=0)

plot_demanda_pre_calibracao

# Comparar Simulações com Dados históricos de Demanda
ensemble_com_erro = adicionar_erro_ao_ensemble(results = resultados_casos_plausiveis, variavel_calibracao = variavel_calibracao, planilha_calibracao = "./calibracao/dados_calibracao.xlsx", opcoes = opcoes)
ensemble_com_erro = as.data.frame(ensemble_com_erro)


# Exibir Comparação com Dados Históricos.

variaveis_analise_fit = c("RSquared", "r", "RootMeanSquareError", "SumOfSquareResiduals", "MeanSquareError", "MeanAbsoluteError", "MeanAbsolutePercentError", "UM_ThielBiasDiffMeans", "US_ThielUnequalVariation", "UC_ThielUnequalCovariation")
variaveis_exibir_ensemble = c("Scenario", variaveis_analise_fit)
cenarios_a_exibir_tabela = sample(1:opcoes$N,size = 5)

# Demonstração do erro calculado
tabela_de_erro_calculado = t(ensemble_com_erro[1:5,variaveis_exibir_ensemble])

tabela_de_erro_calculado = as.data.frame(tabela_de_erro_calculado)

tabela_de_erro_calculado[,"VariavelCalculada"] = rownames(tabela_de_erro_calculado)

list_tabelas_output[["CalibracaoErroCalculado"]] <- tabela_de_erro_calculado

tabela_de_erro_calculado

# Exibindo Ordem de Grandeza dos Erros Médios Absolutos

histograma_erro_percentual = ggplot(ensemble_com_erro, aes(x=MeanAbsolutePercentError)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Erro Médio Percentual") + 
  ylab("Densidade")


histograma_erro_percentual


histograma_erro_medio_quadrado = ggplot(ensemble_com_erro, aes(x=MeanSquareError)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Erro Médio Quadrado") + 
  ylab("Densidade")

histograma_erro_medio_quadrado


# Exibir caso com melhor Fit
cenario_menor_erro = ensemble_com_erro[which(ensemble_com_erro[,"MeanSquareError"]==min(ensemble_com_erro[,"MeanSquareError"])),opcoes$VarCenarios]

# Exibir parâmetros do cenario com menor erro:

params_cenario_menor_erro = ensemble_com_erro[which(ensemble_com_erro[,opcoes$VarCenarios]==cenario_menor_erro),]

parametros_cenario_menor_erro = t(ensemble_com_erro[which(ensemble_com_erro[,opcoes$VarCenarios]==cenario_menor_erro),])

parametros_cenario_menor_erro

write.csv2(parametros_cenario_menor_erro, "output_calibracao.csv")

parametros_cenario_menor_erro = as.data.frame(parametros_cenario_menor_erro)

parametros_cenario_menor_erro[,"Parâmetro"] = rownames(parametros_cenario_menor_erro)

list_tabelas_output[["ParametrosCenarioMenorErro"]] <- parametros_cenario_menor_erro

# Condições Iniciais do cenário com Menor Erro:

resultados_casos_plausiveis$DadosUltimoPeriodo[which(resultados_casos_plausiveis$DadosUltimoPeriodo$Scenario==cenario_menor_erro),]

# Condições Finais do Cenário com Menor erro (pode ser usado como base):

VARIAVEIS_FINAIS_CASO_BASE = resultados_casos_plausiveis$DadosUltimoPeriodo[which(resultados_casos_plausiveis$DadosUltimoPeriodo$Scenario==cenario_menor_erro),]

# Plotar Gráfico do Cenário com Menor Erro:
# Selecionando Pontos de Dados para Exibir no Gráfico
time_points<-seq(from=1, to=length(SIM_TIME),by=1/STEP)
time_plot = seq(from=START, to=FINISH)
resultados_exibir = dplyr::filter(resultados_casos_plausiveis$DadosSimulados, Scenario == cenario_menor_erro)[time_points,]

# Exibir Demanda Global.
dados_calibracao <- as.data.frame(read_xlsx(path = "./calibracao/dados_calibracao.xlsx", sheet = "Plan1"))
# Exibir Share dos Players e outras variáveis.

plot_cenario_base_e_historico <-ggplot()+
  geom_point(data=dados_calibracao,size=1.5,aes(time,aIndustryShipments,colour="Data"))+
  geom_line(data=resultados_exibir,size=1,aes(x=time,y=aIndustryShipments,colour="Model"))+
  ylab("Professional 3D Printers Demand")+
  xlab("Years")+
  scale_y_continuous(labels = format_for_humans)+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",
                      values=c(Data="red", 
                               Model="blue"),
                      labels=c("Data",
                               "Calibrated Model")) + 
  ggtitle("Model fit to Historical Data")
plot_cenario_base_e_historico 



# Definição de Casos Considerados Plausíveis para a Geração de Ensemble 2.0 e 2.1

percentil_ssr = quantile(ensemble_com_erro[,"SumOfSquareResiduals"], probs = c(percentil_utilizado_como_criterio))

cenarios_quartis = ensemble_com_erro[which(ensemble_com_erro[,"SumOfSquareResiduals"] <= percentil_ssr),opcoes$VarCenarios]

cenarios_considerados_plausiveis = cenarios_quartis

# Como critério irei utilizar apenas os casos que erram em média menos do que 30 %.
#erro_percentual_medio_maximo = 0.5

dados_calibracao$Scenario = 1000

# cenarios_considerados_plausiveis = ensemble_com_erro[which(ensemble_com_erro$MeanAbsolutePercentError < erro_percentual_medio_maximo),opcoes$VarCenarios]

plot_cenarios_plausiveis = plot_linha_uma_variavel_ensemble(dados = resultados_casos_plausiveis$DadosSimulados[which(resultados_casos_plausiveis$DadosSimulados$Scenario %in% cenarios_considerados_plausiveis),]
                                                            ,variavel = variavel_calibracao, nome_amigavel_variavel = nome_amigavel_variavel_calibracao) + geom_point(data=dados_calibracao, size = 1.5, aes(time, aIndustryShipments))

plot_cenarios_plausiveis



# Gerando Gráficos da Calibração
plots_calibracao = list(
  calibracao_plot_cenarios_plausiveis = plot_cenarios_plausiveis,
  calibracao_plot_cenario_base_e_historico = plot_cenario_base_e_historico,
  calibracao_plot_demanda_pre_calibracao = plot_demanda_pre_calibracao,
  calibracao_histograma_erro_percentual = histograma_erro_percentual,
  calibracao_histograma_erro_medio_quadrado = histograma_erro_medio_quadrado
)

# Salvando Gráficos da Calibração.
mapply(ggsave, file=paste0("./images/", names(plots_calibracao), ".png"), plot=plots_calibracao, width = plots_width, height = plots_heigh)


tabelas_calibracao = list(
  calibracao_parametros_cenario_menor_erro = parametros_cenario_menor_erro,
  calibracao_tabela_de_erro_calculado = tabela_de_erro_calculado
)

# Salvando Tabelas da Calibração:
mapply(write.csv2, 
       x = tabelas_calibracao, 
       file = paste0("./tabelas/",names(tabelas_calibracao),".csv")
)
