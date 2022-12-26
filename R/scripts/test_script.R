
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

invisible(sapply(X = paste0(list.files(path = "./R/functions/", pattern = "*.R",full.names = T)),FUN = source, echo = F)) 

a = 1

print(a)

saveRDS(a, file = paste0("./outputs/test_results.rds"))