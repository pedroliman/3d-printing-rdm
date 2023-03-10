

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



#### DADOS EXTERNOS ####

#' obter_dados_fundamentos_us_fundamentals
#' Esta rotina obtem dados do site http://www.usfundamentals.com/ usando a api deles. Esta rotina também trata os dados obtidos e os deixa prontos para serem usados em gráficos pelo ggplot2.
#' Todos os dados baixados são mantidos na pasta /fundamentals-data/ para consulta posterior.
#' 
#' @param api_us_fundamentals chave para a api
#' @param empresas nomes de empresas a buscar na base (pode ser definido arbitráriamente).
#' @param codigos códigos de empresas a buscar na base
#' @param indicadores vetor com o nome dos indicadores a manter
#'
#' @return
#' @export
#'
#' @examples
obter_dados_fundamentos_us_fundamentals = function(api_us_fundamentals = "AzfxuwuOWMDrCA28nAEUcw", empresas = c("Stratasys Inc", "3D Systems Corp", "Exone CO", "Proto Labs"), codigos = c(915735, 910638, 1561627, 1443669), indicadores = c("Revenues", "OperatingIncomeLoss", "SalesRevenueNet ", "GrossProfit", "NetCashProvidedByUsedInOperatingActivities", "ResearchAndDevelopmentExpense")) {
  codigos_api = paste(codigos, collapse = ",")
  
  indicadores_api = paste(indicadores, collapse = ",")
  
  call_api = paste(
    "https://api.usfundamentals.com/v1/indicators/xbrl?",
    "&companies=",
    codigos_api,
    "&period_type=yq",
    "&token=",
    api_us_fundamentals
    , sep = ""
  )
  
  anos = 2011:2016
  
  csv_dados_us_fundamentals <- RCurl::getURL(call_api)
  
  dados_us_fundamentals = read.csv(textConnection(csv_dados_us_fundamentals))
  
  # renomeando colunas
  colnames(dados_us_fundamentals) = c("company_id", "indicator_id", anos)
  
  # Gerando tabela de empresas
  empresas_e_codigos = data.frame(company_id = codigos, empresa = empresas)
  
  # Indicando nome da empresa
  dados_us_fundamentals = dplyr::inner_join(dados_us_fundamentals, empresas_e_codigos)
  
  
  # Escrevendo CSV para guardar os dados antes de filtrar:
  write.csv2(x = dados_us_fundamentals, file = "./fundamentals-data/dados_us_fundamentals.csv")
  
  # Filtrando só o indicador desejado, ou definindo um conjunto de indicadores
  if (length(indicadores)> 0){
    dados_us_fundamentals = subset(dados_us_fundamentals, indicator_id %in% indicadores)  
  } else {indicadores = unique(dados_us_fundamentals$indicator_id)}
  
  dados_finais = dados_us_fundamentals %>% tidyr::gather(Ano, Valor, 3:8) %>% tidyr::spread(indicator_id, Valor)
  
  dados_finais  
}


#' obter_fundamentos_financeiros_quandl
#' Função obtém dados de fundamentos financeiros usando a biblioteca quandl.
#' @param company_code código da empresa constante na base de dados https://www.quandl.com/data/SF0-Free-US-Fundamentals-Data
#'
#' @return list com dados financeiros
#' @export
obter_fundamentos_financeiros_quandl = function(company_code = "DDD") {
  
  # Base de Dados: - 
  # https://www.quandl.com/data/SF0-Free-US-Fundamentals-Data
  # Com algumas alterações é possível usar outras bases do quandl.
  
  # Definindo Chave de Acesso - Esta chave pertence a Pedro Lima, não utilizar:
  Quandl.api_key("RsCuvs4_WjRPP_zzSzfv")
  
  prefixo_base = "SF0/"
  
  variable_codes = c("REVENUE_MRY",
                     "INVENTORY_MRY",
                     "ASSETS_MRY",
                     "CAPEX_MRY",
                     "NETINC_MRY",
                     "GP_MRY",
                     "COR_MRY",
                     "TANGIBLES_MRY",
                     "EBT_MRY", 
                     "FCF_MRY",
                     "INTANGIBLES_MRY",
                     "NCFI_MRY",
                     "NCFF_MRY",
                     'NCFO_MRY',
                     "RND_MRY", 
                     "EBITDA_MRY")
  
  
  variable_names = c("Revenue",
                     "Inventory",
                     "Assets",
                     "Capex",
                     "NetIncome",
                     "GrossProfit",
                     "CostOfRevenue",
                     "TangibleAssets",
                     "EBT", 
                     "FreeCashFlow",
                     "IntangibleAssets",
                     "NetCashFlowFromInvestment",
                     "NetCashFlowFromFinancing",
                     'NetCashFlowFromOperations',
                     "ResearchAndDevelopmentExpenses", 
                     "EBITDA")
  
  
  variable_descriptions = c(
    "[Revenues]: Amount of Revenue recognized from goods sold, services rendered, insurance premiums, or other activities that constitute an earning process. Interest income for financial institutions is reported net of interest expense and provision for credit losses."
    ,"[Inventory]: A component of [ASSETS] representing the amount after valuation and reserves of inventory expected to be sold, or consumed within one year or operating cycle, if longer."
    ,"[Total Assets]: Sum of the carrying amounts as of the balance sheet date of all assets that are recognized. Major components are [CASHNEQ], [INVESTMENTS],[INTANGIBLES], [PPNENET],[TAXASSETS] and [RECEIVABLES]."
    ,"[Capital Expenditure]: A component of [NCFI] representing the net cash inflow (outflow) associated with the acquisition & disposal of long-lived, physical & intangible assets that are used in the normal conduct of business to produce goods and services and are not intended for resale. Includes cash inflows/outflows to pay for construction of self-constructed assets & software."
    ,"[Net Income]: The portion of profit or loss for the period, net of income taxes, which is attributable to the parent after the deduction of [NETINCNCI] from [CONSOLINC], and before the deduction of [PREFDIVIS]."
    ,"[Gross Profit]: Aggregate revenue [REVENUE] less cost of revenue [COR] directly attributable to the revenue generation activity."
    ,"[Cost of Revenue]: The aggregate cost of goods produced and sold and services rendered during the reporting period."
    ,"[Tangible Asset Value]: The value of tangibles assets calculated as the difference between [ASSETS] and [INTANGIBLES]."
    ,"[Earnings before Tax]: Earnings Before Tax is calculated by adding [TAXEXP] back to [NETINC]."
    ,"[Free Cash Flow]: Free Cash Flow is a measure of financial performance calculated as [NCFO] minus [CAPEX]."
    ,"[Goodwill and Intangible Assets]: A component of [ASSETS] representing the carrying amounts of all intangible assets and goodwill as of the balance sheet date, net of accumulated amortization and impairment charges."
    ,"[Net Cash Flow from Investing]: A component of [NCF] representing the amount of cash inflow (outflow) from investing activities, from continuing and discontinued operations. Principal components of investing cash flow are: capital (expenditure) disposal of equipment [CAPEX], business (acquisitions) disposition [NCFBUS] and investment (acquisition) disposal [NCFINV]."
    ,"[Net Cash Flow from Financing]: A component of [NCF] representing the amount of cash inflow (outflow) from financing activities, from continuing and discontinued operations. Principal components of financing cash flow are: issuance (purchase) of equity shares, issuance (repayment) of debt securities, and payment of dividends & other cash distributions."
    ,"[Net Cash Flow from Operations]: A component of [NCF] representing the amount of cash inflow (outflow) from operating activities, from continuing and discontinued operations."
    ,"[Research and Development Expense]: A component of [OPEX] representing the aggregate costs incurred in a planned search or critical investigation aimed at discovery of new knowledge with the hope that such knowledge will be useful in developing a new product or service."
    ,"[Earnings Before Interest, Taxes & Depreciation Amortization (EBITDA)]: EBITDA is a non-GAAP accounting metric that is widely used when assessing the performance of companies, calculated by adding [DEPAMOR] back to [EBIT]."
  )
  
  
  df_variaveis = data.frame(
    VariableCodes = variable_codes,
    VariableNames = variable_names,
    VariableDescriptions = variable_descriptions
  )
  
  sep = "_"
  
  queries = paste(prefixo_base, company_code,sep, variable_codes, sep = "")
  
  list_company = list()
  for (q in queries){
    message(paste("Queriying Quandl for variable", q))
    qnumber = which(queries == q)
    list_company[[variable_names[qnumber]]] = Quandl(q, collapse="annual", start_date="1900-01-01", type="ts") 
  }
  
  df_company = data.frame(time = as.vector(time(list_company[[1]])),
                          as.data.frame(list_company))
  
  ## Salvar Dados Coletados
  write.csv2(df_company, file = paste("./fundamentals-data/financial_data", company_code, ".csv", sep = ""))
  
  message(paste("Finalizada coleta de dados da empresa", company_code))
  
  ## Gerar List com descrição das variáveis e resultados
  list(Variaveis = df_variaveis, Dados = df_company)
}