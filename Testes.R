library(tidyverse)
library(readxl)
library(xts)
library(PerformanceAnalytics)
library(lmtest)
library(AER)
library(ggthemes)
library(lubridate)
library(stargazer)
library(kableExtra)
library(furrr)
library(progressr)
library(parallel)

plan(multisession, workers = parallel::detectCores())

source("Funcoes.R")

"%ni%" <- Negate("%in%")

# Construção dos portfólios e ordenamento dos ativos ----

paises <- c("Franca", "Alemanha", "Canada", "UK", "Dinamarca", "Finlandia", "Espanha", "Suecia", "Suica", "Coreia", "Japao", "Taiwan")

retornos_geral <- list()
lista_ativos_AR1 <- list()
lista_ativos_mom <- list()
for (pais in paises) {
  path_dado <- paste0("Dados/", pais)
  
  # Importa a composicao do indice
  dbComp_ip <- read_csv(paste(path_dado, "comp_maior100k.csv", sep = "\\"))
  dbComp <- data.frame(dbComp_ip$Ativos, apply(dbComp_ip[,-1], 2, as.numeric))
  colnames(dbComp) <- colnames(dbComp_ip)
  rm(dbComp_ip)
  colnames(dbComp) <- append("Ativos", as.character(as.Date(colnames(dbComp)[2]) %m+% months(0:(ncol(dbComp)-2))))
  
  # Importa o retorno diário dos ativos que compoe o indice
  dbRet_ativos_ip <- read_csv(paste(path_dado, "ativos.csv", sep = "\\"))
  dbRet_ativos <- data.frame(as.Date(dbRet_ativos_ip$Data), apply(dbRet_ativos_ip[,-1], 2, as.numeric))
  colnames(dbRet_ativos) <- colnames(dbRet_ativos_ip)
  rm(dbRet_ativos_ip)
  
  # inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
  lista_ativos <- params_estim(20021231, 20211031, c(1), c(12), dbRet_ativos, "Momentum-1", "asc", 12)
  
  lista_ativos_mom[[pais]] <- params_estim(20021231, 20211031, c(1), c(1), dbRet_ativos, "Momentum", "asc", 12)
  
  lista_ativos_AR1[[pais]] <- params_estim(20021231, 20211031, c(1), c(1), dbRet_ativos, "AR", "asc", 12)
  
  ret_port_geral <- list()
  for(j in names(lista_ativos)){
    for(i in names(lista_ativos[[j]])){
      nome <- paste(j, i, sep = "_")
      ret_port_geral[[nome]] <- backtest(lista_ativos[[j]][[i]], 20021231, 20211031, 0.3, "EW") 
    }
  }
  
  df_ports <- do.call("cbind.xts", ret_port_geral)
  
  nomes_df_ports <- vector()
  cont <- 1
  for(i in names(ret_port_geral)){
    for(j in c("long", "ls")){
      nomes_df_ports[cont] <- paste(j, i, sep = "_")
      cont <- cont + 1
    }
  }
  
  colnames(df_ports) <- paste(pais, nomes_df_ports, sep = "_")
  
  retornos_geral[[pais]] <- df_ports
}  

vuong_test <- vector("list", length = length(paises))
granger_mom <- vector("list", length = length(paises))
granger_ar1 <- vector("list", length = length(paises))
granger_ar1_rev <- vector("list", length = length(paises))
teste_F <- vector("list", length = length(paises))
teste_F_reverso <- vector("list", length = length(paises))
grafico_series <- vector("list", length = length(paises))
for (j in seq_along(retornos_geral)) {
  port_pais <- retornos_geral[[j]]
  
  inicio_data <- 20021231
  
  ret_acum_port <- c()
  n_meses <- interval(ymd(inicio_data), ymd(20211031)) %/% months(1)
  for (i in 1:n_meses) {
    inicio_estim <- ymd(inicio_data) %m+% months(i-1)
    
    fim_estim <- ymd(inicio_data) %m+% months(i)
    
    port_pais_loop <- port_pais[paste(inicio_estim, fim_estim, sep = "/")]

    ret_acumul <- prod(1 + port_pais_loop[,2]) - 1
    
    ret_acum_port <- c(ret_acum_port, ret_acumul)
  }
  
  ativos_mom <- lista_ativos_mom[[j]][[1]][[1]]
  ativos_ar1 <- lista_ativos_AR1[[j]][[1]][[1]]
  
  # Como podemos perceber, "ativos_*" e "ret_acum_port" possuem o mesmo tamanho
  # Entretanto, queremos que o fim dos períodos de estimação de cada um seja o mesmo
  # Como o primeiro elemento de "ativos_*" não está representado em "ret_acum_port", eliminamos essa data
  # Como o último elemento de "ret_acum_port" não possui data equivalente em "ativos_*", eliminamos essa data
  
  ativos_mom <- ativos_mom[-1]
  ativos_ar1 <- ativos_ar1[-1]

  ret_acum_port <- ret_acum_port[-length(ret_acum_port)]
  
  diff_media <- function(df){
    n_elemen <- 0.3 * nrow(df)
    buy <- head(df, n_elemen)
    sell <- tail(df, n_elemen)
    
    return(mean(buy$indic) - mean(sell$indic))
  }
  
  diff_avg_mom <- lapply(ativos_mom, diff_media)
  diff_avg_ar1 <- lapply(ativos_ar1, diff_media)
  
  diff_avg_mom <- do.call("c", diff_avg_mom)
  diff_avg_ar1 <- do.call("c", diff_avg_ar1)
  
  # Primeiro realizamos o teste de granger para cada estatística
  granger_mom[[j]] <- grangertest(ret_acum_port ~ diff_avg_mom, order = 3)
  
  granger_ar1[[j]] <- grangertest(ret_acum_port ~ diff_avg_ar1, order = 3)
  granger_ar1_rev[[j]] <- grangertest(diff_avg_ar1 ~ ret_acum_port, order = 3)
  
  # Em seguida, extraímos a estatística F e o R^2 ajustado do modelo AR1
  
  # Em seguida, realizamos o teste de Vuong para comparar os dois modelos
  modelo_mom <- lm(ret_acum_port ~ lag(diff_avg_mom, 1) + lag(diff_avg_mom, 2) + lag(diff_avg_mom, 3) + lag(ret_acum_port, 1) + lag(ret_acum_port, 2) + lag(ret_acum_port, 3))
  modelo_ar1 <- lm(ret_acum_port ~ lag(diff_avg_ar1, 1) + lag(diff_avg_ar1, 2) + lag(diff_avg_ar1, 3) + lag(ret_acum_port, 1) + lag(ret_acum_port, 2) + lag(ret_acum_port, 3))
  
  vuong_test[[j]] <- nonnest2::vuongtest(modelo_mom, modelo_ar1)
 
  # Além disso, realizamos um teste Anova para testar se um modelo contendo as duas variáveis é superior a um modelo contendo apenas uma
  modelo_restrito <- lm(ret_acum_port ~ lag(diff_avg_mom, 1) + lag(diff_avg_mom, 2) + lag(diff_avg_mom, 3) + lag(ret_acum_port, 1) + lag(ret_acum_port, 2) + lag(ret_acum_port, 3))
  modelo_irrestrito <- lm(ret_acum_port ~ lag(diff_avg_mom, 1) + lag(diff_avg_mom, 2) + lag(diff_avg_mom, 3) + lag(diff_avg_ar1, 1) + lag(diff_avg_ar1, 2) + lag(diff_avg_ar1, 3) + lag(ret_acum_port, 1) + lag(ret_acum_port, 2) + lag(ret_acum_port, 3))
  
  teste_F[[j]] <- anova(modelo_restrito, modelo_irrestrito)
  
  modelo_restrito <- lm(ret_acum_port ~ lag(diff_avg_ar1, 1) + lag(diff_avg_ar1, 2) + lag(diff_avg_ar1, 3) + lag(ret_acum_port, 1) + lag(ret_acum_port, 2) + lag(ret_acum_port, 3))
  modelo_irrestrito <- lm(ret_acum_port ~ lag(diff_avg_mom, 1) + lag(diff_avg_mom, 2) + lag(diff_avg_mom, 3) + lag(diff_avg_ar1, 1) + lag(diff_avg_ar1, 2) + lag(diff_avg_ar1, 3) + lag(ret_acum_port, 1) + lag(ret_acum_port, 2) + lag(ret_acum_port, 3))
  
  teste_F_reverso[[j]] <- anova(modelo_restrito, modelo_irrestrito)
  
  # Por fim, criamos um gráfico com as duas variáveis
  df <- data.frame(Data = ymd(20030131) %m+% months(0:(length(ret_acum_port)-1)), 
                   Diff = (diff_avg_ar1 - mean(diff_avg_ar1)) / sd(diff_avg_ar1),
                   Portfolio = (ret_acum_port - mean(ret_acum_port)) / sd(ret_acum_port))
  
  grafico_series[[j]] <- ggplot(data = df) + geom_line(aes(x = Data, y = Diff, color = "Diferença de Médias")) + geom_line(aes(x = Data, y = Portfolio, color = "Retorno do Portfólio")) +
    scale_color_manual(values=c('darkblue','black')) + ylab("") + xlab("") + ggthemes::theme_base() + theme(legend.position = "none")
  
}

# Tabelas e Figuras Artigo ----

# Com todos os dados já prontos para integrarem o artigo, precisamos apenas de fazer alguns ajustes para criar as tabelas

paises_tab <- c("França", "Alemanha", "Canadá", "Reino Unido", "Dinamarca", "Finlândia", "Espanha", "Suécia", "Suíça", "Coréia", "Japão", "Taiwan")

simb_sig <- function(vetor_pvalores){
  vetor_simb_sig <- vector(length = length(paises))
  for (i in seq_along(vetor_pvalores)) {
    if(as.numeric(vetor_pvalores[i]) < 0.01){
      vetor_simb_sig[i] <- "***"
    } else if(as.numeric(vetor_pvalores[i]) < 0.05){
      vetor_simb_sig[i] <- "**"
    } else if(as.numeric(vetor_pvalores[i]) < 0.1){
      vetor_simb_sig[i] <- "*"
    } else{
      vetor_simb_sig[i] <- ""
    }
  }
  return(vetor_simb_sig)
}

estat_F_simb <- function(lista){
  aux_list <- lapply(lista, function(x) as.data.frame(c(x$F, x$`Pr(>F)`[2])))
  aux_df <- as.data.frame(t(do.call("cbind", aux_list)), row.names = paises)[,-1] %>% set_names("F", "pvalor")
  vector_simb <- paste0(round(aux_df$`F`,2), simb_sig(aux_df$pvalor))
  return(vector_simb)
}

granger_graus_lib <- paste0("Modelo Restrito: ", granger_mom[[1]]$Res.Df[1], "; Modelo Irrestrito: ", granger_mom[[1]]$Res.Df[2])

granger_mom <- estat_F_simb(granger_mom)
granger_ar1 <- estat_F_simb(granger_ar1)
granger_ar1_rev <- estat_F_simb(granger_ar1_rev)

tabela_granger <- data.frame(granger_ar1, granger_ar1_rev, granger_mom, row.names = paises_tab) %>% set_names("AR(1)", "AR(1) Reverso", "Retorno Acumulado")
tabela_granger <- tabela_granger[order(rownames(tabela_granger)), ]

# Tabelas Teste Vuong

vuong_var <- lapply(vuong_test, function(x) as.data.frame(c(x$LRTstat, as.numeric(x$p_LRT[2]))))
vuong_var <- as.data.frame(t(do.call("cbind", vuong_var)), row.names = paises) %>% set_names("LRT", "pvalor")

vuong_var_simb <- simb_sig(vuong_var$pvalor)

tabela_vuong <- data.frame(Omega = paste0(round(vuong_var$LRT,2), vuong_var_simb), row.names = paises_tab) %>% set_names("LRT")
tabela_vuong <- tabela_vuong[order(rownames(tabela_vuong)), , drop = FALSE]

# Tabela Teste F

tabela_F <- estat_F_simb(teste_F)
tabela_F <- data.frame(`F` = tabela_F, row.names = paises_tab)
tabela_F <- tabela_F[order(rownames(tabela_F)), , drop = FALSE]

tabela_F_rev <- estat_F_simb(teste_F_reverso)
tabela_F_rev <- data.frame(`F` = tabela_F_rev, row.names = paises_tab)
tabela_F_rev <- tabela_F_rev[order(rownames(tabela_F_rev)), , drop = FALSE]

tabela_F <- cbind(tabela_F, tabela_F_rev) %>% set_names("AR(1)", "Retorno Acumulado")

# Gráfico Séries
# Nesse caso específico ainda precisa ser analisado como colocar os 12 gráficos juntos

for (i in seq_along(grafico_series)) {
  grafico_series[[i]] <- grafico_series[[i]] + ggtitle(paises_tab[i])
}
names(grafico_series) <- paises_tab

grafico_series <- grafico_series[order(names(grafico_series))]


