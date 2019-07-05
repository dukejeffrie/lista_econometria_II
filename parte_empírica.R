# Lista de pacotes a utilizados
# Criar um objeto com os pacotes
pacotes <- c("tidyverse","data.table","tabulizer","knitr","ggplot2",
             "pracma","nnet")

# Instalar pacotes, caso não estejam installados
if (length(setdiff(pacotes, rownames(installed.packages()))) > 0) {
  
  install.packages(setdiff(pacotes, rownames(installed.packages())))
  
} else {"Todos os pacotes estão instalados"}

# Carregar pacotes necessários
library(tidyverse)
library(data.table)
library(tabulizer)
library(knitr)
library(ggplot2)
library(pracma)
library(nnet)




#### Ler o dicionário de variáveis
#  Extrair tabela do PDF com a descrição das variáveis
dic <- tabulizer::extract_tables("lista2019.pdf",
                                 output = "data.frame")[[1]]

# Listar arquivos com as bases de dados
bases<- data.frame(arquivo=list.files("rust_data"),
                   nome=gsub(".asc","", list.files("rust_data")),
                   linhas=c(rep(137,4),128,36,60,81),
                   stringsAsFactors = F)

# Looping para carregar as bases
for(b in bases$arquivo){
  
  # Ler a base de dados
  base <- data.table::fread(paste0("rust_data/",b))
  
  # Definir o número de linhas da matriz
  nl<-bases[bases$arquivo==b,]$linhas
  
  # Definir número de meses existentes na base
  nm<-nl-12+1
  
  # Criar objeto para receber os dados fixos de cada ônibus
  dados <- c()
  
  # Criar um objeto para receber as informações mensais
  t<-c()
  
  # Criar um objeto para receber as referências
  ref<-c()
  
  # Iniciar looping para carregar as informações fixas,
  # repetindo para o número de meses
  for(i in 1:11){
    
    # Capturar as informações de cada ônibus
    assign(paste0("V",i), unlist(rep(base[seq(i,nrow(base),nl),],nm)))
    
    # Juntar resultados
    dados <- cbind(dados,get(paste0("V",i)))
    
  }
  
  # Retirar as informações mensais (odômetros)
  
  for(j in 12:nl){
    
    
    # Retirar para cada ônibus
    V12 <- base[seq(j,nrow(base),nl)]
    
    # Juntar resultados
    t <- rbind(t,V12)
  }
  
  # Criar um objeto com a referência,
  # tendo como base o mês e ano inicial do odômetro
  
  
  for(i in seq(10,nrow(base),nl)){
    
    # Capturar o mês inicial, o ano inicial, definir sempre o primeiro
    # dia de cada mês para transformar em data e criar a sequência de 
    # meses conforme o número de meses(nm) disponíveis na base
    
    r <- data.frame(ref=seq(
      # Define a data
      lubridate::dmy(paste("01",base[i],
                           base[i+1],sep = "/")),
      # Sequência mensal
      by="month",
      # Pelo número de meses
      length.out = nm))
    
    # Juntar resultados
    ref<- rbind(ref,r)
    
  }
  
  # Juntar a base final
  assign(bases[bases$arquivo==b,]$nome,
         cbind(dados,V12=t) %>%
           # Organizar por ônibus
           dplyr::arrange(V1) %>%
           # Trazer referência
           dplyr::bind_cols(ref) %>%
           # Ajustar odômetros para ocasião da troca
           dplyr::mutate(V12_adj=case_when(V12.V1>V6&V6>0~V12.V1-V6,
                                           TRUE~V12.V1), # Primeira troca
                         V12_adj=case_when(V12.V1>V9&V9>0~V12.V1-V9,
                                           TRUE~V12_adj)) %>% # Segunda troca
           # Agrupar por ônibus
           dplyr::group_by(V1) %>%
           # Criar indicador de troca
           dplyr::mutate(troca=ifelse(V12_adj>lead(V12_adj),1,0), 
                         troca=ifelse(is.na(troca)==T,0,troca),
                         # Crirar variável de estado com
                         # intervalos de 5000
                         estado=cut(V12_adj,
                                    breaks = c(seq(0,max(V12_adj),
                                                   by=5000),Inf),
                                    labels = FALSE),
                         # Calcular a milhagem mensal ajustada
                         difV12=V12_adj-lag(V12_adj),
                         # Ajustar os casos de troca
                         difV12=ifelse(difV12<0,V12_adj,difV12),
                         # Definir a mudança de estado nos intervalos
                         # [0,5000),[5000,10000),[10000,Inf)
                         diffV12_mult=case_when(estado==lag(estado)~1,
                                                estado-lag(estado)==1~2,
                                                estado-lag(estado)>1~3),
                         # Ajustar valor inicial
                         diffV12_mult=ifelse(is.na(diffV12_mult)==T,1,
                                             diffV12_mult)) %>%
           # Dropar primeira observação (não há lag)
           na.omit)  
  
  # Remover objetos desnecessários
  rm(base,dados,r,ref,t,list=ls(pattern = "V\\d"),b,i,j,nl,nm)
}




# Definição da função custo e a função escolha (Passo 2)
source("cmiope.R",encoding ="UTF-8")

# Algoritmo do mapeamento de contração 
# (gera a escolha de probabilidade "forward-looking")
source("cont_map.R",encoding ="UTF-8")

# 1) Modelo Logit de utilidade dinâmica
source("dinam_logit.R",encoding ="UTF-8")


# Definir base: Grupo 1,2,3
d1 = rbind(g870[,c("V1","troca","V12_adj","estado",
                   "difV12","diffV12_mult")],
           rt50[,c("V1","troca","V12_adj","estado",
                   "difV12","diffV12_mult")],
           t8h203[,c("V1","troca","V12_adj","estado",
                     "difV12","diffV12_mult")])

# Grupo 4
d2= rbind(a530875[,c("V1","troca","V12_adj","estado",
                     "difV12","diffV12_mult")])

# Grupo 1,2,3 e 4
d3= rbind(g870[,c("V1","troca","V12_adj","estado",
                  "difV12","diffV12_mult")],
          rt50[,c("V1","troca","V12_adj","estado",
                  "difV12","diffV12_mult")],
          t8h203[,c("V1","troca","V12_adj","estado",
                    "difV12","diffV12_mult")],
          a530875[,c("V1","troca","V12_adj","estado",
                     "difV12","diffV12_mult")])

# Objeto para receber os resultados

resultados <- c()

erro <- c()

# Calcular para cada base
for(i in c("d1","d2","d3")){
  
  # Defifnir a base do loop
  data <- get(i)  
  
  # Definir o label do loop
  if(i=="d1"){
    label="Grupo 1, 2 e 3"
  } else{if(i=="d2"){
    label="Grupo 4"} else{
      
      label="Grupo 1, 2, 3 e 4"
    }
  }
  ####################
  ###### Passo 1 #####
  ####################
  
  # Verificar a probabilidade empírica da mudança em cada estado
  
  p <- data %>%
    # Analisar as mudanças de estado
    dplyr::mutate(p=case_when(estado==lag(estado)~"p_x0",
                              estado-lag(estado)==1~"p_x1",
                              estado-lag(estado)>1~"p_x2"),
                  p=ifelse(is.na(p)==T,"p_x0",p)) %>%
    # Agrupar pelas quebras
    dplyr::group_by(p) %>%
    # Contar a quantidade de observações nas quebras
    dplyr::summarise(n=n()) %>%
    # Desagrupar
    dplyr::ungroup() %>%
    # Calcular a proporção
    dplyr::mutate(n=n/sum(n))
  # Armazenar proporção em um objeto
  p <- p$n
  
  # p_x0 é a chance de manutenção na mesma faixa de milhagem,
  # p_x1 é a chance de passa para o status 2,
  # enquanto p_x1 chance de estar no último status 
  
  # Decisão do agente
  
  # Variável iniciais
  # O custo de troca, os parâmetros de manutenção da função custo, 
  # e a taxa de desconto são definidas aqui.
  # A taxa de desconto de beta era pra ser = 0.9999
  # Por questões de convergência, consegui somente com 0.91
  # Os tetas iniciais foram definidos de acordo com os resultados
  # da tabela IX do paper
  
  if(i=="d1"){
    rc=20
    theta1_1=5
    beta=0.91
  } else{
    
    if(i=="d2"){
      rc=20
      theta1_1=2
      beta=0.91} else{
        rc=20
        theta1_1=2
        beta=0.91
      }
  }
  
  # Definir parâmetros iniciais
  params_lin = c(rc,theta1_1)
  p = p
  S = 90
  
  ####################
  ###### Passo 2 #####
  ####################
  
  # Rodar modelo
  out = contraction_mapping(S=S, 
                            p=p, 
                            FCM=lin_cost, 
                            params=params_lin, 
                            beta = beta)
  
  # Guardar resultados em um objeto
  lin_forward=out$CP_forward
  lin_miope=out$CP_miope
  pescolha = lin_forward[,1]
  
  # Definir base com resultados
  ggdat1 = data.frame(Regra=c(rep("Forward-Looking",nrow(lin_forward)),
                              rep("Míope",nrow(lin_forward))),
                      pMaint=c(lin_forward[,1],lin_miope[,1]),
                      State=rep(1:S,times=2))
  
  # Fazer o gráfico
  assign(paste0("g_",i),
         ggplot(ggdat1,aes(y=pMaint,x=State,color=Regra))+
           geom_line(lwd=1)+
           theme_bw(12)+
           scale_x_continuous(breaks = seq(0,90,by=10))+
           scale_y_continuous(labels = scales::percent)+
           labs(x="Estado",
                y="Prob.",
                caption = label))
  
  
  # limites
  bounds = c(1e-9, Inf)
  
  ####################
  ###### Passo 3 #####
  ####################
  
  lin_fit = optim(par=params_lin,fn=dynamiclogit,method=c("L-BFGS-B"),
                  lower=bounds[1],upper=bounds[2],
                  data=data,S=S,p=p,FCM=lin_cost,
                  control=list(fnscale=1),hessian = T)
  # Retornar os parâmetros obtidos com a função
  loglike =  lin_fit$value
  fit_params = lin_fit$par
  
  # Computar erro-padrão
  EP <-sqrt(diag(solve(lin_fit$hessian)))
  
  # Armazenar resultados no objeto
  er <- data.frame(Grupo=label,
                   EP_RC=EP[1],
                   EP_T1=EP[2])
  
  # Agregar resultados
  erro <- rbind(erro,er)
  
  # Armazenar resultados no objeto
  r <- data.frame(Grupo=label,
                  LL=loglike*-1,
                  RC=fit_params[1],
                  T1=fit_params[2],
                  T30=p[1],
                  T31=p[2])
  
  # Agregar resultados
  resultados <- rbind(resultados,r)
}


resultados %>%
  xtable::xtable(digits = c(0,rep(3,ncol(resultados))),
                 caption="Estimativas para a questão n)",
                 label="tab:2") %>%
  print(include.rownames=F,
        comment=FALSE)
# Gráfico grupo 1, 2 e 3
g_d1

# Gráfico grupo 4
g_d2

# Gráfico grupo 1, 2, 3 e 4
g_d3

erro %>%
  xtable::xtable(digits = c(0,rep(3,ncol(erro))),
                 caption="Estimativas do erro-padrão de RC e T1",
                 label="tab:3") %>%
  print(include.rownames=F,
        comment=FALSE)



# Preparar a base com estados em intervalos de 2.500 milhas

# Looping para carregar as bases
for(b in bases$arquivo){
  
  # Ler a base de dados
  base <- data.table::fread(paste0("rust_data/",b))
  
  # Definir o número de linhas da matriz
  nl<-bases[bases$arquivo==b,]$linhas
  
  # Definir número de meses existentes na base
  nm<-nl-12+1
  
  # Criar objeto para receber os dados fixos de cada ônibus
  dados <- c()
  
  # Criar um objeto para receber as informações mensais
  t<-c()
  
  # Criar um objeto para receber as referências
  ref<-c()
  
  # Iniciar looping para carregar as informações fixas,
  # repetindo para o número de meses
  for(i in 1:11){
    
    # Capturar as informações de cada ônibus
    assign(paste0("V",i), unlist(rep(base[seq(i,nrow(base),nl),],nm)))
    
    # Juntar resultados
    dados <- cbind(dados,get(paste0("V",i)))
    
  }
  
  # Retirar as informações mensais (odômetros)
  
  for(j in 12:nl){
    
    
    # Retirar para cada ônibus
    V12 <- base[seq(j,nrow(base),nl)]
    
    # Juntar resultados
    t <- rbind(t,V12)
  }
  
  # Criar um objeto com a referência,
  # tendo como base o mês e ano inicial do odômetro
  
  
  for(i in seq(10,nrow(base),nl)){
    
    # Capturar o mês inicial, o ano inicial, definir sempre o primeiro
    # dia de cada mês para transformar em data e criar a sequência de 
    # meses conforme o número de meses(nm) disponíveis na base
    
    r <- data.frame(ref=seq(
      # Define a data
      lubridate::dmy(paste("01",base[i],
                           base[i+1],sep = "/")),
      # Sequência mensal
      by="month",
      # Pelo número de meses
      length.out = nm))
    
    # Juntar resultados
    ref<- rbind(ref,r)
    
  }
  
  # Juntar a base final
  assign(bases[bases$arquivo==b,]$nome,
         cbind(dados,V12=t) %>%
           # Organizar por ônibus
           dplyr::arrange(V1) %>%
           # Trazer referência
           dplyr::bind_cols(ref) %>%
           # Ajustar odômetros para ocasião da troca
           dplyr::mutate(V12_adj=case_when(V12.V1>V6&V6>0~V12.V1-V6,
                                           TRUE~V12.V1), # Primeira troca
                         V12_adj=case_when(V12.V1>V9&V9>0~V12.V1-V9,
                                           TRUE~V12_adj)) %>% # Segunda troca
           # Agrupar por ônibus
           dplyr::group_by(V1) %>%
           # Criar indicador de troca
           dplyr::mutate(troca=ifelse(V12_adj>lead(V12_adj),1,0), 
                         troca=ifelse(is.na(troca)==T,0,troca),
                         estado=cut(V12_adj,
                                    breaks = c(seq(0,max(V12_adj), by=2500),Inf),
                                    labels = FALSE),
                         difV12=V12_adj-lag(V12_adj),
                         difV12=ifelse(difV12<0,V12_adj,difV12),
                         diffV12_mult=case_when(estado==lag(estado)~1,
                                                estado-lag(estado)==1~2,
                                                estado-lag(estado)==2~3,
                                                estado-lag(estado)==3~4,
                                                estado-lag(estado)==4~5,
                                                estado-lag(estado)>4~6,),
                         diffV12_mult=ifelse(is.na(diffV12_mult)==T,
                                             1,
                                             diffV12_mult)) %>%
           na.omit)  # Ajustar primeiro valor
  
  
  # Remover objetos desnecessários
  rm(base,dados,r,ref,t,list=ls(pattern = "V\\d"),b,i,j,nl,nm)
}

# Salvar as novas bases

# Grupo 1,2,3
d4 = rbind(g870[,c("V1","troca","V12_adj","estado",
                   "difV12","diffV12_mult")],
           rt50[,c("V1","troca","V12_adj","estado",
                   "difV12","diffV12_mult")],
           t8h203[,c("V1","troca","V12_adj","estado",
                     "difV12","diffV12_mult")])

#Grupo 4
d5= rbind(a530875[,c("V1","troca","V12_adj","estado",
                     "difV12","diffV12_mult")])

# Grupo 1,2,3 e 4
d6= rbind(g870[,c("V1","troca","V12_adj","estado",
                  "difV12","diffV12_mult")],
          rt50[,c("V1","troca","V12_adj","estado",
                  "difV12","diffV12_mult")],
          t8h203[,c("V1","troca","V12_adj","estado",
                    "difV12","diffV12_mult")],
          a530875[,c("V1","troca","V12_adj","estado",
                     "difV12","diffV12_mult")])


# Objeto para receber os resultados

resultados <- c()

# Calcular para cada base
for(i in c("d4","d5","d6")){
  
  data <- get(i)  
  
  if(i=="d4"){
    label="Grupo 1, 2 e 3"
  } else{if(i=="d5"){
    label="Grupo 4"} else{
      
      label="Grupo 1, 2, 3 e 4"
    }
  }
  
  
  # Verificar a probabilidade empírica da mudança em cada estado
  p <- data %>%
    # Analisar as mudanças de estado
    # Agrupar pelas quebras
    dplyr::group_by(diffV12_mult) %>%
    # Contar a quantidade de observações nas quebras
    dplyr::summarise(n=n()) %>%
    # Desagrupar
    dplyr::ungroup() %>%
    # Calcular a proporção
    dplyr::mutate(n=n/sum(n))
  
  # Armazenar proporção em um objeto
  p <- p$n
  
  # Decisão do agente
  
  # Variável iniciais
  # O custo de troca, os parâmetros de manutenção da função custo, 
  # e a taxa de desconto são definidas aqui.
  # A taxa de desconto de beta era pra ser = 0.9999
  # Por questões de convergência, consegui somente com 0.95
  
  if(i=="d4"){
    rc=11
    theta1_1=5
    theta1_2=0.01
    beta=0.91
  } else{
    
    if(i=="d5"){
      rc=10
      theta1_1=2
      theta1_2=0.01
      beta=0.91} else{
        
        rc=10
        theta1_1=3
        theta1_2=0.01
        beta=0.91
      }
  }
  
  # Definir parâmetros iniciais
  params_log = c(rc,theta1_1,theta1_2)
  p = p
  S = 180
  
  # Rodar modelo
  out = contraction_mapping(S=S, 
                            p=p, 
                            # Mudando a função de custo
                            FCM=log_cost, 
                            params=params_log, 
                            beta = beta)
  
  lin_forward=out$CP_forward
  lin_miope=out$CP_miope
  pescolha = lin_forward[,1]
  
  ggdat1 = data.frame(Regra=c(rep("Forward-Looking",nrow(lin_forward)),
                              rep("Míope",nrow(lin_forward))),
                      pMaint=c(lin_forward[,1],lin_miope[,1]),
                      State=rep(1:S,times=2))
  
  # Fazer o gráfico
  assign(paste0("g_",i),
         ggplot(ggdat1,aes(y=pMaint,x=State,color=Regra))+
           geom_line(lwd=1)+
           theme_bw(12)+
           scale_x_continuous(breaks = seq(0,90,by=10))+
           scale_y_continuous(labels = scales::percent)+
           labs(x="Estado",
                y="Prob.",
                caption = label))
  
  # Estimação aplicação à função custo
  
  # limites
  bounds = c(1e-9, Inf)
  
  
  # Aplicação ao custo linear
  log_fit = optim(par=params_log,fn=dynamiclogit,method=c("L-BFGS-B"),
                  lower=bounds[1],upper=bounds[2],
                  data=data,S=S,p=p,FCM=log_cost,control=list(fnscale=1))
  
  # Retornar os parâmetros obtidos com a função
  loglike =  log_fit$value
  fit_params = log_fit$par
  
  # Computar erro-padrão
  EP <-sqrt(diag(solve(lin_fit$hessian)))
  
  # Armazenar resultados no objeto
  er <- data.frame(Grupo=label,
                   EP_RC=EP[1],
                   EP_T1=EP[2])
  
  # Agregar resultados
  erro <- rbind(erro,er)
  
  # Armazenar resultados no objeto
  r <- data.frame(Grupo=label,
                  LL=loglike*-1,
                  RC=fit_params[1],
                  T11=fit_params[2],
                  T12=fit_params[3],
                  T30=p[1],
                  T31=p[2],
                  T32=p[3],
                  T34=p[4],
                  T35=p[5])
  
  # Agregar resultados
  resultados <- rbind(resultados,r)
  
}




resultados %>%
  xtable::xtable(digits = c(0,rep(3,ncol(resultados))),
                 caption="Estimativas para o modelo modificado: 
                 Grid: 2.500, S: 180 e função custo logarítmica",
                 label="tab:4") %>%
  print(include.rownames=F,
        comment=FALSE)



# Objeto para receber os dados
resultados <- c()

# Iniciar o loop de estimativa para cada conjunto de dados
for(i in c("d1","d2","d3")){
  
  data <- get(i) 
  
  if(i=="d1"){
    label="Grupo 1, 2 e 3"
  } else{if(i=="d2"){
    label="Grupo 4"} else{
      label="Grupo 1, 2, 3 e 4"
    }
  }
  
  # Rodar o logit multinomial
  l_m <- nnet::multinom(lead(diffV12_mult)~diffV12_mult-1,
                        family = "binomial",data = data)
  
  # Armazenar as estimativas
  est = l_m$fitted.values
  
  # Computar a probabilidade
  p <- c(mean(est[,1]),mean(est[,2]),mean(est[,3]))
  
  # Armazenar resultados
  r <- data.frame(Grupo=label,
                  p1=mean(est[,1]),
                  p2=mean(est[,2]),
                  p3=mean(est[,3]))
  
  # Agregar resultados
  resultados <- rbind(resultados,r)
  
}


# Tabela
resultados %>%
  xtable::xtable(digits = c(0,3,3,3,3),
                 caption="Estimativas do Logit multinomial",
                 label="tab:5") %>%
  print(include.rownames=F,
        comment=FALSE)



# Objeto para receber os dados
resultados <- c()

# Iniciar loop para cada conjunto de dados
for(i in c("d1","d2","d3")){
  
  data <- get(i) 
  
  if(i=="d1"){
    label="Grupo 1, 2 e 3"
  } else{if(i=="d2"){
    label="Grupo 4"} else{
      label="Grupo 1, 2, 3 e 4"
    }
  }
  
  # Rodar o logit 
  glmout=glm(troca~difV12-1,family='binomial',data=data)
  # Recuperar o coeficiente
  coef=glmout$coef
  # Computar estimaticas
  est = exp(-(cbind(data$difV12)%*%coef))
  # Calcular probabilidade
  prob0 = 1/(1+est)
  
  # Armazenar resultados
  r <- data.frame(Grupo=label,
                  p_troca=mean(prob0),
                  # Probabilidade complementar
                  p_mant=mean(1-prob0))
  
  # Agregar resultados
  resultados <- rbind(resultados,r)
  
}


# Fonte:
# https://github.com/waynejtaylor/Single-Agent-Dynamic-Choice/blob/
# master/Import%20Data%20and%20Estimate.R


#  Programa para estimar o modelo de Rust, Econometrica 1987,  
#  usando o algorítmo Nested Pseudo Likelihood (NPL) de
#  Aguirregabiria e Mira (Econometrica, 2002).

#  Funções auxiliares
source('npl_sing.R',encoding ="UTF-8")
source('clogit.R',encoding ="UTF-8")


# Definindo as constantes

# Nome dos parâmetros estruturais
namespar = c("ReplaceC","MaintenC")

# Número de variáveis de estado
kvarx = 1

# Número de alternativas
jchoice = 2

# Número de grids
ncelx = 90

# Objeto para receber os resultados
resultados <- c()

# Calcular os resultados para cada base

# Iniciar o loop
for(d in c("d1","d2","d3")){
  
  # Definir a base
  data <- get(d)  
  
  # Definir o label
  if(i=="d1"){
    label="Grupo 1, 2 e 3"
  } else{if(i=="d2"){
    label="Grupo 4"} else{
      
      label="Grupo 1, 2, 3 e 4"
    }
  }
  
  # Definir o beta
  
  if(d=="d1"){
    beta=0.9999
  } else{
    
    if(i=="d2"){
      beta=0.9999} else{
        
        beta=0.99
      }
  }
  
  # Objeto com a decisão de troca
  aobs = data$troca  
  
  # Ajuste da troca (para começar em 1)
  indobsa = aobs+1 
  
  # Custo de manutenção em cada estado
  xval = 1:ncelx        
  
  # Indicados do estado (deve começar em 1)
  indobsx = data$estado
  
  # 3. Função utilidade
  
  # Com manutenção (zmat1), 
  # custo da troca = 0 e custo de manutenção = -xval
  zmat1= cbind(0,-xval)           
  
  # Se troca (zmat2), custo da troca = 1 
  # e custo da manutenção = 0
  zmat2= cbind(-1,rep(0,ncelx))  
  # Juntar matrizes
  zmat=cbind(zmat1,zmat2)
  # Remover objetos temporários
  rm(zmat1,zmat2)
  
  # Outra alternativa
  # zmat1= cbind(1,-xval)    
  # zmat2= cbind(0,rep(0,ncelx))
  # zmat=cbind(zmat1,zmat2)
  # rm(zmat1,zmat2)
  
  # 4. Estimativa das probabilidades de transição
  
  # Definir a matriz
  fmat1 = matrix(0,ncelx,ncelx)
  # Definir as probabilidades
  # Verificar a probabilidade empírica da mudança em cada estado
  l_m <- nnet::multinom(lead(diffV12_mult)~diffV12_mult-1,
                        family = "binomial",data = data)
  
  
  est = l_m$fitted.values
  
  p <- c(mean(est[,1]),mean(est[,2]),mean(est[,3]))
  
  # Definir o tamanho do vetor
  lp = length(p)
  # Inicir o loop
  for(i in 1:ncelx){
    for(j in 1:lp){
      if((i+j-1)<ncelx)  fmat1[i,i+j-1] = p[j]
      if((i+j-1)==ncelx) fmat1[i,i+j-1] = sum(p[j:lp]) 
    }
  }
  fmat2 = cbind(1,matrix(0,ncelx,ncelx-1))
  fmat=cbind(fmat1,fmat2)
  
  
  #***********************************
  # 5. Probabilidades iniciais
  #***********************************
  
  # Fazer com base numa polinomial de indobsx  
  xprob0=cbind(indobsx,indobsx^2,indobsx^3)
  # Rodar logit
  glmout=glm(aobs~xprob0-1,family='binomial')
  # Recuperar coeficientes
  coef=glmout$coef
  # Definir estimativa
  est = exp(-(cbind(xval,xval^2,xval^3)%*%coef))
  # Calcular probabilidade de troca
  prob0 = 1/(1+est)                             
  # Calcular o complementar (manter) e guardar no objeto
  prob0 = cbind(1-prob0,prob0)                   
  
  # 6. Estimativa estrutural
  
  # Rodar o NPL
  out = npl_sing(indobsa,indobsx,zmat,prob0,beta,fmat,namespar)
  
  # Recuperar LL
  LL = out[[length(out)]][[4]]
  
  # Recuperar parâmetro RC
  RC <- out[[length(out)]][[1]][1]
  
  # Calcular SE
  RC_SE <- sqrt(out[[length(out)]][[2]][1])
  
  # Recuperar parâmetro T1
  T1 <- out[[length(out)]][[1]][2]
  
  # Cacular SE
  T1_SE <- sqrt(out[[length(out)]][[2]][4])
  
  # Juntar resultados
  r <- data.frame(Grupo=label,
                  LL=LL,
                  RC=RC,
                  RC_SE=RC_SE,
                  T1=T1*1000,
                  T1_SE=T1_SE*1000)
  
  # Juntar com resultado do grupo anterior
  resultados <- rbind(resultados,r)
  
}



resultados %>%
  xtable::xtable(digits = c(0,rep(3,ncol(resultados))),
                 caption="Estimativas pelo método NPL",
                 label="tab:5") %>%
  print(include.rownames=F,
        comment=FALSE)