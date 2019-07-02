# Definir a função de custo míope
custo_miope=function(S, FCM, params, p){
  
  "A função custo míope computa o custo esperado associado com
  cada decisão para cada estado, retornando um conjunto de 
  decisões/estados de custo.
    
   Inputs:
    * Um inteiro S, descrevendo os possíveis estados do ônibus.
    Na pratica, é o número de meses analisados
    * A função de custo de manutenção FMC, que tem como input 
    um vetor de parâmetros e estados
    * O vetor params, como entrada para FMC. O primeiro elemento do vetor 
      é o custo de substituição cs.
    * Um vetor (3x1) descrevendo os estados de transição de probabilidades 
        
    Outputs:
    * Uma matriz (Sx2) contendo os custos de manutenção e troca para os N 
      possíveis estados dos ônibus"
  
  # Definir o custo de substituição a partir dos 
  # Parâmetros fornecidos
  cs = params[1]
  # Definir um vetor para receber os custos
  # de manutenção
  maint_cost = rep(NA,S)
  # Definir um vetor para receber os custos
  # de substituição
  repl_cost = rep(NA,S)
  
  for(s in 1:S){
    maint_cost[s] = FCM(s,params[-1])
    repl_cost[s] = cs
  }
  
  cbind(maint_cost,repl_cost)
}

# Possíveis formas para a função custo:
# Linear: FCM(s,theta)=theta_11*s
# Quadrática: FCM(s,theta)=theta_11*s+theta_12*s^2
# Exponencial: FCM(s,theta)=exp(theta_11*s)
# Log: FCM(s,theta)=log(theta_11+theta_12*s)

lin_cost=function(s,params) s*params[1]*0.001
quad_cost=function(s,params) (s*params[1]*0.001)+s^2*params[2]
exp_cost=function(s,params) exp(s*params[1]*0.001)
log_cost=function(s,params) log(params[1]*0.001 + s*params[2])

# Definição das escolhas de probabilidade, como função do vetor
# de custos

escolha_prob=function(cost_array){
  
  # Retorna a probabilidade de cada escolha, 
  # condicional a um vetor de custos.
  
  S = nrow(cost_array)
  
  #toma a diferença pois 1) resultados são iguais
  
  cost = cost_array-apply(cost_array,1,min) 
  
  # 2) mais estável com exp()
  util = exp(-cost)
  pescolha = util/rowSums(util)
  
  pescolha
}