# Função Contraction map
contraction_mapping=function(S, 
                             p, 
                             FCM, 
                             params, 
                             beta=beta, 
                             threshold=1e-9, 
                             suppr_output=FALSE){
  
  "Computa o valor esperado não-míope do agente para cada decisão possível 
  e cada possível estado do ônibus.
  A iteração é realizada até a diferença obtida entre o passo
  anterior e o atual
  é menor que o limiar.
  Inputs:
  * Um número finito de estados S
  * Um vetor com as probabilidades de transição de estado 
  p = [p(0), p(1), p(2), ..., p(k)] de tamanho k < S
  * Uma função de manutenção de custos FMC
  * Um vetor de parâmetros para a função custo
  * Um fator de desconto beta (opcional)
  * Um limiar para convergência (opcional)
  Outputs:
  * A convergência para a escolha de probabilidade forward-looking e 
  míope para cada estado, condicional aos 'params'"
  
  achieved = TRUE
  
  # Inicialização das matrizes de estado de transição
  # ST_mat: descreve as probabilidades de transição de 
  # estado se os custos de manutenção ocorrem
  # RT_mat: Estado regenerado para 0 se o custo de troca acontece.
  # [a,b] = transição do estado "a" para "b"
  ST_mat = matrix(0,S,S)
  lp = length(p)
  for(i in 1:S){
    for(j in 1:lp){
      if((i+j-1)<S)  ST_mat[i,i+j-1] = p[j]
      #sobre as colunas (probabilidades colapsadas)
      if((i+j-1)==S) ST_mat[i,i+j-1] = sum(p[j:lp]) 
    }
  }
  
  R_mat = cbind(1,matrix(0,S,S-1))
  
  # Inicialização do valor esperado 
  # (que também é a decisão de custo do agente míope).
  # Aqui, o componente forward-looking é inicializado em 0.
  k = 0
  EV = matrix(0,S,2)
  EV_miope = EV_nova = custo_miope(S, FCM, params, p)
  
  # Contraction mapping loop
  while(max(abs(EV_nova-EV)) > threshold){
    # Guardar o valor esperado anterior
    EV = EV_nova
    # Obter a probabilidade de manutenção e 
    # troca a partir do valor esperado anterior
    pescolha = escolha_prob(EV)
    # Computar o custo esperado para cada estado: vetor Nx1
    ecost = rowSums(pescolha*EV)
    # Computar os dois componentes da utilidade forward-looking: 
    # No caso de manutenção, a utilidade dos futuros 
    # estados ponderada pela
    # probabilidade de transição. No caso de troca,
    # a utilidade futura é a utilidade do estado 0
    futil_maint = ST_mat%*%ecost
    futil_repl = R_mat%*%ecost
    futil = cbind(futil_maint,futil_repl)
    # A utilidade futura é descontada por beta, e 
    # adicionado ao custo míope. 
    EV_nova = EV_miope + beta*futil
    k = k+1
    if(k == 10000) achieved = FALSE
  }
  
  if(!suppr_output){
    if(achieved){
      cat("Convergência atingida em ",k," iterações")
    } else {
      cat("CM não convergiu! Diferênça média = ",
          round(mean(EV_nova-EV),2))
    }
  }
  
  list(CP_forward=escolha_prob(EV_nova),CP_miope=escolha_prob(EV_miope))
}