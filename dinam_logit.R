# logit dinâmico
dynamiclogit=function(params,data,S,p,FCM){
  
  "Avalia os parâmetros de custos subjacente ao padrão de troca do motor
  do ônibus por um agente forward-looking.
  
  Inputs:
  * Base de dados: um 'data.frame' com:
    -escolha: nome da coluna contendo uma variável
    dummy de escolha endógena
    -estado:nome da coluna contendo uma variável de estado exógeno 
  
  * p: o vetor de transição de estado da variável exógena.
      Exemplo: p = [0, 0.6, 0.4] significa que o ônibus irá  
      mudar para o próximo estado de milhagem com probabilidade 0.6, 
      e para o segundo estado de milhagem 0.4.
  
  * FCM: Função de custo de manutenção, que deve deve ter como
  primeiro argumento o estado s e como segundo argumento um vetor
  de parâmetros."
  
  endog = data$troca
  exog = data$estado
  
  N=length(endog)
  S=S
  
  # Matrizes para acelerar a computação do log-likelihood
  
  # Uma matriz (SxN) indicando o estado de cada computação
  state_mat=matrix(0,S,N)
  
  for(s in 0:(S-1)) state_mat[s+1,]=(exog==s)*1 
  
  # Uma matriz (2xN) indicanto com uma dummy a decisão 
  # tomada por cada agente (manutenção ou troca)
  dec_mat = rbind(t(1-endog),endog)
  
  "
  A log-likelihood do modelo dinâmico é estimada em alguns passos:
  1) Os parâmetros atuais entram na função  contraction mapping
  2) A função retorna uma matriz de decisão 
     de probabilidade para cada estado
  3) Essa matriz é utilizada para computar a log-likelihood das observações
  4) A log-likelihood é somada entre os indivíduos
  "
  
  util = contraction_mapping(S=S, 
                             p=p, 
                             FCM=FCM, 
                             params=params, 
                             beta = beta,
                             suppr_output = TRUE)
  pescolha = util$CP_forward
  logprob = log(t(state_mat)%*%pescolha)
  -sum(logprob*t(dec_mat))
}