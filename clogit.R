clogit=function(ydum,x,restx,namesb,sup_out=T){
  
  "Estimativa do Logit condicional de McFadden's Conditional
  Alguns parâmetros podem ser restringidos com
  o algorítmo de otimização: Newton's com
  Gradiente e Hessiano analítico
  
  Fonte: https://github.com/waynejtaylor/Single-Agent-Dynamic-Choice/blob/master/clogit.R

  Input       ydum    - (nobs x 1) vetor da variável dependente categórica:
                        {1, 2, ..., nalt}
              x       - (nobs x (k * nalt)) matriz das variáveis
              explicativas irrestritas, seguindo as alternativas de ydum.
              restx   - (nobs x nalt) vetor da soma das variáveis explicativas
              com restrição de parâmetro igual a 1
              namesb  - (k x 1) vetor com o nome dos parâmetros
              sup_out - TRUE/FALSE mostrar ou não o output
  Output      best    - (k x 1) vetor com as estimativas ML
              varest  - (k x k) matrizes de variância e covariância"
  
  # Tolerância
  cconvb = 1e-6
  # Toerância do zero
  myzero = 1e-16
  # Número de observações
  nobs = length(ydum)
  # Número de alternativas
  nalt = max(ydum)
  # Número de parâmetros
  npar = ncol(x)/nalt
  # Teste para parâmetros
  if(npar!=length(namesb)) cat("Erro: Número e nome dos parâmetros não batem ")
  
  # Objeto para receber Valorda soma
  xysum = rep(0,npar)
  # Passo 1
  j=1
  # Iniciar loop
  while(j<=nalt){
    # Somar
    xysum = xysum + colSums((ydum==j)*x[,(npar*(j-1)+1):(npar*j)])
    j=j+1
  }
  
  # Iteração
  iter=1
  # Número de iterações
  criter = 1000
  # Likelihood
  llike = -nobs
  # Matriz para os parâmetros
  b0 = matrix(0,npar,1)
  
  # Iniciar looping
  while(criter>cconvb){
    
    # A cada passo, mostrar
    if(!sup_out){
      cat("Iteração                = ",iter,fill=TRUE)
      cat("Função Log-Likelihood  = ",llike,fill=TRUE)
      cat("Norm of b(k)-b(k-1)      = ",criter,fill=TRUE)
    }
    
    #Computando as probabilidades
    phat = matrix(0,nobs,nalt)
    j=1
    # Iniciar looping
    while(j<=nalt){
      phat[,j] = x[,(npar*(j-1)+1):(npar*j)]%*%b0 + restx[,j]
      j=j+1
    }
    
    # Valores estimados
    phat = phat - apply(phat,1,max)
    phat = exp(phat)/rowSums(exp(phat))
    
    # Computar a média
    sumpx = matrix(0,nobs,npar)
    xxm = matrix(0,npar,npar)
    llike = 0
    j=1
    # Loop
    while(j<=nalt){
      xbuff = x[,(npar*(j-1)+1):(npar*j)]
      sumpx = sumpx + phat[,j]*xbuff
      xxm = xxm + t(phat[,j]*xbuff)%*%xbuff
      llike = llike+sum((ydum==j)*
                          log((phat[,j]>myzero)*phat[,j]
                              +(phat[,j]<myzero)*myzero))
      j=j+1
    }
    
    # Computar o gradiente
    d1llike = xysum - colSums(sumpx)
    
    # Computar a hessiana
    d2llike = - (xxm - t(sumpx)%*%sumpx)
    
    # Iteração Gauss
    b1 = b0 - solve(d2llike)%*%d1llike
    criter = sqrt(t(b1-b0)%*%(b1-b0))
    b0 = b1
    iter = iter + 1
  }
  
  # Erro-padrão
  Avarb  = solve(-d2llike)
  sdb    = sqrt(diag(Avarb))
  tstat  = b0/sdb
  
  if(!sup_out){
    numyj  = as.vector(table(ydum))
    logL0  = sum(numyj*log(numyj/nobs))
    lrindex = 1 - llike/logL0
    
    cat("---------------------------------------------------------------------",fill=TRUE)
    cat("Nº de Iterações     = ",iter,fill=TRUE)
    cat("Nº de observações   = ",nobs,fill=TRUE)
    cat("Função Log-Likelihood  = ",llike,fill=TRUE)
    cat("Likelihood Ratio Index   = ",lrindex,fill=TRUE)
    cat("---------------------------------------------------------------------",fill=TRUE)
    print(data.frame(Parameter=namesb,
                     Estimate=round(as.numeric(b0),3),
                     StandardErrors=round(sdb,3),
                     tratios=round(as.numeric(tstat),3)))
    cat("---------------------------------------------------------------------",fill=TRUE)
  }
  
  list(b0=b0,Avarb=Avarb,L=llike)
}
