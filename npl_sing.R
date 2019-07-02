library(pracma)

npl_sing=function(inda,indx,zmat,pini,bdisc,fmat,names){
  
  "
  Estimativa dos parâmetros estruturais por Máxima Verossimilhança 
  de escolha discreta de programação dinâmica (single-agent)
  usando o algorítmo NPL (Aguirregabiria and Mira, Econometrica, 2002)
  
  Fonte: https://github.com/waynejtaylor/Single-Agent-Dynamic-Choice/blob/master/npl_sing.R 
   
  ---------------------------------------------------------------
  
  INPUTS:
    inda    - (nobs x 1) Vetor com o índice das escolhas discretas (1,...,J)
  
    indx    - (nobs x 1) Vetor com o índice da variável de estado (1,..,S)
  
    zmat    - (zmat1,zmat2,...,zmatJ) matrizes com os valores das variáives
              z(a=j,x)
            obs.: cada zmat possui J colunas
            para representar a utilizade da escolha j
            dada uma ação a
  
    pini    - (numx x J) vetor com as 
    estimativas iniciais de probabilidade Pr(a=j|x)
  
    bdisc   - Fator de desconto (entre 0 e 1)
  
    fmat    - (fmat1,fmat2,...,fmatJ) matriz com
    a probabilidade condicional de transição
  
    names   - (npar x 1) vetor com o nome dos parâmtros
  
   OUTPUTS:
    Lista de tamanho K na qual a k'th estrada contém:
    
    tetaest - (npar x 1) matrizes com os parâmetros estruturais 
    do k'th passo
  
    varest  - (npar x npar) matrizes de variânca e covariância 
    do k'th passo
  
    pest    - (numx x J) matriz com as probabilidades
    Pr(d=1|x),...,Pr(d=J|x) estimadas k'th passo
  ---------------------------------------------------------------"
  
  # Número de parâmetros
  npar = length(names)
  # Número de observações
  nobs = length(inda)
  # Número de escolhas
  nchoice = max(inda)
  # Teste para a matriz zmat
  if(ncol(zmat)!=(npar*nchoice)){
    print("Erro: O número de colunas em 'zmat' não bate",fill=TRUE)
    print("com o número de: 'escolhas * nº parâmetros'",fill=TRUE)
  }
  
  # Tolerância para o valor zero
  myzero = 1e-9
  # Constante de Euler
  eulerc = 0.5772
  # Tamanho do pini
  numx = nrow(pini)
  # Número de interações
  convcrit = 1000
  # Tolerância convergência
  convcons = 1e-9
  # Theta estimado
  tetaest0 = matrix(0,npar,1)
  # Objeto do output
  out = NULL
  
  #---------------------------------------------------------
  #             Estimativa dos parâmetros estruturais
  #---------------------------------------------------------
  # COntrole inicial
  ks=1
  
  # Iniciar loop
  
  while(convcrit>=convcons){
    
    #cat("-----------------------------------------------------",fill=TRUE)
    #cat("Estimador: ESTÁGIO =",ks,fill=TRUE)
    #cat("-----------------------------------------------------",fill=TRUE)
    
    #1. Obter as matrizes 
    #"A=(I-beta*Fu)", "Bz=sumj{Pj*Zj}" e o vetor Be=sumj{Pj*ej}
    #-----------------------------------------------------------------------------------
    
    i_fu = matrix(0,numx,numx)
    sumpz = matrix(0,numx,npar)
    sumpe = matrix(0,numx,1)
    j=1
    
    # Loop da escolha
    
    while (j<=nchoice){
      i_fu = i_fu + pini[,j]*fmat[,(numx*(j-1)+1):(numx*j)] #coluna de referência
      sumpz = sumpz + pini[,j]*zmat[,(npar*(j-1)+1):(npar*j)]
      sumpe = sumpe + pini[,j]*(eulerc - log(pini[,j]+myzero)) # Tratamento para o log
      j=j+1 ;
    }
    
    i_fu = diag(numx) - bdisc * i_fu
    
    #2. Resolução dos sistemas
    #"A*Wz = Bz" e "A*We = Be" usando  a decomposição CROUT
    #-----------------------------------------------------------------------------------
    
    i_fu = lu(i_fu)
    wz = solve(i_fu$L,cbind(sumpz,sumpe))
    wz = solve(i_fu$U,wz)
    
    we = wz[,npar+1]
    wz = wz[,1:npar]
    
    #OU:
    # we=solve(i_fu,sumpe)
    # wz=solve(i_fu,sumpz)
    
    #3. Computando 
    #"ztilda(a,x) = z(a,x) + beta * F(a,x)'*Wz" and "etilda(a,x) = beta * F(a,x)'*We"
    #-----------------------------------------------------------------------------------
    
    ztilda = matrix(0,numx,nchoice*npar)
    etilda = matrix(0,numx,nchoice)
    j=1
    while(j<=nchoice){
      ztilda[,(npar*(j-1)+1):(npar*j)] = zmat[,(npar*(j-1)+1):(npar*j)]+bdisc*fmat[,(numx*(j-1)+1):(numx*j)]%*%wz
      etilda[,j] = bdisc * fmat[,(numx*(j-1)+1):(numx*j)]%*%we  
      j=j+1
    }
    
    #4. Observações de "ztilda" e "etilda"
    #-----------------------------------------------------------------------------------
    
    zobs = ztilda[indx,]
    eobs = etilda[indx,]
    
    #-----------------------------------------------------------------------------------
    #5. Estimação de Pseudo Maximum Likelihood 
    
    clogitout=clogit(inda,zobs,eobs,names)
    tetaest1=clogitout$b0
    varest=clogitout$Avarb
    L=clogitout$L
    
    #6. Recomputando as probabilidades
    #-----------------------------------------------------------------------------------
    
    pini = matrix(0,numx,nchoice)
    j=1
    while(j<=nchoice){
      pini[,j] = ztilda[,(npar*(j-1)+1):(npar*j)]%*%tetaest1 + etilda[,j]
      j=j+1
    }
    pini = pini - apply(pini,1,max)
    pini = exp(pini)
    pini = pini/rowSums(pini)
    
    #7. Critério de convergência
    #-----------------------------------------------------------------------------------
    convcrit = max(abs(tetaest1-tetaest0))
    tetaest0 = tetaest1
    #cat("Critério NPL =",convcrit,fill=TRUE)
    
    #8. Salvar o output do estágio k
    #------------------------------------------------------------------------------------
    out[[ks]]=list(tetaest=tetaest1,varest=varest,pini=pini,L=L)
    
    # Seguir para o próximo passo
    ks=ks+1
  }
  
  # Retornar o output
  out
}