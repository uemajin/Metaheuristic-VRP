
#      ********* METAHEURISTICA ABC para Solucao do PRV *********
#                   ------- JANEIRO/2021 -------
# ---- LABORATORIO BigMAAp - Big Data e Metodos Analeticos Aplicados ----
#               ----- Prof. Arnaldo Vallim -----

# ESTE CoDIGO TEM A APLICAcaO DA METAHEURiSTICA ABC - ARTIFICIAL BEE COLONY
#   O codigo busca solucionar o PRV - Problema de Roteirizacao de Veiculos

# Para a aplicacao da Metaheuristica (MH) ABC tem-se tambem neste codigo:
#   . A REPRESENTAcaO de ROTAS em um VETOR (tem-se DOIS tipos de Representacao)
#   . UMA FUNcaO OBJETIVO para CaLCULO de DISTaNCIAS da SOLUcaO


# O CoDIGO ESTa NA SEGUINTE SEQUeNCIA:

# 1. MODELAGEM TIPO I para REPRESENTAcaO de ROTAS em um VETOR, INTEGRADA
#    com a FUNcaO OBJETIVO (f_Obj_Meta2)
#      Nesta Modelagem I, tinha-se inicialmente duas Alternativas de Algoritmo
#      Foi mantida somente a Alternativa 2
#      Esta Alternativa 2 trabalha com Fun??o Objetivo "f_Obj_Meta2"

# 2. MODELAGEM TIPO II para REPRESENTAcaO de ROTAS em um VETOR, INTEGRADA
#    com a FUNcaO OBJETIVO (f_Obj_Meta3)
#      Nesta Modelagem II, tem-se apenas uma Alternativa de Algoritmo
#      Foi chamada de Alternativa 3
#      Esta Alternativa 3 trabalha com Funcao Objetivo "f_Obj_Meta3"

# Ao final do codigo tem-se DUAS CHAMADAS para a Funcao da MH "ABC"

# 3. APLICAcaO DA METAHEURiSTICA ABC para a Modelagem Tipo I, Alternativa 2 
#    de algoritmo (META2)

# 4. APLICAcaO DA METAHEURiSTICA ABC para a Modelagem Tipo II, Alternativa 3 
#    de algoritmo (META3)


# *****************************************************************************
#            ******************* ATEN??O *******************

#         *********** FORMA DE OPERAcaO DESTE CoDIGO *********** 

# Passo 1: Da-se <CTRL> <ENTER> na chamada da Funcao Objetivo
#          Tem-se duas Alternativas:
#            . <CTRL> <ENTER> NA "f_Obj_Meta2" quando se utiliza a Modelagem I
#            . <CTRL> <ENTER> NA "f_Obj_Meta3" quando se utiliza a Modelagem II
# Passo 2: Roda-se todo a parte do codigo da MH "ABC" (esta ao final do codigo)
#          Tem-se duas Alternativas:
#            . Roda-se "META2" quando se utiliza a Modelagem I
#            . Roda-se "META3" quando se utiliza a Modelagem II
# *****************************************************************************



# --------------  REPRESENTAcaO de ROTAS A PARTIR de um VETOR ----------------
#    -----------  IDENTIFICADAS AS ROTAS, CALCULA-SE A F. OBJ. -------------


# ------------------------------ ATENcaO ----------------------------------
#  -------- Tem-se aqui duas alternativas de modelagem dos dados --------



# _____________________________________________________________________
# _____________________________________________________________________
# _____________________** MODELAGEM TIPO I **_________________________
# _____________________________________________________________________
# _____________________________________________________________________

# -- Tem-se um vetor com todos os Pontos de uma Regiao e com os Numeros
# --    dos Veiculos de uma Frota que vao atender esses Pontos
# Com "n" Pontos e "m" Veiculos, o Vetor tera "n + m" posicoes
# As "n" primeiras posicoes correspondem aos Pontos
# As posicoes "n+1" ate "n+m" correspondem aos Veiculos

# O que se busca aqui, eh dado um vetor permutado, identificar todas os 
# pontos alocados a cada veiculo

# Considera-se que os pontos alocados a um veiculo, sao aqueles que vem 
# na sequencia do seu numero, ate que surja o numero de outro veiculo

# Quando se tem uma sequencia de Pontos apos o numero de um Veiculo, e
# essa sequencia atinge o final do vetor, considera-se que os Pontos 
# alocados aquele veiculo sao esses, dessa sequencia, mais todos do
# inicio do vetor, ate que se atinja o numero de outro veiculo


# ***********************************************************************
#   ------------------- INiCIO da MODELAGEM TIPO I ------------------
# ***********************************************************************

# -------- LIMPA MEMoRIA ------------------
rm(list =ls())
# -----------------------------------------

# ***********************************************************************
#     ---------------------- INICIALIZAcaO -----------------------
#        ---------------- Parametros e Dados ------------------
# ***********************************************************************

# 1 - Inicializa a biblioteca MetaheuristicOpt
library(metaheuristicOpt) 


# 2 - Inicializa a Função Objetivo utilizada nos experimentos.
f_Obj_Meta2<- function(x){
  
  # ########################### FUNcoES ###################################
  
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 01 - INiCIO -----------------------
  # ***********************************************************************
  # BLOCO No. 01 - Localiza a Posicao dos Veiculos no Vetor Permutado
  # Recebe vetor "x" da metaheuristica 
  # Passa valores de "x" para o Vetor Permutado
  # .... ATEN??O: >>>>>>> Adotamos como "x" o "x Ordenado"
  # E passamos para a funcao esse vetor "ordenado" como sendo o Vetor Permutado
  # Esquecemos "x" original e trabalhamos apenas com o "novo" x
  # Com esta "jogada", sempre ter-se-a um vetor com valores de 1 a N_vetor
  
  new_x <- order(x)
  vetor_permutado <- new_x
  vetor_permutado
  
  pos_rota<-c()
  for (i in 1:N_veiculos) {
    pos_rota[i] <-0  }
  pos_rota
  pos_veics_Original
  
  for (j in 1:N_veiculos) {
    for (i in 1:N_vetor) {
      if (vetor_permutado[i] == pos_veics_Original[j])
        pos_rota[j] <- i
    }
  }
  pos_rota
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 02 - INiCIO ---------
  # ***********************************************************************
  # BLOCO No. 02 - Ordem dos Veiculos no Vetor com suas Posicoes
  #                 Identifica a Ordem dos Veiculos no Vetor
  pos_rota_ord<-order(pos_rota)
  pos_rota_ord
  
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 03 - INiCIO -----------------------
  # ***********************************************************************
  
  # ---------BLOCO No. 03 - CRIAcaO de um VETOR de ROTAS -------------------
  # Transformar Vetor Permutado para uma Sequencia de Rotas
  # O vetor deve ter uma sequencia do tipo: Rota 1, Rota 2, ..... Rota n
  # A Funcao puxa para a 1a posicao o 1o Veiculo que aparece no vetor
  # O vetor passa a iniciar com um Veiculo
  # Puxa para as posicoes seguintes toda a sequencia apos o 1o Veiculo
  # Depois disso:
  # Funcao transfere para o final, os primeiros elementos do vetor
  # que estavam nas posicoes do vetor anteriores ao primeiro veiculo
  
  # ------ Montagem do novo Vetor de Rotas (Vide explicacao acima) -----
  #               Pontos do inicio do vetor vao para o final
  
  N0<-pos_rota[pos_rota_ord[1]] - 1
  N0
  vetor_rota<-0
  
  j=1
  for   (i in 1:N_vetor){
    vetor_rota[i] <- vetor_permutado[N0 + i] 
    {
      if (N0 + i > N_vetor)
      {
        vetor_rota[i] <- vetor_permutado[j]
        j <- j+1
      }
    }
  }
  vetor_rota
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 04 - INiCIO -----------------------
  # ***********************************************************************
  # ---- BLOCO No. 04 ----
  # ---- Identifica Posicao de Inicio de cada Rota no Vetor de Rotas ----
  
  pos_vetor_rota<-0
  for (i in 1:N_veiculos) {
    pos_vetor_rota[i] <-0  }
  pos_vetor_rota
  
  for (j in 1:N_veiculos) {
    for (i in 1:N_vetor) {
      if (vetor_rota[i] == pos_veics_Original[j])
        pos_vetor_rota[j] <- i  }
  }
  pos_vetor_rota
  
  # Ordena pos_vetor_rota
  pos_vetor_rota_ord<-order(pos_vetor_rota)
  pos_vetor_rota_ord
  
  # ***********************************************************************
  #     ------------------- BLOCOS No. 05 e 06 - INiCIO -----------------------
  # ***********************************************************************
  # ----- CRIA Rotas na forma de GOTAS (Circulares)
  # ... Todas as rotas iniciam e terminam no Ponto de Origem 
  #   Todas formam um circuito...ficam no formato de uma "GOTA"
  #   Todas as rotas ficam em forma de Gota no proprio Vetor de Rotas
  # Basta acrescentar Ponto de Origem de cada Rota ao Final da Rota (forma a GOTA)
  # -----------------------------------------------------------------
  
  ## *********************** ATENcaO:
  # ==>> pos_rota e pos_rota_ord = Posicoes dos Veiculos no Vetor Permutado
  # ==>> pos_vetor_rota e pos_vetor_rota_ord = Posicoes de Veics no Vetor Circular
  
  # ----------------------- BLOCO No. 05 ---------------------------
  # -----         Identifica No. de Pontos em cada Rota        ------
  N_rota<-0
  Pontos <- 0
  for   (i in 1:N_veiculos){
    if (i<= N_veiculos -1){
      N_rota[i]<- pos_vetor_rota[pos_vetor_rota_ord[i+1]] - pos_vetor_rota[pos_vetor_rota_ord[i]]
      Pontos <- Pontos + N_rota[i]
    }
    else {
      N_rota[i]<- N_vetor - Pontos
    }
  } 
  
  N_rota
  Maior_rota<-max(N_rota)
  Maior_rota
  
  # ---------------------- BLOCO No. 06 -------------------------------
  # -----     BLOCO No. 06 - CRIA ROTAS em "GOTAS" (CIRCULARES)   -----
  # ------- Acrescenta Ponto de Origem ao Final de cada Rota
  
  # --- Identificacao da posicao de inicio de cada Rota  
  k<-0
  for (j in 1:N_veiculos)
  { 
    k[j]<-pos_vetor_rota[pos_vetor_rota_ord[j]]
  }
  k
  
  # --- Cria e Inicializa um Vetor Circular com Rotas em Gotas ( rotas Circulares)
  # ............   Vetor Circular Inicial = Vetor_Rota    ........
  vetor_rota_circ<-vetor_rota
  K_N_rota<-0   #... Acumulador para posicoes iniciais das rotas no Vetor Circular
  K_N_rota
  vetor_rota_circ
  # ----- Montagem do Vetor com Rotas em Gotas (rotas Circulares)
  for (i in 1:N_veiculos) {
    if (i==1)  { K_N_rota <- K_N_rota + N_rota[i] }
    else       {K_N_rota <- K_N_rota + N_rota[i] + 1}
    vetor_rota_circ<- append(vetor_rota_circ, vetor_rota[k[i]], after = K_N_rota)
  }
  vetor_rota_circ
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 07 - IN?CIO -----------------------
  # ***********************************************************************
  
  # BLOCO No. 07 - Identifica Inicio/Fim de cada Rota no Vetor de Rotas em Gotas
  #                 Limites das Rotas
  
  Limites_Rota<-matrix(ncol = 9, nrow = N_veiculos)
  Pontos_na_Rota <- 0
  for (i in 1:N_veiculos){
    Limites_Rota[i,1] <- N_pontos + i #...No. da Rota ou Veiculo ou Instalacao
    Limites_Rota[i,2] <- which(vetor_rota_circ == N_pontos+i)[1] # ...Inicio
    Limites_Rota[i,3] <- which(vetor_rota_circ == N_pontos+i)[2] # ...Fim
    Limites_Rota[i,4] <-  Limites_Rota[i,3] -  Limites_Rota[i,2] # Qtde Ptos
    Limites_Rota[i,5] <- 0  # Distancia da Rota
    Limites_Rota[i,6] <- 0  # Peso Movimentado na Rota
    Limites_Rota[i,7] <- 0  # Capacidade do Veiculo da Rota
    Limites_Rota[i,8] <- 0  # Distancia Media por Ponto
    Limites_Rota[i,9] <- 0  # Peso Medio por Ponto
  }
  Limites_Rota
  # ATENÇÃO: Limites_Rota guarda os Nos. das Posições no vetor_rota_circ 
  #          NÃO guarda o No. dos Pontos que estão na Rota
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 08 - INiCIO -----------------------
  #         BLOCO No. 08 - CALCULA DISTaNCIAS - "Funcao Objetivo"
  # ***********************************************************************
  
  # Funcao Preliminar: Calculo de Distancia Euclidiana entre dois Pontos
  Dist <- function(XA, YA, XB, YB){
    D_AB <- sqrt((XA-XB)**2+(YA-YB)**2)
    return(D_AB)
  }
  
  # BLOCO No. 08 - CALCULA DISTANCIAS - "Funcao Objetivo"
  # Tres saidas do Bloco No. 08:  . Vetor de Distancias por Trecho por Rota
  #                               . Vetor de Distancias Totais por Rota e
  #                               . Dist?ncia Global
  
  # .... Inicializacao......
  # ... Dimensiona Matriz de Distancias pela Rota com maior No. de Pontos
  Distances_Vetor <- matrix(nrow = N_veiculos, ncol = Maior_rota)
  # ... Inicializa Distancia Total de cada Rota
  Distance_Rota <- 0 
  for (i in 1:N_veiculos) Distance_Rota[i] <- 0
  
  PESO_ROTA <- 0 
  for (i in 1:N_veiculos) PESO_ROTA[i] <- 0
  
  PESO_PONTO<-0
  
  
  # .... CALCULOS de DISTANCIAS......chama Funcao <Dist() >
  for (i in 1:N_veiculos){
    for (j in 0:Limites_Rota[i,4]){
      Dist_AB <- Dist (Pontos_Coord$X[vetor_rota_circ[Limites_Rota[i,2]+j]], 
                       Pontos_Coord$Y[vetor_rota_circ[Limites_Rota[i,2]+j]],
                       Pontos_Coord$X[vetor_rota_circ[Limites_Rota[i,2]+j+1]],
                       Pontos_Coord$Y[vetor_rota_circ[Limites_Rota[i,2]+j+1]] ) 
      
      if ( (j > 0) & (j< Limites_Rota[i,4]) ){
        PESO_PONTO <- Pontos_Coord$W[vetor_rota_circ[Limites_Rota[i,2]+j]]     
      }
      
      if (j < Limites_Rota[i,4]){
        Distances_Vetor[i,j+1] <- Dist_AB 
        Distance_Rota[i] <- Distance_Rota[i] + Distances_Vetor[i,j+1]
        PESO_ROTA[i] <- PESO_ROTA[i] + PESO_PONTO    
        PESO_PONTO <- 0
      }
      
    }
  }
  
  # ------------ CALCULO de DISTANCIA GLOBAL ==>> FUNCAO OBJETIVO ------------
  Distance_Global <- 0
  # for (i in 1:N_veiculos) Distance_Global <- Distance_Global + Distance_Rota[i]
  Distances_Vetor
  Distance_Rota
  # Distance_Global
  
  # -------- Preenchimento de campos da MATRIZ "Limites_Rota" --------
  # com Dados Associados a PESOS e DISTANCIAS das ROTAS ou INSTALACOES
  #      ... VIDE ABAIXO DICIONARIO da MATRIZ "Limites_Rota" ...
  for (i in 1:N_veiculos){
    Limites_Rota[i,5] <- round(Distance_Rota[i], digits = 1)
    Limites_Rota[i,6] <- PESO_ROTA[i]
    Limites_Rota[i,7] <- Pontos_Coord$W[Limites_Rota[i,1]]
    Limites_Rota[i,8] <- round(Distance_Rota[i]/(Limites_Rota[i,4]), digits = 1) 
    Limites_Rota[i,9] <- round(PESO_ROTA[i]/Limites_Rota[i,4], digits = 1) 
  }
  
  #
  # ----------------- DICIONARIO da MATRIZ "Limites_Rota" -------------------
  # Limites_Rota[i,1] <- N_pontos + i ===>>>  No. da ROTA ou VEICULO ou INSTALACAO
  # Limites_Rota[i,2] <- which(vetor_rota_circ == N_pontos+i)[1] ==>> Inicio
  # Limites_Rota[i,3] <- which(vetor_rota_circ == N_pontos+i)[2] ==>> Fim
  # Limites_Rota[i,4] <-  Limites_Rota[i,3] -  Limites_Rota[i,2] ==>> Qt. Ptos
  # Limites_Rota[i,5] <- 0            ===>>>  Distancia da Rota
  # Limites_Rota[i,6] <- 0            ===>>>  Peso Movimentado na Rota
  # Limites_Rota[i,7] <- 0            ===>>>  Capacidade do Veiculo da Rota
  # Limites_Rota[i,8] <- 0            ===>>>  Distancia Media por Ponto
  # Limites_Rota[i,9] <- 0            ===>>>  Peso Medio por Ponto    
  #
  
  
  # ----- Inicializacao de Variaveis Associadas a "PENALIDADES" --------
  #   ....PENALIDADES associadas a EXCESSO de PESO em VEICULOS ....
  Penalty_Dist_Peso <- 0
  Excesso_Peso <- 0
  Pontos_Adicionais <- 0
  for (i in 1:N_veiculos){
    Penalty_Dist_Peso[i] <- 0
    Excesso_Peso[i] <- 0
    Pontos_Adicionais[i] <- 0
    
    # CHAVE PERDIDA AQUI?????
  }
  
  # ------------- CALCULOS de "PENALIDADES" ---------------
  #    A PENALIDADE "aumenta" o valor da Distancia Global
  # Calcula-se uma Distancia Adicional na Rota devida ao Excesso de Peso
  #    Essa Distancia Adicional é somada a Distancia da Rota
  #    
  for (i in 1:N_veiculos){
    if (Limites_Rota[i,6] > Limites_Rota[i,7]){
      Excesso_Peso[i] <- (Limites_Rota[i,6] - Limites_Rota[i,7])
      Pontos_Adicionais[i] <- Excesso_Peso[i] / mean(Limites_Rota[i,9])
      Penalty_Dist_Peso[i] <- Pontos_Adicionais[i]*mean(Limites_Rota[i,8])
      Distance_Rota[i] = Distance_Rota[i] + Penalty_Dist_Peso[i]
    }
  }
  
  #       ----- CALCULO da Distancia Golbal com PENALIDADE -----
  # As Distancias Adicionais de cada Rota sao somadas a Distancia Global
  #    
  for (i in 1:N_veiculos) Distance_Global <- Distance_Global + Distance_Rota[i]
  
  Len_routes<- length(vetor_rota_circ)
  Rotas_e_Dist<- append(vetor_rota_circ, round(Distance_Global, digits =1), after = Len_routes)
  
  setwd("C:/Users/j.uema/Documents/Metaheuristicas/")
  
  res_dir <- paste("Results/",substr(database_filename,1,nchar(database_filename)-4),sep='')
  dir.create(res_dir,showWarnings = F)
  res_dir_loop <- paste(res_dir, loop, sep='/')
  dir.create(res_dir_loop,showWarnings = F)
  
  # OK!!!!! ***** O APPEND Funciona com o comando write.table() abaixo: *******
  write.table(round(Distances_Vetor, digits = 1), paste(res_dir,loop,"Distancias_por_Trecho_de_Rota2.csv",sep='/'), sep = ";",
              col.names = !file.exists(paste(res_dir,"Distancias_por_Trecho_de_Rota2.csv",sep='/')), append = T)
  
  write.table(round(Distance_Rota, digits = 1), paste(res_dir,loop,"Distancias_por_Rota2.csv",sep='/'), sep = ";",
              col.names = !file.exists(paste(res_dir,"Distancias_por_Rota2.csv",sep='/')), append = T)
  
  write.table(round(Distance_Global, digits = 2), paste(res_dir,loop,"Distancia_Global2.csv",sep='/'), sep = ";",
              col.names = !file.exists(paste(res_dir,"Distancia_Global2.csv",sep='/')), append = T)
  
  write.table(Rotas_e_Dist, paste(res_dir,loop,"Rotas_Circulares2.csv",sep='/'), sep = ";",
              col.names = !file.exists(paste(res_dir,"Rotas_Circulares2.csv",sep='/')), append = T)
  
  write.table(Limites_Rota, paste(res_dir,loop,"Pontos_por_Rota2.csv",sep='/'), sep = ";",
              col.names = !file.exists(paste(res_dir,"Pontos_por_Rota2.csv",sep='/')), append = T)
  
  # -------------- GRAVA PESOS por ROTA em ARQUIVO CSV -----------------
  write.table(PESO_ROTA, paste(res_dir,loop,"PESOS_ROTAS2.csv",sep='/'), sep = ";",
              col.names = !file.exists(paste(res_dir,"PESOS_ROTAS2.csv",sep='/')), append = T)
  
  # ***********************************************************************
  #     ------------------- BLOCOS 1 a 8 - FIM -----------------------
  # ***********************************************************************
  
  # ***********************************************************************
  #     -------------RETORNO da FUNcaO OBJETIVO -----------------------
  # ***********************************************************************
  # return( list(Posicoes_de_Veiculos_no_Vetor = pos_rota,
  #              Ordem_dos_Veiculos_no_Vetor = pos_rota_ord,
  #              Vetor_com_as_Rotas_em_Sequencia = vetor_rota,
  #              o_Pontos_por_Rota = N_rota,
  #              Vetor_de_Rotas_Circulares_GOTAS = vetor_rota_circ,
  #              Posicoes_de_Inicio_Fim_das_Rotas = Limites_Rota,
  #              No._de_Pontos_por_Rota = Limites_Rota[,4],
  #              Vetores_de_Distancias_por_Rota = Distances_Vetor,
  #              Dist?ncias_Totais_por_Rota = Distance_Rota,
  #              Dist?ncia_GLOBAL_da_OPERAcaO = Distance_Global))
  #   
  
  # .... Para a META retorna s? a F. Obj.
  return(Distance_Global)
  
}  # ..... Chave da FUN??O


# ------------------ LEITURA das COORDENADAS dos PONTOS


# 3 - Define o ambiente de execução
setwd("C:/Users/j.uema/Documents/Metaheuristicas/")

# 4 - Lê lista das instâncias 
list.files('Datasets/Uchoa 2014/CSVs')


lapply(list.files('Datasets/Uchoa 2014/CSVs'), function(x) {
  
  loop <<- 1
  
  print(x)

  database_filename <<- x
  fullpath <- paste('Datasets/Uchoa 2014/CSVs/',database_filename, sep="")
  fullpath
  
  Pontos_Coord <<- read.csv2(fullpath,header=TRUE,sep=";")
  
  # 5 - Define os parâmetros da instância a ser executada.
  
  N_pontos <<- max(Pontos_Coord[1]) - 10
  vet_pontos <<- N_pontos + 10
  
  vetor<<- c(1:vet_pontos)
  
  N_vetor<<-length(vetor)
  
  N_veiculos<<-10
  
  N_vetor<<- N_pontos + N_veiculos
  
  # ------------ Localizacao dos Veiculos no Vetor
  pos_veics_Original<<- which(vetor > N_pontos)
  N_pos_veics<<-length(pos_veics_Original)
  
  # ______________________________________________________________________
  
  # ------------------------------------------------------------------------
  
  
  # ***********************************************************************
  # ------------APLICAÇÃO da FUNÇÃO ABC para MODELAGEM TIPO I -------------
  # ***********************************************************************
  
  # FUNÇÃO ABC() ===>>>> para F.OBJ_META2
  #   ***** ATEN??O: ANTES DE RODAR ESTA MH, RODA-SE A F.OBJ_META2 *******
  
  # install.packages(metaheuristicOpt)
  ## INICIALIZACAO: Definicao de Parametros da ABC (numVar e rangeVar)
  
  # 6 - Laço de repetição para N instâncias (N=10)
  
  while (loop <= 10){
    
    print(loop)
    
    numVar <- N_vetor
    numVar
    
    rangeVar<- matrix(nrow= 2, ncol=N_vetor)
    for (i in 1:2){
      for (j in 1:N_vetor){
        rangeVar[1,j] <- as.integer(1)
        rangeVar[2,j] <- as.integer(N_pontos)
      }}
    rangeVar 
    
    ## 6.1 - Calculo da solução por ABC - Artificial Bee Colony algorithm
    
    start.time <- Sys.time() # 6.2 - Benchmark para a medição de tempo de execução

    Solucao_ABC <- ABC(f_Obj_Meta2, optimType="MIN", numVar, numPopulation=10,
                     maxIter=100, rangeVar)
    
    Rotas_Solucao.Otima_ajustada <- order(Solucao_ABC)
    
    Solucao.Otima <- f_Obj_Meta2(Solucao_ABC)
    sol_ot <- round(Solucao.Otima, digits =2)
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    
    setwd("C:/Users/j.uema/Documents/Metaheuristicas/")
    
    # 6.3 - Cria as pastas para guardar os resultados da instância
    
    res_dir <- paste("Results/",substr(database_filename,1,nchar(database_filename)-4),sep='')
    res_dir_loops <- paste(res_dir,loop,sep='/')
    
    # 6.4 Grava os resultados da instância
    file_global <- paste(res_dir_loops,"Distancia_Global2.csv",sep='/')
    
    
    # 6.5 - Abastece DF com resultados.
    df_val <- read.csv2(file_global, header=F,sep=";")
    
    file <- substr(database_filename,1,nchar(database_filename)-4)
    first_sol <- df_val[2,2]
    stand_dev <- sd(as.numeric(unlist(df_val[2])), na.rm=T)
    mins <- as.numeric(time.taken)
    
    df_fin <- data.frame(
      instancia = c(file),
      rodada = c(loop),
      FO_inicial = c(first_sol),
      FO_final = c(sol_ot),
      DP = c(stand_dev),
      tempo = c(mins)
    )
    
    # 7 - Grava em arquivo DF de sumário de Resultados.
    write.table(df_fin, 'Resultados.csv', sep = ";",
                col.names = !file.exists('Resultados.csv'), append = T)
    print('Resultado OK')
    
    loop <<- loop + 1
  }
})
#...... VIDE RESULTADOS nos ARQS .CSV GRAVADOS .......
# -----------------------------------------------------------------------

