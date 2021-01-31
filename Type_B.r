rm(list =ls())

# _____________________________________________________________________
# _____________________________________________________________________
# _____________________** MODELAGEM TIPO II **_________________________
# _____________________________________________________________________
# _____________________________________________________________________

#                  --- VETOR de Dimensáo = N_pontos  ---
# ---  Cada posição do vetor define a Rota a que o Ponto foi alocado  ---
#  ---  Cada posição do vetor (variável) varia entre 1 e N_veiculos  ---

# ***********************************************************************

# EXEMPLOS: Vetores com 10 e 100 pontos / 3 e 10 Veículos

# ------------ Cria o caminho para o diretório do Script
sFolder = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(sFolder))

#   --------------- INICIALIZAÇÃO - VETOR INICIAL ----------------
# V <- rep(1:3, each=3)
# vetor<- append(V, 3, after = 9)

bVetor_100 = TRUE
N_populacao <- 10
N_iteracoes <- 50

if (bVetor_100) {
  
  V <- rep(1:10, each=10)
  N_pontos<-100
  N_veiculos<-10
  
  # ------------------ LEITURA das COORDENADAS dos PONTOS
  # ....... Faz a Leitura de um Total de Pontos = (N_pontos + N_veiculos)
  Pontos_Coord <- read.csv2("Data/110pontos.csv",header=TRUE,sep=";")
  
} else {
  
  V <- rep(1:3, each=3)
  N_pontos<-10
  N_veiculos<-3
  Pontos_Coord <- read.csv2("Data/13pontos.csv",header=TRUE,sep=";")
  
}

vetor <- V
N_vetor<-length(vetor)


length(vetor)
vetor

Vetor_veiculos <- 0
for (i in 1:N_veiculos) Vetor_veiculos[i] <- N_pontos + i
Vetor_veiculos

#       ------------------- FUNÇÃO PRINCIPAL  -----------------------
#        --- Blocos de Código dentro de uma Função Principal  ---
#                  --- VETOR de Dimensáo = N_pontos  ---
#   ---  Cada posição do vetor (variável) varia entre 1 e N_veiculos  ---
#  ---  Cada posição do vetor define a Rota a que o Ponto foi alocado  ---
# ***********************************************************************
#

#                  ********** ATENÇÃO **********
# **** AQUI QUE SE DÁ <CTRL> <ENTER> para uso da Modelagem II ******
f_Obj_Meta3<- function(x){
  
  # ########################### FUNÇÕES ###################################
  
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 01 - INÍCIO -----------------------
  # ***********************************************************************
  
  # BLOCO No. 01 - Recebe vetor "x" da metaheuristica (Vetor Permutado)
  
  # Recebe vetor "x" da metaheuristica
  # Arredonda valores de "x" para o Inteiro mais próximo
  # Varia entre 1 e N_veiculos
  x
  x_ajustado <- (x + 0.5)
  x_ajustado
  
  vetor_permutado <- as.integer(x_ajustado)
  vetor_permutado
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 02 - INÍCIO ---------
  # ***********************************************************************
  # BLOCO No. 02 - Ordem dos Pontos no Vetor por Sequência de Rotas
  #     O vetor está na sequência das Rotas: rota 1, rota 2, ....rota n
  #   O valor de cada posição do Vetor é o No. do Ponto que está na Rota
  #  Ele apresenta os Pontos da Rota 1, Pontos da Rota 2....Pontos da Rota n
  
  pos_rota_ord<-order(vetor_permutado)
  pos_rota_ord
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 03 - INÍCIO -----------------------
  # ***********************************************************************
  
  # ---------BLOCO No. 03 - CRIAÇÃO de um VETOR de ROTAS -------------------
  # Transformar Vetor Permutado ( pos_rota_ord) para uma Sequencia de Rotas
  # O vetor tem uma sequência do tipo: Rota 1, Rota 2, ..... Rota n
  # O número da rota aparece tantas vezes quantos forem os No. de Ptos/rota
  
  vetor_rota<-0
  for (i in 1:N_vetor) vetor_rota[i] <- vetor_permutado[pos_rota_ord[i]]
  vetor_rota
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 04 - INÍCIO -----------------------
  # ***********************************************************************
  # ---- BLOCO No. 04 ----
  # ---- Identifica Posição de Início de cada Rota no Vetor de Rotas ----
  
  pos_vetor_rota<-0
  for (i in 1:N_veiculos) {
    pos_vetor_rota[i] <-0  }
  pos_vetor_rota
  
  for (j in 1:N_veiculos) {
    for (i in 1:N_vetor) {
      if (vetor_rota[i] == j)
        pos_vetor_rota[j] <- i  } }
  pos_vetor_rota  
  
  # AJUSTE das POSIÇÕES para os casos em que não há rota para um Veículo
  # Nestes casos a posição do veículo é incializada com ZERO
  # Aqui faz-se este ajuste: troca-se o 0 pelo valor da Posição Anterior
  for (i in 2:(N_veiculos-1)) {
    if (pos_vetor_rota[i] == 0) pos_vetor_rota[i]<- pos_vetor_rota[i-1]
  }
  if (pos_vetor_rota[N_veiculos] == 0) pos_vetor_rota[N_veiculos]<- N_vetor
  pos_vetor_rota
  
  
  # ***********************************************************************
  #     ------------------- BLOCOS No. 05 - INÍCIO -----------------------
  
  # -----     BLOCO No. 05 - CRIA ROTAS em "GOTAS" (CIRCULARES)   -----
  # ... Todas as rotas iniciam e terminam no Ponto de Origem 
  #   Todas formam um circuito...ficam no formato de uma "GOTA"
  #   Todas as rotas ficam em forma de Gota no próprio Vetor de Rotas
  # Acrescenta-se Ponto de Origem no Início/Final das Rotas (forma a GOTA)
  # -----------------------------------------------------------------
  
  # --- Cria e Inicializa um Vetor Circular com Rotas em Gotas (rotas Circulares)
  # ............   Vetor Circular Inicial = Vetor "pos_rota_ord"    ........
  vetor_rota_circ <- pos_rota_ord
  vetor_rota_circ
  
  # -----     CRIA ROTAS em "GOTAS" (CIRCULARES)   -----
  # ------- Acrescenta Ponto de Origem ao Início/Fim de cada Rota
  
  ## *********************** ATENÇÃO:
  # ==>> pos_rota_ord = Posições dos Veículos no Vetor Permutado Inicial
  # ==>> pos_vetor_rota = Posições de Veícs no Vetor Circular
  
  j=0
  for (i in N_veiculos:1) {
    vetor_rota_circ<- append(vetor_rota_circ,N_pontos+N_veiculos-j, after = pos_vetor_rota[i])
    if (i !=1){
      vetor_rota_circ<- append(vetor_rota_circ,N_pontos+N_veiculos-j, after = pos_vetor_rota[i-1])
    }
    j <- j+1
  }
  vetor_rota_circ<- append(vetor_rota_circ,Vetor_veiculos[1], after = 0)
  vetor_rota_circ
  
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 06 - INÍCIO -----------------------
  # ***********************************************************************
  
  # BLOCO No. 06 - Identifica Início/Fim das Rotas no Vetor de Rotas em Gotas
  #                Limites das Rotas 
  # -----          Identifica também o No. de Pontos em cada Rota    ------
  
  Limites_Rota_circ<-matrix(ncol = 4, nrow = N_veiculos)
  Pontos_na_Rota <- 0
  for (i in 1:N_veiculos){
    Limites_Rota_circ[i,1] <- N_pontos + i #  .....No. da Rota
    Limites_Rota_circ[i,2] <- which(vetor_rota_circ == N_pontos+i)[1] # ...Início
    Limites_Rota_circ[i,3] <- which(vetor_rota_circ == N_pontos+i)[2] # ...Fim
    Limites_Rota_circ[i,4] <-  Limites_Rota_circ[i,3] -  Limites_Rota_circ[i,2]
  }
  Limites_Rota_circ
  Maior_rota <- max(Limites_Rota_circ[,4])
  Maior_rota
  vetor_rota_circ
  
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 07 - INÍCIO -----------------------
  #         BLOCO No. 07 - CALCULA DISTÂNCIAS - "Função Objetivo"
  # ***********************************************************************
  
  # Função Preliminar: Cálculo de DisTãncia Euclideana entre dois Pontos
  Dist <- function(XA, YA, XB, YB){
    D_AB <- sqrt((XA-XB)**2+(YA-YB)**2)
    return(D_AB)
  }
  
  # BLOCO No. 07 - CALCULA DISTÂNCIAS - "Função Objetivo"
  # Três saídas do Bloco No. 07:  . Vetor de Distâncias por Trecho por Rota
  #                               . Vetor de Distâncias Totais por Rota e
  #                               . Distância Global
  
  # .... INICIALIZAÇÃO ......
  # ... Dimensiona Matriz de Distâncias pela Rota com maior No. de Pontos
  Distances_Vetor <- matrix(nrow = N_veiculos, ncol = Maior_rota)
  
  # ... Inicializa Distância Total de cada Rota
  Distance_Rota <- 0 
  for (i in 1:N_veiculos) Distance_Rota[i] <- 0
  
  # .... CÁLCULOS de DISTÂNCIAS......chama Função <Dist() >
  for (i in 1:N_veiculos){
    for (j in 0:Limites_Rota_circ[i,4]){
      Dist_AB <- Dist (Pontos_Coord$X[vetor_rota_circ[Limites_Rota_circ[i,2]+j]], 
                       Pontos_Coord$Y[vetor_rota_circ[Limites_Rota_circ[i,2]+j]],
                       Pontos_Coord$X[vetor_rota_circ[Limites_Rota_circ[i,2]+j+1]],
                       Pontos_Coord$Y[vetor_rota_circ[Limites_Rota_circ[i,2]+j+1]] ) 
      if (j < Limites_Rota_circ[i,4]){
        Distances_Vetor[i,j+1] <- Dist_AB 
        Distance_Rota[i] <- Distance_Rota[i] + Distances_Vetor[i,j+1]
      }
    }
  }
  
  # ------------ CÁLCULO de DISTÃNCIA GLOBAL ==>> FUNÇÃO OBJETIVO ------------
  Distance_Global <- 0
  for (i in 1:N_veiculos) Distance_Global <- Distance_Global + Distance_Rota[i]
  Distances_Vetor
  Distance_Rota
  Distance_Global
  
  Len_routes<- length(vetor_rota_circ)
  Rotas_e_Dist<- append(vetor_rota_circ, round(Distance_Global, digits =1), after = Len_routes)
  
  sFolder = rstudioapi::getActiveDocumentContext()$path
  setwd(dirname(sFolder))
  
  # OK!!!!! ***** O APPEND Funciona com o comando write.table() abaixo: *******
  write.table(round(Distances_Vetor, digits = 1), "./Output/Distancias_por_Trecho_de_Rota3.csv", sep = ";",
              col.names = !file.exists("./Output/Distancias_por_Trecho_de_Rota3.csv"), append = T)
  
  write.table(round(Distance_Rota, digits = 1), "./Output/Distancias_por_Rota3.csv", sep = ";",
              col.names = !file.exists("./Output/Distancias_por_Rota3.csv"), append = T)
  
  write.table(round(Distance_Global, digits = 2), "./Output/Distancia_Global3.csv", sep = ";",
              col.names = !file.exists("./Output/Distancia_Global3.csv"), append = T)
  
  write.table(Rotas_e_Dist, "./Output/Rotas_Circulares3.csv", sep = ";",
              col.names = !file.exists("./Output/Rotas_Circulares3.csv"), append = T)
  
  write.table(Limites_Rota_circ, "./Output/Pontos_por_Rota3.csv", sep = ";",
              col.names = !file.exists("./Output/Pontos_por_Rota3.csv"), append = T)
  
  # ***********************************************************************
  #     ------------------- BLOCOS 1 a 7 - FIM -----------------------
  # ***********************************************************************
  
  
  # ***********************************************************************
  #     -------------RETORNO da FUNÇÃO OBJETIVO -----------------------
  # ***********************************************************************
  # return( list(Alocação_de_Rotas_por_Ponto_do_Vetor = vetor_permutado,
  #              Pontos_por_Sequência_das_Rotas_no_Vetor = pos_rota_ord,
  #              Vetor_com_as_Rotas_em_Sequencia = vetor_rota,
  #              Qtde_de_Veiculos = N_veiculos,
  #              Numeros_dos_Veiculos = Vetor_veiculos,
  #              Vetor_de_Rotas_Circulares_GOTAS = vetor_rota_circ,
  #              Posições_de_Início_Fim_das_Rotas_em_Gotas = Limites_Rota_circ,
  #              No._de_Pontos_por_Rota_em_Gota = Limites_Rota_circ[,4],
  #              Vetores_de_Distâncias_por_Rota = Distances_Vetor,
  #              Distâncias_Totais_por_Rota = Distance_Rota,
  #              Distância_GLOBAL_da_OPERAÇÃO = Distance_Global))
  
  return(Distance_Global) # .... Para a META retorna só a F. Obj.
  
}  # ..... Chave da FUNÇÃO

# ***********************************************************************
# ----------- APLICAÇÃO da FUNÇÃO ABC para MODELAGEM TIPO II ------------
# ***********************************************************************

# FUNÇÃO ABC() ===>>>> para F.OBJ_META3
#   ***** ATENÇÃO: ANTES DE RODAR ESTA MH, RODA-SE A F.OBJ_META3 *******

# install.packages(metaheuristicOpt)
library(metaheuristicOpt)
## INICIALIZAÇÃO: Definição de Parâmetros da ABC (numVar e rangeVar)
numVar <- N_pontos
numVar

rangeVar<- matrix(nrow= 2, ncol=N_pontos)
for (i in 1:2){
  for (j in 1:N_pontos){
    rangeVar[1,j] <- as.integer(1)
    rangeVar[2,j] <- as.integer(N_veiculos)
  }}
rangeVar


## CÁLCULO da SOLUÇÃO ÓTIMA por ABC - Artificial Bee Colony algorithm
Solucao_ABC <- ABC(f_Obj_Meta3, optimType="MIN", numVar, numPopulation=N_populacao,
                   maxIter=N_iteracoes, rangeVar)
Solucao_ABC   # ... Vetor de números REAIS com No.s das Rota de cada Ponto

Rotas_Solucao.Otima_ajustada <- as.integer(Solucao_ABC + 0.5)
Rotas_Solucao.Otima_ajustada  # ... Vetor de números INTEIROS com No.s das Rota de cada Ponto

## RESULTADOS da SOLUÇÃO ÓTIMA usando a Função META3
Solucao.Otima <- f_Obj_Meta3(Rotas_Solucao.Otima_ajustada)
round(Solucao.Otima, digits=2)
#...... VIDE RESULTADOS nos ARQS .CSV GRAVADOS .......
# -----------------------------------------------------------------------