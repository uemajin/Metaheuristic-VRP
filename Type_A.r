rm(list =ls())

# _____________________________________________________________________
# _____________________________________________________________________
# _____________________** MODELAGEM TIPO I **_________________________
# _____________________________________________________________________
# _____________________________________________________________________

# -- Tem-se um vetor com todos os Pontos de uma Região e com os Números
# --    dos Veículos de uma Frota que vão atender esses Pontos
# Com "n" Pontos e "m" Veículos, o Vetor terá "n + m" posições
# As "n" primeiras posições correspondem aos Pontos
# As posições "n+1" até "n+m" correspondem aos Veículos

# O que se busca aqui, é dado um vetor permutado, identificar todas as 
# pontos alocados a cada veículo

# Considera-se que os pontos alocados a um veículo, são aqueles que vêm 
# na sequência do seu número, até que surja o número de outro veículo

# Quando se tem uma sequência de Pontos após o número de um Veiculo, e
# essa sequência atinge o final do vetor, considera-se que os Pontos 
# alocados àquele veículo são esses, dessa sequência, mais todos do
# início do vetor, até que se atinja o número de outro veículo

# EXEMPLOS: Vetores com 10 e 100 pontos / 3 Veículos e 10 veículos

# ------------ Cria o caminho para o diretório do Script
sFolder = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(sFolder))

bVetor_100 <- TRUE # Caso TRUE, 100 pontos e 10 veiculos
N_populacao <- 20
N_iteracoes <- 100

if (bVetor_100) {

  vetor<- c(1:110)
  N_pontos<-100
  N_veiculos<-10
  
  # ------------------ LEITURA das COORDENADAS dos PONTOS
  # ....... Faz a Leitura de um Total de Pontos = (N_pontos + N_veiculos)
  Pontos_Coord <- read.csv2("Data/110pontos.csv",header=TRUE,sep=";")
  
} else { # Caso FALSE, 10 pontos e 3 veicul
  
  vetor<- c(1:13)
  N_pontos<-10
  N_veiculos<-3
  Pontos_Coord <- read.csv2("Data/13pontos.csv",header=TRUE,sep=";")
  
}

N_vetor<-length(vetor)

# ------------ Vetor
vetor
# ------------ Numero de Pontos
N_pontos
# ------------ Numero de Veiculos
N_veiculos
# ------------ Tamanho do vetor (Pontos + Veiculos)
N_vetor

# ------------ Localização dos Veículos no Vetor
pos_veics_Original<- which(vetor > N_pontos)
pos_veics_Original

N_pos_veics<-length(pos_veics_Original)
N_pos_veics

#                  ********** ATENÇÃO **********
# **** AQUI QUE SE DÁ <CTRL> <ENTER> para uso da Modelagem I ******
f_Obj_Meta2<- function(x){
  
  # ########################### FUNÇÕES ###################################
  
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 01 - INÍCIO -----------------------
  # ***********************************************************************
  # BLOCO No. 01 - Localiza a Posição dos Veículos no Vetor Permutado
  # Recebe vetor "x" da metaheuristica 
  # Passa valores de "x" para o Vetor Permutado
  # .... ATENÇÃO: >>>>>>> Adotamos como "x" o "x Ordenado"
  # E passamos para a função esse vetor "ordenado" como sendo o Vetor Permutado
  # Esquecemos "x" original e trabalhamos apenas com o "novo" x
  # Com esta "jogada", sempre ter-se-á um vetor com valores de 1 a N_vetor
  
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
  #     ------------------- BLOCO No. 02 - INÍCIO ---------
  # ***********************************************************************
  # BLOCO No. 02 - Ordem dos Veículos no Vetor com suas Posições
  #                 Identifica a Ordem dos Veículos no Vetor
  pos_rota_ord<-order(pos_rota)
  pos_rota_ord
  
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 03 - INÍCIO -----------------------
  # ***********************************************************************
  
  # ---------BLOCO No. 03 - CRIAÇÃO de um VETOR de ROTAS -------------------
  # Transformar Vetor Permutado para uma Sequencia de Rotas
  # O vetor deve ter uma sequência do tipo: Rota 1, Rota 2, ..... Rota n
  # A Função puxa para a 1a posição o 1o Veículo que aparece no vetor
  # O vetor passa a iniciar com um Veículo
  # Puxa para as posições seguintes toda a sequência após o 1o Veículo
  # Depois disso:
  # Função transfere para o final, os primeiros elementos do vetor
  # que estavam nas posições do vetor anteriores ao primeiro veículo
  
  # ------ Montagem do novo Vetor de Rotas (Vide explicação acima) -----
  #               Pontos do início do vetor vão para o final
  
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
      if (vetor_rota[i] == pos_veics_Original[j])
        pos_vetor_rota[j] <- i  }
  }
  pos_vetor_rota
  
  # Ordena pos_vetor_rota
  pos_vetor_rota_ord<-order(pos_vetor_rota)
  pos_vetor_rota_ord
  
  # ***********************************************************************
  #     ------------------- BLOCOS No. 05 e 06 - INÍCIO -----------------------
  # ***********************************************************************
  # ----- CRIA Rotas na forma de GOTAS (Circulares)
  # ... Todas as rotas iniciam e terminam no Ponto de Origem 
  #   Todas formam um circuito...ficam no formato de uma "GOTA"
  #   Todas as rotas ficam em forma de Gota no próprio Vetor de Rotas
  # Basta acrescentar Ponto de Origem de cada Rota ao Final da Rota (forma a GOTA)
  # -----------------------------------------------------------------
  
  ## *********************** ATENÇÃO:
  # ==>> pos_rota e pos_rota_ord = Posições dos Veículos no Vetor Permutado
  # ==>> pos_vetor_rota e pos_vetor_rota_ord = Posições de Veícs no Vetor Circular
  
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
  
  # --- Identificação da posição de início de cada Rota  
  k<-0
  for (j in 1:N_veiculos)
  { 
    k[j]<-pos_vetor_rota[pos_vetor_rota_ord[j]]
  }
  k
  
  # --- Cria e Inicializa um Vetor Circular com Rotas em Gotas ( rotas Circulares)
  # ............   Vetor Circular Inicial = Vetor_Rota    ........
  vetor_rota_circ<-vetor_rota
  K_N_rota<-0   #... Acumulador para posições iniciais das rotas no Vetor Circular
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
  #     ------------------- BLOCO No. 07 - INÍCIO -----------------------
  # ***********************************************************************
  
  # BLOCO No. 07 - Identifica Início/Fim de cada Rota no Vetor de Rotas em Gotas
  #                 Limites das Rotas
  
  Limites_Rota<-matrix(ncol = 4, nrow = N_veiculos)
  Pontos_na_Rota <- 0
  for (i in 1:N_veiculos){
    Limites_Rota[i,1] <- N_pontos + i #  .....No. da Rota
    Limites_Rota[i,2] <- which(vetor_rota_circ == N_pontos+i)[1] # ...Início
    Limites_Rota[i,3] <- which(vetor_rota_circ == N_pontos+i)[2] # ...Fim
    Limites_Rota[i,4] <-  Limites_Rota[i,3] -  Limites_Rota[i,2]
  }
  Limites_Rota
  
  
  # ***********************************************************************
  #     ------------------- BLOCO No. 08 - INÍCIO -----------------------
  #         BLOCO No. 08 - CALCULA DISTÂNCIAS - "Função Objetivo"
  # ***********************************************************************
  
  # Função Preliminar: Cálculo de DisTãncia Euclidiana entre dois Pontos
  Dist <- function(XA, YA, XB, YB){
    D_AB <- sqrt((XA-XB)**2+(YA-YB)**2)
    return(D_AB)
  }
  
  # BLOCO No. 08 - CALCULA DISTÂNCIAS - "Função Objetivo"
  # Três saídas do Bloco No. 08:  . Vetor de Distâncias por Trecho por Rota
  #                               . Vetor de Distâncias Totais por Rota e
  #                               . Distância Global
  
  # .... Inicialização......
  # ... Dimensiona Matriz de Distâncias pela Rota com maior No. de Pontos
  Distances_Vetor <- matrix(nrow = N_veiculos, ncol = Maior_rota)
  # ... Inicializa Distância Total de cada Rota
  Distance_Rota <- 0 
  for (i in 1:N_veiculos) Distance_Rota[i] <- 0
  
  # .... CÁLCULOS de DISTÂNCIAS......chama Função <Dist() >
  for (i in 1:N_veiculos){
    for (j in 0:Limites_Rota[i,4]){
      Dist_AB <- Dist (Pontos_Coord$X[vetor_rota_circ[Limites_Rota[i,2]+j]], 
                       Pontos_Coord$Y[vetor_rota_circ[Limites_Rota[i,2]+j]],
                       Pontos_Coord$X[vetor_rota_circ[Limites_Rota[i,2]+j+1]],
                       Pontos_Coord$Y[vetor_rota_circ[Limites_Rota[i,2]+j+1]] ) 
      if (j < Limites_Rota[i,4]){
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
  write.table(round(Distances_Vetor, digits = 1), "./Output/Distancias_por_Trecho_de_Rota2.csv", sep = ";",
              col.names = !file.exists("./Output/Distancias_por_Trecho_de_Rota2.csv"), append = T)
  
  write.table(round(Distance_Rota, digits = 1), "./Output/Distancias_por_Rota2.csv", sep = ";",
              col.names = !file.exists("./Output/Distancias_por_Rota2.csv"), append = T)
  
  write.table(round(Distance_Global, digits = 2), "./Output/Distancia_Global2.csv", sep = ";",
              col.names = !file.exists("./Output/Distancia_Global2.csv"), append = T)
  
  write.table(Rotas_e_Dist, "./Output/Rotas_Circulares2.csv", sep = ";",
              col.names = !file.exists("./Output/Rotas_Circulares2.csv"), append = T)
  
  write.table(Limites_Rota, "./Output/Pontos_por_Rota2.csv", sep = ";",
              col.names = !file.exists("./Output/Pontos_por_Rota2.csv"), append = T)
  
  
  
  # ***********************************************************************
  #     ------------------- BLOCOS 1 a 8 - FIM -----------------------
  # ***********************************************************************
  
  # ***********************************************************************
  #     -------------RETORNO da FUNÇÃO OBJETIVO -----------------------
  # ***********************************************************************
  # return( list(Posições_de_Veiculos_no_Vetor = pos_rota,
  #              Ordem_dos_Veiculos_no_Vetor = pos_rota_ord,
  #              Vetor_com_as_Rotas_em_Sequencia = vetor_rota,
  #              o_Pontos_por_Rota = N_rota,
  #              Vetor_de_Rotas_Circulares_GOTAS = vetor_rota_circ,
  #              Posições_de_Início_Fim_das_Rotas = Limites_Rota,
  #              No._de_Pontos_por_Rota = Limites_Rota[,4],
  #              Vetores_de_Distâncias_por_Rota = Distances_Vetor,
  #              Distâncias_Totais_por_Rota = Distance_Rota,
  #              Distância_GLOBAL_da_OPERAÇÃO = Distance_Global))
  #   
  
  # .... Para a META retorna só a F. Obj.
  return(Distance_Global)
  
}  # ..... Chave da FUNÇÃO

# ***********************************************************************
# ------------APLICAÇÃO da FUNÇÃO ABC para MODELAGEM TIPO I -------------
# ***********************************************************************

# FUNÇÃO ABC() ===>>>> para F.OBJ_META2
#   ***** ATENÇÃO: ANTES DE RODAR ESTA MH, RODA-SE A F.OBJ_META2 *******

# install.packages(metaheuristicOpt)
library(metaheuristicOpt)
## INICIALIZAÇÃO: Definição de Parâmetros da ABC (numVar e rangeVar)
numVar <- N_vetor
numVar

rangeVar<- matrix(nrow= 2, ncol=N_vetor)
for (i in 1:2){
  for (j in 1:N_vetor){
    rangeVar[1,j] <- as.integer(1)
    rangeVar[2,j] <- as.integer(numVar)
  }}
rangeVar 

## CÁLCULO da SOLUÇÃO ÓTIMA por ABC - Artificial Bee Colony algorithm
Solucao_ABC <- ABC(f_Obj_Meta2, optimType="MIN", numVar, numPopulation=N_populacao,
                   maxIter=N_iteracoes, rangeVar)
Solucao_ABC   # ... Vetor de números REAIS (cada posição entre 1 e N_vetor)

Rotas_Solucao.Otima_ajustada <- order(Solucao_ABC)
Rotas_Solucao.Otima_ajustada  # ... Vetor de números INTEIROS entre 1 e N_vetor

## RESULTADOS da SOLUÇÃO ÓTIMA usando a Função META2
Solucao.Otima <- f_Obj_Meta2(Solucao_ABC)
round(Solucao.Otima, digits =2)
#...... VIDE RESULTADOS nos ARQS .CSV GRAVADOS .......
# -----------------------------------------------------------------------
