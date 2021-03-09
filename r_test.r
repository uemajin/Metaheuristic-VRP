rm(list =ls())
gc()

sFolder = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(sFolder))

mat_pontos <- read.csv2("Data/110pontos.csv",header=TRUE,sep=",")
vetor_rota_circ <- read.csv2("Output/Rotas_Circulares3.csv",header=TRUE,sep=';')

teste <- vetor_rota_circ[2,2]

peso_veiculo <- mat_pontos[vetor_rota_circ[1,2],4] # valor da capacidade do veiculo

soma_peso <- 0 # Acumulador



for (i in vetorRotaCircular){
}
numero_veiculo <- matPontos[vetorRotaCircular[1],4] # valor da capacidade do veiculo
soma_inicial <- matPontos[vetorRotaCircular[1],4] # valor da capacidade do veiculo

soma_peso = soma_peso + matPontos[i,4]

soma_peso = soma_peso - (soma_inicial*2) # Retira a capacidade do veiculo

if (soma_peso - soma_inicial > 0) {peso_final <- fator_pp * soma_peso}
else if (soma_peso - soma_inicial < 0) {peso_final <- fator_pn * soma_peso}
else {peso_final <- soma_peso}

return(soma_peso)