# PO 230
# Modelo do Mercado do Zé
# Script em R 

# Função que simula um dia de funcionamento

simula.funcionamento <- function(t.abre = 9, 
                           t.fecha = 22, 
                          lambda.chegada = 1/10){
  ans <- list()
  
  ## DEFINICOES DE VARIAVEIS:
  # clinica
  t.entra  <- ( t.fecha - t.abre ) * 3600  # duração do expediente na clínica (em segundos)
  
  # pacientes
  lambda.chegada <- lambda.chegada / 60  # taxa de chegadas por segundo 
  x <- t.entra * lambda.chegada * 5      # numero de pacientes que chegam ate a clinica
  # (valor superestimado => fator 5)
  
  # gera tempos de chegada dos pacientes
  t.chegada <- cumsum(rexp(x, lambda.chegada))  
  
  # seleciona pacientes que serao admitidos na clinica
  # serão atendidos apenas aqueles que chegarem antes de `t.entra`:
  pac.entra    <- which( t.chegada < t.entra ) # pacientes admitidos na clinica
  t.chegada    <- t.chegada[pac.entra]         # tempo de chegada dos pacientes admitidos
  n.entra      <- length(pac.entra)            # no. de pacientes admitidos na clinica
  
  ## INICIALIZACAO
  ans <- data.frame(t.chegada = t.chegada)
  
  fat_total = sum(rnorm(n.entra, mean = 40, sd = 10))
  
  return(fat_total)
}

simula.funcionamento()

# TODO:
# Aprimorar as escolhas de distribuição de chegada e de gasto com os dados

# Modelagem da Chegada
## Uso de distribuição teórico
## Com pandemia:
### - baseado nos dados de RO
### - compravadas com Google Mob
## Sem pandemia:
### - ajuste com Google Mob

# Modelagem do Gasto Total Médio
## Uso do distribuição fictícia
### - Dist Normal (u = 80, var = 40)

# Faturamento 
## Chegada x Gasto Total Médio por Dia
### Comparação antes e depois da pandemia

# Obs: Não há diferença entre gastos para fds e semana