##
## aluno: Micael Levi L. C. - 21554923
## versão: 05-07-2021
##

################################################################################
## 1
################################################################################

q1 <- function() {
  notas=rep(NA, times = 4)
  i = 1
  while (i <= 4) {
    prompt_msg = paste("Informe sua ", i, "a", " nota:", " ", sep="")
    nota_lida = readline(prompt_msg)
    notas[i] = as.numeric(nota_lida)

    i = i + 1
  }
  media_notas = mean(notas)

  sexo = readline("Informe seu sexo (M ou F): ")
  sexo = toupper(substr(sexo, 1, 1))

  loc = data.frame(
    "M"=c("Caro aluno,", "aprovado!!", "reprovado"),
    "F"=c("Cara aluna,", "aprovada!!", "reprovada")
  )

  aprovado = ifelse(media_notas >= 5, 2, 3)

  msg = paste(loc[1, sexo], "você está", loc[aprovado, sexo])
  cat(msg)
}

################################################################################
## 2
################################################################################

q2 <- function () {
  altura = as.numeric( readline("Altura (m): ") )
  peso_atual = as.numeric( readline("Peso (kg): ") )
  idade = as.numeric( readline("Idade (anos): ") )
  sexo = substr( readline("Sexo [M ou F]: "), 1, 1 )

  imc_desejado = ifelse(sexo == "M", 22, 21)
  peso_ideal = imc_desejado * (altura^2)
  cat("Peso Ideal (PI) = ", peso_ideal, "\n")

  adequacao_peso = (peso_atual / peso_ideal) * 100
  estado_nutricional = "[não definido]"

  if (adequacao_peso <= 70) {
    estado_nutricional = "Desnutrição Grave"
  } else if (adequacao_peso <= 80) {
    estado_nutricional = "Desnutrição Moderada"
  } else if (adequacao_peso <= 90) {
    estado_nutricional = "Desnutrição Leve"
  } else if (adequacao_peso <= 110) {
    estado_nutricional = "Eutrofia (peso ideal)"
  } else if (adequacao_peso <= 120) {
    estado_nutricional = "Sobrepeso"
  } else {
    estado_nutricional = "Obesidade"
  }
  cat("Estado nutricional = ", estado_nutricional, "\n")
}

################################################################################
## 3
################################################################################

q3 <- function (vet) {
  vet_ordenado = rep(NA, 3)
  a = vet[1]
  b = vet[2]
  c = vet[3]

  if (a < b) {
    if (a < c) {
      vet_ordenado[1] = a
      if (b < c) {
        vet_ordenado[2] = b
        vet_ordenado[3] = c
      } else {
        vet_ordenado[2] = c
        vet_ordenado[3] = b
      }
    } else {
      vet_ordenado[1] = c
      vet_ordenado[2] = a
      vet_ordenado[3] = b
    }
  } else {
    if (b < c) {
      vet_ordenado[1] = b
      if (a < c) {
        vet_ordenado[2] = a
        vet_ordenado[3] = c
      } else {
        vet_ordenado[2] = c
        vet_ordenado[3] = a
      }
    }
  }

  return(vet_ordenado)
}

################################################################################
## 4
################################################################################

q4 <- function () {
  for (numero in 100:200) {
    if (numero %% 2 != 0) print(numero)
  }

  ## Versão vetorial
  # numeros = 100:200
  # impares = numeros[numeros %% 2 != 0]
  # return(impares)
}

################################################################################
## 5
################################################################################

q5 <- function () {
  x = as.numeric( readline("x: ") )
  n = as.numeric( readline("n: ") )

  h_x_n = 0
  for (i in seq(0, n)) {
    h_x_n = h_x_n + `^`(x, i)
  }
  return(h_x_n)
}

################################################################################
## 6
################################################################################

q6 <- function () {
  ranges = c(
    "[0;25]",
    "(25;50]",
    "(50;75]",
    "(75;100]"
  )

  contadores = c(0, 0, 0, 0)

  floor = 0
  ceil = 100
  cat("Informe os números no intervalo [0; 100], e fora desse intervalo quando quiser parar:")
  repeat {
    x = as.numeric( readline() )
    if (x < floor || x > ceil) break

    idx_contador = NA
    if (x <= 25) idx_contador = 1
    else if (x <= 50) idx_contador = 2
    else if (x <= 75) idx_contador = 3
    else idx_contador = 4

    contadores[idx_contador] = contadores[idx_contador] + 1
  }

  idx_contador = 1
  for (range in ranges) {
    qtd_lidos = contadores[idx_contador]
    cat("No intervalo", range, "há", qtd_lidos, "valores", "\n")
    idx_contador = idx_contador + 1
  }
}

################################################################################
## 7
################################################################################

q7 <- function () {
  a1 = 1.5 # m
  tx1 = 1 # cm
  cm_crescidos1 = 0 # cm

  a2 = 1.2 # m
  tx2 = 2 # cm
  cm_crescidos2 = 0 # cm

  anos = 0
  repeat {
    anos = anos + 1

    cm_crescidos1 = anos * tx1
    cm_crescidos2 = anos * tx2

    aa1 = (a1 * 100) + cm_crescidos1
    aa2 = (a2 * 100) + cm_crescidos2

    if (aa2 > aa1) break
  }

  cat("João passará de Paulo após", anos, "anos", "\n")
  cat("Paulo crescerá", cm_crescidos1, "centímetros", "\n")
  cat("João crescerá", cm_crescidos2, "centímentros", "\n")
}

################################################################################
## 8
################################################################################

q8 <- function () {
  pesos = c(1, 2)

  qtd = 0
  repeat {
    if (qtd > 10) break
    qtd = qtd + 1

    nome = readline("Seu nome: ")
    notas = c(
      as.numeric(readline("Nota A:")),
      as.numeric(readline("Nota B:"))
    )
    media_ponderada = round(weighted.mean(notas, pesos), 2)
    cat("A média ponderada de", nome, "é", media_ponderada)
  }
}
