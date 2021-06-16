##
## Atividade 01 - Implementar qualquer algoritmo usando IF e loops.
## aluno: Micael Levi L. C. - 21554923
## versão: 16-06-2021
## Problema escolhido: https://www.urionlinejudge.com.br/repository/UOJ_1076.html
##

# Usa o algoritmo Depth-First Search (DFS) para visitar o grafo, retornando
# a quantidade de passos feitos
dfs.count <- function (adj_list, visted_nodes, node) {
  total = 0
  if (! (node %in% visted_nodes) ) visted_nodes <- append(visted_nodes, node)
  adjs = adj_list[[node]]
  for (adj in adjs) {
    if (! (adj %in% visted_nodes))
      total = total + dfs.count(adj_list, visted_nodes, adj) + 1
  }
  return( total )
}

# Retorna quantidade de movimentos de caneta que devem ser feitos para desenhar o labirinto
# considerando que o início e o fim são sempre a partir do mesmo ponto (nodo)
# e que não é possível levantar a caneta do papel.
count_moviments_for <- function (graph_data, start_node) {
  return( dfs.count(graph_data, list(), start_node) * 2 )
}


## Uma lista de adjacências AxV para representar o labirinto da figura B
## Para entender melhor como funciona esse tipo de representação, leia: https://en.wikipedia.org/wiki/Adjacency_list
labirinto = list(
  "0"= c(),
  "1"= c("2", "4"),
  "2"= c("1"),
  "3"= c("4"),
  "4"= c("1", "3", "7"),
  "5"= c(),
  "6"= c(),
  "7"= c("4", "8"),
  "8"= c("7")
)

print( count_moviments_for(labirinto, "1") )
