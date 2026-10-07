#MANTEL TEST
if (!require(ecodist)) {install.packages("ecodist")}
library(ecodist)

mat1 <- dist(matrix(rnorm(40), ncol = 2))  # e.g., distance in evaulating the leadership
mat2 <- dist(matrix(rnorm(40), ncol = 2))  # e.g., shortest path distance in workplace emailing 

mantel_result <- mantel(mat1 ~ mat2, nperm = 999)
print(mantel_result)


#MRQAP 
if (!require(asnipe)) install.packages("asnipe")
library(asnipe)

m1 <- rgraph(10, m = 1, tprob = 0.5, mode = "graph")  # friendship network
m2 <- rgraph(10, m = 1, tprob = 0.5, mode = "graph")  # organizational network 
m3 <- rgraph(10, m = 1, tprob = 0.5, mode = "graph")  # political dyadic similarity network

model_asnipe <- mrqap.dsp(m1 ~ m2 + m3, directed = "undirected")
print(model_asnipe)

