#Author=ORCID:0000-0002-3324-2255
#Task=PhD Studies -- Networks and system stability -- DI7224_1M

#DATA PREPARATION
#DATA PREPARATION

#general environment.
rm(list = ls())
library("igraph")
library("ggplot2")

#key variables.
#sno = number of nodes of the simulated graph
#sno = 18-65 years old HU population without isolated nodes
sno  <- 5180220

#rddr = degree distribution of the sample
rddr <- c(116,130,109,85,111,61,41,54,11,161,13,15,4,3,13)


#smoothening of the degree distribution of the sample. 
rddsma5 <- NULL
rddsma5[1] <- rddr[1]

for (i in 2:14)
{
  rddsma5[i] <- round(rddr[i] - ((rddr[i] - mean(rddr[c(i-1,i,i+1)]))*0.5),0)
}

i<- 15
rddsma5[i] <- round(rddr[i] - ((rddr[i] - mean(rddr[c(i-1,i)]))*0.5),0) 

#plotting the original and smoothened distribution.
gdat <- test_data <-
  data.frame(
    survey = rddr,
    simitott = rddsma5,
    id = c(1:15)
  )

ggplot(gdat, aes(id)) + 
  geom_point(aes(y = survey, shapes = 15, size = 3,  colour = "survey"),  show.legend = F ,) + 
  geom_point(aes(y = simitott, shapes = 16, size = 3,  colour = "smoothened"),  show.legend = F,) +
  xlab("degree") + 
  ylab("n") +
  theme_minimal(base_size = 16) +
  geom_abline(intercept=130, slope=-8)

#deceision: we use the smoothened.
rdd <- rddsma5 

#red = number of degrees.
red <- (1*rdd[1])+(2*rdd[2])+(3*rdd[3])+(4*rdd[4])+(5*rdd[5])+(6*rdd[6])+(7*rdd[7])+(8*rdd[8])+(9*rdd[9])+(10*rdd[10])+(11*rdd[11])+(12*rdd[12])+(13*rdd[13])+(14*rdd[14])+(15*rdd[15])

#pdd = degree distribution in proportions / probability.
pdd <- rdd/sum(rdd) #fokszameloszlas aranyok
if (sum(pdd)!=1) {print("Error! Sum of the degree distribution probabilites is not ZERO")}

#dd = simulated graph degree distribution of the nodes, number of degrees.
dd <- sno*pdd 
dd2 <- round(dd,0) 

#outdeg = outdegree distribution of the simulated graph.
outdeg <- c(rep(1,dd2[1]),rep(2,dd2[2]),rep(3,dd2[3]),rep(4,dd2[4]),rep(5,dd2[5]),rep(6,dd2[6]),rep(7,dd2[7]),rep(8,dd2[8]),rep(9,dd2[9]),rep(10,dd2[10]),rep(11,dd2[11]),rep(12,dd2[12]),rep(13,dd2[13]),rep(14,dd2[14]),rep(15,dd2[15]))
if (sum(outdeg) %% 2 != 0) { outdeg[1] <- outdeg[1] + 1 }

#sed = number of degrees of the simulated graph.
sed <- sum(outdeg) 

#checking visually the degree distribution of the simulated graph.
hist(outdeg, freq = FALSE)

#GRAPH SIMULATION
#GRAPH SIMULATION
vgraf <- sample_degseq( out.deg = outdeg, method = 'simple.no.multiple' )

#checking vgraf
sum(which_loop(vgraf))
sum(which_multiple(vgraf))

#creating a copy before transforming
vgraf2 <- vgraf 

#RESULTS
#RESULTS
object.size(vgraf2)
ec1<-ecount(vgraf2)
vc1<-vcount(vgraf2)
co1<-components(vgraf2)$no
ed1<-edge_density(vgraf2, loops = FALSE)

#analyzing the components.
co1
table(co1$csize)
plot(table(co1$csize), log="x")

#these take a lot of time, better to estimate
di1<-diameter(vgraf2, directed = FALSE)
mo1<-modularity(cluster_fast_greedy(as.undirected(vgraf2)))
bc1<-betweenness(vgraf2, directed=FALSE)
cc1<-closeness(vgraf2, normalized=TRUE) 
