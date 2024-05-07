##SOURCE:: https://statnet.org/workshop-ergm/ergm_tutorial.html

## ----install-ergm,eval=FALSE--------------------------------------------------
## install.packages('ergm')


## ----loadPackage--------------------------------------------------------------
library(ergm)


## ----version------------------------------------------------------------------
packageVersion("ergm")


## ----binary-terms-search---------------------------------------------------
search.ergmTerms(keyword='binary')

## KEY HYPERLINK :: https://search.r-project.org/CRAN/refmans/ergm/html/ergmTerm.html


## ----network-help-------------------------------------------------------------
?network


## ----ergm-datasets------------------------------------------------------------
data(package='ergm') # tells us the datasets in our packages


## ----florentine-plots---------------------------------------------------------
set.seed(1)  #to have similar results/plots

data(florentine) # loads flomarriage and flobusiness data

print(flomarriage)

pdf("ergm1_2_plots.pdf")
par(mfrow=c(1,2)) # Set up a 2-column (and 1-row) plot area
plot(flomarriage, 
     main="Florentine Marriage", 
     cex.main=0.8, 
     label = network.vertex.names(flomarriage)) # Equivalent to plot.network(...)


wealth <- flomarriage %v% 'wealth' # %v% references vertex attributes
wealth


plot(flomarriage, 
     vertex.cex=wealth/25, # Make vertex size proportional to wealth attribute
     main="Florentine marriage by wealth", cex.main=0.8) 
dev.off()



## ----flomarriage-edges--------------------------------------------------------
summary(flomarriage ~ edges) # Calculate the edges statistic for this network
flomodel.01 <- ergm(flomarriage ~ edges) # Estimate the model 
summary(flomodel.01) # Look at the fitted model object


## ----flomarriage-triangle, message = FALSE------------------------------------
set.seed(1)
summary(flomarriage~edges+triangle) # Look at the g(y) statistics for this model
flomodel.02 <- ergm(flomarriage~edges+triangle) # Estimate the theta coefficients
summary(flomodel.02)


## ----ilogit-------------------------------------------------------------------
plogis(coef(flomodel.02)[[1]] + (0:2) * coef(flomodel.02)[[2]])


## ----flomarriage-ergm-object--------------------------------------------------
class(flomodel.02) # this has the class ergm
names(flomodel.02) # the ERGM object contains lots of components.


## ----ergm-object-coef---------------------------------------------------------
coef(flomodel.02) # you can extract/inspect individual components


## ----flomarriage-wealth-------------------------------------------------------
summary(wealth) # summarize the distribution of wealth
# plot(flomarriage, 
#      vertex.cex=wealth/25, 
#      main="Florentine marriage by wealth", 
#      cex.main=0.8) # network plot with vertex size proportional to wealth
summary(flomarriage~edges+nodecov('wealth')) # observed statistics for the model
flomodel.03 <- ergm(flomarriage ~ edges 
                    + nodecov('wealth') 
                    + degree(1)
                    )
summary(flomodel.03)

pdf("mcmc_flo.pdf")
tmp_mcmc <- mcmc.diagnostics(flomodel.03)
dev.off()


pdf("gof_glo.pdf")
flomodel.03.gof <- gof(flomodel.03)
plot(flomodel.03.gof)
dev.off()
