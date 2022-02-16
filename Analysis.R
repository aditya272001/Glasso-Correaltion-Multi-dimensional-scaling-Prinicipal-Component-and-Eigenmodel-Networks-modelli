#------Packages----------# 
library(bootnet)
library(qgraph)
library(psychTools)
library(tidyverse)
library(psych)
library(ggbiplot)
#-------Dataset----------#
data("iqitems")
na.omit(iqitems)

data("bfi")
#------Correlation-Matrix-Visualization-IQ-----------# 
IQCOR <- cor(iqitems, method = "pearson", 
             use = "complete.obs")
setwd("D:/Network analysis on Big five and IQ")
pdf("P1_IQ_correaltion.pdf", height = 12, width = 16, paper = "USr")
P1_IQ <- qgraph(IQCOR, layout = "spring", minimum = 0.1, 
             graph = "cor", groups = list(Reasoning = 1:4, 
            Letter = 5:8, Matrix = 9:12, Rotation = 13:16), 
            color = c("#BFFFF0", "#F0FFC2", "#FFE4C0", "#FFBBBB"), 
            lables = colnames(iqitems), title = "16 item, 4 factor IQ test correlation network", 
            theme = "Hollywood")
dev.off()
pdf("P1_IQ_Centrality.pdf", height = 12, width = 16, paper = "USr")
P1_IQ_Centrality <- centralityPlot(P1_IQ, scale = c("raw"), include = "all", orderBy = "default")
dev.off()
P1_IQ_Centrality


#------Correlation-Matrix-Visualization-Big-five--------------# 
keysbfi <- unlist(bfi.keys)
bfireverse <- reverse.code(keysbfi, bfi)
colnames(bfireverse) <- c('A1', 'A2', 'A3', 'A4', 'A5', 'C1', 'C2', 'C3', 
                          'C4', 'C5', 'E1', 'E2', 'E3', 'E4', 'E5', 'N1', 
                          'N2', 'N3', 'N4', 'N5', 'O1', 'O2', 'O3', 'O4', 
                          'O5', 'Gender', 'education', 'age')

BFICOR <- cor(bfireverse[,c(1:25)], method = "pearson", use = "complete.obs")
pdf("P1_BFI_Correlation.pdf", height = 12, width = 16, paper = "USr")
P1_BFI <- qgraph(BFICOR, layout = "spring", minimum = 0.1, 
             graph = "cor", groups = list(Agreeablness = 1:5, 
                                          Conscientiousness = 6:10, Extraversion = 11:15, Neuroticism = 16:20, 
                                          Openness = 21:25), 
             color = c("#BFFFF0", "#F0FFC2", "#FFE4C0", "#FFBBBB", "#F1D00A"), 
             lables = colnames(iqitems), title = "25 item five factor Big test correlation network", 
             theme = "Hollywood")
dev.off()
pdf("P1_BFI_Centrality.pdf", height = 12, width = 16, paper = "USr")
P1_BFI_Centrality <- centralityPlot(P1_BFI, scale = c("raw"), include = "all")
dev.off()
#----Glasso-Regularized-Correlation-Matrix-Visualization-IQ----# 
pdf("P2_IQ_glasso.pdf", height = 12, width = 16, paper = "USr")
P2_IQ <- qgraph(IQCOR, layout = "spring", sampleSize = nrow(iqitems),
                graph = "glasso", groups = list(Reasoning = 1:4, 
                                             Letter = 5:8, Matrix = 9:12, Rotation = 13:16), 
                color = c("#BFFFF0", "#F0FFC2", "#FFE4C0", "#FFBBBB"), 
                lables = colnames(iqitems), title = "16 item, 4 factor IQ test glasso network", 
                theme = "Hollywood")
dev.off()
P2_IQ_Centrality <- centralityPlot(P2_IQ, scale = c("raw"), include = "all", orderBy = "default")
P2_IQ_Centrality

#-----Glasso-Regularized-Correlation-Matrix-Visualization-Big-five------# 
pdf("P2_BFI_glasso.pdf", height = 12, width = 16, paper = "USr")
P2_BFI <- qgraph(BFICOR, layout = "spring", sampleSize = nrow(bfireverse),
                 graph = "glasso", groups = list(Agreeablness = 1:5, 
                                              Conscientiousness = 6:10, Extraversion = 11:15, Neuroticism = 16:20, 
                                              Openness = 21:25), 
                 color = c("#BFFFF0", "#F0FFC2", "#FFE4C0", "#FFBBBB", "#F1D00A"), 
                 lables = colnames(iqitems), title = "25 item five factor Big test glasso network", 
                 theme = "Hollywood")
dev.off()
P2_BFI_Centrality <- centralityPlot(P1_BFI, scale = c("raw"), include = "all")

#--------------LATENT-NETWORK-MODELS----------------------# 
#----IQ-Test-Latent-Test----# 
library(networktools)
pdf("P1_IQ_Eigenmodel.pdf", height = 12, width = 16, paper = "USr")
P1_IQ_EigenModel <- EIGENnet(P1_IQ, S = 10000, burn = 500, 
                             seed = 12, repulse = T, repulsion = 1)
dev.off()
pdf("P1_IQ_PCA.pdf", height = 12, width = 16, paper = "USr")
P1_IQ_PCA <- PCAnet(P1_IQ, cormat = IQCOR, repulse = T)
dev.off()
pdf("P1_IQ_MDS.pdf", height = 12, width = 16, paper = "USr")
P1_IQ_MDS <- MDSnet(P1_IQ, MDSadj = IQCOR)
dev.off()

#----Big-five-latent-test----# 
pdf("P2_BFI_Eigenmodel.pdf", height = 12, width = 16, paper = "USr")
P2_BFI_EigenModel <- EIGENnet(P2_BFI, S = 10000, burn = 500, 
                              seed = 12, repulse = T, repulsion = 1)
dev.off()
pdf("P2_BFI_PCA.pdf", height = 12, width = 16, paper = "USr")
P2_BFI_PCA <- PCAnet(P2_BFI, cormat = BFICOR, repulse = T)
dev.off()
pdf("P2_BFI_MDS.pdf", height = 12, width = 16, paper = "USr")
P2_BFI_MDS <- MDSnet(P2_BFI, MDSadj = BFICOR)
dev.off()

#-----------Prinicipal-Component-Analysis----------------# 
#-----IQ_PCA------# 
#-Principal-Component-Analysis-# 
PCA_DAT_IQ <- na.omit(iqitems)
pc_IQ <- prcomp(PCA_DAT_IQ,
                scale = T, center = T)

attributes(pc_IQ)
pc_IQ$center
print(pc_IQ)

summary(pc_IQ)

P1_PCA <- ggbiplot(pc_IQ, obs.scale = 1, 
                   var.scale = 1)
pdf("P1_PCA.pdf", height = 12, width = 16, paper = "USr")
P1_PCA
dev.off()
#------BFI_PCA------# 
PCA_DAT_BFI <- na.omit(bfireverse[,c(1:25)])
pc_BFI <- prcomp(PCA_DAT_BFI, 
                 scale = T, center = T)
summary(pc_BFI)

P2_PCA <- ggbiplot(pc_BFI, obs.scale = 1, var.scale = 1)
pdf("P2_PCA.pdf", height =12, width = 16, paper = "USr")
P2_PCA
dev.off()
#-----Network-Tree-Comparison-between-Genders-BFI-----# 
install.packages("networktree", dependencies = T)
library(networktree)
bfireverse$gender[bfireverse$gender == 1] <- "Male"
bfireverse$gender[bfireverse$gender == 2] <- "Female"
bfireverse <- na.omit(bfireverse)
Gender_BFI_Plot <- networktree(nodevars = bfireverse[,c(1:25)], 
                               splitvars = bfireverse[,26], 
                               transform = "cor")
pdf("Gender_BFI_Comparison_networktree.pdf", height = 12, width = 16, paper = "USr")
plot(Gender_BFI_Plot)
dev.off()
#i.e. no difference between networks observed 