## Glasso, Correaltion, Multi-dimensional scaling, Prinicipal Component and Eigenmodel Networks modelling on 16 item MCQ IQ & 25 item Big five inventory

The following project contains five major types of network estimation used psychological network analysis (Jones, Mair & McNally, 2018), all the networks 
are estimated using various R-packages and visualizations have been performed using q-graph R-package (Epskmap et al., 2012). Both the datasets have been 
used from "psychtools" R-package (Revelle & Revelle, 2021). 

## Demo code

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

![Screenshot 2022-02-16 202116](https://user-images.githubusercontent.com/96023170/154291609-5c1c265f-a581-4e08-80c6-954f6671f1f1.png)


pdf("P1_IQ_Centrality.pdf", height = 12, width = 16, paper = "USr")
P1_IQ_Centrality <- centralityPlot(P1_IQ, scale = c("raw"), include = "all", orderBy = "default")
dev.off() 

![Screenshot 2022-02-16 202649](https://user-images.githubusercontent.com/96023170/154291647-7802d462-ab0b-4dcc-81b8-711b26878e8a.png)


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

![Screenshot 2022-02-16 202710](https://user-images.githubusercontent.com/96023170/154291675-45af4b50-4f19-4d9f-8bb3-d64115e42981.png)


pdf("P1_BFI_Centrality.pdf", height = 12, width = 16, paper = "USr")
P1_BFI_Centrality <- centralityPlot(P1_BFI, scale = c("raw"), include = "all")
dev.off()

![Screenshot 2022-02-16 202727](https://user-images.githubusercontent.com/96023170/154291695-e1a677ad-6b17-43db-a565-7ff6961a65e4.png)


#----Glasso-Regularized-Correlation-Matrix-Visualization-IQ----# 
pdf("P2_IQ_glasso.pdf", height = 12, width = 16, paper = "USr")
P2_IQ <- qgraph(IQCOR, layout = "spring", sampleSize = nrow(iqitems),
                graph = "glasso", groups = list(Reasoning = 1:4, 
                                             Letter = 5:8, Matrix = 9:12, Rotation = 13:16), 
                color = c("#BFFFF0", "#F0FFC2", "#FFE4C0", "#FFBBBB"), 
                lables = colnames(iqitems), title = "16 item, 4 factor IQ test glasso network", 
                theme = "Hollywood")
dev.off()
![Screenshot 2022-02-16 202940](https://user-images.githubusercontent.com/96023170/154292303-2dfec6fd-daec-4294-8452-d1e2d3a2afa3.png)



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
![Screenshot 2022-02-16 202958](https://user-images.githubusercontent.com/96023170/154292322-df069606-0240-4810-96ec-2cd0b0f0255f.png)



#--------------LATENT-NETWORK-MODELS----------------------# 
#----IQ-Test-Latent-Test----# 
library(networktools)
pdf("P1_IQ_Eigenmodel.pdf", height = 12, width = 16, paper = "USr")
P1_IQ_EigenModel <- EIGENnet(P1_IQ, S = 10000, burn = 500, 
                             seed = 12, repulse = T, repulsion = 1)
dev.off()
![Screenshot 2022-02-16 203022](https://user-images.githubusercontent.com/96023170/154292357-26ad36ee-a6d1-4639-8d60-f9f3084db750.png)

pdf("P1_IQ_PCA.pdf", height = 12, width = 16, paper = "USr")
P1_IQ_PCA <- PCAnet(P1_IQ, cormat = IQCOR, repulse = T)
dev.off()
![Screenshot 2022-02-16 203045](https://user-images.githubusercontent.com/96023170/154292379-6dd34bca-24d6-49d6-922d-b8e14770f637.png)

pdf("P1_IQ_MDS.pdf", height = 12, width = 16, paper = "USr")
P1_IQ_MDS <- MDSnet(P1_IQ, MDSadj = IQCOR)
dev.off()
![Screenshot 2022-02-16 203102](https://user-images.githubusercontent.com/96023170/154292392-93e819ad-3b2c-4eef-9f87-49fe3b2999e5.png)

#----Big-five-latent-test----# 
pdf("P2_BFI_Eigenmodel.pdf", height = 12, width = 16, paper = "USr")
P2_BFI_EigenModel <- EIGENnet(P2_BFI, S = 10000, burn = 500, 
                              seed = 12, repulse = T, repulsion = 1)
dev.off()
![Screenshot 2022-02-16 203841](https://user-images.githubusercontent.com/96023170/154294178-dd8f4486-6663-4734-b091-8818f03f90bc.png)

pdf("P2_BFI_PCA.pdf", height = 12, width = 16, paper = "USr")
P2_BFI_PCA <- PCAnet(P2_BFI, cormat = BFICOR, repulse = T)
dev.off()
![Screenshot 2022-02-16 203902](https://user-images.githubusercontent.com/96023170/154294200-74dca41b-42e9-48d8-950b-c317aa5cb849.png)

pdf("P2_BFI_MDS.pdf", height = 12, width = 16, paper = "USr")
P2_BFI_MDS <- MDSnet(P2_BFI, MDSadj = BFICOR)
dev.off()
![Screenshot 2022-02-16 203919](https://user-images.githubusercontent.com/96023170/154294230-21c45b5d-6284-45ed-9c4e-b9b03a1a28b2.png)

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
![Screenshot 2022-02-16 203938](https://user-images.githubusercontent.com/96023170/154294256-ac026c20-9428-48f2-9806-341942501bb0.png)

#------BFI_PCA------# 
PCA_DAT_BFI <- na.omit(bfireverse[,c(1:25)])
pc_BFI <- prcomp(PCA_DAT_BFI, 
                 scale = T, center = T)
summary(pc_BFI)

P2_PCA <- ggbiplot(pc_BFI, obs.scale = 1, var.scale = 1)
pdf("P2_PCA.pdf", height =12, width = 16, paper = "USr")
P2_PCA
dev.off()
![Screenshot 2022-02-16 203956](https://user-images.githubusercontent.com/96023170/154294282-5f2b7305-c2a4-42a3-b4a9-e386adaeafbb.png)

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
![Screenshot 2022-02-16 204013](https://user-images.githubusercontent.com/96023170/154294295-ef47d750-cc5f-49f5-9269-1d9662b88163.png)

#i.e. no difference between networks observed 


## References

Jones, P. J., Mair, P., & McNally, R. J. (2018). Visualizing psychological networks: A tutorial in R. Frontiers in Psychology, 1742.

Epskamp, S., Cramer, A. O., Waldorp, L. J., Schmittmann, V. D., & Borsboom, D. (2012). qgraph: Network visualizations of relationships in psychometric data. Journal of statistical software, 48, 1-18.

Revelle, W., & Revelle, M. W. (2021). Package ‘psychTools’.
