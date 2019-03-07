library(ggplot2)
library(plyr)
install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)

datapath <- '~/Wen-Juan/peduncle_mercurialis/final_figures/'
outpath <- '~/Wen-Juan/peduncle_mercurialis/final_figures/'

anthers <- read.table("~/Wen-Juan/peduncle_mercurialis/final_figures/hexaploid_comparison.txt", header = T)
str(anthers)

pdf(file="~/Wen-Juan/peduncle_mercurialis/final_figures/hexaploid_anther.pdf")
ggplot(anthers, aes(x=P_G, y=Anther, fill=Gender)) + 
  scale_fill_manual(values = c("grey","firebrick2","blue")) +
  theme(legend.position="none") +
  geom_boxplot() +
  labs(x='Population and Gender', y='Number of anther') +
  scale_x_discrete(labels = c("F1_1","F1_2","A2_H","A2_M","A20_H","A20_M","A29_H","A29_M")) +
  theme(axis.text=element_text(size=12, color="black"),text = element_text(size=15,color="black"))
dev.off()

pdf(file="~/Wen-Juan/peduncle_mercurialis/final_figures/hexaploid_pollen.pdf")
ggplot(anthers, aes(x=P_G, y=Pollen, fill=Gender)) + 
  scale_fill_manual(values = c("grey","firebrick2","blue")) +
  theme(legend.position="none") +
  geom_boxplot() +
  labs(x='Population and Gender', y='Number of pollen') +
  scale_x_discrete(labels = c("F1_1","F1_2","A2_H","A2_M","A20_H","A20_M","A29_H","A29_M")) +
  theme(axis.text=element_text(size=12, color="black"),text = element_text(size=15,color="black"))
dev.off()

pdf(file="~/Wen-Juan/peduncle_mercurialis/final_figures/hexaploid_pollen_avsize.pdf")
ggplot(anthers, aes(x=P_G, y=AveSize, fill=Gender)) + 
  scale_fill_manual(values = c("grey","firebrick2","blue")) +
  theme(legend.position="none") +
  geom_boxplot() +
  labs(x='Population and Gender', y='Pollen average size') +
  scale_x_discrete(labels = c("F1_1","F1_2","A2_H","A2_M","A20_H","A20_M","A29_H","A29_M")) +
  theme(axis.text=element_text(size=12, color="black"),text = element_text(size=15,color="black"))
dev.off()

###################################
anther_H <-subset(anthers,anthers$Gender!='M')

pdf(file="~/Wen-Juan/peduncle_mercurialis/final_figures/hexaploidH_anther.pdf")
ggplot(anther_H, aes(x=Population, y=Anther, fill=Gender)) + 
  scale_fill_manual(values = c("grey","firebrick2")) +
  theme(legend.position="none") +
  geom_boxplot() +
  labs(x='Population and Gender', y='Number of anther') +
  scale_x_discrete(labels = c("F1_1","F1_2","A2_H","A20_H","A29_H")) +
  theme(axis.text=element_text(size=12, color="black"),text = element_text(size=15,color="black"))
dev.off()

y<- glm(Anther~Population,data=anther_H,family=quasipoisson)
anova(y)
summary(y)

################
anther_H <-subset(anthers,anthers$Gender!='M')

pdf(file="~/Wen-Juan/peduncle_mercurialis/final_figures/hexaploidH_pollen.pdf")
ggplot(anther_H, aes(x=Population, y=Pollen, fill=Gender)) + 
  scale_fill_manual(values = c("grey","firebrick2")) +
  theme(legend.position="none") +
  geom_boxplot() +
  labs(x='Population and Gender', y='Number of pollen') +
  scale_x_discrete(labels = c("F1_1","F1_2","A2_H","A20_H","A29_H")) +
  theme(axis.text=element_text(size=12, color="black"),text = element_text(size=15,color="black"))
dev.off()

y1<- glm(Pollen~Population,data=anther_H,family=quasipoisson)
anova(y1)
summary(y1)

################
anther_H <-subset(anthers,anthers$Gender!='M')

pdf(file="~/Wen-Juan/peduncle_mercurialis/final_figures/hexaploidH_avesize.pdf")
ggplot(anther_H, aes(x=Population, y=AveSize, fill=Gender)) + 
  scale_fill_manual(values = c("grey","firebrick2")) +
  theme(legend.position="none") +
  geom_boxplot() +
  labs(x='Population and Gender', y='Average pollen size (um)') +
  scale_x_discrete(labels = c("F1_1","F1_2","A2_H","A20_H","A29_H")) +
  theme(axis.text=element_text(size=12, color="black"),text = element_text(size=15,color="black"))
dev.off()

y2<- glm(AveSize~Population,data=anther_H,family=quasipoisson)
summary(y2)

###############
###############
anther_M <-subset(anthers,anthers$Gender!='H')

pdf(file="~/Wen-Juan/peduncle_mercurialis/final_figures/hexaploidM_anther.pdf")
ggplot(anther_M, aes(x=Population, y=Anther, fill=Gender)) + 
  scale_fill_manual(values = c("grey","blue")) +
  theme(legend.position="none") +
  geom_boxplot() +
  labs(x='Population and Gender', y='Number of anther') +
  scale_x_discrete(labels = c("F1_1","F1_2","A2_H","A20_H","A29_H")) +
  theme(axis.text=element_text(size=12, color="black"),text = element_text(size=15,color="black"))
dev.off()

y<- glm(Anther~Population,data=anther_M,family=quasipoisson)
anova(y)
summary(y)

#########
################
anther_H <-subset(anthers,anthers$Gender!='M')

pdf(file="~/Wen-Juan/peduncle_mercurialis/final_figures/hexaploidM_pollen.pdf")
ggplot(anther_M, aes(x=Population, y=Pollen, fill=Gender)) + 
  scale_fill_manual(values = c("grey","blue")) +
  theme(legend.position="none") +
  geom_boxplot() +
  labs(x='Population and Gender', y='Number of pollen') +
  scale_x_discrete(labels = c("F1_1","F1_2","A2_H","A20_H","A29_H")) +
  theme(axis.text=element_text(size=12, color="black"),text = element_text(size=15,color="black"))
dev.off()

y1<- glm(Pollen~Population,data=anther_M,family=quasipoisson)
anova(y1)
summary(y1)

################
anther_H <-subset(anthers,anthers$Gender!='M')

pdf(file="~/Wen-Juan/peduncle_mercurialis/final_figures/hexaploidM_avesize.pdf")
ggplot(anther_M, aes(x=Population, y=AveSize, fill=Gender)) + 
  scale_fill_manual(values = c("grey","blue")) +
  theme(legend.position="none") +
  geom_boxplot() +
  labs(x='Population and Gender', y='Average pollen size (um)') +
  scale_x_discrete(labels = c("F1_1","F1_2","A2_H","A20_H","A29_H")) +
  theme(axis.text=element_text(size=12, color="black"),text = element_text(size=15,color="black"))
dev.off()

y2<- glm(AveSize~Population,data=anther_M,family=quasipoisson)
summary(y2)

