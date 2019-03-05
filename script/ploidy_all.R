# install R packages, and set working directoy
install.packages("ggplot2")
library(ggplot2)
install.packages("multcomp")

setwd("/Users/Wen-Juan/Dropbox (Amherst College)/my_postdoc/plant/FC_results")

fc<-read.table("/Users/Wen-Juan/Dropbox (Amherst College)/my_postdoc/plant/FC_results/results/092016/all_pop_05112016.csv",header=T, sep=",")
str(fc)

pdf("/Users/Wen-Juan/Dropbox (Amherst College)/my_postdoc/postdoc_manuscripts/WMa_publication/peduncle_mercurialis/resubmit/main_figures/Genomesize_correct.pdf", width=8, height=8)
fc$species <- factor(fc$species)
fc$species <- reorder(fc$species, fc$order)
ggplot(fc, aes(x=fc$species, y=C.values,group=species)) +
geom_dotplot(binaxis='y', stackdir='center',stackratio=1, dotsize=1,binwidth = 0.06) +
  theme(axis.text.x = element_text(colour="black",size=12), axis.title.x = element_text(size=12,colour = "black")) +
  theme(axis.text.y = element_text(colour="black",size=12), axis.title.y = element_text(size=12,colour = "black")) +
  ylim(0.5,2.5) +
  labs(x="", y = "Genome size in C values(pg)")
dev.off()

## test whether the genome size is significantly differnet from each other. 
genome_size<-read.table("/Users/Wen-Juan/Dropbox (Amherst College)/my_postdoc/plant/FC_results/results/092016/all_individuals_05112016.txt",header=T)
genome_size
head(genome_size)
str(genome_size)

#check data distribution
qqnorm(test_morph_0511$C_value)
shapiro.test(test_morph_0511$C_value) ##data is not normally distributed. 

model0<-glm(log(C_value)~morph+Pop-1,family=quasibinomial,data=genome_size)
summary(model0)

#it shows genome_size$Pop does not have significant influence. 

model1<-glm(log(C_value)~morph-1,family=quasibinomial,data=genome_size)
summary(model1)

anova(model0,model1,test="F")
#there is no significant difference between the two glm models. 

library(multcomp)
myTukey <- glht(model1, linfct = mcp(morph = "Tukey"),data=genome_size)
summary(myTukey)



