install.packages("lmerTest")
library(lme4)
library(lmerTest)

setwd("~/Dropbox (Amherst College)/my_postdoc/postdoc_manuscripts/WMa_publication/peduncle_mercurialis/john/writing_fragments/Luis_figures")
rm(list=ls(all=TRUE))
gen<-read.table("compiled_nomissingvalues2.csv", header=T, sep=",")
gen$block<-as.factor(gen$block)
names(gen)

# Do P+ and P- morphs differ in terms of phenotypic traits
mod_biomass<-lmer(biom~morph+(1|pop)+(1|block), data=gen)
anova(mod_biomass)

mod_pollen<-lmer(pollen~morph+(1|pop)+(1|block), data=gen)
anova(mod_pollen)

mod_seed<-lmer(seed~morph+(1|pop)+(1|block), data=gen)
anova(mod_seed)

mod_seedn<-lmer(seedn~morph+(1|pop)+(1|block), data=gen)
anova(mod_seedn)

mod_avgseedw<-lmer(avgseedw~morph+(1|pop)+(1|block), data=gen)
anova(mod_avgseedw)

mod_pollalloc<-lmer(pollalloc~morph+(1|pop)+(1|block), data=gen)
anova(mod_pollalloc)

mod_seedallo<-lmer(seedallo~morph+(1|pop)+(1|block), data=gen)
anova(mod_seedallo)

mod_sexallo<-lmer(sexallo~morph+(1|pop)+(1|block), data=gen)
anova(mod_sexallo)

mod_p_length<-lmer(p_length~morph+(1|pop)+(1|block), data=gen)
anova(mod_p_length)

mod_b_length<-lmer(b_length~morph+(1|pop)+(1|block), data=gen)
anova(mod_b_length)

mod_b_w<-lmer(b_w~morph+(1|pop)+(1|block), data=gen)
anova(mod_b_w)

mod_b_area<-lmer(b_area~morph+(1|pop)+(1|block), data=gen)
anova(mod_b_area)

mod_b_peri<-lmer(b_peri~morph+(1|pop)+(1|block), data=gen)
anova(mod_b_peri)

mod_b_circ<-lmer(b_circ~morph+(1|pop)+(1|block), data=gen)
anova(mod_b_circ)

mod_per_area<-lmer(per_area~morph+(1|pop)+(1|block), data=gen)
anova(mod_per_area)

mod_bla_pet<-lmer(bla_pet~morph+(1|pop)+(1|block), data=gen)
anova(mod_bla_pet)

mod_height<-lmer(height~morph+(1|pop)+(1|block), data=gen)
anova(mod_height)

mod_branch_0<-lmer(branch_0~morph+(1|pop)+(1|block), data=gen)
anova(mod_branch_0)

mod_i.node.3<-lmer(i.node.3~morph+(1|pop)+(1|block), data=gen)
anova(mod_i.node.3)

mod_bcurly<-lmer(curly~morph+(1|pop)+(1|block), data=gen)
anova(mod_bcurly)

mod_ped_n<-lmer(ped_n~morph+(1|pop)+(1|block), data=gen)
anova(ped_n)

mod_top_ped<-lmer(top_ped~morph+(1|pop)+(1|block), data=gen)
anova(mod_top_ped)

mod_fem_ped<-lmer(fem_ped~morph+(1|pop)+(1|block), data=gen)
anova(mod_fem_ped)

mod_branch_1<-lmer(branch_1~morph+(1|pop)+(1|block), data=gen)
anova(mod_branch_1)

mod_branch_2<-lmer(branch_2~morph+(1|pop)+(1|block), data=gen)
anova(mod_branch_2)

mod_i.node_4<-lmer(i.node_4~morph+(1|pop)+(1|block), data=gen)
anova(mod_i.node_4)

mod_i.node_5<-lmer(i.node_5~morph+(1|pop)+(1|block), data=gen)
anova(mod_i.node_5)

#correct for multiple test
p <- c (0.54,0.03,0.02,0.84,0.09,0.90,0.67,0.95,0.0001,0.02,0.24,0.03,0.05,0.01,0.04,0.05,0.01, 0.19,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,
        0.0001, 0.01,0.0001,0.0001,0.0001,0.0001)
p.adjust(p, method = "bonferroni", n =30)


##PCA plot
setwd("/Users/Wen-Juan/my_postdoc/postdoc_manuscripts/peduncle_mercurialis/john/writing_fragments/Luis_figures")

rm(list=ls(all=TRUE))
gen<-read.table("compiled_nomissingvalues.csv", header=T, sep=",")
names(gen)[4:30]<-1:27
head(gen)
gen2<-gen[,(4:30)]


fit <- princomp(gen2, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)


library(psych)
fit <- principal(gen2, nfactors=5, rotate="varimax")
fit # print results

#factominer
standard_v <- as.data.frame(scale(gen[4:30])) # standardise the variables
ped.pca <- prcomp(standard_v )                 # do a PCA
plot(ped.pca$x[,1],ped.pca$x[,2]) # make a scatterplot
text(ped.pca$x[,1],ped.pca$x[,2], gen$morph_n, cex=0.6, pos=4, col="red") # add labels


install.packages("devtools")
library("devtools")
install_github("kassambara/factoextra")
gen<-read.table("compiled_nomissingvalues.csv", header=T, sep=";")
names(gen)[4:30]<-1:27
res.pca <- prcomp(gen[,-c(1:3)],  scale = TRUE)
library("factoextra")

fviz_pca_var(res.pca, col.var="contrib")
scale_color_gradient2(low="pink", mid="dark blue",
           high="red", midpoint=96) +
 theme_minimal()
fviz_pca_biplot(res.pca,label="var", pointsize = 2.7, repel=TRUE, habillage=gen$morph, palette=c("black","dark grey"), addEllipses=TRUE, ellipse.level=0.95, title=NULL)
#fviz_pca_biplot(res.pca, label="var", habillage=gen$pop, addEllipses=FALSE, ellipse.level=0.95)

########################
setwd("/Users/Wen-Juan/my_postdoc/postdoc_manuscripts/peduncle_mercurialis/john/writing_fragments/Luis_figures")
rm(list=ls(all=TRUE))
gen<-read.table("pvclustdata.csv", header=T, sep=";")
library(pvclust)

pop.pv <- pvclust(gen, nboot=10000)
plot(pop.pv)

## random forest
rm(list=ls(all=TRUE))
gen<-read.table("randomforestdata.csv", header=T, sep=";")
require(randomForest)
require(MASS)
rf_pop<-randomForest(gen,ntree=100000,mtry=9,importance=TRUE)
importance(rf_pop)
varImpPlot(rf_pop, sort=TRUE,cex=0.9,main=NULL,pch = 20)


# hybrid dataset

rm(list=ls(all=TRUE))
gen<-read.table("hyb_plots.csv", header=T, sep="\t")
gen$block<-as.factor(gen$block)
names(gen)

library(lmerTest)
mod1<-lmer(biomass~infl*hybrid+(1|pop), data=gen)
mod2<-lmer(FRE~infl*hybrid+(1|pop), data=gen)

############
## Graphs ##
############

rm(list=ls(all=TRUE))
gen<-read.table("/Users/Wen-Juan/my_postdoc/postdoc_manuscripts/peduncle_mercurialis/john/writing_fragments/Luis_figures/hyb_plots.txt", header=T, sep = "\t")
str(gen)      
        
gen$hyb<-as.factor(gen$hyb)
library(ggplot2)
gen$cross <- as.character(gen$cross)
gen$cross <- factor(gen$cross, levels=unique(gen$cross))   # order according to given order

# biomass
p<-ggplot(gen, aes(x=cross, y=biom,group=cross)) + geom_errorbar(aes(ymin=biom-errbiom, ymax=biom+errbiom), colour="black",width=0.6)
gen$pop <- as.character(gen$pop)
gen$pop <- factor(gen$pop, levels=unique(gen$pop))   # order according to given order
p<-ggplot(gen, aes(x=pop, y=seedallo, group=infl))+ 
  geom_errorbar(aes(ymin=seedallo-error, ymax=seedallo+error), colour="black",width=0.6)
#geom_pointrange(aes(ymin=seedallo-error, ymax=seedallo+error))   # main plot
p2 <- p + geom_point(aes(shape=factor(hyb)),size=5, shape=c(1,19)) 
p3<-p2 + geom_point(aes(fill=factor(infl)),size=5, shape=21) + scale_fill_grey(start = 0, end = 1)

#Completely clear all lines except axis lines and make background white
t1<-theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.line = element_line(size=.5) )
t2<-theme(                              
  axis.title.x = element_text(face="plain", color="black", size=17),
  axis.title.y = element_text(face="plain", color="black", size=17),
  plot.title = element_text(face="plain", color = "black", size=15) ) 
p5<-p2 + theme_bw() + t2 + labs(x="", y = "Biomass (g)", title= "", size=6)
p6<-p5+ theme(axis.text.x = element_text(color="black",size=13)) # change aspect $ size of text in x axis
p7<-p6+ theme(axis.text.y = element_text(size = 13,colour ="black"))
p8<-p7 + theme(legend.position="none") # no legend
p10<-p8+ annotate("text", x = 0.7, y = 13, label = "a)", size=6)
p11<-p10+t1
p_hyb_biom <- p11 + geom_pointrange(aes(ymin=biom-errbiom, ymax=biom+errbiom),size=0.3) 
p_hyb_biom

# female reproductive effort
pdf("/Users/Wen-Juan/my_postdoc/postdoc_manuscripts/peduncle_mercurialis/john/writing_fragments/final_figures/hybrid_FRE.pdf", width=8, height=8)
gen$cross <- factor(gen$cross, levels=unique(gen$cross)) 
p<-ggplot(gen, aes(x=cross, y=FRE, shape=gen$cross))   # main plot
p2<-p + geom_point(aes(shape=factor(gen$cross)),size=8) + scale_shape_manual(values = c(0,15,1,19))
p3<-p2
#Completely clear all lines except axis lines and make background white
t1<-theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.line = element_line(size=.5) )
t2<-theme(                              
  axis.title.x = element_text(face="plain", color="black", size=15),
  axis.title.y = element_text(face="plain", color="black", size=15),
  plot.title = element_text(face="plain", color = "black", size=17))

p5<-p3+ t1
p5.1<- p5 + t2+ labs(x="", y = "Female reproductive effort", title= "", size=6)
p6<-p5.1+ theme(axis.text.y = element_text(color="black",size=13)) # change aspect $ size of text in x axis
p7<-p6+ theme(axis.text.x = element_text(color="black",size=13))
p8<-p7 + theme(legend.position="none") # no legend
p9<- p8+ annotate("text", x = 0.7, y = 1, label = "b)", size=6)
p10 <- p9+t1
p_hyb_FRE<-p9+geom_pointrange(aes(ymin=FRE-errFRE, ymax=FRE+errFRE))
p_hyb_FRE
dev.off()

library(grid)
library(gridExtra)
grid.arrange(p_hyb_biom, p_hyb_FRE, ncol=2)


############
## Graphs ##
############
setwd("/Users/Wen-Juan/my_postdoc/postdoc_manuscripts/peduncle_mercurialis/john/writing_fragments/Luis_figures")
library(ggplot2)

#seedallocation
gen<-read.table("plot_seedallo.csv", header=T, sep=";")  # read data
gen$pop <- as.character(gen$pop)
gen$pop <- factor(gen$pop, levels=unique(gen$pop))   # order according to given order
p<-ggplot(gen, aes(x=pop, y=seedallo, group=infl))+ 
  geom_errorbar(aes(ymin=seedallo-error, ymax=seedallo+error), colour="black",width=0.6)
#geom_pointrange(aes(ymin=seedallo-error, ymax=seedallo+error))   # main plot
p2<-p + geom_point(aes(fill=factor(infl)),size=5, shape=21) + scale_fill_grey(start = 1, end = 0)
# colour by inflorescence type
myLoc <- 
  (which(levels(gen$pop) == "P-_HOR") +
     which(levels(gen$pop) == "P-")) /  2
p3<-p2 + geom_vline(aes(xintercept = myLoc)) # insert vertical line 
#Completely clear all lines except axis lines and make background white
t1<-theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.line = element_line(size=.5) )
#Use theme to change axis label style
t2<-theme(                              
  axis.title.x = element_text(face="plain", color="black", size=14),
  axis.title.y = element_text(face="plain", color="black", size=14),
  plot.title = element_text(face="plain", color = "black", size=14) ) 
p4<-p3+t2
p5<-p4 + theme_bw() + t2 + labs(x="", y = "Female reproductive effort", title= "")
p6<-p5+ theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11,color="black")) # change aspect $ size of text in x axis
p7<-p6+ theme(axis.text.y = element_text(size = 11,color="black"))
p8<-p7 + theme(legend.position="none") # no legend
p10<-p8+ annotate("text", x = 2, y = 20, label = "d)", size=6)
p_seedallo<-p10+t1
p_seedallo
###########################################################################
#number of peduncles
gen<-read.table("plot_pedn.csv", header=T, sep=";")  # read data
gen$pop <- as.character(gen$pop)
gen$pop <- factor(gen$pop, levels=unique(gen$pop))   # order according to given order
p<-ggplot(gen, aes(x=pop, y=value, group=infl))+ 
geom_errorbar(aes(ymin=value-error, ymax=value+error), colour="black",width=0.6)   # main plot
p2<-p + geom_point(aes(fill=factor(infl)),size=5, shape=21) + scale_fill_grey(start = 1, end = 0)
# colour by inflorescence type
myLoc <- 
  (which(levels(gen$pop) == "P+_BNC3") +
     which(levels(gen$pop) == "P-")) /  2
p3<-p2 + geom_vline(aes(xintercept = myLoc)) # insert vertical line 
#Completely clear all lines except axis lines and make background white
t1<-theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.line = element_line(size=.5) )
#Use theme to change axis label style
t2<-theme(                              
  axis.title.x = element_text(face="plain", color="black", size=14),
  axis.title.y = element_text(face="plain", color="black", size=14),
  plot.title = element_text(face="plain", color = "black", size=14) ) 
p4<-p3+t2
p5<-p4 + theme_bw() + t2 + labs(x="", y = "Number of peduncles (n)", title= "")
p6<-p5+ theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11,colour="black")) # change aspect $ size of text in x axis
p7<-p6+ theme(axis.text.y = element_text(size = 11,color="black"))
p8<-p7 + theme(legend.position="none") # no legend
p10<-p8+ annotate("text", x = 2, y = 60, label = "a)", size=6) 
p_pedn<-p10+t1
p_pedn
##########################################################################
#drybiomass
gen<-read.table("plot_biom.csv", header=T, sep=";")  # read data
gen$pop <- as.character(gen$pop)
gen$pop <- factor(gen$pop, levels=unique(gen$pop))   # order according to given order
p<-ggplot(gen, aes(x=pop, y=value, group=infl))+ 
  geom_errorbar(aes(ymin=value-error, ymax=value+error), colour="black",width=0.6)
p2<-p + geom_point(aes(fill=factor(infl)),size=5, shape=21) + scale_fill_grey(start = 1, end = 0)

# colour by inflorescence type
myLoc <- 
  (which(levels(gen$pop) == "P-_ALM") +
     which(levels(gen$pop) == "P-")) /  2
p3<-p2 + geom_vline(aes(xintercept = myLoc)) # insert vertical line 
#Completely clear all lines except axis lines and make background white
t1<-theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.line = element_line(size=.5) )
#Use theme to change axis label style
t2<-theme(                              
  axis.title.x = element_text(face="plain", color="black", size=14),
  axis.title.y = element_text(face="plain", color="black", size=14),
  plot.title = element_text(face="plain", color = "black", size=14) ) 
p4<-p3+t2
p5<-p4 + theme_bw() + t2 + labs(x="", y = "Dry biomass (g)", title= "")
p6<-p5+ theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11, colour = "black")) # change aspect $ size of text in x axis
p7<-p6+ theme(axis.text.y = element_text(size = 11,colour = "black"))
p8<-p7 + theme(legend.position="none") # no legend
p9<-p8
p10<-p9 + annotate("text", x = 2, y = 5.2, label = "f)", size=6)
p_biom<-p10+t1
p_biom
###########################################################################
#pedunclelength
gen<-read.table("plot_pedlen.csv", header=T, sep=";")  # read data
gen$pop <- as.character(gen$pop)
gen$pop <- factor(gen$pop, levels=unique(gen$pop))   # order according to given order
p<-ggplot(gen, aes(x=pop, y=value, group=infl))+ 
  geom_errorbar(aes(ymin=value-error, ymax=value+error), colour="black",width=0.6)
p2<-p + geom_point(aes(fill=factor(infl)),size=5, shape=21) + scale_fill_grey(start = 1, end = 0)
# colour by inflorescence type
myLoc <- 
  (which(levels(gen$pop) == "P+_BNC3") +
     which(levels(gen$pop) == "P-")) /  2
p3<-p2 + geom_vline(aes(xintercept = myLoc)) # insert vertical line 
#Completely clear all lines except axis lines and make background white
t1<-theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.line = element_line(size=.5) )
#Use theme to change axis label style
t2<-theme(                              
  axis.title.x = element_text(face="plain", color="black", size=14),
  axis.title.y = element_text(face="plain", color="black", size=14),
  plot.title = element_text(face="plain", color = "black", size=14) ) 
p4<-p3+t2
p5<-p4 + theme_bw() + t2 + labs(x="", y = "Peduncle length (mm)", title= "")
p6<-p5+ theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11,colour = "black")) # change aspect $ size of text in x axis
p7<-p6+ theme(axis.text.y = element_text(size = 11,colour = "black"))
p8<-p7 + theme(legend.position="none") # no legend
p9<-p8
p10<-p9+ annotate("text", x = 2, y = 80, label = "b)", size=6)
p_pedlen<-p10+t1
p_pedlen
###########################################################################
#male repr effort
gen<-read.table("plot_pollaloc.csv", header=T, sep=";")  # read data
gen$pop <- as.character(gen$pop)
gen$pop <- factor(gen$pop, levels=unique(gen$pop))   # order according to given order
p<-ggplot(gen, aes(x=pop, y=value, group=infl))+ 
  geom_errorbar(aes(ymin=value-error, ymax=value+error), colour="black",width=0.6)
p2<-p + geom_point(aes(fill=factor(infl)),size=5, shape=21) + scale_fill_grey(start = 1, end = 0)
# colour by inflorescence type
myLoc <- 
  (which(levels(gen$pop) == "P+_ORI") +
     which(levels(gen$pop) == "P-")) /  2
p3<-p2 + geom_vline(aes(xintercept = myLoc)) # insert vertical line 
#Completely clear all lines except axis lines and make background white
t1<-theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.line = element_line(size=.5) )
#Use theme to change axis label style
t2<-theme(                              
  axis.title.x = element_text(face="plain", color="black", size=14),
  axis.title.y = element_text(face="plain", color="black", size=14),
  plot.title = element_text(face="plain", color = "black", size=14) ) 
p4<-p3+t2
p5<-p4 + theme_bw() + t2 + labs(x="", y = "Male reproductive effort", title= "")
p6<-p5+ theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11,colour = "black")) # change aspect $ size of text in x axis
p7<-p6+ theme(axis.text.y = element_text(size = 11,colour = "black"))
p8<-p7 + theme(legend.position="none") # no legend
p9<-p8
p10<-p9+ annotate("text", x = 2, y = 8.5, label = "c)", size=6)
p_pollaloc<-p10+t1
p_pollaloc
###########################################################################
#sex allocation
gen<-read.table("plot_sexallo.csv", header=T, sep=";")  # read data
gen$pop <- as.character(gen$pop)
gen$pop <- factor(gen$pop, levels=unique(gen$pop))   # order according to given order
p<-ggplot(gen, aes(x=pop, y=value, group=infl)) +
geom_errorbar(aes(ymin=value-error, ymax=value+error), colour="black",width=0.6)
p2<-p + geom_point(aes(fill=factor(infl)),size=5, shape=21) + scale_fill_grey(start = 1, end = 0)
# colour by inflorescence type
myLoc <- 
  (which(levels(gen$pop) == "P+_VAC") +
     which(levels(gen$pop) == "P-")) /  2
p3<-p2 + geom_vline(aes(xintercept = myLoc)) # insert vertical line 
#Completely clear all lines except axis lines and make background white
t1<-theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.line = element_line(size=.5) )
#Use theme to change axis label style
t2<-theme(                              
  axis.title.x = element_text(face="plain", color="black", size=14),
  axis.title.y = element_text(face="plain", color="black", size=14),
  plot.title = element_text(face="plain", color = "black", size=14) ) 
p4<-p3+t2
p5<-p4 + theme_bw() + t2 + labs(x="", y = "Sex allocation", title= "")
p6<-p5+ theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11,colour = "black")) # change aspect $ size of text in x axis
p7<-p6+ theme(axis.text.y = element_text(size = 9,colour = "black"))
p8<-p7 + theme(legend.position="none") # no legend
p9<-p8
p10<-p9+ annotate("text", x = 2, y = 0.75, label = "e)", size=6)
p_sexallo<-p10+t1
p_sexallo
###########################################################################
#male/female
gen<-read.table("plot_male_fem.csv", header=T, sep=";")  # read data
gen$pop <- as.character(gen$pop)
gen$pop <- factor(gen$pop, levels=unique(gen$pop))   # order according to given order
p<-ggplot(gen, aes(x=pop, y=value, group=infl))+ 
  geom_errorbar(aes(ymin=value-error, ymax=value+error), colour="black",width=0.6)
p2<-p + geom_point(aes(fill=factor(infl)),size=5, shape=21) + scale_fill_grey(start = 1, end = 0)
# colour by inflorescence type
myLoc <- 
  (which(levels(gen$pop) == "P+_SJV") +
     which(levels(gen$pop) == "P-")) /  2
p3<-p2 + geom_vline(aes(xintercept = myLoc)) # insert vertical line 
#Completely clear all lines except axis lines and make background white
t1<-theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.line = element_line(size=.5) )
#Use theme to change axis label style
t2<-theme(                              
  axis.title.x = element_text(face="plain", color="black", size=14),
  axis.title.y = element_text(face="plain", color="black", size=14),
  plot.title = element_text(face="plain", color = "black", size=14) ) 
p4<-p3+t2
p5<-p4 + theme_bw() + t2 + labs(x="", y = "Male/Female reproductive investment", title= "")
p6<-p5+ theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11)) # change aspect $ size of text in x axis
p7<-p6+ theme(axis.text.y = element_text(size = 11))
p8<-p7 + theme(legend.position="none") # no legend
p9<-p8
p10<-p9+ annotate("text", x = 2, y = 3.75, label = "g)", size=6)
p_malefem<-p10+t1
p_malefem

library(grid)
library(gridExtra)
grid.arrange(p_pedn,p_pedlen,p_pollaloc,p_seedallo,p_sexallo,p_biom, ncol=3, nrow=2)
###########################################################################

ggsave("p_sexallo.tiff", p_sexallo, dpi=300)
ggsave("p_pollaloc.tiff", p_pollaloc, dpi=300)
ggsave("p_biom.tiff", p_biom, dpi=300)
ggsave("p_seedallo.tiff", p_seedallo, dpi=300)
ggsave("p_pedn.tiff", p_pedn, dpi=300)
ggsave("p_pedlen.tiff", p_pedlen, dpi=300)
ggsave("p_malefem.tiff", p_malefem, dpi=300)

###########################################################################
#Cross experiment plots





