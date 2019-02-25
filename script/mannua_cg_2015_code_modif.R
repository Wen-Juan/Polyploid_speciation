install.packages("lmerTest")
library(lme4)
library(lmerTest)

setwd("/Users/Wen-Juan/my_postdoc/postdoc_manuscripts/peduncle_mercurialis/john/writing_fragments/Luis_figures")
rm(list=ls(all=TRUE))
gen<-read.table("compiled_nomissingvalues.csv", header=T, sep=";")
gen$block<-as.factor(gen$block)
names(gen)

# Do P+ and P- morphs differ in terms of biomass and reproductive allocation?
mod_biomass<-lmer(biom~morph+(1|pop)+(1|block), data=gen)
anova(mod_biomass)
rand(mod_biomass)
mod_pollen<-lmer(pollen~morph+(1|pop)+(1|block), data=gen)
anova(mod_pollen)
rand(mod_pollen)
mod_seed<-lmer(seed~morph+(1|pop)+(1|block), data=gen)
anova(mod_seed)
rand(mod_seed)
mod_seedn<-lmer(seedn~morph+(1|pop)+(1|block), data=gen)
anova(mod_seedn)
rand(mod_seedn)
mod_avgseedw<-lmer(avgseedw~morph+(1|pop)+(1|block), data=gen)
anova(mod_avgseedw)
rand(mod_avgseedw)
mod_pollalloc<-lmer(pollalloc~morph+(1|pop)+(1|block), data=gen)
anova(mod_pollalloc)
rand(mod_pollalloc)
mod_seedallo<-lmer(seedallo~morph+(1|pop)+(1|block), data=gen)
anova(mod_seedallo)
rand(mod_seedallo)
mod_sexallo<-lmer(sexallo~morph+(1|pop)+(1|block), data=gen)
anova(mod_sexallo)
rand(mod_sexallo)


rm(list=ls(all=TRUE))
gen<-read.table("cg_gh_2015.csv", header=T, sep=";")
gen$Block<-as.factor(gen$Block)
gen$Table<-as.factor(gen$Table)
names(gen)

"height"     "height.w.i" "branch_0"   "branch_1"   "branch_2"   "i.node.3"   "i.node_4"   "i.node_5"   
"ped_n"      "top_ped"    "fem_ped"    "p_1_length" "p_1_clus"   "p_2_length"
"p_2_clus"   "p_3_length" "p_3_clus"   "p_4_length" "p_4_clus"   "p_5_length" "p_5_clus"   "avg_p"      

mod_height<-lmer(height~Morph+(1|Tag)+(1|Block), data=gen)
anova(mod_height)
rand(mod_height)


mod_height.w.i<-lmer(height.w.i~Morph+(1|Tag)+(1|Block), data=gen)
anova(mod_height.w.i)
rand(mod_height.w.i)

mod_branch_0<-lmer(branch_0~Morph+(1|Tag)+(1|Block), data=gen)
anova(mod_branch_0)
rand(mod_branch_0)

mod_branch_1<-lmer(branch_1~Morph+(1|Tag)+(1|Block), data=gen)
anova(mod_branch_1)
rand(mod_branch_1)

mod_branch_2<-lmer(branch_2~Morph+(1|Tag)+(1|Block), data=gen)
anova(mod_branch_2)
rand(mod_branch_2)

mod_i.node.3<-lmer(i.node.3~Morph+(1|Tag)+(1|Block), data=gen)
anova(mod_i.node.3)
rand(mod_i.node.3)

mod_i.node_4<-lmer(i.node_4~Morph+(1|Tag)+(1|Block), data=gen)
anova(mod_i.node_4)
rand(mod_i.node_4)

mod_i.node_5<-lmer(i.node_5~Morph+(1|Tag)+(1|Block), data=gen)
anova(mod_i.node_5)
rand(mod_i.node_5)

mod_ped_n<-lmer(ped_n~Morph+(1|Tag)+(1|Block), data=gen)
anova(mod_ped_n)
rand(mod_ped_n)

mod_avg_p<-lmer(avg_p~Morph+(1|Tag)+(1|Block), data=gen)
anova(mod_avg_p)
rand(mod_avg_p)

mod_fem_ped<-lmer(fem_ped~Morph+(1|Tag)+(1|Block), data=gen)
anova(mod_fem_ped)
rand(mod_fem_ped)


mod_top_ped<-lmer(top_ped~Morph+(1|Tag)+(1|Block), data=gen)
anova(mod_top_ped)
rand(mod_top_ped)


mod_avg_clus<-lmer(avg_clus~Morph+(1|Tag)+(1|Block), data=gen)
anova(mod_avg_clus)
rand(mod_avg_clus)


"curly"    "trichomes"
mod_curly<-glmer(curly~Morph+(1|Tag)+(1|Block), family=binomial, data=gen)
mod_curly2<-glmer(curly~Morph+(1|Block), family=binomial, data=gen)
mod_curly3<-glmer(curly~Morph+(1|Tag), family=binomial, data=gen)
anova(mod_curly)
anova(mod_curly,mod_curly2)
anova(mod_curly,mod_curly3)
summary(mod_curly)
inv.logit(-2.7806)-inv.logit(-2.7806+0.68)
inv.logit(0.8921)-inv.logit(0.8921+0.5382)

mod_trichomes<-glmer(trichomes~Morph+(1|Tag)+(1|Block), family=binomial,data=gen)
mod_trichomes3<-glmer(trichomes~Morph+(1|Tag), family=binomial,data=gen)
mod_trichomes2<-glmer(trichomes~Morph+(1|Block), family=binomial,data=gen)
anova(mod_trichomes)
anova(mod_trichomes,mod_trichomes2)
anova(mod_trichomes,mod_trichomes3)
summary(mod_trichomes)
inv.logit(-2.0891)-inv.logit(-2.0891+0.777)
inv.logit(-3.4417)-inv.logit(-3.4417+0.8035)

rm(list=ls(all=TRUE))
gen<-read.table("leaves.csv", header=T, sep=";")
gen$Rep<-as.factor(gen$Rep)
names(gen)

"petioleLength"    "bladeLength"      "bladeWidth"       "bladeArea"        "bladePerimeter"   "bladeCircularity" 
"perimeter_area"   "blade_petiole" 

mod_petioleLength<-lmer(petioleLength~morph+(1|genotype)+(1|Rep), data=gen)
anova(mod_petioleLength)
rand(mod_petioleLength)

mod_bladeLength<-lmer(bladeLength~morph+(1|genotype)+(1|Rep), data=gen)
anova(mod_bladeLength)
rand(mod_bladeLength)

mod_bladeWidth<-lmer(bladeWidth~morph+(1|genotype)+(1|Rep), data=gen)
anova(mod_bladeWidth)
rand(mod_bladeWidth)

mod_bladeArea<-lmer(bladeArea~morph+(1|genotype)+(1|Rep), data=gen)
anova(mod_bladeArea)
rand(mod_bladeArea)

mod_bladePerimeter<-lmer(bladePerimeter~morph+(1|genotype)+(1|Rep), data=gen)
anova(mod_bladePerimeter)
rand(mod_bladePerimeter)

mod_bladeCircularity<-lmer(bladeCircularity~morph+(1|genotype)+(1|Rep), data=gen)
anova(mod_bladeCircularity)
rand(mod_bladeCircularity)

mod_perimeter_area<-lmer(perimeter_area~morph+(1|genotype)+(1|Rep), data=gen)
anova(mod_perimeter_area)
rand(mod_perimeter_area)

mod_blade_petiole<-lmer(blade_petiole~morph+(1|genotype)+(1|Rep), data=gen)
anova(mod_blade_petiole)
rand(mod_blade_petiole)

#####################################################################

setwd("/Users/Wen-Juan/my_postdoc/postdoc_manuscripts/peduncle_mercurialis/john/writing_fragments/Luis_figures")

rm(list=ls(all=TRUE))
gen<-read.table("compiled_nomissingvalues.csv", header=T, sep=";")
names(gen)[4:30]<-1:27

head(gen)

gen2<-gen[,(4:30)]

#Run on R 3.2.2 64!!!!

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





