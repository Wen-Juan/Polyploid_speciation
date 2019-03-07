#install R packages and libraries
install.packages("lmerTest")
library(lme4)
library(lmerTest)

#setting up working directory
setwd("~/Dropbox (Amherst College)/my_postdoc/useful_scripts/Polyploid_speciation/input/Luis/")


#load datasets and analyze datasets
gen<-read.table("biomass.csv", header=T, sep=";")
gen$block<-as.factor(gen$block)

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


mod_curly<-glmer(curly~Morph+(1|Tag)+(1|Block), family=binomial, data=gen)
mod_curly2<-glmer(curly~Morph+(1|Block), family=binomial, data=gen)
mod_curly3<-glmer(curly~Morph+(1|Tag), family=binomial, data=gen)
anova(mod_curly)
anova(mod_curly,mod_curly2)
anova(mod_curly,mod_curly3)
summary(mod_curly)


mod_trichomes<-glmer(trichomes~Morph+(1|Tag)+(1|Block), family=binomial,data=gen)
mod_trichomes3<-glmer(trichomes~Morph+(1|Tag), family=binomial,data=gen)
mod_trichomes2<-glmer(trichomes~Morph+(1|Block), family=binomial,data=gen)
anova(mod_trichomes)
anova(mod_trichomes,mod_trichomes2)
anova(mod_trichomes,mod_trichomes3)
summary(mod_trichomes)


rm(list=ls(all=TRUE))
gen<-read.table("leaves.csv", header=T, sep=";")
gen$Rep<-as.factor(gen$Rep)
 

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

rm(list=ls(all=TRUE))
gen<-read.table("compiled_nomissingvalues.csv", header=T, sep=";")
gen2<-gen[,(4:30)]

standard_v <- as.data.frame(scale(gen[4:30])) # standardise the variables
ped.pca <- prcomp(standard_v )                 # do a PCA
plot(ped.pca$x[,1],ped.pca$x[,2]) # make a scatterplot
text(ped.pca$x[,1],ped.pca$x[,2], gen$morph_n, cex=0.7, pos=4, col="red") # add labels


#install packages and libraries, and perform PCA analysis
install.packages("devtools")
library("devtools")
install_github("kassambara/factoextra")
gen<-read.table("compiled_nomissingvalues.csv", header=T, sep=";")
res.pca <- prcomp(gen[,-c(1:3)],  scale = TRUE)
library("factoextra")

fviz_pca_var(res.pca, col.var="contrib")
 scale_color_gradient2(low="white", mid="blue",
           high="red", midpoint=96) +
 theme_minimal()
 
 
fviz_pca_biplot(res.pca, label="var", repel=TRUE, habillage=gen$morph, palette=c("black","grey"), addEllipses=TRUE, ellipse.level=0.95, title=NULL)
fviz_pca_biplot(res.pca, label="var", habillage=gen$pop, addEllipses=FALSE, ellipse.level=0.95)







