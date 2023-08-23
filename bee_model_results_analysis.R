# ------------------- Setup --------------------

# import libraries
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# set working directory
setwd("C:/Users/wjt/OneDrive - Leuphana Universit�t/SoSe 23/Ecosystem Modeling - Interdisciplinary Sustainability Studies (Major)/Ecosystem Modeling (S)/model_bees")
setwd("C:/Users/swenj/OneDrive/Dokumente/OneDrive/Dokumente/Uni/6 Semester/Ecosystem Modelling/Netlogo/model_bees/Ergebnisse/experiment2")

# load data
results<-read.csv("table_results_2.csv", sep=";")

# inspect data
View(results)
str(results)
summary(results)


# -------------------- Pre-processing -------------------

# calculate actual breeding and feeding habitat number
# because sometimes less than the specified number fit in the view
results$actual.breeding.habitat.number<-results$count.breeding.habitat/(results$breeding.habitat.size**2)
results$actual.feeding.habitat.number<-results$count.feeding.habitat/(results$feeding.habitat.size**2)

# change data type from integer to factor for categorical/ordinal variables to enable TukeyHSD posthoc test later
cat_cols<-c("breeding.habitat.number", "feeding.habitat.number", "breeding.habitat.size", "feeding.habitat.size", "min.distance.breed", "min.distance.feed", "max.distance.breed", "max.distance.feed", "actual.breeding.habitat.number", "actual.feeding.habitat.number")
for (column in cat_cols){
  results[[column]]<-as.factor(results[[column]])
}

# rename columns
results<-results %>%
  rename(count.generalist.bees = count.bees...count.specialized.bees, count.cyan.flowers = count.flowers.with..color...cyan., sum.crop.seeds = sum..seeds..of.crops)

# check changes
str(results)
summary(results)

attach(results)


# --------------------- Visualizations --------------------

# boxplots

predictorvars <- list(actual.breeding.habitat.number, breeding.habitat.size, min.distance.breed, actual.feeding.habitat.number, feeding.habitat.size, min.distance.feed)
predictornames <- list("Number of breeding habitats", "Breeding habitat size", "Distance of breeding habitats to each other", "Feeding habitat number", "Feeding habitat size", "Distance of feeding to breeding habitat")

par(mfrow=c(2,3))
      
# plot all bees 
for (k in 1:6) {
  boxplot(count.bees~predictorvars[[k]], col="#26734d", xlab=predictornames[k], ylab="Bees")
}    
  
# plot specialized bees
for (k in 1:6) {
  boxplot(count.specialized.bees~predictorvars[[k]], col="#26734d", xlab=predictornames[k], ylab="Specialized Bees")
}    

# plot generalist bees
for (k in 1:6) {
  boxplot(count.generalist.bees~predictorvars[[k]], col="#26734d", xlab=predictornames[k], ylab="Generalist Bees")
}    


# plot the relation between bees and flowers

flowers <- list(count.flowers, count.wildflowers, count.cyan.flowers, sum.crop.seeds)
bees <- list(count.bees, count.bees, count.specialized.bees, count.generalist.bees)
flowerslabel <- list("Flowers", "Wildflowers", "Cyan flowers", "Seeds of Crops")
beelabels <- list("Bees", "Bees", "Specialized Bees", "Generalist Bees")

par(mfrow=c(2,2))

for (i in (1:4)) {
  plot(flowers[[i]], bees[[i]], 
       xlab=flowerslabel[i], 
       ylab=beelabels[i],
       col=rgb(0, 0.4, 0, 0.3)
       )
}


# -------------------- Anova --------------------

# check if the dependent variable is normally distributed 
hist(count.bees, breaks=100) # not normal, but rather poisson distribution 
hist(count.generalist.bees, breaks=100)
hist(count.specialized.bees, breaks=100)
shapiro.test(count.bees) # not normally distributed


# test for differences in the amount of bees between treatments using an anova with all 6 predictors
model6<-aov(count.bees~actual.breeding.habitat.number+breeding.habitat.size+min.distance.breed+actual.feeding.habitat.number+feeding.habitat.size+min.distance.feed)
summary(model6)

# model reduction to 5 predictors
model5<-update(model6,~.-min.distance.feed)
summary(model5)
anova(model5, model6) # model6 does not explain significantly more than model5
AIC(model5);AIC(model6)

# model reduction to 4 predictors
model4<-update(model5,~.-min.distance.breed)
summary(model4)
anova(model4, model5) # model5 does not explain significantly more than model4
AIC(model4);AIC(model5)

# model reduction to 3 predictors
model3<-update(model4,~.-actual.breeding.habitat.number)
summary(model3)
anova(model3, model4) # model4 does explain significantly more than model3
AIC(model3);AIC(model4)

# results of model4
summary(model4)
TukeyHSD(model4)


# test for differences in the amount of generalist bees between treatments using an anova with all 6 predictors
model6_generalists<-aov(count.generalist.bees~actual.breeding.habitat.number+breeding.habitat.size+min.distance.breed+actual.feeding.habitat.number+feeding.habitat.size+min.distance.feed)
summary(model6_generalists)

# model reduction to 5 predictors
model5_generalists<-update(model6_generalists,~.-min.distance.breed)
summary(model5_generalists)
anova(model5_generalists, model6_generalists) # model6 does not explain significantly more than model5
AIC(model5_generalists);AIC(model6_generalists)

# model reduction to 4 predictors
model4_generalists<-update(model5_generalists,~.-min.distance.feed)
summary(model4_generalists)
anova(model4_generalists, model5_generalists) # model5 does not explain significantly more than model4
AIC(model4_generalists);AIC(model5_generalists)

# model reduction to 3 predictors
model3_generalists<-update(model4_generalists,~.-actual.breeding.habitat.number)
summary(model3_generalists)
anova(model3_generalists, model4_generalists) # model4 does not explain significantly more than model3
AIC(model3_generalists);AIC(model4_generalists)

# model reduction to 2 predictors
model2_generalists<-update(model3_generalists,~.-actual.feeding.habitat.number)
summary(model2_generalists)
anova(model2_generalists, model3_generalists) # model3 does explain significantly more than model2
AIC(model2_generalists);AIC(model3_generalists)

# results of model3
summary(model3_generalists)
TukeyHSD(model3_generalists)


# test for differences in the amount of specialist bees between treatments using an anova with all 6 predictors
model6_specialists<-aov(count.specialized.bees~actual.breeding.habitat.number+breeding.habitat.size+min.distance.breed+actual.feeding.habitat.number+feeding.habitat.size+min.distance.feed)
summary(model6_specialists)

# model reduction to 5 predictors
model5_specialists<-update(model6_specialists,~.-min.distance.feed)
summary(model5_specialists)
anova(model5_specialists, model6_specialists) # model6 does not explain significantly more than model5
AIC(model5_specialists);AIC(model6_specialists)

# model reduction to 4 predictors
model4_specialists<-update(model5_specialists,~.-min.distance.breed)
summary(model4_specialists)
anova(model4_specialists, model5_specialists) # model5 does not explain significantly more than model4
AIC(model4_specialists);AIC(model5_specialists)

# model reduction to 3 predictors
model3_specialists<-update(model4_specialists,~.-actual.breeding.habitat.number)
summary(model3_specialists)
anova(model3_specialists, model4_specialists) # model4 does explain significantly more than model3
AIC(model3_specialists);AIC(model4_specialists)

# results of model4
summary(model4_specialists)
TukeyHSD(model4_specialists)


# ------------------ Correlations ---------------------

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matrix of the p-value of Spearman correlations for non-normal data
p.mat <- cor.mtest(results[,c(13,14,15,17,18,19,20)], method = "spearman")
head(p.mat[, 1:5])

# visualize the significant Spearman correlations
par(mfrow=c(1,1))
corrplot::corrplot(cor(results[,c(13,14,15,17,18,19,20)], method = "spearman"), 
                   method = "color",  col=brewer.pal(n=8, name="RdYlGn"),
                   type = "upper", order = "hclust", diag = FALSE,
                   addCoef.col = "black", number.cex = 0.8, cl.pos = "n",
                   tl.col="black", tl.cex = 0.8, 
                   p.mat = p.mat, sig.level = 0.05)


# ------------------- GLM --------------------

# test for differences in the amount of bees between treatments using a glm with all 6 predictors
model6_glm<-glm(count.bees~actual.breeding.habitat.number+breeding.habitat.size+min.distance.breed+actual.feeding.habitat.number+feeding.habitat.size+min.distance.feed, family = quasipoisson())
summary(model6_glm)

# check if residuals are normally distributed
hist(resid(model6_glm))
qqnorm(resid(model6_glm))
qqline(resid(model6_glm))
shapiro.test(resid(model6_glm)) # residuals are almost normally distributed

# model reduction to 5 predictors
model5_glm<-update(model6_glm,~.-min.distance.breed)
summary(model5_glm)
anova(model5_glm, model6_glm, test = "F") # model5_glm does not explain significantly more than model4_glm

# model reduction to 4 predictors
model4_glm<-update(model5_glm,~.-min.distance.feed)
summary(model4_glm)
anova(model4_glm, model5_glm, test = "F") # model4_glm does not explain significantly more than model3_glm

# model reduction to 3 predictors
model3_glm<-update(model4_glm,~.-actual.feeding.habitat.number)
summary(model3_glm)
anova(model3_glm, model4_glm, test = "F") # model4_glm does explain significantly more than model3_glm

# compare results of the anova and the glm
summary(model4)
summary(model4_glm) # very similar results;
# feeding habitat number is significant in the anova, but only marginally significant in the glm
