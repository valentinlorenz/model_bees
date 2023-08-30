# Authors: Valentin Lorenz & Wanja Tolksdorf 
# Copyright 2023 by Valentin Lorenz & Wanja Tolksdorf 
# This work is licensed under a Creative Commons Attribution 4.0 International License (CC BY 4.0). 
# You are free to share and adapt this code, but have to give appropriate credit. 
# Full license terms: https://creativecommons.org/licenses/by/4.0/legalcode
# Link to work: https://github.com/valentinlorenz/model_bees

# Credits go to the authors of the following packages for their work:
# dplyr ((c) 2023 dplyr authors)
# ggplot2 ((c) 2020 ggplot2 authors)
# RColorBrewer by Cynthia Brewer ((c) 2002 Cynthia Brewer, Mark Harrower, and The Pennsylvania State University.)


# ------------------- Setup --------------------

# import libraries
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# set working directory
# setwd("")

# load data
results<-read.csv("table_results.csv", sep=";")

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

par(mfrow=c(2,3), mar = c(5,5,2,3))
      
# plot all bees 
for (k in 1:6) {
  boxplot(count.bees~predictorvars[[k]], col="#26734d", xlab=predictornames[k], ylab="Bees", cex.axis = 2, cex.lab = 2)
}    
  
# plot specialized bees
for (k in 1:6) {
  boxplot(count.specialized.bees~predictorvars[[k]], col="#26734d", xlab=predictornames[k], ylab="Specialized Bees", cex.axis = 2, cex.lab = 2)
}    

# plot generalist bees
for (k in 1:6) {
  boxplot(count.generalist.bees~predictorvars[[k]], col="#26734d", xlab=predictornames[k], ylab="Generalist Bees", cex.axis = 2, cex.lab = 2)
}    


# plot the relation between bees and flowers

flowers <- list(count.flowers, count.wildflowers, count.cyan.flowers, sum.crop.seeds)
bees <- list(count.bees, count.bees, count.specialized.bees, count.generalist.bees)
flowerslabel <- list("Flowers", "Wildflowers", "Cyan flowers", "Seeds of Crops")
beelabels <- list("Bees", "Bees", "Specialized Bees", "Generalist Bees")

par(mfrow=c(2,2), mar = c(5,5,2,2))

for (i in (1:4)) {
  plot(flowers[[i]], bees[[i]], 
       xlab=flowerslabel[i], 
       ylab=beelabels[i],
       col=rgb(0, 0.4, 0, 0.3),
       cex.axis = 2, cex.lab = 2
       )
}


# -------------------- ANOVA --------------------

# check if the dependent variable is normally distributed 
par(mfrow=c(2,2), mar = c(5,5,5,2))
hist(count.bees, breaks=100, col="#26734d", cex.main = 2, cex.axis = 2, cex.lab = 2) # not normal, but rather poisson distribution 
hist(count.generalist.bees, col="#26734d", cex.main = 2, breaks=100, cex.axis = 2, cex.lab = 2)
hist(count.specialized.bees, breaks=100, col="#26734d", cex.main = 2, cex.axis = 2, cex.lab = 2)
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
model4<-update(model5,~.-actual.breeding.habitat.number)
summary(model4)
anova(model4, model5) # model5 does explain significantly more than model4
AIC(model4);AIC(model5)

# results of model5
summary(model5)
TukeyHSD(model5)


# test for differences in the amount of generalist bees between treatments using an anova with all 6 predictors
model6_generalists<-aov(count.generalist.bees~actual.breeding.habitat.number+breeding.habitat.size+min.distance.breed+actual.feeding.habitat.number+feeding.habitat.size+min.distance.feed)
summary(model6_generalists)

# model reduction to 5 predictors
model5_generalists<-update(model6_generalists,~.-min.distance.feed)
summary(model5_generalists)
anova(model5_generalists, model6_generalists) # model6 does not explain significantly more than model5
AIC(model5_generalists);AIC(model6_generalists)

# model reduction to 4 predictors
model4_generalists<-update(model5_generalists,~.-min.distance.breed)
summary(model4_generalists)
anova(model4_generalists, model5_generalists) # model5 does not explain significantly more than model4
AIC(model4_generalists);AIC(model5_generalists)

# model reduction to 3 predictors
model3_generalists<-update(model4_generalists,~.-actual.breeding.habitat.number)
summary(model3_generalists)
anova(model3_generalists, model4_generalists) # model4 does explain significantly more than model3
AIC(model3_generalists);AIC(model4_generalists)

# results of model4
summary(model4_generalists)
TukeyHSD(model4_generalists)


# test for differences in the amount of specialist bees between treatments using an anova with all 6 predictors
model6_specialists<-aov(count.specialized.bees~actual.breeding.habitat.number+breeding.habitat.size+min.distance.breed+actual.feeding.habitat.number+feeding.habitat.size+min.distance.feed)
summary(model6_specialists)

# model reduction to 5 predictors
model5_specialists<-update(model6_specialists,~.-min.distance.feed)
summary(model5_specialists)
anova(model5_specialists, model6_specialists) # model6 does not explain significantly more than model5
AIC(model5_specialists);AIC(model6_specialists)

# model reduction to 4 predictors
model4_specialists<-update(model5_specialists,~.-actual.breeding.habitat.number)
summary(model4_specialists)
anova(model4_specialists, model5_specialists) # model5 does explain significantly more than model4
AIC(model4_specialists);AIC(model5_specialists)

# results of model5
summary(model5_specialists)
TukeyHSD(model5_specialists)


# ------------------- GLM --------------------

# test for differences in the amount of bees between treatments using a glm with all 6 predictors
model6_glm<-glm(count.bees~actual.breeding.habitat.number+breeding.habitat.size+min.distance.breed+actual.feeding.habitat.number+feeding.habitat.size+min.distance.feed, family = quasipoisson())
summary(model6_glm)

# check if residuals are normally distributed
par(mfrow=c(1,2), mar = c(5,5,5,5))
hist(resid(model6_glm), col="#26734d", cex.main = 2, cex.axis = 2, cex.lab = 2)
qqnorm(resid(model6_glm), col="#26734d", cex.main = 2, cex.axis = 2, cex.lab = 2)
qqline(resid(model6_glm))
shapiro.test(resid(model6_glm)) # residuals are almost normally distributed

# model reduction to 5 predictors
model5_glm<-update(model6_glm,~.-min.distance.feed)
summary(model5_glm)
anova(model5_glm, model6_glm, test = "F") # model6_glm does not explain significantly more than model5_glm

# model reduction to 4 predictors
model4_glm<-update(model5_glm,~.-min.distance.breed)
summary(model4_glm)
anova(model4_glm, model5_glm, test = "F") # model5_glm does explain significantly more than model4_glm

# compare results of the anova and the glm
summary(model5)
summary(model5_glm) # very similar results


# ------------------ Correlations ---------------------

# function for calculating p-values of multiple correlations
# df : dataframe containing the variables for correlation tests
# ... : further arguments to pass to the cor.test function
cor.test.mat <- function(df, ...) {
  
  n <- ncol(df)
  p.value.mat <- matrix(NA, n, n)
  colnames(p.value.mat) <- colnames(df)
  rownames(p.value.mat) <- colnames(df)
  diag(p.value.mat) <- 0
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      cor.test.results <- cor.test(df[, i], df[, j], ...)
      p.value.mat[i, j] <- cor.test.results$p.value
      p.value.mat[j, i] <- cor.test.results$p.value
    }
  }
  
  return(p.value.mat)
}

# matrix of the p-value of Spearman correlations for non-normal data
p.value.mat <- cor.test.mat(results[,c(13,14,15,17,18,19,20)], method = "spearman")
head(p.value.mat)

# visualize the significant Spearman correlations
par(mfrow=c(1,1))
corrplot::corrplot(cor(results[,c(13,14,15,17,18,19,20)], method = "spearman"), 
                   method = "color",  col=brewer.pal(n=8, name="RdYlGn"),
                   type = "upper", order = "hclust", diag = FALSE,
                   addCoef.col = "black", number.cex = 1, cl.pos = "n",
                   tl.col="black", tl.cex = 1, 
                   p.mat = p.value.mat, sig.level = 0.05)


# -------------------- Chi-square -------------------

# calculate the number of specialist and generalist bee extinctions/survivals and create a table
extinction_table<-as.table(rbind(c(sum(count.specialized.bees == 0), sum(count.specialized.bees != 0)), c(sum(count.generalist.bees == 0), sum(count.generalist.bees != 0))))
dimnames(extinction_table)<-list(c("specialist bees", "generalist bees"), c("extinction","survival"))

# calculate Chi-square test
extinction_chisq<-chisq.test(extinction_table)

# Chi-square results
extinction_chisq
extinction_chisq$observed   
extinction_chisq$expected   

# visualize bee extinctions as barplot
par(mfrow=c(1,1), mar = c(5,5,5,5))
barplot(extinction_table,  col=c("#26734d", "#7CCD7C"), beside = TRUE, 
        legend.text = TRUE, args.legend = list(x = "topleft", cex = 1.25, bty = "n"),
        main = "Bee extinctions", cex.main = 1.5, cex.axis = 1.5, cex.names = 1.5)

