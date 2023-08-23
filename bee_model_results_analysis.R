# ------------------- Setup --------------------

# set working directory
setwd("C:/Users/wjt/OneDrive - Leuphana Universität/SoSe 23/Ecosystem Modeling - Interdisciplinary Sustainability Studies (Major)/Ecosystem Modeling (S)/model_bees")

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
library("dplyr")
results<-results %>%
  rename(count.generalist.bees = count.bees...count.specialized.bees, count.cyan.flowers = count.flowers.with..color...cyan., sum.crop.seeds = sum..seeds..of.crops)

# check changes
str(results)
summary(results)

attach(results)


# --------------------- Visualizations --------------------

# check for normal distribution 
hist(count.bees, breaks=100) # not normal, but poisson distribution 
hist(count.generalist.bees, breaks=100)
hist(count.specialized.bees, breaks=100)
shapiro.test(count.bees) # not normally distributed

# visualize the difference in the amount of bees between different treatments with boxplots
boxplot(count.bees~actual.breeding.habitat.number)
boxplot(count.bees~breeding.habitat.size)
boxplot(count.bees~min.distance.breed)
boxplot(count.bees~actual.feeding.habitat.number)
boxplot(count.bees~feeding.habitat.size)
boxplot(count.bees~min.distance.feed)

# visualize the difference in the amount of specialist vs generalist bees between different treatments with boxplots
boxplot(count.generalist.bees~actual.breeding.habitat.number)
boxplot(count.specialized.bees~actual.breeding.habitat.number)
boxplot(count.generalist.bees~breeding.habitat.size)
boxplot(count.specialized.bees~breeding.habitat.size)
boxplot(count.generalist.bees~min.distance.breed)
boxplot(count.specialized.bees~min.distance.breed)
boxplot(count.generalist.bees~actual.feeding.habitat.number)
boxplot(count.specialized.bees~actual.feeding.habitat.number)
boxplot(count.generalist.bees~feeding.habitat.size)
boxplot(count.specialized.bees~feeding.habitat.size)
boxplot(count.generalist.bees~min.distance.feed)
boxplot(count.specialized.bees~min.distance.feed)

# visualize the relation between bees and flowers
plot(count.flowers, count.generalist.bees)
plot(count.wildflowers, count.generalist.bees)
plot(count.cyan.flowers, count.specialized.bees)
plot(sum.crop.seeds, count.generalist.bees)


# -------------------- Anova --------------------

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

# test for correlations between flowers and bees using Spearman correlations for non-normally distributed data
cor(count.flowers, count.generalist.bees, method = "spearman")
cor.test(count.flowers, count.generalist.bees, method = "spearman")
cor(count.wildflowers, count.generalist.bees, method = "spearman")
cor.test(count.wildflowers, count.generalist.bees, method = "spearman")
cor(count.cyan.flowers, count.specialized.bees, method = "spearman")
cor.test(count.cyan.flowers, count.specialized.bees, method = "spearman")
cor(sum.crop.seeds, count.generalist.bees, method = "spearman")
cor.test(sum.crop.seeds, count.generalist.bees, method = "spearman")


# ------------------- Garbage --------------------

# anova on lm
model_lm<-lm(count.bees~feeding.habitat.number)
model_lm<-lm(count.bees~actual.feeding.habitat.number)

hist(resid(model_lm))
shapiro.test(resid(model_lm))

model_aov<-anova(model_lm)
summary(model_aov)

# anova on glm ?
model_glm<-glm(count.bees~breeding.habitat.number, family = poisson())
model_glm_aov<-anova(model_glm)
summary(model_glm_aov)