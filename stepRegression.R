http://stats.stackexchange.com/questions/214682/stepwise-regression-in-r-how-does-it-work

step(lm(mpg~wt+drat+disp+qsec,data=mtcars),direction="both")
step(lm(mpg~wt+drat+disp+qsec,data=mtcars),direction="backward")


#install.packages('C:/RPAckages/leaps_3.0.zip', lib='C:/RPackages/mypackages',repos = NULL)

looking at all 15 possible lm models. 


library(leaps)
tmp<-regsubsets(mpg ~ wt + drat + disp + qsec, data=mtcars, nbest=1000, really.big=T, intercept=F)
all.mods <- summary(tmp)[[1]]
all.mods <- lapply(1:nrow(all.mods, function(x)as.formula(paste("mpg~", paste(names(which(all.mods[x,])), collapse="+"))))

head(all.mods)


all.lm<-lapply(all.mods, lm, mtcars)
AIC values for each of the model are extracted with:
sapply(all.lm, extractAIC)[2,]


--------------------------------------------------------------------
-- STEP ON LOGISTIC REGRESION

model <- glm(formula= vs ~ ., data=mtcars, family=binomial)
summary(model)

model <-step(model, direction='both', criterion='BIC')
model

#only qsec is relevant

#What is the probability that this engine is S -Straight
newdata = data.frame(wt = 2.1, qsec = 17,wt=3500 )
predict(model, newdata, type="response")

# more qsec, more probability of VS
newdata = data.frame(wt = 2.1, qsec = 17)
predict(model, newdata, type="response")

--------------------------------------------------------------------






