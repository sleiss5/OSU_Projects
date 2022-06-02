library(tidyverse)
library(Lahman)
setwd("~/OSU/F2021/ST 623/Project/")
##Question 1
Season_by_Team <- Teams ##Data Set from Lahman Package
Season_by_Team <- Season_by_Team%>%subset(yearID >= 1998)
Season_by_Team <- Season_by_Team %>% mutate(Playoffs = ifelse(DivWin == "Y" | WCWin == "Y",1,0),WS_Appearance = ifelse(LgWin == "Y",1,0),WS_Champ = ifelse(WSWin == "Y",1,0) )
Season_Offensive <- Season_by_Team[Season_by_Team$yearID != 2020,c(15:21,26,44,49:51)] 
mod_inital <- glm(Playoffs ~ R+AB+H+X2B+X3B+HR,data=Season_Offensive,family=binomial)
summary(mod_inital)
##Overdispersion?
mod_inital$deviance/mod_inital$df.residual ##Not bad, most likely no overdispersion
mod_inital_ov <- glm(Playoffs ~ R+AB+H+X2B+X3B+HR,data=Season_Offensive,family=quasibinomial)
summary(mod_inital_ov)
##Overdispersion not present and thus not needed for this model 
## Code that got deleted: Using R,H,AB with the interactions between R/AB and H/AB. The analysis was done for these before it was found that the R and AB model is simpiler and fits the model better
##Model with only Runs, and AB
mod_reduced <- glm(Playoffs ~ R+AB,data=Season_Offensive,family=binomial)
summary(mod_reduced)
mod_reduced$deviance/mod_reduced$df.residual ##Even better evidence of no overdispersion
anova(mod_reduced,mod_inital,test="LRT")##Reduced should be used and will be the base model going on from here
##Interaction terms?
mod_reduced_int <- glm(Playoffs ~ R+AB+R*AB,data=Season_Offensive,family=binomial)
summary(mod_reduced_int)
mod_reduced_int$deviance/mod_reduced_int$df.residual ##Less evidence of overdispersion and lower AIC
anova(mod_reduced,mod_reduced_int,test="LRT")## Somewhat signifcant that the interaction helps the model fit
##Just Runs?
mod_reduced_alot <- glm(Playoffs ~ R,data=Season_Offensive,family=binomial)
summary(mod_reduced_alot)
mod_reduced_alot$deviance/mod_reduced_alot$df.residual ##Overdispersion possibility higher
anova(mod_reduced_alot, mod_reduced_int,test="LRT") ##The interation reduced model should be used
anova(mod_reduced_alot, mod_inital,test="LRT") ##Inital fits better, but the inital with interaction fits better than both 
##Interation of all?
mod_all_int <- glm(Playoffs ~ R*AB*H*X2B*X3B*HR,data=Season_Offensive,family=binomial)
summary(mod_all_int)
anova(mod_all_int,mod_reduced_int,test="LRT")##This model makes no practical sense and shouldn't be used since it is not significant 
##Transformations of variables?
Season_Offensive <- Season_Offensive%>% mutate(RperAB = R/AB, HperAB = H/AB)
new_mod <- glm(Playoffs ~RperAB + HperAB,data=Season_Offensive,family=binomial)
summary(new_mod)
new_mod$deviance/new_mod$df.residual##No evidence of overdispersion
##Interaction makes no sense but it will be tried:
new_mod_int <- glm(Playoffs ~RperAB + HperAB+RperAB*HperAB,data=Season_Offensive,family=binomial)
summary(new_mod_int)
new_mod_int$deviance/new_mod_int$df.residual##No evidence of overdispersion
anova(new_mod_int,new_mod,test="LRT")##good, the model without the interaction should be used
new_mod_red <- glm(Playoffs ~RperAB,data=Season_Offensive,family=binomial)
summary(new_mod_red)
anova(new_mod,new_mod_red,test="LRT")##good, the model with both should be used 


##Runs are approximatley normally distributed 
##Hits are also approximatley normally distributed
##Aswell as at bats
##All variables being considered have are approximatley normally distributed 
## Trying different links:
##Using the reduced model with 2 of the 2 way interations, and will also work with the model that uses runs and hits per at bat 
mod1 <- glm(Playoffs ~ R+AB+R*AB,data=Season_Offensive,family=binomial)
mod2 <- glm(Playoffs ~RperAB + HperAB,data=Season_Offensive,family=binomial)
mod1.1 <- glm(Playoffs ~ R+AB+R*AB,data=Season_Offensive,family=binomial(link="probit"))
summary(mod1.1)##No overdispersion and smallest AIC so far 
mod1.1.reduced <- glm(Playoffs ~ R+AB,data=Season_Offensive,family=binomial(link="probit"))
summary(mod1.1.reduced)
anova(mod1.1.reduced,mod1.1,test="LRT") ##Model with interactions shoudld be used
mod2.1 <- glm(Playoffs ~RperAB + HperAB,data=Season_Offensive,family=binomial(link="probit"))
summary(mod2.1)##No overdispersion and smallest AIC so far 
mod2.1.2<- glm(Playoffs ~RperAB + HperAB+RperAB*HperAB,data=Season_Offensive,family=binomial(link="probit"))
summary(mod2.1.2)##No overdispersion and smallest AIC so far 
anova(mod2.1.2,mod2.1,test="LRT") ##Reduced model without the interaction should be used 
mod1.2 <- glm(Playoffs ~ R+AB+R*AB,data=Season_Offensive,family=binomial(link="cloglog"))
summary(mod1.2)##No overdispersion and smallest AIC so far 
mod1.2.red <- glm(Playoffs ~ R+AB,data=Season_Offensive, family = binomial(link="cloglog"))
summary(mod1.2.red)
anova(mod1.2,mod1.2.red,test="LRT")##Model with interaction should be used
mod2.2 <- glm(Playoffs ~RperAB + HperAB,data=Season_Offensive,family=binomial(link="cloglog"))
summary(mod2.2)##Slight overdispersion
mod2.2.more <- glm(Playoffs ~RperAB + HperAB+RperAB*HperAB,data=Season_Offensive,family=binomial(link="cloglog"))
summary(mod2.2.more)##No overdispersion and smallest AIC so far 
anova(mod2.2.more,mod2.2,test="LRT") ##Interactions fit better

AIC.mod1 <- c(mod1$aic, mod1.1$aic, mod1.2$aic)
AIC.mod2 <- c(mod2$aic, mod2.1$aic, mod2.2.more$aic)
## It can be seen that for the first model using the outright R,AB - the probit model has the lowest AIC barely; however it is still high and is not a very large reduction to justify the computational complexity that comes with the probit model.
##It can be seen that for the R/H per AB has the best fit when using the cloglog link and interactions are used. Again, for computational ease and consistency, the logistic model will still be used for any further analysis.
##It should also be noted that the fit using the pure R and AB stats will fit very slightly better than the corresponding models when R/H per AB are used.

## Checking residuals and other parts of the model
## Teams Runs, Hits and AB are independent of other teams
##Since Overdispersion is not present, it can also be assumed that the teams(observations) are independent of each other, 
##the logistic model is also probably correct. Assume that the base probability of going to the playoffs is the same for all teams. 
##This last assumption is realistically not true but is a much more complicated aspect to analyse. 
##This is where: Does previous seasons win percentage effect the probability of making the playoffs? 
##THis would be an intresting thing to explore - as an offset or a covariate - but requires a lot of data cleaning and manipulation to get the data in a proper form to see if this works.

## Still using Model 1 and Model 2, the logistic models
##Residual models
rd.mod1 <- resid(mod1,"deviance")
rd.mod2 <- resid(mod2,"deviance")
plot(mod1$fitted.values,rd.mod1)##Residual Problems, most likely due to the correlation and dependence of the explanatory variables 
df <- data.frame(Fit = mod1$fitted.values, Residuals =rd.mod1,Season = Season_by_Team[Season_by_Team$yearID != 2020,1])
p1 <- ggplot(df, aes(x=Fit,y=Residuals,col=as.factor(Season)))+
  geom_point(alpha=0.5)+
  ggtitle("Model 1")+
  labs(col="Season")+
  theme(legend.position = "none")
df2 <- data.frame(Fit = mod2$fitted.values, Residuals =rd.mod2,Season = Season_by_Team[Season_by_Team$yearID != 2020,1])
p2 <- ggplot(df2, aes(x=Fit,y=Residuals,col=as.factor(Season)))+
  geom_point(alpha=0.5)+
  ggtitle("Model 2")+
  labs(col="Season")
ggarrange(p1,p2,ncol=1,common.legend = TRUE,legend = "right")

plot(mod2$fitted.values,rd.mod2)##Residual Problems, most likely due to the correlation and dependence of the explanatory variables 
qqnorm(rd.mod1)
qqnorm(rd.mod2)
## It should be noted that the residual plot does show that the variance does tend to stay constant
anova(mod1)
anova(mod2)
rd.mod1.1 <- resid(mod1,"working") ##Constant variance problems when the working residuals are used
plot(mod1$fitted.values, rd.mod1.1)
qqnorm(rd.mod1.1)
rd.mod1.2 <- resid(mod1,"pearson") ##Also appears to be constant variance problems when using pearson residuals, but this is not as bad as when the working residals are used
plot(mod1$fitted.values, rd.mod1.2)
qqnorm(rd.mod1.2)
rd.mod2.1 <- resid(mod2,"working") ##Constant variance problems when the working residuals are used 
plot(mod2$fitted.values, rd.mod2.1)
qqnorm(rd.mod2.1)
rd.mod2.2 <- resid(mod2,"pearson")
plot(mod2$fitted.values, rd.mod2.2)
qqnorm(rd.mod2.2)
##There may be season correlation. Since the probability of a specific team making the playoffs depends on the probability of another team in their division - There are 15 teams per league and 5 teams per division per league. Since only one team in each division will get a playoff spot, plus one additional playoff spot, if aother team in the division has already gotten the playoff spot, there is only one other possible playoff spot for a team to get. Below will be some residual plots for chosen seasons to see if the seasons are doing anything
plot(mod1$fitted.values[1:30],rd.mod1[1:30])
plot(mod1$fitted.values[31:60],rd.mod1[31:60])
plot(mod1$fitted.values[61:90],rd.mod1[61:90])
plot(mod1$fitted.values[91:120],rd.mod1[91:120])
plot(mod1$fitted.values[121:150],rd.mod1[121:150])
plot(mod1$fitted.values[151:180],rd.mod1[151:180])
plot(mod1$fitted.values[181:210],rd.mod1[181:210])
plot(mod1$fitted.values[211:240],rd.mod1[211:240])
plot(mod1$fitted.values[241:270],rd.mod1[241:270])
plot(mod1$fitted.values[271:300],rd.mod1[271:300])
## It should be noted that the residual plot does show that the variance does tend to stay constant

## Testing fit of the model using the 2019 data since there were large errors with the 2020 data and 2021 season has not been made available 
Offensive_2019 <- Season_by_Team %>% subset(yearID == 2019)%>% mutate(RperAB = R/AB, HperAB = H/AB)
Offensive_2019<- Offensive_2019[,c(15,16,17,49,53,54)]
mod1.coefs <- mod1$coefficients
new.data <- data.frame(Inter = rep(1,30), Offensive_2019$R, Offensive_2019$AB, Offensive_2019$R*Offensive_2019$AB)
log_odds <- as.matrix(new.data) %*% t(t(mod1.coefs))
odds <- exp(log_odds)
probs <- odds/(1+odds)
team_probs_predicted <- data.frame(Team = Season_by_Team[Season_by_Team$yearID == 2019,4], Playoff_Prob_mod_1 = probs,Playoffs = Season_by_Team[Season_by_Team$yearID == 2019,49],World_Series = Season_by_Team[Season_by_Team$yearID == 2019,51])


mod2.coefs <- mod2$coefficients
new.data.2 <- data.frame(Inter = rep(1,30), Offensive_2019$RperAB, Offensive_2019$HperAB)
log_odds_2 <- as.matrix(new.data.2) %*%t(t(mod2.coefs))
odds_2 <- exp(log_odds_2)
probs_2 <- odds_2/(1+odds_2)
team_probs_predicted <- data.frame(Team = Season_by_Team[Season_by_Team$yearID == 2019,4], Playoff_Prob_mod_1 = probs,Playoff_Prob_mod_2 = probs_2,Playoffs = Season_by_Team[Season_by_Team$yearID == 2019,49],World_Series = Season_by_Team[Season_by_Team$yearID == 2019,51])

## Question 2
##Using the second model as it is simplier and easier to interprete, will try working with first model, but the interaction term may cause problems.
summary(Season_Offensive$HperAB)
sd.HpB <- sd(Season_Offensive$HperAB)
X.HpB <- mean(Season_Offensive$HperAB)
summary(Season_Offensive$RperAB)
Y.RpB <- mean(Season_Offensive$RperAB)
sd.RpB <- sd(Season_Offensive$RperAB)
##If a team has an average team batting average(Hits per at bat) of 0.2603, what Runs per at bat percentage would they need for their probability of going to the playoffs to be 50%
PR_50 <- -(mod2.coefs[1]+(mod2.coefs[3]*X.HpB))/mod2.coefs[2]
PR_50
PR_90 <- (log(.9/.1)-(mod2.coefs[1]+(mod2.coefs[3]*X.HpB)))/mod2.coefs[2]
PR_90
##In reverse, if a team has an average Runs per at bat, what team batting average do they need to acheive:
HR_50 <- -(mod2.coefs[1]+(mod2.coefs[2]*Y.RpB))/mod2.coefs[3]
HR_50
HR_90 <- (log(.1/.9)-mod2.coefs[1]-(mod2.coefs[2]*Y.RpB))/mod2.coefs[3]
HR_90

##Using the second model to create confidence intervals:
V.2 <- vcov(mod2)
a.2 <- mod2.coefs[1]
c.2 <- mod2.coefs[3]*X.HpB
b.2 <- mod2.coefs[2]
g.2.prime <- c(-1/b.2, (a.2+c.2)/b.2^2)
PR.var <- g.2.prime%*%V.2[1:2,1:2]%*%g.2.prime
PR.sd <- sqrt(PR.var)
PR_50 +c(-1,1)*qnorm(.975)*PR.sd

V <- vcov(mod2)
a <- mod2.coefs[1]
b <- mod2.coefs[3]
c <- mod2.coefs[2]*Y.RpB
g.prime <- c(-1/b, (a+c)/b^2)
PH.var <- g.prime%*%V[c(1,3),c(1,3)]%*%g.prime
PH.sd <- sqrt(PH.var)
HR_50 +c(-1,1)*qnorm(.975)*PH.sd

##Similar analysis for the first model, but the number of AB will be set
## Comment that the LD's were found, however, the confidence intervals do not make practical sense because they have negative lower bounds and very high - realistically unabtainable - upper bounds 
AB_Avg <- mean(Season_Offensive$AB)
Playoff_50 <- -(mod1.coefs[1]+(mod1.coefs[3]*AB_Avg))/(mod1.coefs[2] + (mod1.coefs[4]*AB_Avg))
Playoff_50
V <- vcov(mod1)
a <- mod1.coefs[1]
c <- mod1.coefs[3]*AB_Avg
b <- mod1.coefs[2]+(mod1.coefs[4]*AB_Avg)
g.prime <- c(-1/b, (a+c)/b^2)
Play.var <- g.prime%*%V[1:2,1:2]%*%g.prime
Play.sd <- sqrt(Play.var)
Playoff_50 +c(-1,1)*qnorm(.975)*Play.sd ## Not very useful since a team cannot score negative runs and the most runs a team has scored in a season are 1009

Playoff_90 <- (log(.9/.1)-(mod1.coefs[1]+(mod1.coefs[3]*AB_Avg)))/(mod1.coefs[2] + (mod1.coefs[4]*AB_Avg))
Playoff_90
const <- log(.9/.1)
g.prime <- c(-1/b, (a+c-const)/b^2)
Play.var <- g.prime%*%V[1:2,1:2]%*%g.prime
Play.sd <- sqrt(Play.var)
Playoff_90 +c(-1,1)*qnorm(.975)*Play.sd ## Not very useful once again

##Question 3
Season_by_Team_Updated <- Season_by_Team %>% subset(yearID != 2020)
HR_mod1 <- glm(HR ~ lgID-1, data=Season_by_Team_Updated, poisson)
summary(HR_mod1)##Evidence of overdispersion
HR_Over_mod <- glm(HR ~ lgID-1, data=Season_by_Team_Updated, quasipoisson)
summary(HR_Over_mod)
resid.mod.leg.over <- resid(HR_Over_mod,"deviance")
plot(HR_Over_mod$fitted.values,resid.mod.leg.over)
HR_mod1_offset <- glm(HR ~lgID -1 +offset(log(BPF)),data=Season_by_Team_Updated,poisson)
summary(HR_mod1_offset)##Slightly better fit but still evidence of overrdispersion
##Offset of ballpark factor - Homeruns
HR_mod1_offset_over <- glm(HR ~lgID -1 +offset(log(BPF)),data=Season_by_Team_Updated,quasipoisson)
summary(HR_mod1_offset_over)##Slightly better fit but still evidence of overrdispersion
mod.resids.offset.over <- resid(HR_mod1_offset_over,"deviance")
plot(HR_mod1_offset_over$fitted.values,mod.resids.offset.over) ##Looks pretty good except maybe towards the end 
qqnorm(mod.resids.offset.over) ##Looks pretty good except maybe near the tails
rate_AL <- exp(HR_mod1_offset_over$coefficients[1]) ##AL has a higher HR rate than NLs
rate_NL <- exp(HR_mod1_offset_over$coefficients[2])


## Division?
HR_dvmod <- glm(HR ~ divID,data=Season_by_Team_Updated,poisson)
summary(HR_dvmod)##Fits worse than just the leagues and has bad overdispersion
HR_div_over_mod <- glm(HR ~ divID,data=Season_by_Team_Updated,quasipoisson)
summary(HR_div_over_mod)
resid.mod.div.over <- resid(HR_div_over_mod,"deviance")
plot(HR_div_over_mod$fitted.values,resid.mod.div.over)
##offset
HR_div_overoff_mod <- glm(HR ~ divID+offset(log(BPF)),data=Season_by_Team_Updated,quasipoisson)
summary(HR_div_overoff_mod)
rate_C <- exp(HR_div_overoff_mod$coefficients[1]) 
rate_E <- exp(HR_div_overoff_mod$coefficients[1]+HR_div_overoff_mod$coefficients[2])##East has the highest rate
rate_W <- exp(HR_div_overoff_mod$coefficients[1]+HR_div_overoff_mod$coefficients[3])


resid.mod.div.off.over <- resid(HR_div_overoff_mod,"deviance")
plot(HR_div_overoff_mod$fitted.values,resid.mod.div.off.over) ##Seems to have a nonconstant varaiance issue near the higher fitted values

##Division and League?
HR_ldiv_mod <- glm(HR ~ lgID + divID,data=Season_by_Team_Updated,poisson)
summary(HR_ldiv_mod)##Not great fit and evidence of overdispersion
resid.1 <- resid(HR_ldiv_mod,"deviance")
plot(HR_ldiv_mod$fitted.values,resid.1)
qqnorm(resid.1)##Residuals look good 
HR_ldiv_mod_over <- glm(HR ~ lgID + divID,data=Season_by_Team_Updated,quasipoisson)
summary(HR_ldiv_mod_over)
resid.1 <- resid(HR_ldiv_mod_over,"deviance")
plot(HR_ldiv_mod_over$fitted.values,resid.1)
qqnorm(resid.1)##Residuals look good 

HR_ldiv_inter_over <- glm(HR ~ lgID + divID + lgID*divID,data=Season_by_Team_Updated,quasipoisson)
summary(HR_ldiv_inter_over)
anova(HR_ldiv_inter_over,HR_ldiv_mod_over,test="F")##Interaction should be used
resid.1 <- resid(HR_ldiv_inter_over,"deviance")
plot(HR_ldiv_inter_over$fitted.values,resid.1)
qqnorm(resid.1)##Residuals look good 
Season_by_Team_Updated<- Season_by_Team_Updated %>% mutate(lgDiv = ifelse(lgID == "NL" & divID == "W", "NW",
                                                                    ifelse(lgID == "NL" & divID == "C", "NC",
                                                                     ifelse(lgID == "NL" & divID == "E", "NE",
                                                                      ifelse(lgID == "AL" & divID == "W", "AW",
                                                                       ifelse(lgID == "AL" & divID == "C","AC","AE"))))))
HR_combo_mod <- glm(HR ~ as.factor(lgDiv)-1, data=Season_by_Team_Updated, poisson)
summary(HR_combo_mod) ##overdispersion
HR_combo_mod_over <- glm(HR ~ as.factor(lgDiv)-1, data=Season_by_Team_Updated, quasipoisson)
summary(HR_combo_mod_over)
resid.1 <- resid(HR_combo_mod_over,"deviance")
plot(HR_combo_mod_over$fitted.values,resid.1)
qqnorm(resid.1)##Residuals look good 
##Adding Offset
HR_combo_mod_off_over <- glm(HR ~ as.factor(lgDiv)+offset(log(BPF))-1, data=Season_by_Team_Updated, quasipoisson)
summary(HR_combo_mod_off_over)
resid.1 <- resid(HR_combo_mod_off_over,"deviance")
plot(HR_combo_mod_over$fitted.values,resid.1)
qqnorm(resid.1)
rate_AC <- exp(HR_combo_mod_off_over$coefficients[1])
rate_AE <- exp(HR_combo_mod_off_over$coefficients[2])##Has the highest rate
rate_AW <- exp(HR_combo_mod_off_over$coefficients[3])
rate_NC <- exp(HR_combo_mod_off_over$coefficients[4])
rate_NE <- exp(HR_combo_mod_off_over$coefficients[5])
rate_NW <- exp(HR_combo_mod_off_over$coefficients[6])


## This may be too general, lets try it with franchise?
HR_lgmod <- glm(HR ~ franchID-1,data=Season_by_Team_Updated,poisson)
summary(HR_lgmod)##There is overdispersion but the model does seem to fit better than just the league
HR_lgmod_over <- glm(HR ~ franchID-1,data=Season_by_Team_Updated,quasipoisson)
summary(HR_lgmod_over)
resid.1 <- resid(HR_lgmod_over,"deviance")
plot(HR_lgmod_over$fitted.values,resid.1)
qqnorm(resid.1)##There is nonconstant variance since each team has their own variance 
##Adding offset
HR_lgmod_over_off <- glm(HR ~ franchID+offset(log(BPF))-1,data=Season_by_Team_Updated,quasipoisson)
summary(HR_lgmod_over_off)
resid.1 <- resid(HR_lgmod_over_off,"deviance")
plot(HR_lgmod_over_off$fitted.values,resid.1)
qqnorm(resid.1)##Offset helps the residuals a lot, there appears to possibly be a single outlier
HR_lgmod_off <- glm(HR ~ franchID+offset(log(BPF))-1,data=Season_by_Team_Updated,poisson)
summary(HR_lgmod_off)##This has the best fit! but still not a great fit at all 
rates <- exp(HR_lgmod_over_off$coefficients)
Team_rates <- data.frame(HR_Rate = rates,Team = c("ANA","ARI","ATL","BAL","BOS","CHC","CHW","CIN","CLE","COL","DET","FLA","HOU","KCR","LAD","MIL","MIN","NYM","NYY","OAK","PHI","PIT","SDP","SEA","SFG","STL","TBD","TEX","TOR","WSN"))
rownames(Team_rates) <- NULL
Team_rates <- Team_rates[order(Team_rates$HR_Rate,decreasing = F),] ##Homerun rates for each team. per game?
