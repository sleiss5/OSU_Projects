X04291_0001_Data <- read_por("C:/Users/GSLEI/Downloads/04291-0001-Data.por")

require(dplyr)

Athlete_status <- X04291_0001_Data[,335]
Greek_status <- X04291_0001_Data[,9]
GPA <- X04291_0001_Data[,305]

interest <- cbind(Athlete_status, Greek_status, GPA)
##Deleting NAs _> 10904 to 10621
comp_int <- interest[complete.cases(interest),]
##Deleting unknown GPAs 10621 to 10472
comp_int_GPA <- comp_int[comp_int$F5 != 10,]
GPA_1 <- comp_int_GPA[comp_int_GPA$F5 == 1,]
GPA_1$F5[GPA_1$F5 == 1]<-4.00
GPA_2 <- comp_int_GPA[comp_int_GPA$F5 == 2,]
GPA_2$F5[GPA_2$F5 == 2]<- 3.67
GPA_3 <- comp_int_GPA[comp_int_GPA$F5 == 3,]
GPA_3$F5[GPA_3$F5 == 3]<- 3.33
GPA_4 <- comp_int_GPA[comp_int_GPA$F5 == 4,]
GPA_4$F5[GPA_4$F5 == 4]<- 3.00
GPA_5 <- comp_int_GPA[comp_int_GPA$F5 == 5,]
GPA_5$F5[GPA_5$F5 == 5]<- 2.67
GPA_6 <- comp_int_GPA[comp_int_GPA$F5 == 6,]
GPA_6$F5[GPA_6$F5 == 6]<- 2.33
GPA_7 <- comp_int_GPA[comp_int_GPA$F5 == 7,]
GPA_7$F5[GPA_7$F5 == 7]<- 2.00
GPA_8 <- comp_int_GPA[comp_int_GPA$F5 == 8,]
GPA_8$F5[GPA_8$F5 == 8]<- 1.67


GPA_adjust <- rbind(GPA_1, GPA_2, GPA_3, GPA_4, GPA_5, GPA_6, GPA_7, GPA_8)

## Broken on Greek Life
Greek_yes <- GPA_adjust[GPA_adjust$A5 == 1,]
Greek_no <- GPA_adjust[GPA_adjust$A5 == 0,]

## Broken on HS Athletics
Varsity <- GPA_adjust[GPA_adjust$G13 == 2,]
JV <- GPA_adjust[GPA_adjust$G13 == 3,]
Fan <- GPA_adjust[GPA_adjust$G13 == 1,]

##Post stratification with unknown N and unknown Nh, however since the SRS is large enough, nh/n will be approximatley equal to Nh/N

##Average GPA for Greek life for post vs SRS:
ng <- length(Greek_yes[,3]) ##1307
nng <- length(Greek_no[,3])##9146
n <- length(GPA_adjust[,3])##10453
GPA_greek <- sum(Greek_yes[,3])/ng
##3.238439
GPA_nogreek <- sum(Greek_no[,3])/nng
##3.239225
GPA_post <- ((ng/n)*GPA_greek)+((nng/n)*GPA_nogreek)
##3.239127
GPA_srs <- sum(GPA_adjust[,3])/n 
##3.239127
var_GPA_post <- ((ng/n)*(var(as.numeric(Greek_yes[,3]))/n)) + ((nng/n)*(var(as.numeric(Greek_no[,3]))/n))
SE_GPA_post <- sqrt(var_GPA_post)
##0.005499742
lb <- GPA_post - (1.96*SE_GPA_post)
ub <- GPA_post + (1.96*SE_GPA_post)
ci95 <- c(lb,ub)
##(3.228347 3.249906)
##Overestimate since students with lower GPAs would be more likely not to report their GPA and thus get removed from the sample


##Average GPA for HS Athletes
nvar <- length(Varsity[,3])
##5226
njv <- length(JV[,3])
##1633
nfan <- length(Fan[,3])
##3594
GPA_Var <- sum(Varsity[,3])/nvar
##3.234059
GPA_JV <- sum(JV[,3])/njv
##3.22504
GPA_Fan <- sum(Fan[,3])/nfan
##3.252896
GPA_Sport_post <- ((nvar/n)*GPA_Var)+((njv/n)*GPA_JV)+((nfan/n)*GPA_Fan)
##3.239127
var_sport_GPA <- ((nvar/n)*(var(as.numeric(Varsity[,3]))/n)) + ((njv/n)*(var(as.numeric(JV[,3]))/n)) + ((nfan/n)*(var(as.numeric(Fan[,3]))/n))
SE_Sports_GPA <- sqrt(var_sport_GPA)
##0.005499065
lb <- GPA_Sport_post - (1.96*SE_Sports_GPA)
ub <- GPA_Sport_post + (1.96*SE_Sports_GPA)
ci95 <- c(lb,ub)
## (3.228348 3.249905)

## Proportion of athletes in Greek life
p_ath_greek <- length(Greek_yes[Greek_yes$G13 == 2| Greek_yes$G13 == 3, 2])/ng ## prop of students in greek life who were athletes 
##0.7773527

## Proportion of students who join greek life 
p_var_greek <- length(Varsity[Varsity$A5 ==1,2])/nvar ##Prop of varsity athletes that join greek life
##0.1615002
p_jv_greek <- length(JV[JV$A5 == 1,2])/njv
##0.1053276
p_fans_greek <- length(Fan[Fan$A5 == 1, 2])/nfan
##0.08096828
p_greek <- ((nvar/n)*p_var_greek)+((njv/n)*p_jv_greek)+((nfan/n)*p_fans_greek)
## 0.1250359
var_prop_greek <- (((nvar/n)^2)*((p_var_greek*(1-p_var_greek))/(nvar - 1))) + (((njv/n)^2)*((p_jv_greek*(1-p_jv_greek))/(njv - 1))) + (((nfan/n)^2)*((p_fans_greek*(1-p_fans_greek))/(nfan - 1)))
se_pop_greek <- sqrt(var_prop_greek)
##0.003214901
lb <- p_greek - (1.96*se_pop_greek)
ub <- p_greek + (1.96*se_pop_greek)
ci95 <- c(lb,ub)
##(0.1187347 0.1313371)
