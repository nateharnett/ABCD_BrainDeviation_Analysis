### Analyze the Normative Model Data from the ABCD Analyses ###

#


# Load your libraries
library(olsrr)
library(tidyverse)
library(dplyr)
library(relaimpo)
library(caTools)
library(skimr)
library(plyr)
library(QuantPsyc)
options(scipen = 999)

#Specify a priori regions
ThreatRegions = c('lh_G.S_frontomargin_thickness', 'lh_G.S_transv_frontopol_thickness', 'lh_G.S_cingul.Ant_thickness', 
                  'lh_G.S_cingul.Mid.Ant_thickness', 'lh_G.S_cingul.Mid.Post_thickness', 'lh_G_front_inf.Opercular_thickness', 
                  'lh_G_front_inf.Orbital_thickness', 'lh_G_front_inf.Triangul_thickness', 'lh_G_front_middle_thickness', 
                  'lh_G_front_sup_thickness', 'lh_G_Ins_lg.S_cent_ins_thickness', 'lh_G_insular_short_thickness', 
                  'lh_G_orbital_thickness', 'lh_G_rectus_thickness', 'lh_G_subcallosal_thickness', 
                  'lh_S_circular_insula_ant_thickness', 'lh_S_circular_insula_inf_thickness', 'lh_S_circular_insula_sup_thickness', 
                  'lh_S_front_inf_thickness', 'lh_S_front_middle_thickness', 'lh_S_front_sup_thickness', 'lh_S_orbital_lateral_thickness', 
                  'lh_S_orbital_med.olfact_thickness', 'lh_S_orbital.H_Shaped_thickness', 'lh_S_suborbital_thickness', 'Left.Amygdala', 
                  'Left.Hippocampus', 'rh_G.S_frontomargin_thickness', 'rh_G.S_transv_frontopol_thickness', 'rh_G.S_cingul.Ant_thickness', 
                  'rh_G.S_cingul.Mid.Ant_thickness', 'rh_G.S_cingul.Mid.Post_thickness', 'rh_G_front_inf.Opercular_thickness', 
                  'rh_G_front_inf.Orbital_thickness', 'rh_G_front_inf.Triangul_thickness', 'rh_G_front_middle_thickness', 
                  'rh_G_front_sup_thickness', 'rh_G_Ins_lg.S_cent_ins_thickness', 'rh_G_insular_short_thickness', 'rh_G_orbital_thickness', 
                  'rh_G_rectus_thickness', 'rh_G_subcallosal_thickness', 'rh_S_circular_insula_ant_thickness', 'rh_S_circular_insula_inf_thickness', 
                  'rh_S_circular_insula_sup_thickness', 'rh_S_front_inf_thickness', 'rh_S_front_middle_thickness', 'rh_S_front_sup_thickness', 
                  'rh_S_orbital_lateral_thickness', 'rh_S_orbital_med.olfact_thickness', 'rh_S_orbital.H_Shaped_thickness', 'rh_S_suborbital_thickness', 
                  'Right.Amygdala', 'Right.Hippocampus', 'rh_Lat_Fis.ant.Horizont_thickness', 'rh_Lat_Fis.ant.Vertical_thickness', 'rh_Lat_Fis.post_thickness', 
                  'lh_Lat_Fis.ant.Horizont_thickness', 'lh_Lat_Fis.ant.Vertical_thickness', 'lh_Lat_Fis.post_thickness', 'lh_S_pericallosal_thickness', 
                  'rh_S_pericallosal_thickness')

#Load the data from a CSV
HCP_DataPre= read.csv('data.csv')
HCP_DataPre = HCP_DataPre[, -2]
HCP_Data = subset(HCP_DataPre, iqc_t1_1_qc_score!=0)

HCP_Data$demo_comb_income_v2_l_y <- na_if(HCP_Data$demo_comb_income_v2_l_y, 777)
HCP_Data$demo_comb_income_v2_l_y <- na_if(HCP_Data$demo_comb_income_v2_l_y, 999)
HCP_Data$Base_ADI <- na_if(HCP_Data$Base_ADI, 777)
HCP_Data$Base_ADI <- na_if(HCP_Data$Base_ADI, 999)
HCP_Data$Base_Racism <- na_if(HCP_Data$Base_Racism, 777)
HCP_Data$Base_Racism <- na_if(HCP_Data$Base_Racism, 999)
HCP_Data$dim_matrix_q1<- na_if(HCP_Data$dim_matrix_q1, 777)
HCP_Data$dim_matrix_q1<- na_if(HCP_Data$dim_matrix_q1, 999)
HCP_Data$dim_matrix_q2<- na_if(HCP_Data$dim_matrix_q2, 777)
HCP_Data$dim_matrix_q2<- na_if(HCP_Data$dim_matrix_q2, 999)
HCP_Data$dim_matrix_q3<- na_if(HCP_Data$dim_matrix_q3, 777)
HCP_Data$dim_matrix_q3<- na_if(HCP_Data$dim_matrix_q3, 999)
HCP_Data$dim_matrix_q4<- na_if(HCP_Data$dim_matrix_q4, 777)
HCP_Data$dim_matrix_q4<- na_if(HCP_Data$dim_matrix_q4, 999)
HCP_Data$dim_matrix_q5<- na_if(HCP_Data$dim_matrix_q4, 777)
HCP_Data$dim_matrix_q5<- na_if(HCP_Data$dim_matrix_q4, 999)
HCP_Data$dim_matrix_q6<- na_if(HCP_Data$dim_matrix_q4, 777)
HCP_Data$dim_matrix_q6<- na_if(HCP_Data$dim_matrix_q4, 999)
HCP_Data$dim_matrix_q7<- na_if(HCP_Data$dim_matrix_q4, 777)
HCP_Data$dim_matrix_q7<- na_if(HCP_Data$dim_matrix_q4, 999)
HCP_Data$Discrimination <- (HCP_Data$dim_matrix_q1+HCP_Data$dim_matrix_q2+HCP_Data$dim_matrix_q3+HCP_Data$dim_matrix_q4+HCP_Data$dim_matrix_q5+HCP_Data$dim_matrix_q6+HCP_Data$dim_matrix_q7)


#Establish matrices you'll need later
CoeffCols = 5
RaceEffect_Univariate_NoSite <- matrix(ncol=CoeffCols, nrow=(152))
IncomeEffect_Univariate_NoSite <- matrix(ncol=CoeffCols, nrow=(152))
ADIEffect_Univariate_NoSite <- matrix(ncol=CoeffCols, nrow=(152))
SREffect_Univariate_NoSite <- matrix(ncol=CoeffCols, nrow=(152))
DisEffect_Univariate_NoSite <- matrix(ncol=CoeffCols, nrow=(152))

RaceEffect_Univariate_Site <- matrix(ncol=CoeffCols, nrow=(152))
IncomeEffect_Univariate_Site <- matrix(ncol=CoeffCols, nrow=(152))
ADIEffect_Univariate_Site <- matrix(ncol=CoeffCols, nrow=(152))
SREffect_Univariate_Site <- matrix(ncol=CoeffCols, nrow=(152))
DisEffect_Univariate_Site <- matrix(ncol=CoeffCols, nrow=(152))

RaceEffect_BLR <- matrix(ncol=CoeffCols, nrow=(152))
IncomeEffect_BLR <- matrix(ncol=CoeffCols, nrow=(152))
ADIEffect_BLR <- matrix(ncol=CoeffCols, nrow=(152))
SREffect_BLR <- matrix(ncol=CoeffCols, nrow=(152))
DisEffect_BLR <- matrix(ncol=CoeffCols, nrow=(152))

Race_BLR_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))
Income_BLR_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))
ADI_BLR_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))
SR_BLR_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))
Dis_BLR_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))

Race_NoSite_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))
Income_NoSite_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))
ADI_NoSite_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))
SR_NoSite_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))
Dis_NoSite_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))

Race_Site_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))
Income_Site_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))
ADI_Site_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))
SR_Site_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))
Dis_Site_Threat <- matrix(ncol=6, nrow=(length(ThreatRegions)))

# Run the Mass Univariate Statistics with and without site, for the BLR outputs, using race or SES
## - Updated 11-28-23 to add an lm.beta - first column will be the Standardized beta
for (x in 25:176) {
  RaceEffect_Univariate_NoSite[x-24 ,] = c(lm.beta(lm(HCP_Data[, x] ~ WvB + sex + age, data = HCP_Data))[1],summary(lm(HCP_Data[, x] ~ WvB + sex + age, data = HCP_Data))$coefficients[2 ,])#
  IncomeEffect_Univariate_NoSite[x-24 ,] = c(lm.beta(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age, data = HCP_Data))[1],summary(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age, data = HCP_Data))$coefficients[2 ,])
  ADIEffect_Univariate_NoSite[x-24 ,] = c(lm.beta(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age, data = HCP_Data))[2],summary(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age, data = HCP_Data))$coefficients[3 ,])
  SREffect_Univariate_NoSite[x-24 ,] = c(lm.beta(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age, data = HCP_Data))[3],summary(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age, data = HCP_Data))$coefficients[4 ,])
  DisEffect_Univariate_NoSite[x-24 ,] = c(lm.beta(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age, data = HCP_Data))[4],summary(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age, data = HCP_Data))$coefficients[5 ,])
  
}

for (x in 25:176) {
  RaceEffect_Univariate_Site[x-24 ,] = c(lm.beta(lm(HCP_Data[, x] ~ WvB + sex + age + site, data = HCP_Data))[1],summary(lm(HCP_Data[, x] ~ WvB + sex + age + site, data = HCP_Data))$coefficients[2 ,])
  IncomeEffect_Univariate_Site[x-24 ,] = c(lm.beta(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age + site, data = HCP_Data))[1],summary(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age + site, data = HCP_Data))$coefficients[2 ,])
  ADIEffect_Univariate_Site[x-24 ,] = c(lm.beta(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age + site, data = HCP_Data))[2], summary(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age + site, data = HCP_Data))$coefficients[3 ,])
  SREffect_Univariate_Site[x-24 ,] = c(lm.beta(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age + site, data = HCP_Data))[3],summary(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age + site, data = HCP_Data))$coefficients[4 ,])
  DisEffect_Univariate_Site[x-24 ,] = c(lm.beta(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age + site, data = HCP_Data))[4],summary(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination + sex + age + site, data = HCP_Data))$coefficients[5 ,])
  }

for (x in 189:340) {
  RaceEffect_BLR[x-188 ,] = c(lm.beta(lm(HCP_Data[, x] ~ WvB, data = HCP_Data))[1],summary(lm(HCP_Data[, x] ~ WvB, data = HCP_Data))$coefficients[2 ,])
  IncomeEffect_BLR[x-188 ,] = c(lm.beta(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data))[1],summary(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data))$coefficients[2 ,])
  ADIEffect_BLR[x-188 ,] = c(lm.beta(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data))[2], summary(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data))$coefficients[3 ,])
  SREffect_BLR[x-188 ,] = c(lm.beta(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data))[3],summary(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data))$coefficients[4 ,])
  DisEffect_BLR[x-188 ,] = c(lm.beta(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data))[4],summary(lm(HCP_Data[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data))$coefficients[5 ,])
  }

# Add column names to the data
UnivariateColNames = colnames(HCP_Data[, 25:176])
RaceEffect_Univariate_NoSite_Named = cbind(UnivariateColNames, RaceEffect_Univariate_NoSite)
IncomeEffect_Univariate_NoSite_Named = cbind(UnivariateColNames, IncomeEffect_Univariate_NoSite)
ADIEffect_Univariate_NoSite_Named = cbind(UnivariateColNames, ADIEffect_Univariate_NoSite)
SREffect_Univariate_NoSite_Named = cbind(UnivariateColNames, SREffect_Univariate_NoSite)
DisEffect_Univariate_NoSite_Named = cbind(UnivariateColNames, DisEffect_Univariate_NoSite)

RaceEffect_Univariate_Site_Named = cbind(UnivariateColNames, RaceEffect_Univariate_Site)
IncomeEffect_Univariate_Site_Named = cbind(UnivariateColNames, IncomeEffect_Univariate_Site)
ADIEffect_Univariate_Site_Named = cbind(UnivariateColNames, ADIEffect_Univariate_Site)
SREffect_Univariate_Site_Named = cbind(UnivariateColNames, SREffect_Univariate_Site)
DisEffect_Univariate_Site_Named = cbind(UnivariateColNames, DisEffect_Univariate_Site)

RaceEffect_BLR_Named = cbind(UnivariateColNames, RaceEffect_BLR)
IncomeEffect_BLR_Named = cbind(UnivariateColNames, IncomeEffect_BLR)
ADIEffect_BLR_Named = cbind(UnivariateColNames, ADIEffect_BLR)
SREffect_BLR_Named = cbind(UnivariateColNames, SREffect_BLR)
DisEffect_BLR_Named = cbind(UnivariateColNames, DisEffect_BLR)

# You could select by significance
RaceNoSite_SignificantData = RaceEffect_Univariate_NoSite_Named[RaceEffect_Univariate_NoSite_Named[,6] < 0.05,]
RaceSite_SignificantData = RaceEffect_Univariate_Site_Named[RaceEffect_Univariate_Site_Named[,6] < 0.05,]
BLR_SignificantData = RaceEffect_BLR_Named[RaceEffect_BLR_Named[,6] < 0.05,]

# Get all out the outputs together by the aprior regions
for (y in 1:length(ThreatRegions)){
  Race_NoSite_Threat[y ,] = RaceEffect_Univariate_NoSite_Named[RaceEffect_Univariate_NoSite_Named[,1] == ThreatRegions[y]]
  Income_NoSite_Threat[y ,] = IncomeEffect_Univariate_NoSite_Named[IncomeEffect_Univariate_NoSite_Named[,1] == ThreatRegions[y]]
  ADI_NoSite_Threat[y ,] = ADIEffect_Univariate_NoSite_Named[ADIEffect_Univariate_NoSite_Named[,1] == ThreatRegions[y]]
  SR_NoSite_Threat[y ,] = SREffect_Univariate_NoSite_Named[SREffect_Univariate_NoSite_Named[,1] == ThreatRegions[y]]
  Dis_NoSite_Threat[y ,] = DisEffect_Univariate_NoSite_Named[DisEffect_Univariate_NoSite_Named[,1] == ThreatRegions[y]]
  
  
  Race_Site_Threat[y ,] = RaceEffect_Univariate_Site_Named[RaceEffect_Univariate_Site_Named[,1] == ThreatRegions[y]]
  Income_Site_Threat[y ,] = IncomeEffect_Univariate_Site_Named[IncomeEffect_Univariate_Site_Named[,1] == ThreatRegions[y]]
  ADI_Site_Threat[y ,] = ADIEffect_Univariate_Site_Named[ADIEffect_Univariate_Site_Named[,1] == ThreatRegions[y]]
  SR_Site_Threat[y ,] = SREffect_Univariate_Site_Named[SREffect_Univariate_Site_Named[,1] == ThreatRegions[y]]
  Dis_Site_Threat[y ,] = DisEffect_Univariate_Site_Named[DisEffect_Univariate_Site_Named[,1] == ThreatRegions[y]]
  
  Race_BLR_Threat[y ,] = RaceEffect_BLR_Named[RaceEffect_BLR_Named[,1] == ThreatRegions[y]]
  Income_BLR_Threat[y ,] = IncomeEffect_BLR_Named[IncomeEffect_BLR_Named[,1] == ThreatRegions[y]]
  ADI_BLR_Threat[y ,] = ADIEffect_BLR_Named[ADIEffect_BLR_Named[,1] == ThreatRegions[y]]
  SR_BLR_Threat[y ,] = SREffect_BLR_Named[SREffect_BLR_Named[,1] == ThreatRegions[y]]
  Dis_BLR_Threat[y ,] = DisEffect_BLR_Named[DisEffect_BLR_Named[,1] == ThreatRegions[y]]
  
}

Race_NoSite_Threat_Corr = cbind(Race_NoSite_Threat, p.adjust(Race_NoSite_Threat[, 6], method = "fdr"))
Income_NoSite_Threat_Corr = cbind(Income_NoSite_Threat, p.adjust(Income_NoSite_Threat[, 6], method = "fdr"))
ADI_NoSite_Threat_Corr = cbind(ADI_NoSite_Threat, p.adjust(ADI_NoSite_Threat[, 6], method = "fdr"))
SR_NoSite_Threat_Corr = cbind(SR_NoSite_Threat, p.adjust(SR_NoSite_Threat[, 6], method = "fdr"))
Dis_NoSite_Threat_Corr = cbind(Dis_NoSite_Threat, p.adjust(Dis_NoSite_Threat[, 6], method = "fdr"))


Race_Site_Threat_Corr = cbind(Race_Site_Threat, p.adjust(Race_Site_Threat[, 6], method = "fdr"))
Income_Site_Threat_Corr = cbind(Income_Site_Threat, p.adjust(Income_Site_Threat[, 6], method = "fdr"))
ADI_Site_Threat_Corr = cbind(ADI_Site_Threat, p.adjust(ADI_Site_Threat[, 6], method = "fdr"))
SR_Site_Threat_Corr = cbind(SR_Site_Threat, p.adjust(SR_Site_Threat[, 6], method = "fdr"))
Dis_Site_Threat_Corr = cbind(Dis_Site_Threat, p.adjust(Dis_Site_Threat[, 6], method = "fdr"))


Race_BLR_Threat_Corr = cbind(Race_BLR_Threat, p.adjust(Race_BLR_Threat[, 6], method = "fdr"))
Income_BLR_Threat_Corr = cbind(Income_BLR_Threat, p.adjust(Income_BLR_Threat[, 6], method = "fdr"))
ADI_BLR_Threat_Corr = cbind(ADI_BLR_Threat, p.adjust(ADI_BLR_Threat[, 6], method = "fdr"))
SR_BLR_Threat_Corr = cbind(SR_BLR_Threat, p.adjust(SR_BLR_Threat[, 6], method = "fdr"))
Dis_BLR_Threat_Corr = cbind(Dis_BLR_Threat, p.adjust(Dis_BLR_Threat[, 6], method = "fdr"))

Corrected_Race_Effects_Table = data.frame(Race_NoSite_Threat_Corr[, 1], Race_NoSite_Threat_Corr[, 2], Race_NoSite_Threat_Corr[, 7], 
                                          Race_Site_Threat_Corr[, 2], Race_Site_Threat_Corr[, 7],
                                          Race_BLR_Threat_Corr[, 2], Race_BLR_Threat_Corr[, 7])
Corrected_Income_Effects_Table = data.frame(Race_NoSite_Threat_Corr[, 1], Income_NoSite_Threat_Corr[, 2], Income_NoSite_Threat_Corr[, 7], 
                                          Income_Site_Threat_Corr[, 2], Income_Site_Threat_Corr[, 7],
                                          Income_BLR_Threat_Corr[, 2], Income_BLR_Threat_Corr[, 7])
Corrected_ADI_Effects_Table = data.frame(Race_NoSite_Threat_Corr[, 1], ADI_NoSite_Threat_Corr[, 2], ADI_NoSite_Threat_Corr[, 7], 
                                          ADI_Site_Threat_Corr[, 2], ADI_Site_Threat_Corr[, 7],
                                          ADI_BLR_Threat_Corr[, 2], ADI_BLR_Threat_Corr[, 7])
Corrected_Dis_Effects_Table = data.frame(Race_NoSite_Threat_Corr[, 1], Dis_NoSite_Threat_Corr[, 2], Dis_NoSite_Threat_Corr[, 7], 
                                         Dis_Site_Threat_Corr[, 2], Dis_Site_Threat_Corr[, 7],
                                         Dis_BLR_Threat_Corr[, 2], Dis_BLR_Threat_Corr[, 7])
Corrected_SR_Effects_Table = data.frame(Race_NoSite_Threat_Corr[, 1], SR_NoSite_Threat_Corr[, 2], SR_NoSite_Threat_Corr[, 7], 
                                         SR_Site_Threat_Corr[, 2], SR_Site_Threat_Corr[, 7],
                                         SR_BLR_Threat_Corr[, 2], SR_BLR_Threat_Corr[, 7])

#########################################
###        Get Descriptives.    ###
#########################################
library(plyr)
ddply(HCP_Data,~WvB,summarise,mean=mean(Base_ADI,na.rm=TRUE),sd=sd(Base_ADI, na.rm=TRUE))
ddply(HCP_Data,~WvB,summarise,mean=mean(Discrimination,na.rm=TRUE),sd=sd(Discrimination, na.rm=TRUE))
ddply(HCP_Data,~WvB,summarise,mean=mean(Base_Racism,na.rm=TRUE),sd=sd(Base_Racism, na.rm=TRUE))
ddply(HCP_Data,~WvB,summarise,mean=mean(age,na.rm=TRUE),sd=sd(age, na.rm=TRUE))
ddply(HCP_Data,~WvB,summarise,Income=table(demo_comb_income_v2_l_y))
ddply(HCP_Data,~WvB,summarise,Income=table(sex_y))
ddply(HCP_Data,~WvB,summarise,Income=table(site))


Chi_Sex_WvB <- chisq.test(table(HCP_Data$WvB, HCP_Data$sex_y))
Chi_Sex_WvB_Site <- chisq.test(table(HCP_Data$WvB, HCP_Data$site))
T_Inc_WvB <- t.test(HCP_Data$demo_comb_income_v2_l_y ~ HCP_Data$WvB)
T_Dis_WvB <- t.test(HCP_Data$Discrimination ~ HCP_Data$WvB)
T_ADI_WvB <- t.test(HCP_Data$Base_ADI ~ HCP_Data$WvB)
T_Rac_WvB <- t.test(HCP_Data$Base_Racism ~ HCP_Data$WvB)
T_Age_WvB <- t.test(HCP_Data$age ~ HCP_Data$WvB)

summary(lm(demo_comb_income_v2_l_y ~ site, data = HCP_Data))
summary(lm(Base_ADI ~ site, data = HCP_Data))
summary(lm(Base_Racism ~ site, data = HCP_Data))
summary(lm(Discrimination ~ site, data = HCP_Data))

#########################################
###        Run Mediation analyses.    ###
#########################################
library(lavaan)
options(scipen=999)

PercentMediated <- matrix(ncol=4, nrow=(length(ThreatRegions)))

#sink(file = "MediationResults1.txt")
for (B in 1:length(ThreatRegions)) {
  MedData = data.frame(HCP_Data$WvB, HCP_Data$Base_ADI, HCP_Data$Base_Racism, HCP_Data$Discrimination, HCP_Data$demo_comb_income_v2_l_y, HCP_Data[, ThreatRegions[B]], HCP_Data$age, HCP_Data$sex)
  colnames(MedData) = c('Race', 'Base_ADI','Base_Racism','Discrimination', 'Income', 'Thickness', 'age', 'sex')
  mediation.model <- ' 
             #FullModel
            Thickness ~  c*Race + age + sex + b1*Base_ADI + b2*Base_Racism + b3*Discrimination + b4*Income

             # mediator
             Base_ADI ~ a1*Race + age + sex
             Base_Racism ~ a2*Race  + age + sex
             Discrimination ~ a3*Race  + age + sex
             Income ~ a4*Race  + age + sex


           # indirect effect (a*b)
             ab := (a1*b1)+(a2*b2)+(a3*b3)+(a4*b4)

           # total effect
             total := c + (a1*b1)+(a2*b2)+(a3*b3)+(a4*b4)
          #Proportion Mediated
          Percentage Mediated := ((ab) / (c + ab))*100
          #Covariances
          Base_ADI~~Base_Racism
          Base_ADI~~Discrimination
          Base_ADI~~Income
          Base_Racism~~Discrimination
          Base_Racism~~Income
          Discrimination~~Income

                    '

  fit <- sem(mediation.model, data = MedData, meanstructure = TRUE,
           se = "robust")
  out <- paste0("Parallel Mediation Results for ", ThreatRegions[B], ".")
  MedSum = summary(fit, fit.measures=T, standardized=TRUE, ci=TRUE)
  MedSumPE = MedSum[["pe"]]
  #For the MedSumPE, Row 39 is ab, row 40 is totalm row 41 is %m. So, get p-values for ab/total
  # then get pct med value.
  PercentMediated[B, ] = cbind(ThreatRegions[B], MedSumPE[45, 9], MedSumPE[46, 9],MedSumPE[47, 14])
  print(out)
  print(summary(fit, fit.measures=T, standardized=TRUE, ci=TRUE))
}
#sink(file = NULL)

#########################################
###        Run Mediation analyses (BRAINCHARTS).    ###
### THIS WAS UPDATED FOR EDITS TO LAVAAN, THE ABOVE WAS ALSO MODIFIED FOR THESE EDITS ###
#########################################
library(lavaan)
options(scipen=999)

PercentMediated <- matrix(ncol=4, nrow=(length(ThreatRegions)))

sink(file = "MediationResults_BrainCharts.txt")
A <- 1
for (B in 189:340) {
  MedData = data.frame(HCP_Data$WvB, HCP_Data$Base_ADI, HCP_Data$Base_Racism, HCP_Data$Discrimination, HCP_Data$demo_comb_income_v2_l_y, HCP_Data[, B], HCP_Data$age, HCP_Data$sex)
  colnames(MedData) = c('Race', 'Base_ADI','Base_Racism','Discrimination', 'Income', 'Thickness', 'age', 'sex')
  mediation.model <- ' 
             #FullModel
            Thickness ~  c*Race + age + sex + b1*Base_ADI + b2*Base_Racism + b3*Discrimination + b4*Income

             # mediator
             Base_ADI ~ a1*Race + age + sex
             Base_Racism ~ a2*Race  + age + sex
             Discrimination ~ a3*Race  + age + sex
             Income ~ a4*Race  + age + sex


           # indirect effect (a*b)
             ab := (a1*b1)+(a2*b2)+(a3*b3)+(a4*b4)

           # total effect
             total := c + (a1*b1)+(a2*b2)+(a3*b3)+(a4*b4)
          #Proportion Mediated
          Percentage Mediated := ((ab) / (c + ab))*100
          #Covariances
          Base_ADI~~Base_Racism
          Base_ADI~~Discrimination
          Base_ADI~~Income
          Base_Racism~~Discrimination
          Base_Racism~~Income
          Discrimination~~Income

                    '
  
  fit <- sem(mediation.model, data = MedData, meanstructure = TRUE,
             se = "robust")
  out <- paste0("Parallel Mediation Results for ", ThreatRegions[A], ".")
  MedSum = summary(fit, fit.measures=T, standardized=TRUE, ci=TRUE)
  MedSumPE = MedSum[["pe"]]
  #For the MedSumPE, Row 45 is ab, row 46 is totalm row 47 is %m. So, get p-values for ab/total
  # then get pct med value.
  PercentMediated[A, ] = cbind(ThreatRegions[A], MedSumPE[45, 9], MedSumPE[46, 9],MedSumPE[47, 14])
  print(out)
  print(summary(fit, fit.measures=T, standardized=TRUE, ci=TRUE))
  A <- A+1
}
sink(file = NULL)

####################
# Make Brain Plots #
####################

library(ggsegDesterieux)
library(ggseg3d)
library(ggseg)
library(ggplot2)
library(patchwork)
library(extrafont)
font_import(pattern = "arial")

# Define custom labelling function
#custom_labeller <- function(variable, value) {
#  if (variable == "ind") {
#    value <- ifelse(value == "left", 
#                    as.character(expression(paste("<span style='font-family: Arial;'>Left</span>"))),
#                    ifelse(value == "right", 
#                           as.character(expression(paste("<span style='font-family: Arial;'>Right</span>"))),
#                           value))
#  }
#  return(value)
#}

DDThreatRegs = c('lh_G_and_S_frontomargin', 'lh_G_and_S_transv_frontopol', 'lh_G_and_S_cingul-Ant', 
                 'lh_G_and_S_cingul-Mid-Ant', 'lh_G_and_S_cingul-Mid-Post', 'lh_G_front_inf-Opercular', 
                 'lh_G_front_inf-Orbital', 'lh_G_front_inf-Triangul', 'lh_G_front_middle', 'lh_G_front_sup', 
                 'lh_G_Ins_lg_and_S_cent_ins', 'lh_G_insular_short', 'lh_G_orbital', 'lh_G_rectus', 'lh_G_subcallosal', 
                 'lh_S_circular_insula_ant', 'lh_S_circular_insula_inf', 'lh_S_circular_insula_sup', 'lh_S_front_inf', 
                 'lh_S_front_middle', 'lh_S_front_sup', 'lh_S_orbital_lateral', 'lh_S_orbital_med-olfact', 
                 'lh_S_orbital-H_Shaped', 'lh_S_suborbital', 'Left-Amygdala', 'Left-Hippocampus', 
                 'rh_G_and_S_frontomargin', 'rh_G_and_S_transv_frontopol', 'rh_G_and_S_cingul-Ant', 
                 'rh_G_and_S_cingul-Mid-Ant', 'rh_G_and_S_cingul-Mid-Post', 'rh_G_front_inf-Opercular', 
                 'rh_G_front_inf-Orbital', 'rh_G_front_inf-Triangul', 'rh_G_front_middle', 'rh_G_front_sup', 
                 'rh_G_Ins_lg_and_S_cent_ins', 'rh_G_insular_short', 'rh_G_orbital', 'rh_G_rectus', 'rh_G_subcallosal', 
                 'rh_S_circular_insula_ant', 'rh_S_circular_insula_inf', 'rh_S_circular_insula_sup', 'rh_S_front_inf', 
                 'rh_S_front_middle', 'rh_S_front_sup', 'rh_S_orbital_lateral', 'rh_S_orbital_med-olfact', 
                 'rh_S_orbital-H_Shaped', 'rh_S_suborbital', 'Right-Amygdala', 'Right-Hippocampus', 
                 'rh_Lat_Fis-ant-Horizont', 'rh_Lat_Fis-ant-Vertical', 'rh_Lat_Fis-post', 
                 'lh_Lat_Fis-ant-Horizont', 'lh_Lat_Fis-ant-Vertical', 'lh_Lat_Fis-post',
                 'lh_S_pericallosal', 'rh_S_pericallosal')

## Plot Race
GG_Race_Data = data.frame(DDThreatRegs, Corrected_Race_Effects_Table[, 3], Corrected_Race_Effects_Table[, 5], Corrected_Race_Effects_Table[, 7])
colnames(GG_Race_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')
GG_Race_Data_ggseg = cbind(GG_Race_Data[1], stack(GG_Race_Data[2:4]))

GG_Race_Data_ggseg = GG_Race_Data_ggseg %>% 
  group_by(ind)

Fig1A = ggplot()+
  geom_brain(atlas=desterieux, data=GG_Race_Data_ggseg, aes(fill=as.numeric(values)), colour="black", show.legend = FALSE ) +
  theme_dark()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(family = "Arial"),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient(low="gold",high="red", na.value = "white",
                      limits=c(0,0.05)) +
  labs(title="Race-related Differences",fill="p(FDR)")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")


Fig1B = ggplot()+
  geom_brain(atlas=aseg, data=GG_Race_Data_ggseg, aes(fill=as.numeric(values)), side = "coronal", colour="black", show.legend = FALSE ) +
  theme_dark()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(family = "Arial", size = 8),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient(low="gold",high="red", na.value = "white",
                      limits=c(0,0.05)) +
  labs(title="Race-related Differences",fill="p(FDR)")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")


## Plot Income
GG_Income_Data = data.frame(DDThreatRegs, Corrected_Income_Effects_Table[, 3], Corrected_Income_Effects_Table[, 5], Corrected_Income_Effects_Table[, 7])
colnames(GG_Income_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')
GG_Income_Data_ggseg = cbind(GG_Income_Data[1], stack(GG_Income_Data[2:4]))

GG_Income_Data_ggseg = GG_Income_Data_ggseg %>% 
  group_by(ind)

Fig1C = ggplot()+
  geom_brain(atlas=desterieux, data=GG_Income_Data_ggseg, aes(fill=as.numeric(values)), colour="black", show.legend = FALSE ) +
  theme_void()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient(low="gold",high="red", na.value = "white",
                      limits=c(0,0.05)) +
  labs(title="Associations with Income",fill="p(FDR)")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

Fig1D = ggplot()+
  geom_brain(atlas=aseg, data=GG_Income_Data_ggseg, aes(fill=as.numeric(values)), side = "coronal", colour="black", show.legend = FALSE ) +
  theme_void()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient(low="gold",high="red", na.value = "white",
                      limits=c(0,0.05)) +
  labs(title="Associations with Income",fill="p(FDR)")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

## Plot ADI
GG_ADI_Data = data.frame(DDThreatRegs, Corrected_ADI_Effects_Table[, 3], Corrected_ADI_Effects_Table[, 5], Corrected_ADI_Effects_Table[, 7])
colnames(GG_ADI_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')
GG_ADI_Data_ggseg = cbind(GG_ADI_Data[1], stack(GG_ADI_Data[2:4]))

GG_ADI_Data_ggseg = GG_ADI_Data_ggseg %>% 
  group_by(ind)

Fig1E = ggplot()+
  geom_brain(atlas=desterieux, data=GG_ADI_Data_ggseg, aes(fill=as.numeric(values)), colour="black" ) +
  theme_void()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient(low="gold",high="red", na.value = "white",
                      limits=c(0,0.05)) +
  labs(title="Associations with ADI",fill="p(FDR)")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

Fig1F = ggplot()+
  geom_brain(atlas=aseg, data=GG_ADI_Data_ggseg, aes(fill=as.numeric(values)),side = "coronal",  colour="black" ) +
  theme_void()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient(low="gold",high="red", na.value = "white",
                      limits=c(0,0.05)) +
  labs(title="Associations with ADI",fill="p(FDR)")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

## Plot Discrimination
GG_Dis_Data = data.frame(DDThreatRegs, Corrected_Dis_Effects_Table[, 3], Corrected_Dis_Effects_Table[, 5], Corrected_Dis_Effects_Table[, 7])
colnames(GG_Dis_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')
GG_Dis_Data_ggseg = cbind(GG_Dis_Data[1], stack(GG_Dis_Data[2:4]))

GG_Dis_Data_ggseg = GG_Dis_Data_ggseg %>% 
  group_by(ind)

Fig1H = ggplot()+
  geom_brain(atlas=desterieux, data=GG_Dis_Data_ggseg, aes(fill=as.numeric(values)), colour="black", show.legend = FALSE ) +
  theme_void()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient(low="gold",high="red", na.value = "white",
                      limits=c(0,0.05)) +
  labs(title="Associations with Discrimination",fill="p(FDR)")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

Fig1I = ggplot()+
  geom_brain(atlas=aseg, data=GG_Dis_Data_ggseg, aes(fill=as.numeric(values)), side = "coronal", colour="black", show.legend = FALSE ) +
  theme_void()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient(low="gold",high="red", na.value = "white",
                      limits=c(0,0.05)) +
  labs(title="Associations with Discrimination",fill="p(FDR)")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

## Plot State-level Racism
GG_SR_Data = data.frame(DDThreatRegs, Corrected_SR_Effects_Table[, 3], Corrected_SR_Effects_Table[, 5], Corrected_SR_Effects_Table[, 7])
colnames(GG_SR_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')
GG_SR_Data_ggseg = cbind(GG_SR_Data[1], stack(GG_SR_Data[2:4]))

GG_SR_Data_ggseg = GG_SR_Data_ggseg %>% 
  group_by(ind)

Fig1J =  ggplot()+
  geom_brain(atlas=desterieux, data=GG_SR_Data_ggseg, aes(fill=as.numeric(values)), colour="black", show.legend = FALSE ) +
  theme_void()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient(low="gold",high="red", na.value = "white",
                      limits=c(0,0.05)) +
  labs(title="Associations with State-level Racism",fill="p(FDR)")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")


Fig1K = ggplot()+
  geom_brain(atlas=aseg, data=GG_SR_Data_ggseg, aes(fill=as.numeric(values)), side = "coronal", colour="black", show.legend = FALSE ) +
  theme_void()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient(low="gold",high="red", na.value = "white",
                      limits=c(0,0.05)) +
  labs(title="Associations with State-level Racism",fill="p(FDR)")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

SurfacePlot <- Fig1A / Fig1C / Fig1E / Fig1H / Fig1J +
  plot_layout(widths = c(1, 1, 1, 1, 1), heights = c(0.5, 0.5, 0.5, 0.5, 0.5))
AsegPlot <- Fig1B / Fig1D / Fig1F / Fig1I / Fig1K +
  plot_layout(widths = c(1, 1, 1, 1, 1), heights = c(0.5, 0.5, 0.5, 0.5, 0.5))

#ggsave("~/R_WorkingDir/Fig1A.png", plot = Fig1A)
#ggsave("~/R_WorkingDir/Fig1C.png", plot = Fig1C)
#ggsave("~/R_WorkingDir/Fig1E.png", plot = Fig1E)
#ggsave("~/R_WorkingDir/Fig1H.png", plot = Fig1H)
#ggsave("~/R_WorkingDir/Fig1J.png", plot = Fig1J)
#ggsave("~/R_WorkingDir/Aseg.png", plot = AsegPlot)


## Plot Percent Mediated
GG_PM_Data = data.frame(DDThreatRegs, PercentMediated[, 2:4])
colnames(GG_PM_Data) = c('label', 'p-value1','p-value2', 'PercentMediated')
  #Remove %m if either the total or ab paths were not significant (not a med)
is.na(GG_PM_Data[4]) <- GG_PM_Data[2] > 0.05
is.na(GG_PM_Data[4]) <- GG_PM_Data[3] > 0.05
GG_PM_Data[is.na(GG_PM_Data)] = 0
GG_PM_Data[, 4][GG_PM_Data[, 4] < 0] <- 0


Fig2A =  ggplot()+
  geom_brain(atlas=desterieux, data=GG_PM_Data, aes(fill=as.numeric(PercentMediated)), colour="black", show.legend = TRUE ) +
  theme_void()+
  theme(legend.position = "bottom", 
    legend.key.width = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient(low="skyblue", high="blue", na.value = "white",
                      limits=c(0,100)) +
  labs(title="Percent Mediated", fill="%")+
  theme_brain(text.size=12,text.family="Arial")

Fig2B =  ggplot()+
  geom_brain(atlas=aseg, data=GG_PM_Data, aes(fill=as.numeric(PercentMediated)), colour="black", show.legend = FALSE ) +
  theme_void()+
  theme(legend.position = "bottom", 
        legend.key.width = unit(2.5, "cm"), 
        legend.text = element_text(size=12),
        legend.title= element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_blank(),
        text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient(low="skyblue", high="blue", na.value = "white",
                      limits=c(0,100)) +
  labs(title="Percent Mediated", fill="%")+
  theme_brain(text.size=12,text.family="Arial")

ggsave("~/R_WorkingDir/Fig2A_BC_022724.png", plot = Fig2A)
ggsave("~/R_WorkingDir/Fig2B_BC_022724.png", plot = Fig2B)

## Reviewer Requested Analyses ##
#Re-run the Z-deviation analyses split by Black/White#

HCP_Data_W<-HCP_Data[!(HCP_Data$WvB==0),]
HCP_Data_B<-HCP_Data[!(HCP_Data$WvB==1),]


Income_BLR_Threat_W <- matrix(ncol=5, nrow=(length(ThreatRegions)))
ADI_BLR_Threat_W <- matrix(ncol=5, nrow=(length(ThreatRegions)))
SR_BLR_Threat_W <- matrix(ncol=5, nrow=(length(ThreatRegions)))
Dis_BLR_Threat_W <- matrix(ncol=5, nrow=(length(ThreatRegions)))
Income_BLR_Threat_B <- matrix(ncol=5, nrow=(length(ThreatRegions)))
ADI_BLR_Threat_B <- matrix(ncol=5, nrow=(length(ThreatRegions)))
SR_BLR_Threat_B <- matrix(ncol=5, nrow=(length(ThreatRegions)))
Dis_BLR_Threat_B <- matrix(ncol=5, nrow=(length(ThreatRegions)))

IncomeEffect_BLR_W <- matrix(ncol=4, nrow=(152))
ADIEffect_BLR_W <- matrix(ncol=4, nrow=(152))
SREffect_BLR_W <- matrix(ncol=4, nrow=(152))
DisEffect_BLR_W <- matrix(ncol=4, nrow=(152))
IncomeEffect_BLR_B <- matrix(ncol=4, nrow=(152))
ADIEffect_BLR_B <- matrix(ncol=4, nrow=(152))
SREffect_BLR_B <- matrix(ncol=4, nrow=(152))
DisEffect_BLR_B <- matrix(ncol=4, nrow=(152))

for (x in 189:340) {
  IncomeEffect_BLR_W[x-188 ,] = summary(lm(HCP_Data_W[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data_W))$coefficients[2 ,]
  ADIEffect_BLR_W[x-188 ,] = summary(lm(HCP_Data_W[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data_W))$coefficients[3 ,]
  SREffect_BLR_W[x-188 ,] = summary(lm(HCP_Data_W[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data_W))$coefficients[4 ,]
  DisEffect_BLR_W[x-188 ,] = summary(lm(HCP_Data_W[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data_W))$coefficients[5 ,]
  IncomeEffect_BLR_B[x-188 ,] = summary(lm(HCP_Data_B[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data_B))$coefficients[2 ,]
  ADIEffect_BLR_B[x-188 ,] = summary(lm(HCP_Data_B[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data_B))$coefficients[3 ,]
  SREffect_BLR_B[x-188 ,] = summary(lm(HCP_Data_B[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data_B))$coefficients[4 ,]
  DisEffect_BLR_B[x-188 ,] = summary(lm(HCP_Data_B[, x] ~ demo_comb_income_v2_l_y + Base_ADI + Base_Racism + Discrimination, data = HCP_Data_B))$coefficients[5 ,]
  
}

# Add column names to the data
UnivariateColNames = colnames(HCP_Data[, 25:176])

IncomeEffect_BLR_W_Named = cbind(UnivariateColNames, IncomeEffect_BLR_W)
ADIEffect_BLR_W_Named = cbind(UnivariateColNames, ADIEffect_BLR_W)
SREffect_BLR_W_Named = cbind(UnivariateColNames, SREffect_BLR_W)
DisEffect_BLR_W_Named = cbind(UnivariateColNames, DisEffect_BLR_W)
IncomeEffect_BLR_B_Named = cbind(UnivariateColNames, IncomeEffect_BLR_B)
ADIEffect_BLR_B_Named = cbind(UnivariateColNames, ADIEffect_BLR_B)
SREffect_BLR_B_Named = cbind(UnivariateColNames, SREffect_BLR_B)
DisEffect_BLR_B_Named = cbind(UnivariateColNames, DisEffect_BLR_B)


# Get all out the outputs together by the aprior regions
for (y in 1:length(ThreatRegions)){
  Income_BLR_Threat_W[y ,] = IncomeEffect_BLR_W_Named[IncomeEffect_BLR_W_Named[,1] == ThreatRegions[y]]
  ADI_BLR_Threat_W[y ,] = ADIEffect_BLR_W_Named[ADIEffect_BLR_W_Named[,1] == ThreatRegions[y]]
  SR_BLR_Threat_W[y ,] = SREffect_BLR_W_Named[SREffect_BLR_W_Named[,1] == ThreatRegions[y]]
  Dis_BLR_Threat_W[y ,] = DisEffect_BLR_W_Named[DisEffect_BLR_W_Named[,1] == ThreatRegions[y]]
  
  Income_BLR_Threat_B[y ,] = IncomeEffect_BLR_B_Named[IncomeEffect_BLR_B_Named[,1] == ThreatRegions[y]]
  ADI_BLR_Threat_B[y ,] = ADIEffect_BLR_B_Named[ADIEffect_BLR_B_Named[,1] == ThreatRegions[y]]
  SR_BLR_Threat_B[y ,] = SREffect_BLR_B_Named[SREffect_BLR_B_Named[,1] == ThreatRegions[y]]
  Dis_BLR_Threat_B[y ,] = DisEffect_BLR_B_Named[DisEffect_BLR_B_Named[,1] == ThreatRegions[y]]
  
  
}

Income_BLR_Threat_W_Corr = cbind(Income_BLR_Threat_W, p.adjust(Income_BLR_Threat_W[, 5], method = "fdr"))
ADI_BLR_Threat_W_Corr = cbind(ADI_BLR_Threat_W, p.adjust(ADI_BLR_Threat_W[, 5], method = "fdr"))
SR_BLR_Threat_W_Corr = cbind(SR_BLR_Threat_W, p.adjust(SR_BLR_Threat_W[, 5], method = "fdr"))
Dis_BLR_Threat_W_Corr = cbind(Dis_BLR_Threat_W, p.adjust(Dis_BLR_Threat_W[, 5], method = "fdr"))

Income_BLR_Threat_B_Corr = cbind(Income_BLR_Threat_B, p.adjust(Income_BLR_Threat_B[, 5], method = "fdr"))
ADI_BLR_Threat_B_Corr = cbind(ADI_BLR_Threat_B, p.adjust(ADI_BLR_Threat_B[, 5], method = "fdr"))
SR_BLR_Threat_B_Corr = cbind(SR_BLR_Threat_B, p.adjust(SR_BLR_Threat_B[, 5], method = "fdr"))
Dis_BLR_Threat_B_Corr = cbind(Dis_BLR_Threat_B, p.adjust(Dis_BLR_Threat_B[, 5], method = "fdr"))

Corrected_BW_Income_Effects_Table = data.frame(Income_BLR_Threat_W_Corr[, 1], Income_BLR_Threat_W_Corr[, 2], Income_BLR_Threat_W_Corr[, 6], 
                                               Income_BLR_Threat_B_Corr[, 2], Income_BLR_Threat_B_Corr[, 6])
Corrected_BW_ADI_Effects_Table = data.frame(ADI_BLR_Threat_W_Corr[, 1], ADI_BLR_Threat_W_Corr[, 2], ADI_BLR_Threat_W_Corr[, 6], 
                                            ADI_BLR_Threat_B_Corr[, 2], ADI_BLR_Threat_B_Corr[, 6])
Corrected_BW_Dis_Effects_Table = data.frame(Dis_BLR_Threat_W_Corr[, 1], Dis_BLR_Threat_W_Corr[, 2], Dis_BLR_Threat_W_Corr[, 6], 
                                            Dis_BLR_Threat_B_Corr[, 2], Dis_BLR_Threat_B_Corr[, 6])
Corrected_BW_SR_Effects_Table = data.frame(SR_BLR_Threat_W_Corr[, 1], SR_BLR_Threat_W_Corr[, 2], SR_BLR_Threat_W_Corr[, 6], 
                                           SR_BLR_Threat_B_Corr[, 2], SR_BLR_Threat_B_Corr[, 6])

####################
# Make Brain Plots BUT USING BETA VALUES #
####################

library(ggsegDesterieux)
library(ggseg3d)
library(ggseg)
library(ggplot2)
library(patchwork)
library(extrafont)
font_import(pattern = "arial")

# Define custom labelling function
#custom_labeller <- function(variable, value) {
#  if (variable == "ind") {
#    value <- ifelse(value == "left", 
#                    as.character(expression(paste("<span style='font-family: Arial;'>Left</span>"))),
#                    ifelse(value == "right", 
#                           as.character(expression(paste("<span style='font-family: Arial;'>Right</span>"))),
#                           value))
#  }
#  return(value)
#}

DDThreatRegs = c('lh_G_and_S_frontomargin', 'lh_G_and_S_transv_frontopol', 'lh_G_and_S_cingul-Ant', 
                 'lh_G_and_S_cingul-Mid-Ant', 'lh_G_and_S_cingul-Mid-Post', 'lh_G_front_inf-Opercular', 
                 'lh_G_front_inf-Orbital', 'lh_G_front_inf-Triangul', 'lh_G_front_middle', 'lh_G_front_sup', 
                 'lh_G_Ins_lg_and_S_cent_ins', 'lh_G_insular_short', 'lh_G_orbital', 'lh_G_rectus', 'lh_G_subcallosal', 
                 'lh_S_circular_insula_ant', 'lh_S_circular_insula_inf', 'lh_S_circular_insula_sup', 'lh_S_front_inf', 
                 'lh_S_front_middle', 'lh_S_front_sup', 'lh_S_orbital_lateral', 'lh_S_orbital_med-olfact', 
                 'lh_S_orbital-H_Shaped', 'lh_S_suborbital', 'Left-Amygdala', 'Left-Hippocampus', 
                 'rh_G_and_S_frontomargin', 'rh_G_and_S_transv_frontopol', 'rh_G_and_S_cingul-Ant', 
                 'rh_G_and_S_cingul-Mid-Ant', 'rh_G_and_S_cingul-Mid-Post', 'rh_G_front_inf-Opercular', 
                 'rh_G_front_inf-Orbital', 'rh_G_front_inf-Triangul', 'rh_G_front_middle', 'rh_G_front_sup', 
                 'rh_G_Ins_lg_and_S_cent_ins', 'rh_G_insular_short', 'rh_G_orbital', 'rh_G_rectus', 'rh_G_subcallosal', 
                 'rh_S_circular_insula_ant', 'rh_S_circular_insula_inf', 'rh_S_circular_insula_sup', 'rh_S_front_inf', 
                 'rh_S_front_middle', 'rh_S_front_sup', 'rh_S_orbital_lateral', 'rh_S_orbital_med-olfact', 
                 'rh_S_orbital-H_Shaped', 'rh_S_suborbital', 'Right-Amygdala', 'Right-Hippocampus', 
                 'rh_Lat_Fis-ant-Horizont', 'rh_Lat_Fis-ant-Vertical', 'rh_Lat_Fis-post', 
                 'lh_Lat_Fis-ant-Horizont', 'lh_Lat_Fis-ant-Vertical', 'lh_Lat_Fis-post',
                 'lh_S_pericallosal', 'rh_S_pericallosal')

## Plot Race

GG_Race_Data = data.frame(DDThreatRegs, Corrected_Race_Effects_Table[, 3], Corrected_Race_Effects_Table[, 5], Corrected_Race_Effects_Table[, 7])
GG_Race_Beta_Data = data.frame(DDThreatRegs, Corrected_Race_Effects_Table[, 2], Corrected_Race_Effects_Table[, 4], Corrected_Race_Effects_Table[, 6])
GG_Race_Beta_Data[, 2][ GG_Race_Data[, 2]>0.05 ] <- 0
GG_Race_Beta_Data[, 3][ GG_Race_Data[, 3]>0.05 ] <- 0
GG_Race_Beta_Data[, 4][ GG_Race_Data[, 4]>0.05 ] <- 0
GG_Race_Beta_Data[GG_Race_Beta_Data == 0] <- NA
colnames(GG_Race_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')
colnames(GG_Race_Beta_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')
GG_Race_Data_ggseg = cbind(GG_Race_Data[1], stack(GG_Race_Data[2:4]))
GG_Race_Beta_Data_ggseg = cbind(GG_Race_Beta_Data[1], stack(GG_Race_Beta_Data[2:4]))


GG_Race_Beta_Data_ggseg = GG_Race_Beta_Data_ggseg %>% 
  group_by(ind)

Fig1A = ggplot()+
  geom_brain(atlas=desterieux, data=GG_Race_Beta_Data_ggseg, aes(fill=as.numeric(values)), colour="black",  show.legend = TRUE ) +
  theme_void()+
  theme(legend.position = "right", 
        legend.key.height = unit(2.25, "cm"), 
        legend.key.width = unit(0.5, "cm"), 
        legend.text = element_text(size=12),
        legend.title= element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_blank(),
        text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient2(low="blue",high="red", midpoint = 0,   na.value = "white", limits=c(-0.25,0.25)) +
  labs(title="Race-related Differences",fill="β")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")


Fig1B = ggplot()+
  geom_brain(atlas=aseg, data=GG_Race_Beta_Data_ggseg, aes(fill=as.numeric(values)), side = "coronal", colour="black", show.legend = FALSE ) +
  theme_dark()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(family = "Arial", size = 8),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient2(low="blue",high="red", midpoint = 0,   na.value = "white", limits=c(-0.25,0.25)) +
  labs(title="Race-related Differences",fill="β")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

## Plot Income
GG_Income_Data = data.frame(DDThreatRegs, Corrected_Income_Effects_Table[, 3], Corrected_Income_Effects_Table[, 5], Corrected_Income_Effects_Table[, 7])
GG_Income_Beta_Data = data.frame(DDThreatRegs, Corrected_Income_Effects_Table[, 2], Corrected_Income_Effects_Table[, 4], Corrected_Income_Effects_Table[, 6])
GG_Income_Beta_Data[, 2][ GG_Income_Data[, 2]>0.05 ] <- 0
GG_Income_Beta_Data[, 3][ GG_Income_Data[, 3]>0.05 ] <- 0
GG_Income_Beta_Data[, 4][ GG_Income_Data[, 4]>0.05 ] <- 0
GG_Income_Beta_Data[GG_Income_Beta_Data == 0] <- NA
colnames(GG_Income_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')
colnames(GG_Income_Beta_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')

GG_Income_Data_ggseg = cbind(GG_Income_Data[1], stack(GG_Income_Data[2:4]))
GG_Income_Beta_Data_ggseg = cbind(GG_Income_Beta_Data[1], stack(GG_Income_Beta_Data[2:4]))

GG_Income_Beta_Data_ggseg = GG_Income_Beta_Data_ggseg %>% 
  group_by(ind)

Fig1C = ggplot()+
  geom_brain(atlas=desterieux, data=GG_Income_Beta_Data_ggseg, aes(fill=as.numeric(values)), colour="black",  show.legend = FALSE ) +
  theme_void()+
  theme(legend.position = "bottom", 
    legend.key.height = unit(0.25, "cm"), 
    legend.key.width = unit(3.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient2(low="blue",high="red", midpoint = 0, na.value = "white",limits=c(-0.25,0.25)) +
  labs(title="Associations with Income",fill="β")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

Fig1D = ggplot()+
  geom_brain(atlas=aseg, data=GG_Income_Beta_Data_ggseg, aes(fill=as.numeric(values)), side = "coronal", colour="black", show.legend = FALSE ) +
  theme_void()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient2(low="blue",high="red", midpoint = 0, na.value = "white",limits=c(-0.25,0.25)) +
  labs(title="Associations with Income",fill="β")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

## Plot ADI
GG_ADI_Data = data.frame(DDThreatRegs, Corrected_ADI_Effects_Table[, 3], Corrected_ADI_Effects_Table[, 5], Corrected_ADI_Effects_Table[, 7])
GG_ADI_Beta_Data = data.frame(DDThreatRegs, Corrected_ADI_Effects_Table[, 2], Corrected_ADI_Effects_Table[, 4], Corrected_ADI_Effects_Table[, 6])
GG_ADI_Beta_Data[, 2][ GG_ADI_Data[, 2]>0.05 ] <- 0
GG_ADI_Beta_Data[, 3][ GG_ADI_Data[, 3]>0.05 ] <- 0
GG_ADI_Beta_Data[, 4][ GG_ADI_Data[, 4]>0.05 ] <- 0
GG_ADI_Beta_Data[GG_ADI_Beta_Data == 0] <- NA
colnames(GG_ADI_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')
colnames(GG_ADI_Beta_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')

GG_ADI_Data_ggseg = cbind(GG_ADI_Data[1], stack(GG_ADI_Data[2:4]))
GG_ADI_Beta_Data_ggseg = cbind(GG_ADI_Beta_Data[1], stack(GG_ADI_Beta_Data[2:4]))

GG_ADI_Beta_Data_ggseg = GG_ADI_Beta_Data_ggseg %>% 
  group_by(ind)


Fig1E = ggplot()+
  geom_brain(atlas=desterieux, data=GG_ADI_Beta_Data_ggseg, aes(fill=as.numeric(values)), colour="black", show.legend = FALSE ) +
  theme_void()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.key.width = unit(0.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient2(low="blue",high="red", midpoint = 0, na.value = "white", limits=c(-0.25,0.25)) +
  labs(title="Associations with ADI",fill="β")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

Fig1F = ggplot()+
  geom_brain(atlas=aseg, data=GG_ADI_Beta_Data_ggseg, aes(fill=as.numeric(values)),side = "coronal",  colour="black" ) +
  theme_void()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient2(low="blue",high="red", midpoint = 0, na.value = "white", limits=c(-0.25,0.25)) +
  labs(title="Associations with ADI",fill="β")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

## Plot Discrimination
GG_Dis_Data = data.frame(DDThreatRegs, Corrected_Dis_Effects_Table[, 3], Corrected_Dis_Effects_Table[, 5], Corrected_Dis_Effects_Table[, 7])
GG_Dis_Beta_Data = data.frame(DDThreatRegs, Corrected_Dis_Effects_Table[, 2], Corrected_Dis_Effects_Table[, 4], Corrected_Dis_Effects_Table[, 6])
GG_Dis_Beta_Data[, 2][ GG_Dis_Data[, 2]>0.05 ] <- 0
GG_Dis_Beta_Data[, 3][ GG_Dis_Data[, 3]>0.05 ] <- 0
GG_Dis_Beta_Data[, 4][ GG_Dis_Data[, 4]>0.05 ] <- 0
GG_Dis_Beta_Data[GG_Dis_Beta_Data == 0] <- NA
colnames(GG_Dis_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')
colnames(GG_Dis_Beta_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')
GG_Dis_Data_ggseg = cbind(GG_Dis_Data[1], stack(GG_Dis_Data[2:4]))
GG_Dis_Beta_Data_ggseg = cbind(GG_Dis_Beta_Data[1], stack(GG_Dis_Beta_Data[2:4]))

GG_Dis_Beta_Data_ggseg = GG_Dis_Beta_Data_ggseg %>% 
  group_by(ind)

Fig1H = ggplot()+
  geom_brain(atlas=desterieux, data=GG_Dis_Beta_Data_ggseg, aes(fill=as.numeric(values)), colour="black", show.legend = FALSE  ) +
  theme_void()+
  theme(legend.position = "bottom", 
        legend.key.height = unit(0.25, "cm"), 
        legend.key.width = unit(3.5, "cm"), 
        legend.text = element_text(size=12),
        legend.title= element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_blank(),
        text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient2(low="blue",high="red", midpoint = 0, na.value = "white", limits=c(-0.25,0.25)) +
  labs(title="Associations with Discrimination",fill="β")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

Fig1I = ggplot()+
  geom_brain(atlas=aseg, data=GG_Dis_Beta_Data_ggseg, aes(fill=as.numeric(values)), side = "coronal", colour="black", show.legend = FALSE ) +
  theme_void()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient2(low="blue",high="red", midpoint = 0, na.value = "white", limits=c(-0.25,0.25)) +
  labs(title="Associations with Discrimination",fill="β")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

## Plot State-level Racism
GG_SR_Data = data.frame(DDThreatRegs, Corrected_SR_Effects_Table[, 3], Corrected_SR_Effects_Table[, 5], Corrected_SR_Effects_Table[, 7])
GG_SR_Beta_Data = data.frame(DDThreatRegs, Corrected_SR_Effects_Table[, 2], Corrected_SR_Effects_Table[, 4], Corrected_SR_Effects_Table[, 6])
GG_SR_Beta_Data[, 2][ GG_SR_Data[, 2]>0.05 ] <- 0
GG_SR_Beta_Data[, 3][ GG_SR_Data[, 3]>0.05 ] <- 0
GG_SR_Beta_Data[, 4][ GG_SR_Data[, 4]>0.05 ] <- 0
GG_SR_Beta_Data[GG_SR_Beta_Data == 0] <- NA
colnames(GG_SR_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')
colnames(GG_SR_Beta_Data) = c('label', 'GLM (Age + Sex)', 'GLM (Age + Sex + Site)', 'Brainchart Deviations')
GG_SR_Data_ggseg = cbind(GG_SR_Data[1], stack(GG_SR_Data[2:4]))
GG_SR_Beta_Data_ggseg = cbind(GG_SR_Beta_Data[1], stack(GG_SR_Beta_Data[2:4]))

GG_SR_Beta_Data_ggseg = GG_SR_Beta_Data_ggseg %>% 
  group_by(ind)

Fig1J =  ggplot()+
  geom_brain(atlas=desterieux, data=GG_SR_Beta_Data_ggseg, aes(fill=as.numeric(values)), colour="black", show.legend = FALSE  ) +
  theme_void()+
  theme(legend.position = "bottom", 
        legend.key.height = unit(0.25, "cm"), 
        legend.key.width = unit(3.5, "cm"), 
        legend.text = element_text(size=12),
        legend.title= element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_blank(),
        text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient2(low="blue",high="red", midpoint = 0, na.value = "white", limits=c(-0.25,0.25)) +
  labs(title="Associations with State-Level Racism",fill="β")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")


Fig1K = ggplot()+
  geom_brain(atlas=aseg, data=GG_SR_Beta_Data_ggseg, aes(fill=as.numeric(values)), side = "coronal", colour="black", show.legend = FALSE ) +
  theme_void()+
  theme(#legend.position = "bottom", 
    legend.key.height = unit(2.5, "cm"), 
    legend.text = element_text(size=12),
    legend.title= element_text(size=12),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  scale_fill_gradient2(low="blue",high="red", midpoint = 0, na.value = "white", limits=c(-0.25,0.25)) +
  labs(title="Associations with State-Level Racism",fill="β")+
  facet_wrap(~ind,nrow = 1) +
  theme_brain(text.size=12,text.family="Arial")

SurfacePlot <- Fig1A / Fig1C / Fig1E / Fig1H / Fig1J +
  plot_layout(widths = c(1, 1, 1, 1, 1), heights = c(0.5, 0.5, 0.5, 0.5, 0.5))
AsegPlot <- Fig1B / Fig1D / Fig1F / Fig1I / Fig1K +
  plot_layout(widths = c(1, 1, 1, 1, 1), heights = c(0.5, 0.5, 0.5, 0.5, 0.5))

ggsave("~/R_WorkingDir/Fig1A_Beta_022724.png", plot = Fig1A)
ggsave("~/R_WorkingDir/Fig1C_Beta_022724.png", plot = Fig1C)
ggsave("~/R_WorkingDir/Fig1E_Beta_022724.png", plot = Fig1E)
ggsave("~/R_WorkingDir/Fig1H_Beta_022724.png", plot = Fig1H)
ggsave("~/R_WorkingDir/Fig1J_Beta_022724.png", plot = Fig1J)
ggsave("~/R_WorkingDir/Aseg_Beta_022724.png", plot = AsegPlot)


## Plot Percent Mediated
GG_PM_Data = data.frame(DDThreatRegs, PercentMediated[, 2:4])
colnames(GG_PM_Data) = c('label', 'p-value1','p-value2', 'PercentMediated')
#Remove %m if either the total or ab paths were not significant (not a med)
is.na(GG_PM_Data[4]) <- GG_PM_Data[2] > 0.05
is.na(GG_PM_Data[4]) <- GG_PM_Data[3] > 0.05
GG_PM_Data[is.na(GG_PM_Data)] = 0


Fig2A =  ggplot()+
  geom_brain(atlas=desterieux, data=GG_PM_Data, aes(fill=as.numeric(PercentMediated)), colour="black", show.legend = TRUE ) +
  theme_void()+
  theme(legend.position = "bottom", 
        legend.key.width = unit(2.5, "cm"), 
        legend.text = element_text(size=12),
        legend.title= element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_blank(),
        text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient2(low="red",high="blue", midpoint = 0, na.value = "white", 
                       limits=c(-100,100)) +
  labs(title="Percent Mediated", fill="%")+
  theme_brain(text.size=12,text.family="Arial")

Fig2B =  ggplot()+
  geom_brain(atlas=aseg, data=GG_PM_Data, aes(fill=as.numeric(PercentMediated)), colour="black", show.legend = FALSE ) +
  theme_void()+
  theme(legend.position = "bottom", 
        legend.key.width = unit(2.5, "cm"), 
        legend.text = element_text(size=12),
        legend.title= element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_blank(),
        text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient2(low="red",high="blue", midpoint = 0, na.value = "white", 
                       limits=c(-100,100)) +
  labs(title="Percent Mediated", fill="%")+
  theme_brain(text.size=12,text.family="Arial")

ggsave("~/R_WorkingDir/Fig2A.png", plot = Fig2A)
ggsave("~/R_WorkingDir/Fig2B.png", plot = Fig2B)