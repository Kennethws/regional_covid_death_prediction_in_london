###########################################################
### Topic: STAT0023 ICA2
### Author: Group G 19005054 19000151
###########################################################

###########################################################
### Setup
library(ggplot2)
if (!(names(dev.cur()) %in% c('windows', 'x11cairo'))) x11(width = 10, height = 10)

###########################################################
#### EDA
### load the UKCovidWave data
cat('=================================================================\n')
cat('Load data:\n')
UKCovid <- read.csv('UKCovidWave1.csv', header = 1, na.strings = '-1')
cat('Finish loading\n')


### data description
# data structure: all numeric except Region and RUCode
# 7201 observations in total (death numbers missing for 1800 of them)
cat('=================================================================\n')
cat('Data structure:\n')
str(UKCovid)

# missing value: none
cat('=================================================================\n')
cat('Except for the death numbers to be predicted, there are', sum(is.na(UKCovid)) - 1800, 'missing values.\n')



###################
### dimension reduction
## RUcode
# specify new groups for interpretation and potential interaction
cat('=================================================================\n')
cat('Dimension reduction\n')
cat('=================================================================\n')
cat('RUCode: group 8 levels to 3 levels (new.RU)\n')

# combine 8 levels into 3 levels for the purpose of better reference
# major city (U1), urban city and town(U2), and rural (R)
new.RU <- rep(0, nrow(UKCovid))
new.RU[UKCovid$RUCode %in% c('A1', 'B1')] = 'U1'
new.RU[UKCovid$RUCode %in% c('C1', 'C2')] = 'U2'
new.RU[UKCovid$RUCode %in% c('D1', 'D2', 'E1', 'E2')] = 'R'
UKCovid <- cbind(UKCovid, new.RU)



###################
## population
cat('=================================================================\n')
cat('Population\n')
names.population <- c('PopM', 'PopF', 'PopComm', 'PopDens')

# matrix plot
cat('Matrix plot and correlation\n')
pairs(UKCovid[,names.population])
cor(UKCovid[,names.population])
cat('PopM and PopF highly correlated\n')
# PopM and PopF highly correlated
# keep them for now because gender is known to be a major factor



###################
## household for each MSOA
cat('=================================================================\n')
cat('Household for each MOSA\n')
names.household <- c('HH', 'HH_1Pers', 'HH_1Fam', 'HH_Oth', 
                     'HH_HealthPrb', 'HHNoCH', 'HHRooms', 
                     'HHBedrooms', 'HHDepriv1', 'HHDepriv2',
                     'HHDepriv3', 'HHDepriv4', 'HHAdultUKLang',
                     'HHChildUKLang', 'HHNoUKLang')

# Matrix plot of first 8
cat('Matrix plot of first 8\n')
pairs(UKCovid[, names.household[1:8]])
cor(UKCovid[, names.household[1:8]])
# HHRooms and HHBedrooms highly co-linear, HH and HH_1Pers co-linear
# HH is the sum of the other three types of household and should be deleted due to linear dependence
# keep HH_1Pers, HH_1Fam, HH_Oth as literature says household type might play a big role
# HH_HealthPrb: unhealthy people are supposed to suffer higher risk
# HHNoCH: household without central heating might be safer as virus cannot spread through ventilation
# HHRooms and HHBedrooms are highly co-linear


# Matrix plot of last 7
cat('Matrix plot of last 7\n')
pairs(UKCovid[, names.household[9:15]])
cor(UKCovid[, names.household[9:15]])
# Identified two groups that have co-linear relationship: deprivation and language
# they are both quite co-linear within groups
cat('Identified two groups that have co-linear relationship: deprivation and language\n')


# deprive & language, apply PCA to each to confirm combination
cat('\nPCA of deprivation\n')
pca.deprive <- prcomp(UKCovid[,c('HHDepriv1', 'HHDepriv2',
                              'HHDepriv3', 'HHDepriv4')],
                      scale. = TRUE)
print(pca.deprive, digits = 2)
summary(pca.deprive)$importance
# 3 var retain 98.8% variance
cat('Keep 3 out of 4, sum HHDepriv3 and HHDepriv4\n')
# keep 3 out of 4, sum HHDepriv3 and HHDepriv4
UKCovid$HHDepriv34 <- rowSums(UKCovid[, c('HHDepriv3', 'HHDepriv4')])


# PCA of language
cat('\nPCA of language\n')
pca.lang <- prcomp(UKCovid[,c('HHAdultUKLang',
                           'HHChildUKLang', 'HHNoUKLang')],
                   scale. = TRUE)
print(pca.lang, digits = 2)
summary(pca.lang)$importance
# 1 var already retains 91% variance
cat("These three aren't too related to deaths and PC1 is enough, sum them all\n")
# these three aren't too related to deaths and PC1 is enough, sum them all
UKCovid$HHLang <- rowSums(UKCovid[,c('HHAdultUKLang', 'HHChildUKLang', 'HHNoUKLang')])



###################
### age
# mean and median: whether to keep one or both
cat('=================================================================\n')
cat('Age\n')
cat('MeanAge and MedianAge might be functionally overlapping, so look at their plot and correlation\n')
plot(UKCovid$MeanAge, UKCovid$MedianAge)
cor(UKCovid$MeanAge, UKCovid$MedianAge)
# Delete MeanAge due to high linear relationship, and mean is less robust
cat("Delete MeanAge due to high linear relationship\n")

names.age <- c('Age0.4','Age5.7','Age8.9','Age10.14',
               'Age15','Age16.17','Age18.19','Age20.24',
               'Age25.29','Age30.44','Age45.59','Age60.64',
               'Age65.74','Age75.84','Age85.89','Age90.',
               'MeanAge','MedianAge')

# age groups
cat('Matrix plot\n')
pairs(UKCovid[, names.age[1:8]]) # age 0-17 very clearly linear
cat('Age 0-17 very clearly linear\n')
pairs(UKCovid[, names.age[9:16]]) # age 60-90 clearly linear
cat('Age 60-90 clearly linear\n')

# PCA to check grouping
cat('PCA of age\n')
pca.age <- prcomp(UKCovid[, names.age[1:16]], scale. = TRUE)
print(pca.age, digits = 2)
# PC1 highlights 0-17, PC2 highlights 60-90, PC3 18-29, PC4 30-59 (also considering literature)
summary(pca.age)$importance
cat('Four groups: 0-17, 18-29, 30-59, 60-90.\n')

# 0-17, 18-29, 30-59, 60-90.
UKCovid$Age0.17 <- rowSums(UKCovid[,c('Age0.4','Age5.7','Age8.9','Age10.14','Age15','Age16.17')])
UKCovid$Age18.29 <- rowSums(UKCovid[,c('Age18.19','Age20.24','Age25.29')])
UKCovid$Age30.59 <- rowSums(UKCovid[,c('Age30.44','Age45.59')])
UKCovid$Age60.90. <- rowSums(UKCovid[,c('Age60.64','Age65.74','Age75.84','Age85.89','Age90.')])



###################
### ethnicity and immigration
cat('=================================================================\n')
cat('Ethnicity\n')
names.race <- c('EthWhite', 'EthMixed', 'EthAsian', 'EthBlack',
                'EthOther', 'BornIreland', 'BornEU', 'BornNonEU')


# Matrix plot and correlation
cat('Matrix plot and correlation\n')
pairs(UKCovid[, names.race])
cor(UKCovid[, names.race])
cat('Linear relationship hard to determine, can only keep all for now\n')
# hard to determine, can only keep all for now



###################
### unpaid carers
cat('=================================================================\n')
cat('Unpaid carers\n')

# Matrix plot and correlation
cat('Matrix plot and correlation\n')
pairs(UKCovid[,c('CarersLo', 'CarersMid','CarersHi')])
cor(UKCovid[,c('CarersLo', 'CarersMid','CarersHi')])
cat('All seem somewhat linear, Mid and High closer to each other\n')
# all seem somewhat linear, Mid and High closer to each other

# PCA to check whether keep 1 or 2 variables
cat('PCA of unpaid carers\n')
pca.unpaid <- prcomp(UKCovid[, c('CarersLo', 'CarersMid','CarersHi')],
                     scale. = TRUE)
print(pca.unpaid, digits = 2)
summary(pca.unpaid)$importance
cat("PCA shows it's best to keep 2 variables - take sum of Mid and High\n")
# best to keep 2 variables (take sum of Mid and High)
UKCovid$CarersMidHi <- rowSums(UKCovid[,c('CarersMid','CarersHi')])



###################
### household accommodation - dwell
cat('=================================================================\n')
cat('Dwelling\n')

# dwell and Household seem very similar
cat('Scatterplot of dwell and HH\n')
plot(UKCovid$Dwell, UKCovid$HH, main = 'HH vs. Dwell')
cor(UKCovid$Dwell, UKCovid$HH)
# Dwell is highly co-linear with HH. And since they represent pretty much the same aspect, it is valid to be deleted
cat("Dwell is highly co-linear with HH. And since they represent pretty much the same aspect, it is valid to be deleted.\n")

# dwell highly linear with HH, delete; use PCA to see how to dispose shared
cat('PCA to see how to dispose shared\n')
pca.dwell <- prcomp(UKCovid[, c('DwellShared2', 'DwellShared3.')],
                    scale. = TRUE)
print(pca.dwell, digits = 2)
summary(pca.dwell)$importance
cat('PCA shows we can sum shared2 and shared3\n')
# can sum shared2 and shared3
UKCovid$DwellShared <- rowSums(UKCovid[,c('DwellShared2', 'DwellShared3.')])



###################
### people living in communal establishments
cat('=================================================================\n')
cat('Communal establishments\n')
names.commmunal <- c('CommEstab', 'LACare', 'PrivCareNurs', 'PrivCareNoNurs')

# Matrix plot and correlation
cat('\nMatrix plot and correlation\n')
pairs(UKCovid[,names.commmunal])
cor(UKCovid[,names.commmunal])
# three types of care rooms has no obvious pattern
cat("Doesn't seem valid to delete any variable\n")

# above doesn't seem to delete any variable, use PCA to double-check
cat('\nPCA to double check\n')
pca.communal <- prcomp(UKCovid[,names.commmunal], scale. = TRUE)
print(pca.communal, digits = 2)
summary(pca.communal)$importance
cat('Add two new variable: CommPerPerson, people in hospital\n')

# communal per person, people in hosipital
UKCovid$CommPerPerson <- UKCovid$CommEstab / UKCovid$PopTot
UKCovid$Hospital <- UKCovid$PopComm - rowSums(UKCovid[,c('LACare', 'PrivCareNurs', 'PrivCareNoNurs')])



###################
### employment / occupation
cat('=================================================================\n')
cat('Occupation\n')
names.work <- c('WrkMgr', 'WrkProf', 'WrkProfTech', 'WrkAdmin', 'WrkSkilled', 'WrkCaring', 'WrkSales', 'WrkMachine', 'WrkElementary')

# Matrix plot and correlation
cat('Matrix plot and correlation\n')
pairs(UKCovid[, names.work])
cor(UKCovid[,names.work])

# according to reference Elementary, caring & machine (WrkC) leads to
# overall highest death rate, followed by skilled, sales & admin (WrkB). 
# Mgr, ProfTech, Prof (WrkA) lead to the lowest death rate.
cat('Combine into 3 groups by plots and reference\n')
UKCovid$WrkA <- rowSums(UKCovid[,c('WrkMgr', 'WrkProf', 'WrkProfTech')])
UKCovid$WrkB <- rowSums(UKCovid[,c('WrkSkilled', 'WrkSales', 'WrkAdmin')])
UKCovid$WrkC <- rowSums(UKCovid[,c('WrkElementary', 'WrkMachine', 'WrkCaring')])



###################
### social grade
cat('=================================================================\n')
cat('Social grade\n')

# Matrix plot and correlation
cat('Matrix plot and correlation')
pairs(UKCovid[,c('WrkA','WrkB','WrkC', 'GradeAB', 'GradeC1', 'GradeC2','GradeDE')])
cor(UKCovid[,c('WrkA','WrkB','WrkC', 'GradeAB', 'GradeC1', 'GradeC2','GradeDE')])
cat('WrA vs. Grade AB, WrkB vs. Grade C2, WrkC vs. Grade DE\n')
# WrA vs. Grade AB, WrkB vs. Grade C2, WrkC vs. Grade DE



###################
### public transport
cat('=================================================================\n')
cat('Public transport\n')
names.transport <- c('MetroUsers', 'TrainUsers', 'BusUsers')

# Matrix plot and correlation
cat('Matrix plot and correlation\n')
pairs(UKCovid[,names.transport])
cor(UKCovid[,names.transport])
cat('No linear pattern, keep them all\n')
# no linear pattern, keep them all



###################
### education
cat('=================================================================\n')
cat('Education\n')
names.edu <- c('NoQual', 'Qual1', 'Qual2', 'Qual3', 'Qual4.', 'QualApp','QualOther','Stud18.')

# Matrix plot and correlation
cat('Matrix plot and correlation\n')
pairs(UKCovid[,names.edu])
cor(UKCovid[,names.edu])
cat('Several variables are quite linear with each other, indicating some potential
groups could be formed.\n')

# PCA
cat('\nPCA of education to further grouping\n')
pca.edu <- prcomp(UKCovid[,names.edu], scale. = TRUE)
print(pca.edu, digits = 2)
summary(pca.edu)$importance
cat("'NoQual', 'Qual1', 'Qual2', 'QualApp' - QualLow\n")
cat("'Qual3', 'Qual4.', 'Stud18.' - QualHigh\n")

# 'NoQual', 'Qual1', 'Qual2', 'QualApp' - QualLow
# 'Qual3', 'Qual4.', 'Stud18.' - QualHigh
UKCovid$QualLow <- rowSums(UKCovid[,c('NoQual', 'Qual1', 'Qual2', 'QualApp')])
UKCovid$QualHigh <- rowSums(UKCovid[,c('Qual3', 'Qual4.', 'Stud18.')])

# so far, we've identified 46 variables of interest
cat('\nSo far, 46 variables in total\n')
names.all <- c('Region', 'new.RU', 'PopM', 'PopF', 'PopComm', 'PopDens',
               'HH_1Pers', 'HH_1Fam', 'HH_Oth', 'HH_HealthPrb', 'HHNoCH',
               'HHRooms', 'HHDepriv1', 'HHDepriv2', 'HHDepriv34', 'HHLang',
               'Age0.17', 'Age18.29','Age30.59','Age60.90.','MedianAge',
               'EthWhite', 'EthMixed', 'EthAsian', 'EthBlack', 'EthOther', 
               'BornIreland', 'BornEU', 'BornNonEU', 'CarersLo', 'CarersMidHi', 
               'DwellShared', 'CommPerPerson', 'LACare', 'PrivCareNurs', 
               'PrivCareNoNurs','Hospital', 'WrkA', 'WrkB', 'WrkC', 'MetroUsers', 
               'TrainUsers', 'BusUsers', 'QualLow', 'QualHigh', 'QualOther')



###################
### possible transformation: x distributions
cat('=================================================================\n')
cat('Identify possible transformations\n')
par(mfrow = c(4,2))

tmp <- c('PopComm', 'PopDens', 'HH_Oth', 'HHDepriv34', 'HHLang', 'Age18.29', 'EthWhite', 'EthMixed', 'EthAsian', 'EthBlack', 'EthOther', 
  'BornIreland', 'BornEU', 'BornNonEU', 'CommPerPerson', 'LACare', 'PrivCareNurs', 
  'PrivCareNoNurs','Hospital', 'MetroUsers', 'TrainUsers', 'BusUsers')

# Histograms for all variables that need a square-root transformation
cat('Histograms for all variables that need a square-root transformation\n')
for (i in 1:8) {
  hist(UKCovid[,tmp[i]], main = tmp[i], breaks = 20)
}

for (i in 1:8) {
  hist(sqrt(UKCovid[,tmp[i]]), main = tmp[i], breaks = 20)
}

for (i in 9:16) {
  hist(UKCovid[,tmp[i]], main = tmp[i], breaks = 20)
}

for (i in 9:16) {
  hist(sqrt(UKCovid[,tmp[i]]), main = tmp[i], breaks = 20)
}

for (i in 17:22) {
  hist(UKCovid[,tmp[i]], main = tmp[i], breaks = 20)
}

par(mfrow = c(4,2))
for (i in 17:22) {
  hist(sqrt(UKCovid[,tmp[i]]), main = tmp[i], breaks = 20)
}
# PopComm, PopDens, HHDepriv34, Age18.29, all variables about ethnicity, dwelling, communal
# establishments, and public transport



###################
### detect interaction by region / new.RU (faceting)
cat('=================================================================\n')
cat('Detect possible interaction by region / new.RU\n')
## new.RU
cat('Region is negligible and possible interaction by new.RU are as follows\n')
# population
plot(ggplot(UKCovid, aes(y = Deaths / PopTot, x = sqrt(PopDens), col = new.RU)) + 
       geom_point() + 
       facet_grid(. ~ new.RU) + 
       geom_smooth(method = 'glm') + 
       ggtitle('PopDens faceted by new.RU')) 

# household
plot(ggplot(UKCovid, aes(y = Deaths / PopTot, x = HHRooms, col = new.RU)) + 
       geom_point() + 
       facet_grid(. ~ new.RU) + 
       geom_smooth(method = 'glm') + 
       ggtitle('HHRooms faceted by new.RU')) 

# age
plot(ggplot(UKCovid, aes(y = Deaths / PopTot, x = Age60.90., col = new.RU)) + 
       geom_point() + 
       facet_grid(. ~ new.RU) + 
       geom_smooth(method = 'glm') + 
       ggtitle('Age60.90. faceted by new.RU')) 

# ethnicity
plot(ggplot(UKCovid, aes(y = Deaths / PopTot, x = sqrt(EthAsian), col = new.RU)) + 
       geom_point() + 
       facet_grid(. ~ new.RU) + 
       geom_smooth(method = 'glm') + 
       ggtitle('EthAsian faceted by new.RU')) 

plot(ggplot(UKCovid, aes(y = Deaths / PopTot, x = sqrt(EthBlack), col = new.RU)) + 
       geom_point() + 
       facet_grid(. ~ new.RU) + 
       geom_smooth(method = 'glm') + 
       ggtitle('EthBlack faceted by new.RU')) 

# carers
plot(ggplot(UKCovid, aes(y = Deaths / PopTot, x = sqrt(CarersMidHi), col = new.RU)) + 
       geom_point() + 
       facet_grid(. ~ new.RU) + 
       geom_smooth(method = 'glm') + 
       ggtitle('CarersMidHi faceted by new.RU'))

# communal
plot(ggplot(UKCovid, aes(y = Deaths / PopTot, x = sqrt(CommPerPerson), col = new.RU)) + 
       geom_point() + 
       facet_grid(. ~ new.RU) + 
       geom_smooth(method = 'glm') + 
       ggtitle('CommPerPerson faceted by new.RU'))

# transport
plot(ggplot(UKCovid, aes(y = Deaths / PopTot, x = sqrt(MetroUsers), col = new.RU)) + 
       geom_point() + 
       facet_grid(. ~ new.RU) + 
       geom_smooth(method = 'glm') + 
       ggtitle('MetroUsers faceted by new.RU'))

# edu
plot(ggplot(UKCovid, aes(y = Deaths / PopTot, x = QualHigh, col = new.RU)) + 
       geom_point() + 
       facet_grid(. ~ new.RU) + 
       geom_smooth(method = 'glm') + 
       ggtitle('QualHigh faceted by new.RU'))



###################
### y vs. x plot to select from GLM or GAM
cat('=================================================================\n')
cat('Scatter plot y vs. x to select from GLM or GAM\n')
par(mfrow = c(3,2))
for (i in 1:6) {
  plot(x = UKCovid[,names.all[i+2]], y = UKCovid$Deaths / UKCovid$PopTot,
       xlab = names.all[i+2], ylab = 'Death rate', log = 'y',
       main = paste('Death rate vs.', names.all[i+2]))
}

for (i in 7:12) {
  plot(x = UKCovid[,names.all[i+2]], y = UKCovid$Deaths / UKCovid$PopTot,
       xlab = names.all[i+2], ylab = 'Death rate', log = 'y',
       main = paste('Death rate vs.', names.all[i+2]))
}

# some patterns did look like a poisson PMF and most were not weird
cat('Patterns did look like a poisson PMF\n')



###########################################################
#### model building
cat('=================================================================\n')
cat('Model building\n')

# Model 1: Poisson GLM, log link (ensure modelled means are positive)
cat('\nModel 1: Poisson GLM, log link\n')
model1 <- glm(Deaths ~ offset(log(PopTot)) + Region + new.RU + PopM + PopF + sqrt(PopComm) + sqrt(PopDens)*new.RU + HH_1Pers + HH_1Fam +       
              sqrt(HH_Oth) + HH_HealthPrb + HHNoCH + HHRooms*new.RU + HHDepriv1 + HHDepriv2 + 
              sqrt(HHDepriv34) + sqrt(HHLang) + Age0.17 + sqrt(Age18.29) + Age30.59 + Age60.90.*new.RU + 
              MedianAge + sqrt(EthWhite) + sqrt(EthMixed) + sqrt(EthAsian)*new.RU + sqrt(EthBlack)*new.RU + sqrt(EthOther) +
              sqrt(BornIreland) + sqrt(BornEU) + sqrt(BornNonEU) + sqrt(CarersLo) + sqrt(CarersMidHi)*new.RU + sqrt(DwellShared) +
              sqrt(CommPerPerson)*new.RU + sqrt(LACare) + sqrt(PrivCareNurs) + sqrt(PrivCareNoNurs) + sqrt(Hospital) + WrkA + WrkB + WrkC +
              sqrt(MetroUsers)*new.RU + sqrt(TrainUsers) + sqrt(BusUsers) + QualLow + QualHigh*new.RU + QualOther, 
            data = UKCovid,
            family = poisson(link = 'log'))
summary(model1)
par(mfrow = c(2,2))
plot(model1, which = 1:4, main = 'Model 1')

cat('Pearson residual is :', sum( resid(model1,type="pearson")^2 ) / model1$df.residual, '\n')
cat('Variance of the Pearson residuals is not close to 1, suggesting quasipoisson distribution\n')
# the variance of the Pearson residuals is not close to 1, suggesting quasipoisson distribution


# Model 2: Quasipoisson GLM, log link
cat('\nModel 2: Quasipoisson GLM, log link')
model2 <- glm(Deaths ~ offset(log(PopTot)) + Region + new.RU + PopM + PopF + sqrt(PopComm) + sqrt(PopDens)*new.RU + HH_1Pers + HH_1Fam +       
              sqrt(HH_Oth) + HH_HealthPrb + HHNoCH + HHRooms*new.RU + HHDepriv1 + HHDepriv2 + 
              sqrt(HHDepriv34) + sqrt(HHLang) + Age0.17 + sqrt(Age18.29) + Age30.59 + Age60.90.*new.RU + 
              MedianAge + sqrt(EthWhite) + sqrt(EthMixed) + sqrt(EthAsian)*new.RU + sqrt(EthBlack)*new.RU + sqrt(EthOther) +
              sqrt(BornIreland) + sqrt(BornEU) + sqrt(BornNonEU) + sqrt(CarersLo) + sqrt(CarersMidHi)*new.RU + sqrt(DwellShared) +
              sqrt(CommPerPerson)*new.RU + sqrt(LACare) + sqrt(PrivCareNurs) + sqrt(PrivCareNoNurs) + sqrt(Hospital) + WrkA + WrkB + WrkC +
              sqrt(MetroUsers)*new.RU + sqrt(TrainUsers) + sqrt(BusUsers) + QualLow + QualHigh*new.RU + QualOther, 
            data = UKCovid,
            family = quasipoisson(link = 'log'))
summary(model2)
cat('Diagnostic plots\n')
plot(model2, which = 1:4, main = 'Model 2')


# delete education (all with interaction)
cat("\nModel 2.1: delete education (all with interaction)")
test_edu <- glm(Deaths ~ offset(log(PopTot)) + Region + new.RU + PopM + PopF + sqrt(PopComm) + sqrt(PopDens)*new.RU + HH_1Pers + HH_1Fam +       
                  sqrt(HH_Oth) + HH_HealthPrb + HHNoCH + HHRooms*new.RU + HHDepriv1 + HHDepriv2 + 
                  sqrt(HHDepriv34) + sqrt(HHLang) + Age0.17 + sqrt(Age18.29) + Age30.59 + Age60.90.*new.RU + 
                  MedianAge + sqrt(EthWhite) + sqrt(EthMixed) + sqrt(EthAsian)*new.RU + sqrt(EthBlack)*new.RU + sqrt(EthOther) +
                  sqrt(BornIreland) + sqrt(BornEU) + sqrt(BornNonEU) + sqrt(CarersLo) + sqrt(CarersMidHi)*new.RU + sqrt(DwellShared) +
                  sqrt(CommPerPerson)*new.RU + sqrt(LACare) + sqrt(PrivCareNurs) + sqrt(PrivCareNoNurs) + sqrt(Hospital) + WrkA + WrkB + WrkC +
                  sqrt(MetroUsers)*new.RU + sqrt(TrainUsers) + sqrt(BusUsers), 
                data = UKCovid,
                family = quasipoisson(link = 'log'))
cat('ANOVA\n')
anova(test_edu, model2, test = 'Chi')
summary(test_edu)
plot(test_edu, which = 1:4, main = 'Model 2.1')


# delete dwell
cat('\nModel 2.2: delete Dwellshared\n')
test_dwell <- glm(Deaths ~ offset(log(PopTot)) + Region + new.RU + PopM + PopF + sqrt(PopComm) + sqrt(PopDens)*new.RU + HH_1Pers + HH_1Fam +       
                  sqrt(HH_Oth) + HH_HealthPrb + HHNoCH + HHRooms*new.RU + HHDepriv1 + HHDepriv2 + 
                  sqrt(HHDepriv34) + sqrt(HHLang) + Age0.17 + sqrt(Age18.29) + Age30.59 + Age60.90.*new.RU + 
                  MedianAge + sqrt(EthWhite) + sqrt(EthMixed) + sqrt(EthAsian)*new.RU + sqrt(EthBlack)*new.RU + sqrt(EthOther) +
                  sqrt(BornIreland) + sqrt(BornEU) + sqrt(BornNonEU) + sqrt(CarersLo) + sqrt(CarersMidHi)*new.RU + 
                  sqrt(CommPerPerson)*new.RU + sqrt(LACare) + sqrt(PrivCareNurs) + sqrt(PrivCareNoNurs) + sqrt(Hospital) + WrkA + WrkB + WrkC +
                  sqrt(MetroUsers)*new.RU + sqrt(TrainUsers) + sqrt(BusUsers), 
                data = UKCovid,
                family = quasipoisson(link = 'log'))
cat('ANOVA\n')
anova(test_dwell, test_edu, test = 'Chi')
summary(test_dwell)
plot(test_dwell, which = 1:4, main = 'Model 2.2')


# delete CarersMidHi with interaction
cat('\nModel 2.3: delete CarersMidHi with interaction\n')
test_carers <- glm(Deaths ~ offset(log(PopTot)) + Region + new.RU + PopM + PopF + sqrt(PopComm) + sqrt(PopDens)*new.RU + HH_1Pers + HH_1Fam +       
                    sqrt(HH_Oth) + HH_HealthPrb + HHNoCH + HHRooms*new.RU + HHDepriv1 + HHDepriv2 + 
                    sqrt(HHDepriv34) + sqrt(HHLang) + Age0.17 + sqrt(Age18.29) + Age30.59 + Age60.90.*new.RU + 
                    MedianAge + sqrt(EthWhite) + sqrt(EthMixed) + sqrt(EthAsian)*new.RU + sqrt(EthBlack)*new.RU + sqrt(EthOther) +
                    sqrt(BornIreland) + sqrt(BornEU) + sqrt(BornNonEU) + sqrt(CarersLo) + 
                    sqrt(CommPerPerson)*new.RU + sqrt(LACare) + sqrt(PrivCareNurs) + sqrt(PrivCareNoNurs) + sqrt(Hospital) + WrkA + WrkB + WrkC +
                    sqrt(MetroUsers)*new.RU + sqrt(TrainUsers) + sqrt(BusUsers), 
                  data = UKCovid,
                  family = quasipoisson(link = 'log'))
cat('ANOVA\n')
anova(test_carers, test_dwell, test = 'Chi')
summary(test_carers)
plot(test_carers, which = 1:4, main = 'Model 2.3')


# delete CommPerPerson with interaction, LACare
cat('\nModel 2.4: delete CommPerPerson with interaction, LACare\n')
test_Comm <- glm(Deaths ~ offset(log(PopTot)) + Region + new.RU + PopM + PopF + sqrt(PopComm) + sqrt(PopDens)*new.RU + HH_1Pers + HH_1Fam +       
                     sqrt(HH_Oth) + HH_HealthPrb + HHNoCH + HHRooms*new.RU + HHDepriv1 + HHDepriv2 + 
                     sqrt(HHDepriv34) + sqrt(HHLang) + Age0.17 + sqrt(Age18.29) + Age30.59 + Age60.90.*new.RU + 
                     MedianAge + sqrt(EthWhite) + sqrt(EthMixed) + sqrt(EthAsian)*new.RU + sqrt(EthBlack)*new.RU + sqrt(EthOther) +
                     sqrt(BornIreland) + sqrt(BornEU) + sqrt(BornNonEU) + sqrt(CarersLo) + 
                     sqrt(PrivCareNurs) + sqrt(PrivCareNoNurs) + sqrt(Hospital) + WrkA + WrkB + WrkC +
                     sqrt(MetroUsers)*new.RU + sqrt(TrainUsers) + sqrt(BusUsers), 
                   data = UKCovid,
                   family = quasipoisson(link = 'log'))
cat('ANOVA\n')
anova(test_Comm, test_carers, test = 'Chi')
summary(test_Comm)
plot(test_Comm, which = 1:4, main = 'Model 2.4')


# delete household: HHRooms with interaction, HH_Oth, HHLang, HH_1Fam
cat('\nModel 2.5: delete household - HHRooms with interaction, HH_Oth, HHLang, HH_1Fam\n')
test_HH <- glm(Deaths ~ offset(log(PopTot)) + Region + new.RU + PopM + PopF + sqrt(PopComm) + sqrt(PopDens)*new.RU + HH_1Pers +       
                   HH_HealthPrb + HHNoCH + HHDepriv1 + HHDepriv2 + 
                   sqrt(HHDepriv34) + Age0.17 + sqrt(Age18.29) + Age30.59 + Age60.90.*new.RU + 
                   MedianAge + sqrt(EthWhite) + sqrt(EthMixed) + sqrt(EthAsian)*new.RU + sqrt(EthBlack)*new.RU + sqrt(EthOther) +
                   sqrt(BornIreland) + sqrt(BornEU) + sqrt(BornNonEU) + sqrt(CarersLo) + 
                   sqrt(PrivCareNurs) + sqrt(PrivCareNoNurs) + sqrt(Hospital) + WrkA + WrkB + WrkC +
                   sqrt(MetroUsers)*new.RU + sqrt(TrainUsers) + sqrt(BusUsers), 
                 data = UKCovid,
                 family = quasipoisson(link = 'log'))
cat('ANOVA')
anova(test_HH, test_Comm, test = 'Chi')
summary(test_HH)
plot(test_HH, which = 1:4, main = 'Model 2.5')


# delete population: PopF
cat('\nModel 2.6: delete population - PopF')
test_Pop <- glm(Deaths ~ offset(log(PopTot)) + Region + new.RU + PopM + sqrt(PopComm) + sqrt(PopDens)*new.RU + HH_1Pers +       
                 HH_HealthPrb + HHNoCH + HHDepriv1 + HHDepriv2 + 
                 sqrt(HHDepriv34) + Age0.17 + sqrt(Age18.29) + Age30.59 + Age60.90.*new.RU + 
                 MedianAge + sqrt(EthWhite) + sqrt(EthMixed) + sqrt(EthAsian)*new.RU + sqrt(EthBlack)*new.RU + sqrt(EthOther) +
                 sqrt(BornIreland) + sqrt(BornEU) + sqrt(BornNonEU) + sqrt(CarersLo) + 
                 sqrt(PrivCareNurs) + sqrt(PrivCareNoNurs) + sqrt(Hospital) + WrkA + WrkB + WrkC +
                 sqrt(MetroUsers)*new.RU + sqrt(TrainUsers) + sqrt(BusUsers), 
               data = UKCovid,
               family = quasipoisson(link = 'log'))
cat('ANOVA\n')
anova(test_Pop, test_HH, test = 'Chi')
summary(test_Pop)
plot(test_Pop, which = 1:4, main = 'Model 2.6')


# delete ethnicity: 3 bornplaces
cat('\nFinal Model 3: delete ethnicity - BornIreland, BornEU, BornNonEU')
test_Eth <- glm(Deaths ~ offset(log(PopTot)) + Region + new.RU + PopM + sqrt(PopComm) + sqrt(PopDens)*new.RU + HH_1Pers +       
                  HH_HealthPrb + HHNoCH + HHDepriv1 + HHDepriv2 + 
                  sqrt(HHDepriv34) + Age0.17 + sqrt(Age18.29) + Age30.59 + Age60.90.*new.RU + 
                  MedianAge + sqrt(EthWhite) + sqrt(EthMixed) + sqrt(EthAsian)*new.RU + sqrt(EthBlack)*new.RU + sqrt(EthOther) +
                  sqrt(CarersLo) + 
                  sqrt(PrivCareNurs) + sqrt(PrivCareNoNurs) + sqrt(Hospital) + WrkA + WrkB + WrkC +
                  sqrt(MetroUsers)*new.RU + sqrt(TrainUsers) + sqrt(BusUsers), 
                data = UKCovid,
                family = quasipoisson(link = 'log'))
cat('ANOVA\n')
anova(test_Eth, test_Pop, test = 'Chi')
summary(test_Eth)
plot(test_Eth, which = 1:4, main = 'Model 3')


# Model 3
model3 <- test_Eth



###########################################################
#### prediction
prediction <- predict(model3, newdata = data.frame(UKCovid[5402:7201,]), type = 'response', se.fit = TRUE)
summary(model3)
sigma <- sqrt(prediction$se.fit^2 + prediction$fit * summary(model3)$dispersion)
pred.dat <- cbind(5402:7201, prediction$fit, sigma)

write.table(pred.dat, file = 'ICA2Group_G_pred.dat', sep = " ", col.names = FALSE, row.names = FALSE)



###########################################################
#### all plots used in report
if (!(names(dev.cur()) %in% c('windows', 'x11cairo'))) x11(width = 10, height = 10)
dev.copy(pdf, 'Plots generated for the report.pdf')

pairs(UKCovid[,names.population])

pairs(UKCovid[,names.transport])


par(mfrow = c(4,2))
for (i in 5:8) {
  hist(UKCovid[,tmp[i]], main = tmp[i], breaks = 20)
}

for (i in 5:8) {
  hist(sqrt(UKCovid[,tmp[i]]), main = tmp[i], breaks = 20)
}


par(mfrow = c(1,1))
# population
plot(ggplot(UKCovid, aes(y = Deaths / PopTot, x = sqrt(PopDens), col = new.RU)) + 
       geom_point() + 
       facet_grid(. ~ new.RU) + 
       geom_smooth(method = 'glm') + 
       ggtitle('PopDens faceted by new.RU')) 


par(mfrow = c(3,2))
for (i in 1:6) {
  plot(x = UKCovid[,names.all[i+2]], y = UKCovid$Deaths / UKCovid$PopTot,
       xlab = names.all[i+2], ylab = 'Death rate', log = 'y',
       main = paste('Death rate vs.', names.all[i+2]))
}

for (i in 7:12) {
  plot(x = UKCovid[,names.all[i+2]], y = UKCovid$Deaths / UKCovid$PopTot,
       xlab = names.all[i+2], ylab = 'Death rate', log = 'y',
       main = paste('Death rate vs.', names.all[i+2]))
}

par(mfrow = c(2,2))
plot(model1, which = 1:4, main = 'Model 1')

plot(test_Eth, which = 1:4, main = 'Model 3')

dev.off()