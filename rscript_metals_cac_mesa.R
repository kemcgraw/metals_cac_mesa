library(tidyverse)
library(sas7bdat)
library(haven)
library(tableone)
library(sjlabelled)


##Read MESA SAS Files-----------------------------------------------------------
mesa1 <- read_sas('MESAe1FinalLabel20220125.sas7bdat')
mesa2 <- read_sas("MESAe2FinalLabel20220125.sas7bdat")
mesa3 <- read_sas("MESAe3FinalLabel20220125.sas7bdat")
mesa4 <- read_sas("MESAe4FinalLabel20220125.sas7bdat")
mesa5 <- read_sas('MESAe5_FinalLabel_20220125.sas7bdat')
mesa6 <- read_sas('MESAe6_FinalLabel_20220125.sas7bdat')
mesaev <- read_sas('MESAEvThru2018_20210518.sas7bdat')
mesact <- read_sas('MesaExam12345CT_20210204.sas7bdat')
mesametal <- read_csv('MESA metals and As species_2023_0224.csv') %>% mutate_at('exam', as.factor) #time varying exposure (at exam 1 and 5)
ex1metal <- read_csv('MESA metals and As species_2023_0224.csv') %>% mutate_at('exam', as.factor) %>% filter(exam==1) %>% 
  mutate_at(vars(ends_with("_cr")), .funs = list(ti = ~.)) %>% #creates time invariant exposure at exam 1
  mutate_at(vars(ends_with("_sg")), .funs = list(ti = ~.)) %>% #creates time invariant exposure at exam 1
  mutate_at(vars(ends_with("_ugl")), .funs = list(ti = ~.)) %>% #creates time invariant exposure at exam 1
  dplyr::select(contains("cr_ti", "ugl_ti", "sg_ti","idno")) #creates time invariant exposure at exam 1

mesawater <- read_csv('mesa_water_as_u_2023-0227.csv') %>% mutate_at('idno', as.numeric)
mesaegfr_new <- read_csv('newegfr.csv') %>% select(idno, egfrnrbcrcc1c) %>% #new egfr 
  mutate_at('idno', as.numeric) %>% mutate(newegfr_ti=egfrnrbcrcc1c) %>% select(c(idno,newegfr_ti))
swcs <- read_sas('D://KM//Columbia//MESA//Data//SWCS//MESAe1to5_SWCS_20220615.sas7bdat') %>% #cac-sw
  pivot_longer(-idno, names_to = c('swcs','exam' ),
               names_pattern = "(.*)([0-9])") %>% 
  pivot_wider(names_from = swcs, values_from = value) %>% 
  mutate_at('exam', as.factor)
mesapm <- read_csv('MESA_pm25_AG_JK.csv')%>% mutate_at('idno', as.numeric) #pm2.5
mesacot <- read_sas('cotininefinal11142007.sas7bdat') #cotinine
mesanses <- read_sas('MESAa23_CensTrctSES_20220824.sas7bdat') #census ses


##Join Datasets-----------------------------------------------------------------
m1 <- mesa1[grepl('idno|age|race|gender|site|bmi|educ|income|cig|evsmk|agesmk|cursmk|agequit|cigsday|shndsmk|pkyrs|curalc|alcwk|dmtype|insln|diabet|diabins|htnmed|htn|ldl|lipid|hdl|chol|trig|lipid|diabhx|cholmed|bpmed|dm03|agat|pamvcm|pamcm|pamvcm|egfr|cepgfr|glucose|sbp|dbp|hrtrate|abi|month|season|dyc|visit|glucos|alc|ualbcre1|nhddid1|nprob1c|mlpd1c|group31c|sttn1c', names(mesa1))] %>% 
  dplyr::select(c(idno, age1c, race1c, gender1, bmi1c, bmicat1c, cig1c, pkyrs1c, sbp1c, dbp1c, glucos1c, glucos1u, dm031c, diabet1, insln1c, diabins1,
                  htn1c, ldl1, hdl1, chol1, trig1, agatpm1c, bpmed1, educ1, income1, 
                  cursmk1, cigsday1, shndsmk1, evsmk1, curalc1, egfr1c, cepgfr1c, lipid1c, htnmed1c, htn1c,
                  pamvcm1c, month1, season1, site1c, ualbcre1, nhddid1, nprob1c, mlpd1c, group31c, sttn1c))
m2 <- mesa2[grepl('idno|age|race|gender|site|bmi|educ|income|cig|evsmk|agesmk|cursmk|agequit|cigsday|shndsmk|pkyrs|curalc|alcwk|dmtype|insln|diabet|diabins|htnmed|htn|ldl|lipid|hdl|chol|trig|lipid|diabhx|cholmed|bpmed|dm03|agat|pamvcm|pamcm|pamvcm|egfr|cepgfr|glucose|sbp|dbp|hrtrate|abi|month|season|dyc|visit|glucos|alc', names(mesa2))] %>% 
  dplyr::select(c(idno, bmi2c, cig2c, pkyrs2c, sbp2c, dbp2c, glucos2c, glucos2u, dm032c, insln2c, diabins2,
                  htn2c, ldl2, hdl2, chol2, trig2, agatpm2c, income2, 
                  cursmk2, cigsday2, shndsmk2, curalc2, lipid2c, htnmed2c, htn2c,
                  pamvcm2c, month2, season2, site2c, e12dyc, e12ctdyc))
m3 <- mesa3[grepl('idno|age|race|gender|site|bmi|educ|income|cig|evsmk|agesmk|cursmk|agequit|cigsday|shndsmk|pkyrs|curalc|alcwk|dmtype|insln|diabet|diabins|htnmed|htn|ldl|lipid|hdl|chol|trig|lipid|diabhx|cholmed|bpmed|dm03|agat|pamvcm|pamcm|pamvcm|egfr|cepgfr|glucose|sbp|dbp|hrtrate|abi|month|season|dyc|visit|glucos|alc', names(mesa3))]  %>%
  dplyr::select(c(idno, bmi3c, cig3c, pkyrs3c, sbp3c, dbp3c, glucos3c, glucos3u, dm033c, insln3c, diabins3,
                  htn3c, ldl3, hdl3, chol3, trig3, agatpm3c, income3, 
                  cursmk3, cigsday3, shndsmk3, curalc3, egfr3c, cepgfr3c, lipid3c, htnmed3c, htn3c,
                  pamvcm3c, month3, season3, site3c, e13dyc, e13ctdyc))
m4 <- mesa4[grepl('idno|age|race|gender|site|bmi|educ|income|cig|evsmk|agesmk|cursmk|agequit|cigsday|shndsmk|pkyrs|curalc|alcwk|dmtype|insln|diabet|diabins|htnmed|htn|ldl|lipid|hdl|chol|trig|lipid|diabhx|cholmed|bpmed|dm03|agat|pamvcm|pamcm|pamvcm|egfr|cepgfr|glucose|sbp|dbp|hrtrate|abi|month|season|dyc|visit|glucos|alc', names(mesa4))] %>% 
  dplyr::select(c(idno, bmi4c, cig4c, pkyrs4c, sbp4c, dbp4c, glucos4c, glucos4u, dm034c, insln4c, diabins4,
                  htn4c, ldl4, hdl4, chol4, trig4, agatpm4c, 
                  cursmk4, cigsday4, shndsmk4, curalc4, egfr4c, cepgfr4c, lipid4c, htnmed4c, htn4c,
                  month4, season4, site4c, e14dyc, e14ctdyc))
m5 <- mesa5[grepl('idno|age|race|gender|site|bmi|educ|income|cig|evsmk|agesmk|cursmk|agequit|cigsday|shndsmk|pkyrs|curalc|alcwk|dmtype|insln|diabet|diabins|htnmed|htn|ldl|lipid|hdl|chol|trig|lipid|diabhx|cholmed|bpmed|dm03|agat|pamvcm|pamcm|pamvcm|egfr|cepgfr|glucose|sbp|dbp|hrtrate|abi|month|season|dyc|visit|glucos|alc', names(mesa5))] %>% 
  dplyr::select(c(idno, bmi5c, cig5c, pkyrs5c, sbp5c, dbp5c, glucose5, dm035c, insln5c, diabins5,
                  htn5c, ldl5, hdl5, chol5, trig5, agatpm5c, income5, 
                  cursmk5, cigsday5, curalc5, egfr5c, cepgfr5c, lipid5c, htnmed5c, htn5c,
                  pamvcm5c, month5, season5, site5c, e15dyc, e15ctdyc))
m6 <- mesa6[grepl('idno|age|race|gender|site|bmi|educ|income|cig|evsmk|agesmk|cursmk|agequit|cigsday|shndsmk|pkyrs|curalc|alcwk|dmtype|insln|diabet|diabins|htnmed|htn|ldl|lipid|hdl|chol|trig|lipid|diabhx|cholmed|bpmed|dm03|agat|pamvcm|pamcm|pamvcm|egfr|cepgfr|glucose|sbp|dbp|hrtrate|abi|month|season|dyc|visit|glucos|alc', names(mesa6))] %>% 
  dplyr::select(c(idno, bmi6c, cig6c, pkyrs6c, sbp6c, dbp6c, glucose6, dm036c, insln6c,
                  htn6c, ldl6, hdl6, chol6, trig6, income6, 
                  cursmk6, shndsmk6, curalc6, egfr6c, cepgfr6c, lipid6c, htnmed6c, htn6c,
                  pamvcm6c, month6, season6, site6c, e16dyc)) #agatpmc missing? #e16ctdyc


##need to consider how we will analyze the data - longitudinal data - which variables are time-varying
m <- left_join(m1, m2, by='idno') %>% #join visit 1 and 2
  left_join(m3, by='idno') %>%        #join visit 3
  left_join(m4, by='idno') %>%        #join visit 4
  left_join(m5, by='idno') %>%        #join visit 5
  left_join(m6, by='idno')            #join visit 6

mesa_long <- m %>% 
  mutate(cte11 = 0, #mutate ct time variable because too similar to exam time variable in pivot
         cte12 = e12ctdyc, 
         cte13 = e13ctdyc, 
         cte14 = e14ctdyc, 
         cte15 = e15ctdyc,
         glucos5c = glucose5, #renaming these last two glucoses so are included in timevarying glucose
         glucos6c = glucose6,
         glucu1c = glucos1u, #renaming glucose1u variable so pivot_longer works for both glucose variables
         glucu2c = glucos2u,
         glucu3c = glucos3u, 
         glucu4c = glucos4u,
         egfr_ti = egfr1c, #creating time invarying mdrd egfr var
         cepgfr_ti = cepgfr1c,#creating time invarying ckdepi egfr var
         bmi_ti = bmi1c,
         shndsmk_ti = shndsmk1,
         pkyrs_ti = pkyrs1c,
         cig_ti = cig1c,
         pamvcm_ti = pamvcm1c,
         ldl_ti = ldl1,
         hdl_ti = hdl1,
         dm03_ti = dm031c,
         sbp_ti = sbp1c,
         lipid_ti = lipid1c,
         htn_ti = htn1c,
         htnmed_ti = htnmed1c,
         glucos_ti = glucos1c,
         sttn_ti = sttn1c) %>% 
  dplyr::select(-c(e12ctdyc, e13ctdyc, e14ctdyc,e15ctdyc, glucos1u, glucos2u, glucos3u, glucos4u)) %>%   #remove old ct time variables
  pivot_longer(-c(idno, age1c, race1c, gender1, educ1, site1c, egfr_ti, cepgfr_ti,   #transform from short to long, keep non-time varying variables same
                  bmicat1c, bmi_ti, cig_ti, shndsmk_ti, pkyrs_ti, pamvcm_ti, ldl_ti,
                  hdl_ti, dm03_ti, sbp_ti, lipid_ti, htnmed_ti, htn_ti, glucos_ti,
                  diabet1, ualbcre1,sttn_ti), #if we want all variables at baseline for our models #nprob1c,mlpd1c,group31c
               names_to = c('variable','exam'),           #uses number from variable name for exam number  
               names_pattern = "(.*)([0-9])") %>%         #tells which part of variable to change (number + c) 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate_at(c('race1c', 'gender1', 'educ1', 'exam', 'cig_ti',#change variables to factor class
              'dm03_ti', 'htnmed_ti',
              'site1c'), 
            as.factor) %>% 
  left_join(swcs, by=c('idno', 'exam')) %>%              #join swcs dataset
  left_join(mesact, by='idno') %>%                       #join ct scan dataset
  left_join(mesaev, by='idno') %>%                       #join events dataset
  left_join(ex1metal, by='idno') %>%                     #join ex1metals dataset time invarying exposures for metals of interest only metal_cr_ti
  left_join(mesametal, by=c('idno', 'exam')) %>%         #join mesametals dataset timevarying exposures and whole dataset metal_cr
  left_join(mesawater, by='idno') %>%                    #join mesa water metals dataset time invarying exposure
  left_join(mesapm, by='idno') %>%                       #join mesapm dataset
  left_join(mesacot, by='idno') %>%                      #join mesa cotinine dataset
  left_join(mesaegfr_new, by='idno') %>%                 #join mesa new egfr
  left_join(mesanses %>% filter(exam==1) %>%             #join mesa neighborhood ses
            select(c(idno, inc_medhh, inc_infladj, pov, f1_pc2, f1_pc3, factor_ana)), 
            by='idno') %>%         
  arrange(idno, exam) %>% 
  mutate(e1 = ifelse(is.na(e1), 0, e1), #days since exam1
    time = ifelse(exam==1, 0, e1/365), #days in years since exam1
    time_ct = ifelse(exam==1, 0, cte1/365), #days since ct_scan
    swcs_new = ifelse(swcs>0, swcs, NA_real_), #there are four zeroes in the swcs dataset - removed them
    logswcs = ifelse(swcs>0,log(swcs),NA_real_),     #modelling both ways prelog transform and post - post better bc captures more variance in x)
    swcs_rev = ifelse(revctt>e1, swcs_new, NA_real_), #remove cases with revascularization
    logswcs_rev = log(swcs_rev),                      #log transformed swcs with revasc removed
    agatpm_rev = ifelse(revctt>e1, agatpm, NA_real_), ##remove cases with revascularization
    caczero = ifelse(agatpm>0, 1, 0) %>% as.factor(),
    caczero_rev = ifelse(agatpm_rev>0, 1, 0) %>% as.factor()) %>% 
  select(-ends_with('.x')) 

##Save RDA File-----------------------------------------------------------------
save(mesa_long, file="mesa_long.rda")
write.csv(mesa_long, file = "mesa_long.csv")

##Variable List-----------------------------------------------------------------
#Outcomes: 
#AGATPM: Mean agatston calcium score, phantom-adjusted; coronary revascularization cases removed
#SWCS_NEW: Spatially weighted calcium score
#LOGSWCS: Log transformed spatially weighted calcium score
#AGATPM_REV: Mean agatston calcium score, phantom-adjusted
#SWCS_REV: Spatially weighted calcium score; coronary revascularization cases removed
#LOGSWCS_REV: Log transformed spatially weighted calcium score; coronary revascularization cases removed
#CACZERO: Dichotomous CAC score
#CACZERO_REV: Dichotomous CAC score; coronary revascularization cases removed


#Time Varying Exposures (Exams 1 & 5): 
#sum_uas_asb_cr: Sum of urinary inorganic As, DMA and MMA  adjusted by arsenobetaine and divided by urinary creatinine (ug/g) 
#uba_cr: Urinary barium concentrations divided by urinary creatinine  (ug/g)  
#ucd_cr: Urinary cadmium concentrations divided by urinary creatinine  (ug/g)  
#ucs_cr: Urinary cesium concentrations divided by urinary creatinine  (ug/g)  
#uco_cr: Urinary cobalt concentrations divided by urinary creatinine  (ug/g)  
#ucu_cr: Urinary copper concentrations divided by urinary creatinine  (ug/g)  
#ugd_cr: Urinary gadolinium concentrations divided by urinary creatinine  (ug/g)  
#upb_cr: Urinary lead concentrations divided by urinary creatinine  (ug/g)  
#umn_cr: Urinary manganese concentrations divided by urinary creatinine  (ug/g)
#umo_cr: Urinary molybdenum concentrations divided by urinary creatinine  (ug/g)
#uni_cr: Urinary nickel concentrations divided by urinary creatinine  (ug/g)
#usb_cr: Urinary antimony concentrations divided by urinary creatinine  (ug/g)
#use_cr: Urinary selenium concentrations divided by urinary creatinine  (ug/g)
#usr_cr: Urinary strontium concentrations divided by urinary creatinine  (ug/g)
#utl_cr: Urinary thallium concentrations divided by urinary creatinine  (ug/g)
#uw_cr: Urinary tungsten concentrations divided by urinary creatinine  (ug/g)
#uu_cr: Urinary uranium concentrations divided by urinary creatinine  (ug/g)
#uzn_cr: Urinary zinc concentrations divided by urinary creatinine  (ug/g)

#Time Invarying Exposures: 
#sum_uas_asb_cr_ti: Sum of urinary inorganic As, DMA and MMA  adjusted by arsenobetaine and divided by urinary creatinine (ug/g) 
#uba_cr_ti: Urinary barium concentrations divided by urinary creatinine  (ug/g)  
#ucd_cr_ti: Urinary cadmium concentrations divided by urinary creatinine  (ug/g)  
#ucs_cr_ti: Urinary cesium concentrations divided by urinary creatinine  (ug/g)  
#uco_cr_ti: Urinary cobalt concentrations divided by urinary creatinine  (ug/g)  
#ucu_cr_ti: Urinary copper concentrations divided by urinary creatinine  (ug/g)  
#ugd_cr_ti: Urinary gadolinium concentrations divided by urinary creatinine  (ug/g)  
#upb_cr_ti: Urinary lead concentrations divided by urinary creatinine  (ug/g)  
#umn_cr_ti: Urinary manganese concentrations divided by urinary creatinine  (ug/g)
#umo_cr_ti: Urinary molybdenum concentrations divided by urinary creatinine  (ug/g)
#uni_cr_ti: Urinary nickel concentrations divided by urinary creatinine  (ug/g)
#usb_cr_ti: Urinary antimony concentrations divided by urinary creatinine  (ug/g)
#use_cr_ti: Urinary selenium concentrations divided by urinary creatinine  (ug/g)
#usr_cr_ti: Urinary strontium concentrations divided by urinary creatinine  (ug/g)
#utl_cr_ti: Urinary thallium concentrations divided by urinary creatinine  (ug/g)
#uw_cr_ti: Urinary tungsten concentrations divided by urinary creatinine  (ug/g)
#uu_cr_ti: Urinary uranium concentrations divided by urinary creatinine  (ug/g)
#uzn_cr_ti: Urinary zinc concentrations divided by urinary creatinine  (ug/g)
#as_avg20062011_p: water arsenic
#u_avg20002011_p: water uranium

#Covariates: 
# e1: days since last visit
# cte1: days since last ct scan
# idno:	PARTICIPANT ID NUMBER
# age1c:	AGE AT EXAM 1
# race1c:	RACE / ETHNICITY
# gender1:	GENDER
# site1c:	SITE
# bmi1c:	BODY MASS INDEX (kg)/(m^2)
# cig1c:	CIGARETTE SMOKING STATUS
# pkyrs1c:	EXAM 1 PACK-YEARS OF CIGARETTE SMOKING
# sbp1c:	SEATED SYSTOLIC BLOOD PRESSURE (mmHg)
# dbp1c:	SEATED DIASTOLIC BLOOD PRESSURE (mmHg)
# glucos1c:	FASTING GLUCOSE (mg/dL) CALIBRATED
# glucos1u:	FASTING GLUCOSE (mg/dL) ORIGINAL
# dm031c:	EXAM 1 DIABETES MELLITUS BY 2003 ADA FASTING CRITERIA ALGORITHM
# htnmed1c:	HYPERTENSION MEDICATION
# htn1c:	HYPERTENSION BY JNC VI (1997) CRITERIA
# ldl1:	LDL CHOLESTEROL (mg/dL)
# hdl1:	HDL CHOLESTROL (mg/dL)
# chol1:	TOTAL CHOLESTEROL (mg/dL)
# trig1:	TRIGLYCERIDES (mg/dL)
# lipid1c:	ANY LIPID-LOWERING MEDICATION
# agatpm1c:	MEAN: AGATSTON CALCIUM SCORE, PHANTOM-ADJUSTED
# bpmed1:	TAKING MEDS FOR HYPERTENSION
# evsmk1:	SMOKED AT LEAST 100 CIGARETTES IN LIFETIME
# shndsmk1:	EXPOSURE TO SECOND-HAND SMOKE, HOURS PER WEEK
# alc1c:	ALCOHOL USE
# exercm1c:	TOTAL INTENTIONAL EXERCISE (Q9-15) MET-MIN/WK
# pamcm1c:	MODERATE PHYSICAL ACTIVITY TOTAL (MET-MIN/WK M-SU)
# pavcm1c:	VIGOROUS PHYSICAL ACTIVITY TOTAL (MET-MIN/WK M-SU)
# pamvcm1c:	MODERATE AND VIGOROUS PHYSICAL ACTIVITY TOTAL (MET-MIN/WK M-SU)
# egfr1c:	MDRD EGFR BASED ON EXAM 1 SCALE CR
# egfr1m:	EGFR MISSING VALUE
# egfr1t:	MDRD EGFR BASED ON EXAM 5 SCALE CR
# cepgfr1c:	CKD-EPI EGFR BASED ON EXAM 1 SCALE CR
# cepgfr1t:	CKD-EPI EGFR BASED ON EXAM 5 SCALE CR
# ctsmod1: scanner type




#Used linear mixed effect models with random intercept on subject and random slope on time
#for continuous measures of spatially weighted cac (SWCS)
##baseline mean difference
##annual mean change
##mean difference at follow-up

#Used modified poisson for prevalent and incident CAC
#Used cox hazard models for incident CAC?

##Mixed Model Description------------------------------------------------------

# Mixed model (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4599452/)

# The mixed model jointly models the cross-sectional and longitudinal relationships between
# CVD risk factors and CAC. The cross-sectional terms modeled an estimated baseline and control for that baseline.
# Participants with a variable number of observations, or even a single observation, can be included in analysis.
# The assumption that data are missing completely at random is not required.

# Yiv = [a0 + Xi0a1 + ai] + [tivB0 + WivtivB1 + tivbi] + [UivYi + eiv]

# Yiv = CAC measurement for subject i at vth follow up exam
# Xi0 = time-invariant cross-sectional confounders and risk factors at exam 1 for participant i
# Wiv = possibly time varying longitudinal confounders and risk factors at exam v for participant i
# Uiv = time-varying variables to adjust measurements at exam v for participant i
# tiv = time in years from exam 1 to the vth follow up exam for participant i
# B0  = CAC progression (annual rate of change) in average participants in the reference group
# B1  = coefficients for associations between confounders and risk factors and rate of CAC progression (term of interest)
# a0  = average CAC measurement at exam 1 for participants in the reference group
# a1  = coefficients for associations between exam1 CAC measurements and risk factors or confounders
# Y1  = coefficients for cross-sectional associations between time-varying variables and CAC measurements at all exams
# ai  = participant-specific random intercept
# bi  = participant-specific random slope
# eiv = error associated with Yiv

# [a0 + Xi0a1 + ai] = cross-sectional relationship between the amount of CAC at exam 1 and values of covariates at exam1
# [tivB0 + WivtivB1 + tivbi] =longitudinal relationship to model rate of change
# [UivYi + eiv] = time-varying transient terms that adjust for variables relevant to specific measurements

# natural log of CAC

# time in years from exam 1 to vth follow up exam

# Time Invarying Variables
# age, sex, race, education, household income

# Time Varying Variables
# Season of Examination


#Adjustments
# model1 <- " + age1c + as.factor(gender1) + as.factor(race1c) + as.factor(educ) + as.factor(site)"
# model2 <- paste0(model1, "+  pamvcm + bmi + as.factor(cig) + pkyrs + shndsmk")
# model3 <- paste0(model2, "+ as.factor(dm03)+ as.factor(htn) + ldl + egfr +  log(glucos) + sbp")
# model4 <- paste0(model3, " + log(pm25)")


# adjustments from Gassett et al 
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4599452/  
# fully adjusted model: age at exam1, sex, race, study site, bmi, smoking status
# total cholesterol, hdl, ldl, diabetes status, statin use at exam 1, sbp, dbp, htn, 
# education, household income, neighborhood socioecnomic status index, family history of cvd


# age, sex, race, education, household income, neighborhood socioeconomic status index,
# sbp, dbp, bmi, ldl, hdl, triglycerides, total cholesterol, statin use, antihypertensive use, 
# cig, pkyrs, shndsmk, dm03, family history of CVD, fibrinogen, hscrp, creatinine


##Edit Dataset------------------------------------------------------------------

##removing outliers
mesa_long <- mesa_long %>% 
  mutate_at(vars(ends_with("_cr")), #selects metals that end with _cr  ##replaces all outlier values with NA
            ~(ifelse((. >=(quantile(., na.rm=T, probs = 0.25)-(10*IQR(., na.rm=T))) &
                        . <=(quantile(., na.rm=T, probs = 0.75)+(10*IQR(., na.rm=T)))), ., NA_real_))) %>% 
  mutate_at(vars(ends_with("_cr_ti")), #selects metals that end with _ti  ##replaces all outlier values with NA
            ~(ifelse((. >=(quantile(., na.rm=T, probs = 0.25)-(10*IQR(., na.rm=T))) &
                        . <=(quantile(., na.rm=T, probs = 0.75)+(10*IQR(., na.rm=T)))), ., NA_real_)))

##creating quartiles
quarts <- function(V) cut(V, quantile(V, probs = seq(0, 1, 1/4), na.rm=T), include.lowest=TRUE,labels=c(0,1,2,3))
mesa_long <- mesa_long %>% 
  mutate_at(.vars = (142:168), .funs = list(qrt = quarts)) %>% 
  mutate_at(.vars = (239:256), .funs = list(qrt = quarts))

mesa.final <- mesa_long %>% 
  dplyr::select( "idno", "exam", 
          "ucd_cr_ti", "uco_cr_ti","ucu_cr_ti","uu_cr_ti",  "uw_cr_ti",  "uzn_cr_ti",
          "ucd_cr", "uco_cr", "ucu_cr", "uu_cr", "uw_cr", "uzn_cr",
          'swcs_new', 'logswcs',
          'swcs_rev', "agatpm", "agatpm_rev",
          'caczero_rev','logswcs', 'logswcs_rev', 
          'age1c', 'gender1', 'gender_f', 'race1c', 'race1c_f', 'site1c', 'site1c_f', 'educ', 'educ_f',
          'bmi_ti', 'bmicat1c',  'cig_ti', 
          'pkyrs_ti', 
          'pamvcm_ti', 
          'ldl_ti', 'hdl_ti', 'dm03_ti', 'sbp_ti', 'glucos_ti', 'newegfr_ti', 'time_ct','lipid_ti', 'htnmed_ti',
          'htn_ti') %>% 
  drop_na() %>% 
  filter(uco_cr_ti<2000) %>% ##not sure if this is correct/removed three obs
  filter(ucu_cr_ti<2000) %>% ##removes one obs
  filter(uu_cr_ti<7)

##Table  1. Participant Characteristics------------------------------------------

mesa.final %>% 
  filter(exam==1) %>% 
  summarize_at()

table(mesa.final$dm03_ti)
table(mesa_long$lipid)

mesa.final %>% 
  filter(exam==1) %>%
  group_by(pmcat) %>% 
  summarise(n = n(), med = median(swcs_rev), p25=quantile(swcs_rev, na.rm=T, probs = 0.25), p75=quantile(swcs_rev, na.rm=T, probs = 0.75)) %>%
  ungroup %>% 
  mutate(total = sum(n), rel.freq = n / total, mediqr = paste0(round(med,1), " [",round(p25,1),", ", round(p75,1),"]") ) %>% 
  select(n, mediqr)

mesa.final %>% 
  filter(exam==1) %>%
  group_by(pmcat, caczero) %>% 
  summarise(n = n(), med = median(swcs_rev), p25=quantile(swcs_rev, na.rm=T, probs = 0.25), p75=quantile(swcs_rev, na.rm=T, probs = 0.75)) %>%
  ungroup %>% 
  mutate(total = sum(n), rel.freq = n / total, mediqr = paste0(round(med,2), " [",round(p25,2),", ", round(p75,2),"]")) %>% 
  group_by(pmcat) %>% 
  mutate(cactot = sum(n), cac.freq=(n/cactot)*100, freqp = paste0(round(n,2), " (",round(cac.freq,1),")")) %>% 
  #select(n, mediqr)
  filter(caczero==1) %>% 
  select(freqp)
table(mesa_long$cig_f, mesa_long$cig_ti)

##Figure 1. Metal Levels by Participant Characteristic--------------------------
folder.name <- format(Sys.Date(), format="%y%m%d")
folder.output<- paste0(folder.name,"_Output")
if(!file.exists(folder.output)) {dir.create(file.path(folder.output))}

data.mesa <- mesa.final %>% filter(exam==1); dim(data.mesa) #6734 

# Categorize variables
data.mesa$age.cat <- NA 
data.mesa$age.cat[data.mesa$age1c<55] <- 1                  
data.mesa$age.cat[data.mesa$age1c>=55 & data.mesa$age1c<65] <- 2 
data.mesa$age.cat[data.mesa$age1c>=65] <- 3 

data.mesa$edu.cat <- as.numeric(data.mesa$educ)
data.mesa$edu.cat[data.mesa$educ==0] <- 1                  # High school or less
data.mesa$edu.cat[data.mesa$educ==1] <- 2  # Some college
data.mesa$edu.cat[data.mesa$educ==2] <- 3  # College degree or more

data.mesa$race.cat <- NA 
data.mesa$race.cat[data.mesa$race1c==3] <- "Black"                  # High school or less
data.mesa$race.cat[data.mesa$race1c==2] <- "Chinese"
data.mesa$race.cat[data.mesa$race1c==4] <- "Hispanic/Latino"
data.mesa$race.cat[data.mesa$race1c==1] <- "White"

data.mesa$egfr.cat <- NA
data.mesa$egfr.cat <- 0*(data.mesa$newegfr_ti<60) + 1*(data.mesa$newegfr_ti>=60)
table(mesa.final$race1c_f, mesa.final$race1c)
table(data.mesa$race.cat)

# Delete  missing values
data_m <- data.mesa; dim(data_m)   #6734
data_m <- subset(data_m, !is.na(data_m$age1c)); dim(data_m)     #6733
data_m <- subset(data_m, !is.na(data_m$gender1)); dim(data_m)   
data_m <- subset(data_m, !is.na(data_m$race1c)); dim(data_m)    
data_m <- subset(data_m, !is.na(data_m$site1c)); dim(data_m)    
data_m <- subset(data_m, !is.na(data_m$edu.cat)); dim(data_m)   #6711
data_m <- subset(data_m, !is.na(data_m$cig_ti)); dim(data_m)     
data_m <- subset(data_m, !is.na(data_m$bmi_ti)); dim(data_m)     
mesa.dt <- data_m; dim(mesa.dt) #6711

metals <- c("ucd_cr_ti", # non-essential
            "uw_cr_ti","uu_cr_ti", # non-essential
            "uco_cr_ti", "ucu_cr_ti", "uzn_cr_ti"); length(metals) #17

##Forest Plots - Metal Levels by Participant CHaracteristic

for (i in metals) {
  
  age <- table(mesa.dt$age.cat)
  sex <- table(mesa.dt$gender1)
  race <- table(mesa.dt$race.cat)
  center <- table(mesa.dt$site1c)
  edu <- table(mesa.dt$edu.cat)
  smoke <- table(mesa.dt$cig_ti)
  bmi <- table(mesa.dt$bmicat1c)
  cac <- table(mesa.dt$caczero_rev)
  #data <- table(mesa.dt$lab_method)
  
  cases <- c( age[[1]], age[[2]], age[[3]],
              sex[[1]], sex[[2]],
              race[[1]], race[[4]], race[[3]], race[[2]], 
              center[[1]], center[[2]], center[[3]],center[[4]], center[[5]], center[[6]],
              edu[[1]], edu[[2]], edu[[3]],
              smoke[[1]], smoke[[2]], smoke[[3]],
              bmi[[1]], bmi[[2]], sum(bmi[[3]], bmi[[4]]), 
              cac[[1]], cac[[2]],
              nrow(mesa.dt))
  
  total <- rep(nrow(mesa.dt), length(cases))
  
  var.age.1 <- round(quantile ( mesa.dt %>% filter(age.cat==1) %>% select(i), probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.age.2 <- round(quantile ( mesa.dt %>% filter(age.cat==2) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.age.3 <- round(quantile ( mesa.dt %>% filter(age.cat==3) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  
  var.sex.1 <- round(quantile ( mesa.dt %>% filter(gender1==0) %>% select(i), probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.sex.2 <- round(quantile ( mesa.dt %>% filter(gender1==1) %>% select(i), probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  
  var.center.3 <- round(quantile ( mesa.dt %>% filter(site1c==3) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.center.4 <- round(quantile ( mesa.dt %>% filter(site1c==4) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.center.5 <- round(quantile ( mesa.dt %>% filter(site1c==5) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.center.6 <- round(quantile ( mesa.dt %>% filter(site1c==6) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.center.7 <- round(quantile ( mesa.dt %>% filter(site1c==7) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.center.8 <- round(quantile ( mesa.dt %>% filter(site1c==8) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  
  var.race.1 <- round(quantile ( mesa.dt %>% filter(race.cat=="Black") %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.race.2 <- round(quantile ( mesa.dt %>% filter(race.cat=="Chinese") %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.race.3 <- round(quantile ( mesa.dt %>% filter(race.cat=="Hispanic/Latino") %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.race.4 <- round(quantile ( mesa.dt %>% filter(race.cat=="White") %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  
  var.edu.1 <- round(quantile ( mesa.dt %>% filter(edu.cat==1) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.edu.2 <- round(quantile ( mesa.dt %>% filter(edu.cat==2) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.edu.3 <- round(quantile ( mesa.dt %>% filter(edu.cat==3) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  
  var.smoke.1 <- round(quantile ( mesa.dt %>% filter(cig_ti==0) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.smoke.2 <- round(quantile ( mesa.dt %>% filter(cig_ti==1) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.smoke.3 <- round(quantile ( mesa.dt %>% filter(cig_ti==2) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  
  var.bmi.1 <- round(quantile ( mesa.dt %>% filter(bmicat1c==1) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.bmi.2 <- round(quantile ( mesa.dt %>% filter(bmicat1c==2) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.bmi.3 <- round(quantile ( mesa.dt %>% filter(bmicat1c==3) %>% select(i) , probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  
  var.cac.1 <- round(quantile ( mesa.dt %>% filter(caczero_rev==0) %>% select(i), probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  var.cac.2 <- round(quantile ( mesa.dt %>% filter(caczero_rev==1) %>% select(i), probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  
  var.overall <- round(quantile ( mesa.dt[,i], probs=c(0.5, 0.25, 0.75), na.rm=T), 4)
  
  overall <- paste0(var.overall[[1]]," (", var.overall[[2]], ", ", var.overall[[3]], ")")
  
  var.median <- c ( var.age.1[[1]], var.age.2[[1]], var.age.3[[1]], 
                    var.sex.1[[1]], var.sex.2[[1]],
                    var.race.1[[1]], var.race.3[[1]], var.race.4[[1]],var.race.2[[1]], 
                    var.center.3[[1]], var.center.4[[1]], var.center.5[[1]],var.center.6[[1]],var.center.7[[1]],var.center.8[[1]],
                    var.edu.1[[1]], var.edu.2[[1]], var.edu.3[[1]],
                    var.smoke.1[[1]], var.smoke.2[[1]], var.smoke.3[[1]],
                    var.bmi.1[[1]], var.bmi.2[[1]], var.bmi.3[[1]],
                    var.cac.1[[1]], var.cac.2[[1]],
                    var.overall[[1]])
  
  var.q1 <- c ( var.age.1[[2]], var.age.2[[2]], var.age.3[[2]], 
                var.sex.1[[2]], var.sex.2[[2]],
                var.race.1[[2]], var.race.3[[2]], var.race.4[[2]],var.race.2[[2]], 
                var.center.3[[2]], var.center.4[[2]], var.center.5[[2]],var.center.6[[2]],var.center.7[[2]],var.center.8[[2]],
                var.edu.1[[2]], var.edu.2[[2]], var.edu.3[[2]],
                var.smoke.1[[2]], var.smoke.2[[2]], var.smoke.3[[2]],
                var.bmi.1[[2]], var.bmi.2[[2]], var.bmi.3[[2]],
                var.cac.1[[2]], var.cac.2[[2]],
                var.overall[[2]])
  
  var.q3 <- c ( var.age.1[[3]], var.age.2[[3]], var.age.3[[3]], 
                var.sex.1[[3]], var.sex.2[[3]],
                var.race.1[[3]], var.race.3[[3]], var.race.4[[3]],var.race.2[[3]], 
                var.center.3[[3]], var.center.4[[3]], var.center.5[[3]],var.center.6[[3]],var.center.7[[3]],var.center.8[[3]],
                var.edu.1[[3]], var.edu.2[[3]], var.edu.3[[3]],
                var.smoke.1[[3]], var.smoke.2[[3]], var.smoke.3[[3]],
                var.bmi.1[[3]], var.bmi.2[[3]], var.bmi.3[[3]],
                var.cac.1[[3]], var.cac.2[[3]],
                var.overall[[3]])
  
  data.forestplot1 <- cbind (cases, total, var.median, var.q1, var.q3)
  
  rows <- c("<55", "55-64", ">=65", 
            "Female", "Male",
            "White","Black","Hispanic/Latino","Chinese",
            "Salem", "NY", "Baltimore", "St. Paul", "Chicago", "Los Angeles",
            "≤High school", "Some college", "≥College",
            "Never", "Former", "Current",
            "<25", "25-30", ">30",
            "CAC-AS=N", "CAC-AS=Y",
            "Overall")
  
  data.forestplot1 <- as.data.frame(data.forestplot1, row.names=rows)
  names(data.forestplot1) <- c("cases","total","median","quartile1","quartile3")
  
  or <- data.forestplot1; dim(or) #27 5
  
  group <- c( expression(bold("Age, years")),
              expression(bold("Sex")),
              expression(bold("Race")),
              expression(bold("Center")),
              expression(bold("Education")),
              expression(bold("Smoking status")),
              expression(bold(paste("BMI, kg/",bold(m^{2})))),  
              expression(bold("CAC-AS")),
              expression(bold("Overall")))
  
  label <- c("<55", "55-64", "\u226565", 
             "Female", "Male",
             "White","Black","Hispanic/Latino","Chinese",
             "Salem, NC", "New York, NY", "Baltimore, MD", "St. Paul, MN", "Chicago, IL", "Los Angeles, CA",
             "\u2264High school", "Some college", "\u2265College",
             "Never","Former", "Current",
             "<25", "25-30", ">30",
             "No", "Yes")
  
  name <- paste0("plot.", i)
  assign(name, or)
  
}

y1 <- c(34:32, 30:29, 27:24, 22:17, 15:13, 11:9, 7:5, 3:2, 0.05) #sub groups lines
# it goes in the opposite direction as you could expect
y2 <- c(35, 31, 28, 23, 16, 12, 8, 4, 0.05) #groups lines



##All six for publication
pdf(file=paste0( folder.output , "/Fig 4.3 Forestplot 6 priority (ugg).pdf"), width = 18, height = 14) 
jpeg(file=paste0( folder.output , "/Fig 4.3 Forestplot 6 priority (ugg).jpeg"), units="in", width=18, height=14, res=300)
par(mfrow=c(1,6),mar=c(6, 2, 4, 2),oma=c(2, 26, 1, 1)) #oma: all the paper   #bottom, left, top, right

metals <- c("ucd_cr_ti","uw_cr_ti","uu_cr_ti", # non-essential
            "uco_cr_ti", "ucu_cr_ti", "uzn_cr_ti"); length(metals) #17
metals.select <- c("ucd_cr_ti","uw_cr_ti","uu_cr_ti", # non-essential
                   "uco_cr_ti", "ucu_cr_ti", "uzn_cr_ti"); length(metals.select) #15


for (i in metals.select[1:6]) {
  
  
  if (i=="ucd_cr_ti"){name <- expression(paste("Cd"))}#,",symbol("m"),"g/L"))}
  if (i=="uw_cr_ti"){name <- expression(paste("W"))}#,",symbol("m"),"g/L"))}
  if (i=="uu_cr_ti"){name <- expression(paste("U"))}#,",symbol("m"),"g/L"))}
  if (i=="uco_cr_ti"){name <- expression(paste("Co"))}#,",symbol("m"),"g/L"))}
  if (i=="ucu_cr_ti"){name <- expression(paste("Cu"))}#,",symbol("m"),"g/L"))}
  if (i=="uzn_cr_ti"){name <- expression(paste("Zn"))}#,",symbol("m"),"g/L"))}
  
  plot.metal <- get(paste0("plot.",i))
  
  # Determine values for the forest
  plot.metal$points <- as.numeric(plot.metal$median)
  plot.metal$low <- as.numeric(plot.metal$quartile1) ; min(plot.metal$low) #0.3
  plot.metal$up <- as.numeric(plot.metal$quartile3) ; max(plot.metal$up) #0.7
  min.x <- min(plot.metal$low)
  max.x <- max(plot.metal$up)
  
  # Call the plot
  plot(plot.metal$points,y1,type="n",xlim=c(min.x,max.x),ylim=c(0,max(y1)), xlab="", cex.lab=1.6, ylab="",axes=F)
  # main=expression("Urinary trace elements levels", cex.main=1.8)
  #title( main=c(name,cex.main=1.6, line=0))   # to obtain microgrames : expression(symbol("m")) 
  mtext(side=1, name ,cex=1.4,font=1,line=3)
  
  for(c in 1:(nrow(plot.metal)-1)){
    points(plot.metal$points[c],y1[c], pch=22, bg=1, cex=2)   
    segments(plot.metal$low[c], y1[c], plot.metal$up[c], y1[c], lty=1, lwd=2)
  }
  
  points(plot.metal$points[nrow(plot.metal)],y1[nrow(plot.metal)],pch=18,cex=3,col="black")
  segments(plot.metal$low[nrow(plot.metal)],y1[nrow(plot.metal)],plot.metal$up[nrow(plot.metal)],y1[nrow(plot.metal)],lty=1.4,lwd=1.4)
  
  # Vertical line in the overall median
  segments(plot.metal$points[nrow(plot.metal)],y1[nrow(plot.metal)],plot.metal$points[nrow(plot.metal)],max(y1),lty=8,lwd=1, col="gray25")
  
  labels <- sprintf("%.2f", seq(min.x, max.x, length=4))
  if (i=="uu_cr_ti") { labels <- sprintf("%.3f", seq(min.x, max.x, length=4)) }    
  if (i=="usr_cr_ti") { labels <- sprintf("%.0f", seq(min.x, max.x, length=4)) }      
  
  axis(1, at=seq(min.x, max.x, length=4),labels=labels,cex.axis=1.4, font.axis=1, line=-1, lty=1, lwd=1.2, lwd.ticks=0.5)
  
  # With this condition we only repeated these texts one in each page, selecting the first metal per page
  
  if (i =="ucd_cr_ti"#|i =="uba_cr_ti"|i =="ucd_cr_ti"|i =="ucs_cr_ti" # non-essential
      #|i =="upb_cr_ti"|i =="usr_cr_ti"|i =="utl_cr_ti"|i =="uu_cr_ti"|i =="uw_cr_ti"
  ) {  #i == "ucs_cr" | i == "utl_cr"   ) {
    mtext(side=2, at=y2, group, cex=1.4, font=2, las=1, adj=0, line=23) # There are the title of the groups
    mtext(side=2, at=y1[-nrow(plot.metal)], label,cex=1.4, font=1, las=1, adj=0, line=23) # These are the subgroups
    mtext(side=2, at=y1, plot.metal$cases ,cex=1.4,font=1,las=1,adj=1,line=3) # These are the cases
    mtext(side=2,at=max(y2)+0.6,"N",cex=1.4, font=2,las=1,adj=1,line=3) # this is the N
  }
}

dev.off()



##Table  2. LMM SWCS Mean Differences Table----------------------------------------------
model1 <- "+ age1c + as.factor(gender1) + as.factor(race1c) + as.factor(site1c) + newegfr_ti + cig_ti+ as.factor(educ) + bmi_ti +pkyrs_ti  + pamvcm_ti"   
model2 <- paste0(model1, "+ ldl_ti + hdl_ti  + sbp_ti + dm03_ti + as.factor(lipid_ti) + as.factor(htnmed_ti)") 

exposure <- c("ucd_cr_ti",  "uu_cr_ti", 
              "uw_cr_ti", 
              "uco_cr_ti", "ucu_cr_ti",  "uzn_cr_ti")

increase <- 1

table.metal <- as.character()

for (i in exposure) { #categorized by nonessential and essential metals
  if (i=="ucd_cr"|i=="ucd_cr_ti") { name <- "U.Cadmium,ug/g"}
  if (i=="uw_cr"|i=="uw_cr_ti")  { name <-"U.Tungsten, ug/g"}
  if (i=="uu_cr"|i=="uu_cr_ti")  { name <-"U.Uranium, ug/g" }
  if (i=="uco_cr"|i=="uco_cr_ti")  { name <-"U.Cobalt, ug/g"}
  if (i=="ucu_cr"|i=="ucu_cr_ti")  { name <-"U.Copper, ug/g"}
  if (i=="uzn_cr"|i=="uzn_cr_ti")  { name <-"U.Zinc, ug/g"}
  
  
  table.model <- as.character()
  
  for (loop.model in 1:2){
    
    exposure.percent <- quantile(mesa.final[,i], na.rm=T, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) 
    p10 <- log(exposure.percent["10%"])
    p25 <- log(exposure.percent["25%"])
    p50 <- log(exposure.percent["50%"])
    p75 <- log(exposure.percent["75%"])
    p90 <- log(exposure.percent["90%"])
    
    model.temp.obj <- get( paste0( "model", loop.model ) )
    
    ## ALL
    variables <- paste0("logswcs_rev ~ log(",i,")*time_ct", model.temp.obj)
    
    fit <- lme( as.formula(variables) , data=mesa.final, na.action = na.exclude, control=(msMaxIter=1000), random=~1 + time_ct|idno) 
    n <- length(fixef(fit))
    
    coef.baseline <- fixef(fit)[[2]] ; se.baseline <- sqrt(vcov(fit)[2,2])
    baseline <- increase*c(exp((p75 - p25) * coef.baseline), exp((p75 - p25) *coef.baseline - 1.96*se.baseline), exp((p75 - p25) *coef.baseline + 1.96*se.baseline))
    baseline <- (baseline-1)*100
    
    coef.change <- fixef(fit)[[n]] ; se.change <- sqrt(vcov(fit)[n,n])
    change <- increase*c(exp((p75 - p25) * coef.change), exp((p75 - p25) * coef.change - 1.96*se.change), exp((p75 - p25) * coef.change + 1.96*se.change ))
    change <- (change-1)*100
    
    #Obtain values at follow-up (we considered 10 years of follow-up)
    years <- 10
    coef.followup <- coef.baseline + years*coef.change
    se.followup <- se.baseline + years*se.change
    followup <- increase*c(exp((p75 - p25)*coef.followup), exp((p75 - p25)*coef.followup - 1.96*se.followup), exp((p75 - p25)*coef.followup + 1.96*se.followup))
    followup <- (followup-1)*100
    
    baseline <- convert.to.ci( sprintf( "%.2f", baseline) )
    change <- convert.to.ci( sprintf( "%.2f", change) )
    followup <- convert.to.ci( sprintf( "%.2f", followup) )
    
    if (loop.model==1) { result.all <- cbind(name, loop.model, baseline, change, followup) }
    if (loop.model!=1) { result.all <- cbind("", loop.model, baseline, change, followup) } 
    
    table.model <- rbind(table.model, result.all)
  }
  table.metal <- rbind(table.metal, table.model)
}

folder.name <- format(Sys.Date(), format="%y%m%d")
folder.output<- paste0(folder.name,"_Output")
if(!file.exists(folder.output)) {dir.create(file.path(folder.output))}


colnames(table.metal) <- c("Metal", "Model", "PD at baseline", "Annual change", "PD at 10y follow-up")
file <- paste0(folder.output, "/%Diff of CAC-SW by metal biomarkers_6418.csv")
write.csv(table.metal, file=file)


##Table  2. LMM SWCS Metal Quartiles Table----------------------------------------------

#model1 <- ""  # unadjusted
model1 <- "+ age1c + as.factor(gender1) + as.factor(site1c) + as.factor(race1c) + newegfr_ti + as.factor(educ)+ cig_ti + bmi_ti   + pamvcm_ti+pkyrs_ti"   
model2 <- paste0(model1, "+ ldl_ti + hdl_ti + dm03_ti + sbp_ti + as.factor(lipid_ti) + as.factor(htnmed_ti)") 
model3 <- paste0(model2, "+ pm25_lik_0001_0112_wght") 

exposure <- c("ucd_cr_ti_qrt", "uu_cr_ti_qrt", "uw_cr_ti_qrt", 
              "uco_cr_ti_qrt", "ucu_cr_ti_qrt", "uzn_cr_ti_qrt")

increase <- 1

table.metal <- as.character()

for (i in exposure) { #categorized by nonessential and essential metals
  if (i=="ucd_cr_ti_qrt"|i=="ucd_cr_ti_qrt1"|i=="ucd_cr_ti_qrt2"|i=="ucd_cr_ti_qrt3") { name <- "U.Cadmium,ug/g"}
  if (i=="uu_cr_ti_qrt"|i=="uu_cr_ti_qrt1"|i=="uu_cr_ti_qrt2"|i=="uu_cr_ti_qrt3")  { name <-"U.Uranium, ug/g" }
  if (i=="uw_cr_ti_qrt"|i=="uw_cr_ti_qrt1"|i=="uw_cr_ti_qrt2"|i=="uw_cr_ti_qrt3")  { name <-"U.Tungsten, ug/g"}
  if (i=="uco_cr_ti_qrt"|i=="uco_cr_ti_qrt1"|i=="uco_cr_ti_qrt2"|i=="uco_cr_ti_qrt3")  { name <-"U.Cobalt, ug/g"}
  if (i=="ucu_cr_ti_qrt"|i=="ucu_cr_ti_qrt1"|i=="ucu_cr_ti_qrt2"|i=="ucu_cr_ti_qrt3")  { name <-"U.Copper, ug/g"}
  if (i=="uzn_cr_ti_qrt"|i=="uzn_cr_ti_qrt1"|i=="uzn_cr_ti_qrt2"|i=="uzn_cr_ti_qrt3")  { name <-"U.Zinc, ug/g"}
  

  table.model <- as.character()
  
  for (loop.model in 1:2){
    
    model.temp.obj <- get( paste0( "model", loop.model ) )
    
    ## ALL
    variables <- paste0("logswcs_rev ~ ",i,"*time_ct", model.temp.obj)
    
    fit <- lme( as.formula(variables) , data=mesa.final, na.action = na.exclude, control=lmeControl(opt='optim'), random=~1 + time_ct|idno) 
    
    n <- length(fixef(fit))
    
    coef.baseline.q1 <- fixef(fit)[[2]] ; se.baseline.q1 <- sqrt(vcov(fit)[2,2])
    coef.baseline.q2 <- fixef(fit)[[3]] ; se.baseline.q2 <- sqrt(vcov(fit)[3,3])
    coef.baseline.q3 <- fixef(fit)[[4]] ; se.baseline.q3 <- sqrt(vcov(fit)[4,2])
    baseline.q1 <- increase*c(exp(coef.baseline.q1), exp(coef.baseline.q1 - 1.96*se.baseline.q1), exp(coef.baseline.q1 + 1.96*se.baseline.q1))
    baseline.q2 <- increase*c(exp(coef.baseline.q2), exp(coef.baseline.q2 - 1.96*se.baseline.q2), exp(coef.baseline.q2 + 1.96*se.baseline.q2))
    baseline.q3 <- increase*c(exp(coef.baseline.q3), exp(coef.baseline.q3 - 1.96*se.baseline.q3), exp(coef.baseline.q3 + 1.96*se.baseline.q3))
    baseline.q1 <- (baseline.q1-1)*100
    baseline.q2 <- (baseline.q2-1)*100
    baseline.q3 <- (baseline.q3-1)*100
    
    coef.change.q1 <- fixef(fit)[[n-2]] ; se.change.q1 <- sqrt(vcov(fit)[n-2,n-2])
    coef.change.q2 <- fixef(fit)[[n-1]] ; se.change.q2 <- sqrt(vcov(fit)[n-1,n-1])
    coef.change.q3 <- fixef(fit)[[n]] ; se.change.q3 <- sqrt(vcov(fit)[n,n])
    change.q1 <- increase*c(exp(coef.change.q1), exp(coef.change.q1 - 1.96*se.change.q1), exp(coef.change.q1 + 1.96*se.change.q1 ))
    change.q2 <- increase*c(exp(coef.change.q2), exp(coef.change.q2 - 1.96*se.change.q2), exp(coef.change.q2 + 1.96*se.change.q2 ))
    change.q3 <- increase*c(exp(coef.change.q3), exp(coef.change.q3 - 1.96*se.change.q3), exp(coef.change.q3 + 1.96*se.change.q3 ))
    change.q1 <- (change.q1-1)*100
    change.q2 <- (change.q2-1)*100
    change.q3 <- (change.q3-1)*100
    
    #Obtain values at follow-up (we considered 1 years of follow-up)
    years <- 10
    coef.followup.q1 <- coef.baseline.q1 + years*coef.change.q1
    coef.followup.q2 <- coef.baseline.q2 + years*coef.change.q2
    coef.followup.q3 <- coef.baseline.q3 + years*coef.change.q3
    se.followup.q1 <- sqrt( vcov(fit)[ 2 , 2 ] + vcov(fit)[ n-2 , n-2 ] + 2*vcov(fit)[ 2 , n-2 ] )
    se.followup.q2 <- sqrt( vcov(fit)[ 2 , 2 ] + vcov(fit)[ n-1 , n-1 ] + 2*vcov(fit)[ 2 , n-1 ] )
    se.followup.q3 <- sqrt( vcov(fit)[ 2 , 2 ] + vcov(fit)[ n , n ] + 2*vcov(fit)[ 2 , n ] )
    followup.q1 <- increase*c(exp(coef.followup.q1), exp(coef.followup.q1 - 1.96*se.followup.q1), exp(coef.followup.q1 + 1.96*se.followup.q1))
    followup.q2 <- increase*c(exp(coef.followup.q2), exp(coef.followup.q2 - 1.96*se.followup.q2), exp(coef.followup.q2 + 1.96*se.followup.q2))
    followup.q3 <- increase*c(exp(coef.followup.q3), exp(coef.followup.q3 - 1.96*se.followup.q3), exp(coef.followup.q3 + 1.96*se.followup.q3))
    followup.q1 <- (followup.q1-1)*100
    followup.q2 <- (followup.q2-1)*100
    followup.q3 <- (followup.q3-1)*100
    
    baseline.q1 <- convert.to.ci( sprintf( "%.2f", baseline.q1) )
    baseline.q2 <- convert.to.ci( sprintf( "%.2f", baseline.q2) )
    baseline.q3 <- convert.to.ci( sprintf( "%.2f", baseline.q3) )
    change.q1 <- convert.to.ci( sprintf( "%.2f", change.q1) )
    change.q2 <- convert.to.ci( sprintf( "%.2f", change.q2) )
    change.q3 <- convert.to.ci( sprintf( "%.2f", change.q3) )
    followup.q1 <- convert.to.ci( sprintf( "%.2f", followup.q1) )
    followup.q2 <- convert.to.ci( sprintf( "%.2f", followup.q2) )
    followup.q3 <- convert.to.ci( sprintf( "%.2f", followup.q3) )
    
    if (loop.model==1) { result.all <- cbind(name, loop.model, baseline.q1, baseline.q2, baseline.q3, change.q1,change.q2,change.q3, followup.q1, followup.q2, followup.q3) }
    if (loop.model!=1) { result.all <- cbind("", loop.model, baseline.q1, baseline.q2, baseline.q3, change.q1,change.q2,change.q3, followup.q1, followup.q2, followup.q3) } 
    
    table.model <- rbind(table.model, result.all)
  }
  table.metal <- rbind(table.metal, table.model)
}

folder.name <- format(Sys.Date(), format="%y%m%d")
folder.output<- paste0(folder.name,"_Output")
if(!file.exists(folder.output)) {dir.create(file.path(folder.output))}


colnames(table.metal) <- c("Metal", "Model", "MD at baseline Q1", "MD at baseline Q2","MD at baseline Q3","Annual change Q1","Annual change Q2","Annual change Q3", "MD at follow-up Q1", "MD at follow-up Q2", "MD at follow-up Q3")
file <- paste0(folder.output, "/Mean difference of CAC by metal biomarkers time_invarying exposure_quartiles.6418_nofpg.csv")
write.csv(table.metal, file=file)


##Figure 2. Dose Response---------------------------------------------------------------
mesa.final$time_ct_div10 <- mesa.final$time_ct/10
blue_s<-rgb(red=0, green=0, blue=255, alpha=70, max=255)
red_s<-rgb(red=255, green=0, blue=0, alpha=50, max=255)
green_s<-rgb(red=0, green=255, blue=0, alpha=70, max=255)
orange_s<-rgb(red=255, green=128, blue=0, alpha=70, max=255)
brown_s<-rgb(red=200, green=100, blue=0, alpha=100, max=255)

folder.name <- format(Sys.Date(), format="%y%m%d")
folder.output<- paste0(folder.name,"_Output")
if(!file.exists(folder.output)) {dir.create(file.path(folder.output))}
jpeg(file=paste0( folder.output , "/Fig.SWCS by metal levels custom axis.legend.bottom_CACSW.jpeg"), units="in", width=12, height=8, res=600) 

mat <- matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = T) # 
layout(mat = mat, heights = c(0.5,0.5)) 
par(mar=c(0,6,0,6),oma=c(2,1,2,1), pty="s", xpd=T)   #oma=general ; mar=each plot

#### Cadmium

# Determine splines
percentiles <- quantile(mesa.final$ucd_cr_ti, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))  
mesa.final$ucd_cr_ti.rqs2 <- (pmax(log(mesa.final$ucd_cr_ti) - (log(percentiles ["10%"])), 0 )^2 - pmax(log(mesa.final$ucd_cr_ti) - (log(percentiles ["90%"])), 0 )^2)
mesa.final$ucd_cr_ti.rqs3 <- (pmax(log(mesa.final$ucd_cr_ti) - (log(percentiles ["50%"])), 0 )^2 - pmax(log(mesa.final$ucd_cr_ti) - (log(percentiles ["90%"])), 0 )^2 )

# Define axes of the graphs 
# I have checked that the confidence intervals are pretty wide, I'm going to cut the histogram. 

min <- log(0.06) ##here we log transform because of the cd distribution
max <- log(5)  ## this is what is needed to change ## This is the max/min % of participants included in the graph
vector <- seq(min, max, length=1000)
values <- vector

# Matrix: 10% - 90%  ;  50% - 90%
matrix <- cbind( vector , 
                 pmax( vector - log( percentiles["10%"] ) , 0 )^2 - pmax( vector - log( percentiles["90%"] ) , 0 )^2 ,
                 pmax( vector - log( percentiles["50%"] ) , 0 )^2 - pmax( vector - log( percentiles["90%"] ) , 0 )^2)

# Sweep predictor at the reference value ( 10th percentile )
p10.near <- which.min( abs( matrix[ , 1 ] - log( percentiles["10%"] ))) + 1 #482
matrix <- sweep(matrix,2,matrix[p10.near,])
matrix.long <- cbind(matrix, matrix) ; dim(matrix.long)

# Breaks and Frequencies
wtdtab <- table(log(mesa.final$ucd_cr_ti))
breaks <- seq(min, max, length=13)
breaks[1] <- min

wtdfreq <- NULL
for(c in 1:12) wtdfreq[c] <- sum(wtdtab[breaks[c] <= as.numeric(names(wtdtab)) & 
                                          as.numeric(names(wtdtab)) < breaks[c+1]])/sum(wtdtab)
### Model 1 
fit <- lme(logswcs_rev ~ (log(ucd_cr_ti) +ucd_cr_ti.rqs2 + ucd_cr_ti.rqs3)*time_ct + age1c + as.factor(gender1) + as.factor(site1c) + as.factor(race1c) + newegfr_ti + as.factor(educ)
           + bmi_ti   + pamvcm_ti + as.factor(cig_ti) + ldl_ti + hdl_ti + dm03_ti + sbp_ti + as.factor(lipid_ti) + as.factor(htnmed_ti) + pkyrs_ti, 
           data=mesa.final, na.action = na.exclude, control=lmeControl(opt='optim'), random=~1 + time_ct|idno) 


##Baseline
b.b <- summary(fit)$coefficients$fixed[2:4]
b.varb <- vcov(fit)[ 2:4, 2:4]
b.pred <-  matrix%*%b.b  
b.se <- sqrt(diag(matrix%*%b.varb%*%t(matrix)))
b.ul <- b.pred + 1.96*b.se
b.ll <- b.pred - 1.96*b.se


fit <- lme(logswcs_rev ~ (log(ucd_cr_ti) +ucd_cr_ti.rqs2 + ucd_cr_ti.rqs3)*time_ct_div10 + age1c + as.factor(gender1) + as.factor(site1c) + as.factor(race1c) + newegfr_ti + as.factor(educ)
           + bmi_ti   + pamvcm_ti + as.factor(cig_ti) + ldl_ti + hdl_ti + dm03_ti + sbp_ti + as.factor(lipid_ti) + as.factor(htnmed_ti) + pkyrs_ti, 
           data=mesa.final, na.action = na.exclude, control=lmeControl(opt='optim'), random=~1 + time_ct_div10|idno) 

##followup
n <- length(fixef(fit))
c.b <- summary(fit)$coefficients$fixed[c(2:4, n-2, n-1, n)]
c.b[4:6] <- c.b[4:6]
c.varb <- vcov(fit)[c(2:4, n-2, n-1, n), c(2:4, n-2, n-1, n)]
c.se <- sqrt(diag(matrix.long%*%c.varb%*%t(matrix.long)))
c.pred <- matrix.long%*%c.b
c.ul <- c.pred + 1.96*c.se
c.ll <- c.pred - 1.96*c.se


min.x <- min(values) ; max.x <- log(5.0) # 
min.y <- log(0.5) ; max.y <- log(5)
exp(seq(min.y, max.y, length.out = 4)) 

plot(
  values ,
  b.pred ,
  type = "n" ,
  xlim = c(min.x, max.x) , ylim = c(min.y, max.y) ,
  xlab = "" , ylab = "" , axes = F ) 

rect(
  breaks[1:(length(breaks)-1)],
  rep(min.y,length(breaks)-1),
  breaks[2:length(breaks)],
  min.y + 0.01 + (100/30) * wtdfreq*1.0*(max.y - min.y), ### this puts the histogram and the 0.7 is to change the size of the histogram. To make it larger * >1 
  density=-1,
  border="gray", col="white",
  lty=1, lwd=1.5)

#lines
b.ul[b.ul>max.y] <- max.y
b.ll[b.ll<min.y] <- min.y
polygon(c(values, rev(values)), c(b.ul, rev(b.ll)), col=orange_s,  border=NA)
lines(values,b.pred,lty=1,lwd=4, col="orange")
lines(values[b.pred>b.ll],b.pred[b.pred>b.ll],lty=2,lwd=4, col="orange")
lines(values[b.pred<b.ul],b.pred[b.pred<b.ul],lty=2,lwd=4, col="orange")

##change
c.ul[c.ul>max.y] <- max.y
c.ll[c.ll<min.y] <- min.y
polygon(c(values, rev(values)), c(c.ul, rev(c.ll)), col=blue_s,  border=NA)
lines(values,c.pred,lty=1,lwd=4, col="blue")
lines(values[c.pred>c.ll],c.pred[c.pred>c.ll],lty=2,lwd=4, col="blue")
lines(values[c.pred<c.ul],c.pred[c.pred<c.ul],lty=2,lwd=4, col="blue")

segments(min.x, log(1), max.x, log(1), lty=1, lwd=1) # nule value

# Axis
axis(1,at=seq(min.x, max.x, length=4),
     labels=sprintf("%.2f", exp(seq(min.x, max.x, length=4))),  ###%.2f is the number of decimals to round the axis
     cex.axis=1.1,font.axis=1,line=0,lty=1,lwd=1)
axis(2, at=seq(min.y, max.y, length=4),
     labels= sprintf("%.2f", exp(seq(min.y, max.y, length=4))),
     cex.axis=1.1,font.axis=1,las=1,adj=1,line=0,lty=1,lwd=1)
axis(4,at=seq(min.y, max.y, length=4),labels=seq(0, 30, length=4),
     cex.axis=1.1,font.axis=1,las=1,hadj=0.5,line=0,lty=1,lwd=1, mgp=c(3, 1.5, 0))

mtext(side=1,expression(paste("Cadmium, \u03bcg/g")),cex=1,font=1,line=3)
mtext(side=2,"CAC-SW",cex=0.9,font=1,las=3,line=3.5)
mtext(side=4,"% Exposed participants",cex=0.9,font=1,las=3,line=2.5)

dev.off()


#### Tungsten

# Determine splines
percentiles <- quantile(mesa.final$uw_cr_ti, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))  
mesa.final$uw_cr_ti.rqs2 <- (pmax(log(mesa.final$uw_cr_ti) - (log(percentiles ["10%"])), 0 )^2 - pmax(log(mesa.final$uw_cr_ti) - (log(percentiles ["90%"])), 0 )^2)
mesa.final$uw_cr_ti.rqs3 <- (pmax(log(mesa.final$uw_cr_ti) - (log(percentiles ["50%"])), 0 )^2 - pmax(log(mesa.final$uw_cr_ti) - (log(percentiles ["90%"])), 0 )^2 )

# Define axes of the graphs 
# I have checked that the confidence intervals are pretty wide, I'm going to cut the histogram. 

min <- log(percentiles["0%"]) ##here we log transform because of the cd distribution
max <- log(5)  ## this is what is needed to change ## This is the max/min % of participants included in the graph
vector <- seq(min, max, length=1000)
values <- vector

# Matrix: 10% - 90%  ;  50% - 90%
matrix <- cbind( vector , 
                 pmax( vector - log( percentiles["10%"] ) , 0 )^2 - pmax( vector - log( percentiles["90%"] ) , 0 )^2 ,
                 pmax( vector - log( percentiles["50%"] ) , 0 )^2 - pmax( vector - log( percentiles["90%"] ) , 0 )^2)

# Sweep predictor at the reference value ( 10th percentile )
p10.near <- which.min( abs( matrix[ , 1 ] - log( percentiles["10%"] ))) + 1 #482
matrix <- sweep(matrix,2,matrix[p10.near,])
matrix.long <- cbind(matrix, matrix) ; dim(matrix.long)


# Breaks and Frequencies
wtdtab <- table(log(mesa.final$uw_cr_ti))
breaks <- seq(min, max, length=13)
breaks[1] <- min

wtdfreq <- NULL
for(c in 1:12) wtdfreq[c] <- sum(wtdtab[breaks[c] <= as.numeric(names(wtdtab)) & 
                                          as.numeric(names(wtdtab)) < breaks[c+1]])/sum(wtdtab)
### Model 1 
fit <- lme(logswcs_rev ~ (log(uw_cr_ti) +uw_cr_ti.rqs2 + uw_cr_ti.rqs3)*time_ct + age1c + as.factor(gender1) + as.factor(site1c) + as.factor(race1c) + newegfr_ti + as.factor(educ)
           + bmi_ti   + pamvcm_ti + as.factor(cig_ti) + ldl_ti + hdl_ti + dm03_ti + sbp_ti + as.factor(lipid_ti) + as.factor(htnmed_ti) + pkyrs_ti, 
           data=mesa.final, na.action = na.exclude, control=lmeControl(opt='optim'), random=~1 + time_ct|idno) 


##Baseline
b.b <- summary(fit)$coefficients$fixed[2:4]
b.varb <- vcov(fit)[ 2:4, 2:4]
b.pred <-  matrix%*%b.b  
b.se <- sqrt(diag(matrix%*%b.varb%*%t(matrix)))
b.ul <- b.pred + 1.96*b.se
b.ll <- b.pred - 1.96*b.se


##followup
n <- length(fixef(fit))
c.b <- summary(fit)$coefficients$fixed[c(2:4, n-2, n-1, n)]
c.b[4:6] <- c.b[4:6]
c.varb <- vcov(fit)[c(2:4, n-2, n-1, n), c(2:4, n-2, n-1, n)]
c.se <- sqrt(diag(matrix.long%*%c.varb%*%t(matrix.long)))
c.pred <- matrix.long%*%c.b
c.ul <- c.pred + 1.96*c.se
c.ll <- c.pred - 1.96*c.se


min.x <- min(values) ; max.x <- log(5.0) # 
min.y <- log(0.5) ; max.y <- log(5)
exp(seq(min.y, max.y, length.out = 4)) 


plot(
  values ,
  b.pred ,
  type = "n" ,
  xlim = c(min.x, max.x) , ylim = c(min.y, max.y) ,
  xlab = "" , ylab = "" , axes = F ) 

rect(
  breaks[1:(length(breaks)-1)],
  rep(min.y,length(breaks)-1),
  breaks[2:length(breaks)],
  min.y + 0.01 + (100/30) * wtdfreq*1.0*(max.y - min.y), ### this puts the histogram and the 0.7 is to change the size of the histogram. To make it larger * >1 
  density=-1,
  border="gray", col="white",
  lty=1, lwd=1.5)

#lines
b.ul[b.ul>max.y] <- max.y
b.ll[b.ll<min.y] <- min.y
polygon(c(values, rev(values)), c(b.ul, rev(b.ll)), col=orange_s,  border=NA)
lines(values,b.pred,lty=1,lwd=4, col="orange")
lines(values[b.pred>b.ll],b.pred[b.pred>b.ll],lty=2,lwd=4, col="orange")
lines(values[b.pred<b.ul],b.pred[b.pred<b.ul],lty=2,lwd=4, col="orange")

##change
c.ul[c.ul>max.y] <- max.y
c.ll[c.ll<min.y] <- min.y
polygon(c(values, rev(values)), c(c.ul, rev(c.ll)), col=blue_s,  border=NA)
lines(values,c.pred,lty=1,lwd=4, col="blue")
lines(values[c.pred>c.ll],c.pred[c.pred>c.ll],lty=2,lwd=4, col="blue")
lines(values[c.pred<c.ul],c.pred[c.pred<c.ul],lty=2,lwd=4, col="blue")

segments(min.x, log(1), max.x, log(1), lty=1, lwd=1) # nule value


# Axis
axis(1,at=seq(min.x, max.x, length=4),
     labels=sprintf("%.2f", exp(seq(min.x, max.x, length=4))),  ###%.2f is the number of decimals to round the axis
     cex.axis=1.1,font.axis=1,line=0,lty=1,lwd=1)
axis(2, at=seq(min.y, max.y, length=4),
     labels= sprintf("%.2f", exp(seq(min.y, max.y, length=4))),
     cex.axis=1.1,font.axis=1,las=1,adj=1,line=0,lty=1,lwd=1)
axis(4,at=seq(min.y, max.y, length=4),labels=seq(0, 30, length=4),
     cex.axis=1.1,font.axis=1,las=1,hadj=0.5,line=0,lty=1,lwd=1, mgp=c(3, 1.5, 0))

mtext(side=1,expression(paste("Tungsten, \u03bcg/g")),cex=1,font=1,line=3)
mtext(side=2,"CAC-SW",cex=0.9,font=1,las=3,line=3.5)
mtext(side=4,"% Exposed participants",cex=0.9,font=1,las=3,line=2.5)


#### Uranium

# Determine splines
percentiles <- quantile(mesa.final$uu_cr_ti, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))  
mesa.final$uu_cr_ti.rqs2 <- (pmax(log(mesa.final$uu_cr_ti) - (log(percentiles ["10%"])), 0 )^2 - pmax(log(mesa.final$uu_cr_ti) - (log(percentiles ["90%"])), 0 )^2)
mesa.final$uu_cr_ti.rqs3 <- (pmax(log(mesa.final$uu_cr_ti) - (log(percentiles ["50%"])), 0 )^2 - pmax(log(mesa.final$uu_cr_ti) - (log(percentiles ["90%"])), 0 )^2 )

# Define axes of the graphs 
# I have checked that the confidence intervals are pretty wide, I'm going to cut the histogram. 

min <- log(percentiles["0%"]) ##here we log transform because of the cd distribution
max <- log(0.5)  ## this is what is needed to change ## This is the max/min % of participants included in the graph
vector <- seq(min, max, length=1000)
values <- vector

# Matrix: 10% - 90%  ;  50% - 90%
matrix <- cbind( vector , 
                 pmax( vector - log( percentiles["10%"] ) , 0 )^2 - pmax( vector - log( percentiles["90%"] ) , 0 )^2 ,
                 pmax( vector - log( percentiles["50%"] ) , 0 )^2 - pmax( vector - log( percentiles["90%"] ) , 0 )^2)

# Sweep predictor at the reference value ( 10th percentile )
p10.near <- which.min( abs( matrix[ , 1 ] - log( percentiles["10%"] ))) + 1 #482
matrix <- sweep(matrix,2,matrix[p10.near,])
matrix.long <- cbind(matrix, matrix) ; dim(matrix.long)

# Breaks and Frequencies
wtdtab <- table(log(mesa.final$uu_cr_ti))
breaks <- seq(min, max, length=13)
breaks[1] <- min

wtdfreq <- NULL
for(c in 1:12) wtdfreq[c] <- sum(wtdtab[breaks[c] <= as.numeric(names(wtdtab)) & 
                                          as.numeric(names(wtdtab)) < breaks[c+1]])/sum(wtdtab)
### Model 1 
fit <- lme(logswcs_rev ~ (log(uu_cr_ti) +uu_cr_ti.rqs2 + uu_cr_ti.rqs3)*time_ct + age1c + as.factor(gender1) + as.factor(site1c) + as.factor(race1c) + newegfr_ti + as.factor(educ)
           + bmi_ti   + pamvcm_ti + as.factor(cig_ti) + ldl_ti + hdl_ti + dm03_ti + sbp_ti + as.factor(lipid_ti) + as.factor(htnmed_ti) + pkyrs_ti, 
           data=mesa.final, na.action = na.exclude, control=lmeControl(opt='optim'), random=~1 + time_ct|idno) 


##Baseline
b.b <- summary(fit)$coefficients$fixed[2:4]
b.varb <- vcov(fit)[ 2:4, 2:4]
b.pred <-  matrix%*%b.b  
b.se <- sqrt(diag(matrix%*%b.varb%*%t(matrix)))
b.ul <- b.pred + 1.96*b.se
b.ll <- b.pred - 1.96*b.se

##followup
n <- length(fixef(fit))
c.b <- summary(fit)$coefficients$fixed[c(2:4, n-2, n-1, n)]
c.b[4:6] <- c.b[4:6]
c.varb <- vcov(fit)[c(2:4, n-2, n-1, n), c(2:4, n-2, n-1, n)]
c.se <- sqrt(diag(matrix.long%*%c.varb%*%t(matrix.long)))
c.pred <- matrix.long%*%c.b
c.ul <- c.pred + 1.96*c.se
c.ll <- c.pred - 1.96*c.se


min.x <- min(values) ; max.x <- log(0.5) # 
min.y <- log(0.5) ; max.y <- log(5)
exp(seq(min.y, max.y, length.out = 4)) 



plot(
  values ,
  b.pred ,
  type = "n" ,
  xlim = c(min.x, max.x) , ylim = c(min.y, max.y) ,
  xlab = "" , ylab = "" , axes = F ) 
dev.off()
rect(
  breaks[1:(length(breaks)-1)],
  rep(min.y,length(breaks)-1),
  breaks[2:length(breaks)],
  min.y + 0.01 + (100/30) * wtdfreq*1.0*(max.y - min.y), ### this puts the histogram and the 0.7 is to change the size of the histogram. To make it larger * >1 
  density=-1,
  border="gray", col="white",
  lty=1, lwd=1.5)

#lines
b.ul[b.ul>max.y] <- max.y
b.ll[b.ll<min.y] <- min.y
polygon(c(values, rev(values)), c(b.ul, rev(b.ll)), col=orange_s,  border=NA)
lines(values,b.pred,lty=1,lwd=4, col="orange")
lines(values[b.pred>b.ll],b.pred[b.pred>b.ll],lty=2,lwd=4, col="orange")
lines(values[b.pred<b.ul],b.pred[b.pred<b.ul],lty=2,lwd=4, col="orange")

##change
c.ul[c.ul>max.y] <- max.y
c.ll[c.ll<min.y] <- min.y
polygon(c(values, rev(values)), c(c.ul, rev(c.ll)), col=blue_s,  border=NA)
lines(values,c.pred,lty=1,lwd=4, col="blue")
lines(values[c.pred>c.ll],c.pred[c.pred>c.ll],lty=2,lwd=4, col="blue")
lines(values[c.pred<c.ul],c.pred[c.pred<c.ul],lty=2,lwd=4, col="blue")

segments(min.x, log(1), max.x, log(1), lty=1, lwd=1) # nule value

# Axis
axis(1,at=seq(min.x, max.x, length=4),
     labels=sprintf("%.3f", exp(seq(min.x, max.x, length=4))),  ###%.2f is the number of decimals to round the axis
     cex.axis=1.1,font.axis=1,line=0,lty=1,lwd=1)
axis(2, at=seq(min.y, max.y, length=4),
     labels= sprintf("%.2f", exp(seq(min.y, max.y, length=4))),
     cex.axis=1.1,font.axis=1,las=1,adj=1,line=0,lty=1,lwd=1)
axis(4,at=seq(min.y, max.y, length=4),labels=seq(0, 30, length=4),
     cex.axis=1.1,font.axis=1,las=1,hadj=0.5,line=0,lty=1,lwd=1, mgp=c(3, 1.5, 0))

mtext(side=1,expression(paste("Uranium, \u03bcg/g")),cex=1,font=1,line=3)
mtext(side=2,"CAC-SW",cex=0.9,font=1,las=3,line=3.5)
mtext(side=4,"% Exposed participants",cex=0.9,font=1,las=3,line=2.5)


#### Cobalt
library(nlme)
# Determine splines
percentiles <- quantile(mesa.final$uco_cr_ti, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))  
mesa.final$uco_cr_ti.rqs2 <- (pmax(log(mesa.final$uco_cr_ti) - (log(percentiles ["10%"])), 0 )^2 - pmax(log(mesa.final$uco_cr_ti) - (log(percentiles ["90%"])), 0 )^2)
mesa.final$uco_cr_ti.rqs3 <- (pmax(log(mesa.final$uco_cr_ti) - (log(percentiles ["50%"])), 0 )^2 - pmax(log(mesa.final$uco_cr_ti) - (log(percentiles ["90%"])), 0 )^2 )

# Define axes of the graphs 
# I have checked that the confidence intervals are pretty wide, I'm going to cut the histogram. 

min <- log(0.3) ##here we log transform because of the cd distribution
max <- log(5)  ## this is what is needed to change ## This is the max/min % of participants included in the graph
vector <- seq(min, max, length=1000)
values <- vector

# Matrix: 10% - 90%  ;  50% - 90%
matrix <- cbind( vector , 
                 pmax( vector - log( percentiles["10%"] ) , 0 )^2 - pmax( vector - log( percentiles["90%"] ) , 0 )^2 ,
                 pmax( vector - log( percentiles["50%"] ) , 0 )^2 - pmax( vector - log( percentiles["90%"] ) , 0 )^2)

# Sweep predictor at the reference value ( 10th percentile )
p10.near <- which.min( abs( matrix[ , 1 ] - log( percentiles["10%"] ))) + 1 #482
matrix <- sweep(matrix,2,matrix[p10.near,])
matrix.long <- cbind(matrix, matrix) ; dim(matrix.long)

# Breaks and Frequencies
wtdtab <- table(log(mesa.final$uco_cr_ti))
breaks <- seq(min, max, length=13)
breaks[1] <- min

wtdfreq <- NULL
for(c in 1:12) wtdfreq[c] <- sum(wtdtab[breaks[c] <= as.numeric(names(wtdtab)) & 
                                          as.numeric(names(wtdtab)) < breaks[c+1]])/sum(wtdtab)
### Model 1 
fit <- lme(logswcs_rev ~ (log(uco_cr_ti) +uco_cr_ti.rqs2 + uco_cr_ti.rqs3)*time_ct + age1c + as.factor(gender1) + as.factor(site1c) + as.factor(race1c) + newegfr_ti + as.factor(educ)
           + bmi_ti   + pamvcm_ti + as.factor(cig_ti) + ldl_ti + hdl_ti + dm03_ti + sbp_ti + as.factor(lipid_ti) + as.factor(htnmed_ti) + pkyrs_ti, 
           data=mesa.final, na.action = na.exclude, control=lmeControl(opt='optim'), random=~1 + time_ct|idno) 


##Baseline
b.b <- summary(fit)$coefficients$fixed[2:4]
b.varb <- vcov(fit)[ 2:4, 2:4]
b.pred <-  matrix%*%b.b  
b.se <- sqrt(diag(matrix%*%b.varb%*%t(matrix)))
b.ul <- b.pred + 1.96*b.se
b.ll <- b.pred - 1.96*b.se


##followup
n <- length(fixef(fit))
c.b <- summary(fit)$coefficients$fixed[c(2:4, n-2, n-1, n)]
c.b[4:6] <- c.b[4:6]
c.varb <- vcov(fit)[c(2:4, n-2, n-1, n), c(2:4, n-2, n-1, n)]
c.se <- sqrt(diag(matrix.long%*%c.varb%*%t(matrix.long)))
c.pred <- matrix.long%*%c.b
c.ul <- c.pred + 1.96*c.se
c.ll <- c.pred - 1.96*c.se


min.x <- min(values) ; max.x <- log(5.0) # 
min.y <- log(0.5) ; max.y <- log(5)
exp(seq(min.y, max.y, length.out = 4)) 


# Example for saving as PNG
png("cobalt_cac.png", width = 700, height = 600, res = 120)
plot(
  values ,
  b.pred ,
  type = "n" ,
  xlim = c(min.x, max.x) , ylim = c(min.y, max.y) ,
  xlab = "" , ylab = "" , axes = F ) 

rect(
  breaks[1:(length(breaks)-1)],
  rep(min.y,length(breaks)-1),
  breaks[2:length(breaks)],
  min.y + 0.01 + (100/30) * wtdfreq*1.2*(max.y - min.y), ### this puts the histogram and the 0.7 is to change the size of the histogram. To make it larger * >1 
  density=-1,
  border="gray", col="white",
  lty=1, lwd=1.5)

#lines
b.ul[b.ul>max.y] <- max.y
b.ll[b.ll<min.y] <- min.y
polygon(c(values, rev(values)), c(b.ul, rev(b.ll)), col=orange_s,  border=NA)
lines(values,b.pred,lty=1,lwd=4, col="orange")
lines(values[b.pred>b.ll],b.pred[b.pred>b.ll],lty=2,lwd=4, col="orange")
lines(values[b.pred<b.ul],b.pred[b.pred<b.ul],lty=2,lwd=4, col="orange")

##change
c.ul[c.ul>max.y] <- max.y
c.ll[c.ll<min.y] <- min.y
polygon(c(values, rev(values)), c(c.ul, rev(c.ll)), col=blue_s,  border=NA)
lines(values,c.pred,lty=1,lwd=4, col="blue")
lines(values[c.pred>c.ll],c.pred[c.pred>c.ll],lty=2,lwd=4, col="blue")
lines(values[c.pred<c.ul],c.pred[c.pred<c.ul],lty=2,lwd=4, col="blue")

segments(min.x, log(1), max.x, log(1), lty=1, lwd=1) # nule value

# Axis
axis(1,at=seq(min.x, max.x, length=4),
     labels=sprintf("%.2f", exp(seq(min.x, max.x, length=4))),  ###%.2f is the number of decimals to round the axis
     cex.axis=1.1,font.axis=1,line=0,lty=1,lwd=1)
axis(2, at=seq(min.y, max.y, length=4),
     labels= sprintf("%.2f", exp(seq(min.y, max.y, length=4))),
     cex.axis=1.1,font.axis=1,las=1,adj=1,line=0,lty=1,lwd=1)
axis(4,at=seq(min.y, max.y, length=4),labels=seq(0, 30, length=4),
     cex.axis=1.1,font.axis=1,las=1,hadj=0.5,line=0,lty=1,lwd=1, mgp=c(3, 1.5, 0))

mtext(side=1,expression(paste("Cobalt, \u03bcg/g")),cex=1,font=1,line=3)
mtext(side=2,"CAC-SW",cex=0.9,font=1,las=3,line=3.5)
mtext(side=4,"% Exposed participants",cex=0.9,font=1,las=3,line=2.5)




#### Copper

# Determine splines
percentiles <- quantile(mesa.final$ucu_cr_ti, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))  
mesa.final$ucu_cr_ti.rqs2 <- (pmax(log(mesa.final$ucu_cr_ti) - (log(percentiles ["10%"])), 0 )^2 - pmax(log(mesa.final$ucu_cr_ti) - (log(percentiles ["90%"])), 0 )^2)
mesa.final$ucu_cr_ti.rqs3 <- (pmax(log(mesa.final$ucu_cr_ti) - (log(percentiles ["50%"])), 0 )^2 - pmax(log(mesa.final$ucu_cr_ti) - (log(percentiles ["90%"])), 0 )^2 )

# Define axes of the graphs 
# I have checked that the confidence intervals are pretty wide, I'm going to cut the histogram. 

min <- log(4) ##here we log transform because of the cd distribution
max <- log(60)  ## this is what is needed to change ## This is the max/min % of participants included in the graph
vector <- seq(min, max, length=1000)
values <- vector

# Matrix: 10% - 90%  ;  50% - 90%
matrix <- cbind( vector , 
                 pmax( vector - log( percentiles["10%"] ) , 0 )^2 - pmax( vector - log( percentiles["90%"] ) , 0 )^2 ,
                 pmax( vector - log( percentiles["50%"] ) , 0 )^2 - pmax( vector - log( percentiles["90%"] ) , 0 )^2)

# Sweep predictor at the reference value ( 10th percentile )
p10.near <- which.min( abs( matrix[ , 1 ] - log( percentiles["10%"] ))) + 1 #482
matrix <- sweep(matrix,2,matrix[p10.near,])
matrix.long <- cbind(matrix, matrix) ; dim(matrix.long)


# Breaks and Frequencies
wtdtab <- table(log(mesa.final$ucu_cr_ti))
breaks <- seq(min, max, length=13)
breaks[1] <- min

wtdfreq <- NULL
for(c in 1:12) wtdfreq[c] <- sum(wtdtab[breaks[c] <= as.numeric(names(wtdtab)) & 
                                          as.numeric(names(wtdtab)) < breaks[c+1]])/sum(wtdtab)
### Model 1 
fit <- lme(logswcs_rev ~ (log(ucu_cr_ti) +ucu_cr_ti.rqs2 + ucu_cr_ti.rqs3)*time_ct + age1c + as.factor(gender1) + as.factor(site1c) + as.factor(race1c) + newegfr_ti + as.factor(educ)
           + bmi_ti   + pamvcm_ti + as.factor(cig_ti) + ldl_ti + hdl_ti + dm03_ti + sbp_ti + as.factor(lipid_ti) + as.factor(htnmed_ti) + pkyrs_ti, 
           data=mesa.final, na.action = na.exclude, control=lmeControl(opt='optim'), random=~1 + time_ct|idno) 


##Baseline
b.b <- summary(fit)$coefficients$fixed[2:4]
b.varb <- vcov(fit)[ 2:4, 2:4]
b.pred <-  matrix%*%b.b  
b.se <- sqrt(diag(matrix%*%b.varb%*%t(matrix)))
b.ul <- b.pred + 1.96*b.se
b.ll <- b.pred - 1.96*b.se

##followup
n <- length(fixef(fit))
c.b <- summary(fit)$coefficients$fixed[c(2:4, n-2, n-1, n)]
c.b[4:6] <- c.b[4:6]
c.varb <- vcov(fit)[c(2:4, n-2, n-1, n), c(2:4, n-2, n-1, n)]
c.se <- sqrt(diag(matrix.long%*%c.varb%*%t(matrix.long)))
c.pred <- matrix.long%*%c.b
c.ul <- c.pred + 1.96*c.se
c.ll <- c.pred - 1.96*c.se


min.x <- min(values) ; max.x <- max(values) # 
min.y <- log(0.5) ; max.y <- log(5)
exp(seq(min.y, max.y, length.out = 4)) 


plot(
  values ,
  b.pred ,
  type = "n" ,
  xlim = c(min.x, max.x) , ylim = c(min.y, max.y) ,
  xlab = "" , ylab = "" , axes = F ) 

rect(
  breaks[1:(length(breaks)-1)],
  rep(min.y,length(breaks)-1),
  breaks[2:length(breaks)],
  min.y + 0.01 + (100/30) * wtdfreq*1.0*(max.y - min.y), ### this puts the histogram and the 0.7 is to change the size of the histogram. To make it larger * >1 
  density=-1,
  border="gray", col="white",
  lty=1, lwd=1.5)

#lines
b.ul[b.ul>max.y] <- max.y
b.ll[b.ll<min.y] <- min.y
polygon(c(values, rev(values)), c(b.ul, rev(b.ll)), col=orange_s,  border=NA)
lines(values,b.pred,lty=1,lwd=4, col="orange")
lines(values[b.pred>b.ll],b.pred[b.pred>b.ll],lty=2,lwd=4, col="orange")
lines(values[b.pred<b.ul],b.pred[b.pred<b.ul],lty=2,lwd=4, col="orange")

##change
c.ul[c.ul>max.y] <- max.y
c.ll[c.ll<min.y] <- min.y
polygon(c(values, rev(values)), c(c.ul, rev(c.ll)), col=blue_s,  border=NA)
lines(values,c.pred,lty=1,lwd=4, col="blue")
lines(values[c.pred>c.ll],c.pred[c.pred>c.ll],lty=2,lwd=4, col="blue")
lines(values[c.pred<c.ul],c.pred[c.pred<c.ul],lty=2,lwd=4, col="blue")

segments(min.x, log(1), max.x, log(1), lty=1, lwd=1) # nule value

# Axis
axis(1,at=seq(min.x, max.x, length=4),
     labels=sprintf("%.2f", exp(seq(min.x, max.x, length=4))),  ###%.2f is the number of decimals to round the axis
     cex.axis=1.1,font.axis=1,line=0,lty=1,lwd=1)
axis(2, at=seq(min.y, max.y, length=4),
     labels= sprintf("%.2f", exp(seq(min.y, max.y, length=4))),
     cex.axis=1.1,font.axis=1,las=1,adj=1,line=0,lty=1,lwd=1)
axis(4,at=seq(min.y, max.y, length=4),labels=seq(0, 30, length=4),
     cex.axis=1.1,font.axis=1,las=1,hadj=0.5,line=0,lty=1,lwd=1, mgp=c(3, 1.5, 0))

mtext(side=1,expression(paste("Copper, \u03bcg/g")),cex=1,font=1,line=3)
mtext(side=2,"CAC-SW",cex=0.9,font=1,las=3,line=3.5)
mtext(side=4,"% Exposed participants",cex=0.9,font=1,las=3,line=2.5)

#### Zinc

# Determine splines
percentiles <- quantile(mesa.final$uzn_cr_ti, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))  
mesa.final$uzn_cr_ti.rqs2 <- (pmax(log(mesa.final$uzn_cr_ti) - (log(percentiles ["10%"])), 0 )^2 - pmax(log(mesa.final$uzn_cr_ti) - (log(percentiles ["90%"])), 0 )^2)
mesa.final$uzn_cr_ti.rqs3 <- (pmax(log(mesa.final$uzn_cr_ti) - (log(percentiles ["50%"])), 0 )^2 - pmax(log(mesa.final$uzn_cr_ti) - (log(percentiles ["90%"])), 0 )^2 )

# Define axes of the graphs 
# I have checked that the confidence intervals are pretty wide, I'm going to cut the histogram. 

min <- log(50) ##here we log transform because of the cd distribution
max <- log(5000)  ## this is what is needed to change ## This is the max/min % of participants included in the graph
vector <- seq(min, max, length=1000)
values <- vector

# Matrix: 10% - 90%  ;  50% - 90%
matrix <- cbind( vector , 
                 pmax( vector - log( percentiles["10%"] ) , 0 )^2 - pmax( vector - log( percentiles["90%"] ) , 0 )^2 ,
                 pmax( vector - log( percentiles["50%"] ) , 0 )^2 - pmax( vector - log( percentiles["90%"] ) , 0 )^2)

# Sweep predictor at the reference value ( 10th percentile )
p10.near <- which.min( abs( matrix[ , 1 ] - log( percentiles["10%"] ))) + 1 #482
matrix <- sweep(matrix,2,matrix[p10.near,])
matrix.long <- cbind(matrix, matrix) ; dim(matrix.long)


# Breaks and Frequencies
wtdtab <- table(log(mesa.final$uzn_cr_ti))
breaks <- seq(min, max, length=13)
breaks[1] <- min

wtdfreq <- NULL
for(c in 1:12) wtdfreq[c] <- sum(wtdtab[breaks[c] <= as.numeric(names(wtdtab)) & 
                                          as.numeric(names(wtdtab)) < breaks[c+1]])/sum(wtdtab)
### Model 1 
fit <- lme(logswcs_rev ~ (log(uzn_cr_ti) +uzn_cr_ti.rqs2 + uzn_cr_ti.rqs3)*time_ct + age1c + as.factor(gender1) + as.factor(site1c) + as.factor(race1c) + newegfr_ti + as.factor(educ)
           + bmi_ti   + pamvcm_ti + as.factor(cig_ti) + ldl_ti + hdl_ti + dm03_ti + sbp_ti + as.factor(lipid_ti) + as.factor(htnmed_ti) + pkyrs_ti, 
           data=mesa.final, na.action = na.exclude, control=lmeControl(opt='optim'), random=~1 + time_ct|idno) 


##Baseline
b.b <- summary(fit)$coefficients$fixed[2:4]
b.varb <- vcov(fit)[ 2:4, 2:4]
b.pred <-  matrix%*%b.b  
b.se <- sqrt(diag(matrix%*%b.varb%*%t(matrix)))
b.ul <- b.pred + 1.96*b.se
b.ll <- b.pred - 1.96*b.se


##followup
n <- length(fixef(fit))
c.b <- summary(fit)$coefficients$fixed[c(2:4, n-2, n-1, n)]
c.b[4:6] <- c.b[4:6]
c.varb <- vcov(fit)[c(2:4, n-2, n-1, n), c(2:4, n-2, n-1, n)]
c.se <- sqrt(diag(matrix.long%*%c.varb%*%t(matrix.long)))
c.pred <- matrix.long%*%c.b
c.ul <- c.pred + 1.96*c.se
c.ll <- c.pred - 1.96*c.se


min.x <- min(values) ; max.x <- log(5000.0) # 
min.y <- log(0.5) ; max.y <- log(5)
exp(seq(min.y, max.y, length.out = 4)) 



plot(
  values ,
  b.pred ,
  type = "n" ,
  xlim = c(min.x, max.x) , ylim = c(min.y, max.y) ,
  xlab = "" , ylab = "" , axes = F ) 

rect(
  breaks[1:(length(breaks)-1)],
  rep(min.y,length(breaks)-1),
  breaks[2:length(breaks)],
  min.y + 0.01 + (100/30) * wtdfreq*1.0*(max.y - min.y), ### this puts the histogram and the 0.7 is to change the size of the histogram. To make it larger * >1 
  density=-1,
  border="gray", col="white",
  lty=1, lwd=1.5)

#lines
b.ul[b.ul>max.y] <- max.y
b.ll[b.ll<min.y] <- min.y
polygon(c(values, rev(values)), c(b.ul, rev(b.ll)), col=orange_s,  border=NA)
lines(values,b.pred,lty=1,lwd=4, col="orange")
lines(values[b.pred>b.ll],b.pred[b.pred>b.ll],lty=2,lwd=4, col="orange")
lines(values[b.pred<b.ul],b.pred[b.pred<b.ul],lty=2,lwd=4, col="orange")

##change
c.ul[c.ul>max.y] <- max.y
c.ll[c.ll<min.y] <- min.y
polygon(c(values, rev(values)), c(c.ul, rev(c.ll)), col=blue_s,  border=NA)
lines(values,c.pred,lty=1,lwd=4, col="blue")
lines(values[c.pred>c.ll],c.pred[c.pred>c.ll],lty=2,lwd=4, col="blue")
lines(values[c.pred<c.ul],c.pred[c.pred<c.ul],lty=2,lwd=4, col="blue")

segments(min.x, log(1), max.x, log(1), lty=1, lwd=1) # nule value

# Axis
axis(1,at=seq(min.x, max.x, length=4),
     labels=sprintf("%.2f", exp(seq(min.x, max.x, length=4))),  ###%.2f is the number of decimals to round the axis
     cex.axis=1.1,font.axis=1,line=0,lty=1,lwd=1)
axis(2, at=seq(min.y, max.y, length=4),
     labels= sprintf("%.2f", exp(seq(min.y, max.y, length=4))),
     cex.axis=1.1,font.axis=1,las=1,adj=1,line=0,lty=1,lwd=1)
axis(4,at=seq(min.y, max.y, length=4),labels=seq(0, 30, length=4),
     cex.axis=1.1,font.axis=1,las=1,hadj=0.5,line=0,lty=1,lwd=1, mgp=c(3, 1.5, 0))

mtext(side=1,expression(paste("Zinc, \u03bcg/g")),cex=1,font=1,line=3)
mtext(side=2,"CAC-SW",cex=0.9,font=1,las=3,line=3.5)
mtext(side=4,"% Exposed participants",cex=0.9,font=1,las=3,line=2.5)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE, cex=1)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = c("Baseline", "10-Year Cumulative Change"), col = c("orange", "blue"), lwd = 3, xpd = TRUE, horiz = TRUE, cex = 1, seg.len=1, bty = 'n')

dev.off()

##Figure 3. BKMR--------------------------------------------------------------------------
library(tidylog)
library(Hmisc)
library(kableExtra)
library(broom)
library(janitor)
library(DT)
library(stargazer)
library(ggthemes)
library(survey)
library(bkmr)
library(fields)
library(foreign)
library(Hmisc)
library(ggcorrplot)
library(Hmisc)
library(tableone)
library(broom)
library(lars)
library(glmnet)
library(kableExtra)
library(caret)
library(gtools)
library(bkmr)

mesa.final <- mesa.final %>% filter(exam==1) %>% 
  select('age1c', 'site1c', 'gender1', 'race1c', 'newegfr_ti', 'educ',
         'cig_ti', 'bmi_ti', 'pamvcm_ti', 'ldl_ti','hdl_ti', 'dm03_ti', 'sbp_ti',
         'lipid_ti', 'htnmed_ti', 'glucos_ti', 'pkyrs_ti',
         "ucd_cr_ti",  "uco_cr_ti", "ucu_cr_ti","uu_cr_ti",  "uw_cr_ti",  "uzn_cr_ti", "logswcs_rev") %>% 
  na.omit() # Delete missing values 


# Exposures (to introduce in the kernel)
cdk <- (log(mesa.final$ucd_cr_ti) - mean(log(mesa.final$ucd_cr_ti)))/sd(log(mesa.final$ucd_cr_ti), na.rm = T)
cok <- (log(mesa.final$uco_cr_ti) - mean(log(mesa.final$uco_cr_ti)))/sd(log(mesa.final$uco_cr_ti), na.rm = T)
cuk <- (log(mesa.final$ucu_cr_ti) - mean(log(mesa.final$ucu_cr_ti)))/sd(log(mesa.final$ucu_cr_ti), na.rm = T)
uk <- (log(mesa.final$uu_cr_ti) - mean(log(mesa.final$uu_cr_ti)))/sd(log(mesa.final$uu_cr_ti), na.rm = T)
wk <- (log(mesa.final$uw_cr_ti) - mean(log(mesa.final$uw_cr_ti)))/sd(log(mesa.final$uw_cr_ti), na.rm = T)
znk <- (log(mesa.final$uzn_cr_ti) - mean(log(mesa.final$uzn_cr_ti)))/sd(log(mesa.final$uzn_cr_ti), na.rm = T)

Z<-cbind(cdk, cok, cuk, uk, wk, znk)


names(Z) <- c("Cd", "Co", "Cu", "U", "W", "Zn")


# Covariates
X0 <- mesa.final$age1c ##question
X1 <- as.factor(mesa.final$gender1)
X2 <- as.factor(mesa.final$race1c)
X3 <- as.factor(mesa.final$site1c)
X4 <- as.factor(mesa.final$educ)
X5 <- mesa.final$newegfr_ti
X6 <- as.factor(mesa.final$cig_ti)
X7 <- mesa.final$bmi_ti
X8 <- mesa.final$pamvcm_ti
X9 <- mesa.final$ldl_ti
X10 <- as.factor(mesa.final$lipid_ti)
X11 <- as.factor(mesa.final$dm03_ti)
X12 <- mesa.final$sbp_ti
X13 <- as.factor(mesa.final$htnmed_ti)
X14 <- mesa.final$hdl_ti
X15 <- mesa.final$pkyrs_ti

X<-cbind(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15)
dim(X)


#Outcome
y <- mesa.final$logswcs_rev


# Knots (to speed the computation up, optional)
des <- fields::cover.design(Z[!duplicated(Z),], nd = 100)
nodos <- des$design

# BKMR fit
#continuous
fitkm_corr <- kmbayes(y = y, Z = Z, X = X, iter = 20000, varsel = TRUE, 
                      verbose=TRUE,  ##between 30 and 45 acceptance rates if not change control parameters
                      knots=nodos,
                      control.params = list(r.jump2 = 0.1))

ExtractPIPs(fitkm_corr) %>% as.data.frame() %>% write.csv('D://KM//Columbia//MESA//Data//metals_bkmr_swcs_6metals_pips.csv')

save(fitkm_corr, file="metals_bkmr_swcs_6metals.RData")
load("metals_bkmr_swcs_6metals.RData")



# Overall plot
risks.overall.approx <- OverallRiskSummaries(fit = fitkm_corr_rj0.1, y = y, Z = Z, X = X,
                                             qs = seq(0.05, 0.95, by = 0.1), q.fixed = 0.25)
risks.overall.approx %>% data.frame()
write.csv(risks.overall.approx, 'metals_bkmr_swcs_6metals_riskoverall.csv')

tiff(file=paste0("metals_bkmr_swcs_6metals_riskoverall.tiff"), res=600, 
     width = 5, height = 4, units = 'in', compression = c("lzw"))

ggplot(risks.overall.approx, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()+
  geom_hline(yintercept=0, linetype="dotted", color= "blue", size=1) +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line    = element_line(color='black'),
        axis.text = element_text(color='black'),
        axis.text.x = element_text(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(
          size = 10, color = "black", face = "bold"
        ))+
  ylab("CAC-SW")+
  xlab("Quantile of the Joint Urinary Metal Distribution")
dev.off()


