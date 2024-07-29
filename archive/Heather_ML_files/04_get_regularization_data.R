library(tidyverse)
# Code taken from Statistics for Health Data Science 
# by Ruth Etzioni, Micha Mandel, and Roman Gulati
# https://roman-gulati.github.io/statistics-for-health-data-science/


##################################################
# Load or download, extract, read, and save MEPS
##################################################
grab_meps <- function(year, inscope_only=TRUE){
  code <- paste0('h', seq(12, 201, by=9))
  names(code) <- seq(1996, 2017)
  stopifnot(year %in% names(code))
  year_code <- code[[as.character(year)]]
  archive_filename <- paste0(year_code, 'ssp.zip')
  remote_sas_archive <- paste0('https://meps.ahrq.gov/mepsweb/data_files/pufs/',
                               archive_filename)
  local_sas_archive <- paste0(year_code, 'ssp.zip')
  local_rdata <- paste(year_code, 'Rdata', sep='.')
  if(file.exists(local_rdata)){
    load(file=local_rdata)
  } else {
    download.file(remote_sas_archive, local_sas_archive, mode='wb')
    dset <- foreign::read.xport(unzip(local_sas_archive))
    local_sas_file <- sub('ssp.zip$', '.ssp', local_sas_archive)
    file.copy(local_sas_file, local_rdata, overwrite=TRUE)
    unlink(local_sas_file)
    save(dset, file=local_rdata)
  }
  if(inscope_only){
    suffix <- substr(year, 3, 4)
    inscope <- paste0('INSCOP', suffix)
    dset <- dset[dset[[inscope]] == 1, ]
  }
  return(dset)
}

##################################################
# Load or download/extract/save longitudinal MEPS
##################################################
grab_longitudinal_meps <- function(panel){
  code <- c(23, 35, 48, 58, 65, 71, 80, 86, 98, 106, 114, 122, 130, 139, 148, 156, 164, 172, 183, 193, 202)
  code <- paste0('h', code)
  stopifnot(between(panel, 1, 21))
  year_code <- code[panel]
  archive_filename <- paste0(year_code, 'ssp.zip')
  remote_sas_archive <- paste0('https://meps.ahrq.gov/mepsweb/data_files/pufs/',
                               archive_filename)
  local_sas_archive <- paste0(year_code, 'ssp.zip')
  local_rdata <- paste(year_code, 'Rdata', sep='.')
  if(file.exists(local_rdata)){
    load(file=local_rdata)
  } else {
    download.file(remote_sas_archive, local_sas_archive, mode='wb')
    dset <- foreign::read.xport(unzip(local_sas_archive))
    local_sas_file <- sub('ssp.zip$', '.ssp', local_sas_archive)
    file.copy(local_sas_file, local_rdata, overwrite=TRUE)
    unlink(local_sas_file)
    save(dset, file=local_rdata)
  }
  dset <- dset %>% filter(INSCOPY1 == 1, INSCOPY2 == 1)
  return(dset)
}

##################################################
# Curate MEPS data from a longitudinal panel with
# predictor variables from a specified survey year
# or round
##################################################
grab_and_curate_meps <- function(panel=21, survey_round=3, survey_year=1){
  stopifnot(between(survey_round, 1, 5))
  dset <- grab_longitudinal_meps(panel)
  variables <- c(Sex='SEX',
                 Race='RACEV1X',
                 Education='EDUCYR',
                 Age=paste0('AGEY', survey_year, 'X'),
                 Married=paste0('MARRYY', survey_year, 'X'),
                 Region=paste0('REGIONY', survey_year),
                 Poverty=paste0('POVCATY', survey_year),
                 Insurance=paste0('INSCOVY', survey_year),
                 BP.diagnosis=paste0('HIBPDXY', survey_year),
                 MI.diagnosis=paste0('MIDXY', survey_year),
                 CHD.diagnosis=paste0('CHDDXY', survey_year),
                 OHD.diagnosis=paste0('OHRTDXY', survey_year),
                 Angina.diagnosis=paste0('CHDDXY', survey_year),
                 Asthma.diagnosis=paste0('ASTHDXY', survey_year),
                 Arthritis.diagnosis=paste0('ARTHDXY', survey_year),
                 Cancer.diagnosis=paste0('CANCERY', survey_year),
                 Cholesterol.diagnosis=paste0('CHOLDXY', survey_year),
                 Diabetes.diagnosis=paste0('DIABDXY', survey_year),
                 Emphysema.diagnosis=paste0('EMPHDXY', survey_year),
                 Stroke.diagnosis=paste0('STRKDXY', survey_year),
                 Perceived.health=paste0('RTHLTH', survey_round),
                 Mental.health=paste0('MNHLTH', survey_round),
                 Inpatient.year1='IPTEXPY1',
                 Inpatient.year2='IPTEXPY2',
                 Outpatient.year1='OPTEXPY1',
                 Outpatient.year2='OPTEXPY2',
                 Total.year1='TOTEXPY1',
                 Total.year2='TOTEXPY2')
  # some variables are only available for certain rounds
  if(survey_round %in% c(3, 5)){
    variables <- c(variables, Physical.exercise=paste0('PHYEXE', survey_round))
    if(survey_round == 3)
      variables <- c(variables, BMI=paste0('BMINDX', survey_round))
  } else if (survey_round %in% c(2, 4)){
    variables <- c(variables, Current.smoker=paste0('ADSMOK', survey_round))
  }
  dset <- dset %>% select(all_of(variables))
  dset <- dset %>% mutate(Sex=factor(Sex,
                                     levels=c(1, 2),
                                     labels=c('Male', 'Female')),
                          Race=factor(Race,
                                      levels=c(1, 2, 3, 4, 6),
                                      labels=c('White',
                                               'Black',
                                               'Amer Indian/Alasaka Native',
                                               'Asian/Native Hawaiian/Pacific Islander',
                                               'Mixed')),
                          Education=ifelse(between(Education, 0, 17), Education, NA),
                          Married=factor(Married,
                                         levels=c(1, 2, 3, 4, 5, 6),
                                         labels=c('Married',
                                                  'Widowed',
                                                  'Divorced',
                                                  'Separated',
                                                  'Never married',
                                                  'Inapplicable')),
                          Region=factor(Region,
                                        levels=c(1, 2, 3, 4),
                                        labels=c('Northeast',
                                                 'South',
                                                 'Midwest',
                                                 'West')),
                          Poverty=factor(Poverty,
                                         levels=c(1, 2, 3, 4, 5),
                                         labels=c('Poor',
                                                  'Near poor',
                                                  'Low income',
                                                  'Middle income',
                                                  'High income')),
                          Insurance=factor(Insurance,
                                           levels=c(1, 2, 3),
                                           labels=c('Any private',
                                                    'Public only',
                                                    'Uninsured')),
                          BP.diagnosis=factor(BP.diagnosis,
                                              levels=c(2, 1),
                                              labels=c('No', 'Yes')),
                          MI.diagnosis=factor(MI.diagnosis,
                                              levels=c(2, 1),
                                              labels=c('No', 'Yes')),
                          CHD.diagnosis=factor(CHD.diagnosis,
                                               levels=c(2, 1),
                                               labels=c('No', 'Yes')),
                          OHD.diagnosis=factor(OHD.diagnosis,
                                               levels=c(2, 1),
                                               labels=c('No', 'Yes')),
                          Angina.diagnosis=factor(Angina.diagnosis,
                                                  levels=c(2, 1),
                                                  labels=c('No', 'Yes')),
                          Asthma.diagnosis=factor(Asthma.diagnosis,
                                                  levels=c(2, 1),
                                                  labels=c('No', 'Yes')),
                          Arthritis.diagnosis=factor(Arthritis.diagnosis,
                                                     levels=c(2, 1),
                                                     labels=c('No', 'Yes')),
                          Cancer.diagnosis=factor(Cancer.diagnosis,
                                                  levels=c(2, 1),
                                                  labels=c('No', 'Yes')),
                          Cholesterol.diagnosis=factor(Cholesterol.diagnosis,
                                                       levels=c(2, 1),
                                                       labels=c('No', 'Yes')),
                          Diabetes.diagnosis=factor(Diabetes.diagnosis,
                                                    levels=c(2, 1),
                                                    labels=c('No', 'Yes')),
                          Emphysema.diagnosis=factor(Emphysema.diagnosis,
                                                     levels=c(2, 1),
                                                     labels=c('No', 'Yes')),
                          Stroke.diagnosis=factor(Stroke.diagnosis,
                                                  levels=c(2, 1),
                                                  labels=c('No', 'Yes')),
                          Perceived.health=factor(Perceived.health,
                                                  levels=seq(5),
                                                  labels=c('Excellent',
                                                           'Very good',
                                                           'Good',
                                                           'Fair',
                                                           'Poor')),
                          Mental.health=factor(Mental.health,
                                               levels=seq(5),
                                               labels=c('Excellent',
                                                        'Very good',
                                                        'Good',
                                                        'Fair',
                                                        'Poor')),
                          Anyhosp2016=Inpatient.year1 > 0,
                          Anyhosp2017=Inpatient.year2 > 0,
                          Edu9plus=Education >= 9,
                          Edu12plus=Education >= 12,
                          Edu13plus=Education >= 13,
                          Edu16plus=Education >= 16)
  if('Physical.exercise' %in% names(dset))
    dset <- dset %>% mutate(Physical.exercise=factor(Physical.exercise,
                                                     levels=c(2, 1),
                                                     labels=c('No', 'Yes')))
  if('BMI' %in% names(dset))
    dset <- dset %>% mutate(BMI=ifelse(between(BMI, 12.5, 83.2), BMI, NA))
  if('Current.smoker' %in% names(dset))
    dset <- dset %>% mutate(Current.smoker=factor(Current.smoker,
                                                  levels=c(2, 1),
                                                  labels=c('No', 'Yes')))
  cat('Remove',
      dset %>% filter(Age <= 17) %>% nrow(),
      'age 17 or younger\n')
  dset <- dset %>% filter(Age > 17)
  return(dset)
}

##################################################
# Read longitudinal MEPS data
##################################################
mset <- grab_and_curate_meps(panel=21, survey_round=2)

##################################################
# Restrict to candidate predictor variables
##################################################
predictors <- c('Age', 'Sex', 'Race', 'Married', 'Poverty',
                'Edu9plus', 'Edu12plus', 'Edu13plus', 'Edu16plus',
                'Insurance',
                'Angina.diagnosis',
                'Asthma.diagnosis',
                'Arthritis.diagnosis',
                'Cancer.diagnosis',
                'CHD.diagnosis',
                'Diabetes.diagnosis',
                'Emphysema.diagnosis',
                'MI.diagnosis',
                'BP.diagnosis',
                'Cholesterol.diagnosis',
                'OHD.diagnosis',
                'Stroke.diagnosis',
                'Perceived.health',
                'Mental.health',
                'Anyhosp2016')
meps <- mset %>% drop_na(any_of(predictors)) %>% 
  select(c(all_of(predictors), "Anyhosp2017"))

# Subsample 1000 observations
set.seed(1)
meps <- meps[sample(1:nrow(meps), 1000),]

# Remove downloaded files
unlink(c("h202.rData", "h202ssp.zip"))

# Save final dataset
save(meps, file = "meps.rData")
