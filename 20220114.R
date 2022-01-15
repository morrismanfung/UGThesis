# The script is written to extract data from Qualtrics raw output.
# Function is not used intentionally to maintain clarity.
# Written by CHAN, Man Fung Morris on 01-14-2022

# Notes on Jan-15-2022: Priority is not extracted yet.

# Basic setups ----
library( dplyr)
# Import the packages

setwd( 'C:\\Users\\User\\Desktop\\yr4 sem2\\PSYG4008 - Thesis in Psychology\\Data\\Preprocessing')

rm( list = ls())

header = read.csv( 'C:\\Users\\User\\Desktop\\yr4 sem2\\PSYG4008 - Thesis in Psychology\\Data\\Preprocessing\\Pretest_Thesis_January+14,+2022_01.08.csv', nrows = 1, header = F)
# Get a vector of strings as header for the data frame

df = read.csv( 'C:\\Users\\User\\Desktop\\yr4 sem2\\PSYG4008 - Thesis in Psychology\\Data\\Preprocessing\\Pretest_Thesis_January+14,+2022_01.08.csv', skip = 3, header = F)
# Get only the content of the raw output

colnames( df) = header
# Change the data frame headers to the desired strings

# Preprocessing
df_cln = subset( df, select = -c( Status, IPAddress, Progress, ResponseId,
                                  RecipientLastName, RecipientFirstName, RecipientEmail,
                                  ExternalReference, LocationLatitude, LocationLongitude,
                                  DistributionChannel, UserLanguage
                                  ))
# To remove all the irrelevant columns

df_cln_consent = subset( df_cln, Consent == 1)

# Experimental Group ----
## Demographics ----
df_E = subset( df_cln_consent, Group == 'E')
df_E_demo = select( df_E, contains( c( 'Gender', 'Year_of_birth', 'Education',
                                       'Perceived_Importance_1',
                                       'Perceived_Importance_2', 
                                       'Group')))
colnames( df_E_demo)[4] = 'Imp_Env'
colnames( df_E_demo)[5] = 'Imp_Fin'

## Item level ----
df_E_item = select( df_E, contains( c('E_priority', 'E_SSU', 'E_InvInt')))

# Reverse coding for 2 items (4 & 5) in SSU
cols = colnames( df_E_item)[grep( 'SSU_4|SSU_5', colnames( df_E_item))]
df_E_item[ , cols] = 8 - df_E_item[ , cols]
df_E_item = cbind( df_E_demo, df_E_item)

## Trial level -----
### SSU -----
df_SSU_score_tmp = select( df_E_item, contains( '1E_SSU'))
df_E_item$'1E_SSU_mean' = rowSums( df_SSU_score_tmp) / 7

df_SSU_score_tmp = select( df_E_item, contains( '2E_SSU'))
df_E_item$'2E_SSU_mean' = rowSums( df_SSU_score_tmp) / 7

df_SSU_score_tmp = select( df_E_item, contains( '3E_SSU'))
df_E_item$'3E_SSU_mean' = rowSums( df_SSU_score_tmp) / 7

df_SSU_score_tmp = select( df_E_item, contains( '4E_SSU'))
df_E_item$'4E_SSU_mean' = rowSums( df_SSU_score_tmp) / 7

df_SSU_score_tmp = select( df_E_item, contains( '5E_SSU'))
df_E_item$'5E_SSU_mean' = rowSums( df_SSU_score_tmp) / 7

### InvInt -----
df_InvInt_score_tmp = select( df_E_item, contains( '1E_InvInt'))
df_E_item$'1E_InvInt_mean' = rowSums( df_InvInt_score_tmp) / 7

df_InvInt_score_tmp = select( df_E_item, contains( '2E_InvInt'))
df_E_item$'2E_InvInt_mean' = rowSums( df_InvInt_score_tmp) / 7

df_InvInt_score_tmp = select( df_E_item, contains( '3E_InvInt'))
df_E_item$'3E_InvInt_mean' = rowSums( df_InvInt_score_tmp) / 7

df_InvInt_score_tmp = select( df_E_item, contains( '4E_InvInt'))
df_E_item$'4E_InvInt_mean' = rowSums( df_InvInt_score_tmp) / 7

df_InvInt_score_tmp = select( df_E_item, contains( '5E_InvInt'))
df_E_item$'5E_InvInt_mean' = rowSums( df_InvInt_score_tmp) / 7
# To create an extra column storing the mean score (Condition Level)

df_E_trial = select( df_E_item, contains( '_mean'))
df_E_trial = cbind( df_E_demo, df_E_trial)

## Condition Level ----
df_E_condition_tmp = select( df_E_trial, contains( 'SSU'))
df_E_condition_SSU = rowMeans( df_E_condition_tmp)

df_E_condition_tmp = select( df_E_trial, contains( 'InvInt'))
df_E_condition_InvInt = rowMeans( df_E_condition_tmp)

df_E_condition = data.frame( df_E_condition_SSU, df_E_condition_InvInt)
df_E_condition = cbind( df_E_demo, df_E_condition)

# Control Group ----
## Demographics ----
df_C = subset( df_cln_consent, Group == 'C')
df_C_demo = select( df_C, contains( c( 'Gender', 'Year_of_birth', 'Education',
                                       'Perceived_Importance_1',
                                       'Perceived_Importance_2', 
                                       'Group')))
colnames( df_C_demo)[4] = 'Imp_Env'
colnames( df_C_demo)[5] = 'Imp_Fin'

## Item level ----
df_C_item = select( df_C, contains( c('C_priority', 'C_SSU', 'C_InvInt')))

# Reverse coding for 2 items (4 & 5) in SSU
cols = colnames( df_C_item)[grep( 'SSU_4|SSU_5', colnames( df_C_item))]
df_C_item[ , cols] = 8 - df_C_item[ , cols]
df_C_item = cbind( df_C_demo, df_C_item)

## Trial level -----
### SSU -----
df_SSU_score_tmp = select( df_C_item, contains( '1C_SSU'))
df_C_item$'1C_SSU_mean' = rowSums( df_SSU_score_tmp) / 7

df_SSU_score_tmp = select( df_C_item, contains( '2C_SSU'))
df_C_item$'2C_SSU_mean' = rowSums( df_SSU_score_tmp) / 7

df_SSU_score_tmp = select( df_C_item, contains( '3C_SSU'))
df_C_item$'3C_SSU_mean' = rowSums( df_SSU_score_tmp) / 7

df_SSU_score_tmp = select( df_C_item, contains( '4C_SSU'))
df_C_item$'4C_SSU_mean' = rowSums( df_SSU_score_tmp) / 7

df_SSU_score_tmp = select( df_C_item, contains( '5C_SSU'))
df_C_item$'5C_SSU_mean' = rowSums( df_SSU_score_tmp) / 7

### InvInt -----
df_InvInt_score_tmp = select( df_C_item, contains( '1C_InvInt'))
df_C_item$'1C_InvInt_mean' = rowSums( df_InvInt_score_tmp) / 7

df_InvInt_score_tmp = select( df_C_item, contains( '2C_InvInt'))
df_C_item$'2C_InvInt_mean' = rowSums( df_InvInt_score_tmp) / 7

df_InvInt_score_tmp = select( df_C_item, contains( '3C_InvInt'))
df_C_item$'3C_InvInt_mean' = rowSums( df_InvInt_score_tmp) / 7

df_InvInt_score_tmp = select( df_C_item, contains( '4C_InvInt'))
df_C_item$'4C_InvInt_mean' = rowSums( df_InvInt_score_tmp) / 7

df_InvInt_score_tmp = select( df_C_item, contains( '5C_InvInt'))
df_C_item$'5C_InvInt_mean' = rowSums( df_InvInt_score_tmp) / 7
# To create an extra column storing the mean score (Condition Level)

df_C_trial = select( df_C_item, contains( '_mean'))
df_C_trial = cbind( df_C_demo, df_C_trial)

## Condition Level ----
df_C_condition_tmp = select( df_C_trial, contains( 'SSU'))
df_C_condition_SSU = rowMeans( df_C_condition_tmp)

df_C_condition_tmp = select( df_C_trial, contains( 'InvInt'))
df_C_condition_InvInt = rowMeans( df_C_condition_tmp)

df_C_condition = data.frame( df_C_condition_SSU, df_C_condition_InvInt)
df_C_condition = cbind( df_C_demo, df_C_condition)

# Files creation ----
maindir <- getwd()
dir.create( file.path( maindir, 'Output'), showWarnings = FALSE)
outputdir <- file.path( maindir, 'Output')
setwd( outputdir)

write.csv( df_E_item, 'Item_E.csv', row.names = FALSE)
write.csv( df_E_trial, 'Trial_E.csv', row.names = FALSE)
write.csv( df_E_condition, 'Condition_E.csv', row.names = FALSE)
write.csv( df_C_item, 'Item_C.csv', row.names = FALSE)
write.csv( df_C_trial, 'Trial_C.csv', row.names = FALSE)
write.csv( df_C_condition, 'Condition_C.csv', row.names = FALSE)
