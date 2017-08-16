## Compare new metric with 

## dinesafe data can be found at
## https://www1.toronto.ca/wps/portal/contentonly?vgnextoid=b54a5f9cd70bb210VgnVCM1000003dd60f89RCRD
## For more details, please check
## http://www.toronto.ca/health/dinesafe/index.htm


## This step takes some time to load. On my i5-4200/ 8gb RAM laptop, it took about 10 minutes to load.
## https://stackoverflow.com/questions/17198658/how-to-parse-xml-to-r-data-frame
library(XML)
dinesafe_xml <- xmlParse('./data/dinesafe.xml')
dinesafe_df <- xmlToDataFrame(dinesafe_xml, stringsAsFactors = F)

## write.csv(dinesafe_df, './data/dinesafe.csv')

# Note that establishments whose status is 'Pass' or 'Conditional Pass' do have inspections that have resulted in some level of severity. 
# Hence we look at what the outcome of each inspection is instead.
# Note that one inspeciton can result in more than one infraction. (See inspection id 103237909)

table(dinesafe_df$ESTABLISHMENT_STATUS, dinesafe_df$SEVERITY)

## Note that 'NA - Not Applicable' does not imply that there was no infraction. Check insepction details.
## So we will rename NA to 'others' and empty cells to "No infraction."
dinesafe_df$SEVERITY[dinesafe_df$SEVERITY == 'NA - Not Applicable' ] <- 'Others'

## Since the column is read as string, we tese if it is an empty string, not if it is null or char.
dinesafe_df$SEVERITY[dinesafe_df$SEVERITY == ''] <- 'NA - Not Applicable'
dinesafe_df$INFRACTION_DETAILS[dinesafe_df$INFRACTION_DETAILS == ''] <- 'No Infraction'
dinesafe_df$ACTION[dinesafe_df$ACTION == ''] <- 'Not Applicable'


## Dropped the columns that are not needed for the analysis
## For example, 'ROW ID", ""Court Outcome", "Amount Fined"
dinesafe_df <- dinesafe_df[-c(1,13,14)]

## Here, I geocoded (check geocoded.R)
dinesafe_df$Address <- paste(dinesafe_df$ESTABLISHMENT_ADDRESS, "Toronto, ON, Canada", sep = ', ')

geocoded_ent <- read.csv('./data/geocoded_ent.csv')
colnames(geocoded_ent) <- c('dummy','lat','long','Address','IsED')

geocoded_ent <- geocoded_ent[c(2:5)]
geocoded_ent$IsED[is.na(geocoded_ent$IsED)] <- 0

dinesafe_df <- merge(dinesafe_df, geocoded_ent, by = 'Address')


## Create new dimensions 
## 'ISEntertain' is an indicator variable
## TODO: Geographic feature
## HOME/AWAY -> ISEntertain or Neighbourhood



## Minor < Sig < Crucial

## Now pivot this data frame at the establishment level and at the inspection level.
library("dplyr")

no_inf_df <- dinesafe_df %>%
    filter(SEVERITY == 'NA - Not Applicable') %>%
    group_by(INSPECTION_ID) %>%
    summarise(No_Infraction = n())     

# Not going to include 'other' cases as it has to do with Municipal by-law infractions, not the health hazard.

# other_inf_df <- dinesafe_df %>%
#     filter(SEVERITY == 'Others') %>%
#     group_by(INSPECTION_ID) %>%
#     summarise(Others = n())     

minor_inf_df <- dinesafe_df %>%
    filter(SEVERITY == 'M - Minor') %>%
    group_by(INSPECTION_ID) %>%
    summarise(Minor = n())     

sig_inf_df <- dinesafe_df %>%
    filter(SEVERITY == 'S - Significant') %>%
    group_by(INSPECTION_ID) %>%
    summarise(Significant = n())     

crucial_inf_df <- dinesafe_df %>%
    filter(SEVERITY == 'C - Crucial') %>%
    group_by(INSPECTION_ID) %>%
    summarise(Crucial = n())     

total_inf_df <- dinesafe_df %>%
  #filter(SEVERITY != 'NA - Not Applicable') %>%
  filter(SEVERITY != 'NA - Not Applicable' & SEVERITY != 'Others') %>%
  group_by(INSPECTION_ID) %>%
  summarise(Total = n())  

#inspection_df <- Reduce(function(x, y) merge(x, y, all=TRUE), list(no_inf_df, other_inf_df, minor_inf_df, sig_inf_df, crucial_inf_df, total_inf_df))
inspection_df <- Reduce(function(x, y) merge(x, y, all=TRUE), list(no_inf_df, minor_inf_df, sig_inf_df, crucial_inf_df, total_inf_df))
inspection_df[is.na(inspection_df)] <- 0

length(unique(dinesafe_df$INSPECTION_ID)) == nrow(inspection_df) ## No longer returns TRUE!

## Total number of inspections. i.e. total number of records
inspection_count <- nrow(inspection_df)

## Add establishment_id to the data frame
is_ent_df <- dinesafe_df %>%
  filter(SEVERITY != 'Others') %>%
  select(ESTABLISHMENT_ID, INSPECTION_ID, IsED, MINIMUM_INSPECTIONS_PERYEAR) %>%
  distinct(ESTABLISHMENT_ID, INSPECTION_ID, IsED, MINIMUM_INSPECTIONS_PERYEAR)

inspection_df <- merge(is_ent_df, inspection_df, by = "INSPECTION_ID", all = TRUE)
#inspection_df <- inspection_df[c(2,1,3:8)]

## Aggregate at the establishment level
## For IsED, used average. since average of 0's is 0 and that of 1's is 1.
est_df <- inspection_df %>%
  group_by(ESTABLISHMENT_ID) %>%
  summarize(IsED = mean(IsED), Inspection = n(), Minor = sum(Minor), Significant = sum(Significant), Crucial = sum(Crucial), Total = sum(Total))


head(est_df)

write.csv(inspection_df, './data/compliance.csv')
write.csv(est_df, './data/infraction.csv')

