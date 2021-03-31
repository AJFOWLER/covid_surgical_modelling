# for DM; lower GI #

source('R/library.R') #load libraries

source('R/model_funs.R') # get in all functions needed for modelling

##############################
# LOAD data from NHS England #
##############################
options(scipen = 999)

# data 2014/15 to 2019/20
years = c(2014:2019)

file_links = get_HES_filelink(years) #eyeball to ensure '-proc-' 

# these functions sort out the data.
' This requires some input to check header rows correctly selected and total rows removed' 
' All headers happen to be row 1 and totals row 2'
cleaned_files = lapply(file_links, function(x)download_hes_records_clean(x))

' Data for core analysis downloaded on 18May2020'
' Data for analysis post-peer review on 13July2020' #(no change to data accessed)
names(cleaned_files) = years

##################
# clean HES data # 
##################

# to clean we need to first drop any ',' in numeric cols
lapply(cleaned_files, function(x) length(which(grepl(",",as.vector(t(x))))))
# there are none, so we can make cols numeric safely
# commas used in older versions of HES APC data to delineate thousands

# small numbers == [*]
lapply(cleaned_files, function(x) length(which(grepl("[*]",as.vector(t(x))))))

# drop star replaces * with 7 (from model_funs)
cleaned_drop = lapply(cleaned_files, drop_star)

# check all changed
lapply(cleaned_drop, function(x) length(which(grepl("[*]",as.vector(t(x))))))

# '-' becomes 0
# In 2016, there are no 0. All are '-' so make - into 0.
lapply(cleaned_drop, function(x) length(which(grepl("-",as.vector(t(x))))))

# drop_hyp replaces '-' with 0
cleaned_up = lapply(cleaned_drop, drop_hyp)

# check all sorted
lapply(cleaned_up, function(x) length(which(grepl("-",as.vector(t(x))))))

# confirm no rows dropped #
lapply(cleaned_files, nrow) 
lapply(cleaned_drop, nrow)
lapply(cleaned_up, nrow)

# ready to bind 
all_dat = rbindlist(cleaned_up, idcol = T)

# names
names(all_dat)[1] = 'Year'
names(all_dat)[2] = 'OPCS'
names(all_dat)[3] = 'Description'
names(all_dat)[4] = 'FCE'
names(all_dat)[44:46] = c('EmergencyDC', 'ElectiveDC', 'OtherDC') #day cases

# drop non-alphanumeric & whitespace from column 1 
# this is for sex specific proceures where reported sex may not match procedure
all_dat$OPCS = gsub('[^[:alnum:]]', '', all_dat$OPCS)

# ensure all dropped successfully
table(all_dat$OPCS)

# sort out numeric columns
num_cols = 4:46
all_dat[,(num_cols) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = num_cols]

# enforced NAs are empty cells #
table(is.na(all_dat)) # leave for now (356)

# all_dat is ready #
rm(cleaned_drop, cleaned_files, cleaned_up, file_links, num_cols, years)


# bind in procedure grouping
opcs <- fread('opcs_grouping.csv')

all_dat[opcs, on=.(OPCS = OPCS), grouping := i.Grouping]

all_dat[,elective := `Waiting list` + Planned]

all_dat[,other := `Other Admission Method`]

all_dat[,lapply(.SD, function(x) sum(x)), by=Year, .SDcols = c("Emergency", "elective", "other", "FCE")]
# total admissions

#apply definition
surg <- all_dat[OPCS %in% opcs$OPCS & OPCS != 'L91']

surg[,lapply(.SD, function(x) sum(x)), by=Year, .SDcols = c("Emergency", "elective", "other", "FCE")]

# by specialty #

spec <- surg[, sum(Admissions), by=.(Year, grouping)]
all_y <- surg[,lapply(.SD, function(x) sum(x)), by=Year, .SDcols = c("Emergency", "elective", "other", "FCE", "Admissions")]
spec[all_y, on=.(Year=Year), tot_adm := i.Admissions]

spec[,propr := V1/tot_adm*100]

procs <- surg[,sum(Admissions), by=OPCS]
procs[order(-V1)]

procs[surg, on=.(OPCS = OPCS), `:=`(description=i.Description, grouping = i.grouping)]
procs[, tot_adm := surg[,sum(Admissions)] ]

procs[,propr_total := V1/tot_adm*100]
write.csv(procs, 'procedure_codes_14_20.csv')

by_grouping <- spec[,sum(V1), by=grouping]
by_grouping[,tot_adm := surg[,sum(Admissions)]]
by_grouping[,propr := V1/tot_adm*100]

write.csv(by_grouping, 'by_grouping_14_20.csv')
