source(paste0(getwd(),'/R/library.R')) #load libraries

source(paste0(getwd(), '/R/model_funs.R')) # get in all functions needed for modelling

##############################
# LOAD data from NHS England #
##############################
options(scipen = 999)

# data 2014/15 to 2018/19
years = c(2014:2018)

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

#####################
# Bed activity data #
#####################
source(paste0(getwd(), '/R/MAR_data_load_clean.R'))

# detailed working in file #
# returns elec_by_mo which is monthly distribution of elective care
# returns emerg_by_mo which is monthly distribution of emergency care

#######################
# Allocation to class #
#######################
'
wait time is in days.
each procedure therefore gets:
count of class 2, 3, 4.
class 1 = emergency
'

# count of emergency admissions is class 1
#'other' into non-elective as this includes maternity care etc, which aligns with MAR definitions
all_dat[,class1 := Emergency+`Other Admission Method`] 

# planned + waiting == elective
# note planned and waiting list are both elective, just depends on clinical/clerical nature of wait time
all_dat[,elective := `Waiting list`+Planned] 

# validate that class1+elective== all admissions
which(!(all_dat$class1 + all_dat$elective) == all_dat$Admissions)

# sort day case vs. inpatient
# need to express inaptient as proportion of FCEs that are NOT daycase.
all_dat[, new_daycase := `Day case`+ ElectiveDC]
all_dat[, inpatient := c_ceiling(((FCE - new_daycase)/FCE)*elective)] #c_ceiling as whole numbers

all_dat[,`Mean time waited` := as.integer(`Mean time waited`)]

# make empty cols
all_dat[,c('class2', 'class3', 'class4') := 0]

# those with mean wait <4 weeks ==> class 2
# page 117 of data dictionary: wait time is difference in days
all_dat[`Mean time waited` < 28, class2 := as.integer(elective)]

# those with mean wait <12 weeks ==> class 3
all_dat[`Mean time waited` >=28 & `Mean time waited` < 85, class3 := as.integer(elective)]

# all else ==> class 4
all_dat[`Mean time waited` >= 85, class4 := as.integer(elective)]

## assignment done ##

#####################
# define 'surgery'  #
#####################
opcs_codes = setDT(read.csv('opcs_grouping.csv', stringsAsFactors = F))

#note 1047 codes from 2017 paper + 27 identified as new since our prior work #

# supplementary table 1 #
names(opcs_codes)[1] = 'OPCS'

suppl_1 = opcs_codes[, toString(OPCS), by=Grouping]

# select surgery #
all_dat_surg = all_dat[OPCS %in% opcs_codes$OPCS]

# assign grouping #
all_dat_surg[opcs_codes, on = 'OPCS', 'proc_group' := i.Grouping]

# Total episodes:
sum(all_dat_surg$FCE, na.rm=T) #23,710,168 episodes over the study period.

# Total admissions:
sum(all_dat_surg$Admissions, na.rm=T) #22,513,872 admissions for procedures over the study period

cols = c('class1', 'class2', 'class3', 'class4')

# sum by class
all_dat_surg[,lapply(.SD, sum), .SDcols=cols]

# discrepancy here between classified/unclassified/
# 56 procedures over the course of the study had elective procedures, but no mean time recorded so could not be classifed.
# The total number of elective procedures associated with these 56 codes were: 73, representing <0.0003% of procedures, so were removed from elective calculations
all_dat_surg[which(rowSums(all_dat_surg[,c('class2', 'class3', 'class4')]) == 0),sum(elective)]

# clean as you go
rm(all_dat, opcs_codes, suppl_1)

###############################
# Model procedures per annum  #
###############################
'this is based on linear model for each procedure for each class 
extrapolated to 2019/20 and 2020/2021'

# sum of procedures by class for each procedure group and each year
proc_grouped = all_dat_surg[,lapply(.SD, sum), .SDcols = cols, by=.(proc_group, Year)]

# output frame including data for 2019/2020 and 2020/2021
master_frame = data.frame(proc_group = character(), fit = numeric(), lwr=numeric(), upr = numeric(),Year = numeric(), class_= character()) 

for(x in cols){
  # run prediction (see models for explanation)
  prediction = proc_grouped[, as.list(clean_return(get(x), Year)), by=proc_group]
  #assign to class
  prediction$class_ = x
  # this goes into a master_frame
  master_frame= rbind(master_frame, prediction)
  gc()
}

setDT(master_frame)

# we can check these look sensible
master_frame[proc_group == 'Neuro']
proc_grouped[proc_group == 'Neuro']

# not possible to have a negative fit (can't do negative number of procedures) #
# While some of these numbers are quite big (eg -15K orthopaedic procedures), they are mostly 
# procedures shifting over from class 3 to class 4  as waiting list times increase over study period
master_frame[fit <0, fit := 0]
master_frame[lwr <0, lwr := 0]

# check NAs
table(is.na(master_frame$fit | master_frame$lwr)) # no NAs

#######################
# historic data by mo #
#######################

# calculate total number of procedures by year
procs_per_year = all_dat_surg[,lapply(.SD, sum), .SDcols=cols, by=Year]

# melt long
procs_per_y_melt = melt(procs_per_year, id.vars='Year', measure.vars = c('class1', 'class2', 'class3', 'class4'))

# multiply class1 (emergency/urgent) by emerg grow; mapply to make wide df
emerg = data.frame(mapply('*', procs_per_y_melt[variable =='class1', 'value'], emerg_by_mo))

# same for elective
non_emerg = data.frame(mapply('*', procs_per_y_melt[variable != 'class1', 'value'], elec_by_mo))

# sort out names
names(non_emerg) = names(emerg) = period

#add Year and measure
non_emerg[,c('Year', 'measure')] = procs_per_y_melt[variable !='class1', c('Year', 'variable')]
emerg[,c('Year', 'measure')] = procs_per_y_melt[variable == 'class1', c('Year', 'variable')]

# bind together
table1 = setDT(rbind(non_emerg, emerg))

#ceiling as total procedures, select median by month
table1.1 = table1[,lapply(.SD, function(x) ceiling(median(x))), .SDcols=period, by=measure]
table1.2 = table1[,lapply(.SD, function(x) ceiling(IQR(x))), .SDcols=period, by=measure]

table1.3 = mapply(paster_iqr, table1.1, table1.2)

# Median procedures by month and class clean #
table1.3

# Overall median
table1.1[,median(unlist(lapply(.SD, sum))),.SDcols=(2:13)] #382,768
# Overall IQR
table1.1[,IQR(unlist(lapply(.SD, sum))), .SDcols=(2:13)] #22,890

# The wide IQR in class3/class4 procedures reflects the shifting of procedures broadly from class 3-->4 over study period.

rm(procs_per_year, procs_per_y_melt, emerg, non_emerg, table1, table1.1, table1.2, table1.3)

###################################
# Modelled data split by month    #
###################################
cols_over = c('fit', 'lwr', 'upr')

predicted_numbers = master_frame[, as.list(unlist(lapply(.SD, function(x) ceiling(sum_d(x))))), by=.(class_, Year), .SDcols=cols_over]

melt_pred = melt(predicted_numbers, id.vars=c('class_', 'Year'),measure.vars = cols_over) # melt to long

# select class != 1, multiply each value by elec_gro
non_emerg = data.frame(mapply(function(x,y) ceiling(x*y), melt_pred[class_ != 'class1', 'value'], elec_by_mo))

# select class == 1, multiply each value by emerg_gro
emerg = data.frame(mapply(function(x,y) ceiling(x*y), melt_pred[class_ =='class1', 'value'], emerg_by_mo))

#harmonise
names(non_emerg) = names(emerg) = period

#class/Year
non_emerg[,c('class_', 'measure', 'Year')] = melt_pred[class_!='class1', c('class_', 'variable', 'Year')]
emerg[,c('class_', 'measure', 'Year')] = melt_pred[class_ == 'class1', c('class_', 'variable', 'Year')]

# make into a single data frame for analysis
# this has fit, lwr & upr 95% CI by year and class
all_comb = setDT(rbind(non_emerg, emerg))

rm(emerg, non_emerg, melt_pred)

##################################################
# output aggregated procedures by month and year #
##################################################
head(all_comb)

melted_all_comb = melt(all_comb, id.vars = c('class_', 'Year', 'measure'), measure.vars = period)
melted_all_comb[, pasted := paste0(variable,'_', Year)]

# cast wide again
all_comb_c = dcast(melted_all_comb, measure+class_~pasted, value.var = "value")

# reorder columns to make them April 2019 - March 2019 and April 2020 to March 2020
new_order = c('measure', 'class_',  c(paste0(period, '_2019'), paste0(period, '_2020')))

setcolorder(all_comb_c, new_order)

all_comb.1 = melt(all_comb_c, id = c('class_', 'measure'))
all_comb.2 = dcast(all_comb.1, class_+variable~measure)
all_comb.2[, format_col := paste0(c_ceiling(fit), ' (', c_ceiling(lwr), ' to ', c_ceiling(upr), ')')]
all_comb.3 = dcast(all_comb.2, class_~variable, value.var = 'format_col')

# for tables may wish to limit to January 2019: march 2020 #
#all_comb.3 is the clean version with 95% CI formatted nicely #
rm(all_comb.1, all_comb.2)

# total expected procedures 1st March 2020 to 28th February 2021 #
melted_all_comb[(Year == '2020' | pasted == 'march_2019') & pasted != 'march_2020',sum(value), by=measure]

#################
# deficit table #
#################

# this runs on a monthly basis, and all numbers are by the end of the month
# only need data from January 2019 onwards #
selected_columns = names(all_comb_c)[c(1,2,12:ncol(all_comb_c))]

all_comb_roll = all_comb_c[,..selected_columns]

selected_ =  selected_columns[3:17] # select Jan2019-March2021

# deficit is the accumulation of non-done procedures
# january + february, all procedures happen so not done == 0
# March class2 = 0.8, class 3&4 = 0.5, class 1 = 1
# April onwards assuming nil activity in class 2-4
all_comb_roll[,january_2019 := 0]
all_comb_roll[,february_2019 := 0]
all_comb_roll[class_ == 'class1', (selected_) := 0] # all class 1 done.
all_comb_roll[class_ == 'class2', march_2019 :=c_ceiling(march_2019*0.2)] # 80% done so 20% remaining
all_comb_roll[class_ %in% c('class3', 'class4'), march_2019 := c_ceiling(march_2019*0.5)] # 50% done in classes 3&4 in march

# deficit is therefore what should have been done but isn't, cumulative sum along this.
deficit = all_comb_roll[,apply(.SD, 1, function(x) c_ceiling(cumsum(x))), .SDcols = selected_]

# CLEAN_UP
deficit_c = setDT(as.data.frame(t(deficit)))
deficit_c[, c('class_', 'measure')] = all_comb_roll[,c('class_', 'measure')] 

tots = deficit_c[class_ %in% cols[2:4], lapply(.SD, sum_d), .SDcols = selected_, by=measure]
tots[,class_ := 'total']

# deficit assuming no surgical activity after March in classes 2,3,4, class 1 continues per normal
# supplementary table 6 in final manuscript, note the table in the manuscript only runs until february 2020.
suppl_6 = rbind(clean_fit_by_mo(deficit_c), clean_fit_by_mo(tots))

# class 1 and class continue at normal levels #
tots_34only = deficit_c[class_ %in% cols[3:4], lapply(.SD, sum_d), .SDcols = selected_, by=measure]

#####################
# class 2 scenarios #
#####################

# first calculate procedures done #
all_comb_c2 = all_comb_c[class_ == 'class2', ..selected_columns]
all_comb_c2[, march_2019 := c_ceiling(march_2019*0.8)] # march is 80% of normal activity
scenario_cols = selected_[4:length(selected_)]

# then four scenarios; 20/40/60/80% procedures done #
scenarios = lapply(c(0.2,0.4,0.6,0.8), function(x) cbind(all_comb_c2[,!..scenario_cols],c_ceiling(all_comb_c2[, ..scenario_cols]*x)))
names(scenarios) = c('twenty', 'forty', 'sixty', 'eighty')
scenarios_c2 = rbindlist(scenarios, idcol = T)

#calcualte new deficit by subtracting the actual number expected from all_comb_c away from the number expected to continue 
scen_deficit = lapply(scenarios, function(x) cbind(x[,!..selected_], all_comb_c[class_ == 'class2', ..selected_]-x[,..selected_]))
scen_deficit = rbindlist(scen_deficit, idcol = T)

#########################
# surgery restart       #
#########################
# class 1 continues as before
# class 2 runs from 80% back to baseline june-july-august-september back at full
# class 3 runs from 0% back to baseline september-october-november-december back at full
# class 4 runs from 0% back to baseline december-january-february-march back at full

restart_performed_2 = scenarios_c2[.id =='eighty', ..selected_columns] # is class 2 procedures performed at 80% capacity
june_restart = selected_[6:9]

# diff fun gets the difference between performed and expected for a given mo, then fractional proportion of those performed
restart_performed_2[,(june_restart) := do.call(diff_fun, list(.SD, 1:length(june_restart))),  .SDcols = june_restart, by='measure']
# back at full capacity after june_restart period (x*1.25 because baseline is 80%)
restart_performed_2[,(selected_[10:15]) := lapply(.SD, function(x) c_ceiling(x*1.25)), .SDcols = selected_[10:15], by='measure']

# class 3
restart_performed_34 = all_comb_c[class_ %in% cols[3:4], ..selected_columns]
# march at 50%
restart_performed_34[,march_2019 := c_ceiling(march_2019*0.5)]

september_restart = selected_[9:12]
restart_performed_34[class_ == 'class3',(selected_[4:8]) := 0] # no activity between april and september
restart_performed_34[class_ == 'class3', (september_restart) := do.call(prop_fun, list(.SD, 1:length(september_restart))), .SDcols = september_restart, by=c('measure', 'class_')]

#class4
december_restart = selected_[12:15]
restart_performed_34[class_ == 'class4', (selected_[4:11]):=0]
restart_performed_34[class_ == 'class4', (december_restart) := do.call(prop_fun, list(.SD, 1:length(december_restart))), .SDcols = december_restart, by=c('measure', 'class_')]

restart_performed = rbind(all_comb_c[class_ == 'class1', ..selected_columns], restart_performed_2, restart_performed_34)
#############
# Key table # 
#############
tots = restart_performed[,lapply(.SD, sum_d), .SDcols = selected_, by=measure]
tots[,class_ := 'total']

top_panel1 = rbind(clean_fit_by_mo(restart_performed), clean_fit_by_mo(tots))
#top panel of table 1, note table 1 only runs until end of february

###########################
# cumulative deficit table#
###########################
# get expected data for the year
restart_deficit = all_comb_c[,..selected_columns]

#reorder to ensure things all being subtracted accurately etc
setorder(restart_deficit, class_, measure)
setorder(restart_performed, class_, measure)

# deficit is difference between the two
restart_deficit_table = cbind(restart_deficit[,!..selected_], restart_deficit[,..selected_] - restart_performed[,..selected_])

# rolling sum the deficit
restart_rolling_deficit = restart_deficit_table[,apply(.SD, 1, function(x) c_ceiling(cumsum(x))), .SDcols = selected_]

deficit_roll = setDT(as.data.frame(t(restart_rolling_deficit)))
deficit_roll[, c('class_', 'measure')] = restart_deficit_table[,c('class_', 'measure')] 

tots = deficit_roll[, lapply(.SD, sum_d), .SDcols = selected_, by=measure]
tots[,class_ := 'total']

# note rounding issues may push certain cells to +1/-1 #

##################
# key table      #
##################
cumul_def = rbind(clean_fit_by_mo(deficit_roll), clean_fit_by_mo(tots))
# this is the bottom panel of table 1

#2.32 M procedures cancelled by March 1st.
' ------------------------------------------------------------------------------------------'
######################################
# Proportion tables for calculations #
######################################
source(paste0(getwd(), '/R/proportions_table.R'))
# ignore NA coercion
# logic in that script # 
# returns 'prop_table_w' which is used for resource estimations
setDT(prop_table_w) # should have dim 27*1073

dim(prop_table_w)
#########################
# resource implications #
#########################
# functions for resource implications are in here #
source(paste0(getwd(), '/R/resource_model.R'))
# EURO #
# OANDA on 1/7/20 = 1GBP = 1.09766 Euro #
#resource_dat = copy(deficit_roll) # note with some things in deficit roll, the number will be 0

# we only want to do resource calculations from March 2019 to February 2020 (it's actually feb 2021, but used financial years for ease)
selected_resource = selected_[3:14]

#make eu == 1 for GBP costs
performed = resource_calculator(resource_dat = restart_performed, selected_months = selected_resource, eu = 1.09766, tots=TRUE)
# if(class) errors are fine and can be ignored
deficit = resource_calculator(resource_dat = deficit_roll, selected_months = selected_resource, eu=1.09766, tots = FALSE)
# ignore deficit_roll grand total


# these each return a list of two tables#
# first === costs/resource implications.
# second === grand totals
# due to rounding across classes there may be very minor differences in terms of count*cost (e.g. value for bed day cost which = number ip * 222 * eu 
# is slightly different after rounding across classes)

#########
# plots #
#########

###########################################
## figure one has a number of components ##
###########################################

# restart surgical numbers #
restart_long = melt(restart_performed, id.vars = c('class_', 'measure'), measure.vars = selected_)
restart_cast = dcast(restart_long, variable+class_~measure, value.var = 'value')
# limit to feb 2020 to March 2021
restart_plot = restart_cast[variable %in% selected_[2:15]]

#relevel factors for labelling, easiest to do here
levels(restart_plot$class_) = c('Emergency surgery: required within 72 hours (Class 1)', 'Urgent surgery: required within 1 month (Class 2)',
                                'Semi-urgent surgery: required within 3 months (Class 3)', 'Elective surgery: required in over 3 months (Class 4)')

r_plot = restart_cast[,lapply(.SD, sum), .SDcols = (3:5), by=variable]

#################
# expected data #
#################
m_all_comb = melt(all_comb_c, id.vars = c('class_', 'measure'), measure.vars = selected_)
cast_all_comb = dcast(m_all_comb, variable+class_~measure, value.var = 'value')
expect_plot = cast_all_comb[variable %in% selected_[2:15]]

e_plot = cast_all_comb[,lapply(.SD, sum), .SDcols = (3:5), by=variable]

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

xlabs = firstup(gsub('\\_.*', '', selected_))

p1 = ggplot()+
  geom_line(aes(x=factor(variable, levels = selected_), group = 1, colour = 'b',  y=fit), size = 1.4, data = r_plot)+
  geom_ribbon(aes(x = factor(variable, levels = selected_), ymin=lwr, ymax=upr, group=1), fill = 'blue', lty=2, alpha=0.2, data = r_plot)+
  geom_line(aes(x=factor(variable, levels = selected_), group = 2, colour = 'a',  y=fit), size = 1.4, data = e_plot)+
  geom_ribbon(aes(x = factor(variable, levels = selected_), ymin=lwr, ymax=upr, group=2), fill = 'red', lty=2, alpha=0.2, data = e_plot)+
  scale_colour_manual(name = NULL, values = c('b'='blue','a'='red'), labels = c('Normal surgical activity','COVID-19 surgical activity'))+
  labs(x = NULL, y='Number of Procedures', fill = NULL)+
  scale_y_continuous(labels=function(n){format(n, scientific = F, big.mark = ',')}) +
  scale_x_discrete(labels = NULL)+
  theme_bw()+
  theme(legend.position = "top", legend.text = element_text(size = 12,face = 'bold'),axis.text = element_text(size=12,face = 'bold'), axis.title = element_text(size=12, face = 'bold'))

######################
# cumulative deficit #
######################
m_def = melt(deficit_roll, id.vars = c('class_', 'measure'), measure.vars = selected_)
cast_def = dcast(m_def, variable+class_~measure, value.var='value')
cast_def = cast_def[variable %in% selected_[2:15]]

# sort out ordering/levels
levels(cast_def$class_)= c('Emergency surgery: required within 72 hours (Class 1)', 'Urgent surgery: required within 1 month (Class 2)',
                           'Semi-urgent surgery: required within 3 months (Class 3)', 'Elective surgery: required in over 3 months (Class 4)')

#relevel to sort out bar stack order
cast_def$class_ = factor(cast_def$class_, levels=rev(levels(cast_def$class_)))

p2 = ggplot(cast_def)+
  geom_bar(aes(x=factor(variable, levels = selected_), y=fit, group=class_, colour = class_, fill=class_), stat = 'identity', alpha=0.09)+
  theme_bw()+
  labs(x=NULL, y='Number of procedures', colour = NULL, fill=NULL)+
  scale_y_continuous(labels=function(n){format(n, scientific = F, big.mark = ',')})+
  scale_x_discrete(labels = xlabs[2:length(xlabs)])+
  theme(legend.position = "bottom", legend.text = element_text(size = 10,face = 'bold'),axis.text = element_text(size=12,face = 'bold'), axis.title = element_text(size=12, face = 'bold'))

pp1 = ggplotGrob(p1)
pp2 = ggplotGrob(p2)
grid::grid.draw(rbind(pp1,pp2, size='last'))
# figure 2

##############
## figure 3 ##
##############

# performed by class #

fig_3 = ggplot(restart_plot)+
  geom_line(aes(x=factor(restart_plot$variable, levels = selected_[2:15]), y=fit, group=class_, colour=class_))+
  geom_ribbon(aes(x=factor(restart_plot$variable, levels=selected_[2:15]), ymin=lwr, ymax=upr, group=class_, colour=class_, fill = class_),alpha=0.09, lty=3)+
  theme_bw()+
  labs(x = 'Month', y='Number of procedures', colour = NULL) + 
  scale_y_continuous(labels=function(n){format(n, scientific = F, big.mark = ',')})+
  guides(fill=FALSE, group=FALSE, colour = guide_legend(override.aes = list(fill=NA)))+
  scale_x_discrete(labels = xlabs[2:15])+
  theme(axis.text.x=element_text(hjust=2), legend.position = "bottom", legend.text = element_text(size = 10,face = 'bold'),axis.text = element_text(size=12,face = 'bold'), axis.title = element_text(size=12, face = 'bold'))

# figure three needs a little manual cleaning to sort out the x axis labels.

#######################
# age tables #
##############

source(paste0(getwd(), '/R/age_profiling.R'))

a # this is the age proportions table.

###########################
# peer review sensitivity #
###########################
# 
#######################################################
# all surgery restarts on 1st June and takes 6 months #
#######################################################
restart_performed_2 = scenarios_c2[.id =='eighty', ..selected_columns] # is class 2 procedures performed at 80% capacity

june_restart = selected_[6:12]

prop_fun_six = function(x,y){
  #@x = value
  #@y = position
  return(c_ceiling((x/6)*y)) #return fraction by position if use 1:length(mo)
}

diff_fun_six = function(x,y){
  # for june because we need to account for ceiling from 80% baseline
  gap = c_ceiling(x*1.25 - x)
  done = prop_fun_six(gap, y)
  return(x+done)
}
# diff fun gets the difference between performed and expected for a given mo, then fractional proportion of those performed
restart_performed_2[,(june_restart) := do.call(diff_fun_six, list(.SD, 1:length(june_restart))),  .SDcols = june_restart, by='measure']

# back at full capacity after june_restart period (x*1.25 because baseline is 80%)
restart_performed_2[,(selected_[13:15]) := lapply(.SD, function(x) c_ceiling(x*1.25)), .SDcols = selected_[13:15], by='measure']

# class 3 & 4
restart_performed_34 = all_comb_c[class_ %in% cols[3:4], ..selected_columns]
# march at 50%
restart_performed_34[,march_2019 := c_ceiling(march_2019*0.5)]
restart_performed_34[,(selected_[4:5]) := 0] # no activity between april-may
restart_performed_34[, (june_restart) := do.call(prop_fun_six, list(.SD, 1:length(june_restart))), .SDcols = june_restart, by=c('measure', 'class_')]

restart_performed = rbind(all_comb_c[class_ == 'class1', ..selected_columns], restart_performed_2, restart_performed_34)

restart_deficit = all_comb_c[,..selected_columns]

#reorder to ensure things all being subtracted accurately etc
setorder(restart_deficit, class_, measure)
setorder(restart_performed, class_, measure)

# deficit is difference between the two
restart_deficit_table = cbind(restart_deficit[,!..selected_], restart_deficit[,..selected_] - restart_performed[,..selected_])

# rolling sum the deficit
restart_rolling_deficit = restart_deficit_table[,apply(.SD, 1, function(x) c_ceiling(cumsum(x))), .SDcols = selected_]

deficit_roll = setDT(as.data.frame(t(restart_rolling_deficit)))
deficit_roll[, c('class_', 'measure')] = restart_deficit_table[,c('class_', 'measure')] 

tots = deficit_roll[, lapply(.SD, sum_d), .SDcols = selected_, by=measure]
tots[,class_ := 'total']

##################
# key table      #
##################
cumul_def = rbind(clean_fit_by_mo(deficit_roll), clean_fit_by_mo(tots))
write.csv(cumul_def, file = 'sensitivity_analysis.csv', row.names = T)
