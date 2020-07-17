##########################################
# Proportions table for subsequent calcs #
##########################################

'Create a table per-OPCS code & class that 
enables subsequent estimation of the proportion
each should contribute to different features
'
# these are cols to run over
cols_ = c("Admissions", 'class1', 'class2', 'class3', 'class4')

# proportion of inpatient
mean_ip = all_dat_surg[,sum(inpatient)/sum(elective), by=OPCS]

# median mean LOS (median of these values used given non-normal distribution)
mean_LOS = all_dat_surg[,.(median(`Mean length of stay`, na.rm=T)), by=OPCS]

# median median LOS
median_LOS = all_dat_surg[,.(median(`Median length of stay`, na.rm=T)), by=OPCS]

# mean number of admissions and class by OPCS codes
adm_class = all_dat_surg[, lapply(.SD, function(x) mean(x,na.rm=T)), .SDcols = cols_, by=OPCS]
# note this is mean of years of data present; handful of codes only present for a small number of years so might be slightly over represented
# these account for <0.05% of procedures so will not meaningfully influence our estimations

# merging all of the above together with standard data.table syntax
prop_table_w = mean_ip[mean_LOS, on='OPCS'][median_LOS, on='OPCS'][adm_class, on='OPCS', `:=`('admissions' = i.Admissions, 'class1' = i.class1, 'class2' = i.class2, 'class3' = i.class3, 'class4' = i.class4)]

# change names to make next steps easier and clearer
names(prop_table_w) = c('OPCS', 'Prop_IP', 'Median Mean LOS', 'Median Median LOS', 'Admissions', 'class1', 'class2', 'class3', 'class4')

rm(mean_ip, mean_LOS, median_LOS, adm_class)

######################################################################
# Calculate proportion to each class that each procedure contributes #
######################################################################

# note all is class based from here on down as rounding/averaging by main admission counts adds additional complexity #

# make columns for return
return_cols = do.call(paste0, list(cols, '_proportion'))

# number of each class / sum of col class by procedure
prop_table_w[,(return_cols) :=lapply(.SD, function(x) x/sum_d(x)), .SDcols = cols]

# calculate proportion of cases that are day case
day_cols = do.call(paste0, list(cols, '_prop_dc'))

# to do this we need to calculate the proportion of each class that is 1-prop_IP
# this is the class proportion * 1-Prop
# this weights the contribution of all procedures by class
prop_table_w[, (day_cols):=lapply(.SD, function(x) x*(1-Prop_IP) ), .SDcols=return_cols, by=OPCS]

bed_cols = do.call(paste0, list(cols, '_prop_bed'))

# multiply proportion by LOS then when multipled up gives bed days for IP procedures only
prop_table_w[, (bed_cols) := lapply(.SD, function(x) (x*Prop_IP)*`Median Mean LOS`), .SDcols=return_cols, by=OPCS]

#class 1 == all inpatient, so proportion*LOS is fine.
prop_table_w[, class1_prop_bed := class1_proportion*`Median Mean LOS`, by=OPCS]

####################
# Example/checking #
####################
# A01 , class 1
#0.000395434331 *sum(prop_table_w$class1)
#3.954343e-04 * sum(prop_table_w$class1) = 325.8387
#8.712264 *37.4 # Median Mean LOS & median class 1 for this code == 325.8387
# this is correct!

# F22, class 2
#View(all_dat_surg[OPCS == 'F22'])
#View(prop_table_w[OPCS == 'F22'])
# day case
# so amongst sum(prop_table_w$class3) should == 36.3 day case procedures
#0.00001469073*sum(prop_table_w$class3) # WORKS

######################################
# add in cost data                   #
# replicated 2017 paper costing data #
######################################

cost_df = setDT(read.csv('opcs_grouping.csv', stringsAsFactors = F))

names(cost_df)[1] = 'OPCS'

prop_table_w[cost_df, on='OPCS', 'value' := as.numeric(i.HRG_value)]

cost_cols = do.call(paste0, list(cols, '_proportional_cost'))

prop_table_w[, (cost_cols) := lapply(.SD, function(x) x*value), .SDcols = (10:13)]

# now we multiply this value for each class by the total number
# check cost of A03
a = 'A03' # class3 cost
prop_table_w[,sum(class3)] # 2,471,429 
prop_table_w[OPCS == a] # 11.2 cases on average, value of this procedure is 5,287
# WORKS
#11.2*5287 #59214.4
#prop_table_w[OPCS == a, class3_proportional_cost*2471429] # works.

rm(a, bed_cols, day_cols, return_cols, cost_df, cost_cols)

# returns prop_table_w
#------------------------ peer review alterations ---------------------#

# per peer reviewer comments, modified CT scan frequency to be those having:
# class1 thoracic/cardiac/lowerGI/upperGI/Female UGU/Urological/HPB/Major Vessel
relevant_codes = unique(all_dat_surg[proc_group %in% c('Cardiac', 'Thoracic', 'HPB', 'Female UGU', 'Lower GI', 'Major Vessel', 'Thoracic', 'Upper GI', 'Urological'), OPCS])
prop_table_w[OPCS %in% relevant_codes, ct_scan := class1_proportion] # this * by the number of cases will give the number of scans weighted for frequency of different procs
rm(relevant_codes)