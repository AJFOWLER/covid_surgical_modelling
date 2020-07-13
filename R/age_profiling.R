################
## Age tables ##
################

'we want to be able to calculate how many patients in different age categories
across different classes. This uses the same logic as the prop_table_w above
'

age_cols = names(all_dat_surg)[18:41]

# mean number in each age group per opcs
age_table = all_dat_surg[,lapply(.SD, mean), by=OPCS, .SDcols=age_cols]

#return cols
age_return = do.call(paste0, list(age_cols, '_proportion_of_all'))

# calculate what proportion of the total number of procedures each age category accounts for
age_table[,tot_age_cols :=sum_d(.SD), .SDcols = age_cols, by=OPCS]

age_table[,(age_return) := lapply(.SD, function(x) x/tot_age_cols), .SDcols=age_cols, by=OPCS]

# now calculate per class
# bind in proportions from prop_table_w
age_table[prop_table_w, on='OPCS', `:=`('class1_proportion' = i.class1_proportion, 'class2_proportion' = i.class2_proportion, 'class3_proportion' = i.class3_proportion, 'class4_proportion' = i.class4_proportion)]

# for each class, multiply proportion of this OPCS code makes up of all procs by class by proportion of each age category
class_1 = do.call(paste0, list(age_cols, '_of_class1'))
age_table[, (class_1) := lapply(.SD, function(x) x * class1_proportion), by=OPCS, .SDcols=age_return]

class_2 = do.call(paste0, list(age_cols, '_of_class2'))
age_table[, (class_2) := lapply(.SD, function(x) x * class2_proportion), by=OPCS, .SDcols=age_return]

class_3 = do.call(paste0, list(age_cols, '_of_class3'))
age_table[, (class_3) := lapply(.SD, function(x) x * class3_proportion), by=OPCS, .SDcols=age_return]

class_4 = do.call(paste0, list(age_cols, '_of_class4'))
age_table[, (class_4) := lapply(.SD, function(x) x * class4_proportion), by=OPCS, .SDcols=age_return]


class2cols=names(age_table)[grepl('_of_class2', names(age_table))]
proc_no=sum(prop_table_w$class2)
age_table[, lapply(.SD, function(x) x*proc_no), .SDcols=class2cols]

rm(class_1, class_2, class_3, class_4, class2cols, proc_no, age_cols, age_return)

--  clean this up ---
  ####################################################
# pooled mean age for this group                   #
####################################################
for_age_base_data = all_comb.1[variable %in% mo_order[3:14], sum(value), by=c('measure', 'class_')]

# proportion by class
for_age_props = prop_table_w[, c('OPCS', 'class1_proportion', 'class2_proportion', 'class3_proportion', 'class4_proportion')]

age_tab_1 = all_dat_surg[, c(list(OPCS=OPCS), lapply(.SD, function(x) `Mean age`*x)), .SDcols=cols] # multiply count of admissions in each class by mean age
# mean * number for each class, for each opcs
# now sum these by OPCS
age_tab_2 = age_tab_1[,lapply(.SD, sum), by=OPCS, .SDcols=(2:5)]

all_opcs_count = all_dat_surg[,lapply(.SD, sum), by=OPCS, .SDcols=cols]

pooled_mean_age = (age_tab_2[,2:5]/all_opcs_count[,2:5])

master_age = for_age_props[,2:5]*pooled_mean_age

#number of 'pooled years' contributed to proportionately by class

sum_d(master_age$class1_proportion * 2471429)/2471429

# this works
one = sum(for_age_base_data$V1[10]*master_age$class1, na.rm=T)/for_age_base_data$V1[10]

two = sum(for_age_base_data$V1[1]*master_age$class2, na.rm=T)/for_age_base_data$V1[1]

three = sum(for_age_base_data$V1[2]*master_age$class3, na.rm=T)/for_age_base_data$V1[2]

four = sum(for_age_base_data$V1[3]*master_age$class4, na.rm=T)/for_age_base_data$V1[3]

tot_age = one*for_age_base_data$V1[10] + two*for_age_base_data$V1[1] + three*for_age_base_data$V1[2] + four*for_age_base_data$V1[3]

tot_age/4547534

'mean number of procedures by age groups'

# class columns
class_cols = c('class1', 'class2', 'class3', 'class4')

# create age columns
age_classes = do.call(paste0, list('_of_',class_cols))

#
mean_by_class = all_comb[measure=='fit',Reduce('mean',.SD), .SDcols = (1:12), by = c('class_')] # mean number of procedures per month

return_frame = data.frame(class_ = character(), totals = integer(), zf = integer(), f_fn = integer(), s_sf=integer(), sf_=integer(), stringsAsFactors = F)  

for(classy in 1:length(class_cols)){
  # get number of procedures for that class 
  
  # order of class_cols and age_classes are the same
  proc_no = mean_by_class$V1[mean_by_class$class_==class_cols[[classy]]]
  
  # find relevant columns from age_table
  find_cols = names(age_table)[grepl(age_classes[[classy]], names(age_table))]
  
  # multiply the procedure numbers by the proportion in each age group 
  all_class = age_table[,lapply(.SD, function(x) c_ceiling(sum_d(x*proc_no))), .SDcols=find_cols]
  # sum by groupings 0:14
  zf = all_class[,Reduce(`+`, .SD), .SDcols =(1:4)] 
  # 15-59
  f_fn = all_class[, Reduce(`+`, .SD), .SDcols=(5:17)]
  #60-74
  s_sf = all_class[, Reduce(`+`, .SD), .SDcols=(18:20)]
  #75+
  sf_ = all_class[, Reduce(`+`, .SD), .SDcols=(21:24)]
  # into master frame
  return_frame[classy, ] = c(class_cols[[classy]], proc_no, zf, f_fn, s_sf, sf_)
}

setDT(return_frame)

#calculate proportions by group
a = return_frame[, lapply(.SD, function(x) round(as.numeric(x)/as.numeric(totals)*100,1)), .SDcols = (3:6)]
a[,class_ := c('class1', 'class2', 'class3', 'class4')]
return_frame[, class_ := c('class1', 'class2', 'class3', 'class4')]
a[return_frame, on = 'class_', 'Total procedures' := i.totals]
mean_by_class[class_ == 'class2', ]
# ensure numbers match (totals & mean_by_class)
names(a) = c('0 to 14 years', '15 to 59 years', '60 to 74 years', '75+ years','Class', 'Total procedures')