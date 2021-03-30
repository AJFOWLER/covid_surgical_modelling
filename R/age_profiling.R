################
## Age tables ##
################

'we want to be able to calculate how many patients in different age categories
across different classes. This uses the same logic as the prop_table_w above but
is slightly more complicated because it requires additional logic to deal with
the missingness of certain portions of age data.
'

age_cols = names(all_dat_surg)[18:41]

# mean number in each age group per opcs
age_table = all_dat_surg[,lapply(.SD, mean), by=OPCS, .SDcols=age_cols]

#return cols
age_return = do.call(paste0, list(age_cols, '_proportion_of_all'))

# calculate what proportion of the total number of procedures each age category accounts for
# we need to do this because there is a variable level of missingness int he age data we need to account for; assume that the distribution of missing data is the same as the observed.
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

# check this manually - works:

rm(class_1, class_2, class_3, class_4, class2cols, proc_no, age_cols, age_return)

# class columns
class_cols = c('class1', 'class2', 'class3', 'class4')

# create age columns
age_classes = do.call(paste0, list('_of_',class_cols))

# do this based on an 'average month' across 2019-2020 and 2020-2021
mean_by_class = all_comb[measure=='fit',rowMeans(.SD), .SDcols = (1:12), by = .(class_)] # mean number of procedures per month

mean_by_class = mean_by_class[,mean(V1), by=class_]

return_frame = data.frame(class_ = character(), totals = integer(), zf = integer(), f_fn = integer(), s_sf=integer(), sf_=integer(), stringsAsFactors = F)  

for(classy in 1:length(class_cols)){
  
  # get number of procedures for that class 
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

rm(age_table, all_class, return_frame,mean_by_class,class_cols)

# class_           totals   zf  f_fn  s_sf   sf_
# 1: class1 70048.2916666667 5707 41394 11516 11431
# 2: class2 34294.2916666667 1841 17410  9745  5300
# 3: class3 155284.958333333 7274 64668 43010 40332
# 4: class4 117526.791666667 9445 54491 35733 17856
