############################
# import bed activity data #
# from NHS England site    #
############################

# this is downloaded from NHS England site
'date of analysis download == 18th May 2020'
MAR = get_MAR()

clean_MAR = initial_clean_hes(MAR) # initial clean uses count of NAs to drop empty rows.

names(clean_MAR) = clean_MAR[1,] # names top now

clean_MAR = clean_MAR[-1,] # drop top row

setDT(clean_MAR) # confirm rows match vs. manual (143 rows of data)

#------------ analysis logic ---------------------------------------------------------------------------------
'
MAR data is spell based (e.g. admissions, not episodes)
Elective G&A Total Admissions = all elective admissions. 
G&A planned total admissions are separated out, but are included in this total, so do not need to be added.
Total Non-elective G&A admissions = non-elective admissions.
Importantly Non-elective includes maternity, other and emergency admissions.
'
clean_MAR = clean_MAR[,c('Year', 'Period', 'Elective G&A Total Admissions (FFCEs)', 'Total Non-elective G&A Admissions (FFCEs)')]

# select numeric cols
num_cols = c('Elective G&A Total Admissions (FFCEs)', 'Total Non-elective G&A Admissions (FFCEs)')

# to numeric, visual check confirms there are no , that will cause problems
clean_MAR[, (num_cols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = num_cols]

# change names to make next steps easier
names(clean_MAR)[3:4] = c('elective', 'non_elective')

# limit to time period of interest (2014/15 to 2018/2019)
years = c('2014-15', '2015-16', '2016-17', '2017-18', '2018-19')
clean_MAR = clean_MAR[Year %in% years,]

# total admissons = Elective total + non-elective totals
clean_MAR[,total_admissions := elective + non_elective]

# mean number of admissions per month over the years
# if you use mean of the calculated proportion_by_total, then the number >1 because of inter-year fluctuations
# calculate as total number of electives over the whole time period, and then /by 
# distribution of elective cases over the year:

clean_MAR[,elective_year_tot := sum(elective), ]
clean_MAR[, proportion_of_elective_by_total := sum(elective)/elective_year_tot, by=Period]

#distribution of emergnecy cases over the year
clean_MAR[,emerg_year_tot := sum(non_elective)]
clean_MAR[,proportion_of_emerg_by_total := sum(non_elective)/emerg_year_tot, by=Period]

# what proportion of elective admissions are performed each month?
elec_by_mo = clean_MAR$proportion_of_elective_by_total[1:12] # starts april
emerg_by_mo =clean_MAR$proportion_of_emerg_by_total[1:12] #starts april
period = tolower(clean_MAR$Period[1:12]) # starts april

rm(clean_MAR, MAR, num_cols, years)