######################################################
# 1. Identify urls of interest for years of interest #
######################################################

get_HES_url <- function(years) { #year format = 1999,2000 etc. as list c(year,year,year)
  
  base_before_15 <- "https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity/hospital-episode-statistics-admitted-patient-care-england"
  base_after_15 <-  "https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity"
  
  # add one year, paste by - and remove two digits after '-'
  years_add = years+1
  
  # now get only those bits
  years_added = substr(years_add, 3,4)
  
  # paste together
  years_list = paste(years, years_added, sep = '-')
  
  early = which(years < 2015)
  late = which(years >= 2015)
  
  all_url = c()
  if(length(early >= 1)){
    early_url = paste(base_before_15, years_list[early], sep='-')
    all_url = c(all_url, early_url)
  }
  if(length(late)>=1){
    late_url = paste(base_after_15, years_list[late], sep='/')
    all_url = c(all_url, late_url)
  }
  return(all_url)
}

#############################################
# 2: URLS of interest for years of interest #
#############################################

# two parameters; years(vector), diag_op(character, options = 'ops', 'diag', 'both'), or both
get_HES_filelink <- function(years, diag_op = 'ops'){
  if(toupper(diag_op) == 'OPS'){
    diag_op_grep = 'ops|proc-'
  }else if(toupper(diag_op) == 'DIAG'){
    diag_op_grep = 'diag'}
  else if(toupper(diag_op) == 'BOTH'){
    diag_op_grep = 'diag|ops|proc|opcs'
  }
  else{
    stop('must specify either DIAG or OPS for different code sets, choose both if you feel greedy')
  }
  
  out = c()
  urls = get_HES_url(years)
  for(i in 1:length(years)){
    page = read_html(urls[i])
    a = page %>%
      html_nodes('a') %>% #get links
      html_attr("href") #identify those links
    
    excels = which(grepl('\\.xls',a)) # find excel sheets
    
    diag_op_correct = which(grepl(diag_op_grep, a)) # find those with diag/op in them
    
    to_add = intersect(excels, diag_op_correct)
    
    out = c(out, a[to_add])
  }
  return(out)
  #this may return more files than you'd like!
}

# download files and process
download_hes_records_clean = function(file_link){
  dl_hes = download_hes(file_link)
  sorted_rows = initial_clean_hes(dl_hes)
  sorted_headers = assign_header_hes(sorted_rows)
  return(sorted_headers)
}
  
download_hes = function(file_link){
  # will work for current data, historic will be more challenging
  tmp = tempfile()
  #download to temp file
  file = download.file(file_link, mode='wb', destfile=tmp)
  # find three character primary procedure coding sheet
  TCharPrim = which(grepl('primary.+[3].+', tolower(excel_sheets(tmp))))
  #read this sheet
  dt = read_xlsx(tmp, sheet=TCharPrim)
  # remove only this tempfile from tempdir
  file.remove(tmp)
  return(dt)
  }

initial_clean_hes = function(dat){
  # remove entirely NA rows
  dt_r = dat[rowSums(is.na(dat)) != ncol(dat),] # 
  # remove entirely NA cols
  drop_cols = which(colSums(is.na(dat)) == nrow(dat))
  # remove Na columns
  if(length(drop_cols) >= 1){
    dt_r = dt_r[,-drop_cols]
  }
  # there are rows at the bottom which are notes, drop these
  a = rowSums(is.na(dt_r))
  bottom_drop = which(max(a) - a <5 )
  dt_r = dt_r[-bottom_drop,]
  return(dt_r)
}

assign_header_hes = function(dat){
  potential_headers = find_header_hes(dat)
  View(dat[1:max(potential_headers)])
  selected_header = type_integer('Enter the row number of header row::')
  selected_total_row = type_integer('Enter the row number of total row, N if no total row::')
  rows_ = selected_header:nrow(dat)
  rows_no_tot = rows_[rows_ != selected_total_row]
  fresh_dt = dat[rows_no_tot,] 
  colnames(fresh_dt) = fresh_dt[1,]
  fresh_dt = fresh_dt[-1,]
  return(fresh_dt)
}

# find header
find_header_hes = function(dat){
  # to find header rows, identify which rows have the same number 
  a = rowSums(!is.na(dat))
  # should have <10 missing
  potential_headers = which(mean(a) - a <10)[1:3]
  return(potential_headers)
}

# return values from typed options
type_integer = function(prompt_text){
  n = readline(prompt = prompt_text)
  if(tolower(n) == 'n'){
    n = 'No total'
  }
  else{
    n = as.integer(n)
  }
  return(n)
  }

# replace '*' with 7 #
drop_star=function(x) as.data.frame(apply(x, 2, function(y) gsub('[*]', '7', y)))

# replace '-' with 0 #
drop_hyp=function(x) as.data.frame(apply(x, 2, function(y) gsub('-', '0', y)))

# standard rounding
c_ceiling = function(x){return(round(x,0))} # this is needed because sometimes 'ceiling' results in some very odd behaviour when numbers >6 digits

# Download MAR data#

get_MAR = function(){
  mar_dat ='https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/04/MAR_Comm-Timeseries-Feb-20-EfWXs.xls'
  # will work for current data, historic will be more challenging
  temp_f = tempfile()
  #download to temp file
  dl_file = download.file(mar_dat, mode='wb', destfile=temp_f)
  #read this sheet
  dt = readxl::read_xls(temp_f, sheet=1)
  #remove only this temporary file from tempdir()
  file.remove(temp_f)
  return(dt)
}



plot_MAR_data = function(x, variable, average, grouper, ...){
    order = tolower(month.name)
    
    ggplot2::ggplot(x)+
      geom_line(aes, x=factor(get(Period), levels=order), y=get(variable), group=get(grouper), colour = get(grouper))+
      geom_line(aes(y=average))+
      scale_colour_manual(values = c(rep('darkgrey',12), "darkslategray1"))+
      theme_bw()
}

# model functions #

# prediction function #
pred_fun = function(model){
  return(predict(model, interval = c('confidence'), level = 0.95, type='response', newdata = data.frame(x=2014:2021)))
}

# modelling function for dt
model_fun_dt = function(lhs, year){
  y = lhs
  x = as.numeric(year)
  mod = lm(y~x, model = F)
  p = pred_fun(mod) #run prediction from 2014-2021
  p = cbind(p,Year = 2014:2021)
  return(p)
}

# clean up return for 2019:2020
clean_return = function(class, year){
  output = model_fun_dt(class, year)
  print(colnames(output))
  clean_output = output[output[,'Year'] %in% 2019:2020, colnames(output)]
  return(as.data.frame(clean_output))  
}

# BJS style is that of 100 000 not 100,000
thousand_gap = function(x){format(x, big.mark=" ", trim = TRUE)}

sum_d = function(x,...){
  sum(x, na.rm=T,...)
}

# clean return of fit/lwr/upper
clean_fit_by_mo = function(dt){
  cp = dt
  cp.1 = melt(cp, id=c('class_', 'measure'))
  cp.2 = dcast(cp.1, class_+variable~measure)
  cp.2[,format_col := paste0(thousand_gap(c_ceiling(fit)), ' (', thousand_gap(c_ceiling(lwr)), ' to ', thousand_gap(c_ceiling(upr)), ')')]
  cp.3 = dcast(cp.2, class_~variable, value.var='format_col')
  return(cp.3)
  }

# format table_1
paster_iqr = function(x,y){paste0(x, ' (', y,')')}

# nice format generic
nice_format = function(a){
  if(class(a) %in% c('data.table', 'data.frame')){
    fit = a[1,2]
    lwr = a[2,2]
    upr = a[3,2]
  }
  else if(class(a) == 'numeric'){
    fit=a[1]
    lwr = a[2]
    upr =a[3]
  }else{
  fit = a[1,2]
  lwr = a[2,2]
  upr = a[3,2]
  }
  return(paste0(thousand_gap(c_ceiling(fit)), ' (', thousand_gap(c_ceiling(lwr)), ' to ', thousand_gap(c_ceiling(upr)), ')'))
}

# functions for return of surgery
prop_fun = function(x,y){
  #@x = value
  #@y = position
  return(c_ceiling((x/4)*y)) #return fraction by position if use 1:length(mo)
}

diff_fun = function(x,y){
  # for june class2 because we need to account for ceiling from 80% baseline
  gap = c_ceiling(x*1.25 - x)
  done = prop_fun(gap, y)
  return(x+done)
}
