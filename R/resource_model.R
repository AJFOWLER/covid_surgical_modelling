####################################
# calculate resource use functions #
####################################

# do calculations on basis of proportions table
prop_table_calcs = function(value, class_, measure_ = '_prop_bed'){
  # prop bed as standard, can also be '_prop_dc' or '_proportional_cost'
  if(!(measure_ %in% c('_prop_bed', '_prop_dc', '_proportional_cost'))){stop('must be one of _prop_bed, _prop_dc, _proportional_cost')}
  
  # paste class and measure together
  select_prop_t = paste0(class_, measure_)
  
  # select correct multiplier from prop_table_w, which is tagged by class_measure
  
  multipliers = prop_table_w[,..select_prop_t] ## not ideal to call something outside direct namespace but this is a very particular use case.
  
  return(unlist(lapply(1:length(value), function(x) sum_d(value[[x]]*multipliers[[x]]))))
  # return the output which is value passed * multipliers
}

#return data table collapsing selected_*multiply with !selected_
dt_return = function(dt, multiply, selected_months){
  # where selected_ are relevant columns  
  return(cbind(dt[,!..selected_months], c_ceiling(dt[,..selected_months]*multiply)))
}


select_months_return = function(selected_months){
  # initialise selected months to reduce duplication
  dt_return = function(dt, multiply){
    return(cbind(dt[,!..selected_months], c_ceiling(dt[,..selected_months]*multiply)))
  }
}

select_resource_return = function(selected_months){
  #initialise with selected_months and then can run over easily
  return_resource = function(x){
    # per resource we want to return a single line per resource #    
    values = x[,lapply(.SD, function(y) sum_d(y)),.SDcols = selected_months, by=measure]
    values[,class_ := 'val']
    return(clean_fit_by_mo(values))
  }
}

totalled_costs = function(resource_list, list_of_summed_vars, selected_months){
  #calculate totals for specific resources (e.g. grand high/grand low etc)
  e_l = rbindlist(resource_list[list_of_summed_vars])
  e_l_c = e_l[,lapply(.SD, function(x) c_ceiling(sum_d(x))), .SDcols = selected_months, by=measure]
  #add summed total column
  e_l_c[,total := Reduce(sum_d, .SD), .SDcols = selected_months, by=measure]
  e_l_c_format = e_l_c[, lapply(.SD, function(x) nice_format(x)), .SDcols = c(selected_months, 'total')]
  
  return(e_l_c_format)
}

# calculate totals (end row)
return_total = function(x, selected_months){

  # sum across selected months
  
  values = x[, as.integer64(Reduce(sum_d, .SD)), .SDcols = selected_months, by=measure]
  # as.integer64 because billions mishandled by 32bit numeric values  
  # interesting problem with cost; this is value is large and >32bit number
  
  return(nice_format(values))
  # this will throw warning errors as nice_format has ='numeric' and we are iterating over dt.)
}


resource_calculator = function(resource_dat, selected_months, eu, tots = TRUE){
  # for calculations we need to work out bed days, day case, cost and ip. 
  # then we can do simple multiplications to determine the overall costs associated
  # initialise resource returners:
  dt_r = select_months_return(selected_months)
  # columns to be used
  touse = c('measure', 'class_', selected_months)
  # get dat restricted to selected_months
  resource_dat = resource_dat[,..touse]
  
  # bed days
  bdays = cbind(resource_dat[,!..selected_months],resource_dat[,lapply(.SD, function(x) prop_table_calcs(x, class_, '_prop_bed')), .SDcols = selected_months])

  # day case
  dcase = cbind(resource_dat[,!..selected_months],resource_dat[,lapply(.SD, function(x) prop_table_calcs(x, class_, '_prop_dc')), .SDcols = selected_months])
  
  # no emergency procedures are day case
  dcase[class_=='class1', (selected_months):=0]

  # cost from HRGs
  cost = cbind(resource_dat[,!..selected_months],resource_dat[,lapply(.SD, function(x) prop_table_calcs(x, class_, '_proportional_cost')), .SDcols = selected_months])

  # need to alter cost here to account for euros
  cost = cbind(cost[,!..selected_months], cost[,..selected_months]*eu)
  
  # IP care (is also the number of beddays swabs if they have an extra bed days)
  ip = cbind(resource_dat[,!..selected_months], resource_dat[,..selected_months] - dcase[,..selected_months])

  # classes 2:4 1%of IP admissions
  ITU = cbind(ip[,!..selected_months], c_ceiling(ip[,..selected_months]*0.01))

  # class 1 = 4%, not relevant to deficit roll as no deficit
  ITU[class_ == 'class1', (selected_months) := lapply(.SD, function(x) x*4), .SDcols = selected_months] #4% in class1
  
  # PPE high is 4 items for 8 staff
  ppe_h = dt_r(resource_dat, (8*4)) #TOTAL ITEMS

  # PPE low is 4 items for 4 staff (half ppe abve which is 8*4)
  ppe_l = dt_r(ppe_h, 0.5) # TOTAL ITEMS

  # bedday cost
  bdcost_l = dt_r(ip, 222*eu) # low
  bdcost_h = dt_r(ip, 346*eu) # high estimate

  swabs = dt_r(resource_dat, 2) # two swabs per procedure
  swabscost = dt_r(swabs, 19*eu) # £19 each

  # this changed with peer reviewer comments #
  ct = resource_dat[class_ == 'class1', lapply(.SD, function(x) c_ceiling(sum_d(x*prop_table_w$ct_scan))), by=measure, .SDcols = selected_months]
  ct[,class_:= 'class1']
  ct_cost = dt_r(ct, 69*eu)
  
  gown_low = dt_r(resource_dat, (4*3)*eu) # 4 people at cost of £3
  gown_high = dt_r(resource_dat, (8*3)*eu) # 8 people at cost of 3

  # visor cost == ffp3 cost hence repeated below.
  visor_low = dt_r(resource_dat, (2.90*4)*eu)
  visor_high = dt_r(resource_dat, (2.90*8)*eu)

  gloves_low = dt_r(resource_dat, (0.298*4)*eu)
  gloves_high = dt_r(resource_dat, (0.298*8)*eu)

  all_ppe_low = dt_r(resource_dat, (9.098*4)*eu) #cost of a donn for one person
  all_ppe_high = dt_r(resource_dat, (9.098*8)*eu)
  
  # now construct overall costs and format
  resources = list(resource_dat,bdays, dcase, cost, ip, ITU, ppe_l, bdcost_l, ppe_h, bdcost_h, swabs, swabscost, ct, ct_cost, 
                       gown_low, visor_low, gloves_low, visor_low, gown_high, visor_high, gloves_high, visor_high, all_ppe_low, all_ppe_high)
  
  names(resources) = c('total_no','bdays', 'dcase', 'cost', 'ip', 'ITU','ppe_l', 'bdcost_l', 'ppe_h', 'bdcost_h', 'swabs', 'swabscost', 'ct', 'ct_cost',
                       'gown_low', 'visor_low', 'gloves_low', 'ffp3_low', 'gown_high', 'visor_high', 'gloves_high', 'ffp3_high', 'all_ppe_low', 'all_ppe_high')
  
  # initialise return_resource model with selected months
  return_resource = select_resource_return(selected_months = selected_months)
  
  out_frame = rbindlist(lapply(resources, return_resource), idcol = T)
  
  excess_low = c('ppe_l', 'bdcost_l', 'swabscost', 'ct_cost', 'all_ppe_low')
  
  excess_l_formatted = totalled_costs(resources, excess_low, selected_months=selected_months)
  
  excess_high = c('ppe_h', 'bdcost_h', 'swabscost', 'ct_cost', 'all_ppe_high')
  
  excess_h_formatted = totalled_costs(resources, excess_high, selected_months=selected_months)
  
  excess_grand_low = totalled_costs(resources, c(excess_low, 'cost'), selected_months = selected_months)
  
  excess_grand_high = totalled_costs(resources, c(excess_high, 'cost'), selected_months = selected_months)
  
  excesses_ = rbindlist(list(excess_l_formatted, excess_h_formatted, excess_grand_low, excess_grand_high), idcol = T)
  excesses_$totals = c('low_excess', 'high_excess', 'low_grand', 'high_grand')
  if(tots == TRUE){
    out_frame = cbind(out_frame, 'Total' = unlist(lapply(resources, return_total, selected_months = selected_resource)))
    return(list(out_frame, excesses_))
  }
  else{return(list(out_frame, excesses_))}
}

# 
# lapply(resources, return_total, selected_months = selected_resource)
# a = rbindlist(resources[excess_low])
# a = rbindlist(resources[excess_high])
#     
#     cbind(out_frame, 'Total' = unlist(lapply(resources, return_total)))
    
# totals return clean #
# only for future stuff # 

