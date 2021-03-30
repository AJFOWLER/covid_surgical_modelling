###########################################
# update graph based on SAIL data numbers #
###########################################

# start with all_comb from line 279 #
# don't need to do this at the level of the class as we just need overall numbers
# core_assumptions <- list('march20' = .6, 'april20' = .25, 'may20' = .30, 'june20' = .35, 'july20' = .35, 'august20' = .45)
# 
# lockdown_assumptions <- append(core_assumptions, list('september20' = .55, 'october20' = .65, 'november20' = 0.5, 'december20' = .25, 'january21' = .3, 'february21' = .25, 'march21' = .35,
#                                'april21' = .45, 'may21' = .55, 'june21' = .65))
# 
# persist_assumptions <- append(core_assumptions, list('september20' = .55, 'october20' = .55, 'november20' = 0.55, 'december20' = .55, 'january21' = .55, 'february21' = .55, 'march21' = .65,
#                                                       'april21' = .75, 'may21' = .85, 'june21' = .95))
# 
# grow_assumptions <- append(core_assumptions, list('september20' = .55, 'october20' = .65, 'november20' = 0.75, 'december20' = .85, 'january21' = .95, 'february21' = 1, 'march21' = 1,
#                                                      'april21' = 1, 'may21' = 1, 'june21' = 1))

# assumptions table
assumption <- data.frame('month' = period, 'year' = c(rep(2019, 12), rep(2020,12), rep(2021, 12)))
# select months 2019 January --> July 2021
assumption <- assumption[10:27,]

assumption$lockdown <- c(1,1,.6,.25,.30,.35, .35, .45, .55, .65, .5, .25, .3, .35, .35, .45, .55, .65)
assumption$persist <- c(1,1,0.6, .25, .30, .35, .35, .45, .55, .55, .55, .55, .55, .55, .65, .75, .85, .95)
assumption$grow <- c(1,1,.6,.25,.3,.35,.35,.45,.55,.65,.75,.85,.95,1,1,1,1,1)

# 2019 = 2019 to MArch 2020, 2020 is to March 2021, 2021 is to March 2022

# for the purpose of the graph, assume that 2020-2021 data is simply the same as 2019-2020
head(all_comb)

# collapse by class
all_class <- all_comb[,lapply(.SD, sum), by=.(Year, measure), .SDcols = (1:12)]

# need April, May and June for 2021
next_yr <- all_class[Year == 2020]
next_yr[,Year := 2021]

full_dat <- rbind(all_class, next_yr)

# melt to long
melted_d <- melt(full_dat, id.vars = c('Year', 'measure'), measure.vars = period)

# join on assumptions data
melted_d[assumption, on=.(Year = year, variable = month), `:=`('lockdown' = i.lockdown, 'persist' = i.persist, 'grow' = i.grow)]

melted_d <- melted_d[!is.na(lockdown),] # drop those where no assumptions needed.

# calculate the number of procedures done in that period
calcols <- paste0(names(melted_d)[5:7], '_calc')
melted_d[,(calcols) := lapply(.SD, function(x) value*x), .SDcols = 5:7]

# calculate the deficit
defcols <- paste0(names(melted_d)[5:7], '_deficit')
melted_d[,(defcols) := lapply(.SD, function(x) value-x), .SDcols = calcols]

# calculate rolling deficit based on these numbers
# order by month and by year
setorder(melted_d, Year)
head(melted_d) # works

cumsumcols <- paste0(names(melted_d)[5:7], '_deficit_rolling')
melted_d[,yearmo := paste0(Year, variable)]
def <- melted_d[,lapply(.SD, function(x) c_ceiling(cumsum(x))), .SDcols = defcols, by=measure]

order_col <- unique(melted_d$yearmo)
def$yearmo <- rep(order_col, 3)

top_line <- dcast(melted_d, yearmo~measure, value.var = 'value')
# change bottom line for the various elements
bottom_line <- dcast(melted_d, yearmo~measure, value.var = 'lockdown_calc')

# first plot the top element
p1 <- ggplot()+
  geom_line(aes(x = factor(yearmo, levels = order_col), y=fit, colour = 'b', group = 1), data = top_line)+
  geom_ribbon(aes(x=factor(yearmo, levels = order_col), ymin = lwr, ymax = upr, group = 1), fill='red', alpha = 0.2, data = top_line)+
  geom_line(aes(x=factor(yearmo, levels = order_col), group = 1, colour = 'a',  y=fit), size = 1.4, data = bottom_line)+
  geom_ribbon(aes(x = factor(yearmo, levels = order_col), ymin=lwr, ymax=upr, group=1), fill = 'blue', lty=2, alpha=0.2, data = bottom_line)+
  scale_colour_manual(name = NULL, values = c('b'='red','a'='blue'), labels = c('COVID-19 surgical activity', 'Normal surgical activity')) +
  labs(x = NULL, y='Number of Procedures', fill = NULL)+
  scale_y_continuous(labels=function(n){format(n, scientific = F, big.mark = ',')}) +
  scale_x_discrete(labels = NULL)+
  theme_bw()+
  theme(legend.position = "top", legend.text = element_text(size = 12,face = 'bold'),axis.text = element_text(size=12,face = 'bold'), axis.title = element_text(size=12, face = 'bold'))

barplot_dat <- dcast(def, yearmo~measure, value.var = 'lockdown_deficit')

# three chars and yr
xlabs <- c('jan-20', 'feb-20', 'mar-20', 'apr-20', 'may-20', 'jun-20', 'jul-20', 'aug-20', 'sep-20', 'oct-20', 'nov-20', 'dec-20', 'jan-21', 'feb-21', 'mar-21', 'apr-21', 'may-21', 'jun-21')
  
p2 <- ggplot(barplot_dat)+
  geom_bar(aes(x=factor(yearmo, levels = order_col), y=fit), stat = 'identity', colour = 'orange', fill = 'orange')+
  geom_errorbar(aes(x=factor(yearmo, levels=order_col), ymin = lwr, ymax=upr, alpha = 0.2, colour = 'orange'))+
  theme_bw()+
  labs(x=NULL, y='Number of procedures', colour = NULL, fill=NULL)+
  scale_y_continuous(labels=function(n){format(n, scientific = F, big.mark = ',')})+
  scale_x_discrete(labels = xlabs)+
  theme(legend.position = "bottom", legend.text = element_text(size = 10,face = 'bold'),axis.text = element_text(size=12,face = 'bold'), axis.title = element_text(size=12, face = 'bold'))

pp1 = ggplotGrob(p1)
pp2 = ggplotGrob(p2)
grid::grid.draw(rbind(pp1,pp2, size='last'))
# figure 2

