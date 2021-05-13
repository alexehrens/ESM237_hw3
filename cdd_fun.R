### function for calculating CDD from Hamlet et al. (2010)

# CDD = cooling degree days
# tmax = daily maximum temperature
# tmin = daily minimum temperature
# A/C_Pen = the estimated total residential air conditioning market penetration (i.e. the fraction of the population that has access to either central or window air conditioning) as a function of CDD

Hamlet <- function(clim_data, pop){
  
  # calculate CDD and HDD using Hamlet equations
  daily <- clim_data %>% 
    mutate(year = as.numeric(format(date,'%Y'))) %>% 
    mutate(daily_cdd = ((tmax+tmin)/2)-23.89) %>% 
    mutate(daily_hdd = 18.33-((tmax+tmin)/2))
  
  # take maximum value between calculated CDD/HDD and 0 (as in set negative values to 0)
  daily <- daily %>% 
    mutate(daily_cdd = case_when(
      daily_cdd > 0 ~ daily_cdd,
      daily_cdd < 0 ~ 0
    )) %>% 
    mutate(daily_hdd = case_when(
      daily_hdd > 0 ~ daily_hdd,
      daily_hdd < 0 ~ 0
    ))
  
  # aggregate daily CDD/HDD values by year
  annual <- daily %>% 
    group_by(year) %>% 
    summarize(cdd = sum(daily_cdd),
              hdd = sum(daily_hdd))
  
  # calculates A/C_Pen based on calculated CDD - Hamlet et al. sets minmimum value of A/C_pen to 0.08
  ac_pen <- annual %>% 
    mutate(ac_pen = 0.944-(1.17*exp(-0.00298*cdd))) %>% 
    mutate(ac_pen = case_when(
      ac_pen > 0.08 ~ ac_pen,
      ac_pen < 0.08 ~ 0.08 ### MOST A/C PEN VALUES NEGATIVE --> ALMOST EVERYTHING ENDS UP AS 0.08
    ))
  
  # calculates HEDI and CEDI using population input
  results <- ac_pen %>% 
    mutate(CEDI = ac_pen*pop*cdd) %>% 
    mutate(HEDI = pop*hdd)
  
  return(results)
}