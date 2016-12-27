map_contours <- function(data_trunc, alp) {
  p1 = ggmap(map, extent='device') + 
    geom_point(data=data_trunc, aes(x=X, y=Y), alpha= alp) + 
    stat_density2d(aes(x = X, y = Y,
                       fill = ..level.., alpha = ..level..),
                   size = 0.1, data = data_trunc, n=100,
                   geom = "polygon") +
    theme(legend.position="none")
  return(p1)
}

plot_marginals <- function(data_trunc) {
  p2 = ggplot(data=data_trunc, aes(x=X, y=Y), alpha=0.1)+
    geom_point()
  p2 = ggMarginal(p2 + theme_gray(), type = "histogram",
                  fill = "steelblue", col = "darkblue")
  return(p2)
  
}

# functions to make variables from date info.

make_vars_date <- function(crime_df) {
  crime_df$Years = strftime(strptime(crime_df$Dates,
                                     "%Y-%m-%d %H:%M:%S"),"%Y")
  crime_df$Month = strftime(strptime(crime_df$Dates,
                                     "%Y-%m-%d %H:%M:%S"),"%m")
  crime_df$DayOfMonth = strftime(strptime(crime_df$Dates,
                                          "%Y-%m-%d %H:%M:%S"),"%d")
  crime_df$Hour = strftime(strptime(crime_df$Dates,
                                    "%Y-%m-%d %H:%M:%S"),"%H")
  crime_df$YearsMo = paste( crime_df$Years, crime_df$Month , 
                            sep = "-" )
  
  
  
  crime_df$DayOfWeek = factor(crime_df$DayOfWeek,
                              levels=c("Monday","Tuesday",
                                       "Wednesday","Thursday",
                                       "Friday","Saturday","Sunday"),
                              ordered=TRUE)
  
  
  crime_df$weekday = "Weekday"
  crime_df$weekday[crime_df$DayOfWeek== "Saturday" | 
                     crime_df$DayOfWeek== "Sunday" | 
                     crime_df$DayOfWeek== "Friday" ] = "Weekend"
  
  
  addr_spl = strsplit(as.character(crime_df$Address),"/")
  crime_df$AddressType = "Non-Intersection"
  ind_l = vector()
  ind_inxn = sapply(1:dim(crime_df)[1], 
                    function(x) length(addr_spl[[x]]) == 2)
  crime_df$AddressType[ ind_inxn ]="Intersection"
  
  
  
  return(crime_df)
}
