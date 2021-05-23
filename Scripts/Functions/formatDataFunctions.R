#Format dates function
format_dates = function(column){
  as.Date(column, format = "%Y-%m-%d")
}