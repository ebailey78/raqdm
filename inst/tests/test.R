setAQDMdefaults(user = "ebailey@idem.in.gov", pw = "bolefox64", param = "44201", state = "18", county = "089", bdate='20130101', edate='20131231')

x <- getAQDMdata(format = "AQS")
y <- getAQDMrequest(x, stringsAsFactors = FALSE)
