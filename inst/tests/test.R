setAQDMdefaults(user = "ebailey@idem.in.gov", pw = "bolefox64", param = 44201, state = "18", county = "89", bdate=as.Date('2013-01-01'), edate='20131231')

x <- getAQDMdata(format = "AQS")
y <- getAQDMrequest(x, stringsAsFactors = FALSE)
