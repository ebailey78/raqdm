cache <- new.env()

buildDefault <- as.list(rep("", 18))
names(buildDefault) <- c("user", "pw", "format", "pc", "param", "bdate", "edate", "minlat",
                         "maxlat", "minlon", "maxlon", "state", "county", "site", "cbsa", 
                         "csa", "dur", "frmonly")
assign("default.params", buildDefault, envir = cache)
rm(buildDefault)

