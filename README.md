raqdm
=====

Access data from EPA's Air Quality Data Mart in R

raqdm is an R package for directly accessing data from U.S. EPA's Air Quality Data Mart (AQDM). It uses the web interface described [here](http://www.epa.gov/airdata/toc.html) to query the service and returns the results as a `data.frame`. Data can be queried synchronously or asynchronously, default values can be saved across R sessions, and a simple GUI is available to make it easier to make requests.

### Installing

You can install from github with `devtools`:

```R
  devtools::install_github("ebailey78/raqdm", ref = "update")
```

### Getting Access
You will need a username and password from EPA to access the actual data. You can visit the [Air Quality Data Mart](http://www.epa.gov/airdata/tas_Data_Mart_Registration.html) for information on registering. It's free!

### GUI
![raqdm GUI](http://i.imgur.com/tDKILij.png)

Use the GUI to make requests, set defaults, or create custom function calls to use later.

### Setting Defaults
`setAQDMdefaults` is used to set default values for any of AQDM's query parameters:

```R
  setAQDMdefaults(user = "myemail@example.com", pw = "niftymint56", 
                  param = "44201", frmonly = TRUE)
```
In this example we set defaults for username, password, param, and frmonly. Any request you make can skip these parameters and raqdm will insert them for you and any time you open the GUI these parameters will be entered by default.

raqdm will also save these default values and reload them the next time you load the package, preventing you from having to reenter the same information over and over.

### Requesting data

#### Synchronous (rawData)
This option is currently disabled by EPA.

#### Asynchronous (rawDataNotify)
Use `getAQDMdata()` with `synchronous = FALSE` to make an asynchronous data request. This will return a `AQDMrequest` object. When the data is available, use `getAQDMrequest()` to retrieve the data from the server:

```R

  request <- getAQDMdata(bdate = "20140101", edate = "20140531", 
                         state = "18", county = "089")
  
  ## Once the request is processed on the server:
  
  data <- getAQDMrequest(request, stringsAsFactors = FALSE)

```

### To Do
* [ ] make sure the data.frames are being created correctly and clean them up a bit.
* [ ] Get synchronous pulls working when EPA reenables that functionality.
* [x] Create a GUI to make it easier to do pulls
* [ ] ...
