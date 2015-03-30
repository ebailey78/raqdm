raqdm
=====

access data from EPA's Air Quality Datamart in R

This is mostly working, but perhaps not super user-friendly. I've written basic documentation but you will
probably need to know something about AQDM and air monitoring to find this package really useful.

EPA has shut down synchronous requests again so I cant get that working yet. But asynchronous seems to be working.

###To Do
* make sure the data.frames are being created correctly and clean them up a bit.
* Get synchronous pulls working when EPA reenables that functionality.
* ...

You can install with `devtools`:

```R
  devtools::install_github("ebailey78/raqdm", ref = "update")
```

You will need a username and password from EPA to access the actual data. You can visit the [Air Quality Data Mart](http://www.epa.gov/airdata/tas_Data_Mart_Registration.html) for information on registering. It's free!
