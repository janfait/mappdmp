# mappdmp

**This package is under development, all production use at your own risk**

Thin r wrapper for the Mapp DMP API with some convenience and analytical functions

## Author
Jan Fait, jan.fait@mapp.com,
Marketing Analytics at Mapp

## About
This is an unofficial project to make manipulation with raw data exports from the Mapp Data Management Platform somewhat easier for
analysts and data scientists. The early release will just wrap the API, later analytical functions will be added.

## Getting started


### Setup
Depending on the stage of development, you may be able to install the package 
```r
install_github('janfait/mappdmp')
library("mappdmp")
```
or simply source the main class dmpClass.R in the R folder which contains all the code.


### Initialize
All you need really are the login credentials. To understand what the package is doing in the background, you can initialize it 
with debug=T parameter. 
```r
my_dmp <- mappDmp$new("username","password",debug=T)
```
### Login
The login function is implicit. This means that whenever the package calls the Mapp DMP API, it will check the existing session and if the token is not present or expired, it will trigger the login() function. If however you want to test your login credentials, you can just do:
Authentication details you have supplied upon initialization are stored in the $username and $password fields and this is where the login() function gets them from
```r
my_dmp$login()
```

## Getting Data

The core function of the package is the getData() and getBatch() method.
You can supply your dimensions,measures,filters and limit to it just like you would in the JSON body of the MAPP DMP API request, except that you can choose between a list() or a single concatenated string. For each input, the package will supply defaults if omitted by user.

**The package automatically prepends the flx_ prefix to dimensions,meausures and filters if missing**

You can review and even redefine defaults like:

```r

defaults <- my_dmp$dictionary$dimensions$default
print(defaults)

#... do something with the defaults

my_dmp$dictionary$dimensions$default <- 'uuid,event_type'

```

Just like the Mapp DMP API, the package offers two ways to grab the raw data. The interface to both below methods is identical. The getBatch method offers additional parameters.

**The package automatically splits input parameters separated by comma if needed**

### 1. $getData() method


```r
#returns a json formatted response
f <-list(
  list(dimension="date",date_start="2016-12-01",date_end=as.chaer(Sys.Date())),
  list(dimension="pixel_id",includes="16234")
)
d <- 'flx_date,flx_event_type,flx_uuid'
m <- 'flx_interactions_dmp,flx_clicks_dmp'

my_data <- my_dmp$getData(dimensions=d,measures=m,filters=f)
```

The package offers a $data field for convenience and a $saveData() method which saves the slot content as .rds file to a specified destination:

```r
my_data$data$test <- dmp$getData(dimensions=d,measures=m,filters=f)

try(my_dmp$saveData(slot="test","/my_folder/test.rds"))

```

### 2. $getBatch() method

A request to the batch-export endpoint is much more complicated that the simple immediate export. The package submits the export request and continues to check its status by querying the viz/list-exports endpoint periodically until it has been completed.
You can specify the period of querying by the period (seconds) parameter.

This will request the export, wait for its execution and once ready, stream the content of the export into a temporary file in the current working directory which is deleted once the data has been loaded into a data frame.
If getContent = FALSE, the export is stored in the working directory under a name 'MappDmpExport_YOUREXPORTID.csv' and the function only returns this filename. 


```r
#requests the export and checks every 10 seconds whether it has been completed

f <-list(
  list(dimension="date",date_start="2016-12-01",date_end=as.chaer(Sys.Date())),
  list(dimension="pixel_id",includes="16234")
)
d <- 'flx_date,flx_event_type,flx_uuid'
m <- 'flx_interactions_dmp,flx_clicks_dmp'

my_data <- my_dmp$getBatch(dimensions=d,measures=m,filters=f,period=10,getContent=T)

```
Behind the scenes, the package executes calls to the $getExports() method which delivers a list of existing exports and their states

```r
exports <- my_dmp$getExports()

```

The export you have already generated can be retrieved by

```r
my_export_id <- 44800
my_data <- my_dmp$getExport(id=my_export_id,getContent=T)


```
### Analytical functions

The package has multiple analytical functions which make you more productive when working with the data

removeInvalidUsers

createSessionData

getUniqueUsers

getCustomEvents

applyFilter









