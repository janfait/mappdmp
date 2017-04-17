# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

mappDmp <- setRefClass("mappDmp",
                       fields=list(
                         debug = "logical",
                         settings = "list",
                         root = "character",
                         endpoints = "list",
                         username = "character",
                         password = "character",
                         authentication = "list",
                         dictionary = "list",
                         cache = "list",
                         log="character"
                       ),
                       methods=list(
                         debugPrint = function(x){
                           if(.self$debug){
                             str(x)
                           }else{
                             invisible(.self$log <- paste(.self$log,print(x),"\n"))
                           }
                         },
                         loadPackages = function(){
                           .self$debugPrint("Loading packages")
                           if(!all(require("httr",quietly = T),require("jsonlite",quietly = T),require("urltools",quietly = T),require("lubridate",quietly = T))){
                             stop("The MappDmp class requires the following R packages: httr,lubridate,jsonlite,urltools")
                           }
                         },
                         validateFilters = function(input){

                           if(missing(input)){
                             .self$debugPrint("Running with defualt date filters")
                             output <- .self$dictionary$filters$default
                             return(output)
                           }

                           if(is.list(input)){
                             if("date" %in% unlist(input) & "date_start" %in% names(unlist(input)) & "date_end" %in% names(unlist(input))){
                               if(all(sapply(input,is.list)) & sum(sapply(input,is.list))==length(input)){
                                 output <- input
                               }else{
                                 stop("Filters must be a list of lists, some of the elements of the top node are not lists")
                               }
                             }else{
                               stop("When specified, filters must contain the 'date' dimension. Example: list(dimension='date',date_start='2017-01-01',date_end='2017-01-31'")
                             }
                           }else{
                             stop("filters must be a list of lists. Example: list(list(dimension='x' ...),list(dimension='y',....))")
                           }
                           return(output)
                         },
                         validateIdentifiers = function(input){

                           what <- deparse(substitute(input))
                           .self$debugPrint(paste("Validating",what))

                           if(missing(input)){
                             output <- .self$dictionary[[what]]$default
                             output <- unlist(strsplit(output,","))
                             output <- paste0("flx_",output)
                             .self$debugPrint(paste("Selected",what,":",paste(output,collapse=",")))
                           }else if(input=="*"){
                             output <- .self$dictionary[[what]]$all
                             output <- unlist(strsplit(output,","))
                             output <- paste0("flx_",output)
                           }else{
                             output <- .self$dictionary[[what]]$all
                             input <- unlist(strsplit(input,","))
                             output <- unlist(strsplit(output,","))
                             .self$debugPrint(paste("Removing the non-existent:",paste(input[!(input %in% output)],collapse=",")))
                             output <- input[input %in% output]
                             output <- paste0("flx_",output)
                             .self$debugPrint(paste("Selected are:",paste(output,collapse=",")))
                           }
                           return(output)
                         },
                         prepareQuery = function(dimensions,measures,filters,limit){
                           dimensions <- .self$validateIdentifiers(dimensions)
                           measures <- .self$validateIdentifiers(measures)
                           filters <- .self$validateFilters(filters)
                           .self$debugPrint(paste("Validation complete"))
                           .self$debugPrint(dimensions)
                           .self$debugPrint(measures)
                           if(missing(limit)){
                             limit <- .self$dictionary$limit$default
                           }
                           .self$debugPrint(paste("Preparing query"))
                           query <- list(dimensions=dimensions,measures=measures,filters=filters,limit=limit)
                           .self$cache$query <- list()
                           .self$cache$query$parsed <- query
                           .self$debugPrint(paste("Parsed query",query))
                           query <- jsonlite::toJSON(query,auto_unbox=T)
                           .self$cache$query$json <- query
                           query <- urltools::url_encode(query)
                           .self$cache$query$encoded <- query
                           query <- paste0("x=[",query,"]")
                           return(query)
                         },
                         postCall = function(url,body){
                           if(.self$checkLogin()){
                             url <- paste0(url,"?csrf=",.self$authentication$csrf)
                             .self$debugPrint(paste("Calling",url))
                             call <- POST(url,body=body,config(verbose=.self$debug))
                             callResult <- content(call,"parsed")
                             return(callResult)
                           }else{
                             stop("Authentication has failed")
                           }
                         },
                         getCall = function(url){
                           if(.self$checkLogin()){
                             .self$debugPrint(paste("Calling",url))
                             call <- GET(url,config(verbose=.self$debug))
                             callResult <- content(call,"parsed")
                             return(callResult)
                           }else{
                             stop("Authentication has failed")
                           }
                         },
                         login = function(){
                           authString <- paste0("username=",.self$username,"&password=",.self$password)
                           .self$debugPrint(paste("Authenticating with",authString))
                           callResult <- POST(url=.self$endpoints$auth,body=authString)
                           .self$authentication <- content(callResult,"parsed")
                           if(!.self$authentication$response$logged_in){
                             .self$debugPrint("Authentication failed")
                             .self$debugPrint(.self$authentication$response)
                             return(FALSE)
                           }else{
                             .self$debugPrint("Authentication successful")
                             return(TRUE)
                           }
                         },
                         checkLogin = function(){
                           now <- as.POSIXlt(Sys.time(), "GMT")
                           expiry <- as.POSIXlt(.self$authentication$debug$now,"GMT")+1800
                           .self$debugPrint(paste("Checking login at",now,", Expires at",expiry))
                           .self$debugPrint(paste("Token is valid:",now<=expiry))
                           if(now>=expiry){
                             .self$login()
                           }else{
                             return(TRUE)
                           }
                         },
                         getPixels = function(){
                           url <- paste0(.self$endpoints$trackinglist)
                           callContent <- .self$getCall(url)
                           if(callContent$response$status=="OK"){
                             data <- callContent$response$beacons
                             data$methods <- paste(unlist(data$permissions$methods),collapse=",")
                             data$permissions <- NULL
                             data <- as.data.frame(data)
                             return(data)
                           }else{
                             return(callContent)
                           }
                         },
                         getNames = function(dimension=character(),search=NULL){

                           if(substring(dimension,1,3)!='flx'){
                             dimension <- paste0("flx_",dimension)
                           }
                           url <- paste0(.self$endpoints$names,"?dimension=",dimension,ifelse(is.null(search),"",paste0("&search=",search)))
                           callContent <- .self$getCall(url)
                           if(callContent$response$status=="OK"){
                             data <- callContent$response$items
                             data <- as.data.frame(do.call("rbind",lapply(data,unlist)),stringsAsFactors=F)
                             return(data)
                           }else{
                             return(callContent)
                           }
                         },
                         makeUrlCategories = function(data,sitemap){

                           if("event_url_trunc" %in% colnames(data)){
                             if(!is.null(sitemap)){
                               .self$dictionary$sitemap <- read.csv(sitemap,header=T,stringsAsFactors = F,sep=";")
                               data <- merge(data,sitemap,by.x="event_url_trunc",by.y="url",all.x=T)
                             }else{
                               .self$debugPrint("Path to sitemap not provided")
                             }

                           }else{
                             print("Cannot categorize URL because the 'event_url' dimension is missing in the data")
                           }
                           return(data)

                         },
                         makeNewVars = function(data){
                           if(.self$settings$makeNewVars){
                             if("event_url" %in% colnames(data)){
                               data$event_url_trunc <- tolower(gsub("\\?(.*)$","",data$event_url))
                               data$event_url_trunc <- gsub("https://mapp.com|http://mapp.com","",data$event_url_trunc)
                               data$event_url_trunc <- gsub("/fr/|/de/|/it/|/us/|/uk/","/home/",data$event_url_trunc)
                               data$event_url_trunc <- gsub("//","/",data$event_url_trunc,fixed=T)
                               params <- urltools::param_get(data$event_url,c("utm_medium","utm_source","utm_campaign","uid"))
                               data <- cbind(data,params)
                               data$uid[nchar(data$uid)!=10] <- 0
                               data$uid <- as.numeric(data$uid)
                             }
                             if("interaction_timeonsite" %in% colnames(data)){
                               seconds <- c("null"=0,"5 seconds"=5,"10 seconds"=10,"20 seconds"=20,"30 seconds"=30,"1 minute"=60,"1.5 minutes"=90,"2 minutes"=120,"3 minutes"=180,"4 minutes"=240,"5 minutes"=300)
                               data$interaction_timeonsite_sec <- seconds[match(data[,"interaction_timeonsite"],names(seconds))]
                             }
                           }
                           return(data)
                         },
                         makeLabels = function(data){
                           if(.self$settings$makeLabels){
                             data[] <- lapply(colnames(data),function(col){
                               if(col %in% unlist(strsplit(.self$dictionary$dimensions$named,","))){
                                 names <-dmp$getNames(dimension=col)
                                 data[,col]<-names$name[match(data[,col],names$id)]
                               }else{
                                 data[,col]
                               }
                             })
                           }
                           return(data)
                         },
                         getDimensions = function(pattern=NULL){
                           if(is.null(pattern)){
                             dimensions <- unlist(strsplit(.self$dictionary$dimensions$all,","))
                           }else{
                             dimensions <- unlist(strsplit(.self$dictionary$dimensions$all[pattern],","))
                           }
                           return(dimensions)
                         },
                         getData = function(dimensions,measures,filters,limit,makeLabels=F,makeNewVars=F){
                           .self$settings$makeLabels <- makeLabels
                           .self$settings$makeNewVars <- makeNewVars
                           query <- .self$prepareQuery(dimensions,measures,filters,limit)
                           callContent<- .self$postCall(url=.self$endpoints$data,body=query)
                           if(callContent$response$status=="OK"){
                             data <- callContent$response$data[[1]]$data
                             data <- do.call("rbind",data)
                             data <- as.data.frame(data)
                             colnames(data)<-gsub("flx_","",colnames(data))
                             data[] <- apply(data,2,unlist)
                             data <- .self$makeLabels(data)
                             data <- .self$makeNewVars(data)
                             return(data)
                           }else{
                             return(callContent)
                           }
                         },
                         getExports = function(){
                           callContent <- .self$getCall(.self$endpoints$listexports)
                           if(callContent$response$status=="OK"){
                             data <- callContent$response$exports
                             return(data)
                           }else{
                             return(callContent)
                           }
                         },
                         getExport = function(id){
                           url <- paste0(.self$endpoints$export,"?id=",id)
                           call <- GET(url,config(verbose=.self$debug))
                           tmp<- tempfile()
                           try(writeBin(content(call,"raw"), tmp))
                           data<-read.csv(tmp,sep=",",header=T,stringsAsFactors=F)
                           rm(tmp)
                           colnames(data)<-gsub("flx_","",colnames(data))
                           data <- .self$makeLabels(data)
                           data <- .self$makeNewVars(data)
                           return(data)
                         },
                         getBatch = function(dimensions,measures,filters,period=10,attempts=20,makeLabels=F,makeNewVars=F){
                           .self$settings$makeLabels <- makeLabels
                           .self$settings$makeNewVars <- makeNewVars
                           query <- .self$prepareQuery(dimensions,measures,filters,limit=.self$dictionary$limit$maxvolume)
                           callContent<- .self$postCall(url=.self$endpoints$batch,body=query)
                           if(callContent$response$status=="OK"){
                             exportId <- callContent$response$id
                             exportReady <- F
                             attemptCounter <- 0
                             while(!exportReady){
                               Sys.sleep(period)
                               exports <- .self$getExports()
                               exportReady <- any(sapply(exports,function(export) ifelse(export$id==exportId & export$state=='COMPLETED',TRUE,FALSE)))
                               if(attemptCounter==attempts){
                                 stop("Was not able to retrieve the export after defined number of attempts")
                               }
                               .self$debugPrint(paste("Attempt number",attemptCounter))
                               attemptCounter <- attemptCounter+1
                             }
                             export <- .self$getExport(exportId)
                             return(export)
                           }else if(callContent$response$status=="ERROR" & grepl("This report has already",callContent$response$error)){
                             exportId <- callContent$response$id
                             export <- .self$getExport(exportId)
                             return(export)
                           }else{
                             return(callContent)
                           }
                         },
                         initialize = function(username,password,debug=FALSE){
                           #set parameters to fields
                           .self$settings <- list()
                           .self$settings$makeLabels <- F
                           .self$settings$makeNewVars <- F
                           .self$debug <- debug
                           .self$username <- username
                           .self$password <- password
                           .self$data <- list()
                           #set dictionary
                           .self$dictionary <- list(
                             dimensions = list(
                               all="advertiser_id,auction_id,browser,buyer_id,campaign_id,conversion_dmp,creative_id,creative_size,date,day_in_month,day_in_week,day_in_year,destination_url,device_brand,device_id_md5,device_id_sha1,device_id_openudid,device_id_odin,device_id_apple_ida,device_id_google_adid,device_type,event_referer_url,event_type,event_url,external_data,external_pixel_id,geo_city,geo_country,geo_lat,geo_long,geo_region,hour,insertion_order_id,interaction_type,interaction_value,lineitem_id,month_in_year,operating_system,pixel_id,placement_id,platform,platform_exchange,publisher_id,segment_dmp,seller_id,site_domain,site_id,site_type,timestamp,user_agent,user_ip,user_ip_truncated,uuid,week_in_year,clicks_dmp,impressions_dmp,interaction_adhover,interaction_pagescroll,interaction_timeonsite,interactions_dmp,pixel_loads_dmp,record_sum,total_events_dmp,unique_users_approx_dmp",
                               default="uuid,date,timestamp,pixel_id,event_type,segment_dmp,conversion_dmp,event_referer_url,event_url,interaction_type,interaction_pagescroll,interaction_timeonsite",
                               named="segment_dmp,conversion_dmp,interaction_type,event_type"
                             ),
                             measures = list(
                               all = "impressions_dmp,interactions_dmp,clicks_dmp",
                               default = "impressions_dmp,interactions_dmp,clicks_dmp,"
                             ),
                             filters = list(
                               default = as.data.frame(list(dimension = "date", date_start=Sys.Date()-30, date_end = Sys.Date()),stringsAsFactors = F)
                             ),
                             limit = list(
                               default = "1000",
                               maxvolume = "10000000"
                             )
                           )
                           .self$debugPrint("Initializing Mapp DMP instance")
                           invisible(.self$loadPackages())
                           .self$root <- "https://platform.flxone.com/api"
                           .self$endpoints <- list(
                             auth=paste0(.self$root,"/auth"),
                             api=paste0(.self$root,"/"),
                             data=paste0(.self$root,"/viz/data"),
                             batch=paste0(.self$root,"/viz/batch-export"),
                             listexports=paste0(.self$root,"/viz/list-exports"),
                             export=paste0(.self$root,"/viz/export"),
                             names=paste0(.self$root,"/report-central/search"),
                             trackinglist = paste0(.self$root,"/tracking/list")
                           )
                           .self$cache <- list()
                           if(!.self$login()){
                             stop("Authentication has failed, please check your credentials")
                           }
                         }
                       )
)
