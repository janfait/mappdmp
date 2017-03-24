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
                         root = "character",
                         endpoints = "list",
                         username = "character",
                         password = "character",
                         authentication = "list",
                         dictionary = "list",
                         cache = "list"
                       ),
                       methods=list(
                         debugPrint = function(x){
                           if(.self$debug){
                             str(x)
                           }
                         },
                         loadPackages = function(){
                           .self$debugPrint("Loading packages")
                           if(!all(require("httr"),require("jsonlite"),require("lubridate"))){
                             stop("The MappDmp class requires the following packages: httr,lubridate,jsonlite")
                           }
                         },
                         validateInput = function(input){
                           .self$debugPrint(paste("Validating",deparse(substitute(input))))
                           if(missing(input)){
                             output <- .self$dictionary[[deparse(substitute(input))]]$default
                             output <- unlist(strsplit(output,","))
                             output <- paste0("flx_",output)
                             .self$debugPrint(paste("Selected",deparse(substitute(input)),":",paste(output,collapse=",")))
                           }else{
                             output <- .self$dictionary[[deparse(substitute(input))]]$all
                             input <- unlist(strsplit(input,","))
                             output <- unlist(strsplit(output,","))
                             output <- input[input %in% output]
                             output <- paste0("flx_",output)
                             .self$debugPrint(paste("Selected are:",paste(output,collapse=",")))
                           }
                           return(output)
                         },
                         prepareQuery = function(dimensions,measures,filters,limit){
                           dimensions <- .self$validateInput(dimensions)
                           measures <- .self$validateInput(measures)
                           .self$debugPrint(paste("Validation complete"))
                           .self$debugPrint(dimensions)
                           .self$debugPrint(measures)
                           if(missing(limit)){
                             limit <- .self$dictionary$limit$default
                           }
                           if(missing(filters)){
                             filters <- .self$dictionary$filters$default
                           }else{
                             filters <- as.data.frame(
                               list(
                                 dimension = "date", date_start=Sys.Date()-30, date_end = Sys.Date(),
                                 dimension = "pixel_id"
                               ),stringsAsFactors = F)
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
                         getData = function(dimensions,measures,filters,limit){
                           query <- .self$prepareQuery(dimensions,measures,filters,limit)
                           callContent<- .self$postCall(url=.self$endpoints$data,body=query)
                           if(callContent$response$status=="OK"){
                             data <- callContent$response$data[[1]]$data
                             data <- do.call("rbind",data)
                             data <- as.data.frame(data)
                             colnames(data)<-gsub("flx_","",colnames(data))
                             data[] <- apply(data,2,unlist)
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
                           writeBin(content(call,"raw"), tmp)
                           data<-read.csv(tmp,sep=",",header=T,stringsAsFactors=F)
                           rm(tmp)
                           colnames(data)<-gsub("flx_","",colnames(data))
                           return(data)
                         },
                         getBatch = function(dimensions,measures,filters,period=10){
                           query <- .self$prepareQuery(dimensions,measures,filters,limit=.self$dictionary$limit$maxvolume)
                           callContent<- .self$postCall(url=.self$endpoints$batch,body=query)
                           if(callContent$response$status=="OK"){
                             exportId <- callContent$response$id
                             exportReady <- F
                             while(!exportReady){
                               Sys.sleep(period)
                               exports <- .self$getExports()
                               exportReady <- any(sapply(exports,function(export) ifelse(export$id==exportId & export$state=='COMPLETED',TRUE,FALSE)))
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
                           .self$debug <- debug
                           .self$username <- username
                           .self$password <- password
                           #set dictionary
                           .self$dictionary <- list(
                             dimensions = list(
                               all="advertiser_id,auction_id,browser,buyer_id,campaign_id,conversion_dmp,creative_id,creative_size,date,day_in_month,day_in_week,day_in_year,destination_url,device_brand,device_id_md5,device_id_sha1,device_id_openudid,device_id_odin,device_id_apple_ida,device_id_google_adid,device_type,event_referer_url,event_type,event_url,external_data,external_pixel_id,geo_city,geo_country,geo_lat,geo_long,geo_region,hour,insertion_order_id,interaction_type,interaction_value,lineitem_id,month_in_year,operating_system,pixel_id,placement_id,platform,platform_exchange,publisher_id,segment_dmp,seller_id,site_domain,site_id,site_type,timestamp,user_agent,user_ip,user_ip_truncated,uuid,week_in_year,clicks_dmp,impressions_dmp,interaction_adhover,interaction_pagescroll,interaction_timeonsite,interactions_dmp,pixel_loads_dmp,record_sum,total_events_dmp,unique_users_approx_dmp",
                               default="uuid,date,pixel_id,event_type,event_referer_url,event_url,interaction_type,interaction_pagescroll,interaction_timeonsite"
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
                             ),
                             interactions = c(),
                             events = c()
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
                             export=paste0(.self$root,"/viz/export")
                           )
                           .self$cache <- list()
                           if(!.self$login()){
                             stop("Authentication has failed, please check your credentials")
                           }
                         }
                       )
)

