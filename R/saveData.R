##' Function to create the data base on mongodb, not working yet
##'
##' @param data 3D array
##' @author Dimitri Fichou
##' @export

saveData <- function(data){
  print('data not saved, here is what you lose')
  print(dim(data))
}



# ## mongodb collection insertion
# # install.packages('mongolite')
# library(mongolite)
# ## Initialize
# options(mongodb = list(
#   "host" = "ds037155.mongolab.com:37155",
#   "username" = "dimitrifi",
#   "password" = "D148010"
# ))
# databaseName <- "tlc-brain"
# collectionName <- "TLC-brain"
#
#
#
# saveData <- function(data) {
#   # Connect to the database
#   db <- mongo(collection = collectionName,
#               url = sprintf(
#                 "mongodb://%s:%s@%s/%s",
#                 options()$mongodb$username,
#                 options()$mongodb$password,
#                 options()$mongodb$host,
#                 databaseName))
#   # deconstruct the data as chromato
#   data <- deconstruct(data,margin=2,transform = T) %>%
#     apply(c(1,2),function(x){as.integer(256*x)}) %>%
#     as.data.frame
#   print(dim(data))
#   # Insert the data into the mongo collection as a data.frame
#   db$insert(data)
#   rm(db)
#   gc()
# }
#
# ## To test, work but need to reduce size in order to not overload the DB but hey,
# ## So let's go for 256*512, return an error in case of less pixel.
# ## 512*256: 8Mb if as.numeric
# ## 512*256: 4Mb if as.integer go for it
# # data <- f.read.image('www/rTLC_demopicture.JPG',format='jpeg',native=F,height=256)
# # data <- data[,seq(1,dim(data)[2],length.out=512),]
# # saveData(data)
#
# ## install.packages('RMongo') : Not working yet, not really happy with the mongolite package as it only upload dataframe
# # library(RMongo)
# # mg1 <- mongoDbConnect('db',host='192.168.1.24',port=8012)
# # print(dbShowCollections(mg1))
# # query <- dbGetQuery(mg1, 'test', "{'AGE': {'$lt': 10}, 'LIQ': {'$gte': 0.1}, 'IND5A': {'$ne': 1}}")
# # data1 <- query
# # summary(data1)
#
# # apply_by_pages <- function(x, FUN, pagesize, verbose, ...){
# #   stopifnot(is.data.frame(x))
# #   nr <- nrow(x)
# #   npages <- nr %/% pagesize;
# #   lastpage <- nr %% pagesize;
# #
# #   for(i in seq_len(npages)){
# #     from <- pagesize * (i-1) + 1;
# #     to <- pagesize * i
# #     FUN(x[from:to, ,drop = FALSE], ...)
# #     if(verbose) cat("\rProcessed", i * pagesize, "rows...")
# #   }
# #
# #   if(lastpage){
# #     from <- nr - lastpage + 1;
# #     FUN(x[from:nr, ,drop = FALSE], ...)
# #   }
# #   if(verbose) cat("\rComplete! Processed total of", nr, "rows.\n")
# #   invisible();
# # }
# #
# # mongo_stream_out <- function(data, mongo, pagesize = 1000, verbose = TRUE){
# #   stopifnot(is.data.frame(data))
# #   stopifnot(is.numeric(pagesize))
# #   stopifnot(is.logical(verbose))
# #   FUN <- function(x){
# #     mongo_collection_insert_page(mongo, jsonlite:::asJSON(x, digits = 9,
# #                                                           POSIXt = "mongo", raw = "mongo", collapse = FALSE))
# #   }
# #   jsonlite:::apply_by_pages(data, FUN, pagesize = pagesize, verbose = verbose)
# #   TRUE
# # }
# #
# # mongo_collection_insert_page <- function(col, json, stop_on_error = TRUE){
# #   .Call(R_mongo_collection_insert_page, col, json, stop_on_error)
# # }
