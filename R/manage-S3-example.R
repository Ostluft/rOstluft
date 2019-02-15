library(aws.s3)

local_store <- "/Users/awel/Documents/Sync/AWEL/rOstluft/localstore"
setwd(local_store)


#' @ Thomas: das folgende setup würde ich eigentlich dann direkt den Nutzern, welche anfragen, schicken und nicht im package verankern
#'
#' setup R as AWS S3 client for rOstluft datastore
#' needs to be setup only once per R installation
#' apply at joerg.sintermann@bd.zh.ch in order to retrieve the 'accesskey', 'secretkey' and 'sessiontoken'
#' please keep the information confident
#' read-only access (unless you are called Jörg Sintermann or Thomas von Allmen)!

#' for 'Joerg' (an admin user IAM role with read/write access rights for bucket 'rostluft', created by Jörg for himself on his AWS)
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAIHC3BUHAIB3T5CHQ", "AWS_SECRET_ACCESS_KEY" = "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")

#' for 'Thomas' (an admin user IAM role with read/write access rights for bucket 'rostluft', created by Jörg for Thomas von Allmen on AWS)
# Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAILPWSP26OPA6REGA", "AWS_SECRET_ACCESS_KEY" = "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")

#' for 'rOstluftUser' (a user IAM role with read-only access rights for bucket 'rostluft', created by Jörg on AWS)
# Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAJJX3KAUVLIU34MTA", "AWS_SECRET_ACCESS_KEY" = "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")







#' test if you have setup a working rOstluft datastore AWS S3 interface
bucket_exists("rostluft/datastore")

#' check what's in there
get_bucket(bucket = "rostluft/datastore")

#' backup current datastore (only for admin with write access)
put_bucket(bucket = paste0("rostluft/backup/",format(Sys.Date(), "%Y%m%d")))
# copy_bucket(from_bucket = "rostluft/datastore", to_bucket = paste0("rostluft/backup/",format(Sys.Date(), "%Y%m%d")))

#' upload complete directory to S3 (only for admin with write access)
s3sync(files = dir(local_store, recursive = TRUE), bucket = "rostluft/datastore", direction = "upload", verbose = TRUE)

#' download complete directory to S3
s3sync(bucket = "rostluft/datastore", direction = "download", verbose = TRUE)

#' save a single *.RDS to datastore  (only for admin with write access)
a <- 1
s3saveRDS(a, "a.RDS", bucket = "rostluft/datastore")

#' read a single *.RDS to datastore
s3readRDS("a.RDS", bucket = "rostluft/datastore")


