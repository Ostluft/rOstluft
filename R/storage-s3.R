#' https://github.com/cloudyr/aws.signature/ options to authenticate with aws
#'
#'
#'
#' additional constructor options: bucket_name, region, prefix

s3_buckets_exists <- function() {
  aws.s3::bucket_exists("rostluft", region="eu-central-1")
}

s3_bucket_content <- function() {
  aws.s3::get_bucket("rostluft", region="eu-central-1")
}


s3_bucket_content_datastore <- function() {
  aws.s3::get_bucket("rostluft", region="eu-central-1", prefix = "datastore")
}
