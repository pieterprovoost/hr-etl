library(hagel)
library(aws.s3)
library(stringr)
library(geojsonsf)
library(glue)
library(readr)

readRenviron(".env")

res <- get_object("hagel.json", bucket = "pieterprovoost-hagel", as = "text")

if (str_detect(res, "The specified key does not exist")) {
  message("S3 data file not found")
  data <- list(images = list())
} else {
  data <- jsonlite::fromJSON(res)
}

min_after <- lubridate::now("UTC") - lubridate::minutes(30)
if (length(data$images) > 0) {
  message(glue("Found {length(data$images)} images in S3 data file"))
  date_string <- str_match(names(data$images)[1], "([0-9]+)(?:.h5)")[,2]
  after <- max(min_after, readr::parse_datetime(date_string, format = "%Y%m%d%H%M"))
} else {
  after <- min_after
}
message(glue("Setting after to {after}"))

# OVERRIDE
#data$images <- list()
#after <- readr::parse_datetime("202206042000", format = "%Y%m%d%H%M")

images <- radar_knmi("radar_hail_warning_5min", "1.0", after = after)
message(glue("Retrieved {length(images)} images from KNMI"))
geoms <- lapply(images, knmi_to_polygon)
json <- lapply(geoms, function(x) {
  if (is.null(x)) return(NULL)
  sfc_geojson(sf::st_transform(x, 4326))
})
data$images <- append(data$images, json)
data$images <- data$images[order(names(data$images), decreasing = TRUE)]
data$images <- head(data$images, 12)

data_output <- charToRaw(jsonlite::toJSON(data, auto_unbox = TRUE))
put_object(data_output, object = "hagel.json", bucket = "pieterprovoost-hagel", acl = "public-read")
