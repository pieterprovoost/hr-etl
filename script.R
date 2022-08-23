library(hagel)
library(aws.s3)
library(stringr)
library(geojsonsf)
library(glue)
library(readr)
library(jsonlite)
library(dplyr)

readRenviron(".env")

# radar images

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
geoms <- lapply(images, knmi_to_polygon, threshold = 127)
print(geoms)
json <- lapply(geoms, function(x) {
  if (is.null(x)) return(NULL)
  sf_geojson(sf::st_transform(x, 4326), digits = 6)
})
data$images <- append(data$images, json)
data$images <- data$images[order(names(data$images), decreasing = TRUE)]
data$images <- head(data$images, 12)

# alerts

tryCatch(
  {
    start <- paste0(lubridate::format_ISO8601(lubridate::now("UTC") - lubridate::minutes(60)), ".000Z", sep = "")
    url <- glue::glue("https://hub.meteoalarm.org/api/v1/stream-buffers/all-warnings/warnings?startDate={start}&language=en&include_geocodes=1&exclude_severity_minor=1")
    res <- fromJSON(url, simplifyDataFrame = FALSE, simplifyVector = FALSE)
    alerts <- purrr::map(res$warnings, function(warning) {
      regions <- unlist(warning$regions) %>% paste0(collapse = ",")
      info <- purrr::pluck(warning, "alert", "info") %>%
        purrr::map(~ .[intersect(names(.), c("certainty", "description", "headline", "language"))]) %>%
        bind_rows() %>%
        filter(language == "en-GB")
      info$regions <-  regions
      return(info)
    }) %>%
      bind_rows() %>%
      filter(str_detect(tolower(headline), "hail") | str_detect(tolower(description), "hail")) %>%
      filter(str_detect(regions, "NL") | str_detect(regions, "BE"))
    message(glue("found {nrow(alerts)} alerts"))
    data$alerts <- alerts
  },
  error = function(cond) {},
  warning = function(cond) {},
  finally = {}
)

# output

data_output <- charToRaw(jsonlite::toJSON(data, auto_unbox = TRUE, null = "null", na = "null"))
put_object(data_output, object = "hagel.json", bucket = "pieterprovoost-hagel", acl = "public-read")
