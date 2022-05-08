HOST <- "20.25.61.17"

system2("scp",
  c(
    sprintf(
      "root@%s:/root/docker-compose-shiny-example/log/access.log",
      HOST
    ),
    "access.log"
  )
)


x <- lapply(
  readLines("access.log"),
  jsonlite::fromJSON)

## timestamp
ts <- as.POSIXct(sapply(x, "[[", "ts"), origin = "1970-01-01")
## URI
uri <- sapply(x, function(z) z$request$uri)

## find bare URIs
s <- endsWith(req_uri, "/") & !endsWith(req_uri, "/websocket/")

table(req_uri[s], as.Date(ts[s]))
