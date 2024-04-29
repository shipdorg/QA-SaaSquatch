library(rstudioapi)
library(sf)
library(DBI)
library(RPostgres)
library(config)

connectDB <- function(wd){
  setwd(wd)
  print(getwd())
  
  # Get credentials
  dw <- config::get("db")
  tryCatch({
    #drv <- dbDriver("PostgreSQL")
    print("Connecting to Database...")
    con <- dbConnect(RPostgres::Postgres(),
                     host = dw$server,
                     port = dw$port,
                     dbname = dw$database,
                     user = dw$uid,
                     password = dw$pwd
    )
    print("Database Connected!")
    return(con)
  },
  error=function(cond) {
    print("Unable to connect to Database.")
    return(cond)
  })
}

# email="will@orbica.world"
# dbConnection <- connectDB()
# if (exists("dbConnection") && !inherits(dbConnection, "try-error")) {
#   sqlQuery <- paste("SELECT id, aoi_label, ST_Transform(aoi_polygon_4326, 27700) as geometry FROM public.processing_queue
#     WHERE user_email ='", email, "' and processing_status = 'queued'
#     ORDER BY upload_time ASC
#     LIMIT 1", sep="")
#   Options <- st_read(dbConnection, query=sqlQuery)
#   print(st_crs(Options)) # SRID resolved by the database, not by GDAL!
#   plot(Options$geometry)
#   dbDisconnect(dbConnection)
# }
