# Global script executed once at the launch of the app

# Code needed to initialize global variables ------------------------------

# We consider each academic year (Belgian term for university year) to start in
# a given date (provided by start.month and start.day, by default, on September
# 1st) and to end the day before that stating date
get_acad_year <- function(date = Sys.Date(), start.month = 9, start.day = 1) {
  datelt <- as.POSIXlt(date)
  year <- 1900 + datelt$year
  month <- 1 + datelt$mon
  day <- datelt$mday
  start_datelt <- datelt
  start_datelt$mon <- start.month - 1
  start_datelt$mday <- start.day
  if (datelt < start_datelt) {
    paste(year - 1, year, sep = "-")
  } else {
    paste(year, year + 1, sep = "-")
  }
}


# Global variables --------------------------------------------------------

acad_year     <- Sys.getenv("ACAD_YEAR", get_acad_year())
sdd_url       <- Sys.getenv("COURSE_URL", "mongodb://127.0.0.1:27017/sdd")


# Connection to the MongoDB server ----------------------------------------

sdd_events    <- try(mongolite::mongo("events", url = sdd_url), silent = TRUE)
sdd_users2    <- try(mongolite::mongo("users2", url = sdd_url), silent = TRUE)
sdd_teams     <- try(mongolite::mongo("teams", url = sdd_url), silent = TRUE)
sdd_planning  <- try(mongolite::mongo("planning", url = sdd_url), silent = TRUE)
sdd_courses   <- try(mongolite::mongo("courses", url = sdd_url), silent = TRUE)
sdd_modules   <- try(mongolite::mongo("modules", url = sdd_url), silent = TRUE)
sdd_apps      <- try(mongolite::mongo("apps", url = sdd_url), silent = TRUE)

courses_init  <- try(sdd_courses$find('{}'), silent = TRUE)
modules_init  <- try(sdd_modules$find('{}'), silent = TRUE)
apps_init     <- try(sdd_apps$find('{}'), silent = TRUE)
planning_init <- try(sdd_planning$find('{}'), silent = TRUE)
users2_init   <- try(sdd_users2$find('{}'), silent = TRUE)


# Configuration of the Shiny app ------------------------------------------

thematic::thematic_shiny(font = NA, qualitative = c("#00c0ef","#c40c42"))
