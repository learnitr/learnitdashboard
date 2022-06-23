# 
# Global script executed once at the launch of the app
# 

# --- Global vars ---
acad_year <- "2021-2022"

# URL to access databases
sdd_url <- "mongodb://127.0.0.1:27017/sdd"

# --- Connecting to the tables ---
sdd_events <- try(mongolite::mongo("events", url = sdd_url), silent = TRUE)

sdd_users2 <- try(mongolite::mongo("users2", url = sdd_url), silent = TRUE)
sdd_teams <- try(mongolite::mongo("teams", url = sdd_url), silent = TRUE)
sdd_planning <- try(mongolite::mongo("planning", url = sdd_url), silent = TRUE)

sdd_courses <- try(mongolite::mongo("courses", url = sdd_url), silent = TRUE)
sdd_modules <- try(mongolite::mongo("modules", url = sdd_url), silent = TRUE)
sdd_apps <- try(mongolite::mongo("apps", url = sdd_url), silent = TRUE)

# --- Initial loading of Courses ---
courses_init <- try(sdd_courses$find('{}'), silent = TRUE)
# --- Initial loading of Modules ---
modules_init <- try(sdd_modules$find('{}'), silent = TRUE)
# --- Initial loading of Apps ---
apps_init <- try(sdd_apps$find('{}'), silent = TRUE)
# --- Initial loading of Planning ---
planning_init <- try(sdd_planning$find('{}'), silent = TRUE)
# --- Initial loading of Users2 ---
users2_init <- try(sdd_users2$find('{}'), silent = TRUE)

# Execution
thematic::thematic_shiny(font = NA, qualitative = c("#00c0ef","#c40c42"))
