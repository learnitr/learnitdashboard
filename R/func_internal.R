# Internal code not to be exported in the package -------------------------

# TODO: change the default
sdd_url <- Sys.getenv("COURSE_URL", "mongodb://127.0.0.1:27017/sdd")

# TODO: get this form a table in the database instead (should work in a
# different context!)
# Get a list of all user logins in a given class
class_logins <- function(class, url = sdd_url, as.json = FALSE) {
  query <- switch(class,
    sdd1m = '{ "icflag": { "$in": ["S-BIOG-006", "S-BIOG-006,S-BIOG-015", "S-BIOG-027", "S-BIOG-027,S-BIOG-061"] }, "enrolled": "yes" }',
    sdd1c = '{ "icourse": "S-BIOG-921", "enrolled": "yes" }',
    sdd2m = '{ "icflag": { "$in": ["S-BIOG-015", "S-BIOG-006,S-BIOG-015", "S-BIOG-061", "S-BIOG-027,S-BIOG-061"] }, "enrolled": "yes" }',
    sdd2c = '{ "icourse": "S-BIOG-937-958-959", "enrolled": "yes" }',
    sdd3m = '{ "icourse": "S-BIOG-025", "enrolled": "yes" }',
    sdd4m = '{ "icourse": "S-BIOG-043", "enrolled": "yes" }',
    sdd5m = '{ "icourse": "S-BIOG-077", "enrolled": "yes" }',
    stop("Only 'sdd1m/c', 'sdd2m/c', 'sdd3m', 'sdd4m', or 'sdd5m' are recognized classes"))
  mdb <- try(mongolite::mongo("users", url = url), silent = TRUE)
  if (inherits(mdb, "try-error"))
    stop("Error: impossible to connect to the users database")
  res <- mdb$find(query,
    '{ "user_login" : true, "iemail" : true, "_id" : false }')
  
  email <- res$iemail
  res <- res$user_login
  
  # Do we output the json string to be used in MongoDB directly?
  if (isTRUE(as.json)) {
    res2 <- paste(res, collapse = '", "')
    res <- structure(paste0('{ "$in": ["', res2, '"] }'),
      logins = res, n = length(res))
  } else {
    res <- structure(res, email = email)
  }
  res
}

# TODO: get this form a table in the database instead (should work in a
# different context!)
# Get a selection of the apps that match a given aa
# (optionally restrict to a single module)
# tt <- paste0("A", formatC(1:12, width = 2, flag = "0")); tt
# grepl("^A0[1-6]", tt)
# grepl("^A(0[7-9])|(1[0-2])", tt)
class_aa <- function(aa, url = sdd_url, as.json = FALSE, module = NULL) {
  # This should be extracted from the database instead!
  # 2021-2022: we also exclude XnnA that correspond to H5P exercices in
  if (!is.null(module)) {
    # We only look for exercises in a specific module
    rx <- switch(aa,
      sdd1mq1 = ,
      sdd1cq1 = ,
      sdd1mq2 = ,
      sdd1cq2 = ,
      sdd1mq3 = ,
      sdd1cq3 = '^A',
      sdd2mq1 = ,
      sdd2cq1 = ,
      sdd2mq2 = ,
      sdd2cq2 = ,
      #sdd2cq3 = ,
      sdd2mq3 = ,
      sdd2cq3 = '^B',
      sdd3mq1 = ,
      sdd3mq3 = '^C',
      sdd4mq1 = '^D',
      sdd5mq1 = '^E',
      stop("Not implemented for this aa"))
    rx <- paste0(rx, module, '[^A]')
    if (isTRUE(as.json))
      rx <- paste0('{ "$regex": "', rx, '", "$options": "" }')
    return(rx)
  }
  
  # prerequisite sections and should not be counted here
  if (isTRUE(as.json)) {
    switch(aa,
      sdd1mq1 = ,
      sdd1cq1 = '{ "$regex": "^A0[1-6][^A]", "$options": "" }',
      sdd1mq2 = ,
      sdd1cq2 = '{ "$regex": "^A(0[7-9])|(1[0-2])[^A]", "$options": "" }',
      sdd1mq3 = ,
      sdd1cq3 = '{ "$regex": "^A(0[1-9])|(1[0-2])[^A]", "$options": "" }',
      sdd2mq1 = ,
      sdd2cq1 = '{ "$regex": "^B0[1-4][^A]", "$options": "" }',
      sdd2mq2 = ,
      sdd2cq2 = '{ "$regex": "^B0[5-8][^A]", "$options": "" }',
      #sdd2cq3 = '{ "$regex": "^B(09)|(1[0-2])[^A]", "$options": "" }',
      sdd2mq3 = ,
      sdd2cq3 = '{ "$regex": "^B0[1-8][^A]", "$options": "" }',
      sdd3mq1 = '{ "$regex": "^C0[1-6][^A]", "$options": "" }',
      sdd3mq3 = '{ "$regex": "^C0[1-6][^A]", "$options": "" }',
      sdd4mq1 = '{ "$regex": "^D0[1-2][^A]", "$options": "" }',
      sdd5mq1 = '{ "$regex": "^E0[1-4][^A]", "$options": "" }',
      stop("Not implemented for this aa"))
  } else {
    switch(aa,
      sdd1mq1 = ,
      sdd1cq1 = "^A0[1-6][^A]",
      sdd1mq2 = ,
      sdd1cq2 = "^A(0[7-9])|(1[0-2])[^A]",
      sdd1mq3 = ,
      sdd1cq3 = "^A(0[1-9])|(1[0-2])[^A]",
      sdd2mq1 = ,
      sdd2cq1 = "^B0[1-4][^A]",
      sdd2mq2 = ,
      sdd2cq2 = "^B0[5-8][^A]",
      sdd2mq3 = ,
      sdd2cq3 = "^B0[1-8][^A]",
      sdd3mq1 = "^C0[1-6][^A]",
      sdd3mq3 = "^C0[1-6][^A]",
      sdd4mq1 = "^D0[1-2][^A]",
      sdd5mq1 = "^E0[1-4][^A]",
      # Would be more difficult for sdd1mq2!
      stop("Not implemented for this aa"))
  }
}

# A general function to extract data from a collection
get_data <- function(collection, query, fields = '{}', url = sdd_url,
    count = FALSE) {
  mdb <- try(mongolite::mongo(collection, url = url), silent = TRUE)
  if (inherits(mdb, "try-error")) {
    stop("Error: impossible to connect to the database")
  } else {
    if (isTRUE(count)) {
      res <- mdb$count(query)
    } else {
      res <- mdb$find(query, fields = fields)
    }
  }
  mdb$disconnect()
  res
}

# Get Shiny apps data for a whole class
shiny_class_prog <- function(class_logins, class_aa, url = sdd_url) {
  mdb_shiny <- try(mongolite::mongo("shiny", url = url), silent = TRUE)
  if (inherits(mdb_shiny, "try-error"))
    stop("Error: impossible to connect to the shiny database")
  
  # Not used for now, but kept for future use...
  #"progress_avg": { "$avg": {
  #  "$cond": [ { "$in": ["$verb", ["evaluated"] ] }, "$max", null ]
  #} },
  
  if (!mdb_shiny$count(paste0('{ "login": ', class_logins, ',
    "app": ', class_aa, ' }')))
    return(NULL)
  
  mdb_shiny$aggregate(paste0('[ {
  "$match": {
    "login": ', class_logins, ',
    "app": ', class_aa, '
  }
}, {
  "$group": {
    "_id": "$app",
    "count": { "$sum": 1 },
    "raw_score_max": { "$max": "$score" },
    "raw_score_avg": { "$avg": "$score" },
    "max": { "$max": "$max" }
  }
}, {
  "$project": {
    "app": "$_id",
    "count_all": "$count",
    "count_avg": { "$divide": ["$count", ', attr(class_logins, "n"), '] },
    "progress_max": "$max",
    "raw_score_max": "$raw_score_max",
    "raw_score_avg": "$raw_score_avg",
    "max": "$max",
    "_id": false
  }
} ]'))
}

shiny_user_prog <- function(user_login, class_aa, url = sdd_url) {
  mdb_shiny <- try(mongolite::mongo("shiny", url = url), silent = TRUE)
  if (inherits(mdb_shiny, "try-error"))
    stop("Error: impossible to connect to the shiny database")
  
  if (!mdb_shiny$count(paste0('{ "login": "', user_login, '",
    "app": ', class_aa, ' }')))
    return(NULL)
  
  mdb_shiny$aggregate(paste0('[ {
  "$match": {
    "login": "', user_login, '",
    "app": ', class_aa, '
  }
}, {
  "$group": {
    "_id": "$app",
    "count": { "$sum": 1 },
    "progress": { "$max": "$max" },
    "raw_score": { "$max": "$score" }
  }
}, {
  "$project": {
    "app": "$_id",
    "id": "",
    "items": "$_id",
    "items_done": {
      "$cond": [ { "$gt": ["$progress", 0] }, ["$_id"], [] ]
    },
    "count": "$count",
    "progress": "$progress",
    "raw_score": { "$ifNull": [ "$raw_score", 0] },
    "_id": false
  }
} ]'))
}

shiny_prog <- function(user_login, class_logins, class_aa, class_data = NULL,
    url = sdd_url) {
  if (is.null(class_data)) {
    class_data <- shiny_class_prog(class_logins, class_aa, url = url)
  }
  if (is.null(class_data))
    return(NULL) # No Shiny data for this course
  user_data <- shiny_user_prog(user_login, class_aa, url = url)
  if (is.null(user_data)) {
    n <- nrow(class_data)
    user_data <- data.frame(app = class_data$app, id = rep("", n),
      items = class_data$app, items_done = rep("", n), count = rep(0, n),
      progress = rep(0, n), raw_score = rep(0, n))
  }
  
  # Merge statistics for user and class by app
  res <- suppressMessages(dplyr::full_join(class_data, user_data))
  res <- dplyr::select(res, "app", "id", "items", "items_done", "max",
    "progress_max", "progress", "raw_score_max", "raw_score_avg", "raw_score",
    "count_all", "count_avg", "count")
  # mutate(res, activity = pmin(1, count / count_avg) * pmax(max, na.rm = TRUE))
  res$activity <- pmin(1, res$count / res$count_avg) *
    pmax(res$max, na.rm = TRUE)
}

# Get Learnr data for a whole class
learnr_class_prog <- function(class_logins, class_aa, url = sdd_url) {
  mdb_learnr <- try(mongolite::mongo("learnr", url = url), silent = TRUE)
  if (inherits(mdb_learnr, "try-error"))
    stop("Error: impossible to connect to the learnr database")
  
  if (!mdb_learnr$count(paste0('{ "login": ', class_logins, ',
    "app": ', class_aa, ', "max": { "$gt": 0 },
    "verb": { "$in": ["answered", "submitted"] } }')))
    return(NULL)
  
  part1 <- mdb_learnr$aggregate(paste0('[ {
  "$match": {
    "login": ', class_logins, ',
    "app": ', class_aa, ',
    "max": { "$gt": 0 },
    "verb": { "$in": ["answered", "submitted"] }
  }
}, {
  "$group": {
    "_id": "$app",
    "count": { "$sum": 1 },
    "done": { "$addToSet": "$label" },
    "succeeded": { "$addToSet": {
      "$cond": [ { "$eq": ["$score", 1] }, "$label", null ]
    } },
    "max": { "$max": "$max" }
  }
}, {
  "$project": {
    "app": "$_id",
    "items": "$done",
    "count_all": "$count",
    "count_avg": { "$divide": ["$count", ', attr(class_logins, "n"), '] },
    "progress_max": { "$size": "$done" },
    "raw_score_max": { "$size": { "$setIntersection": ["$succeeded", "$done"] } },
    "max": "$max",
    "_id": false
  }
} ]'))
  
  # raw_score_avg
  part2 <- mdb_learnr$aggregate(paste0('[ {
  "$match": {
    "login": ', class_logins, ',
    "app": ', class_aa, ',
    "max": { "$gt": 0 },
    "verb": { "$in": ["answered", "submitted"] }
  }
}, {
  "$group": {
    "_id": { "login": "$login", "app": "$app" },
    "done": { "$addToSet": "$label" },
    "succeeded": { "$addToSet": {
      "$cond": [ { "$eq": ["$score", 1]}, "$label", null ]
    } }
  }
}, {
  "$project": {
    "login": "$_id.login",
    "app": "$_id.app",
    "raw_score_max_by_user": { "$size": { "$setIntersection": ["$succeeded", "$done"] } },
    "_id": false
  }
}, {
  "$group": {
    "_id": "$app",
    "raw_score_avg": { "$avg": "$raw_score_max_by_user" }
  }
}, {
  "$project": {
    "app": "$_id",
    "raw_score_avg": { "$ifNull": [ "$raw_score_avg", 0] },
    "_id": false
  }
} ]'))
  suppressMessages(dplyr::full_join(part1, part2))
}

learnr_user_prog <- function(user_login, class_aa, url = sdd_url) {
  mdb_learnr <- try(mongolite::mongo("learnr", url = url), silent = TRUE)
  if (inherits(mdb_learnr, "try-error"))
    stop("Error: impossible to connect to the learnr database")
  
  if (!mdb_learnr$count(paste0('{ "login": "', user_login, '",
    "app": ', class_aa, ', "max": { "$gt": 0 },
    "verb": { "$in": ["answered", "submitted"] } }')))
    return(NULL)
  
  mdb_learnr$aggregate(paste0('[ {
  "$match": {
    "login": "', user_login, '",
    "app": ', class_aa, ',
    "max": { "$gt": 0 },
    "verb": { "$in": ["answered", "submitted"] }
  }
}, {
  "$group": {
    "_id": "$app",
    "count": { "$sum": 1 },
    "done" : { "$addToSet": "$label" },
    "succeeded": { "$addToSet": {
      "$cond": [ { "$eq": ["$score", 1]}, "$label", null ]
    } }
  }
}, {
  "$project": {
    "app": "$_id",
    "id": "",
    "items_done": { "$setDifference": [ "$succeeded", [null] ] },
    "count": "$count",
    "progress": { "$size": "$done" },
    "raw_score": { "$size": { "$setIntersection": ["$succeeded", "$done"] } },
    "_id": false
  }
} ]'))
}

learnr_prog <- function(user_login, class_logins, class_aa, class_data = NULL,
    url = sdd_url) {
  if (is.null(class_data)) {
    class_data <- learnr_class_prog(class_logins, class_aa, url = url)
  }
  if (is.null(class_data))
    return(NULL) # No Shiny data for this course
  user_data <- learnr_user_prog(user_login, class_aa, url = url)
  if (is.null(user_data)) {
    n <- nrow(class_data)
    user_data <- data.frame(app = class_data$app, id = rep("", n),
      items_done = rep("", n), count = rep(0, n),
      progress = rep(0, n), raw_score = rep(0, n))
  }
  
  # Merge statistics for user and class by app
  res <- suppressMessages(dplyr::full_join(class_data, user_data))
  res <- dplyr::select(res, "app", "id", "items", "items_done", "max",
    "progress_max", "progress", "raw_score_max", "raw_score_avg", "raw_score",
    "count_all", "count_avg", "count")
  # dplyr::mutate(res, activity = pmin(1, count / count_avg) * pmax(max, na.rm = TRUE))
  res$activity <- pmin(1, res$count / res$count_avg) *
    pmax(res$max, na.rm = TRUE)
  res
}

# Get H5P data for a whole class
h5p_class_prog <- function(class_logins, class_aa, url = sdd_url) {
  mdb_h5p <- try(mongolite::mongo("h5p", url = url), silent = TRUE)
  if (inherits(mdb_h5p, "try-error"))
    stop("Error: impossible to connect to the H5P database")
  
  if (!mdb_h5p$count(paste0('{ "login": ', class_logins, ',
    "app": ', class_aa, ' }')))
    return(NULL)
  
  part1 <- mdb_h5p$aggregate(paste0('[ {
  "$match": {
    "login": ', class_logins, ',
    "app": ', class_aa, '
  }
}, {
  "$group": {
    "_id": "$id",
    "apps": { "$addToSet": {
      "$cond": [ { "$eq": ["$version", null] }, "$app", null ]
    } },
    "count": { "$sum": {
      "$cond": [ { "$in": ["$verb", ["attempted"]] }, 0, 1 ]
    } }
  }
}, {
  "$project": {
    "id": "$_id",
    "app": { "$first": { "$setDifference": [ "$apps", [null] ] } },
    "count_all": "$count",
    "count_avg": { "$divide": ["$count", ', attr(class_logins, "n"), '] },
    "_id": false
  }
} ]'))
  
  part2 <- mdb_h5p$aggregate(paste0('[ {
  "$match": {
    "login": ', class_logins, ',
    "app": ', class_aa, ',
    "verb": "answered",
    "app": { "$ne": "" },
    "max": { "$gt": 0 }
  }
}, {
  "$group": {
    "_id": { "id": "$id", "app": "$app" },
    "score_max": { "$max": "$score" },
    "score_avg": { "$avg": "$score" },
    "max": { "$max": "$max" }
  }
}, {
  "$project": {
    "id": "$_id.id",
    "app": "$_id.app",
    "progress": "$max",
    "raw_score_max": "$score_max",
    "raw_score_avg": "$score_avg",
    "max": "$max",
    "_id": false
  }
}, {
  "$group": {
    "_id": "$id",
    "items": { "$addToSet": "$app" },
    "progress": { "$sum": "$progress" },
    "raw_score_max": { "$sum": "$raw_score_max" },
    "raw_score_avg": { "$avg": "$raw_score_avg" },
    "max": { "$sum": "$max" }
  }
}, {
  "$project": {
    "id": "$_id",
    "items": "$items",
    "progress_max": "$progress",
    "raw_score_max": "$raw_score_max",
    "raw_score_avg": "$raw_score_avg",
    "max": "$max",
    "_id": false
  }
} ]'))
  
  res <- suppressMessages(dplyr::full_join(part1, part2))
  # Eliminate entries where count_all is zero
  # dplyr::filter(res, count_all > 0)
  res <- res[res$count_all > 0, ]
  res
}

h5p_user_prog <- function(user_login, class_aa, url = sdd_url) {
  mdb_h5p <- try(mongolite::mongo("h5p", url = url), silent = TRUE)
  if (inherits(mdb_h5p, "try-error"))
    stop("Error: impossible to connect to the H5P database")
  
  if (!mdb_h5p$count(paste0('{ "login": "', user_login, '",
    "app": ', class_aa, ' }')))
    return(NULL)
  
  part1 <- mdb_h5p$aggregate(paste0('[ {
  "$match": {
    "login": "', user_login, '",
    "app": ', class_aa, '
  }
}, {
  "$group": {
    "_id": "$id",
    "count": { "$sum": {
      "$cond": [ { "$in": ["$verb", ["attempted"]] }, 0, 1 ]
    } }
  }
}, {
  "$project": {
    "id": "$_id",
    "count": "$count",
    "_id": false
  }
} ]'))
  
  if (!mdb_h5p$count(paste0('{ "login": "', user_login, '",
    "app": ', class_aa, ', "verb": "answered", "max": { "$gt": 0 } }'))) {
    # Fake data because the student did not answered to anything yet
    n <- nrow(part1)
    part2 <- data.frame(id = part1$id, items_done = rep("", n),
      progress = rep(0, n), raw_score = rep(0, n))
  } else {
    part2 <- mdb_h5p$aggregate(paste0('[ {
  "$match": {
    "login": "', user_login, '",
    "app": ', class_aa, ',
    "verb": "answered",
    "max": { "$gt": 0 }
  }
}, {
  "$group": {
    "_id": { "id": "$id", "app": "$app" },
    "progress": { "$max": "$max" },
    "raw_score": { "$max": "$score" }
  }
}, {
  "$project": {
    "id": "$_id.id",
    "app": "$_id.app",
    "progress": "$progress",
    "raw_score": "$raw_score",
    "_id": false
  }
}, {
  "$group": {
    "_id": "$id",
    "items_done": { "$addToSet": "$app" },
    "progress": { "$sum": "$progress" },
    "raw_score": { "$sum": "$raw_score" }
  }
}, {
  "$project": {
    "id": "$_id",
    "items_done": "$items_done",
    "progress": "$progress",
    "raw_score": { "$ifNull": [ "$raw_score", 0] },
    "_id": false
  }
} ]'))
  }
  
  res <- suppressMessages(dplyr::full_join(part1, part2))
  # Eliminate entries where count is zero
  # dplyr::filter(res, count > 0)
  res <- res[res$count > 0, ]
  res
}

h5p_prog <- function(user_login, class_logins, class_aa, class_data = NULL,
    url = sdd_url) {
  if (is.null(class_data)) {
    class_data <- h5p_class_prog(class_logins, class_aa, url = url)
  }
  if (is.null(class_data))
    return(NULL) # No H5P data for this course
  
  user_data <- h5p_user_prog(user_login, class_aa, url = url)
  if (is.null(user_data)) {
    n <- nrow(class_data)
    user_data <- data.frame(id = class_data$id,
      items_done = rep("", n), count = rep(0, n),
      progress = rep(0, n), raw_score = rep(0, n))
  }
  
  # Merge statistics for user and class by app
  res <- suppressMessages(dplyr::full_join(class_data, user_data))
  res <- dplyr::select(res, "app", "id", "items", "items_done", "max",
    "progress_max", "progress", "raw_score_max", "raw_score_avg", "raw_score",
    "count_all", "count_avg", "count")
  # dplyr::mutate(res, activity = pmin(1, count / count_avg) *
  #   pmax(max, na.rm = TRUE))
  res$activity <- pmin(1, res$count / res$count_avg) *
    pmax(res$max, na.rm = TRUE)
  res
}
