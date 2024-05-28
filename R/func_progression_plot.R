#' Plot the Progression of the Students in the Data Science Exercises
#'
#' @param login The student (GitHub) login.
#' @param course The institutional identifier of the course, e.g., "S-BIOG-015".
#' @param module The module to restrict to, or NULL for all modules in the course.
#' @param sdd_url The URL to the SDD MongoDB database.
#'
#' @return This function is used for its side-effect of plotting a chart
#'   comparing the student and the class progression.
#' @export
#' @author: Philippe Grosjean (phgrosjean\@sciviews.org)
#'
#' @examples
#' #progression_plot(<insert_login_here, "S-BIOG-015",
#' #  module = NULL, sdd_url = "mongodb://127.0.0.1:27017/sdd")
progression_plot <- function(login, course, module = NULL, sdd_url) {
  plot_progression(login, get_progression(login = login,
    icourse = course, module = module, url = sdd_url))
}

# Get the data for the progression plot
get_progression <- function(login, icourse, module = NULL, url = sdd_url) {
  course <- "sdd1m" # Default for S-BIOG-006 & S-BIOG-027
  if (icourse == "S-BIOG-015" || icourse == "S-BIOG-061") course <- "sdd2m"
  if (icourse == "S-BIOG-025") course <- "sdd3m"
  if (icourse == "S-BIOG-921") course <- "sdd1c"
  if (icourse == "S-BIOG-937-958-959") course <- "sdd2c"
  #message("Course: '", course, "'")
  aa <- paste0(course, "q1")
  # Special case for q2 courses
  if (icourse == "S-BIOG-027") aa <- "sdd1mq2"
  if (icourse == "S-BIOG-061") aa <- "sdd2mq2"
  class_logs <- class_logins(course, url = url, as.json = TRUE)
  class_apps <- class_aa(aa, url = url, as.json = TRUE, module = module)
  
  resh <- h5p_prog(login, class_logs, class_apps, url = url)
  #print(nrow(resh))
  #print(head(resh))
  resl <- learnr_prog(login, class_logs, class_apps, url = url)
  #print(nrow(resl))
  #print(head(resl))
  ress <- shiny_prog(login, class_logs, class_apps, url = url)
  #print(nrow(ress))
  #print(head(ress))
  
  # We deal with all the possible cases of missing data
  if (is.null(resh) && is.null(resl) && is.null(ress) )
    return(structure(list(), comment = "Pas encore d'enregistrements."))
  if (is.null(resh)) {
    if (is.null(resl)) {
      res <- ress
    } else {# resl not null
      if (is.null(ress)) {
        res <- resl
      } else {
        res <- rbind(resl, ress)
      }
    }
  } else {# resh not null
    res <- resh
    if (!is.null(resl) && NROW(resl) > 0)
      res <- rbind(res, resl)
    if (!is.null(ress) && NROW(ress) > 0)
      res <- rbind(res, ress)
  }
  
  res <- dplyr::arrange(as.data.frame(res), "app")
  # If not any student finish an exercice, we got NA in different places
  # -> relace these by zero for the plot
  res$max[is.na(res$max)] <- 0
  res$progress_max[is.na(res$progress_max)] <- 0
  res$progress[is.na(res$progress)] <- 0
  res$raw_score_max[is.na(res$raw_score_max)] <- 0
  res$raw_score_avg[is.na(res$raw_score_avg)] <- 0
  res$raw_score[is.na(res$raw_score)] <- 0
  res$count[is.na(res$count)] <- 0
  res$activity[is.na(res$activity)] <- 0
  attr(res, "user") <- login
  res
}

# Make a progression plot for one student
plot_progression <- function(user, data) {
  ggplot(data = data, aes_string(x = "app", y = "max")) +
    geom_hline(yintercept = c(0:max(data$max)), col = "gray") +
    # White background
    geom_col(fill = "white", col = "gray50", width = 0.9) +
    # Average and max progression (avg and max raw_score not shown here)
    geom_col(aes_string(x = "app", y = "progress_max"), fill = "gray95",
      col = "gray50", width = 0.9) +
    geom_col(aes_string(x = "app", y = "raw_score_avg"), fill = "gray85",
      col = "gray50", width = 0.9) +
    # User's progression and raw_score
    geom_col(aes_string(x = "app", y = "progress"), fill = "red4",
      col = "black", width = 0.5) +
    geom_col(aes_string(x = "app", y = "raw_score"), fill = "royalblue4",
      col = "black", width = 0.5) +
    # User's relative activity indicator
    geom_col(aes_string(x = "app", y = "activity"), fill = "black",
      col = "black", width = 0.03, position = position_nudge(x = -0.45)) +
    coord_flip() +
    #scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    # cowplot::theme_cowplot(font_size = 14) +
    labs(title = paste("Progression -", user), x = "", y = "")
}
