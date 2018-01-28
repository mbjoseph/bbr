
maybe_as_numeric <- function(x) {
  # tries to make numeric columns numeric (from char)
  numeric_x <- suppressWarnings(as.numeric(x))
  if (!all(is.na(numeric_x))) x <- numeric_x
  x
}

empty_string_to_na <- function(x) {
  # sometimes (especially old datasets), empty values are ""
  if (class(x) == "character") {
    res <- ifelse(x == "", NA, x)
  } else {
    res <- x
  }
  res
}

clean_colnames <- function(df) {
  # clean up column names for a data frame
  stopifnot(is.data.frame(df))
  df <- df[!(names(df) == "Rk")] # remove "Rank" column
  names(df) <- gsub("\\.", "_pct", names(df))
  names(df) <- gsub("X2", "two_", names(df))
  names(df) <- gsub("X3", "three_", names(df))
  names(df) <- tolower(names(df))
  df
}