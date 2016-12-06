
#' Scrape NBA player data by season
#'
#' This function retrieves NBA player data for a specific season, specified by
#' year. There will be one row per player X team combination. That is, if a
#' player played for multiple teams in one season, they will show up on multiple
#' rows (one row for each team).
#'
#'
#' @param year Which season do you want data for? If you want the 2016-2017 data,
#' use the latter year (2017).
#' @return A data.frame with a row for each player X team combo in that season.
#'
#' The columns include:
#'
#' \describe{
#' \item{player}{Player name}
#' \item{pos}{Position}
#' \item{age}{Age in years}
#' \item{tm}{Team}
#' \item{g}{Games played}
#' \item{gs}{Games started}
#' \item{mp}{Minutes played}
#' \item{fg}{Field goals made}
#' \item{fga}{Field goals attempted}
#' \item{fg_pct}{Field goal shooting percentage}
#' \item{three_p}{Three point shots made}
#' \item{three_pa}{Three point shots attempted}
#' \item{three_p_pct}{Three point shooting percentage}
#' \item{two_p}{Two point shots made}
#' \item{two_pa}{Two point shots attempted}
#' \item{two_p_pct}{Two point shooting percentage}
#' \item{efg_pct}{Effective field goal percentage (adjusts for fact that 3
#' pointers are worth one more point than two pointers)}
#' \item{ft}{Free throws made}
#' \item{fta}{Free throw attempts}
#' \item{ft_pct}{Free throw percentage}
#' \item{orb}{Offensive rebounds}
#' \item{drb}{Defensive rebounds}
#' \item{trb}{Total rebounds}
#' \item{ast}{Assists}
#' \item{stl}{Steals}
#' \item{blk}{Blocks}
#' \item{tov}{Turnovers}
#' \item{pf}{Personal fouls}
#' \item{pts}{Points made}
#' \item{start_year}{Year season began}
#' \item{end_year}{Year seson ended}
#' }
#'
#'
#' @examples
#' d <- get_season(2010)
#' @export
#'
get_season <- function(year) {
  newest_year <- 1 + as.numeric(format(Sys.Date(), "%Y"))
  if (year < 1947 | year > newest_year) {
    stop("Data are only available after 1947 and up to the present.")
  }
  url <- paste0("http://www.basketball-reference.com/leagues/NBA_",
                year,
                "_totals.html")

  html <- xml2::read_html(url)
  node <- rvest::html_node(html, "table")
  table <- rvest::html_table(node, header = TRUE)
  d <- parse_season_table(table)
  d$start_year <- year - 1
  d$end_year <- year
  d
}

parse_season_table <- function(table) {
  duplicated_header_rows <- table$Rk == "Rk"
  table <- table[!duplicated_header_rows, ]
  converted <- lapply(table, maybe_as_numeric)
  converted <- lapply(converted, empty_string_to_na)
  df <- as.data.frame(converted, stringsAsFactors = FALSE)
  df <- df[, !(names(df) == "Rk")] # remove "Rank" column
  names(df) <- gsub("\\.", "_pct", names(df))
  names(df) <- gsub("X2", "two_", names(df))
  names(df) <- gsub("X3", "three_", names(df))
  names(df) <- tolower(names(df))
  df
}

maybe_as_numeric <- function(x) {
  # tries to make numeric columns numeric (from char)
  numeric_x <- suppressWarnings(as.numeric(x))
  if (!all(is.na(numeric_x))) x <- numeric_x
  x
}

empty_string_to_na <- function(x) {
  # sometimes (especially old datasets), empty values are ""
  if (all(x == "")) {
    x <- rep(NA, length(x))
  }
  x
}