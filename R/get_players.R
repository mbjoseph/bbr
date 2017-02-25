#' Find information for all NBA players
#'
#' This function retrieves NBA player data for players by last initial
#'
#' @param initial What's the last initial you're interested in? E.g., to find
#' information on John Stockton, the initial is "S".
#' @return A data.frame with a row for each player with that last initial.
#'
#' The columns include:
#'
#' \describe{
#' \item{player}{Player name}
#' \item{from}{First year in league}
#' \item{to}{Last year in league}
#' \item{pos}{Position(s) played}
#' \item{ht}{Height in 'feet-inches' format}
#' \item{wt}{Weight in pounds}
#' \item{birth_date}{Birth date}
#' \item{college}{What college did the player attend?}
#' }
#'
#' @examples
#' d <- get_players("Z")
#'
#' @export
get_players <- function(initial) {
  stopifnot(nchar(initial) == 1)
  stopifnot(grepl("[[:alpha:]]", initial))
  initial <- tolower(initial)

  url <- paste0("http://www.basketball-reference.com/players/",
                initial, "/")
  html <- xml2::read_html(url)
  node <- rvest::html_node(html, "table")

  table <- rvest::html_table(node, header = TRUE)
  names(table) <- tolower(names(table))

  table$player <- gsub("\\*", "", table$player)
  converted <- lapply(table, empty_string_to_na)

  links <- rvest::html_nodes(html, "th a")
  suffix <- rvest::html_attr(links, name = "href")
  suffix <- gsub(paste0("/players/[[:alpha:]]/"), "", suffix)
  converted$slug <- gsub(".html", "", suffix)
  names(converted) <- gsub(" ", "_", names(converted))

  as.data.frame(converted, stringsAsFactors = FALSE)
}
