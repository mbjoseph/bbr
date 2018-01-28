#' Get data on one NBA player
#'
#' This function retrieves data for one specific NBA player
#'
#' @param slug The slug for the player you're interested in. This
#' can be found using the \code{get_players()} function, and is part of 
#' the URL to the data of a player, e.g., 
#' if the URL is https://www.basketball-reference.com/players/a/abdelal01.html
#' then the slug is abdelal01.
#' 
#' @return A data.frame with data from one player.
#'
#' The columns include:
#'
#' \describe{
#' \item{player}{Player name}
#' \item{season}{What season is this data from?}
#' \item{age}{Age of player in season}
#' \item{lg}{League, e.g. NBA, ABA}
#' \item{pos}{Position played}
#' \item{g}{Number of games played}
#' \item{gs}{Number of games as starter}
#' \item{mp}{Minutes played per game}
#' \item{fg}{Field goals per game}
#' \item{fga}{Field goal attempts per game}
#' \item{fg\%}{Field goal percentage}
#' \item{3p}{3-point field goals per game}
#' \item{3pa}{3-point attempts per game}
#' \item{3p\%}{3-point field goal percentage}
#' \item{2p}{2-point field goals per game}
#' \item{2pa}{2-point attempts per game}
#' \item{2p\%}{2-point field goal percentage}
#' \item{efg\%}{Effective field goal percentage}
#' \item{ft}{Free throws per game}
#' \item{fta}{Free throw attempts per game}
#' \item{ft\%}{Free throw percentage}
#' \item{orb}{Offensive rebounds per game}
#' \item{drb}{Defensive rebounds per game}
#' \item{trb}{Total rebounds per game}
#' \item{ast}{Assists per game}
#' \item{stl}{Steals per game}
#' \item{blk}{Blocks per game}
#' \item{tov}{Turnovers per game}
#' \item{pf}{Personal fouls per game}
#' \item{pts}{Points per game}
#' }
#'
#' @examples
#' abdelnaby_d <- get_player_data('abdelal01')
#' jordan_d <- get_player_data('jordami01')
#' @export
#' 
get_player_data <- function(slug) {
  stopifnot(is.character(slug))
  initial <- substr(slug, 1, 1)
  url <- paste0("http://www.basketball-reference.com/players/",
                initial, "/", 
                slug, '.html')
  html <- xml2::read_html(url)
  node <- rvest::html_node(html, "table#per_game")
  table <- rvest::html_table(node, header = TRUE)
  table$slug <- slug
  
  player_name_node <- rvest::html_node(html, 
                                       '#footer_header > 
                                       div:nth-child(2) > 
                                       span:nth-child(4) > 
                                       strong:nth-child(1) > 
                                       span:nth-child(1)')
  player_name <- rvest::html_text(player_name_node)
  table$player <- gsub("\\*", "", player_name)

  converted <- lapply(table, empty_string_to_na)
  converted <- as.data.frame(converted, stringsAsFactors = FALSE)
  
  # ensure player name is the first column
  num_cols <- ncol(converted)
  reordered_df <- converted[, c(num_cols, 1:(num_cols - 1))]
  reordered_df <- clean_colnames(reordered_df)
  
  # strip out summary rows
  career_row <- which(reordered_df$season == 'Career')
  clean_df <- reordered_df[-c(career_row:nrow(reordered_df)), ]
  clean_df
}
