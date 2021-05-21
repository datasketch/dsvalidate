#' Recommends suitable visualization
#'
#' ADVANCED
#'
#' Suggest suitable visualization types from a loaded df.
#'
#' For this, we would need a list of all possible visualisation we offer
#' with DS apps and sets of minimum requirements for each of them.
#'
#' e.g. app-sankey-interactive: 2+ columns with categorical variables with max 20 categories (more?)
#'      app-simple-charts-orgs: 2+ columns in total AND need at least one categorical column (more?)
#'
#' @param df Loaded dataframe
#'
#' @return List of recommended visualizations
#' @export
#'
#' @examples
recommend_viz <- function(df){

}


#' Recommends suitable app
#'
#' ADVANCED
#'
#' Suggest available DS apps from a loaded df.
#'
#' This uses recommend_viz to suggest the DS apps that can be used
#' to create recommended visualisations from the loaded df.
#'
#' @param df Loaded dataframe
#'
#' @return List of recommended DS apps
#' @export
#'
#' @examples
recommend_app <- function(df){

}
