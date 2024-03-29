#' Pseudo-function to re-export \strong{dplyr}'s common functions. 
#' 
#' @name dplyr functions
#'
#' @importFrom dplyr select rename mutate filter arrange distinct summarise 
#'     do group_by ungroup rowwise do data_frame left_join inner_join everything
#'     bind_rows pull as_tibble if_else tibble
NULL


#' Pseudo-function to re-export \strong{magrittr}'s pipe. 
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL


#' Pseudo-function to re-export \strong{rlang}'s walrus operator.
#'
#' @importFrom rlang :=
#' @name :=
#' @rdname walrus
#' @export
NULL
