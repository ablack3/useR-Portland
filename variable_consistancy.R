#' Check the consistancy of one variable across another
#'
#' When each level of a (categorical) key variable  is associated with one and only one level of a second variable we will say that the second variable is consistant across levels of the first.
#' For example suppose you have some data with a person identifier and a gender variable and multiple rows per person.
#' If gender is consistant across person_id then each person has one and only one gender. When this is the case you can
#' easily summarise the data by taking distinct combinations of person_id and gender.
#' However if gender is not consistant across person_id then summarising using distinct combinations will not work.
#' Use this function when you want to know what percentage of levels of a key variable (eg. person_id) are associated with a single level of a second variable (eg. gender).
#'
#' @param tbl_dbi A tbl_dbi database connection or local dataframe
#' @param variable The bare (unquoted) name of the variable you want to check the consistancy of.
#' @param across The quoted name of the key variable (eg "person_id").
#'
#' @return A summary table describing the variable consistancy
#' @export
#'
#' @examples
#' library(dplyr)
#' starwars %>%
#'     check_variable_consistancy(across = "gender", species)
check_variable_consistancy <- function(tbl_dbi, variable, across){
  variable <- rlang::enquo(variable)
  across_expr <- rlang::parse_expr(across)

  tbl_dbi %>%
    ungroup() %>%
    distinct(!!across_expr, !!variable) %>%
    count(!!across_expr) %>%
    ungroup() %>%
    count(n) %>%
    mutate(pct = round(nn/sum(nn, na.rm = TRUE), 3), cumpct = order_by(n, cumsum(pct))) %>%
    rename(!!as.name(paste0("n_", across, "_levels")) := nn, n_distinct = n) %>%
    arrange(n_distinct)
}




#' Make one variable consistant arcoss levels of another variable
#'
#' This is a powerful data cleaning and imputation function. It often happens that a variable is not consistant in the way it should be.
#' For example billtype is a claim level variable so billtype should be consistant across the mhdo_claim variable that identifies claims.
#' There may be a small number of claims that have multiple billtypes associated with them. We can use the make consistant function to clean
#' up this situaltion by choosing the most frequently occuring non-missing value.
#' Another example might be patients that lived in multiple zipcodes. Perhaps we want to assign a single zip to each patient.
#' The make_consistant() function can be used to impute the most frequently occuring non-missing value for zipcode by patient.
#'
#' @param df A local dataframe or Vertica tbl_dbi object
#' @param ... Bare variable names seprated by commas. These are the variables to make consistant.
#' @param across A key variable quoted. Examples: patkey or mhdo_claim.
#'
#' @return The modified input with desired variables consistant across the key variable.
#' @export
#'
#' @examples
#' df <- tibble::tribble(
#' ~id, ~var1, ~var2,
#' 1, "a", "1",
#' 1, "a", NA,
#' 1, "", "1",
#' 1, "", NA,
#' 1, "b", NA)
#'
#' df2 <- make_consistant(df, var1, var2, across = "id")
make_consistant <- function(df,  ..., across){
  stopifnot(is.character(across))
  across <- as.name(across)
  dots <- rlang::quos(...)
  # print(dots)
  tmp <- paste0("__tmp_n", 1:length(dots))
  
  replacement_expr <- purrr::map2(dots, tmp,
    ~rlang::expr(!!rlang::quo_name(.x) := first(!!.x, order_by = desc(!!as.name(.y)))))

  final_expr <- purrr::map2(dots, tmp, ~list(
    rlang::expr(group_by(!!across, !!.x)),
    rlang::expr(mutate(!!.y := ifelse(is.na(!!.x) | !!.x == "", 0, n())))
    )) %>%
    c(rlang::expr(df), .) %>%
    c(rlang::expr(ungroup())) %>%
    c(rlang::expr(group_by(!!across))) %>%
    c(rlang::expr(mutate(!!!replacement_expr))) %>%
    c(rlang::expr(select(-starts_with("__tmp_n")))) %>%
    rlang::flatten() %>%
    purrr::reduce(function(a,b) rlang::expr(!!a %>% !!b))
  # print(final_expr)
  df_out <- rlang::eval_tidy(final_expr)
  return(df_out)
}
