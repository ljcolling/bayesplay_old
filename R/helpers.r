# use default value

`%||%` <- function(x, y) { # nolint
if (purrr::is_empty(x)) y else x
}

