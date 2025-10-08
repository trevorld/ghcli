#' List secrets in a GitHub repository
#'
#' `gh_secret_list()` returns a data frame of secrets in a GitHub repository.
#' @inheritParams gh_browse
#' @return A data frame with labels with columns "name", "color", "description".
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_secret_list()
#' }
#' @seealso <https://cli.github.com/manual/gh_secret_list>
#' @export
gh_secret_list <- function(..., repo = NULL) {
    chkDots(...)
	fields <- c("name", "updatedAt")
    args <- gh_args(
        c("secret", "list",
          "--json", paste(fields, collapse = ",")),
		repo
    )
    output <- gh_system2(args)
	df <- jsonlite::fromJSON(output)
    if (length(df) == 0L) {
        df <- tibble::tibble()
        for (field in fields) {
			df[[field]] <- character(0L)
        }
    } else {
        df <- df[, fields]
        df <- tibble::as_tibble(df)
    }
    for (field in fields) {
        if (grepl("At$", field)) {
            df[[field]] <- as.POSIXct(df[[field]], format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
        }
    }
    df
}

#' Delete a secret in a GitHub repository
#'
#' `gh_secret_delete()` deletes a secret in a GitHub repository.
#' @inheritParams gh_browse
#' @param secret_name Name of secret.
#' @seealso <https://cli.github.com/manual/gh_secret_delete>
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_secret_delete("SECRET_NAME")
#' }
#' @export
gh_secret_delete <- function(secret_name, ..., repo = NULL) {
    chkDots(...)
	assert_string(secret_name)
    args <- gh_args(
        c("secret", "delete", shQuote(secret_name)),
		repo
    )
    output <- gh_system2(args)
    invisible(NULL)
}

#' Sets a secret in a GitHub repository
#'
#' `gh_secret_set()` sets a secret in a GitHub repository.
#' @inheritParams gh_browse
#' @param secret_name Name of the secret.
#' @param body Value of the secret
#' @seealso <https://cli.github.com/manual/gh_secret_set>
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_secret_set("SECRET_NAME", "SECRET_VALUE")
#' }
#' @export
gh_secret_set <- function(secret_name, body, ..., repo = NULL) {
    chkDots(...)
	assert_string(secret_name)
	assert_string(body)
    args <- gh_args(
        c("secret", "set", shQuote(secret_name), "--body", shQuote(body)),
		repo
    )
    output <- gh_system2(args)
    invisible(NULL)
}
