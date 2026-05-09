#' Make a GitHub GraphQL API request
#'
#' `gh_api_graphql()` makes an authenticated GraphQL request to the GitHub API.
#'
#' @param query A GraphQL query string.
#' @param ... Ignored.
#' @param variables A named list of GraphQL variables.  Passed as the
#'   `variables` key in the JSON request body.
#' @param repo Repository in `OWNER/REPO` format.  Controls what `{owner}`
#'   and `{repo}` placeholders expand to.  If `NULL` uses the current repository.
#' @return A named list representing the `data` element of the GraphQL response.
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_api_graphql('{ repository(owner: "trevorld", name: "ghcli") { hasSponsorshipsEnabled } }')
#'
#'   # with GraphQL variables
#'   gh_api_graphql(
#'     'query($owner: String!, $name: String!) {
#'       repository(owner: $owner, name: $name) { hasSponsorshipsEnabled }
#'     }',
#'     variables = list(owner = "trevorld", name = "ghcli")
#'   )
#' }
#' @seealso <https://cli.github.com/manual/gh_api>
#' @export
gh_api_graphql <- function(query, ..., variables = list(), repo = NULL) {
	chkDots(...)
	assert_string(query)
	body <- list(query = query)
	if (length(variables)) {
		body[["variables"]] <- variables
	}
	tmp <- tempfile(fileext = ".json")
	on.exit(unlink(tmp), add = TRUE)
	writeLines(jsonlite::toJSON(body, auto_unbox = TRUE), tmp)
	args <- c("api", "graphql", "--input", tmp)
	env <- if (!is.null(repo)) {
		assert_string(repo)
		paste0("GH_REPO=", repo)
	} else {
		character(0)
	}
	output <- gh_system2(args, env = env)
	result <- jsonlite::fromJSON(paste(output, collapse = "\n"))
	if (!is.null(result[["errors"]])) {
		cli::cli_abort("GraphQL query failed: {result$errors[[1]]$message}")
	}
	result[["data"]]
}
