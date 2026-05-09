#' Create issue in a repository
#'
#' `gh_issue_create()` creates an issue in a repository.
#'
#' @inheritParams gh_browse
#' @param assignee Character vector or `NULL`.  Use `"@me"` to self-assign.
#' @param body String.
#' @param label Character vector or `NULL`.
#' @param milestone Character vector or `NULL`.
#' @param project Character vector or `NULL`.
#' @param title String.
#' @return `NULL` invisibly
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_issue_create(title = "New issue title")
#' }
#' @seealso <https://cli.github.com/manual/gh_issue_create>
#' @export
gh_issue_create <- function(...,
                            assignee = NULL,
                            body = "",
                            label = NULL,
                            milestone = NULL,
                            project = NULL,
                            title = NULL,
                            repo = NULL) {
    chkDots(...)
    stopifnot(!is.null(title), !is.null(body))
    args <- gh_args(c("issue", "create", "--body-file", "-"), repo)
    if (!is.null(assignee)) {
        stopifnot(is.character(assignee))
        for (a in assignee) {
            args <- c(args, "--assignee", shQuote(a))
        }
    }
    if (!is.null(label)) {
        stopifnot(is.character(label))
        for (l in label) {
            args <- c(args, "--label", shQuote(l))
        }
    }
    if (!is.null(milestone)) {
        stopifnot(is.character(milestone))
        for (m in milestone) {
            args <- c(args, "--milestone", shQuote(m))
        }
    }
    if (!is.null(project)) {
        stopifnot(is.character(project))
        for (p in project) {
            args <- c(args, "--project", shQuote(p))
        }
    }
    if (!is.null(title)) {
        assert_string(title)
        args <- c(args, "--title", shQuote(title))
    }
    output <- gh_system2(args, input = body)
    invisible(NULL)
}
