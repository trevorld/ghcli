#' Open the GitHub repository in a web browser
#'
#' `gh_browse()` opens up a GitHub repository in a web browser.
#'
#' To configure a web browser other than the default use
#' the `BROWSER` environmental variable.
#' @param what A character string, integer, or `NULL`
#'   + `NULL` opens up repository home page
#'   + "commit" opens up last commit
#'   + "projects" opens repository projects
#'   + "settings" opens repository settings
#'   + "wiki" opens repository wiki
#'   + INT or "INT" opens issue or pull request INT
#'   + "PATH" opens file page of file with path PATH
#'   + "PATH:INT" opens file page of file with path PATH at line INT
#'   + "SHA" opens commit page of commit with sha SHA
#' @param ... Ignored.
#' @param branch A string of branch name to use (if other than default branch).
#' @param repo A string of another repository in `[HOST/]OWNER/REPO` format.
#' @return `NULL` invisibly.
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_browse() # open up repo home page
#'   gh_browse(23) # issue or pull request 23
#'   gh_browse("settings") # open up repo settings
#'   gh_browse("R/utils.r:5") # open up file page for R/utils.r at line 5
#' }
#' @seealso <https://cli.github.com/manual/gh_browse>
#' @export
gh_browse <- function(what = NULL, ..., branch = NULL, repo = NULL) {
    chkDots(...)
    assert_gh()
    args <- "browse"
    if (!is.null(what)) {
        if (is.numeric(what))
            what <- as.character(what)
        assert_string(what)
        if (grepl(what, "^(commit|projects|settings|wiki)$")) {
            args <- c(args, switch(what,
                                   commit = "--commit",
                                   project = "--project",
                                   settings = "--settings",
                                   wiki = "--wiki"))
        } else {
            args <- c(args, what)
        }
    }
    if (!is.null(branch)) {
        branch <- assert_string(branch)
        args <- c(args, "--branch", branch)
    }
    if (!is.null(repo)) {
        repo <- assert_string(repo)
        args <- c(args, "--repo", repo)
    }

    gh_system2(args)

    invisible(NULL)
}
