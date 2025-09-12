assert_gh <- function() {
    if (!has_gh())
        cli::cli_abort("Command `gh` not found")
    invisible(NULL)
}

assert_string <- function(x) {
    if (!is.character(x) || length(x) != 1L) {
        nm <- deparse(substitute(x))
        cli::cli_abort("`{nm}` must be a string.")
    }
    invisible(NULL)
}

has_gh <- function() nzchar(gh_cmd())

gh_cmd <- function() Sys.which("gh")

gh_system2 <- function(args, ...) {
    cmd <- gh_cmd()
    output <- system2(cmd, args, ..., stdout = TRUE)
    invisible(output)
}
