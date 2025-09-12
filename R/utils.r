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

assert_min_version <- function(min_version) {
    current_version <- gh_version()
    if (current_version < min_version) {
        cli::cli_abort(c("`gh` must be at least version {min_version}",
                         i = "Your `gh` is version {current_version}"))
    }
}

has_gh <- function() nzchar(gh_cmd())

gh_cmd <- function() Sys.which("gh")

gh_system2 <- function(args, ...) {
    cmd <- gh_cmd()
    output <- system2(cmd, args, ..., stdout = TRUE, stderr = TRUE) |> suppressWarnings()
    status <- attr(output, "status")
    if (status != 0L) {
        msg <- c("`gh` command failed with status {sQuote(status)}",
                 i = "The `gh` standard error is stored in the `stderr` attribute of the error condition",
                 i = 'If last error can print via `cat(rlang::last_error()$stderr, sep = "\\n")`')
        cli::cli_abort(msg, stderr = output)
    }
    invisible(output)
}

gh_version <- function() {
    output <- gh_system2("--version")
    version <- gsub("gh version ([0-9.]+) .*", "\\1", output[[1L]])
    numeric_version(version)
}
