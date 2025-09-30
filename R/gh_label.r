#' List labels in GitHub repository
#'
#' `gh_label_list()` returns a data frame of labels in a GitHub repository.
#'
#' The "color" column in the returned data frame may have SGR control sequences in it.
#' They can be stripped by `df$color <- cli::ansi_strip(df$color)`.
#' Can also disable colors with a `NO_COLOR=true` environmental variable
#' or set `options(cli.ansi = FALSE)`.
#' @inheritParams gh_browse
#' @return A data frame with labels with columns "name", "color", "description".
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_label_list()
#' }
#' @seealso <https://cli.github.com/manual/gh_label_list>
#' @export
gh_label_list <- function(..., repo = NULL) {
    chkDots(...)
    args <- gh_args(
        c("label", "list",
          "--limit", "1000",
          "--json", "name,color,description"),
        repo
    )
    output <- gh_system2(args)
    if (length(output) == 0L) {
        df <- tibble::as_tibble(
            name = character(0L),
            color = character(0L),
            description = character(0L)
        )
    } else {
        df <- jsonlite::fromJSON(output)
        df$color <- paste0("#", df$color)
        df <- df[, c("name", "color", "description")]
        df <- tibble::as_tibble(df)
    }
    class(df$color) <- "gh_color"
    df
}

#' Clone labels from one repo to another
#'
#' `gh_label_clone()` copies labels from one repo to another.
#'
#' @inheritParams gh_browse
#' @param source_repository A string of another repository in `[HOST/]OWNER/REPO` format.
#' @param force If `TRUE` update any existing label(s).
#' @return `NULL` invisibly
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_label_clone("r-lib/cli") # copy labels from {ghcli} to this repo
#' }
#' @seealso <https://cli.github.com/manual/gh_label_clone>
#' @export
gh_label_clone <- function(source_repository, ..., force = TRUE, repo = NULL) {
    chkDots(...)
    assert_string(source_repository)

    args <- gh_args(c("label", "clone", source_repository), repo)
    if (isTRUE(force)) {
        args <- c(args, "--force")
    }
    output <- gh_system2(args)
    invisible(NULL)
}

#' Create label in a repository
#'
#' `gh_label_create()` creates a label in a repository.
#'
#' @inheritParams gh_browse
#' @param name String of label to create.
#' @param color Color of label (default will be a random color).
#' @param description Description of label.
#' @param force If `TRUE` update an existing label.
#' @return `NULL` invisibly
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_label_create("upkeep", color = "grey", description = "Package maintenance")
#' }
#' @seealso <https://cli.github.com/manual/gh_label_create>
#' @export
gh_label_create <- function(name,
                            ...,
                            color = NULL,
                            description = NULL,
                            force = TRUE,
                            repo = NULL) {
    chkDots(...)
    assert_string(name)
    args <- gh_args(c("label", "create", shQuote(name)), repo)
    if (!is.null(color)) {
        assert_string(color)
        color <- as_gh_color(color) |> substring(2L, 7L)
        args <- c(args, "--color", color)
    }
    if (!is.null(description)) {
        assert_string(description)
        args <- c(args, "--description", shQuote(description))
    }
    if (isTRUE(force)) {
        args <- c(args, "--force")
    }
    output <- gh_system2(args)
    invisible(NULL)
}

#' Delete label(s) from a repository
#'
#' `gh_label_delete()` deletes label(s) from a repository.
#'
#' @inheritParams gh_browse
#' @param name Character vector of label(s) to delete.
#' @return `NULL` invisibly
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_label_delete("wontfix") # delete `wontfix` label
#' }
#' @seealso <https://cli.github.com/manual/gh_label_delete>
#' @export
gh_label_delete <- function(name, ..., repo = NULL) {
    chkDots(...)
    name <- as.character(name)
    l <- lapply(name, gh_label_delete_helper)
    invisible(NULL)
}

gh_label_delete_helper <- function(name, repo = NULL) {
    args <- gh_args(c("label", "delete", shQuote(name), "--yes"), repo)
    output <- gh_system2(args)
    invisible(NULL)
}

#' Update label in a repository
#'
#' `gh_label_edit()` updates a label in a repository.
#'
#' @inheritParams gh_browse
#' @param to_edit Name of the label to edit.
#' @param name New name of the label.
#' @param color Color of label.
#' @param description Description of label.
#' @return `NULL` invisibly
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_label_edit("question", color = "orange")
#' }
#' @seealso <https://cli.github.com/manual/gh_label_edit>
#' @export
gh_label_edit <- function(to_edit,
                          ...,
                          color = NULL,
                          description = NULL,
                          name = NULL,
                          repo = NULL) {
    chkDots(...)
    assert_string(to_edit)
    args <- gh_args(c("label", "edit", shQuote(to_edit)), repo)
    if (!is.null(color)) {
        assert_string(color)
        color <- as_gh_color(color) |> substring(2L, 7L)
        args <- c(args, "--color", color)
    }
    if (!is.null(description)) {
        assert_string(description)
        args <- c(args, "--description", shQuote(description))
    }
    if (!is.null(name)) {
        assert_string(name)
        args <- c(args, "--name", shQuote(name))
    }
    output <- gh_system2(args)
    invisible(NULL)
}
