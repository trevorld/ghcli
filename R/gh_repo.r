#' Archive a GitHub repository
#'
#' `gh_repo_archive()` archives a GitHub repository.
#'
#' @param repository Repository to archive.  If `NULL` archive current repository.
#' @return `NULL` invisibly.
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_repo_archive() # archive current repo
#' }
#' @seealso <https://cli.github.com/manual/gh_repo_archive>
#' @export
gh_repo_archive <- function(repository = NULL) {
    args <- c("repo", "archive", "--yes")
    if (!is.null(repository)) {
        repository <- assert_string(repository)
        args <- c(args, shQuote(repository))
    }
    gh_system2(args)
    invisible(NULL)
}

#' Create a GitHub repository
#'
#' `gh_repo_create()` creates a GitHub repository.
#'
#' @param repository Name of repository in `[OWNER/]REPO` format.  `OWNER` defaults to name of authenticating user.
#' @param visibility Either `"public"`, `"private"`, `"internal"`
#' @inheritParams gh_repo_edit
#' @return `NULL` invisibly.
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_repo_create("newrepo", "private", description = "A new GitHub repository")
#' }
#' @seealso <https://cli.github.com/manual/gh_repo_create>
#' @export
gh_repo_create <- function(repository,
                           visibility = "private",
                           ...,
                           description = NULL,
                           enable_issues = TRUE,
                           enable_wiki = FALSE,
                           homepage = NULL) {
    chkDots(...)
    assert_string(repository)
    stopifnot(visibility %in% c("public", "private", "internal"))

    args <- c("repo", "create", shQuote(repository), paste0("--", visibility))

    if (!is.null(description)) {
        assert_string(description)
        args <- c(args, "--description", shQuote(description))
    }

    if (isFALSE(enable_issues))
        args <- c(args, "--disable-issues")

    if (isFALSE(enable_wiki))
        args <- c(args, "--disable-wiki")

    if (!is.null(homepage)) {
        assert_string(homepage)
        args <- c(args, "--homepage", homepage)
    }

    gh_system2(args)
    invisible(NULL)
}

#' Delete a GitHub repository
#'
#' `gh_repo_delete()` deletes a repository.
#'
#' @param repository Character vector of repositories in `[HOST/]OWNER/REPO` format.
#' @seealso <https://cli.github.com/manual/gh_repo_delete>
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_repo_delete("trevorld/repo_that_does_not_exist")
#' }
#' @return `NULL` invisibly.
#' @seealso <https://cli.github.com/manual/gh_label_delete>
#' @export
gh_repo_delete <- function(repository) {
    stopifnot(is.character(repository))
    l <- lapply(repository, gh_repo_delete_helper)
    invisible(NULL)
}

gh_repo_delete_helper <- function(repository) {
    args <- c("repo", "delete", "--yes", shQuote(repository))
    gh_system2(args)
    invisible(NULL)
}

#' Edit repository settings.
#'
#' `gh_repo_edit()` edits repository settings.
#' For all arguments `NULL` means don't edit this setting.
#'
#' @param repository Repository in `[HOST/]OWNER/REPO` format.
#'                   Default is the repository the working directory is in.
#' @param add_topic Either `NULL` or a character vector.
#' @param allow_forking Either `NULL`, `TRUE`, or `FALSE`.
#' @param allow_update_branch Either `NULL`, `TRUE`, or `FALSE`.
#' @param default_branch A string.
#' @param delete_branch_on_merge Either `NULL`, `TRUE`, or `FALSE`.
#' @param description A string.
#' @param enable_advanced_security Either `NULL`, `TRUE`, or `FALSE`.
#' @param enable_discussions Either `NULL`, `TRUE`, or `FALSE`.
#' @param enable_issues Either `NULL`, `TRUE`, or `FALSE`.
#' @param enable_merge_commit Either `NULL`, `TRUE`, or `FALSE`.
#' @param enable_projects Either `NULL`, `TRUE`, or `FALSE`.
#' @param enable_rebase_merge Either `NULL`, `TRUE`, or `FALSE`.
#' @param enable_secret_scanning Either `NULL`, `TRUE`, or `FALSE`.
#' @param enable_secret_scanning_push_protection Either `NULL`, `TRUE`, or `FALSE`.
#' @param enable_squash_merge Either `NULL`, `TRUE`, or `FALSE`.
#' @param enable_wiki Either `NULL`, `TRUE`, or `FALSE`.
#' @param homepage A string (of an URL).
#' @param remove_topic Either `NULL` or a character vector.
#' @param template Either `NULL`, `TRUE`, or `FALSE`.
#' @param visibility Either `NULL`, `"public"`, `"private"`, `"internal"`.
#' @param ... Ignored
#' @seealso <https://cli.github.com/manual/gh_repo_edit>
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_repo_edit(visibility = "public") # make a private repo public
#'   gh_repo_edit(delete_branch_on_merge = TRUE) # enable delete head branch when PRs are merged
#'   gh_repo_edit(enable_merge_commit = FALSE) # disable merging PRs with merge commits
#' }
#' @return `NULL` invisibly.
#' @export
gh_repo_edit <- function(repository = NULL,
                         ...,
                         add_topic = NULL,
                         allow_forking = NULL,
                         allow_update_branch = NULL,
                         default_branch = NULL,
                         delete_branch_on_merge = NULL,
                         description = NULL,
                         enable_advanced_security = NULL,
                         enable_discussions = NULL,
                         enable_issues = NULL,
                         enable_merge_commit = NULL,
                         enable_projects = NULL,
                         enable_rebase_merge = NULL,
                         enable_secret_scanning = NULL,
                         enable_secret_scanning_push_protection = NULL,
                         enable_squash_merge = NULL,
                         enable_wiki = NULL,
                         homepage = NULL,
                         remove_topic = NULL,
                         template = NULL,
                         visibility = NULL
                         ) {
    chkDots(...)
    if (is.null(repository))
        args <- c("repo", "edit")
    else
        args <- c("repo", "edit", shQuote(repository))

    accept_visibility_change_consequences <- !is.null(visibility)
    if (isTRUE(accept_visibility_change_consequences) && gh_version() >= "2.61")
        args <- c(args, "--accept-visibility-change-consequences")

    if (!is.null(add_topic)) {
        stopifnot(is.character(add_topic))
        args <- c(args, "--add-topic", paste(add_topic, collapse = ","))
    }

    if (isTRUE(allow_forking))
        args <- c(args, "--allow-forking")
    else if (isFALSE(allow_forking))
        args <- c(args, "--allow-forking=false")

    if (isTRUE(allow_update_branch))
        args <- c(args, "--allow-update-branch")
    else if (isFALSE(allow_update_branch))
        args <- c(args, "--allow-update-branch=false")

    if (!is.null(default_branch)) {
        assert_string(default_branch)
        args <- c(args, "--default_branch", default_branch)
    }

    if (isTRUE(delete_branch_on_merge))
        args <- c(args, "--delete-branch-on-merge")
    else if (isFALSE(delete_branch_on_merge))
        args <- c(args, "--delete-branch-on-merge=false")

    if (!is.null(description)) {
        assert_string(description)
        args <- c(args, "--description", shQuote(description))
    }

    if (isTRUE(enable_advanced_security))
        args <- c(args, "--enable-advanced-security")
    else if (isFALSE(enable_advanced_security))
        args <- c(args, "--enable-advanced-security=false")

    if (isTRUE(enable_discussions))
        args <- c(args, "--enable-discussions")
    else if (isFALSE(enable_discussions))
        args <- c(args, "--enable-discussions=false")

    if (isTRUE(enable_issues))
        args <- c(args, "--enable-issues")
    else if (isFALSE(enable_issues))
        args <- c(args, "--enable-issues=false")

    if (isTRUE(enable_merge_commit))
        args <- c(args, "--enable-merge-commit")
    else if (isFALSE(enable_merge_commit))
        args <- c(args, "--enable-merge-commit=false")

    if (isTRUE(enable_projects))
        args <- c(args, "--enable-projects")
    else if (isFALSE(enable_projects))
        args <- c(args, "--enable-projects=false")

    if (isTRUE(enable_rebase_merge))
        args <- c(args, "--enable-rebase-merge")
    else if (isFALSE(enable_rebase_merge))
        args <- c(args, "--enable-rebase-merge=false")

    if (isTRUE(enable_secret_scanning))
        args <- c(args, "--enable-secret-scanning")
    else if (isFALSE(enable_secret_scanning))
        args <- c(args, "--enable-secret-scanning=false")

    if (isTRUE(enable_secret_scanning_push_protection))
        args <- c(args, "--enable-secret-scanning-push-protection")
    else if (isFALSE(enable_secret_scanning_push_protection))
        args <- c(args, "--enable-secret-scanning-push-protection=false")

    if (isTRUE(enable_squash_merge))
        args <- c(args, "--enable-squash-merge")
    else if (isFALSE(enable_squash_merge))
        args <- c(args, "--enable-squash-merge=false")

    if (isTRUE(enable_wiki))
        args <- c(args, "--enable-wiki")
    else if (isFALSE(enable_wiki))
        args <- c(args, "--enable-wiki=false")

    if (!is.null(homepage)) {
        assert_string(homepage)
        args <- c(args, "--homepage", homepage)
    }

    if (!is.null(remove_topic)) {
        stopifnot(is.character(remove_topic))
        args <- c(args, "--remove-topic", paste(remove_topic, collapse = ","))
    }

    if (isTRUE(template))
        args <- c(args, "--template")
    else if (isFALSE(template))
        args <- c(args, "--template=false")

    if (!is.null(visibility)) {
        assert_string(visibility)
        stopifnot(visibility %in% c("public", "private", "internal"))
        args <- c(args, "--visibility", visibility)
    }

    gh_system2(args)
    invisible(NULL)
}

#' List GitHub repositories
#'
#' `gh_repo_list()` lists GitHub repositories
#'
#' @param owner Owner/organization of repositories.
#'              If `NULL` it defaults to the name of the authenticating user.
#' @param ... Ignored
#' @param archived Show only archived repositories
#' @param fields Data fields to include.  See <https://cli.github.com/manual/gh_repo_list>.
#' @param fork Show only forks
#' @param language Filter by primary coding language
#' @param limit Maximum number of repositories to list
#' @param omit_archived Omit archived repositories
#' @param omit_fork Omit forks
#' @param topic Filter by topic
#' @param visibility Filter by repository visibility.  Either `NULL`, `"public"`, `"private"`, `"internal"`
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   df <- gh_repo_list("new_name") # rename repo "new_name"
#' }
#' @seealso <https://cli.github.com/manual/gh_repo_list>
#' @export
gh_repo_list <- function(owner = NULL, ...,
                         archived = FALSE,
                         fork = FALSE,
                         language = NULL,
                         limit = 1000L,
                         fields = c("nameWithOwner", "visibility", "stargazerCount", "description"),
                         omit_fork = FALSE,
                         omit_archived = FALSE,
                         topic = NULL,
                         visibility = NULL) {
    stopifnot(!archived || !omit_archived, !fork || !omit_fork)
    if(!is.null(owner)) assert_string(owner)
    chkDots(...)
    if ("visibility" %in% fields)
        assert_min_version("2.28.0")
    args <- gh_args(
        c("repo", "list", owner,
          "--limit", as.character(as.integer(limit)),
          "--json", paste(fields, collapse = ","))
    )
    if (isTRUE(archived)) args <- c(args, "--archived")
    if (isTRUE(fork)) args <- c(args, "--fork")
    if (isTRUE(omit_archived)) args <- c(args, "--no-archived")
    if (isTRUE(omit_fork)) args <- c(args, "--source")
    if (!is.null(language)) {
        assert_string(language)
        args <- c(args, "--language", language)
    }
    if (!is.null(topic)) {
        stopifnot(is.character(topic))
        args <- c(args, "--topic", paste(topic, collapse = ","))
    }
    if (!is.null(visibility)) {
        assert_string(visibility)
        stopifnot(visibility %in% c("public", "private", "internal"))
        args <- c(args, "--visibility", visibility)
    }
    output <- gh_system2(args)
    if (length(output) == 0L) {
        df <- tibble::as_tibble()
        for (field in fields) {
            if (grepl("^is", field)) {
                df[[field]] <- logical(0L)
            } else if (grepl("^has", field) || grepl("^viewerCan") || grepl("^viewerHas")) {
                df[[field]] <- logical(0L)
            } else if (grepl("Count$", field)) {
                df[[field]] <- integer(0L)
            } else {
                df[[field]] <- character(0L)
            }
        }
    } else {
        df <- jsonlite::fromJSON(output)
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

#' Rename a GitHub repository
#'
#' `gh_repo_rename()` renames a GitHub repository.
#'
#' @param new_name New repository name.
#' @inheritParams gh_browse
#' @return `NULL` invisibly.
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_repo_rename("new_name") # rename repo "new_name"
#' }
#' @seealso <https://cli.github.com/manual/gh_repo_rename>
#' @export
gh_repo_rename <- function(new_name, ..., repo = NULL) {
    chkDots(...)
    assert_string(new_name)
    args <- gh_args(c("repo", "rename", shQuote(new_name)), repo)
    gh_system2(args)
    invisible(NULL)
}

#' Unarchive a GitHub repository
#'
#' `gh_repo_unarchive()` unarchives a GitHub repository.
#'
#' @param repository Repository to unarchive.  If `NULL` archive current repository.
#' @return `NULL` invisibly.
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_repo_unarchive() # unarchive current repo (if archived)
#' }
#' @seealso <https://cli.github.com/manual/gh_repo_unarchive>
#' @export
gh_repo_unarchive <- function(repository = NULL) {
    args <- c("repo", "unarchive", "--yes")
    if (!is.null(repository)) {
        repository <- assert_string(repository)
        args <- c(args, shQuote(repository))
    }
    gh_system2(args)
    invisible(NULL)
}

#' View details of a GitHub repository
#'
#' `gh_repo_view()` gets details of a GitHub repository
#'
#' @param repository Name of repository in `[OWNER/]REPO` format.  `OWNER` defaults to name of authenticating user.
#' @param fields Data fields to include.  See <https://cli.github.com/manual/gh_repo_view>.
#' @inheritParams gh_repo_list
#' @return `NULL` invisibly.
#' @examples
#' \dontrun{
#'   # requires `gh` installed and authenticated and working directory in Github repository
#'   gh_repo_view()
#' }
#' @seealso <https://cli.github.com/manual/gh_repo_view>
#' @export
gh_repo_view <- function(repository = NULL,
                         ...,
                         fields = c("nameWithOwner", "deleteBranchOnMerge", "description", "hasDiscussionsEnabled", "hasIssuesEnabled", "hasProjectsEnabled", "hasWikiEnabled", "homepageUrl", "isEmpty", "isFork", "mergeCommitAllowed", "visibility")) {
    chkDots(...)
    if (!is.null(repository))
        assert_string(repository)

    args <- c("repo", "view", shQuote(repository),
              "--json", paste(fields, collapse = ","))

    output <- gh_system2(args)
    l <- jsonlite::fromJSON(output)
    l <- l[fields]
    for (field in fields) {
        if (grepl("At$", field)) {
            l[[field]] <- as.POSIXct(l[[field]], format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
        }
    }
    l
}
