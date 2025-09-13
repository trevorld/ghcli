as_gh_color <- function(x) {
    x <- tolower(x)
    x <- ifelse(grepl("^[0-9a-f]{6}$", x), paste0("#", x), x)
    x <- col2rgb(x) |>
        apply(2, function(x) rgb(x[1L], x[2L], x[3L], maxColorValue = 255)) |>
        tolower()
    class(x) <- "gh_color"
    x
}

#' @export
format.gh_color <- function(x, ...) {
    x <- cli::ansi_strip(x)
    ghc <- sapply(x, function(x) {
        fg <- ifelse(col2grey(x) < 128, "#FFFFFF", "#000000")
        cli::combine_ansi_styles(
            cli::make_ansi_style(x, bg = TRUE),
            cli::make_ansi_style(fg, bg = FALSE)
        )(x)
    })
    ghc <- as.character(ghc)
    class(ghc) <- "cli_ansi_string"
    ghc
}

#' @export
print.gh_color <- function(x, ...) {
    cat(paste0("<gh_color<", length(x), ">>\n"))
    if (length(x)) {
        cat(format(paste0("[", seq_along(x), "] ", format(x))), 
            sep = "\n")
    }
    invisible(x)
}

# Assumes to alpha transparency (true for Github)
#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.gh_color <- function(x, ...) {
    ghc <- format(x)
    pillar::new_pillar_shaft_simple(ghc)
}

# black is 0.0, white is 255.0
col2grey <- function(color) {
    mat <- col2rgb(color)
    # 0.2126 * red + 0.7152 * green + 0.0722 * blue # BT. 709
    # 0.299 * red + 0.587 * green + 0.114 * blue # BT. 601
    0.299 * mat[1L, ] + 0.587 * mat[2L, ] + 0.114 * mat[3L, ]
}
