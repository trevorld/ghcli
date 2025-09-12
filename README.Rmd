# ghcli

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/ghcli)](https://cran.r-project.org/package=ghcli)
[![R-CMD-check](https://github.com/trevorld/ghcli/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/trevorld/ghcli/actions)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)
* [Related Links](#links)

## <a name="overview">Overview</a>

This package provides functions that wrap some of the [Github command-line interface `gh`](https://cli.github.com/) commands.

## <a name="installation">Installation</a>

```r
remotes::install_github("trevorld/ghcli")
```

You also need to install the [`gh` command](https://cli.github.com/) and
afterwards in a terminal run `gh auth login` to authenticate with your GitHub account or alternatively
set an appropriate `GITHUB_TOKEN` environmental variable.

## <a name="examples">Examples</a>

## <a name="links">Related Links</a>

* [Github CLI interface `gh`](https://cli.github.com/)

  + [GitHub CLI manual](https://cli.github.com/manual/)

* [`{gh}` R package](https://github.com/r-lib/gh)
* [`{usethis}` R package](https://github.com/r-lib/usethis)

  + [`usethis::browse_github()`](https://usethis.r-lib.org/reference/browse_github.html)
  + [`usethis::browse_github_actions()`](https://usethis.r-lib.org/reference/browse_github_actions.html)
  + [`usethis::browse_github_pulls()`](https://usethis.r-lib.org/reference/browse_github_pulls.html)
  + [`usethis::browse_github_issues()`](https://usethis.r-lib.org/reference/browse_github_issues.html)
  + [`usethis::issue_*()`](https://usethis.r-lib.org/reference/issue-this.html)
  + [`usethis::pr_*()`](https://usethis.r-lib.org/reference/pull-requests.html)
  + [`usethis::use_github_labels()`](https://usethis.r-lib.org/reference/use_github_labels.html)
  + [`usethis::use_github_release()`](https://usethis.r-lib.org/reference/use_github_release.html)
