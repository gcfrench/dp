# https://hypebright.nl/index.php/en/2024/09/25/s7-a-new-object-oriented-programming-system-in-r-2/

# run on loading package to register S7 methods
.onLoad <- function(...) {
  S7::methods_register()
}

#' @noRd
data_project <- S7::new_class(
  "data_project",
  properties = list(
    location = S7::new_property(
      getter = function(self) {
        here::here() |>
          as.character()
      }),
    name = S7::new_property(
      getter = function(self) {
        desc::description$new()$get("Package") |>
          as.character()
      }),
    version = S7::new_property(
      getter = function(self) {
        desc::description$new()$get("Version") |>
          as.character()
      }),
    lifecycle = S7::new_property(
      getter = function(self) {
        if(stringr::str_split(self@version, "\\.")[[1]][[1]] != '0' & 
           stringr::str_split(self@version, "\\.")[[1]][[2]] == '0') {
          "stable" |> 
            as.character()
        } else {
          "maturing" |> 
            as.character()
        }
      }
    ),
    data_project = S7::new_property(
      getter = function(self) {
        if(exists("dp")) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }),
    config = S7::new_property(
      getter = function(self) {
        if(fs::file_exists(fs::path(self@location, "config.yml"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    data = S7::new_property(
      getter = function(self) {
        if(fs::file_exists(fs::path(self@location, "data"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    databases = S7::new_property(
      getter = function(self) {
        if(fs::dir_exists(fs::path(self@location, "database"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    documentation = S7::new_property(
      getter = function(self) {
        if(fs::file_exists(fs::path(self@location, "R", glue::glue("{self@name}-package.R")))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    fme = S7::new_property(
      getter = function(self) {
        if(fs::file_exists(fs::path(self@location, "fme"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    local_version_control = S7::new_property(
      getter = function(self) {
        if(fs::file_exists(fs::path(self@location, ".git"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    magrittr_pipe = S7::new_property(
      getter = function(self) {
        if(fs::file_exists(fs::path(self@location, "R", "utils-pipe.R"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    package_version_control = S7::new_property(
      getter = function(self) {
        if(fs::file_exists(fs::path(self@location, "renv.lock"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    products = S7::new_property(
      getter = function(self) {
        if(fs::file_exists(fs::path(self@location, "NEWS.md"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    quality_assurance = S7::new_property(
      getter = function(self) {
        if(fs::file_exists(fs::path(self@location, "R", "quality_assurance.R"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    rtools = S7::new_property(
      getter = function(self) {
        devtools::find_rtools(debug = TRUE) |> 
          as.logical()
      }),
    spreadsheets = S7::new_property(
      getter = function(self) {
        if(fs::dir_exists(fs::path(self@location, "spreadsheet"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    tasks = S7::new_property(
      getter = function(self) {
        if(fs::file_exists(fs::path(self@location, "vignettes", "tasks.qmd"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    typst_documents = S7::new_property(
      getter = function(self) {
        if(fs::file_exists(fs::path(self@location, "typst_docs"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    unit_tests = S7::new_property(
      getter = function(self) {
        if(fs::file_exists(fs::path(self@location, "tests"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    ),
    vignettes = S7::new_property(
      getter = function(self) {
        if(fs::file_exists(fs::path(self@location, "vignettes"))) {
          TRUE |> as.logical()
        } else {
          FALSE |> as.logical()
        }
      }
    )
  ),
  validator = function(self) {
    if(!length(fs::dir_ls(glob = "*.Rproj")) == 1) {
      glue::glue("{self@name} is not a R project with a {self@name}.Rproj file")
    } else if(!fs::file_exists("DESCRIPTION")) {
      glue::glue("{self@name} is not a R package with a DESCRIPTION file")
    }
  }
)
# ------------------------------------------------------------------------------
#' @title access version remote control
#' 
#' @description
#' This generic function goes to GitHub to create a Personal Access Token which 
#' is then stored in the credential store when pasting this token into the console
#' message
#' 
#' @family version_control
#' 
#' @export
access_remote_version_control <- S7::new_generic("access_remote_version_control", "x")

#' @noRd
S7::method(access_remote_version_control, data_project) <- function(x) {
  
  # create GitHub Personal Access Token
  usethis::create_github_token()
  
  # store GitHub Personal Access Token
  gitcreds::gitcreds_set()
  
}

# ------------------------------------------------------------------------------
#' @title add data script
#'
#' @description
#' This generic function does the necessary setup to include data within the project
#' by running [usethis use_data_raw function] (https://usethis.r-lib.org/reference/use_data.html)
#' to add a data-raw script for the dataset and creating a data folder
#' 
#' @details
#' Data objects are saved within the data folder by running [usethis use_data function](https://usethis.r-lib.org/reference/use_data.html)
#' 
#' ROxygen comments added to function script in R folder
#' * @title TITLE
#' * @description
#' * @details
#' \preformatted{
#'   <R CODE CHUNK>
#'     DATASET_NAME |> 
#'        dplyr::select(FIELDNAMES) |> 
#'        gt::gt_preview(top_n = nrow(DATASET_NAME) |> 
#'        gt::tab_style(style = gt::cell_text(style = "italic"), 
#'                      locations = gt::cells_body(columns = name)) |> 
#'        gt::tab_options(table.align='left')
#'   <R CODE CHUNK>
#' }
#' \preformatted{
#'   @format A tibble with  <backslash> r nrow(DATASET_NAME) <backslash> rows and  
#'           <backslash> r ncol(DATASET_NAME) <backslash> variables
#'   \describe{
#'     \item{FIELDNAME}{DESCRIPTION}
#'   }
#' }
#' "DATASET NAME"
#'
#' @param name script_title character title for the data-raw script
#'
#' @export
add_data_script <- S7::new_generic("add_data_script", "x")

#' @noRd
S7::method(add_data_script, data_project) <- function(x, script_title = "Blank Data Script") {
  
  # Add data raw script
  usethis::use_data_raw(janitor::make_clean_names(script_title))
  
  # Add data folder
  if(!x@data) {
    message("data folder created in data project")
    fs::dir_create("data")
  } else {
    message("data folder already included within the data project")
  }
}

# ------------------------------------------------------------------------------
#' @title add function script
#' 
#' @description
#' This generic function adds a function script in the R folder by running 
#' [usethis use_r function](https://usethis.r-lib.org/reference/use_r.html)
#' 
#' @details
#' Roxygen comments
#' * @title TITLE
#' * @author
#' * @inherit FUNCTION_NAME ROXGYEN COMMENT
#' * @description
#' \preformatted{
#'  <backslash> r lifecycle::badge('experimental') <backslash>
#' }
#' * @details
#' * @section
#' * @inheritSection
#' * @seealso
#' * @keywords KEYWORD NAME
#' * @family FAMILY NAME
#' * @inheritParams PARENT FUNCTION NAME
#' * @param PARAMETER NAME, TYPE AND DESCRIPTION
#' * @export / @keywords internal
#' * @example MAN/EXAMPLES/EXAMPLE.R
#' \preformatted{
#'   @examples
#'    \\\dontrun{
#'      suppressPackageStartupMessages({
#'        suppressWarnings({
#'          library(PACKAGE_NAME)
#'        })
#'      })
#'      EXAMPLE SCRIPT
#'   }
#' }
#' FUNCTION NAME
#' 
#' @param name script_title character title for the function script
#' 
#' @export
add_function_script <- S7::new_generic("add_function_script", "x")

#' @noRd
S7::method(add_function_script, data_project) <- function(x, script_title = "Blank Function Script") {
  
  # Add function script
  usethis::use_r(janitor::make_clean_names(script_title))
}


# ------------------------------------------------------------------------------
#' @title add lookup script
#'
#' @description
#' This generic function adds a lookup-raw script to the data-raw folder, adding
#' the data folder is this folder does not exist
#'
#' @export
add_lookup_script <- S7::new_generic("add_lookup_script", "x")

#' @noRd
S7::method(add_lookup_script, data_project) <- function(x) {
  
  # Add lookup script
  add_data_script(dp, "lookup_raw")
}

# ------------------------------------------------------------------------------
#' @title add quarto document
#'
#' @description
#' This generic function adds a blank quarto document from the data project's templates 
#' folder into the project's quarto docs folder, creating the quarto docs folder structure 
#' if it does not already exist
#'
#' @details
#' Add the document html document link to the pkgdown's yaml file for documentation.
#'
#' @param document_title character title for document
#'
#' @keywords internal
add_quarto_document <- S7::new_generic("add_quarto_document", "x")

#' @noRd
S7::method(add_quarto_document, data_project) <- function(x, document_title = "Blank Quarto Document") {
  
  # add vignettes folder structure
  if(!x@vignettes) {
    include_vignettes(dp)
  }
    
    # Add quarto document
    replace_text(original_text = readr::read_lines(system.file("templates", "blank_quarto_document.qmd", package = "dp")),
                 new_text = glue::glue("title: {document_title}"),
                 section = "title") |> 
      readr::write_lines(fs::path("vignettes", glue::glue("{janitor::make_clean_names(document_title)}.qmd")))
 }

# ------------------------------------------------------------------------------
#' @title add quarto vignette
#'
#' @description
#' This generic function adds a blank quarto vignette from the data project's
#' templates folder into the project's vignette folder, creating the vignette docs 
#' folder structure if it does not already exist 
#'
#' @details
#' Quarto vignette documents used with installed Quarto version 1.5. Add the
#' vignettes html document link to the pkgdown's yaml file for documentation.
#'
#' @param vignette_title character title for vignette
#'
#' @export
add_quarto_vignette <- S7::new_generic("add_quarto_vignette", "x")

#' @noRd
S7::method(add_quarto_vignette, data_project) <- function(x, vignette_title = "Blank Quarto vignette") {
  
  # add vignettes folder structure
  if(!x@vignettes) {
    include_vignettes(dp)
  }
  
  # Read blank quarto text
  text <- readr::read_lines(system.file("templates", "blank_quarto_vignette.qmd", package = "dp"))
  
  # Add vignette title
  text <- replace_text(original_text = text,
                       new_text = c(glue::glue('title: "{vignette_title}"')),
                       section = "title")
  
  # Add vignette
  text <- replace_text(original_text = text,
                       new_text = c("vignette: >",
                                    stringr::str_c("  %\\VignetteIndexEntry{", vignette_title, "}"),
                                    "  %\\VignetteEngine{quarto::html}",
                                    "  %\\VignetteEncoding{UTF-8}"),
                       section = "vignette")
  
  # Add package setup chunk
  text <- append_text(original_text = text,
                      new_text = c("",
                                   "### Package setup",
                                   "```{r packages_setup}",
                                   "#| label: packages_setup",
                                   "",
                                   glue::glue("library({x@name})"),
                                   "```")) 
  
  # Write updated vignette
  text |>
    readr::write_lines(fs::path("vignettes", glue::glue("{janitor::make_clean_names(vignette_title)}.qmd")))
}

# ------------------------------------------------------------------------------
#' @title add typst document
#'
#' @description
#' This generic function adds a blank typst document from the data project's templates
#' folder into the project's typst_docs folder, , creating the typst_docs folder 
#' structure if it does not already exist 
#'
#' @param document_title character title for document
#'
#' @export
add_typst_document <- S7::new_generic("add_typst_document", "x")

#' @noRd
S7::method(add_typst_document, data_project) <- function(x, document_title = "Blank Typst Document") {
  
  # add typst folder structure
  if(!x@typst_documents) {
    include_typst_documents(dp)
  }
    
    # Read blank typst text
    text <- readr::read_lines(system.file("templates", "blank_typst_document.qmd", package = "dp"))
                              
    # Add document title
    text <- replace_text(original_text = text,
                         new_text = glue::glue("title: {document_title}"),
                         section = "title")
    
    # Add current date
    text <- replace_text(original_text = text,
                         new_text = glue::glue("date: {lubridate::today()}"),
                         section = "date")

    # Write updated typst document
    text |>
      readr::write_lines(fs::path("typst_docs", glue::glue("{janitor::make_clean_names(document_title)}.qmd")))
}

# ------------------------------------------------------------------------------
#' @title add unit tests
#'
#' @description
#' This generic function adds an unit test file into the tests folder by running 
#' [usethis use_test function](https://usethis.r-lib.org/reference/use_test.html).
#' The data project to set up to include unit tests if necessary,
#' by running [usethis use_data_raw function] (https://usethis.r-lib.org/reference/use_testthat.html).
#'
#' @details
#' Tests are created in the testthat sub-folder by running [usethis use_test function](https://usethis.r-lib.org/reference/use_r.html)
#'
#' @export
add_unit_test <- S7::new_generic("add_unit_test", "x")

#' @noRd
S7::method(add_unit_test, data_project) <- function(x, test_title = "Blank Test File") {
  
  # set up tests folders if not already done
  if(!x@unit_tests) {
    usethis::use_testthat(edition = 3)
  } else {
    message("unit testing already exists within the data project")
  }
  
  # add test file
  usethis::use_test(janitor::make_clean_names(test_title))
}

# ------------------------------------------------------------------------------
#' @title document data project
#' #'
#' @description
#' This generic function builds the site documentation in to the doc folder of the
#' data project, as created by the [pkgdown package](https://pkgdown.r-lib.org/)
#' 
#' @export
document_data_project <- S7::new_generic("document_data_project", "x")

#' @noRd
S7::method(document_data_project, data_project) <- function(x) {
  
  if(x@documentation) {
    
    # remove previous documentation
    pkgdown::clean_site()
    
    # create quarto documents including tasks
    if(!x@vignettes) {
      pkgdown::init_site()
    }
    document_all_quarto_articles(dp)
    
    # create function and lookup reference documentation
    document_all_functions_and_lookups(dp)
    
    # create products document
    document_products(dp)
    
    # create home page
    document_home(dp)
    
  } else {
    message("documentation not set up within data project")
  }
}

# ------------------------------------------------------------------------------
#' @title document all functions and lookups
#'
#' @description
#' This generic function builds all the functional and lookup documentation into
#' corresponding html documents. These documents are moved into the reference sub-folder
#' of the doc folder so that they can be included in the package documentation, created by
#' the [pkgdown package](https://pkgdown.r-lib.org/)
#' 
#' @export
document_all_functions_and_lookups <- S7::new_generic("document_all_functions_and_lookups", "x")

#' @noRd
S7::method(document_all_functions_and_lookups, data_project) <- function(x) {
  
  # include documentation if not present
  if(!x@documentation) {
    include_documentation(dp)
  }
  
  # render function and lookup documents as html
  pkgdown::build_reference()
}

# ------------------------------------------------------------------------------
#' @title document home page
#'
#' @description
#' This generic function renders the README home page to be included in the package
#' documentation, created by the [pkgdown package](https://pkgdown.r-lib.org/)
#' 
#' @export
document_home <- S7::new_generic("document_home", "x")

#' @noRd
S7::method(document_home, data_project) <- function(x) {
  
  # include documentation if not present
  if(!x@documentation) {
    include_documentation(dp)
  }
  
  # render READMe R markdown document
  rmarkdown::render("README.Rmd")
  fs::file_delete("README.html")
  
  # render home page
  pkgdown::build_home()
}

# ------------------------------------------------------------------------------
#' @title document products
#'
#' @description
#' This generic function builds the products documentation into the corresponding
#' html document. This document is moved into the news sub-folder of the doc folder
#' so that it can be included in the package documentation, created by the 
#' [pkgdown package](https://pkgdown.r-lib.org/)
#' 
#' @export
document_products <- S7::new_generic("document_products", "x")

#' @noRd
S7::method(document_products, data_project) <- function(x) {
  
  # include documentation if not present
  if(!x@documentation) {
    include_documentation(dp)
  }
  
  # render products documentation as html
  pkgdown::build_news()
}
# ------------------------------------------------------------------------------

#' @title document all quarto articles
#'
#' @description
#' This generic function builds all the quarto articles in the package's vignettes folder 
#' into corresponding html articles. These articles are moved into the articles sub-folder 
#' of the doc folder so that it can be included in package documentation, created by the 
#' [pkgdown package](https://pkgdown.r-lib.org/)
#'
#' @export
document_all_quarto_articles <- S7::new_generic("document_all_quarto_articles", "x")

#' @noRd
S7::method(document_all_quarto_articles, data_project) <- function(x) {
  
  if(x@vignettes) {
    
    # include documentation if not present
    if(!x@documentation) {
      include_documentation(dp)
    }
    
    # render rmarkdown articles as html
    pkgdown::build_articles()
    
    # console message
    message("articles created, if not already present add to _pkgdown.yml")
    
  } else {
    message("no rmarkdown articles in the data project")
  }
}

#' @title document single quarto article
#'
#' @description
#' This generic function builds a single selected quarto article in the package's vignettes folder 
#' into a corresponding html article. This article is moved into the articles sub-folder 
#' of the doc folder so that it can be included in package documentation, created by the 
#' [pkgdown package](https://pkgdown.r-lib.org/)
#'
#' @export
document_single_quarto_article <- S7::new_generic("document_single_quarto_article", "x")

#' @noRd
S7::method(document_single_quarto_article, data_project) <- function(x) {
  
  if(x@vignettes) {
    
    # include documentation if not present
    if(!x@documentation) {
      include_documentation(dp)
    }
    
    # render rmarkdown article as html
    fs::path(choose.files(default = "vignettes",
                          caption = "Select quarto article", multi = FALSE)) |>
      fs::path_ext_remove() |>
      fs::path_file() |> 
      pkgdown::build_article()
    
    # console message
    message("article created, if not already present add to _pkgdown.yml")
    
  } else {
    message("no quarto article in the data project")
  }
}

# ------------------------------------------------------------------------------
#' @title document tasks document
#'
#' @description
#' This generic function builds the tasks document in the package's vignette folder 
#' into a corresponding html article. This article is moved into the articles sub-folder 
#' of the doc folder so that it can be included in package documentation, created by the 
#' [pkgdown package](https://pkgdown.r-lib.org/)
#'
#' @export
document_tasks <- S7::new_generic("document_tasks", "x")

#' @noRd
S7::method(document_tasks, data_project) <- function(x) {
  
  # tasks --------------------------------------------------------------------
  if(!x@tasks) {
    include_tasks(dp)
  }
  
  # document single quarto tasks article
  pkgdown::build_article("tasks")
}

# ------------------------------------------------------------------------------
#' @title include config
#'
#' @description
#' This generic function does the necessary setup to include config file within the
#' project, running [usethis use_package function](https://usethis.r-lib.org/reference/use_package.html)
#' to add config package to the DESCRIPTION file and creating config and .Renviron files
#' within the project. The config file is initially set up to run in a development environment.
#'
#' @details
#' The config file can be changed to a production environment by updating the R_CONFIG_ACTIVE
#' setting in .Renviron file to "production"
#'
#' @export
include_config <- S7::new_generic("include_config", "x")

#' @noRd
S7::method(include_config, data_project) <- function(x)  {
  
  if(!x@config) {
    # Add config package to DESCRIPTION
    usethis::use_package("config")
  
    # Append text into config file
    append_text(original_text = vector() |> as.character(),
                new_text = c("default:",
                             "# local",
                             glue::glue('  local_folder: "{fs::path_dir(x@location)}"'),
                             "# network",
                             "", 
                             "development:", 
                             "", 
                             "production:")) |> 
      readr::write_lines("config.yml")
    
    # Append text in .Renviron file
    append_text(original_text = vector() |> as.character(),
      new_text = c('R_CONFIG_ACTIVE = "development"')) |> 
      readr::write_lines(".Renviron")
    
    # Append text to .gitignore file
    if(fs::file_exists(".gitignore")) {
      append_text(original_text = readr::read_lines(".gitignore"),
                  new_text = c("config.yml",
                               "*.Renviron")) |> 
        readr::write_lines(".gitignore")
    }
  } else {
    message("config and .Renviron files already included within the data project")
  }
}

# ------------------------------------------------------------------------------
#' @title include connections to database and SQL queries
#'
#' @description
#' This generic function does the necessary setup to include connecting to databases
#' and SQL queries within the project
#' 
#' @details
#' Database functions are added to securely store database connections in the Windows credential store, 
#' using the [keyring](https://keyring.r-lib.org/) package, and create connection strings for SQL Server, 
#' mySQL and PostGres databases
#'
#' @export
include_databases <- S7::new_generic("include_databases", "x")

#' @noRd
S7::method(include_databases, data_project) <- function(x) {
  
  if(!x@databases) {
    
    # set up database folder
    message("database folder created in data project")
    fs::dir_create("database")
    
    # copy database functions into R folder
    fs::file_copy(system.file("scripts", "database.R", package = "dp"),
                  fs::path("R", "database.R"), 
                  overwrite = TRUE)
    
    # Add database functions to _pkgdown file
    if(x@documentation) {
      replace_text(original_text = readr::read_lines(fs::path("_pkgdown.yml")),
                   new_text = pkgdown_database_section(),
                   section = "reference") |> 
        readr::write_lines(fs::path("_pkgdown.yml"))
    }
    
    # create database documentation
    devtools::document()
  } else {
    message("database folder and functions already included within the data project")
  }
}

# ------------------------------------------------------------------------------
#' @title include documentation
#'
#' @description
#' This generic function does the necessary setup to allow package documentation
#' by running [usethis use_package_doc function] (https://usethis.r-lib.org/reference/use_package_doc.html)
#'
#' @details
#' In addition the following are added
#'
#' * README.Rmd by running the [usethis use_readme_rmd function](https://usethis.r-lib.org/reference/use_readme_rmd.html)
#' and [devtools build_readme function](https://devtools.r-lib.org/reference/build_rmd.html)
#' * NEWS.md by running the [usethis use_news_md function](https://usethis.r-lib.org/reference/use_news_md.html)
#' * [lifecycle](https://lifecycle.r-lib.org/) image files are added to the figures sub-folder of the man folder by running the [usethis use_lifecycle function](https://usethis.r-lib.org/reference/use_lifecycle.html)
#' 
#' @export
include_documentation <- S7::new_generic("include_documentation", "x")

#' @noRd
S7::method(include_documentation, data_project) <- function(x) {
  
  # set up documenation
  if(!x@documentation) {
    
    # add package.R file
    usethis::use_package_doc()
    
    # add man folders and subfolders -------------------------------------------
    message("man folder and subfolders created in data project")
    fs::dir_create("man")
    fs::dir_create(fs::path("man", "examples"))
    fs::dir_create(fs::path("man", "figures"))
  
    # ReadMe -------------------------------------------------------------------
    usethis::use_readme_rmd()
    
    # ReadMe file from templates folder
    fs::file_copy(system.file("templates", "README.Rmd", package = "dp"),
                  fs::path("README.Rmd"),
                  overwrite = TRUE)
    
    # Build README markdown document
    devtools::build_readme()
    
    # add products document populated with initial version ----------------------------------
    usethis::use_news_md()
    
    # update products document with date of initial project created
    append_text(original_text = "NEWS.md",
                new_text = (c(glue::glue('# {x@name} 0.0.0.9000 <font size="4">{lubridate::today()}</font>'),
                              "",
                              "-   Initial project created")
                ) |> 
                  readr::write_lines("NEWS.md"))
    
    # tasks --------------------------------------------------------------------
    if(!x@tasks) {
      include_tasks(dp)
    }
    
    # pkgdown YAML file --------------------------------------------------------
    
    ## initiate pkgdown
    usethis::use_pkgdown()
    pkgdown::init_site()
    
    ## Read blank _pkgdown.yml file
    text <- readr::read_lines(system.file("templates", "_pkgdown.yml", package = "dp"))
    
    ## Add _pkgdown.yml title
    text <- replace_text(original_text = readr::read_lines(system.file("templates", "_pkgdown.yml", package = "dp")),
                         new_text = c(glue::glue("title: {x@name}")),
                         section = "title")
    
    # Add data quality functions to _pkgdown.yml
    if(x@quality_assurance) {
      text <- replace_text(original_text = text,
                           new_text = pkgdown_quality_assurance_section(),
                           section = "reference")
    }
    
    # Add database functions to _pkgdown.yml
    if(x@databases) {
      text <- replace_text(original_text = text,
                           new_text = pkgdown_database_section(),
                           section = "reference")
    }
    
    ## Write updated _pkgdown.yml file
    text |> 
      readr::write_lines("_pkgdown.yml")
    
    # add lifecycle package and figures ----------------------------------------
    usethis::use_lifecycle()

  } else {
    message("documentation already set up within the data project")
  }
}

# ------------------------------------------------------------------------------
#' @title include FME workspaces and custom transformers
#'
#' @description
#' This generic function does the necessary setup to include FME workspaces and custom transformers
#'  within the project
#'
#' @export
include_fme <- S7::new_generic("include_fme", "x")

#' @noRd
S7::method(include_fme, data_project) <- function(x) {
  
  # set up FME folders
  if(!x@fme) {
    message("fme folder and subfolders created in data project")
    fs::dir_create("fme")
    fs::dir_create(fs::path("fme", "CoordinateSystemExceptions"))
    fs::dir_create(fs::path("fme", "CoordinateSystemGridOverrides"))
    fs::dir_create(fs::path("fme", "CoordinateSystems"))
    fs::dir_create(fs::path("fme", "CsmapTransformationExceptions"))
    fs::dir_create(fs::path("fme", "Formats"))
    fs::dir_create(fs::path("fme", "ProjData"))
    fs::dir_create(fs::path("fme", "TransformerCategories"))
    fs::dir_create(fs::path("fme", "Transformers"))
    fs::dir_create(fs::path("fme", "Workspaces"))
  } else {
    message("fme folder already included within the data project")
  }
}

# ------------------------------------------------------------------------------
#' @title include local version control
#' 
#' @description
#' This generic function adds the data project to a Git repository, updating the
#' .gitignore file with files to ignore in version control and allows an initial
#' commit to this repository.
#' 
#' @family version_control
#' 
#' @details
#' In the Terminal check Git installed on local machine
#' * where git
#' * git --version
#' 
#' In the Terminal check Git credentials are present
#' * git config --global --list
#' 
#' Set Git credentials if not present
#' * usethis::use_git_config(user.name = "USERNAME", user.email = "EMAIL")
#' 
#' In RStudio check Git Global options
#' * version control interface for RStudio projects is ticked
#' * Git executable path is set to C:/Program Files/Git/bin/git.exe
#' 
#' 
#' @export
include_local_version_control <- S7::new_generic("include_local_version_control", "x")

#' @noRd
S7::method(include_local_version_control, data_project) <- function(x) {
  
  if(!x@local_version_control) {
    # Add git repository
    message(".git folder created in data project")
    usethis::use_git()
    
    # add config.yml and .Renviron files to .gitignore
    usethis::use_git_ignore(c("config.yml", ".Renviron"))
  } else {
    message("Git repository already exists for the data project")
  }
}

# ------------------------------------------------------------------------------
#' @title include magrittr pipe
#'
#' @description
#' This generic function does the necessary setup to allow the magrittr pipe to
#' be used in the package by running [usethis use_pipe function](https://usethis.r-lib.org/reference/use_pipe.html)
#'
#' @export
include_magrittr_pipe <- S7::new_generic("include_magrittr_pipe", "x")

#' @noRd
S7::method(include_magrittr_pipe, data_project) <- function(x){

  # use magrittr function
  if(!x@magrittr_pipe) {
    usethis::use_pipe()

  # document magrittr function
    devtools::document()
    
  } else {
    message("magrittr pipe already included within the data project")
  }
}

# ------------------------------------------------------------------------------
#' @title include package version control using renv
#' 
#' @description
#' This generic function creates a renv lock file storing package versions used within
#' the project
#' 
#' @keywords internal
include_package_version_control <- S7::new_generic("include_package_version_control", "x")

#' @noRd
S7::method(include_package_version_control, data_project) <- function(x) {
    
    # create renv lock file
    renv::snapshot(prompt = FALSE)
}

# ------------------------------------------------------------------------------
#' @title include quality assurance
#' 
#' @description
#' This generic function adds the quality assurance functions, with example penguin
#' dataset, used in validation and quality assurance of dataset outputs
#' 
#' @export
include_quality_assurance <- S7::new_generic("include_quality_assurance", "x")

S7::method(include_quality_assurance, data_project) <- function(x) {
  
  if(!x@quality_assurance) {
    
    # copy quality assurance functions from templates folder
    fs::file_copy(system.file("scripts", "quality_assurance.R", package = "dp"),
                  fs::path("R", "quality_assurance.R"),
                  overwrite = TRUE)
    
    # copy penguin example from templates folder
    ## add man folders and subfolders if not present
    if(!fs::dir_exists(fs::path("man", "examples"))) {
      message("examples folder created in man folder")
      fs::dir_create(fs::path("man", "examples"))
    }
    
    fs::file_copy(system.file("scripts", "heaviest_penguins.R", package = "dp"),
                  fs::path("man", "examples", "heaviest_penguins.R"),
                  overwrite = TRUE)
    
    usethis::use_package("palmerpenguins", "Suggests", min_version = "0.1.0")
    
    # Add quality assurance functions to _pkgdown file
    if(x@documentation) {
      replace_text(original_text = readr::read_lines(fs::path("_pkgdown.yml")),
                   new_text = pkgdown_quality_assurance_section(),
                   section = "reference") |> 
        readr::write_lines(fs::path("_pkgdown.yml"))
    }
    
    # document quality assurance functions
    devtools::document()
  }
}

# ------------------------------------------------------------------------------
#' @title include quarto documents
#'
#' @description
#' This generic function does the necessary setup to include quarto documents and
#' quarto yaml file within the project
#'
#' @keywords internal
include_quarto_documents <- S7::new_generic("include_quarto_documents", "x")

#' @noRd
S7::method(include_quarto_documents, data_project) <- function(x) {
  
  # set up vignettes folders
  if(!x@vignettes) {
    include_vignettes(dp)
  } else {
    message("vignettes folder already included within the data project")
  }
  
  # copy quarto yaml file
  if(!fs::file_exists(path("vignettes", "_quarto.yml"))) {
    fs::file_copy(system.file("templates", "_quarto.yml", package = "dp"),
                  fs::path("vignettes", "_quarto.yml"))
  } else {
    message("_quarto file already included within the data project")
  }
}

# ------------------------------------------------------------------------------
#' @title include remote version control
#' 
#' @description
#' This generic function adds the data project to a new GitHub repository and makes
#' an initial commit
#' 
#' @family version_control
#' 
#' @export
include_remote_version_control <- S7::new_generic("include_remote_version_control", "x")

#' @noRd
S7::method(include_remote_version_control, data_project) <- function(x) {
  
  if(x@local_version_control) {
    # create new GitHub data project repository
    usethis::use_github()
    message("remote data project repository created on GitHub")
  } else {
    message("Local version control required for the data project")
  }
  
  # create new GitHub data project repository
  usethis::use_github()
  message("remote data project repository created on GitHub")
}

# ------------------------------------------------------------------------------
#' @title include spreadsheets
#'
#' @description
#' This generic function does the necessary setup to include spreadsheets within the project
#'
#' @export
include_spreadsheets <- S7::new_generic("include_spreadsheets", "x")

#' @noRd
S7::method(include_spreadsheets, data_project) <- function(x) {
  
  # set up spreadsheet folder
  if(!x@spreadsheets) {
    message("spreadsheet created in data project")
    fs::dir_create("spreadsheet")
  } else {
    message("spreadsheet folder already included within the data project")
  }
}

# ------------------------------------------------------------------------------
#' @title include tasks
#'
#' @description
#' This generic function copies the tasks quarto document from the data project's
#' templates folder in the project's vignettes folder, creating the vignettes folder structure 
#' if it does not already exist. The current date is added as a header to the tasks document
#'
#' @details
#' Add the tasks html document link to the pkgdown's yaml file for documentation.
#'
#' @keywords internal
include_tasks <- S7::new_generic("include_tasks", "x")

#' @noRd
S7::method(include_tasks, data_project) <- function(x) {
  
  # add vignettes folder structure
  if(!x@vignettes) {
    include_vignettes(dp)
  }
  
  if(!x@tasks) {
    # add current date to tasks file
    append_text(original_text = readr::read_lines(system.file("templates", "tasks.qmd", package = "dp")),
                new_text = c(glue::glue("### {lubridate::today()}"))) |> 
      readr::write_lines(fs::path("vignettes", "tasks.qmd"))
  } else {
    message("tasks document already included within the data project")
  }
}

# ------------------------------------------------------------------------------
#' @title include typst documents
#'
#' @description
#' This generic function does the necessary setup to include tyspt documents within the project
#'
#' @keywords internal
include_typst_documents <- S7::new_generic("include_typst_documents", "x")

#' @noRd
S7::method(include_typst_documents, data_project) <- function(x) {
  
  # set up typst documents folders
  if(!x@typst_documents) {
    message("typst docs folder and subfolder created in data project")
    fs::dir_create("typst_docs")
    fs::dir_create(fs::path("typst_docs", "images"))
  } else {
    message("typst docs folder already exists within the data project")
  }
}

# ------------------------------------------------------------------------------
#' @title include vignettes
#'
#' @description
#' This generic function does the necessary setup to include vignette Rmarkdown
#' documents within the project
#'
#' @details
#' Vignettes are created in the vignettes sub-folder by running [usethis use_vignette function](https://usethis.r-lib.org/reference/use_vignette.html)
#'
#' @keywords internal
include_vignettes <- S7::new_generic("include_vignettes", "x")

#' @noRd
S7::method(include_vignettes, data_project) <- function(x) {
  
  # set up vignettes folders
  if(!x@vignettes) {
    message("vignettes folder and subfolder created in data project")
    fs::dir_create("vignettes")
    fs::dir_create(fs::path("vignettes", "images"))
    
    # insert Vignette Builder section in DESCRIPTION
    insert_text(original_text = readr::read_lines("DESCRIPTION"),
                new_text = c("VignetteBuilder: ", 
                             "   quarto"), 
                section = "License") |> 
      readr::write_lines("DESCRIPTION")
  } else {
    message("vignettes folder already exists within the data project")
  }
}

# ------------------------------------------------------------------------------
#' @title increment data project's version
#' 
#' @description
#' This generic function increments the data project's version level
#' 
#' @details
#' The version level may be incremented by one of three levels
#' * patch 0.0.1 to 0.0.2
#' * minor 0.0.1 to 0.1.0
#' * major 0.0.1 to 1.0.0
#' 
#' @param version_type character level of version to increment (patch (default), minor or major)
#' 
#' @keywords internal
increment_version <- S7::new_generic("increment_version", "x")

#' @noRd
S7::method(increment_version, data_project) <- function(x, version_type = "patch") {
  
  # increment version
  usethis::use_version(which = version_type)
}

#' @title increment data project's major version
#' 
#' @description
#' This generic function increments the data project's major version level,
#' with message to update lifecycle badge
#' 
#' @export
increment_major_version <- S7::new_generic("increment_major_version", "x")

#' @noRd
S7::method(increment_major_version, data_project) <- function(x) {
  
  # increment version
  increment_version(dp, version_type = "major")
  
  # update lifecycle
  usethis::use_lifecycle_badge("stable")
}

#' @title increment data project's minor version
#' 
#' @description
#' This generic function increments the data project's minor version level, with 
#' with message to update lifecycle badge
#' 
#' @export
increment_minor_version <- S7::new_generic("increment_minor_version", "x")

S7::method(increment_minor_version, data_project) <- function(x) {
  
  # increment minor version
  increment_version(dp, version_type = "minor")
  
  # update lifecycle
  message("Copy and paste the following lines into README:
  <!-- badges: start -->
  [![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
  <!-- badges: end -->")
}

# ------------------------------------------------------------------------------
#' @title open document
#' 
#' @description
#' This generic function opens the specified project document in RStudio for editing
#' 
#' @param file_path character path to document within the project directory
#' 
#' @keywords internal
open_document <- S7::new_generic("open_document", "x")

#' @noRd
S7::method(open_document, data_project) <- function(x, document_path = NA_character_) {
  
  # open project file for opening
  usethis::edit_file(document_path)
  
}

#' @title open config file
#'
#' @description
#' This generic function opens the config and .Renviron files in RStudio for editing
#' 
#' @export
open_config <- S7::new_generic("open_config", "x")

#' @noRd
S7::method(open_config, data_project) <- function(x) {
  
  #.Renviron
  if(fs::file_exists(".Renviron")) {
    open_document(dp, document_path = ".Renviron")
  } else {
    message(".Renviron document does not exist in this data project")
  }
  
  # config.yml
  if(fs::file_exists("config.yml")) {
    open_document(dp, document_path = "config.yml")
  } else {
    message("config yaml document does not exist in this data project")
  }
}

#' @title open DESCRIPTION document
#'
#' @description
#' This generic function opens the DESCRIPTION document in RStudio for editing
#' 
#' @export
open_description <- S7::new_generic("open_description", "x")

#' @noRd
S7::method(open_description, data_project) <- function(x) {
  
  if(fs::file_exists("DESCRIPTION")) {
    open_document(dp, document_path = "DESCRIPTION")
  } else {
    message("DESCRIPTION document does not exist in this data project")
  }
}

#' @title open documentation
#' 
#' @description
#' This generic function opens the data project's documentation in a web browser
#' 
#' @export
open_documentation <- S7::new_generic("open_documentation", "x")

#' @noRd
S7::method(open_documentation, data_project) <- function(x) {
  
  if(fs::file_exists("docs/index.html")) {
    browseURL("docs/index.html")
  } else {
    message("documentation does not exits for this data project")
  }
}
  
#' @title open home document
#'
#' @description
#' This generic function opens the README.Rmd document in RStudio for editing
#' 
#' @export
open_home <- S7::new_generic("open_home", "x")

#' @noRd
S7::method(open_home, data_project) <- function(x) {
  
  if(fs::file_exists("README.Rmd")) {
    open_document(dp, document_path = "README.Rmd")
  } else {
    message("README.Rmd document does not exist in this data project")
  }
}

#' @title open pkgdown document
#'
#' @description
#' This generic function opens the _pkgdown document in RStudio for editing
#' 
#' @export
open_pkgdown <- S7::new_generic("open_pkgdown", "x")

#' @noRd
S7::method(open_pkgdown, data_project) <- function(x) {
  
  if(fs::file_exists("_pkgdown.yml")) {
    open_document(dp, document_path = "_pkgdown.yml")
  } else {
    message("_pkgdown.yml document does not exist in this data project")
  }
}

#' @title open product document
#'
#' @description
#' This generic function opens the product document in RStudio for editing
#' 
#' @export
open_products <- S7::new_generic("open_products", "x")

#' @noRd
S7::method(open_products, data_project) <- function(x) {
  
  if(fs::file_exists("NEWS.md")) {
    open_document(dp, document_path = "NEWS.md")
  } else {
    message("NEWS.md document does not exist in this data project")
  }
}

#' @title open quarto document
#'
#' @description
#' This generic function opens the quarto yml document in RStudio for editing
#' 
#' @keywords internal
open_quarto <- S7::new_generic("open_quarto", "x")

#' @noRd
S7::method(open_quarto, data_project) <- function(x) {
  
  if(fs::file_exists(fs::path("vignettes", "_quarto.yml"))) {
    open_document(dp, document_path = fs::path("vignettes", "_quarto.yml"))
  } else {
    message("_quarto.yml document does not exist in this data project")
  }
}

#' @title open tasks document
#'
#' @description
#' This generic function opens the tasks document in RStudio for editing
#' 
#' @export
open_tasks <- S7::new_generic("open_tasks", "x")

#' @noRd
S7::method(open_tasks, data_project) <- function(x) {
  
  if(fs::file_exists(fs::path("vignettes", "tasks.qmd"))) {
    open_document(dp, document_path = fs::path("vignettes", "tasks.qmd"))
  } else {
    message("tasks document does not exist in this data project")
  }
}

# ------------------------------------------------------------------------------
#' @title refresh imports package list
#' 
#' @description
#' This generic function refreshes the package list in the Imports section of the
#' DESCRIPTION file by using the [attachment att_amend_desc](https://thinkr-open.github.io/attachment/reference/att_amend_desc.html) 
#' function to scan through the R scripts in the R directory, vignettes in the vignettes directory 
#' and tests in the tests directory
#' 
#' @keywords internal
refresh_imports_package_list <- S7::new_generic("refresh_imports_package_list", "x")

#' @noRd
S7::method(refresh_imports_package_list, data_project) <- function(x) {
  
  # refresh DESCRIPTION Imports package list
  attachment::att_amend_desc()
}

# ------------------------------------------------------------------------------
#' @title sync local version control
#'
#' @description
#' This generic function stages and commits data project files to the git repository
#' 
#' @family version_control
#' 
#' @details
#' Alternatively use the Terminal to sync with version control
#' * git -add -A
#' * git commit -m "COMMIT MESSAGE"
#' 
#' @export
sync_local_version_control <- S7::new_generic("sync_local_version_control", "x")

#' @noRd
S7::method(sync_local_version_control, data_project) <- function(x) {
  
  if(x@local_version_control) {
    # stage all files to git
    gert::git_add(files = ".")
    
    # git commit message
    commit_message  <- ""
    while(commit_message == "") {
      commit_message <- readline(prompt = "Commit message : ")
    }
    
    # commit all files to git
    gert::git_commit(message = commit_message)
    
    # alternate commit message
    message("data project files sync'd with version control")
  } else {
    message("Git repository does not exists for this data project")
  }
}

# ------------------------------------------------------------------------------
#' @title activate data project
#'
#' @description
#' This function creates a new data project class instance in the global environment,
#' updating the project's version
#'
#' @details
#' Data project properties
#' * location - read only location path to the data project
#' * name - read only name of the data project
#' * version - read only version of the project
#' * data_project - read only data project class exists in global environment
#' * config - project set up to include the use of the config file to store path within the project
#' * data - project set up to include data
#' * databases - project set up to include the connection to databases and use of SQL queries
#' * documentation - project set up to include package documentation
#' * fme - project set up to include the use of FME workspaces and custom transformers
#' * local_version_control - project set up to use Git as version control system
#' * magrittr_pipe - project set up to use the magrittr's pipe, %>% in the package
#' * package_version_control uses [renv](https://rstudio.github.io/renv/) lock file to manage the package versions within the project
#' * quality_assurance functions used for the validation and quality assurance of datasets
#' * rtools - read only check that the correct version of rtools is available on the local machine
#' * spreadsheets - project set up to include the use of spreadsheets
#' * tasks - project uses tasks quarto document to list tasks for the project
#' * typst_documents - project set up to include typst documents
#' * unit_tests - project set up to include function unit tests using the [testthat package](https://testthat.r-lib.org/)
#' * vignettes - project set up to include vignette quarto documents
#'
#' Data project generic functions and methods
#' * access_version_control - create and store a GitHub Personal Access token in the local credential store
#' * add_data_script - includes the use of data within the project, adding a data-raw script for the dataset(s)
#' * add_function_script - adds a functions script in the R folder, including suggested Roxygen comments
#' * add_lookup_script - adds a lookup-raw script to the data-raw folder
#' * add_quarto_vignette - adds a blank Quarto vignette to the vignettes folder
#' * add_typst_document - adds a blank Typst document to the typst_docs folder
#' * add_unit_test - setups the data project to use unit testing, adds a unit test file to the tests folder
#' * document_all_functions_and_lookups - converts all the functions and lookup documentation into corresponding
#'  html files for documentation
#' * create_new_package - opens a new package form in RStudio to run and fill in to create a new R package 
#' within the data project
#' * document_data_project - updates the data project's documentation
#' * document_home - renders the home page into corresponding markdown document for documentation
#' * document_products - converts products document into corresponding html file for documentation
#' * document_all_functions_and_lookups - converts all the functions and lookups markdown documents into
#' corresponding html files for documentation
#' * document_all_quarto_articles - converts all quarto articles to corresponding html files for documentation
#' * document_single_quarto_article - converts single quarto article to corresponding html file for documentation
#' * document_tasks - converts tasks quarto article to corresponding html file for documentation
#' * include_config - included the use of the config file within the project
#' * include_databases - includes the use of connecting to databases and SQL queries within the project
#' * include_documentation - includes documentation in the project
#' * include_fme - includes the use of FME workspaces and custom transformers within the project
#' * include_local_version_control - creates a Git repository for the project
#' * include_magrittr_pipe - includes the use of the magrittr pipe in the project
#' * include_quality_assurance - adds functions used for the validation and quality assurance of datasets
#' * include_remote_version_control - craetes a GitHub repository for the project
#' * include_spreadsheets - includes the use of spreadsheets within the project
#' * increment_major_version - increments major level of data project version, with message to update lifecycle badge
#' * increment_minor_version - increments minor level of data project version, with message to update lifecycle badge
#' * open_config - opens the config yaml and .Renviron files in RStudio for editing
#' * open_description - opens the DESCRIPTION document in RStudio for editing
#' * open_home - opens the README Rmarkdown document in RStudio for editing
#' * open_pkgdown - opens the _pkgdown document in RStudio for editing
#' * open_products - opens the NEWS document in RStudio for editing
#' * open_tasks - opens the tasks document in RStudio for editing
#' * sync_version_control - stages and commits data project files to the git repository
#'
#' @return data project class
#' @export
activate <- function()  {
  
  if (!exists("dp")) {
    
    # create dp class instance
    dp <- data_project()
    assign("dp", dp, envir = globalenv())
    
    # update version
    increment_version(dp)
    
  } else {
    message("data project already activated")
  }
}

# ------------------------------------------------------------------------------
#' @title create new package
#'
#' @description
#' This function opens a new package form in RStudio to run and fill in to create
#' a new R package within the data project
#' 
#' @export
create_new_package <- function() {
  
  if(!fs::file_exists("DESCRIPTION")) {
    # copy create_new_package form into data project
    fs::file_copy(system.file("forms", "create_new_package.qmd", package = "dp"),
                  fs::path("create_new_package.qmd"),
                  overwrite = TRUE)
    
    # open create_new_package form
    usethis::edit_file("create_new_package.qmd")
  } else {
    message("data project is already a R package")
  }
}

# ------------------------------------------------------------------------------
#' @title deactivate data project
#' 
#' @description
#' This function updates the data project prior to copying it and removing the data
#' project class instance from the global environment
#' * creating or updating a renv lock file to store data project's package dependencies versions
#' * refreshing the list of package dependencies in package's description file
#' 
#' @details
#' The data project can be unzipped and copied onto a different machine, using the
#' [renv](https://rstudio.github.io/renv/) package to manage the data project's
#' packages
#' * renv::activate()
#' * renv::restore(prompt = FALSE))
#' 
#' @export
deactivate <- function() {
  
  if (exists("dp")) {
    
    # remove package set up files
    if(fs::file_exists("create_new_package.qmd")) {
      fs::file_delete("create_new_package.qmd")
    }
    
    if(fs::file_exists("create_new_package.html")) {
      fs::file_delete("create_new_package.html")
    }
    
    # delete empty folders in vignettes
    fs::dir_ls("vignettes", type = "directory") |> 
      purrr::walk(function(.x){
        if(length(fs::dir_ls(.x)) == 0) {
          message(glue::glue("{.x} folder deleted in vignettes folder"))
          fs::dir_delete(.x)
        }
      })
    
    # delete .quarto folder in vignettes
    if(fs::dir_exists("vignettes/.quarto")) {
      message(".quarto folder deleted in vignettes folder")
      fs::dir_delete("vignettes/.quarto")
    }
    
    # create folders if they have been deleted
    if(!fs::dir_exists("vignettes/archive")) {
      message("archive folder created in vignettes folder")
      fs::dir_create("vignettes/archive")
    }
    if(!fs::dir_exists("vignettes/images")) {
      message("images folder created in vignettes folder")
      fs::dir_create("vignettes/images")
    }
    
    # update renv lock file
    include_package_version_control(dp)
    
    # check and add missing package dependencies to description file
    refresh_imports_package_list(dp)
    
    # zip data project
    zip_file_location <- glue::glue('{dp@name}_{dp@version}_{lubridate::today() |>
                                                stringr::str_remove_all("-")}.zip')
    dir_name <- dp@name
    zip::zip(zipfile = zip_file_location,
             files = c(dir_name),
             root = fs::path_dir(here::here()))
    
    # remove dp class instance
    rm(dp, envir = globalenv())
    
  } else {
    message("data project not activated")
  }
}

# ------------------------------------------------------------------------------
#' @title update text file
#' 
#' @description
#' This private function appends or inserts text in an original file,
#' immediately following a specified file section.
#' 
#' @param original_text vector containing the original text, created by 
#' [readr read_lines](https://readr.tidyverse.org/reference/read_lines.html) function
#' @param new_text vector containing new text to append or insert
#' @param section character text of section name before the colon, to insert after. 
#' @param combine_type character text indicating type of combination ("append", "insert")
#'
#' @return vector combining new vector text in original text
#' @keywords internal
update_text <- function(combine_type) {
  
  function(original_text, new_text, section = NA_character_, ...) {
    
    # insert text at specified section -----------------------------------------
    insert_text <- function () {
      
      # get size of original vector
      original_text_size <- original_text |> 
        vctrs::vec_size()
      
      # get location of middle vector matched to text
      middle_location <- vctrs::vec_match(section, 
                                          stringr::str_split(original_text, ":", simplify = TRUE)[, 1])
      
      # combine vector if section is present in original vector
      if(!is.na(middle_location)) {
        
        # chop original vector in parts, top, middle and bottom
        if(middle_location != 1) {
          middle_at_top <- FALSE
          original_text_parts <- original_text |> 
            vctrs::vec_chop(indices = list(top = 1:(middle_location - 1), 
                                           middle = middle_location, 
                                           bottom = (middle_location + 1):original_text_size))
        } else {
          middle_at_top <- TRUE
          original_text_parts <- original_text |> 
            vctrs::vec_chop(indices = list(middle = middle_location, 
                                           bottom = (middle_location + 1):original_text_size))
        }
        
        # original top part
        original_text_top_part <- original_text_parts[1 - middle_at_top] |> 
          vctrs::list_unchop()
        
        # original middle part
        original_text_middle_part <- original_text_parts[2 - middle_at_top] |> 
          vctrs::list_unchop()
        
        # original bottom part
        original_text_bottom_part <- original_text_parts[3 - middle_at_top] |> 
          vctrs::list_unchop()
        
        # combine original and vectors
        switch(combine_type,
               insert = vctrs::vec_c(original_text_top_part,  
                                     original_text_middle_part,
                                     new_text, 
                                     original_text_bottom_part),
               replace = vctrs::vec_c(original_text_top_part,
                                      new_text, 
                                      original_text_bottom_part)
        )
      } else {
        message("WARNING: Section not found in original vector, new vector appended to bottom of original vector")
        vctrs::vec_c(original_text, new_text)
      }
    }
    
    # append text to end of document -------------------------------------------
    append_text <- function () {
      vctrs::vec_c(original_text, new_text)
      
    }
    
    # run appropriate text function
    switch(combine_type,
           insert = insert_text(),
           append = append_text(),
           replace = insert_text()
    )
    
  }
}

# ------------------------------------------------------------------------------
#' @title append text
#' 
#' @description
#' This function calls the update text function to append text to a text file
#'
#' @inheritParams update_text
#' 
#' @inherit update_text return
#' @keywords internal
append_text <- update_text(combine_type = "append")

# ------------------------------------------------------------------------------
#' @title insert text
#' 
#' @description
#' This function calls the update text function to insert text into a text file
#'
#' @inheritParams update_text
#' 
#' @inherit update_text return
#' @keywords internal
insert_text <- update_text(combine_type = "insert")

# ------------------------------------------------------------------------------
#' @title replace text
#' 
#' @description
#' This function calls the update text function to replace text in a text file
#'
#' @inheritParams update_text
#' 
#' @inherit update_text return
#' @keywords internal
replace_text <- update_text(combine_type = "replace")

# ------------------------------------------------------------------------------
#' @title pkgdown database section text
#'
#' @description
#' Vector containing the database section reference text to be inserted into the
#' _pkgdown.yml file
#' 
#' @return vector containing database section reference text
#' @keywords internal 
pkgdown_database_section <- function() {
  c("reference:",
    '- title: "Store credentials"',
    '  desc: "Securely store and retrive database connections using the keyring package"',
    "  contents:",
    "  - add_key",
    "  - create_locked_keyring",
    "  - delete_key",
    "  - delete_keyring",
    "  - get_keyring_status",
    "  - list_keyrings",
    "  - list_keys",
    "  - lock_keyring",
    "  - unlock_keyring",
    "  - view_key",
    '- title: "Database connections"',
    '  desc: "Connect to databases"',
    "  contents:",
    "  - get_mysql_connection",
    "  - get_postgres_connection",
    "  - get_sqlserver_connection")
}

# ------------------------------------------------------------------------------
#' @title pkgdown quality assurance section text
#'
#' @description
#' Vector containing the quality assurance section reference text to be inserted 
#' into the _pkgdown.yml file
#' 
#' @return vector containing quality_assurance section reference text
#' @keywords internal 
pkgdown_quality_assurance_section <- function() {
  c("reference:",
    '- title: "Quality assurance"',
    '  desc: "Functions and tables used in quality assurance"',
    "  contents:",
    "  - compare_dataset_versions",
    "  - unnest_failed_validation_results")
}

# ------------------------------------------------------------------------------
#' @title code example
#'
#' @description
#' A code example pasted into RStudio and added to the RStudio Addins, as described
#' in [RStudio Addins](https://rstudio.github.io/rstudioaddins/)
#' 
#' @details
#' This code example function is automatically added into the RStudio's Addins list
#' by adding the following fields in inst/rstudio/addins.dcf file
#' 
#' Name: Name of code example
#' Description: Description of code example
#' Binding: Code example function name
#' Interactive: false
#' 
#' @export
code_example <- function() {
  rstudioapi::insertText("CODE EXAMPLE TEXT WRITTEN HERE")
}
