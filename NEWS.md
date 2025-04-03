# dp 0.4.9

# dp 0.4.8

* added code examples to add a list column into a data frame and extract a column
from a data frame's list column

# dp 0.4.6

* added a code examples to apply a function to a list column, create a list column
within a data frame, iterate through a data frame to create multiple parameterized 
reports, save a data frame to a spreadsheet, select a file path and tidyup unspecified
objects in the global environment

# dp 0.4.4

* added **activate** and **deactivate** functions as RStudio Addins

# dp 0.4.0 <font size="4">2025-01-28</font>

* added **code_example** inserting code example text into RStudio using Rstudio's
Addins functionality, as described in [RStudio Addins](https://rstudio.github.io/rstudioaddins/)

# dp 0.3.9 <font size="4">2025-01-28</font>

- renamed **access_remote_version_control**, **include_local_version_control** and
**sync_local_version_control**
- added **include_remote_version_control** generic function to create a GitHub 
repository for the data project

# dp 0.3.6 <font size="4">2024-12-20</font>

- added **access_version_control** to create and store a GitHub Personal Access
Token in the local credential store
- added **version_control** property
- added **include_version_control** generic function to create a Git repository for
the project, adding files to ignore in version control in .gitignore. Information on 
using Git with Rstudio can be found in [Happy Git and Github for the useR](https://happygitwithr.com/)
- added **sync_version_control** generic function to stage and commit project files 
to Git repository

# dp 0.2.11 <font size="4">2024-12-18</font>

- updated **validation checks notes** adding row_reduction_fn issue

# dp 0.2.8 <font size="4">2024-12-06</font>

- remove empty folders and .quarto folder creating during documentation in vignettes folder
- added **Run validation checks** document as data project vignette

# dp 0.2.4 <font size="4">2024-12-04</font>

- removed archive subfolders, add archive_ prefix to file path instead

# dp 0.2.2 <font size="4">2024-12-04</font>

- removed **add_rmarkdown_vignette**, **document_all_rmarkdown_articles**, 
**document_single_rmarkdown_article** generic functions and **blank_rmarkdown_vignette**
template

# dp 0.2.0 <font size="4">2024-12-04</font>

-   updated package to use quarto vignettes instead of Rmarkdown
    vignettes (following update of RStudio using Quarto 1.5)

    -   updated **DESCRIPTION VignetteBuilder** to use Quarto instead of
        knitr
    -   removed quarto documents property.
    -   exported **add_quarto_vignette** generic function, making
        **add_rmarkdown_vignette** and **add_quarto_document** internal
        generic functions
    -   removed **document_quarto_article**, updated
        **document_all_quarto_articles** and
        **document_single_quarto_article** and made
        document_all_rmarkdown_articles and
        document_single_rmarkdown_article internal generic functions
    -   updated **include_quarto_documents** and **open_quarto** making
        them internal generic functions
    -   updated **include_vignettes** generic function to add quarto as
        VignetteBuilder
    -   updated **document_tasks** and **open_tasks** generic functions
    -   updated ymal in **blank_quarto_vignette** and tasks quarto
        templates

# dp 0.1.6 <font size="4">2024-12-02</font>

-   added FME transformer notes article as vignette into data project documentation

# dp 0.1.5 <font size="4">2024-11-28</font>

-   Roxygen comments added to top of function script

# dp 0.1.4 <font size="4">2024-11-27</font>

-   added **add_function_script** generic function to add a function
    script in the R folder

# dp 0.1.3 <font size="4">2024-11-27</font>

-   included maturing and stable lifecycle in the data_project
    properties based on the data project's major and minor versions,
    including lifecycle badges message in **increment_major_version**
    and **increment_minor_version** generic functions

# dp 0.1.2 <font size="4">2024-11-26</font>

-   added **open documentation** generic function to data project's
    documentation in a web browser

# dp 0.1.0 <font size="4">2024-11-23</font>

-   included documentation in data project

# dp 0.0.24

# dp 0.0.23 <font size="4">2024-11-23</font>

-   added **increment_major_version** and **increment_minor_version**
    generic functions to increment the data project's version by either
    a major or minor interval

# dp 0.0.22 <font size="4">2024-11-23</font>

-   added **include_spreadsheets** generic function to add the folder
    structure required for including spreadsheets in the data project
-   added **add_unit_test** generic function to setup the data project
    for unit testing, adding a blank test file in the tests folder

# dp 0.0.21 <font size="4">2024-11-23</font>

-   added **include_quality_assurance** generic function to include
    quality assurance functions in the data project

# dp 0.0.20 <font size="4">2024-11-23</font>

-   added **include_fme** generic function to add the folder structure
    required for including FME workspaces in the data project
-   added **include_magrittr_pipe** generic function to setup the data
    project to use the magrittr pipe, %\>%

# dp 0.0.17 <font size="4">2024-11-23</font>

-   added **include_databases** generic function to include database
    functions used to store connection strings and connect to databases

# dp 0.0.16 <font size="4">2024-11-23</font>

-   added **add_data_script** generic function to include data within
    the project, adding a data-raw script and data folder
-   added **add_lookup_script** generic function to add a lookup-raw
    script to the data-raw folder

# dp 0.0.15 <font size="4">2024-11-22</font>

-   added **include_config** generic function to add config and
    .Renviron files to the data project and **open_config** generic
    function to open the config and .Renviron files in RStudio for
    editing. config.yml and .Renviron file added to .gitignore

# dp 0.0.12 <font size="4">2024-11-22</font>

\*\* added **document_all_functions_and_lookups**, **document_home**,
**document_products**, and **document_tasks** to create documentation
for all the functions and lookups, home page, products article and tasks
article in the docs folder

# dp 0.0.11 <font size="4">2024-11-22</font>

-   added **add_rmarkdown_vignette**,
    **document_single_rmarkdown_article**,
    **document_all_rmarkdown_articles** generic functions with
    **include_vignettes** internal function to add a single or all
    vignette articles into the vignettes folder and document them in the
    docs folder

# dp 0.0.10 <font size="4">2024-11-22</font>

-   added **document_single_quarto_articles** and
    **document_all_quarto_article** generic functions, with internal
    function **document_quarto_article** to create documentation for a
    single or all the quarto articles in the quarto_docs folder

# dp 0.0.9 <font size="4">2024-11-22</font>

-   added **document_data_project** to create the data project
    documentation

# dp 0.0.8 <font size="4">2024-11-22</font>

-   added **create_new_package** function to open a new package form in
    RStudio which can then be run to create a new package in the data
    project. **deactivate** function updating to remove this form from
    the data project as it will no longer be required

# dp 0.0.7 <font size="4">2024-11-22</font>

-   added **add_quarto_document** generic function to add a quarto
    document into the quarto_docs folder

# dp 0.0.3 <font size="4">2024-11-22</font>

-   added **open_new_package_form** generic function to open a new
    package form which is run and filled in, in RStudio to create a new
    package

# dp 0.0.2 <font size="4">2024-11-18</font>

-   added **open_quarto** generic function to open the quarto.yml
    document for editing
-   added **add_typst_document** generic function, along with internal
    generic function **include_typst_documents**, to add a typst
    document into the typst_docs folder
-   added **open_description** generic function to open the DESCRIPTION
    document for editing

# dp 0.0.1 <font size="4">2024-11-18</font>

-   added **open_home** generic function to open the README R markdown
    document for editing
-   added **open_pkgdown** generic function to open the \_pkgdown
    document for editing
-   added **open_tasks** generic function to open the tasks document for
    editing
-   added **open_products** generic function to open the NEWS document
    for editing
-   added **open_document** internal generic function to open the
    specified document for editing
-   added internal generic functions **increment_version**,
    **include_package_version_control**,
    \*\*refresh_imports\_\_package_list**,** include_blank_state**,**
    include_tasks**,** include_quarto_documents**, and**
    document_quarto_article\*\*
-   added **include_documentation** generic function to setup the
    project to allow package documentation, including added the NEWS,
    README, pkgdown.yml and tasks documents
-   added **deactivate** function to update the data project prior to
    copying it and removing the data project class instance from the
    global environment
-   added **activate** function to create a new data project class
    instance, including the data project's properties in the global
    environment and update the project's version

# dp 0.0.0.9000 <font size="4">2024-11-18</font>

-   Initial project created
