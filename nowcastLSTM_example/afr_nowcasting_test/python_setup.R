# renv::init() # Run only first time creating project, see: https://posit.co/blog/renv-project-environments-for-r/
renv::use_python() # When prompted, select 1: ~/.virtualenvs/r-reticulate/Scripts/python.exe
if("librarian" %in% rownames(installed.packages()) == FALSE) {install.packages("librarian")}
librarian::shelf(here, renv, devtools, reticulate)
devtools::install_github("dhopp1/nowcastLSTM")
python_packages <- "dill numpy pandas pmdarima torch nowcast-lstm"
py_install(as.list(strsplit(python_packages, " ")[[1]]), pip = TRUE, conda = conda_binary())

# After installing packages, call renv::snapshot() to update the renv.lock