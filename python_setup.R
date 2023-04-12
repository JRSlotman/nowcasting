if("librarian" %in% rownames(installed.packages()) == FALSE) {install.packages("librarian")}
librarian::shelf(here, devtools, reticulate)
devtools::install_github("dhopp1/nowcastLSTM") # Only first time

install_miniconda(path = miniconda_path(), update = TRUE, force = FALSE)
python_path <- paste(miniconda_path(),"/python.exe", sep = "")
use_python(python = python_path, required = T)
python_packages <- "dill numpy pandas pmdarima torch nowcast-lstm"
py_install(as.list(strsplit(python_packages, " ")[[1]]), pip = TRUE, conda = conda_binary())

#### Conda instructions ####
# conda env list
# conda activate "C:\Users\jrslo\AppData\Local\r-miniconda\envs\r-reticulate"
# pip install packagename

# install_miniconda(path = miniconda_path(), update = TRUE, force = FALSE)
# use_python(python = miniconda_path(), required = T)
# conda_install(miniconda_path(), "dill numpy pandas pmdarima torch nowcast-lstm", pip = TRUE)
# initialize_session(python_path = miniconda_path()) # this function should be run at the beginning of every Python session. Python path can be left empty to use the system default.

#sys.setenv(reticulate_python  = "c:/users/jrslo/miniconda3/lib") # set in path in windows environment variables
#Sys.setenv(RETICULATE_PYTHON = python_path)
# Sys.getenv()

# py_config()
# Should return something like:
# python:         C:/Users/julian.slotman/AppData/Local/r-miniconda/python.exe
# libpython:      C:/Users/julian.slotman/AppData/Local/r-miniconda/python39.dll
# pythonhome:     C:/Users/julian.slotman/AppData/Local/r-miniconda
# version:        3.9.7 (default, Sep 16 2021, 16:59:28) [MSC v.1916 64 bit (AMD64)]
# Architecture:   64bit
# numpy:          C:/Users/julian.slotman/AppData/Local/r-miniconda/Lib/site-packages/numpy
# numpy_version:  1.22.3
# 
# NOTE: Python version was forced by use_python function
