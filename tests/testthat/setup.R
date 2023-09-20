source(file.path("..", "..", "src", "common", "logger.R"))
source(file.path("..", "..", "src", "io", "app_files.R"))
source(file.path("..", "..", "src", "io", "io_handler.R"))
Sys.setenv("LOCAL_APP_FILES_DIR" = "../../data/local_app_files")
# the system under test (sut)
source(file.path("..", "..", "./RFunction.R"))

# movebank credentials stored in HOME .Renviron file
usr <- Sys.getenv("vult_usr")
pwd <- Sys.getenv("vult_pwd")