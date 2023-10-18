test_data <- function(test_file) {
    test_data_root_dir <- test_path("data")
    readRDS(file = file.path(test_data_root_dir, test_file))
}


generate_acc_string <- function(naxis, sep = " "){
  
  axis_label <- sort(sample(letters, naxis))
  values <- rpois(10*naxis, 500)
  string_values <- paste(values, collapse = sep)
  
  list(
    str = string_values, 
    axis_id = paste(axis_label, collapse = ""),
    expected = matrix(
      values, ncol = naxis, byrow = TRUE, 
      dimnames = list(NULL, paste0("acc_", axis_label))
    )
  )
}
