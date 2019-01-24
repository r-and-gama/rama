# load experiment
gaml_file <- system.file("examples", "sir.gaml", package = "rama")
exp1 <- load_experiment("sir", gaml_file, "sir")
#create path to folder in working direction
output_dir <- create_output_dir(exp1, "")
parameter_xml_file <-  paste0(name(exp1), ".xml")
