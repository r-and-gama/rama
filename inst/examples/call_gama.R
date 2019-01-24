parameter_xml_file <- system.file("examples", "sir.xml", package = "rama")
outfiles <- call_gama(parameter_xml_file, hpc = 1, output_dir = tempdir())
