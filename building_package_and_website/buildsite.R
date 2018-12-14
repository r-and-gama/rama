.buildsite <- function() {
  pkgdown::build_site()

# loading index.html:
  index_file <- "docs/index.html"
  index <- readLines(index_file)

# removing the link to bioconductor:
  sel <- grep("bioconductor", index)
  sel <- sel:(sel + 1)
  index <- index[-sel]

# inserting favicon into index.html:
  # sel <- grep("</head>", index)
  # index <- c(index[1:(sel - 1)],
  #            "<link rel=\"shortcut icon\" type=\"image/x-icon\" href=\"favicon.ico\">",
  #            index[sel:length(index)])

# rewriting index.html:
  writeLines(index, index_file)
}

