onload <- function(){
  options(rama.workspace=paste0(getwd(),"/workspace"))
  options(rama.temp_dir=paste0(options("rama.workspace"),"/temp"))
  options(rama.gama_dir=paste0(options("rama.workspace"),"/gama"))

  options(rama.default.gama.win = "/latest_gama_win.zip");
  options(rama.default.gama.osx = "/Gama1.7-macosx.cocoa.x86_64.zip");
  options(rama.default.gama.linux = "latest_gama_linux.zip");
  options(rama.repo="file:///Users/nicolas/git/gama/gama/ummisco.gama.product/target/products")
}

gama_repo <- function(repo=NULL){
  if(! is.null(repo)) {
    options(rama.repo=repo)
  }
}

gama_remote_distrib <- function() {
  os <- paste0(Sys.info()["sysname"])
  switch(os,
         "Darwin" = paste0(options("rama.repo"),options("rama.default.gama.osx")),
         "Window" = "truc",
         "linux" = "bidule");
}

download_gama <- function(){
  distrib <- gama_remote_distrib();
  path <- paste0(options("rama.temp_dir"),"/");
  distrib_file <- paste0(path,"downloaded_gama.zip");
  if(!dir.exists(path))
    dir.create(path, recursive = TRUE);
  download.file(distrib, distrib_file, quiet = FALSE, mode = "wb",cacheOK = TRUE);
  unzip(distrib_file, exdir = paste0(options("rama.gama_dir"), "/"));
  file.remove(distrib_file);

}

