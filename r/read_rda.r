read_rda <- function(file){
  load(file)
  as_tibble(get(ls()[ls() != "file"]))
}
