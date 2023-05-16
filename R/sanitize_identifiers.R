sanitize_identifiers <- function(definition) {
  definition %>% stringr::str_replace_all("\\.", "_")
}
