#' Extract the packages used in a RStudio Project
#'
#' @return A character string of all packages in a Project
#' @export
#'
#' @examples
extract_pkgs <- function(){
  require(fs)
  require(here)
  require(tidyverse)

  codefiles <- dir_ls(here(), regex = 'R[md]*$', recurse=TRUE, type='file')
  a <- map(codefiles, ~grep('library\\(.*\\)', readLines(.), value=TRUE))
  b <- map(codefiles, ~grep('p_load', readLines(.), value=TRUE)) %>%
    unlist() %>% unname()
  d <- map(codefiles, ~grep('require\\(', readLines(.), value=TRUE))

  pkg_a <- map(a, ~str_remove(.x, '#.*')) %>%
    map(~str_extract_all(.x, '[\\w\\.]+')) %>%
    modify_depth(2, ~.x[length(.x)]) %>%
    unlist() %>% unname() %>%
    tibble('x' = .) %>%
    filter(str_detect(x, '_+', negate=TRUE)) %>%
    pull(x)

  b1 <- b[str_detect(b,'char')]
  b2 <- b[str_detect(b, 'char', negate=TRUE)]

  pkg_b1 <- b1 %>%
    map(~str_match(.x, 'c\\((.*)\\)?')[,2]) %>%
    map(~str_extract_all(.x, '[\\w\\.]+')) %>%
    unlist()
  pkg_b2 <- map(b2, ~str_extract_all(.x, "[\\w\\.]+")) %>%
    modify_depth( 2, ~.x[length(.x)]) %>%
    unlist()

  pkg_d <- d %>%
    map(~str_remove(.x, 'install.*')) %>%
    map(~str_extract_all(.x, '[\\w\\.]+')) %>%
    modify_depth(2, ~.x[length(.x)]) %>%
    unlist() %>% unname() %>% unique()

  pkgs <- Reduce(union, list(pkg_a, pkg_b1, pkg_b2, pkg_d))

  return(sort(pkgs))
}
