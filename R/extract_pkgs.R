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

  codefiles <- fs::dir_ls(here::here(), regex = 'R[md]*$', recurse=TRUE, type='file')
  a <- purrr::map(codefiles, ~grep('^library\\(.*\\)', readLines(.),
                                   value=TRUE))
  b <- purrr::map(codefiles, ~grep('p_load', readLines(.), value=TRUE)) %>%
    unlist() %>% unname()
  d <- purrr::map(codefiles, ~grep('^require\\(', readLines(.), value=TRUE))

  pkg_a <- purrr::map(a, ~str_remove(.x, '#.*')) %>%
    purrr::map(~stringr::str_extract_all(.x, '[\\w\\.]+')) %>%
    purrr::modify_depth(2, ~.x[length(.x)]) %>%
    unlist() %>% unname() %>%
    tibble::tibble('x' = .) %>%
    dplyr::filter(stringr::str_detect(x, '_+', negate=TRUE)) %>%
    dplyr::pull(x)

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

  pkg_e = purrr::map(codefiles, ~grep('[:alnum:]+::', readLines(.x),
                                      value=TRUE)) %>%
    purrr::map(~stringr::str_match(.x, '(\\w+)::\\w+')[,2]) %>%
    unlist() %>% unname()

  pkgs <- Reduce(union, list(pkg_a, pkg_b1, pkg_b2, pkg_d, pkg_e))

  return(sort(pkgs))
}
