browse_mygithub <- function(type = c('issues', 'projects','wiki')){
  require(fs)
  require(stringr)
  require(glue)
  require(here)
  type <- match.arg(type)
  if (!dir_exists(here('.git'))) {
    stop('Not a git repository')
  }
  `%!in%` <- Negate(`%in%`)
  git_url <- readLines(pipe("grep url .git/config | sed -e 's/\turl = //g'"))
  if(length(git_url)==0L) stop('Not linked with GitHub')
  bl <- str_match(git_url, 'git@github.com:([a-z]+)/([[:alnum:]]+).git')
  git_user <- bl[1,2]; git_repo <- bl[1,3]
  git_url <- glue::glue("https://github.com/{git_user}/{git_repo}")
  browseURL(paste0(git_url,'/',type))
}

