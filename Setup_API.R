install.packages("tidyverse")
install.packages("remotes")
install.packages("usethis")
remotes::install_github("charlie86/spotifyr")

usethis::edit_r_environ()

library(spotifyr)
spotifyr::get_spotify_access_token()
