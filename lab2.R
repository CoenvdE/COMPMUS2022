library(tidyverse)
library(spotifyr)

grammy <- get_playlist_audio_features("", "4kQovkgBZTd8h2HCM3fF31")
edison <- get_playlist_audio_features("", "37i9dQZF1DX8mnKbIkppDf")

awards %>%
  ggplot(aes(x = energy)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~category)

awards %>%
  ggplot(aes(x = category, y = energy)) +
  geom_boxplot()

awards %>%
  ggplot(aes(x = category, y = energy)) +
  geom_violin()

grammy %>% ggplot(aes(x = valence, y = energy)) + geom_point() + geom_smooth() 
