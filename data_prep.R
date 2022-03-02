library(tidyverse)
library(spotifyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(viridisLite)
library(ggridges)
library(plotly)

fkj <- get_playlist_audio_features("", "37i9dQZF1DZ06evO1x7AE9")
tom_misch <- get_playlist_audio_features("", "37i9dQZF1DZ06evO0P3UNG")
lianne_de_havas <- get_playlist_audio_features("", "37i9dQZF1DZ06evO1EwgdW")
joy_crookes <- get_playlist_audio_features("", "37i9dQZF1DZ06evO3w1CV3")
jorja_smith <- get_playlist_audio_features("", "37i9dQZF1DX1ykpeqTwA5m")
mahalia <- get_playlist_audio_features("", "37i9dQZF1DZ06evO0AHQ3T")

erykah_badu <- get_playlist_audio_features("", "37i9dQZF1DZ06evO4zhZAI")
steve_wonder <- get_playlist_audio_features("", "37i9dQZF1DZ06evO4iAGsg")
bill_withers <- get_playlist_audio_features("", "37i9dQZF1DX5MwHlrzAPLQ")
marvin_gaye <- get_playlist_audio_features("", "37i9dQZF1DZ06evO1VI5MY")
ray_charles <- get_playlist_audio_features("", "37i9dQZF1DZ06evO0FPX4A")
aretha_franklin <- get_playlist_audio_features("", "37i9dQZF1DZ06evO4mP172")

new <- rbind(fkj,tom_misch,lianne_de_havas,jorja_smith,mahalia,joy_crookes)
new <- new %>%
  add_column(generation = "new")

old <- rbind(erykah_badu,steve_wonder,bill_withers,marvin_gaye,ray_charles,aretha_franklin)
old <- old %>%
  add_column(generation = "old")

all <- rbind(old, new)


# outlier <- all %>% filter(tempo == 0)
# ggplot(outlier, aes(x = track.name,)) + geom_bar()

ggplot(new, aes(x=tempo , y= playlist_name, size = valence, color = playlist_name)) +
  geom_point() + scale_fill_viridis_d() + xlim(0,250) + 
  labs(title = "New Generation", subtitle = "Valence and tempo per artist playlist", colour = "Playlist name",
         x = "Tempo", y = "Playlist name", size = "Valence") + theme_tufte()
 
ggplot(old, aes(x=tempo , y= playlist_name, size = valence, color = playlist_name)) +
  geom_point() + scale_fill_viridis_d()  + xlim(0,250) + 
  labs(title = "Old Generation", subtitle = "Valence and tempo per artist playlist", colour = "Playlist name",
       x = "Tempo", y = "Playlist name", size = "Valence") + theme_tufte()

ggplot(all, aes(x=tempo , y= playlist_name, size = valence, color = playlist_name)) +
  geom_point() + scale_fill_viridis_d()  + xlim(0,250) + 
  labs(title = "Old Generation", subtitle = "Valence and tempo per artist playlist", colour = "Playlist name",
       x = "Tempo", y = "Playlist name", size = "Valence") + theme_tufte() + facet_grid( facets = . ~ generation)

ggplot(all, aes(x=energy , y= danceability, color = generation)) +
  geom_point() + scale_fill_viridis_d() + 
  labs(title = "Both Generations", subtitle = "energy and danceability per generation", colour = "Generation",
       x = "Energy", y = "Danceability",) + theme_tufte() + facet_grid( facets = . ~ generation)

# facet_grid( facets = . ~ VARIABLE)

ggplot(all, aes(x=valence , y= tempo, color = generation)) +
  geom_point(size = 3) + scale_fill_viridis_d() + ylim(45,250)+ 
  labs(title = "Both Generations", subtitle = "Valence and tempo per generation", colour = "Generation",
       x = "Valence", y = "Tempo") + theme_tufte()

# HI IS EEN OUTLIER VAN ERYKAH BADU
# dont want you on my mind tempo tussen 240 en 250

ggplot(all, aes(x = valence, y = tempo, fill = generation)) + 
  geom_density_ridges(scale = 4) + theme_tufte() + facet_grid( facets = . ~ generation)


top_50_nl <- get_playlist_audio_features("", "37i9dQZEVXbKCF6dqVpDkS")
top_50_be <- get_playlist_audio_features("", "37i9dQZEVXbJNSeeHswcKB")
top_50_world <- get_playlist_audio_features("", "37i9dQZEVXbMDoHDwVN2tF")
top_50s <-
  top_50_nl %>%
  mutate(country = "The Netherlands") %>%
  bind_rows(top_50_be %>% mutate(country = "Belgium")) %>%
  bind_rows(top_50_world %>% mutate(country = "Global")) %>%
  mutate(
    country = fct_relevel(country, "Global", "The Netherlands", "Belgium")
  )

ggplot(old,                         # Set up the plot.
    aes(
      x = valence,
      y = energy,
      size = track.popularity,
      colour = danceability,
      label = track.name           # Labels will be interactively visible.
    )
  ) +
  geom_point() +                   # Scatter plot.
  geom_rug(size = 0.1) +           # Add 'fringes' to show data distribution.
  facet_wrap(~playlist_name) +           # Separate charts per country.
  scale_x_continuous(              # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),        # Use grid-lines for quadrants only.
    minor_breaks = NULL            # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(              # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_viridis_c(          # Use the cividis palette
    option = "E",                  # Qualitative set.
    alpha = 0.8,                   # Include some transparency
    guide = "none"
  ) +
  scale_size_continuous(           # Fine-tune the sizes of each point.
    guide = "none"                 # Remove the legend for size.
  ) +
  theme_light() +                  # Use a simpler theme.
  labs(                            # Make the titles nice.
    x = "Valence",
    y = "Energy"
  )
