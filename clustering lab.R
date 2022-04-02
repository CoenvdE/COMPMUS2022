library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(spotifyr)
library(compmus)
library(dendextend)
library(pvclust)

theme_ilo <- function(){
  theme_minimal() +
    theme(
      text = element_text(color = "gray25"),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(color = "gray30"),
      plot.background = element_rect(fill = "gray95"),
      plot.margin = unit(c(5, 10, 5, 10), units = "mm")
    )
}

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

# set up classification
get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit %>% 
    collect_predictions() %>% 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit %>% 
    conf_mat_resampled() %>% 
    group_by(Prediction) %>% mutate(precision = Freq / sum(Freq)) %>% 
    group_by(Truth) %>% mutate(recall = Freq / sum(Freq)) %>% 
    ungroup() %>% filter(Prediction == Truth) %>% 
    select(class = Prediction, precision, recall)
}  

# set up playlist
artists_all <-
  all%>%
  add_audio_analysis() %>%
  mutate(
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean"
      )
  ) %>%
  mutate(pitches = map(pitches, compmus_normalise, "clr")) %>%
  mutate_at(vars(pitches, timbre), map, bind_rows) %>%
  unnest(cols = c(pitches, timbre))

artists_all_filter <- artists_all %>% group_by(playlist_name) %>%
  summarise_at(vars(danceability, energy, speechiness, acousticness, instrumentalness, valence, tempo, c04, c02), mean)



artists_all_filter_juice <-
  recipe(
    playlist_name ~ #track artist RECAST AS FACTOR EN DAN <- factor(list, levels = c("name goeie orders moet precies"))
      danceability +
      energy +
      speechiness +
      acousticness +
      instrumentalness +
      valence +
      tempo +
      c02 + c04,
    data = artists_all_filter
  ) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>% 
  # step_range(all_predictors()) %>% 
  prep(artists_all_filter %>% mutate(playlist_name = substring(playlist_name, 8))) %>%
  juice() %>%
  column_to_rownames("playlist_name")


artists_all_filter_dist <- dist(artists_all_filter_juice, method = "euclidean")

hc_all <- hclust(artists_all_filter_dist, method = "average")
dend <- as.dendrogram(hc_all)
dend <- dend %>%
  color_branches(k = 12) %>%
  set("branches_lwd", c(2))
dend <- color_labels(dend, k = 12)
plot(dend)


fkj_dist %>% 
  hclust(method = "complete") %>% # Try single, average, and complete.
  dendro_data() %>%
  ggdendrogram(scale.color = cols)


heatmaply(
  fkj_juice,
  hclustfun = hclust,
  hclust_method = "complete",  # Change for single, average, or complete linkage.
  dist_method = "euclidean"
)


