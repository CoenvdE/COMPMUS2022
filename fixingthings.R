#------------------------------------------------------------------------------
# DTW self similarity matrix of cover and original of "Say A Little Prayer"

#------------------------------------------------------------------------------
# Libraries 
library(tidyverse)
library(spotifyr)
library(compmus)
spotifyr::get_spotify_access_token()
#------------------------------------------------------------------------------
# id of orignal - f#-
# id of cover - e- 
#------------------------------------------------------------------------------
# Helper Functions 

transpose_pitch = function(pitch_list, number_of_semitones) { # n = 1: C -> C#
  names = names(pitch_list)
  new_list = setNames(circshift(unname(pitch_list), number_of_semitones), names)
  new_list
}

transpose_pitches = function(df, n) { # n = 1 is C -> C#
  df %>% dplyr::mutate(pitches = purrr::map2(pitches, n, transpose_pitch))
}

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

#      C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
major_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
minor_chord <-
  c(   1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
seventh_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)

major_key <-
  c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <-
  c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

chord_templates <-
  tribble(
    ~name, ~template,
    "Gb:7", circshift(seventh_chord, 6),
    "Gb:maj", circshift(major_chord, 6),
    "Bb:min", circshift(minor_chord, 10),
    "Db:maj", circshift(major_chord, 1),
    "F:min", circshift(minor_chord, 5),
    "Ab:7", circshift(seventh_chord, 8),
    "Ab:maj", circshift(major_chord, 8),
    "C:min", circshift(minor_chord, 0),
    "Eb:7", circshift(seventh_chord, 3),
    "Eb:maj", circshift(major_chord, 3),
    "G:min", circshift(minor_chord, 7),
    "Bb:7", circshift(seventh_chord, 10),
    "Bb:maj", circshift(major_chord, 10),
    "D:min", circshift(minor_chord, 2),
    "F:7", circshift(seventh_chord, 5),
    "F:maj", circshift(major_chord, 5),
    "A:min", circshift(minor_chord, 9),
    "C:7", circshift(seventh_chord, 0),
    "C:maj", circshift(major_chord, 0),
    "E:min", circshift(minor_chord, 4),
    "G:7", circshift(seventh_chord, 7),
    "G:maj", circshift(major_chord, 7),
    "B:min", circshift(minor_chord, 11),
    "D:7", circshift(seventh_chord, 2),
    "D:maj", circshift(major_chord, 2),
    "F#:min", circshift(minor_chord, 6),
    "A:7", circshift(seventh_chord, 9),
    "A:maj", circshift(major_chord, 9),
    "C#:min", circshift(minor_chord, 1),
    "E:7", circshift(seventh_chord, 4),
    "E:maj", circshift(major_chord, 4),
    "G#:min", circshift(minor_chord, 8),
    "B:7", circshift(seventh_chord, 11),
    "B:maj", circshift(major_chord, 11),
    "D#:min", circshift(minor_chord, 3)
  )

key_templates <-
  tribble(
    ~name, ~template,
    "Gb:maj", circshift(major_key, 6),
    "Bb:min", circshift(minor_key, 10),
    "Db:maj", circshift(major_key, 1),
    "F:min", circshift(minor_key, 5),
    "Ab:maj", circshift(major_key, 8),
    "C:min", circshift(minor_key, 0),
    "Eb:maj", circshift(major_key, 3),
    "G:min", circshift(minor_key, 7),
    "Bb:maj", circshift(major_key, 10),
    "D:min", circshift(minor_key, 2),
    "F:maj", circshift(major_key, 5),
    "A:min", circshift(minor_key, 9),
    "C:maj", circshift(major_key, 0),
    "E:min", circshift(minor_key, 4),
    "G:maj", circshift(major_key, 7),
    "B:min", circshift(minor_key, 11),
    "D:maj", circshift(major_key, 2),
    "F#:min", circshift(minor_key, 6),
    "A:maj", circshift(major_key, 9),
    "C#:min", circshift(minor_key, 1),
    "E:maj", circshift(major_key, 4),
    "G#:min", circshift(minor_key, 8),
    "B:maj", circshift(major_key, 11),
    "D#:min", circshift(minor_key, 3)
  )
# DTW self similarity matrix of cover and original of "Say A Little Prayer"

# Import Audio Analysis of Original and Cover 

original_id <- "3NfxSdJnVdon1axzloJgba"
cover_id <- "1ZczOoLuCyDO5dKUPndxf5"

original <-
  get_tidy_audio_analysis("3NfxSdJnVdon1axzloJgba") %>%
  compmus_align(sections, segments) %>%
  select(sections) %>%
  unnest(sections) %>%
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      )
  )

cover <-
  get_tidy_audio_analysis("1ZczOoLuCyDO5dKUPndxf5") %>%
  compmus_align(sections, segments) %>%
  select(sections) %>%
  unnest(sections) %>%
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      )
  )

# Check chord-gram
chords_original <- original %>% 
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "euclidean",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) %>%
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "", title = "Original | Aretha") 


chords_cover <- cover %>% 
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "euclidean",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) %>%
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "", title = "Cover | Lianne Le Havas")


cover_transposed <- transpose_pitches(cover, 2)

chords_cover_transposed <- cover_transposed %>% 
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "euclidean",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) %>%
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "", title = "Cover | Lianne Le Havas | Transposed")


gridExtra::grid.arrange(chords_original, chords_cover, chords_cover_transposed)

#-------------------------------------------------------------------------------------
# Checked 

original_X <-
  get_tidy_audio_analysis(original_id) %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

## La Chapelle Royale
cover_X <-
  get_tidy_audio_analysis(cover_id) %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

cover_X

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

transpose_pitch = function(pitch_list, number_of_semitones) { # n = 1: C -> C#
  names = names(pitch_list)
  new_list = setNames(circshift(unname(pitch_list), number_of_semitones), names)
  new_list
}

transpose_pitches = function(df, n) { # n = 1 is C -> C#
  df %>% dplyr::mutate(pitches = purrr::map2(pitches, n, transpose_pitch))
}

cover_transposed = transpose_pitches(cover_X, 2)

compmus_long_distance(
  original_X %>% mutate(pitches = map(pitches, compmus_normalise, "chebyshev")),
  cover_transposed %>% mutate(pitches = map(pitches, compmus_normalise, "chebyshev")),
  feature = pitches,
  method = "euclidean"
) %>%
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_equal() +
  labs(x = "Original", y = "Cover | Transposed") +
  theme_minimal() +
  scale_fill_viridis_c(guide = NULL)

