library(tidyverse)
library(ggiraph)
source(
  file.path(
    "C:/Users/caewim02/Documents/_Personal_/golf/Touren_blogdown/my_code",
    "helpers.R"
  ),
  encoding = "UTF8"
)
d <- read_rds(
  file.path(
    "C:/Users/caewim02/Documents/_Personal_/golf/Touren_blogdown/my_code",
    "data/complete_data.RDS"
  )
)
theme_set(get_my_ggtheme())

# comparing gross and net scores over time... ----
plot_player_scores <- function (ds) {
  # ds = d

  # Extract and format scores
  d_scores <-
    ds %>%
    filter(!map_lgl(abs_gross, is.null)) %>%
    select(tour_id, abs_gross, abs_net) %>%
    pivot_longer(
      cols = -tour_id,
      names_to = "score_type",
      values_to = "score"
    ) %>%
    unnest(score) %>%
    mutate(
      hole_no = str_remove(hole_id, "hole_") %>% as.integer(),
      round_no = case_when(
        between(hole_no, 1, 18) ~ "round_1",
        between(hole_no, 19, 36) ~ "round_2",
        between(hole_no, 37, 54) ~ "round_3"
      )
    ) %>%
    select(-hole_no) %>%
    relocate(round_no, .after = hole_id)

  # Calculate each player's mean score per Tour
  d_scores_avg <-
    d_scores %>%
    group_by(score_type, tour_id, round_no) %>%
    summarise(across(where(is.integer), sum), .groups = "drop_last") %>%
    summarise(across(where(is.integer), mean), .groups = "drop")

  # Format for plotting
  d_plot <-
    d_scores_avg %>%
    pivot_longer(
      cols = where(is.numeric), names_to = "player", values_to = "avg_score"
    ) %>%
    pivot_wider(names_from = score_type, values_from = avg_score) %>%
    left_join(unnest(ds, meta) %>% select(tour_id, date), by = "tour_id") %>%
    mutate(
      date = as_factor(date),
      txt_gross = glue::glue("{date}, Brutto\nmedel: {round(abs_gross)}"),
      txt_net = glue::glue("{date}, Netto\nmedel: {round(abs_net)}"),
    )

  p <-
    ggplot(d_plot, aes(x = date, group = 1)) +
    geom_hline(yintercept = 72) +
    geom_ribbon(
      data = filter(d_plot, !is.na(abs_gross)),
      aes(ymin = abs_net, ymax = abs_gross, fill = player),
      alpha = .6
    ) +
    scale_fill_manual(
      values = get_player_colour(),
      guide = "none"
    ) +
    geom_line(aes(y = abs_gross), size = .5, na.rm = TRUE) +
    geom_line(aes(y = abs_net), size = .5, na.rm = TRUE) +
    geom_point_interactive(
      aes(y = abs_gross, tooltip = txt_gross, fill = player),
      na.rm = TRUE,
      shape = 21,
      size = 1.2
    ) +
    geom_point_interactive(
      aes(y = abs_net, tooltip = txt_net, fill = player),
      na.rm = TRUE,
      shape = 21,
      size = 1.2
    ) +
    theme(
      axis.text.x = element_text(angle = 90, size = 6, vjust = .5, hjust = 1),
      axis.title.x = element_blank()
    ) +
    facet_wrap(
      facets = vars(player)
    ) +
    labs(
      title = "Medelresultat per Tour (54 hål)",
      subtitle = "absolut score",
      caption = "Tar ej hänsyn till att vissa scorer kommer från banor med par 71"
    )
  p_interactive <- ggiraph::girafe(
    ggobj = p,
    height_svg = 6
    # width_svg = 5,
    # options = list(
    #   opts_sizing(rescale = TRUE, width = .7)
    # )
  )
  p_interactive
}



