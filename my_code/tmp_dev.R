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

plot_season <- function(ds) {
  #ds <- d

  ds %>%
    filter(!map_lgl(rel_gross, is.null)) %>%
    mutate(
      date = map_chr(meta, ~ ..1$date),
      season = str_extract(date, "\\w{2}"),
      year = str_extract(date, "\\d{2}$")
    ) %>%
    relocate(c(date, season, year), .after = tour_id) %>%
    select(tour_id, date, season, year, rel_gross) %>%
    unnest(rel_gross) %>%
    mutate(
      hole_no = str_remove(hole_id, "hole_") %>% as.integer(),
      round_no = case_when(
        between(hole_no, 1, 18) ~ "round_1",
        between(hole_no, 19, 36) ~ "round_2",
        between(hole_no, 37, 54) ~ "round_3"
      )
    ) %>%
    select(-hole_no) %>%
    relocate(round_no, .after = hole_id) %>%
    group_by(tour_id, season, year, round_no) %>%
    summarise(across(where(is.integer), sum), .groups = "drop_last") %>%
    summarise(across(where(is.integer), mean), .groups = "drop")
    # fixa
    # beräkna `höst - vår` inom samma år

}


# comparing gross and net scores over time... ----
get_plot <- function(ds) {
  # ds <- d
  ds <- ds %>% filter(!map_lgl(abs_gross, is.null))
  get_score_touravg <- function(ds, my_results_type = "abs_gross") {
    score_touravg <-
      ds %>%
      select(tour_id, score = !!my_results_type) %>%
      unnest(score) %>%
      mutate(
        hole_no = str_remove(hole_id, "hole_") %>% as.integer(),
        round_no = case_when(
          between(hole_no, 1,  18) ~ 1,
          between(hole_no, 19, 37) ~ 2,
          between(hole_no, 38, 54) ~ 3
        )
      ) %>%
      relocate(hole_no, round_no, .after = hole_id) %>%
      select(-hole_no) %>%
      group_by(tour_id, round_no) %>%
      summarise(across(where(is.integer), sum), .groups = "drop_last") %>%
      summarise(across(where(is.integer), mean), .groups = "drop")
  }
  touravg_abs_gross <- get_score_touravg(ds, my_results_type = "abs_gross")
  touravg_abs_net <- get_score_touravg(ds, my_results_type = "abs_net")

  # attempt 1 ----
  # d_plot <-
  #   bind_rows(
  #     mutate(touravg_abs_gross, scoretype = "abs_gross"),
  #     mutate(touravg_abs_net, scoretype = "abs_net")
  #   ) %>%
  #   pivot_longer(
  #     cols = -c(tour_id, scoretype),
  #     names_to = "player",
  #     values_to = "avg_score"
  #   )
  #
  # ggplot(d_plot, aes(x = tour_id, y = avg_score, group = scoretype)) +
  #   geom_point(aes(colour = scoretype)) +
  #   geom_line(aes(colour = scoretype)) +
  #   geom_ribbon(aes(ymin = avg_score, ymax = Inf)) +
  #   facet_wrap(facets = vars(player))

  # attempt 2 ----
  player_colours <- get_player_colour()
  d_plot <-
    bind_rows(
      mutate(touravg_abs_gross, scoretype = "abs_gross"),
      mutate(touravg_abs_net, scoretype = "abs_net")
    ) %>%
    pivot_longer(
      cols = all_of(names(player_colours)),
      values_to = "avg_score",
      names_to = "player"
    ) %>%
    pivot_wider(names_from = scoretype, values_from = avg_score)


  ggplot(d_plot, aes(x = tour_id, group = 1)) +
    geom_ribbon(
      data = d_plot %>% filter(!is.na(abs_gross)),
      aes(ymin = abs_gross, ymax = abs_net, fill = player),
      alpha = .6
    ) +
    geom_line(aes(y = abs_gross), size = 1) +
    geom_line(aes(y = abs_net), size = 1) +
    geom_point(aes(y = abs_gross), shape = 21, size = 2, fill = "gray40") +
    geom_point(aes(y = abs_net), shape = 21, size = 2, fill = "gray80") +
    scale_fill_manual(
      values = player_colours,
      guide = "none"
    ) +
    facet_wrap(facets = vars(player))

  # attempt 3 (interactive) ----
  player_colours <- get_player_colour()
  d_plot <-
    bind_rows(
      mutate(touravg_abs_gross, scoretype = "abs_gross_avg"),
      mutate(touravg_abs_net, scoretype = "abs_net_avg")
    ) %>%
    pivot_longer(
      cols = all_of(names(player_colours)),
      values_to = "avg_score",
      names_to = "player"
    ) %>%
    pivot_wider(names_from = scoretype, values_from = avg_score) %>%
    mutate(
      txt_gross = str_c(tour_id, " medel(brutto): ", round(abs_gross_avg)),
      txt_net = str_c(tour_id, " medel(brutto): ", round(abs_gross_avg))
    )


  p <- ggplot(d_plot, aes(x = tour_id, group = 1)) +
    geom_ribbon(
      data = d_plot %>% filter(!is.na(abs_gross_avg)),
      aes(ymin = abs_gross_avg, ymax = abs_net_avg, fill = player),
      alpha = .6
    ) +
    geom_line(aes(y = abs_gross_avg), size = 1) +
    geom_line(aes(y = abs_net_avg), size = 1) +
    geom_point_interactive(
      aes(y = abs_gross_avg, tooltip = txt_gross),
      shape = 21, size = 2, fill = "gray40"
    ) +
    #geom_point(aes(y = abs_gross), shape = 21, size = 2, fill = "gray40") +
    geom_point(aes(y = abs_net_avg), shape = 21, size = 2, fill = "gray80") +
    scale_fill_manual(
      values = player_colours,
      guide = "none"
    ) +
    facet_wrap(facets = vars(player))
  p_interactive <- ggiraph::girafe(
    ggobj = p,
    height_svg = 6
    # width_svg = 5,
    # options = list(
    #   opts_sizing(rescale = TRUE, width = .7)
    # )
  )
}

# comparing spring and autum scores ----
ds <- d
ds <- ds %>% filter(!map_lgl(abs_gross, is.null))
d_plot <-
  ds %>%
  select(tour_id, meta, score = abs_gross) %>%
  mutate(
    season = map_chr(
      meta,
      ~case_when(
        str_detect(..1$date, "HT") ~ "Höst",
        str_detect(..1$date, "VT") ~ "Vår"
      )
    ),
    year = map_int(
      meta,
      ~str_extract(..1$date, "\\d{2}$") %>% str_c("20", .) %>% as.integer()
    )
  ) %>%
  select(-meta) %>%
  unnest(cols = score) %>%
  mutate(
    hole_nr_tmp = str_extract(hole_id, "\\d+$") %>% as.integer(),
    round_id = ceiling(hole_nr_tmp / 18) %>% as.character()
  ) %>%
  select(-hole_nr_tmp) %>%
  relocate(
    round_id, .after = hole_id
  ) %>%
  group_by(tour_id, year, season, round_id) %>%
  summarise(across(where(is.integer), sum), .groups = "drop_last") %>%
  summarise(across(where(is.integer), mean), .groups = "drop")

d_plot2 <-
  d_plot %>%
  group_by(year) %>%
  arrange(year, desc(season)) %>%
  # Jämför genomsnittlig score per runda (Vår - Höst)
  summarise(across(where(is.double), ~ .x[1] - .x[2])) %>%
  pivot_longer(cols = -year, names_to = "player", values_to = "season_diff") %>%
  filter(!is.na(season_diff)) %>%
  mutate(
    best_season = case_when(
      season_diff < 0 ~ "Vår",
      season_diff > 0 ~ "Höst",
      TRUE ~ "Lika"
    )
  )
d_annotation <-
  d_plot2 %>%
  group_by(player) %>%
  summarise(mean = mean(season_diff), .groups = "drop") %>%
  arrange(desc(mean)) %>%
  mutate(
    lab = glue::glue(
      "{round(abs(mean), 1)} slag bättre per runda på \\
      {if_else(mean < 0, 'Vår', 'Höst')}en
      "
    ),
    player = as_factor(player)
  )

d_plot2 %>%
  # faktor för att sortera facets - men det ignoreras
  mutate(player = factor(player, levels = d_annotation$player)) %>%
  arrange(player) %>%
  ggplot(aes(x = year, y = season_diff)) +
  scale_x_continuous(
    breaks = seq(2000, 2030, by = 2)
  ) +
  modelr::geom_ref_line(h = 0) +
  geom_line(alpha = .5) +
  geom_point(aes(colour = best_season), na.rm = TRUE)  +
  facet_wrap(facets = vars(player)) +
  scale_color_brewer(palette = "Dark2") +
  geom_text(
    data = d_annotation,
    aes(label = lab),
    x = Inf,
    y = -Inf,
    hjust = 1.05,
    vjust = -1,
    size = 3
  ) +
  labs(
    title = "Bästa säsongen",
    subtitle = glue::glue(
      "Skillnaden mellan vår och höst (brutto) beräknas inom ett och samma
      spelår, dvs spelaren måste ha deltagit bårde vår och höst samma år för
      att resultaten ska inkluderas här. Magkänslan säger att det ofta går
      bättre på hösten. Denna figur stödjer delvis den känslan."
    ) %>% str_wrap(width = 120),
    color = "Bästa säsong",
    x = NULL,
    caption = glue::glue(
      "Skillnad mellan vår och höst (i genomsnitt per runda, brutto):
      Positiva värden => bättre på hösten; \\
      Negativa värden => bättre på våren"
    )
  )

# Andel spelår
d_plot2 %>%
  group_by(player, best_season) %>%
  summarise(
    best_season = unique(best_season),
    n = n(),
    .groups = "drop_last"
  ) %>%
  summarise(
    best_season = best_season,
    n = n,
    prc = n / sum(n),
    .groups = "drop"
  ) %>%
  arrange(best_season, desc(prc)) %>%
  mutate(
    player = as_factor(player),
    best_season = factor(best_season, levels = c("Vår", "Lika", "Höst"))
  ) %>%
  ggplot(aes(x = player, y = prc)) +
  geom_col(aes(fill = best_season)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Bästa säsongen",
    subtitle = glue::glue(
      "Bästa säsongen beräknas inom ett och samma spelår (brutto), dvs
      spelaren måste ha deltagit bårde vår och höst samma år för att
      jämförelsen ska inkluderas här. Klar minoritet av spelare verkar
      skjuta bättre scorer på våren än på hösten."
    ) %>% str_wrap(width = 120),
    fill = "Bästa säsong",
    x = NULL
  )


