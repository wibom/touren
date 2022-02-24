options(tidyverse.quiet = TRUE)
library(tidyverse)
library(kableExtra)
library(grid)
library(gridExtra)
mydir <- file.path(here::here(), 'my_code')

get_player_colour <- function(players = NULL) {

  player_ids <-
    file.path(mydir, "data/players.txt") %>%
    read_tsv(col_types = "ccccccc") %>%
    filter(id != "Ingen") %>%
    pull(id)
  colours <-
    setNames(
      ggsci::pal_d3(palette = "category20")(length(player_ids)),
      #ggsci::pal_futurama()(length(player_ids)),
      #gplots::rich.colors(n = length(player_ids)),

      # https://www.r-bloggers.com/the-paul-tol-21-color-salute/:
      #c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933",
      #  "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499"),

      # http://tools.medialab.sciences-po.fr/iwanthue/
      # c(
      #   "#d86753",
      #   "#2bff65",
      #   "#ff20c9",
      #   "#d0a87e",
      #   "#852a58",
      #   "#ff9b48",
      #   "#919dbf",
      #   "#95cd9f",
      #   "#003486",
      #   "#5c5e3d",
      #   "#130026",
      #   "#9850bd"
      # ),

      player_ids
    )

  if (!is.null(players))
    return(colours[names(colours) %in% players])
  else
    return(colours)
}
read_players_list <- function() {
  players <-
    file.path(mydir, "data/players.txt") %>%
    read_tsv(
      #locale = locale(encoding = "latin1"),
      col_types = cols(.default = col_character())
    )
}
read_tours_list <- function(){
  tours <-
    file.path(mydir, "data/tours.txt") %>%
    read_tsv(
      #locale = locale(encoding = "latin1"),
      col_types = "ccicccc"
    ) %>%
    rename(tour_id = id)
}
get_leaderboard <- function (ds, my_score_type, my_tour_id) {

  # my_score_type: { "abs_gross", "abs_net", "rel_gross", "rel_net" }
  # my_round_id: { "round_1", "round_2", "round_3", "all" }
  missing_scores <- c("tour01", "tour02", "tour03", "tour04", "tour20")
  if (my_tour_id %in% missing_scores) {
    # Sparade ej scorer - hårdkodar resultatlista / deltagarlista
    if (my_tour_id == "tour01") {
      lb <- tribble(
        ~player,     ~pos,
        "Strömdahl", 1,
        "Wibom",     NA,
        "Wänman",    NA,
        "Karlsson",  NA,
        "Forsgren",  NA,
        "Scherlund", NA,
        "Nyberg",    7
        )
    }
    if (my_tour_id == "tour02") {
      lb <- tribble(
        ~player,     ~pos,
        "Karlsson",  1,
        "Östh",      2,
        "Scherlund", 3,
        "Forsgren",  4
      )
    }
    if (my_tour_id == "tour03") {
      lb <- tribble(
        ~player,     ~pos,
        "Karlsson",  1,
        "Östh",      2,
        "Strömdahl", 3,
        "Wibom",     4,
        "Nyberg",    5,
        "Wänman",    6,
        "Forsgren",  7,
        "Lundström", 8
      )
    }
    if (my_tour_id == "tour04") {
      lb <- tribble(
        ~player,     ~pos,
        "Karlsson",  1,
        "Scherlund", 2,
        "Wibom",     3,
        "Nyberg",    4,
        "Lundström", 5,
        "Forsgren",  6
      )
    }
    if (my_tour_id == "tour20") {
      lb <- tribble(
        ~player,     ~pos,
        "Strömdahl", NA,
        "Wibom",     NA,
        "Wänman",    NA,
        "Karlsson",  NA,
        "Forsgren",  NA,
        "Scherlund", NA,
        "Nyberg",    NA,
        "Wiklund",   NA,
        "Forsell",   NA,
        "Östh",      NA
      )
    }
    lb <-
      lb %>%
      mutate(
        pos = as.integer(pos),
        erh.slag  = NA_integer_,
        `Runda 1` = NA_integer_,
        `Runda 2` = NA_integer_,
        `Runda 3` = NA_integer_,
        tot       = NA_integer_
      ) %>%
      select(
        player, erh.slag, `Runda 1`, `Runda 2`, `Runda 3`, tot, pos
      )

    if (my_score_type != "rel_net") {
      # Ej sparat scorer - kan ej beräkna placering
      lb <- lb %>% mutate(pos = NA_integer_)
    }

    return (lb)
  }

  # givna slag
  d_shots_given <-
    ds %>%
    filter(tour_id == my_tour_id) %>%
    unnest(shots_given) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    gather(key = "player", value = "erh.slag")

  # score
  d_score <-
    ds %>%
    filter(tour_id == my_tour_id) %>%
    unnest(cols = !!rlang::sym(my_score_type))

  # leaderboard (lb) = score för varje runda
  n_played_rounds <- (nrow(d_score) / 18) %>% ceiling(.)
  lb <- d_shots_given
  for (i in 1:n_played_rounds) {
    round_name <- str_c("Runda ", i)
    start <- (i - 1) * 18 + 1
    stop  <- start + 17
    score_round <-
      d_score %>%
      slice(start:stop) %>%
      select_if(is.numeric) %>%
      summarise_all(sum) %>%
      gather(key = "player", value = !! round_name)

    lb <-
      left_join(
        lb,
        score_round,
        by = "player"
      )
  }

  # tot score
  lb <-
    lb %>%
    mutate(
      tot = select(., -c(player, erh.slag)) %>% rowSums() %>% as.integer()
    )

  # Position
  tour_no <- my_tour_id %>% str_extract("(?<=tour)\\d+") %>% as.numeric()
  tie_meth <- if (tour_no <= 13) "hcp" else "math"
  if (tie_meth == "hcp") {
    # vid oavgjort vinner den med lägst antal erhållna slag

    # primär rank
    prim_rank <- rank(lb$tot, ties.method = "min")

    # sekundär rank (skalad 0-1, så den kan användas för att bryta ties)
    sec_rank <- rank(lb$erh.slag, ties.method = "min") / (nrow(lb) + 1)

    # tot_rank
    tot_rank <- prim_rank + sec_rank

    lb <-
      lb %>%
      mutate(pos = rank(tot_rank, ties.method = "min")) %>%
      arrange(pos)

  } else if (tie_meth == "math") {
    # Vid oavgjort vinner den med bäst score mot slutet av tävlingen

    # Ranking baserad på total score
    rank <-
      lb %>%
      select(player, tot) %>%
      mutate(
        prim_rank = rank(tot, ties.method = "min")
      ) %>%
      select(-tot)

    # Uppdatera ranking baserad på sista X hålen
    count_scores_from_hole <- c(19, 37, 46, 49, 51, 54)
    count_scores_from_hole <-
      count_scores_from_hole[count_scores_from_hole <= nrow(d_score)]
    if (length(count_scores_from_hole) >= 1) {
      # Blir bara aktuellt då alla scorer är registrerade. Hoppar över detta då
      # koden körs under pågående tour då endast några scorer är registrerade.
      for(i in 1:length(count_scores_from_hole)) {

        # sekundär ranking
        count_from <- count_scores_from_hole[i]
        recent_scores_rank <-
          d_score %>%
          slice(count_from:nrow(.)) %>%
          summarise_if(is.numeric, sum) %>%
          gather(key = "player", value = "score") %>%
          mutate(
            sec_rank := rank(score, ties.method = "min") / (nrow(lb) + 1)
          ) %>%
          select(-score)

        # ny ranking
        new_rank <-
          rank %>%
          left_join(
            recent_scores_rank,
            by = "player"
          ) %>%
          mutate(
            tot_rank = prim_rank + sec_rank,
            new_rank = rank(tot_rank, ties.method = "min")
          )

        # uppdatera ranking
        rank <-
          new_rank %>%
          select(player, prim_rank = new_rank) %>%
          arrange(prim_rank)
      }
    }
    # Addera slutgiltig ranking till leaderboard
    lb <-
      lb %>%
      left_join(rank, by = "player") %>%
      rename(pos = prim_rank) %>%
      arrange(pos)
  }

  return(lb)
}
get_overview_tab <- function(d, results_type = "results_net") {
  results_type <- rlang::sym(results_type)
  d %>%
    mutate(
      champ     = map_chr(!!results_type, function(x) {x$champ}),
      loser     = map_chr(!!results_type, function(x) {x$loser}),
      deltagare = map_int(!!results_type, function(x) {nrow(x$lb[[1]])})
    ) %>%
    unnest(meta) %>%
    select(
      no,
      city,
      date,
      champ,
      longdrive_champ,
      close_champ,
      loser,
      deltagare
    )
}


# plotters ---------------------------------------------------------------------
get_my_ggtheme <- function() {
  ggthemes::theme_fivethirtyeight() +
    theme(
      strip.text = element_text(hjust = 0, face = "bold")
    )
}
plot_placing <- function(ds, results_type = "results_net",
                         lastname = NA_character_) {
  results_type <- rlang::sym(results_type)

  bg <-
    ds %>%
    # bortse från tour20 (Barcelona)
    filter(tour_id != "tour20") %>%
    unnest(meta) %>%
    select(tour_id, roman_numeral, date, !!results_type) %>%
    mutate(
      n_players = map(!!results_type, function (x) {1:nrow(x$lb[[1]])})
    ) %>%
    select(-!!results_type) %>%
    unnest(n_players) %>%
    mutate(
      date = date %>% factor(., levels = unique(.))
    )

  players <- read_players_list() %>% filter(id != "Ingen") %>% pull(id)
  plots <- list()
  for (p in players) {
    d_player <-
      ds %>%
      # bortse från tour20 (Barcelona)
      filter(tour_id != "tour20") %>%
      unnest(meta) %>%
      select(tour_id, roman_numeral, date, !!results_type) %>%
      mutate(
        plc = map(!!results_type, function (x) {select(x$lb[[1]], player, pos)})
      ) %>%
      select(-!!results_type) %>%
      unnest(plc) %>%
      mutate(
        # ful-fix för att kunna plotta Tour_1, då vi inte vet placeringar
        pos = if_else(is.na(pos), 4L, pos)
      ) %>%
      mutate(
        date = date %>%  factor(., levels = unique(.))
      ) %>%
      filter(
        player == p
      )

    plots[[p]] <-
      ggplot(data = NULL, aes(x = date)) +
      geom_point(
        data = bg,
        aes(y = n_players),
        size = 2.5,
        colour = "gray70"
      ) +
      geom_point(
        data = d_player,
        aes(y = pos),
        colour = "#990000",
        size = 2.5
      ) +
      geom_line(
        data = d_player,
        aes(y = pos, group = 1),
        group = 1,
        colour = "#990000",
        size = 1
      ) +
      scale_y_reverse(
        breaks = 1:15
      ) +
      labs(
        title = p
      ) +
      #theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, size = rel(.8), vjust = .5),
        plot.margin = margin(0, 0.25, 0.25, 0, "cm")
      )
  }

  score_type_txt <- case_when(
    results_type == "results_net" ~ "netto-score",
    results_type == "results_gross" ~ "brutto-score"
  )
  mycaption <- case_when(
    results_type == "results_net" ~ str_c(
      "Finns inga scorekort från Tour 1 - 4, men innebördes placering (netto)",
      "är dokumenterad \n(bortsett från Tour 1 då vi endast vet Champ och Loser",
      sep = " "
    ),
    results_type == "results_gross" ~ "Finns inga scorekort från Tour 1 - 4"
  )
  if (is.na(lastname)) {
    plots2 <- map(
      plots, ~ . + theme(
        plot.title = element_text(size = rel(.8)),
        axis.text.y = element_text(size = rel(.8)),
        plot.margin = margin(.1, .1, .1, .1, "cm")
      )
    )

    pw <-
      plots2 %>%
      patchwork::wrap_plots(ncol = 3) +
      patchwork::plot_annotation(
        title = "Placeringar genom tiderna",
        subtitle = glue::glue("Baserat på {score_type_txt}"),
        caption = mycaption,
        theme = theme(plot.caption = element_text(colour = "gray50"))
      )
    #return (egg::ggarrange(plots = plots, ncol = 3, margin = unit(.2, "line")))
  } else {
    plots[[lastname]] +
      labs(
        title = glue::glue("{lastname}s placeringar genom tiderna"),
        subtitle = glue::glue("Baserat på {score_type_txt}"),
        caption = "Finns inga scorekort från Tour 1 - 4"
      ) +
      theme(
        plot.caption = element_text(colour = "gray50")
      )
  }

}
plot_totscore_over_time <- function(ds, lastname = NA_character_,
                                    sum_by_round = FALSE) {

  # Plottar total-score (brutto och netto) för en spelare
  # Om `lastname` specificeras returneras plott för en specifik spelare, annars
  #   faceteras plotten över spelare.
  # Om `sum_by_round == TRUE` summeras score per runda, annars summeras per Tour

  ds <- ds %>% filter(!map_lgl(abs_gross, is.null))

  # data att plotta
  if (sum_by_round == FALSE) {
    # Summera per tour
    rel_net <-
      ds %>%
      select(tour_id, rel_net) %>%
      unnest(rel_net) %>%
      group_by(tour_id) %>%
      summarise_if(is.numeric, sum)
    rel_gross <-
      ds %>%
      select(tour_id, rel_gross) %>%
      unnest(rel_gross) %>%
      group_by(tour_id) %>%
      summarise_if(is.numeric, sum)
    d_plot_meta <-
      tibble(
        tour_id = ds$tour_id
      ) %>%
      mutate(x_idx = row_number()) %>%
      left_join(unnest(d, meta) %>% select(tour_id:date), by = "tour_id") %>%
      mutate(
        tour_id  = str_remove(tour_id, "tour"),
        tour_lab = glue::glue("{date} (#{no})")
      )
    d_plot <-
      bind_rows(
        netto = rel_net,
        brutto = rel_gross,
        .id = "score_type"
      ) %>%
      pivot_longer(
        cols = -c(score_type, tour_id),
        names_to = "player",
        values_to = "score"
      ) %>%
      mutate(
        tour_id = str_replace(tour_id, "tour", "")
      ) %>%
      left_join(
        d_plot_meta, by = c("tour_id")
      ) %>%
      mutate(
        season = if_else(str_detect(date, "^HT"), "Höst", "Vår")
      )

    # Plot features
    my_x_scale <- ggplot2::scale_x_continuous(
      breaks =
        d_plot_meta %>%
        pull(x_idx),
      labels =
        d_plot_meta %>%
        pull(tour_lab),
      expand = expansion(mult = 0, add = c(0, 0))
    )
    my_vline <- ggplot2::geom_vline(
      xintercept = seq(0, max(d_plot_meta$x_idx), 1) + .5,
      colour = "gray50",
      alpha = .5
    )

  } else {

    # Summera per runda
    rel_net <-
      ds %>%
      select(tour_id, rel_net) %>%
      unnest(rel_net) %>%
      mutate(
        hole_nr = str_remove(hole_id, "hole_") %>% as.numeric(),
        round_id = case_when(
          hole_nr <= 18                 ~ "r1",
          hole_nr > 18 & hole_nr <= 36 ~ "r2",
          hole_nr > 36 & hole_nr <= 54 ~ "r3"
        ),
        round_id = factor(round_id, levels = c("r1", "r2", "r3"))
      ) %>%
      select(-hole_nr) %>%
      group_by(tour_id, round_id) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup()
    rel_gross <-
      ds %>%
      select(tour_id, rel_gross) %>%
      unnest(rel_gross) %>%
      mutate(
        hole_nr = str_remove(hole_id, "hole_") %>% as.numeric(),
        round_id = case_when(
          hole_nr <= 18                 ~ "r1",
          hole_nr > 18 & hole_nr <= 36 ~ "r2",
          hole_nr > 36 & hole_nr <= 54 ~ "r3"
        ),
        round_id = factor(round_id, levels = c("r1", "r2", "r3"))
      ) %>%
      select(-hole_nr) %>%
      group_by(tour_id, round_id) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup()
    d_plot_meta <-
      list(
        tour_id = ds$tour_id,
        round_id = c("r1", "r2", "r3")
      ) %>%
      cross_df() %>%
      arrange(tour_id, round_id) %>%
      mutate(x_idx = row_number()) %>%
      left_join(unnest(d, meta) %>% select(tour_id:date), by = "tour_id") %>%
      mutate(
        tour_id  = str_remove(tour_id, "tour"),
        tour_lab = glue::glue("{date} (#{no})"),
        round_id = factor(round_id)
      )
    d_plot <-
      bind_rows(
        netto = rel_net,
        brutto = rel_gross,
        .id = "score_type"
      ) %>%
      pivot_longer(
        cols = -c(score_type, tour_id, round_id),
        names_to = "player",
        values_to = "score"
      ) %>%
      mutate(
        tour_id = str_replace(tour_id, "tour", "")
      ) %>%
      left_join(
        d_plot_meta, by = c("tour_id", "round_id")
      ) %>%
      mutate(
        season = if_else(str_detect(date, "^HT"), "Höst", "Vår")
      )

    # Plot features
    my_x_scale <- ggplot2::scale_x_continuous(
      breaks =
        d_plot_meta %>%
        filter(round_id == "r2") %>%
        pull(x_idx),
      labels =
        d_plot_meta %>%
        filter(round_id == "r2") %>%
        pull(tour_lab),
      expand = expansion(mult = 0, add = c(0, 3))
    )
    my_vline <- ggplot2::geom_vline(
      xintercept = seq(0, max(d_plot_meta$x_idx), 3) + .5,
      colour = "gray50",
      alpha = .5
    )
  }

  # plot
  my_title <- if_else(
    sum_by_round,
    "Score per runda (18 hål)",
    "Sammanlagd score (54 hål)"
  )
  my_title <- if_else(
    is.na(lastname),
    my_title,
    glue::glue("{lastname}s {str_to_lower(my_title)}") %>% as.character()
  )
  p <-
    ggplot(d_plot, aes(x = x_idx, y = score, group = score_type)) +
    my_x_scale +
    my_vline +
    geom_hline(
      yintercept = 0,
      color = "gray20",
      size = 1.5
    ) +
    geom_point(
      aes(color = score_type, shape = season),
      size = 2.5,
      na.rm = TRUE
    ) +
    geom_line(
      aes(color = score_type),
      na.rm = TRUE,
      size = .8,
      alpha = .6
    ) +
    scale_color_manual(
      values = c(brutto = "#00d4ad", netto = "#4d5452")
    ) +
    labs(
      title = my_title,
      subtitle = "relativt banans par"
    ) +
    #facet_wrap(~player) +
    #theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, size = 8, vjust = .5, hjust = 1),
      axis.title.x = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_blank(),
      panel.grid.major.x = element_blank()
    )

  if (is.na(lastname)) {
    p + facet_wrap(~player, ncol = 3)
  } else {
    p %+% filter(d_plot, player == lastname)
  }

}

# ranking ----------------------------------------------------------------------
calc_rankpoints <- function(my_tour_id, ds, results_type = "results_net") {
  # my_tour_id <- "tour02"
  missing_scores <- c("tour01", "tour02", "tour03", "tour04", "tour20")
  if (my_tour_id %in% missing_scores) {
    return(NULL)
  }

  # Rankingpoäng räknas ut enligt följande:
  #  1) En spelare tilldelas 1 poäng för att deltaga och placera sig på sista
  #     plats.
  #  2) Vidare tilldelas ytterligare 1 poäng per placering bättre än sista
  #     platsen.
  #  3) Till den spelare som vinner tilldelas ytterligare 3 poäng.
  #  4) För varje del- och skillstävling tilldelas vinnaren 1 poäng. Dessa
  #     tävlingar är:
  #       a) Vinnare runda 1
  #       b) Vinnare runda 2
  #       c) Längsta drive
  #       d) Närmast hål


  # points from final placing
  lb <-
    ds %>%
    filter(tour_id == my_tour_id) %>%
    unnest(cols = !!rlang::sym(results_type)) %>%
    select(lb) %>%
    unnest(cols = lb)

  rank_points_place <-
    lb %>%
    arrange(pos) %>%
    mutate(
      # ranking points calculated by reversed final position
      place = rev(pos),
      # plus 3 points to Champ :)
      place = (place + c(3, rep(0, nrow(lb) - 1))) %>% as.integer()
    ) %>%
    select(player, place)

  # add points from skills
  longdrive_champ <-
    ds %>%
    filter(tour_id == my_tour_id) %>%
    unnest(cols = c("meta")) %>%
    .$longdrive_champ
  close_champ <-
    ds %>%
    filter(tour_id == my_tour_id) %>%
    unnest(cols = c("meta")) %>%
    .$close_champ

  # Vinnare av runda 1 och 2 (inklusive rätt särskiljningsregel):
  # Extrahera data från aktuell runda, och använd `get_leaderboard`-funktionen
  score_type <- case_when(
    results_type == "results_net" ~ "rel_net",
    results_type == "results_gross" ~ "rel_gross",
    TRUE ~ NA_character_  # <-- ska inte hända
  )
  score_r1 <-
    ds %>%
    mutate(
      # Behåller endast score från runda 1 i `rel_net` / `rel_gross`
      !!rlang::sym(score_type) := map(
        !!rlang::sym(score_type),
        ~ if(is.null(..1)) { NULL } else {slice(..1, 1:18)}
      )
    )
  winner_r1 <-
    get_leaderboard(ds = score_r1, my_score_type = score_type, my_tour_id) %>%
    slice(1) %>%
    pull(player)

  score_r2 <-
    ds %>%
    mutate(
      # Behåller endast score från runda 2 i `rel_net` / `rel_gross`
      !!rlang::sym(score_type) := map(
        !!rlang::sym(score_type),
        ~ if(is.null(..1)) { NULL } else {slice(..1, 19:36)}
      )
    )
  winner_r2 <-
    get_leaderboard(ds = score_r2, my_score_type = score_type, my_tour_id) %>%
    slice(1) %>%
    pull(player)


  # return ranking points gained at a given tour:
  #    player    place skills   tot
  #    <chr>     <int>  <int> <int>
  skills_winners <- c(longdrive_champ, close_champ, winner_r1, winner_r2)
  rank_points <-
    rank_points_place %>%
    mutate(
      skills = map_int(player, ~ sum(..1 == skills_winners, na.rm = TRUE)),
      tot = place + skills
    )

}
get_rank_data <- function(ds, results_type = "results_net", get = "ov") {
  # get = {"ov", "details", "rollsum"}

  # ranking poäng
  rank_pts_detailed <- map(ds$tour_id, calc_rankpoints, ds, results_type)
  names(rank_pts_detailed) <- ds$tour_id

  # tourerna som saknar score-kort returnerar NULL från `calc_rankpoints`,
  # stryk dessa:
  rank_pts_detailed <- rank_pts_detailed %>% discard(is.null)

  # formatera till tibble, med lämpliga kolumnnamn
  rank_pts_detailed <-
   rank_pts_detailed %>%
   imap(
     # add `tour_id` to colnames, exept first column (`player`)
     # https://stackoverflow.com/a/53969052
     ~ set_names(.x, c(names(.x)[1], str_c(names(.x)[-1], .y, sep = ".")))
    ) %>%
   purrr::reduce(full_join, by = "player")


  rank_pts_overview <-
    rank_pts_detailed %>%
    select(player, starts_with("tot")) %>%
    rename_with(str_remove, pattern = "tot.")


  # beräkna "rolling sum" för senaste 5 tourerna
  rank_ds <-
    rank_pts_overview %>%
    pivot_longer(cols = -player, names_to = "tour_id", values_to = "pts") %>%
    pivot_wider(id_cols = tour_id, names_from = player, values_from = "pts")
  rank_rollsum <-
    rank_ds %>%
    mutate_at(vars(-tour_id), ~ roll::roll_sum(., width = 5, min_obs = 1))


  if (get == "details") {

    return(rank_pts_detailed)

  } else if (get == "ov") {

    return(rank_pts_overview)

  } else if (get == "rollsum") {

    return(rank_rollsum)

  }
}
get_current_rank <- function(ds, results_type = "results_net", lastname) {

  rank_rollsum <- get_rank_data(ds, results_type, get = "rollsum")
  ranking <-
    rank_rollsum %>%
    slice(nrow(.)) %>%
    pivot_longer(cols = -tour_id, names_to = "player", values_to = "rank_pts") %>%
    mutate(rank = rank(-rank_pts, ties.method = "min", na.last = "keep")) %>%
    arrange(rank)

  # aktuell ranking för vald spelare
  ranking %>% filter(player == lastname) %>% pull(rank)

}
