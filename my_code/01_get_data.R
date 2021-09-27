source(
  file.path(
    "C:/Users/caewim02/Documents/_Personal_/golf/Touren_blogdown/my_code",
    "helpers.R"
  ),
  encoding = "UTF8"
)

# Läs data från disk ----
read_courses <- function (tour_id, mydir) {
  not_available <- c("tour01", "tour02", "tour03", "tour04", "tour20")
  if (tour_id %in% not_available) { return(NULL) }

  courses <-
    file.path(mydir, "data/scores/courses.xlsx") %>%
    readxl::read_excel(sheet = tour_id) %>%
    filter(hole_id != "sum") %>%
    filter(complete.cases(.)) %>%
    mutate_if(is.numeric, as.integer)
  return(courses)
}
read_shots_given <- function (mytour_id, mydir, courses) {

  # Läs från disk
  shots_given <-
    file.path(mydir, "data/scores/scores.xlsx") %>%
    readxl::read_excel(sheet = mytour_id) %>%
    filter(str_detect(hole_id, "^erh.")) %>%
    filter(complete.cases(.)) %>%
    mutate(round_nr = str_remove(hole_id, "erh.slag_")) %>%
    select(round_nr, everything(), -hole_id)
  player_names <- shots_given %>% select(-round_nr) %>% names()

  # Beräkna givna slag för varje spelare / hål
  tour_courses <-
    courses %>%
    filter(tour_id == mytour_id) %>%
    unnest(cols = courses) %>%
    mutate(hole_nr = str_extract(hole_id, "[[:digit:]]+$") %>% as.integer())
  if ("round" %in% names(tour_courses)) {
    # ifall round-id är definierad i excel-filen (pga ngn runda är splittad)
    tour_courses <-
      tour_courses %>%
      mutate(round_nr = as.character(round)) %>%
      select(-round)
  } else {
    # i vanliga fall är round-id INTE definierad i excel-filen
    tour_courses <-
      tour_courses %>%
      mutate(
        round_nr = (hole_nr %/% 18.1) + 1,
        round_nr = as.character(round_nr)
      )
  }

  shots_given <- left_join(tour_courses, shots_given, by = "round_nr")
  for (player in player_names) {
    player <- rlang::sym(player)
    calc_shots_per_hole <- function (shots_tot, hcp_hole) {
      (shots_tot - hcp_hole) %/% 18 + 1
    }
    shots_given <- shots_given %>%
      mutate(!! player := calc_shots_per_hole(!! player, hcp))
  }

  r <-
    shots_given %>%
    select(-tour_id, -(par:round_nr)) %>%
    mutate_if(is.numeric, as.integer)

  return (r)
}
read_scores <- function (tour_id, mydir) {
  not_available <- c("tour01", "tour02", "tour03", "tour04", "tour20")
  if (tour_id %in% not_available) { return(NULL) }

  scores <-
    file.path(mydir, "data/scores/scores.xlsx") %>%
    readxl::read_excel(sheet = tour_id) %>%
    filter(str_detect(hole_id, "^hole_")) %>%
    filter(complete.cases(.)) %>%
    mutate_if(is.numeric, as.integer)
  return(scores)
}
tours <- read_tours_list()
courses <-
  tours %>%
  slice(-c(1:4, 20)) %>%
  select(tour_id) %>%
  mutate(courses = map(tour_id, read_courses, mydir))
shots_given <-
  tours %>%
  slice(-c(1:4, 20)) %>%
  select(tour_id) %>%
  mutate(shots_given = map(tour_id, read_shots_given, mydir, courses))

# Lägg ihop data ----
calc_net <- function(given, scores) {
  if(is.null(scores)) { return(NULL) }

  if (nrow(given) != nrow(scores)) {
    # ifall koden körs under pågående tour kan data vara registrerat för
    # `given` men ej för `scores` (givet att jag i förväg fyllt i baninformation
    #  och hcp)
    #
    # bortse från det jag baninformation och hcp ifyllt i förväg:
    given <- given %>% slice(1:nrow(scores))
  }
  players <- scores %>% select(-hole_id) %>% names()
  net <-
    bind_cols(
      scores[, "hole_id"],
      scores[, players] - given[, players]
    )
  return (net)
}
calc_rel <- function(scores, course) {
  if(is.null(scores)) { return(NULL) }

  players <- scores %>% select(-hole_id) %>% names()
  rel_scores <- bind_cols(scores[, "hole_id"], scores[, players] - course$par)
  return(rel_scores)
}
calc_point <- function(rel_net) {
  if(is.null(rel_net)) { return(NULL) }

  rel_net %>%
    mutate_at(
      .vars = vars(-hole_id),
      .funs = ~ case_when(
        . == -5 ~ 7L,
        . == -4 ~ 6L,
        . == -3 ~ 5L,
        . == -2 ~ 4L,
        . == -1 ~ 3L,
        . == 0 ~ 2L,
        . == 1 ~ 1L,
        TRUE ~ 0L
      )
    )
}
get_shot_type <- function(rel_gross) {
  if (is.null(rel_gross)) { return(NULL) }

  rel_gross %>%
    mutate_at(
      .vars = vars(-hole_id),
      .funs = ~
        case_when(
          . < 0 ~ "< par",
          . == 0 ~ "par",
          . == 1 ~ "boogie",
          . == 2 ~ "d_boogie",
          . > 2  ~ "> d_boogie",
          TRUE   ~ NA_character_
        ) %>%
        factor(levels = c("< par", "par", "boogie", "d_boogie", "> d_boogie"))
    )
}
d <-
  tours %>%
  nest(meta = -tour_id) %>%
  left_join(shots_given, by = "tour_id") %>%
  left_join(courses, by = "tour_id") %>%
  mutate(
    abs_gross  = map(tour_id, read_scores, mydir),
    abs_net    = map2(shots_given, abs_gross, calc_net),
    rel_gross  = map2(abs_gross, courses, calc_rel),
    rel_net    = map2(abs_net,   courses, calc_rel),
    points     = map(rel_net, calc_point),
    shot_types = map(rel_gross, get_shot_type)
  )

# Beräkna leader-boards + extra meta-data ----
get_results <- function(ds, score_type) {

  get_pos <- function(x, pos) {
    ridx <- case_when(
      pos == "champ"            ~ 1L,
      pos == "champ_runnerup_1" ~ 2L,
      pos == "champ_runnerup_2" ~ 3L,
      pos == "loser"            ~ as.integer(nrow(x)),
      pos == "loser_runnerup"   ~ as.integer(nrow(x) - 1)
    )
    if_else(is.na(x$pos[ridx]), NA_character_, x$player[ridx])
  }
  get_margin <- function(x, marg) {
    ridx <- case_when(
      marg == "win_marg"  ~ 1L:2L,
      marg == "lose_marg" ~ as.integer((nrow(x)-1):nrow(x))
    )
    x %>% slice(ridx) %>% pull(tot) %>% diff()
  }

  tour_results <-
    ds %>%
    select(tour_id, meta) %>%
    unnest(meta) %>%
    mutate(
      lb = tour_id %>% map(~get_leaderboard(ds, score_type, .x)),
      champ = map_chr(lb, get_pos, pos = "champ"),
      champ_runnerup_1 = map_chr(lb, get_pos, pos = "champ_runnerup_1"),
      champ_runnerup_2 = map_chr(lb, get_pos, pos = "champ_runnerup_2"),
      loser            = map_chr(lb, get_pos, pos = "loser"),
      loser_runnerup   = map_chr(lb, get_pos, pos = "loser_runnerup"),
      margin_win = map_int(lb, get_margin, marg = "win_marg"),
      margin_lose = map_int(lb, get_margin, marg = "lose_marg")
    )
  tour_results %>% select(-c(roman_numeral:date)) %>% select(-lb, everything())
}
results_net   <- get_results(d, score_type = "rel_net")
results_gross <- get_results(d, score_type = "rel_gross")
d <-
  d %>%
  left_join(
    results_net %>% nest(results_net = -tour_id),
    by = "tour_id"
  ) %>%
  left_join(
    results_gross %>% nest(results_gross = -tour_id),
    by = "tour_id"
  )

# Exportera data ----
readr::write_rds(d, path = "./my_code/data/complete_data.RDS")
