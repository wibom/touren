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
)# %>% slice(1:12)
players <- read_players_list() %>% filter(id != "Ingen") %>% pull(id)
tours <- d$tour_id

# re-processing settings:
force_plr_reprocessing <- TRUE
force_img_reprocessing <- TRUE
force_res_reprocessing <- TRUE

# Spelare ----
#players <- players[1]
for (player in players) {
  player_formated <-
    str_to_lower(player) %>%
    str_replace_all("[åä]", "a") %>%
    str_replace_all("[ö]", "o")

  # create player-page if
  # 1) it does not already exist
  #    - or -
  # 2) it should be re-processed
  plr_processed <- file.exists(
    glue::glue("./content/players/{player_formated}.md")
  )
  if(!plr_processed | force_plr_reprocessing) {
    rmarkdown::render(
      input = "my_code/player.Rmd",
      output_file = player_formated,
      output_dir = "content/players",
      clean = FALSE,
      run_pandoc = TRUE
    )
  }
}




# Tourer ----
results_var <- "results_net"
abs_or_rel <- "rel"
my_weights <- setNames(seq_along(rev(tours)), rev(tours))
for(tour in tours) {

  if (tour == "tour20") { next } # no data available

  # create results-page if
  # 1) it does not already exist
  #    - or -
  # 2) it should be re-processed
  res_processed <- file.exists(
    glue::glue("./content/results/{tour}.md")
  )
  if(!res_processed | force_res_reprocessing) {

    weight = my_weights[tour] %>% unname()
    rmarkdown::render(
      input = "my_code/tour.Rmd",
      output_file = tour,
      output_dir = "content/results",
      clean = TRUE,
      run_pandoc = TRUE
    )
  }
}


# Bilder ----
for(tour in tours) {

  if(dir.exists(file.path(here::here(), 'static/img', tour, 'scaled'))){

    # create photo-page if
    # 1) it does not already exist
    #    - or -
    # 2) it should be re-processed
    img_processed <- file.exists(glue::glue("./content/pics/photos_{tour}.md"))
    if(!img_processed | force_img_reprocessing) {
      rmarkdown::render(
        input = "my_code/photo.Rmd",
        output_file = paste0("photos_", tour),
        output_dir = "content/pics",
        clean = TRUE,
        run_pandoc = TRUE
      )
    }
  }
}

