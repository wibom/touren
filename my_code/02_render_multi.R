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
force_plr_reprocessing <- FALSE
force_img_reprocessing <- FALSE
force_res_reprocessing <- FALSE

# argument till pandoc ----
# Jag önskar använda R-kod som genererar html-snuttar, vilka ska utvärderas av
# pandoc tillsammans med övrig markdown när det slutgiltiga html-dokumentet
# skapas (Rmd --(rmarkdown)--> markdown --(pandoc)--> html). Jag önskar kunna
# använda intenderad html-kod för detta, för ökad läsbarhet. Per default tolkar
# pandoc detta som "kodsnuttar", typ (`<pre> min-html </pre>`).
# - https://stackoverflow.com/questions/39220389/embed-indented-html-in-markdown-with-pandoc
#
# Detta kan man lösa genom att lägga till strängen "-markdown_in_html_blocks"
# (eller "-markdown_in_html_blocks+raw_html") till `from`-argument när man ropar
# på pandoc. Pandoc-anropet genereras automatiskt av rmarkdown::render().
#
# Tidigare (2020-09) kunde man göra detta tillägg till pandocs `from` genom att
# specificera det i YAML-frontmatter i denna Rmd-fil:
# - https://stackoverflow.com/questions/39220389/embed-indented-html-in-markdown-with-pandoc
#
#
# Nu (2021-09) visar det sig att det pandoc-call som genereras av
# `rmarkdown::render()` innehåller ett default-värde som skickas till pandoc
# `from` (--from markdown+autolink_bare_uris+tex_math_single_backslash). Det
# värde som anges mha `pandoc_args` i YAML-frontmatter i denna Rmd-fil läggs
# till sist i pandoc-anropet men utvärderas aldrig.
# - https://stackoverflow.com/questions/63551238/how-can-i-override-default-pandoc-options-using-yaml-header-to-specify-github-ma
#
#
# Istället kan man ändra hur pandoc-anropas genom att inklduera
# `output_options = list(md_extensions = "-markdown_in_html_blocks")` i anropet
# till rmarkdown::render() (se `02_render_multi.R`).
# - https://stackoverflow.com/a/68335875/7439717


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
      clean = TRUE,
      run_pandoc = TRUE,
      output_options = list(md_extensions = "-markdown_in_html_blocks+raw_html")
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
      run_pandoc = TRUE,
      output_options = list(md_extensions = "-markdown_in_html_blocks+raw_html")
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
        run_pandoc = TRUE,
        output_options = list(md_extensions = "-markdown_in_html_blocks+raw_html")
      )
    }
  }
}

