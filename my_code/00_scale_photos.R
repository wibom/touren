library(tidyverse)
library(exifr)
library(magick)

# settings:

# - forcera scriptet att hantera alla bild-kataloger, även kataloger där
#   bilderna redan har skalats (användbart ifall innhållet i /Org-katalogen har
#   ändrats)
force_reprocessing <- FALSE

resize_img <- function(SourceFile, orient, new_name, target_dir, ...) {
  # https://cran.r-project.org/web/packages/magick/vignettes/intro.html#Cut_and_edit

  # Create target directory if necessary
  dir.create(target_dir, showWarnings = FALSE)

  # Leave small README in directory, if it does not exist
  readme_txt <- glue::glue(
    "
      Mapp med orginal-foton:
      {dirname(SourceFile)}

      Bilderna i denna mapp har kopierats och skalats mha ett R-script och
      `library(magick)`.
      "
  )
  readme_path <- file.path(target_dir, "README.txt")
  if(!file.exists(readme_path)) {
    write_file(readme_txt, readme_path)
  }

  # Resize and save to disk
  img <- magick::image_read(SourceFile)
  dim <- case_when(
    orient == "landscape" & str_detect(target_dir, "scaled") ~ "x400", # height = 400
    orient == "portrait"  & str_detect(target_dir, "scaled") ~ "400",  # width = 400
    orient == "square"    & str_detect(target_dir, "scaled") ~ "400",  # width = 400

    # https://stackoverflow.com/a/43178447
    str_detect(target_dir, "thumbs")                         ~ "100x67!"
  )
  magick::image_write(
    magick::image_scale(img, geometry = dim),
    path = file.path(target_dir, new_name)
  )

  return(NULL)
}
manage_photos <- function(path, force_reprocessing, target = "static/img") {
  org_dir <- file.path(path, "Org")
  target_dir <- file.path(here::here(), target, basename(path))

  str_extract(path, "tour\\d+") %>% cat('processing', ., "...")

  # Innehåller mappen några bilder?
  if(length(list.files(org_dir)) == 0) {
    cat(' -- empty, moving on...\n')
    invisible(return(NULL))
  }

  # Är bilderna redan skalade?
  if(!force_reprocessing && dir.exists(file.path(target_dir, "scaled")) ) {
    cat(' -- already processed, moving on...\n')
    invisible(return(NULL))
  }

  # # Ersätt eventuella svenska tecken, då dessa verkar superkrångliga att
  # # hantera med Exiftool:
  # #   - https://exiftool.org/forum/index.php?topic=5210.msg25174#msg25174
  # file.rename(
  #   from = file.path(path, list.files(imgdir)),
  #   to =
  #     file.path(path, list.files(imgdir)) %>%
  #     str_replace_all(c("å" = "a", "ä" = "a", "ö" = "o"))
  # )

  # EXIF ----
  # - Sortera m.a.p. 'CreateDate'
  # - annotera med nytt namn i nummerordning (xxx.JPG)
  # - annotera m.a.p. bild-orientering
  d_exif <-
    exifr::read_exif(
      path = org_dir,

      # urval av tillgänliga exif-data
      tags = c(
        "FileType",
        "FileTypeExtension",
        "Make",
        "Model",

        # Tid (https://exiftool.org/forum/index.php?topic=2568.0)
        #"CreateDate" time the file was written to the memory card
        #   - saknas för editerade bilder
        "DateTimeOriginal", # time of the shutter actuation

        # "ExifImageWidth",  saknas för vissa bilder som jag editerat
        # "ExifImageHeight"  saknar för vissa bilder som jag editerat
        "ImageWidth",
        "ImageHeight"
      ),
      recursive = TRUE,
      #quiet = FALSE
    ) %>%
    mutate(
      DateTimeOriginal = lubridate::as_datetime(DateTimeOriginal),
      orient = case_when(
        ImageWidth > ImageHeight ~ "landscape",
        ImageWidth < ImageHeight ~ "portrait",
        TRUE ~ "square"
      )
    ) %>%
    arrange(DateTimeOriginal) %>%
    mutate(
      id = row_number(),
      new_name =
        str_pad(id, width = 3, side = "left", pad = "0") %>%
        str_c(".", FileTypeExtension)
    )

  # Write EXIF-data to disk
  write_tsv(d_exif, path = file.path(org_dir, "exif.tsv"))

  # Photo Magick ----
  # Copy orginal photos and resize
  target_dir_scaled <- file.path(target_dir, "scaled")
  target_dir_thumbs <- file.path(target_dir, "thumbs")
  pwalk(
    .l = d_exif,
    .f = resize_img,
    target_dir = target_dir_scaled
  )
  pwalk(
    .l = d_exif,
    .f = resize_img,
    target_dir = target_dir_thumbs
  )
  cat(' done!\n')
}

main_dir <- file.path(here::here(), "photos_repo")
repo_dirs <-
  list.dirs(path = main_dir, full.names = TRUE, recursive = FALSE) %>%
  enframe(value = "path") %>%
  filter(str_detect(path, "tour\\d+$")) %>%
  select(path) # %>% slice(1:5)

map(repo_dirs$path, manage_photos, force_reprocessing)
