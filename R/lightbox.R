

#' @title Create Lightbox Gallery
#' @param df data.frame containing
#' @param gallery character label identifying gallery
#' @param css character file path to css file. If missing default style is used
#' @param path character folder path, sometimes some frameworks like shiny and 
#' blogdown can look for files in a specific location. 
#' @param width integer thumbnail image size in pixels
#' @param display character 
#'
#' @importFrom shiny tags includeScript includeCSS
#' @importFrom glue glue glue_collapse
#' @importFrom digest sha1
#' @importFrom fs dir_copy
#' 
#' @return
#' @export
#'
#' @examples
lightbox_gallery <- function(df, gallery, css, path = '', width = 80, display = 'block'){
  
  dir.create('www')
  
  if (missing(css)) {
    css <- file.path(system.file('css', package = 'gallerier'), 
                    "styles.css")
  }
  if (!(dir.exists('www/lightbox-2-2.11.3'))) {
    fs::dir_copy(system.file('js/lightbox-2-2.11.3', package = 'gallerier'), 
                 'www/lightbox-2-2.11.3')
  }
  
  # ensure all required columns exist in df
  #if (!('description' %in% colnames(df))) df$description <- NA
  if (!('uid' %in% colnames(df))) df$uid <- strtrim(digest::sha1(df$src), 5)
  
  tags$div(style = sprintf('display: %s;', display),
           tagList(tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "www/lightbox-2-2.11.3/css/lightbox.min.css")
                   ),
                   tags$div(class = 'card-deck',
                            lapply(seq_len(nrow(df)), function(i){
                              tags$div(`data-type`="template", class = 'card',
                                       tags$a(id = df$uid[i],
                                              href = paste0(path, df$src[i]),
                                              `data-lightbox` = gallery, # this identifies gallery group
                                              `data-title` = glue_collapse(df[i,], sep = ' - '), # this is where complex title (glue) added
                                              tags$img(class = 'card-img-top',
                                                       src = paste0(path, df$src[i]),
                                                       width = glue('{width}px'),
                                                       height = 'auto'))
                                       )
                            })
                   ),
                   includeScript("www/lightbox-2-2.11.3/js/lightbox.min.js"),
                   includeCSS(css)
           ))
  
}


#' Lightbox Dependancies
#'
#' @return
#'
#' @examples
lightbox_rmd <- function(){
  cat('<link rel="stylesheet" href="www/lightbox-2-2.11.3/css/lightbox.min.css">\n',
      '<script src="www/lightbox-2-2.11.3/js/lightbox.min.js"></script>')
}
