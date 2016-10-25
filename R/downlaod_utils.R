#' URL
#'
#' url do captcha dependo do tj
#'
#' @param tj no momento só tem 'sp'
#'
url <- function(tj = "sp"){
  if(tj == "sp"){
    "https://esaj.tjsp.jus.br/cjsg/imagemCaptcha.do"
  }
}

#' Baixar captcha
#'
#' @param url do captcha
#' @param file arquivo para salvar o captcha
#'
baixar <- function(url, file =NULL){
  if(is.null(file)){
    file <- tempfile()
  }
  d <- httr::GET(url, httr::write_disk(file))
  return(file)
}

#' Baixar captchas
#'
#' @param n quantidade de captchas
#' @param url do capctha
#' @param folder pasta para salvar os capcthas baixados
#'
baixar_captchas <- function(n, url = url(), folder){
  for(i in 1:n){
    cp <- baixar(url)
    ver(cp)
    message("Digite o valor: ")
    x <- scan(nmax=1, what = character())
    file.copy(from = cp, to = sprintf("%s/%s.png", folder, x))
    file.remove(cp)
  }
}

#' Ver capctha
#'
#' @param file arquivo do captcha
#'
ver <- function(file){
  im <- png::readPNG(file)
  grid::grid.newpage()
  grid::grid.raster(im)
}
