#' Baixar captcha
#'
#' Baixar um captcha dependendo do TJ
#'
#' @param files arquivos para salvar os captchas. Deve ter tamanho 1 se tipo for audio ou image e deve ter tamanho 2 se tipo for both (primeiro image depois audio).
#' @param tipo "audio" para audio e "image" para imagem, 'both' para ambos. Default "both".
#' @param tj TJ do captcha
#'
saj_baixar <- function(files, tipo = 'both', tj = 'sp') {
  saj_verificar_tipo_files(files, tipo)
  if (tj == 'sp') {
    if (tipo %in% c('both', 'audio')) {
      u0 <- 'https://esaj.tjsp.jus.br/cjsg/getArquivo.do'
      u_aud <- 'https://esaj.tjsp.jus.br/cjsg/somCaptcha.do'
      r0 <- httr::GET(u0)
      wd_aud <- httr::write_disk(files[length(files)], overwrite = TRUE)
    }
    if (tipo %in% c('both', 'image')) {
      u_img <- 'https://esaj.tjsp.jus.br/cjsg/imagemCaptcha.do'
      wd_img <- httr::write_disk(files[1], overwrite = TRUE)
    }
    if (tipo %in% c('both', 'image')) httr::GET(u_img, wd_img)
    if (tipo %in% c('both', 'audio')) httr::GET(u_aud, wd_aud)
  } else if (tj == 'sc') {
    if (tipo %in% c('both', 'audio')) {
      u0 <- 'http://esaj.tjsc.jus.br/cposgtj/open.do'
      u_aud <- 'http://esaj.tjsc.jus.br/cposgtj/somCaptcha.do?timestamp=&uuidCaptcha=%s'
      timestamp <- gsub('\\.', '', format(as.numeric(Sys.time()), nsmall = 13))
      r0 <- httr::GET(u0)
      wd_aud <- httr::write_disk(files[length(files)], overwrite = TRUE)
    }
    if (tipo %in% c('both', 'image')) {
      u_img <- 'http://esaj.tjsc.jus.br/cposgtj/imagemCaptcha.do'
      wd_img <- httr::write_disk(files[1], overwrite = TRUE)
    }
    if (tipo == 'image') {
      httr::GET(u_img, wd_img)
    } else if (tipo %in% c('both', 'audio')) {
      body <- list(timestamp = timestamp, uuidCaptcha = "", conversationId = "")
      if (tipo == 'both') {
        r1 <- httr::POST(u_img, body = body, wd_img)
      } else {
        r1 <- httr::POST(u_img, body = body)
      }
      uid <- jsonlite::fromJSON(httr::content(r1, "text"))$uuidCaptcha
      u_aud <- sprintf(u_aud, uid)
      httr::GET(u_aud, wd_aud)
    }
  }
  return(files)
}

saj_verificar_tipo_files <- function(files, tipo) {
  if (tipo == 'both' && length(files) != 2) {
    stop('Quando tipo for both, files deve ser um vetor de tamanho 2.')
  } else if (tipo %in% c('image', 'audio') && length(files) != 1) {
    stop('Quando tipo for image ou audio, files deve ser um vetor de tamanho 1.')
  }
}

#' Baixar e classificar captchas de imagem.
#'
#' Laço criado para baixar e classificar vários captchas de imagem e salvar numa pasta.
#'
#' @param N quantidade de captchas
#' @param baixar_audio se \code{TRUE} baixa o áudio além da imagem (necessário especificar \code{folder_audio}).
#' @param tj Tribunal de origem do captcha (por enquanto só "sp" e "sc").
#' @param folder pasta em que os captchas serão salvos.
#' @param folder_audio pasta para salvar os áudios.
#'
#' @export
saj_baixar_captchas <- function(N, baixar_audio = FALSE, tj = 'sp', folder, folder_audio = NULL){
  if (baixar_audio && is.null(folder_audio)) {
    stop('Necessario especificar o parametro folder audio.')
  }
  for(i in 1:N) {
    temp_img <- sprintf('%s/temp.png', folder)
    if (baixar_audio) {
      temp_aud <- sprintf('%s/temp.mpg', folder_audio)
      cp <- saj_baixar(files = c(temp_img, temp_aud), tipo = 'both', tj = tj)
      cp <- cp[1]
    } else {
      cp <- saj_baixar(files = temp_img, tipo = 'imagem', tj = tj)
    }
    saj_ver_imagem(cp)
    message("Digite o valor: ")
    predito <- scan(nmax = 1, what = character())
    file.copy(from = cp, to = sprintf("%s/%s.png", folder, predito))
    file.remove(cp)
  }
}

#' Ver captcha
#'
#' Desenha o captcha de imagem na tela.
#'
#' @param file arquivo do captcha.
#'
saj_ver_imagem <- function(file){
  im <- png::readPNG(file)
  grid::grid.newpage()
  grid::grid.raster(im)
}

#' Classifica automaticamente N captchas
#'
#' Utiliza a função \code{\link[captchaSajAudio]{decifrar}} que acerta praticamente 100\% dos áudios. A função baixa tanto os captchas de áudio quanto de imagem.
#'
#' @param N número de captchas a serem baixados e classificados automaticamente.
#' @param tj Qual o tribunal. Atualmente funciona para 'sc' e 'sp'
#' @param folder_img pasta onde os arquivos de imagem serão salvos. Default para pasta atual.
#' @param folder_aud pasta onde os arquivos de áudio serão salvos. Default para \code{folder_img}.
#' @param verbose imprimir mensagens de acompanhamento do número de captchas baixados.
#' @param intervalo número de captchas baixados para gerar mensagens caso \code{verbose == TRUE}.
#'
#' @export
saj_classificar_auto <- function(N, tj = 'sp',
                                 folder_img = getwd(), folder_aud = folder_img,
                                 verbose = TRUE, intervalo = 50L) {
  if (!requireNamespace('captchaSajAudio', quietly = TRUE)) {
    opcao <- utils::menu(title = 'Deseja instalar o pacote captchaSajAudio?',
                         choices = c('Sim', 'N\u00e3o'))
    if (opcao == 1L) {
      devtools::install_github('decryptr/captchaSajAudio')
    } else {
      stop('Necessario instalar o pacote captchaSajAudio.')
    }
  }
  if (tj %in% c('sp')) ext <- 'png' else if (tj %in% c('sc')) ext <- 'json'
  arq_img <- sprintf('%s/mudar.%s', folder_img, ext)
  arq_aud <- sprintf('%s/mudar.mpg', folder_aud)
  n <- 0L
  if (verbose) cat('Iniciando download...\n')
  while (n < N) {
    if ((n %% intervalo == 0) && verbose) cat('ja foram', n, '\n')
    Sys.sleep(1)
    try({
      httr::set_config(httr::config(ssl_verifypeer = 0L))
      result <- testar_processo(tj, arq_img, arq_aud)
    }, silent = TRUE)
    if (result[1]) {
      file.copy(arq_img, sprintf('%s/%s.%s', folder_img, result[2], ext))
      file.copy(arq_aud, sprintf('%s/%s.mpg', folder_aud, result[2]))
      n <- n + 1
    }
  }
  file.remove(arq_img)
  file.remove(arq_aud)
  invisible()
}

testar_processo <- function(tj, arq_img, arq_aud) {
  folder_img <- dirname(arq_img)
  folder_aud <- dirname(arq_aud)
  if (tj == 'sp') {
    httr::handle_reset('https://esaj.tjsp.jus.br/cjsg')
    u0 <- 'https://esaj.tjsp.jus.br/cjsg/getArquivo.do?cdAcordao=9922251&cdForo=0'
    r0 <- httr::GET(u0)
    u_aud <- 'https://esaj.tjsp.jus.br/cjsg/somCaptcha.do'
    u_img <- 'https://esaj.tjsp.jus.br/cjsg/imagemCaptcha.do'
    httr::GET(u_img, httr::write_disk(arq_img, overwrite = TRUE))
    httr::GET(u_aud, httr::write_disk(arq_aud, overwrite = TRUE))
    res <- captchaSajAudio::decifrar(arq_aud)
    u1 <- paste0(u0, '&vlCaptcha=', res)
    r1 <- httr::GET(u1)$all_headers[[1]]$headers[['content-type']]
    passou <- r1 == "application/pdf;charset=UTF-8"
  } else if (tj == 'sc') {
    u0 <- 'http://esaj.tjsc.jus.br/cposgtj/open.do'
    httr::handle_reset(u0)
    u_img <- 'http://esaj.tjsc.jus.br/cposgtj/imagemCaptcha.do'
    u_aud <- 'http://esaj.tjsc.jus.br/cposgtj/somCaptcha.do?timestamp=&uuidCaptcha=%s'
    r0 <- httr::GET(u0)
    timestamp <- gsub('\\.', '', format(as.numeric(Sys.time()), nsmall = 13))
    wd_aud <- httr::write_disk(arq_aud, overwrite = TRUE)
    wd_img <- httr::write_disk(arq_img, overwrite = TRUE)
    body <- list(timestamp = timestamp, uuidCaptcha = "", conversationId = "")
    r1 <- httr::POST(u_img, body = body, wd_img)
    uid <- jsonlite::fromJSON(httr::content(r1, "text"))$uuidCaptcha
    u_aud <- sprintf(u_aud, uid)
    httr::GET(u_aud, wd_aud)
    res <- captchaSajAudio::decifrar(arq_aud)
    dados <- list('conversationId' = '',
                  'paginaConsulta' = '1',
                  'cbPesquisa' = 'NUMPROC',
                  'tipoNuProcesso' = 'UNIFICADO',
                  'numeroDigitoAnoUnificado' = '0015254-04.2009',
                  'foroNumeroUnificado' = '0018',
                  'dePesquisaNuUnificado' = '0015254-04.2009.8.24.0018',
                  'dePesquisa' = '',
                  'uuidCaptcha' = uid,
                  'vlCaptcha' = res,
                  'novoVlCaptcha' = '')
    r_proc <- httr::GET('http://esaj.tjsc.jus.br/cposgtj/search.do', query = dados)
    passou <- grepl('Dados do Processo', httr::content(r_proc, 'text'))
  }
  return(c(passou, res))
}
