# captchaSajData

[![Travis-CI Build Status](https://travis-ci.org/decryptr/captchaSajData.svg?branch=master)](https://travis-ci.org/decryptr/captchaSajData)

## Instalando

Você pode instalar o pacote usando:

```R
# install.packages("devtools")
devtools::install_github("decryptr/captchaSajData")
```

## Usando

Depois de instalar o pacote, s arquivos podem ser encontrados usando:

```R
head(list.files(system.file("tjsp/", package = "captchaSajData")))
```

Você também pode baixar mais captchas assim:

```R
baixar_captchas(95, url = url(), folder = "inst/tjsp")
```
