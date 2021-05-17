#setwd("/Users/James/Documents/R Projects/Strobot/Strobot/")

#https://stackoverflow.com/questions/50097136/how-to-play-audio-files-in-shiny-if-no-path-source-is-known
#Example sound cloud link that was taken from the js network load of a random sound cloud page
#https://cf-hls-media.sndcdn.com/playlist/TdXcOLV7PnQS.128.mp3/playlist.m3u8?Policy=eyJTdGF0ZW1lbnQiOlt7IlJlc291cmNlIjoiKjovL2NmLWhscy1tZWRpYS5zbmRjZG4uY29tL3BsYXlsaXN0L1RkWGNPTFY3UG5RUy4xMjgubXAzL3BsYXlsaXN0Lm0zdTgiLCJDb25kaXRpb24iOnsiRGF0ZUxlc3NUaGFuIjp7IkFXUzpFcG9jaFRpbWUiOjE1NDE1OTY4NDl9fX1dfQ__&Signature=pnXxmurmPRef38eCpGGfoMrftywX50mixFffIdBICLmBCQO-ckCTcJ5AxGuS5RQRCzYYLK1vyYjGHscM288aNat3Y7qYcneNHVZM-9mZGR8aIUOA4dkBMdvsxe5IjGnVFplXE0AHcb~5AQ0cOH2ZPUnLP7qF2Ib0eUzKJsGGOIVRbup-WNflRMtnSB042bCWi2-vqlQfDeQYYgMuG2IPONUfQTYHiriecKFWXpjqHl7alfA1qDfBgQ19aTCIbPAXqwceZWyl~y~Ndb5Hq6Kgx8zEIQKaU31sqUthPYEHu2GIGsk3rDfe2sgXn88~HDVMhElAt8JXK-3MjoY5QQ~btA__&Key-Pair-Id=APKAJAGZ7VMH2PFPW6UQ


library(shiny)
#library(ggplot2)
#library(DT)
library(shinyjs)
library(shinythemes)
library(magick)
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
shinyUI(
        fixedPage(
                  useShinyjs(),
                  includeCSS("www/style.css"),
                   extendShinyjs(text = jsResetCode, functions = c("history")),
                   tags$head(
                     tags$style(HTML("body {
                       background-color: #000000;
                     }
                      #sidebar {
            background-color: #000000;
            border-color: #000000;
        }"))
                   ),

  sidebarLayout(
    sidebarPanel(id = "sidebar",
      style = "position:fixed;width:inherit;",
                 width = 0,
                 tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
      tags$style(HTML("
                      .btn action-button {
                      display:block;
                      height: 100%;
                      width: 100%;
                      }
                      "))
    ),
    mainPanel(
      div(id="blanket",
      fixedRow(
      uiOutput("LView"),
      uiOutput("RView")#,
      )
      )
      )
    )
  )
  )

