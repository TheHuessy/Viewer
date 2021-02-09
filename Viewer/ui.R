## THIS: https://livefreeordichotomize.com/2017/03/12/introducing-shinyswipr-swipe-your-way-to-a-great-shiny-ui/

## HOLY SHIT, ALSO THIS: https://github.com/RinteRface/cheatsheets/blob/master/shinyMobile/shinyMobile.pdf

library(shiny)
library(shinyWidgets)
library(magick)
library(shinyjs)


shinyUI(
        fixedPage(
                  ## GET RID OF SIDEBAR
                  tags$head(tags$script('
                                        var dimension = [0, 0];
                                        $(document).on("shiny:connected", function(e) {
                                                         dimension[0] = window.innerWidth;
                                                         dimension[1] = window.innerHeight;
                                                         Shiny.onInputChange("dimension", dimension);});
                                        $(window).resize(function(e) {
                                                           dimension[0] = window.innerWidth;
                                                           dimension[1] = window.innerHeight;
                                                           Shiny.onInputChange("dimension", dimension);});')
                                        ),
                            useShinyjs(),

                            includeCSS("www/style.css"),
                            div(style="display: inline-block;vertical-align:top; width: 100px;",
                                ## In order for these to be side by side, you need this div with this sort of style setting
                                    actionButton("options_button", "O"),
                                ),
                            div(style="display: inline-block;vertical-align:top; width: 100px;",
                                actionButton("kill_switch", "X")
                                ),
                            uiOutput("image_div_object")

                  )
        )


