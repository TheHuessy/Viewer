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
                                        div(style="display: inline-block;vertical-align:top; width: 125px;",
                                            ## In order for these to be side by side, you need this div with this sort of style setting
                                            switchInput(inputId = "auto_toggle_btn",
                                                        onLabel = "Auto",
                                                        value = FALSE)
                                            ),
                                        div(style="display: inline-block;vertical-align:top; width: 125px;",
                                            actionButton("kill_switch", "X")
                                            ),
                            ## It looks like the reason the click box gets so small IS related to the resizing done via css
                            ## I should be able to fix that putting the imageOuput() in a div and specifying thr div size (maybe there's more with our example, but start there)
                            ## This will also mean that I should figure out the height/width resizing issue, use those generated values to fill in the height/width of the div
                            ## and pass that into the div args. 
                            ## Good chance that we will need to move this output over to a renderUI() statement in the server
                                        imageOutput("image_output",
                                                    click = "im_click")
)
                  )


