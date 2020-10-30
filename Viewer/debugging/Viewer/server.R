library(shiny)
library(shinyWidgets)
library(magick)
library(RPostgreSQL)
library(yaml)
library(httr)


###### LOG THE DATE #####
the_date <- format(Sys.time(), "%Y-%m-%d %H:%m")
print(the_date)
#########################



##### PULL IN DATA FROM POSTGRES #####

creds <- read_yaml(Sys.getenv('CREDS_PATH'))

sql_driver <- dbDriver("PostgreSQL")

sql_con <- dbConnect(sql_driver,
                     host=creds$pg_host,
                     user=creds$pg_user,
                     password=creds$pg_pw,
                     dbname="strobot"
)

query = "SELECT end_link, 'culling_direct' as table_name FROM culling_direct WHERE keep = 1
UNION
SELECT piece as end_link, 'culling_external' as table_name FROM culling_external WHERE keep = 1
UNION
SELECT end_link, 'pulls' as table_name FROM pulls WHERE link_type = 'Direct'"

viewables <- dbGetQuery(sql_con,
                        statement=query)
dbDisconnect(sql_con)

tot <- nrow(viewables)

auto_switch <<- FALSE

removes <- data.frame(end_link=c(),table_name=c())

remove_duds <- function(removes_df){
  sql_con <- dbConnect(sql_driver,
                       host=creds$pg_host,
                       user=creds$pg_user,
                       password=creds$pg_pw,
                       dbname="strobot"
  )

  for (i in 1:nrow(removes_df)){
    rm_link <- removes_df[i,1]
    rm_table <- removes_df[i,2]
    delete_row_statement <- paste("DELETE FROM ",
                                  rm_table,
                                  " WHERE end_link = '",
                                  rm_link,
                                  "'",
                                  sep=""
    )

    print(delete_row_statement)
    dbSendQuery(sql_con, delete_row_statement)
  }
  dbDisconnect(sql_con)
}




get_cnt_safe <- function(viewables,removes){
  internal_cnt <<- sample(x = 1:tot, size = 1, replace = TRUE)
  while (TRUE){
    test_url <<- GET(get_link(internal_cnt))
    if (test_url$status_code != 200){
      removes <<- rbind(data.frame(end_link=viewables[internal_cnt,1],table_name=viewables[internal_cnt,2]),removes)
      viewables <<- viewables[-c(internal_cnt),,drop=FALSE]
      internal_cnt <<- sample(x = 1:tot, size = 1, replace = TRUE)
    } else {
      break
    }
  }
  return(internal_cnt)
}

insta_fresh <- function(piece){
  pre_url <- paste("https://www.instagram.com",piece, "media/?size=l", sep="")
  fresh_url = GET(pre_url)
  return(fresh_url$url)
}


get_link <- function(cnt){
  test_link <- viewables[cnt,1]
  test <- grep(pattern="/p/", x=test_link)
  if (length(test) == 0){
    output_link <- viewables[cnt,1]
  } else {
    output_link <- insta_fresh(viewables[cnt,1])
  }
  return(output_link)
}



cnt <<- get_cnt_safe(viewables,removes)


shinyServer(function(input, output, session) {

###################
# UI IMAGE OUTPUT #
###################

#              output$image_div_object <<- renderUI({
#                div(class = "image_container",
#                    imageOutput("image_output",
#                                click = "im_click")
#                    )
#              })




#############
# FUNCTIONS #
#############
              load_image <- function(){
                output$image_output <<- renderImage({
                  list(src = "tmp.jpg",
                       contentType = "imaage/jpeg")},
                  deleteFile = TRUE)
              }

              buffer_new_image <- function(cnt){
                img_link <<- get_link(cnt)
                #tall: "https://i.imgur.com/j526xVz.jpg"
#                img_link <<- "https://i.imgur.com/K3Xp8NG.jpg"
                #wide: "https://i.imgur.com/K3Xp8NG.jpg"
#                img_link <<- "https://i.imgur.com/j526xVz.jpg"
                print(img_link)
                img <- image_read(img_link)
                info <- image_info(img)
                image_write(img,"tmp.jpg")
                return(info)
              }
              observeEvent(input$dimension,{
                             screen_width <<- isolate(input$dimension[1])
                             screen_height <<- isolate(input$dimension[2])
})



              onStop(function(){
#                       if (nrow(removes) != 0){
#                         remove_duds(removes)
#                       }
                       dbDisconnect(sql_con)
                       print("Session disconnected")
})
              img_dat <<- buffer_new_image(cnt)

              output$image_div_object <- renderUI({
#                div_height = screen_height-46.2
#                div_height <- input$dimension[2]-46.2
#                print(div_height)

#                div(class = "image_container",
#                    style=paste("height:",input$dimension[2]),
                    imageOutput("image_output",
#                                height = input$dimension[2]-46.2,
                                height = "100%",
                                click = "im_click")
#                    )
              })




              ########################
              # DEBUG TESTING OUTPUT #
              ########################

#              fit_height <- function(image_height){
#                dim_height <- reactive({input$dimension[2]})
#                height_conv_fctr <- reactive({dim_height()/image_height})
#                new_height <- image_height*isolate(height_conv_fctr())
#                return(new_height)
#              }

#              fit_width <- function(image_width){
#                dim_width <- reactive({input$dimension[1]})
#                width_conv_fctr <- reactive({dim_width()/image_width})
#                new_width <- image_width*as.numeric(isolate(width_conv_fctr()))
#                return(new_width)
#              }





#              dim_width <- reactive({input$dimension[0]})
#              dim_height <- reactive({input$dimension[1]})

#              test_image_height <- 720
#              test_image_width <- 1280

#              height_conv_fctr <- reactive({dim_height()/test_image_height})
#              width_conv_fctr <- reactive({dim_width()/test_image_width})
              #
              ## Going to have to build a funciton that takes the link, makes these vars, writes the image, then releases the link like the img def does above
              test_im_width <- image_info(image_read(img_link))$width
              test_im_height <- image_info(image_read(img_link))$height
              print(image_info(image_read(img_link)))
              print(test_im_width)
              print(test_im_height)
#              print("=======")
              print("=======")
#              observeEvent(input$dimension,{
#                             screen_width <<- isolate(input$dimension[1])
#                             screen_height <<- isolate(input$dimension[2])
#                             gen_width <- test_im_width * reactive({input$dimension[1]/image_width})
#                             print(paste("width:", screen_width))
#                             print(paste("height:", screen_height))
#})
#              observeEvent(input$dimension,{new_height <<- isolate(fit_height(test_im_height))})
#              print(new_width)
#              print(new_height)
#              print(reactive({(fit_height(test_im_height))}))


#              output$testing_output <- renderPrint({paste("Width:", fit_width(test_im_width), "\n", "Height:", fit_height(test_im_height))})




              ########################
              # Initial Image Output #  
              ########################

              load_image()

              ##### KILL BUTTON #####
              observeEvent(input$kill_switch, {
                             removes <<- rbind(data.frame(end_link=viewables[cnt,1],table_name=viewables[cnt,2]), removes)
                             print(paste("Added", viewables[cnt,1], "to removes"))
              })



              ##### TIMER ADVANCEMENT #####
              observeEvent(input$auto_toggle_btn,{
                             if (input$auto_toggle_btn == TRUE){
                               auto_switch <<- TRUE
                             } else {
                               auto_switch <<- FALSE
                             }
              })
              ##############################  TIMER  ###############################

              autoad <- reactiveTimer(intervalMs =  as.numeric(Sys.getenv('VIEWER_INTERVAL')))

              ######################################################################

              observe({
                autoad()
                if (isolate(auto_switch) == TRUE){
                  cnt <<- get_cnt_safe(viewables,removes)

                  img_dat <<- buffer_new_image(cnt)
                  load_image()
                }
              })

              ##### TAP ADVANCEMENT #####

              ## IF YOU'RE HAVING ISSUES WITH IT NOT ADVANCING, TRY CLICKING ON THE UPPER LEFT CORNER OF THE IMAGE
              ## It looks like it only counts clicks on the actual space the image takes up and not the space that the css file fills to meet the width requirement

              observeEvent(input$im_click, {
                             cnt <<- get_cnt_safe(viewables,removes)
                             img_dat <<- buffer_new_image(cnt)
                             load_image()

              })
    })

