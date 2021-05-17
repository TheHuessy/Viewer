##### STROBOT #####
library(shiny)
library(shinyWidgets)
library(magick)
library(RPostgreSQL)
library(yaml)
library(httr)



creds_path <<- Sys.getenv('CREDS_PATH')

if (nchar(creds_path) <= 0) {
  print("Master Variables not sourced, killing app...")
  quit(save="no")
}

creds <<- read_yaml(creds_path)

pull_data <- function(){

  sql_driver <- dbDriver("PostgreSQL")

  sql_con <- dbConnect(sql_driver,
                       host=creds$pg_host,
                       user=creds$pg_user,
                       password=creds$pg_pw,
                       #dbname="strobot"
                       dbname="decoy"
  )

#  query = "SELECT DISTINCT end_link FROM culling_direct WHERE keep = 1
#  UNION
#  SELECT end_link, 'pulls' as table_name FROM pulls WHERE link_type = 'Direct'"

  query = "SELECT DISTINCT end_link FROM dummy_ims"


  viewables <<- dbGetQuery(sql_con,
                          statement=query)
  dbDisconnect(sql_con)

  tot <<- nrow(viewables)

  ################
  print(viewables)
  ################
}
removes <<- data.frame(end_link=c(),table_name=c())

remove_duds <- function(removes_df){
  sql_driver <- dbDriver("PostgreSQL")

  sql_con <- dbConnect(sql_driver,
                       host=creds$pg_host,
                       user=creds$pg_user,
                       password=creds$pg_pw,
#                       dbname="strobot"
                       dbname="decoy"
  )

  for (i in 1:nrow(removes_df)){
    rm_link <- removes_df[i,1]
    rm_table <- removes_df[i,2]
    if (grepl("/p/", rm_link, fixed = TRUE)) {
      link_var <- "piece"
      delete_row_statement <- paste("UPDATE ",
                                    tbl_name,
                                    " SET keep = 9",
                                    " WHERE ",
                                    link_var,
                                    " = '",
                                    rm_link,
                                    "'",
                                    sep = ""
      )
    } else {

      link_var <- "end_link"
      delete_row_statement <- paste("DELETE FROM ",
                                  rm_table,
                                  " WHERE ",
                                  link_var," = '",
                                  rm_link,
                                  "'",
                                  sep=""
    )


    }

    print(delete_row_statement)
    dbSendQuery(sql_con, delete_row_statement)
  }
  dbDisconnect(sql_con)
}

get_cnt_safe <- function(viewables,removes){
  internal_cnt <<- sample(x = 1:tot, size = 1, replace = TRUE)
  while (TRUE){
    test_url <<- GET(get_link(internal_cnt))
    if (test_url$status_code != 200 || grepl("removed.png", test_url$url) == TRUE){
      removes <<- rbind(data.frame(end_link=viewables[internal_cnt,1],table_name=viewables[internal_cnt,2]),removes)
      viewables <<- viewables[-c(internal_cnt),,drop=FALSE]
      internal_cnt <<- sample(x = 1:tot, size = 1, replace = TRUE)
    } else {
      break
    }
  }
  return(internal_cnt)
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
ensure_image_load <- function(cnt){
  tryCatch(
           {
             img_link <<- get_link(cnt)
             img_obj <<- image_read(img_link)
             image_write(img_obj,"tmpR.jpg")
             image_write(img_obj,"tmpL.jpg")

             return(TRUE)
           },
           error = function(err){
             print(paste("error while loading a new image:", err, sep = "\n"))
             return(FALSE)
           }
  )
}

buffer_new_image <- function(cnt){
  im_buff_flag <- ensure_image_load(cnt)
  ##############
  print(im_buff_flag)
  ##############

  while(im_buff_flag == FALSE) {
    cnt <<- get_cnt_safe(viewables,removes)
    im_buff_flag <<- ensure_image_load(cnt)
  }
  raw_info <<- image_info(img_obj)
}

calc_conv <- function(max,raw){
  return(max/raw)
}

get_output_dims <- function(screen_width, screen_height, raw_width, raw_height){
  if (raw_width > raw_height){
    conv <- calc_conv(((screen_width/2)-20), raw_width)
  } else {
    conv <- calc_conv((screen_height-40), raw_height)
  }

  return(list(width=(raw_width*conv), height=(raw_height*conv)))
}

pull_data()
HIDE_FLAG <<- TRUE
cnt <<- get_cnt_safe(viewables,removes)
buffer_new_image(cnt)

shinyServer(function(input, output, session) {
                  wh <<- reactive({input$dimension[2]})
                  ww <<- reactive({input$dimension[1]})


              load_views <- function(){
                ## Execute math on how big the images should be here
                ##      You can still define most of the functions above
                ##      You want to be able to get what the dims are 
                ##      for each image
                ##      raw_width > max_display_width > output_width
                ##
                ##      Conversion factor should be fine, then apply it to
                ##      the image raw width/height
                ##
                ##      Cov = max/raw
                ##      output = raw * conv
                ##
                ##      What if an image is extra wide/extra tall?
                ##      Need to have constraints for each dim to decide
                ##      which dim we are keying the conversion to.
                ##      Example, if it's extra tall, we actually want to key
                ##      the conversion to the height, not width.
                ##      Or just which ever dim is the largest?
                ##          Seems simple enough
                ##
                ##      Function sketch:
                ##      output: list of width and height values
                ##      inputs: Screen max height, screen max width, raw image width, raw image height
#                get_output_dims(ww(), wh(), raw_info$width, raw_info$height)

                  output$LView <<- renderUI({
                    output_dims <- get_output_dims(ww(), wh(), raw_info$width, raw_info$height)
                    fixedPanel({
                      actionButton("nextL", label = "")
                      tags$button(
                                  id = "nextL",
                                  class = "btn action-button",
                                  tags$img(src = "tmpL.jpg",
                                           width = ((ww()/2)-20), height = (wh()-40))
                      )
                      tags$style(HTML("
                                      .btn {
                                        display:block;
                                        height: 100%;
                                        width: 100%;
                                        max-height: 100%;
                                        max-width: 100%;
                                      }
                                      "))
                      renderImage({
                        list(src = "tmpL.jpg",
#                             width = ((ww()/2)-20),
#                             height = (wh()-40),
                             width = output_dims$width,
                             height = output_dims$height,
                             contentType = "image/jpeg"
                        )
                    }, deleteFile = TRUE)},
                               left = 15,
                               top = 15,
                               bottom = 0
                    )
                  })

                  output$RView <<- renderUI({
                    output_dims <- get_output_dims(ww(), wh(), raw_info$width, raw_info$height)
                    fixedPanel({
                      actionButton("nextR", label = "")
                      tags$button(
                                  id = "nextR",
                                  class = "btn action-button",
                                  tags$img(src = "tmpR.jpg",
                                           width = ((ww()/2)-20), height = (wh()-40))
                      )
                      tags$style(HTML("
                                      .btn {
                                        display:block;
                                        height: 100px;
                                        width: 50px;
                                      }
                                      "))
                      renderImage({
                        list(src = "tmpR.jpg",
                             width = output_dims$width,
                             height = output_dims$height,
#                             width = ((ww()/2)-20),
#                             height = (wh()-40),
                             contentType = "image/jpeg"
                        )
                    }, deleteFile = TRUE)},
                               right = 15,
                               top = 15,
                               bottom = 0
                    )

                  })
              }


              next_image <- function(){
                cnt <<- get_cnt_safe(viewables,removes)
                buffer_new_image(cnt)
                load_views()
              }


              onStop(function(){
                       if (nrow(removes) != 0){
                         remove_duds(removes)
                       }

                       if (exists("sql_con")){
                         dbDisconnect(sql_con)
                       }

                       if (file.exists("tmpR.jpg")){
                         file.remove("tmpR.jpg")
                         print("Removed right tmp file")
                       }

                       if (file.exists("tmpL.jpg")){
                         file.remove("tmpL.jpg")
                         print("Removed left tmp file")
                       }

                       print("Session disconnected")
                       print("=====================")
                })

              onclick("blanket",{ ## If this doesn't work, try to convert the ui to a single UIOutput and observe that
                        print("blanket clicked")
                        next_image()
             })

#              print("loading first image with next_image()")
#              next_image()
#              print("loaded!")
            print("Buffering image")
            buffer_new_image(cnt)
            print("Image buffered!")

            print("lading views")
            load_views()
            next_image()
            print("views loaded")


  #Import Links

  ######### POSTGRES POINTER ###########
#  saves <- read.csv("C:/Users/James/Documents/R Projects/ImageRecognition/Image Links/SaveRuns.csv", stringsAsFactors = FALSE)


  ## BRING IN IMAGES
  ## CREATE IMAGE OBJECT
  ## WRITE IMAGE TO TEMP FILE
  ## LOAD BOTH VIEWS WITH SAME IMAGE
  ##    don't need to load the image in each observe, just use univseral obj
  ##
  ##WHAT IS THE DEAL WITH OBSERVING BOTH L AND R BUTTONS?
  ##    Is it that that's the only way to ensure a touch is registered?
  ##    I think that's just an old method. Might be able to do an onClick() and point
  ##    it to the blanket object


    ## TODO:
              ## Function to generate both views based on the global image var
              ## Function to either pull in and/or update existing dimensions
              ##    This needs to be looked into

  #saves$FIT <- NA
  #write.csv(saves, "C:/Users/James/Documents/R Projects/ImageRecognition/Image Links/SaveRuns.csv", row.names = FALSE)
  ## Generate a specific random order:
  # set.seed([Put a number here])
  # corp <- sample(as.character(saves$URLs))
  ##
  #Generate Random order
 # corp <- sample(as.character(saves$URLS))
  ##
  ## Starting a counter
#  ns <- 1

  ## Loading images
#  im <- image_read(as.character(corp[ns]))
  #get original size info
#  info <- image_info(im)
  #get the width
#  tw <- info$width
  #get the height
#  th <- info$height
#  wh <- reactive({input$dimension[2]})
#  ww <- reactive({input$dimension[1]})

#  output$RView <- renderUI({
#    fixedPanel({
#      te <- im %>%
#        image_write(tempfile(fileext = 'jpg'), format = 'jpg')
#      actionButton("nextR", label = "THIS IS THE RIGHT ACTION BUTTON")
#      tags$button(
#        id = "nextR",
#        class = "btn action-button",
#        tags$img(src = "",
#                 width = ((ww()/2)-40), height = (wh()-100))
#      )
#      tags$style(HTML("
#                      .btn {
#                      display:block;
#                      height: 100px;
#                      width: 50px;
#                      }
#                      
#                      "))
#           renderImage({
#             
#             list(src = te, width = ((ww()/2)-40), height = (wh()-100), contentType = "image/jpeg")
#         })},
#         right = 25,
#         top = 50,
#         bottom = 0
#           )
#
#})
#  output$LView <- renderUI({
#    fixedPanel({
#      te <- im %>%
#        image_write(tempfile(fileext = 'jpg'), format = 'jpg')
#      actionButton("nextL", label = "")
#      tags$button(
#        id = "nextL",
#        class = "btn action-button",
#        tags$img(src = "",
#                 width = ((ww()/2)-40), height = (wh()-100))
#      )
#      tags$style(HTML("
#                      .btn {
#                      display:block;
#                      height: 100%;
#                      width: 100%;
#                      }
#                      
#                      "))
#      renderImage({
#        
#        list(src = te, width = ((ww()/2)-40), height = (wh()-100), contentType = "image/jpeg")
#      })},
#      left = 25,
#         top = 50,
#         bottom = 0
#           )
#    
#  })


  #output$nsread <- renderText(ns)
  
#  observeEvent(input$nextL, {
#    ns <<- ns+1
#    im <<- image_read(as.character(corp[ns]))
#    output$LView <<- renderUI({
#      fixedPanel({
#        te <- im %>%
#          image_write(tempfile(fileext = 'jpg'), format = 'jpg')
#        actionButton("nextL", label = "")
#        tags$button(
#          id = "nextL",
#          class = "btn action-button",
#          tags$img(src = "",
#                   width = ((ww()/2)-40), height = (wh()-100))
#        )
#        renderImage({
#          
#          list(src = te, width = ((ww()/2)-40), height = (wh()-100), contentType = "image/jpeg")
#        })},
#        left = 25,
#        top = 50,
#        bottom = 0
#      )
#      
#    })
#  })
#  
#  observeEvent(input$nextR, {
#    ns <<- ns+1
#    im <<- image_read(as.character(corp[ns]))
#    
#    output$RView <<- renderUI({
#      fixedPanel({
#        te <- im %>%
#          image_write(tempfile(fileext = 'jpg'), format = 'jpg')
#        actionButton("nextR", label = "")
#        tags$button(
#          id = "nextR",
#          class = "btn action-button",
#          tags$img(src = "",
#                   width = ((ww()/2)-40), height = (wh()-100))
#        )
#        renderImage({
#          
#          list(src = te, width = ((ww()/2)-40), height = (wh()-100), contentType = "image/jpeg")
#        })},
#        left = 25,
#        top = 50,
#        bottom = 0
#      )
#      
#    })
#  })
#  

              ##############################  TIMER  ###############################

#              autoad <- reactiveTimer(intervalMs =  as.numeric(Sys.getenv('VIEWER_INTERVAL')))

              ######################################################################

#              observe({
#                autoad()
#                if (isolate(auto_switch) == TRUE){
#                  next_image()
#                }
#              })



  ##############################  TIMER  ###############################
   
#  autoad <- reactiveTimer(intervalMs =  5000)
  
  ######################################################################
#   observe({autoad()
#     ns<<- ns+1
#     im <<- image_read(as.character(corp[ns]))
#     output$LView <- renderUI({
#       fixedPanel({
#         te <- im %>%
#           image_write(tempfile(fileext = 'jpg'), format = 'jpg')
#         actionButton("nextL", label = "")
#         tags$button(
#           id = "nextL",
#           class = "btn action-button",
#           tags$img(src = "",
#                    width = ((ww()/2)-40), height = (wh()-100))
#         )
#         tags$style(HTML("
#                      .btn {
#                      display:block;
#                      height: 100%;
#                      width: 100%;
#                      }
#                      
#                      "))
#         renderImage({
#           
#           list(src = te, width = ((ww()/2)-40), height = (wh()-100), contentType = "image/jpeg")
#         })},
#         left = 25,
#         top = 50,
#         bottom = 0
#       )})
#       output$RView <- renderUI({
 ##        fixedPanel({
#           te <- im %>%
#             image_write(tempfile(fileext = 'jpg'), format = 'jpg')
#           actionButton("nextR", label = "")
#           tags$button(
#             id = "nextR",
#             class = "btn action-button",
#             tags$img(src = "",
#                      width = ((ww()/2)-40), height = (wh()-100))
#           )
#           tags$style(HTML("
#                      .btn {
#                      display:block;
#                      height: 100%;
#                      width: 100%;
#                      }
#                      
#                      "))
#           renderImage({
#             
#             list(src = te, width = ((ww()/2)-40), height = (wh()-100), contentType = "image/jpeg")
#           })},
#           right = 25,
#           top = 50,
#           bottom = 0
#         )
#       
#     })
#     })   
 
})
