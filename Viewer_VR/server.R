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
#load_image <- function(){
#  output$image_output <<- renderImage({
#    list(src = "tmp.jpg",
#         contentType = "image/jpeg")},
#                                      deleteFile = TRUE)
  ## For whatever reason, if you don't have a print() command or something at the end
  ## R shiny won't know what to do and throw a session$fileUrl error
  ## That's why unhide wasn't working, it was getting to this function and failing
#  print(img_link)
#}

pull_data()
HIDE_FLAG <<- TRUE
cnt <<- get_cnt_safe(viewables,removes)

# options(shiny.port = 3839)
# 
# options(shiny.host = "192.168.0.103")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
                  wh <<- reactive({input$dimension[2]})
                  ww <<- reactive({input$dimension[1]})

                  output$LView <- renderUI({
                    fixedPanel({
                      renderImage({
                        list(src = "tmp.jpg",
                             width = ((ww()/2)-40),
                             height = (wh()-100),
                             contentType = "image/jpeg"
                        )
                    })},
                               left = 25,
                               top = 50,
                               bottom = 0
                    )
#                    imageOutput("LView",
#                                height = "100%",
#                                width = "50%"
#                    )
                  })

                  output$RView <- renderUI({
                    fixedPanel({
                      renderImage({
                        list(src = "tmp.jpg",
                             width = ((ww()/2)-40),
                             height = (wh()-100),
                             contentType = "image/jpeg"
                        )
                    })},
                               left = 25,
                               top = 50,
                               bottom = 0
                    )
#                    imageOutput("RView",
#                                height = "100%",
#                                width = "50%"
#                                )

                  })

#              get_screen_dims <- function(){
#                  wh <<- reactive({input$dimension[2]})
#                  ww <<- reactive({input$dimension[1]})
#                  wh <<- isolate(input$dimension[2])
#                  ww <<- isolate(input$dimension[1])
#              }
#              get_screen_dims()
#              print(wh())
#              print(ww())

#              general_image_output <- function(){
#                 return(renderImage({
#                   list(src = "tmp.jpg",
#                        width = ((ww()/2)-40),
#                        height = (wh()-100),
#                        contentType = "image/jpeg")
#                 }, deleteFile = TRUE))
#              }


              load_views <- function(){
                  output$LView <- renderUI({
                    fixedPanel({
                      actionButton("nextL", label = "")
                      tags$button(
                                  id = "nextL",
                                  class = "btn action-button",
                                  tags$img(src = "",
                                           width = ((ww()/2)-40), height = (wh()-100))
                      )
                      tags$style(HTML("
                                      .btn {
                                        display:block;
                                        height: 100%;
                                        width: 100%;
                                      }
                                      "))
                      renderImage({
                        list(src = "tmpL.jpg",
                             width = ((ww()/2)-40),
                             height = (wh()-100),
                             contentType = "image/jpeg"
                        )
                    }, deleteFile = TRUE)},
                               left = 25,
                               top = 50,
                               bottom = 0
                    )
#                    imageOutput("LView",
#                                height = "100%",
#                                width = "50%"
#                    )
                  })

                  output$RView <- renderUI({
                    fixedPanel({
                      actionButton("nextR", label = "")
                      tags$button(
                                  id = "nextR",
                                  class = "btn action-button",
                                  tags$img(src = "",
                                           width = ((ww()/2)-40), height = (wh()-100))
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
                             width = ((ww()/2)-40),
                             height = (wh()-100),
                             contentType = "image/jpeg"
                        )
                    }, deleteFile = TRUE)},
                               right = 25,
                               top = 50,
                               bottom = 0
                    )
#                    imageOutput("RView",
#                                height = "100%",
#                                width = "50%"
#                                )

                  })
##                output$RView <- general_image_output()
#                output$LView <- general_image_output()
#                output$RView <<- renderImage({
#                    list(src = "tmp.jpg",
#                         width = "50%",
#                         height = (wh()-100),
#                         contentType = "image/jpeg")
#                  }, deleteFile = TRUE)
#  
#                  output$LView <<- renderImage({
#                      list(src = "tmp.jpg",
#                           width = "50%",
#                           height = (wh()-100),
#                           contentType = "image/jpeg")
#                    }, deleteFile = TRUE)
                    #               print(wh())
 #               print(ww())





                #output$image_output <<- renderImage({
                #  list(src = "tmp.jpg",
                #       contentType = "image/jpeg")},
                #                                    deleteFile = TRUE)
                ## For whatever reason, if you don't have a print() command or something at the end
                ## R shiny won't know what to do and throw a session$fileUrl error
                ## That's why unhide wasn't working, it was getting to this function and failing
 #               print(paste("cnt is:", cnt))
  #              print(paste("viewables for that cnt is:", viewables[cnt,1]))
#                print(paste("img_link object:",img_link))
 #               print("==========")
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

                while(im_buff_flag == FALSE) {
                  cnt <<- get_cnt_safe(viewables,removes)
                  im_buff_flag <<- ensure_image_load(cnt)
                }
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

                       if (file.exists("tmp.jpg")){
                         file.remove("tmp.jpg")
                       }

                       print("Session disconnected")
                       print("=====================")
                })

              onclick("blanket",{ ## If this doesn't work, try to convert the ui to a single UIOutput and observe that
                        print("blanket clicked")
                        next_image()
             })
            print("Buffering image")
            buffer_new_image()
            print("Image buffered!")

            print("lading views")
            load_views()
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
