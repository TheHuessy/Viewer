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
                       dbname="strobot"
  )

  query = "SELECT DISTINCT end_link, 'culling_external' as table_name FROM culling_direct WHERE keep = 1
  UNION
  SELECT end_link, 'pulls' as table_name FROM pulls WHERE link_type = 'Direct'"



  viewables <<- dbGetQuery(sql_con,
                          statement=query)
  dbDisconnect(sql_con)

  tot <<- nrow(viewables)

}
removes <<- data.frame(end_link=c(),table_name=c())

remove_duds <- function(removes_df){
  sql_driver <- dbDriver("PostgreSQL")

  sql_con <- dbConnect(sql_driver,
                       host=creds$pg_host,
                       user=creds$pg_user,
                       password=creds$pg_pw,
                       dbname="strobot"
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
  if (raw_width >= raw_height){
    conv <- calc_conv(((screen_width/2)-10), raw_width)
  } else {
    conv <- calc_conv((screen_height-20), raw_height)
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

                  output$LView <<- renderUI({
                    output_dims <- get_output_dims(ww(), wh(), raw_info$width, raw_info$height)
                    side_margin = (((ww()/2)-output_dims$width)/2)
                    top_margin = ((wh()-output_dims$height)/2)

                    fixedPanel({
                      actionButton("nextL", label = "")
                      tags$button(
                                  id = "nextL",
                                  class = "btn action-button",
                                  tags$img(src = "",
                                           width = ((ww()/2)-10), height = (wh()-20))
                      )
                      tags$style(HTML("
                                      .btn {
                                        display:block;
                                        margin-left: auto;
                                        margin-right: auto;
                                        height: 100%;
                                        width: 100%;
                                      }
                                      "))
                      renderImage({
                        list(src = "tmpL.jpg",
                             width = output_dims$width,
                             height = output_dims$height,
                             contentType = "image/jpeg"
                        )
                    }, deleteFile = TRUE)},
                               left = side_margin,
                               top = top_margin,
                               bottom = 0
                    )
                  })

                  output$RView <<- renderUI({
                    output_dims <- get_output_dims(ww(), wh(), raw_info$width, raw_info$height)
                    side_margin = (((ww()/2)-output_dims$width)/2)
                    top_margin = ((wh()-output_dims$height)/2)

                    fixedPanel({
                      actionButton("nextR", label = "")
                      tags$button(
                                  id = "nextR",
                                  class = "btn action-button",
                                  tags$img(src = "",
                                           width = ((ww()/2)-10), height = (wh()-20))
                      )
                      tags$style(HTML("
                                      .btn {
                                        display:block;
                                        margin-left: auto;
                                        margin-right: auto;
                                        height: 100%;
                                        width: 100%;
                                      }
                                      "))
                      renderImage({
                        list(src = "tmpR.jpg",
                             width = output_dims$width,
                             height = output_dims$height,
                             contentType = "image/jpeg"
                        )
                    }, deleteFile = TRUE)},
                                      ## The next two lines force the images to be equidistant from the top and the respective side the image is on
                                      ## Effectively ensures that things are centered
                               right = side_margin,
                               top = top_margin,
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

              onclick("blanket",{
                        next_image()
             })

            buffer_new_image(cnt)

            load_views()

              ##############################  TIMER  ###############################

              autoad <- reactiveTimer(intervalMs =  as.numeric(Sys.getenv('VIEWER_INTERVAL')))

              ######################################################################

#              observe({
#                autoad()
#                if (isolate(auto_switch) == TRUE){
#                  next_image()
#                }
#              })

})
