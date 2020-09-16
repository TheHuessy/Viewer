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

#cnt <- sample(x = 1:tot, size = 1, replace = TRUE)

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

              onStop(function(){
                       if (nrow(removes) != 0){
                         remove_duds(removes)
                       }
                       dbDisconnect(sql_con)
                       print("Session disconnected")
})
              img_link <<- get_link(cnt)

              img <- image_read(img_link) %>%
                image_write("tmp.jpg")

              ########################
              # Initial Image Output #  
              ########################

              output$image_output <- renderImage({
                list(src = "tmp.jpg",
                     contentType = "image/jpeg")
              },deleteFile = TRUE
              )

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

              autoad <- reactiveTimer(intervalMs =  7000)

              ######################################################################

              observe({
                autoad()
                if (isolate(auto_switch) == TRUE){
                  #                  cnt <<- sample(x = 1:tot, size = 1, replace = TRUE)
                  cnt <<- get_cnt_safe(viewables,removes)

                  img_link <<- get_link(cnt)
                  img <<- image_read(img_link) %>%
                    image_write("tmp.jpg")

                  output$image_output <<- renderImage({
                    list(src = "tmp.jpg",
                         contentType = "image/jpeg")
                  }, deleteFile = TRUE
                  )
                }
              })

              ##### TAP ADVANCEMENT #####

              ## IF YOU'RE HAVING ISSUES WITH IT NOT ADVANCING, TRY CLICKING ON THE UPPER LEFT CORNER OF THE IMAGE
              ## It looks like it only counts clicks on the actual space the image takes up and not the space that the css file fills to meet the width requirement

              observeEvent(input$im_click, {
                             #                             cnt <<- sample(x = 1:tot, size = 1, replace = TRUE)
                             cnt <<- get_cnt_safe(viewables,removes)
                             img_link <<- get_link(cnt)
                             img <<- image_read(img_link) %>%
                               image_write("tmp.jpg")

                             output$image_output <<- renderImage({
                               list(src = "tmp.jpg",
                                    contentType = "image/jpeg")
                             }, deleteFile = TRUE)
              })
    })

