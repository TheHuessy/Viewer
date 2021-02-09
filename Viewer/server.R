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

creds <<- read_yaml(Sys.getenv('CREDS_PATH'))

pull_data <- function(){

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

  viewables <<- dbGetQuery(sql_con,
                          statement=query)
  dbDisconnect(sql_con)

  tot <<- nrow(viewables)
}

#print("pre-pulldata")
pull_data()

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
    if (grepl("/p/", rm_link, fixed = TRUE)) {
      link_var <- "piece"
    } else {
      link_var <- "end_link"
    }

    delete_row_statement <- paste("DELETE FROM ",
                                  rm_table,
                                  " WHERE ",
                                  link_var," = '",
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



parse_csrf <- function(cookie_table){
  output <- cookie_table[which(cookie_table$name == "csrftoken"),7]
  return(output)
}

set_insta_session <- function(full_url){
  time_rn <- round(as.numeric(as.POSIXct(Sys.time())),0)

  response_data <- GET(full_url)
  csrf <- parse_csrf(response_data$cookies)
  login_link <- "https://www.instagram.com/accounts/login/"

  post_body <- list(
                    username = creds$un_insta,
                    enc_password = paste('#PWD_INSTAGRAM_BROWSER:0:{', time_rn, '}:', creds$pw_insta, sep = ""),
                    optIntoOneTap = 'false'
  )

  post_headers <- c(
                    'user-agent' = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36",
                    'x-requested-with' = "XMLHttpRequest",
                    referer = login_link,
                    'x-csrftoken' = csrf
  )

  auth_post <- POST(url = paste(login_link, "ajax/", sep = ""), body = post_body, add_headers(post_headers))

  if ("sessionid" %in% auth_post$cookies$name) {
    print("Reauthenticated!")
    page_retry <- content(GET(full_url, add_headers('x-csrftoken' =  parse_csrf(auth_post$cookies))))
    if (is.null(page_retry$graphql$shortcode_media$display_url) == FALSE){
      return(page_retry)
    } else {
      print("Reuathentication did not work")
      return(page_retry)
    }
  } else {
    print("Not able to authenticate instagram!")
    return(auth_post)
  }

}

reauthenticate <- function(full_url){
  page_data <- set_insta_session(full_url)
  if (is.null(page_data$graphql$shortcode_media$display_url) == TRUE){
    return("https://www.imgur.com/thispageisactuallydead")
  } else{
    return(page_data$graphql$shortcode_media$display_url)
  }
}

insta_fresh <- function(piece){

  full_url <- paste("https://www.instagram.com", piece, "?__a=1", sep = "")

  ## If response is bad [define in a little bit], run set_insta_sesson and then retry••

  page_data <- content(GET(full_url))

  if (is.null(page_data$graphql$shortcode_media$display_url) == TRUE){
    print("reauthenticate!")
    return(reauthenticate(full_url))
  } else {
    return(page_data$graphql$shortcode_media$display_url)
  }
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

#############
# FUNCTIONS #
#############
              initial_view <- function(){
                output$image_div_object <- renderUI({
                  actionButton(inputId = 'START',
                               label = 'START')
                })
              }


              load_image <- function(){
                output$image_output <<- renderImage({
                  list(src = "tmp.jpg",
                       contentType = "image/jpeg")},
                  deleteFile = TRUE)
                print(img_link)
              }

              buffer_new_image <- function(cnt){
                img_link <<- get_link(cnt)
                image_read(img_link) %>%
                image_write("tmp.jpg")
              }

              next_image <- function(){
                cnt <<- get_cnt_safe(viewables,removes)
                buffer_new_image(cnt)
                load_image()
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
              onclick("image_div_object",{
                        next_image()
})

              buffer_new_image(cnt)

              output$image_div_object <- renderUI({
                    imageOutput("image_output",
                                height = "100%"#,
                    )
              })


              ########################
              # Initial HIDDEN Output #  
              ########################
             initial_view()


             observeEvent(input$START, {
                            output$image_div_object <- renderUI({
                              imageOutput("image_output",
                                          height = "100%"#,
                              )
                            })

                            load_image()
              })

              #load_image()

              ##### OPTIONS MODAL #####
              observeEvent(input$options_button, {
                             showModal(
                                       modalDialog(
                                                   title = "Options",
                                                   "Options",
                                                   footer = tagList(switchInput(inputId = "auto_toggle_btn",
                                                                                onLabel = "Auto",
                                                                                value = auto_switch),
                                                                    actionButton("reset", "Reload Data"),
                                                                    actionButton("hide", "Hide")
                                                                    ),
                                                   easyClose = TRUE
                                       )
                             )
              })

              ##### RELOAD IMAGES FROM DB #####
              observeEvent(input$reset, {
                             pull_data()
                             next_image()
              })

              ##### HIDE DISPLAY #####

              observeEvent(input$hide, {
                             initial_view()
              })





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
                  next_image()
                }
              })

    })

