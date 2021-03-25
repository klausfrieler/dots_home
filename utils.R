library(tidyverse)
library(GMSIData)
library(psychTestR)


options(shiny.error = browser)
local_debug <- !grepl("shiny-server", getwd())

#************************** HELPER ************************
messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

reload <- function(package){
  devtools::reload(pkgload::inst(package))
}

get_test_id_from_url <- function(state, session){
  #browser()
  query <- shiny::parseQueryString(session$clientData$url_search)
  #print(query)
  #params <- psychTestR::get_url_params(state)
  #print(params)
  test <- query$test 
  if(is.null(test)) test <- ""
  test
}
check_test_sequence <- function(seq){
  if(length(seq) == 1){
    seq <- strsplit(seq, ",") %>% unlist() %>% trimws()
  }
  seq <- gsub("MDI", "MDT", seq, fixed = T)
  #DEG, SDQ, BAT, SCS, EDT, MHE, TPI, GMS, TOI, MIQ, MDT, DAC, SEM, MPT, SCA
  mandatory <- c("GMS", "BAT", "MDT", "MPT", "DEG")
  listening_tests <- c("EDT", "JAJ", "MIQ", "RAT", "BAT", "MDT", "MPT")
  questionnaires <- c("CCM", "DAC", "MHE", "PAC", "SCA", "SCS", "SDQ", "SEM", "TOI", "TOM", "TPI")
  check_mandatory <- intersect(seq, mandatory)
  okay <- TRUE
  if(length(check_mandatory) != length(mandatory)){
    messagef("Mandatory tests misssing: %s", paste(setdiff(mandatory, seq), collapse = ", "))
    okay <- FALSE
  }
  seq <- setdiff(seq, mandatory)
  num_listening_tests <- length(intersect(seq, listening_tests))
  if(num_listening_tests != 2){
    messagef("Too few listening tests: %d ", num_listening_tests)  
    okay <- FALSE
  }
  num_quest <- length(intersect(seq, questionnaires))
  if(num_quest != 9){
    messagef("Too few questionnaris: %d ", num_quest)  
    okay <- FALSE
  }
  if(okay) messagef("All good.")
}

remove_first_character <- function(p_id, which = c("?", "!", "#"), return_all = T){
  first <- substr(p_id, 1, 1)
  if(first == "!" || first == "?" || first == "#"){
    p_id <- substr(p_id, 2, nchar(p_id))
  } 
  else{
    first <- ""
  }
  if(return_all){
    return(list(p_id = p_id, first_character = first))
  }
  else{
    return(p_id)
  }
}

validate_id <- Vectorize(
  function(p_id){
    if(is.na(p_id)) return(FALSE)
    l <- nchar(p_id)
    valid <- FALSE
    if( l == 10 || l == 14){
      digits <- as.numeric(strsplit(p_id, split = "")[[1]])
      first_digits <- digits[1:(l-2)]
      checksum <- as.numeric(paste(digits[(l-1):l], collapse = ""))
      valid <- sum(first_digits) == checksum
      if(is.na(valid)) {
        valid <- FALSE
      }
    }
    valid
  })

validate_p_id <- function(answer, ...){
  #browser()
  p_id <- answer
  valid <- FALSE
  elems <- remove_first_character(p_id)
  p_id <- elems[["p_id"]]

  l <- nchar(p_id)
  if( l == 10 || l == 14){
    digits <- as.numeric(strsplit(p_id, split = "")[[1]])
    first_digits <- digits[1:(l-2)]
    checksum <- as.numeric(paste(digits[(l-1):l], collapse = ""))
    valid <- sum(first_digits) == checksum
    if(is.na(valid)) {
      valid <- FALSE
    }
  }
  if(valid && elems[["first_character"]] == "!"){
    #browser()
    messagef("Try deleting %s", file.path("output/sessions", p_id))
    unlink(file.path("output/sessions", p_id), recursive = T)
    Sys.sleep(.01)
    messagef("Try deleting %s", file.path("output/sessions", sprintf("%s%s", elems[["first_character"]], p_id)))
    unlink(file.path("output/sessions", sprintf("%s%s", elems[["first_character"]], p_id)), recursive = T)
    Sys.sleep(.01)
  }
  valid
}


register_participant <- function(study_id){
  if(is.null(study_id)){
    stop("Study id must be not NULL")
  }
  psychTestR::code_block(function(state, answer, ...) {
    #browser()
    p_id <- psychTestR::get_session_info(state, complete = F)$p_id %>% 
      remove_first_character(return_all = F)
    messagef("Register participant %s for study id %s", p_id, study_id)
    if(!local_debug){
      db <- GMSIData::db_connect()
      GMSID_session_id <- GMSIData::dbNewParticipant(db,
                                                     study_id = study_id,
                                                     participant_id = p_id)
      GMSIData::db_disconnect(db)
    }
    else{
      GMSID_session_id <- "DUMMY_SESSION"
    }
    set_local("GMSID_session_id", GMSID_session_id, state)
  })
}
feedback_graph_normal_curve <- function(test_name, perc_correct, x_min = 40, x_max = 160, x_mean = 100, x_sd = 15, language = "DE") {
  q <-
    ggplot2::ggplot(data.frame(x = c(x_min, x_max)), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = stats::dnorm, args = list(mean = x_mean, sd = x_sd)) +
    ggplot2::stat_function(fun = stats::dnorm, args=list(mean = x_mean, sd = x_sd),
                           xlim = c(x_min, (x_max - x_min) * perc_correct + x_min),
                           fill = "lightblue4",
                           geom = "area")
  q <- q + ggplot2::theme_bw()
  #q <- q + scale_y_continuous(labels = scales::percent, name="Frequency (%)")
  #q <- q + ggplot2::scale_y_continuous(labels = NULL)
  x_axis_lab <- sprintf(" %s Values", test_name)
  title <- "Your score"
  if(language == "DE"){
    title <- "Dein Testergebnis"
    x_axis_lab <- sprintf(" %s-Werte", test_name)
  }
  fake_IQ <- (x_max - x_min) * perc_correct + x_min  #(15/24 -> 100)
  main_title <- sprintf("%s: %.0f", title, round(fake_IQ, digits = 0))
  
  q <- q + ggplot2::labs(x = x_axis_lab, y = "")
  q <- q + ggplot2::ggtitle(main_title)
  plotly::ggplotly(q, width = 600, height = 450)
}

normal_curve_feedback <- function(test_label = "JAJ", test_name = "Test", dict = JAJ::JAJ_dict) {
  psychTestR::new_timeline(
    c(
      psychTestR::reactive_page(function(state, ...) {
        #browser()
        results <- psychTestR::get_results(state = state, complete = TRUE, add_session_info = FALSE)
        results <- attr(as.list(results)[[test_label]]$ability, "metadata")$results
        perc_correct <- (results$ability_WL[nrow(results)] + 2)/4
        sum_score <- sum(results$score)
        num_question <- nrow(results)
        #perc_correct <- sum_score/num_question
        printf("Test: %s, Sum scores: %d, total items: %d perc_correct: %.2f", test_label, 
               sum_score, num_question, perc_correct)
        text_finish <- psychTestR::i18n("SUM_FEEDBACK",
                                        html = TRUE,
                                        sub = list(num_question = num_question, num_correct = sum_score)) %>% 
          gsub("Ballfolgen", "Fragen", x = .)
        norm_plot <- feedback_graph_normal_curve(test_name, perc_correct)
        psychTestR::page(
          ui = shiny::div(
            shiny::p(norm_plot),
            shiny::p(text_finish),
            shiny::p(psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
          )
        )
      }
      )),
    dict = dict
  )
  
}

BPB_feedback <- function(dict = psyquest::psyquest_dict) {
  psychTestR::new_timeline(
    c(
      psychTestR::reactive_page(function(state, ...) {
        #browser()
        results <- psychTestR::get_results(state = state, complete = TRUE, add_session_info = FALSE)
        GMS_scale_plot <- GMS_scale_plot(as_tibble(results))
        results <- as_tibble(results) %>% select(BDT.ability, BAT.ability) %>% select(!contains("q."))
        print(results)
        #perc_correct <- sum_score/num_question
        #printf("Test: %s, Sum scores: %d, total items: %d perc_correct: %.2f", test_label, 
        #       sum_score, num_question, perc_correct)
        text_finish <- shiny::h3("Your results")  
        BDT_norm_plot <- feedback_graph_normal_curve("Beat Drop Alignment Test", 
                                                     (results$BDT.ability + 4)/8,  language = "EN")
        BAT_norm_plot <- feedback_graph_normal_curve("Beat Perception Test", 
                                                     (results$BAT.ability + 4)/8, language = "EN")
        psychTestR::page(
          ui = shiny::div(
            shiny::h3("Your results"),
            shiny::h4("Beat Drop Alignment Test"),
            shiny::p(BDT_norm_plot),
            shiny::h4("Beat Perception Test", style ="margin-top:50px"),
            shiny::p(BAT_norm_plot),
            shiny::h4("Goldsmith Musical Sophistication Index", style ="margin-top:50px"),
            shiny::p(GMS_scale_plot),
            
            shiny::p(psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
          )
        )
      }
      )),
    dict = dict
  )
  
}
update_results <- function(finished = F, documentation = "umbrella", study_id = NULL, first_entry = F) {
  #messagef("Upload results added (first_entry = %s)", first_entry)
  if(is.null(study_id)){
    stop("Study id must be not NULL")
  }
  psychTestR::code_block(function(state, answer, ...) {
    #browser()
    p_id <- psychTestR::get_session_info(state, complete = F)$p_id %>% remove_first_character(return_all = F)
    value <- psychTestR::get_results(state, complete = TRUE, add_session_info = finished)
    #print(as_tibble(value))
    if(is.na(study_id) || study_id == "NA"){
      messagef("Debug return from upload")
      return()
    }
    #if("results.main_test" %in% names(as_tibble(value))){
    #  browser()
    #}
    GMSID_session_id <- get_local ("GMSID_session_id", state)
    if(!local_debug){
      db <- GMSIData::db_connect()
      if(first_entry){
        messagef("Adding data for participant %s and study id %s (GMSID_session_id = %s, documentation = %s, n_results = %d)", p_id, study_id, GMSID_session_id, documentation, length(value))
        GMSIData::dbAddData(db = db,
                            study_id = study_id,
                            session_id = GMSID_session_id,
                            data = list(documentation = documentation,
                                        value = value),
                            label = sprintf("%s_results", documentation),
                            finished = finished)
      }
      else{
        messagef("Updating data for participant %s and study id %s (GMSID_session_id = %s, documentation = %s, n_results = %d)", p_id, study_id, GMSID_session_id, documentation, length(value))
        GMSIData::dbUpdateData(db = db,
                               study_id = study_id,
                               session_id = GMSID_session_id,
                               data = list(documentation = documentation,
                                      value = value),
                             finished = finished)
      }
      GMSIData::db_disconnect(db)
    }
    else{
      messagef("Debug updating GMSID_session_id = %s, documentation = %s, finished = %s, first_entry = %s",
               GMSID_session_id, 
               documentation, finished, 
               first_entry)
    }
  })
}

upload_results <- function(finished = T, documentation = "umbrella", study_id = NULL) {
  #messagef("Upload results added")
  psychTestR::code_block(function(state, answer, ...) {
    p_id <- psychTestR::get_session_info(state, complete = F)$p_id %>% remove_first_character(return_all = F)
    if(is.null(study_id)){
      stop("Study id must be not NULL")
    }
    messagef("Uploading data for participant %s and study id %s", p_id, study_id)
    value <- psychTestR::get_results(state, complete = TRUE, add_session_info = TRUE)
    #browser()
    print(as_tibble(value))
    
    if(is.na(study_id) || study_id == "NA"){
      messagef("Debug return from upload")
      return()
    }
    if(!local_debug){
      db <- GMSIData::db_connect()
      GMSID_session_id <- GMSIData::dbNewParticipant(db,
                                                     study_id = study_id,
                                                     participant_id = p_id)
      GMSIData::dbAddData(db = db,
                          study_id = study_id,
                          session_id = GMSID_session_id,
                          data = list(documentation = documentation,
                                      value = value),
                          label = sprintf("%s_results", documentation),
                          finished = finished)
      GMSIData::db_disconnect(db)
    }
  })
}
norm_plot <- function (fake_IQ, x_min = 50, x_max = 150, x_mean = 100, x_sd = 15) {

  q <- ggplot2::ggplot(data.frame(x = c(x_min, x_max)), ggplot2::aes(x))
  q <- q+ ggplot2::stat_function(fun = stats::dnorm, args = list(mean = x_mean, sd = x_sd)) 
  q <- q + ggplot2::stat_function(fun = stats::dnorm, args = list(mean = x_mean, sd = x_sd), 
                                  xlim = c(x_min, fake_IQ), 
                                  fill = "lightblue4", 
                                  geom = "area")
  q <- q + ggplot2::theme_bw()
  x_axis_lab <- sprintf(" %s %s", "Musical Sophistication", "Value")
  title <- "Your Score"
  #fake_IQ <- (x_max - x_min) * perc_correct + x_min
  main_title <- sprintf("%s: %.0f", title, round(fake_IQ, digits = 0))
  q <- q + ggplot2::labs(x = x_axis_lab, y = "")
  q <- q + ggplot2::ggtitle(main_title)
  plotly::ggplotly(q, width = 600, height = 450)
}

GMS_feedback_with_graph <- function (dict = psyquest::psyquest_dict) {
  psychTestR::new_timeline(psychTestR::reactive_page(function(state, ...) {
    results <- psychTestR::get_results(state = state, complete = TRUE,  add_session_info = FALSE) 
    results <- results %>% as_tibble(results) %>% select(!starts_with("GMS.q"))
    names(results) <- gsub("GMS.", "", names(results), fixed = T)
    general_score <- results$General * 18
    norm_plot <- norm_plot(general_score, x_min = 32, x_max = 126, x_sd = 10, x_mean = 82)
    psychTestR::page(ui = shiny::div( 
                                     shiny::p(norm_plot), 
                                     shiny::p(psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))))
  }), dict = dict)
}

GMS_scale_plot <- function(data, dict = psyquest::psyquest_dict){
  if(!("scale" %in% names(data))){
    #browser()
    data <- data %>% select(!contains(".q")) %>% 
      pivot_longer(cols = starts_with("GMS"), names_to = "scale", values_to ="scores")    
  }
  data <- data %>% filter(!(scale %in% c("GMS.Instrument", "GMS.Start Age", "GMS.Absolute Pitch")))
  data <- data %>% 
    mutate(scale = gsub("GMS.", "", scale, fixed = T), 
           is.general = (scale == "General"),
           scale = factor(scale, levels = c("Active Engagement", "Emotions", "Musical Training", "Perceptual Abilities", "Singing Abilities", "General")))
  q <- ggplot(data, aes(x = scale, y = scores, fill = is.general)) 
  q <- q + geom_bar(stat = "identity")
  q <- q + scale_fill_manual(values = c("lightblue4", "lightblue3"))
  q <- q + geom_text(aes(y = scores + .5, label = round(scores, 1)), hjust = 0)
  q <- q + theme_bw(base_size = 16)
  q <- q + theme(legend.position = "none")
  q <- q + scale_y_continuous(breaks = 1:7, limits = c(0, 7))
  q <- q + coord_flip()
  q <- q + labs(y = "GMSI Score", x = "")
  
  plotly::ggplotly(q, width = 600, height = 450)
}