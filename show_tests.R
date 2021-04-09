source("utils.R")

test_names <- list("HD0" = "Musikalische Hörtests",
                   "BAT" = c("name" = "Beatwahrnehmungs-Test", 
                             "git_repo" = "https://github.com/pmcharrison/cabat", 
                             "ref_paper" ="https://www.nature.com/articles/s41598-018-30318-8"),
                   "MDT" = c("name" = "Melodieunterscheidungs-Test",
                             "git_repo" = "https://github.com/pmcharrison/mdt",
                             "ref_paper" = "https://www.nature.com/articles/s41598-017-03586-z"),
                   "MPT" = c("name" = "Verstimmungswahrnehmungs-Test",
                             "git_repo" = "https://github.com/pmcharrison/mpt",
                             "ref_paper" = "https://link.springer.com/article/10.3758%2Fs13428-019-01225-1"), 
                   "RAT" = c("name" = "Rhythmusfähigkeits-Test",
                             "git_repo" = "https://github.com/klausfrieler/RAT",
                             "ref_paper" = ""),
                   "PIT" = c("name" =" Tonvorstellungs-Test",
                             "git_repo" = "https://github.com/pmcharrison/piat",
                             "ref_paper" = "https://link.springer.com/article/10.1007/s00426-020-01322-3"),
                   "EDT" = c("name" = "Emotionenunterscheidungs-Test",
                             "git_repo" = "https://github.com/klausfrieler/EDT",
                             "ref_paper" = "https://www.frontiersin.org/articles/10.3389/fpsyg.2019.01955/full"),
                   
                   "HD1"  = "Nicht-musikalische Leistungstests",
                   #"MIQ" = "Cognitive Puzzles Test",
                   "JAJ" = c("name" = "Jack & Jill Arbeitsgedächtnis-Test",
                             "git_repo" = "https://github.com/klausfrieler/JAJ",
                             "ref_paper" = ""),
                   "BDS" = c("name" = "Backward Digit Span Arbeitsgedächtnis-Test",
                             "git_repo" = "https://github.com/klausfrieler/BDS",
                             "ref_paper" = ""),
                   "HD2" = "Selbstauskunftsfragebögen zu musikalischen und anderen Aktivitäten",
                   "GMS" = c("name" = "Goldsmiths Musical Sophistication Index",
                             "git_repo" = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0089642"),
                   "CCM" = c("name" = "Fragebogen zur aktuellen musikalische Aktivititäten ",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "MHE" = c("name" = "Fragebogen zum häuslischen musikalischen Umfeld ",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "DAC" = c("name" = "Fragebogen zu Theateraktivitäten",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "PAC" = c("name" = "Fragebogen zu sportlichen Aktivitäten",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "HD3" = "Selbstauskunftsfragebogen zu psychosozialen Faktoren",
                   "DEG" = c("name" = "Basisdemographische Angaben",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "SES" = c("name" = "Inventar zum sozio-ökonomischen Status",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   #"SCA" = "Academic Self-Concept Questionnaire",
                   #"SCS" = "Social Self-Concept Questionnaire",
                   "TOM" = c("name" = "Fragebogen zur Theorie der  Musikalität",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "TOI" = c("name" = "Fragebogen zur Theorie der Intelligenz",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "SDQ" = c("name" = "Fragebogen zu Stärken und Schwächen",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "SEM" = c("name" = "Fragebogen zu Schulengagement",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "HOP" = c("name" = "Hoffnungsskala für Kinder",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "GRT" = c("name" = "Durchhaltevermögensskala für Kinder",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "MUS" = c("name" = "Kurzer Test zu musikalischen Präferenzen",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "SMP" = c("name" = "Kurzer Fragebogen zu musikalischen Präferenzen",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "TPI" = c("name" = "10-Item Persönlichkeitsinventar",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""),
                   "HD4" = "Verschiedene",
                   "HALT" = c("name" = "Kopfhörer und Lautsprecher Test (HALT)",
                             "git_repo"  = "https://github.com/klausfrieler/HALT",
                             "ref_paper" = ""),
                   
                   "NA" = "")

read_test_info <- function(fname = "data/dots_test_def.xlsx"){
  test_info <- readxl::read_xlsx(fname) %>% 
    mutate(name =  toupper(name)) %>%  
    filter(type != "subscale", !(name %in% c("LIE", "SSS"))) %>% 
    distinct(name, description_de, name_full_de, .keep_all = F) %>% #
    filter(!is.na(description_de))
  assign("test_info", test_info, globalenv())
}

get_info <-function(test_name){
  tmp <- test_info %>% filter(name == test_name)
  if(nrow(tmp) == 0){
    return("")
  }
  return(tmp %>% pull(description_de))
}
get_test_name <- function(test_id){
  tmp <- test_names[[test_id]]
  if("names" %in% names(tmp)){
    return(tmp[["name"]])
  }
}

get_test_prop <- function(test_id, prop){
  tmp <- test_names[[test_id]]
  if(prop %in% names(tmp)){
    return(tmp[[prop]])
  }
  tmp
}

static_selection_page <-function(){
  if(local_debug){
    base_url <- "http://127.0.0.1:3973/dots_demo"
    dots_url <- "http://127.0.0.1:3973"
    
  }
  else{
    base_url <- "http://testing.musikpsychologie.de/dots_demo/"
    dots_url <- "http://testing.musikpsychologie.de"
    
  }
  base_url <- "http://testing.musikpsychologie.de/dots_demo/"
  dots_url <- "http://testing.musikpsychologie.de"
  body_text <- 
    map(names(test_names), function(tn){
      #browser()
      if(substr(tn, 1, 2) == "HD"){
        shiny::p(
          shiny::tags$b(test_names[tn], style = "text-align:left;"), 
          style = "text-align:left; margin-left:0%;margin-top:30px;"
        )
      }
      else{
        href <- sprintf("%s?test=%s", base_url, tn)
        if("name" %in% names(test_names[[tn]])){
          git_repo <- get_test_prop(tn, "git_repo")
          ref_paper <- get_test_prop(tn, "ref_paper")
          if(tn == "HALT"){
            href <- sprintf("%s/%s/?language=de", dots_url, tn)
          }
          #browser()
          info <- shiny::span(get_info(tn), style = "font-size:small;text-align:justify;")
          shiny::p(
            shiny::a(href = href, target = "_blank", get_test_prop(tn, "name")), 
            
            shiny::span(
              if(nchar(git_repo) > 0) shiny::a(href = git_repo, 
                                               target = "_blank", 
                                               "[GitHub]",
                                               style = "color:#f47920;text-decoration:none"), 
              if(nchar(ref_paper) > 0) shiny::a(href = ref_paper, 
                                                target = "_blank", 
                                                "[Quelle]", 
                                                style = "color:#f47920;text-decoration:none"),
              style = "font-size:10pt;margin-left:5pt"
            ),
            #get_info(tn),
            info,
            style = "text-align:justify; margin-left:0%;width:100%;max-width:700px;")
          
        }
        else{
          shiny::p(
            shiny::a(href = href, target = "_blank", get_test_prop(tn, "name")), 
            style = "text-align:left; margin-left:20%;")
          
        }
      }
    })
  body_text
}