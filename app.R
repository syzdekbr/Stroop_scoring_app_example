## **** Stroop Color and Word Test Scoring Application ****
# This application will accept information from a scored Stroop Test and demographic information, will output scaled
# scores, provide dynamic interpretation- text-based and graphical, and provide record management- saving records,
# restoring past records, and performing batch score analysis. User password system with remote database access.

# Sources helper functions- 1. ui_builder- will iteratively read spreadsheet and build ui inputs 
# 2. Rmarkdown style functions for tables
source("shiny_helper_functions.R")

# Load packages -----------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyauthr)
library(dplyr)
library(shinyjs)
library(DT)
library(googlesheets4)
library(readxl)
library(shinysky)
library(eeptools)
library(stringr)
library(ggplot2)
library(tidyr)
library(googledrive)
library(shinyBS)
library(rhandsontable)
library(shinyWidgets)

# Load Data and options ----------------------------------------------------
## Options
{
options(stringsAsFactors=FALSE)
}
####*** In actual app will access google sheet database to retrieve customer information. For example, uses xlsx file to protect security
## Get googlesheets customer data
{
  # Hide following sources of database information 
  # googlesheet_url <- "google_sheet_url"
  #gs4_auth(email = "email_name", token = "token_name")
  #drive_auth(email = "email_name", token = "token_name")
  # Stop hide
  # Start xlsx substitute
  library(xlsx)
  googlesheet_url <- "stroop_scoring_app_customer_data.xlsx"
  google_df <- read.xlsx(googlesheet_url, sheetIndex = 1) 
  # Login dataframe from google sheet
  user_base <- google_df %>% 
    mutate(
      user = customer_email,
      password = customer_password,
      permissions = rep("standard", nrow(.)),
      name = paste(customer_first_name, customer_last_name)
    )
}
# Get UI Input data
{
  ## UI Demographics input fields- to sidebar
ui_demographics <- readxl::read_xlsx("read_data/stroop_ui_inputs_demographics.xlsx")
  ## UI Scoring input fields- to main page
ui_scoring <- readxl::read_xlsx("read_data/stroop_scoring_inputs.xlsx")
  ## Calculated predicted scores for color, word, color-word to calculate scaled scores
predicted_scores <- readxl::read_xlsx("read_data/predicted_stroop_scores_scale_age.xlsx") %>%
  mutate(education = dplyr::recode(education, "BA" = "Bachelor Degree", "Another Value" = "Other"))
  ## Color, Word, Color-Word T-score lookup tables to convert to T-scores
all_scores_t_scores <- readxl::read_xlsx("read_data/t_score_df_final.xlsx")
  ## Interference T-scores separated by age to convert interference raw to T
interference_t_scores <- readxl::read_xlsx("read_data/interference_t_scores_combined.xlsx")
  ## Scale interpretation
stroop_scale_interpretation <- readxl::read_xlsx("read_data/stroop_scale_interpretation.xlsx")

raw_score_names <- ui_scoring$label[grepl("Score", ui_scoring$label)] ## Get Raw score names for output
raw_score_names_interference <- c(raw_score_names, "Interference") # Add Interference as calculated score for some
}
# UI ----------------------------------------------------------------------
ui <- function(request){
  dashboardPage(
## Header and dropdown help menu
  {
    dashboardHeader(title = "Stroop Scoring App Demonstration",
                    # Adds logout button
                    tags$li(class = "dropdown", style = "padding: 8px;", 
                            shinyauthr::logoutUI(id = "logout",
                                                 label = "Log Out/Manage Password")),
                    # Dropdown menu of help/support options 1. Help doc, 2. Help video, 3. Webpage, 4. Email 
                    dropdownMenu(type = "tasks", headerText = "Support",
                                 messageItem(
                                   from = "Help Document",
                                   message = "Instructions for using app",
                                   icon = icon("book"),
                                   time = NULL,
                                   href = "https://rpubs.com/syzdekbr/stroop_app_help_doc"
                                 ),
                                 messageItem(
                                   from = "Help Video",
                                   message = "Instructions for using app",
                                   icon = icon("video"),
                                   time = NULL,
                                   href = "https://www.youtube.com/watch?v=zQIVi12KqoU"
                                 ),
                                 messageItem(
                                   from = "Stroop Webpage",
                                   message = "stoeltingco.com",
                                   icon = icon("question"),
                                   time = NULL,
                                   href = "https://stoeltingco.com/Psychological-Testing/Stroop-Color-and-Word-Test-Kit-for-Adults-and-Children~9967"
                                 ),
                                 messageItem(
                                   from = "Email",
                                   message = "Email for support",
                                   icon = icon("envelope"),
                                   time = NULL,
                                   href = "mailto:psychtests@stoeltingco.com"
                                 )
                    )
    )
    },

## Sidebar Content
  {
    # setup a sidebar menu to be rendered server-side
    dashboardSidebar(
      collapsed = TRUE, sidebarMenuOutput("sidebar")
    )},

## Body Content
  {
    dashboardBody(
    # UI Options
      shinyjs::useShinyjs(), # Call to use shinyjs
      # Add check to navigate from app
              tags$head(tags$script(HTML("
          // Enable navigation prompt
          window.onbeforeunload = function() {
              return 'Your changes will be lost!';
          };
      "))),
      ## Busy indicator
      shinysky::busyIndicator(text = "Processing ... ", wait = 1000),
      tags$head(
        tags$style(HTML(".shinysky-busy-indicator {z-index: 1000;}"))
      ),
      source("shinyBS_tooltips.R",local = TRUE)$value, # Adds bstooltips; "value" so "TRUE" doesn't show
      # put the shinyauthr login ui module here
      h5("Demonstration App Only! Not accurate scoring! Get full app at stoeltingco.com", style = "color: red"),
      shinyauthr::loginUI("login"),

      # Adds password help dropdown menu 1. Forgot password, 2. Reset password, 3. Change retrieve Q&A
      tags$div(id = "password_div",
               fluidRow(
                 column(12, align = "center",
                        tags$h5("Use username 'sample' and password 'sample' for demonstration app"),
                        tags$h4("Password Help"),
                        dropdownButton(tags$h5("Password Support"),
                                       tags$style(type = 'text/css', ".dropdown-menu {margin-left: 50%; margin-right: 50%;}"),
                                       actionButton("forgot_password", label = "Forgot password?"),
                                       br(),
                                       actionButton("reset_password", label = "Reset Password"),
                                       br(),
                                       actionButton("reset_password_retrieve", label = "Reset Password Retrieval Question"),
                                       circle = TRUE, status = "danger",
                                       icon = icon("gear"), width = "300px",
                                       tooltip = tooltipOptions(title = "Click for forgotten password or reset password or retrieval question")
                        )
                 )
               )
               
      ),
      # setup any tab pages you want after login here with uiOutputs
      tabItems(
        ### Tab 1. This tab contains inputs and overview of scaled scores
        tabItem("scoring",
                fluidRow(
                  box(
                    uiOutput("scoring_inputs_ui"), # All input fields to be entered for scoring
                    tableOutput("age_out") # Calculates age if valid, or prompts if not
                  ),
                  box(
                    uiOutput("scaled_score_table_ui"), # Scaled scores together in table
                    uiOutput("scaled_score_plot_ui") # Plot of all scaled scores
                  )
                )
        ),
        
        ### Tab 2. This tab contains scale level interpretation
        tabItem("interpretation",
                h2("Interpretation"),
                uiOutput("interpretation_rows")
        ),
        
        ### Tab 3. This tab contains data management and report formats
        tabItem("data_management",
                h2("Reports and Record Management"),
                # This row allows current record 1. Generate report, 2. Save data
                fluidRow(
                  h3("Current Session Report and Save"),
                  box(
                    h4("Report"),
                    uiOutput("report_selectors")
                  ),
                  box(
                    h4("Save Current Session"),
                    uiOutput("save_client")
                  )
                ),
                # This row has restore options 1. Folder of records, 2. Individual, 3. Multiple 
                fluidRow(
                  h3("Restore Previous Session(s)"),
                  radioButtons("upload_format", "Choose Upload Format", 
                               choices = c("Folder", "Individual", "Multiple Records One File"), inline = T),
                  uiOutput("load_client_ui")
                ),
                
        )
      )
    )
}
) # Final UI parens
}

# Server ------------------------------------------------------------------
    server <- function(input, output, session) {
# Password Setup ----------------------------------------------------------
      ## FOllowing sets up dataframe from collected spreadsheet with user information
        ### credentials()$user_auth will then be true when correct combo entered, use to validate
      credentials <- shinyauthr::loginServer(
        id = "login",
        data = user_base,
        user_col = user,
        pwd_col = password,
        log_out = reactive(logout_init())
      )
      
      # call the logout module with reactive trigger to hide/show
      logout_init <- shinyauthr::logoutServer(
        id = "logout",
        active = reactive(credentials()$user_auth)
      )
      
# Sidebar setup and trigger from password ---------------------------------
      # # this opens or closes the sidebar on login/logout
      observe({
        if(credentials()$user_auth) {
          shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
          shinyjs::hide("password_div") # password help hide/show only at login
        } else {
          shinyjs::addClass(selector = "body", class = "sidebar-collapse")
          shinyjs::show("password_div") # password help hide/show only at login
        }
      })
      
      # only when credentials()$user_auth is TRUE, render your desired sidebar menu
      output$sidebar <- renderMenu({
        req(credentials()$user_auth)
        sidebarMenu( 
          # Following are all tabs
          menuItem("Scoring Information", tabName = "scoring", icon = shiny::icon("clipboard-list")),
          menuItem("Interpretation", tabName = "interpretation", icon = shiny::icon("chart-bar")),
          menuItem("Reports and Records", tabName = "data_management", icon = shiny::icon("book-open")),
          lapply(1:nrow(ui_demographics), function(i) {
            uiOutput(ui_demographics$id[i])
          })
        )
      })
      
# Password Reset Help -----------------------------------------------------
      source("password_reset_functions.R")
# Forgot password retrieve question
      {
      ## Enter user name to get retrieve question
      observeEvent(input$forgot_password, {
        showModal(forgot_password_modal_func())
      })
      ## Show retrieve question with answer field
      observeEvent(input$forgot_password_user_name_submit,{
        showModal(
          forgot_password_retrieve_question_func(
            google_df = google_df,
            user_name_input = input$forgot_password_user_name_input,
            user_name_column = "customer_email", 
            selected_column = "customer_retrieve_password_question"
          )
        )
      })
      ## Displays password if correct retrieval answer input
      observeEvent(input$forgot_password_question_submit,{
        showModal(
          display_password_func(
            google_df = google_df,
            user_name_input = input$forgot_password_user_name_input,
            user_name_column = "customer_email",
            selected_column = "customer_password",
            variable_name = "customer_retrieve_password_answer",
            forgot_password_question_input = input$forgot_password_question_input
          )
        )
      })
      }
# Reset Password Modals
      {
        # Prompt for current user name and password
        observeEvent(input$reset_password, {
          showModal(reset_password_func())
        })
        # Display new password entry
        observeEvent(input$reset_password_submit, {
          showModal(
            reset_password_feedback_func(
              google_df = google_df,
              user_name_input = input$reset_password_user_name_input,
              user_name_column = "customer_email",
              selected_column = "customer_password",
              existing_password_input = input$reset_password_password_input
            )
          )
        })
        # New password feedback and update
        observeEvent(input$reset_password_commit, {
          if (trimws(input$reset_password_new_input) != ""){
            password_reset_commit_func(
              new_password_input = input$reset_password_new_input,
              user_name_input = input$reset_password_user_name_input,
              googlesheet_url = googlesheet_url
            )
            showModal(
              password_updated_func()
            )
          } else{
            showModal(
              unacceptable_information_func()
            )
          }
        })
      }
# Reset Password Retrieval Question
      {
        # Prompts for current user name and password
        observeEvent(input$reset_password_retrieve,{
          showModal(
            reset_password_retrieve_func()
          )
        })
        # Prompt for new retrieve question and answer
        observeEvent(input$reset_password_retrieve_submit,{
          showModal(
            reset_password_retrieve_feedback_func(
              google_df = google_df,
              user_name_input = input$reset_password_retrieve_user_name_input,
              user_name_column = "customer_email",
              selected_column = "customer_password",
              existing_password_input = input$reset_password_retrieve_password_input
            )
          )
        })
        # Feedback on new question and answer and commit
        observeEvent(input$reset_password_retrieve_commit,{
          if (trimws(input$reset_password_retrieve_question_new_input) != "" & 
              trimws(input$reset_password_retrieve_answer_new_input) != ""){
            password_retrieve_reset_commit_func(
              new_password_retrieve_question_input = input$reset_password_retrieve_question_new_input,
              new_password_retrieve_answer_input = input$reset_password_retrieve_answer_new_input,
              user_name_input = input$reset_password_retrieve_user_name_input,
              googlesheet_url = googlesheet_url
            )
            showModal(
              password_retrieve_updated_func()
            )
          } else{
            showModal(
              unacceptable_information_func()
            )
          }
        })
      }
      
      
# Generate ui inputs ------------------------------------------------------
### Load helper functions for ui_generator, rmarkdown functions
      source("shiny_helper_functions.R", local = TRUE)
### Generates UI for Demographic info and scoring info, using function to assign widgets based on data
      ui_builder(ui_demographics)
      ui_builder(ui_scoring)
### Calculate Age from birth date to test date and output in table
      source("shiny_age_functions.R", local = TRUE)
      
      
# Score Input Tab 1 ------------------------------------------

## User Inputs scoring information UI
        output$scoring_inputs_ui <- renderUI({
            req(credentials()$user_auth)
            lapply(1:nrow(ui_scoring), function(i) {
              uiOutput(ui_scoring$id[i])
            })
        })

      
# Collect Inputs and convert scaled score functions ----------------------------------------------------------

## Function to gather all input information
  gather_inputs <- function(input_id, input_labels){
  input_names <- input_id
  values <- lapply(input_names, function(x) input[[x]])
  values[sapply(values, is.null)] <- NA
  cbind.data.frame(values) %>%
    setNames(input_labels)
}
        
# Creates df with predicted scores for scales from education and age
        get_predicted_scores <- function(education_input, age_function, names){
        ## Finds right table by education
          predicted_scores %>% 
            filter(
              education == education_input #education == input$education
            ) -> df
        ## Finds highest age row that is less than input age
          lapply(ui_scoring$label[1:3], function(scale_name){
            df %>%
              filter(
                scale == scale_name,
                ages < floor(age_function) 
              ) %>%
              slice_tail() %>%
              select(scores)
          }) %>% 
            cbind.data.frame() %>%
            setNames(names) # names of scores
        }

# Gives residual scores, which are actual scores minus predicted
        get_residual_scores <- function(){
            gather_inputs(ui_scoring$id, ui_scoring$label) %>%
            dplyr::select(
              raw_score_names
            ) - get_predicted_scores(education_input = input$education, 
                                     age_function = age_decimal(birth_date = input$birth_date, test_date = input$test_date),
                                     names = raw_score_names)
        }

# Interference column
        predicted_interference <- function(word_raw, color_raw){
        ## Returns 0 for scores of 0 at initial set-up to avoid dividing by 0 problems,
          if(word_raw + color_raw == 0){
            0
          } 
        ## Uses Stroop formula to predict interference- from manual
          else{
              (word_raw * color_raw)/(word_raw + color_raw)
          }
        }
        
      ## Interference residual is color_word_raw - predicted interference
        interference_residual <- function(color_word_raw, word_raw, color_raw){
          color_word_raw - predicted_interference(word_raw, color_raw)
        }
        
      ## Interference T-score from lookup table
        interference_t_score <- function(color_word_raw, word_raw, color_raw, birth_date, test_date){
        ## Table is separated by two ages- less than 69, or over
          over_under_68 <- ifelse(age_decimal(birth_date = birth_date, test_date = test_date) <= 68, "young", "old")
          interference_t_scores %>% 
            filter(over_under_68 == age) %>%
          ## This finds t_score by finding the row of raw scores in table that is closest to actual raw score
            mutate(distance = abs(interference_residual(color_word_raw, word_raw, color_raw) - raw_scores)) %>% 
            slice_min(distance, n = 1, with_ties = F) %>%
            select(t_scores) %>%
            pull()
        }
        
      ## Puts all Interference scores- raw, residual, T-score in column
        interference_column <- function(){
          rbind.data.frame(predicted_interference(input$word_raw_score, input$color_raw_score), 
                           interference_residual(input$color_word_raw_score, input$word_raw_score, input$color_raw_score), 
                           interference_t_score(input$color_word_raw_score, input$word_raw_score, input$color_raw_score, input$birth_date, input$test_date)
                           )
        }


        
# Information Tab- First main tab -----------------------------------------

## Scaled Score Table Output- table with raw, residual, and t-scores of all scores
        output$scaled_score_table_ui <- renderUI({
          req(credentials()$user_auth)
        ## Minimum age is 15, checks if met
          validate(
            need(age_decimal(birth_date = input$birth_date, test_date = input$test_date) >= 15, "Age must be at least 15")
          )
          DT::DTOutput("scaled_score_table")
        })
        ## Gives simple dt of all scores
          output$scaled_score_table <- DT::renderDT({
            DT::datatable(frame_builder(), caption = "Stroop T-Scores", 
                          options = list(paging = F, searching = F, dom = "t", ordering = F))
          })        
        
# All T-Scores df- used in plot, and for interpretation, as T-scores are main focus
        get_t_scores <- function(residual_score_source){
          lapply(1:length(raw_score_names), function(i){
            resid_score <- residual_score_source[i] 
          ## -50 and 50 are min and max, returns those if less or greater
            resid_score <- ifelse(residual_score_source[i] < -50, -50, resid_score)
            resid_score <- ifelse(resid_score > 50, 50, resid_score)
            all_scores_t_scores %>%
            ## Looks up corresponding t-score
              filter(raw_deviation_score == resid_score[[1]]) %>%
              select(i + 1) # +1 because first column is score names
          }) %>%
            cbind.data.frame() %>%
            setNames(raw_score_names)
        }
        
# Frame builder- builds dataframe of all scaled score components- predicted, residual, t-scores
        frame_builder <- function(){
          name_column <- c("Predicted", "Residual", "T-Score")
          rbind.data.frame(get_predicted_scores(education_input = input$education, 
                                                age_function = age_decimal(birth_date = input$birth_date, test_date = input$test_date),
                                                names = raw_score_names), 
                           get_residual_scores(), 
                           get_t_scores(residual_score_source = get_residual_scores()))  %>%
            cbind.data.frame(name_column, ., interference_column()) %>%
            setNames(c("Score", raw_score_names_interference))
        }
        
# Plot T-Scores- bar plot, with identification of +/- 1,2 SD zones
{
  rectangle_coordinates=data.frame(x1 = rep(.5, 4), x2=rep(4.5, 4), y1=c(0, 20, 65, 80), 
                                   y2=c(20, 35, 80, 100), co=c('red','lightpink2','lightpink2','red'))
  rectangle_coordinates_short=data.frame(x1 = rep(.5, 4), x2=rep(1.5, 4), y1=c(0, 20, 65, 80), 
                                         y2=c(20, 35, 80, 100), co=c('red','lightpink2','lightpink2','red'))
  definer <- c("Statistical Significance", "Strong Statistical Significance")
        
  t_score_plot_func <- function(select_variable = NULL){
    # select_variable is each scale name
    {if(!is.null(select_variable)){
    # Build df of raw scores and interference
      get_t_scores(residual_score_source = get_residual_scores()) %>%
        cbind.data.frame(., Interference = interference_t_score(input$color_word_raw_score, input$word_raw_score, input$color_raw_score, test_date = input$test_date, birth_date = input$birth_date)) %>%
        select(!!quo(select_variable)) # pulls each variable one by one
    } else{
      get_t_scores(residual_score_source = get_residual_scores()) %>%
        cbind.data.frame(., Interference = interference_t_score(input$color_word_raw_score, input$word_raw_score, input$color_raw_score, test_date = input$test_date, birth_date = input$birth_date))
    }
    }%>%
      pivot_longer(everything()) %>%
      mutate(name = factor(name, levels = c(raw_score_names_interference))) %>% # orders by importance order
      {
        ggplot(., aes(x = name, y = value)) +
          geom_bar(stat = "identity", fill = "slategray") + 
          scale_y_continuous(breaks = seq(0,100,10), labels = seq(0,100,10), limits = c(0,100)) +
          ggtitle("T-Score Plot")  +
        ## select variable returns the rectangle coordinates that are zones of deviation
          geom_rect(data=if(is.null(select_variable)) {rectangle_coordinates} else {rectangle_coordinates_short}, 
                    mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=co), alpha = 0.2, inherit.aes = F) +
          scale_fill_identity(name = 'Significance Range', guide = 'legend',labels = c('Significant','Strongly Significant'))+
          geom_hline(aes(yintercept = 50), color = "lightskyblue", size =1) +
          labs(x = "Scale", y = "T-Score")
      }
  }
        output$t_scores_plot <- renderPlot(
          t_score_plot_func()
        )
  output$scaled_score_plot_ui <- renderUI({
          req(credentials()$user_auth)
          validate(
            need(age_decimal(birth_date = input$birth_date, test_date = input$test_date) >= 15, "Age must be at least 15")
          )
          plotOutput("t_scores_plot")
        })
}

        
# Interpretation Page- Second tab -----------------------------------------------------

# Functions to generate dynamic text for interpretation
## Name generator, based on what is entered, options to select name style
        name_generator <- function(name_options = "first", person = "examinee"){
          if (person == "examinee") {
            if (input$examinee_first_name == "" & input$examinee_last_name == ""){
              "Examinee"
            } else if(input$examinee_first_name != "" & name_options == "first") {
              input$examinee_first_name
            } else if(input$examinee_last_name != "" & name_options == "last") {
              paste("Examinee", input$examinee_last_name)
            } else if (name_options == "full") {
              paste(input$examinee_first_name, input$examinee_last_name)
            }
          } else if (person == "examiner") {
            if (input$examiner_first_name == "" & input$examiner_last_name == ""){
              "the Examiner"
            } else if(input$examiner_first_name != "" & name_options == "first") {
              input$examinee_first_name
            } else if(input$examiner_last_name != "" & name_options == "last") {
              paste("Examiner", input$examiner_last_name)
            } else if (name_options == "full") {
              paste(input$examiner_first_name, input$examiner_last_name)
            }
          }
        }
        
## T-Score Interpretation Function
        interpretive_labels_df <- tibble(
          cutoff_values = c(20, 35, 65, 80, 110), # These are boundaries for -/+ 1,2SD for t-scores
          interpretive_labels = c("more than 2 SD below the mean, strongly significantly worse than peers",
                                  "1 SD below the mean, significantly worse than peers",
                                  "within +/- 1 SD from the mean, average compared to peers",
                                  "1 SD above the mean, significantly better than peers",
                                  "more than 2 SD above the mean, strongly significantly better than peers")
        )
        
      # Finds appropriate label based on score
        interpretive_labels_func <- function(score){
          interpretive_labels_df %>%
            filter(score <= cutoff_values) %>%
            slice_min(cutoff_values) %>%
            select(interpretive_labels)
        }
        
      # Gives text passage for score
        interpretation_function <- function(i){
           paste0(name_generator(), " scored ", frame_builder()[3,(length(raw_score_names_interference) + 1 - i + 1)],
                " on ", gsub(" .*", "", rev(raw_score_names_interference)[i]), ". This score is ",
                 interpretive_labels_func(frame_builder()[3,(length(raw_score_names_interference) + 1 - i + 1)]),
                 ". This scale is an assessment of ", stroop_scale_interpretation[,i])
        }
        
      ## Interpretation Output
        lapply(rev(raw_score_names_interference), function(scale_name){
      ## Gives an individual plot for each score, here select_variable is not NULL, for each score
        output[[scale_name]]<- renderPlot(
          t_score_plot_func(select_variable = scale_name)
        )
        })
        
      ## Creates rows for each scale with text and plot
        output$interpretation_rows <- renderUI({
          req(credentials()$user_auth)
          lapply(1:length(raw_score_names_interference), function(i) { 
            fluidRow(h3(rev(raw_score_names_interference)[i]),
                     box(
                       p(interpretation_function(i))
                       ),
                     box(
                       plotOutput(rev(raw_score_names_interference)[i])
                     )
            )
          })
          
        })
        
## Narrative text for introduction
        narrative_text_func <- function(){
          paste0(name_generator(name_options = "full", person = "examinee"), " was administered the Stroop Color and
                Word Test on ", input$test_date, ", by ", name_generator(name_options = "full", person = "examiner"),
                " on ", input$test_date, ". ", "At the time of testing, ", 
                name_generator(name_options = "first", person = "exmainee"), " was ", round(age_decimal(birth_date = input$birth_date, test_date = input$test_date),1), " and had completed ",
                input$education, " as highest level of education. The Stroop Color-Word Test is an efficient assessment of several 
                neuropsychological and cognitive abilities, including ability to inhibit distracting information and 
                process information quickly and accurately, abilities commonly used in tasks requiring attention,
                working memory, flexibility, and processing speed.")
        }
        
        
# Report- Third tab -----------------------------------------------------------
        
## UI for report  
      output$report_selectors <- renderUI({
          req(credentials()$user_auth)
        ## Taglist for multiple ui in one render
          tagList(
        ## Choose output format
            tipify(radioButtons("report_format", "Choose format for report", choices =
                           c("HTML" = "html",
                             "PDF" = "pdf",
                             "Word/DOCX" = "docx"), inline = TRUE
            ),
            title = "HTML- nice interactive format. PDF- nice static format. Word/DOCX- editable doc.",
            placement = "below"),
            tipify(downloadButton("generate_report", "Generate Report", icon = shiny::icon("file-contract")), 
                   title = "Choose desired report format and click to download a report", placement = "right") 
            
          )
        })
        
## UI for saving client information- individual record
        output$save_client <- renderUI({
          req(credentials()$user_auth)
            tipify(downloadButton("download_data", "Save Data", icon = shiny::icon("file-csv")),
                   title = "Save current input to individual .csv file. Save to dedicated record folder.", 
                   placement = "right")
        })
      ## Gives exact time to avoid duplicate file names
        humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
        
        # Download Examinee Data as file- will use pattern to recognize legit files
        output$download_data <- downloadHandler(
          filename = function() {
            paste(sprintf('%s_%s_%s',input$examinee_first_name, input$examinee_last_name, humanTime()), "_stroop_data.csv", sep = "")
          },
          content = function(file) {
            cd<-cbind.data.frame(gather_inputs(ui_demographics$id, ui_demographics$id),
                                 gather_inputs(ui_scoring$id, ui_scoring$id))
            write.csv(cd, file, row.names = F)
          }
        )



        
# Rmarkdown Report --------------------------------------------------------

        output$generate_report <- downloadHandler(
          # For PDF output, change this to "report.pdf"
          filename = function() {
            paste('stroop_report', input$examinee_last_name, input$examinee_first_name, sep = '_', paste0('.', input$report_format))
          },
          content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "stroop_report.Rmd")
            file.copy("stroop_report.Rmd", tempReport, overwrite = TRUE)
          # I don't think needed because sourced in global, but a way to pass file to rmarkdown
            src2 <- file.path(tempdir(), 'shiny_helper_functions.R')
            file.copy("shiny_helper_functions.R", src2)
            
            # Can perform computations here
            # input_type = input$input_type 
            params <- list(t_score_df = get_t_scores(residual_score_source = get_residual_scores()) %>%
                             cbind.data.frame(., Interference = interference_t_score(input$color_word_raw_score, input$word_raw_score, input$color_raw_score, test_date = input$test_date, birth_date = input$birth_date)),
                           report_format = input$report_format,
                           demographics_inputs = gather_inputs(ui_demographics$id, ui_demographics$label),
                           scoring_inputs = gather_inputs(ui_scoring$id, ui_scoring$label),
                           t_score_plots = lapply(rev(raw_score_names_interference), function(scale_name){
                               t_score_plot_func(select_variable = scale_name)
                           }),
                           interpretation_text = lapply(1:4, function(i) interpretation_function(i)),
                           raw_score_names_interference = raw_score_names_interference,
                           raw_score_names = raw_score_names,
                           narrative_text = narrative_text_func()
            )
            # # Knit the document, passing in the `params` list, and eval it in a
            # # child of the global environment (this isolates the code in the document
            # # from the code in this app).
            rmarkdown::render(tempReport, output_file = file, output_format = switch(input$report_format, pdf = 'pdf_document',
                                                                                     html = 'html_document', docx = 'word_document'),
                              params = params,
                              envir = new.env(parent = globalenv())
            )
          }
        )
        

        
# Restore Past Records- Choose method ----------------------------------------------------
  ## Client chooses method to upload records and will do so
        output$load_client_ui <- renderUI({
          req(credentials()$user_auth)
          fluidRow(column(4,
                        ## Client chooses folder and all stroop records will be loaded to table in app
                          if (input$upload_format == "Folder")
                          {
                            wellPanel(
                              tags$div(
                                class = "form-group shiny-input-container",
                                tags$div(tags$label("Choose Folder with Individual Records")),
                                tipify(
                                  tags$div(
                                    tags$label(
                                      "Choose folder",
                                      class = "btn btn-primary",
                                      tags$input(
                                        id = "fileIn",
                                        webkitdirectory = TRUE,
                                        type = "file",
                                        style = "display: none;",
                                        onchange = "pressed()"
                                      )
                                    )
                                  ), 
                                  "Choose a dedicated folder with individual records, which will display in table."),
                                tags$label("No folder chosen", id = "noFile"),
                                tags$div(
                                  id = "fileIn_progress",
                                  class = "progress progress-striped active shiny-file-input-progress",
                                  tags$div(class = "progress-bar")
                                )
                              )
                            )
                        ## Can choose an individual record
                          } else if (input$upload_format == "Individual") {
                            tipify(
                              fileInput("upload_individual_file", "Choose Individual Record"),
                              "Choose an individual .csv file that was saved"
                            )
                          } else {
                        ## Can choose a file with multiple records
                            tipify(
                              fileInput("uploadFile", "XLSX file"),
                              "Choose a .xlsx file of multiple records that was downloaded from this app."
                            )
                          }
          ),
          column(8,
                 tabsetPanel(
                  ## Displays all records chosen from input method
                   tabPanel(
                     "Previous Session Data",
                     h3("Available Records"),
                     dataTableOutput("full_patient_record"),
                     bsTooltip(
                       "full_patient_record",
                       "All records available. Select individual or multiple records to load into Selected Record table.",
                       "left",
                       options = list(container = "body")
                     ),
                     hr(),
                    ## Can choose one record to restore or download
                     h3("Selected Record"),
                     rHandsontableOutput("selected_patient"),
                     bsTooltip(
                       "selected_patient",
                       "Selected records- whole table will be selected for multiple record download, or select row
                                number for individual record restore. Can edit cells with valid values.",
                       "left",
                       options = list(container = "body")
                     ),
                  ## If any invalid values, will identify
                     dataTableOutput("invalid_df"),
                     bsTooltip(
                       "invalid_df",
                       "Records with invalid values will show with problem cells identified. Edit in table to fix.",
                       "left",
                       options = list(container = "body")
                     ),
                     br(),
                  ## Options of what to do with records
                     fluidRow(
                    ## Restore one record as current session
                       column(4,
                              shiny::actionButton("restore_session", "Restore Selected Record"),
                              bsTooltip(
                                "restore_session",
                                "Click on desired record row number above and click to restore an individual session.",
                                "right",
                                options = list(container = "body")
                              )
                       ),
                    ## Can download multiple records
                       column(4, 
                              downloadButton("save_all_records", "Save all Records"),
                              bsTooltip(
                                "save_all_records",
                                "Select records in Available Records to Selected Record Table to save all records to .xlsx.",
                                "right",
                                options = list(container = "body")
                              )
                       ),
                    ## Can download an appropriate template
                       column(4,
                              tipify(
                                shiny::a(h4("Download Multiple Record Template", class = "btn btn-default action-button" , 
                                            style = "fontweight:600"), target = "_blank",
                                        ## File must be in www folder
                                         href = "stroop_scoring_app_wide_form_data_entry_template.xlsx",
                                         download = NA),
                                "Download an .xlxs template to enter raw data, then upload to perform scoring analysis.",
                                "left"
                              )
                       )
                     )
                   )
                 ))
          )
        })
        
        
        
# Restore Past Records- Load Record ----------------------------------------------------
        
# *** Select client record and restore a session or get all client records
        {
        # Following adds a blank row with same data.frame structure- used to initialize the whole record dt and to add a blank row at end
          cols <- c(ui_demographics$id, ui_scoring$id) # all inputs
          df_row_maker <- function(){
            data.frame(
              matrix(" ", ncol = length(cols))
            ) %>% setNames(., cols)
          }
          
        # Selected records- either all or one sheet
          all_records <- reactiveValues(
            df = df_row_maker()
          )
          
        # Select a folder and upload all csv files in it
          ## To display all 
          df_selected_upload <- eventReactive(input$fileIn, {
            inFiles <- input$fileIn 
            df <- data.frame()
            if (is.null(inFiles))
              return(NULL)
            inFiles <- inFiles[grepl("_stroop_data.csv$", inFiles$name),] # only those files from app
            for (i in seq_along(inFiles$datapath)) {
              tmp <- read.csv(inFiles$datapath[i][grepl("_stroop_data.csv$", inFiles[,1][i])], header = T)
              df <- rbind.data.frame(df, tmp)
            }
            df
          })
      # Assigns whatever method files are input to all_records$df reactive
        ## Upload folder
          observeEvent(input$fileIn, {
            all_records$df <- df_selected_upload()
          })
          
        ## Upload multiple records
          observeEvent(input$uploadFile,{
            all_records$df <- whole_dataset()
          })
          
        ## Upload one record
          observeEvent(input$upload_individual_file,{
            file <- input$upload_individual_file
            ext <- tools::file_ext(file$datapath)
            req(file)
            validate(need(ext == "csv", "Please upload a csv file"))
            all_records$df <- read.csv(file$datapath)
          })
          
# Datatable with all the records selected in df_selected_upload function- this presents all possible records to user
          output$full_patient_record <- DT::renderDataTable({
            datatable(all_records$df,
                      options = list(
                        scrollX = TRUE
                      )
            )
          })

# Reactive values df selected_df$df that will hold selected records and be passed as final df          
          selected_df <- reactiveValues(
            df = df_row_maker()
          )
# Identifies records the user chooses from full list of records and sends to reactive values
          observeEvent(input$full_patient_record_rows_selected, {
            req(any(all_records$df != " "))
            if(!is.null(input$full_patient_record_rows_selected)){
              selected_df$df <- all_records$df[input$full_patient_record_rows_selected,]
              req(nrow(selected_df$df) >0)
            # This adds an extra blank row at end if the last row has any data- so there's always a blank row at end
                selected_df$df <- if (selected_df$df %>% ungroup() %>% slice_tail(n = 1) %>% apply(., 2, function(value) value != " ") %>% any()){
                   rbind.data.frame(selected_df$df, df_row_maker())
                }
            }
            
          })
          
          observeEvent(input$selected_patient,{
            req(!is.null(df_selected_upload()))
            req(nrow(selected_df$df) >0)
          # Takes what's entered in handsontable and sends to reactive values
            out <- hot_to_r (
              input$selected_patient
            )
          # Will take all records with data in it, doesn't take blank row  
            out <- if(out %>% slice_tail(n = 1) %>% apply(., 2, function(value) value != " ") %>% any()){
              bind_rows(out, df_row_maker())
            } else{
              out
            }
            selected_df$df <- out
          })
        ## Outputs selected records
          output$selected_patient <- renderRHandsontable(
            rhandsontable(
              selected_df$df,
              selectCallback = TRUE,
              readOnly = FALSE
            )
          )

          # Function and button to update inputs with selected patient data
          {
            update_functions <- list(
              update_date_input <- function(vars, val) {
                date_value <- as.Date(val, format = "%m/%d/%Y")
                updateDateInput(session, vars, value = date_value) 
              },
              update_numeric_input <- function(vars, val) {
                updateNumericInput(session, vars, value = val)
              },
              update_text_input <- function(vars, val) {
                updateTextInput(session, vars, value = val)
              },
              update_text_area_input <- function(vars, val) {
                updateTextAreaInput(session, vars, value = val)
              },
              update_select_input <- function(vars, val) {
                updateSelectInput(session, vars, selected = val)
              }
            ) %>%
              setNames(c("dateInput", "numericInput", "textInput", "textAreaInput", "selectInput"))
            
          }
            
            observeEvent(input$restore_session, {
              selected_df$df[input$selected_patient_select$select$r, ] %>% # Returns selected row data
                t() %>%
                as_tibble(., rownames = "id") %>%
                setNames(., c("id", "value")) -> out
              ui_scoring %>% dplyr::select(type, id) %>%
                rbind.data.frame(., ui_demographics %>% dplyr::select(type, id)) %>% # Gets information about fields
                inner_join(., out, by = "id")-> out
              out %>%
                group_by(type) %>%
                nest() ->out
              update_functions <- update_functions[order(match(names(update_functions), out$type))]
              
              lapply(1:length(out$data), function(i){
                out$data[[i]] %>%
                rowwise() %>%
                group_map(~ update_functions[[i]](.$id, val = .$value))
              })
              
            })
          }
# Convert raw to scaled scores for all chosen records- wide-form
        {
          validate_values_func <- function(df, validate_column, validate_expression){
            df %>%
              mutate(!!paste0(sym(validate_column), "_validated") := ifelse(!!rlang::parse_expr(validate_expression), "problem_value", !!sym(validate_column))) %>% 
              select(!!paste0(sym(validate_column), "_validated"))
          }
        ## Check for valid values
          test_expressions <- c(
            "word_raw_score < 0",
            "color_raw_score < 0 ",
            "color_word_raw_score < 0 ",
            "! education %in% c('No HS Diploma', 'HS Diploma', 'Some College', 'Bachelor Degree', 'Post-grad', 'Other')",
            "is.na(as.Date(birth_date, format = '%m/%d/%Y'))",
            "is.na(as.Date(test_date, format = '%m/%d/%Y'))"
          )
          
        ## This gets the rows with invalid data
          invalid_df_func <- function(){
          ## First get all the data without empty rows
            selected_df$df %>%
              ungroup() %>%
              filter(!apply(., 1, function(x) all(x == " "))) %>%   
              rowwise() -> full_df
           ## Subset a valid df 
            valid_df <- cbind.data.frame(full_df,
                                         lapply(1:length(test_expressions), function(i) validate_values_func(full_df, ui_scoring$id[i], test_expressions[i])) %>% bind_cols()
            ) %>%
            ## For any problem values prints those columns as "problem_value"
              filter(!apply(., 1, function(x) any(x == "problem_value"))) -> valid_df # filter(!apply(. == "problem_value", 1, any)) -> valid_df
            anti_join(full_df, valid_df, by = colnames(full_df)) -> invalid_df # Gets invalid
            cbind.data.frame(invalid_df,
                                         lapply(1:length(test_expressions), function(i) validate_values_func(invalid_df, ui_scoring$id[i], test_expressions[i])) %>% bind_cols()
            )
          }
          
          valid_df_func <- function(){
            selected_df$df %>%
              ungroup() %>%
              filter(!apply(., 1, function(x) all(x == " "))) %>% 
              rowwise() -> full_df
            
            valid_df <- cbind.data.frame(full_df,
                                         lapply(1:length(test_expressions), function(i) validate_values_func(full_df, ui_scoring$id[i], test_expressions[i])) %>% bind_cols()
            ) %>%
              filter(!apply(., 1, function(x) any(x == "problem_value"))) -> valid_df
            
            valid_df %>% 
              mutate(across(ends_with("score"), as.numeric)) -> valid_df
            
            valid_df  %>% 
              rowwise() %>% 
              mutate(
                birth_date = as.Date(birth_date, format = "%m/%d/%Y"),
                test_date = as.Date(test_date, format = "%m/%d/%Y"),
                age = age_decimal(birth_date = birth_date, test_date = test_date),
                get_predicted_scores(education_input = education, age_function = age, names = paste0(ui_scoring$id[grepl("score", ui_scoring$id)], "_predicted")),
                setNames(across(ends_with("raw_score")) - across(ends_with("predicted")), paste0(ui_scoring$id[grepl("score", ui_scoring$id)], "_residual")),
                setNames(get_t_scores(residual_score_source = across(ends_with("residual"))), paste0(ui_scoring$id[grepl("score", ui_scoring$id)], "_t_scores")),
                predicted_int = predicted_interference(word_raw = word_raw_score, color_raw = color_raw_score),
                interference_resid = interference_residual(color_word_raw = color_word_raw_score, word_raw = word_raw_score, color_raw = color_raw_score),
                interference_t = interference_t_score(color_word_raw = color_word_raw_score, word_raw = word_raw_score, color_raw = color_raw_score, birth_date = birth_date, test_date = test_date)
              ) %>% 
              ungroup() 
          }
          
        ## Reactive value df to hold invalid
          invalid_df <- reactive({
            req(invalid_df_func())
            if(nrow(invalid_df_func()) > 0){
              dat <- invalid_df_func()
            } else {
              dat <- NULL
            }
            validate(
              need(!is.null(dat), "No invalid records")
            )
            return(dat)
          })
          
        ## Prints invalid data
          output$invalid_df <- renderDataTable(
            datatable(
              invalid_df(),
              caption = "Invalid Values",
              options = list(searching = FALSE,
                             paging = FALSE,
                             scrollX = TRUE)
              )
            )
        ## User can download multiple records
          output$save_all_records <- downloadHandler(
            
            filename = function() {
              paste("full_record_", humanTime(), "_stroop_wide_data.csv", sep = "")
            },
            content = function(file) {
              cd <- valid_df_func()
              write.csv(cd, file, row.names = F)
            }
          )
          
        ## User can upload a file with multiple records 
          whole_dataset<-eventReactive(input$uploadFile, { 
            inFile <- input$uploadFile 
            dat<-read_xlsx(inFile$datapath, 1) %>% 
              mutate(across(everything(), as.character),
                     birth_date = format.Date(birth_date, "%m/%d/%Y"),
                     test_date = format.Date(test_date, "%m/%d/%Y")
                     )
            all_records$df <- dat
            return(dat)
          })
        }    
        
} # Final Server bracket
    
# Run the application
    shinyApp(ui = ui, server = server)