#### SHINY CODE FUNCTIONS ####

#**** BUILD UI FUNCTION FROM FILE ****#
## Data should be organized in a file like:
#data <- tibble(
#    type = c("textInput", "actionButton"),
#    id = c("text1", "button1"),
#    label = c("Some Text", "Some Action"),
#    value = c(NA, NA),
#    min = c(NA, NA),
#    max = c(NA, NA),
#    choices = c(NA, NA) ## choices should be a comma seprated string
#)

## function
ui_builder <- function(data){
  lapply(1:nrow(data), function(i) {
    output[[data$id[i]]] <- renderUI({
      if(data$type[i] == "textInput"){
        textInput(inputId = data$id[i], label = data$label[i])
      } else if(data$type[i] == "textAreaInput"){
        textAreaInput(inputId = data$id[i], label = data$label[i])
      } else if(data$type[i] == "actionButton"){
        actionButton(inputId = data$id[i], label = data$label[i])
      } else if(data$type[i] == "dateInput"){
        dateInput(inputId = data$id[i], label = data$label[i], value = ifelse(is.na(data$value[i]), as.character(Sys.Date()), data$value[i]),
                  format = data$options[i])
      } else if(data$type[i] == "numericInput"){
        numericInput(inputId = data$id[i], label = data$label[i], value = as.numeric(data$value[i]),
                     min = as.numeric(data$min[i]), max = as.numeric(data$max[i]))
      } else if(data$type[i] == "selectInput"){
        selectInput(inputId = data$id[i], label = data$label[i], choices = 
                      trimws(unlist(str_split(data$choices[i], ","))))
      } else if(data$type[i] == "radioButtons"){
        radioButtons(inputId = data$id[i], label = data$label[i], choices = 
                      trimws(unlist(str_split(data$choices[i], ","))))
      }
    })
  })
}
## In server, call
# ui_builder(name_of_data)

## Then in ui, insert

#        mainPanel( # or wherever it goes
#            lapply(1:nrow(name_of_data), function(i) {
#                uiOutput(name_of_data$id[i])
#            })        
#        )


## Use to print tables in any format
getOutputFormat <- function() {
  output <- rmarkdown:::parse_yaml_front_matter(
    readLines(knitr::current_input())
  )$output
  if (is.list(output)){
    return(names(output)[1])
  } else {
    return(output[1])
  }
}

table_output_func <- function(df, colnames, caption, ...){
  # with html- typically use table_output_func(df, colnames = c("col1", "col2"), caption = table_counter_func("caption")), df req
  
  if (getOutputFormat() == "html_document"){
    if (missing(colnames)){
      colnames <- names(df)
    }
    if (missing(caption)){
      caption <- NULL
    }
    kable(df, row.names = F, col.names = colnames, caption = caption, ...) %>%
      kable_options()
  } else {
    # with pdf or word use table_output_func(df, colnames = c("col1", "col2"), values = table_counter_func("caption")), df req
    names1 = names(df)
    names2 = if(missing(colnames)){
      names1
    } else {
      colnames
    }
    if (missing(caption)){
      caption <- NULL
    }
    ft <- flextable(df)
    ft <- set_header_df(x = ft, mapping = data.frame(keys = names1, values = names2, stringsAsFactors = FALSE),
                        key = "keys" ) %>%
      add_header_lines(top = TRUE, values = caption)
    theme_zebra(ft) # or theme_booktabs for plain tables
  }
}

table_print <- function(df, colnames, caption, ...){
  if (isTRUE(getOption('knitr.in.progress'))) {
    table_output_func(df, colnames, caption, ...)
  } else {
    df %>%
      kable(caption = caption, col.names = colnames) %>%
      kable_options()
  }
}
