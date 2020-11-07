library(tidyverse)
library(shinyWidgets)
library(shiny)

red_color = "#d23b00"
gold_color = "#e3a200"

# SLIDER INPUTS
ui <- function(request) {
  fluidPage(tags$head(tags$style("label{font-family: Proxima Nova;}")),
            sidebarLayout(
              sidebarPanel(
                setSliderColor(c(red_color, gold_color, "gray", "gray"), c(1, 2, 3, 4)),
                sliderInput(
                  "current_rating_input",
                  "Current rating",
                  min = 3.5,
                  max = 5,
                  value = 4.1,
                  step = 0.1
                ),
                sliderInput(
                  "goal_ratinging_input",
                  "Goal rating",
                  min = 3.5,
                  max = 5,
                  value = 4.5,
                  step = 0.1
                ),
                checkboxGroupInput("issue_checkboxes",
                                   "Issues resolved"),
                
                sliderInput(
                  "future_ratings_input",
                  "# of future ratings",
                  min = 100,
                  max = 1000,
                  value = 700,
                  step = 100
                ),
                tagList("v0.1","Made by ", a("Haystack Reviews", href="https://haystackreviews.com/"))
              ),
              
              # DATA TAB INPUTS
              mainPanel(tabsetPanel(
                type = "tabs",
                tabPanel("Graph", plotOutput("Graph")),
                tabPanel(
                  "Data",
                  wellPanel(
                    textInput("app_name_input", "App Name", value = "Navy Federal Credit Union (Android)"),
                    br(),
                    numericInput("reviews_analyzed_input", "# of reviews analyzed", 142),
                    br(),
                    textInput("issue_one_input", "Issue #1", value = "Bugs"),
                    numericInput("lost_stars_one_input", "Issue #1 Lost Stars", 204),
                    br(),
                    textInput("issue_two_input", "Issue #2", value = "Feature Requests"),
                    numericInput("lost_stars_two_input", "Issue #2 Lost Stars", 23),
                    br(),
                    textInput("issue_three_input", "Issue #3", value = "Version Incompatibility"),
                    numericInput("lost_stars_three_input", "Issue #3 Lost Stars", 17),
                    bookmarkButton()
                  )
                )
              ))
            ))
}

server <- function(input, output, session) {
  output$Graph <- renderPlot({
    # READ SLIDER INPUTS (numeric)
    current_rating <- as.numeric(input$current_rating_input)
    goal_rating <- as.numeric(input$goal_ratinging_input)
    past_ratings <- as.numeric(input$past_ratings_input)
    future_ratings <- as.numeric(input$future_ratings_input)
    
    # READ DATA TAB INPUTS (text)...
    # i.e. change name of check boxes
    issue_one_text <- as.character(input$issue_one_input)
    issue_two_text <- as.character(input$issue_two_input)
    issue_three_text <- as.character(input$issue_three_input)
    
    # READ DATA TAB INPUTS (numeric)
    reviews_analyzed <- (input$reviews_analyzed_input)
    lost_stars_one <- (input$lost_stars_one_input)
    lost_stars_two <- (input$lost_stars_two_input)
    lost_stars_three <- (input$lost_stars_three_input)
    
    original_issue_checkboxes <- input$issue_checkboxes
    
    # update select button names
    updateCheckboxGroupInput(
      session,
      "issue_checkboxes",
      choices = c(issue_one_text,
                  issue_two_text,
                  issue_three_text),
      selected = original_issue_checkboxes
    )
    
    ## CREATE MODEL
    
    # create vector from 0 to 1000 by 1
    # (i.e. this creates the x-axis of the graph)
    ratings_vector <- c(seq(from = 0, to = 1000, by = 1))
    
    # make vector into a column to use for a data frame (x-axis)
    x <- c(ratings_vector)
    
    # use 'diff' (difference between current rating and 5) as a weight
    # (rate will increase slower when current rating is higher, vice versa)
    diff = (5 - current_rating)
    
    ## calculate stars lost due to Issues 1, 2 & 3
    
    # amount of stars available (5 times number of reviews ANALYZED)
    if (is.na(reviews_analyzed) || reviews_analyzed == 0)  {
      stars_potential = 5
    } else {
      stars_potential = (5 * reviews_analyzed)
    }
    
    # stars lost PRIOR to addressing changes
    stars_lost_prior = lost_stars_one + lost_stars_two + lost_stars_three
    
    # stars lost AFTER addressing changes
    stars_lost = lost_stars_one + lost_stars_two + lost_stars_three
    
    
    if (is.na(stars_lost)) {
      stars_lost = 0
    }
    
    if (is.null(original_issue_checkboxes)) {
      # No boxes checked, we haven't resolved any issues to recover stars
    } else {
      if (issue_one_text %in% original_issue_checkboxes &&
          !is.na(lost_stars_one)) {
        stars_lost = stars_lost - lost_stars_one
      }
      if (issue_two_text %in% original_issue_checkboxes &&
          !is.na(lost_stars_two)) {
        stars_lost = stars_lost - lost_stars_two
      }
      if (issue_three_text %in% original_issue_checkboxes &&
          !is.na(lost_stars_three)) {
        stars_lost = stars_lost - lost_stars_three
      }
    }
    
    # stars earned by addressing changes (will = 0 if no changes addressed)
    stars_earned = stars_lost_prior - stars_lost
    
    # create y-axis
    
    if (is.null(original_issue_checkboxes)) {
      # multiplier =1 due to no changes have been addressed (no stars earned back)
      inverse_mult <- 1
      ratio <- c(current_rating - 1 + (ratings_vector)
                 / (ratings_vector))
    }
    
    else {
      # create multipler to account for stars earned after changes made
      inverse_mult = (stars_earned / stars_potential)
      
      # create "ratio" which takes mulitplier, "diff" and y-intercept "current rating" into account
      ratio <-
        c(current_rating + ((inverse_mult)) * diff * (ratings_vector)
          / (ratings_vector + length(ratings_vector) / 2))
    }
    
    # create data frame (combine x & y axis to make graph
    df_test <- data.frame(x, ratio)
    # lower confidence line (dashed line under main line)
    df_test <- df_test %>%
      mutate(x, lwr = 0.90 * ratio)
    # upper confidence line (dahsed line over main line
    df_test <- df_test %>%
      mutate(x, upr = 1.10 * ratio)
    
    # plot dimensions
    par(
      # change font
      #family = 'serif',
      # Change the colors
      col.main = "black",
      col.lab = "black",
      col.sub = "black",
      # Titles in italic and bold
      font.main = 4,
      font.lab = 4,
      font.sub = 4,
      # Change font size
      cex.main = 1.5,
      cex.lab = 1,
      cex.sub = 1
    )
    
    ## plot function (only includes main line "ratio")
    plot(
      ratings_vector,
      ratio,
      xlim = c(0, future_ratings),
      ylim = c(3.5, 5),
      lty = 0.1,
      lwd = 0.1,
      col = "gray",
      main = "",
      xlab = "",
      ylab = "",
      sub = ""
    )
    
    app_name_text <- as.character(input$app_name_input)
    # X&Y axis labels
    title(main = app_name_text,
          xlab = "Future Ratings",
          ylab = "Avg. Rating")
    
    ## lines added to plot
    # lower confidence line
    #lines(df_test$lwr,
    #      lty = 2,
    #      lwd = 1,
    #      col = "gray")
    # upper confidence line
    #lines(df_test$upr,
    #      lty = 2,
    #      lwd = 1,
    #      col = "gray")
    abline(h = current_rating, col = red_color)
    abline(h = goal_rating, col = gold_color)
    
    # legend
    legend(
      "bottomright",
      legend = c("Goal Rating", "Projected Rating", "Current Rating"),
      col = c(gold_color, "gray", red_color),
      lty = 1,
      cex = 1.1
    )
    
  })
}

shinyApp(ui,
         server,
         enableBookmarking = "url")
