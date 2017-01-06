library(shiny)
library(data.table)
library(plotly)
library(zoo)
library(RColorBrewer)
library(shinythemes)

ui <- navbarPage(theme = shinytheme("flatly"),
                 title = "My expenses",
                 
                 tabPanel("General", 
                          fluidPage(
                            fluidRow(
                              column(6),
                              column(3, selectInput(inputId = "start_m1",
                                                    label = "Start month:", 
                                                    choices = period[-1])),
                              column(3, selectInput(inputId = "end_m1", 
                                                    label = "End month:",
                                                    choices = period[-1],
                                                    selected = tail(period, 1))))
                            ),
                          br(),
                          plotlyOutput("plot_gen"),
                          br(),
                          plotlyOutput("plot_capital")),
                 
                 tabPanel("Category", 
                          fluidPage(
                            fluidRow(
                              column(6),
                              column(3, selectInput(inputId = "start_m2",
                                                    label = "Start month:", 
                                                    choices = period[-1])),
                              column(3, selectInput(inputId = "end_m2", 
                                                    label = "End month:",
                                                    choices = period[-1],
                                                    selected = tail(period, 1)))
                            ),
                            br(),
                            plotlyOutput("plot_all_cat"),
                            br(),
                            plotlyOutput("plot_cat_by_mon"))),
                 
                 tabPanel("Subcategory", 
                          fluidPage(
                            fluidRow(
                              column(6, selectizeInput(inputId = "cat",
                                                       label = "Select category:",
                                                       choices = unique(df_expenses[order(-Amount)]$To))),
                              column(3, selectInput(inputId = "start_m3",
                                                    label = "Start month:", 
                                                    choices = period[-1])),
                              column(3, selectInput(inputId = "end_m3", 
                                                    label = "End month:",
                                                    choices = period[-1],
                                                    selected = tail(period, 1)))
                            ),
                            br(),
                            plotlyOutput("plot_subcat"),
                            br(),
                            plotlyOutput("plot_subcat_by_mon")))
                 )