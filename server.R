library(shiny) 
library(ggplot2)
library(zoo)
library(plotly)
library(data.table)
library(RColorBrewer)
library(shinyjs)


server <- function(input, output, session) {
  
  #general plot expenses + incomes by month
  observeEvent(input$start_m1, {
    updateSelectInput(session, 'start_m2', selected = input$start_m1)
    updateSelectInput(session, 'start_m3', selected = input$start_m1)
  })
  
  observeEvent(input$end_m1, {
    updateSelectInput(session, 'end_m2', selected = input$end_m1)
    updateSelectInput(session, 'end_m3', selected = input$end_m1)
  })
  
  df_main <- reactive(df_expenses[MonY >= input$start_m1 & MonY <= input$end_m1])
  
  income <- reactive(df_income[MonY >= input$start_m1 & MonY <= input$end_m1])
  
  df_gen <- reactive(data.table(df_main()[, .(Expenses = round(sum(Amount), digits = 0)), by = .(Month)], 
                             Income = income()[, .(Expenses = round(sum(Amount), digits = 0)), by = .(Month)]$Expenses))
    
  gen <- reactive(
    plot_ly(df_gen(), x = ~Month, y = ~Expenses, 
            name = 'expenses', 
            type = 'scatter',
            mode = 'lines+markers') %>%
      add_trace(y = ~Income, name = 'income', mode = 'lines+markers',
                text = ~paste(c(df_gen()$Income - df_gen()$Expenses))) %>%
      layout(title = "Expenses by month",
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             hovermode = 'compare',
             legend = list(x = 0.1, y = 0.9)) %>%
      config(displayModeBar = F))
  
  output$plot_gen <- renderPlotly({gen()})
  
  capital <- reactive(
    plot_ly(df_accum[MonY >= input$start_m1 & MonY <= input$end_m1],
            x = ~Month,
            y = ~Accum,
            type = 'scatter',
            mode = 'lines+markers') %>%
      layout(title = "My fund by month",
             xaxis = list(title = ""),
             yaxis = list(title = "")) %>%
      config(displayModeBar = F)
  )
  
  output$plot_capital <- renderPlotly({capital()})
  
  #barplot. expenses by category
  
  observeEvent(input$start_m2, {
    updateSelectInput(session, 'start_m1', selected = input$start_m2)
    updateSelectInput(session, 'start_m3', selected = input$start_m2)
  })
  
  observeEvent(input$end_m2, {
    updateSelectInput(session, 'end_m1', selected = input$end_m2)
    updateSelectInput(session, 'end_m3', selected = input$end_m2)
  })
  
  
  allCat <- reactive(df_main()[, .(Expenses = round(sum(Amount), digits = 0)), by = .(To, color)])
    
  all_cat <- reactive(
    plot_ly(allCat(), x = ~reorder(To, -Expenses), y = ~Expenses, 
                     source = 'cat', type = 'bar', 
                     marker = list(color = ~color),
                     text = ~paste(c(round(prop.table(allCat()$Expenses) *100, 1)), '%', sep = "")) %>%
    layout(title = "Expenses by category",
           xaxis = list(title = ""),
           yaxis = list(title = "")) %>%
    config(displayModeBar = F))
  
  output$plot_all_cat <- renderPlotly({all_cat()})
  
  #plot. expenses by month for selected category
  
  click_cat <- reactive(event_data("plotly_click", source = 'cat')) 
  
  catMon <- reactive(df_main()[, .(Expenses = round(sum(Amount), digits = 0)), by = .(To, Month, color)])
  
  cat_by_mon <- reactive(
    plot_ly(catMon()[To == click_cat()$x], 
            type = 'scatter', 
            x = ~Month, y = ~Expenses,
            mode = 'markers', 
            marker = list(color = ~color)) %>% 
        add_trace(name = "", 
                  mode = 'lines', 
                  line = list(color = catMon()[To == click_cat()$x]$color[1])) %>%
        layout(title = click_cat()$x,
               xaxis = list(title = ""),
               showlegend = FALSE) %>%
        config(displayModeBar = F))
    
 observeEvent(click_cat(), output$plot_cat_by_mon <- renderPlotly(cat_by_mon()))

 #barplot. expenses by subcategory for selected category
 
 observeEvent(input$start_m3, {
   updateSelectInput(session, 'start_m1', selected = input$start_m3)
   updateSelectInput(session, 'start_m2', selected = input$start_m3)
 })
 
 observeEvent(input$end_m3, {
   updateSelectInput(session, 'end_m1', selected = input$end_m3)
   updateSelectInput(session, 'end_m2', selected = input$end_m3)
 })
 
 select_subcat <- eventReactive(input$cat, df_main()[To == input$cat, .(Expenses = sum(Amount)), by = .(Tags, sub_color)])
 
 subcat <- reactive(
   plot_ly(select_subcat(), 
           x= ~reorder(Tags, Expenses), y = ~Expenses,
           source = 'subcat', 
           type = 'bar',
           marker = list(color = ~sub_color),
           text = ~paste(c(round(prop.table(select_subcat()$Expenses) *100, 1)), '%', sep = "")) %>%
   layout(title = paste(input$cat, "expenses by subcategory"),
          xaxis = list(title = ""),
          yaxis = list(title = "")) %>%
   config(displayModeBar = F))
 
 output$plot_subcat <- renderPlotly({subcat()})
 
 #plot. expenses by month for selected subcategory
 
 click_subcat <- reactive(event_data("plotly_click", source = 'subcat')) 
 
 subcatMon <- reactive(df_main()[To == input$cat, .(Expenses = sum(Amount)), by = .(Tags, Month, MonY, sub_color)][Tags == click_subcat()$x])
 
 subcat_by_mon <- reactive(
   if (nrow(subcatMon()) > 1) {
     plot_ly(subcatMon(), 
           type = 'scatter',
           x = ~Month, y = ~Expenses,
           mode = 'markers', 
           marker = list(color = ~sub_color)) %>%
   add_trace(name = '',
             x = ~Month, y = ~Expenses,
             mode = 'lines',
             line = list(color = subcatMon()$sub_color[1])) %>%
   layout(title = click_subcat()$x,
          xaxis = list(title = ""),
          yaxis = list(title = ""),
          showlegend = FALSE) %>%
   config(displayModeBar = F)
     } else {
       plot_ly(subcatMon(), 
               type = 'bar',
               x = ~as.character(MonY), y = ~Expenses,
               marker = list(color = ~sub_color)) %>%
         layout(title = click_subcat()$x,
                xaxis = list(title = ""),
                yaxis = list(title = "")) %>%
         config(displayModeBar = F)
     })
 
 observeEvent(click_subcat(), 
                output$plot_subcat_by_mon <- renderPlotly(subcat_by_mon()))
 
}