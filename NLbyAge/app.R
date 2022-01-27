library("dplyr")
library("stringr")
library("ggplot2")
library("viridisLite")
library("tidyverse")
library("grid")
library("shiny")
library("scales")

load("NLbyAge.RData")

# age groups to render an individual plot for
all_ages = sort(unique(all_weekly_long$age))
all_ages = all_ages[all_ages!="Unknown"]

#
# UI ----
#

ui <- fluidPage(
  
  # tags$head(
  #   tags$style(HTML("
  #           div#plot { min-width: 750px; }
  #           div.col-sm-4 { max-width: 450px; }")
  #   )
  # ),
  
  titlePanel("SARS-CoV-2 / COVID-19 in Nederland per leeftijdsgroep"),
  
  tabsetPanel(
    tabPanel("Plots",
             br(),
             plotOutput("plot", height="600px"),
             uiOutput("plots")
             ),
            
    tabPanel("Tabel overzicht besmettingen",
             br(),
             h4('Vastgestelde besmettingen'),
             HTML("als percentage van de bevolking, per leeftijdsgroep, per maand<br />"),
             br(),
             downloadLink('download_cases_relative_monthly', label = "download CSV"),
             br(),
             br(),
             dataTableOutput("table1")
             ),
    
    tabPanel("Tabel per leeftijdsgroep",
             br(),
             h4('Vastgestelde besmettingen, ziekenhuis- en IC-opname, sterfte'),
             HTML("absolute aantallen, per leeftijdsgroep, per week<br />"),
             br(),
             downloadLink('download_all_weekly_long', label = "download CSV"),
             br(),
             br(),
             HTML('<b>cases</b>: aantal vastgestelde besmettingen<br />'),
             HTML('<b>hosp_not</b>: aantal meldingen van nieuwe ziekenhuisopname (<i>hospital notification</i>)<br />'),
             HTML('<b>ic_not</b>: aantal meldingen van nieuwe IC-opname (<i>ICU notification</i>)<br />'),
             HTML('<b>deaths</b>: aantal overledenen (voor personen tot 50 jaar alleen gemeld onder leeftijdsgroep "<50")'),
             br(),
             br(),
             dataTableOutput("table2")
             ),
    
    tabPanel("Info",
             HTML("<br />
                <h5>Data:</h5>
                besmettingen, overledenen: Osiris AIZ, via <a href='https://data.rivm.nl/meta/srv/dut/catalog.search#/metadata/2c4357c8-76e4-4662-9574-1deb8a73f724'>RIVM</a><br />
                ziekenhuisopnames: NICE, via <a href='https://data.rivm.nl/meta/srv/dut/catalog.search#/metadata/45f911c4-3a62-42f1-b594-524a75db2c94'>RIVM</a><br />
                bevolkingsgrootte: <a href='https://opendata.cbs.nl/statline/#/CBS/nl/dataset/83482NED/table?dl=60959'>CBS</a><br />
                <br />")
             )
            ),
  
  fluidRow(
    column(3,
      HTML("<small>Visualisatie: CW Verhey <a href='https://github.com/cwverhey/COVID-19/tree/master/NLbyAge'>GitHub</a> / <a href='https://twitter.com/casparverhey'>Twitter</a></small>")
    )
  )

)

#
# SERVER ----
#

server <- function(input, output, session) {

  output$plot <- renderPlot({
    
    ggplot(cases_relative_monthly, aes(date, age, fill=percentage)) + 
      geom_tile(colour="gray20", size=1.5, stat="identity") +
      geom_text(aes(label=label, color=percentage), show.legend = F, size = 4.5) + 
      
      scale_fill_viridis_c(option = "inferno", begin=0.05, end=0.85) +
      scale_color_viridis_c(option = "inferno", begin=0.55) +
      labs(title="Vastgestelde besmettingen", subtitle="als percentage van de gehele leeftijdsgroep", x="maand", y="leeftijd") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        text = element_text(size = 20, color="white"),
        
        plot.subtitle = element_text(color="white",hjust=0, margin=margin(5,0,10,0), vjust=1, size=rel(0.8)),
        plot.margin = margin(30,30,30,30),
        plot.background = element_rect(fill="gray20"),
        
        panel.background = element_rect(fill="gray30"),
        #panel.grid.major = element_line(color="black"),
        #panel.grid.minor = element_line(color="gray10"),
        panel.spacing = unit(1, "lines"),
        
        axis.text    = element_text(color="white"),
        axis.text.x  = element_text(angle=45, hjust=1, margin=margin(5,0,10,0)),
        axis.text.y  = element_text(hjust=1, margin=margin(0,5,0,10)),
        
        legend.position = "none"
      )
    
  })
  
  
  output$table1 <- renderDataTable({
    
    cases_relative_monthly %>%
      select(maand=date, leeftijden=age, besmettingen=count, bevolkingsgrootte=population, percentage) %>%
      mutate(percentage=formatC(percentage, digits = 2, format = "f") )

    }, options = list(pageLength = 10, lengthMenu = list(10), searching = FALSE, order = list(list(0, 'desc'), list(1,'asc')) ) )
  
  
  output$download_cases_relative_monthly <- downloadHandler(
      filename = function() { paste('cases_relative_monthly_', format(Sys.time(),"%Y-%m-%d_%H-%M"), '.csv', sep='')  },
      content = function(con) { write.csv(cases_relative_monthly, con) }
      )

  
  output$table2 <- renderDataTable({
    
    all_weekly_long %>%
      select(eerste_dag_van_de_week = first_day_of_week, leeftijden = age, data, value) %>%
      pivot_wider(names_from = data, values_from = value)
    
    }, options = list(pageLength = 11, lengthMenu = list(11), searching = FALSE, order = list(list(0, 'desc'), list(1,'asc')) ) )
  
  
  output$download_all_weekly_long <- downloadHandler(
    filename = function() { paste('all_weekly_', format(Sys.time(),"%Y-%m-%d_%H-%M"), '.csv', sep='')  },
    content = function(con) { write.csv(all_weekly_long, con) }
    )
  
  
  
  output$plots <- renderUI({
    plot_output_list <- lapply(all_ages, function(i) {
      plotname <- paste("plot", i)
      plotOutput(plotname, height = 700)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
    })
  
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  for (i in all_ages) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      print(i)
      my_i <- i
      plotname <- paste("plot", my_i)
      
      output[[plotname]] <- renderPlot({ plotAgegroup(my_i) })
    })
  }
  
}

# format date-axis labels
mklab = function(dates) {
  
  retval = c()
  lastyear = ""
  
  for(i in seq_along(dates)) {
    
    d = dates[i]
    
    if(is.na(d)) {
      retval = c(retval, NA)
      next
    }
    
    # year
    if(format(d,"%Y ") != lastyear)
      str = lastyear = format(d,"%Y ")
    else
      str = ""
    
    # begin date
    str = paste0(str, format(d,"%b %d"), "-")
    
    # end date
    if(format(d,"%m") == format(d+6,"%m"))
      str = paste0(str, format(d+6,"%d"))
    else
      str = paste0(str, format(d+6,"%b %d"))
    
    retval = c(retval, str)
    
  }
  
  return(retval)
  
}


# plot age-specific group
plotAgegroup = function(agegr) {
  
  if(agegr %in% c("0-19","20-29","30-39","40-49"))
    df_plot = filter(all_weekly_long, age == agegr, data != "deaths")
  else
    df_plot = filter(all_weekly_long, age == agegr)
  
  ggplot(df_plot, aes(x=first_day_of_week, y = value)) +
    geom_vline(xintercept = as.Date(cut(Sys.Date(), "week")), color='red3', size=1.25) +
    geom_bar(color="grey50", fill="white", stat="identity") +
    facet_grid(data ~ ., scales = "free_y", labeller = labeller(data = c("cases"="besmet","hosp_not"="opnames zkh","ic_not"="opnames IC","deaths"="overleden"))) +
    labs(title=paste("Leeftijd",agegr), x = "week", y = "aantal") +
    scale_x_date(labels = mklab, date_breaks = "4 weeks", date_minor_breaks = "1 week", expand=c(0,0)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3), labels = label_number(accuracy=1)) +
    theme(
      text = element_text(size = 20, color="white"),
      plot.margin = margin(30,30,30,30),
      plot.background = element_rect(fill="gray20"),
      panel.background = element_rect(fill="gray30"),
      panel.grid.major = element_line(color="black"),
      panel.grid.minor = element_line(color="gray10"),
      axis.text    = element_text(color="white"),
      axis.text.x  = element_text(angle=45, hjust=1, margin=margin(5,0,0,0)),
      axis.text.y  = element_text(hjust=1, margin=margin(0,5,0,0)),
      legend.position = "none",
      panel.spacing = unit(1, "lines")
    )
}

#--------------------------

shinyApp(ui = ui, server = server)
