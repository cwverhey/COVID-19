library("dplyr")
library("ggplot2")
library("shiny")

load("omicron_SA.RData")

colors = c("simulated (total)" = "blue", "simulated (omicron)" = "red", "simulated (delta)" = "green", "cases (raw)" = 'black', "cases (OWiD smoothed)" = 'darkgreen', "cases (geom_smooth())" = 'grey')

#
# SIMULATION ----
#

# load functions
source('functions.R')

#
# UI ----
#

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
            div#plot { min-width: 750px; }
            div.col-sm-4 { max-width: 450px; }")
    )
  ),
  
  titlePanel("Omicron growth simulation (South African cases)"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4('Delta'),
      sliderInput("Rdelta", label = "R", min = 0.5, max = 1.0, step = 0.01, round = -2, value = optimal$R.delta),
      sliderInput("init_cases_delta", label = paste("cases on",format(min(owid_SA$date),"%b %d")), min = 5000, max = 15000, step = 10, round = 1, value = optimal$delta_cases),
      br(),
      
      h4('Omicron'),
      sliderInput("Romicron", label = "R", min = 1.0, max = 20.0, step = 0.01, round = -2, value = optimal$R.omicron),
      sliderInput("first_case_omicron",
                  label="initial case",
                  min = min(owid_SA$date),
                  max = as.Date("2021-11-16"),
                  value=optimal$first_omicron,
                  timeFormat="%b %d"),
      br(),
      
      h4('Plot'),
      checkboxInput("logscale", "Logaritmic Y-axis", value = F),
      br(),
      
      HTML("<small>Simulation: CW Verhey <a href='https://github.com/cwverhey/COVID-19/tree/master/omicron_SA'>GitHub</a> / <a href='https://twitter.com/casparverhey'>Twitter</a></small>")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", br(), plotOutput("plot")),
        tabPanel("Table",
                 br(),
                 dataTableOutput("table")
        ),
        tabPanel("Optimal fit",
                 HTML('<br />
                      <h4>Daily optimal fit</h4>
                      To maximize the fit of the model with case data, (locally) optimal parameter values are recalculated
                      daily, every time the amount of cases is updated.<br />
                      <br />
                      First, the locally optimal values are calculated for each possible date that the first Omicron case
                      could have been detected as SARS-CoV-2 positive. Then, the parameters for the best detected fit are selected
                      as the default date in the app.
                      <br />
                      At the time of writing (6 December), this simulation shows how uncertain the R of Omicron is;
                      real-world cases will fit the model similarly over a vast range of R(omicron) values, as long as
                      a suitable date for the first Omicron case is chosen. The later the initial Omicron case, the higher R(omicron) needs to be.<br />
                      <br />'),
                 h5("optimal RMSE per day of first Omicron case"),
                 plotOutput("plot_rmse"),
                 HTML("<br />"),
                 h5("parameters for optimal RMSE per day of first Omicron case"),
                 dataTableOutput("table_rmse")
                 ),
        tabPanel("Info",
                 HTML("<br />
            <h4>Simulation app to estimate R-values for Delta and Omicron</h4>
            <h5>based on total confirmed SARS-CoV-2 cases in South Africa</h5>
            <br />
            <h5>Purpose</h5>
            The app simulates how many cases would be expected each day, given values for R(delta),
            R(omicron), and the date of the initial Omicron case. It also illustrates and quantifies
            the fit with real world case data. This allows to get an estimation of the current R values,
            as well as a grasp of their (un)reliabilities.<br />
            <br />
            The best fitting parameter values are calculated automatically each day, and set as the
            default values when loading the app. They are also visible on the 'Optimal fit' tab.<br />
            <br />
            <h5>Assumptions</h5>
            In this time frame, R stays constant per variant;<br />
            In this time frame, the ascertainment rate stays constant (infections remain as likely to become a confirmed case);<br />
            Daily growth rate is approximated by R^(1/5).<br />
            <br />
            <h5>Legend</h5>
            <b>cases (raw)</b> reported cases per day (there is a clear day-of-the-week effect);<br />
            <b>cases (OWiD smoothed)</b> 7 day average cases from Our World In Data;<br />
            <b>cases (geom_smooth())</b> LOESS smoothed cases, using ggplot2 function geom_smooth().<br />
            <br />
            <h5>Note</h5>
            Looking at data from September suggests that Delta would have mostly died out by November, which is why
            in the default values in this simulation, over 95% of cases are attributed to Omicron by the end of November. However, this is not in line
            with the results from the 61 SARS-CoV-2-positive passengers who were tested in NL on 26 Nov: only 18 of them
            carried the Omicron variant - though it not fully clear if all 61 positive passengers have been sequenced. This suggests that Delta was still
            prevalent during the simulated period, that its R increased, and as such that the R for Omicron is overestimated in this simulation (and all others I've seen).
            On the other hand, the (probably biased) GISAID data (<a href='https://outbreak.info/situation-reports/omicron?selected=ZAF#longitudinal'>outbreak.info</a> or <a href='https://covariants.org/per-country'>covariants.org</a>)
            suggests it's not unlikely that Omicron makes up 95% of cases.<br />
            <br />
            <h5>Credits</h5>
            Simulation/UI: CW Verhey (caspar @ verhey.net) <a href='https://github.com/cwverhey/COVID-19/tree/master/omicron_SA'>source: GitHub</a> / <a href='https://twitter.com/casparverhey'>Twitter</a><br />
            Case data: <a href='https://ourworldindata.org/covid-cases'>Our World In Data</a><br />
            <br />"))
      ))
  )
)

#
# SERVER ----
#

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    # growth rate per day, per variant (R ≈ daily r ^ 5)
    R = c('delta' = input$Rdelta, 'omicron' = input$Romicron)
    
    # run sim
    df = sim(owid_SA, R, input$init_cases_delta, input$first_case_omicron)
    
    # calculate correlation
    rmse_raw = rmse(df$new_cases[!is.na(df$new_cases)], df$sim_cases[!is.na(df$new_cases)])
    rmse_owid_smoothed = rmse(df$new_cases_smoothed[!is.na(df$new_cases_smoothed)], df$sim_cases[!is.na(df$new_cases_smoothed)])
    
    # plot
    subtitle = paste0("Delta: R=",format(R['delta'],nsmall=2),", ",input$init_cases_delta," cases on Aug 26 ■ Omicron: R=",format(R['omicron'],nsmall=2),", first case on ",format(input$first_case_omicron,"%b %d")," ■ R(omicron)/R(delta)=",round(R['omicron']/R['delta'],digits=1),"\nRMSE(raw cases, simulated)=",round(rmse_raw,3)," ■ RMSE(OWiD smoothed cases, simulated)=",round(rmse_owid_smoothed,3))
    plot <- ggplot(df, aes(x = date)) +
      geom_smooth(aes(y=new_cases, color="cases (geom_smooth())"), lty=3) +
      geom_line(aes(y=new_cases_smoothed, color="cases (OWiD smoothed)"), lwd=.75) +
      geom_line(aes(y=sim_cases, color="simulated (total)"), lwd=2.5) +
      geom_line(aes(y=sim_delta, color="simulated (delta)"), lwd=1) +
      geom_line(aes(y=sim_omicron, color="simulated (omicron)"), lwd=1) +
      geom_point(aes(y=new_cases, color="cases (raw)"), cex=.75, lwd=0.1) +
      labs(x = 'day', y = 'new cases', title=paste0('South African cases per day'), color='', subtitle = subtitle) +
      scale_color_manual(values = colors) +
      scale_x_date(date_breaks = "1 week", minor_breaks = "1 day", date_labels="%b %d") +
      theme(text = element_text(size = 15), plot.title = element_text(face="bold"), plot.subtitle = element_text(lineheight=1.2, vjust=-0.5))
    
    # plot options
    if(input$logscale) plot <- plot + scale_y_continuous(trans='log10', limits=c(100,NA)) + annotation_logticks(sides = "l")
    
    print(plot)
    
  }, height=700)
  
  output$table <- renderDataTable({
    
    # growth rate per day, per variant (R ≈ daily r ^ 5)
    R = c('delta' = input$Rdelta, 'omicron' = input$Romicron)
    
    # run sim
    sim(owid_SA, R, input$init_cases_delta, input$first_case_omicron)

  })
  
  output$plot_rmse <- renderPlot({
    
    ggplot(rmse_df, aes(x=first_omicron)) +
      geom_point(aes(y=RMSE)) + 
      scale_x_date(date_breaks = "1 week", minor_breaks = "1 day", date_labels="%b %d") +
      scale_y_continuous(limits=c(0,NA)) +
      labs(x='initial omicron case', y='optimal RMSE')
    
  })
  
  output$table_rmse <- renderDataTable(rmse_df, options = list(paging = FALSE, searching = FALSE))
  
}

#--------------------------

shinyApp(ui = ui, server = server)
