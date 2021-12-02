library("dplyr")
library("ggplot2")
library("shiny")

load("owid_SA.RData")
load("owid_SA_rmse.RData")

colors = c("simulated (total)" = "blue", "simulated (omicron)" = "red", "simulated (delta)" = "green", "cases (raw)" = 'black', "cases (OWiD smoothed)" = 'darkgreen', "cases (geom_smooth())" = 'grey')

#
# SIMULATION ----
#

sim = function(R, cases, omicron_first_case) {
  
  # daily growth
  r_day = R^(1/5)
  
  # iterate over all days
  for(d in sort(owid_SA$date)) {
    
    # add first patient with Omicron at specified date
    if(d == omicron_first_case) cases['omicron'] = 1
    
    # save total cases for this date
    owid_SA$sim_cases[owid_SA$date == d] = sum(cases)
    owid_SA$sim_delta[owid_SA$date == d] = cases['delta']
    owid_SA$sim_omicron[owid_SA$date == d] = cases['omicron']
    
    # calculate cases for next day
    cases = cases * r_day
    
  }
  
  return(owid_SA)
  
}

rmse = function(v1, v2) {
  sqrt(mean((v1 - v2)^2))
}

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
  
  titlePanel("Omicron growth simulation (South Africa cases)"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4('Delta'),
      sliderInput("Rdelta", label = "R", min = 0.5, max = 1.0, step = 0.01, round = -2, value = 0.75),
      sliderInput("init_cases_delta", label = "initial cases (Sep 1)", min = 6000, max = 14000, step = 10, round = 1, value = 11560),
      br(),
      
      h4('Omicron'),
      sliderInput("Romicron", label = "R", min = 1.0, max = 33.0, step = 0.01, round = -2, value = 3.54),
      sliderInput("first_case_omicron",
                  label="initial case",
                  min = min(owid_SA$date),
                  max = max(owid_SA$date),
                  value=as.Date("2021-10-27"),
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
        tabPanel("Info",
                 HTML("<br />
            <h4>Simulation to estimate R-values for Delta and Omicron</h4>
            <h5>based on total confirmed SARS-CoV-2 cases in South Africa</h5>
            <br />
            <h4>Purpose</h4>
            The simulation shows how uncertain the ratio between R(delta) and R(omicron) is;
            real-world data will fit the model similarly over a vast range of R(omicron) values, as long as
            a suitable date for the first omicron case is chosen. The later the initial omicron case, the higher R(omicron) needs to be.<br />
            <br />
            Default values are a local optimum in RMSE (raw cases vs simulation), given the case data available on December 2nd 2021, 17:50 UTC:<br />
            <br />"),
                 h5("optimal RMSE per day of first omicron case"),
                 plotOutput("plot_rmse"),
                 HTML("<i>calculated on December 2nd 2021, 17:50 UTC</i><br />"),
                 HTML("<br />"),
                 h5("parameters for optimal RMSE per day of first omicron case"),
                 dataTableOutput("table_rmse"),
                 HTML("<i>calculated on December 2nd 2021, 17:50 UTC</i><br />"),
                 HTML("<br />
            <h4>Assumptions</h4>
            In this time frame, R stays constant per variant (there was no change in restrictions in SA);<br />
            Daily growth rate is approximated by R^(1/5).<br />
            <br />
            <h4>Legend</h4>
            <b>cases (raw)</b> reported cases per day (there is a clear day-of-the-week effect);<br />
            <b>cases (OWiD smoothed)</b> 7 day average cases from Our World In Data;<br />
            <b>cases (geom_smooth())</b> LOESS smoothed cases, using ggplot2 function geom_smooth().<br />
            <br />
            <h4>Note</h4>
            Looking at data from September suggests that Delta would have mostly died out by November, which is why
            in the default values in this simulation, over 95% of cases are attributed to Omicron by the end of November. However, this is not in line
            with the results from the 61 SARS-CoV-2-positive passengers who were tested in NL on 26 Nov: only 14 of them
            carried the Omicron variant - though it is unclear if all 61 positive passengers have been sequenced. This might imply that Delta made a revival during the simulated period, that its R increased, and as such that
            the R for Omicron is overestimated in this simulation. On the other hand, the (probably biased) GISAID data (<a href='https://outbreak.info/situation-reports/omicron?selected=ZAF#longitudinal'>outbreak.info</a> or <a href='https://covariants.org/per-country'>covariants.org</a>)
            suggests it's not unlikely that Omicron makes up 95% of cases.<br />
            <br />
            <h4>Credits</h4>
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
    
    # initial cases on simulation day 1 (26 Aug 2021)
    cases = c('delta' = input$init_cases_delta, 'omicron' = 0)
    
    # run sim
    owid_SA = sim(R, cases, input$first_case_omicron)
    
    # calculate correlation
    rmse_raw = rmse(owid_SA$new_cases[!is.na(owid_SA$new_cases)], owid_SA$sim_cases[!is.na(owid_SA$new_cases)])
    rmse_owid_smoothed = rmse(owid_SA$new_cases_smoothed[!is.na(owid_SA$new_cases_smoothed)], owid_SA$sim_cases[!is.na(owid_SA$new_cases_smoothed)])
    
    # plot
    subtitle = paste0("Delta: R=",format(R['delta'],nsmall=2),", ",input$init_cases_delta," cases on Aug 26 ■ Omicron: R=",format(R['omicron'],nsmall=2),", first case on ",format(input$first_case_omicron,"%b %d")," ■ R(omicron)/R(delta)=",round(R['omicron']/R['delta'],digits=1),"\nRMSE(raw cases, simulated)=",round(rmse_raw,3)," ■ RMSE(OWiD smoothed cases, simulated)=",round(rmse_owid_smoothed,3))
    plot <- ggplot(owid_SA, aes(x = date)) +
      geom_smooth(aes(y=new_cases, color="cases (geom_smooth())"), lty=3) +
      geom_line(aes(y=new_cases_smoothed, color="cases (OWiD smoothed)"), lwd=.75) +
      geom_point(aes(y=new_cases, color="cases (raw)"), cex=.75, lwd=0.1) +
      geom_line(aes(y=sim_cases, color="simulated (total)"), lwd=2.5) +
      geom_line(aes(y=sim_delta, color="simulated (delta)"), lwd=1) +
      geom_line(aes(y=sim_omicron, color="simulated (omicron)"), lwd=1) +
      labs(x = 'day', y = 'new cases', title=paste0('South Africa cases per day'), color='', subtitle = subtitle) +
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
    
    # initial cases on simulation day 1 (1 sep 2021)
    cases = c('delta' = input$init_cases_delta, 'omicron' = 0)
    
    # run sim
    owid_SA = sim(R, cases, input$first_case_omicron)
    
    owid_SA
  })
  
  output$plot_rmse <- renderPlot({
    
    plot2 <- ggplot(rmse_df, aes(x=first_omicron)) +
      geom_point(aes(y=RMSE)) + 
      scale_x_date(date_breaks = "1 week", minor_breaks = "1 day", date_labels="%b %d") +
      scale_y_continuous(limits=c(0,NA)) +
      labs(x='initial omicron case', y='optimal RMSE')
    
    print(plot2)
    
  })
  
  output$table_rmse <- renderDataTable(rmse_df, options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE))
  
}

#--------------------------

shinyApp(ui = ui, server = server)
