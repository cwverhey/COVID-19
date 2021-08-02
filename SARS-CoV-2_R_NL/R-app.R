library("dplyr")
library("shiny")
library("shinyWidgets")
library("shinyjs")
library("ggplot2")
library("scales")

# source files need to be in <jsondir>/reproductiegetal_*.json
jsondir = '/Volumes/caspar/RIVM'

# remove cache file to update
cachefile = 'R.app.cache.RData'

Sys.setlocale("LC_TIME","nl_NL.UTF-8")

if(!file.exists(cachefile)) {
  
  # load JSONs retrieved every day from https://data.rivm.nl/meta/srv/dut/catalog.search#/metadata/ed0699d1-c9d5-4436-8517-27eb993eab6e
  file.ls <- list.files(path=jsondir, pattern=glob2rx("reproductiegetal_*.json"), full.names=T)
  
  if(length(file.ls) == 0) stop("Error: no input files!")
  
  # get data
  df <- data.frame()
  lastdf <- 0
  for(f in file.ls) {
    print(f)
    date <- gsub('.*(\\d{4}-\\d{2}-\\d{2}).*', '\\1', f)
    print(date)
    df.temp <- jsonlite::read_json(f, simplifyDataFrame=T)
    df.temp$Rt_low <- as.numeric(df.temp$Rt_low)
    df.temp$Rt_avg <- as.numeric(df.temp$Rt_avg)
    df.temp$Rt_up  <- as.numeric(df.temp$Rt_up)
    df.temp$source <- as.Date(date)
    if(nrow(df.temp) != lastdf) {
      df <- bind_rows(df, df.temp)
      lastdf <- nrow(df.temp)
    }
  }
  rm(f, date, df.temp)
  
  df %>%
    select(source, Date, Rt_low, Rt_avg, Rt_up) %>%
    rename(date=Date) %>%
    mutate(source = as.Date(source), date = as.Date(date)) -> df
  
  print(head(df))
  
  # list unique source / dates
  all.sources = unique(df$source)
  all.dates = unique(df$date)
  
  # disease mitigation dates
  # https://nl.wikipedia.org/wiki/Maatregelen_tijdens_de_coronacrisis_in_Nederland
  mitigation.dates = c("2020-10-14", "2020-11-03", "2020-12-14", "2021-01-20", "2021-04-20", "2021-05-11", "2021-06-05", "2021-06-26", "2021-07-09")
  
  save(df, colors, all.sources, all.dates, mitigation.dates, file="R.app.cache.RData")
  print("saved")

}

load(cachefile)
print("loaded")

#------------------------------


ui <- fluidPage(
  
  useShinyjs(),
  
  titlePanel("SARS-CoV-2 reproductiegetal (R) over tijd, NL"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderTextInput("dates","Weergegeven periode:", choices = all.dates, selected = c(all.dates[length(all.dates)-60], all.dates[length(all.dates)]), dragRange = T, force_edges = T, hide_min_max = T),
      br(),
      
      sliderTextInput("source","Publicatiedatum:", choices = all.sources, selected = all.sources[length(all.sources)], force_edges = T, hide_min_max = T),
      br(),

      checkboxInput("show.latest.avg", "Meest recente puntschatter weergeven", value = F),
      checkboxInput("show.mitigation", "Datums maatregelen weergeven (vanaf okt. 2020)", value = F),
      br(),
      
      h5("Y-as:"),
      checkboxInput("auto.y.lim", "automatisch schalen", value = T),
      disabled(sliderInput("y.lim", "", min = 0, max = 6, value = c(0.5,5.0), step = 0.1, dragRange = T, ticks = T, label = NULL)),
      
      HTML("<br /><small>Data:<br />
            RIVM <a href='https://data.rivm.nl/meta/srv/dut/catalog.search#/metadata/ed0699d1-c9d5-4436-8517-27eb993eab6e'>covid-19 reproductiegetal</a><br />
            Wikipedia <a href='https://nl.wikipedia.org/wiki/Maatregelen_tijdens_de_coronacrisis_in_Nederland'>maatregelen coronacrisis</a><br />
            <br />
            Verwerking:<br />
            CW Verhey</small>")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Grafiek", br(), plotOutput("plot")),
        tabPanel("Toelichting",
                 HTML("<br />
                       <h5>R</h5>
                       De beschikbare publicatiedata hangen af van de weergegeven periode.<br />
                       <br />
                       Nieuwe data komt - op dit moment - beschikbaar op dinsdag en vrijdag. Dit wordt nog niet automatisch in deze pagina opgenomen; het duurt daarom 1-2 dagen voordat deze pagina beschikt over de data van de nieuwste publicatiedatum.<br /> 
                       <br />
                       Bron: RIVM <a href='https://data.rivm.nl/meta/srv/dut/catalog.search#/metadata/ed0699d1-c9d5-4436-8517-27eb993eab6e'>covid-19 reproductiegetal</a> (historische data)<br />
                       <br />
                       <h5>Maatregelen</h5>
                       <li>2020-10-14 begin gedeeltelijke lockdown
                       <li>2020-11-03 verstrenging
                       <li>2020-12-14 verstrenging
                       <li>2021-01-20 ingang avondklok
                       <li>2021-04-20 einde avondklok, versoepelingen (stap 1)
                       <li>2021-05-11 versoepelingen (stap 2)
                       <li>2021-06-05 versoepelingen
                       <li>2021-06-26 versoepelingen (stap 4)
                       <li>2021-07-09 verstrenging<br />
                       <br />
                       (laatst bijgewerkt 18 juli 2021)<br />
                       Bron: Wikipedia <a href='https://nl.wikipedia.org/wiki/Maatregelen_tijdens_de_coronacrisis_in_Nederland'>maatregelen coronacrisis</a><br />
                       <br />
                       <h5>Verwerking</h5>
                       CW Verhey (caspar @ verhey.net) <a href='https://github.com/cwverhey/COVID-19'>GitHub</a>"))
      )
    )
  )
)

#------------------------------

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    colors <- c("puntschatting" = "darkred", "95% CI" = "steelblue", "recentste puntschatting" = "orange")
    
    df.temp <- df %>%
                filter(source == input$source, date >= input$dates[1], date <= input$dates[2])
    
    plot <- df.temp %>%
              ggplot() +
                geom_hline(yintercept=1, linetype="dashed", color="black") + # R = 1 line
                geom_vline(xintercept=as.Date(input$source), linetype="solid", color="pink") # date = source date
    
    if(input$show.mitigation) {
      for(d in mitigation.dates) {
        print(d)
        plot <- plot + geom_vline(xintercept=as.Date(d), linetype="dashed", color="grey")
      }
    }
    
    if(input$show.latest.avg) {
      df.temp.latest <- df %>% filter(source == last(all.sources), date >= input$dates[1], date <= input$dates[2])
      plot <- plot +
        geom_line(data=df.temp.latest, aes(x=date, y=Rt_avg, color="recentste puntschatting"), linetype="dashed") +
        scale_color_manual(values = colors, breaks=c("puntschatting","95% CI", "recentste puntschatting"))
    } else {
      plot <- plot +
        scale_color_manual(values = colors, breaks=c("puntschatting","95% CI"))
    }
    
    if(nrow(df.temp) > 0) {
      plot <- plot +
        geom_line(aes(x=date, y=Rt_avg, color="puntschatting")) +
        geom_line(aes(x=date, y=Rt_low, color="95% CI")) +
        geom_line(aes(x=date, y=Rt_up, color="95% CI"))
    }
    
    plot <- plot +
      labs(x="datum", y="R", color="Legenda") +
      scale_x_date(date_minor_breaks="1 day", labels=date_format("%d %b %Y"), limits=as.Date(input$dates), breaks = scales::pretty_breaks(n=10)) +
      theme_bw() +
      theme(text = element_text(size = 15))
    
    if(!input$auto.y.lim) plot <- plot + coord_cartesian(ylim = c(input$y.lim[1],input$y.lim[2]))
    
    plot

  }, height=600)
  
  observeEvent(input$dates, updateSliderTextInput(session, "source", select = input$source, choices = all.sources[all.sources >= input$dates[1]]))
  
  observeEvent(input$auto.y.lim, { if(input$auto.y.lim) disable("y.lim") else enable("y.lim")})
  
}

#--------------------------

shinyApp(ui = ui, server = server)