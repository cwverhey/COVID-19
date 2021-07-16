#
# TODO: kleuren
# TODO: zelf hosten
#

library("shiny")
library("shinyWidgets")
library("ggplot2")
library("dplyr")
library("scales")
library("lubridate")
library("grid")

#--------------------------

# date formatting function
week2dates <- function(weekstr, showYear=FALSE) {
    year = as.numeric(gsub("/(\\d+)$","", weekstr))
    week = as.numeric(gsub("^(\\d+)/","", weekstr))
    date_in    = as.Date((week-1)*7+3, origin = paste0(year,"-01-01"))
    date_start = floor_date(date_in, unit = "week", week_start = 1)
    date_end   = date_start + 6
    if(showYear)
        return(paste(format(date_start,"%d %b %Y"), "-", format(date_end,"%d %b %Y")))
    else
        return(paste(format(date_start,"%d %b"), "-", format(date_end,"%d %b")))
}

#--------------------------

Sys.setlocale("LC_TIME","nl_NL.UTF-8")

load("data.RData")

ui <- fluidPage(
    
    tags$head(
        tags$style(HTML("
            div#plot { min-width: 750px; }
            div.col-sm-4 { max-width: 450px; }")
        )
    ),

    titlePanel("SARS-CoV-2 incidentie per variant, per week (NL)"),

    sidebarLayout(
        
        sidebarPanel(
            
            strong("Varianten:"),
            checkboxInput('variantsToggle', HTML('<b>Alles Aan/Uit</b>')),
            checkboxGroupInput("variants", NULL, choices = all_variants, selected = default_selected_variants),
            br(),
            
            sliderTextInput("weeks","Periode (jaar/week):", choices = all_weeks, selected = c(all_weeks[1], all_weeks[length(all_weeks)]), dragRange = T, force_edges = T, hide_min_max = T),
            br(),
            
            checkboxInput("showTotal", "Totaal aantal positief geteste mensen", value = F),
            checkboxInput("week2date", "Datums ipv weeknummer", value = T),
            checkboxInput("logscale", "Logaritmische schaal", value = F),
            br(),
            
            downloadButton('download', 'Download dataset als CSV'),
            br(), br(),
            
            HTML("<small>Data: RIVM <a href='https://www.rivm.nl/coronavirus-covid-19/virus/varianten'>kiemsurveillance</a> en <a href='https://data.rivm.nl/geonetwork/srv/dut/catalog.search#/metadata/1c0fcd57-1102-4620-9cfa-441e93ea5604?tab=general'>infectierapportage</a><br />
                  Verwerking: CW Verhey</small>")
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Grafiek", br(), br(), plotOutput("plot")),
                tabPanel("Data",
                        br(),
                        h5("codeboek"),
                        HTML("<ul><li /><b>value</b>: aantal personen in de kiemsurveillance-steekproef met deze variant<br/>"),
                        HTML("<li /><b>samplesize</b>: totaal aantal (succesvol verwerkte) samples in de steekproef<br/>"),
                        HTML("<li /><b>percentage</b>: fractie van alle steekproeven met deze variant (0..1)<br/>"),
                        HTML("<li /><b>cases</b>: totaal aantal nieuwe positieve tests in heel NL<br/>"),
                        HTML("<li /><b>estimate</b>: puntschatting van het totaal aantal nieuwe cases dat deze variant had (<i>cases</i> × <i>percentage</i>)<br/>"),
                        HTML("<li /><b>95low/95high</b>: 95%-betrouwbaarheidsinterval voor <i>estimate</i> (o.b.v. binomiale test van <i>value</i> en <i>samplesize</i>)</ul><br/>"),
                        dataTableOutput("table")
                        ),
                tabPanel("Toelichting",
                         HTML("<br /><br />
                         Deze dynamische grafiek toont een schatting van het aantal positief geteste mensen per week, per SARS-CoV-2-variant. Dit is gebaseerd op het totaal aantal positieve tests per week, verdeeld over de varianten volgens de verdeling uit de kiemsurveillance van het RIVM.<br />
                         <br />
                         <b>Doel</b><br />
                         Op basis van deze grafieken is direct te zien hoe de groei/afname van elke variant verloopt.<br />
                         <br />
                         Dit is niet zichtbaar in de ruwe aantallen uit de kiemsurveillance van het RIVM, aangezien er geen vaste verhouding is tussen het totaal aantal cases per week en het aantal steekproefmonsters.<br/>
                         <br />
                         Ook het procentuele aandeel van elke variant (in de media vaak gerapporteerd) biedt hierin slecht inzicht: het relatieve aandeel van een variant kan zelfs toenemen terwijl het absolute aantal infecties van die variant afneemt (en andersom).<br />
                         <br />
                         <b>Methode</b><br />
                         De puntschatter van het aantal nieuwe cases per variant is gebaseerd op het relatieve aandeel van elke variant in de wekelijkse kiemsurveillance-steekproef, geëxtrapoleerd naar het totaal aantal positieve besmettingen per week.<br />
                         <br />
                         Het daaromheen weergegeven 95%-betrouwbaarheidsinterval is bepaald per week, per variant, door binomiaal toetsen van het aantal cases t.o.v. de steekproefgrootte van die week. Dit geeft weer wat de (on)betrouwbaarheid van de beperkte steekproefgrootte is.<br />
                         <br />
                         Overige oorzaken van meetafwijkingen zijn niet gekwantificeerd, zoals de theoretisch mogelijke verschillen in test-bereidheid tussen mensen met verschillende varianten (ascertainment bias).<br />
                         <br />
                         Het is ook belangrijk op te merken dat deze cijfers alleen de geteste personen betreffen, en dus niet alle geïnfecteerden. Het RIVM maakt voorlopig ook <a href='https://data.rivm.nl/meta/srv/dut/catalog.search#/metadata/097155aa-75eb-4caa-8ed3-4c6edb80467e'>geen schattingen meer over het aantal besmettelijke personen</a>.<br />
                         <br />
                         Databronnen: RIVM open data <a href='https://www.rivm.nl/coronavirus-covid-19/virus/varianten'>kiemsurveillance</a> en <a href='https://data.rivm.nl/geonetwork/srv/dut/catalog.search#/metadata/1c0fcd57-1102-4620-9cfa-441e93ea5604?tab=general'>infectierapportage</a><br />
                         Verwerking: CW Verhey (caspar @ verhey.net) <a href='https://github.com/cwverhey/COVID-19/tree/master/SARS-CoV-2_variantsNL'>(broncode op GitHub)</a>"))
            ))
    )
)

#--------------------------

server <- function(input, output, session) {
    
    observeEvent(input$variantsToggle, {
        updateCheckboxGroupInput(
            session, 'variants', choices = all_variants,
            selected = if (input$variantsToggle) all_variants
        )
    },  ignoreInit = T)
    
    output$plot <- renderPlot({
        
        # require selected variants to plot
        req(input$variants)
        
        # assign colors to display
        scale.colors <- colors[c(input$variants)]
        if(input$showTotal) scale.colors <- c(scale.colors, "Totaal"= "#666666")
        
        # set x-range
        x.range <- all_weeks[all_weeks >= input$weeks[1] & all_weeks <= input$weeks[2]]
        print(x.range)
        
        # filter which variants to display
        data_temp <- data %>% filter(variant %in% input$variants, week %in% x.range)
        
        # plot variants
        plot <- ggplot(data_temp, aes(x=week, y=estimate, group=variant, colour=variant)) +
                    scale_x_discrete(limits=x.range) +
                    geom_point(size=1.25) +
                    geom_line() +
                    labs(y = "geschat aantal nieuwe cases (met 95% CI)") +
                    geom_ribbon(aes(ymin=`95low`, ymax=`95high`, fill=variant), linetype=0, alpha=.4) +
                    theme_light() +
                    theme(
                        text = element_text(size = 15),
                        axis.text.x = element_text(angle = 45, hjust=1, vjust=1, margin = margin(t = 5, r = 0, b = 10, l = 0)),
                        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
                        ) +
                    scale_y_continuous(breaks=pretty_breaks(n=10), limits = c(0, NA)) +
                    annotation_custom(grobTree(textGrob(paste("laatste data-update:",lastupdate), x=0.995,  y=0.99, just=c("right","top"), gp=gpar(fontsize=10)))) +
                    scale_fill_manual(values=scale.colors) + scale_color_manual(values=scale.colors)

        # plot options
        if(input$logscale) plot <- plot + scale_y_continuous(trans='log10') + annotation_logticks(sides = "l")
        
        if(input$showTotal) {
            data_temp <- data %>% distinct(week, cases) %>% filter(week %in% x.range) %>% mutate(variant="Totaal")
            plot <- plot + geom_line(data = data_temp, aes(x = week, y = cases), linetype=1, alpha=0.5)
        }
        
        if(input$week2date) plot <- plot + scale_x_discrete(limits=x.range, labels=week2dates(x.range))
        
        print(plot)
            
    }, height=700)
    
    output$table <- renderDataTable({
        data_temp <- data %>% filter(variant %in% input$variants, week >= input$weeks[1], week <= input$weeks[2])
        if(input$week2date) data_temp$week <- week2dates(data_temp$week,showYear=T)
        if(!input$showTotal) data_temp$cases <- NULL
        data_temp
    })
    
    output$download <- downloadHandler(
        filename = function() { paste('data-', Sys.Date(), '.csv', sep='') },
        content = function(con) { write.csv2(data, con) }
    )
}

#--------------------------

shinyApp(ui = ui, server = server)
