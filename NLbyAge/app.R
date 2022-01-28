library("dplyr")
library("stringr")
library("tidyverse")

load("NLbyAge.RData")

# age groups to render an individual plot for
all_ages = c("0-19","20-29","30-39","40-49","<50","50-59","60-69","70-79","80-89","90+")

groupplots = ""
for (agegr in all_ages){
  groupplots = paste(groupplots, paste0("<img src='plots/detail_",make.names(agegr),".svg' width='100%' />"))
}

#
# UI ----
#

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
            h4 {margin-top: 0px; }
            h5 { margin-top: 25px; }")
    )
  ),
  
  titlePanel("SARS-CoV-2 / COVID-19 in Nederland per leeftijdsgroep"),
  
  tabsetPanel(
    tabPanel("Plots",
             HTML("<img src='plots/cases.svg' width='100%' />"),
             HTML(groupplots)
             ),
            
    tabPanel("Info",
             HTML("<br />
                <h4>Info</h4>
                <p><b>Broncode</b>: <a href='https://github.com/cwverhey/COVID-19/'>GitHub</a></p>
                <p><b>Contact</b>: CW Verhey, caspar @ verhey.net</p>
                <p>Data wordt dagelijks opgehaald van onderstaande databronnen. Aangezien sterfte- en opnamecijfers sterk afhankelijk zijn van de leeftijd van
                de patiëntenpopulatie worden de cijfers weergegeven per leeftijdsgroep. Dit geeft een beter overzicht van actuele ontwikkelingen dan een
                overall-weergave.</p>
                <p>Gegevens van de huidige week (aangegeven met een rode lijn) zijn onvolledig (besmettingen, sterfte) of afwezig (opnames).</p>
                <h5>Leeftijdsgroepen</h5>
                Data wordt samengevoegd per leeftijdsgroep van 10 jaar. Wegens overlappende leeftijdsgroepen in de publicaties van Osiris en NICE vormen
                alle patiënten van 0 t/m 19 jaar één groep. Osiris publiceert de sterftecijfers onder 50 jaar als één geheel en niet per decennium, daarom is voorzien in een
                aparte figuur met de cijfers voor 0 t/m 49 jaar.</p>
                <br />
                <h4>Databronnen</h4>
                <h5>Besmettingen en overledenen</h5>
                <p>Bron: Osiris AIZ, via <a href='https://data.rivm.nl/meta/srv/dut/catalog.search#/metadata/2c4357c8-76e4-4662-9574-1deb8a73f724'>RIVM</a></p>
                <p>Elke laboratorium bevestigde COVID-19 patiënt in Nederland, sinds de eerste COVID-19 melding in Nederland op 27/02/2020 (Datum voor
                statistiek kan eerder zijn). Het bestand wordt dagelijks om 16:00 ververst, op basis van de gegevens zoals op 10:00 uur die dag geregistreerd
                staan in het landelijk systeem voor meldingsplichtige infectieziekten (Osiris AIZ).</p>
                <p>Datum voor statistiek: eerste ziektedag, indien niet bekend, datum lab positief, indien niet bekend, melddatum aan GGD.</p>
                <p>Leeftijdsgroep bij leven: 0-9, 10-19, ..., 90+; bij overlijden <50, 50-59, 60-69, 70-79, 80-89, 90+.</p>
                <h5>Ziekenhuis- en IC-opnames</h5>
                <p>Bron: NICE, via <a href='https://data.rivm.nl/meta/srv/dut/catalog.search#/metadata/759f40ae-33b4-4fc1-89d3-c4fa6393622e'>RIVM</a></p>
                <p>De aantallen betreffen COVID-19 ziekenhuis- en IC-opnames sinds de eerste melding in Nederland (27/02/2020) tot en met de meest recente
                complete opnameweek. De registratie van het aantal COVID-19 ziekenhuis- en IC-opnames kan achterlopen. Ziekenhuis- of IC-opnames uit de
                meest recente complete opnameweek kunnen wel in de huidige incomplete week gemeld zijn en zijn dan ook weergegeven in dit bestand. Ziekenhuis-
                en IC-opnames uit de meest recente incomplete week zijn niet opgenomen in dit bestand.</p>
                <p>Een patiënt opgenomen op de IC telt ook mee in de ziekenhuisopname-cijfers. Een patiënt kan meerdere keren in het ziekenhuis of op de IC
                worden opgenomen. In dit open databestand is alleen de eerste opname per patiënt opgenomen.</p>
                <p>Leeftijdsgroepen van vijf jaar worden gehanteerd, met uitzondering van leeftijden onder de 15 jaar (0 - 14) of 90 jaar en hoger
                (90+).</p>
                <h5>Bevolkingsgrootte per leeftijdsgroep</h5>
                <p>Bron: <a href='https://opendata.cbs.nl/statline/#/CBS/nl/dataset/83482NED/table?dl=60959'>CBS</a></p>
                <p>Aan het einde van elke maand worden de voorlopige cijfers per de eerste van die maand gepubliceerd. Tussentijdse bijstellingen van
                voorgaande maanden zijn mogelijk. In het derde kwartaal van elk jaar worden de voorlopige cijfers over het voorgaande jaar vervangen
                door definitieve cijfers.</p>
                <p>In de bevolkingsaantallen zijn uitsluitend personen begrepen die zijn opgenomen in het bevolkingsregister van een Nederlandse gemeente.
                Personen die tot de bevolking van Nederland behoren, maar voor wie geen vaste woonplaats valt aan te wijzen, zijn opgenomen in het
                bevolkingsregister van de gemeente 's-Gravenhage. In de bevolkingsregisters zijn niet opgenomen de in Nederland wonende personen waarvoor
                uitzonderingsregels gelden met betrekking tot opneming in de bevolkingsregisters (bijvoorbeeld diplomaten en NAVO militairen) en personen
                die niet legaal in Nederland verblijven.</p>
                <p><i>Voor de figuur 'vastgestelde besmettingen' wordt per maand de betreffende bevolkingsdata gebruikt. De lopende maand gebruikt de
                bevolkingsdata van de voorgaande maand.</i></p>
                ")
              ),
    
    tabPanel("Tabel procentuele besmettingen",
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
             HTML('<b>hosp</b>: aantal nieuwe ziekenhuisopnames (t/m laatste complete week)<br />'),
             HTML('<b>ic</b>: aantal nieuwe IC-opnames (t/m laatste complete week)<br />'),
             HTML('<b>deaths</b>: aantal overledenen (voor personen tot 50 jaar alleen gemeld onder leeftijdsgroep "<50")'),
             br(),
             br(),
             dataTableOutput("table2")
             )
    
            ),
  
  fluidRow(
    column(3,
      HTML("<small>Visualisatie: CW Verhey <a href='https://github.com/cwverhey/COVID-19/'>GitHub</a> / <a href='https://twitter.com/casparverhey'>Twitter</a></small>")
    )
  )

)

#
# SERVER ----
#

server <- function(input, output, session) {
  
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
  
}

#--------------------------

shinyApp(ui = ui, server = server)
