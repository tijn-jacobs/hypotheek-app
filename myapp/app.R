# Install required packages if not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")

# Laad de shiny bibliotheek
library(shiny)
library(ggplot2)

# Definieer de leningberekeningen functie
lening_berekeningen <- function(P, annual_interest_rate, years, marginaal_belastingtarief, woz_waarde, eigenwoningforfait_percentage, B_vrij_gift) {
  r <- annual_interest_rate / 12  # Maandelijkse rentevoet
  n <- years * 12  # Totaal aantal betalingen
  
  eigenwoningforfait <- woz_waarde * eigenwoningforfait_percentage
  
  A_ann <- (P * r * (1 + r)^n) / ((1 + r)^n - 1)
  totale_kosten_ann <- A_ann * n
  maandelijkse_lasten <- round(A_ann, 2)
  jaarlijkse_kosten <- round(A_ann * 12, 2)
  
  jaar <- c()
  rentebetaling_ann <- c()
  aflossing_ann <- c()
  fiscaal_voordeel_ann <- c()
  fiscaal_voordeel_cum_ann <- c()
  hoofd_som_ann <- c()
  spaarpot <- c()
  netto_voordeel_strategie_1 <- c()
  
  current_principal_ann <- P
  spaarpot_waarde <- 0
  
  for (i in 1:n) {
    interest_payment_ann <- round(current_principal_ann * r, 2)
    principal_payment_ann <- round(A_ann - interest_payment_ann, 2)
    current_principal_ann <- current_principal_ann - principal_payment_ann
    
    if (i %% 12 == 0) {
      jaar <- c(jaar, i / 12)
      
      rentebetaling_ann <- c(rentebetaling_ann, interest_payment_ann * 12)
      aflossing_ann <- c(aflossing_ann, principal_payment_ann * 12)
      fiscaal_voordeel_ann <- c(fiscaal_voordeel_ann, round((interest_payment_ann * 12 - eigenwoningforfait) * marginaal_belastingtarief, 2))
      cumulatieve_voordeel <- sum(fiscaal_voordeel_ann)
      fiscaal_voordeel_cum_ann <- c(fiscaal_voordeel_cum_ann, paste(round(cumulatieve_voordeel / 1000, 1), "k"))
      hoofd_som_ann <- c(hoofd_som_ann, round(current_principal_ann / 1000, 1))
      
      spaarpot_waarde <- spaarpot_waarde + B_vrij_gift - interest_payment_ann * 12
      spaarpot_k <- paste(round(spaarpot_waarde / 1000, 1), "k")
      spaarpot <- c(spaarpot, spaarpot_k)
      
      rest_lening_k <- round(current_principal_ann / 1000, 1)
      netto_voordeel <- cumulatieve_voordeel - (rest_lening_k * 1000 - spaarpot_waarde) / 10
      netto_voordeel_strategie_1 <- c(netto_voordeel_strategie_1, paste(round(netto_voordeel / 1000, 1), "k"))
    }
  }
  
  totaal_fiscaal_voordeel_ann <- sum(fiscaal_voordeel_ann)
  
  resultaten_ann <- data.frame(
    Jaar = jaar,
    Rentebetaling = rentebetaling_ann,
    Aflossing = aflossing_ann,
    Fiscaal_Voordeel = fiscaal_voordeel_ann,
    Cumulatieve_Voordeel = fiscaal_voordeel_cum_ann,
    Rest_Lening = paste(hoofd_som_ann, "k"),
    Spaarpot = spaarpot,
    Netto_voordeel_strategie_1 = netto_voordeel_strategie_1
  )
  
  info <- data.frame(
    Item = c(
      "Lening informatie",
      paste("Initiele lening:", format(P, big.mark = ".", decimal.mark = ",")),
      paste("Rente:", annual_interest_rate * 100, "%"),
      "",
      "Maandelijkse Lasten",
      paste("Maandelijkse Lasten:", format(maandelijkse_lasten, big.mark = ".", decimal.mark = ",")),
      "Jaarlijkse Kosten",
      paste("Jaarlijkse Kosten:", format(jaarlijkse_kosten, big.mark = ".", decimal.mark = ",")),
      "",
      "Annuïteit lening",
      paste("Totale kosten over de gehele looptijd:", format(round(totale_kosten_ann, 2), big.mark = ".", decimal.mark = ",")),
      paste("Totale fiscale voordeel voor annuïteit lening:", format(round(totaal_fiscaal_voordeel_ann, 2), big.mark = ".", decimal.mark = ",")),
      "",
      "Spaarpot informatie",
      paste("Beginwaarde spaarpot:", format(B_vrij_gift, big.mark = ".", decimal.mark = ",")),
      paste("Totale waarde van de spaarpot aan het einde van de looptijd:", format(round(spaarpot_waarde, 2), big.mark = ".", decimal.mark = ","))
    )
  )
  
  list(info = info, resultaten = resultaten_ann)
}

# UI
ui <- fluidPage(
  titlePanel("Hypotheekrente Aftrek Berekening"),
  fluidRow(
    column(12,
           p("Deze app berekent de financiële voordelen van een annuïteitenhypotheek met hypotheekrenteaftrek en een jaarlijkse spaarpot. Jaarlijks zal de betaalde rente teruggeschonken worden. Het restant dat belastingvrij geschonken kan worden zal tevens geschonken worden en worden gespaard in de spaarpot. De lening kan worden afgelost door deze spaarpot. Het restant zal dan worden geschonken tegen 10% schenkbelasting. Het netto voordeel van deze strategie wordt uitgerekend."),
           br(),
           p("Voer de volgende parameters in om de berekeningen te zien:"),
           p("1. Hoofdsom: Het initiële bedrag van de lening."),
           p("2. Jaarlijkse rentevoet (%): Het jaarlijkse rentepercentage van de lening."),
           p("3. Looptijd (jaren): De looptijd van de lening in jaren."),
           p("4. Marginaal belastingtarief (%): Het marginale belastingtarief dat van toepassing is op uw inkomen."),
           p("5. WOZ-waarde van de woning: De WOZ-waarde van uw woning."),
           p("6. Eigenwoningforfait (%): Het eigenwoningforfaitpercentage op basis van de WOZ-waarde. Deze kan de factor op nul gezet worden."),
           p("7. Belastingvrijstelling schenking: Het bedrag dat jaarlijks belastingvrij geschonken mag worden."),
           br(),
           p("De app geeft de volgende waarden:"),
           p("1. Rentebetaling: De totale rente betaald per jaar."),
           p("2. Aflossing: Het bedrag dat per jaar op de lening wordt afgelost."),
           p("3. Fiscaal Voordeel: Het belastingvoordeel per jaar als gevolg van de hypotheekrenteaftrek."),
           p("4. Cumulatieve Voordeel: Het cumulatieve belastingvoordeel tot en met dat jaar."),
           p("5. Rest Lening: Het resterende bedrag van de lening aan het einde van elk jaar."),
           p("6. Spaarpot: Het opgebouwde bedrag in de spaarpot aan het einde van elk jaar."),
           p("7. Netto Voordeel Strategie 1: Het netto voordeel berekend als Cumulatieve Voordeel - (Rest Lening - Spaarpot) / 10.")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      numericInput("lening", "Hoofdsom:", 60000),
      numericInput("rente", "Jaarlijkse rentevoet (%):", 7),
      numericInput("looptijd", "Looptijd (jaren):", 30),
      numericInput("belastingtarief", "Marginaal belastingtarief (%):", 36.97),
      numericInput("wozwaarde", "WOZ-waarde van de woning:", 232000),
      numericInput("eigenwoningforfait", "Eigenwoningforfait (%):", 0),
      numericInput("vrij_gift", "Belastingvrijstelling schenking:", 6633),
      actionButton("bereken", "Bereken")
    ),
    mainPanel(
      verbatimTextOutput("info"),
      tableOutput("resultaten"),
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$bereken, {
    P <- input$lening
    annual_interest_rate <- input$rente / 100
    years <- input$looptijd
    marginaal_belastingtarief <- input$belastingtarief / 100
    woz_waarde <- input$wozwaarde
    eigenwoningforfait_percentage <- input$eigenwoningforfait / 100
    B_vrij_gift <- input$vrij_gift
    
    resultaten <- lening_berekeningen(P, annual_interest_rate, years, marginaal_belastingtarief, woz_waarde, eigenwoningforfait_percentage, B_vrij_gift)
    
    output$info <- renderPrint({
      cat(paste(resultaten$info$Item, collapse = "\n"))
    })
    
    output$resultaten <- renderTable({
      resultaten$resultaten
    })
    
    output$plot <- renderPlot({
      ggplot(resultaten$resultaten, aes(x = Jaar)) +
        geom_line(aes(y = as.numeric(gsub("k", "", Cumulatieve_Voordeel)) * 1000, color = "Cumulatieve Voordeel")) +
        geom_line(aes(y = as.numeric(gsub("k", "", Spaarpot)) * 1000, color = "Spaarpot")) +
        geom_line(aes(y = as.numeric(gsub("k", "", Netto_voordeel_strategie_1)) * 1000, color = "Netto Voordeel Strategie 1")) +
        labs(y = "Bedrag in Euro's", color = "Legend") +
        theme_minimal()
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
