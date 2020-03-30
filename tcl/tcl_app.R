
library(shiny)
library(ggplot2)
library(dplyr)


ui <- fluidPage(

    # Application title
    titlePanel("Demostración de Teorema Central del Límite"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Tamaño de cada muestra uniforme:",
                        min = 1,
                        max = 100,
                        value = 1),
            numericInput("min",
                         "Límite inferior de la distribución",
                         value = 0),
            numericInput("max",
                         "Límite superior de la distribución",
                         value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("hist")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    muestras <- reactive({
        validate(need(input$max > input$min, "El límite superior debe de ser mayor al inferior"))
        map(1:1000, ~ runif(input$n,min = input$min,max = input$max))
        })
    
    muestras_hist <- reactive(map_dbl(muestras(),~ mean(.x)) %>% data.frame(val = .))
    

    output$hist <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        ggplot(muestras_hist())+
            geom_histogram(aes(x = val, y = ..density..), binwidth = 0.005, col = "white")+
            geom_density(aes(x = val),col = "blue", size = 1, alpha = .8)+
            xlim(c(input$min,input$max))+
            theme_bw()+
            xlab("VA")+
            ylab("Densidad")+
            ggtitle(paste0("Histograma de 1000 réplicas de muestras de ",input$n," observacion/es"))

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
