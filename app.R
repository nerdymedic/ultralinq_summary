library(shiny)
library(shinydashboard)
library(tidyverse)
library(janitor)
library(shinyhelper)

ui <- dashboardPage(
    dashboardHeader(title = "Ultralinq Summary"),
    dashboardSidebar(
        fileInput("file1", "Upload Exam List",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")) %>% 
            helper(type = "markdown",
                   content = "readme",
                   colour = "white"),
        radioButtons("weeks", "Length of Rotation",
                     c("3 Weeks" = 3,
                       "4 Weeks" = 4))
        ),
    dashboardBody(
        fluidRow(plotOutput("plot")),
        fluidRow(tableOutput("table"))
        )
    )


server <- function(input, output) {
    scans_data <- reactive({
        req(input$file1)
        scans <- read_csv(input$file1$datapath, skip = 1) %>% 
            clean_names() %>% 
            select(sonographer, exam_type)
        
        scan_cols <- paste0("scan",seq(1,13,1))
        
        scans <- scans %>% 
            separate(exam_type, c(scan_cols), sep = ",")
        
        scans_long <- scans %>% 
            pivot_longer(scan_cols) %>% 
            drop_na() %>% 
            filter(value != "Generic Procedure") %>% 
            mutate(value = str_trim(value, side = "both"))
        })
    
    summary_data <- reactive({
        scans_data() %>% 
            count(value)
    })

    sonographer_name <- reactive({
        sonographer <- scans_data() %>% 
            pull(sonographer)
        sonographer <- sonographer[1]
        return(sonographer)
        })
    
    breaks <- reactive({
        case_when(
            input$weeks == 3 ~ c(0,8,16,25),
            input$weeks == 4 ~ c(6,12,18,25)
        )
    })

    output$plot <- renderPlot({
        summary_data() %>% 
            ggplot(aes(x = value, y = n)) +
            geom_col() +
            labs(x = "Scan Type", y = "Scan Number", title = "Ultrasound Progress Report", subtitle = paste("for", sonographer_name())) +
            geom_hline(yintercept = 25, linetype = "dotted") +
            scale_y_continuous(breaks = breaks()) +
            coord_flip()
        })
    
    output$table <- renderTable({
        summary_data() %>% 
            rename("Study Type" = value) %>% 
            arrange(desc(n))
    })
    
    observe_helpers(help_dir = "help_dir")
}

shinyApp(ui = ui, server = server)
