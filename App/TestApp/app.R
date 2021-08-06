library(tidyverse)
library(readxl)
library(kableExtra)

#setwd("C:/Users/lgaprindashvili/Desktop/IFRS 9/annual_report")
#dt = read_excel('migration.xlsx')
dt = read.csv('https://raw.githubusercontent.com/L-1996-G/Migration/main/data/migration.csv', header = T)
dt = dt %>% mutate_if(is.character, as.factor)
dt= dt%>% mutate_if(is.numeric, round, digit=4)
dt <- dt %>% mutate_each(funs(.*100), starts_with("rate"))
dt <- dt %>% mutate_if(is.numeric, ~round(., 1))


dt$bank = factor(dt$bank, levels = c('TBC', 'BOG','VTB', 'BAS', 'TERA',
                                     'PCB','CARTU','IS','CREDO', 'Halyk',
                                     'PASHA','SILK','ZIR','FINCA'))

dt$STAGE = factor(dt$STAGE, levels = c('ST1_2', 'ST1_3','ST2_3',
                                       'ST2_1','ST3_2','ST3_1'))


# Average-based rate
dt_long_rt = dt %>% select(-c(rate_init, rate_init19,am,am19))%>%
    pivot_longer(c(rate, rate19), "year", values_to = "rate") %>%
    mutate(year=case_when(year=='rate'~"2020", TRUE~"2019"))%>% 
    mutate_if(is.character, as.factor)
# Dec-19-based rate
dt_long_init_rt = dt %>% select(-c(am,am19,rate, rate19))%>%
    pivot_longer(c(rate_init, rate_init19), "year", values_to = "rate")%>%
    mutate(year=case_when(year=='rate_init'~"2020", TRUE~"2019"))%>% 
    mutate_if(is.character, as.factor)


my_plot = function(df, prtfl){
    df %>% filter(portfolio %in% c(prtfl)) %>%
        ggplot(aes(STAGE, rate, fill=STAGE))+
        geom_bar(stat = 'identity', position = 'dodge')+
        facet_grid(year~bank)+
        theme(axis.title = element_text(size = 15, color = "firebrick",
                                        face = "italic"),
              plot.title = element_text(colour = "#7F3D17"),
              plot.subtitle = element_text(color = "blue",
                                           size = 10,
                                           face = 'italic'),
              plot.background = element_rect(fill = "lightgreen"),
              axis.text.x = element_blank())+
        scale_fill_manual("STAGE", values = c('ST1_2' = 'seagreen2', 
                                              'ST2_1' = 'chocolate1',
                                              'ST3_1' = 'red',
                                              'ST3_2'='coral1',
                                              'ST2_3'='seagreen4',
                                              'ST1_3'='darkolivegreen1')) +
        geom_text(aes(label=rate), vjust = .2, angle = -90, position = position_dodge(width=.5), size=2.5, font = 2)
    
}

make_table = function(df, prtf, bnk){
    d = df %>% filter(portfolio %in% c(prtf) & bank %in% c(bnk)) %>%
        select(-c(portfolio, bank, rate_init19, rate_init))
    d %>%
        kbl(caption = paste0(prtf, " migration ", bnk)) %>%
        #kable_minimal(lightable_options = 'hover', font_size =15) %>%
        kable_styling(bootstrap_options = "striped", font_size = 9)%>%
        column_spec(4, color = "white",
                    background = spec_color(d$am, 
                                            begin = .2,
                                            end = .8,
                                            direction = 1,
                                            option = "C")) 
    
}

###
library(shiny)

ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    titlePanel("Stage Migration Analysis"),
        sidebarLayout(
        sidebarPanel(
            selectInput("bank", "Choose Bank", unique(dt_long_rt$bank)),
            tableOutput("table")
        ),
        mainPanel(
            p("Click to change denominator: {mean_based : annual average Portfolio; Jan_based: Portfolio on 01-Jan"),
            actionButton("ave", label = "mean_based"),
            actionButton("init", label = "Jan_based"),
            selectInput("portfolio", "choose Portfolio", unique(dt_long_rt$portfolio)),
            plotOutput("plot")
        )
    )
)

server <- function(input, output, session) {
    my_table <- reactive({
        make_table(dt, input$portfolio, input$bank)
    })
    rv <- reactiveValues(data = dt_long_rt)
    observeEvent(input$ave, {rv$data <- dt_long_rt})
    observeEvent(input$init, {rv$data <- dt_long_init_rt})
    output$text = renderText("Migration within stages")
    output$table = function()my_table()
    output$plot <- renderPlot(my_plot(rv$data, input$portfolio))
    
}


# Run the application 
shinyApp(ui = ui, server = server)
