library(tidyverse)
library(readxl)
library(kableExtra)
library(bslib)
#setwd("C:/Users/lgaprindashvili/Desktop/IFRS 9/annual_report")
#dt = read_excel('migration.xlsx')
dt = read.csv('https://raw.githubusercontent.com/L-1996-G/Migration/main/migration.csv', header = T)
dt = dt %>% mutate_if(is.character, as.factor)
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

rate_table = function(b, p){
    dd <- dt %>%
        separate(STAGE, into = c('ST_start', 'ST_end'), sep = "_") %>%
        #mutate(rate = rate*100) %>%
        mutate(ST_end = paste0("ST",ST_end))%>%
        filter(bank %in% c(b) & portfolio %in% (p))%>%
        arrange(ST_start, ST_end) %>%
        select( ST_start, ST_end, rate)%>%
        mutate(ST_start = factor(ST_start, levels = c('ST1', 'ST2', 'ST3')),
               ST_end = factor(ST_end, levels = c('ST1', 'ST2', 'ST3')))%>%
        pivot_wider(names_from = ST_end, values_from = rate) %>%
        select(ST_start,ST1, ST2,ST3)
    #order(ST_start) %>%
    dd[is.na(dd)] <- 0
    dd %>% mutate_if(is.numeric, round, digit=1)
    row_name= dd$ST_start
    dd$ST_start = NULL
    row.names(dd) <- row_name
    dd  %>%
        kbl()%>%
        kable_styling(bootstrap_options = "striped", full_width = F, position = "left")%>%
        column_spec(1, bold = T, color = 'black', background = "whight")

}


### data preprocessing for reserve analysis
df = read.csv('https://raw.githubusercontent.com/L-1996-G/Migration/main/reserve.csv', header = T)
df = df %>% mutate_if(is.character, as.factor)
df <- df %>% mutate_each(funs(.*100), c(ST1, ST2, ST3, POCI, Total))
df <- df %>% mutate_if(is.numeric, ~round(., 2))

df$Bank = factor(df$Bank, levels = c('TBC', 'BOG', 'LIB','VTB', 'BAS', 'TERA',
                                     'PCB','CARTU','IS','CREDO', 'HALYK',
                                     'PASHA','SILK','ZIR','FINCA'))

df_long_sys =  df %>% filter(Bank %in% c('BOG', 'TBC', 'LIB'))%>%
    pivot_longer(c(ST1, ST2, ST3), "Stage", values_to = "reserve")

df_long =  df %>%
    pivot_longer(c(ST1, ST2, ST3, Total), "Stage", values_to = "reserve")
df$Date=as.factor(df$Date)

## create functions for reserve analysis
res_plot = function(data, prtf){
    data %>% filter(Portfolio %in% c(prtf)) %>%
        ggplot(aes(Stage, reserve, fill=Stage))+
        geom_bar(stat = 'identity', position = 'dodge')+
        facet_grid(Date~Bank)+
        ggtitle(paste0("Reserve for ", prtf, " Portfolio")
        )+
        theme(axis.title = element_text(size = 15, color = "firebrick",
                                        face = "italic"),
              plot.title = element_text(colour = "#7F3D17"),
              plot.subtitle = element_text(color = "blue",
                                           size = 10,
                                           face = 'italic'))+
        scale_fill_manual("STAGE", values = c("ST1" = "orange", "ST2" = "blue",
                                              "ST3" = "red", 'Total'='green'))+
        theme(plot.background = element_rect(fill = "lightgreen"),
              axis.text.x = element_blank())+
        geom_text(aes(label=reserve), vjust = -.2, position = position_dodge(width=1), size=2.5)
}

##
res_table = function(df, prtf, bank){
    df %>% filter(Portfolio %in% c(prtf) & Bank %in% c(bank)) %>%
        select(Bank, Date, AM_ST1, AM_ST2, AM_ST3, Total_AM) %>%
        pivot_longer(c(AM_ST1, AM_ST2, AM_ST3, Total_AM), "Stage", values_to = "LOAN_AM" )%>%
        pivot_wider(c(Stage, Date), names_from = 'Bank', values_from = "LOAN_AM" )%>%
        kbl(caption = paste0(prtf, " Loan Amount by Stages")) %>%
        kable_styling(bootstrap_options = "striped", font_size = 9)%>%
        row_spec(4, bold = T, color = "white", background = "#D7261E")%>%
        row_spec(8, bold = T, color = "white", background = "#D7261E")

}


### Portfolio distrbution across stages
df_st <-read.csv('https://raw.githubusercontent.com/L-1996-G/Migration/main/stage_proportion.csv', header = T)
df_st_l =df_st %>% pivot_longer(c(ST1, ST2, ST3,POCI), names_to = "STAGE", values_to = "st_prop")
df_st_l = df_st_l %>% mutate_if(is.character, as.factor)
df_st_l$Bank = factor(df_st_l$Bank, levels = c('TBC', 'BOG','LIB', 'VTB', 'BAS', 'TERA',
                                     'PCB','CARTU','IS','CREDO', 'Halyk',
                                     'PASHA','SILK','ZIR','FINCA'))

df_st_l$STAGE = factor(df_st_l$STAGE, levels = c('ST1', 'ST2',
                                                 'ST3', 'POCI'))
df_st_l$st_prop = round(df_st_l$st_prop*100, 1)

st_plot = function(df, prtfl){
    df %>% filter(Portfolio %in% c(prtfl)) %>%
        ggplot(aes(STAGE, st_prop, fill=STAGE))+
        geom_bar(stat = 'identity', position = 'dodge')+
        facet_grid(Date~Bank)+
        ggtitle(paste0("Stage Distribution in ", prtfl, " Portfolio"))+
        theme(axis.title = element_text(size = 15, color = "firebrick",
                                        face = "italic"),
              plot.title = element_text(colour = "#7F3D17"),
              plot.subtitle = element_text(color = "blue",
                                           size = 10,
                                           face = 'italic'),
              plot.background = element_rect(fill = "lightgreen"),
              axis.text.x = element_blank()) +
        xlab("Stage")+ ylab("proportion")+
        scale_fill_manual("STAGE", values = c('ST1' = 'forestgreen',
                                              'ST2' = 'gold',
                                              'ST3' = 'firebrick1',
                                              'POCI'='darkorange2')
        )+
        geom_text(aes(label=st_prop), vjust = -.2, position = position_dodge(width=1), size=2)

}


### comment file
comment <-read.csv('https://raw.githubusercontent.com/L-1996-G/Migration/main/comment.csv', header = T)
comment$Portfolio = factor(comment$Portfolio, levels = c('CORP', 'MSME', 'SME',
                                                         'CONS', 'IPO','RETAIL',
                                                         'TOTAL','PAWN','SMALL',
                                                         'LARGE','MICRO','OTH',
                                                         'CARD'))

com_table <- function(prtfl){
    comment %>% filter(Portfolio %in% c(prtfl)) %>%
        select(comments) %>%
        kbl() %>%
        row_spec(1, bold = F, color = "#f1521a", background = "#B4E1FF")
}

theme_bs <- bs_theme(
    bg = "#0b3d91", fg = "white", primary = "#FCC780",
    base_font = font_google("Space Mono"),
    code_font = font_google("Space Mono")
)


library(shiny)

ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "minty"),
    #theme = theme_bs,
    tabsetPanel(
        tabPanel(
            h3("Reserve Analysis"),
            fluidRow(
                column(4,
                       h4("stage distribution"),
                       selectInput("prtfl", "choose Portfolio", unique(comment$Portfolio)),
                       tableOutput("comments")
                        ),
                column(8,
                       h4("Reserve by stages"),
                       selectInput("Portfolio", "choose Portfolio", unique(df_long$Portfolio)),
                       plotOutput("plot_1")

                )

            ),
            br(),
            fluidRow(
                column(4,
                    selectInput("Bank", "Choose Bank", unique(df$Bank)),
                    tableOutput('res_table')

                ),
                column(8,
                       h6("Stage distribution by Bank, including POCI"),
                       plotOutput("st_pl")
                       )
                )
        ),
        tabPanel(
            h3("Stage Migration Analysis"),
            sidebarLayout(
                sidebarPanel(
                    selectInput("bank", "Choose Bank", unique(dt_long_rt$bank)),
                    tableOutput("table"),
                    tableOutput("table_m")
                ),
                mainPanel(
                    h5("Click to change denominator: {mean_based : annual average Portfolio; Jan_based: Portfolio on 01-Jan}"),
                    actionButton("ave", label = "mean_based"),
                    actionButton("init", label = "Jan_based"),
                    selectInput("portfolio", "choose Portfolio", unique(dt_long_rt$portfolio)),
                    plotOutput("plot")
                )
            )
        ),

        tabPanel(
            h3("Data"),
            h5('Here you can see whole dataset, on which previos two page are based on.
               Click "migration" button to get data about stage migration (amount and rate);
               Click "Reserve" button to get data employed for reserve analysis (AM_ST# - is total
               amount of portfolio in stage # at the end of year)'),
            actionButton("migration", label = "migration"),
            actionButton("reserve", label = "Reserve"),
            br(),
            dataTableOutput("dynamic")
            )
        )
    )


server <- function(input, output, session) {
    # migration table
    my_table <- reactive({
        make_table(dt, input$portfolio, input$bank)
    })
    output$table = function()my_table()
    #
    rv <- reactiveValues(data = dt_long_rt)
    observeEvent(input$ave, {rv$data <- dt_long_rt})
    observeEvent(input$init, {rv$data <- dt_long_init_rt})
    output$text = renderText("Migration within stages")
    # rate table 3X# migration
    my_table1 <- reactive({
        rate_table(input$bank, input$portfolio)
    })
    output$table_m = function(){my_table1()}
    # migration plot
    output$plot <- renderPlot(my_plot(rv$data, input$portfolio))
    # comment table
    output$comments <- function(){com_table(input$prtfl)}
    # plot reserve by stage, bank and portfolio
    output$plot_1 <- renderPlot(res_plot(df_long, input$Portfolio))
    # plot stage distribution
    output$st_pl <- renderPlot(st_plot(df_st_l, input$Portfolio))
    # get reserve table for selected bank
    output$res_table = function()res_table(df, input$Portfolio,input$Bank)
    # get data table used for above analysis
    rt <- reactiveValues(data = dt)
    observeEvent(input$migration, {rt$data <- dt})
    observeEvent(input$reserve, {rt$data <- df})
    output$dynamic <- renderDataTable(rt$data, options = list(pageLength = 5))


}


# Run the application
shinyApp(ui = ui, server = server)
