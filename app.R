#--------------------------------------------------------------------------------------------------------
#
# A shiny app for looking a Net Worth data 
# (https://www.stats.govt.nz/information-releases/household-net-worth-statistics-year-ended-june-2018)
#
# Alan Bentley, May 2019
#
#--------------------------------------------------------------------------------------------------------


# 1. Set up

# 1.1 Load packages

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(scales)

# 1.2 Some Stats NZ colours

pal1 <- c("#272726","#4d5b61","#ed7218")

palc <- c("#085c75", "#d2ac2f", "#ae4e51", "#35345d", "#76a93f", "#6f2e38", "#0d94a3", "#dd6829", "#1a6e5b")

# 1.3 Custom chart theme

theme_ab = function(base_size=16) {
  theme_minimal() +
    theme(text = element_text(family = "Source Sans Pro", size = base_size, color=pal1[2]),
          axis.title = element_blank(),
          plot.title = element_text(size=16),
          plot.tag = element_text(size=45, colour=palc[1], face = "bold"),
          plot.subtitle = element_text(size=12),
          plot.caption = element_text(size=10),
          legend.text = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.text.x = element_blank(),
          strip.text = element_text (color=pal1[2], size=14, margin = margin(0,.5,0,.5, "cm")),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1, "lines"))
}

# 2. Load the data

download.file("https://www.stats.govt.nz/assets/Uploads/Household-net-worth-statistics/Household-net-worth-statistics-Year-ended-June-2018/Download-data/household-net-worth-statistics-year-ended-june-2018-csv.zip",
              destfile = "net_worth_2018.zip")

# 2.1 Create household type proportions

hh_type <- read.csv(unz("net_worth_2018.zip","Table501.csv")) %>%
  filter(year==2018 & class %in% c("HC1","HC2","HC3","HC4","HC5","HC6","HC7","HC8","HC9")
         & type==1 & measure==4) %>%
  mutate(hhtype=ifelse(class=="HC8","One person",
                       ifelse(class=="HC1","Couple only",
                              ifelse(class %in% c("HC2","HC3","HC4"),"Couple with child(ren)",
                                     ifelse(class %in% c("HC5","HC6"),"One parent with child(ren)",
                                            "All other households"))))) %>%
  select(-class) %>%
  group_by(hhtype) %>%
  summarise(estimate=sum(estimate)) %>%
  ungroup() %>%
  mutate(prop=estimate/sum(estimate),
         hhtype=factor(hhtype, levels=hhtype[order(prop)]),
         hhtype2=factor(hhtype, levels=c("One person",
                                         "One parent with child(ren)",
                                         "Couple only",
                                         "Couple with child(ren)",
                                         "All other households")))

# 2.2 Read the binned data (for income, assets and debt)

download.file("https://www.stats.govt.nz/assets/Uploads/Household-net-worth-statistics/Household-net-worth-statistics-Year-ended-June-2018/Download-data/household-net-worth-statistics-year-ended-june-2018-interactive-tool-numbers.zip",
              destfile = "net_worth_2018_tool.zip")

d <- read.csv(unz("net_worth_2018_tool.zip","household-net-worth-statistics-year-ended-june-2018-interactive-tool-numbers.csv")) %>%
  rename(class=Household.Composition,
         bin=Ranges,
         bin2=Chart.labels,
         measure=Measure) %>%
  mutate(hhtype=ifelse(class=="One person household","One person",
                       ifelse(class=="Couple only","Couple only",
                              ifelse(class %in% c("Couple with children"),"Couple with child(ren)",
                                     ifelse(class %in% c("Sole parent with children"),"One parent with child(ren)",
                                            ifelse(class %in% c("All NZ Households"),"All NZ Households",
                                            "All other households"))))),
         hhtype3=ifelse(class=="One person household","one-person households",
                       ifelse(class=="Couple only","couple-only households",
                              ifelse(class %in% c("Couple with children"),"couple with child(ren) households",
                                     ifelse(class %in% c("Sole parent with children"),"one-parent with child(ren) households",
                                            ifelse(class %in% c("All NZ Households"),"New Zealand households",
                                                   "all other households")))))) %>%
  group_by(class,measure) %>%
  mutate(freq=as.numeric(Estimate)*1000,
         percent=freq/sum(freq),
         cum_percent=cumsum(percent)) %>%
  ungroup()

# 2.3 Tidy up data

#Income

income <- filter(d, measure=="Income") %>%
  mutate(bin2=as.character(bin2),
         bin2=factor(bin2, ordered=T, levels=rev(unique(bin2))))

# Assets

assets <- filter(d, measure=="Assets") %>%
  mutate(bin2=as.character(bin2),
         bin2=factor(bin2, ordered=T, levels=rev(unique(bin2))))

# Liabilities

liabilities <- filter(d, measure=="Liabilities") %>%
  mutate(bin2=as.character(bin2),
         bin2=factor(bin2, ordered=T, levels=rev(unique(bin2))))

# Net worth (wealth)

wealth <- filter(d, measure=="Net worth") %>%
  mutate(bin2=as.character(bin2),
         bin2=factor(bin2, ordered=T, levels=rev(unique(bin2))))

# 3. Define user interface

ui <- fluidPage(
  
  # 3.1 Google analytics
  
  # tags$head(includeScript("google-analytics.js")),
  
  # 3.2 Custom css & loading icon
  
  tags$style(type="text/css", "

 a{color:#5C6366}
 a:hover {color: #0B0C0C}
 .btn:hover {color: white; background: #196C82 !important}
 .tabbable > .nav > li[class=active] > a {color: #0B0C0C}

           body {
    font-family: Source sans pro;
    width:90%;
    margin-left:auto;
    margin-right:auto;
           }

 .irs-bar-edge {background: #005C75; border: #005C75}
 .irs-bar {background: #005C75; border: #005C75}  
 .irs-from { color: white; background: #005C75}
 .irs-to {color: white; background: #005C75}
 .irs-grid-text {color: #5C6366}
 .irs-single {color: white; background: #005C75}

#loadmessage {
            width: 80%;
            padding: 150px 0px 1000px 0px;
            text-align: center;
            font-size: 200%;
            color: #5C6366;
            background-color: white;
           }
  "),
  
  
   
# 3.3 Application title
  
   titlePanel("How wealthy am I?"),
  
  div(h2("Fill in your details"),style = "color:#005C75; font: bold;"), 
  div( h4("Find out how you compare with households like you"), style = "color:#5C6366;"),

   
# 3.4 Sidebar with inputs

   sidebarLayout(
      sidebarPanel(
        div("Select your",strong("household type"), style = "font-size: 120%;"),
        selectInput("hhtype",NULL,
                     choices=c(levels(hh_type$hhtype2),"All NZ Households"), width='100%',
                    selected=levels(hh_type$hhtype2)[4]),
        br(),
        div("Your household",strong("assets"), style = "font-size: 120%;"),
        div("Include the value of your house and contents, rental properties, cars and boats, money in the bank, pensions, and shares.", 
            style = "font-size: 90%; color:#5C6366;"),
        sliderInput("asset",
                    "",
                    pre = "$",
                    min = 0,
                    max = 2000000,
                    value = 500000,
                    step= 10000,
                    round=-3, ticks = F, width='100%'),
        br(),
        div("Your household",strong("debt"), style = "font-size: 120%;"),
        div("Include the total of your mortgages, car loans, student loans, and credit card debt.", 
            style = "font-size: 90%; color:#5C6366;"),
        sliderInput("debt",
                    "",
                    pre = "$",
                    min = 0,
                    max = 1000000,
                    value = 40000,
                    step= 5000,
                    round=-3, ticks = F, width='100%'),
        br(),
        div("Your household",strong("income"),style = "font-size: 120%;"),
        div("Everyone’s gross annual income (before tax) including wages, salary, interest, and one-off payments.", 
            style = "font-size: 90%; color:#5C6366;"),
        sliderInput("inc",
                    "",
                    pre = "$",
                    min = 0,
                    max = 250000,
                    value = 70000,
                    step= 5000,
                    round=-3, ticks = F, width='100%'),
        br(),
        conditionalPanel("input.go == 0",
        actionButton(inputId = "go",
                     label = "Show comparisons",
                     style="border-color: #ed7218"))
      ),
      
# 3.5 Main panel with plots (tab between different plots)

      mainPanel(
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div(div("",icon("spinner", "fa-4x fa-spin")),id="loadmessage")),
        conditionalPanel("input.go != 0",
        tabsetPanel(type = "tabs",
                    tabPanel("Wealth", 
                             h3(textOutput("text5")),
                             plotOutput("p5"),
                             h2(textOutput("text6"))),
                    tabPanel("Assets", 
                             h3(textOutput("text3")),
                             plotOutput("p3"),
                             h2(textOutput("text7"))),
                    tabPanel("Debt",
                             h3(textOutput("text4")),
                             plotOutput("p4"),
                             h2(textOutput("text9"))),
                    tabPanel("Income", 
                             h3(textOutput("text2")),
                             plotOutput("p2"),
                             h2(textOutput("text8"))),
                    tabPanel("Household type", 
                             conditionalPanel("input.hhtype != 'All NZ Households'",
                             h2(textOutput("text1")),
                             plotOutput("p1")),
                             conditionalPanel("input.hhtype == 'All NZ Households'",
                                              br(),
                                              h4("Select a household type to see breakdowns")))
        ))
        )
   ),
p(div("Created by Stats NZ", style="colour:#4d5b61"), a(href="https://www.stats.govt.nz/about-us/privacy-statement/", "privacy statement", style = "color:#b7bdbf"))
)

# 4. Define server logic 

server <- function(input, output) {
  
# 4.1 Household types chart
  
  output$p1 <- renderPlot({
    
    hh_type_u <- filter(hh_type, hhtype==input$hhtype)
    
    hh_type_plot <- ggplot(hh_type, aes(hhtype,prop))+
      geom_bar(stat="identity", fill="grey70") +
      geom_text(aes(label=paste0("  ", round(prop*100,1),"%"), hjust="left"),colour=pal1[2])+
      geom_bar(stat="identity", data=hh_type_u, fill=palc[1], size=2) +
      scale_y_continuous(limits=c(0,0.5))+
      coord_flip()+
      theme_ab()
    
    
    print(hh_type_plot)
    
  })
  
  output$text1 <- renderText({
    
    hh_type_u <- filter(hh_type, hhtype==input$hhtype)
    
    paste0(round(hh_type_u$prop*100,1), "% of households are like yours: ", hh_type_u$hhtype)
  })
  
# 4.2 Income chart
  
  output$p2 <- renderPlot({
    
    if(input$inc <15000){
    
    income_u <- filter(income, hhtype==input$hhtype) %>%
      mutate(class_u="Less than $15k")
    
    } else if (input$inc >=15000 & input$inc < 45000){
      
      income_u <- filter(income, hhtype==input$hhtype) %>%
        mutate(class_u="$15k to < $45k")
      
    } else if (input$inc >=45000 & input$inc < 75000){
      
      income_u <- filter(income, hhtype==input$hhtype) %>%
        mutate(class_u="$45k to < $75k")
      
    } else if (input$inc >=75000 & input$inc < 105000){
      
      income_u <- filter(income, hhtype==input$hhtype) %>%
        mutate(class_u="$75k to < $105k")
      
    } else if (input$inc >=105000 & input$inc < 135000){
      
      income_u <- filter(income, hhtype==input$hhtype) %>%
        mutate(class_u="$105k to < $135k")
      
    } else if (input$inc >=135000 & input$inc < 165000){
      
      income_u <- filter(income, hhtype==input$hhtype) %>%
        mutate(class_u="$135k to < $165k")
      
    }
    
    else {
      
      income_u <- filter(income, hhtype==input$hhtype) %>%
        mutate(class_u="$165k and higher")
      
    }
    
    p2 <- ggplot(income_u, aes(bin2,percent))+
      facet_wrap(~hhtype)+
      geom_bar(stat="identity", fill="grey70")+
      geom_bar(data=filter(income_u, bin2==class_u), stat="identity", fill=palc[1])+
      geom_text(aes(label=paste0("  ", round(percent*100,1),"%"), hjust="left"),colour=pal1[2])+
      scale_y_continuous(limits=c(0,max(income_u$percent)+0.1))+
      coord_flip()+
      theme_ab()+
      labs(x="", y="")
    
    print(p2)
    
    output$text2 <- renderText({
      
      income_u2 <- filter(income_u, bin2==class_u)
      
      paste0(round(income_u2$percent*100,1), "% of ", income_u2$hhtype3, " have similar income: ", income_u2$class_u)
    })
    
    output$text8 <- renderText({
      
      income_u2 <- filter(income_u, bin2==class_u)
      
      if ((income_u2$cum_percent - income_u2$percent) != 0){
        
        paste0("You have more income than ", round((income_u2$cum_percent - income_u2$percent)*100,1), "% of ", income_u2$hhtype3)
      }
      
    })
    
  })

# 4.3 Assets chart
  
  output$p3 <- renderPlot({
    
    if(input$asset <100000){
      
      asset_u <- filter(assets, hhtype==input$hhtype) %>%
        mutate(class_u="Less than $100k")
      
    } else if (input$asset >=100000 & input$asset < 400000){
      
      asset_u <- filter(assets, hhtype==input$hhtype) %>%
        mutate(class_u="$100k to < $400k")
      
    } else if (input$asset >=400000 & input$asset < 700000){
      
      asset_u <- filter(assets, hhtype==input$hhtype) %>%
        mutate(class_u="$400k to < $700k")
      
    } else if (input$asset >=700000 & input$asset < 1000000){
      
      asset_u <- filter(assets, hhtype==input$hhtype) %>%
        mutate(class_u="$700k to < $1 million")
      
    } else if (input$asset >=1000000 & input$asset < 1300000){
      
      asset_u <- filter(assets, hhtype==input$hhtype) %>%
        mutate(class_u="$1 million to < $1.3 million")
      
    } else if (input$asset >=1300000 & input$asset < 1600000){
      
      asset_u <- filter(assets, hhtype==input$hhtype) %>%
        mutate(class_u="$1.3 million to < $1.6 million")
      
    }
    
    else {
      
      asset_u <- filter(assets, hhtype==input$hhtype) %>%
        mutate(class_u="$1.6 million and higher")
      
    }
    
    p3 <- ggplot(asset_u, aes(bin2,percent))+
      facet_wrap(~hhtype)+
      geom_bar(stat="identity", fill="grey70")+
      geom_bar(data=filter(asset_u, bin2==class_u), stat="identity", fill=palc[1])+
      geom_text(aes(label=paste0("  ", round(percent*100,1),"%"), hjust="left"),colour=pal1[2])+
      scale_y_continuous(limits=c(0,max(asset_u$percent)+0.1))+
      coord_flip()+
      theme_ab()+
      labs(x="", y="")
    
    print(p3)
    
    output$text3 <- renderText({
      
      asset_u2 <- filter(asset_u, bin2==class_u)
      
      paste0(round(asset_u2$percent*100,1), "% of ", asset_u2$hhtype3, " have similar assets: ", asset_u2$class_u)
    })
    
    output$text7 <- renderText({
      
      asset_u2 <- filter(asset_u, bin2==class_u)
      
      if ((asset_u2$cum_percent - asset_u2$percent) != 0){
      
      paste0("You have more assets than ", round((asset_u2$cum_percent - asset_u2$percent)*100,1), "% of ", asset_u2$hhtype3)
      }
      
      })
    
  })
  
# 4.4 Debt chart
  
  output$p4 <- renderPlot({
    
    if(input$debt <=0){
      
      liabilities_u <- filter(liabilities, hhtype==input$hhtype) %>%
        mutate(class_u="No debt")
      
    } else if (input$debt >0 & input$debt < 50000){
      
      liabilities_u <- filter(liabilities, hhtype==input$hhtype) %>%
        mutate(class_u="$0 to < $50k")
      
    } else if (input$debt >=50000 & input$debt < 100000){
      
      liabilities_u <- filter(liabilities, hhtype==input$hhtype) %>%
        mutate(class_u="$50k to < $100k")
      
    } else if (input$debt >=100000 & input$debt < 250000){
      
      liabilities_u <- filter(liabilities, hhtype==input$hhtype) %>%
        mutate(class_u="$100k to < $250k")
      
    } else if (input$debt >=250000 & input$debt < 500000){
      
      liabilities_u <- filter(liabilities, hhtype==input$hhtype) %>%
        mutate(class_u="$250k to < $500k")
      
    }
    
    else {
      
      liabilities_u <- filter(liabilities, hhtype==input$hhtype) %>%
        mutate(class_u="$500k and higher")
      
    }
    
    p4 <- ggplot(liabilities_u, aes(bin2,percent))+
      facet_wrap(~hhtype)+
      geom_bar(stat="identity", fill="grey70")+
      geom_bar(data=filter(liabilities_u, bin2==class_u), stat="identity", fill=palc[1])+
      geom_text(aes(label=paste0("  ", round(percent*100,1),"%"), hjust="left"),colour=pal1[2])+
      scale_y_continuous(limits=c(0,max(liabilities_u$percent)+0.1))+
      coord_flip()+
      theme_ab()+
      labs(x="", y="")
    
    print(p4)
    
    output$text4 <- renderText({
      
      liabilities_u2 <- filter(liabilities_u, bin2==class_u)
      
      paste0(round(liabilities_u2$percent*100,1), "% of ",  liabilities_u2$hhtype3, " have similar debt: ", liabilities_u2$class_u)
    })
    
    output$text9 <- renderText({
      
      liabilities_u2 <- filter(liabilities_u, bin2==class_u)
      
      if ((1 - liabilities_u2$cum_percent) != 0){
        
        paste0("You have less debt than ", round((1 - liabilities_u2$cum_percent)*100,1), "% of ", liabilities_u2$hhtype3)
      }
      
    })
    
  })
  
# 4.5 Wealth chart
  
  output$p5 <- renderPlot({
    
    if((input$asset - input$debt) <20000){
      
      wealth_u <- filter(wealth, hhtype==input$hhtype) %>%
        mutate(class_u="Less than $20k")
      
    } else if ((input$asset - input$debt) >= 20000 & (input$asset - input$debt) < 100000){
      
      wealth_u <- filter(wealth, hhtype==input$hhtype) %>%
        mutate(class_u="$20k to < $100k")
      
    } else if ((input$asset - input$debt) >= 100000 & (input$asset - input$debt) < 250000){
      
      wealth_u <- filter(wealth, hhtype==input$hhtype) %>%
        mutate(class_u="$100k to < $250k")
      
    } else if ((input$asset - input$debt) >= 250000 & (input$asset - input$debt) < 500000){
      
      wealth_u <- filter(wealth, hhtype==input$hhtype) %>%
        mutate(class_u="$250k to < $500k")
      
    } else if ((input$asset - input$debt) >= 500000 & (input$asset - input$debt) < 750000){
      
      wealth_u <- filter(wealth, hhtype==input$hhtype) %>%
        mutate(class_u="$500k to < $750k")
      
    } else if ((input$asset - input$debt) >= 750000 & (input$asset - input$debt) < 1250000){
      
      wealth_u <- filter(wealth, hhtype==input$hhtype) %>%
        mutate(class_u="$750k to < $1,250k")
      
    }
    
    else {
      
      wealth_u <- filter(wealth, hhtype==input$hhtype) %>%
        mutate(class_u="$1.25 million and higher")
      
    }
    
    p5 <- ggplot(wealth_u, aes(bin2,percent))+
      facet_wrap(~hhtype)+
      geom_bar(stat="identity", fill="grey70")+
      geom_bar(data=filter(wealth_u, bin2==class_u), stat="identity", fill=palc[1])+
      geom_text(aes(label=paste0("  ", round(percent*100,1),"%"), hjust="left"),colour=pal1[2])+
      scale_y_continuous(limits=c(0,max(wealth_u$percent)+0.1))+
      coord_flip()+
      theme_ab()+
      labs(x="", y="")
    
    print(p5)
    
    output$text5 <- renderText({
      
      wealth_u2 <- filter(wealth_u, bin2==class_u)
      
      paste0(round(wealth_u2$percent*100,1), "% of ",  wealth_u2$hhtype3, " have similar wealth: ", wealth_u2$class_u)
    })
    
    output$text6 <- renderText({
      
      wealth_u2 <- filter(wealth_u, bin2==class_u)
      
      if ((wealth_u2$cum_percent - wealth_u2$percent) != 0){
      
      paste0("You are wealthier than ", round((wealth_u2$cum_percent - wealth_u2$percent)*100,1), "% of ",  wealth_u2$hhtype3)
      }
        
        })
    
  })
  
}

# 5. Run the application 
shinyApp(ui = ui, server = server)

