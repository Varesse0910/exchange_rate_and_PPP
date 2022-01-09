# Code by David GALLO, Varesse TIATSE SOUOP, and Karunashree NAGARAJ

# to run our web app
library(shiny)

# for themes to make it look pretty
library(bslib)
library(shinythemes)

# to store our data sets
library(data.table)

# to plot our data
library(ggplot2)

# we use this to get percentages easily
library(scales)

# to get our working directory
library(rstudioapi)   

# Set working directory to source file location
#setwd(dirname(getActiveDocumentContext()$path))

# Import the exchange rates and purchasing power parity (ppp) as data tables
DT_exchange = fread("exchange_rates.csv")
DT_ppp = fread("ppp.csv")

# Drops useless columns
DT_exchange = subset(DT_exchange, select = c("LOCATION","TIME","Value"))
DT_ppp = subset(DT_ppp, select = c("LOCATION","TIME","Value"))

# Set join fields as location and time cols
setkey(DT_exchange,LOCATION,TIME)
setkey(DT_ppp,LOCATION,TIME)

# perform an inner join
DT_joined =  merge(x = DT_exchange, y = DT_ppp[, c("LOCATION", "TIME", "Value")], all = FALSE) #<-- change this flag to TRUE if you want to keep nulls using an outer join

#drops rows with bad data
DT_joined = DT_joined[LOCATION != "EU27_2020"]

# Rename columns with ambiguous names
colnames(DT_joined) = c("LOCATION", "TIME", "Exchange Rate", "PPP")


#if (interactive()) {
  ui <- fluidPage(
    # makes the web app look good with a bootstrap theme 
    theme = bs_theme(version = 4, bootswatch = "minty"),
    
    titlePanel("Exchange rate and Personal Purchasing Power (PPP)"),
    
    helpText("Created by David GALLO, and Varesse TIATSE SOUOP, and Karunashree NAGARAJ"),
    
    sidebarLayout(
      
      sidebarPanel(
        
        
        helpText("Compare exchange rate over time by adjusting the criteria below"),
        
        # initialize the date range in the client side
        dateRangeInput("daterange", "Date range:",
                       start = "2001-01-01",
                       end   = "2010-12-31",
                       min = "1950-01-01",
                       max = "2020-12-31",
                       format = "yyyy",
                       startview = "decade"),
        
        # select the base index country with which the other countries/years are compared. Default is USA in the dataset.
        selectInput('base_country', label = "Select a base country to index from ", choices = unique(DT_joined$LOCATION),
                    selected = "USA", multiple = FALSE),
        
        # create list of countries to plot
        selectInput('countries', label = "Select the countries to plot without labels", choices = unique(DT_joined$LOCATION),
                    selected = c("AUS","USA","CAN","FRA", "BRA"), multiple = TRUE),
        
        # create list of countries that will be in color (IDN per default)
        selectizeInput('SelectedCountries', label = "Select the countries to plot with labels", choices = DT_joined$LOCATION,
                       options = list(placeholder = 'select a country code'),
                       multipleClasses(details = TRUE),
                       selected = c("FRA", "USA"), multiple = TRUE),
        
        # adds an option for users to display the graph with a log10 scale -- useful for countries with high inflation
        checkboxInput("logY", "Use Log-10 scale for Y-axis", FALSE),
        
        ),
        
      mainPanel(
        plotOutput("plot"),
        textOutput("text")
      )
    )
  )

  server <- function(input, output) {
    output$plot <- renderPlot({
      
      # Compare and validate the From year and To year. From year must be less than To year for data to appear.
      validate(
        need(strtoi(format(input$daterange[1], format = "%Y")) < strtoi(format(input$daterange[2], format = "%Y")), "End year must be greater than start year")
      )

      # Select the country that will be used as a "base" index -- the dataset originally uses USA.
      base_country = input$base_country
      
      # Create a "base table". This table contains the exchange rate and ppp for all the years of our base country, which we will use as a new annual index
      base_table = DT_joined[LOCATION == base_country, .(`TIME`, `Exchange Rate`, `PPP`)]
      
      # Nested for loops are too slow and memory-inefficient for a dataset with ~3000 rows. Instead, we can split up the data table into years and perform computations after.
      # Here we are dividing the exchange rate and PPP for each year by the base/index country's exchange rate and PPP for that year.
      # Then we join our computed data tables together.
      # Effectively, this gives us a new computed data table with a new base/index country
      
      # initialize a list which we will use to store data tables that contain the exchange rate and PPP for every country, per year (one data table per year).
      list_of_tables = list()
      
      # loops through each row in the base table. Remember that each row is one year of data for the base country.
      for (i in 1:nrow(base_table)) {
        # we then create a temp data table that extracts all the rows in the DT_joined table where the year matches the current row in the for loop of the base table
        temp_yearly_table = DT_joined[`TIME` == base_table[i,`TIME`], .(`LOCATION`, `TIME`, `Exchange Rate`, `PPP`)]
        
        # we append that temp table to a list of tables. When this for loop completes, we'll have a list of data tables, each one for a specific year.
        # this way when we join the tables, we will only have data for years that the base country has data for, preventing nulls.
        list_of_tables = append(list_of_tables, list(temp_yearly_table))
      }
      
      # loops through our list of data tables
      for (i in 1:length(list_of_tables)) {
        # finds the base exchange rate. Each data table is for a specific year, in the same order as the base_table
        # this means that data table number 5 is the same year as the data in base_table 5, which means we can simply lookup the exchange rate in base_table[i]
        base_exchange = base_table[i, `Exchange Rate`]
        
        # do the same for PPP
        base_ppp = base_table[i, `PPP`]
        
        # now we execute the calculation -- dividing the exchange rate and ppp for each country by the base exchange rate and ppp for that year
        list_of_tables[[i]] = list_of_tables[[i]][, .(`LOCATION`, `TIME`, round(`Exchange Rate`/base_exchange, digits = 6), round(`PPP`/base_ppp, digits = 6))]
        
        # Rename columns with ambiguous names
        colnames(list_of_tables[[i]]) = c("LOCATION", "TIME", "Exchange Rate", "PPP")
      }
      
      # finally, we combine all our data tables back together, this time indexed against a different country
      DT_combined = rbindlist(list_of_tables)
      
      # create the plot according to date range
      plt = ggplot(DT_combined[`TIME` %in% seq(format(input$daterange[1], format = "%Y"), format(input$daterange[2], format = "%Y"), 1)
                       & `LOCATION` %in% input$countries],
            # generate axes
            aes(x = `TIME`, y = `Exchange Rate`, group = `LOCATION`)) +
      
            # plot lines
            geom_line() +
            geom_line(data = DT_combined[`TIME` %in% seq(format(input$daterange[1], format = "%Y"), format(input$daterange[2], format = "%Y"), 1) 
                                     & `LOCATION` %in% input$SelectedCountries],
                      aes(col = LOCATION),
                      size = 1.6
            ) +
        # this code below is used to ensure the x axis displays only whole numbers (decimal years don't make sense for us).
        # It uses an anonymous function, x, which takes a sequence of numbers on the x axis from the first number to the last year of the date range,
        # then it expands the scale aka adds padding around each tick on the axis
        scale_x_continuous(
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
          expand = expansion(mult = c(0, 0.05))
        )
      
      # checks the input box to see if the y-scale should be log 10. This is useful for countries with high inflation like Chile.
      if(input$logY)
        plt <- plt + scale_y_log10()
      
      # returns the final plot with our changes
      return(plt)
    })
    
    # Because we weren't able to finish their PPP vs Exchange Rate graph, we have added this simple text to the output to show that we are able to compare
    # and use data from two datasets (PPP and Exchange rate)
    output$text <- renderText({ paste("The Exchange Rate to PPP ratio for the first highlighted country, ", input$SelectedCountries[1], 
                                      ", in the first selected year", format(input$daterange[1], format = "%Y"), ", is", 
                                      DT_joined[`TIME` == format(input$daterange[1], format = "%Y") & `LOCATION` == input$SelectedCountries[1], percent(round(`Exchange Rate`/`PPP`, 3))]
                                      )
      })
  }
  shinyApp(ui, server)
#}