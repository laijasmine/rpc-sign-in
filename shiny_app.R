library(shiny)
library(dplyr)
library(scales)

ui <- fluidPage(
  titlePanel("Pottery Workshop Pricing & Profit Calculator"),

  sidebarLayout(
    sidebarPanel(
      h4("Class Setup"),
      numericInput(
        "students",
        "Students per class",
        value = 10,
        min = 1,
        max = 20
      ),
      numericInput("price", "Price per student ($)", value = 220, min = 0),
      numericInput("weeks", "Number of weeks", value = 6, min = 1),
      numericInput(
        "hours",
        "Hours per class",
        value = 2.5,
        min = 1,
        step = 0.5
      ),

      hr(),
      h4("Instructor & Fixed Costs"),
      numericInput("rate", "Instructor hourly rate ($)", value = 50),
      numericInput("rent", "Rent per class ($)", value = 400),

      hr(),
      h4("Materials & Variable Costs"),
      numericInput("clay", "Clay cost ($)", value = 100),
      numericInput("glaze", "Glaze/materials ($)", value = 50),
      numericInput("firing", "Firing cost ($)", value = 75),
      numericInput("misc", "Misc costs ($)", value = 50),

      hr(),
      h4("Scaling"),
      numericInput("classes_month", "Classes per month", value = 2),
      numericInput("workshops_month", "Workshops per month", value = 2),
      numericInput("workshop_price", "Workshop price ($)", value = 75),
      numericInput("workshop_students", "Workshop students", value = 10)
    ),

    mainPanel(
      h3("Financial Summary"),
      fluidRow(
        column(3, strong("Revenue"), textOutput("revenue")),
        column(3, strong("Total Costs"), textOutput("costs")),
        column(3, strong("Profit"), textOutput("profit")),
        column(3, strong("Margin"), textOutput("margin"))
      ),

      hr(),
      h3("Break-even Analysis"),
      fluidRow(
        column(
          6,
          strong("Break-even students"),
          textOutput("breakeven_students")
        ),
        column(6, strong("Break-even revenue"), textOutput("breakeven_revenue"))
      ),

      hr(),
      h3("Monthly Projection"),
      fluidRow(
        column(4, strong("Monthly Revenue"), textOutput("monthly_revenue")),
        column(4, strong("Monthly Profit"), textOutput("monthly_profit")),
        column(4, strong("Workshops Profit"), textOutput("workshop_profit"))
      ),

      hr(),
      h3("Yearly Projection"),
      fluidRow(
        column(6, strong("Yearly Revenue"), textOutput("yearly_revenue")),
        column(6, strong("Yearly Profit"), textOutput("yearly_profit"))
      ),

      hr(),
      h3("Profit Sensitivity (with Break-even Line)"),
      plotOutput("profitPlot")
    )
  )
)

server <- function(input, output) {
  calc <- reactive({
    revenue <- input$students * input$price
    instructor_cost <- input$rate * input$hours * input$weeks

    total_costs <- instructor_cost +
      input$rent +
      input$clay +
      input$glaze +
      input$firing +
      input$misc

    profit <- revenue - total_costs
    margin <- ifelse(revenue > 0, profit / revenue, 0)

    list(
      revenue = revenue,
      total_costs = total_costs,
      profit = profit,
      margin = margin
    )
  })

  # Break-even
  breakeven_students <- reactive({
    if (input$price > 0) {
      calc()$total_costs / input$price
    } else {
      NA
    }
  })

  output$breakeven_students <- renderText({
    round(breakeven_students(), 1)
  })

  output$breakeven_revenue <- renderText({
    dollar(calc()$total_costs)
  })

  # Outputs
  output$revenue <- renderText(dollar(calc()$revenue))
  output$costs <- renderText(dollar(calc()$total_costs))
  output$profit <- renderText(dollar(calc()$profit))
  output$margin <- renderText(percent(calc()$margin))

  # Monthly projections
  monthly_calc <- reactive({
    class_revenue <- calc()$revenue * input$classes_month
    class_profit <- calc()$profit * input$classes_month

    workshop_revenue <- input$workshop_price *
      input$workshop_students *
      input$workshops_month
    workshop_profit <- (input$workshop_price * input$workshop_students - 300) *
      input$workshops_month

    list(
      revenue = class_revenue + workshop_revenue,
      profit = class_profit + workshop_profit,
      workshop_profit = workshop_profit
    )
  })

  output$monthly_revenue <- renderText(dollar(monthly_calc()$revenue))
  output$monthly_profit <- renderText(dollar(monthly_calc()$profit))
  output$workshop_profit <- renderText(dollar(monthly_calc()$workshop_profit))

  # Yearly projections
  output$yearly_revenue <- renderText({
    dollar(monthly_calc()$revenue * 12)
  })

  output$yearly_profit <- renderText({
    dollar(monthly_calc()$profit * 12)
  })

  # Sensitivity plot with break-even line
  output$profitPlot <- renderPlot({
    prices <- seq(150, 300, by = 10)
    students <- seq(6, 14, by = 1)

    grid <- expand.grid(price = prices, students = students)

    grid <- grid %>%
      mutate(
        revenue = price * students,
        instructor_cost = input$rate * input$hours * input$weeks,
        total_costs = instructor_cost +
          input$rent +
          input$clay +
          input$glaze +
          input$firing +
          input$misc,
        profit = revenue - total_costs
      )

    plot(
      grid$price,
      grid$profit,
      xlab = "Price per Student ($)",
      ylab = "Profit ($)",
      main = "Profit Sensitivity with Break-even",
      pch = 16
    )

    # Break-even line (profit = 0)
    abline(h = 0, lty = 2)
  })
}

shinyApp(ui = ui, server = server)
