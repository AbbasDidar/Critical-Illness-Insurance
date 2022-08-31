



library(ggplot2)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(readxl)
library(shinymanager)
library(DT)


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: Blue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"




inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"




# Define UI
ui <- secure_app(
    head_auth = tags$script(inactivity),
    fluidPage(
        navbarPage("Critical Illness Premium",
                   inverse = TRUE,
                   theme = shinytheme("lumen"),
                   tabPanel("Standalone CI Insurance",
                            fluid = TRUE, icon = icon("hand-pointer-o"),
                            tags$style(button_color_css),
                            sidebarLayout(
                                sidebarPanel(
                                    titlePanel(""),
                                    shinythemes::themeSelector(),
                                    fluidRow(
                                        column(
                                            12,
                                            h4("Age", style = "color:mediumvioletred"),
                                            sliderInput(
                                                inputId = "Age_1",
                                                label = "",
                                                min = 20,
                                                max = 60,
                                                value = 40, step = 1
                                            )
                                        ),
                                        column(
                                            12,
                                            h4("Gender", style = "color:mediumvioletred"),
                                            selectInput(
                                                inputId = "Sex_1",
                                                label = "",
                                                choices = c("Female", "Male"),
                                                selected = "Female",
                                                multiple = FALSE
                                            )
                                        ),
                                        column(
                                            12,
                                            h4("Duration", style = "color:mediumvioletred"),
                                            sliderInput(
                                                inputId = "Duration_1",
                                                label = "",
                                                min = 2,
                                                max = 20,
                                                value = 10, step = 1
                                            )
                                        ),
                                        column(
                                            12,
                                            h4("Interest rate", style = "color:mediumvioletred"),
                                            sliderInput(
                                                inputId = "Interest_rate_1",
                                                label = "",
                                                min = .02,
                                                max = .30,
                                                value = .12, step = .01
                                            )
                                        ),
                                        column(
                                            12,
                                            h4("Capital (Million)", style = "color:mediumvioletred"),
                                            numericInput(
                                                inputId = "Capital_1",
                                                label = "",
                                                min = 10,
                                                max = 100000,
                                                value = 500,
                                                step = 10,
                                                width = "500"
                                            )
                                        ),
                                        column(
                                            12,
                                            h4("Model", style = "color:mediumvioletred"),
                                            selectInput(
                                                inputId = "Model_1",
                                                label = "",
                                                choices = c("Gompertz", "Weibull"),
                                                selected = "Gompertz",
                                                multiple = FALSE
                                            )
                                        )
                                    )
                                ),
                                mainPanel(
                                    fluidRow(
                                        column(width = 3),
                                        column(br(),
                                               textOutput("report_1"),
                                               br(),
                                               width = 6, style = "text-align:center;font-size: 20px;
                                                               font-weight: bold;color:steelblue;
                                                               background-color:papayawhip;
                                                               border-left:10px solid palevioletred;
                                                               border-top: 10px solid palevioletred;
                                                               border-right:10px solid palevioletred;
                                                               border-bottom: 10px solid palevioletred"
                                        ),
                                        column(width = 3)
                                    ),
                                    br(),
                                    column(downloadButton("downloadData_1", "Download"), width = 12),
                                    br(),
                                    br(),
                                    column(withSpinner(dataTableOutput(outputId = "data_report_1")),
                                           width = 12,
                                           style = "font-family: B Mitra;font-size: 15px;font-weight: bold"
                                    )
                                )
                            ),
                            p(em("Developed by"), br(),
                              a(href = "https://www.linkedin.com/in/abbasdidar", "Abbas Didar", target = "_blank"),
                              style = "text-align:center;color:magenta; font-size: 20px; font-family: times"
                            )
                   ),
                   
                   #-------------------------------------------------------------------------------  
                   #------------------------------------------------------------------------------- 
                   #------------------------------------------------------------------------------- 
                   
                   
                   tabPanel("Full Acceleration Rider CI Insurance",
                            fluid = TRUE, icon = icon("hand-peace-o"),
                            tags$style(button_color_css),
                            sidebarLayout(
                                sidebarPanel(
                                    titlePanel(""),
                                    shinythemes::themeSelector(),
                                    fluidRow(
                                        column(
                                            12,
                                            h4("Age", style = "color:tomato"),
                                            sliderInput(
                                                inputId = "Age_2",
                                                label = "",
                                                min = 20,
                                                max = 60,
                                                value = 40, step = 1
                                            )
                                        ),
                                        column(
                                            12,
                                            h4("Gender", style = "color:tomato"),
                                            selectInput(
                                                inputId = "Sex_2",
                                                label = "",
                                                choices = c("Female", "Male"),
                                                selected = "Female",
                                                multiple = FALSE
                                            )
                                        ),
                                        column(
                                            12,
                                            h4("Duration", style = "color:tomato"),
                                            sliderInput(
                                                inputId = "Duration_2",
                                                label = "",
                                                min = 2,
                                                max = 20,
                                                value = 10, step = 1
                                            )
                                        ),
                                        column(
                                            12,
                                            h4("Interest rate", style = "color:tomato"),
                                            sliderInput(
                                                inputId = "Interest_rate_2",
                                                label = "",
                                                min = .02,
                                                max = .30,
                                                value = .12, step = .01
                                            )
                                        ),
                                        column(
                                            12,
                                            h4("Capital (Million)", style = "color:tomato"),
                                            numericInput(
                                                inputId = "Capital_2",
                                                label = "",
                                                min = 10,
                                                max = 100000,
                                                value = 500,
                                                step = 10,
                                                width = "500"
                                            )
                                        ),
                                        column(
                                            12,
                                            h4("Model", style = "color:tomato"),
                                            selectInput(
                                                inputId = "Model_2",
                                                label = "",
                                                choices = c("Gompertz", "Weibull"),
                                                selected = "Gompertz",
                                                multiple = FALSE
                                            )
                                        )
                                    )
                                ),
                                mainPanel(
                                    fluidRow(
                                        column(width = 3),
                                        column(br(),
                                               textOutput("report_2"),
                                               br(),
                                               width = 6, style = "text-align:center;font-size: 20px;
                                                               font-weight: bold;color:steelblue;
                                                               background-color:papayawhip;
                                                               border-left:10px solid lightseagreen;
                                                               border-top: 10px solid lightseagreen;
                                                               border-right:10px solid lightseagreen;
                                                               border-bottom: 10px solid lightseagreen"
                                        )
                                    ),
                                    hr(),
                                    downloadButton("downloadData_2", "Download"),
                                    hr(),
                                    column(withSpinner(dataTableOutput(outputId = "data_report_2")),
                                           width = 12,
                                           style = "font-family: B Mitra;font-size: 15px;font-weight: bold"
                                    )
                                )
                            ),
                            p(em("Developed by"), br(),
                              a(href = "https://www.linkedin.com/in/abbasdidar", "Abbas Didar", target = "_blank"),
                              style = "text-align:center;color:magenta; font-size: 20px; font-family: times"
                            )
                   ),
                   tabPanel("Other Apps",
                            fluid = TRUE, icon = icon("tachometer", verify_fa = FALSE),
                            tags$style(button_color_css),
                            br(),
                            br(),
                            br(),
                            fluidRow(
                                column(
                                    width = 4,
                                    p("(داشبورد مدل‌های یادگیری ماشین(هوش مصنوعی", br(),
                                      actionButton(
                                          inputId = "ab1", label = "کلیک کنید",
                                          icon = icon("brain"),
                                          onclick = "window.open('https://budgetrealizationdayinsurance.shinyapps.io/Model_Predictive/', '_blank')"
                                      ),
                                      style = "font-size: 25px;text-align:center;font-family: B Mitra;color:black;background-color:turquoise;padding:15px;border-radius:10px"
                                    )
                                ),
                                column(
                                    width = 4,
                                    p("داشبورد تحقق بودجه", br(),
                                      actionButton(
                                          inputId = "ab1", label = "کلیک کنید",
                                          icon = icon("coins"),
                                          onclick = "window.open('https://gitypardazesh.shinyapps.io/Gity_Budget_Dashboard/', '_blank')"
                                      ),
                                      style = "font-size: 25px;text-align:center;font-family: B Mitra;color:black;background-color:thistle;padding:15px;border-radius:10px"
                                    )
                                ),
                                column(
                                    width = 4,
                                    p("داشبورد محاسبه حق بیمه درمان سازمان خدماتی", br(),
                                      actionButton(
                                          inputId = "ab1", label = "کلیک کنید",
                                          icon = icon("hospital-user"),
                                          onclick = "window.open('https://abbasdidar5017.shinyapps.io/RATING_DASHBOARD3/', '_blank')"
                                      ),
                                      style = "font-size: 25px;text-align:center;font-family: B Mitra;color:black;background-color:turquoise;padding:15px;border-radius:10px"
                                    )
                                )
                            ),
                            
                            
                            fluidRow(
                                column(
                                    width = 4
                                ),
                                column(
                                    width = 4,
                                    p("linkedin", br(),
                                      actionButton(
                                          inputId = "ab1", label = "کلیک کنید",
                                          icon = icon("linkedin"),
                                          onclick = "window.open('https://www.linkedin.com/in/abbasdidar', '_blank')"
                                      ),
                                      style = "font-size: 25px;text-align:center;font-family: B Mitra;color:black;background-color:thistle;padding:15px;border-radius:10px"
                                    )
                                ),
                                column(
                                    width = 4
                                )
                            ),
                            
                            
                            hr(),
                            p(em("Developed by"), br(),
                              a(href = "https://www.linkedin.com/in/abbasdidar", "Abbas Didar", target = "_blank"),
                              style = "text-align:center; color:magenta; font-size: 20px; font-family: times"
                            )
                   )
        )
    )
)
