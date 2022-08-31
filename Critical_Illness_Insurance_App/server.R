

library(ggplot2)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(readxl)
library(shinymanager)


DATA <- read_excel("Esempio di calcolo.xlsx", sheet = "data")


# data.frame with credentials info
credentials <- data.frame(
    user = c("Didar"),
    password = c("Abbas"),
    stringsAsFactors = FALSE
)




################################################################################
#                                                                              #
#                                   TABLE 3                                    #
#                                                                              #
################################################################################

# _________________________________ Gompertz ____________________________________

# Male


mu_14_Male <- log(DATA$mu_14_MALE)

mu14_MALE_Gompertz_REG <- lm(mu_14_Male ~ DATA$age)

summary(mu14_MALE_Gompertz_REG)

G_M_14_b1 <- exp(mu14_MALE_Gompertz_REG$coefficients[1])
G_M_14_b2 <- mu14_MALE_Gompertz_REG$coefficients[2]

# Female

mu_14_FEMALE <- log(DATA$mu_14_FEMALE)

mu14_FEMALE_Gompertz_REG <- lm(mu_14_FEMALE ~ DATA$age)

summary(mu14_FEMALE_Gompertz_REG)

G_F_14_b1 <- exp(mu14_FEMALE_Gompertz_REG$coefficients[1])
G_F_14_b2 <- mu14_FEMALE_Gompertz_REG$coefficients[2]


# _________________________________ Weibull _____________________________________

# Male


mu14_MALE_Weibull_REG <- lm(mu_14_Male ~ log(DATA$age))

summary(mu14_MALE_Weibull_REG)

W_M_14_b1 <- exp(mu14_MALE_Weibull_REG$coefficients[1])
W_M_14_b2 <- mu14_MALE_Weibull_REG$coefficients[2]
# Female


mu14_FEMALE_Weibull_REG <- lm(mu_14_FEMALE ~ log(DATA$age))

summary(mu14_FEMALE_Weibull_REG)

W_F_14_b1 <- exp(mu14_FEMALE_Weibull_REG$coefficients[1])
W_F_14_b2 <- mu14_FEMALE_Weibull_REG$coefficients[2]



################################################################################
#                                                                              #
#                                   TABLE 4                                    #
#                                                                              #
################################################################################


# _________________________________ Gompertz ____________________________________

# Male



mu_23_Male <- log(DATA$mu_23_MALE)

Gompertz_REG_mu23_MALE <- lm(mu_23_Male ~ DATA$age)

summary(Gompertz_REG_mu23_MALE)

G_M_23_b1 <- exp(Gompertz_REG_mu23_MALE$coefficients[1])
G_M_23_b2 <- Gompertz_REG_mu23_MALE$coefficients[2]



# Female

mu_23_FEMALE <- log(DATA$mu_23_FEMALE)

Gompertz_REG_mu23_FEMALE <- lm(mu_23_FEMALE ~ DATA$age)

summary(Gompertz_REG_mu23_FEMALE)

G_F_23_b1 <- exp(Gompertz_REG_mu23_FEMALE$coefficients[1])
G_F_23_b2 <- Gompertz_REG_mu23_FEMALE$coefficients[2]


# _________________________________ Weibull _____________________________________

# Male


Weibull_REG_mu23_MALE <- lm(mu_23_Male ~ log(DATA$age))

summary(Weibull_REG_mu23_MALE)

W_M_23_b1 <- exp(Weibull_REG_mu23_MALE$coefficients[1])
W_M_23_b2 <- Weibull_REG_mu23_MALE$coefficients[2]


# Female


Weibull_REG_mu23_FEMALE <- lm(mu_23_FEMALE ~ log(DATA$age))

summary(Weibull_REG_mu23_FEMALE)

W_F_23_b1 <- exp(Weibull_REG_mu23_FEMALE$coefficients[1])
W_F_23_b2 <- Weibull_REG_mu23_FEMALE$coefficients[2]




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# A1_(CI)



CI_Weibull <- function(x, N, Delta, Gamma, Sigma, b1h, b2h) {
    n <- 11
    
    y <- seq(0, N, length.out = n)
    
    period <- y + x
    
    j <- rep(1:(sum(period %% 5 == 0) - 1), each = 5)
    
    i <- 1:(n - 1)
    
    Dk <- b1h / (b2h + 1) * (b2h + 1) * (x + (y[i] + y[i + 1]) / 2)^b2h
    
    
    
    
    sum(
        Sigma[i] * exp(-b1h / (b2h + 1) * ((x + (y[i] + y[i + 1]) / 2)^(b2h + 1) - x^(b2h + 1))) *
            exp((y[i] + y[i + 1]) / 2 * Dk) * (exp((-Sigma[i] + Delta + Dk) * y[i]) - exp(-(Sigma[i] + Delta + Dk) * y[i + 1])) /
            (Sigma[i] + Delta + Dk)
    )
}




CI_Gompertz <- function(x, N, Delta, Gamma, Sigma, b1h, b2h) {
    n <- 11
    
    y <- seq(0, N, length.out = n)
    
    period <- y + x
    
    j <- rep(1:(sum(period %% 5 == 0) - 1), each = 5)
    
    i <- 1:(n - 1)
    
    
    Dk <- b1h / (b2h + 1) * (b2h + 1) * (x + (y[i] + y[i + 1]) / 2)^b2h
    
    sum(
        Sigma[i] * exp(-b1h / (b2h + 1) * ((x + (y[i] + y[i + 1]) / 2)^(b2h + 1) - x^(b2h + 1))) *
            exp((y[i] + y[i + 1]) / 2 * Dk) * (exp((-Sigma[i] + Delta + Dk) * y[i]) -
                                                   exp(-(Sigma[i] + Delta + Dk) * y[i + 1])) /
            (Sigma[i] + Delta + Dk)
    )
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# A2_(CI)


CI_2_Weibull <- function(x, N, Delta, Gamma, Sigma, b1h, b2h) {
    n <- 11
    
    y <- seq(0, N, length.out = n)
    
    period <- y + x
    
    j <- rep(1:(sum(period %% 5 == 0) - 1), each = 5)
    
    i <- 1:(n - 1)
    
    Dk <- b1h / (b2h + 1) * (b2h + 1) * (x + (y[i] + y[i + 1]) / 2)^b2h + .03
    
    
    
    
    sum(
        Sigma[i] * exp(-b1h / (b2h + 1) * ((x + (y[i] + y[i + 1]) / 2)^(b2h + 1) - x^(b2h + 1))) *
            exp((y[i] + y[i + 1]) / 2 * Dk) * (exp((-Sigma[i] + Delta + Dk) * y[i]) - exp(-(Sigma[i] + Delta + Dk) * y[i + 1])) /
            (Sigma[i] + Delta + Dk)
    )
}

CI_2_Gompertz <- function(x, N, Delta, Gamma, Sigma, b1h, b2h) {
    n <- 11
    
    y <- seq(0, N, length.out = n)
    
    period <- y + x
    
    j <- rep(1:(sum(period %% 5 == 0) - 1), each = 5)
    
    i <- 1:(n - 1)
    
    
    Dk <- b1h / (b2h + 1) * (b2h + 1) * (x + (y[i] + y[i + 1]) / 2)^b2h + .003
    
    sum(
        Sigma[i] * exp(-b1h / (b2h + 1) * ((x + (y[i] + y[i + 1]) / 2)^(b2h + 1) - x^(b2h + 1))) *
            exp((y[i] + y[i + 1]) / 2 * Dk) * (exp((-Sigma[i] + Delta + Dk) * y[i]) -
                                                   exp(-(Sigma[i] + Delta + Dk) * y[i + 1])) /
            (Sigma[i] + Delta + Dk)
    )
}

# CI AC1
Age <- seq(20, 60, 5)
Coeff_W_M <- c(2, 7, 15, 21, 25, 63, 90, 170, 220) + 10
Coeff_W_F <- c(3, 6, 13.3, 19, 28.7, 53, 83, 160, 230.1) + 10
Coeff_G_M <- c(3, 8, 12, 21, 28, 54, 80, 153, 250) + 10
Coeff_G_F <- c(3.5, 9, 15, 18, 26.4, 60, 87.1, 178, 250.3) + 10


# CI AC2
Age <- seq(20, 60, 5)
Coeff_W_M_CI2 <- c(13, 28, 41, 63, 97, 110, 130, 183, 240)
Coeff_W_F_CI2 <- c(11, 22, 38, 49, 92, 108, 118, 171, 231)
Coeff_G_M_CI2 <- c(15, 28, 40, 63, 101, 111, 127, 190, 251)
Coeff_G_F_CI2 <- c(14, 25, 35, 66, 96, 108, 122, 193, 243)



server <- function(input, output, session) {
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    #-------------------------------------------------------------------------------
    
    CI_Report_1 <- reactive(
        if (input$Model_1 == "Weibull" & input$Sex_1 == "Male") {
            CI_Weibull(
                x = input$Age_1, N = input$Duration_1, Delta = log(1 + input$Interest_rate_1),
                Gamma = 0, Sigma = DATA$sigma, b1h = W_M_14_b1, b2h = W_M_14_b2
            ) * Coeff_W_M[max(which((input$Age_1 + .01) >= Age))]
        } else {
            if (input$Model_1 == "Weibull" & input$Sex_1 == "Female") {
                CI_Weibull(
                    x = input$Age_1, N = input$Duration_1, Delta = log(1 + input$Interest_rate_1),
                    Gamma = 0, Sigma = DATA$sigma, b1h = W_F_14_b1, b2h = W_F_14_b2
                ) * Coeff_W_F[max(which((input$Age_1 + .01) >= Age))]
            } else {
                if (input$Model_1 == "Gompertz" & input$Sex_1 == "Male") {
                    CI_Gompertz(
                        x = input$Age_1, N = input$Duration_1, Delta = log(1 + input$Interest_rate_1),
                        Gamma = 0, Sigma = DATA$sigma, b1h = W_M_14_b1, b2h = W_M_14_b2
                    ) * Coeff_G_M[max(which((input$Age_1 + .01) >= Age))]
                } else {
                    if (input$Model_1 == "Gompertz" & input$Sex_1 == "Female") {
                        CI_Gompertz(
                            x = input$Age_1, N = input$Duration_1, Delta = log(1 + input$Interest_rate_1),
                            Gamma = 0, Sigma = DATA$sigma, b1h = W_F_14_b1, b2h = W_F_14_b2
                        ) * Coeff_G_F[max(which((input$Age_1 + .01) >= Age))]
                    }
                }
            }
        }
    )
    
    
    output$report_1 <- renderText(
        paste("Net Single Premium  of Standalone CI Insurance (Rial):", round(CI_Report_1()/1000 * input$Capital_1 * 10^7 ))
    )
    
    
    output$data_report_1 <- DT::renderDataTable(
        DT::datatable(
            {
                data.frame(
                    "Year" = paste(rep("Premium Year", input$Duration_1), 1:input$Duration_1),
                    "Premium (Rial)" = round( (CI_Report_1()/1000 * input$Capital_1 * 10^7) / input$Duration_1 * (1 + input$Interest_rate_1)^(1:input$Duration_1))
                )
            },
            options = list(
                pageLength = input$Duration_1,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'paleturquoise', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "none",
            selection = "multiple",
            style = "bootstrap4",
            class = "display",
            rownames = FALSE
        )
    )
    
    DATA_RESULT_1 <- reactive(
        
        data.frame(
            "Year" = paste(rep("Premium Year", input$Duration_1), 1:input$Duration_1),
            "Premium (Rial)" = round((CI_Report_1()/1000 * input$Capital_1 * 10^7) / input$Duration_1 * (1 + input$Interest_rate_1)^(1:input$Duration_1))
        )
    )
    
    output$downloadData_1 <- downloadHandler(
        filename = function() {
            paste("Single_CI-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(DATA_RESULT_1(), file)
        }
    )
    
    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    
    
    CI_Report_2 <- reactive(
        if (input$Model_2 == "Weibull" & input$Sex_2 == "Male") {
            CI_2_Weibull(
                x = input$Age_2, N = input$Duration_2, Delta = log(1 + input$Interest_rate_2),
                Gamma = 0, Sigma = DATA$sigma, b1h = W_M_14_b1, b2h = W_M_14_b2
            ) * Coeff_W_M_CI2[max(which((input$Age_2 + .01) >= Age))]
        } else {
            if (input$Model_2 == "Weibull" & input$Sex_2 == "Female") {
                CI_2_Weibull(
                    x = input$Age_2, N = input$Duration_2, Delta = log(1 + input$Interest_rate_2),
                    Gamma = 0, Sigma = DATA$sigma, b1h = W_F_14_b1, b2h = W_F_14_b2
                ) * Coeff_W_F_CI2[max(which((input$Age_2 + .01) >= Age))]
            } else {
                if (input$Model_2 == "Gompertz" & input$Sex_2 == "Male") {
                    CI_2_Gompertz(
                        x = input$Age_2, N = input$Duration_2, Delta = log(1 + input$Interest_rate_2),
                        Gamma = 0, Sigma = DATA$sigma, b1h = W_M_14_b1, b2h = W_M_14_b2
                    ) * Coeff_G_M_CI2[max(which((input$Age_2 + .01) >= Age))]
                } else {
                    if (input$Model_2 == "Gompertz" & input$Sex_2 == "Female") {
                        CI_2_Gompertz(
                            x = input$Age_2, N = input$Duration_2, Delta = log(1 + input$Interest_rate_2),
                            Gamma = 0, Sigma = DATA$sigma, b1h = W_F_14_b1, b2h = W_F_14_b2
                        ) * Coeff_G_F_CI2[max(which((input$Age_2 + .01) >= Age))]
                    }
                }
            }
        }
    )
    
    
    output$report_2 <- renderText(
        paste("Net Single Premium  of Full Acceleration Rider CI Insurance (Rial) :", round(CI_Report_2()/1000 * input$Capital_2 *10^7
        ))
    )
    
    output$data_report_2 <- DT::renderDataTable(
        
        DT::datatable(
            {
                data.frame(
                    "Year" = paste(rep("Premium Year", input$Duration_2), 1:input$Duration_2),
                    "Premium (Rial)" = round( (CI_Report_2()/1000 * input$Capital_1 * 10^7) / input$Duration_2 * (1 + input$Interest_rate_2)^(1:input$Duration_2))
                )
            },
            options = list(
                pageLength = input$Duration_2,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'lightpink', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "none",
            selection = "multiple",
            style = "bootstrap4",
            class = "display",
            rownames = FALSE
        )
    )
    
    DATA_RESULT_2 <- reactive(
        
        data.frame(
            "Year" = paste(rep("Premium Year", input$Duration_2), 1:input$Duration_2),
            "Premium (Rial)" = round( (CI_Report_2()/1000 * input$Capital_2 * 10^6) / input$Duration_2 * (1 + input$Interest_rate_2)^(1:input$Duration_2))
        )
    )
    
    output$downloadData_2 <- downloadHandler(
        filename = function() {
            paste("Accelerat_CI-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(DATA_RESULT_2(), file)
        }
    )
}
