library(shiny)
require(rCharts)
options(RCHART_LIB = 'polycharts')
data(mtcars)

# Text in variables for the documentation part of the application
Introduction <-
    "This application is an exploratory tool for “mtcars” dataset. The application allows visualising the data using different types of plots in R. It is also possible to perform a simple multivariable regression analysis taking into account different variables summarising the statistical results of the fitting. When the outcome is logical, logistic regression is performed automatically. Therefore, “lm” function is used in the lineal regression and “glm” with “family = binomial” in the argument for logistic regression.

At the top part a tab menu is shown. It allows switching between the documentation part (“Instructions” tab), a visual an exploratory analysis (“Plotting” tab) or a regression analysis (“Regression” tab).

This application will be intended to be flexible in the future to allow exploration in any different datasets.
"

PlottingTab <- "In this tab four possible graphs are allowed.

-	Single plot: A single plot choosing two variables, the outcome and regressor, fitting the points. It calculates the correlation between variables and make.
-	Pairs plot :  It makes matrix plots. In the left part it is possible to choose the variables. In the upper part of the plot a correlation calculations between variables is calculated. In the diagonal it shows the histogram of the variables. In the lower part the plots in pairs are shown with a loess fitting.
-	R Chart: One can choose two variables to plot, the outcome and the regressor using facets. The latter can be choose in the menu. These are the variables which can be seen as factors in the dataset. Improvement for this part is necessary.
-	ggplot: A single plot with the outcome and regressor is performed, although one can choose the color of the represented data to see the dependence with the chosen factor variables in the dataset.

"
RegressionTab <-
    "In the left part of the screen and outcome and the desired regressor are chosen for regression.  In the right part, it is possible to switch between summarising the analysis performed and the residual plots obtained from the fitting"


# Here starts the shinyUI
shinyUI(navbarPage(
    "Exploring mtcars",fluid = TRUE,
    tabPanel(
        "Instructions",
        h3("1. Introduction"),
        p(Introduction),
        h3("2. Plotting Tab "),
        p(PlottingTab),
        h3("3. RegressionTab"),
        p(RegressionTab)
    ),
    tabPanel("Plotting",
             fluidPage (
                 titlePanel(h4("")),
                 tabsetPanel(
                     tabPanel('Single Plot',
                              sidebarLayout(
                                  sidebarPanel(
                                      h4('Choose Variables'),
                                      selectInput('id1', 'variable x',
                                                  names(mtcars)),
                                      selectInput('id2', 'variable y',
                                                  names(mtcars)),
                                      h4('correlation '),
                                      verbatimTextOutput('oid')
                                  ),
                                  mainPanel(plotOutput("plot1"))
                              )),
                     tabPanel('Pairs Plot',
                              sidebarLayout(
                                  sidebarPanel(
                                      checkboxGroupInput(
                                          "cid2", h4("Choose Variables:"),
                                          selected = c("mpg","cyl"),
                                          names(mtcars)
                                      )
                                  ),
                                  mainPanel(plotOutput('plot2'))
                              )),
                     tabPanel('R Chart',
                              sidebarLayout(
                                  sidebarPanel(
                                      h4('Choose Variables'),
                                      selectInput('ir1', 'variable X:',
                                                  names(mtcars)),
                                      selectInput('ir2', 'variable Y:',
                                                  names(mtcars)),
                                      h4('Facets'),
                                      selectInput('ir3', 'variable z:',
                                                  c('cyl','vs','am','gear','carb'))
                                  ),
                                  mainPanel(showOutput("chart1", "polycharts"))
                              )),
                     tabPanel('ggplot',
                              sidebarLayout(
                                  sidebarPanel(
                                      h4('Choose Variables'),
                                      selectInput('ig1', 'variable X:',
                                                  names(mtcars)),
                                      selectInput('ig2', 'variable Y:',
                                                  names(mtcars)),
                                      h4('color'),
                                      selectInput('ig3', 'variable z:',
                                                  c('cyl','vs','am','gear','carb'))
                                  ),
                                  mainPanel(plotOutput('ggplot1'))
                              ))
                 )
             )),
    tabPanel("Regression",
             fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         h4('Choose'),
                         selectInput('idy', 'outcome(y):',
                                     names(mtcars)),
                         checkboxGroupInput("cid", "regressors (x1, x2, ...):",
                                            selected = c("cyl"),
                                            names(mtcars))
                     ),
                     mainPanel(tabsetPanel(
                         tabPanel('Results',
                                  h4('Summary'),
                                  verbatimTextOutput('rid2')),
                         
                         tabPanel('Plot Residuals',
                                  plotOutput(
                                      'plotS1',
                                      height = 500, width = 500
                                  ))
                     ))
                 )
             ))
    
    
    
    
    
    ))


