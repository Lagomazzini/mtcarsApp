
library(shiny) 
library(ggplot2)
library(plyr)
data(mtcars)



# functions for the plot
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y, use="complete.obs"))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex =  cex.cor * (1 + r) / 2)
}
panel.hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

# STARTS the server input output function

shinyServer(function(input, output) {
    output$plot1 <- renderPlot({
        x <- mtcars[, input$id1]
        y <- mtcars[, input$id2]
        dfPlot <- data.frame(y = y, x = x)
        g <- ggplot(dfPlot, aes(x, y)) +
            geom_point() +
            stat_smooth(
                method = ifelse(grepl("am|vs", input$id2), "glm", "lm"),
                family = ifelse(grepl("am|vs", input$id2), "binomial", "") )+
            xlab(input$id1) +
            ylab(input$id2)
        print(g)
    })
    
    output$plot2 <- renderPlot({
        df <- mtcars[input$cid2]
        pairs(
            df, upper.panel = panel.cor,
            diag.panel  = panel.hist,
            lower.panel = panel.smooth
        )
    })
    
    
    output$oid <- renderPrint({
        x <- mtcars[, input$id1]
        y <- mtcars[, input$id2]
        cor(x, y)
    })
    
    output$chart1 <- renderChart2({
        x <- mtcars[, input$ir1]
        y <- mtcars[, input$ir2]
        z <- as.character(mtcars[, input$ir3])
        dfPlot <- data.frame(y = y, x = x, z = as.factor(z))
        p1 <-rPlot(y ~ x | z, data = dfPlot, color = 'z', type = "point") 
        p1$addParams(dom = 'myChart', width = 500, height = 400)
        p1$guides(x = list(title = input$ir1))
        p1$guides(y = list(title = input$ir2))
        p1$guides(color = list(title = input$ir3 ))
        return(p1)
    }) 
    
    output$ggplot1 <- renderPlot({
        x <- mtcars[, input$ig1]
        y <- mtcars[, input$ig2]
        z <- as.factor(mtcars[, input$ig3])
        dfPlot <- data.frame(y = y, x = x, z = z)
        g <- ggplot(dfPlot, aes(x, y)) +
            geom_point(aes(color = z), size = 5)+
            xlab(input$ig1) +
            ylab(input$ig2)+
            labs(colour = input$ig3)
        print(g)
        })
    
    
    ### Rendering statistical part    
    
    fit <- reactive({
        nms <- input$cid[!grepl(input$idy, input$cid)]
        df_mtcars <- mtcars[nms]
        y <- mtcars[,input$idy]
        if(input$idy == "vs" | input$idy == "am"){
            glm(y ~ ., data = df_mtcars, family = binomial)
            }
        else{lm(y ~ ., data = df_mtcars)}
       
    })
    
    output$rid2 <- renderPrint({ 
        summary(fit())
        })
    
    output$plotS1 <- renderPlot({
        par(mfrow = c(2,2))
        plot(fit())
    })
    
    
})    
        
        