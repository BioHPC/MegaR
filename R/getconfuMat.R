#' MegaR analysis
#'
#' This is an internal function used to plot the confusion Matrix
#'
#' @param testdata  testdata
#' @param rfmodel the model on which classification is done

#' @import ggplot2
#' @import RColorBrewer
#' @export

#library(ggplot2)
#library(RColorBrewer)
getconfuMat <- function(testdata, rfmodel){
    tpre <- stats::predict(rfmodel, testdata[,-ncol(testdata)])
    #tpre
    confu_mat <- caret::confusionMatrix(tpre,as.factor(testdata$country))
    jBluesFun <- grDevices::colorRampPalette(brewer.pal(n = 6, "Blues"))
    paletteSize <- 256
    jBluesPalette <- jBluesFun(paletteSize)
    ggplotConfusionMatrix <- function(m){
        p <-with(m,{
            ggplot(data = as.data.frame(m$table) ,
                   aes(x = Prediction, y =Reference)) +
            geom_tile(aes(fill = Freq), colour = "White") +
            scale_fill_gradient2(
                low = jBluesPalette[1],
                mid = jBluesPalette[paletteSize/2],
                high = jBluesPalette[paletteSize],
                midpoint = (max(data.frame(m$table)$Freq) +
                                min(data.frame(m$table)$Freq)) / 2,
                name = "") +
            theme(axis.text.x = element_text(face = "bold", color = "black"))+
            theme(axis.text.y  = element_text(face = "bold", color = "black"))+
            geom_text(aes(x = Prediction, y = Reference, label = Freq)) +
            scale_x_discrete( name = element_text("Prediction", face = "bold"))+
            scale_y_discrete( name = "True Label" )+
            ggtitle("Confusion Matrix")+
            theme(legend.key.height = unit(2.4, "cm"),
                  plot.title = element_text(hjust = 0.5))+
            theme(panel.border = element_rect(linetype = "solid", fill = NA))})
        return(p)
    }
    return (list(ggplotConfusionMatrix(confu_mat), confu_mat))
}
