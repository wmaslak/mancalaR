library(shiny)

#' Plot boards
#'
#' @param B_N
#' @param B_S
#'
#' @return
#'
#' @examples
plot_state <- function(B_N,B_S){

  COL <- 8
  ROW <- 4
  state_df <- as.data.frame(rbind(B_N,B_S))

  colnames(state_df) <- 1:COL
  state_df$row <- ROW:1

  state_df <- tidyr::pivot_longer(state_df,-row,names_to = "col")


  ggplot2::ggplot(state_df,ggplot2::aes(col,row,label = value, fontface = 2)) +
    ggplot2::geom_text(size = 6, color = "red")+
    ggplot2::geom_point(size = 20,alpha = 0.2)+
    ggplot2::geom_vline(xintercept = seq(0.5, COL + 0.5, by = 1),
                 size = 2, color = "#006495") +
    ggplot2::geom_hline(yintercept = seq(0.5, ROW + 0.5, by = 1),
                 size = 2, color = "#006495") +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_color_manual(values = c("#006495", "#F2635F"),
                         limits = c("O", "X")) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "#ffeb7d"),
            panel.border = ggplot2::element_rect(color = "#006495", fill = NA, size = 4),
            axis.text.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            legend.position = "none") +
    ggplot2::coord_fixed( xlim = c(0.5,8.5), ylim = c(0.5,4.5), expand = F)

}

############## shiny part ##############

#' App to play against bot
#'
#' @param B_N
#' @param B_S
#'
#' @examples
app <- function(B_N,B_S){
  ui <- shiny::fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  shiny::titlePanel("HusBao"),
  shiny::plotOutput("plot"),

)
server <- function(input, output, session) {
  thematic::thematic_shiny()

  output$plot <- shiny::renderPlot(plot_state(B_N,B_S))
}

shiny::shinyApp(ui = ui, server = server)
}
