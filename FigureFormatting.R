#Colour Scheme for manuscript

red <- "#c95563"
pal <- c("#BFBF6E", "#9CAA78", "#9FC6A8", "#89C0AF", "#5691B0", "#4A8BB3", "#375F89", "#364B70", "#1A2D4A")

pred.alpha <- c("#dfdfbb", "#b1cad8")
theme_cols <- c("#F2F2F2", "#D9D9D9", "#B3B3B3", "#595959", "#333333", "#1A1A1A", "#121212", "#4F647F", "#3E4C65")

#Theme with missing the y axis
scoto_theme1 <- function(){
  theme_classic() +
    theme(text = element_text(colour=theme_cols[7]),
          axis.title = element_text(face = "bold"),
          axis.line.x = element_line(colour=theme_cols[7], linewidth = 0.3),
          axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_line(colour=theme_cols[7], linewidth= 0.3),
          panel.grid.major.y = element_line(colour=theme_cols[2], linewidth=0.2)
          )
}

#Theme with both axis
scoto_theme2 <- function(){
  theme_classic() +
    theme(text = element_text(colour=theme_cols[7]),
          axis.title = element_text(face = "bold"),
          axis.line.y = element_line(colour=theme_cols[7], linewidth = 0.3),
          axis.ticks=element_line(colour=theme_cols[7], linewidth= 0.3),
    )
}

facet_scoto_theme <- function(){
  theme_classic() +
    theme(text = element_text(colour=theme_cols[7]),
          strip.background = element_blank(),
          strip.text  = element_text(face="bold"),
          axis.title = element_text(face = "bold"),
          axis.line.x = element_line(colour=theme_cols[7], linewidth = 0.3),
          axis.line.y = element_line(colour=theme_cols[7], linewidth = 0.3),
          axis.ticks=element_line(colour=theme_cols[7], linewidth= 0.3),
          axis.ticks.x=element_line(colour=theme_cols[7], linewidth= 0.3),
          panel.grid.major.y = element_line(colour=theme_cols[2], linewidth=0.2)
    )
}
