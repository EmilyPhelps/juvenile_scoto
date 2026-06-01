# Colour scheme which is adapted from the economist colour scheme...

plot.pal <- function(pal){
  height <-c(rep("1", length(pal)))
  df <- data.frame(height, pal)
  ggplot(df) + geom_bar(aes(pal, fill=pal, color=pal)) + 
    theme_void() + 
    guides(color=FALSE, fill=FALSE) +
    scale_fill_manual(values = pal) +
    scale_color_manual(values=pal)
}


theme_cols <- c("#141F52", "#F2F2F2", "#D9D9D9", 	"#B3B3B3", "#595959", "#333333", 
                "#1A1A1A")

high_sat <- c("#E3120B", "#F6423C", "#E2365B", "#1DC9A4", "#1F2E7A",
              "#F97A1F",  "#F9C31F", "#B4C424", "#DFFF00") 

low_sat <- c("#D2F9F0", "#D6DBF5","#FCB583", "#FEE1CD", "#FEF2CD", "#F9D2DB")

bg_cols <- c("#E1DFD0", "#D0E1E1", "#EFF5F5", "#F5F4EF")

reds <- RColorBrewer::brewer.pal(9, "YlOrRd")
