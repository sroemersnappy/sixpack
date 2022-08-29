
twopack <- function (xST, xLT = NA, LSL, USL, Target , alpha = 0.05, 
                     f.na.rm = TRUE, f.main = PN_WO, 
                     f.sub = "", f.colours = c("#4682B4", "#d1d1e0", 
                                               "#000000", "#00C800", "#FF0000")) 
{
  if (is.na(Target)) {
    stop("Target is needed")
  }
  if (is.na(LSL) & is.na(USL)) {
    stop("No specification limits provided")
  }
  mST = mean(xST, na.rm = f.na.rm)
  sST = sd(xST, na.rm = f.na.rm)
  nST = length(xST[!is.na(xST)])
  nLT = length(xLT[!is.na(xLT)])
  zST = ss.ca.z(xST, LSL, USL)
  cpST = ss.ca.cp(xST, LSL, USL)
  cpiST = ss.ca.cp(xST, LSL, USL, ci = TRUE, alpha = alpha)
  cpkST = ss.ca.cpk(xST, LSL, USL)
  cpkiST = ss.ca.cpk(xST, LSL, USL, ci = TRUE, alpha = alpha)
  DPMO <- (1 - pnorm(zST - 1.5)) * 10^6
  if (is.numeric(xLT)) {
    mLT = mean(xLT, na.rm = f.na.rm)
    sLT = sd(xLT, na.rm = f.na.rm)
    cpLT = ss.ca.cp(xLT, LSL, USL, LT = TRUE)
    cpiLT = ss.ca.cp(xLT, LSL, USL, LT = TRUE, ci = TRUE, 
                     alpha = alpha)
    cpkLT = ss.ca.cpk(xLT, LSL, USL, LT = TRUE)
    cpkiLT = ss.ca.cpk(xLT, LSL, USL, LT = TRUE, ci = TRUE, 
                       alpha = alpha)
    zLT = ss.ca.z(xLT, LSL, USL, LT = TRUE)
    DPMO <- (1 - pnorm(zLT)) * 10^6
  }
  else {
    mLT = NA
    sLT = NA
    cpLT = NA
    cpiLT = NA
    cpkLT = NA
    cpkiLT = NA
    zLT <- zST - 1.5
  }
  if (length(f.colours) != 5) {
    default <- c("#4682B4", "#868686", "#000000", 
                 "#00C800", "#FF0000")
    f.colours <- c(f.colours, default[(length(f.colours) + 
                                         1):5])
  }
  prepCanvas(f.main, f.sub)
  vp.plots <- grid::viewport(name = "plots", layout = grid::grid.layout(2, 
                                                                        2, c(0.6, 0.4), c(0.6, 0.4)))
  grid::pushViewport(vp.plots)
  vp.hist <- grid::viewport(name = "hist", layout.pos.row = 1, 
                            layout.pos.col = 1)
  grid::pushViewport(vp.hist)
  grid::grid.text("Histogram & Density", y = 1, just = c("center", 
                                                         "top"))
  
  
  
  
  
  
  
  
  
  
  
  binwST <- diff(range(xST))/(sqrt(nST)*1.4) 
  
  
  ggdata <- reshape2::melt(xST)
  qqp <- ggplot(ggdata, aes(x = value))
  hist <- qqp + geom_histogram(aes(y = ..density..), binwidth = binwST, 
                               fill = f.colours[1], stat = "bin")
  xST_density <- density(xST, bw = binwST)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  if (!is.na(LSL)) {
    hist <- hist + annotate(geom = "text", x = LSL - 
                              abs(max(xST_density$x) - min(xST_density$x)) * 0.02, 
                            y = max(xST_density$y), label = "LSL", hjust = "right", 
                            size = 4)
    hist <- hist + expand_limits(x = LSL - (abs(max(xST_density$x) - 
                                                  min(xST_density$x)) * 0.04))
  }
  hist <- hist + annotate(geom = "text", x = Target + 
                            abs(max(xST_density$x) - min(xST_density$x)) * 0.03, 
                          y = max(xST_density$y) + abs(max(xST_density$y) * 0.3), 
                          label = "Target", hjust = "left", size = 4)
  if (!is.na(USL)) {
    hist <- hist + annotate(geom = "text", x = USL + 
                              abs(max(xST_density$x) - min(xST_density$x)) * 0.02, 
                            y = max(xST_density$y), label = "USL", hjust = "left", 
                            size = 4)
    hist <- hist + expand_limits(x = USL + (abs(max(xST_density$x) - 
                                                  min(xST_density$x)) * 0.04))
  }
  hist <- hist + xlab(NULL) + ylab(NULL) + theme(axis.text.y = element_blank())
  if (!is.na(LSL)) {
    hist <- hist + geom_vline(xintercept = LSL, linetype = 2, 
                              size = 1, colour = f.colours[5])
  }
  if (!is.na(USL)) {
    hist <- hist + geom_vline(xintercept = USL, linetype = 2, 
                              size = 1, colour = f.colours[5])
  }
  hist <- hist + geom_vline(xintercept = Target, linetype = 3, 
                            size = 1, colour = f.colours[4]) + stat_density(geom = "path", 
                                                                            position = "identity", size = 1, colour = f.colours[2]) + 
    stat_function(fun = dnorm, args = with(ggdata, c(mean(value), 
                                                     sd(value))), linetype = 2, size = 1, colour = f.colours[2])
  
  
  
  
  
  if (is.numeric(xLT)) {
    binwLT <- diff(range(xLT))/sqrt(nLT)
    ggdataLT <- reshape2::melt(xLT)
    hist <- hist + stat_density(geom = "path", data = ggdataLT, 
                                position = "identity", colour = f.colours[3]) + 
      stat_function(fun = dnorm, args = with(ggdataLT, 
                                             c(mean = mean(value), sd = sd(value))), linetype = 2, 
                    colour = f.colours[3])
  }
  
  
  
  print(hist, newpage = FALSE)
  
  
  grid::popViewport()
  vp.norm <- grid::viewport(name = "normal", layout.pos.row = 2, 
                            layout.pos.col = 1, layout = grid::grid.layout(2, 2, 
                                                                           c(0.6, 0.4), c(0.1, 0.9)))
  grid::pushViewport(vp.norm)
  #grid::grid.text("Check Normality", y = 1, just = c("center", 
          #                                           "top"))
  vp.qq <- grid::viewport(name = "qqp", layout.pos.row = 2, 
                          layout.pos.col = 1, height = unit(0.5, "npc"))
  grid::pushViewport(vp.qq)
  qqp <- qplot(sample = xST) + xlab(NULL) + ylab(NULL) + theme(axis.text.x = element_blank(), 
                                                               axis.text.y = element_blank())
  #print(qqp, newpage = FALSE)
  grid::popViewport()
  vp.testn <- grid::viewport(name = "testn", layout.pos.row = 2, 
                             layout.pos.col = 2)
  grid::pushViewport(vp.testn)
  ss.ts <- shapiro.test(xST)
  ss.tl <- nortest::lillie.test(xST)
  
  
  #if (min(ss.ts$p.value, ss.tl$pvalue) < alpha) {
   # warning("Normality test/s failed")
  #}
  #grid::grid.text("Shapiro-Wilk Test", y = 0.9, just = c("center", 
       #                                                  "top"), gp = grid::gpar(cex = 0.8))
  #grid::grid.text(paste("p-value: ", format(ss.ts$p.value, 
    #                                        digits = 4)), gp = grid::gpar(cex = 0.8), y = 0.8)
  #grid::grid.text("Lilliefors (K-S) Test", gp = grid::gpar(cex = 0.8))
  #grid::grid.text(paste("p-value: ", format(ss.tl$p.value, 
                  #                          digits = 4)), gp = grid::gpar(cex = 0.8), y = 0.4)
  grid::popViewport()
 # grid::grid.text("Normality accepted when p-value > 0.05", 
             #     y = 0.02, just = c("center", "bottom"), gp = grid::gpar(cex = 0.8))
  grid::popViewport()
  vpNumbers <- grid::viewport(name = "numbers", layout.pos.row = c(1, 
                                                                   2), layout.pos.col = 2, layout = grid::grid.layout(4, 
                                                                                                                      1))
  grid::pushViewport(vpNumbers)
  grid::grid.rect(gp = grid::gpar(col = "#BBBBBB", lwd = 2))
  vpLegend <- grid::viewport(name = "legend", layout.pos.row = 1)
  grid::pushViewport(vpLegend)
  grid::grid.rect(gp = grid::gpar(col = "#BBBBBB", lwd = 2))
  grid::grid.text(expression(bold("Density Lines Legend")), 
                  y = 0.95, just = c("center", "top"))
  grid::grid.lines(x = c(0.05, 0.3), y = c(0.7, 0.7), gp = grid::gpar(lty = 1, 
                                                                      lwd = 3, col = f.colours[2]))
  grid::grid.text("Density ST", x = 0.35, y = 0.7, just = c("left", 
                                                            "center"), gp = grid::gpar(cex = 0.8))
  grid::grid.lines(x = c(0.05, 0.3), y = c(0.55, 0.55), gp = grid::gpar(lty = 2, 
                                                                        lwd = 3, col = f.colours[2]))
  grid::grid.text("Theoretical Dens. ST", x = 0.35, y = 0.55, 
                  just = c("left", "center"), gp = grid::gpar(cex = 0.8))
  if (is.numeric(xLT)) {
    grid::grid.lines(x = c(0.05, 0.3), y = c(0.4, 0.4), gp = grid::gpar(lty = 1, 
                                                                        lwd = 1, col = f.colours[3]))
    grid::grid.text("Density LT", x = 0.35, y = 0.4, 
                    just = c("left", "center"), gp = grid::gpar(cex = 0.8))
    grid::grid.lines(x = c(0.05, 0.3), y = c(0.25, 0.25), 
                     gp = grid::gpar(lty = 2, lwd = 1, col = f.colours[3]))
    grid::grid.text("Theoretical Density LT", x = 0.35, 
                    y = 0.25, just = c("left", "center"), 
                    gp = grid::gpar(cex = 0.8))
  }
  grid::popViewport()
  vpSpec <- grid::viewport(name = "spec", layout.pos.row = 2)
  grid::pushViewport(vpSpec)
  grid::grid.text(expression(bold("Specifications")), 
                  y = 0.95, just = c("center", "top"))
  grid::grid.text(expression(bold("LSL: ")), y = unit(0.95, 
                                                      "npc") - unit(1.5, "lines"), just = c("right", 
                                                                                            "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(LSL, y = unit(0.95, "npc") - unit(1.5, 
                                                    "lines"), just = c("left", "top"), 
                  gp = grid::gpar(cex = 0.8))
  grid::grid.text(expression(bold("Target: ")), y = unit(0.95, 
                                                         "npc") - unit(2.5, "lines"), just = c("right", 
                                                                                               "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(Target, y = unit(0.95, "npc") - unit(2.5, 
                                                       "lines"), just = c("left", "top"), 
                  gp = grid::gpar(cex = 0.8))
  grid::grid.text(expression(bold("USL: ")), y = unit(0.95, 
                                                      "npc") - unit(3.5, "lines"), just = c("right", 
                                                                                            "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(USL, y = unit(0.95, "npc") - unit(3.5, 
                                                    "lines"), just = c("left", "top"), 
                  gp = grid::gpar(cex = 0.8))
  grid::popViewport()
  vpProcess <- grid::viewport(name = "proc", layout.pos.row = 3, 
                              layout = grid::grid.layout(1, 2))
  grid::pushViewport(vpProcess)
  grid::grid.lines(x = c(0, 1), y = c(1, 1), gp = grid::gpar(col = "#BBBBBB", 
                                                             lwd = 3))
  grid::grid.text(expression(bold("Process")), y = 0.95, 
                  just = c("center", "top"))
  vpSTp <- grid::viewport(layout.pos.col = 1)
  grid::pushViewport(vpSTp)
  grid::grid.text("Short Term", x = 0.05, y = 0.95, just = c("left", 
                                                             "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(expression(bold("Mean: ")), y = unit(0.95, 
                                                       "npc") - unit(1.5, "lines"), just = c("right", 
                                                                                             "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(sprintf("%.4f", mST), y = unit(0.95, 
                                                 "npc") - unit(1.5, "lines"), just = c("left", 
                                                                                       "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(expression(bold("SD: ")), y = unit(0.95, 
                                                     "npc") - unit(2.5, "lines"), just = c("right", 
                                                                                           "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(sprintf("%.4f", sST), y = unit(0.95, 
                                                 "npc") - unit(2.5, "lines"), just = c("left", 
                                                                                       "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(expression(bold("n: ")), y = unit(0.95, 
                                                    "npc") - unit(3.5, "lines"), just = c("right", 
                                                                                          "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(nST, y = unit(0.95, "npc") - unit(3.5, 
                                                    "lines"), just = c("left", "top"), 
                  gp = grid::gpar(cex = 0.8))
  grid::grid.text(expression(bold(Z[s] * ": ")), y = unit(0.95, 
                                                          "npc") - unit(4.5, "lines"), just = c("right", 
                                                                                                "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(sprintf("%.2f", zST), y = unit(0.95, 
                                                 "npc") - unit(4.5, "lines"), just = c("left", 
                                                                                       "top"), gp = grid::gpar(cex = 0.8))
  grid::popViewport()
  vpLTp <- grid::viewport(layout.pos.col = 2)
  grid::pushViewport(vpLTp)
  grid::grid.text("Long Term", x = 0.95, y = 0.95, just = c("right", 
                                                            "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(expression(bold("Mean: ")), y = unit(0.95, 
                                                       "npc") - unit(1.5, "lines"), just = c("right", 
                                                                                             "top"), gp = grid::gpar(cex = 0.8))
  if (!is.na(mLT)) {
    grid::grid.text(sprintf("%.4f", mLT), y = unit(0.95, 
                                                   "npc") - unit(1.5, "lines"), just = c("left", 
                                                                                         "top"), gp = grid::gpar(cex = 0.8))
  }
  grid::grid.text(expression(bold("SD: ")), y = unit(0.95, 
                                                     "npc") - unit(2.5, "lines"), just = c("right", 
                                                                                           "top"), gp = grid::gpar(cex = 0.8))
  if (!is.na(sLT)) {
    grid::grid.text(sprintf("%.4f", sLT), y = unit(0.95, 
                                                   "npc") - unit(2.5, "lines"), just = c("left", 
                                                                                         "top"), gp = grid::gpar(cex = 0.8))
  }
  grid::grid.text(expression(bold("n: ")), y = unit(0.95, 
                                                    "npc") - unit(3.5, "lines"), just = c("right", 
                                                                                          "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(nLT, y = unit(0.95, "npc") - unit(3.5, 
                                                    "lines"), just = c("left", "top"), 
                  gp = grid::gpar(cex = 0.8))
  grid::grid.text(expression(bold(Z[s] * ": ")), y = unit(0.95, 
                                                          "npc") - unit(4.5, "lines"), just = c("right", 
                                                                                                "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(sprintf("%.2f", zLT), y = unit(0.95, 
                                                 "npc") - unit(4.5, "lines"), just = c("left", 
                                                                                       "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(expression(bold("DPMO: ")), y = unit(0.95, 
                                                       "npc") - unit(5.5, "lines"), just = c("right", 
                                                                                             "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(round(DPMO, 1), y = unit(0.95, "npc") - 
                    unit(5.5, "lines"), just = c("left", "top"), 
                  gp = grid::gpar(cex = 0.8))
  grid::popViewport()
  grid::popViewport()
  vpIndices <- grid::viewport(name = "ind", layout.pos.row = 4, 
                              layout = grid::grid.layout(1, 2))
  grid::pushViewport(vpIndices)
  grid::grid.lines(x = c(0, 1), y = c(1, 1), gp = grid::gpar(col = "#BBBBBB", 
                                                             lwd = 2))
  grid::grid.text(expression(bold("Indices")), y = 0.95, 
                  just = c("center", "top"))
  vpSTi <- grid::viewport(layout.pos.col = 1)
  grid::pushViewport(vpSTi)
  grid::grid.text("Short Term", x = 0.05, y = 0.95, just = c("left", 
                                                             "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(expression(bold(C[p] * ": ")), y = unit(0.95, 
                                                          "npc") - unit(1.5, "lines"), just = c("right", 
                                                                                                "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(sprintf("%.4f", cpST), y = unit(0.95, 
                                                  "npc") - unit(1.5, "lines"), just = c("left", 
                                                                                        "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(expression(bold("CI: ")), y = unit(0.95, 
                                                     "npc") - unit(3, "lines"), just = c("right", 
                                                                                         "top"), gp = grid::gpar(cex = 0.7))
  grid::grid.text(paste("[", paste(sprintf("%.1f", 
                                           cpiST[1]), sep = ""), ",", sprintf("%.1f", 
                                                                              cpiST[2]), "]", sep = ""), y = unit(0.95, 
                                                                                                                  "npc") - unit(3, "lines"), just = c("left", 
                                                                                                                                                      "top"), gp = grid::gpar(cex = 0.7))
  grid::grid.text(expression(bold(C[pk] * ": ")), y = unit(0.95, 
                                                           "npc") - unit(4.5, "lines"), just = c("right", 
                                                                                                 "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(sprintf("%.4f", cpkST), y = unit(0.95, 
                                                   "npc") - unit(4.5, "lines"), just = c("left", 
                                                                                         "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(expression(bold("CI: ")), y = unit(0.95, 
                                                     "npc") - unit(6.5, "lines"), just = c("right", 
                                                                                           "top"), gp = grid::gpar(cex = 0.7))
  grid::grid.text(paste("[", paste(sprintf("%.1f", 
                                           cpkiST[1]), sep = ""), ",", sprintf("%.1f", 
                                                                               cpkiST[2]), "]", sep = ""), y = unit(0.95, 
                                                                                                                    "npc") - unit(6.5, "lines"), just = c("left", 
                                                                                                                                                          "top"), gp = grid::gpar(cex = 0.7))
  grid::popViewport()
  vpLTi <- grid::viewport(layout.pos.col = 2)
  grid::pushViewport(vpLTi)
  grid::grid.text("Long Term", x = 0.95, y = 0.95, just = c("right", 
                                                            "top"), gp = grid::gpar(cex = 0.8))
  grid::grid.text(expression(bold(P[p] * ": ")), y = unit(0.95, 
                                                          "npc") - unit(1.5, "lines"), just = c("right", 
                                                                                                "top"), gp = grid::gpar(cex = 0.8))
  if (!is.na(cpLT)) {
    grid::grid.text(sprintf("%.4f", cpLT), y = unit(0.95, 
                                                    "npc") - unit(1.5, "lines"), just = c("left", 
                                                                                          "top"), gp = grid::gpar(cex = 0.8))
  }
  grid::grid.text(expression(bold("CI: ")), y = unit(0.95, 
                                                     "npc") - unit(3, "lines"), just = c("right", 
                                                                                         "top"), gp = grid::gpar(cex = 0.7))
  if (!is.na(cpiLT[1])) {
    grid::grid.text(paste("[", paste(sprintf("%.1f", 
                                             cpiLT[1]), sep = ""), ",", sprintf("%.1f", 
                                                                                cpiLT[2]), "]", sep = ""), y = unit(0.95, 
                                                                                                                    "npc") - unit(3, "lines"), just = c("left", 
                                                                                                                                                        "top"), gp = grid::gpar(cex = 0.7))
  }
  grid::grid.text(expression(bold(P[pk] * ": ")), y = unit(0.95, 
                                                           "npc") - unit(4.5, "lines"), just = c("right", 
                                                                                                 "top"), gp = grid::gpar(cex = 0.8))
  if (!is.na(cpkLT)) {
    grid::grid.text(sprintf("%.4f", cpkLT), y = unit(0.95, 
                                                     "npc") - unit(4.5, "lines"), just = c("left", 
                                                                                           "top"), gp = grid::gpar(cex = 0.8))
  }
  grid::grid.text(expression(bold("CI: ")), y = unit(0.95, 
                                                     "npc") - unit(6.5, "lines"), just = c("right", 
                                                                                           "top"), gp = grid::gpar(cex = 0.7))
  if (!is.na(cpkiLT[1])) {
    grid::grid.text(paste("[", paste(sprintf("%.1f", 
                                             cpkiLT[1]), sep = ""), ",", sprintf("%.1f", 
                                                                                 cpkiLT[2]), "]", sep = ""), y = unit(0.95, 
                                                                                                                      "npc") - unit(6.5, "lines"), just = c("left", 
                                                                                                                                                            "top"), gp = grid::gpar(cex = 0.7))
  }
  grid::popViewport()
  grid::popViewport()
}
