library(ggplot2)
library(stringr)
library(tidyr)
library(cowplot)
library(emmeans)


# Loading Dataframe -------------------------------------------------------

dat <- read.csv("ContrastValues_BGOF.csv")
dat$Distance <- as.character(dat$Distance)
dat$Distance <- as.numeric(str_replace_all(dat$Distance, "m", ""))
str(dat)

# Converting wide to long form
d <- gather(dat, Contrast, Value, Overall_Contrast:Band3_Contrast, factor_key = T)
# d$Condition <- relevel(d$Condition, ref="OF")
str(d)
levels(d$Contrast)

# Plotting Boxplot --------------------------------------------------------
col <- c("#F44336", "#1E88E5")

# Labelling of facets
contr.labs <- c("Full", "Band 1", "Band 2", "Band 3")
names(contr.labs) <- c("Overall_Contrast", "Band1_Contrast", "Band2_Contrast", "Band3_Contrast")

g <- ggplot(d, aes(x=Contrast, y=Value, colour=Condition, group=Condition)) + 
  geom_point(position = position_dodge(width=0.75), size=2, alpha=0.5, colour="black")
g <- g + geom_point(stat = "summary", fun.y="mean", position = position_dodge(width=0.75), size=4, alpha=0.85) +
  geom_errorbar(stat="summary", fun.data="mean_se", position = position_dodge(width=0.75),  width=0, size=1, alpha=0.85)
g <- g +scale_color_manual(values=col)
g <- g +facet_grid(.~Contrast, scale="free_x", labeller = labeller(Contrast = contr.labs))
g <- g + theme_bw()
g <- g + theme(axis.ticks.x = element_blank(), axis.text = element_text(size=10, colour="black"), 
                     axis.title = element_text(size=12, colour="black"), axis.text.x = element_blank(), axis.title.x = element_blank())
g <- g + theme(panel.grid = element_blank(), panel.background = element_blank())
g <- g + scale_y_continuous(name="Contrast")
g <- g + theme(legend.position = "none")
g <- g + theme(strip.background = element_blank(), strip.text = element_text(size=14, face="bold", colour="black"))
g



# Making a multiplot ------------------------------------------------------

# Need to set up a plot with representative images of the BG and OF, and representative images of the band used
# The plot will have 2 columns.
# First column will have the 2 representative images
# Second column will have 4 images on top and the plot in the bottom


# COlumn 1
bg200 <- ggdraw() + draw_image("BG_200m_Compressed.png")
of200 <- ggdraw() + draw_image("OF_200m_Compressed.png")

rep_img <- plot_grid(bg200, NULL, of200, ncol = 1,
                     rel_heights = c(1,0.001,1),
                     rel_widths = c(0.8, 0.8, 0.8),
                     labels = c("A","","B",""),
                     label_size = 18,
                     label_colour = "white",
                     label_fontfamily = "sans")

# Column 2
# Row 1
con_full <- ggdraw() + draw_image("BG_200m_Compressed_Full.png")
con_b1 <- ggdraw() + draw_image("BG_200m_Compressed_Band1.png")
con_b2 <- ggdraw() + draw_image("BG_200m_Compressed_Band2.png")
con_b3 <- ggdraw() + draw_image("BG_200m_Compressed_Band3.png")

cont_img <- plot_grid(NULL, con_full, NULL, con_b1, NULL, con_b2,NULL,con_b3,NULL, nrow=1, 
                       rel_widths = c(0.9,1,0.2,1, 0.2,1,0.2,1, 0.2))
# Row 2
cont_plot <- plot_grid(cont_img,NULL, g, ncol=1,
                       rel_heights = c(0.3,-0.05,1),
                       labels=c("C",""),
                       label_size = 18,
                       label_fontfamily = "sans")


# Multiplot
final <- plot_grid(rep_img, NULL, cont_plot, nrow=1,
                   rel_widths = c(0.45, 0.001, 0.5))

save_plot("ContrastAnalysis.pdf", final, base_height = 5, base_width = 7.5)
save_plot("ContrastAnalysis.png", final, base_height = 5, base_width = 7.5)
save_plot("ContrastAnalysis.tiff", final, base_height = 5, base_width = 7.5)
save_plot("ContrastAnalysis.svg", final, base_height = 5, base_width = 7.5)


# Analysis ----------------------------------------------------------------

# Running simple LM for overall contrast
m1 <- lm(Value~Condition, data=d[which(d$Contrast=="Overall_Contrast"),])
plot(m1)
# Some increase in variance. Normality is fine
summary(m1)
# OF contrast is significantly less

# Models for band wise contrast
m2 <- lm(Value~Condition+Contrast, data=d[which(d$Contrast=="Band1_Contrast" | d$Contrast=="Band2_Contrast" | d$Contrast=="Band3_Contrast"),])
plot(m2)
# Normality and Variance seems fine
m2int <- lm(Value~Condition*Contrast, data=d[which(d$Contrast=="Band1_Contrast" | d$Contrast=="Band2_Contrast" | d$Contrast=="Band3_Contrast"),])
plot(m2int)
# Variance changes with the fitted value. Normality seems fine

anova(m2, m2int)
# Significant interaction effect
summary(m2int)

emmip(m2int, Condition~Contrast)
emm_cond <- emmeans(m2int, pairwise~Condition|Contrast)
emm_cond



# Output Analysis Summary -------------------------------------------------

sink(file="ContrastAnalysis_Overall_LMResults.txt")
summary(m1)
sink()

sink(file="ContrastAnalysis_Bandwise_LMResults.txt")
anova(m2, m2int)
summary(m2int)
emm_cond
sink()
