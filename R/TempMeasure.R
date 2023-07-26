
temp.data <- read_xlsx('../data/bulbtemp.xlsx')

temp.data$time <- strptime(temp.data$time, format="%Y-%m-%d %H:%M") # time is not in UTC

temp.data$Device <- as.character(temp.data$Device)

location_order <- c("BKW Park", "Victoria Platz", "Kornhaus Brücke", "Kornhaus", "Zytglogge", "Rathausgasse", "Rathausplatz")

# Convert the component variable to a factor with the desired order
temp.data$location <- factor(temp.data$location, levels = location_order)

WBGT <- ggplot(temp.data, aes(x = location, y = LUBGT, group = Device, color = Device))+
  xlab("Location") + ylab("WBGT [°C]") +
  guides(color = guide_legend(title = "Nr. of Device")) +
  geom_line(linetype='dashed') +
  geom_point(size = 4) +
  geom_label(
    data=temp.data,
    aes(label=exposure), show.legend = F) +
  theme_light()


pdf('../analysis/graphs_report/WBGT.pdf',
    width = 8, height = 5)
WBGT
dev.off()

TA <- ggplot(temp.data, aes(x = location, y = TA, group = Device, color = Device)) +
  xlab("Location") + ylab("TA [°C]") +
  guides(color = guide_legend(title = "Nr. of Device")) +
  geom_line(linetype='dashed') +
  geom_point(size = 4) +
  geom_label(data=temp.data, aes(label=exposure), show.legend = F) +
  theme_light()

pdf('../analysis/graphs_report/TA.pdf',
    width = 8, height = 5)
TA

dev.off()

TG <- ggplot(temp.data, aes(x = location, y = TG, group = Device, color = Device)) +
  xlab("Location") + ylab("TG [°C]") +
  guides(color = guide_legend(title = "Nr. of Device")) +
  geom_line(linetype='dashed') +
  geom_point(size = 4) +
  geom_label(
    data=temp.data,
    aes(label=exposure), show.legend = F
  ) +
  theme_light()

pdf('../analysis/graphs_report/TG.pdf',
    width = 8, height = 5)
TG
dev.off()

Humidity <- ggplot(temp.data, aes(x = location, y = humidity, group = Device, color = Device)) +
  xlab("Location") + ylab("Humidity [%]") +
  guides(color = guide_legend(title = "Nr. of Device")) +
  geom_line(linetype='dashed') +
  geom_point(size = 4) +
  geom_label(data = temp.data, aes(label=exposure), show.legend = F) +
  theme_light()

pdf('../analysis/graphs_report/Humidity.pdf',
    width = 8, height = 5)
Humidity
dev.off()


prow <- plot_grid(
  WBGT + theme(legend.position="none"),
  TA + theme(legend.position="none"),
  TG + theme(legend.position="none"),
  Humidity + theme(legend.position="none"),
  align = 'vh',
  labels = c("a)", "b)", "c)", "d)"),
  hjust = -1,
  nrow = 2)


legend <- get_legend(
  WBGT +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom"))

final.plot <- plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .1))

pdf('../analysis/graphs_report/final.pdf',
    width = 20, height = 12)
final.plot
dev.off()


# measures radiation nd thus clouds etc. can have big effects even after even one minute

