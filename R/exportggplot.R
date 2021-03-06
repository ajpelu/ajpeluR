
# Function modify from https://gist.github.com/sheymann/2399659
# Function to export ggplot2 into 4 formats: png, pdf, eps, jpg

ExportPlot <- function(gplot, filename, width=2, height=1.5) {
    # Export plot PDF, EPS, PNG and 
    # Notice that A4: width=11.69, height=8.27
    ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
    postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
    print(gplot)
    dev.off()
    png(file = paste(filename, '.png', sep=""), width = width * 100, height = height * 100)
    print(gplot)
    dev.off()
    jpeg(file = paste(filename, '.jpg', sep=""), width = width * 100, height = height * 100)
    print(gplot)
    dev.off()
    }
