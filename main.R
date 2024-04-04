
library(dplyr)
library(ggplot2)
library(reshape2)

drug.consumption = read.csv("./drug_consumption.csv", header = TRUE)

drug.list = c(
    "Alcohol",
    "Amphet",
    "Amyl",
    "Benzos",
    "Caff",
    "Cannabis",
    "Choc",
    "Coke",
    "Crack",
    "Ecstasy",
    "Heroin",
    "Ketamine",
    "Legalh",
    "LSD",
    "Meth",
    "Mushrooms",
    "Nicotine",
    "Semer",
    "VSA"
)

clx.to.x = function(clx) {
    as.numeric(sub("^CL", "", clx))
}

drug.consumption = sapply(drug.consumption[,drug.list], clx.to.x)

remove.upper.tri = function(cor.matrix) {
    cor.matrix[upper.tri(cor.matrix)] <- NA
    diag(cor.matrix) <- NA
    cor.matrix
}

reorder = function(cor.matrix) {
    dd <- as.dist((1-cor.matrix)/2)
    hc <- hclust(dd)
    cor.matrix[hc$order, hc$order]
}

drug.consumption.cor = cor(drug.consumption) %>% reorder() %>% remove.upper.tri()

drug.consumption.melt = melt(drug.consumption.cor, na.rm = TRUE)

caption <- "http://archive.ics.uci.edu/dataset/373/drug+consumption+quantified"

ggplot(drug.consumption.melt, aes(x = Var1, y = Var2, fill = value)) +
geom_tile() +
geom_text(aes(label = round(value, 2)), color = "black", size = 2) +
scale_fill_gradient(low = "white", high = "steelblue") + 
labs(x = "Drug", y = "Drug", fill = "Pearson\nCorrelation") +
labs(caption = caption) +
ggtitle("Correlation Between Drug Consumption") +
theme(
    plot.margin = unit(c(0,1,0,1), "cm"), 
    plot.caption = element_text(margin = margin(t = 30)),
    plot.title = element_text(hjust = 0.5), 
    axis.title = element_blank()
    panel.grid.major = element_line(linetype = "dashed"), 
    panel.grid.minor = element_line(linetype = "blank"), 
    axis.text.x = element_text(angle = 90, hjust = 1)) +
coord_fixed()
