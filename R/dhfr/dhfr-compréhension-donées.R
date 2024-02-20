
library(datasets)
data(dhfr)

# head() / tail()
head(dhfr, 5)
tail(dhfr, 5)


# summary()
summary(dhfr)
summary(dhfr$Y)


library(skimr)

skim(dhfr) # Effectue un survol pour afficher des statistiques sommaires

# Regrouper les données par Y (activité biologique) puis effectuer un écrémage
dhfr %>%
  dplyr::group_by(Y) %>%
  skim()


# Diagramme de dispersion
plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol)

plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, col = "red")     # Forme des cercles rouges

plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, col = dhfr$Y)    # Couleur selon Y

plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, col = "red",     # Crée des cercles rouges + Ajoute des étiquettes sur les axes x et y
     xlab = "moe2D_zagreb", ylab = "moe2D_weinerPol")

# Histogramme
hist(dhfr$moe2D_zagreb)
hist(dhfr$moe2D_zagreb, col = "red")   # Makes red bars

# Tracés des caractéristiques
featurePlot(x = dhfr[,2:21],
            y = dhfr$Y,
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")))
