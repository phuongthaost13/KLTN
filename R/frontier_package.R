library(frontier)

# Cobb-Doughlas production frontier
data("front41Data")

cobbDouglas <- sfa(log(output) ~ log(capital) + log(labour),
                   data = front41Data)
summary(cobbDouglas)

# load data about rice producers in the Philippines (panel data)
data("riceProdPhil")
rice <- sfa(log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
            data = riceProdPhil)
summary(rice)

# Error Components Frontier (Battese & Coelli 1992)
# with "true" fixed individual effects and observation-specific efficiencies
riceTrue <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) + 
                   factor( FMERCODE ),  data = riceProdPhil )
summary( riceTrue )

# add data set with information about its panel structure
library( "plm" )
ricePanel <- pdata.frame( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )
