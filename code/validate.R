# Input cicloexpresso data
comboios = sf::st_read("data/comboios.gpkg")
tm_shape(comboios) + tm_lines("name", lwd = 2)

# Could also add data for extra routes in Almada, etc

# Make a buffer around each line and compare the centroids within the buffer to the centroids within an equivalent buffer around the candidate bike bus routes