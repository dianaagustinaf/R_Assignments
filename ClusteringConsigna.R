TP Clustering 

Usando el archivo vinos.csv realizar lo siguiente:

- Importar el archivo en un data frame.

- Realizar un agrupamiento en 3 grupos usando agrupamiento jerárquico y kmeans. Para ello no utilizar la columna bodega.

- Comparar los grupos obtenidos con la columna bodega. Resulta el agrupamiento un buen predictor de la bodega?

- Usando los resultados obtenidos con kmeans realizar un gráfico con lo siguiente:

  * eje x = flavonoides

  * eje y = color_int

  * color = cluster

  * agregar los centroides con otra forma y tamaño pero mismo color para que sea visible.

  * marcar los puntos en que no coincide el cluster con la bodega real (tener en cuenta que el número de cluster es arbitrario y puede no coincidir con la bodega, revisar con table cual cluster corresponde a cada bodega)

- Dividir en un conjunto de entrenamiento del 80% de los datos y el resto para testeo. Realizar knn con k=raiz cuadrada de la cantidad de datos.

- Verificar si el método knn resulta un buen predictor de la bodega.

Entregar el script R utilizado y gráfico resultante.

(ORT Argentina)
