TP Árboles de Decisión

La base HMDA ("Home Mortgage Disclosure Act Data") contiene datos sobre 2380 solicitudes de hipoteca 
y si fueron denegadas (deny=yes) o no (deny=no).

Se puede leer la especificación de la base en: https://rdrr.io/cran/AER/man/HMDA.html

Se solicita:

- Importar el CSV.

- Importar como factor aquellas columnas que lo sean.

- Discretizar las columnas que no sean factores 
(tener en cuenta que la función C5.0 rechaza valores que contengan el caracter ",")

- Armar un árbol de decisión para determinar si una hipoteca debería ser denegada usando C50 o J48 
con una muestra del 80% de los datos. Analizar su significado. 
Qué es lo primero que deberíamos fijarnos para determinar si otorgar una hipoteca o no?

- Evaluar el árbol con los datos no utilizados en la muestra y comparar con el valor real. Armar su matriz de confusión y analizar.

- Si el costo de otorgar una hipoteca cuando debería haberse denegado es 5 veces que el de denegar una que debería haberse otorgado, 
cuál sería el nuevo arbol de decisión? Analizar la nueva matriz de confusión, tiene sentido el cambio?

