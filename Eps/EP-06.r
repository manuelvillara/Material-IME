#Pregunta 1
#Se realizó un ensayo clínico para estudiar la toxina botulínica, una toxina muy potente que puede ser
#usado como medicamento en dosis diminutas, como posible tratamiento para el dolor de espalda crónico.
#Un total de 31 pacientes participaron del estudio, de los cuales 15 fueron asignados aleatoriamente al
#grupo de tratamiento y los otros 16 al grupo de control (placebo). Luego de ocho semanas, 9 personas
#voluntarias del grupo de tratamiento reportaron alivio del dolor, mientras que 2 personas lo hicieron en el
#grupo de control. ¿Qué se podría decir del tratamiento?

# Datos conocidos
n <- 31


#tabla de contigencia
                alivio  dolor   Total
tratamiento1     9        6       15
tratamiento2     2        14      16
Total            11       20      31
   
#tabla de contigencia
                alivio  dolor   Total
tratamiento1     5.32    9.6       15
tratamiento2     5.67    10.32     16
Total            11       20      31


tratamiento <- c(9,6)
control <-c(2,14)

# Se verifica que cada observacion esperada sea mayor a 5 para poder realizar la prueba de homogeneidad
x1 = (sum(tratamiento) * (tratamiento[0] + control[0])) / n
y1 = (sum(tratamiento) * (tratamiento[1] + control[1])) / n
x2 = (sum(control) * (tratamiento[0] + control[0])) / n
y2 = (sum(control) * (tratamiento[1] + control[1])) / n

tratamiento_Esperado <- c(x1, x2)
control_Esperado <- c(x2, y2)

tabla_valores_esperados <- as.table(rbind(tratamiento_Esperado, control_Esperado))
dimnames(tabla_valores_esperados) <- list(grupo = c("Tratamiento ", "Control") , respuesta = c("Alivio", "Dolor"))
print(tabla_valores_esperados)







tabla <- as.table(rbind(tratamiento, control))

dimnames(tabla) <- list(grupo = c("tratamiento ", "control") , respuesta = c("alivio", "dolor"))
t
print(tabla)

# Se realiza prueba chi-cuadrado de homogeneidad
prueba <- chisq.test(tabla)
print(prueba)
