tiempo <- c(4.8,5.2,5.0,4.7,5.3,5.1,4.9,5.0)
t.test(tiempo, mu = 5)

volumen <- c(499.8,500.2,499.9,500.1,500.0,499.7,500.3,500.0)
BSDA::z.test(volumen, sigma.x = sqrt(0.25), mu = 500)

ldl <- c(98,102,105,97,99,104,106,95)
wilcox.test(ldl, mu = 100)

aprobado <- c(1,1,0,1,1,1,0,1,1,1,1,0)
binom.test(sum(aprobado), length(aprobado), p = 0.5, alt = "greater")

nota <- c(78,82,80,72,75,74)
seccion <- factor(rep(c("A","B"), each = 3))
t.test(nota ~ seccion, var.equal = TRUE)

ingreso <- c(1200,1350,1280,1100,1050,1080)
genero <- factor(rep(c("M","F"), each = 3))
t.test(ingreso ~ genero, var.equal = FALSE)

satisf <- c(8,7,9,5,6,4)
turno <- factor(rep(c("Dia","Noche"), each = 3))
wilcox.test(satisf ~ turno)

pas_antes <- c(140,138,142,139,141)
pas_despues <- c(135,132,136,134,133)
t.test(pas_antes, pas_despues, paired = TRUE)

dolor_pre <- c(8,7,9,8,7)
dolor_post <- c(5,4,6,5,4)
wilcox.test(dolor_pre, dolor_post, paired = TRUE)

altura <- c(20,21,19,24,23,25,18,17,19)
fertil <- factor(rep(c("A","B","C"), each = 3))
model <- aov(altura ~ fertil)
summary(model)
TukeyHSD(model)

tiempo_ms <- c(150,160,155,170,175,168,140,145,142)
interfaz <- factor(rep(c("I1","I2","I3"), each = 3))
kruskal.test(tiempo_ms ~ interfaz)

library(tidyr)
pre <- c(7,6,8,7); dur <- c(5,4,6,5); post <- c(6,5,7,6)
datos <- data.frame(id = factor(1:4), pre, dur, post)
datos_long <- pivot_longer(datos, -id, names_to = "tiempo", values_to = "punt")
summary(aov(punt ~ tiempo + Error(id/tiempo), data = datos_long))

sem1 <- c(3,4,3,4); sem2 <- c(4,4,3,5); sem3 <- c(2,3,2,3)
friedman.test(cbind(sem1, sem2, sem3))

tiempo <- c(15,14,16,20,22,21)
ruta <- factor(rep(c("A","B"), each = 3))
var.test(tiempo ~ ruta)

library(car)
peso_perdido <- c(4,5,4,6,7,6,3,2,3)
dieta <- factor(rep(c("D1","D2","D3"), each = 3))
leveneTest(peso_perdido ~ dieta)

resist <- c(12,13,11,10,14,15,13,12)
aleacion <- factor(rep(paste0("Al",1:4), each = 2))
bartlett.test(resist ~ aleacion)

