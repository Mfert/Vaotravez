library(ggplot2) # visualization

install.packages("ggrepel")
install.packages("ggthemes")
install.packages("VIM")

install.packages("plotly")
install.packages("GGally")
install.packages("caret")

install.packages("formattable")

library(ggrepel)
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(VIM)
library(data.table)
library(formattable)
library(plotly)
library(corrplot)
library(GGally)
library(caret)
library(car)

read.csv("file:///C:/Users/proceso/Desktop/R-TAREAS/movie_metadata_2.csv")

peliculas <- read.csv("file:///C:/Users/proceso/Desktop/R-TAREAS/movie_metadata_2.csv")

#Se sacan los datos del datafreame
head(peliculas, 2)
class(peliculas)
str (peliculas)

#Se buscan los duplicados
sum(duplicated(peliculas))

peliculas <- peliculas[!duplicated(peliculas), ]

names(peliculas)

head(peliculas$genres)

#Se eliminan los caracteres especiales de los titulos
head(peliculas$movie_title)

library(stringr)
peliculas$movie_title <- gsub("Â", "", as.character(factor(peliculas$movie_title)))
str_trim(peliculas$movie_title, side = "right")

peliculas <- select(peliculas, movie_title, director_name, genres, title_year, language, country, plot_keywords, actor_1_name,             
                    actor_1_facebook_likes, actor_2_name, actor_2_facebook_likes, gross, num_voted_users, num_critic_for_reviews, num_user_for_reviews,  budget,                 
                    imdb_score, movie_facebook_likes)

names(peliculas)

#Se eliminan los NA, de laa diferentes columnas
colSums(sapply(peliculas, is.na))
dim()

peliculas <- peliculas[!is.na(peliculas$gross), ]
peliculas <- peliculas[!is.na(peliculas$budget), ]
dim(peliculas)

colSums(sapply(peliculas, is.na))

sum(complete.cases(peliculas))

#se crean dós variables nuevas, sobre el presupuesto y la ganancia en taquilla
peliculas <- peliculas %>% mutate(beneficio = gross - budget,
         return_on_investment_perc = (beneficio/budget)*100)

#Se empiezana a visualizar los datos
table(peliculas$language)

table(peliculas$country)



levels(peliculas$country) <- c(levels(peliculas$country), "Others")
peliculas$country[(peliculas$country != 'USA')&(peliculas$country != 'UK')] <- 'Others' 
peliculas$country <- factor(peliculas$country)
table(peliculas$country)



#Se revisan los años de producción de las pelíulas

table(peliculas$title_year)


ggplot(peliculas, aes(title_year)) +
  geom_bar() +
  labs(x = "Año de producción", y = "Movie Count", title = "En que año se producieron más películas") +
  theme(plot.title = element_text(hjust = 0.5))



#Realizacíon de Histogramas de frecuencia 

hist(x = peliculas$gross)

hist(x = peliculas$gross, main = "Histograma de Ganancias en la taquilla", 
     xlab = "Ganancias", ylab = "Frecuencia",
     col = "blue")


hist(peliculas$title_year)

hist(x = peliculas$title_year, main = "Histograma de Películas producidad por año", 
     xlab = "Año", ylab = "Frecuencia",
     col = "RED")


plot(x = peliculas$title_year)

peliculas %>%
  filter(title_year %in% c(2000:2016)) %>%
  arrange(desc(beneficio)) %>%
  top_n(20, beneficio) %>%
  ggplot(aes(x=budget/1000000, y=beneficio/1000000)) +
  geom_point() +
  geom_smooth() + 
  geom_text_repel(aes(label=movie_title)) +
  labs(x = "Budget $million", y = "Beneficio $million", title = "Top 10 Profitable Movies") +
  theme(plot.title = element_text(hjust = 0.5))

peliculas %>%
  filter(title_year %in% c(1980:2016)) %>%
  arrange(desc(beneficio)) %>%
  top_n(20, beneficio) %>%
  ggplot(aes(x=budget/1000000, y=beneficio/1000000)) +
  geom_point() +
  geom_smooth() + 
  geom_text_repel(aes(label=movie_title)) +
  labs(x = "Presupuesto$million", y = "Beneficios $million", title = "Top 20 de las peliculas basadas en sus ganancias") +
  theme(plot.title = element_text(hjust = 0.5))



peliculas %>%
  filter(title_year %in% c(1980:2016)) %>%
  arrange(desc(beneficio)) %>%
  top_n(20, beneficio) %>%
  ggplot(aes(x=num_critic_for_reviews/1000000, y=beneficio/1000000)) +
  geom_point() +
  geom_smooth() + 
  geom_text_repel(aes(label=movie_title)) +
  labs(x = "Criticas$million", y = "Beneficios $million", title = "Top 20 de las peliculas basadas en sus Criticas") +
  theme(plot.title = element_text(hjust = 0.5))


peliculas %>%
  filter(title_year %in% c(1980:2016)) %>%
  arrange(desc(num_voted_users)) %>%
  top_n(20, num_voted_users) %>%
  ggplot(aes(x=num_critic_for_reviews/10000, y=num_voted_users/10000)) +
  geom_point() +
  geom_smooth() + 
  geom_text_repel(aes(label=movie_title)) +
  labs(x = "Criticas", y = "Votos del publico", title = "Top 20 de las peliculas basadas en sus Criticas") +
  theme(plot.title = element_text(hjust = 0.5))





peliculas %>%
  filter(budget > 100000) %>%
  mutate(beneficio = gross - budget,
         return_on_investment_perc = (beneficio/budget)*100) %>%
  arrange(desc(beneficio)) %>%
  top_n(25, beneficio) %>%
  ggplot(aes(x=budget/1000000, y = return_on_investment_perc)) + 
  geom_point(size = 2) + 
  geom_smooth(size = 1) + 
  geom_text_repel(aes(label = movie_title), size = 3) + 
  xlab("Presupuesto$million") + 
  ylab("Porcentaje de retorno de Inversion") + 
  ggtitle("20 Mejores peliculas basadas en su retorno de Inversión ")



table(peliculas$imdb_score)

mean( peliculas$imdb_score)
median(peliculas$imdb_score)

mode(peliculas$imdb_score)

var(peliculas$imdb_score)
sd(peliculas$imdb_score)
summary(peliculas$imdb_score)


hist(x = peliculas$imdb_score, main = "Histograma de puntaje en IMDb", 
     xlab = "Puntaje en IMDb", ylab = "Frecuencia",
     col = "yellow")

peliculas %>%
  group_by(director_name) %>%
  summarise(avg_peliculas = mean(imdb_score)) %>%
  arrange(desc(avg_peliculas)) %>%
  top_n(10, avg_peliculas) %>%
  formattable(list(avg_peliculas = color_bar("skyblue")), align = 'l')

peliculas %>%
  group_by(director_name) %>%
  summarise(avg_peliculas = mean(num_voted_users)) %>%
  arrange(desc(avg_peliculas)) %>%
  top_n(10, avg_peliculas) %>%
  formattable(list(avg_peliculas = color_bar("green")), align = 'l')


              




install.packages("sparkline")

library(sparkline)
sparkline(c(1,2,7,6,5), type = "tristate", barColor = "green")


df = data.frame("Type" = c("bar", "line", "bullet", "pie", "tristate", "discrete"),
                Sparkline = c(as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "bar"))), 
                              as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "line"))), 
                              as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "bullet"))), 
                              as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "pie"))), 
                              as.character(htmltools::as.tags(sparkline(c(-1,0,1,1,1,-1,0,2), type = "tristate"))), 
                              as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "discrete")))))
out = as.htmlwidget(formattable(df))
out$dependencies = c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
out


peliculas %>%
  group_by(director_name) %>%
  summarise(avg_peliculas = mean(num_critic_for_reviews)) %>%
  arrange(desc(avg_peliculas)) %>%
  top_n(10, avg_peliculas) %>%
  formattable(list(avg_peliculas = color_bar("red")), align = 'l')


peliculas %>%
  group_by(movie_title) %>%
  summarise(avg_peliculas = mean(num_voted_users)) %>%
  arrange(desc(avg_peliculas)) %>%
  top_n(10, avg_peliculas) %>%
  formattable(list(avg_peliculas = color_bar("pink")), align = 'l')


peliculas %>%
  group_by(movie_title) %>%
  summarise(avg_peliculas = mean(imdb_score)) %>%
  arrange(desc(avg_peliculas)) %>%
  top_n(10, avg_peliculas) %>%
  formattable(list(avg_peliculas = color_bar("yellow")), align = 'l')


peliculas %>%
  top_n(15, beneficio) %>%
  ggplot(aes(x = imdb_score, y = gross/10^6, size = beneficio/10^6, color = num_user_for_reviews)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 600)) + 
  geom_vline(aes(xintercept = 7.75)) + 
  geom_text_repel(aes(label = movie_title), size = 4) +
  xlab("Imdb score") + 
  ylab("Gross money earned in million dollars") + 
  ggtitle("Éxito comercial vs aclamación de la crítica") +
  annotate("text", x = 8.5, y = 700, label = "High ratings \n & High gross") +
  theme(plot.title = element_text(hjust = 0.5))


peliculas %>%
  plot_ly(x = ~movie_facebook_likes, y = ~imdb_score, color = ~num_voted_users , mode = "markers", text = ~num_voted_users, alpha = 0.7, type = "scatter")
  

