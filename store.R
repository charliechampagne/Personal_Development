# Read multiple csv files in as one data frame
files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)

df <- vector("list", length(files))

for (i in seq_along(files)) {
  data <- read_csv(files[[i]], col_names = TRUE)
  df[[i]] <- data
}

bindrows(df)

#-----------------------------------------------------------------------

# This code finds the number of rolls of a dice to get two (6)
die <- c(1:6)
roll <- function() {sample(die, 2, replace = TRUE)}

rolls <- 0
nsix <- 0

while(nsix < 1){
  six <- roll()
  if (!(n_distinct(six) == 2) & six[[1]] == 6){
    nsix <- nsix + 1
  } else {
    nsix <- 0
  }
  rolls <- rolls + 1
}
rolls





# This function rescales vectors to a range of 0 to 1
rescale01 <- function(x){
  rng <- range(x, na.rm = TRUE)
  (x - rng[1])/ (rng[2] - rng[1])
}




# This code creates the nursery rhyme 'Ten in the Bed'
n = 20
numbers <- c(n:1)
for (i in numbers) {
  if (i != 1){
    cat(
      str_c("There were ", i, " in the bed\n",
            "And the little one said,\n",
            "\"Roll over! Roll over!\"\n",
            "So they all rolled over\n",
            "And one fell out\n"), "\n"
    )
  } else {
    cat(str_c(
      "There was one in the bed\n",
      "And the little one said,\n",
      "Good night!"
    ))
  }
  remove(i)
}





# Code for the nursery rhyme 'Alice the camel'

lyrics <- vector("character", 5)
last <- 
  "Alice the camel has no humps.
Alice the camel has no humps.
Alice the camel has no humps.
‘Cause Alice is a horse, of course!"

out <- c("five", "four", "three", "two", "one")

# This function creates Alice the camel three times
alice_camel <- function(number) {
  alice_text <- vector("character", 3)
  for (i in seq_along(1:3)) {
    alice_text[i] <- str_c("Alice the camel has", number, "humps.\n", sep = " ")
  }
  alice_text = str_c(alice_text, collapse = "")
  alice_text
}

# This function creates all the paragraphs using a loop
each_paragraph <- function(out) {
  for (i in seq_along(1:5)) {
    temp <- alice_camel(out[i])
    lyrics[i] <- 
      str_c(temp, "So go, Alice, go!\n", "‘Cause Alice is a horse, of course!\n\n", sep = "")
  }
  #lyrics <- str_c(lyrics, collpase = "")
  #lyrics <- str_c(lyrics, last)
  #empty_string <- vector("character", 5)
  lyrics = paste(lyrics, sep="", collapse="") 
  lyrics = paste(lyrics, last, sep = "", collapse = "")
  lyrics
}



temp <- seq(-10, 50, by = 5)
x <- cut(temp, c(-Inf, 0, 10, 20, 30, Inf),
         right = FALSE,
         labels = c("freezing", "cold", "cool", "warm", "hot")
)

df <- tibble(degree = temp, feeling = x)



# Example of switch statement
operation <- function(x, y, op){
  result = switch(op,
                  "plus" = x + y,
                  "minus" = x - y,
                  "times" = x * y,
                  "divide" = x / y,
                  stop("Unknown op!")
  )
  print(result)
}



# rescale a set of numbers to the range 0 - 1
rescale01 <- function(x){
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}


# vector of dates giving the first day of every month of 2022
jan2022 <- rep(ymd(20220101), times = 12)
offset <- c(0:11)
month_start <- jan2022 + months(offset)


# This function takes a birthday as input and return's a person's age as output
age <- function(birthday){
  current_age = today() - ymd(birthday)
  current_age = as.duration(current_age)
  current_age = as.period(current_age)
  current_age = as.character(current_age)
  cat(c("You are", current_age, "years old."))
}



polparty <- gss_cat %>% 
  mutate(
    partyid = fct_collapse(
      partyid,
      Democrat = c("Not str democrat", "Strong democrat"),
      Republican = c("Strong republican", "Not str republican"),
      Independent = c("Ind,near rep", "Independent", "Ind,near dem"),
      Other = c("No answer", "Don't know", "Other party")
    )
  ) %>% 
  group_by(year, partyid) %>% 
  summarise(
    n = n(),
  ) %>% 
  mutate(prop = n / sum(n) * 100) %>% 
  filter(!(partyid == "Other"))

ggplot(polparty, aes(year, prop)) + 
  geom_line(aes(color = partyid))

gss_cat %>% count(partyid)



patt <- "\\b([^ ]?)(.*)(.$?)\\b"

str_view_all(words, patt)

new_words <- str_replace(words, patt, "\\3\\2\\1")
head(new_words, 10)

words[new_words == words]

intersect(words, new_words)



tibble(sentence = sentences) %>% 
  extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)",
    remove = FALSE
  )


number <- 
  "(\\bone\\b|\\btwo\\b|\\bthree\\b|\\bfour\\b|\\bfive\\b|\\bsix\\b|\\bseven\\b|\\beight\\b|\\bnine\\b|\\bten\\b) ([^ ]+)"
head(str_match(sentences, number))

tibble(sentence = sentences) %>% 
  extract(
    sentence, c("number", "word"), number, remove = FALSE
  ) %>% filter(!is.na(number))


gogl <- "([^ ]+)(\\')([^ ]+)"
string <- "It\'s a boy that\'s really good"
s <- tibble(string = string)
extract(df, sentence, into = c("main", "div", "minor"), regex = gogl) %>% 
  filter(!is.na(main))
str_view_all(sentences, gogl, match = TRUE)


noun <- "(a|the) ([^ ]+)"
has_noun <- sentences %>% 
  str_subset(noun) %>% 
  head(10)
has_noun %>% str_match(noun)


color <- c("\\bblue\\b", "\\bred\\b", "\\borange\\b", "\\bgreen\\b", "\\byellow\\b", "\\bpurple\\b")
color_match <- str_c(color, collapse = "|")

colored_sentences <- str_subset(sentences, color_match)
more <- colored_sentences[(str_count(colored_sentences, color_match) > 1)]
str_view_all(more, color_match)


(whobase <- who %>% 
  gather(key = "key", value = "cases",
         new_sp_m014:newrel_f65,
         na.rm = TRUE) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>% 
  separate(key, into = c("new", "type", "sexage"), sep = "_") %>% 
  separate(sexage, into = c("sex", "age"), sep = 1) %>% 
  select(-iso2, -iso3, -new, -age)
)

whoyear <- whobase %>% 
  group_by(country, year, sex) %>% 
  summarise(total_cases_sex = sum(cases)) %>% 
  filter(total_cases_sex > 100000)


ggplot(whoyear,aes(year, total_cases_sex, color = country)) + 
  geom_line() + 
  facet_wrap(~sex)


temp <- diamonds %>% 
  group_by(color) %>% 
  summarise(n = n())

View(temp)

diamonds %>% 
  count(color)

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) + 
  geom_histogram(binwidth = 0.01)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) + 
  coord_cartesian(ylim = c(0, 50))

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(
    mapping = aes(color = cancelled),
    binwidth = 1/4
  )

df <- tibble(
  x = rep(c("a", "b", "c"), times = c(2, 5, 10)),
  y = rep(c("fair", "good", "ideal"), times = c(2, 5, 10)),
)
ggplot(df) + 
  geom_count(mapping = aes(x = x, y = y))

View(df)

temp <- flights %>% 
  group_by(dest, month) %>% 
  summarise(delay_mean = mean(dep_delay, na.rm = TRUE),
            n = n()) %>% 
  mutate(sum_n = sum(n)) %>% 
  select(dest, month, delay_mean, n, sum_n) %>% 
  as.data.frame() %>% 
  filter(dest == "ABQ") %>% 
  ggplot(mapping = aes(x = as.factor(month), y = dest)) + 
  geom_tile(aes(fill = delay_mean))

lung <- read.csv(
  "~/Data_Science/Essex/Bruce P. Practical Statistics for Data Scientists...Using R and Python 2ed 2020/practical-statistics-for-data-scientists-master/data/LungDisease.csv"
)



df <- tibble(x = c(1:10), y = 3 - 5*x)

m <- lm(y ~ x, data = df)


library(modelr)

mod <- lm(price ~ carat, data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod)

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))

ggplot(diamonds2) + 
  geom_boxplot(aes(cut, resid))


library(tidyverse)

ggplot(diamonds, aes(carat, price)) + 
  geom_hex()

ggsave("diamonds.pdf")

write.csv(diamonds, "diamonds.csv")

tibble(
  x = 1:5,
  y = 1,
  z = x ^ 2 + y
)

tribble(
  ~x, ~y, ~z,
  #--/--/----
  "a", 2, 3.6,
  "b", 1, 8.5
)

tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

df <- tibble(
  x = runif(5),
  y = rnorm(5)
)


table <- read_csv("table2.csv")

cases <- table %>% 
  filter(type == "cases")

population <- table %>% 
  filter(type == "population")

temp <- tibble(
  country = cases$country,
  year = cases$year,
  type = cases$type,
  cases = cases$count,
  population = population$count
)

temp <- temp %>% 
  mutate(rate = cases / population * 10000)

View(temp)


t1 <- tibble(
  x = c("a", "b", "c"),
  y = c(1, 2, 3),
  z = c(10, 20, 30)
)


t2 <- tibble(
  x = c("a", "b", "c"),
  y = c(1.1, 2.2, 3.3),
  z = c(10.1, 20.2, 30.3)
)

t3 <- tibble(x = c(t1$x, t2$x))


d1 <- tibble(
  x = c("a", "b", "c", "a", "b", "c"),
  y = c("one", "two", "one", "two", "one", "two"),
  z = c(1, 2, 3, 4, 5, 6)
)

temp <- d1 %>% 
  spread(y, z)

d2 <- temp %>% 
  gather(2, 3, key = "y", value = "z")


shoes <- tibble(
  names = c("c", "f", "r", "c", "f", "r"),
  shoes = c("b", "w", "b", "w", "b", "w"),
  count = c(1, 2, 3, 4, 5, 6)
)

bata <- shoes %>% 
  spread(shoes, count)

nike <- bata %>% 
  gather(2, 3, key = "color", value = "count")


flights3 <- flights %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(dest) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
  ) %>% 
  filter(avg_delay > 0)

flights3 %>% 
  left_join(airports, by = c("dest" = "faa")) %>% 
  ggplot(aes(lon, lat, size = avg_delay)) + 
  borders("state") +
  geom_point(aes(color = avg_delay)) +
  coord_quickmap()


x <- c("Americans")
mid <-  str_length(x)/2
ifelse((mid%%1 == 0) , mid, ceiling(mid))
str_sub(
  x, 
  ifelse((mid%%1 == 0) , mid, ceiling(mid)),
  ifelse((mid%%1 == 0) , mid+1, ceiling(mid))
)