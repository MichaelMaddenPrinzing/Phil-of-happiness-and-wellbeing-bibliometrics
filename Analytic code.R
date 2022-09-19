library(tidyverse)
library(bibliometrix)

#———————————————————————————————————————————————————————————————————————————————————————————————————
# I first went on Journal Citation Reports and got the list of all journals categorized as
# Philosophy. There were 320. They are listed in the file called Philosophy Journals.
# I then sorted the list by total citations (from all time), and selected the top 50.
# Then, I queried Web of Science for all the articles from those 50 journals that included
# the terms happiness, well-being, wellbeing, or "the good life" in the title, abstract, or keywords.
# This yielded 673 records. I exported these as text files.
#———————————————————————————————————————————————————————————————————————————————————————————————————

papers <- c("Data/wellBeingPapers1.txt","Data/wellBeingPapers2.txt")
d <- convert2df(file = papers, dbsource = "wos", format = "plaintext")

# 1 duplicated record: Kagan's "Well-being as enjoying the good"
d %>%
  group_by(AU, TI) %>%
  filter(n()>1) %>%
  select(AU, TI)

d <- d %>% distinct(AU, TI, .keep_all = T) # Remove the duplicate

# Some are book reviews, editor's notes, corrections, etc.
table(d$DT)

d <- d %>% # Remove those as well
  filter(DT %in% c("ARTICLE",
                   "ARTICLE; EARLY ACCESS", 
                   "ARTICLE; PROCEEDINGS PAPER",
                   "DISCUSSION", 
                   "PROCEEDINGS PAPER"))


# Some papers have yet to be assigned to an issue, and so have NA for year
d$PY <- ifelse(is.na(d$PY), 2022, d$PY) # Call them 2022

basicStats <- biblioAnalysis(d, sep = ";")
summary(basicStats)
# Now we have 521 documents


#——————————————————————————————————————————————————————————————————————————————————————————————————
# Pull the citations from each document, classify the sources as scientific or non-scientific and
# then give each paper a scientific references score.
#——————————————————————————————————————————————————————————————————————————————————————————————————
cited <- citations(d) # Pull the cited documents
length(unique(cited$Source)) # There are 7,389 unique sources
sources <- as_tibble(cited$Source) %>% table() %>% as_tibble() %>% arrange(desc(n))
names(sources) <- c("source", "count")
write.csv(sources, file = "sources.csv", row.names = F)
# I used sources.csv to manually code sources that received at least 5 citations. That cuts the 
# count down from 7,389 to 318. Of these 111 proved to be scientific sources. 5 could not be 
# categorized because it was impossible to determine, from the abbreviated title, what exactly the
# source was or whether it was scientific.
# These were: P BRIT ACAD, CRITICAL NE IN PRESS, DROP, VALUE ETHICS EC, WELL BEING.

# Import the categorized source list
sciSource <- read_csv("Data/Categorized Sources.csv") %>%
  filter(scientific==1) %>% 
  pull(source)

# 8 papers have no cited sources. Maybe an error in the database?
nrow(d[which(is.na(d$CR)), ])
# Whatever the reason, they can't be included in the analysis.
d <- d[which(!is.na(d$CR)), ]

# Create a variable for the number of scientific references found in each paper
d$sciRefs <- 0
# For each paper in d, pull the sources from each citation and count the scientific ones
for(i in 1:nrow(d)) {
  s <- sapply(strsplit(unlist(strsplit(d[i, ]$CR, split=";")), split = ", "), `[`, 3)
  d[i, ]$sciRefs <- length(which(s %in% sciSource))
}


#——————————————————————————————————————————————————————————————————————————————————————————————————
# Create a plot with number of publications, in total and about happiness and well-being
#——————————————————————————————————————————————————————————————————————————————————————————————————

# Import total publication counts for each year from the top 50 journals
phil <- read_csv("Data/Total Philosophy Publications.csv") %>%
  filter(year >=1947 & year <= 2022) # Just for the same period

dAgg <- d %>% 
  group_by(PY) %>% 
  summarise(pubsWB = length(TI), # Happiness and well-being papers/year, 
            citeSci = length(which(sciRefs > 0))) %>% # How many cite science?
  full_join(y=phil, by=c("PY" = "year")) %>% # Join with total publication volume
  mutate(pubsT = publications / 100) %>% # For two y-axes, we have to transform one variable
  pivot_longer(cols = c("pubsT", "pubsWB"))

ggplot(dAgg, aes(x=PY, y=value, color=name)) +
  geom_point() + 
  geom_smooth(se=F) +
  scale_color_manual(values=c("#0072B2", "black")) +
  scale_y_continuous(name = "Happiness & Well-Being Publications",
                     sec.axis = sec_axis( trans=~.*100, 
                                          name="Total Philosophy Publications"))+
  theme_minimal() + 
  theme(axis.title.y.left = element_text(color = "black"),
        axis.text.y.left = element_text(color = "black"),
        axis.title.y.right = element_text(color = "#0072B2"),
        axis.text.y.right = element_text(color = "#0072B2"),
        text = element_text(size=14), legend.position = "none") +
  labs(x="Year", title="Papers Published in the 50 Most-Cited Philosophy Journals")


#——————————————————————————————————————————————————————————————————————————————————————————————————
# Create a plot of the proportion of happiness and well-being papers that cite science
#——————————————————————————————————————————————————————————————————————————————————————————————————
# Create a decade variable (too few papers per year in the 20th century)
d$decade <- ifelse(d$PY < 1950, 0,
            ifelse(d$PY < 1960, 1,
            ifelse(d$PY < 1970, 2,
            ifelse(d$PY < 1980, 3,
            ifelse(d$PY < 1990, 4,
            ifelse(d$PY < 2000, 5,
            ifelse(d$PY < 2010, 6,
            ifelse(d$PY < 2020, 7, 8))))))))

# Summarize by decade
dDec <- d %>%
  group_by(decade) %>% 
  summarise(wbPapers = length(TI),
            propSci = length(which(sciRefs > 0)) / wbPapers * 100)


ggplot(dDec, aes(x=decade, y=propSci)) +
  geom_point() + 
  geom_smooth(se=F,color="black",
              method="lm", formula= y ~ x + poly(x, 2)) +
  scale_x_continuous(breaks=0:8,
                     labels=c("1940s","1950s","1960s","1970s","1980s",
                              "1990s","2000s","2010s","2020s")) +
  theme_minimal() + 
  theme(text=element_text(size=14)) + 
  labs(x="Decade", y="Percentage",
       title="Happiness & Well-Being Papers Citing Scientific Sources")
