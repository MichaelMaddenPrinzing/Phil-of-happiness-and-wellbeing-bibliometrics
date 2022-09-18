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

# Remove those as well
d <- d %>% 
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
# Pull the citations from each document, and classify the sources as scientific or non-scientific
#——————————————————————————————————————————————————————————————————————————————————————————————————
cited <- citations(d) # Pull the cited documents
length(unique(cited$Source)) # There are 7,389 unique sources
sources <- as_tibble(cited$Source) %>% table() %>% as_tibble() %>% arrange(desc(n))
names(sources) <- c("source", "count")
write.csv(sources, file = "sources.csv", row.names = F)
# I used sources.csv to manually sources that received at least 5 citations. That cuts the total
# down from 7,389 to 318. Of these 111 proved to be scientific sources. 5 could not be 
# categorized because it was impossible to determine, from the abbreviated title, what 
# exactly what the source was or whether it was scientific (P BRIT ACAD, 
# CRITICAL NE IN PRESS, DROP, VALUE ETHICS EC, WELL BEING).

# Import the categorized source list
sciSource <- read_csv("Data/Categorized Sources.csv") %>%
  filter(scientific==1) %>% 
  pull(source)

# 8 papers have no cited sources. Maybe an error in the database? But in any case,
# there's no choice but to exclude them from analysis.
nrow(d[which(is.na(d$CR)), ])
d <- d[which(!is.na(d$CR)), ]


d$sciRefs <- 0
for(i in 1:nrow(d)) {
  s <- sapply(strsplit(unlist(strsplit(d[i, ]$CR, split=";")), split = ", "), `[`, 3)
  d[i, ]$sciRefs <- length(which(s %in% sciSource))
}

#——————————————————————————————————————————————————————————————————————————————————————————————————
# Results
#——————————————————————————————————————————————————————————————————————————————————————————————————
# Create a decade variable
d$decade <- ifelse(d$PY < 1950, 0,
            ifelse(d$PY < 1960, 1,
            ifelse(d$PY < 1970, 2,
            ifelse(d$PY < 1980, 3,
            ifelse(d$PY < 1990, 4,
            ifelse(d$PY < 2000, 5,
            ifelse(d$PY < 2010, 6,
            ifelse(d$PY < 2020, 7, 8))))))))

# Compute papers/year, both total and that cite science
dAgg <- d %>% 
  group_by(PY) %>% 
  summarise(wbPapers = length(TI),
            citeSci = length(which(sciRefs > 0)))

# Compute by decade, including the proportion of total papers that cite science
dDec <- d %>%
  group_by(decade) %>% 
  summarise(wbPapers = length(TI),
            propSci = length(which(sciRefs > 0)) / wbPapers * 100)

# Import total publication volume from the top 50 journals
phil <- read_csv("Data/Total Philosophy Publications.csv")

# Total papers published in the top 50 philosophy journals
ggplot(phil %>% filter(year >=1947 & year < 2022), aes(year, publications)) +
  geom_point() + geom_smooth(se=F, color="black") + 
  theme_minimal() + 
  labs(x="Year", y="Papers", title="Total publications")


# Happiness and well-being papers
ggplot(dAgg %>% pivot_longer(cols=c("wbPapers", "citeSci")),
       aes(PY, value, color=name)) +
  geom_point() + geom_smooth(se=F) + 
  scale_color_discrete(labels=c("Citing science", "Total")) +
  theme_minimal() + 
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs(x="Year", y="Papers", title="Happiness & Well-Being Publications")
# Something really dramatic happened around the turn of the millennium.


# The proportion that cite science by decade
ggplot(dDec, aes(x=decade, y=propSci)) +
  geom_point() + 
  geom_smooth(se=F,color="black",
              method="lm", formula= y ~ x + poly(x, 2)) +
  scale_x_continuous(breaks=0:8,
                     labels=c("1940s","1950s","1960s","1970s","1980s",
                              "1990s","2000s","2010s","2020s")) +
  theme_minimal() + 
  labs(x="Decade", y="Percentage",
       title="Percentage citing scientific sources")