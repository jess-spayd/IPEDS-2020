library(tidyverse)

df <- na.omit(adm2020)

df$admit_rate <- df$ADMSSN / df$APPLCN
df$admit_rate <- df$admit_rate * 100

df$selectivity <- case_when(df$admit_rate <= 25 ~ "highly selective",
                            df$admit_rate <= 75 & df$admit_rate > 25 ~ "selective",
                            df$admit_rate > 75 ~ "not selective")

df$selectivity <- as_factor(df$selectivity)

df$SAT_math_50th <- (df$SATMT25 + df$SATMT75) / 2
df$SAT_eng_50th <- (df$SATVR25 + df$SATVR75) / 2
df$ACT_math_50th <- (df$ACTEN25 + df$ACTEN75) / 2
df$ACT_eng_50th <- (df$ACTMT25 + df$ACTMT75) /2

df_new <- select(df, admit_rate, selectivity, 
                 SAT_eng_50th, SAT_math_50th, 
                 ACT_math_50th, ACT_eng_50th, UNITID)

ggplot(data=df_new, aes(x=admit_rate)) +
  geom_point(aes(y=SAT_eng_50th), color="red") +
  geom_point(aes(y=SAT_math_50th), color="green")

ggplot(data=df_new, aes(x=admit_rate)) +
  geom_point(aes(y=ACT_eng_50th), color="red") +
  geom_point(aes(y=ACT_math_50th), color="green")

ggplot(data=df_new, aes(x=admit_rate)) +
  geom_point(aes(y=ACT_eng_50th))

ggplot(data=df, aes(x=admit_rate)) +
  geom_point(aes(y=ACTCM25), color="orangered") +
  geom_point(aes(y=ACTCM75), color="orangered4") +
  labs(title="Admission Rates and ACT Score Ranges",
       x="Percent of Applicants Accepted",
       y="ACT Score")
