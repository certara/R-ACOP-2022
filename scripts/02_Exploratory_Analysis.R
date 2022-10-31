# Exploratory Analysis ----
library(gtsummary)
library(gt)

# 1: Import Data ----
# * 1.1 Read in xpt datasets ----
pkdata <- read_csv("acop2022.csv")

# 2: Explore/Summarize Data ----
# * 2.1. Descriptive Statistics Tables ----

# * * 2.1.1. Demographic summary table using gt ----
# the distinct function provides all unique rows of specified variables - in this case 1 row per individual.
# Note that a dataset with time varying covariates would require a different approach
# to isolate the unique rows to use for a demography summary table
dm_table <- pkdata %>%
  distinct(ID,AGE,SEX,RACE,WT) %>%
  select(AGE,WT,SEX,RACE) %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} ({min} - {max})")

gtsave(as_gt(dm_table), filename = "./tables/dm_table.html")


# *  * 2.1.2. Concentration summary by time and dose group using flextable ----
# gtsummary allows only one grouping column, we'll use dplyr for more
# complex data summary operations, then convert summary data.frame to a flextable

mean_conc_table <- pkdata %>%
  select(TIME, CONC, DOSEGRP) %>%
  #filter(TIME > 0) %>%  #Generally would include in table if PK collected pre-dose
  group_by(DOSEGRP, TIME) %>%
  summarise(CONC_mean = signif(mean(CONC), 3),
            CONC_min = signif(min(CONC), 3),
            CONC_max = signif(max(CONC), 3)) %>%
  mutate(CONC_combined_stat = paste0(CONC_mean, " (", CONC_min, "-", CONC_max, ")")) %>%
  select(DOSEGRP, TIME, CONC_combined_stat) %>%
  pivot_wider(names_from = "DOSEGRP", values_from = "CONC_combined_stat") %>%
  flextable(cwidth = 1.5) %>%
  set_header_labels(`TIME` = "Time (hr)",
                    `5000` = paste0("5000  ","\U03BC","g"),
                    `10000` = paste0("10000 ","\U03BC","g"),
                    `20000` = paste0("20000 ","\U03BC","g")) %>%
  add_header_row(colwidths = c(1,3), values = c("", "Dose Group")) %>%
  set_caption("Mean Concentration by Dose Group and Time") %>%
  align(align = "center", part = "all") %>%
  bold(part="header")

flextable::save_as_html(mean_conc_table,  path = "./tables/mean_conc_table.html")


# * 2.2 Exploratory Data Analysis ----

# * * 2.2.1. Time-Concentration by Subject Linear ----
pkplot <- pkdata %>%
  mutate(ID = as.factor(ID)) %>%
  ggplot(aes(TIME,CONC,group=ID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~DOSEGRP)

ggsave("./plots/pkplot.png", pkplot)

# * * 2.2.2. Time-Concentration by Subject Log ----
pkplotlog <- pkplot +
  scale_y_log10()

saveRDS(pkplotlog,file="./plots/pkplotlog.RDS")
ggsave("./plots/pkplotlog.png", pkplotlog)


# * * 2.2.3. Time-Concentration by Gender & Dose Group ----
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun = fun,  geom=geom,  ...)
}

pkplot_dosesex <- pkplot + facet_grid(DOSEGRP~SEX) +
  stat_sum_single(mean, geom = "line", aes(group = NULL),
                  col='red', alpha = .75, size = 1.5)

saveRDS(pkplot_dosesex,file="./plots/pkplot_dosesex.RDS")
ggsave("./plots/pkplot_dosesex.png", pkplot_dosesex)


# * * 2.2.4. Time-Concentration by Subject Faceted by Dose Group & Race ----
pkplot_doserace <- pkplot + facet_grid(DOSEGRP~RACE) +
  stat_sum_single(mean, geom = "line", aes(group = NULL),
                  col='red', alpha = .75, size = 1.5)

saveRDS(pkplot_doserace,file="./plots/pkplot_doserace.RDS")
ggsave("./plots/pkplot_doserace.png", pkplot_doserace)


# * * 2.2.5. Time-Concentration by Subject Faceted by Equal WT Intervals ----
pkplot_dosewt <- pkplot + facet_wrap(DOSEGRP~cut(WT,3)) + #group into 3 equal "cuts" of WT
  stat_sum_single(mean, geom = "line", aes(group = NULL),
                  col='red', alpha = .75, size = 1.5)

saveRDS(pkplot_dosewt,file="./plots/pkplot_dosewt.RDS")
ggsave("./plots/pkplot_dosewt.png", pkplot_dosewt)


# * * 2.2.6. Time-Concentration by Subject Faceted by Age Quantiles ----
pkplot_doseage <- pkplot + facet_wrap(~cut(AGE,quantile(AGE),include.lowest=TRUE)) + #group into quartile "cuts"
  stat_sum_single(mean, geom = "line", aes(group = NULL),
                  col='red', alpha = .75, size = 1.5)

saveRDS(pkplot_doseage,file="./plots/pkplot_doseage.RDS")
ggsave("./plots/pkplot_doseage.png", pkplot_doseage)


