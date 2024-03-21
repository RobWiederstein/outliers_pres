file <- "~/Dropbox/coding/rproj/outliers2/data/2021_mdcr_urban_clms.rds"
df <- readRDS(file = file)
saveRDS(df[1, c(1:10)], file = "./data/2021_mdcr_clm_ex.rds")
library(dplyr)
library(ggplot2)
# ortho ----
ortho <-
    df |>
    filter(rndrng_prvdr_type == "Orthopedic Surgery") |>
    filter(rndrng_prvdr_city %in% c("Los Angeles", "New York")) |>
    select(rndrng_npi, rndrng_prvdr_city, tot_benes_mean, tot_srvcs_mean,
           out_scores, type)
saveRDS(ortho, file = "./data/2021_mdcr_ortho_clms.rds")
# ortho plot ----
ortho |>
    ggplot() +
    aes(tot_benes_mean, tot_srvcs_mean) +
    geom_point(alpha = .65) +
    scale_y_continuous(labels = scales::label_comma()) +
    facet_wrap(vars(rndrng_prvdr_city)) +
    theme_bw()
ggsave(file = "./plots/2021_mdcr_ortho.jpg",
       width = 8,
       height = 5,
       units = "in")
# ortho table ----
tbl_ortho <- psych::describe(ortho[, c(3, 4)]) |>  select(!c(mad, trimmed)) |>
    kable(digits = 1) |>
    kableExtra::kable_styling(bootstrap_options = "striped", font_size = 20) |>
    column_spec(c(10, 11), color = "red")
saveRDS(tbl_ortho, "./tables/2021_mdcr_orth_tbl.rds")

# plot medicare claims with outliers ----
df |>
    ggplot() +
    aes(tot_benes_mean, tot_srvcs_mean) +
    geom_point() +
    scale_x_continuous(label = scales::label_comma()) +
    scale_y_continuous(label = scales::label_comma()) +
    theme_bw()
ggsave(file = "./plots/2021_top_25_w_outliers.jpg",
       height = 5,
       width = 8,
       units = "in")
# table medicare claims with outliers ----
mdcr_top_25_outliers_tbl <-
    psych::describe(df[, c("tot_benes_mean",
                           "tot_srvcs_mean")]) |>
    select(!c(mad, trimmed)) |>
    kable(digits = 1) |>
    kableExtra::kable_styling(bootstrap_options = "striped", font_size = 20) |>
    column_spec(c(10, 11), color = "red")
saveRDS(mdcr_top_25_outliers_tbl, "./tables/2021_mdcr_all_clm_outliers_tbl.rds")

# medicare claims no outliers
df |>
    filter(type == "typical") |>
    ggplot() +
    aes(tot_benes_mean, tot_srvcs_mean) +
    geom_point() +
    scale_x_continuous(label = scales::label_comma()) +
    scale_y_continuous(label = scales::label_comma()) +
    theme_bw()
ggsave("./plots/2021_top_25_no_outliers.jpg",
       height = 5,
       width = 8,
       units = "in")

# table medicare claims no outliers ----

 df1 <-
    df |>
    filter(type == "typical")
mdcr_top_25_no_outliers_tbl <-
    psych::describe(df1[, c("tot_benes_mean",
                           "tot_srvcs_mean")]) |>
    select(!c(mad, trimmed)) |>
    kable(digits = 1) |>
    kableExtra::kable_styling(bootstrap_options = "striped", font_size = 20) |>
    column_spec(c(10, 11), color = "red")

saveRDS(mdcr_top_25_no_outliers_tbl, "./tables/2021_mdcr_all_clm_no_outliers_tbl.rds")
