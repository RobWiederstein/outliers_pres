library(ggplot2)
df <- data.frame(id = 1:100,
                 normal = rnorm(100, mean = 0, sd = 1),
                 log = rlnorm(100, meanlog = 0, sdlog = 1)) |>
                 dplyr::mutate(transformed = log10(log)) |>
                 tidyr::pivot_longer(cols= c("normal", "log", "transformed"),
                        names_to = "distribution") |>
                dplyr::mutate(distribution = forcats::as_factor(distribution))
p <- df |>
        ggplot() +
        aes(value, group = distribution, color = distribution) +
        geom_density() +
        facet_wrap(vars(distribution)) +
        theme_minimal() +
        scale_y_continuous(name = "") +
        scale_x_continuous(name = "") +
        theme(legend.position="none")
ggsave(p, filename = "./img/log_transformed.jpeg", width = 6, height = 4, unit = "in")

