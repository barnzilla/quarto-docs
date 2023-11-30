# Import dependencies
library(dplyr)
library(interpretCI)
library(stringr)

# Add user-defined functions to the Global Environment
get_chi_square <- function(x, y, alpha = 0.05) {
  
  # Run Chi-square test
  if(missing(y)) {
    test <- chisq.test(x %>% table) 
  } else {
    test <- chisq.test(x, y) 
  }
  
  # Add to output
  output <- test$observed %>%
    as_tibble
  
  if("." %in% names(output)) output <- output %>% rename(x = ".")
  
  # Add props and CIs
  if(missing(y)) {
    output <- output %>%
      mutate(
        prop = n / sum(n),
        ci_lower = prop - 1 * qnorm(1 - alpha / 2) * sqrt(prop * (1 - prop) / n),
        ci_upper = prop + 1 * qnorm(1 - alpha / 2) * sqrt(prop * (1 - prop) / n)
      )
  } else {
    output <- bind_rows(
      lapply(
        X = unique(output$y), 
        FUN = function(x) {
          df <- output %>% 
            filter(y == !! x) %>%
            mutate(
              group = x,
              prop = n / sum(n),
              ci_lower = prop - 1 * qnorm(1 - alpha / 2) * sqrt(prop * (1 - prop) / n),
              ci_upper = prop + 1 * qnorm(1 - alpha / 2) * sqrt(prop * (1 - prop) / n)
            ) %>%
            relocate(group, .before = n)
        }
      )
    )
  }
  
  # Transform props and CIs by a factor of 100
  output <- output %>%
    mutate(
      prop = prop * 100,
      ci_lower = pmax(0, ci_lower) * 100,
      ci_upper = pmax(0, ci_upper) * 100
    )
  
  return(output)
  
}

# Create fake data
df <- tibble(
  drinks = sample(
    x = 1:5,
    size = 1000,
    replace = TRUE,
    prob = c(0.2, 0.2, 0.1, 0.2, 0.3)
  ) %>% factor(labels = c("Wine", "Beer", "Vodka", "Rum", "Tequila")),
  drunk_status = sample(
    x = 1:2,
    size = 1000,
    replace = TRUE,
    prob = c(0.4, 0.6)
  ) %>% factor(labels = c("Drunk", "Sober"))
)

# Collapse sentiment vector into 3 levels
df <- df %>%
  mutate(
    drinks3 = case_when(
      drinks %in% c("Wine", "Beer") ~ "Light drinks",
      drinks %in% c("Vodka", "Tequila") ~ "Heavy drinks",
      TRUE ~ "Moderate drinks"
    ) %>% factor
  )

# Get Chi-square
plot_df <- get_chi_square(
  x = df$drinks3,
  y = df$drunk_status
)

# Render bar plot
ggplot(
  data = plot_df,
  mapping = aes(
    x = x,
    y = prop,
    fill = y,
    label = prop %>% round(2) %>% format(nsmall = 2)
  )
) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    position = position_dodge(width = 0.6),
    aes(
      vjust = -1.5, 
      y = ci_upper
    ), 
    size = 2.5
  ) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    color = "#434343",
    width = 0.05,
    position = position_dodge(width = 0.6)
  ) +
  labs(
    title = "",
    x = "",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_blank()
  )