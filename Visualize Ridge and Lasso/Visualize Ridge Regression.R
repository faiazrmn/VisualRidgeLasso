
### Data for Regression
x <- 1:100
y <- 1 * x + rnorm(100, 0, 1)

### Ridge SSR
### Sum of Squared Residuals with L2 Norm Formula
ssr_ridge <- function(beta_1, Lam){
  sum((y - (beta_1*x))^2) + (Lam*(beta_1^2))
}

### Lasso SSR
### Sum of Squared Residuals with L1 Norm Formula
ssr_lasso <- function(beta_1, Lam){
  sum((y - (beta_1*x))^2) + (Lam*abs(beta_1))
}

### Beta_1 values for graph
b1 <- ((-2000:2000) / 1000)

### Lambda Values
lam <- c(0, 50000, 100000, 200000, 300000, 400000, 500000, 600000, 1000000, 5000000, 50000000)

### Combination of Beta_1 and Lambda Values
B1 <- rep(b1, times = length(lam))
L  <- rep(lam, each = length(b1))
d <- data.frame(B1, L)

### Calculate Ridge and Lasso Sum of Square for each combination of Beta_1 and Lambda
d <- d %>% 
  group_by(B1, L) %>% 
  mutate(val_ridge = ssr_ridge(B1, L),
         val_lasso = ssr_lasso(B1, L)) %>%
  ungroup() 

### Visualize Ridge
d %>%
  ggplot(aes(B1, val_ridge, color = as.factor(L))) +
  geom_point(alpha = 0.5, size = 0.3) +
  theme_minimal() +
  scale_y_log10() +
  labs(title = "Beta_1 vs Sum of Squares of Residuals in Simple Regression",
       subtitle = "L2 Norm used, and for different values of Penalty Lambda",
       x = "Beta_1",
       y = "Sum of Squares with L2 Penalty",
       color = "Penalty Value")

### Visualize Lasso
d %>%
  ggplot(aes(B1, val_lasso, color = as.factor(L))) +
  geom_point(alpha = 0.5, size = 0.3) +
  theme_minimal() +
  scale_y_log10() +
  labs(title = "Beta_1 vs Sum of Squares of Residuals in Simple Regression",
       subtitle = "L1 Norm used, and for different values of Penalty Lambda",
       x = "Beta_1",
       y = "Sum of Squares with L1 Penalty",
       color = "Penalty Value")


### Find the Value of Beta_1, WHen Lambda is Maximum, and SSR_Ridge Minimum
d %>% 
  group_by(B1, L) %>% 
  mutate(val = ssr(B1, L)) %>% 
  ungroup() %>% 
  filter(L == 50000000) %>% 
  filter(val_ridge == min(val_ridge)) 

### Find the Value of Beta_1, WHen Lambda is Maximum, and SSR_Lasso Minimum
d %>% 
  group_by(B1, L) %>% 
  mutate(val = ssr(B1, L)) %>% 
  ungroup() %>% 
  filter(L == 50000000) %>% 
  filter(val_lasso == min(val_lasso)) 
