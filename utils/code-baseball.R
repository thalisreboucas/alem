###
#
# Code by : Thalis Rebouças
#
# URL : https://www.r-bloggers.com/2017/01/understanding-mixture-models-and-expectation-maximization-using-baseball-statistics/
#
###


# Library i need
library(dplyr , quietly = TRUE)
library(tidyr , quietly = TRUE)
library(Lahman , quietly = TRUE)
library(ggplot2 , quietly = TRUE)
library(tibble ,quietly = TRUE)
library(plotly,quietly = TRUE)
library(purrr , quietly = TRUE)
library(VGAM,quietly = TRUE)

theme_set(theme_bw())

# Identify those who have pitched at least three games
pitchers <- Pitching %>%
  dplyr::group_by(playerID) %>%
  dplyr::summarize(gamesPitched = sum(G)) %>%
  dplyr::filter(gamesPitched > 3)

# in this setup, we're keeping some extra information for later in the post:
# a "bats" column and a "year" column

career <- Batting %>%
  dplyr::filter(AB > 0, lgID == "NL", yearID >= 1980) %>%
  dplyr::group_by(playerID) %>%
  dplyr::summarize(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  dplyr::mutate(average = H / AB,
         isPitcher = playerID %in% pitchers$playerID)

# Add player names

career <- Master %>%
  dplyr::tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast, bats) %>%
  tidyr::unite(name, nameFirst, nameLast, sep = " ") %>%
  dplyr::inner_join(career, by = "playerID")

set.seed(1036)

# We'll fit the clusters only with players that have had at least 20 at-bats
starting_data <- career %>%
  dplyr::filter(AB >= 20) %>%
  dplyr::select(-year, -bats, -isPitcher) %>%
  dplyr::mutate(cluster = factor(sample(c("A", "B"), n(), replace = TRUE)))


# Plotly dens

dens <- with(starting_data, tapply(average, INDEX = cluster, density))
starting_data_dens <- data.frame(
  x = unlist(lapply(dens, "[[", "x")),
  y = unlist(lapply(dens, "[[", "y")),
  cluster = rep(names(dens), each = length(dens[[1]]$x))
)

starting_data_dens %>% 
    plotly::plot_ly(x = ~x, y = ~y, color = ~cluster) %>% 
    plotly::add_lines( )%>%                     
    plotly::layout(title = "Gráfico do Cluster",
                   legend = list(x = 0.10, y = 0.97),
                   font = list(size = 10),
                   margin = list(l = 100, r = 20, t = 70, b = 70),
                   paper_bgcolor = '#f8f8ff',
   xaxis = list(title = "Média",
                ticks = 'outside'),
   yaxis = list(title = "Densidade",
             ticks = 'outside'))

 # Function Beta Binomial
 
 fit_bb_mle <- function(x, n) {
   # dbetabinom.ab is the likelihood function for a beta-binomial
   # using n, alpha and beta as parameters
   ll <- function(alpha, beta) {
     -sum(dbetabinom.ab(x, n, alpha, beta, log = TRUE))
   }
   m <- stats4::mle(ll, start = list(alpha = 3, beta = 10), method = "L-BFGS-B",
                    lower = c(0.001, .001))
   ab <- stats4::coef(m)
   data_frame(alpha = ab[1], beta = ab[2], number = length(x)) }
 
 # function
 fit_bb_mle(starting_data$H, starting_data$AB)

 fits <- starting_data %>%
   dplyr::group_by(cluster) %>%
   dplyr::do(fit_bb_mle(.$H, .$AB)) %>%
   dplyr::ungroup() %>%
   dplyr::mutate(prior = number / sum(number)) 

 crosses <- starting_data %>%
   dplyr::select(-cluster) %>%
   tidyr::crossing(fits) %>%
   dplyr::mutate(likelihood = prior * VGAM::dbetabinom.ab(H, AB, alpha, beta)) 
 
 assignments <- starting_data %>%
   dplyr::select(-cluster) %>%
   tidyr::crossing(fits) %>%
   dplyr::mutate(likelihood = prior * VGAM::dbetabinom.ab(H, AB, alpha, beta)) %>%
   dplyr::group_by(playerID) %>%
   dplyr::top_n(1, likelihood) %>%
   dplyr::ungroup()

 ggplot2::ggplot(assignments, aes(average, fill = cluster)) +
   geom_histogram(bins = 80) 

 
 # Algorithm EM
 
 iterate_em <- function(state, ...) {
   fits <- state$assignments %>%
     dplyr::group_by(cluster) %>%
     dplyr::do(mutate(fit_bb_mle(.$H, .$AB), number = nrow(.))) %>%
     dplyr::ungroup() %>%
     dplyr::mutate(prior = number / sum(number))
   assignments <- assignments %>%
     dplyr::select(playerID:average) %>%
     tidyr::crossing(fits) %>%
     dplyr::mutate(likelihood = prior * VGAM::dbetabinom.ab(H, AB, alpha, beta)) %>%
     dplyr::group_by(playerID) %>%
     dplyr::top_n(1, likelihood) %>%
     dplyr::ungroup()
   
   list(assignments = assignments,
        fits = fits)
 }
 
 
 iterations <- purrr::accumulate(1:5, iterate_em, .init = list(assignments = starting_data))

 fit_iterations <- iterations %>%
   map_df("fits", .id = "iteration")
 fit_iterations %>%
   tidyr::crossing(x = seq(.001, .4, .001)) %>%
   dplyr::mutate(density = prior * dbeta(x, alpha, beta)) %>%
   ggplot2::ggplot(aes(x, density, color = iteration, group = iteration)) +
   geom_line() +
   facet_wrap(~ cluster)  
 
 batter_100 <- career %>%
   dplyr::filter(AB == 100) %>%
   dplyr::arrange(average)
 batter_100
 
 # EM CLUSTER
 career_likelihoods <- career %>%
   dplyr::filter(AB > 20) %>%
   tidyr::crossing(final_parameters) %>%
   dplyr::mutate(likelihood = prior * VGAM::dbetabinom.ab(H, AB, alpha, beta)) %>%
   dplyr::group_by(playerID) %>%
   dplyr::mutate(posterior = likelihood / sum(likelihood))
 
 career_assignments <- career_likelihoods %>%
   dplyr::top_n(1, posterior) %>%
   dplyr::ungroup()
 