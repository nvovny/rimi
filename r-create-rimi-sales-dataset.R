# Code written for simulation of fictional product sales in 281 RIMI stores in order
# to create a dataset for visualization

# Create date sequence within given period 

period_start <- '2020-01-01'
period_end <- '2020-12-31'

set.seed(1)

dates <- seq(as.Date(period_start),as.Date(period_end),by = 1)


period_length <- length(dates)

# Express: 21(1-21), Hyper: 87(22-108), Mini: 101(109-209), Super: 72(210-281)

n_exp <- 21
n_hyp <- 87
n_mini <- 101
n_super <- 72

nb_of_stores <- n_exp + n_hyp + n_mini + n_super

# Relative store size -> sales volume modifiers per product

s_hyp <- 1
s_super <- 0.8
s_mini <- 0.6
s_exp <- 0.5

# Number of products to generate

nb_of_products <- 100

# Create an array of randomized unique integers equal to size of product list

prod_list <- sample(nb_of_products)

# Create product subsets for stores of different sizes

prod_list_hyp <- prod_list
prod_list_super <- prod_list_hyp[sample(60)]
prod_list_mini <- prod_list_super[sample(40)]
prod_list_exp <- prod_list_mini[sample(20)]


# Generate an array with time period repetitions according to number of stores of each type and length of product subsets

date_column <- c(rep(dates, n_exp*length(prod_list_exp)), rep(dates, n_hyp*length(prod_list_hyp)), 
                 rep(dates, n_mini*length(prod_list_mini)),rep(dates, n_super*length(prod_list_super)))

# Generate numeric weekdays array from date_column

weekday_num <- as.numeric(format(date_column,"%u"))

# Generate numeric month array from date_column

month_num <- as.numeric(format(date_column,"%m"))

store_id <- vector()
prod_id <- vector()
sold_qty <- vector()


# Sales base modifier (if needed) dependent on number of products

mod_np <- 100/nb_of_products


for (i in 1:n_exp) {
  
  store_id <- c(store_id, rep(i, period_length*length(prod_list_exp)))
  
 
    for (p in 1:length(prod_list_exp)){
      prod_id <- c(prod_id, rep(prod_list_exp[p],period_length))
      
      # Generate sales for the whole period by Poisson distribution function taking as mean an index value
      # of the product in the prod_list and by applying to it store size modifier
      
      sales <- rpois(period_length,lambda=mod_np*match(prod_list_exp[p],prod_list)*s_exp)
      sold_qty <- c(sold_qty,sales)
      
    }
  
  # Individual performance modifier within store size group
  
  mod_store = rnorm(1,1,0.05)
  sold_qty <- sold_qty*mod_store
}

  
for (i in (i+1):(i+n_hyp)) {
  
  store_id <- c(store_id, rep(i, period_length*length(prod_list_hyp))) 
  
  for (p in 1:length(prod_list_hyp)){
    prod_id <- c(prod_id, rep(prod_list_hyp[p],period_length))
    sales <- rpois(period_length,lambda=mod_np*match(prod_list_hyp[p],prod_list)*s_hyp)
    sold_qty <- c(sold_qty,sales)
    
  }
  
  mod_store = rnorm(1,1,0.05)
  sold_qty <- sold_qty*mod_store
}


for (i in (i+1):(i+n_mini)) {
  
  store_id <- c(store_id, rep(i, period_length*length(prod_list_mini)))
  
  for (p in 1:length(prod_list_mini)){
    prod_id <- c(prod_id, rep(prod_list_mini[p],period_length))
    sales <- rpois(period_length,lambda=mod_np*match(prod_list_mini[p],prod_list)*s_mini)
    sold_qty <- c(sold_qty,sales)
    
  }
  
  mod_store = rnorm(1,1,0.05)
  sold_qty <- sold_qty*mod_store
}

for (i in (i+1):(i+n_super)) {
  
  store_id <- c(store_id, rep(i, period_length*length(prod_list_super))) 
  
  for (p in 1:length(prod_list_super)){
    prod_id <- c(prod_id, rep(prod_list_super[p],period_length))
    sales <- rpois(period_length,lambda=mod_np*match(prod_list_super[p],prod_list)*s_super)
    sold_qty <- c(sold_qty,sales)
    
  }
  
  mod_store = rnorm(1,1,0.05)
  sold_qty <- sold_qty*mod_store
}


# To introduce season-dependent sales modifier, create several arrays containing adjustments by Month

mod_summer0 <- c(1, 1, 1, 1, 1.1, 1.6, 1.8, 3, 1.5, 1, 1, 1)
mod_summer1 <- c(0.5, 0.7, 1.2, 1.8, 2, 2.5, 2, 1.8, 1.5, 0.8, 0.5, 0.5)
mod_summer2 <- c(0.3, 0.4, 0.5, 0.7, 1.6, 4, 6, 5, 2.5, 1.2, 0.5, 0.4)

mod_winter0 <- c(1.5, 1.3, 1, 0.5, 0.5, 0.5, 0.5, 0.8, 1.5, 1.8, 2, 1.9)
mod_winter1 <- c(1, 0.8, 0.5, 0.3, 1, 1, 0.5, 0.4, 1.2, 1.4, 1.5, 1.3)
mod_winter2 <- c(2, 1, 0.5, 0.4, 0.3, 0.3, 1, 1.2, 1.4, 1.6, 1.8, 2.1)

mod_season_collection <- data.frame(mod_summer0, mod_summer1, mod_summer2, mod_winter0, mod_winter1, mod_winter2)

# Season (Month): generate random number of integers choose a column in modifiers data frame
# or do not modify value if generated number is greater than column count. 6/18 = 33% of products will be seasonal

prod_seasonality_id <- sample(1:18, nb_of_products, replace = TRUE)


# Sales modifiers for weekdays

mod_week <- c(1.1, 1, 1, 1, 1.5, 1.3, 0.9)


# Run through sold_qty and apply Sales volume modifiers

# prod_group <- vector()

for (i in 1:length(date_column)) {
  
  # Weekday
  
    sold_qty[i] <- sold_qty[i]*rnorm(1, mean = mod_week[weekday_num[i]], sd = mod_week[weekday_num[i]]/10)
  
    c <- prod_seasonality_id[prod_id[i]]

    if (c <= 6) {
    sold_qty[i] <- round(sold_qty[i]*rnorm(1, mean = mod_season_collection[month_num[i],c], sd = mod_season_collection[month_num[i],c]/10),0)
    }

    else {
    sold_qty[i] <- round(sold_qty[i])
    }
    
 }
    
    

# Write combined output to file

output_dataset <- data.frame(date_column,store_id,prod_id,sold_qty)

write.csv(output_dataset, file = "dataset.csv", row.names=FALSE)

