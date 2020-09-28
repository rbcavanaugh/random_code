
m <- glmer(response~session + (session|item_id_crossed),
           data = df84,
           family = binomial)

new_dat <- tibble(
  session = seq(1,15,1)
)

b <- bootMer(m, nsim=100, 
             FUN=function(x)predict(x, newdata=new_dat, re.form=NA))

#extract the quantiles and the fitted values.
lci <- apply(b$t, 2, quantile, 0.025)   
uci <- apply(b$t, 2, quantile, 0.975)   
preds <- predict(m, newdata = new_dat, re.form = NA)

preds <- tibble(
  preds, lci, uci
)

