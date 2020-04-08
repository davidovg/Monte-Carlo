#Real price of a Call from BS is exp(-r * T) * N(d2) and the 
# probability of being in-the-money is just d2

d2<-(log(100/100)+(0.05-0.5*(0.2^2))*(1-0))/0.2*sqrt(1)

d2

Nd2<-pnorm(d2)

Nd2

exp(-0.05)*Nd2 # value of call 

#Function to calculate the theoretical (BS) price
# Type = 1 for call, Type=-1 for put

#T - time to maturity in years
#S0 - initial price
#K - execution price or strike
#sigma - annual volatility

BS_digital<-function(S0, K, sigma, r, T, Type, show_price=TRUE)
{
  d2 = (log(S0/K) + (r - 0.5 * sigma^2) * T)/ (sigma*sqrt(T))
  Put_or_Call<-c()
  ifelse(Type==1, Put_or_Call<-"Call", Put_or_Call<-"Put")
  price<-exp(-r * T) * pnorm(Type*d2)
  if(show_price==TRUE) {print(paste("The BS price of the digital ", paste(Put_or_Call), " is ",
                                    round(price,4),sep=""))} else {
                                      print(paste("The BS probability of ending ITM of the digital" , paste(Put_or_Call), "is ", 
                                                  round(pnorm(Type*d2),4),sep=""))
                                    }
 return(price) 
}

BS_binary_call_theoretical_price<-BS_digital(100,100,0.2,0.05,1,1)
BS_binary_call_theoretical_price

BS_binary_put_theoretical_price<-BS_digital(100,100,0.2,0.05,1,-1)
BS_binary_put_theoretical_price

# this is the payoff of the option
# just an indicator function
digital_call_payoff<-function(S_T,K)
{
  if (S_T >= K) {
    return(1)} else {
      return(0)}
}
# a plot illustrates
plot(sapply((0:150),digital_call_payoff,100),
     main="Payoff of a digital call option",col="red",
     xlab="Stock price",ylab='Payoff')


# run simulation using Euler - Maruyama Scheme

monte_carlo_sim_euler<-function(simulations, days, init_price,annual_vol, risk_free) {
  price_matrix<-matrix(0,nrow=(days+1),ncol=simulations)
  dt<-1/days
  for (i in 1:simulations) {
  price_matrix[1,i]<-init_price
    for (j in 2:(days+1)) {
      price_matrix[j,i]=price_matrix[j-1,i]*(1+risk_free*dt+annual_vol*sqrt(dt)*rnorm(1))
    } 
  } 
  matplot(price_matrix, main="MC - Euler-Maruyama Scheme", ylab="Path", xlab="Time", type="l")
  price_matrix
return(price_matrix)
}

#calls error analysis

mc_100<-monte_carlo_sim_euler(100,250,100,0.2,0.05)
call_payoffs_100<-c(mc_100[nrow(mc_100),]>100)
binary_call_price_100<-mean(call_payoffs_10)*exp(-0.05*1)
binary_call_price_100

absolute_error<-(binary_call_price_100-BS_binary_call_theoretical_price)/100
absolute_error


put_payoffs_10<-c(mc_10[251,]<100)
binary_put_price_10<-mean(put_payoffs)*exp(-0.05*1)
binary_put_price_10




table_results <- cbind(c(return_portfolio1,return_portfolio2,return_portfolio3,return_portfolio4),
                       c(sigma_portfolio1,sigma_portfolio2,sigma_portfolio3,sigma_portfolio4))
colnames(talbe_results) <- c("Type","Standard Deviation")
rownames(table_results) <- c(rep("Call",4),rep("Put",4))
results <- as.table(table_results)
results

binary_option_value<-function(simulations, days, init_price, strike, annual_vol, risk_free) {
  call_payoffs<-0
  put_payoffs<-0
  call_value<-0
  put_value<-0
  value<-0
  dt<-1/days
  price_vector[1]<-strike
  for (i in 1:simulations) {
    price_vector<-c()
    price_vector[1]<-strike
    for (j in 2:(days+1)) {
      price_vector[j]=price_vector[j-1]*(1+risk_free*dt+annual_vol*sqrt(dt)*rnorm(1))
    }
    if (price_vector[length(price_vector)]>strike) {
      call_payoffs<-call_payoffs+1 } else { put_payoffs<-put_payoffs+1}
  }
  call_value<-exp(-risk_free)*(call_payoffs/simulations)
  put_value<-exp(-risk_free)*(put_payoffs/simulations)
return(list("Put" = put_value, "Call" =call_value))    
}

binary_option_value(100,250,100,100,0.2,0.05)

x<-replicate(10,binary_option_value(100,250,100,100,0.2,0.05),TRUE)

Reduce("+",x["Call",])/10

Reduce("+",x["Put",])/10


