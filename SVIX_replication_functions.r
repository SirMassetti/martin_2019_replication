

  # Theory-adjusted R^2
    # model.forecast: the theory-implied expected return (i.e. with parameter values fixed to the values implied by theory)
    # data.realized: contains the realized returns
    # pooled.panel.reg: contains the regression obejct; only needed to determine the degrees of freedom
      
      theory.adj.r.squared <- function(model.forecast, data.realized, pooled.panel.reg){
        theory.R2 = 1 - sum((data.realized - model.forecast)^2) / sum((data.realized -mean(data.realized))^2)
        adj.theory.R2 = (theory.R2 - (1 - theory.R2)*((summary(pooled.panel.reg))$fstatistic["numdf"]/(length(model.forecast) - (summary(pooled.panel.reg))$fstatistic["numdf"] - 1)))
        return(adj.theory.R2)
      }

      
  # test statistic and p-value for Wald tests
    # parameters: k-length vector containing the parameter estimates
    # restrictions: k x k matrix containing 1s for restricted parameters, and 0s otherwise
    # h0.values: k-length vector containing the parameter values under the null hypothesis for the restricted parameters (entries for parameters on which no restrictions are tested are irrelevant)
    # covariance.matrix: variance-covariance-matrix associated with the parameter estimates
    
      # Wald statistic
        wald.stat <- function(parameters, restrictions, h0.values, covariance.matrix){
          wald.statistic = t(restrictions%*%parameters - h0.values) %*% ginv(restrictions%*%boot.sigma%*%t(restrictions)) %*% (restrictions%*%parameters - h0.values)
          return(wald.statistic)
        }
      
      # p-value
        p.wald <- function(wald.stat, df){
          p.value = 1 - pchisq(wald.stat, df)
          return(p.value)
        }
      
      
  # time-series average of the sum of value-weighted firm fixed effects
    # fixed.effects.data: data.frame containing the estimated fixed effects
    # fe.variable: label for fixed effects (typically "alpha.i")
    # firm.identifier: (typically "gvkey")
    # date.variable: (typically "date") 
    # vw.variable: variable used for value-weighting (typically "mkt.cap.wweight")
      
    average.vw.fixed.effects <- function(fixed.effects.data, fe.variable, panel.data, firm.identifier, date.variable, vw.variable){
      sum.vw.FE = c()
      FE.dates = levels(factor(panel.data[, date.variable]))
      for (date.t in FE.dates){
        data.t = panel.data[panel.data[, date.variable] == date.t, c(firm.identifier, vw.variable)]
        data.t = merge(data.t, fixed.effects.data, by = firm.identifier, all.x = TRUE)
        sum.vw.FE = c(sum.vw.FE, sum(data.t[, vw.variable] * data.t[, fe.variable]))
        rm(data.t)
      }
      return(mean(sum.vw.FE))
    }


    
  # Time-series averages of rxm, 0.5*svix2_basket.svix2, and svix2 for firms/portfolios with full time series coverage
    # ts.data: data.frame that contains observations of rxm, 0.5*svix2_basket.svix2, and svix2 for firms identified by "gvkey" and at times t identified by "date"
    ts.averages <- function(ts.data){
      ts.dates = levels(factor(ts.data$date))
      ts.gvkeys = levels(factor(ts.data$gvkey))
      ts.avg = data.frame("gvkey" = ts.gvkeys, "rxm" = NA, "half_svix2_basket.svix2" = NA, "svix2" = NA)
      for (gvkey.i in ts.gvkeys){
        data.i = ts.data[ts.data$gvkey == gvkey.i, c("rxm", "svix2_basket.svix2", "svix2")]
        if (nrow(data.i) < length(ts.dates)) next
        ts.avg[ts.avg$gvkey == gvkey.i, "rxm"] = mean(data.i$rxm)
        ts.avg[ts.avg$gvkey == gvkey.i, "half_svix2_basket.svix2"] = 0.5 * mean(data.i$svix2_basket.svix2)
        ts.avg[ts.avg$gvkey == gvkey.i, "svix2"] = mean(data.i$svix2)
        rm(data.i)
      }    
      rm(gvkey.i)
      ts.avg = na.omit(ts.avg)
      return(ts.avg)
    }

    
  # Plot rxm vs 0.5*svix2_basket.svix2; always used with the time-series averages
    sml.plot <- function(sml.data){
      sml.reg = lm(rxm ~ half_svix2_basket.svix2, data = sml.data)
      par(mfrow = c(1, 1), mar = c(3.5, 3, 1.75, 2.25), cex.main=1.25, mgp= c(2,0.5,0))											
      plot(y = sml.data$rxm, x = sml.data$half_svix2_basket.svix2, pch = 3, col = "blue", xlab = TeX("$0.5 \\times (SVIX_i^2 - \\bar{SVIX}^2)$"), ylab = "Return in excess of the market")
      abline(sml.reg, lwd = 1.25 , col = "black")
      points(y = c(-1:1), x = c(-1:1), type = "l", lty = "dotted")
      abline(v = 0, lty = "dotted")
      abline(h = 0, lty = "dotted")
      legend("topleft", legend = c(paste("Slope: ", format(round(sml.reg$coefficients["half_svix2_basket.svix2"], 2), nsmall = 2), ",  R-squ: ", format(round(summary(sml.reg)$r.squared*100, 1), nsmall = 1), "%", sep = "")), col = c("white"), cex = 0.8, pch = c(0), bty = "n")
    }        
    

  # generate portfolio data: use sort.var to allocate firms into nr.cl portfolios at each date and compute the portfolios' rxm and svix2_basket.svix2 at each date
    generate.portfolio.data <- function(original.data, sort.var, nr.cl){
      original.data = original.data[, c("date", "gvkey", "rxm", "svix2_basket.svix2", "rx", "index.svix2", "svix2", sort.var)]
      sort.dates = levels(factor(original.data$date))
      data.counter = 1
      for (date.t in sort.dates){
        data.t = data[data$date == date.t, c("date", "gvkey", "rxm", "svix2_basket.svix2", "rx", "index.svix2", "svix2", sort.var)]
        data.t$port = quantile.portfolios(data.t[, sort.var], nr.cl)
        for (port.i in 1:nr.cl){
          data.t.i = data.t[!is.na(data.t$port) & data.t$port == port.i, ]
          port.data.t.i = data.frame("date" = date.t, "gvkey" = paste("P.", port.i, sep = ""), "rxm" = mean(data.t.i$rxm), "svix2_basket.svix2" = mean(data.t.i$svix2_basket.svix2), "rx" = mean(data.t.i$rx), "index.svix2" = mean(data.t.i$index.svix2), "svix2" = mean(data.t.i$svix2))
          if (data.counter == 1) port.data = port.data.t.i
          if (data.counter > 1) port.data = as.data.frame(rbind(port.data, port.data.t.i))
          rm(list  = c("data.t.i", "port.data.t.i"))
          data.counter = data.counter + 1
        }
        rm(list = c("port.i", "data.t"))
      }
      rm(list = c("date.t", "data.counter"))
      return(port.data)
    }

    quantile.portfolios = function(v, n){
      # v .. values to be sorted
      # n .. number of groups
      # sort will be from hi to low (highest = 1, lowest = n)
      
      v.allocations = rep(NA, length(v))
      quantiles = c()
      for (i in 1:(n-1)){
        quantiles = c(quantiles, (100/n)*i)
      }
      rm(i)
      
      port.quantiles = quantile(v, prob = quantiles/100, na.rm = TRUE, type = 5)
      
      for (i in 1:n){
        class.i = i
        if (i == 1) v.allocations[!is.na(v) & v <= port.quantiles[i]] = class.i	
        if (i > 1 & i < n) v.allocations[!is.na(v) & v > port.quantiles[i-1] & v <= port.quantiles[i]] = class.i	
        if (i == n) v.allocations[!is.na(v) & v > port.quantiles[i-1]] = class.i	
      }
      rm(i)		
      return(v.allocations)
      
    }
    