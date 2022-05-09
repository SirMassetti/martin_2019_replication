#
# For details and disclaimer, see MartinWagner_replication.pdf
#   


  rm(list=ls())
  gc(reset = TRUE)
  
  # specify path of the folder with the replication files
    files.path = "/Users/christianwagner/Dropbox/research/cross section SVIX/MartinWagner_replication/"
    #
    setwd(files.path)
  
  # load packages required below
    library(car)
    library(latex2exp)
    library(MASS)
    library(plm)
    
  # load functions needed below and defined in separate file SVIX_replication_functions.r
    source("SVIX_replication_functions.r")
  
  # load the data 
    load("SVIX_replication_data.Rdata")
  
  # define additional variables
    data$svix2_basket.svix2 = data$svix2 - data$basket.svix2     # firm i's risk neutral excess variance
    data$E.rxm = 0.5*data$svix2_basket.svix2     # model-implied expected return in excess of the market            
    data$E.rx = data$index.svix2 + 0.5*data$svix2_basket.svix2     # model-implied expected excess return
    data$E.rxm.os = data$E.rxm * (1 + data$rf) * (rx.maturity/12)
    data$E.rx.os = data$E.rx * (1 + data$rf) * (rx.maturity/12)
    #
    data.gvkeys = levels(factor(data$gvkey))     # vector containing all firm identifiers

    
  
    
  ##### Replication of results in the introduction

    ### Figure 1: Expected excess returns and expected returns in excess of the market. Annual horizon.
      # for Apple, set plot.gvkey = 1690
      # for JPM, set plot.gvkey =  2968
      
      plot.gvkey = 1690
      
      plot.data  = data[data$gvkey == plot.gvkey,]
      plot.data = plot.data[order(plot.data$date), ]    
      #
      x.ticks = seq(from = 1, to = nrow(plot.data), by = 48)
      x.ticks2 = seq(from = 1, to = nrow(plot.data), by = 12)
      date.labels = format(as.Date(plot.data$date), "%b/%y")
      if (plot.gvkey == 1690) legend.pos = "topright"
      if (plot.gvkey == 2968) legend.pos = "topleft"
      
      ## Expected excess returns
        plot.data$rx.model.forecast = plot.data$E.rx.os     # model forecast
        plot.data$rx.svix.capm.forecast = (plot.data$beta) * plot.data$index.svix2 * (1 + plot.data$rf)  * (rx.maturity/12)     # SVIX-CAPM forecast
        plot.data$rx.sixpct.capm.forecast = (plot.data$beta) * 0.06     # 6%-CAPM forecast
        #
        par(mfrow = c(1, 1), mar = c(3.5, 3, 1.75, 2.25), cex.main=1, mgp= c(2,1,0))
        y.min = min(plot.data[, c("rx.model.forecast", "rx.svix.capm.forecast", "rx.sixpct.capm.forecast")], na.rm = TRUE, 0)
        y.max = max(plot.data[, c("rx.model.forecast", "rx.svix.capm.forecast", "rx.sixpct.capm.forecast")], na.rm = TRUE)
        plot(plot.data$rx.model.forecast, type = "l", xaxt="n", yaxt="n", xlab="", ylim = c(y.min, y.max), ylab="Expected Excess Return", col="blue", lwd=2, main = paste("Firm:", plot.gvkey))
        points(plot.data$rx.svix.capm.forecast, type = "l", col="darkred", lwd=1.75, lty = "53")
        points(plot.data$rx.sixpct.capm.forecast, type = "l", col="black", lwd=1.75, lty = "12")
        axis(side = 2, padj = 0.4, cex.axis = 0.9, ylab = "")
        axis(side = 1, at = x.ticks2, labels = rep("", length(x.ticks2)), padj = -0.90, cex.axis = 0.9)
        axis(side = 1, at = x.ticks, labels = date.labels[x.ticks], padj = -0.90, cex.axis = 0.9)
        abline(h = 0, lty = "44", col = grey(0.15))
        legend(legend.pos, legend = c("Model", TeX("$SVIX_t$ CAPM"), "6% CAPM"), lty = c("solid", "53", "12"), col = c("blue", "darkred", "black"), lwd = c(2, 1.75, 1.75), cex= 0.8, bty = "n")
        rm(list = c("y.min", "y.max"))
        
      ## Expected returns in excess of the market
        plot.data$rxm.model.forecast = plot.data$E.rxm.os     # model forecast
        plot.data$rxm.svix.capm.forecast = (plot.data$beta - 1) * plot.data$index.svix2  * (1 + plot.data$rf) * (rx.maturity/12)     # SVIX-CAPM forecast
        plot.data$rxm.sixpct.capm.forecast = (plot.data$beta - 1) * 0.06     # 6%-CAPM forecast
        #
        par(mfrow = c(1, 1), mar = c(3.5, 3, 1.75, 2.25), cex.main=1, mgp= c(2,1,0))
        y.min = min(plot.data[, c("rxm.model.forecast", "rxm.svix.capm.forecast", "rxm.sixpct.capm.forecast")], na.rm = TRUE, 0)
        y.max = max(plot.data[, c("rxm.model.forecast", "rxm.svix.capm.forecast", "rxm.sixpct.capm.forecast")], na.rm = TRUE)
        plot(plot.data$rxm.model.forecast, type = "l", xaxt="n", yaxt="n", xlab="", ylim = c(y.min, y.max), ylab="Expected Return in Excess of the Market", col="blue", lwd=2, main = paste("Firm:", plot.gvkey))
        points(plot.data$rxm.svix.capm.forecast, type = "l", col="darkred", lwd=1.75, lty = "53")
        points(plot.data$rxm.sixpct.capm.forecast, type = "l", col="black", lwd=1.75, lty = "12")
        axis(side = 2, padj = 0.4, cex.axis = 0.9, ylab = "")
        axis(side = 1, at = x.ticks2, labels = rep("", length(x.ticks2)), padj = -0.90, cex.axis = 0.9)
        axis(side = 1, at = x.ticks, labels = date.labels[x.ticks], padj = -0.90, cex.axis = 0.9)
        abline(h = 0, lty = "44", col = grey(0.15))
        legend(legend.pos, legend = c("Model", TeX("$SVIX_t$ CAPM"), "6% CAPM"), lty = c("solid", "53", "12"), col = c("blue", "darkred", "black"), lwd = c(2, 1.75, 1.75), cex= 0.8, bty = "n")
        rm(list = c("y.min", "y.max"))
        
      rm(list = c("plot.gvkey", "plot.data", "x.ticks", "x.ticks2", "date.labels", "legend.pos"))
      
    
    
      
  ##### Replication of results in Section II: Three measures of risk-neutral variance
      
      ### Using daily data 
            
        date.labels = format(as.Date(data.daily$date), "%b/%y")
        x.ticks = c((1:20)-1)*251.5+1

        # Figure 2: Option-implied equity variance of S&P 500 firms (Panel D. One-year horizon)
          par(mfrow = c(1,1), mar = c(3.5, 4, 1.75, 2.25), cex.main=1.1, mgp= c(2,1,0))
          plot(data.daily$basket.svix2, type = "l", col = "darkred", lwd = 3, lty = 1, xaxt="n", yaxt="n", xlab="", ylab = "Stock variance", main = "", cex.lab = 1, ylim = c(0, max(data.daily$basket.svix2)))
          points(data.daily$index.svix2, type = "l", col = "blue", lwd = 3, lty = "dashed")			  
          axis(side = 2, padj = 0.4, cex.axis = 1)
          axis(side = 1, at = x.ticks, labels = date.labels[x.ticks], padj = -0.90, cex.axis = 1) 
          abline(v = c(x.ticks), col = 1, lty = "dotted")
          abline(h = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.50, 0.55, 0.6, 0.65, 0.70, 0.75, 0.8), col = 1, lty = "dotted")
          legend("topleft", legend = c(TeX("$SVIX_t^2$"), TeX("$\\bar{SVIX_t^2}$")), lty = c("dashed", "solid"), col = c("blue", "darkred"), lwd = c(3, 3), bg = "white", cex= 0.8)

        # Figure IA.2: A measure of average risk-neutral correlation between stocks (Panel D. S&P 500, one-year horizon)
          par(mfrow = c(1,1), mar = c(3.5, 4, 1.75, 2.25), cex.main=1.1, mgp= c(2,1,0))
          plot(data.daily$index.svix2/data.daily$basket.svix2, type = "l", col = "blue", xaxt="n", yaxt="n", xlab="", ylab = TeX("$SVIX_t^2 / \\bar{SVIX_t^2}$"), main = "", lwd = 3, lty = "solid", cex.lab = 1, ylim = c(0.1, 1.0))
          axis(side = 2, padj = 0.4, cex.axis = 1)
          axis(side = 1, at = x.ticks, labels = date.labels[x.ticks], padj = -0.90, cex.axis = 1) 
          abline(v = c(x.ticks), col = 1, lty = "dotted")
          abline(h = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), col = 1, lty = "dotted")
        
        rm(list = c("date.labels", "x.ticks"))    

    ### Using monthly data 
        
        # Table I: Sample data (Panel C, column 365 days)
          sample.statistics = c(
            nrow(data),     # Observations
            length(levels(factor(data$date))),     # Sample months
            length(levels(factor(data$gvkey))),     # Sample firms
            nrow(data)/length(levels(factor(data$date)))     # Average firms/month
          )
          names(sample.statistics) = c("Observations", "Sample months", "Sample firms", "Average firms/month")
          print(sample.statistics)
          rm(sample.statistics)

        # Figure 3: Beta, size, value, momentum, and option-implied equity variance
          # generate portfolio data, by sorting firms by sort.var into nr.cl portfolios and computing average excess-of-market returns and risk neutral variance at each date
          # compute the portfolios' time-series averages of svix2 using the function ts.averages()
          # plot the portfolios' average svix2
          # choose sort.var to be "beta" or "btm" to obtain results from Panels A and C, respectively  
          nr.cl = 10
          sort.var = "beta"     
          port.data = generate.portfolio.data(data, sort.var, nr.cl)
          port.averages = ts.averages(port.data)
          barplot(port.averages$svix2[nr.cl:1], col = "blue2", ylim = c(0, 0.3), names.arg = c("High", paste("P.", 2:(nr.cl -1), sep = ""), "Low"))
          abline(h = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3), col = 1, lty = "dashed")
          rm(list = c("nr.cl", "sort.var", "port.data", "port.averages"))      

        # Figure 4: Beta, size, value, momentum, and option-implied equity variance
          # generate portfolio data, by sorting firms by sort.var into nr.cl portfolios and computing average excess-of-market returns and risk neutral variance at each date
          # plot the portfolios' time-series of svix2
          # choose sort.var to be "beta" or "btm" to obtain results from Panels A and C, respectively  
          nr.cl = 3
          sort.var = "beta"
          port.data = generate.portfolio.data(data, sort.var, nr.cl)
          #
          x.ticks = c((1:20)-1)*12+1
          date.labels = format(as.Date(port.data$date), "%b/%y")
          par(mfrow = c(1,1), mar = c(3.5, 3, 1.75, 2.25), cex.main=1.1, mgp= c(2,1,0))
          plot(port.data$svix2[port.data$gvkey == "P.3"], type = "l", col = "darkred", ylim = c(0, max(port.data$svix2[port.data$gvkey == "P.3"], na.rm = TRUE)), xaxt="n", yaxt="n", xlab="", ylab = "Stock variance", main = "", lwd = 3, cex.lab = 1.2, lty = "solid")
          points(port.data$svix2[port.data$gvkey == "P.2"], type = "l", col = "black", lwd = 3, lty = "dotted")
          points(port.data$svix2[port.data$gvkey == "P.1"], type = "l", col = "blue", lwd = 3, lty = "dashed")			  
          axis(side = 2, padj = 0.4, cex.axis = 1.2)
          axis(side = 1, at = x.ticks, labels = date.labels[x.ticks], padj = -0.90, cex.axis = 1.2) 
          abline(v = c(x.ticks), col = 1, lty = "dotted")
          abline(h = c(0:20)/10, col = 1, lty = "dotted")
          legend("topleft", legend = c("High", "Medium", "Low"), lty = c("solid", "dotted", "dashed"), col = c("darkred", "black", "blue"), lwd = c(3, 3, 3), bg = "white", cex= 0.8)
          rm(list = c("nr.cl", "sort.var", "port.data", "x.ticks", "date.labels"))      
           

        
          
  ##### Replication of results in Section III: Testing the model 

    
    ### Figure 5: Average equity returns in excess of the market (Panel D. One-year horizon)
      # compute time-series averages of firm's excess-of-market returns (rxm) and 0.5 x excess risk neutral variance (0.5 * svix2_basket.svix2) for firms with full sample coverage using the function ts.averages()
      # plot average excess-of-market returns vs 0.5 x average excess risk neutral variance using the function sml.plot()
        firm.averages = ts.averages(data)
        sml.plot(firm.averages)
        rm(firm.averages)

                    
    ### Figure 6: Portfolios sorted by excess stock volatility
      # generate portfolio data, by sorting firms by sort.var into nr.cl portfolios and computing average excess-of-market returns and risk neutral variance at each date
      # compute the portfolios' time-series averages of excess-of-market returns (rxm) and 0.5 x excess risk neutral variance (0.5 * svix2_basket.svix2)  using the function ts.averages()
      # plot the portfolios' average excess-of-market returns vs 0.5 x average excess risk neutral variance using the function sml.plot()
      # set nr.cl to 10, 25, 50, and 100 to obtain results from Panels A, B, C, and D, respectively  
        nr.cl = 25 
        sort.var = "svix2"
        port.data = generate.portfolio.data(data, sort.var, nr.cl)
        port.averages = ts.averages(port.data)
        sml.plot(port.averages)
        rm(list = c("nr.cl", "sort.var", "port.data", "port.averages"))
        
        
    ### Regression results presented in Tables II, III, IV, and V (in each table Panel B, column 365 days)
    
        # For each of the four regression specifications (rxm, rxm.fe, rx, rx.fe), we proceed in three steps 
          # (i) For each of the four regression specifications, define data.frames that will be used to collect the results
          # (ii) For each of the four regression specifications, obtain regression coefficients for all four regression specifications
          # (iii) For each of the four regression specifications, obtain bootstrap distributions of regression coefficient estimates
          # (iv) For each of the four regression specifications, compute the bootstrap standard errors and p-values of hypothesis tests; present the results
        #
        # For the pooled panel regressions, we also compute the "Theory adjusted R^2", i.e. the adjusted R^2 when setting the parameter values to the values implied by our theory. The computation is based on the function theory.adj.r.squared()
        # The results of the panel regressions with fixed effects can be obtained by using 
          # either the pooled panel specification, adding firm-dummies, and excluding the intercept; we do this in (ii)
          # or using plm() from the plm-package; this is slightly faster and we do this in the bootstraps in  (iii)
        # For the fixed effects regressions, we also need to compute the time-series average of the value-weighted sum of firm fixed effects; we do this using the function average.vw.fixed.effects()

    
        ### (i) define data frames, using the same structure as the tables in the paper, that will be used to collect final results
          
          # [rxm] Table II : Expected returns in excess of the market: Pooled panel regressions (Panel B, column 365 days)
            rxm.regression.results = matrix(nrow = 9, ncol = 1, NA)
            rownames(rxm.regression.results) = c("alpha", "se.alpha", "gamma", "se.gamma", "Adj R^2 (%)", "H0: alpha = 0, gamma = 0.5", "H0: gamma = 0.5", "H0: gamma = 0", "Theory adj R^2 (%)")
            colnames(rxm.regression.results) = "365 days"
            rxm.regression.results = as.data.frame(rxm.regression.results)
        
          # [rxm.fe] Table III : Expected returns in excess of the market: Panel regressions with fixed effects (Panel B, column 365 days)
            rxm.fe.regression.results = matrix(nrow = 8, ncol = 1, NA)
            rownames(rxm.fe.regression.results) = c("sum.vw.alpha", "se.sum.vw.alpha", "gamma", "se.gamma", "Adj R^2 (%)", "H0: sum.vw.alpha = 0, gamma = 0.5", "H0: gamma = 0.5", "H0: gamma = 0")
            colnames(rxm.fe.regression.results) = "365 days"
            rxm.fe.regression.results = as.data.frame(rxm.fe.regression.results)

          # [rx] Table IV : Expected excess returns: Pooled panel regressions (Panel B, column 365 days)
            rx.regression.results = matrix(nrow = 12, ncol = 1, NA)
            rownames(rx.regression.results) = c("alpha", "se.alpha", "beta", "se.beta", "gamma", "se.gamma", "Adj R^2 (%)", "H0: alpha = 0, beta = 1, gamma = 0.5", "H0: beta = 0, gamma = 0", "H0: gamma = 0.5", "H0: gamma = 0", "Theory adj R^2 (%)")
            colnames(rx.regression.results) = "365 days"
            rx.regression.results = as.data.frame(rx.regression.results)
            
          # [rx.fe] Table V : Expected excess returns: Panel regressions with fixed effects (Panel B, column 365 days)
            rx.fe.regression.results = matrix(nrow = 11, ncol = 1, NA)
            rownames(rx.fe.regression.results) = c("sum.vw.alpha", "se.sum.vw.alpha", "beta", "se.beta", "gamma", "se.gamma", "Adj R^2 (%)", "H0: sum.vw.alpha = 0, beta = 1, gamma = 0.5", "H0: beta = 0, gamma = 0", "H0: gamma = 0.5", "H0: gamma = 0")
            colnames(rx.fe.regression.results) = "365 days"
            rx.fe.regression.results = as.data.frame(rx.fe.regression.results)


        ### (ii) run the regressions and obtain coefficient estimates

          # [rxm] Table II : Expected returns in excess of the market: Pooled panel regressions (Panel B, column 365 days)
            rxm.reg = lm(rxm ~ svix2_basket.svix2, data = data)
            rxm.regression.results["alpha", "365 days"] = rxm.reg$coefficients["(Intercept)"]
            rxm.regression.results["gamma", "365 days"] = rxm.reg$coefficients["svix2_basket.svix2"]
            rxm.regression.results["Adj R^2 (%)", "365 days"] = summary(rxm.reg)$adj.r.squared * 100
            rxm.regression.results["Theory adj R^2 (%)", "365 days"] = theory.adj.r.squared(data$E.rxm, data$rxm, rxm.reg)*100

          # [rxm.fe] Table III : Expected returns in excess of the market: Panel regressions with fixed effects (Panel B, column 365 days)
            rxm.fe.reg = lm(rxm ~ svix2_basket.svix2 + factor(gvkey) - 1, data = data)
            # rxm.fe.reg = plm(rxm ~ svix2_basket.svix2 - 1, data = data, index = c("gvkey", "date"), model = "within")
            rxm.fe.regression.results["gamma", "365 days"] = rxm.fe.reg$coefficients["svix2_basket.svix2"]
            rxm.fe.regression.results["Adj R^2 (%)", "365 days"] = summary(rxm.fe.reg)$adj.r.squared * 100
            fixed.effects = data.frame("alpha.i" = rxm.fe.reg$coefficients[paste("factor(gvkey)", data.gvkeys, sep = "")], "gvkey" = data.gvkeys)
            rxm.fe.regression.results["sum.vw.alpha", "365 days"] = average.vw.fixed.effects(fixed.effects, "alpha.i", data, "gvkey", "date", "mkt.cap.weight")
            rm(fixed.effects)

          # [rx] Table IV : Expected excess returns: Pooled panel regressions (Panel B, column 365 days)
            rx.reg = lm(rx ~ index.svix2 + svix2_basket.svix2, data = data)
            rx.regression.results["alpha", "365 days"] = rx.reg$coefficients["(Intercept)"]
            rx.regression.results["beta", "365 days"] = rx.reg$coefficients["index.svix2"]
            rx.regression.results["gamma", "365 days"] = rx.reg$coefficients["svix2_basket.svix2"]
            rx.regression.results["Adj R^2 (%)", "365 days"] = summary(rx.reg)$adj.r.squared * 100
            rx.regression.results["Theory adj R^2 (%)", "365 days"] = theory.adj.r.squared(data$E.rx, data$rx, rxm.reg)*100

          # [rx.fe] Table V : Expected excess returns: Panel regressions with fixed effects (Panel B, column 365 days)
            rx.fe.reg = lm(rx ~ index.svix2 + svix2_basket.svix2 + factor(gvkey) - 1, data = data)
            # rx.fe.reg = plm(rx ~ index.svix2 + svix2_basket.svix2 - 1, data = data, index = c("gvkey", "date"), model = "within")
            rx.fe.regression.results["beta", "365 days"] = rx.fe.reg$coefficients["index.svix2"]
            rx.fe.regression.results["gamma", "365 days"] = rx.fe.reg$coefficients["svix2_basket.svix2"]
            rx.fe.regression.results["Adj R^2 (%)", "365 days"] = summary(rx.fe.reg)$adj.r.squared * 100
            fixed.effects = data.frame("alpha.i" = rx.fe.reg$coefficients[paste("factor(gvkey)", data.gvkeys, sep = "")], "gvkey" = data.gvkeys)
            rx.fe.regression.results["sum.vw.alpha", "365 days"] = average.vw.fixed.effects(fixed.effects, "alpha.i", data, "gvkey", "date", "mkt.cap.weight")
            rm(fixed.effects)


        ### (iii) obtain bootstrap distributions of regression coefficient estimates

          # set the number of bootstrap iterations (and if desired the seed); in the paper we use nr.boot.runs = 1,000 (and to obtain identical bootstrap results set the seed to 1234)
            nr.boot.runs = 5
            set.seed(1234)

          # data used in the bootstrap
            reg.data = data[, c("date", "gvkey", "mkt.cap.weight", "svix2", "rx", "index.svix2")]
            colnames(reg.data)[colnames(reg.data) == "mkt.cap.weight"] = "mkt.cap"
            reg.dates = as.Date(levels(factor(reg.data$date)))
            reg.gvkeys = levels(factor(reg.data$gvkey))
            nr.boot.firms = length(reg.gvkeys)

          # data frames that collect coefficient estimates from all bootstrap samples
            # [rxm]
              rxm.boot.coefficients = as.data.frame(matrix(ncol = 1 + 1, nrow = nr.boot.runs, NA))
              colnames(rxm.boot.coefficients) = c("(Intercept)", "svix2_basket.svix2")
            # [rxm.fe]
              rxm.fe.boot.coefficients = as.data.frame(matrix(ncol = 1 + 1, nrow = nr.boot.runs, NA))
              colnames(rxm.fe.boot.coefficients) = c("(Intercept)", "svix2_basket.svix2")
            # [rx]
              rx.boot.coefficients = as.data.frame(matrix(ncol = 1 + 2, nrow = nr.boot.runs, NA))
              colnames(rx.boot.coefficients) = c("(Intercept)", "index.svix2", "svix2_basket.svix2")
            # [rx.fe]
              rx.fe.boot.coefficients = as.data.frame(matrix(ncol = 1 + 2, nrow = nr.boot.runs, NA))
              colnames(rx.fe.boot.coefficients) = c("(Intercept)", "index.svix2", "svix2_basket.svix2")

          # construct sequences of date blocks of length rx.maturity, for nr.boot.runs samples, such that time-series of each samples is as long as the original time-series
            bootstrap.obs = length(reg.dates)
            bootstrap.block.length = rx.maturity
            last.possible.starting.obs = bootstrap.obs - bootstrap.block.length +1
            full.blocks = floor(bootstrap.obs/bootstrap.block.length)
            for (boot.run in 1:nr.boot.runs){
              for (block.i in 1:full.blocks){
                start.block = sample(1:last.possible.starting.obs)[1]
                if (block.i == 1) index = c(start.block:(start.block + bootstrap.block.length -1))
                if (block.i > 1) index = c(index, start.block:(start.block + bootstrap.block.length -1))
              }
              if (length(index) < bootstrap.obs){
                start.block = sample(1:last.possible.starting.obs)[1]
                index = c(index, start.block:(start.block + (bootstrap.obs - length(index)) -1))
              }
              if (boot.run == 1) index.all = index
              if (boot.run > 1) index.all = cbind(index.all, index)
              rm(list = c("block.i", "start.block", "index"))
            }
            rm(list = c("bootstrap.obs", "bootstrap.block.length", "last.possible.starting.obs", "full.blocks"))

          # construct the bootstrap samples, run the regressions, and collect the coefficients
            for (boot.run in 1:nr.boot.runs){
              firms = sample(1:length(reg.gvkeys), replace = TRUE)[1:nr.boot.firms]     # randomly select firms, with replacement
              for (boot.t in 1:length(reg.dates)){
                boot.reg.data.t = reg.data[reg.data$date %in% reg.dates[index.all[boot.t, boot.run]] & reg.data$gvkey %in% reg.gvkeys[firms], ]     # time-t (corresponding to the date block for iteration boot.run out of nr.boot.runs) data of the randomly selected firms
                boot.reg.data.t$time.index = boot.t     # define time index
                boot.reg.data.t$mkt.cap.weight = boot.reg.data.t$mkt.cap/sum(boot.reg.data.t$mkt.cap)     # compute market cap weights
                boot.reg.data.t$basket.svix2 = sum(boot.reg.data.t$mkt.cap.weight*boot.reg.data.t$svix2)     # compute average risk neutral variance
                boot.reg.data.t$svix2_basket.svix2 = boot.reg.data.t$svix2 - boot.reg.data.t$basket.svix2     # compute firm i's risk neutral excess variance
                boot.reg.data.t$rx.vw.port = sum(boot.reg.data.t$mkt.cap.weight*boot.reg.data.t$rx)     # compute the market excess return as the value-weighted excess returns of all firms
                boot.reg.data.t$rxm = boot.reg.data.t$rx - boot.reg.data.t$rx.vw.port     # compute firm i's return in excess of the market
                if (boot.t == 1) boot.reg.data = boot.reg.data.t
                if (boot.t > 1) boot.reg.data = as.data.frame(rbind(boot.reg.data, boot.reg.data.t))
                rm(list = c("boot.reg.data.t"))
              }
              #
              # [rxm]
                rxm.boot.reg = lm(rxm ~ svix2_basket.svix2, data = boot.reg.data)
                rxm.boot.coefficients[boot.run, names(rxm.boot.reg$coefficients)] = rxm.boot.reg$coefficients
                rm(rxm.boot.reg)
              #
              # [rxm.fe]
                rxm.fe.boot.reg = plm(rxm ~ svix2_basket.svix2 - 1, data = boot.reg.data, index = c("gvkey", "time.index"), model = "within")
                rxm.fe.boot.coefficients[boot.run, names(rxm.fe.boot.reg$coefficients)] = rxm.fe.boot.reg$coefficients
                fixed.effects = data.frame("alpha.i" = as.matrix(fixef(rxm.fe.boot.reg)), "gvkey" = names(fixef(rxm.fe.boot.reg)))
                rxm.fe.boot.coefficients[boot.run,  "(Intercept)"] = average.vw.fixed.effects(fixed.effects, "alpha.i", boot.reg.data, "gvkey", "time.index", "mkt.cap.weight")
                rm(list = c("rxm.fe.boot.reg", "fixed.effects"))
              #
              # [rx]
                rx.boot.reg = lm(rx ~ index.svix2 + svix2_basket.svix2, data = boot.reg.data)
                rx.boot.coefficients[boot.run, names(rx.boot.reg$coefficients)] = rx.boot.reg$coefficients
                rm(rx.boot.reg)
              #
              # [rx.fe]
                rx.fe.boot.reg = plm(rx ~ index.svix2 + svix2_basket.svix2 - 1, data = boot.reg.data, index = c("gvkey", "time.index"), model = "within")
                rx.fe.boot.coefficients[boot.run, names(rx.fe.boot.reg$coefficients)] = rx.fe.boot.reg$coefficients
                fixed.effects = data.frame("alpha.i" = as.matrix(fixef(rx.fe.boot.reg)), "gvkey" = names(fixef(rx.fe.boot.reg)))
                rx.fe.boot.coefficients[boot.run,  "(Intercept)"] = average.vw.fixed.effects(fixed.effects, "alpha.i", boot.reg.data, "gvkey", "time.index", "mkt.cap.weight")
                rm(list = c("rx.fe.boot.reg", "fixed.effects"))
              #
              rm(list = c("firms", "boot.reg.data", "boot.t"))
              gc(reset = TRUE)
              print(paste("Bootstrap sample", boot.run, "of", nr.boot.runs))
            }
            rm(list = c("index.all", "reg.data", "reg.dates", "reg.gvkeys", "nr.boot.firms", "nr.boot.runs", "boot.run"))

            
          ### (iv) compute bootstrap standard errors and p-values of hypothesis tests; present the results

            ### [rxm] Table II : Expected returns in excess of the market: Pooled panel regressions (Panel B, column 365 days)
              # bootstrap covariance matrix
                boot.sigma = var(rxm.boot.coefficients)
              # standard errors
                rxm.regression.results["se.alpha", "365 days"] = sqrt(boot.sigma["(Intercept)", "(Intercept)"])
                rxm.regression.results["se.gamma", "365 days"] = sqrt(boot.sigma["svix2_basket.svix2", "svix2_basket.svix2"])
              # p-values of hypothesis tests
                # H0: alpha = 0, gamma = 0.5
                  rxm.regression.results["H0: alpha = 0, gamma = 0.5", "365 days"] = linearHypothesis(rxm.reg, c("(Intercept)=0", "svix2_basket.svix2=0.5"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
                # H0: gamma = 0.5
                  rxm.regression.results["H0: gamma = 0.5", "365 days"] = linearHypothesis(rxm.reg, c("svix2_basket.svix2=0.5"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
                # H0: gamma = 0
                  rxm.regression.results["H0: gamma = 0", "365 days"] = linearHypothesis(rxm.reg, c("svix2_basket.svix2=0"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
                # # Alternatively, compute the Wald statistic and its p-value using the functions wald.stat() and p.wald(), as we'll have to do for the specifications with fixed effects below
                #     parameters = rxm.regression.results[c("alpha", "gamma"), "365 days"]
                #     # H0: alpha = 0, gamma = 0.5
                #       h0.values = c(0, 0.5)
                #       restrictions = as.matrix(rbind(c(1, 0), c(0, 1)))
                #       p.wald(wald.stat(parameters, restrictions, h0.values, boot.sigma), sum(restrictions))
                #       rm(list = c("h0.values", "restrictions"))
                #     # H0: gamma = 0.5
                #       h0.values = c(0, 0.5)
                #       restrictions = as.matrix(rbind(c(0, 0), c(0, 1)))
                #       p.wald(wald.stat(parameters, restrictions, h0.values, boot.sigma), sum(restrictions))
                #       rm(list = c("h0.values", "restrictions"))
                #     # H0: gamma = 0
                #       h0.values = c(0, 0)
                #       restrictions = as.matrix(rbind(c(0, 0), c(0, 1)))
                #       p.wald(wald.stat(parameters, restrictions, h0.values, boot.sigma), sum(restrictions))
                #       rm(list = c("h0.values", "restrictions"))
                #     #
                #     rm(parameters)
              #
              print("Table II : Expected returns in excess of the market: Pooled panel regressions (Panel B, column 365 days)")
              round(rxm.regression.results, 3)
              rm(list = c("boot.sigma", "rxm.boot.coefficients", "rxm.reg", "rxm.regression.results"))
              
            ### [rxm.fe] Table III : Expected returns in excess of the market: Panel regressions with fixed effects (Panel B, column 365 days)
              # bootstrap covariance matrix
              boot.sigma = var(rxm.fe.boot.coefficients)
              # standard errors
                rxm.fe.regression.results["se.sum.vw.alpha", "365 days"] = sqrt(boot.sigma["(Intercept)", "(Intercept)"])
                rxm.fe.regression.results["se.gamma", "365 days"] = sqrt(boot.sigma["svix2_basket.svix2", "svix2_basket.svix2"])
              # p-values of hypothesis tests
                parameters = rxm.fe.regression.results[c("sum.vw.alpha", "gamma"), "365 days"]
                # H0: sum.vw.alpha = 0, gamma = 0.5
                  h0.values = c(0, 0.5)
                  restrictions = as.matrix(rbind(c(1, 0), c(0, 1)))
                  rxm.fe.regression.results["H0: sum.vw.alpha = 0, gamma = 0.5", "365 days"] = p.wald(wald.stat(parameters, restrictions, h0.values, boot.sigma), sum(restrictions))
                  rm(list = c("h0.values", "restrictions"))
                # H0: gamma = 0.5
                  h0.values = c(0, 0.5)
                  restrictions = as.matrix(rbind(c(0, 0), c(0, 1)))
                  rxm.fe.regression.results["H0: gamma = 0.5", "365 days"] = p.wald(wald.stat(parameters, restrictions, h0.values, boot.sigma), sum(restrictions))
                  rm(list = c("h0.values", "restrictions"))
                # H0: gamma = 0
                  h0.values = c(0, 0)
                  restrictions = as.matrix(rbind(c(0, 0), c(0, 1)))
                  rxm.fe.regression.results["H0: gamma = 0", "365 days"] = p.wald(wald.stat(parameters, restrictions, h0.values, boot.sigma), sum(restrictions))
                  rm(list = c("h0.values", "restrictions"))
                #
                rm(parameters)
              #
              print("Table III : Expected returns in excess of the market: Panel regressions with fixed effects (Panel B, column 365 days)")
              round(rxm.fe.regression.results, 3)
              rm(list = c("boot.sigma", "rxm.fe.boot.coefficients", "rxm.fe.reg", "rxm.fe.regression.results"))
              
            ### [rx] Table IV : Expected excess returns: Pooled panel regressions (Panel B, column 365 days)
              # bootstrap covariance matrix
                boot.sigma = var(rx.boot.coefficients)
              # standard errors
                rx.regression.results["se.alpha", "365 days"] = sqrt(boot.sigma["(Intercept)", "(Intercept)"])
                rx.regression.results["se.beta", "365 days"] = sqrt(boot.sigma["index.svix2", "index.svix2"])
                rx.regression.results["se.gamma", "365 days"] = sqrt(boot.sigma["svix2_basket.svix2", "svix2_basket.svix2"])
              # p-values of hypothesis tests
                rx.regression.results["H0: alpha = 0, beta = 1, gamma = 0.5", "365 days"] = linearHypothesis(rx.reg, c("(Intercept)=0", "index.svix2 =1", "svix2_basket.svix2=0.5"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
                rx.regression.results["H0: beta = 0, gamma = 0", "365 days"] = linearHypothesis(rx.reg, c("index.svix2 =0", "svix2_basket.svix2=0"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
                rx.regression.results["H0: gamma = 0.5", "365 days"] = linearHypothesis(rx.reg, c("svix2_basket.svix2=0.5"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
                rx.regression.results["H0: gamma = 0", "365 days"] = linearHypothesis(rx.reg, c("svix2_basket.svix2=0"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
              #
              print("Table IV : Expected excess returns: Pooled panel regressions (Panel B, column 365 days)")
              round(rx.regression.results, 3)
              rm(list = c("boot.sigma", "rx.boot.coefficients", "rx.reg", "rx.regression.results"))

            ### [rx.fe] Table V : Expected excess returns: Panel regressions with fixed effects (Panel B, column 365 days)
              # bootstrap covariance matrix
                boot.sigma = var(rx.fe.boot.coefficients)
              # standard errors
                rx.fe.regression.results["se.sum.vw.alpha", "365 days"] = sqrt(boot.sigma["(Intercept)", "(Intercept)"])
                rx.fe.regression.results["se.beta", "365 days"] = sqrt(boot.sigma["index.svix2", "index.svix2"])
                rx.fe.regression.results["se.gamma", "365 days"] = sqrt(boot.sigma["svix2_basket.svix2", "svix2_basket.svix2"])
              # p-values of hypothesis tests
                parameters = rx.fe.regression.results[c("sum.vw.alpha", "beta", "gamma"), "365 days"]
                # H0: sum.vw.alpha = 0, gamma = 0.5
                  h0.values = c(0, 1, 0.5)
                  restrictions = as.matrix(rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)))
                  rx.fe.regression.results["H0: sum.vw.alpha = 0, beta = 1, gamma = 0.5", "365 days"] = p.wald(wald.stat(parameters, restrictions, h0.values, boot.sigma), sum(restrictions))
                  rm(list = c("h0.values", "restrictions"))
                # H0: beta = 0, gamma = 0
                  h0.values = c(0, 0, 0)
                  restrictions = as.matrix(rbind(c(0, 0, 0), c(0, 1, 0), c(0, 0, 1)))
                  rx.fe.regression.results["H0: beta = 0, gamma = 0", "365 days"] = p.wald(wald.stat(parameters, restrictions, h0.values, boot.sigma), sum(restrictions))
                  rm(list = c("h0.values", "restrictions"))
                # H0: gamma = 0.5
                  h0.values = c(0, 1, 0.5)
                  restrictions = as.matrix(rbind(c(0, 0, 0), c(0, 0, 0), c(0, 0, 1)))
                  rx.fe.regression.results["H0: gamma = 0.5", "365 days"] = p.wald(wald.stat(parameters, restrictions, h0.values, boot.sigma), sum(restrictions))
                  rm(list = c("h0.values", "restrictions"))
                # H0: gamma = 0
                  h0.values = c(0, 0, 0)
                  restrictions = as.matrix(rbind(c(0, 0, 0), c(0, 0, 0), c(0, 0, 1)))
                  rx.fe.regression.results["H0: gamma = 0", "365 days"] = p.wald(wald.stat(parameters, restrictions, h0.values, boot.sigma), sum(restrictions))
                  rm(list = c("h0.values", "restrictions"))
                #
                rm(list = c("parameters"))
              #
              print("Table V: Expected excess returns: Panel regressions with fixed effects (Panel B, column 365 days)")
              round(rx.fe.regression.results, 3)
              rm(list = c("boot.sigma", "rx.fe.boot.coefficients", "rx.fe.reg", "rx.fe.regression.results"))
              
              
    ### Figure 7: Regression estimates in subsamples
      # choose the year(s) of the subsample in subsample.years               
        subsample.years = c(2008, 2009, 2010)
        #
        subsample.data = data[substr(data$date, 1, 4) %in% subsample.years, ]
        #
        # [rxm.subsample]
          rxm.subsample.reg = lm(rxm ~ svix2_basket.svix2, data = subsample.data)
          rxm.subsample.regression.results = rxm.subsample.reg$coefficients
          names(rxm.subsample.regression.results) =c ("alpha", "gamma")
          print(rxm.subsample.regression.results)
          rm(list = c("rxm.subsample.reg", "rxm.subsample.regression.results"))
        #
        # [rx.subsample]
          rx.subsample.reg = lm(rx ~ index.svix2 + svix2_basket.svix2, data = subsample.data)      
          rx.subsample.regression.results = rx.subsample.reg$coefficients
          names(rx.subsample.regression.results) =c ("alpha", "beta", "gamma")
          print(rx.subsample.regression.results)
          rm(list = c("rx.subsample.reg", "rx.subsample.regression.results"))
        #
        rm(list = c("subsample.years", "subsample.data"))

        
    ### Regressions to test the theory using SVIX^2_{i,t}-sorted portfolios
        # generate portfolio data, by sorting firms by sort.var into nr.cl portfolios and computing average excess-of-market returns and risk neutral variance at each date
        # run the regression as for the individual firms described for Section III above
        # the code below computes estimates for regression coefficients; to obtain bootstrapped standard errors and p-values of Wald tests use the code for individual firms from above
        nr.cl = 100
        sort.var = "svix2"
        port.data = generate.portfolio.data(data, sort.var, nr.cl)

        # Table IA.3: Expected excess returns of S&P 500 stock portfolios sorted by SVIX
          # [rx.portfolio] Pooled panel regression (Panel A, column 365 days)
            rx.portfolio.reg = lm(rx ~ index.svix2 + svix2_basket.svix2, data = port.data)
            rx.portfolio.regression.results = c(rx.portfolio.reg$coefficients, summary(rx.portfolio.reg)$adj.r.squared*100) 
            names(rx.portfolio.regression.results) =c ("alpha", "beta", "gamma", "Adj R^2 (%)")
            print(round(rx.portfolio.regression.results, 3))
            rm(list = c("rx.portfolio.reg", "rx.portfolio.regression.results"))

        # Table IA.4: Expected returns in excess of the market of S&P 500 stock portfolios sorted by SVIX
          # [rxm.portfolio] Pooled panel regression (Panel A, column 365 days)
            rxm.portfolio.reg = lm(rxm ~ svix2_basket.svix2, data = port.data)
            rxm.portfolio.regression.results = c(rxm.portfolio.reg$coefficients, summary(rxm.portfolio.reg)$adj.r.squared*100) 
            names(rxm.portfolio.regression.results) =c ("alpha", "gamma", "Adj R^2 (%)")
            print(round(rxm.portfolio.regression.results, 3))
            rm(list = c("rxm.portfolio.reg", "rxm.portfolio.regression.results"))
          #
          rm(list = c("nr.cl", "sort.var", "port.data"))  
        
        

          
  ##### Replication of results in Section IV: Risk premia and stock characteristics
    # The code shows how to replicate the results in Section IV; but we cannot produce all of the results due to copyright protection restricting us from providing data on size (i.e. market capitalization) and past returns.    
        
      ### Figure 8: Portfolios sorted by beta, size, book-to-market, and momentum (Panels A and C)
        # generate portfolio data, by sorting firms by sort.var into nr.cl portfolios and computing average excess-of-market returns and risk neutral variance at each date
        # compute the portfolios' time-series averages of excess-of-market returns (rxm) and 0.5 x excess risk neutral variance (0.5 * svix2_basket.svix2)  using the function ts.averages()
        # plot the portfolios' average excess-of-market returns vs 0.5 x average excess risk neutral variance using the function sml.plot()
        # choose sort.var to be "beta" or "btm" to obtain results from Panels A and C, respectively  
          nr.cl = 25
          sort.var = "beta"
          port.data = generate.portfolio.data(data, sort.var, nr.cl)
          port.averages = ts.averages(port.data)
          sml.plot(port.averages)
          rm(list = c("nr.cl", "sort.var", "port.data", "port.averages"))
          
          
      ### Regression results presented in Tables VI and VII
        # The code shows how to replicate the results in Tables VI and VII, but the replication results are not identdical to those in the paper because, we cannot include data on size (market cap) and past returns due to copyright protection
        # For users having access to these other data, extend the code to include log(size) and past returns in the same way as beta and book-to-market below
        #
        # For both Tables VI and VII the code produces results for regressing realized returns on characteristics (first column in the tables) and regressing realized returns on characteristics and SVIX quantities (second column in the tables).
        # For the remaining columns, using expected and unexpected returns as left-hand-side variables, the procedure is the same, as we explain below. 
        #
        # For details on the setup of the regression and bootstrap procedures, see the description for replicating the results of Section III above
        #
        
        ### define data frames that will be used to collect final results and run the regressions to obtain coefficient estimates
            
          # [char.rxm] Table VI: The relationship between realized, expected, and unexpected excess-of-market returns and characteristics
            char.rxm.regression.results = matrix(nrow = 12, ncol = 2, NA)
            rownames(char.rxm.regression.results) = c("const", "se.const", "beta", "se.beta", "btm", "se.btm", "svix2_basket.svix2", "se.svix2_basket.svix2", "Adj R^2 (%)", "H0: bi = 0", "H0: bi = 0, c = 0.5", "H0: bi = 0, c = 0")
            colnames(char.rxm.regression.results) = c("(i)", "(ii)")
            char.rxm.regression.results = as.data.frame(char.rxm.regression.results)
            #
            # [char.rxm (i)]
              char.rxm.i.reg = lm(rxm ~ beta + btm, data = data)
              char.rxm.regression.results[c("const", "beta", "btm"), "(i)"] = char.rxm.i.reg$coefficients
              char.rxm.regression.results["Adj R^2 (%)", "(i)"] = summary(char.rxm.i.reg)$adj.r.squared*100
            #
            # [char.rxm (ii)]
              char.rxm.ii.reg = lm(rxm ~ beta + btm + svix2_basket.svix2, data = data)
              char.rxm.regression.results[c("const", "beta", "btm", "svix2_basket.svix2"), "(ii)"] = char.rxm.ii.reg$coefficients
              char.rxm.regression.results["Adj R^2 (%)", "(ii)"] = summary(char.rxm.ii.reg)$adj.r.squared*100
            #
            # [char.rxm (iii)] repeat regression [char.rxm (i)] using as l.h.s.-variable: data$E.rxm.estimated = rxm.reg$fitted.values
            # [char.rxm (iv)] repeat regression [char.rxm (i)] using as l.h.s.-variable: data$E.rxm = 0.5*data$svix2_basket.svix2 # already defined above
            # [char.rxm (v)] repeat regression [char.rxm (i)] using as l.h.s.-variable: data$UE.rxm.estimated = data$rxm - data$E.rxm.estimated
            # [char.rxm (vi)] repeat regression [char.rxm (i)] using as l.h.s.-variable: data$UE.rxm = data$rxm - data$E.rxm


            # [char.rxm] Table VII: The relationship between realized, expected, and unexpected returns and characteristics
              char.rx.regression.results = matrix(nrow = 14, ncol = 2, NA)
              rownames(char.rx.regression.results) = c("const", "se.const", "beta", "se.beta", "btm", "se.btm", "index.svix2", "se.index.svix2", "svix2_basket.svix2", "se.svix2_basket.svix2", "Adj R^2 (%)", "H0: bi = 0", "H0: bi = 0, c0 = 1, c1 = 0.5", "H0: bi = 0, c0 = 0, c1 = 0")
              colnames(char.rx.regression.results) = c("(i)", "(ii)")
              char.rx.regression.results = as.data.frame(char.rx.regression.results)
              #
              # [char.rx (i)]
                char.rx.i.reg = lm(rx ~ beta + btm, data = data)
                char.rx.regression.results[c("const", "beta", "btm"), "(i)"] = char.rx.i.reg$coefficients
                char.rx.regression.results["Adj R^2 (%)", "(i)"] = summary(char.rx.i.reg)$adj.r.squared*100
              #  
              # [char.rm (ii)]
                char.rx.ii.reg = lm(rxm ~ beta + btm + index.svix2 + svix2_basket.svix2, data = data)
                char.rx.regression.results[c("const", "beta", "btm", "index.svix2", "svix2_basket.svix2"), "(ii)"] = char.rx.ii.reg$coefficients
                char.rx.regression.results["Adj R^2 (%)", "(ii)"] = summary(char.rx.ii.reg)$adj.r.squared*100
              #
              # [char.rx (iii)] repeat regression [char.rx (i)] using as l.h.s.-variable: data$E.rx.estimated = rx.reg$fitted.values
              # [char.rx (iv)] repeat regression [char.rx (i)] using as l.h.s.-variable: data$E.rx = data$index.svix2 + 0.5*data$svix2_basket.svix2 # already defined above
              # [char.rx (v)] repeat regression [char.rx (i)] using as l.h.s.-variable: data$UE.rx.estimated = data$rx - data$E.rx.estimated
              # [char.rx (vi)] repeat regression [char.rx (i)] using as l.h.s.-variable: data$UE.rx = data$rx - data$E.rx


        ### obtain bootstrap distributions of regression coefficient estimates
                
          # set the number of bootstrap iterations (and if desired the seed); in the paper we use nr.boot.runs = 1,000 (and to obtain identical bootstrap results set the seed to 1234)
            nr.boot.runs = 5
            # set.seed(1234)

          # data used in the bootstrap
            reg.data = data[, c("date", "gvkey", "mkt.cap.weight", "svix2", "rx", "index.svix2", "beta", "btm")]
            colnames(reg.data)[colnames(reg.data) == "mkt.cap.weight"] = "mkt.cap"
            reg.dates = as.Date(levels(factor(reg.data$date)))
            reg.gvkeys = levels(factor(reg.data$gvkey))
            nr.boot.firms = length(reg.gvkeys)

          # define data.frames that collect coefficient estimates from all bootstrap samples
            #
            # [char.rxm]
              # [char.rxm.i]
                char.rxm.i.boot.coefficients = as.data.frame(matrix(ncol = 1 + 2, nrow = nr.boot.runs, NA))
                colnames(char.rxm.i.boot.coefficients) = c("(Intercept)", "beta", "btm")
              # [char.rxm.ii]
                char.rxm.ii.boot.coefficients = as.data.frame(matrix(ncol = 1 + 3, nrow = nr.boot.runs, NA))
                colnames(char.rxm.ii.boot.coefficients) = c("(Intercept)", "beta", "btm", "svix2_basket.svix2")
            #
            # [char.rx]
              # [char.rx.i]
                char.rx.i.boot.coefficients = as.data.frame(matrix(ncol = 1 + 2, nrow = nr.boot.runs, NA))
                colnames(char.rx.i.boot.coefficients) = c("(Intercept)", "beta", "btm")
              # [char.rx.ii]
                char.rx.ii.boot.coefficients = as.data.frame(matrix(ncol = 1 + 4, nrow = nr.boot.runs, NA))
                colnames(char.rx.ii.boot.coefficients) = c("(Intercept)", "beta", "btm", "index.svix2", "svix2_basket.svix2")

          # construct sequences of date blocks of length rx.maturity, for nr.boot.runs samples, such that time-series of each samples is as long as the original time-series
            bootstrap.obs = length(reg.dates)
            bootstrap.block.length = rx.maturity
            last.possible.starting.obs = bootstrap.obs - bootstrap.block.length +1
            full.blocks = floor(bootstrap.obs/bootstrap.block.length)
            for (boot.run in 1:nr.boot.runs){
              for (block.i in 1:full.blocks){
                start.block = sample(1:last.possible.starting.obs)[1]
                if (block.i == 1) index = c(start.block:(start.block + bootstrap.block.length -1))
                if (block.i > 1) index = c(index, start.block:(start.block + bootstrap.block.length -1))
              }
              if (length(index) < bootstrap.obs){
                start.block = sample(1:last.possible.starting.obs)[1]
                index = c(index, start.block:(start.block + (bootstrap.obs - length(index)) -1))
              }
              if (boot.run == 1) index.all = index
              if (boot.run > 1) index.all = cbind(index.all, index)
              rm(list = c("block.i", "start.block", "index"))
            }
            rm(list = c("bootstrap.obs", "bootstrap.block.length", "last.possible.starting.obs", "full.blocks"))
            
            # construct the bootstrap samples, run the regressions, and collect the coefficients
            for (boot.run in 1:nr.boot.runs){
              firms = sample(1:length(reg.gvkeys), replace = TRUE)[1:nr.boot.firms]
              for (boot.t in 1:length(reg.dates)){
                boot.reg.data.t = reg.data[reg.data$date %in% reg.dates[index.all[boot.t, boot.run]] & reg.data$gvkey %in% reg.gvkeys[firms], ]
                boot.reg.data.t$time.index = boot.t
                boot.reg.data.t$mkt.cap.weight = boot.reg.data.t$mkt.cap/sum(boot.reg.data.t$mkt.cap)
                boot.reg.data.t$basket.svix2 = sum(boot.reg.data.t$mkt.cap.weight*boot.reg.data.t$svix2)
                boot.reg.data.t$svix2_basket.svix2 = boot.reg.data.t$svix2 - boot.reg.data.t$basket.svix2
                #
                boot.reg.data.t$rx.vw.port = sum(boot.reg.data.t$mkt.cap.weight*boot.reg.data.t$rx)
                boot.reg.data.t$rxm = boot.reg.data.t$rx - boot.reg.data.t$rx.vw.port
                #
                if (boot.t == 1) boot.reg.data = boot.reg.data.t
                if (boot.t > 1) boot.reg.data = as.data.frame(rbind(boot.reg.data, boot.reg.data.t))
                rm(list = c("boot.reg.data.t"))
              }

                # [char.rxm.i]
                  char.rxm.i.boot.reg = lm(rxm ~ beta + btm, data = boot.reg.data)
                  char.rxm.i.boot.coefficients[boot.run, names(char.rxm.i.boot.reg$coefficients)] = char.rxm.i.boot.reg$coefficients
                  rm(char.rxm.i.boot.reg)
                #
                # [char.rxm.ii]
                  char.rxm.ii.boot.reg = lm(rxm ~ beta + btm + svix2_basket.svix2, data = boot.reg.data)
                  char.rxm.ii.boot.coefficients[boot.run, names(char.rxm.ii.boot.reg$coefficients)] = char.rxm.ii.boot.reg$coefficients
                  rm(char.rxm.ii.boot.reg)

                #
                # [char.rx.i]
                  char.rx.i.boot.reg = lm(rx ~ beta + btm, data = boot.reg.data)
                  char.rx.i.boot.coefficients[boot.run, names(char.rx.i.boot.reg$coefficients)] = char.rx.i.boot.reg$coefficients
                  rm(char.rx.i.boot.reg)
                  #
                  # [char.rx.ii]
                  char.rx.ii.boot.reg = lm(rx ~ beta + btm + index.svix2 + svix2_basket.svix2, data = boot.reg.data)
                  char.rx.ii.boot.coefficients[boot.run, names(char.rx.ii.boot.reg$coefficients)] = char.rx.ii.boot.reg$coefficients
                  rm(char.rx.ii.boot.reg)
                  #
                rm(list = c("firms", "boot.reg.data", "boot.t"))
                gc(reset = TRUE)
                print(paste("Bootstrap sample", boot.run, "of", nr.boot.runs))
              }
              rm(list = c("index.all", "reg.data", "reg.dates", "reg.gvkeys", "nr.boot.firms", "nr.boot.runs", "boot.run"))

              
        ### compute bootstrap standard errors and p-values of hypothesis tests; present the results
              
          ### [char.rxm] Table VI: The relationship between realized, expected, and unexpected excess-of-market returns and characteristics
            
            ## First column
              # bootstrap covariance matrix
                boot.sigma = var(char.rxm.i.boot.coefficients)
              # standard errors
                char.rxm.regression.results[c("se.const", "se.beta", "se.btm"), "(i)"] = sqrt(diag(boot.sigma))
              # p-values of hypothesis tests
                char.rxm.regression.results["H0: bi = 0", "(i)"] = linearHypothesis(char.rxm.i.reg, c("beta=0", "btm=0"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
              #
              rm(list = c("boot.sigma", "char.rxm.i.boot.coefficients", "char.rxm.i.reg"))

            ## Second column
              # bootstrap covariance matrix
                boot.sigma = var(char.rxm.ii.boot.coefficients)
              # standard errors
                char.rxm.regression.results[c("se.const", "se.beta", "se.btm", "se.svix2_basket.svix2"), "(ii)"] = sqrt(diag(boot.sigma))
              # p-values of hypothesis tests
                char.rxm.regression.results["H0: bi = 0", "(ii)"] = linearHypothesis(char.rxm.ii.reg, c("beta=0", "btm=0"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
                char.rxm.regression.results["H0: bi = 0, c = 0.5", "(ii)"] = linearHypothesis(char.rxm.ii.reg, c("beta=0", "btm=0", "svix2_basket.svix2=0.5"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
                char.rxm.regression.results["H0: bi = 0, c = 0", "(ii)"] = linearHypothesis(char.rxm.ii.reg, c("beta=0", "btm=0", "svix2_basket.svix2=0"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
              #
              rm(list = c("boot.sigma", "char.rxm.ii.boot.coefficients", "char.rxm.ii.reg"))
                
              ## Show results
                print("Table VI: The relationship between realized, expected, and unexpected excess-of-market returns and characteristics")
                round(char.rxm.regression.results, 3)
                rm(char.rxm.regression.results)
                    
          ### [char.rx] Table VII: The relationship between realized, expected, and unexpected returns and characteristics
                
            ## First column
              # bootstrap covariance matrix
                boot.sigma = var(char.rx.i.boot.coefficients)
              # standard errors
                char.rx.regression.results[c("se.const", "se.beta", "se.btm"), "(i)"] = sqrt(diag(boot.sigma))
              # p-values of hypothesis tests
                char.rx.regression.results["H0: bi = 0", "(i)"] = linearHypothesis(char.rx.i.reg, c("beta=0", "btm=0"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
              #
              rm(list = c("boot.sigma", "char.rx.i.boot.coefficients", "char.rx.i.reg"))

            ## Second column
              # bootstrap covariance matrix
                boot.sigma = var(char.rx.ii.boot.coefficients)
              # standard errors
                char.rx.regression.results[c("se.const", "se.beta", "se.btm", "se.index.svix2", "se.svix2_basket.svix2"), "(ii)"] = sqrt(diag(boot.sigma))
              # p-values of hypothesis tests
                char.rx.regression.results["H0: bi = 0", "(ii)"] = linearHypothesis(char.rx.ii.reg, c("beta=0", "btm=0"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
                char.rx.regression.results["H0: bi = 0, c0 = 1, c1 = 0.5", "(ii)"] = linearHypothesis(char.rx.ii.reg, c("beta=0", "btm=0", "index.svix2=1", "svix2_basket.svix2=0.5"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
                char.rx.regression.results["H0: bi = 0, c0 = 0, c1 = 0", "(ii)"] = linearHypothesis(char.rx.ii.reg, c("beta=0", "btm=0", "index.svix2=0", "svix2_basket.svix2=0"), vcov = boot.sigma, test = "Chisq")[2, c("Pr(>Chisq)")]
              #
              rm(list = c("boot.sigma", "char.rx.ii.boot.coefficients", "char.rx.ii.reg"))
                
            ## Show results
              print("Table VII: The relationship between realized, expected, and unexpected returns and characteristics")
              round(char.rx.regression.results, 3)
              rm(char.rx.regression.results)
              
                            


  ##### Replication of results in Section V: Out-of-sample analysis

    ### Cross-sectional variation in expected returns (Figure 9, Panel B)
      # compute quantiles of model expected returns and expected returns using the CAPM with a market risk premium of 6%
        expected.return.quantiles = data.frame("date" = as.Date(levels(factor(data$date))), "q10" = NA, "q25" = NA, "q75" = NA, "q90" = NA)
        capm6pct.expected.return.quantiles = data.frame("date" = as.Date(levels(factor(data$date))), "q10" = NA, "q25" = NA, "q75" = NA, "q90" = NA)
        for (date.t in expected.return.quantiles$date){
          E.rx.t = data$E.rx[data$date == date.t]
          expected.return.quantiles[expected.return.quantiles$date == date.t, c("q10", "q25", "q75", "q90")] = quantile(E.rx.t, prob = c(0.1, 0.25, 0.75, 0.9))
          capm6pct.E.rx.t = data$beta[data$date == date.t] * 0.06
          capm6pct.expected.return.quantiles[expected.return.quantiles$date == date.t, c("q10", "q25", "q75", "q90")] = quantile(capm6pct.E.rx.t, prob = c(0.1, 0.25, 0.75, 0.9), na.rm = TRUE)
          rm(list = c("E.rx.t", "capm6pct.E.rx.t"))
        }          
      # plot expected return quantiles 90% vs 10%
        x.ticks = seq(from = 1, to = nrow(expected.return.quantiles), by = 48)
        x.ticks2 = seq(from = 1, to = nrow(expected.return.quantiles), by = 12)
        date.labels = format(as.Date(expected.return.quantiles$date), "%b/%y")
        par(mfrow = c(1, 1), mar = c(3.5, 3, 1.75, 2.25), cex.main=1, mgp= c(2,1,0))
        y.min = min(expected.return.quantiles$q90 - expected.return.quantiles$q10, na.rm = TRUE, 0)
        y.max = max(expected.return.quantiles$q90 - expected.return.quantiles$q10, na.rm = TRUE)
        plot(expected.return.quantiles$q90 - expected.return.quantiles$q10, type = "l", ylim = c(y.min, y.max), xaxt="n", yaxt="n", xlab="", ylab="Difference in expected returns", col="blue", lwd=2, main = TeX(paste("$", 90, "\\% - ", 10, "\\%$ quantiles of expected returns", sep = "")))
        points(capm6pct.expected.return.quantiles$q90 - capm6pct.expected.return.quantiles$q10, type = "l", col="black", lwd=1.75, lty = "12")
        axis(side = 2, padj = 0.4, cex.axis = 0.9, ylab = "")
        axis(side = 1, at = x.ticks2, labels = rep("", length(x.ticks2)), padj = -0.90, cex.axis = 0.9)
        axis(side = 1, at = x.ticks, labels = date.labels[x.ticks], padj = -0.90, cex.axis = 0.9)
        abline(h = 0, lty = "44", col = grey(0.15))
        legend("topleft", legend = c("Model", "6% CAPM"), lty = c("solid", "12"), col = c("blue", "black"), lwd = c(2, 1.75), cex= 0.8, bty = "n")
      

    # The rest of the code shows how to replicate the other results in Section V. 
    # We cannot produce all of the results due to copyright protection restricting us from providing some of the data used in this section. 
    # From the code below it will be obvious how to evaluate other benchmark forecasts
    #          
    # First, we create new data.frame (oos.data) that restricts sample to observations, for which beta is available (oos.data has 90,306 observations compare to data which has 91,585 observations)
    # Second, we define the function os.r2(), which computes the out-of-sample R^2 (in %) using
        # the `model.forecast'
        # relative to the `benchmark.forecast'
        # for the returns `realized'
    #
    # NB: all model-based forecasts are 
      # multiplied by (1+rf), i.e. the cumulative gross risk free return over the forecast horizon (see footnote 13 in the paper)
      # adjusted for the forecast horizon (which does not make a difference in the replication exercise because the forecast horizon is one year)

      oos.data = data[!is.na(data$beta),]

      os.r2 <- function(model.forecast, benchmark.forecast, realized){
        os.r2 = (1 - sum((model.forecast - realized)^2)/sum((benchmark.forecast - realized)^2))*100
        return(os.r2)
      }

        
      ### Forecasting excess returns (Table IX, Panel A, column 365 days)
        #
          model.forecast = oos.data$E.rx * (1 + oos.data$rf) * (rx.maturity/12)
          realized = oos.data$rx.os
        #
        # using SVIX^2_t as the benchmark forecast (row 1)
          benchmark.forecast = oos.data$index.svix2 * (1 + oos.data$rf) * (rx.maturity/12)
          os.r2(model.forecast, benchmark.forecast, realized)
        #
        # using 6% as the benchmark forecast (row 4)
          benchmark.forecast = 0.06
          os.r2(model.forecast, benchmark.forecast, realized)
        #
        # using the SVIX^2_{i,t} as the benchmark forecast (row 5)
          benchmark.forecast = oos.data$svix2 * (1 + oos.data$rf) * (rx.maturity/12)
          os.r2(model.forecast, benchmark.forecast, realized)
        #
        # using SVIX^2_t-CAPM as the benchmark forecast (row 9)
          benchmark.forecast = oos.data$beta * oos.data$index.svix2  * (1 + oos.data$rf) * (rx.maturity/12)
          os.r2(model.forecast, benchmark.forecast, realized)
        #
        # using the 6%-CAPM as the benchmark forecast  (row 10)
          benchmark.forecast = oos.data$beta * 0.06
          os.r2(model.forecast, benchmark.forecast, realized)

      ### Forecasting excess-of-market-returns (Table IX, Panel B, column 365 days)
        #
        model.forecast = oos.data$E.rxm * (1 + oos.data$rf) * (rx.maturity/12)
        realized = oos.data$rxm.os
        #
        # using a random walk (zero)  benchmark forecast (row 1)
          benchmark.forecast = 0
          os.r2(model.forecast, benchmark.forecast, realized)
        #
        # using SVIX^2_t-CAPM as the benchmark forecast (row 4)
          benchmark.forecast = (oos.data$beta - 1) * oos.data$index.svix2  * (rx.maturity/12)
          os.r2(model.forecast, benchmark.forecast, realized)
        #
        # using the 6%-CAPM as the benchmark forecast  (row 5)
          benchmark.forecast = (oos.data$beta - 1) * 0.06
          os.r2(model.forecast, benchmark.forecast, realized)
      



                








