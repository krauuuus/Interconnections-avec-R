require(Spillover)
cryptos<-read.csv(file.choose())
require(dplyr)
require(tidyr)
require(ggplot2)
require(vars)
require(timeDate)
crypto<-subset(cryptos, select=-c(X))


crypto2 = {
  crypto %>% dplyr::mutate(Date_1 = as.Date(as.character(crypto[,1]))) %>% 
    tibble %>% dplyr::select(-1) %>% 
    pivot_longer(cols = -Date_1,names_to = "variables") %>%
    ggplot(aes(x = Date_1,y = value)) +
    geom_line() +
    facet_wrap(~variables,scales = "free_y") + 
    labs(title = "Returns", x= "", y = "")}
crypto2


VARselect(crypto[,-1], lag.max=7, type="none")#1/7
require(Spillover)
crypto %>% 
  dplyr::select(-Date) %>% 
  VAR(p=1) %>% 
  G.spillover(standardized = TRUE) %>% 
  round(2)

col=c(BTC="red",
      BNB="cyan",
      ADA="blue",
      XRP="darkorange", 
      ETH="burlywood",
      LTC='green')

  
crypto_links<-as.matrix(crypto %>% 
                dplyr::select(-Date) %>% 
                VAR(p=1) %>% 
                G.spillover(standardized = TRUE) %>% 
                round(2))
circlize::chordDiagram(crypto_links[1:6,1:6], link.border=0)


crypto_dyn<-dynamic.spillover(crypto, p=1, width=200, standardized=TRUE )

crypto_from<-plotdy(crypto_dyn, direction="from")
crypto_from+ylim(0,15)

crypto_to<-plotdy(crypto_dyn, direction="to")
crypto_to+ylim(0,15)

crypto_net<-plotdy(crypto_dyn, direction="net")
crypto_net+ylim(-3,4)

crypto_pairwise<-plotdy(crypto_dyn, direction="net_pairwise")

