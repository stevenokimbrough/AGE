globals [ dSlope ; (negative of) demand curve slope: Price = priceIntercept - dSlope * totalQuantity
          episodeAbsoluteCount ; the number of episodes begun in the current run
          currentQ ; the sum of all the quantity bids in the current episode
          currentP ; the market price in the current episode, given the demand curve and currentQ
          marketTotalReward
          quantityIntercept
          priceIntercept ; redundant with pIntercept
          m-quantity ; monopoly quantity, assuming costs are proportional to quantities: ki*Qi
          m-price ; monopoly price, assuming we have the m-quantity
          runningAverageBidList ; used to hold runningAvgLength currentQs
          runningAverageBid ; mean of runningAverageBidList
          runningAverageBidSD ; standard deviation of runningAverageBidList
          cournotQuantity
          runningAverageCournotList ; for the Cournot values
          runningAverageCournot
          runningAverageMonopolyList ; for the monopoly values, m-quantity
          runningAverageMonopoly
          competitiveQuantity
          runningAverageCompetitiveList ; for the Cournot values
          runningAverageCompetitive
          totalUnitProductionCost ; the sum of all the production costs for all the turtles
          monopolyQFirm0 monopolyQFirm1
          daVersion
          RepetitionCounter ;
          ratioOfferedCournot ; ratio of runningAverageBid to runningAverageCournot.  Collusion if < 1.
          rPrime ; see paper with Fred
  ] ; end of globals

breed [pAndAers pAndA]     ; Probe-and-Adjust

turtles-own [quantityBid totalReward bidsWon unitProductionCost
             netReward ; equivalent to Fred's totreward. See FindNetReward
             myEpochBids ; list of bids I made during the present epoch
             marketEpochBids ; list of all bids made during the present epoch
             ]
pAndAers-own [epsilon delta currentBaseValue epochLength episodeCount
              upRewards downRewards ; for "Own Rewards"
              marketUpRewards marketDownRewards ; for "Market Rewards"
              updateType pricesUp pricesDown PQUp PQdown
              runningAverageReward ; each pAndA player will keep track of the running average of its rewards, across episodes
              marketOwnMixture ; in [0, 1] 100% = Market Rewards, 0% = Own Rewards, 75% = 75% Market Rewards, 25% Own Rewards
              upMarketOwnMixedRewards downMarketOwnMixedRewards ; for Market-Own Mixed Rewards, used with parameter marketOwnMixture
               ;
              ]
          ;delta ; probe-and-adjust parameter
          ;epsilon ; probe-and-adjust parameter
          ;currentBaseValue ; probe-and-adjust parameter, the base from which probes are made
          ;epochLength ; probe-and-adjust parameter; the number of episodes in an epoch
          ;episodeCount ; the count of the number of episodes in the current epoch
          ;upRewards ; list of rewards received, P*Q(i), when bidding at or above currentAnchorValue
          ;downRewards ; list of rewards received, P*Q(i), when bidding below currentAnchorValue

to Setup ;[daRepetitionCount]
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  ;set RepetitionCount daRepetitionCount
  set daVersion "$Id: OligopolyPutQuantity.nlogo 4089 2014-03-20 21:01:52Z sok $"
  if (daRandomSeed != "system clock")
    [random-seed daRandomSeed]
  set priceIntercept InitPIntercept
  set quantityIntercept (InitPIntercept / InitDSlope)
  set dSlope InitDSlope

  set runningAverageBidList []
  set runningAverageCournotList []
  set runningAverageMonopolyList []
  set runningAverageCompetitiveList []

  if (initialBaseQuantityFirm0 > initialBaseQuantityFirm1)
    [print "WARNING! initialBaseQuantityFirm0 > initialBaseQuantityFirm1."]

  ;PlotInitialDemandCurve
  ; Initialize other globals
  set episodeAbsoluteCount 0

  ; Create agents/firms supplying product to the market
  create-pAndAers numPandAFirms
  ask turtles [set totalReward 0
               set bidsWon 0
               set netReward 0
               set myEpochBids []
               set marketEpochBids []]
  ask pAndAers [set upRewards []
                set downRewards []
                set marketUpRewards []
                set marketDownRewards []
                set pricesUp []
                set pricesDown []
                set PQUp []
                set PQDown []
                set runningAverageReward []
                set marketOwnMixture  (runresult ((word "marketOwnMixtureFirm"  [who] of self)))
                set upMarketOwnMixedRewards []
                set downMarketOwnMixedRewards []
                ]
  ; Initialize agents/firms supplying product to the market
  if (numPandAFirms = 1)
    [ask pAndA 0
      [set epsilon epsilonFirm0
       set delta deltaFirm0
       set currentBaseValue initialBaseQuantityFirm0
       set unitProductionCost unitCostFirm0
       set epochLength epochLengthFirm0
       set episodeCount 0
       set updateType updateTypeFirm0
      ]
      ]
  if (numPandAFirms = 2)
    [ask pAndA 0
      [set epsilon epsilonFirm0
       set delta deltaFirm0
       set currentBaseValue initialBaseQuantityFirm0
       set unitProductionCost unitCostFirm0
       set epochLength epochLengthFirm0
       set episodeCount 0
       set updateType updateTypeFirm0
      ]
    ask pAndA 1
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue initialBaseQuantityFirm1
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
  ] ; end of if numPandAFirms = 2
  if (numPandAFirms = 3)
    [Setup3PandAFirms]
  if (numPandAFirms = 4)
    [Setup4PandAFirms]
  if (numPandAFirms = 5)
    [Setup5PandAFirms]
  if (numPandAFirms = 10)
    [Setup10PandAFirms]

  set totalUnitProductionCost 0
  ask turtles
    [set totalUnitProductionCost (totalUnitProductionCost + unitProductionCost)]
   ;;;;
   ; priceIntercept and dSlope determine the quantityIntercept
   ; Price = PriceIntercept - dSlope*Quantity
   ; QuantityIntercept is the Quantity when Price = 0
   ; So, quantityIntercept = priceIntercept/dSlope
   set quantityIntercept priceIntercept / dSlope
   ; This is the monopolist's quantity when the monopolist has costs
   ; proportional to quantity: k*Quantity
   ; Here we explicitly assume that Firm0's costs are universal wrt
   ; determining the monopoly quantity.
   set m-quantity ((priceIntercept - unitCostFirm0) / (2 * dSlope))
   set m-price (InitPIntercept - InitDSlope * m-quantity)
   set cournotQuantity (numPandAFirms * priceIntercept - totalUnitProductionCost) / ((numPandAFirms + 1) * dSlope)
   set competitiveQuantity (priceIntercept - unitCostFirm0) / dSlope
   PlotInitialDemandCurve
end ; of setup

to Go
  ; See the documentation in the Information tab.
  set episodeAbsoluteCount episodeAbsoluteCount + 1
  ; 0. Perturb the demand curve if we're doing random-walk-as
  if (random-walk-as)
   [set priceIntercept (random-float 2 * aDelta) - aDelta + priceIntercept
   set quantityIntercept priceIntercept / dSlope
   ; This is the monopolist's quantity given proportional costs of totalUnitProductionCost
   set m-quantity ((priceIntercept - totalUnitProductionCost) / (2 * dSlope))
   ] ; if of if (random-as)

  ; 1. All of the turtles are asked to determine a bid quantity. Each records his own, in quantityBid.
  ask turtles [
    set-quantityBid
  ] ; end of ask turtles
  ; 2. Sum the bids, the Q(i)s, to get currentQ
  set currentQ 0
  ask turtles [set currentQ (currentQ + quantityBid)]
  ; 2a. Collect quantities bid
  let quantitiesBid []
  ask turtles [set quantitiesBid lput quantityBid quantitiesBid]
  ask turtles [Update-QuantitiesBid(quantitiesBid)]
  ; 3. Determine the current price, P, for the episode
  set currentP PriceGivenQuantity(currentQ)
  ask turtles [findnetreward]
  ; Calculate total profits by all players:
  set marketTotalReward 0
  ask turtles [set marketTotalReward (marketTotalReward + netReward)]
  ; 4. Turtles observe currentP, and record stats for the episode
  ask turtles [Observe-and-Record(currentP)(currentQ)]
  ; 5. Have agents/suppliers postpare episode
  ask turtles [Postpare-Episode]
  ; 6. Collect and calculate statistics
  set runningAverageBidList lput currentQ runningAverageBidList
  if (length runningAverageBidList > runningAvgLength)
    [set runningAverageBidList but-first runningAverageBidList]
  set runningAverageBid mean runningAverageBidList
  if length runningAverageBidList > 1
    [set runningAverageBidSD standard-deviation runningAverageBidList]
  ;set runningAverageCournotList lput cournotQuantity runningAverageCournotList
  ;if (length runningAverageCournotList > runningAvgLength)
   ; [set runningAverageCournotList but-first runningAverageCournotList]
  ;set runningAverageCournot mean runningAverageCournotList
  set runningAverageMonopolyList lput m-Quantity runningAverageMonopolyList
  if (length runningAverageMonopolyList > runningAvgLength)
    [set runningAverageMonopolyList but-first runningAverageMonopolyList]
  set runningAverageMonopoly mean runningAverageMonopolyList
  set runningAverageCompetitiveList lput competitiveQuantity runningAverageCompetitiveList
  if (length runningAverageCompetitiveList > runningAvgLength)
    [set runningAverageCompetitiveList but-first runningAverageCompetitiveList]
  set runningAverageCompetitive mean runningAverageCompetitiveList
  ; 7. Plot monopoly quantity, bid Qs, and other statistics:
  ;ifelse (runningAverageCournot > 0)
   ;  [set ratioOfferedCournot (runningAverageBid / runningAverageCournot)]
    ; [set ratioOfferedCournot "N/A"]
  set ratioOfferedCournot (runningAverageBid / cournotQuantity)
  if (cournotQuantity - m-quantity > 0)
   [set rPrime (cournotQuantity - runningAverageBid) / (cournotQuantity - m-quantity)]
;  ifelse (runningAverageCournot - runningAverageMonopoly > 0)
;     [set rPrime (runningAverageCournot - runningAverageBid) / (runningAverageCournot - runningAverageMonopoly)]
;     [set rPrime "N/A"]
  Plot-Quantities(m-quantity)(currentQ) ; (priceIntercept / (2 * dSlope))(currentQ)
  Plot-Bids
  Plot-Average-Rewards
end ; of Go

to Postpare-Episode
  ;print "episodeAbsoluteCount = " + episodeAbsoluteCount + " " + who-of self
  if (is-pAndA? self) [
    let meanUp 0
    let meanDown 0
    if (episodeCount >= epochLength) ; then update and reset
      [; The following I added for debugging purposes 2007-5-25, but
       ; it's a better way to handle things anyway.
       let meanUpMarketReturns 0
       let meanDownMarketReturns 0
       ifelse (marketUpRewards = [])
         [set meanUpMarketReturns 0]
         [set meanUpMarketReturns mean marketUpRewards]
       ifelse (marketDownRewards = [])
         [set meanDownMarketReturns 0]
         [set meanDownMarketReturns mean marketDownRewards]

       let meanUpMarketOwnMixedRewards  0
       let meanDownMarketOwnMixedRewards  0
       ifelse (upMarketOwnMixedRewards = [])
         [set meanUpMarketOwnMixedRewards 0]
         [set meanUpMarketOwnMixedRewards mean upMarketOwnMixedRewards]
       ifelse (downMarketOwnMixedRewards = [])
         [set meanDownMarketOwnMixedRewards 0]
         [set meanDownMarketOwnMixedRewards mean downMarketOwnMixedRewards]
       ;print "111: " + who-of self + " " + meanUpMarketReturns + " " + meanUpMarketOwnMixedRewards + " " + meanDownMarketReturns + " " + meanDownMarketOwnMixedRewards
       ;print "222: " + who-of self + " " + marketUpRewards
       ;print "333: " + who-of self + " " + upMarketOwnMixedRewards
       ; end of added for debugging
       if (updateType = "Own Returns") [
       ifelse (upRewards = [])
         [set meanUp 0]
         [set meanUp mean upRewards]
       ifelse (downRewards = [])
         [set meanDown 0]
         [set meanDown mean downRewards]
       ifelse (meanUp > meanDown)
         [set currentBaseValue currentBaseValue + epsilon]
         [set currentBaseValue max (list epsilon (currentBaseValue - epsilon))]
         ] ; end of if (updateType = "Own Returns")

       if (updateType = "Market Returns") [
         ifelse (marketUpRewards = [])
           [set meanUp 0]
           [set meanUp mean marketUpRewards]
         ; oops! this looks like a bug!! 2007-5-25: ifelse (downRewards = [])
         ; should be: It is a bug. But it didn't make much difference.
         ifelse (marketDownRewards = [])
           [set meanDown 0]
           [set meanDown mean marketDownRewards]
         ifelse (meanUp > meanDown)
           [set currentBaseValue currentBaseValue + epsilon]
           [set currentBaseValue max (list epsilon (currentBaseValue - epsilon))]
         ] ; end of if (updateType = "Market Returns")

       if (updateType = "Market Prices") [
         ifelse (pricesUp = [])
           [set meanUp 0]
           [set meanUp mean pricesUp]
         ifelse (pricesDown = [])
           [set meanDown 0]
           [set meanDown mean pricesDown]
         ifelse (meanUp > meanDown)
           [set currentBaseValue currentBaseValue + epsilon]
           [set currentBaseValue max (list epsilon (currentBaseValue - epsilon))]
         ] ; end of if (updateType = "Market Prices")

       if (updateType = "Market P*Q") [
         ifelse (PQUp = [])
           [set meanUp 0]
           [set meanUp mean PQUp]
         ifelse (PQDown = [])
           [set meanDown 0]
           [set meanDown mean PQDown]
         ifelse (meanUp > meanDown)
           [set currentBaseValue currentBaseValue + epsilon]
           [set currentBaseValue max (list epsilon (currentBaseValue - epsilon))]
         ] ; end of if (updateType = "Market P*Q")

       if (updateType = "Market Returns, Constrained by Own Returns") [
         let referenceLevel 0
         let tolerance 0
         if (referenceBidGroup = "All bids")
           [set referenceLevel mean marketEpochBids
            set tolerance epsilon]
         if (referenceBidGroup = "Upper quartile of bids")
           [set referenceLevel mean UpperQuartile(marketEpochBids)
            set tolerance delta]
         ifelse (mean myEpochBids + tolerance <= referenceLevel)
           [set currentBaseValue currentBaseValue + epsilon]
           [ ; else, I'm ok so I play the good citizen
            ifelse (marketUpRewards = [])
             [set meanUp 0]
             [set meanUp mean marketUpRewards]
            ifelse (downRewards = [])
             [set meanDown 0]
             [set meanDown mean marketDownRewards]
            ifelse (meanUp > meanDown)
             [set currentBaseValue currentBaseValue + epsilon]
             [set currentBaseValue max (list epsilon (currentBaseValue - epsilon))]
            ] ; end of else in ifelse (mean myEpochBids...
         ] ; end of if (updateType = "Market Returns and Own Returns")

       if (updateType = "Market Prices and Own Returns") [
         ifelse ((mean myEpochBids) + epsilon <= mean marketEpochBids)
           [set currentBaseValue currentBaseValue + epsilon]
           [ ; else, I'm ok so I play the good citizen
            ifelse (pricesUp = [])
             [set meanUp 0]
             [set meanUp mean pricesUp]
            ifelse (pricesDown = [])
             [set meanDown 0]
             [set meanDown mean pricesDown]
             ;print "meanUp = " + meanUp + " meanDown = " + meanDown
            ifelse (meanUp > meanDown)
             [set currentBaseValue currentBaseValue + epsilon]
             [set currentBaseValue max (list epsilon (currentBaseValue - epsilon))]
            ] ; end of else in ifelse (mean myEpochBids...
         ] ; end of if (updateType = "Market Prices and Own Returns")

       if (updateType = "Market Prices and Own Returns") [
         ifelse ((mean myEpochBids) + epsilon <= mean marketEpochBids)
           [set currentBaseValue currentBaseValue + epsilon]
           [ ; else, I'm ok so I play the good citizen
            ifelse (PQUp = [])
             [set meanUp 0]
             [set meanUp mean PQUp]
            ifelse (PQDown = [])
             [set meanDown 0]
             [set meanDown mean PQDown]
            ifelse (meanUp > meanDown)
             [set currentBaseValue currentBaseValue + epsilon]
             [set currentBaseValue max (list epsilon (currentBaseValue - epsilon))]
            ] ; end of else in ifelse (mean myEpochBids...
         ] ; end of if (updateType = "Market Prices and Own Returns")

        if (updateType = "Mixture of Market and Own Returns") [
            ifelse (upMarketOwnMixedRewards = [])
             [set meanUp 0]
             [set meanUp mean upMarketOwnMixedRewards]
            ifelse (downMarketOwnMixedRewards = [])
             [set meanDown 0]
             [set meanDown mean downMarketOwnMixedRewards]
            ifelse (meanUp > meanDown)
             [set currentBaseValue currentBaseValue + epsilon]
             [set currentBaseValue max (list epsilon (currentBaseValue - epsilon))]
         ] ; end of if (updateType = "Mixture of Market Own Returns")

       set upRewards []
       set downRewards []
       set marketUpRewards []
       set marketDownRewards []
       set pricesUp []
       set pricesDown []
       set PQUp []
       set PQDown []
       set upMarketOwnMixedRewards []
       set downMarketOwnMixedRewards []
       set marketEpochBids []
       set myEpochBids []
       set episodeCount 0
      ] ; of if episodeCount >= epochLength
   ] ; end of if is-pAndA?
end ; of postpare-episode

to Plot-Bids
  set-current-plot "Individual Bids"
  set-current-plot-pen "Agent0"
  plot [currentbasevalue] of panda 0
  if (numPandAFirms > 1)
    [set-current-plot-pen "Agent1"
     plot [currentbasevalue] of panda 1]
  if (numPandAFirms > 2)
    [set-current-plot-pen "Agent2"
     plot [currentbasevalue] of panda 2]
  if (numPandAFirms > 3)
    [set-current-plot-pen "Agent3"
     plot [currentbasevalue] of panda 3]
  if (numPandAFirms > 4)
    [set-current-plot-pen "Agent4"
     plot [currentbasevalue] of panda 4
     ]
  if (numPandAFirms > 5)
    [set-current-plot-pen "Agent5"
     plot [currentbasevalue] of panda 5
     ]
  if (numPandAFirms > 6)
    [set-current-plot-pen "Agent6"
     plot [currentbasevalue] of panda 6
     ]
  if (numPandAFirms > 7)
    [set-current-plot-pen "Agent7"
     plot [currentbasevalue] of panda 7
     ]
  if (numPandAFirms > 8)
    [set-current-plot-pen "Agent8"
     plot [currentbasevalue] of panda 8
     ]
  if (numPandAFirms > 9)
    [set-current-plot-pen "Agent9"
     plot [currentbasevalue] of panda 9
     ]
end ; of Plot-Bids

to Plot-Average-Rewards
  set-current-plot "Average Rewards"
  set-current-plot-pen "Agent0"
  let daAvg ([totalReward] of panda 0 / episodeAbsoluteCount)
  plot daAvg
  if (numPandAFirms > 1)
    [set-current-plot-pen "Agent1"
     set daAvg ([totalReward] of panda 1 / episodeAbsoluteCount)
     plot daAvg]
  if (numPandAFirms > 2)
    [set-current-plot-pen "Agent2"
     set daAvg ([totalReward] of panda 2 / episodeAbsoluteCount)
     plot daAvg]
  if (numPandAFirms > 3)
    [set-current-plot-pen "Agent3"
     set daAvg ([totalReward] of panda 3 / episodeAbsoluteCount)
     plot daAvg]
  if (numPandAFirms > 4)
    [set-current-plot-pen "Agent4"
     set daAvg ([totalReward] of panda 4 / episodeAbsoluteCount)
     plot daAvg
     ]
  if (numPandAFirms > 5)
    [set-current-plot-pen "Agent5"
     set daAvg ([totalReward] of panda 5 / episodeAbsoluteCount)
     plot daAvg
     ]
  if (numPandAFirms > 6)
    [set-current-plot-pen "Agent6"
     set daAvg ([totalReward] of panda 6 / episodeAbsoluteCount)
     plot daAvg
     ]
  if (numPandAFirms > 7)
    [set-current-plot-pen "Agent7"
     set daAvg ([totalReward] of panda 7 / episodeAbsoluteCount)
     plot daAvg
     ]
  if (numPandAFirms > 8)
    [set-current-plot-pen "Agent8"
     set daAvg ([totalReward] of panda 8 / episodeAbsoluteCount)
     plot daAvg
     ]
  if (numPandAFirms > 9)
    [set-current-plot-pen "Agent9"
     set daAvg ([totalReward] of panda 9 / episodeAbsoluteCount)
     plot daAvg
     ]
end ; of Plot-Average-Rewards

to Plot-Quantities [daMonopoly daTotalBid]
  set-current-plot "Quantities Bid"
  set-current-plot-pen "Total Quantity Bid"
  plot daTotalBid
  ;set-current-plot-pen "Monopoly Quantity"
  ;plot daMonopoly
  ;set competitiveQuantity priceIntercept / dSlope
  set-current-plot-pen "Competitive Quantity"
  plot competitiveQuantity
  ; set cournotQuantity (numPandAFirms * priceIntercept - totalUnitProductionCost) / ((numPandAFirms + 1) * dSlope)
  set-current-plot-pen "Cournot Quantity"
  plot cournotQuantity
  set-current-plot-pen "Monopoly Q, Firm(s) 1"
  if (numPandAFirms > 1)
    [set monopolyQFirm1 ((priceIntercept - [unitProductionCost] of pAndA 1) / (2 * dSlope))
     plot monopolyQFirm1]
  set-current-plot-pen "Monopoly Q, Firm 0"
  set monopolyQFirm0 ((priceIntercept - [unitProductionCost] of pAndA 0) / (2 * dSlope))
  plot monopolyQFirm0
  set-current-plot-pen "Monopoly Quantity"
  plot daMonopoly
end ; of Plot-Quantities

to Observe-and-Record [price quantity]
  if (is-pAndA? self) [
    let reward (price * quantityBid) - (unitProductionCost * quantityBid)
    let mixedReward (1 - marketOwnMixture) * reward + (marketOwnMixture * (marketTotalReward )  / numPandAFirms)
    ;print reward + " " + mixedReward + " " + marketTotalReward
    set totalReward totalReward + reward
    ifelse (quantityBid >= currentBaseValue)
      [set upRewards lput reward upRewards
       ;set pricesDown lput 0 pricesDown
       set pricesUp lput price pricesUp
       set PQUp lput (price * quantity) PQUp]
      [set downRewards lput reward downRewards
       ;set pricesUp lput 0 pricesUp
       set pricesDown lput price pricesDown
       set PQDown lput (price * quantity) PQDown]
    ifelse (quantityBid >= currentBaseValue)
      [set marketUpRewards lput marketTotalReward marketUpRewards
       set upMarketOwnMixedRewards lput mixedReward upMarketOwnMixedRewards]
      [set marketDownRewards lput marketTotalReward marketDownRewards
       set downMarketOwnMixedRewards  lput mixedReward downMarketOwnMixedRewards]
   ] ; end of if is-pAndA?
end ; of observe-and-record

to Set-QuantityBid
 if (is-pAndA? self) [
  set quantityBid max (list 0 ((random-float 2 * delta) - delta + currentBaseValue))
  set episodeCount episodeCount + 1
  set myEpochBids lput quantityBid myEpochBids ; record my bid in the epoch record
 ]
end ; of Set-QuantityBid

to PlotInitialDemandCurve
  set-current-plot "Initial Demand Curve"
  set-current-plot-pen "Demand Curve"
  plot-pen-up
  plotxy 0 InitPIntercept
  plot-pen-down
  plotxy InitPIntercept / InitDSlope 0 ; plot to the quantity intercept
  let InitMonopolyQ m-quantity ; InitPIntercept / 2
  let InitMonopolyP InitPIntercept - dSlope * InitMonopolyQ ;InitPIntercept / (2 * InitDSlope)
  set-current-plot-pen "Monopoly Q"
  plot-pen-up
  plotxy InitMonopolyQ 0
  plot-pen-down
  plotxy InitMonopolyQ InitMonopolyP
  set-current-plot-pen "Monopoly P"
  plot-pen-up
  plotxy 0 InitMonopolyP
  plot-pen-down
  plotxy InitMonopolyQ InitMonopolyP
end ; of PlotInitialDemandCurve

to-report PriceGivenQuantity [daQuantity]
  let daPrice priceIntercept - dSlope * daQuantity
  report max (list 0 daPrice)
end ; of to-report PriceGivenQuantity

to FindNetReward
  set netReward 0
  if (is-pAndA? self) [
  set netReward ((CurrentP * quantityBid) - (unitProductionCost * quantityBid))
  set runningAverageReward lput netReward runningAverageReward
   if (length runningAverageReward > runningAvgLength)
    [set runningAverageReward but-first runningAverageReward]
  ]
end ; of FindNetReward

to Update-QuantitiesBid [daBids]
 if (is-pAndA? self) [
  foreach daBids [ [?1] ->
  set marketEpochBids lput ?1 marketEpochBids
  ] ; end of foreach
 ]
end ; of Update-QuantitiesBid

to Setup3PandAFirms
    ask pAndA 0
      [set epsilon epsilonFirm0
       set delta deltaFirm0
       set currentBaseValue initialBaseQuantityFirm0
       set unitProductionCost unitCostFirm0
       set epochLength epochLengthFirm0
       set episodeCount 0
       set updateType updateTypeFirm0
      ]
    ask pAndA 1
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue initialBaseQuantityFirm1
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 2
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue ((initialBaseQuantityFirm1 + initialBaseQuantityFirm0) / 2)
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
end ; of Setup3PandAFirms

to Setup4PandAFirms
  let increment (initialBaseQuantityFirm1 - initialBaseQuantityFirm0)
    ask pAndA 0
      [set epsilon epsilonFirm0
       set delta deltaFirm0
       set currentBaseValue initialBaseQuantityFirm0
       set unitProductionCost unitCostFirm0
       set epochLength epochLengthFirm0
       set episodeCount 0
       set updateType updateTypeFirm0
      ]
    ask pAndA 1
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue initialBaseQuantityFirm1
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 2
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue  (initialBaseQuantityFirm1 + increment)
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 3
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue  (initialBaseQuantityFirm1 + (2 * increment))
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]


end ; of Setup4PandAFirms

to Setup5PandAFirms
  let increment (initialBaseQuantityFirm1 - initialBaseQuantityFirm0)
    ask pAndA 0
      [set epsilon epsilonFirm0
       set delta deltaFirm0
       set currentBaseValue initialBaseQuantityFirm0
       set unitProductionCost unitCostFirm0
       set epochLength epochLengthFirm0
       set episodeCount 0
       set updateType updateTypeFirm0
      ]
    ask pAndA 1
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue initialBaseQuantityFirm1
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 2
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue  (initialBaseQuantityFirm1 + increment)
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 3
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue  (initialBaseQuantityFirm1 + (2 * increment))
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 4
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue  (initialBaseQuantityFirm1 + (3 * increment))
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]

end ; of Setup5PandAFirms

to Setup10PandAFirms
  let increment (initialBaseQuantityFirm1 - initialBaseQuantityFirm0)
    ask pAndA 0
      [set epsilon epsilonFirm0
       set delta deltaFirm0
       set currentBaseValue initialBaseQuantityFirm0
       set unitProductionCost unitCostFirm0
       set epochLength epochLengthFirm0
       set episodeCount 0
       set updateType updateTypeFirm0
      ]
    ask pAndA 1
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue initialBaseQuantityFirm1
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 2
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue (initialBaseQuantityFirm1 + increment)
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 3
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue (initialBaseQuantityFirm1 + (2 * increment))
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 4
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue (initialBaseQuantityFirm1 + (3 * increment))
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 5
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue (initialBaseQuantityFirm1 + (4 * increment))
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 6
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue (initialBaseQuantityFirm1 + (5 * increment))
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 7
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue (initialBaseQuantityFirm1 + (6 * increment))
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 8
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue (initialBaseQuantityFirm1 + (7 * increment))
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
    ask pAndA 9
     [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentBaseValue (initialBaseQuantityFirm1 + (8 * increment))
     set unitProductionCost unitCostFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set updateType updateTypeFirm1
    ]
end ; of Setup10PandAFirms

to-report UpperQuartile [ aList ]
  if (empty? aList)
   [print "In UpperQuartile reporter. The list is empty."
    let bob 1 / 1]  ; fix this to halt
  let bList reverse sort aList
  ;print bList
  let daQuartileSize int (length aList / 4)
  ;print daQuartileSize
  ifelse (length bList mod 4 = 0)
    [report sublist bList 0 (daQuartileSize)]
    [report sublist bList 0 (daQuartileSize + 1)]
  ;ifelse (daRemainder

end

to RRepetitions
print "Output coming from the RRepetitions procedure:"
;let daRepetitionCount RepetitionCount
if (file-exists? "oligopolyBidQuantity-repetitions.txt")
  [file-delete "oligopolyBidQuantity-repetitions.txt"]
; Open the parameter sweep file and print the column headers.
; Also print the column headers to the output window
; Then close the parameter sweep file

file-open "oligopolyBidQuantity-repetitions.txt"
file-print "RepetitionCount,runningAverageBid,runningAverageBidSd,ratioOfferedCournot,rPrime,meanRewardFirm0,meanRewardFirm1"
file-close
print "RepetitionCount,runningAverageBid,runningAverageBidSd,ratioOfferedCournot,rPrime,meanRewardFirm0,meanRewardFirm1"
let numFirms numPandAFirms
let runningAverageBidAccumulator []
;let pAndAAccumulator []
let pAndA0Accumulator []
let pAndA1Accumulator []
let pAndA2Accumulator []
let pAndA3Accumulator []
let pAndA4Accumulator []
let pAndA5Accumulator []
let pAndA6Accumulator []
let pAndA7Accumulator []
let pAndA8Accumulator []
let pAndA9Accumulator []
;foreach (n-values NumRepetitions [?])
let RepetitionCount 0
while [RepetitionCount < NumRepetitions]
[
Setup ; (RepetitionCount)
let torun numEpisodesToRun
while [torun > 0]
[go
set torun torun - 1
]
set RepetitionCount RepetitionCount + 1
set RepetitionCounter RepetitionCount
;set rPrime (runningAverageCournot - runningAverageBid) / (runningAverageCournot - runningAverageMonopoly)
file-open "oligopolyBidQuantity-repetitions.txt"
file-print (word RepetitionCount  ","  runningAverageBid  ","  runningAverageBidSD  ","  ratioOfferedCournot  ","  rPrime  ","  mean [runningAverageReward] of pAndA 0  ","  mean [runningAverageReward] of pAndA 1)
file-close
print (word RepetitionCount   ","   runningAverageBid   ","   runningAverageBidSD   ","   ratioOfferedCournot   ","   rPrime    ","   mean [runningAverageReward] of pAndA 0   ","   mean [runningAverageReward] of pAndA 1)
set runningAverageBidAccumulator fput runningAverageBid runningAverageBidAccumulator

; This is really inelegant, but I couldn't get run and runresult to do it for me.
; Could use lists as arrays... well I'm in a hurry and the max is 10
; Actually, to test in the future, I can try creating local agents mirroring
; the panda agents. The local ones won't be zapped by Setup. I tried it doesn't work.
set pAnda0Accumulator fput mean [runningAverageReward] of pAndA 0 pAnda0Accumulator
if (numFirms > 1)
[set pAnda1Accumulator fput mean [runningAverageReward] of pAndA 1 pAnda1Accumulator]
if (numFirms > 2)
[set pAnda2Accumulator fput mean [runningAverageReward] of pAndA 2 pAnda2Accumulator]
if (numFirms > 3)
[set pAnda3Accumulator fput mean [runningAverageReward] of pAndA 3 pAnda3Accumulator]
if (numFirms > 4)
[set pAnda4Accumulator fput mean [runningAverageReward] of pAndA 4 pAnda4Accumulator]
if (numFirms > 5)
[set pAnda5Accumulator fput mean [runningAverageReward] of pAndA 5 pAnda5Accumulator]
if (numFirms > 6)
[set pAnda6Accumulator fput mean [runningAverageReward] of pAndA 6 pAnda6Accumulator]
if (numFirms > 7)
[set pAnda7Accumulator fput mean [runningAverageReward] of pAndA 7 pAnda7Accumulator]
if (numFirms > 8)
[set pAnda8Accumulator fput mean [runningAverageReward] of pAndA 8 pAnda8Accumulator]
if (numFirms > 9)
[set pAnda9Accumulator fput mean [runningAverageReward] of pAndA 9 pAnda9Accumulator]


] ; end of  while
if (RepetitionCounter > 1)
[
print (word "overall mean sd of avg. bid,"   mean runningAverageBidAccumulator   ","   standard-deviation runningAverageBidAccumulator)
print "\\begin{center}"
print "\\begin{tabular}{|r|r|r|}\\hline"
print "Firm & Mean of Running Averages & SD of Running Averages \\\\ \\hline\\hline"
if (numFirms > 0)
[print (word "0 & "   mean pAnda0Accumulator   " & "   standard-deviation pAnda0Accumulator   "\\\\ \\hline")]
if (numFirms > 1)
[print (word "1 & "  mean pAnda1Accumulator  " & "  standard-deviation pAnda1Accumulator  "\\\\ \\hline")]
if (numFirms > 2)
[print (word "2 & "  mean pAnda2Accumulator  " & "  standard-deviation pAnda2Accumulator  "\\\\ \\hline")]
if (numFirms > 3)
[print (word "3 &"  mean pAnda3Accumulator  " & "  standard-deviation pAnda3Accumulator   "\\\\ \\hline")]
if (numFirms > 4)
[print (word "4 & "  mean pAnda4Accumulator  " & "  standard-deviation pAnda4Accumulator  "\\\\ \\hline")]
if (numFirms > 5)
[print (word "5 & "  mean pAnda5Accumulator  " & "  standard-deviation pAnda5Accumulator   "\\\\ \\hline")]
if (numFirms > 6)
[print (word "6 & "  mean pAnda6Accumulator  " & "  standard-deviation pAnda6Accumulator  "\\\\ \\hline")]
if (numFirms > 7)
[print (word "7 & "  mean pAnda7Accumulator  " & "  standard-deviation pAnda7Accumulator  "\\\\ \\hline")]
if (numFirms > 8)
[print (word "8 & "  mean pAnda8Accumulator  " & "  standard-deviation pAnda8Accumulator  "\\\\ \\hline")]
if (numFirms > 9)
[print (word "9 & "  mean pAnda9Accumulator  " & "  standard-deviation pAnda9Accumulator  "\\\\ \\hline")]
print "\\end{tabular}"
print "\\end{center}"
]
end

to ParameterSweep
 reset-timer
; Delete the parameter sweep file if it exists, so that we
; don't append data to existing data.
;if (file-exists? "oligopolyBidQuantity-parametersweep-epochlengths.txt")
;  [file-delete "oligopolyBidQuantity-parametersweep-epochlengths.txt"]
if (file-exists? "oligopolyBidQuantity-parametersweep.txt")
  [file-delete "oligopolyBidQuantity-parametersweep.txt"]
; Open the parameter sweep file and print the column headers.
; Also print the column headers to the output window
; Then close the parameter sweep file
;file-open "oligopolyBidQuantity-parametersweep-epochlengths.txt"

file-open "oligopolyBidQuantity-parametersweep.txt"
file-print "RepetitionCount,numEpisodesToRun,epochLengthFirm0,epochLengthFirm1,meanRewardFirm0,meanRewardFirm1,runningAverageBid,runningAverageBidSD,deltaFirm0,deltaFirm1,epsilonFirm0,epsilonFirm1,timer"
print "RepetitionCount,numEpisodesToRun,epochLengthFirm0,epochLengthFirm1,meanRewardFirm0,meanRewardFirm1,runningAverageBid,runningAverageBidSD,deltaFirm0,deltaFirm1,epsilonFirm0,epsilonFirm1,timer"
file-close

let deltaFirm0sweep 0
let deltaFirm0hold deltaFirm0
let deltaFirm1sweep 0
let deltaFirm1hold deltaFirm1

let epochLengthFirm0sweep 0
let epochLengthFirm0hold epochLengthFirm0
let epochLengthFirm1sweep 0
let epochLengthFirm1hold epochLengthFirm1

let torun 0
let RepetitionCount 0
while [RepetitionCount < NumRepetitions]
[
foreach (list 6600 7600)
[ [?1] -> set numEpisodesToRun ?1
foreach (list 2.2  3.8)
[ [??1] -> set deltaFirm0 ??1
foreach (list 2.2  3.8)
[ [???1] -> set deltaFirm1 ???1
foreach (list 0.4 1.0)
[ [????1] -> set epsilonFirm0 ????1
foreach (list 0.4 1.0)
[ [?????1] -> set epsilonFirm1 ?????1
foreach (list 24  36)
[ [??????1] -> set epochLengthFirm0sweep ??????1
foreach (list 24  36)
[ [???????1] -> set epochLengthFirm1sweep ???????1
Setup
set RepetitionCounter RepetitionCount
set epochLengthFirm0 epochLengthFirm0sweep
set epochLengthFirm1 epochLengthFirm1sweep
set torun numEpisodesToRun
while [torun > 0]
[go
set torun torun - 1
]
file-open "oligopolyBidQuantity-parametersweep.txt"
file-print (word RepetitionCount  ","  numEpisodesToRun  ","  epochLengthFirm0  ","  epochLengthFirm1  ","   mean [runningAverageReward] of pAndA 0  ","
 mean [runningAverageReward] of pAndA 1  ","  runningAverageBid  ","  runningAverageBidSD  ","  deltaFirm0  ","  deltaFirm1
   ","  epsilonFirm0  ","  epsilonFirm1  ","  timer)
file-close
print (word RepetitionCount  ","  numEpisodesToRun  ","  epochLengthFirm0  ","  epochLengthFirm1  ","   mean [runningAverageReward] of pAndA 0
","   mean [runningAverageReward] of pAndA 1  ","  runningAverageBid  ","  runningAverageBidSD
","  deltaFirm0  ","  deltaFirm1  ","  epsilonFirm0  ","  epsilonFirm1  ","  timer)

 ] ; end of first foreach
 ] ; end of second foreach
 ] ; end of third foreach
 ] ; end of fourth foreach
 ] ; end of fifth foreach
 ] ; end of sixth foreach
 ] ; ebd if seventh foreach
set RepetitionCount RepetitionCount + 1
set RepetitionCounter RepetitionCount

] ; end of while
; Restore original values
set epochLengthFirm0 epochLengthFirm0hold
set epochLengthFirm1 epochLengthFirm1hold
end

to test
print "Hi from test."
let pAndA0Accumulator []
print pAndA0Accumulator
set pAndA0Accumulator fput "bob" pAndA0Accumulator
print pAndA0Accumulator
set pAndA0Accumulator fput "carol" runresult ((word "pAndA"  0  "Accumulator"))
print pAndA0Accumulator
;set (run runresult ("pAndA" + 0 + "Accumulator") fput "ted" runresult ("pAndA" + 0 + "Accumulator"))
print pAndA0Accumulator
end
@#$#@#$#@
GRAPHICS-WINDOW
508
230
711
434
-1
-1
5.57143
1
10
1
1
1
0
1
1
1
-17
17
-17
17
0
0
1
ticks
30.0

CHOOSER
8
75
146
120
numPandAFirms
numPandAFirms
1 2 3 4 5 10
1

SLIDER
0
251
172
284
InitPIntercept
InitPIntercept
0
400
400.0
1
1
NIL
HORIZONTAL

SLIDER
0
284
172
317
InitDSlope
InitDSlope
0
10
2.0
0.1
1
NIL
HORIZONTAL

MONITOR
904
510
1030
555
Init. Demand Intercept
InitPIntercept / InitDSlope
3
1
11

PLOT
921
201
1274
510
Initial Demand Curve
Quantity = Q
Price = P
0.0
100.0
0.0
500.0
true
true
"" ""
PENS
"Demand Curve" 1.0 0 -16777216 true "" ""
"Monopoly Q" 1.0 0 -2674135 true "" ""
"Monopoly P" 1.0 0 -10899396 true "" ""

BUTTON
8
10
68
43
Setup
Setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1030
510
1130
555
Init. Monopoly Q
;InitPIntercept / (2 * InitDSlope)\nm-quantity
3
1
11

MONITOR
1130
510
1229
555
Init. Monopoly P
;InitPIntercept / 2.0\n;InitPIntercept - InitDSlope * m-quantity\nm-price
4
1
11

SLIDER
0
120
172
153
deltaFirm0
deltaFirm0
0
5
3.0
0.1
1
NIL
HORIZONTAL

SLIDER
0
153
172
186
deltaFirm1
deltaFirm1
0
5
3.0
0.1
1
NIL
HORIZONTAL

SLIDER
0
186
172
219
epsilonFirm0
epsilonFirm0
0
5
0.7
0.1
1
NIL
HORIZONTAL

SLIDER
0
219
172
252
epsilonFirm1
epsilonFirm1
0
5
0.7
0.1
1
NIL
HORIZONTAL

SLIDER
2
350
204
383
initialBaseQuantityFirm0
initialBaseQuantityFirm0
0
200
40.0
1
1
NIL
HORIZONTAL

SLIDER
2
383
205
416
initialBaseQuantityFirm1
initialBaseQuantityFirm1
0
200
40.0
1
1
NIL
HORIZONTAL

SLIDER
172
219
344
252
epochLengthFirm0
epochLengthFirm0
0
100
30.0
1
1
NIL
HORIZONTAL

SLIDER
172
251
344
284
epochLengthFirm1
epochLengthFirm1
0
100
30.0
1
1
NIL
HORIZONTAL

BUTTON
63
10
118
43
NIL
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
543
10
1273
201
Quantities Bid
Y-axis: Quantities assuming proportional costs
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Monopoly Quantity" 1.0 0 -16777216 true "" ""
"Total Quantity Bid" 1.0 0 -2674135 true "" ""
"Competitive Quantity" 1.0 0 -14835848 true "" ""
"Cournot Quantity" 1.0 0 -13345367 true "" ""
"Monopoly Q, firm 0" 1.0 0 -11221820 true "" ""
"Monopoly Q, firm(s) 1" 1.0 0 -5825686 true "" ""

SLIDER
172
282
344
315
unitCostFirm0
unitCostFirm0
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
171
315
343
348
unitCostFirm1
unitCostFirm1
0
100
0.0
1
1
NIL
HORIZONTAL

PLOT
343
201
922
511
Individual Bids
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Agent0" 1.0 0 -16777216 true "" ""
"Agent1" 1.0 0 -13345367 true "" ""
"Agent2" 1.0 0 -11221820 true "" ""
"Agent3" 1.0 0 -5825686 true "" ""
"Agent4" 1.0 0 -10899396 true "" ""
"Agent5" 1.0 0 -2674135 true "" ""
"Agent6" 1.0 0 -955883 true "" ""
"Agent7" 1.0 0 -6459832 true "" ""
"Agent8" 1.0 0 -1184463 true "" ""
"Agent9" 1.0 0 -8630108 true "" ""

CHOOSER
2
416
328
461
updateTypeFirm0
updateTypeFirm0
"Own Returns" "Market Returns" "Market Prices" "Market P*Q" "Market Returns, Constrained by Own Returns" "Market Prices and Own Returns" "Market P*Q and Own Returns" "Mixture of Market and Own Returns"
0

CHOOSER
2
461
328
506
updateTypeFirm1
updateTypeFirm1
"Own Returns" "Market Returns" "Market Prices" "Market P*Q" "Market Returns, Constrained by Own Returns" "Market Prices and Own Returns" "Market P*Q and Own Returns" "Mixture of Market and Own Returns"
0

SWITCH
8
43
167
76
random-walk-as
random-walk-as
1
1
-1000

SLIDER
-1
317
171
350
aDelta
aDelta
0
8.0
3.0
0.01
1
NIL
HORIZONTAL

MONITOR
182
509
295
554
Running Ave. Q
runningAverageBid
3
1
11

MONITOR
623
511
727
556
NIL
cournotQuantity
3
1
11

MONITOR
474
510
623
555
NIL
runningAverageMonopoly
3
1
11

MONITOR
727
511
904
556
Competitive Q at Firm0 costs
competitiveQuantity
3
1
11

CHOOSER
205
347
343
392
daRandomSeed
daRandomSeed
"system clock" 0 1 2 3 4 5 6 7 8 9 10 100
1

PLOT
171
10
543
217
Average Rewards
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Agent0" 1.0 0 -16777216 true "" ""
"Agent1" 1.0 0 -13345367 true "" ""
"Agent2" 1.0 0 -11221820 true "" ""
"Agent3" 1.0 0 -5825686 true "" ""
"Agent4" 1.0 0 -10899396 true "" ""
"Agent5" 1.0 0 -2674135 true "" ""
"Agent6" 1.0 0 -955883 true "" ""
"Agent7" 1.0 0 -6459832 true "" ""
"Agent8" 1.0 0 -1184463 true "" ""
"Agent9" 1.0 0 -8630108 true "" ""

MONITOR
5
562
407
607
                                                                              Current Version
daVersion
3
1
11

MONITOR
407
558
547
603
Firm 0: Average Reward
mean [runningAverageReward] of pAndA 0
3
1
11

MONITOR
547
558
684
603
Firm 1: Average Reward
mean [runningAverageReward] of pAndA 1
3
1
11

MONITOR
683
558
819
603
Firm 2: Average Reward
mean [runningAverageReward] of pAndA 2
3
1
11

MONITOR
819
558
956
603
Firm 3: Average Reward
mean [runningAverageReward] of pAndA 3
3
1
11

MONITOR
956
558
1091
603
Firm 4: Average Reward
mean [runningAverageReward] of pAndA 4
3
1
11

MONITOR
1091
558
1227
603
Firm 5: Average Reward
mean [runningAverageReward] of pAndA 5
3
1
11

MONITOR
295
510
423
555
Running Ave. Q SD
runningAverageBidSD
3
1
11

SLIDER
722
673
899
706
numEpisodesToRun
numEpisodesToRun
0
10000
6600.0
100
1
NIL
HORIZONTAL

BUTTON
118
10
173
43
Go N
let torun numEpisodesToRun\nwhile [torun > 0]\n[go\nset torun torun - 1\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
5
607
188
640
marketOwnMixtureFirm0
marketOwnMixtureFirm0
0
1.0
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
188
607
365
640
marketOwnMixtureFirm1
marketOwnMixtureFirm1
0.0
1.0
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
364
607
542
640
marketOwnMixtureFirm2
marketOwnMixtureFirm2
0.0
1.0
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
542
607
721
640
marketOwnMixtureFirm3
marketOwnMixtureFirm3
0.0
1.0
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
720
607
899
640
marketOwnMixtureFirm4
marketOwnMixtureFirm4
0.0
1.0
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
5
640
188
673
marketOwnMixtureFirm5
marketOwnMixtureFirm5
0.0
1.0
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
188
640
365
673
marketOwnMixtureFirm6
marketOwnMixtureFirm6
0.0
1.0
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
364
640
542
673
marketOwnMixtureFirm7
marketOwnMixtureFirm7
0.0
1.0
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
542
640
721
673
marketOwnMixtureFirm8
marketOwnMixtureFirm8
0.0
1.0
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
721
640
899
673
marketOwnMixtureFirm9
marketOwnMixtureFirm9
0.0
1.0
0.5
0.01
1
NIL
HORIZONTAL

MONITOR
899
607
1035
652
Firm 6: Average Reward
mean [runningAverageReward] of pAndA 6
2
1
11

MONITOR
1035
607
1170
652
Firm 7: Average Reward
mean [runningAverageReward] of pAndA 7
2
1
11

MONITOR
899
655
1036
700
Firm 8: Average Reward
mean [runningAverageReward] of pAndA 8
2
1
11

MONITOR
1036
655
1170
700
Firm 9: Average Reward
mean [runningAverageReward] of pAndA 9
2
1
11

CHOOSER
2
505
182
550
referenceBidGroup
referenceBidGroup
"All bids" "Upper quartile of bids"
1

SLIDER
2
549
206
582
runningAvgLength
runningAvgLength
0
1000
1000.0
10
1
NIL
HORIZONTAL

BUTTON
5
673
114
706
NIL
RRepetitions
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
114
673
286
706
NumRepetitions
NumRepetitions
0
100
30.0
1
1
NIL
HORIZONTAL

MONITOR
286
673
412
718
NIL
RepetitionCounter
3
1
11

BUTTON
603
673
722
706
NIL
ParameterSweep
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
411
673
482
718
ratioOC
;runningAverageBid / runningAverageCournot\nratioOfferedCournot
6
1
11

MONITOR
482
673
603
718
NIL
rPrime
3
1
11

@#$#@#$#@
$Id: OligopolyPutQuantity.nlogo 4089 2014-03-20 21:01:52Z sok $

## NOTE WELL

I've added some features and the documentation is now incomplete.

## WHAT IS IT?

This NetLogo program, oligopolyBidQuantity.nlogo, models the following situation. A market for a product exists with a linear price function. A number of firms, N, supply the market by offering quantities, Q(i) for firm i, during each period (i = 0, 1, ..., (N-1)). The market price, P, for a unit of the product in a given period is a linear function of the total quantity, Q, of the product on offer: P(Q) = P = priceIntercept - dSlope*Q (priceIntercept > 0, dSlope > 0).  Thus, as Q, the total quantity on offer, increases, the market's unit price for the product, P, decreases. (Until it reaches 0. We assume that P >= 0.)

The market properates discretely by periods. During each period, the firms supplying the market bid quantities, Q(i), that they wish to offer for that period. We assume that the firms only bid quantities that they are able to supply and that all bids are in fact realized. At the end of each period, Q is determined as the sum of the Q(i)s and the market price is determined as 

     P = max(0, priceIntercept - dSlope*Q).  
    

(This is set in the procedure PriceGivenQuantity.)  
Each firm, i, then receives revenue = P*Q(i), and the period ends.  If a stopping condition has been reached, the run is over; otherwise the market continues with the start of a new period.

## HOW IT WORKS

The firms supplying the market and offering quantity bids are the only agents in this model.  At the end of each time period, each firm, i, observes the the realized price, P, for that period. The firm calculates its reward (profit, �(i)) for the period as

     reward = �(i) = revenue - costs 
     =  P*Q(i) - unitProductionCost(i)*Q(i)
     = (P - unitProductionCost(i))*Q(i)

(So we assume that each firm, i, has its own production costs, which are proportional to the amount supplied, Q(i).) Each firm records its reward for the period as follows.  If the firm's bid in the period, Q(i), was greater than or equal to its currentBaseValue, its anchor bid value, then the reward received is recorded in the firm's record of upRewards; otherwise the reward is recorded in the firm's record of downRewards.

Further, each firm, i, calculates the total reward (net of costs) for ALL the firms supplying the market in the current period.  This is called marketTotalReward. As before, if the firm's bid in the period, Q(i), was greater than or equal to its currentBaseValue, then the marketTotalReward is recorded in the firm's record of marketUpRewards; otherwise the marketTotalReward is recorded in the firm's record of marketDownRewards.

Note well: recording in this way the marketTotalReward values assumes that the individual firms have access to the bids and the costs of all the firms in the market. 

The following procedure is used by each agent to record this information as described:

    to Observe-and-Record [price]
      if (is-pAndA? self) [
        let reward (price * quantityBid) - (unitProductionCost * quantityBid)
        set totalReward totalReward + reward
        ifelse (quantityBid >= currentBaseValue)
          [set upRewards lput reward upRewards]
          [set downRewards lput reward downRewards]
        ifelse (quantityBid >= currentBaseValue)
          [set marketUpRewards lput marketTotalReward marketUpRewards]
          [set marketDownRewards lput marketTotalReward marketDownRewards]
       ] ; end of if is-pAndA?
     end ; of observe-and-record

Each firm uses one of a number of policies in formutating its bids. In a given period, a firm's bid of a quantity, Q(i), is determined as follows. The firm has a currentBaseValue for its bid. The actual bid it produces is drawn uniformly in the range from (currentBaseValue - delta(i)) to (currentBaseValue + delta(i)).

Each firm adjusts its currentBaseValue after a certain number of periods, called an epoch. The number of periods in a firm's epoch is called its epochLength.  Each firm may have its own epochLength.  If at the end of a period a firm's current epoch reaches its epochLength, then the firm uses on of three available policies to update its currentBaseValue. Once it does this, it forgets its statistics, recorded as above, and it starts a new epoch.

The policies available are as follows.

1)  Simple Probe-and-Adjust ("Own Returns")

Each simple probe-and-adjust agent is characterized by four parameters---currentQbase (i.e., currantBaseValue), delta, epsilon, and epochLength---plus initialQbase, the initial value for currentQbase.  Typical values would be: currentQbase, 50; delta, 3; epsilon, 1; epochLength, 30. At the beginning of a run, the agent's currentQbase is intialized with its value of initialQbase. The agent's bid is uniformly drawn from [currentQbase - delta, currentQbase + delta].  When a period is over the agent receives revenue = P*Q(i) and profit (reward) of (P - unitProductionCost(i))*Q(i). The agent records this profit value in one of two lists. If the agent bid a quantity, Q(i) >= currentQbase, then the agent records reward, �(i), in upperReturn; otherwise the �(i) is recorded in lowerReturn.  Each agent keeps track of its own epochs, which consist of market periods of lengh epochLength. (These are all agent-specific values.) At the end of an epoch, an agent may adjust its value of currentQbase. Examining upperReturn and lowerReturn, the agent will increase currentQbase by epsilon if on average it gets a higher return in upperReturn than in lowerReturn; and conversely the agent will reduce currentQbase by epsilon if lowerReturn gives a higher on-average return. After making any required adjustment to currentQbase, the agent resets its epocCounter, upRewards, and downRewards to 0 and is ready for the next period.

When both agents use this policy, they converge in quantity offered and the sum of the quantities offered converges to the Cournot solution (but meanders stochastically).

        if (episodeCount >= epochLength) ; then update and reset
          [
           if (updateType = "Own Returns") [
           ifelse (upRewards = [])
             [set meanUp 0]
             [set meanUp mean upRewards]
           ifelse (downRewards = [])
             [set meanDown 0]
             [set meanDown mean downRewards]
           ifelse (meanUp > meanDown) 
             [set currentBaseValue currentBaseValue + epsilon]
             [set currentBaseValue max (list epsilon (currentBaseValue - epsilon))]
             ] ; end of if (updateType = "Own Returns")

2) "Market Returns"

Each agent records the total market returns and adjusts according to them.  See the Postpare-Episode procedure.  What happens is that the total quantity bid moves to the monopoly price, but the quantities bid by the two agents do not converge to each other.

Notice that if one player plays "Own Returns" and the other plays "Market Returns", the monopoly price is reached, but the "Market Returns" player gets exploited and eventually bids almost nothing.

        if (episodeCount >= epochLength) ; then update and reset
          [ ...


           if (updateType = "Market Returns") [
             ifelse (marketUpRewards = [])
               [set meanUp 0]
               [set meanUp mean marketUpRewards]
             ifelse (downRewards = [])
               [set meanDown 0]
               [set meanDown mean marketDownRewards]
             ifelse (meanUp > meanDown) 
               [set currentBaseValue currentBaseValue + epsilon]
               [set currentBaseValue max (list epsilon (currentBaseValue - epsilon))] 
             ] ; end of if (updateType = "Market Returns")

3) "Market Returns and Own Returns"

An agent pursuing this policy looks, at the end of its epochs, at its mean quantity bid versus the mean quantity bid for the entire market (including its own bids). If its mean quantity bid plus epsilon is lower than the mean quantity bid for the market, the agent raises its baseline bid by epsilon; otherwise, it uses the "Market Returns" policy.

If both agents follow this policy, they achieve the monopoly quanity and equal bid quantities on average.  If one agent instead follows the "Own Returns" policy, they converge to Cournot and to each other.

        if (episodeCount >= epochLength) ; then update and reset
          [ ...


           if (updateType = "Market Returns and Own Returns") [
             ifelse (mean myEpochBids + epsilon <= mean marketEpochBids) 
               [set currentBaseValue currentBaseValue + epsilon]  
               [ ; else, I'm ok so I play the good citizen
                ifelse (marketUpRewards = [])
                 [set meanUp 0]
                 [set meanUp mean marketUpRewards]
                ifelse (downRewards = [])
                 [set meanDown 0]
                 [set meanDown mean marketDownRewards]
                ifelse (meanUp > meanDown) 
                 [set currentBaseValue currentBaseValue + epsilon]
                 [set currentBaseValue max (list epsilon (currentBaseValue - epsilon))] 
                ] ; end of else in ifelse (mean myEpochBids...
             ] ; end of if (updateType = "Market Returns and Own Returns")

## HOW TO USE IT

Try especially different combinations of the three policies, with different numbers of agents. If only one agent (or firm) is present, we have a monopoly situation and the firm is firm 0.  If two agents are present we have a duopoloy between firms 0 and 1. If more than two agents are present, we may have firms 3, 5 or 10 firms. See the SetupNPandAFirms procedures for N=3, 5, 10.

## THINGS TO NOTICE

In the "Quantities Bid" plot, the system plots the monopoly, Cournot, and competive price in the market, given the agents and their costs. Where do the suppliers end up? What price prevails? Look at the other plots to see how well each agent does in the market.

## THINGS TO TRY

Change the other parameters of the model, accessible on the Interface tab. Look especially at costs and epoch length.  Also, turn on random walk for the price function and see how well the agents track the changing price curve.

## EXTENDING THE MODEL

There are indefinitely many ways to extend this model.  
See in particular the SetupNPandAFirms procedures for N=3, 5, 10, which is fairly limited. You might generalize or at least alter these initialization procedures in order to explore other issues.



## RELATED MODELS

oligopolyBidPrice.nlogo, by sok, served as a model for this model. In oligopolyBidPrice.nlogo, "The simulated market proceeds in episodes in which each participating firm offers to supply the entire demand of the market at a bid price. The market selects the price of the lowest bidder, and a new episode begins." This models what is called Bertrand competition.

But here, each firm bids a quantity. The market sums the quantities bid for the total quantity on offer. This sets the price that each firm gets and is often called Cournot competition.

## CREDITS AND REFERENCES

This model was created and written by Steven O. Kimbrough in collaboration with Frederic H. Murphy. Please give us credit if you use this model or program. 

 The model is   
freely downloadable at:  
http://opim.wharton.upenn.edu/~sok/AGEbook/nlogo/OligopolyPutQuantity.nlogo.

To refer to this model in academic publications, please use: Kimbrough, Steven O. and Murphy, Frederic H. (2007, 2011). Oligopoly Put Quantity model. http://opim.wharton.upenn.edu/~sok/AGEbook/nlogo/OligopolyPutQuantity.nlogo.   
In other publications, please use: Copyright  Steven O. Kimbrough and Frederic H. Murphy. All rights reserved.

$Id: OligopolyPutQuantity.nlogo 4089 2014-03-20 21:01:52Z sok $
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
