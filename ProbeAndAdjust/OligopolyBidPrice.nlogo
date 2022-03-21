globals [pstar current-price-firm-1 priceIntercept m-price m-price-at0cost m-quantity m-quantity-at0cost
         m-price-firm0 m-price-firm1
         epochOver
         up-data down-data total-revenue-firm-1 probe-count history quantityIntercept
         up-prices down-prices monop-prices m-up-prices m-dn-prices m-up-revs m-dn-revs
         up-as down-as current-price-firm-2 total-revenue-firm-2
         up-revs-firm-2 down-revs-firm-2 win-count-firm-1 win-count-firm-2
         firm-1-bid firm-2-bid
         episodeAbsoluteCount avgWinningBid runningAverageBidList runningAverageBid
         totalUnitProductionCost
         version
         episodeBids ; a list to hold all price bids in this episode
         ]

breed [pAndAers pAndA]     ; Probe-and-Adjust

turtles-own [priceBid totalReward bidsWon unitProductionCost]
pAndAers-own [epsilon delta cost epochLength episodeCount
              currentPrice upRewards downRewards updateType
              ownUpRewards ownDownRewards runningAverageReward
              myEpisodeReward ; what the pAndAer gets in the current episode
              myPricesBid industryPricesBid ; these are lists of prices bid during an epoch
              patience
              ]

;;;;;;;;;;;;;;;;;;;;;;;;;
;; ShowVersion  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

to ShowVersion
  set version "$Id: OligopolyBidPrice.nlogo 5330 2016-04-14 18:56:52Z sok $"
end

;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

to Setup
;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  ;__clear-all-and-reset-ticks
  clear-all
set version "$Id: OligopolyBidPrice.nlogo 5330 2016-04-14 18:56:52Z sok $"
if (daRandomSeed != "system clock")
  [random-seed daRandomSeed]
set episodeAbsoluteCount 0
set runningAverageBidList []
set  quantityIntercept qIBase
;set current-price-firm-1 initialPriceFirm0
;set current-price-firm-2 initialPriceFirm1
; set pstar intercept / (2 * slope)
; show random-float 2 * delta
set priceIntercept quantityIntercept / dSlope ; the price when the quantity is 0
; This is the monopolist's price when the monopolist has 0 costs.
; Otherwise, it is m-price = (quantityIntercept + k*dSlope)/(2*dSlope)
set m-price-at0cost quantityIntercept / (2 * dSlope)
; This is the monopolist's quantity given m-price as the monopolist's price
set m-quantity-at0cost (quantityIntercept - (dSlope * m-price-at0cost))
clear-all-plots
; This plots the demand curve, and the monopoly price and
; quantity for that demand curve, assuming 0 costs for the
; monopolist.
plot-quantity-price ; note that this is hidden, so the user doesn't actually see it, by default

create-pAndAers numPandAFirms
ask turtles [set totalReward 0
             set bidsWon 0]
ask pAndAers [setxy -4 who
     set upRewards []
     set downRewards []
     set ownUpRewards []
     set ownDownRewards [] ]
; ok, from the Information tab:
;|numPandAFirms
;If set to 1, we have the monopoly case and the firm uses the Firm0 parameter values.
;If set to 2, we have the duopoly case. Firm 0 uses the Firm0 parameter values and
;Firm 1 uses the Firm1 values. If set to 3 or more, Firm 0 uses the Firm0
;parameter values and all other firms use the Firm1 parameter values.
;(Later I can have a randomized option for parameter values.)

; First, then, I'll set ALL firms with the Firm1 parameter values.
; Then I'll set Firm 0 with the Firm0 values.
ask pAndAers
    [set epsilon epsilonFirm1
     set delta deltaFirm1
     set currentPrice initialPriceFirm1
     set cost kFirm1
     set epochLength epochLengthFirm1
     set episodeCount 0
     set unitProductionCost kFirm1
     set updateType updateTypeFirm1
     set myEpisodeReward 0
     set myPricesBid []
     set industryPricesBid []
     set patience patienceFirm1
    ]
  ; Now do firm 0
  ;set [epsilon] of pAndA 0 epsilonFirm0
  ask pAndA 0 [set epsilon epsilonFirm0]
  ;set [delta] of pAndA 0 deltaFirm0
  ask pAndA 0 [set delta deltaFirm0]
  ;set [currentPrice] of pAndA 0 initialPriceFirm0
  ask pAndA 0 [set currentPrice initialPriceFirm0]
  ;set [cost] of pAndA 0 kFirm0
  ask pAndA 0 [set cost kFirm0]
  ;set [epochLength] of pAndA 0 epochLengthFirm0
  ask pAndA 0 [set epochLength epochLengthFirm0]
  ;set [episodeCount] of pAndA 0 0
  ask pAndA 0 [set episodeCount 0]
  ;set [unitProductionCost] of pAndA 0 kFirm0
  ask pAndA 0 [set unitProductionCost kFirm0]
  ;set [updateType] of pAndA 0 updateTypeFirm0
  ask pAndA 0 [set updateType updateTypeFirm0]
  ;set [patience] of pAndA 0 patienceFirm0
  ask pAndA 0 [set patience  patienceFirm0]

  if (variableStarts = True)
   [ask pAndAers
     [set currentPrice (random-float 2 * (variableFactor * currentPrice)) + (currentPrice - (variableFactor * currentPrice))
      print (word "Starting currentPrice for agent "  self  " is "  currentPrice  ".")
   ]
   ] ; end of if (variableStarts = True)

  set m-price-firm0 (quantityIntercept + [unitProductionCost] of pAndA 0 * dSlope) / (2 * dSlope)
  if (numPandAFirms > 1)
    [set m-price-firm1 (quantityIntercept + [unitProductionCost] of pAndA 1 * dSlope) / (2 * dSlope)]
  ;;;;;;;;;;
  if (reporting)
    [write "episode"
     write "ID"
     write "breed"
     write "currentPrice"
     write "bidsWon"
     write "totalReward"
     print " "]
  if (reporting)
     [turtle-report]
  ; Now get the total unit production cost
  set totalUnitProductionCost 0
  ask turtles
    [set totalUnitProductionCost (totalUnitProductionCost + unitProductionCost)]
  ; m-price = (quantityIntercept + k*dSlope)/(2*dSlope)
  set m-price (quantityIntercept + totalUnitProductionCost * dSlope) / (2 * dSlope)
  reset-ticks
end ; of setup

;;;;;;;;;;;;;;;;;;;;
;; go ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;

to Go
  tick
  ;if ticks >= numEpisodesToRun [stop]
; See the documentation in the Information tab.
set episodeAbsoluteCount episodeAbsoluteCount + 1
set episodeBids [] ; a list to hold all price bids in this episode
; 0. Perturb the demand curve if we're doing random-walk-as
if (random-walk-as)
   [set quantityIntercept (random-float 2 * aDelta) - aDelta + quantityIntercept
   set priceIntercept quantityIntercept / dSlope
   ; This is the monopolist's price when the monopolist has 0 costs.
   ; Otherwise, it is m-price = (quantityIntercept + k*dSlope)/(2*dSlope)
   set m-price-at0cost quantityIntercept / (2 * dSlope)
   ; This is the monopolist's quantity given m-price as the monopolist's price
   set m-quantity-at0cost (quantityIntercept - (dSlope * m-price))
   set m-price (quantityIntercept + totalUnitProductionCost * dSlope) / (2 * dSlope)
   set m-price-firm0 (quantityIntercept + [unitProductionCost] of pAndA 0 * dSlope) / (2 * dSlope)
   if (numPandAFirms > 1)
     [set m-price-firm1 (quantityIntercept + [unitProductionCost] of pAndA 1 * dSlope) / (2 * dSlope)]
   ] ; if of if (random-as)

; 1. All of the turtles are asked to determine a bid price. Each records his own, in bidPrice.
ask turtles [
  Set-PriceBid
]
;print "in Go... episodeBids = " + episodeBids
;ask turtles [set industryPricesBid episode
; 2. We figure out the lowest bid and take it.
; daWinner is the winning turtle, with the or a lowest bid.
let daWinner min-one-of turtles [priceBid]
let daWinnersID [who] of daWinner
;print daWinnersID

let daWinningBid [priceBid] of daWinner
;write " " + daWinner + " " + daWinningBid
;print " "
let daQuantityDemanded Quantity-Demanded(daWinningBid)
;print daQuantityDemanded
let daCost [cost] of daWinner
;print who-of daWinner
; 3. The turtle with the lowest bid gets a reward, gets all the business.
let tempReward [totalReward] of daWinner
;set [totalReward] of daWinner tempReward + (daWinningBid * daQuantityDemanded - daCost * daQuantityDemanded)
ask daWinner [set totalReward (tempReward + (daWinningBid * daQuantityDemanded - daCost * daQuantityDemanded))]
;set [bidsWon] of daWinner [bidsWon] of daWinner + 1
ask daWinner [set bidsWon (bidsWon + 1)]

; 4. All of the turtles observe the winning bid (daWinningBid)
; and the resulting quantity demanded (daQuantityDemanded).
; All of the turtles record the profit they would have had (or did have
; in the case of the winner) as either upProfits or downProfits, profits they
; got or could have gotten by bidding high or low.  More carefully put,
; each firm observes the winning bid and resulting demand, then calculates
; the reward/profit it would have had had it won with that bid. This reward
; is recorded in the list of upRewards or downRewards depending on whether
; daWinningBid is > or <= currentPrice. Now all this applies only to turtles
; of breed pAndA.
ask turtles [observe-and-record(daWinningBid)(daQuantityDemanded)(daWinner)]
; 5. Plot monopoly price and winning bid price.
plot-prices(m-price)(daWinningBid)
; This completes the episode. We ask the turtles to do any
; appropriate housekeeping.
ask turtles [postpare-episode]

ifelse (episodeAbsoluteCount > 1)
  [set avgWinningBid (avgWinningBid * (episodeAbsoluteCount - 1) + daWinningBid) / episodeAbsoluteCount ]
  [set avgWinningBid daWinningBid]

set runningAverageBidList lput daWinningBid runningAverageBidList
if (length runningAverageBidList > runningAvgLength)
  [set runningAverageBidList but-first runningAverageBidList]
set runningAverageBid mean runningAverageBidList

if (episodeAbsoluteCount mod reportEveryNEpisodes = 0 and reporting)
  [turtle-report]
end ; of go

to Set-PriceBid
 if (is-pAndA? self) [
  set priceBid max (list cost ((random-float 2 * delta) - delta + currentPrice))
  set episodeBids lput priceBid episodeBids
  set myPricesBid lput priceBid myPricesBid
  set episodeCount episodeCount + 1
  ;write who-of self + " " + priceBid
 ]
end ; of set-priceBid

to Observe-And-Record [price quantity daWinner]
  if (is-pAndA? self) [
    let reward price * quantity - cost * quantity
    ifelse (price > currentPrice)
      [set upRewards lput reward upRewards]
      [set downRewards lput reward downRewards]
    ifelse (priceBid > currentPrice)
      [ifelse (daWinner = self)
       [set ownUpRewards lput reward ownUpRewards]
       [set ownUpRewards lput 0 ownUpRewards]
       ]
      [ifelse (daWinner = self)
       [set ownDownRewards lput reward ownDownRewards]
       [set ownDownRewards lput 0 ownDownRewards]
       ]
    ;print "In observe-and-record, episodeBids=" + episodeBids
    foreach episodeBids [ [?1] -> set industryPricesBid lput ?1 industryPricesBid ]
   ] ; end of if is-pAndA?
end ; of observe-and-record

to plot-prices [monopolyPrice winningBid]
set-current-plot "Current Bid Price"
set-current-plot-pen "Winning Bid"
plot winningBid
; I no longer think this makes any sense, so I'm deleting it.
;set-current-plot-pen "Monopoly Price"
;plot monopolyPrice
set-current-plot-pen "Monopoly Price at 0 Costs"
plot m-price-at0cost
set-current-plot-pen "Monopoly Price, Firm 0"
plot m-price-firm0
set-current-plot-pen "Monopoly Price, Firm(s) 1"
plot m-price-firm1
set-current-plot "Current Base Prices"
let numToPlot min (list 10 numPandAFirms)
foreach n-values numToPlot [ [?1] -> ?1 ]
[ [?1] -> set-current-plot-pen (word "Agent"  ?1)
plot [currentPrice] of pAndA ?1
 ]
;if (numPandAFirms > 1)
 ; [set-current-plot-pen "Agent1"
 ;  plot currentPrice-of pAndA 1]
end ; of plot-prices

to Postpare-Episode
  if (is-pAndA? self) [
    ;if (who-of self = 0)
    ;[print "about to postpare turtle 0. updateType is " + updateType-of self]
    ;[print "My episode count is " + episodeCount]
    let meanUp 0
    let meanDown 0
    if (episodeCount >= epochLength) ; then update and reset
      [
      ; first, collect statistics
      set runningAverageReward (sum ownUpRewards + sum ownDownRewards) / epochLength
      if (updateType = "Market Returns") [
       ifelse (upRewards = [])
         [set meanUp 0]
         [set meanUp mean upRewards]
       ifelse (downRewards = [])
         [set meanDown 0]
         [set meanDown mean downRewards]
       ifelse (meanUp > meanDown)
         [set currentPrice currentPrice + epsilon]
         [set currentPrice max (list (cost + delta) (currentPrice - epsilon))]
       ] ; end of if (updateType = "Market Returns")
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       if (updateType = "Own Returns") [
       ifelse (ownUpRewards = [])
         [set meanUp 0]
         [set meanUp mean ownUpRewards]
       ifelse (ownDownRewards = [])
         [set meanDown 0]
         [set meanDown mean ownDownRewards]
       ifelse (meanUp > meanDown)
         [set currentPrice currentPrice + epsilon]
         ; I think the delta below is a bug and should be cost 2007-5-1
         ;[set currentPrice max (list delta (currentPrice - epsilon))]
         [set currentPrice max (list (cost + delta) (currentPrice - epsilon))]
       ;write self + " " + ownUpRewards + " " + ownDownRewards + " " + meanUp + " " + meanDown
       ;print " "
       ] ; end of if (updateType = "Own Returns")
       ;;;;;;;;;;;;;;;;;;;;;;;;  Dropping this: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       if (updateType = "Market Returns and Own Returns") [
       ; See if you've won ANY bids. If not, reduce your price, but don't go lower than your costs,
       ; else follow the Market Returns rule
       ;if (who-of self = 0)
       ;[
       ;print "about to postpare turtle 0. updateType is " + updateType-of self
       ;print ownUpRewards + " " + ownDownRewards
       ;]
       ifelse ((ownUpRewards = [] and ownDownRewards = []) or (sum ownUpRewards = 0 and sum ownDownRewards = 0))
         [set currentPrice max (list (cost + delta) (currentPrice - epsilon))
          ;print "My new currentPrice is " + currentPrice + "My episodeCount is " + episodeCount
          ]
         [ ; now the else
       ifelse (upRewards = [])
         [set meanUp 0]
         [set meanUp mean upRewards]
       ifelse (downRewards = [])
         [set meanDown 0]
         [set meanDown mean downRewards]
       ifelse (meanUp > meanDown)
         [set currentPrice currentPrice + epsilon]
         [set currentPrice max (list (cost + delta) (currentPrice - epsilon))]
         ] ; end of the else
       ] ; end of if (updateType = "Market Returns and Own Returns")
       ;;;;;;;;;;;;;;;;;; <== dropped the above ;;;;;;;;;;;;;;;;;;;;;;
       if (updateType = "Market Returns s.t. Own Returns") [
         ; if my price bids have been higher than the industry average by delta,
         ; set my currentPrice to the mean of the industry's prices bid in the
         ; epoch; epsilon is too impatient
         ifelse ((mean myPricesBid) - (delta * patience) > (mean industryPricesBid))
           [;print "Hello!!!!!"
            set currentPrice (mean industryPricesBid) - delta] ; aggressively reduce price
           ; else behave as a Market Returns guy
           [ifelse (upRewards = [])
             [set meanUp 0]
             [set meanUp mean upRewards]
           ifelse (downRewards = [])
             [set meanDown 0]
             [set meanDown mean downRewards]
            ifelse (meanUp > meanDown)
             [set currentPrice currentPrice + epsilon]
             [set currentPrice max (list (cost + delta) (currentPrice - epsilon))]
       ] ;

       ] ; end of if (updateType = "Market Returns s.t. Own Returns")
       set upRewards []
       set downRewards []
       set ownUpRewards []
       set ownDownRewards []
       set myPricesBid []
       set industryPricesBid []
       set episodeCount 0
      ] ; of if episodeCount = epochLength
   ] ; end of if is-pAndA?
end ; of postpare-episode

to turtle-report
ask turtles [
write episodeAbsoluteCount
write who
write [breed] of self
write currentPrice
write bidsWon
write totalReward
print " "
]
end ; of turtle-report

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plot-quantity-price  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to plot-quantity-price
set-current-plot "Demand and Monopoly Quantity & Price at 0 Costs"
clear-plot
set-plot-y-range 0 priceIntercept
set-plot-x-range 0 quantityIntercept
set-current-plot-pen "demand-curve"
plot-pen-up
plotxy 0 priceIntercept
plot-pen-down
plotxy quantityIntercept 0
set-current-plot-pen "monopoly-quantity"
plot-pen-up
plotxy m-quantity-at0cost 0
plot-pen-down
plotxy m-quantity-at0cost m-price-at0cost
set-current-plot-pen "monopoly-price"
plot-pen-up
plotxy 0 m-price-at0cost
plot-pen-down
plotxy m-quantity-at0cost m-price-at0cost
end

to-report daRevenue [daPrice]
  report daPrice * (quantityIntercept - (dSlope * daPrice))
end

to plot-price [daPrice daPrice-firm-2 mono-price]
set-current-plot "Current Bid Price"
set-current-plot-pen "Bid Price Firm 1"
plot daPrice ; daRevenue(daPrice)
set-current-plot-pen "Bid Price Firm 2"
plot daPrice-firm-2
set-current-plot-pen "Monopoly Price"
plot mono-price ; daRevenue(mono-price)
end

to debug
let probe 0


set probe (random-float 2 * deltaFirm0) - deltaFirm0 + current-price-firm-1
show (word "a = "  quantityIntercept)
show (word "price intercept = "  priceIntercept)
show (word "slope = " dSlope)
show (word "monopoly price = "  m-price)
show (word "monopoly demand = "  (quantityIntercept - dSlope * m-price))
show (word "monopoly revenue = " daRevenue(m-price))
show (word "current-price = " current-price-firm-1)
show (word "probe = " probe)
show (word "probe revenue = " daRevenue(probe))
end




;; New for oligopoly, price setting
to-report Quantity-Demanded [daPrice]
  ; if the price is too high, demand is zero
  let bob  quantityIntercept - (dSlope * daPrice)
  let carol (list 0 bob)
  ;print max carol
  report max carol
end ; of Quantity-Demanded
@#$#@#$#@
GRAPHICS-WINDOW
363
128
558
324
-1
-1
17.0
1
10
1
1
1
0
1
1
1
-5
5
-5
5
0
0
1
ticks
30.0

SLIDER
288
404
471
437
qIBase
qIBase
0
1000
222.0
1
1
NIL
HORIZONTAL

SLIDER
117
404
289
437
dSlope
dSlope
0
5
0.5
0.01
1
NIL
HORIZONTAL

BUTTON
2
10
68
43
Setup
setup
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
2
239
174
272
initialPriceFirm0
initialPriceFirm0
0
300
193.0
1
1
NIL
HORIZONTAL

SLIDER
2
109
174
142
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
2
43
174
76
epsilonFirm0
epsilonFirm0
0
1
0.7
0.1
1
NIL
HORIZONTAL

PLOT
516
469
964
646
Demand and Monopoly Quantity & Price at 0 Costs
Q, quantity
P, price
0.0
200.0
0.0
400.0
true
true
"" ""
PENS
"demand-curve" 1.0 0 -16777216 true "" ""
"monopoly-quantity" 1.0 0 -2674135 true "" ""
"monopoly-price" 1.0 0 -10899396 true "" ""

BUTTON
68
10
131
43
Go
go
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
175
10
991
347
Current Bid Price
NIL
NIL
0.0
10.0
197.0
203.0
true
true
"" ""
PENS
"Winning Bid" 1.0 0 -2674135 true "" ""
"Monopoly Price at 0 Costs" 1.0 0 -16777216 true "" ""
"Monopoly Price, Firm 0" 1.0 0 -11221820 true "" ""
"Monopoly Price, Firm(s) 1" 1.0 0 -5825686 true "" ""

SLIDER
514
437
686
470
aDelta
aDelta
0
1
0.3
0.01
1
NIL
HORIZONTAL

SWITCH
356
437
515
470
random-walk-as
random-walk-as
1
1
-1000

SLIDER
2
76
174
109
epsilonFirm1
epsilonFirm1
0
1.0
0.7
0.1
1
NIL
HORIZONTAL

SLIDER
2
142
174
175
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
2
272
174
305
initialPriceFirm1
initialPriceFirm1
0
300
193.0
1
1
NIL
HORIZONTAL

CHOOSER
481
393
599
438
daRandomSeed
daRandomSeed
0 1 17 100 "system clock"
4

SLIDER
2
305
174
338
epochLengthFirm0
epochLengthFirm0
0
200
50.0
1
1
NIL
HORIZONTAL

SLIDER
2
338
174
371
epochLengthFirm1
epochLengthFirm1
0
200
30.0
1
1
NIL
HORIZONTAL

CHOOSER
502
347
640
392
numPandAFirms
numPandAFirms
1 2 3 4 5 6 8 10 12 14 16 18 20
1

SLIDER
2
174
174
207
kFirm0
kFirm0
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
2
206
174
239
kFirm1
kFirm1
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
1
370
174
403
reportEveryNEpisodes
reportEveryNEpisodes
0
100
1.0
1
1
NIL
HORIZONTAL

BUTTON
640
347
724
380
Run N Times
let bob numEpisodesTorun\nwhile [bob > 0]\n[;go\nset bob bob - 1\n]
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
599
396
792
429
numEpisodesToRun
numEpisodesToRun
0
20000
5000.0
100
1
NIL
HORIZONTAL

MONITOR
726
347
829
392
NIL
avgWinningBid
3
1
11

SWITCH
2
404
118
437
reporting
reporting
1
1
-1000

SLIDER
790
396
962
429
runningAvgLength
runningAvgLength
0
100
50.0
1
1
NIL
HORIZONTAL

MONITOR
829
347
961
392
NIL
runningAverageBid
3
1
11

MONITOR
3
437
173
482
Monopoly Price for firm 0
(quantityIntercept + [unitProductionCost] of pAndA 0 * dSlope) / (2 * dSlope)
3
1
11

MONITOR
172
437
358
482
Monopoly Price for firm(s) 1
(quantityIntercept + [unitProductionCost] of pAndA 1 * dSlope) / (2 * dSlope)
3
1
11

CHOOSER
831
256
1082
301
updateTypeFirm0
updateTypeFirm0
"Own Returns" "Market Returns" "Market Returns s.t. Own Returns"
0

CHOOSER
831
301
1082
346
updateTypeFirm1
updateTypeFirm1
"Own Returns" "Market Returns" "Market Returns s.t. Own Returns"
0

PLOT
3
486
490
684
Current Base Prices
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
"Agent0" 1.0 0 -11221820 true "" ""
"Agent1" 1.0 0 -5825686 true "" ""
"Agent2" 1.0 0 -7500403 true "" ""
"Agent3" 1.0 0 -13345367 true "" ""
"Agent4" 1.0 0 -10899396 true "" ""
"Agent5" 1.0 0 -2674135 true "" ""
"Agent6" 1.0 0 -955883 true "" ""
"Agent7" 1.0 0 -6459832 true "" ""
"Agent8" 1.0 0 -1184463 true "" ""
"Agent9" 1.0 0 -8630108 true "" ""

SWITCH
685
437
807
470
variableStarts
variableStarts
1
1
-1000

SLIDER
807
437
963
470
variableFactor
variableFactor
0
1.0
0.1
0.01
1
NIL
HORIZONTAL

MONITOR
175
346
330
391
Agent0 Epoch Ave. Reward
[runningAverageReward] of turtle 0
0
1
11

MONITOR
329
346
481
391
Agent1 Epoch Ave. Reward
[runningAverageReward] of turtle 1
0
1
11

MONITOR
491
647
978
692
NIL
version
3
1
11

MONITOR
964
346
1121
391
NIL
[currentPrice] of turtle 0
3
1
11

MONITOR
964
394
1121
439
NIL
[currentPrice] of turtle 1
3
1
11

MONITOR
964
442
1121
487
NIL
[currentPrice] of turtle 2
3
1
11

MONITOR
964
491
1121
536
NIL
[currentPrice] of turtle 3
3
1
11

SLIDER
830
107
991
140
patienceFirm0
patienceFirm0
0
5
1.1
0.1
1
NIL
HORIZONTAL

SLIDER
830
140
991
173
patienceFirm1
patienceFirm1
0
5.0
1.1
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
## NOTE WELL

The documentation below is incomplete.  It is in draft form only.  What is here, however, is correct (or so I think).

## HOW TO RUN THE MODEL

1. Set values for the various sliders and switches (in green) on the Interface tab.

The default values should be OK, so you can skip this step at first.

2. Click the Setup button.

3. Click the Go button.

The Go button is an "always" button, so the run will continue until you stop it, which you can do by clicking the Go button again.

## WHAT DO YOU SEE WHEN YOU RUN THE MODEL?

Most prominently you see an active plot entitled "Current Bid Price".  The simulated market proceeds in episodes in which each participating firm offers to supply the entire demand of the market at a bid price.  The market selects the price of the lowest bidder, and a new episode begins.

What you see plotted are two things.  In black is the monopolist's price, given the (linear) demand curve specified via a number of sliders (discussed below), PROVIDING THE MONOPOLIST HAS 0 COSTS.  This is just: m-price = quantityIntercept / (2 * dSlope).
quantityIntercept is given initially by the qIBase slider on the Interface tab. dSlope is given by the dSlope slider.

In red on the plot is the lowest price bid in the episode.  Also plotted are the monopoly price of Firm0 (agent 0) given its costs (this is in cyan) and the monopoly price of Firm1 (agent 1) given its costs (this is in magenta).

In addition to the "Current Bid Price" plot there are several monitors that report information during a run ...

1. Monopoly Price for firm 0

This is the numerical value of the cyan line in the above plot.

2. Monopoly Price for firm(s) 1

This is the numerical value of the magenta line in the above plot.

3.  avgWinningBid

This is the running average over all the episodes in the run of the (price) value of the winning bid.

4. runningAverageBid

This is the average of the (price) value of the winning bid for the last N episodes. N is set by the slider runningAvgLength.  (At the inception of the run, before N episodes have been completed, runningAverageBid is just the average winnng price of the episodes that have been completed.)

## WHAT ARE THE UPDATETYPE CHOOSERS ABOUT?

On the Interface tab you will find two choosers for selecting update policies to be used by the agents.  "updateTypeFirm0" sets the policy for firm (agent) 0. "updateTypeFirm1" sets the policy for all higher-numbered firms (agents).

See the procedure Postpare-Episode for the implementation of these policies.  The procedure Observe-and-Record is also important.

In brief, "Own Returns" is a myopic and selfish policy. At the end of the agent's epoch the agent raises or lowers its current price (about which it bids randomly using Probe and Adjust) in the direction of its own returns exclusively. In particular, if the agent fails to win the bid during an episode, it records a 0 reward for that episode. Then, if its bids during the epoch about its current price returned on average more than its bids below the current price, then the agent raises its price; and similarly for lowering the price.

Using the "Market Returns" policy, the agent takes the industry view and records what it would have received had it won the bid. Then, if its bids during the epoch about its current price would have returned on average (had it won each of them) more than its bids below the current price, then the agent raises its price; and similarly for lowering the price.

Using the "Market Returns and Own Returns" policy, the agent first determines whether it has won any bids during the epoch. If not, it lowers its current price. If it has won some bids, then the agent applies the "Market Returns" policy.

## WHAT DO THE MONITORS REPORT?

The monitor labelled "Agent0 Epoch Ave. Reward" displays the average reward per episode received by firm 0 (agent 0) during the agent's previous epoch.  During an epoch, each agent records the reward it achieved for each episode. If the agent did not win the bid, the reward is 0, otherwise it is the usual: price * quantity - cost * quantity.  See the procedure Observe-and-Record.  At the end of an epoch, the agent averages its rewards over the episodes in the epoch and this is what is reported in the monitor for agent 0. See the procedure Postpare-Episode.

The monitor labelled "Agent1 Epoch Ave. Reward" works just like "Agent0 Epoch Ave. Reward", but for firm (agent) 1.

## WHAT DO THE SLIDERS, SWITCHES, AND CHOOSERS DO?

They set parameters, global variables, for the run of the model.

    numPandAFirms

This model, oligopolyBidPrice.nlogo, is about markets in which there are a few firms competing to supply the entire demand in a winner-take-all-by-episode market. This chooser lets the user set the number of firms in the market who will be using Probe and Adjust (explained below) to arrive at their bids.

Note that the firm-specific parameters, set by sliders to the left of the plot, all end in ...Firm0 or ...Firm1.  If there are two firms in the run, i.e., if numPandAFirms is set to 2, they are called firm 0 and firm 1, and are assigned the parameter values as indicated.  If there is one firm in the model, it gets the parameter values ...Firm0. Finally, if there are 3 or more firms in the model, they ALL get the parameter values ...Firm0.

/* more to do here*/

## WHAT IS IT?

This model explores how oligopolists might discover their prices in the face of, here, a linear demand function. The discovery process does not allow explicit coordination or collusion between the duopolists.  Instead, a main purpose of this model is to investigate the potential for tacit collusion by the duopolists, using information gained from repeated interactions in the market.

In this model, the firms adjust prices. During each market episode, each firm announces its unit price.  The market satisfies its entire demand at the lower of the two prices (or randomly in the case of a tie).  This is, then, a winner-take-all market during each episode.

Prices are being adjusted and price is a continuous variable. The firms in this program use what I call a "Probe and Adjust" model, and a very simple one at that.

    1.  set

 Each agent begins with a current value, here currentPriceFirmX (X = 1, 2), set by initialPriceFirmX in the Interface tab. Play is organized by epochs, each containing a number of episodes. In each episode, the agent probes its market by offering a price in the range [currentPriceFirmX - deltaFirmX, currentPriceFirmX + deltaFirmX]. The agent records the revenue it gets from the probe, classifying the revenue as coming from a price offered that is either above or below its current price. When the epoch is over (a number of episodes, set by epochLengthFirmX in the Interface tab), the agent determines whether on average it did better by offering more than its current price or less. Depending on the results, it increases its currentPriceFirmX by epsilonFirmX or decreases it by epsilonFirmX. Then a new epoch begins. Both deltaFirmX and epsilonFirmX are set in the Interface. The key point about this model is that neither agent has access to the demand function, except as it responds to prices placed to the market.

## MONOPOLY PRICE-SETTING

Introductory textbooks in microeconomics will tell a story roughly as follows. The market's demand, Q (think: quantity demanded), for our product is a linear function of its price, P:

(1)  Q(P) = Q = c - dSlope*P

Here, c is a constant, representing the quantity demanded when price is 0 and, since we are linear, the price point at which demand disappears when the price is too high (when dSlope*P = c). (We assume that c, dSlope > 0, so -dSlope < 0.)

Let us assume that the cost of production (for the monopolist) is k*Q

The profit, pi, made by the monopolist is P*Q (since the cost of production is 0, we need only account for revenue, which is defined here as P*Q).

(3) pi = P*Q = (a - dSlope*Q)*Q = a*Q - dSlope*Q**2

The monopolist will seek to maximize pi, which can be done by a simple exercise with the calculus.

(4) dpi/dQ = a - 2*dSlope*Q

Setting a - 2*dSlope*Q to zero and solving for Q yields

(5) Q = a/(2*dSlope)

So Q in (5) is Q*, the optimal quantity for the monopolist to put on the market.

(Checking that d**2 pi/dQ**2 = -2*dSlope < 0 verifies that we indeed have found a maximum.)

See, for example, _Intermediate_Microeconomics:_A_Modern_Approach_, 6th ed., by Hal. R. Varian. I draw explicitly on _Microeconomic_Theory_, 2nd ed., 1978, The Dryden Press, Hinsdale, Illinois, by Walter Nicholson. See "Appendix to Chapter 13: Models of Interdependent Output Decisions," pp. 389-398. Nicholson's is the more advanced text. My treatment generalizes his discussion.  Kreps's _A_Course_in_Microeconomic_Theory_, 1990, is excellent and more advanced.


## HOW IT WORKS

There are two key passages from the code. First this:

    ; 4. All of the turtles observe the winning bid (daWinningBid)
    ; and the resulting quantity demanded (daQuantityDemanded).
    ; All of the turtles record the profit they would have had (or did have
    ; in the case of the winner) as either upProfits or downProfits, profits they
    ; got or could have gotten by bidding high or low.  More carefully put,
    ; each firm observes the winning bid and resulting demand, then calculates
    ; the reward/profit it would have had had it won with that bid. This reward
    ; is recorded in the list of upRewards or downRewards depending on whether
    ; daWinningBid is > or <= currentPrice. Now all this applies only to turtles
    ; of breed pAndA.
    ask turtles [observe-and-record(daWinningBid)(daQuantityDemanded)(daWinner)]

and this:

    to Observe-And-Record [price quantity daWinner]
      if (is-pAndA? self) [
        let reward price * quantity - cost * quantity
        ifelse (price > currentPrice)
          [set upRewards lput reward upRewards]
          [set downRewards lput reward downRewards]
        ifelse (priceBid > currentPrice)
          [ifelse (daWinner = self)
           [set ownUpRewards lput reward ownUpRewards]
           [set ownUpRewards lput 0 ownUpRewards]
           ]
          [ifelse (daWinner = self)
           [set ownDownRewards lput reward ownDownRewards]
           [set ownDownRewards lput 0 ownDownRewards]
           ]
       ] ; end of if is-pAndA?
    end ; of observe-and-record

Second, is this:

    to Postpare-Episode
      if (is-pAndA? self) [
        let meanUp 0
        let meanDown 0
        if (episodeCount >= epochLength) ; then update and reset
          [if (updateType = "Market Returns") [
           ifelse (upRewards = [])
             [set meanUp 0]
             [set meanUp mean upRewards]
           ifelse (downRewards = [])
             [set meanDown 0]
             [set meanDown mean downRewards]
           ifelse (meanUp > meanDown)
             [set currentPrice currentPrice + epsilon]
             [set currentPrice currentPrice - epsilon]
           ] ; end of if (updateType = "Market Returns")
           if (updateType = "Own Returns") [
           ifelse (ownUpRewards = [])
             [set meanUp 0]
             [set meanUp mean ownUpRewards]
           ifelse (ownDownRewards = [])
             [set meanDown 0]
             [set meanDown mean ownDownRewards]
           ifelse (meanUp > meanDown)
             [set currentPrice currentPrice + epsilon]
             [set currentPrice max (list delta (currentPrice - epsilon))]
           ] ; end of if (updateType = "Own Returns")
           set upRewards []
           set downRewards []
           set ownUpRewards []
           set ownDownRewards []
           set episodeCount 0
          ] ; of if episodeCount = epochLength
       ] ; end of if is-pAndA?
    end ; of postpare-episode

Something to add would be an analog of the cautious policy in oligopolyBidQuantity.nlogo. Here, the firm needs to protect itself from others continually giving low bids.  So this needs to be added.

The runs, and the players' learning regimes, are organized into epochs, which consist of a number of episodes or rounds of play.  Each agent keeps its own epochLength. When an epoch begins, a probe-and-adjust player has a currentPrice which stays constant throughout the epoch.

After each episode (round) of play, the Postpare-Episode procedure is called for each agent.
For a given agent, its epoch continues until its episodeCount reaches the agent's epochLength. If the agent's epoch is in fact over, the code in Postpare-Episode comes into play for updating the agent's currentPrice and resetting the epoch accumulators.



## THINGS TO NOTICE

With two firms (agents), i.e. duopoly and the standard setting (see THINGS TO TRY) and both agents  using the policy of "Market Returns", the price converges to the monopoly price of the lower cost agent.

How robust is this result? What does it take to change it?

What does this suggest about business stragey?

## THINGS TO TRY

Here's a nice standard setting: kFirm0=10, kFirm1=5, epochLengthFirm0 = epochLengthFirm1 = 30. initialPriceFirm0 = 193, initialPriceFirm1 = 220. dSlope=0.5, qIBase=200. variableStarts=On.  variableFactor=0.10.
Try this with numPandAFirms = 1, 2, 3, 4, 5, 6, 8, 10. What happens?
Try varying epoch lengths. What happens? Be sure to mix up "Market Returns" and "Own Returns".

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

Created 2007-03-25 from oligopolyProbeAndAdjust.nlogo.

## CREDITS AND REFERENCES

This model was created and written by Steven O. Kimbrough: kimbrough at wharton.upenn.edu and http://opim.wharton.upenn.edu/~sok/.  The model is
freely downloadable at:
http://opim.wharton.upenn.edu/~sok/AGEbook/nlogo/OligopolyBidPrice.nlogo

Please give me credit if you use this model or program.

To refer to this model in academic publications, please use: Kimbrough, Steven O. (2011). Oligopoly Bid Price
model. http://opim.wharton.upenn.edu/~sok/AGEbook/nlogo/OligopolyBidPrice.nlogo University of Pennsylvania, Philadelphia, PA 19004, USA.
In other publications, please use: Copyright 2011 Steven O. Kimbrough. All
rights reserved.

Version information: $Id: OligopolyBidPrice.nlogo 5330 2016-04-14 18:56:52Z sok $
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
<experiments>
  <experiment name="experiment1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 2000</exitCondition>
    <metric>runningAverageBid</metric>
    <enumeratedValueSet variable="numEpisodesToRun">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reportEveryNEpisodes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variableFactor">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deltaFirm1">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilonFirm0">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qIBase">
      <value value="222"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reporting">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epochLengthFirm0">
      <value value="10"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="runningAvgLength">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daRandomSeed">
      <value value="&quot;system clock&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilonFirm1">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dSlope">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patienceFirm1">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm0">
      <value value="&quot;Market Returns s.t. Own Returns&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPriceFirm1">
      <value value="193"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kFirm1">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patienceFirm0">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aDelta">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm1">
      <value value="&quot;Market Returns s.t. Own Returns&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-walk-as">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numPandAFirms">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kFirm0">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPriceFirm0">
      <value value="193"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deltaFirm0">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epochLengthFirm1">
      <value value="10"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variableStarts">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 8000</exitCondition>
    <metric>runningAverageBid</metric>
    <enumeratedValueSet variable="numEpisodesToRun">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reportEveryNEpisodes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variableFactor">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deltaFirm1">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilonFirm0">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qIBase">
      <value value="222"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reporting">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epochLengthFirm0">
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="runningAvgLength">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daRandomSeed">
      <value value="&quot;system clock&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilonFirm1">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dSlope">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patienceFirm1">
      <value value="0.9"/>
      <value value="1"/>
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm0">
      <value value="&quot;Market Returns s.t. Own Returns&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPriceFirm1">
      <value value="193"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kFirm1">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patienceFirm0">
      <value value="0.9"/>
      <value value="1"/>
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aDelta">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm1">
      <value value="&quot;Market Returns s.t. Own Returns&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-walk-as">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numPandAFirms">
      <value value="6"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kFirm0">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPriceFirm0">
      <value value="193"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deltaFirm0">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epochLengthFirm1">
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variableStarts">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
