; Q = a - b*P -- the demand function, demand as a function of price.
; or
; Q = QIntercept - dSlope*P

globals [ CostDemandInterceptPrice CostDemandInterceptQuantity
          InitialCostDemandInterceptPrice InitialCostDemandInterceptQuantity
          NominalProfitFirm1 NominalProfitFirm2
          MyEpsilon
          DaVersion
          RunningIndustryProfits
          RunningPrices
          RunningQuantities
          EpisodeRunCounter ; tallies the number of episodes actually completed in a run
          ReplicationsCompleted ; counts the number of replications completed. Used for comparison with
          DaEpsilon
          NominalIndustryProfit
] ; end of globals

breed [markets market]
breed [suppliers supplier]

markets-own [BidSum
             CurrentMarketPrice
             CurrentMarketQuantity]

suppliers-own [BidVector
             EpisodeCount
             EpochLength
             BidderType
             UpdateType
             MyEpisodeProfit IndustryEpisodeProfit ; for a single episode
             MyEpisodeProfits IndustryEpisodeProfits ; lists of episode results during an epoch
             CostsOfPlants ; A vector holding the costs of the agent's various plants.
             ; It should be strictly increasing.
             ; In priciple, each agent may have a different number of plants.
             CapacitiesOfPlants; A vector holding the capacities of the agent's various plants.
             ; CostsOfPlants and CapacitiesOfPlants should have the same length
             TotalCapacity ; the sum of the CapacitiesOfPlants vector.
             BasePrices ; A vector holding the baseline prices for bids by the agent.
             BaseQuantities ; A vector holding the baseline quantities for bids by the agent.
             ; BasePrices and BaseQuantities should have the same length.
             PriceDeltas ; Should have the same length as BasePrices. These are the deltas
             ; the agent will use in Probing.
             QuantityDeltas  ; Should have the same length as BasePrices. These are the deltas
             ; the agent will use in Probing.
             PriceEpsilons
             QuantityEpsilons
             CurrentBidPrices
             CurrentBidQuantities
             CurrentBidBreaks
             CurrentQuantityDemanded
             CurrentGrossRevenue
             CurrentCosts
             CurrentProfit
             EpochCostPerformance EpochCostPerformanceMarket
             EpochQuantityPerformance EpochQuantityPerformanceMarket
             EpochCostCountPerformance
             EpochQuantityCountPerformance
             RunningProfits
             BaseQuantitiesBreaks ; This is to be a cumulative distribution without the final term, the 1, e.g., (0.2 0.7). Then to get to the 1 we need 0.3
             ]

to Setup
  ; This is the first part of SetupAndGo.
;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks ; Clear everything, reset variables, reset plots, etc.
; Determine the random seed to use and print out the first random-float:
ifelse (RandomNumberSeed = "System Clock")
   [random-seed new-seed]
   [random-seed RandomNumberSeed]
; For displaying the version info from the CVS repository:
set daVersion "$Id: SupplyCurveBidding.nlogo 2351 2011-05-22 17:28:38Z sok $"
; myEpsilon is simply a small floating point number to handle numerical
; comparisons:
set MyEpsilon 0.000001
let meanProfits1 0
if (Logging?) [
  if (file-exists? "runsOutput.txt")
   [file-close
    file-delete "runsOutput.txt"]
 file-open "runsOutput.txt"
 let fileHeader (word "replicationNumber,StoredCases,EpisodesPerRun,RandomNumberSeed,numberOfFirms,numberOfReplications,meanRunningIndustryProfits,varianceRunningIndustryProfits")
 set fileHeader (word fileHeader "," "meanRunningPrices" "," "varianceRunningPrices,meanrunningQuantities,varianceRunningQuantities")
 set fileHeader (word fileHeader ",runningAvgProfitsFirm0,runningAvgProfitsFirm1")
 set fileHeader (word fileHeader ",Cost1Firm0,Cost1Firm1,MaxQ1Firm0,MaxQ1Firm1")
 set fileHeader (word fileHeader ",Cost2Firm0,Cost2Firm1,MaxQ2Firm0,MaxQ2Firm1")
 set fileHeader (word fileHeader ",InitialBasePrice1Firm0,InitialBasePrice1Firm1,InitialBasePrice2Firm0,InitialBasePrice2Firm1")
 set fileHeader (word fileHeader ",updateTypeFirm1,updateTypeFirm1,bidderTypeFirm0,bidderTypeFirm1")
 set fileHeader (word fileHeader ",initialBaseQ2ShadeFirm0,initialBaseQ2ShadeFirm1,deltaFirm0,deltaFirm1,epsilonFirm0,epsilonFirm1")
 set fileHeader (word fileHeader ",epochLengthFirm0,epochLengthFirm1,QIntercept,DSlope,runningAverageLength,QuantityChoosing?,daVersion,date-and-time")
 file-print fileHeader
 file-flush
 ] ; end of if (logging?) [
  reset-ticks
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to SetupAndGo
;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks ; Clear everything, reset variables, reset plots, etc.
; Determine the random seed to use and print out the first random-float:
ifelse (RandomNumberSeed = "System Clock")
   [random-seed new-seed]
   [random-seed RandomNumberSeed]
;print (word "The first random float is " random-float 1)
; For displaying the version info from the CVS repository:
set daVersion "$Id: SupplyCurveBidding.nlogo 2351 2011-05-22 17:28:38Z sok $"
; myEpsilon is simply a small floating point number to handle numerical
; comparisons:
set MyEpsilon 0.000001
let meanProfits1 0
SetupARun ; a dummy, just to get the data to print the file headers
if (Logging?) [
  if (file-exists? "runsOutput.txt")
   [file-close
    file-delete "runsOutput.txt"]
 file-open "runsOutput.txt"
 let fileHeader (word "replicationNumberBob,StoredCases,EpisodesPerRun,RandomNumberSeed,numberOfFirms,numberOfReplications,meanRunningIndustryProfits,varianceRunningIndustryProfits")
 set fileHeader (word fileHeader "," "meanRunningPrices" "," "varianceRunningPrices,meanrunningQuantities,varianceRunningQuantities")
   foreach sort-by [ [?1 ?2] -> [who] of ?1 < [who] of ?2 ] suppliers
  [ ?1 -> let tempString ""
    ask ?1 [
      set tempString (word tempString   "Supplier" [who] of self "meanRunningProfits" "," "Supplier" [who] of self "costsofplants" ","  "Supplier" [who] of self "capacitiesofplants")
      set tempString (word tempString "," "Supplier" [who] of self "updatetype")
      set fileHeader (word fileHeader "," tempString)
  ]
  ]
; set fileHeader (word fileHeader ",runningAvgProfitsFirm0,runningAvgProfitsFirm1")
; set fileHeader (word fileHeader ",Cost1Firm0,Cost1Firm1,MaxQ1Firm0,MaxQ1Firm1")
; set fileHeader (word fileHeader ",Cost2Firm0,Cost2Firm1,MaxQ2Firm0,MaxQ2Firm1")
; set fileHeader (word fileHeader ",InitialBasePrice1Firm0,InitialBasePrice1Firm1,InitialBasePrice2Firm0,InitialBasePrice2Firm1")
; set fileHeader (word fileHeader ",updateTypeFirm1,updateTypeFirm1,bidderTypeFirm0,bidderTypeFirm1")
; set fileHeader (word fileHeader ",initialBaseQ2ShadeFirm0,initialBaseQ2ShadeFirm1,deltaFirm0,deltaFirm1,epsilonFirm0,epsilonFirm1")
; set fileHeader (word fileHeader ",epochLengthFirm0,epochLengthFirm1,QIntercept,DSlope,runningAverageLength,QuantityChoosing?,daVersion,date-and-time")
set fileHeader (word fileHeader ",QIntercept,DSlope,runningAverageLength,daVersion,date-and-time")
 file-print fileHeader
 file-flush
 ] ; end of if (logging?) [

;;;; These foreach's are a bit of a kludge. Perhaps useful for conducting multiple runs systematically.
foreach [160000] [  ; A
; set EpisodesPerRun ?
 foreach ["N Bidder" ] [ ; B
  ;set bidderTypeFirm0 ?
  set replicationsCompleted 0
  while [replicationsCompleted < NumberOfReplications]
   [SetupARun
    GoNEpisodes
    set replicationsCompleted (replicationsCompleted + 1)
  if (Logging?)[
    let stringToPrint (word replicationsCompleted "," StoredCases "," EpisodesPerRun "," RandomNumberSeed "," (count turtles - 1) "," numberOfReplications)
    set stringToPrint (word stringToPrint "," mean runningIndustryProfits "," variance runningIndustryProfits)
    set stringToPrint (word stringToPrint "," mean runningPrices "," variance runningPrices)
    set stringToPrint (word stringToPrint "," mean runningQuantities "," variance runningQuantities) ; ok to here, Lth column
    ; Now, get the number of firms and the sizes of their plant lists and bid lists  ***here***
  foreach sort-by [ [???1 ???2] -> [who] of ???1 < [who] of ???2 ] suppliers
  [ ???1 -> let tempString ""
    ask ???1 [
      set tempString (word tempString   mean [RunningProfits] of self "," [costsofplants] of self ","  [capacitiesofplants] of self)
      set tempString (word tempString "," [updatetype] of self)
      set stringToPrint (word stringToPrint "," tempString)
  ]
  ]
    set stringToPrint (word stringToPrint "," QIntercept "," DSlope "," runningAverageLength  "," daVersion ",'" date-and-time "'")
;;;;;;;;;;;;;;;;
;    ifelse (count turtles > 1)
;     [set meanProfits1 mean [runningProfits] of turtle 1]
;     [set meanProfits1 "N/A"]
;    set stringToPrint (word stringToPrint "," mean [runningProfits] of turtle 1 "," meanProfits1)
;    set stringToPrint (word stringToPrint "," Cost1Firm1 "," Cost1Firm2 "," MaxQ1Firm1 "," MaxQ1Firm2)
;    set stringToPrint (word stringToPrint "," Cost2Firm1 "," Cost2Firm2 "," MaxQ2Firm1 "," MaxQ2Firm1)
;    set stringToPrint (word stringToPrint "," InitialBasePrice1Firm1 "," InitialBasePrice1Firm2 "," InitialBasePrice2Firm1 "," InitialBasePrice2Firm2)
;    set stringToPrint (word stringToPrint "," updateTypeFirm1 "," updateTypeFirm2 )
;    set stringToPrint (word stringToPrint ","  priceDeltaFirm1 "," priceDeltaFirm2 "," priceEpsilonFirm1 "," priceEpsilonFirm2)
;    set stringToPrint (word stringToPrint "," epochLengthFirm1 "," epochLengthFirm2 "," QIntercept "," DSlope "," runningAverageLength  "," daVersion "," date-and-time)
    file-print stringToPrint
    file-flush
   ] ; end of if (Logging?)[
     ] ; end of while [runsCompleted < NumberOfRuns]
 ] ; end of foreach [4.5 5.0 5.5] [ ; B
] ; end of foreach [0.8 1.0 1.2] [  ; A
if (Logging?)[
 file-close
 ] ; end of if (Logging?)[
end ; of SetupAndGo

to test

  foreach sort-by [ [?1 ?2] -> [who] of ?1 < [who] of ?2 ] suppliers
  [ ?1 -> let tempString ""
    ask ?1 [set tempString (word tempString "," [who] of self "," length [costsofplants] of self )
      print tempString
  ]
  ]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to SetupARun
; We can't use clear-all (ca) and clear everything, reset variables, reset plots, etc.
; because we want to conduct multiple runs and we want Setup to set things up for a
; singe run. So we
clear-all-plots
clear-output
clear-turtles
; We could use the built-in tick function and ticks reporter:
set episodeRunCounter 0
; Initialize as empty various accumulating lists. The running average lists
; are to hold industry profits, industry prices and industry quantities. The
; averages are to be taken over the contents of the various lists.
; Shades holds the "shaded" quantities per episode.

set runningIndustryProfits []
set runningPrices []
set runningQuantities []

; Turtle 0 will be a market agent.
create-markets 1
SetupAgents ; The is the new code.

; Now get their initial "bids", in which the bids equal the costs.
ask suppliers [
;      ; Each turtle/firm records in its private BidVector attribute its price and quantity bids.
;      ; Here we set them to their true costs and full quantities.

       ; The old pattern has triplets: [cost pricebid quantity]. This needs to change to [pricebid quantity]. Sigh.
       set BidVector []
       foreach n-values length CostsOfPlants [ ?1 -> ?1 ] [ ?1 ->
         set BidVector lput (list ([who] of self) (item ?1 CostsOfPlants) (item ?1 CapacitiesOfPlants)) BidVector
       ] ; OK.

]
; Report costs for plotting in the (true) "Cost & Demand Curves" plot.

ask market 0 [SumTheFirms]

ReportCosts(QIntercept)(dSlope)

set NominalProfitFirm1 [CurrentProfit] of supplier 1
if (count suppliers > 1)
[set NominalProfitFirm2 [CurrentProfit] of supplier 2]

set NominalIndustryProfit 0
ask suppliers [
  set NominalIndustryProfit (NominalIndustryProfit + CurrentProfit)
]
  reset-ticks
end ; of SetupARun



to GoNEpisodes
while [episodeRunCounter < episodesPerRun]
[Go]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Go
  tick
  ask suppliers [
    MakeANewBid-Suppliers
    ]
  let daBidSum 0
  ask market 0 [SumTheFirms]
  set daBidSum [BidSum] of market 0

  ; 6. Figure out the (price, quantity) intersection of the summed bids and the demand curve
  let PriceQuantityIntersection daIntersection(daBidSum)([])(QIntercept)(DSlope)
  set CostDemandInterceptPrice first PriceQuantityIntersection
  set CostDemandInterceptQuantity last PriceQuantityIntersection
  ; 7. And then plot them
  ;print (word "In Go, starting 7, about to PlotSupplyAndDemandCurves). daBidSum = " daBidSum)
  PlotSupplyAndDemandCurves(QIntercept)(DSlope)(daBidSum)
  ; 8. Have the suppliers calculate their episode profits.
  ask suppliers [DetermineFirmProfits-Suppliers]
  ;  Record episode profits (redundant)
  ask suppliers [RecordFirmEpisodeProfit] ; (CostDemandInterceptPrice)(CostDemandInterceptQuantity)(daBidSum)]
  let industryProfit 0
  ask suppliers [
    set industryProfit (industryProfit + myEpisodeProfit)]
  ask suppliers [
      set industryEpisodeProfit industryProfit
      set industryEpisodeProfits lput industryEpisodeProfit industryEpisodeProfits
      ]
  ; 9. Record and calculate statistics for the episode
  set runningIndustryProfits lput industryProfit runningIndustryProfits
  if (length runningIndustryProfits > runningAverageLength)
    [set runningIndustryProfits but-first runningIndustryProfits]

  set runningPrices lput CostDemandInterceptPrice runningPrices
  if (length runningPrices > runningAverageLength)
    [set runningPrices but-first runningPrices]

  set runningQuantities lput CostDemandInterceptQuantity runningQuantities
  if (length runningQuantities > runningAverageLength)
    [set runningQuantities but-first runningQuantities]

  ; 10. Each firm observes what has happened and records in an internal representation what goes on.
  ask suppliers [ObserveAndRecord-Suppliers]
  ; 11. Each firm/turtle updates its policies of play, if the episode has concluded an epoch.
  ;print (word "Pre: [episodeCount] of supplier 1 = " [episodeCount] of supplier 1)
  ask suppliers [set episodeCount episodeCount + 1]
  ask suppliers [PostPareEpisode-Suppliers]
  ; 12. Plot the price realized.
  PlotPrice(CostDemandInterceptPrice)
  ; 13. Plot the profit realized in the episode for each firm
  PlotProfits
  ; 14. Advance the episode run counter
  set episodeRunCounter (episodeRunCounter + 1) ; since we've completed an episode

  ask turtle 1 [PlotTestPlot]
  if (count suppliers > 1)
   [ask turtle 2 [PlotTestPlot]]
end ; of Go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to PostPareEpisode-Suppliers
  ; First see if the supplier's epoch is indeed over:
 if (episodeCount  >= epochLength) [
     if (not first AreBreaksValid?(BaseQuantitiesBreaks))
    [print (word "000. Turtle " who " ticks= " ticks " Invalid BaseQuantitiesBreaks: " BaseQuantitiesBreaks)]

  let numberOfBidItems length BasePrices
  let daIndices n-values numberOfBidItems [ ?1 -> ?1 ]

  ; I do prices first.
  let NewBasePrices []
  let numberOfBidBreaks length BaseQuantitiesBreaks
  let daBreaksIndices n-values numberOfBidBreaks [ ?1 -> ?1 ]
  let NewBaseQuantitiesBreaks []
  let TempUpdateType ""
  if (UpdateType = "Own Returns") [
    set TempUpdateType "Own Returns"]
  if (UpdateType = "Market Returns") [
    set TempUpdateType "Market Returns"]
  if (UpdateType = "Market Returns Constrained by Own Returns") [
        ifelse (((MyMean myEpisodeProfits) * Patience) >= (MyMean industryEpisodeProfits)  / count suppliers)
        [set TempUpdateType "Market Returns"
          ]
        [set TempUpdateType "Own Returns"
          ]
     ]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Own Returns ==> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;; Prices ;;;;;;;;;;;;;;;;;
  if (TempUpdateType = "Own Returns") [   ; "Own Returns" Market Returns
     if (not first AreBreaksValid?(BaseQuantitiesBreaks))
    [print (word "333. Turtle " who " ticks= " ticks " Invalid BaseQuantitiesBreaks: " BaseQuantitiesBreaks)]

  foreach daIndices [ ?1 ->
    let daIndex ?1
    let daResult GetBestPerformance(daIndex)(EpochCostPerformance)(EpochCostCountPerformance)
    if (daResult = 0) ; then we did best bidding low
      [set NewBasePrices lput ((item daIndex BasePrices) - (item daIndex PriceEpsilons)) NewBasePrices]
    if (daResult = 1) ; then we did best staying as we were
      [set NewBasePrices lput ((item daIndex BasePrices)) NewBasePrices]
    if (daResult = 2) ; then we did best bidding high
      [
       set NewBasePrices lput ((item daIndex BasePrices) + (item daIndex PriceEpsilons)) NewBasePrices
       ]
    ]

  ;;;;;;;;;;;;;;;;; quantities ;;;;;;;;;;;;;;;;;;;;
  foreach daBreaksIndices [ ?1 ->
    let daIndex ?1
    ; EpochQuantityPerformance is a list of lists.
    ; Each top-level list contains information about the performance of one quantity break, e.g., 0.3 or 0.7, if there are two.
    ; Each bottom-level list contains three items: 0, performance score for bidding low, 1, performance score for stasis,
    ; 2, performance score for bidding high. In the governing foreach loop we process all top-level items in
    ; EpochQuantityPerformance. daIndex is the position number being processed. The corresponding item is
    ; (item daIndex EpochQuantityPerformance). In it we want to know which of the three values is largest.
    ; (max item daIndex EpochQuantityPerformance) is for that. But we need to know where it is.
    ; position is for that. daResult gets the position of the largest score in the triple corresponding to the current index.
    let daResult GetBestPerformance(daIndex)(EpochQuantityPerformance)(EpochQuantityCountPerformance)
    if (daResult = 0) ; then we did best bidding low
      [let downQuantity ((item daIndex BaseQuantitiesBreaks) - (item daIndex QuantityEpsilons))  ; reduced by epsilon
       set downQuantity max (list downQuantity 0.01) ; so it is positive and not zero
       if (daIndex > 0 and downQuantity < last NewBaseQuantitiesBreaks) ; if we are not at the first break, make sure we don't drop below the previous one.
         [
           set downQuantity max (list downQuantity ((last NewBaseQuantitiesBreaks) ))
           ]
        set NewBaseQuantitiesBreaks lput downQuantity NewBaseQuantitiesBreaks]
    if (daResult = 1) ; then we did best staying as we were
      [let sameQuantity (item daIndex BaseQuantitiesBreaks)
       if (daIndex > 0 and sameQuantity < last NewBaseQuantitiesBreaks) ; if we are not at the first break, make sure we don't drop below the previous one.
           [
           set sameQuantity max (list sameQuantity ((last NewBaseQuantitiesBreaks) ))
           ]
        set NewBaseQuantitiesBreaks lput sameQuantity NewBaseQuantitiesBreaks
        ]
    if (daResult = 2) ; then we did best bidding high
      [let upQuantity ((item daIndex BaseQuantitiesBreaks) + (item daIndex QuantityEpsilons)) ; increased by epsilon
       set upQuantity min (list upQuantity 1.0) ; so it is not more than 1.0.
       if (daIndex > 0 and upQuantity < last NewBaseQuantitiesBreaks) ; if we are not at the first break, make sure we don't drop below the previous one.
         [
           set upQuantity max (list upQuantity ((last NewBaseQuantitiesBreaks) ))
           ]
        set NewBaseQuantitiesBreaks lput upQuantity NewBaseQuantitiesBreaks
        ]
  ] ; end of foreach daBreaksIndices [

  let NextBaseQuantities NewBaseQuantitiesBreaks
  set BaseQuantitiesBreaks NextBaseQuantities ; NewBaseQuantities
  if (not first AreBreaksValid?(BaseQuantitiesBreaks))
    [print (word "444. Turtle " who " ticks= " ticks " Invalid BaseQuantitiesBreaks: " BaseQuantitiesBreaks)]
 ] ; end of if (UpdateType = "Own Returns")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  <=== Own Returns  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Market Returns ===>  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if (TempUpdateType = "Market Returns") [
  foreach daIndices [ ?1 ->
    let daIndex ?1
    let daResult GetBestPerformance(daIndex)(EpochCostPerformanceMarket)(EpochCostCountPerformance)
    if (daResult = 0) ; then we did best bidding low
      [set NewBasePrices lput ((item daIndex BasePrices) - (item daIndex PriceEpsilons)) NewBasePrices]
    if (daResult = 1) ; then we did best staying as we were
      [set NewBasePrices lput ((item daIndex BasePrices)) NewBasePrices]
    if (daResult = 2) ; then we did best bidding high
      [set NewBasePrices lput ((item daIndex BasePrices) + (item daIndex PriceEpsilons)) NewBasePrices]
   ]  ; end of foreach
  ;;;;;;;;;;;;;;;;; quantities ;;;;;;;;;;;;;;;;;;;;
  foreach daBreaksIndices [ ?1 ->
    let daIndex ?1
    let daResult GetBestPerformance(daIndex)(EpochQuantityPerformanceMarket)(EpochQuantityCountPerformance)
    if (daResult = 0) ; then we did best bidding low
      [let downQuantity ((item daIndex BaseQuantitiesBreaks) - (item daIndex QuantityEpsilons))  ; reduced by epsilon
        set downQuantity max (list downQuantity 0.01) ; so it is positive and not zero
       if (daIndex > 0 and downQuantity < last NewBaseQuantitiesBreaks) ; if we are not at the first break, make sure we don't drop below the previous one.
         [set downQuantity max (list downQuantity ((last NewBaseQuantitiesBreaks) ))
           ]
       set NewBaseQuantitiesBreaks lput downQuantity NewBaseQuantitiesBreaks] ; end of if (daResult = 0)
    if (daResult = 1) ; then we did best staying as we were
      [let sameQuantity (item daIndex BaseQuantitiesBreaks)
       if (daIndex > 0 and sameQuantity < last NewBaseQuantitiesBreaks) ; if we are not at the first break, make sure we don't drop below the previous one.
         [set sameQuantity max (list sameQuantity ((last NewBaseQuantitiesBreaks) ))
           ]
       set NewBaseQuantitiesBreaks lput sameQuantity NewBaseQuantitiesBreaks
        ]
    if (daResult = 2) ; then we did best bidding high
      [let upQuantity ((item daIndex BaseQuantitiesBreaks) + (item daIndex QuantityEpsilons)) ; increased by epsilon
       set upQuantity min (list upQuantity 1.0) ; so it is not more than 1.0.
       if (daIndex > 0 and upQuantity < last NewBaseQuantitiesBreaks) ; if we are not at the first break, make sure we don't drop below the previous one.
         [set upQuantity max (list upQuantity ((last NewBaseQuantitiesBreaks) ))
           ]
       set NewBaseQuantitiesBreaks lput upQuantity NewBaseQuantitiesBreaks]
  ] ; end of foreach daBreaksIndices [
  let NextBaseQuantities NewBaseQuantitiesBreaks
  set BaseQuantitiesBreaks NextBaseQuantities ; NewBaseQuantities
  if (not first AreBreaksValid?(BaseQuantitiesBreaks))
    [print (word "555. Turtle " who " ticks= " ticks " Invalid BaseQuantitiesBreaks: " BaseQuantitiesBreaks)]
    ] ; end of if (UpdateType = "Market Returns")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  <=== Market Returns  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  if (max BaseQuantitiesBreaks > 1.0) [
    print (word "Warning: BaseQuantitiesBreaks=" BaseQuantitiesBreaks " of turtle " [who] of self ". Rounding, but continuing.")
    let ted position max BaseQuantitiesBreaks BaseQuantitiesBreaks
    set BaseQuantitiesBreaks replace-item ted BaseQuantitiesBreaks 1.0
  ]
  if (min BaseQuantitiesBreaks < 0.0) [
    print (word "Warning: BaseQuantitiesBreaks=" BaseQuantitiesBreaks " of turtle " [who] of self ". Rounding, but continuing.")
    let ted position min BaseQuantitiesBreaks BaseQuantitiesBreaks
    set BaseQuantitiesBreaks replace-item ted BaseQuantitiesBreaks 0.0
  ]
  if (not first AreBreaksValid?(BaseQuantitiesBreaks))
    [print (word "111. Turtle " who " ticks= " ticks " Invalid BaseQuantitiesBreaks: " BaseQuantitiesBreaks)]

  SetBaseQuantities
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Now check the prices and raise them if they are too low:
  let NextBasePrices []
  foreach daIndices [ ?1 ->
    let daIndex ?1
    if (item daIndex BaseQuantities < 0)
      [set BaseQuantities replace-item daIndex BaseQuantities 0]
    let daCost PriceForQuantity(item ?1 BaseQuantities)(CapacitiesOfPlants)(CostsOfPlants)
    ifelse (item ?1 NewBasePrices < daCost)
      [set NextBasePrices lput daCost NextBasePrices]
      [set NextBasePrices lput item ?1 NewBasePrices NextBasePrices]
  ]
  set BasePrices NextBasePrices

  if (not first AreBreaksValid?(BaseQuantitiesBreaks))
    [print (word "222. Turtle " who " ticks= " ticks " Invalid BaseQuantitiesBreaks: " BaseQuantitiesBreaks)]

  if (abs ((sum BaseQuantities) - TotalCapacity) > 0.001)  ; /* here */
    [print (word "Problem, (sum BaseQuantities) = " (sum BaseQuantities) " and TotalCapacity = " TotalCapacity " BaseQuantities= " BaseQuantities)
     print (word "I am turtle " who " and I am Quitting. BaseQuantitiesBreaks= " BaseQuantitiesBreaks " ticks=" ticks)
     let bob 0
     let carol 7 / bob]
   if (min BaseQuantities < 0)
    [print (word "Problem, BaseQuantities = " BaseQuantities " I am turtle " who " and I am Quitting.")
           let bob 0
     let carol 7 / bob]

       set episodeCount 0
       set myEpisodeProfits []
       set industryEpisodeProfits []
       let performanceItem (list 0 0 0)
       set EpochCostPerformance []
       set EpochCostCountPerformance []
       set EpochQuantityPerformance []
       set EpochQuantityCountPerformance []
       set EpochCostPerformanceMarket []
       set EpochQuantityPerformanceMarket []
       foreach BasePrices [
       set EpochCostPerformance lput performanceItem EpochCostPerformance
       set EpochCostCountPerformance lput performanceItem EpochCostCountPerformance
       set EpochQuantityPerformance lput performanceItem EpochQuantityPerformance
       set EpochQuantityCountPerformance lput performanceItem EpochQuantityCountPerformance
       set EpochCostPerformanceMarket lput performanceItem EpochCostPerformanceMarket
       set EpochQuantityPerformanceMarket lput performanceItem EpochQuantityPerformanceMarket
               ]
      ] ; end of if (episodeCount >= epochLength)
 ; ] ; end of if (is-turtle? self)
end ; of PostPareEpisode

to RecordFirmEpisodeProfit
  if (is-turtle? self) [
   set myEpisodeProfit CurrentProfit
   set myEpisodeProfits lput myEpisodeProfit myEpisodeProfits
   set RunningProfits lput myEpisodeProfit RunningProfits
   if (length RunningProfits > RunningAverageLength)
    [set RunningProfits but-first RunningProfits]
   ]
end

to ObserveAndRecord-Suppliers
  ; When I bid, I am bidding N prices and N-1 quantities.  I have to do it with lists, for generality. Here's now.
  ; I keep two fields for each supplier: EpochCostPerformance and EpochQuantityPerformance. Each is a list
  ; of lists. The internal lists will be ordered as in BasePrices and BaseQuantities.
  ; Each internal list has three elements, in order: epoch profits when bidding low,
  ; epoch profits when bidding the same, epoch profits when bidding high.
  ; There are what are updated in ObserveAndRecord.
  ; Notice that the agent has all the information it needs to update the values. It knows
  ;
  ; EpochCostPerformance and EpochQuantityPerformance for the agent's performance.
  ; EpochCostPerformanceMarket and EpochQuantityPerformanceMarket for the overall industry,
  ; that is, all suppliers.
  ; CurrentProfit,BasePrices, BaseQuantities, CurrentBidPrices, and CurrentBidQuantities.
  ; CurrentProfit holds the agent's episode profit.
  ; IndustryEpisodeProfit holds the.... overall industry profit for the episode.
  ;
  ; A bid will count as the same, if its value is within 0.001 of its base value.
  ; First, note that I'll need to initialize the suppliers properly now. Here is how I did it:
  ;               let performanceItem (list 0 0 0)
  ;               set EpochCostPerformance []
  ;               set EpochQuantityPerformance []
  ;               foreach BasePrices [
  ;                 set EpochCostPerformance lput performanceItem EpochCostPerformance
  ;                 set EpochQuantityPerformance lput performanceItem EpochQuantityPerformance
  ;               ]
  if (CurrentProfit != MyEpisodeProfit)
  [print (word "Quitting. CurrentProfit != MyEpisodeProfit " CurrentProfit " " MyEpisodeProfit)
    let sam 0
    set sam 6 / sam]
  if (count suppliers > 1 and [IndustryEpisodeProfit] of turtle 1 != [IndustryEpisodeProfit] of turtle 2)
  [print (word "Quitting. [IndustryEpisodeProfit] of turtle 1 != [IndustryEpisodeProfit] of turtle 2 " [IndustryEpisodeProfit] of turtle 1 " " [IndustryEpisodeProfit] of turtle 2)
    let sam 0
    set sam 6 / sam]
  let microEpsilon 0.001
  let numberOfBidItems length BasePrices
  let daIndices n-values numberOfBidItems [ ?1 -> ?1 ]
  let NewEpochCostPerformance []
  let NewEpochCostCountPerformance []
  let NewEpochQuantityPerformance []
  let NewEpochQuantityCountPerformance []
  let NewEpochCostPerformanceMarket []
  let NewEpochQuantityPerformanceMarket []
  ; First, do it for cost performance.
  foreach daIndices [ ?1 ->
    let BidItem ?1
    ifelse (item BidItem CurrentBidPrices > ((item BidItem BasePrices) + microEpsilon))
      [; First, the agent's own profits
       let newLow item 0 item BidItem EpochCostPerformance
       let newSame item 1 item BidItem EpochCostPerformance
       let newHigh item 2 item BidItem EpochCostPerformance + CurrentProfit ; should be equal to MyEpisodeProfit
       set NewEpochCostPerformance lput (list newLow newSame newHigh) NewEpochCostPerformance
       ; Next, the industry profits
       set newLow item 0 item BidItem EpochCostPerformanceMarket
       set newSame item 1 item BidItem EpochCostPerformanceMarket
       set newHigh item 2 item BidItem EpochCostPerformanceMarket + IndustryEpisodeProfit
       set NewEpochCostPerformanceMarket lput (list newLow newSame newHigh) NewEpochCostPerformanceMarket
       ; Now do the counts
       set newLow (item 0 item BidItem EpochCostCountPerformance)
       set newSame (item 1 item BidItem EpochCostCountPerformance)
       set newHigh ((item 2 item BidItem EpochCostCountPerformance) + 1)
       set NewEpochCostCountPerformance lput (list newLow newSame newHigh) NewEpochCostCountPerformance
      ]
      [ifelse (item BidItem CurrentBidPrices < item BidItem BasePrices - microEpsilon)
      [; First, the agent's own profits
       let newLow item 0 item BidItem EpochCostPerformance + CurrentProfit
       let newSame item 1 item BidItem EpochCostPerformance
       let newHigh item 2 item BidItem EpochCostPerformance
       set NewEpochCostPerformance lput (list newLow newSame newHigh) NewEpochCostPerformance
       ; Next, the industry profits
       set newLow item 0 item BidItem EpochCostPerformanceMarket + IndustryEpisodeProfit
       set newSame item 1 item BidItem EpochCostPerformanceMarket
       set newHigh item 2 item BidItem EpochCostPerformanceMarket
       set NewEpochCostPerformanceMarket lput (list newLow newSame newHigh) NewEpochCostPerformanceMarket
       ; Now do the counts
       set newLow ((item 0 item BidItem EpochCostCountPerformance) + 1)
       set newSame (item 1 item BidItem EpochCostCountPerformance)
       set newHigh (item 2 item BidItem EpochCostCountPerformance)
       set NewEpochCostCountPerformance lput (list newLow newSame newHigh) NewEpochCostCountPerformance
      ]
      [; First, the agent's own profits
       let newLow item 0 item BidItem EpochCostPerformance
       let newSame item 1 item BidItem EpochCostPerformance + CurrentProfit
       let newHigh item 2 item BidItem EpochCostPerformance
       set NewEpochCostPerformance lput (list newLow newSame newHigh) NewEpochCostPerformance
       ; Next, the industry profits
       set newLow item 0 item BidItem EpochCostPerformanceMarket
       set newSame item 1 item BidItem EpochCostPerformanceMarket + IndustryEpisodeProfit
       set newHigh item 2 item BidItem EpochCostPerformanceMarket
       set NewEpochCostPerformanceMarket lput (list newLow newSame newHigh) NewEpochCostPerformanceMarket
       ; Now do the counts
       set newLow (item 0 item BidItem EpochCostCountPerformance)
       set newSame ((item 1 item BidItem EpochCostCountPerformance) + 1)
       set newHigh (item 2 item BidItem EpochCostCountPerformance)
       set NewEpochCostCountPerformance lput (list newLow newSame newHigh) NewEpochCostCountPerformance
      ]
      ]
  ] ; end of foreach daIndices [

  set EpochCostPerformance NewEpochCostPerformance
  set EpochCostPerformanceMarket NewEpochCostPerformanceMarket
  set EpochCostCountPerformance NewEpochCostCountPerformance
;;;;;;;;;;;;;;;;;;;;;;;;;;;quality performance;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Then, do it for quality performance.
  ; But now I have a different kind of index. CurrentBidBreaks
  let numberOfBidBreaks length CurrentBidBreaks
  let daQuantityIndices n-values numberOfBidBreaks [ ?1 -> ?1 ]
  foreach daQuantityIndices [ ?1 ->
    let BidBreakItem ?1
    ifelse (item BidBreakItem CurrentBidBreaks > ((item BidBreakItem BaseQuantitiesBreaks) + microEpsilon)) ; 9/1/10: bombs here
      [; First, the agent's own profits
       let newLow item 0 item BidBreakItem EpochQuantityPerformance
       let newSame item 1 item BidBreakItem EpochQuantityPerformance
       let newHigh item 2 item BidBreakItem EpochQuantityPerformance + CurrentProfit
       set NewEpochQuantityPerformance lput (list newLow newSame newHigh) NewEpochQuantityPerformance
       ; Next, the industry profits
       set newLow item 0 item BidBreakItem EpochQuantityPerformanceMarket
       set newSame item 1 item BidBreakItem EpochQuantityPerformanceMarket
       set newHigh item 2 item BidBreakItem EpochQuantityPerformanceMarket + IndustryEpisodeProfit
       set NewEpochQuantityPerformanceMarket lput (list newLow newSame newHigh) NewEpochQuantityPerformanceMarket
       ; Now do the counts
       set newLow (item 0 item BidBreakItem EpochQuantityCountPerformance)
       set newSame (item 1 item BidBreakItem EpochQuantityCountPerformance)
       set newHigh ((item 2 item BidBreakItem EpochQuantityCountPerformance) + 1)
       set NewEpochQuantityCountPerformance lput (list newLow newSame newHigh) NewEpochQuantityCountPerformance
      ] ; end of ifelse (item BidBreakItem CurrentBidBreaks >...  Next is the else part:
      [ifelse (item BidBreakItem CurrentBidQuantities < item BidBreakItem BaseQuantities - microEpsilon)
      [; First, the agent's own profits
       let newLow item 0 item BidBreakItem EpochQuantityPerformance + CurrentProfit
       let newSame item 1 item BidBreakItem EpochQuantityPerformance
       let newHigh item 2 item BidBreakItem EpochQuantityPerformance
       set NewEpochQuantityPerformance lput (list newLow newSame newHigh) NewEpochQuantityPerformance
       ; Next, the industry profits
       set newLow item 0 item BidBreakItem EpochQuantityPerformanceMarket + IndustryEpisodeProfit
       set newSame item 1 item BidBreakItem EpochQuantityPerformanceMarket
       set newHigh item 2 item BidBreakItem EpochQuantityPerformanceMarket
       set NewEpochQuantityPerformanceMarket lput (list newLow newSame newHigh) NewEpochQuantityPerformanceMarket
       ; Now do the counts
       set newLow ((item 0 item BidBreakItem EpochQuantityCountPerformance) + 1)
       set newSame (item 1 item BidBreakItem EpochQuantityCountPerformance)
       set newHigh (item 2 item BidBreakItem EpochQuantityCountPerformance)
       set NewEpochQuantityCountPerformance lput (list newLow newSame newHigh) NewEpochQuantityCountPerformance
      ]
      [; First, the agent's own profits
       let newLow item 0 item BidBreakItem EpochQuantityPerformance
       let newSame item 1 item BidBreakItem EpochQuantityPerformance + CurrentProfit
       let newHigh item 2 item BidBreakItem EpochQuantityPerformance
       set NewEpochQuantityPerformance lput (list newLow newSame newHigh) NewEpochQuantityPerformance
       ; Next, the industry profits
       set newLow item 0 item BidBreakItem EpochQuantityPerformanceMarket
       set newSame item 1 item BidBreakItem EpochQuantityPerformanceMarket + IndustryEpisodeProfit
       set newHigh item 2 item BidBreakItem EpochQuantityPerformanceMarket
       set NewEpochQuantityPerformanceMarket lput (list newLow newSame newHigh) NewEpochQuantityPerformanceMarket
       ; Now do the counts
       set newLow (item 0 item BidBreakItem EpochQuantityCountPerformance)
       set newSame ((item 1 item BidBreakItem EpochQuantityCountPerformance) + 1)
       set newHigh (item 2 item BidBreakItem EpochQuantityCountPerformance)
       set NewEpochQuantityCountPerformance lput (list newLow newSame newHigh) NewEpochQuantityCountPerformance
      ]
      ]
  ]
  set EpochQuantityPerformance NewEpochQuantityPerformance
  set EpochQuantityPerformanceMarket NewEpochQuantityPerformanceMarket
  set EpochQuantityCountPerformance NewEpochQuantityCountPerformance
end ; of to ObserveAndRecord-Suppliers



to ReportCosts [daQIntercept daDSlope] ; Now this has to be defined for an arbitrary number of suppliers.
  let daCostSum  [BidSum] of market 0
  PlotDemandCurve("Cost & Demand Curves")(daQIntercept)(daDSlope)
  PlotUniqueSummedSupplyCurve("Cost & Demand Curves")(daCostSum)
  let PriceQuantityIntersection daIntersection(daCostSum)([])(daQIntercept)(daDSlope)
  ;print word "PriceQuantityIntersection = " PriceQuantityIntersection
  set InitialCostDemandInterceptPrice first PriceQuantityIntersection
  set InitialCostDemandInterceptQuantity last PriceQuantityIntersection
  let daQuantity InitialCostDemandInterceptQuantity
  ask market 0 [
     set CurrentMarketPrice  first PriceQuantityIntersection
     set CurrentMarketQuantity last PriceQuantityIntersection
     ]
  ask suppliers [DetermineFirmProfits-Suppliers]
end ; of ReportCosts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Stopped cleaning here on 2010-10-9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report daIntersection [daSummedPrices daLastSummedPrices daQIntercept daSlope]
  ;print (word "In daIntersection, daSummedPrices = " daSummedPrices " and daLastSummedPrices = " daLastSummedPrices " and daQIntercept = " daQIntercept " and daSlope = " daSlope)
  ; typically called with:
  ; let PriceQuantityIntersection daIntersection(daBidSum)([])(QIntercept)(DSlope)
  ;print word "In daIntersection, daSummedPrices = " daSummedPrices
  ; Recall for daSummedPrices "The pattern is [firm cost bid low high]"
  ; We need a policy for what to do when the demand curve intersects
  ; a gap between two supply curves. Policy: set the quantity to
  ; the upper bound of the lower-priced curve and set the price
  ; using the demand curve.
  ; Demand curve: Q = daQintercept - daSlope*P
  ; Inverse demand curve: P = daQIntercept/daSlope - Q/daSlope
  ; So DemandCurvePrice = daQIntercept/daSlope - maxQ/daSlope, where maxQ = high
  ;print word "In daIntersection, daSummedPrices = " daSummedPrices
  if daSummedPrices = [] ; Demand outstrips supply
    [let daIQuantity last daLastSummedPrices
     let daIPrice ((daQIntercept / daSlope) - (daIQuantity / daSlope))
     report (list daIPrice daIQuantity)
    ] ; end of if daSummedPrices = []
  ; assuming now daSummedPrices != []
  ; First check to see if the demand curve is below this
  ; segment of the supply curve. If it is, then report
  ; daIQuantity as the high of the LAST demand curve segment
  ; and daIPrice as ((daQIntercept / daSlope) - (daIQuantity / daSlope))
  ;let bid first but-first but-first first daSummedPrices
  let bid item 1 first daSummedPrices
  ;print word "bid = " bid ; bid is ok it seems
  let upperBound last first daSummedPrices
  ;print word "upperBound = " upperBound
  let lowerBound last but-last first daSummedPrices
  ;print word "lowerBound = " lowerBound
  let DemandCurveQ  (daQIntercept - (daSlope * bid))
  ;print word "DemandCurveQ = " DemandCurveQ
  if DemandCurveQ < lowerBound - myEpsilon
    [let DemandCurvePrice ((daQIntercept / daSlope) - (last daLastSummedPrices / daSlope)) ; 2010-7-12 here is where the error is, it bombs
    report (list DemandCurvePrice (last daLastSummedPrices))]
  ; ---------- OK, done with when the demand curve is below.
  ; Now the normal case -->
  ;print "Normal case..."
  let Q (daQIntercept - daSlope * bid)
  ifelse Q <= upperBound
    [report (list bid Q)]
    [report daIntersection(but-first daSummedPrices)(first daSummedPrices)(daQIntercept)(daSlope)]
end

to DetermineFirmProfits-Suppliers
  ; This procedure is called with ask suppliers [DetermineFirmProfits]
  ; See the original CalculateFirmProfit reporter. I'm changing to a procedure
  ; which will be called with ask suppliers. I'll have each supplier record the results in
  ; its own fields, myEpisodeProfit and myEpsisodeProfits (better would be myEpochProfits, both lists),
  ; etc. The summed bids are held by and taken from the market agent, turtle 0 in its BidSum
  ; field.
  ;
  ; Elements of BidSum are: [firmWho  bidPrice startQuantity stopQuantity]
  ; daPrice daQuantity on input are the pre-determined market price and quantity.
  ; Ok, here we go.
  ; Note that what makes this tricky is that we can have more bid steps than plants
  ; for any given bidder. So, determining costs is a little trickier.
  ; First, the agent needs to determine its quantity demanded, given the market quantity, daQuantity.
  ; Second, the agent determines its gross revenue, which is its quantity times daPrice.
  ; Third, the agent determines its costs for the quantity demanded.
  ; Fourth, the agent determines its profits as daGrossRevenue - daCosts.
  ;
  ; First now, determine the quantity required of this supplier,
  ; and put it into the agent's CurrentQuantityDemanded field.
  let daIndex 0
  let daBidSum [BidSum] of turtle 0
  let daPrice [CurrentMarketPrice] of turtle 0
  let daQuantity [CurrentMarketQuantity] of turtle 0
  set CurrentQuantityDemanded 0
  while [daIndex < length daBidSum] [
    let daItem item daIndex daBidSum ; get the next item in the summed bids
    if (first daItem = [who] of self) ; if this is one of mine, process it.
      [let myMaxQuantity last daItem ; item 3
       let myMinQuantity item 2 daItem
       if (daQuantity > myMaxQuantity)
         [set CurrentQuantityDemanded (CurrentQuantityDemanded + (myMaxQuantity - myMinQuantity))]
       if (daQuantity > myMinQuantity and daQuantity <= myMaxQuantity)
         [set CurrentQuantityDemanded (CurrentQuantityDemanded + (daQuantity - myMinQuantity))]
         ]
     set daIndex (daIndex + 1) ; Increment the index variable for the while loop.
  ] ; end of while [daIndex < length daBidSum] [
   ; Second, calculate the agent's gross revenue and store it in the agent's
   ; CurrentGrossRevenue field
   set CurrentGrossRevenue (CurrentQuantityDemanded * daPrice)
   ; Third, determine the agent's costs for supplying the quantity demanded,
   ; and store it in the agent's CurrentCosts field.
   ; Use CostsOfPlants and CapacitiesOfPlants fields. Both are lists.
   let countOfMyPlants length CostsOfPlants
   set CurrentCosts 0
   set daIndex 0
   let daCapacityUsed 0
   while [daIndex < countOfMyPlants] [
     ifelse (CurrentQuantityDemanded > (daCapacityUsed + item daIndex CapacitiesOfPlants))
       [set daCapacityUsed (daCapacityUsed + item daIndex CapacitiesOfPlants)
        set CurrentCosts (CurrentCosts + item daIndex CapacitiesOfPlants * item daIndex CostsOfPlants)
        ;print (word daIndex ". Agent " [who] of self ". In DetermineFirmProfits-Suppliers, 1. CurrentCosts = " CurrentCosts)
        set daIndex (daIndex + 1) ; Increment the index variable for the while loop.
        ]
       [if (CurrentQuantityDemanded <= (daCapacityUsed + item daIndex CapacitiesOfPlants))
         [set CurrentCosts (CurrentCosts + ((CurrentQuantityDemanded - daCapacityUsed) * item daIndex CostsOfPlants))
        ;print (word daIndex ". Agent " [who] of self ". In DetermineFirmProfits-Suppliers, 2. CurrentCosts = " CurrentCosts)
        set daIndex (daIndex + 1 + countOfMyPlants) ; Increment the index variable to end the while loop.
        ]
       ]
   ]
   ; Fourth, calculate net profit.
   set CurrentProfit (CurrentGrossRevenue - CurrentCosts)
end ; end of DetermineFirmProfits-Suppliers


to-report CalculateFirmProfit [daFirm daPrice daQuantity daSummedSupplies daStart]

; new:
; Elements of daSummedSupplies are: [firmWho  bidPrice startQuantity stopQuantity]
  ifelse daSummedSupplies = []
    [report daStart]
    [ifelse first first daSummedSupplies = daFirm
      [let daLowerBound (last but-last first daSummedSupplies)
      ;print word "daLowerBound = " daLowerBound
      ifelse daQuantity < daLowerBound ; this supply is not demanded
       [report daStart]
       [; else we supply the quantity
        let daUpper min (list daQuantity (last first daSummedSupplies))
        ;print word "In CalculateFirm profit. daUpper = "  daUpper
        let daRange (daUpper - last but-last first daSummedSupplies)
        ;print word "daRange = " daRange
        let daNominalProfit daRange * daPrice
        ;print word "daNominalProfit = " daNominalProfit
        let daActualCost (daRange * first but-first first daSummedSupplies)
        ;print word "daActualCost = " daActualCost
       let daNewStart (daStart + daNominalProfit - daActualCost)
       report CalculateFirmProfit(daFirm)(daPrice)(daQuantity)(but-first daSummedSupplies)(daNewStart)
       ] ; end of else in ifelse daQuantity <
      ;] ; end of ifelse daQuantity
     ] ; end of if in ifelse first first daSummedSupplies
     [report CalculateFirmProfit(daFirm)(daPrice)(daQuantity)(but-first daSummedSupplies)(daStart)
      ]  ; end of else in ifelse first first daSummedSupplies
    ] ; end of ifelse first first daSummedSupplies
end

to PlotPrice [daPrice]
  set-current-plot "Prices"
  set-current-plot-pen "price"
  plot-pen-down
  plot daPrice
  set-current-plot-pen "InitialCostDemandInterceptPrice"
  plot-pen-down
  plot InitialCostDemandInterceptPrice
end ; of PlotPrice

to PlotSummedFirmAndDemandCurves [daQIntercept daDSlope daSummed]
  set-current-plot "Cost & Demand Curves"
  set-current-plot-pen "Demand Curve"
  ;set-plot-pen-color red
  plot-pen-up
  plotxy daQIntercept 0
  plot-pen-down
  plotxy 0  (daQIntercept / daDSlope) ; plot to the price intercept
  ;print word "(daQIntercept / daDSlope)=" (daQIntercept / daDSlope)
  PlotSummedSupplyCurve("Cost & Demand Curves")(daSummed)(0)
end ; of PlotInitialDemandCurve

to PlotDemandCurve [daPlot daQIntercept daDSlope]
  set-current-plot daPlot ; "Supply & Demand Curves"
  set-current-plot-pen "Demand Curve"
  ;set-plot-pen-color red
  plot-pen-up
  plotxy daQIntercept 0
  plot-pen-down
  plotxy 0  (daQIntercept / daDSlope) ; plot to the price intercept
end ;

to PlotSupplyAndDemandCurves [daQIntercept daDSlope daBidCurve]
  ;print word "In PlotSupplyAndDemandCurves " daSummedPrices
  set-current-plot "Supply & Demand Curves"
  clear-plot
  set-current-plot-pen "Demand Curve"
  ;set-plot-pen-color red
  plot-pen-up
  plotxy daQIntercept 0
  plot-pen-down
  plotxy 0  (daQIntercept / daDSlope) ; plot to the price intercept
  ;print word "(daQIntercept / daDSlope)=" (daQIntercept / daDSlope)
  ;PlotExtSupplyCurve("Supply & Demand Curves")("Supply Curve")(daSummedPrices)(0)
  ;PlotExtSupplyCurve("Supply & Demand Curves")(daBidCurve)(0)
  PlotSupplyCurve(daBidCurve)
end ;

to PlotSupplyCurve [daBidCurve]
  set-current-plot  "Supply & Demand Curves"
  ;print (word "In PlotSupplyCurve. daBidCurve = " daBidCurve)
  let daNumberOfSteps length daBidCurve
  let daIndices n-values daNumberOfSteps [ ?1 -> ?1 ]
  foreach daIndices [ ?1 ->
    let daItem item ?1 daBidCurve ; [supplierID price startQuantity finishQuantity]
    let daGuy first daItem
    let daPrice item 1 daItem
    let daStartQuantity item 2 daItem
    let daFinishQuantity item 3 daItem
    set-current-plot-pen (word "Supply, Firm " daGuy)
    plot-pen-up
    plotxy  daStartQuantity daPrice
    plot-pen-down
    plotxy  daFinishQuantity daPrice
  ]

end

to PlotExtSupplyCurve [daPlot  daSummedPrices daStart]
  set-current-plot daPlot ; "Supply & Demand Curves"
  ;set-current-plot-pen daPen ;"Supply Curve"
  plot-pen-up
  if daSummedPrices != []
  [ifelse (first first daSummedPrices = "firm0")
   [set-current-plot-pen "Supply, Firm 0"]
   [set-current-plot-pen "Supply, Firm 1"]
   plot-pen-up
  let price item 2 first daSummedPrices
   let upperBound last first daSummedPrices
   plotxy daStart price
   plot-pen-down
   plotxy upperBound price
   let daNewStart upperBound
   ;PlotExtSupplyCurve(daPlot)(daPen)(but-first daSummedPrices)(daNewStart)
   PlotExtSupplyCurve(daPlot)(but-first daSummedPrices)(daNewStart)
  ]
end

to PlotSummedSupplyCurve [daPlot daSummed daStart]
  set-current-plot daPlot ; "Supply & Demand Curves"
  ;set-current-plot-pen daPen ;"Supply Curve"
  ;plot-pen-up
  if daSummed != []
  [ifelse first first daSummed = "firm1"
    [set-current-plot-pen "Cost Curve, Firm1"]
    [set-current-plot-pen "Cost Curve, Firm2"]
  let price first but-first first daSummed
   let upperBound last first daSummed
   plotxy daStart price
   plot-pen-down
   plotxy upperBound price
   let daNewStart upperBound
   PlotSummedSupplyCurve(daPlot)(but-first daSummed)(daNewStart)

  ]
end

to PlotUniqueSummedSupplyCurve [daPlot daSummed]
  set-current-plot daPlot ; "Supply & Demand Curves"
  let daFirm -1
  let daIndexes n-values length daSummed [ ?1 -> ?1 ]
  let daPrice 0
  let daItem []
  let daStart 0
  let daFinish 0
foreach daIndexes [ ?1 ->
  set daItem item ?1 daSummed
  set daFirm item 0 daItem
  if (daFirm <= 6) [
  set-current-plot-pen (word "Cost Curve, Firm " daFirm)
  set daPrice item 1 daItem
  set daStart item 2 daItem
  set daFinish item 3 daItem
  plot-pen-up
  plotxy daStart daPrice
  plot-pen-down
  plotxy daFinish daPrice
  ]
 ]
end

to PlotCostAndDemandCurves [daQIntercept daDSlope daFirm1Costs daFirm2Costs daSummedBids]
  let daPlot "Cost & Demand Curves"
  set-current-plot "Cost & Demand Curves"
  set-current-plot-pen "Demand Curve"
  plot-pen-up
  plotxy daQIntercept 0
  plot-pen-down
  plotxy 0  (daQIntercept / daDSlope) ; plot to the price intercept
  ;print word "(daQIntercept / daDSlope)=" (daQIntercept / daDSlope)
;  PlotSupplyCurve(daPlot)("Cost Curve, Firm 1")(daFirm1Costs)(0)
;  PlotSupplyCurve(daPlot)("Cost Curve, Firm 2")(daFirm2Costs)(0)
  PlotSupplyCurve(daSummedBids)
end

to SumTheFirms ;[firm0 firm1 sumList start]
  ; To be done by the market agent, turtle 0
  ; OK, the new version is all different.
  ; Now the bidvectors of the suppliers look like this:
;  observer> show [bidvector] of supplier 1
;observer: [[1 8 8] [1 8 67] [1 10 20]]
;observer> show [bidvector] of supplier 2
;observer: [[2 8 8] [2 10 67] [2 12 20]]
;The pattern is [who bid/cost quantity] in increasing order of cost, item 1
; To be returned is a list of triples in increasing order of cost, item 1
; So, first we get a list of all the triples:
  let daTriples []
  ask suppliers [
    let daIndexes n-values length [BidVector] of self [ ?1 -> ?1 ] ; daIndexes now = [0 1 2 ...]
    foreach daIndexes [ ?1 ->
      set daTriples fput item ?1 [BidVector] of self daTriples
    ]
  ] ; end of ask suppliers
; Now we sort on item 1 of the constituent lists:
let daSumming sort-by [ [?1 ?2] -> item 1  ?1 < item 1  ?2 ] daTriples
; Now we have to replace the last item in each case with two items, the start
; and the end of the quantity. The start is the end of the previous.
; The quantity is the start plus the old last.
let daIndexes n-values length daSumming [ ?1 -> ?1 ]
let daItem []
let daNewSumming []
let daPrevious 0
let daLength 0
foreach daIndexes [ ?1 ->
  set daItem item ?1 daSumming
  set daLength last daItem
  set daItem remove-item (length daItem - 1) daItem
  set daItem lput daPrevious daItem
  set daItem lput (daPrevious + daLength) daItem
  set daPrevious (daPrevious + daLength)
  set daNewSumming lput daItem daNewSumming
 ]
;print (word "In SumTheFirms, daNewSumming = " daNewSumming)
;report daNewSumming
set BidSum daNewSumming
; Now determine and record the market price and quantity demanded:
  let PriceQuantityIntersection daIntersection(BidSum)([])(QIntercept)(DSlope)
  ;print word "PriceQuantityIntersection = " PriceQuantityIntersection
  ;set InitialCostDemandInterceptPrice first PriceQuantityIntersection
  ;set InitialCostDemandInterceptQuantity last PriceQuantityIntersection
  let daQuantity InitialCostDemandInterceptQuantity
  ;print (word "daQuantity = " daQuantity)
  ;print (word "daCostSum = " daCostSum)
  set CurrentMarketPrice  first PriceQuantityIntersection
  set CurrentMarketQuantity last PriceQuantityIntersection
end

to PlotProfits
  set-current-plot "Profits"
  set-current-plot-pen "Industry (Total) Profit"
  let totalIndustryProfit 0
  ask suppliers [set totalIndustryProfit (totalIndustryProfit + myEpisodeProfit)]
  plot totalIndustryProfit
  ;set-current-plot-pen "Firm 0 Profit"
  ; Now plot the profits of the firms, up to and including supplier 6
  let daFirm -1
  let daProfit -1
  let daIndexes n-values count suppliers [ ?1 -> ?1 ]
  let supplierID -1
  foreach daIndexes [ ?1 ->
    set supplierID (?1 + 1)
    if (supplierID <= 6) [ ; suppliers 1-6 only
      set daFirm (word "Firm " supplierID " Profit")
      ;print (word "In PlotProfits, daFirm = " daFirm)
      set-current-plot-pen daFirm
      set daProfit [myEpisodeProfit] of supplier supplierID
      ;print (word "In PlotProfits, daProfit = " daProfit)
      plot daProfit
    ]
  ]
end


to-report MyDivision [numerator denominator]
  ifelse (denominator = 0)
    [report 0]
    [report numerator / denominator]
end

to-report MyMean [aList]
  ifelse (not empty? aList)
   [report mean aList]
   [report 0]
end


to SetupAgents

if (InstantiateScenarios = "NFirmsSimple")
  [create-suppliers NumberOfSuppliers
   ask suppliers [
     set CostsOfPlants (list 0 0)
     set CapacitiesOfPlants (list 15 55)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks (list 0.2 0.4 0.6 0.7 0.8) ; (list 0.5)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list 2 2 2 2 2 2)
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who ". So I'm quitting.")
         let bob 0
         set bob (5 / bob)]

     set PriceDeltas (list 3.0 3.0 3.0 3.0 3.0 3.0)
     set PriceEpsilons (list 1.0 1.0 1.0 1.0 1.0 1.0)
     set QuantityDeltas (list 0.04 0.04 0.04 0.04 0.04)
     set QuantityEpsilons (list 0.01 0.01 0.01 0.01 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength 100
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType  UpdateTypeFirm1 ; "Market Returns" ; "Own Returns"
   ] ; end of ask supplier 1
  ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if (InstantiateScenarios = "Monopolist")
  [create-suppliers 1
   ask supplier 1 [
     set CostsOfPlants (list 0 0)
     set CapacitiesOfPlants (list 210 210)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks (list 0.5)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list 10 15)
     ;;;;;;;;; Do a validity check. ;;;;;;;;;;;;;;
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who ". So I'm quitting.")
         let bob 0
         set bob (5 / bob)]

     set PriceDeltas (list 3.0 3.0)
     set PriceEpsilons (list 1.0 1.0)
     set QuantityDeltas (list 0.04)
     set QuantityEpsilons (list 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength EpochLengthFirm1
     ;;;;;;;;; Do a validity check. ;;;;;;;;;;;;;;
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType  UpdateTypeFirm1
   ] ; end of ask supplier 1
  ] ; if (InstantiateScenarios = "Monopolist")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if (InstantiateScenarios = "One Firm, Zero Costs")
  [create-suppliers 1
   ask supplier 1 [
     set CostsOfPlants (list 0 0)
     set CapacitiesOfPlants (list 55 55)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks (list 0.5)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list 10 10)
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who ". So I'm quitting.")
         let bob 0
         set bob (5 / bob)]

     set PriceDeltas (list 3.0 3.0)
     set PriceEpsilons (list 1.0 1.0)
     set QuantityDeltas (list 0.04)
     set QuantityEpsilons (list 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength EpochLengthFirm1 ; 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType UpdateTypeFirm1 ; "Market Returns Constrained by Own Returns" ; "Own Returns" ; "Own Returns"
     ;print (word "End of create supplier 1 in 'Two Firms, Zero Costs'. BasePrices = " BasePrices)
   ] ; end of ask supplier 1
  ]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if (InstantiateScenarios = "Two Firms, Zero Costs")
  [create-suppliers 2
   ask supplier 1 [
     set CostsOfPlants (list 0 0)
     set CapacitiesOfPlants (list 55 55)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks (list 0.5)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list 10 12)
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who ". So I'm quitting.")
         let bob 0
         set bob (5 / bob)]

     set PriceDeltas (list 3.0 3.0)
     set PriceEpsilons (list 1.0 1.0)
     set QuantityDeltas (list 0.04)
     set QuantityEpsilons (list 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength EpochLengthFirm1 ; 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType UpdateTypeFirm1 ; "Market Returns Constrained by Own Returns" ; "Own Returns" ; "Own Returns"
     ;print (word "End of create supplier 1 in 'Two Firms, Zero Costs'. BasePrices = " BasePrices)
   ] ; end of ask supplier 1
    ask supplier 2 [
     set CostsOfPlants (list 0 0)
     set CapacitiesOfPlants (list 55 55)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks (list 0.5)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list 10 10)
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who ". So I'm quitting.")
         let bob 0
         set bob (5 / bob)]

     set PriceDeltas (list 3.0 0.0)
     set PriceEpsilons (list 1.0 0.0)
     set QuantityDeltas (list 0.04)
     set QuantityEpsilons (list 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength EpochLengthFirm2 ; 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType UpdateTypeFirm2 ; "Market Returns Constrained by Own Returns" ; "Own Returns" ; "Own Returns"
   ] ; end of ask supplier 2

  ]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if (InstantiateScenarios = "From the Interface" and NumberOfSuppliers > 2)
        [print (word "Quitting because the NumberOfSuppliers > 2.")
         let bob 0
         set bob (5 / bob)]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if (InstantiateScenarios = "From the Interface" and numberOfSuppliers = 1)
  [create-suppliers 1
   ask supplier 1 [
     set CostsOfPlants (list Cost1Firm1 Cost2Firm1)
     set CapacitiesOfPlants (list MaxQ1Firm1 MaxQ2Firm1)
     set TotalCapacity sum CapacitiesOfPlants
;     let daRemainder (TotalCapacity - InitialBaseQuantity1Firm1)
;     if (daRemainder < 0)
;        [print (word "Quitting because CapacitiesOfPlants - InitialBaseQuantity1Firm1 < 0")
;         let bob 0
;         set bob (5 / bob)]
     set BaseQuantitiesBreaks (list QuantityBreak1Firm1 QuantityBreak2Firm1)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list InitialBasePrice1Firm1 InitialBasePrice2Firm1 InitialBasePrice3Firm1)
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who ". So I'm quitting.")
         let bob 0
         set bob (5 / bob)]
     set PriceDeltas (list priceDeltaFirm1 priceDeltaFirm1 priceDeltaFirm1)
     set PriceEpsilons (list priceEpsilonFirm1 priceEpsilonFirm1 priceEpsilonFirm1)
     set QuantityDeltas (list quantityDeltaFirm1 quantityDeltaFirm1)
     set QuantityEpsilons (list quantityEpsilonFirm1 quantityEpsilonFirm1)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength epochLengthFirm1
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType UpdateTypeFirm1
   ] ; end of ask supplier 1
  ]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if (InstantiateScenarios = "From the Interface" and numberOfSuppliers = 2)
  [ ; print (word "Welcome to InstantiateScenarios = 'From the Interface' and numberOfSuppliers = 2")
   create-suppliers 2
   ask supplier 1 [
     set CostsOfPlants (list Cost1Firm1 Cost2Firm1)
     set CapacitiesOfPlants (list MaxQ1Firm1 MaxQ2Firm1)
     set TotalCapacity sum CapacitiesOfPlants
;     let daRemainder (TotalCapacity - InitialBaseQuantity1Firm1)
;     if (daRemainder < 0)
;        [print (word "Quitting because CapacitiesOfPlants - InitialBaseQuantity1Firm1 < 0")
;         let bob 0
;         set bob (5 / bob)]
     set BaseQuantitiesBreaks (list QuantityBreak1Firm1 QuantityBreak2Firm1)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list InitialBasePrice1Firm1 InitialBasePrice2Firm1 InitialBasePrice3Firm1)
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who)]
     set PriceDeltas (list priceDeltaFirm1 priceDeltaFirm1 priceDeltaFirm1)
     set PriceEpsilons (list priceEpsilonFirm1 priceEpsilonFirm1 priceEpsilonFirm1)
     set QuantityDeltas (list quantityDeltaFirm1 quantityDeltaFirm1)
     set QuantityEpsilons (list quantityEpsilonFirm1 quantityEpsilonFirm1)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength epochLengthFirm1
       if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]

     set UpdateType UpdateTypeFirm1
   ] ; end of ask supplier 1
  ask supplier 2 [
     set CostsOfPlants (list Cost1Firm2 Cost2Firm2)
     set CapacitiesOfPlants (list MaxQ1Firm2 MaxQ2Firm2)
     set TotalCapacity sum CapacitiesOfPlants
;     let daRemainder (TotalCapacity - InitialBaseQuantity1Firm2)
;     if (daRemainder < 0)
;        [print (word "Quitting because CapacitiesOfPlants - InitialBaseQuantity1Firm1 < 0")
;         let bob 0
;         set bob (5 / bob)]
     set BaseQuantitiesBreaks (list QuantityBreak1Firm2 QuantityBreak2Firm2)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list InitialBasePrice1Firm2 InitialBasePrice2Firm2 InitialBasePrice3Firm2)
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who)]
     set PriceDeltas (list priceDeltaFirm2 priceDeltaFirm2 priceDeltaFirm2)
     set PriceEpsilons (list priceEpsilonFirm2 priceEpsilonFirm2 priceEpsilonFirm2)
     set QuantityDeltas (list quantityDeltaFirm2 quantityDeltaFirm2)
     set QuantityEpsilons (list quantityEpsilonFirm2 quantityEpsilonFirm2)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength epochLengthFirm2
  if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         print (word "BasePrices=" BasePrices " BaseQuantities=" BaseQuantities " CostsOfPlants= " CostsOfPlants " CapacitiesOfPlants=" CapacitiesOfPlants)
         let bob 0
         set bob (5 / bob)]

      set UpdateType UpdateTypeFirm2
   ] ; end of ask supplier 2
  ; print (word "End of InstantiateScenarios = 'From the Interface' and numberOfSuppliers = 2")
  ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if (InstantiateScenarios = "TwoFirmsSimple")
  [    ; Need to add lots of error checking somewhere. 2010-7-16: Some of this is done.
   create-suppliers 2
   ask supplier 1 [
     set CostsOfPlants (list 3 7)
     set CapacitiesOfPlants (list 10 85)
     set TotalCapacity sum CapacitiesOfPlants
; 2010-8-31, see the notes in the Information tab.
; I'm replacing this:
;     set BaseQuantities (list 8 67 20) ;
;     ; Above, this says that the agent is using 3 bid segments
;     let daRemainder (TotalCapacity - (sum BaseQuantities))
;     if (daRemainder < 0)
;        [print (word "Quitting because CapacitiesOfPlants - (sum BaseQuantities) < 0 for supplier " who)
;         let bob 0
;         set bob (5 / bob)]
; with this:
     set BaseQuantitiesBreaks (list (8.0 / TotalCapacity)  ((8.0 + 67.0) / TotalCapacity)) ; Could use some validity checking.
     ; Now set BaseQuantities from the breaks
     SetBaseQuantities
; Explanation: If there are to be N levels (here 3) then we need (N-1) breakpoints.
; I'll give the breakpoints as cumulative portions of the Total Capacity.
; Thus, e.g., (list 1.0) means one price. (list 0.9) means two prices, 90% of capacity, 10%. etc.
     ; Do some validity checking:
;     if (max BaseQuantities > 1 or min BaseQuantities <= 0)
;        [print (word "Quitting because BaseQuantities is invalid for turtle " [who] of self ". " BaseQuantities)
;         let bob 0
;         set bob (5 / bob)]
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     set BasePrices (list 8   9 10)
     ; Above, this says that the agent is using 3 bid segments
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who)]
     set PriceDeltas (list 1.0 1.0 1.0)
     set PriceEpsilons (list 0.1 0.1 0.1)
;     set QuantityDeltas (list 1.0 1.0 1.0)
;     set QuantityEpsilons (list 0.1 0.1 0.1)
     set QuantityDeltas (list 0.04 0.04)
     set QuantityEpsilons (list 0.01 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength EpochLengthFirm1 ; 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType  UpdateTypeFirm1 ; "Own Returns" ; "Market Returns Constrained by Own Returns" ; "Own Returns"
   ]
   ask supplier 2 [set CostsOfPlants (list 4 9)
     set CapacitiesOfPlants (list 8 87)
     set TotalCapacity sum CapacitiesOfPlants
;     set BaseQuantities (list 8 67 20) ;
;     ; Above, this says that the agent is using 3 bid segments
;     let daRemainder (TotalCapacity - (sum BaseQuantities))
;     if (daRemainder < 0)
;        [print (word "Quitting because CapacitiesOfPlants - (sum BaseQuantities) < 0 for supplier " who)
;         let bob 0
;         set bob (5 / bob)]
     set BaseQuantitiesBreaks (list (8.0 / TotalCapacity)  ((8.0 + 67.0) / TotalCapacity)) ; Could use some validity checking.
     ; Now set BaseQuantities from the breaks
     SetBaseQuantities
; Explanation: If there are to be N levels (here 3) then we need (N-1) breakpoints.
; I'll give the breakpoints as cumulative portions of the Total Capacity.
; Thus, e.g., (list 1.0) means one price. (list 0.9) means two prices, 90% of capacity, 10%. etc.
     ; Do some validity checking:
;     if (max BaseQuantities > 1 or min BaseQuantities <= 0)
;        [print (word "Quitting because BaseQuantities is invalid for turtle " [who] of self ". " BaseQuantities)
;         let bob 0
;         set bob (5 / bob)]
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     set BasePrices (list 7 10 12)
     ; Above, this says that the agent is using 3 bid segments
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who)]
     set PriceDeltas (list 1.0 1.0 1.0)
     set PriceEpsilons (list 0.1 0.1 0.1)
     set QuantityDeltas (list 0.04 0.04)
     set QuantityEpsilons (list 0.01 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength EpochLengthFirm2 ; 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType UpdateTypeFirm2 ;  "Own Returns" ;  "Market Returns Constrained by Own Returns" ; "Own Returns"
   ]

  ] ; end of if (InstantiateScenarios = "TwoFirmsSimple")

if (InstantiateScenarios = "Three Firms, Simple")
  [create-suppliers 3
   ask supplier 1 [
     set CostsOfPlants (list 3 7)
     set CapacitiesOfPlants (list 10 85)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks  (list (8.0 / TotalCapacity) ((8.0 + 67.0) / TotalCapacity)) ;
     SetBaseQuantities
     ; Above, this says that the agent is using 3 bid segments
     set BasePrices (list 8   9 10)
     ; Above, this says that the agent is using 3 bid segments
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who)
         print (word "Quitting.")
         let bob TotalCapacity / 0]
     set PriceDeltas (list 1.0 1.0 1.0)
     set PriceEpsilons (list 0.1 0.1 0.1)
     set QuantityDeltas (list 0.04 0.04)
     set QuantityEpsilons (list 0.01 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType  "Own Returns" ;
   ]
   ask supplier 2 [
     set CostsOfPlants (list 4 9)
     set CapacitiesOfPlants (list 8 87)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks  (list (8.0 / TotalCapacity) ((8.0 + 67.0) / TotalCapacity)) ;
     SetBaseQuantities
     ; Above, this says that the agent is using 3 bid segments
     set BasePrices (list 7 10 12)
     ; Above, this says that the agent is using 3 bid segments
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who)
         print (word "Quitting.")
         let bob 0
         set bob (5 / bob)]
     set PriceDeltas (list 1.0 1.0 1.0)
     set PriceEpsilons (list 0.1 0.1 0.1)
     set QuantityDeltas (list 0.04 0.04)
     set QuantityEpsilons (list 0.01 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType  "Own Returns" ;
   ]
   ask supplier 3 [set CostsOfPlants (list 4 9)
     set CapacitiesOfPlants (list 8 87)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks  (list (8.0 / TotalCapacity) ((8.0 + 67.0) / TotalCapacity)) ;
     SetBaseQuantities
     ; Above, this says that the agent is using 3 bid segments
     set BasePrices (list 7 10 12)
     ; Above, this says that the agent is using 3 bid segments
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who)
         print (word "Quitting.")
         let bob 0
         set bob (5 / bob)]
     set PriceDeltas (list 1.0 1.0 1.0)
     set PriceEpsilons (list 0.1 0.1 0.1)
     set QuantityDeltas (list 0.04 0.04)
     set QuantityEpsilons (list 0.01 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType  "Own Returns" ;
   ]

  ] ; end of if InstantiateScenarios = "Three Firms, Simple"
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if (InstantiateScenarios = "Q2Firm1Is250 6 steps")
  [create-suppliers 2
   ask supplier 1 [
     set CostsOfPlants (list 4 15)
     set CapacitiesOfPlants (list 20 250)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks (list 0.2 0.4 0.6 0.7 0.8)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list 16 16 16 16 16 16)
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who ". So I'm quitting.")
         let bob 0
         set bob (5 / bob)]

     set PriceDeltas (list 3.0 3.0 3.0 3.0 3.0 3.0)
     set PriceEpsilons (list 1.0 1.0 1.0 1.0 1.0 1.0)
     set QuantityDeltas (list 0.04 0.04 0.04 0.04 0.04)
     set QuantityEpsilons (list 0.01 0.01 0.01 0.01 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength EpochLengthFirm1 ; 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType UpdateTypeFirm1 ; "Market Returns Constrained by Own Returns" ; "Own Returns" ; "Own Returns"
     ;print (word "End of create supplier 1 in 'Two Firms, Zero Costs'. BasePrices = " BasePrices)
   ] ; end of ask supplier 1
    ask supplier 2 [
     set CostsOfPlants (list 4 15)
     set CapacitiesOfPlants (list 20 100)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks (list 0.2 0.4 0.6 0.7 0.8)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list 100 100 100 100 100 100) ;(list 16 16 16 16 16 16)
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who ". So I'm quitting.")
         let bob 0
         set bob (5 / bob)]

     set PriceDeltas (list 3.0 3.0 3.0 3.0 3.0 3.0)
     set PriceEpsilons (list 1.0 1.0 1.0 1.0 1.0 1.0)
     set QuantityDeltas (list 0.04 0.04 0.04 0.04 0.04)
     set QuantityEpsilons (list 0.01 0.01 0.01 0.01 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength EpochLengthFirm2 ; 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType UpdateTypeFirm2 ; "Market Returns Constrained by Own Returns" ; "Own Returns" ; "Own Returns"
   ] ; end of ask supplier 2

  ]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if (InstantiateScenarios = "P2Both2Is25 6 steps")
  [create-suppliers 2
   ask supplier 1 [
     set CostsOfPlants (list 4 15)
     set CapacitiesOfPlants (list 20 100)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks (list 0.2 0.4 0.6 0.7 0.8)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list 100 100 100 100 100 100) ;(list 16 16 16 16 16 16)
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who ". So I'm quitting.")
         let bob 0
         set bob (5 / bob)]

     set PriceDeltas (list 3.0 3.0 3.0 3.0 3.0 3.0)
     set PriceEpsilons (list 1.0 1.0 1.0 1.0 1.0 1.0)
     set QuantityDeltas (list 0.04 0.04 0.04 0.04 0.04)
     set QuantityEpsilons (list 0.01 0.01 0.01 0.01 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength EpochLengthFirm1 ; 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType UpdateTypeFirm1 ; "Market Returns Constrained by Own Returns" ; "Own Returns" ; "Own Returns"
     ;print (word "End of create supplier 1 in 'Two Firms, Zero Costs'. BasePrices = " BasePrices)
   ] ; end of ask supplier 1
    ask supplier 2 [
     set CostsOfPlants (list 4 15)
     set CapacitiesOfPlants (list 20 100)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks (list 0.2 0.4 0.6 0.7 0.8)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list 100 100 100 100 100 100) ;(list 16 16 16 16 16 16)
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who ". So I'm quitting.")
         let bob 0
         set bob (5 / bob)]

     set PriceDeltas (list 3.0 3.0 3.0 3.0 3.0 3.0)
     set PriceEpsilons (list 1.0 1.0 1.0 1.0 1.0 1.0)
     set QuantityDeltas (list 0.04 0.04 0.04 0.04 0.04)
     set QuantityEpsilons (list 0.01 0.01 0.01 0.01 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength EpochLengthFirm2 ; 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType UpdateTypeFirm2 ; "Market Returns Constrained by Own Returns" ; "Own Returns" ; "Own Returns"
   ] ; end of ask supplier 2

  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if (InstantiateScenarios = "Bertrand250Both 6 steps")
  [create-suppliers 2
   ask supplier 1 [
     set CostsOfPlants (list 4 15)
     set CapacitiesOfPlants (list 20 250)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks (list 0.2 0.4 0.6 0.7 0.8)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list 16 16 16 16 16 16)
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who ". So I'm quitting.")
         let bob 0
         set bob (5 / bob)]

     set PriceDeltas (list 3.0 3.0 3.0 3.0 3.0 3.0)
     set PriceEpsilons (list 1.0 1.0 1.0 1.0 1.0 1.0)
     set QuantityDeltas (list 0.04 0.04 0.04 0.04 0.04)
     set QuantityEpsilons (list 0.01 0.01 0.01 0.01 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength EpochLengthFirm1 ; 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType UpdateTypeFirm1 ; "Market Returns Constrained by Own Returns" ; "Own Returns" ; "Own Returns"
     ;print (word "End of create supplier 1 in 'Two Firms, Zero Costs'. BasePrices = " BasePrices)
   ] ; end of ask supplier 1
    ask supplier 2 [
     set CostsOfPlants (list 4 15)
     set CapacitiesOfPlants (list 20 250)
     set TotalCapacity sum CapacitiesOfPlants
     set BaseQuantitiesBreaks (list 0.2 0.4 0.6 0.7 0.8)
     SetBaseQuantities
     ;;;;;;;;;;;; Now Prices ;;;;;;;;
     set BasePrices (list 16 16 16 16 16 16)
     if (length BaseQuantities != length BasePrices)
        [print (word "Whoa! In SetupAgents. (length BaseQuantities != length BasePrices) Turtle " who ". So I'm quitting.")
         let bob 0
         set bob (5 / bob)]

     set PriceDeltas (list 3.0 3.0 3.0 3.0 3.0 3.0)
     set PriceEpsilons (list 1.0 1.0 1.0 1.0 1.0 1.0)
     set QuantityDeltas (list 0.04 0.04 0.04 0.04 0.04)
     set QuantityEpsilons (list 0.01 0.01 0.01 0.01 0.01)
     set CurrentBidQuantities []
     set CurrentBidPrices []
     set EpochLength EpochLengthFirm2 ; 50
     if (not IsFeasibleBid?(BasePrices)(BaseQuantities)(CostsOfPlants)(CapacitiesOfPlants))
        [print (word "Quitting because BasePrices and Quantities is not feasible for supplier" [who] of self)
         let bob 0
         set bob (5 / bob)]
     set UpdateType UpdateTypeFirm2 ; "Market Returns Constrained by Own Returns" ; "Own Returns" ; "Own Returns"
   ] ; end of ask supplier 2

  ]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; General Initializations for All Suppliers ;;;;;;;;;;;;;;
  ask suppliers [
                 set episodeCount 0
               ; Now we initialize as empty a number of lists maintained by the
               ; firm during an epoch. These are mandated by Probe and Adjust for
               ; various policies under consideration. For example, episodeOwnProfits1Up
               ; stores the firm's own profits for an episode in the event that the firm's
               ; price bid for its resource 1 was HIGHER than its current base price.
               ; episodeOwnProfits1Down holds the profit when the bid was LOWER than its
               ; current base price. episodeMarketProfits1Up stores the total market profits
               ; for an episode in the event that the firm's
               ; price bid for its resource 1 was HIGHER than its current base price.
               ; And so on.
              ; set episodeOwnProfits1Up []
              ; set episodeOwnProfits1Down []
             ;  set episodeOwnQuantity1Up []
              ; set episodeOwnQuantity1down []
           ;    set episodeOwnProfits2Up []
           ;    set episodeOwnProfits2Down []
           ;    set episodeOwnProfitsShadeUp []
            ;   set episodeOwnProfitsShadeDown []
           ;    set episodeMarketProfits1Up []
            ;   set episodeMarketProfits1Down []
          ;     set episodeMarketProfits2Up []
          ;     set episodeMarketProfits2Down []
           ;    set episodeMarketProfitsShadeUp []
           ;    set episodeMarketProfitsShadeDown []
         ;      set episodeMarketQuantity1Up []
          ;     set episodeMarketQuantity1Down []
               ; Initializes as stored attributes various quantities of use to the firm
;               set BaseQ2Shade runresult word "InitialBaseQ2ShadeFirm" who
;               set Q2Shade BaseQ2Shade
;               set bidderType runresult word "bidderTypeFirm" who
;               set updateType runresult word "updateTypeFirm" who
               set myEpisodeProfit 0
               set industryEpisodeProfit 0
               set myEpisodeProfits []
               set industryEpisodeProfits []
          ;     set industryShadeList []
               ;set myShadeList (list Q2Shade)
            ;   set myBid2s []
             ;  set industryBid2s []
               set runningProfits []

               let performanceItem (list 0 0 0)
               set EpochCostPerformance []
               set EpochCostCountPerformance []
               set EpochQuantityPerformance []
               set EpochQuantityCountPerformance []
               set EpochCostPerformanceMarket []
               set EpochQuantityPerformanceMarket []
               set RunningProfits []
               foreach BasePrices [
                 set EpochCostPerformance lput performanceItem EpochCostPerformance
                 set EpochCostCountPerformance lput performanceItem EpochCostCountPerformance
                 set EpochQuantityPerformance lput performanceItem EpochQuantityPerformance
                 set EpochQuantityCountPerformance lput performanceItem EpochQuantityCountPerformance
                 set EpochCostPerformanceMarket lput performanceItem EpochCostPerformanceMarket
                 set EpochQuantityPerformanceMarket lput performanceItem EpochQuantityPerformanceMarket
               ]
               ] ; end of turtle/firm initialization
  ; print (word "Final End of SetupAgents.")
end ; of SetupAgents

to MakeANewBid-Suppliers
  ;print (word "Entering MakeANewBid-Suppliers. turtle = " [who] of self " BasePrices=" BasePrices " BaseQuantities= " BaseQuantities " BaseQuantitiesBreaks= " BaseQuantitiesBreaks)
  ; The object here is to probe on each of the base prices and base quantities,
  ; modify as necesssary to maintain feasibility, and set a feasible bid.
  ; Do we also need to record Hi, Lo, No Change?
  ;print (word "Entering MakeANewBid. BaseQuantities = " BaseQuantities)
  ; OK, the procedure is this.
  ; We proceed "left to right", perturbing both price and quantity,
  ; modifying ex post if infeasible.
;  A relevant example of declarations in SetupAgents:
;     set TotalCapacity sum CapacitiesOfPlants
;     set BaseQuantities (list 8 67 20)
;     set BasePrices (list 8   9 10)
;     set PriceDeltas (list 1.0 1.0 1.0)
;     set QuantityDeltas (list 0.0 1.0 1.0)
;     set CurrentBidQuantities []
;     set CurrentBidPrices []
;  Each supplier has a TotalCapacity field, set at initialization.
;  We need to set CurrentBidPrices and CurrentBidQuantities by probing on
;  BasePrices and BaseQuantities, and fixing, if need be, to ensure
;  feasibility, sensibility.

; 2010-9-1: Am changing how this is done for quantities.
; Now what drives quantities is BaseQuantitiesBreaks, a cumulative with N-1 terms.
; I'll probe with that, with the N-1 terms, then convert to the N quantities proper.
; The trick is with any adjustments for validity purposes.
  let pricePrev 0  ; previous price in the step function.
;  let quantityPrev 0
  let daBidPrices [] ; These are the current working versions, which
  let daBidQuantities [] ; are being built.
  let indexSet n-values (length BasePrices - 1) [ ?1 -> ?1 ] ; the last step is treated differently
  ; For example, if there are 3 BasePrices, then indexSet becomes {0 1}
  let daPriceDelta 0
  let daQuantityDelta 0
  let daNewPrice 0
  let daNewQuantityBreak 0
  let daNewQuantityBreaks []
  ; why don't we have daNewQuantity? Because we don't have to check it initially
  ; for feasibility. Quantities are not cumulative.
;  let daQuantity 0
  let feasibleFound? False
 while [not feasibleFound?] [
  set pricePrev 0  ; previous price in the step function.
  set daBidPrices [] ; These are the current working versions, which
  set daBidQuantities [] ; are being built.
  set indexSet n-values (length BasePrices - 1) [ ?1 -> ?1 ] ; the last step is treated differently
  ; For example, if there are 3 BasePrices, then indexSet becomes {0 1}
  set daPriceDelta 0
  set daQuantityDelta 0
  set daNewPrice 0
  set daNewQuantityBreak 0
  set daNewQuantityBreaks []

  foreach indexSet [ ?1 ->
    ; Draw a random number and set the amount of change to be tried for the price,
    ; i.e., daPriceDelta.
    set daPriceDelta ((random-float 1 * item ?1 PriceDeltas * 2) - item ?1 PriceDeltas)
    ; Tentatively set the new price, daNewPrice, equal to daPriceDelta plus the base price.
    set daNewPrice (daPriceDelta + item ?1 BasePrices)
    ; Adjust the new price, if needed, to make sure it is epsilon above costs, if
    ; it is the first price, or else epsilon above the price bid for the previous step.
    ; Note that this does not guarantee feasibility.
    ifelse (?1 = 0) ; If this is the first step, 0 in the indexSet
     [set daNewPrice (max (list (item ?1 CostsOfPlants + daEpsilon) daNewPrice) )
      ; daNewPrice is the current value of daNewPrice or the costs of plant 0, whichever is higher.
     ]
     [set pricePrev item (?1 - 1) daBidPrices ; If this is the second or later step, 1 or high in the indexSet
       ;print (word "The previous price is " pricePrev " and should be " item 0 daBidPrices " and daPriceDelta = " daPriceDelta)
       set daNewPrice max (list daNewPrice (pricePrev + daEpsilon))
       ; daNewPrice is the current value of daNewPrice or the costs of plant (? - 1), whichever is higher.
     ]
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Now set the quantity bid at this step.
    set daQuantityDelta ((random-float 1 * item ?1 QuantityDeltas * 2) - item ?1 QuantityDeltas)
    ; 2010-9-2:
    set daNewQuantityBreak ((item ?1 BaseQuantitiesBreaks) + daQuantityDelta)
    ; Now make sure it is positive!
    set daNewQuantityBreak (max (list 0.0 daNewQuantityBreak))
    ; And not more than TotalCapacity
    set daNewQuantityBreak (min (list daNewQuantityBreak 1.0))
    if (?1 > 0) [ ; we're not at the first step
      ; make sure it's larger than the previous step
      set daNewQuantityBreak (max (list daNewQuantityBreak (item (?1 - 1) daNewQuantityBreaks)))
    ]
    ; Add daNewQuantityBreak to  daNewQuantityBreaks
    set daNewQuantityBreaks lput daNewQuantityBreak daNewQuantityBreaks
    ; no set daQuantityDelta (min (list 1.0 daQuantityDelta)) ; should be <= 1
    ; 2010-7-9: Now make sure it isn't too long:
    ;print (word "At x119: " daBidQuantities " and " sum daBidQuantities " and " daQuantityDelta " and " CapacitiesOfPlants)
    ; 2010-9-1: Not needed, or appropriate:
;    if (sum daBidQuantities + daQuantityDelta + item ? BaseQuantities > sum CapacitiesOfPlants) ; could also use TotalCapacity
;      [;print (word "At x120: Adjust for over quantity ")
;        set daQuantityDelta (sum CapacitiesOfPlants - sum daBidQuantities - item ? BaseQuantities)]
;    if (? > 0) [ ; if we're not at the first step
;      set daQuantityDelta (max (list daQuantityDelta (item (? - 1) daBidQuantities)))
;    ]
;    ; Add the tentative new qantity bid to the list, daBidQuantities:
;    ;print (word "At x120.1 daQuantityDelta = " daQuantityDelta)
;    set daBidQuantities lput (daQuantityDelta + item ? BaseQuantities) daBidQuantities
;    ;print (word "At x121: daBidQuantities = " daBidQuantities)
;    ;
;    ; Evidently, daQuantityDelta + item ? BaseQuantities can still go negative.
;    set daQuantity max (list (daQuantityDelta + item ? BaseQuantities) 0)
;    ;print (word "In MakeANewBid-Suppliers. About to call PriceForQuantity. daQuantity = " daQuantity " and ? = " ? " and episodeRunCounter = " episodeRunCounter)
;    set daNewPrice max (list daNewPrice (daEpsilon + PriceForQuantity(daQuantity)(CapacitiesOfPlants)(CostsOfPlants)))
    set daBidPrices lput daNewPrice daBidPrices
    ; print (word ? " initially feasible? " IsFeasibleBid?(daBidPrices)(daBidQuantities)(CostsOfPlants)(CapacitiesOfPlants))
;    ; 2010-9-1: This has to be fixed:
;    if not IsFeasibleBid?(daBidPrices)(daBidQuantities)(CostsOfPlants)(CapacitiesOfPlants)
;      [set daBidPrices replace-item ? daBidPrices (item ?  daBidPrices - daPriceDelta)]
;    ; 2010-9-1: This has to be fixed:
;    if not IsFeasibleBid?(daBidPrices)(daBidQuantities)(CostsOfPlants)(CapacitiesOfPlants)
;      [set daBidQuantities replace-item ? daBidQuantities (item ?  daBidQuantities - daQuantityDelta)]
  ] ; end of foreach indexSet
  ; Now do the right-most segment
  ; Quantities first
  ;print (word "daNewQuantityBreaks="  daNewQuantityBreaks)
  set daBidQuantities []
  let quantitySoFar 0
  foreach daNewQuantityBreaks [ ?1 ->
    ;print (word "daBidQuantities=" daBidQuantities)
    let daBreak ?1
    ;print (word "daBreak=" daBreak)
    let toAdd (daBreak * TotalCapacity - quantitySoFar)
    ;print (word "toAdd=" toAdd)
    set daBidQuantities lput toAdd daBidQuantities
    set quantitySoFar (sum daBidQuantities)
  ]
  ;print (word "daBidQuantities=" daBidQuantities)
  ;set daBidQuantities lput (TotalCapacity - quantitySoFar) daBidQuantities
  set daBidQuantities lput (TotalCapacity - sum daBidQuantities) daBidQuantities
  ; Now the Prices:
  set daNewPrice ((random-float 2 * last PriceDeltas) - last PriceDeltas + last BasePrices)
  ;set daNewPrice max (list daNewPrice (item (length daBidPrices - 1) daBidPrices + daEpsilon) PriceForQuantity(TotalCapacity)(CapacitiesOfPlants)(CostsOfPlants))  set daBidPrices lput daNewPrice daBidPrices
  set daNewPrice (max (list daNewPrice (last daBidPrices + daEpsilon)))
  set daBidPrices lput daNewPrice daBidPrices
  ; Now test for feasibility:
  ;print (word "About to call IsFeasibleBid?" " daBidQuantities= " daBidQuantities " daBidPrices=" daBidPrices " CostsOfPlants=" CostsOfPlants " CapacitiesOfPlants=" CapacitiesOfPlants)
  set feasibleFound? IsFeasibleBid?(daBidPrices)(daBidQuantities)(CostsOfPlants)(CapacitiesOfPlants)
] ; end of while not feasibleFound?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  set CurrentBidBreaks daNewQuantityBreaks
  set CurrentBidQuantities daBidQuantities
  set CurrentBidPrices daBidPrices
  ;print (word "Prices: base and bid: " BasePrices " " daBidPrices)
  ;print (word "Quantities: base and bid, then sums: " BaseQuantities " " daBidQuantities " " sum BaseQuantities " " sum daBidQuantities)
  ;print (word "At x124, daBidQuantities = " daBidQuantities " and sum daBidQuantities = " sum daBidQuantities)
  ;print (word IsFeasibleBid?(daBidPrices)(daBidQuantities)(CostsOfPlants)(CapacitiesOfPlants))
  ; 2010-6-23: OK, now populate BidVector ***here*** See similar code in SetupARun, but based on true costs.
  set BidVector []
  foreach n-values length CurrentBidPrices [ ?1 -> ?1 ] [ ?1 ->
         set BidVector lput (list ([who] of self) (item ?1 CurrentBidPrices) (item ?1 CurrentBidQuantities)) BidVector
  ]
end ; end of MakeANewBid

to-report PriceForQuantity [daQuantity daCapacitiesOfPlants daCostsOfPlants]
  ;print (word "In PriceForQuantity. Supplier = " who "and daQuantity = " daQuantity " and daCapacitiesOfPlants = " daCapacitiesOfPlants " and daCostsOfPlants = " daCostsOfPlants)
  ; called from IsFeasibleBid?(daBidPrices)(daBidQuantities)(CostsOfPlants)(CapacitiesOfPlants)
  ; Given a quantity demanded, daQuantity, return the cost of providing it.
  ; Am assuming all quantities are non-negative.
  ; And that the demand does not exceed the total capacity. (This may be a problem.)
  ; Also this is called for a particular supplier.
  if (daQuantity < 0)
       [print "In PriceForQuantity, quantity is negative! Dividing by 0 now."
        print daQuantity
        let dummy daQuantity / 0
       ]
  if (daQuantity > sum daCapacitiesOfPlants)
       [print "In PriceForQuantity, quantity is too large! Dividing by 0 now."
        print (word "daQuantity = " daQuantity " and sum daCapacitiesOfPlants = " sum daCapacitiesOfPlants " and BaseQuantityBreaks=" BaseQuantitiesBreaks)
        let dummy daQuantity / 0
       ]
  ; First, we need to find which plant comes into play for daQuantity demanded.
  let daPrice -1
  let daSegment 0
  let capacityUsed 0
  let found false
  while [found = false and daSegment  < length daCostsOfPlants]
    [set capacityUsed capacityUsed + item daSegment daCapacitiesOfPlants
      ifelse (daQuantity <= capacityUsed )
      [set found true
       set daPrice item daSegment daCostsOfPlants
      ]
      [set daSegment daSegment + 1]

    ] ; end of while...
    if (daPrice < 0)
       [print "In PriceForQuantity, too much quantity! Dividing by 0 now."
        print daPrice
        let dummy daPrice / 0
       ]
    report daPrice
end


to-report IsFeasibleBid?[daBidPrices daBidQuantities daCosts daCapacities]
  ;print (word "In IsFeasibleBid? daBidPrices = " daBidPrices " and daBidQuantities = " daBidQuantities " and daCosts = " daCosts " and daCapacities = " daCapacities)
  let IsFeasible? true ; until proven false
  let daQuantity 0
  let daSegment 0
  while [daSegment < length daBidPrices]
    [set daQuantity (daQuantity + item daSegment daBidQuantities) ; getting the cumulation
     if (item daSegment daBidPrices < PriceForQuantity(daQuantity)(daCapacities)(daCosts))
        [set IsFeasible? false]
     set daSegment (daSegment + 1)
    ] ; end of while
  report IsFeasible?
end

to PlotTestPlot ;[daQIntercept daDSlope daFirm1Costs daFirm2Costs daCostSum]
  ;let daPlot "testplot"
  ;print (word "Hellow from PlotTestPlot, called on supplier " [who] of self)
  set-current-plot (word "testplot supplier " [who] of self)
  clear-plot
  set-current-plot-pen "Cost Curve"
  let CapacityStart 0
  let indexSet n-values (length CostsOfPlants) [ ?1 -> ?1 ]
  foreach indexSet [ ?1 ->
    plot-pen-up
    plotxy CapacityStart item ?1 CostsOfPlants
    plot-pen-down
    plotxy (item ?1 CapacitiesOfPlants + CapacityStart) item ?1 CostsOfPlants
    set CapacityStart (item ?1 CapacitiesOfPlants + CapacityStart)
  ]
  set-current-plot-pen "Bid Curve"
  set CapacityStart 0
  set indexSet n-values (length CurrentBidPrices) [ ?1 -> ?1 ]
  foreach indexSet [ ?1 ->
    plot-pen-up
    plotxy CapacityStart item ?1 CurrentBidPrices
    plot-pen-down
    plotxy (item ?1 CurrentBidQuantities + CapacityStart) item ?1 CurrentBidPrices
    set CapacityStart (item ?1 CurrentBidQuantities + CapacityStart)
  ]
  set-current-plot-pen "Base Curve"
  set CapacityStart 0
  set indexSet n-values (length BasePrices) [ ?1 -> ?1 ]
  foreach indexSet [ ?1 ->
    plot-pen-up
    plotxy CapacityStart item ?1 BasePrices
    plot-pen-down
    plotxy (item ?1 BaseQuantities + CapacityStart) item ?1 BasePrices
    set CapacityStart (item ?1 BaseQuantities + CapacityStart)
  ]
end
; DefaultSettings StoredCases
to SetCaseValues
  ; This is for the screen widgets
;;;;;;;;;;;; Base Case Parameterization ;;;;;;;;;;;;;
 if (StoredCases = "Base Case Parameterization") [
  set DSlope 1.0
  set QIntercept 200
  set Logging? false
  set RunningAverageLength 2000
  set NumberOfReplications 30
  set EpisodesPerRun 50000
  set RandomNumberSeed "System Clock"
  set NumberOfSuppliers 2
  set InstantiateScenarios "From the Interface"
  set EpochLengthFirm1 100
  set EpochLengthFirm2 100
  set Patience 1.05
  set UpdateTypeFirm1 "Own Returns"
  set UpdateTypeFirm2 "Own Returns"
  set PriceDeltaFirm1 0.2
  set PriceDeltaFirm2 0.2
  set PriceEpsilonFirm1 0.2
  set PriceEpsilonFirm2 0.2
  set Cost1Firm1 4
  set Cost1Firm2 4
  set MaxQ1Firm1 20
  set MaxQ1Firm2 20
  set Cost2Firm1 15
  set Cost2Firm2 15
  set MaxQ2Firm1 100
  set MaxQ2Firm2 100
  set InitialBasePrice1Firm1 16
  set InitialBasePrice1Firm2 16
  set InitialBasePrice2Firm1 150
  set InitialBasePrice2Firm2 150
  set InitialBasePrice3Firm1 150
  set InitialBasePrice3Firm2 150
  set QuantityBreak1Firm1 0.5
  set QuantityBreak1Firm2 0.5
  set QuantityBreak2Firm1 0.9
  set QuantityBreak2Firm2 0.9
  set QuantityDeltaFirm1 0.01
  set QuantityDeltaFirm2 0.01
  set QuantityEpsilonFirm1 0.01
  set QuantityEpsilonFirm2 0.01
] ; end of if (StoredCases = "Base Case Parameterization")
 ;;;;;;;;;;;;;;;;;;;;;;;; P2BothIs25 ;;;;;;;;;;;;;;;;;
 if (StoredCases = "P2BothIs25") [
  set DSlope 1.0
  set QIntercept 200
  set Logging? false
  set RunningAverageLength 2000
  set NumberOfReplications 30
  set EpisodesPerRun 50000
  set RandomNumberSeed "System Clock"
  set NumberOfSuppliers 2
  set InstantiateScenarios "From the Interface"
  set EpochLengthFirm1 100
  set EpochLengthFirm2 100
  set Patience 1.05
  set UpdateTypeFirm1 "Own Returns"
  set UpdateTypeFirm2 "Own Returns"
  set PriceDeltaFirm1 0.2
  set PriceDeltaFirm2 0.2
  set PriceEpsilonFirm1 0.2
  set PriceEpsilonFirm2 0.2
  set Cost1Firm1 4
  set Cost1Firm2 4
  set MaxQ1Firm1 20
  set MaxQ1Firm2 20
  set Cost2Firm1 15
  set Cost2Firm2 15
  set MaxQ2Firm1 100
  set MaxQ2Firm2 100
  set InitialBasePrice1Firm1 16
  set InitialBasePrice1Firm2 16
  set InitialBasePrice2Firm1 25
  set InitialBasePrice2Firm2 25
  set InitialBasePrice3Firm1 150
  set InitialBasePrice3Firm2 150
  set QuantityBreak1Firm1 0.5
  set QuantityBreak1Firm2 0.5
  set QuantityBreak2Firm1 0.9
  set QuantityBreak2Firm2 0.9
  set QuantityDeltaFirm1 0.01
  set QuantityDeltaFirm2 0.01
  set QuantityEpsilonFirm1 0.01
  set QuantityEpsilonFirm2 0.01
] ; end of if (StoredCases = "P2BothIs25")
;;;;;;;;;;;;;;;;  "Q2Firm1Is250" ;;;;;;;;;;;
 if (StoredCases = "Q2Firm1Is250") [
  set DSlope 1.0
  set QIntercept 200
  set Logging? false
  set RunningAverageLength 2000
  set NumberOfReplications 30
  set EpisodesPerRun 50000
  set RandomNumberSeed "System Clock"
  set NumberOfSuppliers 2
  set InstantiateScenarios "From the Interface"
  set EpochLengthFirm1 100
  set EpochLengthFirm2 100
  set Patience 1.05
  set UpdateTypeFirm1 "Own Returns"
  set UpdateTypeFirm2 "Own Returns"
  set PriceDeltaFirm1 0.2
  set PriceDeltaFirm2 0.2
  set PriceEpsilonFirm1 0.2
  set PriceEpsilonFirm2 0.2
  set Cost1Firm1 4
  set Cost1Firm2 4
  set MaxQ1Firm1 20
  set MaxQ1Firm2 20
  set Cost2Firm1 15
  set Cost2Firm2 15
  set MaxQ2Firm1 250
  set MaxQ2Firm2 100
  set InitialBasePrice1Firm1 16
  set InitialBasePrice1Firm2 16
  set InitialBasePrice2Firm1 25
  set InitialBasePrice2Firm2 25
  set InitialBasePrice3Firm1 100
  set InitialBasePrice3Firm2 100
  set QuantityBreak1Firm1 0.5
  set QuantityBreak1Firm2 0.5
  set QuantityBreak2Firm1 0.9
  set QuantityBreak2Firm2 0.9
  set QuantityDeltaFirm1 0.01
  set QuantityDeltaFirm2 0.01
  set QuantityEpsilonFirm1 0.01
  set QuantityEpsilonFirm2 0.01
] ; end of if (StoredCases = "Q2Firm1Is250")
;;;;;;;;;;;;;;; Bertrand250Both ;;;;;;;;;;;
 if (StoredCases = "Bertrand250Both") [
  set DSlope 1.0
  set QIntercept 200
  set Logging? false
  set RunningAverageLength 2000
  set NumberOfReplications 30
  set EpisodesPerRun 50000
  set RandomNumberSeed "System Clock"
  set NumberOfSuppliers 2
  set InstantiateScenarios "From the Interface"
  set EpochLengthFirm1 100
  set EpochLengthFirm2 100
  set Patience 1.05
  set UpdateTypeFirm1 "Own Returns"
  set UpdateTypeFirm2 "Own Returns"
  set PriceDeltaFirm1 0.2
  set PriceDeltaFirm2 0.2
  set PriceEpsilonFirm1 0.2
  set PriceEpsilonFirm2 0.2
  set Cost1Firm1 4
  set Cost1Firm2 4
  set MaxQ1Firm1 20
  set MaxQ1Firm2 20
  set Cost2Firm1 15
  set Cost2Firm2 15
  set MaxQ2Firm1 250
  set MaxQ2Firm2 250
  set InitialBasePrice1Firm1 16
  set InitialBasePrice1Firm2 16
  set InitialBasePrice2Firm1 25
  set InitialBasePrice2Firm2 25
  set InitialBasePrice3Firm1 100
  set InitialBasePrice3Firm2 100
  set QuantityBreak1Firm1 0.5
  set QuantityBreak1Firm2 0.5
  set QuantityBreak2Firm1 0.9
  set QuantityBreak2Firm2 0.9
  set QuantityDeltaFirm1 0.01
  set QuantityDeltaFirm2 0.01
  set QuantityEpsilonFirm1 0.01
  set QuantityEpsilonFirm2 0.01
] ; end of if (StoredCases = "Bertrand250Both")
end

to codetest
  foreach [160000] [ ?1 ->  ; A
    print ?1
  ]
end

to SetBaseQuantities
  ; 2010-9-1
  let daBreaks BaseQuantitiesBreaks
  let daTotalQ TotalCapacity
  set BaseQuantities []
  let capacitySoFar 0
  foreach BaseQuantitiesBreaks [ ?1 ->
   let daCum ?1
   ;print (word "daCum=" daCum)
   set BaseQuantities lput (daCum * daTotalQ - capacitySoFar) BaseQuantities
   ;print (word "BaseQuantities=" BaseQuantities)
   set capacitySoFar (daCum * daTotalQ)
   ;print (word "capacitySoFar=" capacitySoFar)
  ]

  set BaseQuantities lput (daTotalQ - capacitySoFar) BaseQuantities
  ;print (word "In SetBaseQuantities. Turtle " [who] of self ". BaseQuantities=" BaseQuantities " BaseQuantitiesBreaks=" BaseQuantitiesBreaks " TotalCapacity=" TotalCapacity)
end

to-report TheBaseQuantities [daBreaks daTotalQ]
    set BaseQuantities []
  let capacitySoFar 0
  foreach BaseQuantitiesBreaks [ ?1 ->
   let daCum ?1
   set BaseQuantities lput (daCum * daTotalQ - capacitySoFar) BaseQuantities
   set capacitySoFar (capacitySoFar + daCum * daTotalQ)
  ]
  set BaseQuantities lput (daTotalQ - capacitySoFar) BaseQuantities
end



to-report GetBestPerformance [TheIndex ThePerformances TheCounts]
  ; ThePerformances should be a list of one or more triples, as should TheCounts.
  ; TheIndex picks out which of the triples we are to use.
  let WhichPerformances item TheIndex ThePerformances
  let WhichCounts item TheIndex TheCounts
  let TheMeans []
  let TheValue 0
  foreach n-values length WhichPerformances [ ?1 -> ?1 ]
  [ ?1 -> set TheValue (MyDivision(item ?1 WhichPerformances)(item ?1 WhichCounts))
   set TheMeans lput TheValue TheMeans
  ]
  ; let daResult position max item daIndex EpochCostPerformance item daIndex EpochCostPerformance
  report position max TheMeans TheMeans
end

to-report GetMeanPerformances [ThePerformances TheCounts]
  print (word "ThePerformances=" ThePerformances " TheCounts=" TheCounts)
  let TheMeans []
  let TheMean 0
  foreach n-values length ThePerformances [ ?1 -> ?1 ]
  [ ?1 -> set TheMean (MyDivision(item ?1 ThePerformances)(item ?1 TheCounts))
   set TheMeans lput TheMean TheMeans
  ]
  report TheMeans
end

to-report AreBreaksValid? [TheBaseQuantityBreaks]
  let previous 0
  let IsValid? true
  foreach TheBaseQuantityBreaks [ ?1 ->
    if (?1 < previous)
     [set IsValid? false]
    if (?1 < 0.0 or ?1 > 1.0)
     [set IsValid? false]
    set previous ?1
  ]
  report (list IsValid? TheBaseQuantityBreaks)
end
@#$#@#$#@
GRAPHICS-WINDOW
1288
757
1469
939
-1
-1
5.242424242424242
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
259
10
367
43
NIL
SetupARun
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
388
233
933
477
Supply & Demand Curves
Quantity
Price
0.0
400.0
0.0
250.0
true
true
"" ""
PENS
"Demand Curve" 1.0 0 -16777216 true "" ""
"Supply, Firm 1" 1.0 0 -3026479 true "" ""
"Supply, Firm 2" 1.0 0 -13345367 true "" ""
"Supply, Firm 3" 1.0 0 -11221820 true "" ""
"Supply, Firm 4" 1.0 0 -955883 true "" ""
"Supply, Firm 5" 1.0 0 -2674135 true "" ""
"Supply, Firm 6" 1.0 0 -10899396 true "" ""

PLOT
597
24
1119
233
Cost & Demand Curves
Quantity
Price
0.0
200.0
0.0
120.0
true
true
"" ""
PENS
"Demand Curve" 1.0 0 -16777216 true "" ""
"Cost Curve, Firm 1" 1.0 0 -3026479 true "" ""
"Cost Curve, Firm 2" 1.0 0 -13345367 true "" ""
"Cost Curve, Firm 3" 1.0 0 -11221820 true "" ""
"Cost Curve, Firm 4" 1.0 0 -955883 true "" ""
"Cost Curve, Firm 5" 1.0 0 -2674135 true "" ""
"Cost Curve, Firm 6" 1.0 0 -10899396 true "" ""

SLIDER
0
333
188
366
Cost1Firm1
Cost1Firm1
0
100
4.0
1
1
NIL
HORIZONTAL

SLIDER
0
365
188
398
MaxQ1Firm1
MaxQ1Firm1
0
120
20.0
1
1
NIL
HORIZONTAL

SLIDER
-1
398
188
431
Cost2Firm1
Cost2Firm1
0
200
15.0
1
1
NIL
HORIZONTAL

SLIDER
-1
431
188
464
MaxQ2Firm1
MaxQ2Firm1
0
300
100.0
1
1
NIL
HORIZONTAL

SLIDER
195
332
382
365
Cost1Firm2
Cost1Firm2
0
100
4.0
1
1
NIL
HORIZONTAL

SLIDER
195
365
382
398
MaxQ1Firm2
MaxQ1Firm2
0
100
20.0
1
1
NIL
HORIZONTAL

SLIDER
195
398
382
431
Cost2Firm2
Cost2Firm2
0
200
15.0
1
1
NIL
HORIZONTAL

SLIDER
195
431
382
464
MaxQ2Firm2
MaxQ2Firm2
0
300
100.0
1
1
NIL
HORIZONTAL

SLIDER
1120
25
1292
58
QIntercept
QIntercept
0
400
200.0
1
1
NIL
HORIZONTAL

SLIDER
1120
58
1292
91
DSlope
DSlope
0.0
3.0
1.0
0.01
1
NIL
HORIZONTAL

MONITOR
386
100
597
145
NIL
InitialCostDemandInterceptPrice
17
1
11

MONITOR
387
144
597
189
NIL
InitialCostDemandInterceptQuantity
17
1
11

MONITOR
388
189
554
234
NominalProfitFirm1
NominalProfitFirm1
17
1
11

MONITOR
500
189
612
234
NIL
NominalProfitFirm2
17
1
11

MONITOR
388
626
933
671
NIL
daVersion
17
1
11

SLIDER
0
267
188
300
PriceDeltaFirm1
PriceDeltaFirm1
0
10.0
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
0
300
188
333
PriceDeltaFirm2
PriceDeltaFirm2
0
10.0
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
195
266
382
299
PriceEpsilonFirm1
PriceEpsilonFirm1
0
10.0
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
195
299
382
332
PriceEpsilonFirm2
PriceEpsilonFirm2
0
10.0
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
0
464
188
497
InitialBasePrice1Firm1
InitialBasePrice1Firm1
0
100
16.0
1
1
NIL
HORIZONTAL

SLIDER
0
497
188
530
InitialBasePrice2Firm1
InitialBasePrice2Firm1
InitialBasePrice1Firm1
200
150.0
1
1
NIL
HORIZONTAL

SLIDER
195
464
382
497
InitialBasePrice1Firm2
InitialBasePrice1Firm2
0
100
16.0
1
1
NIL
HORIZONTAL

SLIDER
195
497
382
530
InitialBasePrice2Firm2
InitialBasePrice2Firm2
InitialBasePrice1Firm2
200
150.0
1
1
NIL
HORIZONTAL

BUTTON
486
10
597
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

MONITOR
933
233
1111
278
NIL
CostDemandInterceptPrice
2
1
11

MONITOR
933
278
1111
323
NIL
CostDemandInterceptQuantity
2
1
11

MONITOR
933
323
1137
368
NIL
[myEpisodeProfit] of supplier 1
2
1
11

MONITOR
933
368
1113
413
NIL
[myEpisodeProfit] of supplier 2
2
1
11

SLIDER
200
140
387
173
EpochLengthFirm1
EpochLengthFirm1
0
400
100.0
1
1
NIL
HORIZONTAL

SLIDER
200
173
388
206
EpochLengthFirm2
EpochLengthFirm2
0
400
100.0
1
1
NIL
HORIZONTAL

PLOT
388
477
933
627
Prices
Episode
Price
0.0
1000.0
0.0
30.0
true
false
"" ""
PENS
"Price" 1.0 0 -16777216 true "" ""
"InitialCostDemandInterceptPrice" 1.0 0 -10899396 true "" ""

CHOOSER
388
671
714
716
UpdateTypeFirm1
UpdateTypeFirm1
"Own Returns" "Market Returns" "Market Returns Constrained by Own Returns"
2

CHOOSER
713
671
1039
716
UpdateTypeFirm2
UpdateTypeFirm2
"Own Returns" "Market Returns" "Market Returns Constrained by Own Returns"
2

MONITOR
1120
181
1254
226
Industry Episode Profit
[industryEpisodeProfit] of turtle 1
2
1
11

SLIDER
1291
25
1506
58
RunningAverageLength
RunningAverageLength
0
10000
2000.0
10
1
NIL
HORIZONTAL

MONITOR
1254
136
1423
181
NIL
mean runningIndustryProfits
2
1
11

PLOT
933
413
1518
671
Profits
NIL
NIL
0.0
10.0
0.0
3000.0
true
true
"" ""
PENS
"Industry (Total) Profit" 1.0 0 -16777216 true "" ""
"Firm 1 Profit" 1.0 0 -3026479 true "" ""
"Firm 2 Profit" 1.0 0 -13345367 true "" ""
"Firm 3 Profit" 1.0 0 -11221820 true "" ""
"Firm 4 Profit" 1.0 0 -955883 true "" ""
"Firm 5 Profit" 1.0 0 -2674135 true "" ""
"Firm 6 Profit" 1.0 0 -10899396 true "" ""

MONITOR
1110
233
1276
278
NIL
mean runningPrices
2
1
11

MONITOR
1111
278
1298
323
NIL
mean runningQuantities
2
1
11

MONITOR
1112
323
1298
368
Running Avg. Profits, Firm 1
mean [RunningProfits] of supplier 1
2
1
11

MONITOR
1113
368
1298
413
Running Ave. Profits, Firm 2
mean [RunningProfits] of supplier 2
2
1
11

CHOOSER
0
116
200
161
RandomNumberSeed
RandomNumberSeed
"System Clock" 1 2 3 4 5 6 7 8 9 10
0

SLIDER
0
51
200
84
EpisodesPerRun
EpisodesPerRun
0
344430
50000.0
100
1
NIL
HORIZONTAL

BUTTON
367
10
488
43
NIL
GoNEpisodes
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
1120
136
1254
181
NIL
episodeRunCounter
17
1
11

SLIDER
0
84
200
117
NumberOfReplications
NumberOfReplications
0
100
30.0
1
1
NIL
HORIZONTAL

BUTTON
0
10
117
43
NIL
SetupAndGo
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
1120
91
1254
136
NIL
replicationsCompleted
17
1
11

SWITCH
1292
58
1404
91
Logging?
Logging?
1
1
-1000

MONITOR
1275
234
1457
279
NIL
variance runningPrices
2
1
11

MONITOR
1297
279
1502
324
NIL
variance runningQuantities
2
1
11

MONITOR
1254
181
1439
226
NIL
variance runningIndustryProfits
2
1
11

MONITOR
1298
323
1518
368
NIL
variance [RunningProfits] of supplier 1
2
1
11

MONITOR
1298
368
1518
413
NIL
variance [RunningProfits] of supplier 2
2
1
11

SLIDER
199
96
386
129
Patience
Patience
0
10.0
1.05
0.05
1
NIL
HORIZONTAL

CHOOSER
0
206
388
251
StoredCases
StoredCases
"Base Case Parameterization" "P2BothIs25" "Q2Firm1Is250" "Bertrand250Both"
0

CHOOSER
199
51
401
96
InstantiateScenarios
InstantiateScenarios
"Monopolist" "One Firm, Zero Costs" "Two Firms, Zero Costs" "From the Interface" "TwoFirmsSimple" "NFirmsSimple" "Three Firms, Simple" "Q2Firm1Is250 6 steps" "P2Both2Is25 6 steps" "Bertrand250Both 6 steps"
5

PLOT
388
716
838
962
testplot supplier 1
NIL
NIL
0.0
100.0
0.0
60.0
true
true
"" ""
PENS
"Cost Curve" 1.0 0 -16777216 true "" ""
"Bid Curve" 1.0 0 -10899396 true "" ""
"Base Curve" 1.0 0 -2674135 true "" ""

BUTTON
117
10
259
43
NIL
SetCaseValues
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
838
716
1288
962
testplot supplier 2
NIL
NIL
0.0
100.0
0.0
40.0
true
true
"" ""
PENS
"Cost Curve" 1.0 0 -16777216 true "" ""
"Bid Curve" 1.0 0 -10899396 true "" ""
"Base Curve" 1.0 0 -2674135 true "" ""

MONITOR
1
828
388
873
NIL
[basequantities] of supplier 2
2
1
11

MONITOR
1
917
388
962
NIL
[BasePrices] of supplier 2
17
1
11

MONITOR
1
739
207
784
NIL
[BaseQuantitiesBreaks] of supplier 2
17
1
11

SLIDER
0
563
187
596
QuantityBreak1Firm1
QuantityBreak1Firm1
0
1.0
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
0
595
187
628
QuantityBreak2Firm1
QuantityBreak2Firm1
QuantityBreak1Firm1
1.0
0.9
0.01
1
NIL
HORIZONTAL

SLIDER
0
628
187
661
QuantityDeltaFirm1
QuantityDeltaFirm1
0
0.5
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
0
661
187
694
QuantityEpsilonFirm1
QuantityEpsilonFirm1
0
0.2
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
195
563
382
596
QuantityBreak1Firm2
QuantityBreak1Firm2
0
1.0
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
195
596
382
629
QuantityBreak2Firm2
QuantityBreak2Firm2
QuantityBreak1Firm2
1.0
0.9
0.01
1
NIL
HORIZONTAL

SLIDER
195
629
382
662
QuantityDeltaFirm2
QuantityDeltaFirm2
0
0.5
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
195
662
382
695
QuantityEpsilonFirm2
QuantityEpsilonFirm2
0
0.2
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
-1
530
187
563
InitialBasePrice3Firm1
InitialBasePrice3Firm1
InitialBasePrice2Firm1
150
150.0
1
1
NIL
HORIZONTAL

SLIDER
195
530
382
563
InitialBasePrice3Firm2
InitialBasePrice3Firm2
InitialBasePrice2Firm2
150
150.0
1
1
NIL
HORIZONTAL

CHOOSER
0
161
201
206
NumberOfSuppliers
NumberOfSuppliers
1 2 3 4 5 6
3

MONITOR
1
784
388
829
NIL
[BaseQuantities] of supplier 1
2
1
11

MONITOR
1
873
388
918
NIL
[BasePrices] of supplier 1
17
1
11

MONITOR
1
694
207
739
NIL
[BaseQuantitiesBreaks] of supplier 1
17
1
11

MONITOR
467
55
597
100
NIL
NominalIndustryProfit
17
1
11

BUTTON
386
43
467
76
Setup CA
Setup\nSetupARun
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## FOR A QUICK START

The simplest start is just to click the SetupAndGo button in the upper left-hand corner of the Interface tab. This will set up the scenario visible in the InstantiateScenarios chooser and run it for the number of episodes (market transactions) shown in the EpisodesPerRun chooser, which is just below the SetupAndGo button. The InstantiateScenarios chooser is just to the right of the EpisodesPerRun chooser, centered under the row of blue buttons at the top-left of the Interface tab.

The run (of EpisodesPerRun episodes) will be conducted the number of times indicted in the NumberOfReplications chooser, which is just below the EpisodesPerRun chooser. By default, it is set to 1.

On the Interface tab there are several choices in the InstantiateScenarios chooser including: "Monopolist", "One Firm, Zero Costs", "Two Firms, Zero Costs" "TwoFirmsSimple", "From the Interface", and "Three Firms, Simple". All of these scenarios are set up in the code, in the SetupAgents procedure. You may inspect them, modify them (not recommended), and add new ones (recommended). Generally, they read some of their parameter values from the Interface tab, for example EpisodesPerRun, NumberOfReplications, RandomNumberSeed, and Patience. You may adjust these (and other) widgets before you direct a run to be made.

If you pick the "From the Interface" scenario you can run with NumberOfSuppliers set to 1 or 2. The suppliers are called Firm1 and Firm2. The setup process reads its parameter values from the widgets on the Interface tab.

Again, SetupAndGo does EpisodesPerRun episodes, and does it NumberOfReplications times. You can always stop things by selecting Halt under the Tools menu.

SetupAndGo is equivalent to
1) Setup
2) SetupARun
3) GoNEpisodes
except that only one replication will be done. You can also substitute Go for GoNEpisodes. This is a "go forever" button. You stop the run by clicking on it again.

So here are some options for conducting one or more runs:

1) Adjust the widgets on the interface
   (Note: You can select a setting under the DefaultSettings chooser, then click the SetDefaultValues button.)
2) Click SetupAndGo

1) Adjust the widgets on the interface
   (Note: You can select a setting under the DefaultSettings chooser, then click the SetDefaultValues button.)
2) Click SetupARun
3) Click Go (forever)

1) Adjust the widgets on the interface
   (Note: You can select a setting under the DefaultSettings chooser, then click the SetDefaultValues button.)
2) Click SetupARun
3) Click GoNEpisodes

There are small differences among these, but they are not generally important and have more to do with collecting experimental data than exploring with the tool.

## STRUCTURE OF THE MODEL

There are two types of agents: one Market agent, with a who or ID of 0, and 1 or more Supplier agents, with whos or IDs of 1, 2, ...

Crucially, the Market agent holds the current summation of all the bids, BidSum, and the CurrentMarketPrice and CurrentMarketQuantity.

First initialization, in SetDefaultValues, then SetupAndGo, for further initialization, ten, still in SetupAndGo, this:

     while [replicationsCompleted < NumberOfReplications]
         [SetupARun
          GoNEpisodes
          set replicationsCompleted (replicationsCompleted + 1)

(with SetupARun calling SetupAgents0 and GoNEpisodes calling Go) then do logging.

So, what I am going to do is to create a 0 version (old) and then in parallel a non-zero (new) version. The key thing will be to make Go work. I have it:

     SetDefaultValues0
     SetupAndGo0
     SetupARun0
     SetupAgents0
     GoNEpisodes0
     Go0


## OLD STUFF AFTER HERE

## WHAT IS IT?

Two firms, firm0 and firm1 (turtles 0 and 1), supply a market, intended to resemble the electric power market.  Each firm has two plants whose costs and outputs are set by the sliders Cost1Firm0 (firm 0's unit cost for plant 1), MaxQ1Firm0 (maximum capacity, or quantity, available for firm 0's plant 1), etc. At the start of each period, each of the two firms bids (price, quantity) pairs for each of its plants and perhaps for an extra, notional plant. Firms will bid prices at or above their actual costs, and quantities at or below their actual quantities available.

## HOW IT WORKS

Most centrally, each firm is independently characterized by two properties: (1) its update type and (2) its bidder type. These are set by choosers in the southwest corner of the Interface tab.

There are two bidder types: N bidders and N+1 bidders. N bidders always put all of their two quantities on the market. They make decisions about how to price them. N+1 bidders shade the market by pricing at a very high level some portion of their resource from their more expensive plant. In addition to the pricing decisions made by N bidders, N+1 bidders decide how much of their expensive resources they will effectively price out of the market, thereby effectively reducing supply.

At present there are 4 update types: (1) Own Returns, (2) Market Returns, (3) Market Returns, subject to Own Returns (MR-COR), and (4) Market Returns, Constrained by Bid2 Match. The code handles this in PostPareEpisode, which is perhaps the central procedure of the program.

There are, in consequence, 8 (= 4 * 2) possible conditions under which information must be processed for each agent.  Since there are two agents, there are 8 * 8 = 64 pairwise combinations. Neglecting symmetric pairs (e.g., neglecting (1,2) if we have (2,1)) that leaves 64 - (7+6+5+4+3+2+1) = n^2 - n*(n-1)/2 = 64 - 28 = 36, when n=8.

* * *

The program is driven fundamentally by two procedures: SetupARun and (then) Go. SetupARun initializes the system, Go runs it.  Go is executed from a forever button on the Interface tab. One loop through Go constitutes one episode (of posting bids and receiving payoffs). Alternatively to the forever version of Go, GoNEpisodes runs Go EpisodesPerRun times, using the Interface tab slider to determine the value of EpisodersPerRun (set to 120000 by default).  For serious experimental purposes multiple replications are necessary.  This number is set by NumberOfRuns on the Interface tab and it all can be run with one click of the SetupAndGo button on the Interface tab.

In Go, perhaps the key two called procedures are ObserveAndRecord and PostPareEpisode. But first, we need to describe Set-PricesBid and Set-QuantitiesBid.

*** Set-PricesBid & Set-QuantitiesBid ***

There are two types of bidders, N bidders and N+1 bidders. N bidders have two prices to set, Bid1, the price bid for units from resource 1, and Bid2, the price bid for units from resource 2. These are set (in the Probe and Adjust fashion) by uniform random draws from plus and minus delta from the agent's base prices for the resources. N+1 bidders have a third price to set, Bid3. It is set deterministically as 4 * Bid2. The intention here is that Bid3 is to be too high to be accepted by the demand curve. This is the price of the "hold out" portion of resource 2.

Both N bidders and N+1 bidders deterministically set the quantity for their resource 1 to its maximum, MaxQ1. N bidders also deterministically set the quantity for their resource 2 to its maximum, MaxQ2. N+1 bidders, however, set the level of their hold out quantity, Q2Shade, by using a Probe and Adjust process. Their BidQ2 is then set deterministically to MaxQ2 - Q2Shade.

In sum, in setting its bids an N bidding player is making substantive decisions about two quantities: Bid1 (the price for its lower-cost resource), Bid2 (the price for its higher cost resource). An N+1 bidding player is, in addition, making a decision about a third quantity, Q2Shade.

*** ObserveAndRecord ***

In ObserveAndRecord, each player records, in Probe and Adjust fashion, the results of the episode just played.  Players keep track of both their own rewards and of the total market rewards. For just their own rewards, the players need to have 6 accumulators, 2 for each of the 3 substantive decisions it may be making. (N+1 bidders need all 6, N bidders only 4.) The same holds for keeping track of total market rewards.

On entering ObserveAndRecourd each player has had calculated and stored myEpisodeProfit and industryEpisodeProfit. The player updates accordingly...

*** PostPareEpisode ***

Postparing is only done for those firms whose epochs are over.

Key here is MR-COR. Presently only N bidder code is implemented. Note that the behavior of (MR-COR, MR-COR) is not really different from (Own Returns, Own Returns). Why? Because once one of the firms becomes the "leader" and withholds production, the other guy's bids don't matter and just drift AND, as seen in (Own Returns, Own Returns) it is in the leader's myopic interest to withhold a fair amount of production (under the standard setup).

## HOW TO USE IT

Default settings are set fully in the procedure SetDefaultValues, which is executed from a button on the Interface tab. Here are some key values:

QIntercept=344. DSlope=0.5. runningAverageLength=2000. Cost1Firm0=Cost1Firm1=50.
MaxQ1Firm0=MaxQ1Firm1=60. Cost2Firm0=Cost2Firm1=110. MaxQ2Firm0=MaxQ2Firm1=82.
InitialBasePrice1Firm0=InitialBasePrice1Firm1=65.
InitialBasePrice2Firm0=InitialBasePrice2Firm1=150.
InitialBaseQ2ShadeFirm0=InitialBaseQ2ShadeFirm1=10.0.
deltaFirm0=deltaFirm1=5.0. epsilonFirm0=epsilonFirm1=1.0.
epochLengthFirm0=epochLengthFirm1=50.

The easiest way to set it up and run is (1) click on the SetDefaultValues button, then (2) click on the SetupAndGo button. (Optionally, first make a selection from the DefaultSettings choosers, which is just below the SetDefaultValues button, then proceed (1), (2), as just described.)

This will run the simulation with both firms using the "Own Returns" update policy for Probe and Adjust and being N-bidders. Now experiment with, say, both firms using MR-COR ("Market Returns, Constrained by Own Returns") and N+1 Bidding. See how they do! Then try various combinations.

There are presently two options in the DefaultSettings chooser: Base Case Parameterization, which is above, and Bertrand Long Parameterization.  In the latter, when both firms use "Own Returns" we see Bertrand-type behavior, a "rush to the bottom". Now try MR-COR, etc.

## THINGS TO NOTICE

This section could give some ideas of things for the user to notice while running the model.

## THINGS TO TRY

Try it with just one turtle, a monopolist. For N bidding, use the default settings (above) but set MaxQ1Firm0=120, instead of 60, and MaxQ2Firm0=164, instead of 82. Now run it with "Own Returns". This monopolist will learn to set its BasePrice2 at about 408 and achieve an industry profit level of about 48,920.

Now try it (N bidders both) with 2 firms using the default settings.  Now, typically, the industry profit is much lower, about 38,800, split roughly 24,000 and 14,000.  Which firm wins or loses is random. One of the firms will discover that holding out some production is in its (myopic) self-interest and will do so. The other firm will find that its Bid2 price is not affecting anything and will let it drift.

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## TO NOTICE

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.


## CREDITS AND REFERENCES

To refer to this model in academic publications, please use:  Kimbrough, Steven O. and Murphy, Frederic H. (2010).  NetLogo Supply Curve Bidding model.  http://opim.wharton.upenn.edu/~sok/AGEbook/nlogo/SupplyCurveBidding.nlogo . University of Pennsylvania, Philadelphia, PA 19104.

In other publications, please use:  Copyright 2010 Steven O. Kimbrough and Frederic H. Murphy.  All rights reserved.

Version: $Id: SupplyCurveBidding.nlogo 2351 2011-05-22 17:28:38Z sok $
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
NetLogo 6.2.0
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
