globals [pstar current-price price-intercept m-price epochOver
         up-data down-data total-revenue probe-count ;episodeLength
         quantityIntercept
         up-prices down-prices ;monop-prices
         monop-quantities
         m-up-prices m-dn-prices m-up-revs m-dn-revs
         up-as down-as currentQuantity m-quantity initial-m-quantity initial-m-price initial-price
         version ; CVS (or Subversion) version string
         ]
to setup
set version "$Id: MonopolyProbeAndAdjust.nlogo 4082 2014-03-20 00:40:40Z sok $"
if (daRandomSeed != "system clock")
  [random-seed daRandomSeed]

set  quantityIntercept qIBase

set currentQuantity initialQuantity

set price-intercept daPriceIntercept(quantityIntercept)(dSlope)
set current-price (price-intercept - (dSlope * initialQuantity))
set initial-price current-price
set m-quantity daMQuantity ; (price-intercept / ( 2 * slope)) ; set the monopoly quantity
set initial-m-quantity m-quantity
set initial-m-price (price-intercept - (dSlope * initial-m-quantity))

set m-price getMPrice(price-intercept)(dSlope)(m-quantity)
clear-all-plots
plot-quantity-price(current-price)
set epochOver false
set up-data []
set up-prices []
set down-data []
set down-prices []
set monop-quantities []
set m-up-prices []
set m-dn-prices []
set m-up-revs []
set m-dn-revs []
set up-as []
set down-as []
set total-revenue 0
set probe-count 0


end

to go
let probe 0
  let myRevenue 0

while [not epochOver]
[
; If random-as, that is if a in the demand function, = a + b*p, is to be be set
; via random walk, then
; figure out the new demand curve.
if (random-as)
  [
   ifelse (random-walk-as)
   [set quantityIntercept (random-float 2 * aDelta) - aDelta + quantityIntercept]
   [set quantityIntercept (random-float 2 * aDelta) - aDelta + qIBase] ;quantityIntercept
  set price-intercept daPriceIntercept(quantityIntercept)(dSlope) ; quantityIntercept * slope
  set m-quantity daMQuantity ; quantityIntercept / (2 * slope)
  set m-price getMPrice(price-intercept)(dSlope)(m-quantity)
  ] ; end of if (random-as)

; repeated-sampling. If On, our new probe is currentQuantity plus or minus delta.
; If Off, our new probe ranges uniformly between currentQuantity plus and minus delta.
ifelse (repeated-sampling)
[set probe (random-float 1)
 ifelse (probe <= 0.5)
   [set probe currentQuantity - delta]
   [set probe currentQuantity + delta]] ; end of if in ifelse(repeated-sampling)
[set probe (random-float 2 * delta) - delta + currentQuantity] ; end of ifelse repeated-sampling


 set current-price (price-intercept - (dSlope * probe))
 set probe-count probe-count + 1
 set myRevenue daRevenue(probe)
 set total-revenue total-revenue + myRevenue ; daRevenue(probe)
 ifelse (probe >= currentQuantity)
   [set up-data lput myRevenue up-data]
   [set down-data lput myRevenue down-data]

 if (fine-grained)
   [plot-quantities(currentQuantity)(m-quantity)]

; if (length up-data > episodeLength and length down-data > episodeLength)
 if (length up-data + length down-data >= epochLength)
   [set epochOver true]

] ; end of do while

if (epochOver)
[ifelse (mean up-data >= mean down-data)
  [set currentQuantity (currentQuantity + epsilon)]
  [set currentQuantity (currentQuantity - epsilon)]

  if (not fine-grained)
    [plot-quantities(currentQuantity)(m-quantity)]

  set epochOver false
  set up-data []
  set down-data []

  ] ; end of if(epochOver)

end


to plot-quantity-price [daPrice]
set-current-plot "quantity-price"
clear-plot
set-plot-y-range 0 price-intercept
set-plot-x-range 0 quantityIntercept
set-current-plot-pen "Demand Curve"
plot-pen-up
plotxy 0 price-intercept
plot-pen-down
plotxy quantityIntercept 0
set-current-plot-pen "Monopoly Quantity"
plot-pen-up
plotxy m-quantity 0
plot-pen-down
plotxy m-quantity m-price
set-current-plot-pen "Monopoly Price"
plot-pen-up
plotxy 0 m-price
plot-pen-down
plotxy m-quantity m-price
end

to-report daRevenue [daQuantity] ; [daPrice]
  let daPrice 0

  set daPrice (price-intercept - (dSlope * daQuantity))
  report daPrice * daQuantity
end

to-report daPriceIntercept [q-intercept daSlope]
  set price-intercept (q-intercept * daSlope)
  report price-intercept
end

to-report daMQuantity
  set m-quantity (price-intercept / ( 2 * dSlope))
  report m-quantity
end

to-report getMPrice [da-price-intercept da-slope da-m-quantity]
  report (da-price-intercept - (da-slope * da-m-quantity))
end

to plot-quantities [daQuantity mono-quant]
set-current-plot "Monopoly and Current Quantities"
set-current-plot-pen "Agent's Quantity"
plot daQuantity
set-current-plot-pen "Monopoly Quantity"
plot mono-quant
end


to debug
let probe 0

if (random-as)
  [ set quantityIntercept (random-float 2 * aDelta) - aDelta + qIBase ;quantityIntercept
  set price-intercept quantityIntercept / dSlope
  set m-price quantityIntercept / (2 * dSlope)
  ;set monop-prices lput m-price monop-prices
  ] ; if of if (random-as)
set probe (random-float 2 * delta) - delta + current-price
show (word "a = "  quantityIntercept)
show (word "price intercept = "  price-intercept)
show (word "dSlope = "  dSlope)
show (word "monopoly price = "  m-price)
show (word "monopoly demand = "  (quantityIntercept - dSlope * m-price) )
show (word "monopoly revenue = "  daRevenue(m-price) )
show (word "current-price = "  current-price)
show (word "probe = "  probe)
show (word "probe revenue = "  daRevenue(probe))
end
@#$#@#$#@
GRAPHICS-WINDOW
350
10
535
196
-1
-1
5.06
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

SLIDER
9
140
181
173
qIBase
qIBase
0
1000
200.0
1
1
NIL
HORIZONTAL

SLIDER
9
173
181
206
dSlope
dSlope
0
5
2.0
0.01
1
NIL
HORIZONTAL

MONITOR
211
428
342
473
Monopoly Price
m-price
3
1
11

BUTTON
10
10
76
43
NIL
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

MONITOR
467
378
590
423
Current Quantity Bid
currentQuantity
3
1
11

MONITOR
467
427
562
472
Current Price
current-price
3
1
11

MONITOR
467
476
562
521
Revenue
currentQuantity * current-price
3
1
11

SLIDER
9
73
181
106
delta
delta
0
5
3.0
0.1
1
NIL
HORIZONTAL

SLIDER
9
107
181
140
epsilon
epsilon
0
5
1.0
0.1
1
NIL
HORIZONTAL

PLOT
600
280
979
527
quantity-price
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
"Demand Curve" 1.0 0 -11221820 true "" ""
"Monopoly Price" 1.0 0 -10899396 true "" ""
"Monopoly Quantity" 1.0 0 -16777216 true "" ""

MONITOR
342
357
467
402
Price Intercept
quantityIntercept * dSlope
3
1
11

BUTTON
105
10
168
43
NIL
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
181
10
979
280
Monopoly and Current Quantities
NIL
NIL
0.0
1000.0
90.0
110.0
true
true
"" ""
PENS
"Agent's Quantity" 1.0 0 -2674135 true "" ""
"Monopoly Quantity" 1.0 0 -16777216 true "" ""

SWITCH
11
273
137
306
random-as
random-as
0
1
-1000

SLIDER
10
306
182
339
aDelta
aDelta
0
10
1.0
0.1
1
NIL
HORIZONTAL

SWITCH
10
207
144
240
fine-grained
fine-grained
0
1
-1000

SWITCH
11
339
170
372
random-walk-as
random-walk-as
0
1
-1000

SWITCH
11
372
185
405
repeated-sampling
repeated-sampling
1
1
-1000

SLIDER
10
240
182
273
initialQuantity
initialQuantity
0
200
115.0
0.1
1
NIL
HORIZONTAL

MONITOR
211
379
342
424
Monopoly Quantity
m-quantity
3
1
11

MONITOR
211
477
342
522
Monopoly Revenue
m-price * m-quantity
3
1
11

MONITOR
811
348
978
393
Initial Monopoly Quantity
initial-m-quantity
3
1
11

MONITOR
811
397
978
442
Initial Monopoly Price
initial-m-price
3
1
11

TEXTBOX
285
285
588
355
Current Monopoly and Firm Values\n---------------------------\nMonopoly quantities, prices, and revenues on the left.\nAgent's quantities, prices, and revenues on the right.
11
0.0
0

SLIDER
11
405
183
438
epochLength
epochLength
0
100
50.0
1
1
NIL
HORIZONTAL

CHOOSER
11
438
149
483
daRandomSeed
daRandomSeed
0 1 2 3 4 5 6 100 "system clock"
0

MONITOR
14
521
455
566
NIL
version
17
1
11

@#$#@#$#@
## WHAT IS IT?

This model explores how a monopolist might discover the "monopoly quantity" (the quantity that maximizes profit for the monopolist agent) in the face of, here, a linear demand function.

Quantity is being adjusted and quantity is a continuous variable. This program uses what I call a "Probe and Adjust" model, and a very simple one at that.  The agent begins with a current value, here currentQuantity, set by initialQuanity in the Interface tab. Play is organized by epochs, each containing a number of episodes. In each episode, the agent probes its market by offering a quantity in the range [currentQuantity - delta, currentQuantity + delta]. The agent records the revenue it gets form the probe, classifying the revenue as coming from a quantity offered that is either above or below the currentQuantity. When the epoch is over (a number of episodes, set by epochLength in the Interface tab), the agent determines whether on average it did better by offering more than the currentQuantity or less. Depending on the results, it increases its currentQuantity by epsilon or decreases it by epsilon. Then a new epoch begins. Both delta and epsilon are set in the Interface. The key point about this model is that the agent does not have access to the demand function, except as it responds to probing.

Introductory textbooks in microeconomics will tell the story roughly as follows.  Suppose, for the sake of simplicity that the product in question can be produced at 0 cost. (Extensions of the model can be made for non-zero costs. The results will not be terribly sensitive to this.) The market's demand, Q (think: quantity demanded), for our product is a linear function of its price, P:

(1)  Q(P) = Q = c - b*P

Here, c is a constant, representing the quantity demanded when price is 0 and, since we are linear, the price point at which demand disappears when the price is too high (when b*P = c). Rearranging (1) we get:

(2) P(Q) = P = a - dSlope*Q

where a = c/b and dSlope = 1/b. (We assume that dSlope > 0, so -dSlope < 0.)

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

The program variable slope corresponds to slope in equation (2). A slider on the Interface sets its value. The program variable qIBase (quantity-intercept, basic or initial) is the value of Q (quantity) at which P (price) equals 0. A slider on the Interface sets its value. See equation (2). So, a = dSlope*qIBase.

Put slightly differently, the demand function can be expressed as

    Price(Quantity) = Price = priceIntercept - (dSlope * Quantity)

From the Interface tab we are given at initialization:

    dSlope

and 

    qIBase

qIBase is the initial value of quantityIntercept.  The program uses dSlope and qIBase to solve for priceIntercept, since:

    0 = priceIntercept - (dSlope * quantityIntercept)

This gives us priceIntercept and dSlope, from which we can compute Price given a Quantity via the above equation, reproduced here:

    Price(Quantity) = Price = priceIntercept - (dSlope * Quantity)

(Note on notation: Price and Quantity are variables, priceIntercept and dSlope are constants.)

Play is organized by epochs. There is a series of probes by the monopolist within each epoch, one probe per episode (market transaction). When the number of probes (episodes) is large enough (>= epochLength), the epoch concludes, adjustments are made, and a new epoch begins.

## HOW TO USE IT

random-as is an On-Off switch. When it is off, the demand curve is set and fixed throughout the run, determined by q-i (initial quantity intercept) and slope. When random-as is on, then at the beginning of each probe (play) in an episode, quantity-intercept is randomly perturbed. There are two ways this might be done.

First, if the On-Off switch for random-walk-as is on, quantity-intercept is randomly reset uniformly between the current value of quantity-intercept plus a-delta and the current value of quantity-intercept minus a-delta.  Second, if the On-Off switch for random-walk-as is off, quantity-intercept is randomly reset uniformly between the current value of q-i plus a-delta and the current value of q-i minus a-delta. In either case, the value for price-intercept is recalculated/reset, as are m-quantity and m-price.

## RUNNING THE MODEL

On the Interface tab, click the setup button, then click the go button. The model will run until you click the go button again to stop it.

The Interface tab offers a number of sliders and switches for setting run parameter values.

    initialQuantity 

Sets the base, or anchor, quantity the monopolist agent uses during its first episode.  As in all episodes, the agent has a base value for quantity and it probes by offering to the market quantities that are somewhat above or below this base quantity.

    episodeLength 

Sets the number of samples the monopolist agent takes before considering an adjustment of its base quantity value.  The agent samples until the number of samples is greater than or equal to episodeLength. Then the agent adjusts its base quantity and a new episode begins.

    daRandomSeed

Picking an item with this chooser determines how the random number generator is to be initialized for the run.  Picking "system clock" lets NetLogo do the initialization. This will result in a different random number stream for each run. Picking a number results in the random number generated with that number, resulting in an identical random number stream for each run.  This is useful for comparison and communication purposes.  Also, note that the chooser is user-editable, so it it possible to add and delete entries.

    random-as, random-walk-as

Price = a - dSlope*Quanity. dSlope is set by the interface slider:

    dSlope

and a is the price intercept for the demand function (the value of the demand function when Quantity = 0.  The initial value of the quantity intercept (the value of Quantity when Price = 0) is set with the Interface tab slider

    qIBase

At initialization (in setup) qIBase is read into the program variable quantityIntercept.

    aDelta

When random-as is on and random-walk-as is off, quantityIntercept is randomly changed each period by uniformly drawing in [quantityIntercept - aDelta, quantityIntercept + aDelta].

When both random-as is on and random-walk-as is on, quantityIntercept is randomly change each period as follows:

quantityIntercept(t+1) = U[-aDelta, aDelta] +  quantityIntercept(t)

## THINGS TO NOTICE

This section could give some ideas of things for the user to notice while running the model.

/* here, I need to say more */

## THINGS TO TRY

Leave debug-switch off.

1) Simple example.

delta 3.0  
epsilon  1.0  
q-i 200  
slope 2.0  
fine-grained ON  
initialQuantity 115.0  
random-as Off  
a-delta -  
random-walk-as -  
repeated-sampling Off

Quickly finds the static optimum quantity and bounces around, a little above and below. Experiment with reducing epsilon.  Once the agent gets in the neighborhood of the static optimum, reduce epsilon, say to 0.1.  The agent, of course, doesn't know the demand function. Is there a way the agent could sense that epsilon is too big (or too small)? Yes!

2) Random walk example 

delta 3.0  
epsilon 2.0  
q-i  200  
slope  2.0  
fine-grained ON  
initialQuantity 100  
random-as  On, random-seed 17  
a-delta  0.9  
random-walk-as  On  
repeated-sampling Off

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

## CREDITS AND REFERENCES

This model was created and written by Steven O. Kimbrough: kimbrough at wharton.upenn.edu and http://opim-sky.wharton.upenn.edu/~sok/.  The model is   
freely downloadable at:  
http://opim-sky.wharton.upenn.edu/~sok/agebook/applications/nlogo/monopolyProbeAndAdjust.nlogo

Please give me credit if you use this model or program. 

To refer to this model in academic publications, please use: Kimbrough, Steven O. (2007). Monopoly Probe and Adjust  
model. http://opim-sky.wharton.upenn.edu/~sok/agebook/applications/nlogo/monopolyProbeAndAdjust.nlogo University of Pennsylvania, Philadelphia, PA 19004, USA.  
In other publications, please use: Copyright 2007 Steven O. Kimbrough. All  
rights reserved.

$Id: MonopolyProbeAndAdjust.nlogo 4082 2014-03-20 00:40:40Z sok $
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
