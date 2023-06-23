; ------------------------------------------------------------------------------------------------------------
;                                                     Create Variables
; ------------------------------------------------------------------------------------------------------------


globals [
  ;; agentsets
  agriculture
  feeding-habitat
  breeding-habitat
  free-patches

  ;; habitats & flowers
  density ;; flower density in feeding habitat (in flowers per patch)
  flower-ratio ;; ratio of flowers in agricultural fields to flowers in feeding habitats (in 0.x)
  germ-prob ;; germination probability of seeds (in 0 - 100)
  energy-con ;; energy consumed when bee feeds on flower
  brood-energy ;; energy required to create brood

  ;; time passage
  tick-counter
  season-length ;; length of one season of bee flights (in ticks)
  lifetime-crops ;; lifetime of agricultural crops (in ticks)
  bee-number
]

patches-own [
  seeds
  brood-cells
]

breed [flowers flower]
flowers-own [
  energy
]

breed [bees bee]
bees-own [
    energy
    nest? ;; does the bee have a nest (true/false)
    nest-cor ;; the patch that the nest is located on
 ]


to startup
  setup
end

; ------------------------------------------------------------------------------------------------------------
;                                            Setup
; ------------------------------------------------------------------------------------------------------------

to setup
  clear-all
  set-globals
  setup-patches
  ask patches [ patch-variables ]
  setup-flowers
  setup-bees
  reset-ticks
end

; ------------------------------------------------------------------------------------------------------------
;                                       Initialize Variables
; ------------------------------------------------------------------------------------------------------------

to set-globals
  ;set max-distance 5
  ;set min-distance 2
  set season-length 500
  set lifetime-crops 200
  set bee-number 4
  set brood-energy 100
  set energy-con 3
end


to patch-variables
   set density 1
   set flower-ratio 2
   set germ-prob 60
end


; ------------------------------------------------------------------------------------------------------------
;                                          Set up Patches
; ------------------------------------------------------------------------------------------------------------

;; to add: variety in habitat size, randomification?; habitat distance
to setup-patches
  if min-distance > max-distance [
    set max-distance min-distance
    print "max-distance cannot be smaller than min-distance. max-distance was automatically set to the same value as min-distance" ;; give error message
  ]
  ;; set all patches yellow [agriculture]
  ask patches [
    set pcolor yellow
  ]

  set free-patches patches with [ (pcolor = yellow) and (pxcor < max-pxcor - feeding-habitat-size) and (pxcor > min-pxcor + feeding-habitat-size) and (pycor < max-pycor - feeding-habitat-size) and (pycor > min-pycor + feeding-habitat-size) ]
  ask one-of free-patches [ set pcolor green ]

  setup-habitats green (feed-number - 1) feeding-habitat-size ;; create green patches [feeding habitats]
  setup-habitats brown breed-number breeding-habitat-size ;; create brown patches [breeding habitats]
  enlarge-habitats green feeding-habitat-size
  enlarge-habitats brown breeding-habitat-size


  ;; assign the different colored patches to different agent sets for further code
  set agriculture patches with [pcolor = yellow]
  set feeding-habitat patches with [pcolor = green]
  set breeding-habitat patches with [pcolor = brown]
end


to setup-habitats [ habitat-color habitat-number habitat-size ] ;; create habitats
  ;; create a local agentset of patches that are still free for putting a center-patch of a new habitat on them
  ;; so far these are all yellow (= agricultural) patches that are not too close to the edge of the world

  ;; turn some other patches [amount: habitat-number] into habitat-color that are within the set minimum/maximum distance of each other
    repeat (habitat-number)[
      carefully [ ;; to avoid crash if no fitting patch is found
        ;; choose a random yellow patch that has the required distance from an existing habitat patch as initial patch (will later be the center of the new habitat)

      set free-patches free-patches with [ all? patches in-radius ((0.5 * habitat-size + 0.5 * feeding-habitat-size) * sqrt 2 + min-distance) [ pcolor = yellow ] ]
      ; ask free-patches [ set pcolor red ]
      ;; turn the center patch into habitat-color
      ask one-of free-patches with [ any? patches in-radius ((0.5 * habitat-size + 0.5 * feeding-habitat-size) * sqrt 2 + max-distance) with [ pcolor = green ] ] [
        set pcolor habitat-color
      ]
     ; ask free-patches with [ pcolor = red ] [ set pcolor yellow ]
     ][ print "Not enough patches within distance parameters found. Number of patches may not match input." ] ;; error message if there are not enough fitting patches
   ]
end

  ;; turn neighbouring patches into habitat-color until the habitat size is reached

to enlarge-habitats [ habitat-color habitat-size ]
 repeat ((habitat-size - 1) / 2) [
    ask patches with [ pcolor = habitat-color ] [
      ask neighbors [
        set pcolor habitat-color
     ]
    ]
   ]
end



; ------------------------------------------------------------------------------------------------------------
;                                       Set up Flowers
; ------------------------------------------------------------------------------------------------------------

to setup-flowers
 ;; sprout flowers on feeding habitat with a specific density per patch
   ask feeding-habitat [
    sprout-flowers density
  ]

  agriculture-flowers ;; create flowers on agricultural patches
  flowers-birth ;; set shape & size of flowers
end

to agriculture-flowers
  ;; sprout flowers on a certain number of agricultural patches (density * amount of agricultural patches * ratio of agricultural to feeding habitat)
  let n 0
  while [ n < (density * flower-ratio * count agriculture)  ] [ ;; create flowers on randomly chosen patches
    ask one-of agriculture [ sprout-flowers 1 ]
    set n (n + 1)
  ]
end

to flowers-birth
  ask flowers [
    set shape "flower" ;; make the flowers look nice
    if member? patch-here feeding-habitat [ set color one-of [ yellow magenta cyan orange ] ]
     if member? patch-here agriculture [ set color red ]
    set energy 10 ;; give the flowers 10 energy. TO DO: how many energy points shall they have?
    ]
end


; ------------------------------------------------------------------------------------------------------------
;                                           SET UP BEES
; ------------------------------------------------------------------------------------------------------------

to setup-bees
  let n 0 ;; as a counter during loop
  while [ n < bee-number ] [ ;; create bees on randomly chosen breeding habitat patches
    ask one-of breeding-habitat [ sprout-bees 1 ]
    set n (n + 1)
  ]
  bee-birth ;; set shape and color of bees
end

to bee-birth
  ask bees [
    set shape "bee"
    set color black
    set energy 10 ;; give the bees 10 energy. TO DO: how many energy points shall they have?
    set nest? false
  ]
end


; ------------------------------------------------------------------------------------------------------------
;                                           GO
; ------------------------------------------------------------------------------------------------------------

to go
  ;; BEES
  ask bees [
    ifelse nest? = false [ nest ] [ ;; bees might make a nest if they do not have one yet
    ifelse energy > brood-energy and patch-here = nest-cor [ create-cell ] ;; beescreate a brood cell if they have enough energy and are at the location of their nest
    [ wiggle ;; bees change direction
      move   ;; bees move
      eat  ;; bees eat
    ]
  ] ]
  ;; GENERAL
  check-if-dead
  generation-passage
  tick
end


; ------------------------------------------------------------------------------------------------------------
;                                           BEE MOVEMENT
; ------------------------------------------------------------------------------------------------------------

to wiggle
  ; if bees have enough energy to breed, they turn towards their nest
  ifelse nest? = true and energy > brood-energy + 20 [
    set heading towards nest-cor
  ]
  ;; otherwise they choose a random direction
  ;; turn right then left, so the average is straight ahead
  [ rt random 90
    lt random 90 ]
end

to move
  forward 1
  set energy energy - 0.1 ;; reduce the energy by the cost of 0.1. TO DO: how much energy should moving cost?
end


; ------------------------------------------------------------------------------------------------------------
;                                           FEEDING
; ------------------------------------------------------------------------------------------------------------

;; bees eat if any of the flowers on their patch have enough energy to be def on
to eat
  if any? flowers-here with [ energy >= energy-con ] [
    ask one-of flowers-here with [ energy >= energy-con ] [
      set energy energy - energy-con ;; reduce flower energy
        if random-float 100 < 45 [ set seeds seeds + 1 ] ;; successful pollination leading to seed with a 45% chance
    ]
    set energy energy + energy-con ;; increase bee energy
   ]
end


; ------------------------------------------------------------------------------------------------------------
;                                           REPRODUCTION
; ------------------------------------------------------------------------------------------------------------

;; if bees are on a patch of breeding habitat, there is an 80% chance they will make a nest there
to nest
  if [pcolor] of patch-here = brown [
    if random-float 100 <= 80 [
      set nest-cor patch-here
      set nest? true
   ]
  ]
end

;; bees create brood cells on their nest and lose the required amount of energy
to create-cell
      ask self [
        set energy energy - brood-energy ]
      ask patch-here [
        set brood-cells brood-cells + 1 ]
end


; ------------------------------------------------------------------------------------------------------------
;                                           PASSAGE OF SEASON
; ------------------------------------------------------------------------------------------------------------

to generation-passage
  set tick-counter tick-counter + 1 ;; tick counter increases every tick
  ;; once the season is over ...
  if tick-counter > season-length [
    ;; bees and flowers die
    ask bees [ die ]
    ask flowers [ die ]
    ;; new bees emerge from brood cells TO DO: what is overwinter mortality rate?
    ask breeding-habitat [
      sprout-bees brood-cells
      set brood-cells 0 ]
    ;; seeds on breeding habitats sprout new flowers with a certain probability (germ-prob)
    ;; 50/50 chance on whether flower grows on same patch or neighbouring feeding habitat
    ;; TO DO: Flowers have same color as "parent" flower
    ask feeding-habitat [
      let n 0
      while [ n < seeds ] [
        if random-float 100 < germ-prob [
          ifelse random-float 100 < 50 [ sprout-flowers 1 ]
          [ carefully [ ask one-of neighbors with [ pcolor = green ] [ sprout-flowers 1 ] ] [  ] ;; nothing happens if there are no neighbouring habitats - the seed dies
          ]
        ]
        set n n + 1 ]
      set seeds 0
    ]
    agriculture-flowers ;; new agricultural flowers grow regardless of pollination (as they are planted instead of reproducing by seeds)
    bee-birth ;; set shape of new bees
    flowers-birth ;; set shape of new flowers
    set tick-counter 0
    ]
end



; ------------------------------------------------------------------------------------------------------------
;                                              DEATH
; ------------------------------------------------------------------------------------------------------------
to check-if-dead
  ;; flowers on agricultural fields die early
  if tick-counter > lifetime-crops [
    ask flowers with [ color = red ] [ die ]
  ]
  ;; bees die if their energy is 0
  ask bees [if energy < 0 [ die ] ]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
628
429
-1
-1
10.0
1
10
1
1
1
0
0
0
1
-20
20
-20
20
0
0
1
ticks
10.0

BUTTON
26
14
89
47
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

BUTTON
102
14
165
47
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

SLIDER
11
57
183
90
feed-number
feed-number
0
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
11
100
183
133
breed-number
breed-number
0
10
2.0
1
1
NIL
HORIZONTAL

SLIDER
9
145
181
178
feeding-habitat-size
feeding-habitat-size
1
7
1.0
2
1
NIL
HORIZONTAL

SLIDER
11
234
183
267
min-distance
min-distance
0
5
0.0
1
1
NIL
HORIZONTAL

SLIDER
10
277
182
310
max-distance
max-distance
min-distance + 1
10
1.0
1
1
NIL
HORIZONTAL

PLOT
654
10
854
160
Plot
Time
Number
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -15973838 true "" "plot count bees"
"pen-1" 1.0 0 -8053223 true "" "plot count flowers with [ color != red]"

SLIDER
10
186
182
219
breeding-habitat-size
breeding-habitat-size
1
7
7.0
2
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

bee
true
0
Polygon -1184463 true false 152 149 77 163 67 195 67 211 74 234 85 252 100 264 116 276 134 286 151 300 167 285 182 278 206 260 220 242 226 218 226 195 222 166
Polygon -16777216 true false 150 149 128 151 114 151 98 145 80 122 80 103 81 83 95 67 117 58 141 54 151 53 177 55 195 66 207 82 211 94 211 116 204 139 189 149 171 152
Polygon -7500403 true true 151 54 119 59 96 60 81 50 78 39 87 25 103 18 115 23 121 13 150 1 180 14 189 23 197 17 210 19 222 30 222 44 212 57 192 58
Polygon -16777216 true false 70 185 74 171 223 172 224 186
Polygon -16777216 true false 67 211 71 226 224 226 225 211 67 211
Polygon -16777216 true false 91 257 106 269 195 269 211 255
Line -1 false 144 100 70 87
Line -1 false 70 87 45 87
Line -1 false 45 86 26 97
Line -1 false 26 96 22 115
Line -1 false 22 115 25 130
Line -1 false 26 131 37 141
Line -1 false 37 141 55 144
Line -1 false 55 143 143 101
Line -1 false 141 100 227 138
Line -1 false 227 138 241 137
Line -1 false 241 137 249 129
Line -1 false 249 129 254 110
Line -1 false 253 108 248 97
Line -1 false 249 95 235 82
Line -1 false 235 82 144 100

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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
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
