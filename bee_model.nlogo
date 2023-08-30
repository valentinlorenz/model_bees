; Authors: Valentin Lorenz & Wanja Tolksdorf
; Copyright 2023 by Valentin Lorenz & Wanja Tolksdorf
; This work is licensed under a Creative Commons Attribution 4.0 International License (CC BY 4.0).
; You are free to share and adapt this model, but have to give appropriate credit.
; Full license terms: https://creativecommons.org/licenses/by/4.0/legalcode
; Link to work: https://github.com/valentinlorenz/model_bees



; ------------------------------------------------------------------------------------------------------------
;                                                     CREATE VARIABLES
; ------------------------------------------------------------------------------------------------------------


globals [

  ; agentsets
  agriculture
  feeding-habitat
  breeding-habitat
  free-patches
  crops
  wildflowers
  specialized-bees

  ; flowers: general
  density                ; initial flower density on feeding habitat (in flowers per patch)
  flower-ratio           ; ratio of flowers in agricultural fields to flowers in feeding habitats (as decimal number)
  max-flowers-per-patch  ; maximum amount of flowers that can inhabit a patch
  flower-colors          ; list of possible colors for flowers
  ; flowers: reproduction
  percent-perennials     ; percentage of flowers that do not die at the end of a season (in percent)
  germ-prob              ; germination probability of seeds (in 0 - 100 percent)
  ; flowers: pollen provision
  max-pollen             ; maximum of pollen flowers have available
  pollen-reset-time      ; time it takes for pollen to become available again if it has been eaten (in ticks)
  pollen-timer           ; timer that counts ticks until pollen reset

  ; bees: movement
  max-flight-dist        ; maximum flight distance of bees (in patches)
  initial-energy         ; initial energy of bees
  energy-movement        ; energy it costs a bee to move 1 patch forward
  ; bees: reproduction
  larvae-survival-rate   ; percentage of bee larvae that survive the winter (in 0 - 100 percent)
  max-nest-amount        ; maximum amount of nests that can be build on one patch
  brood-energy           ; energy required to create one brood cell
  ; bees: feeding
  pollen-consumption     ; energy consumed when a bee feeds on pollen
  energy-gain            ; energy bees gain from feeding on pollen or nectar
  specialist-food-colors ; list of flower colors that specialist bees feed on
  generalist-food-colors ; list of flower colors that generalist bees feed on

  ; time passage
  tick-counter           ; timer that counts ticks until season end
  season-length          ; length of one season of bee flights (in ticks)
  lifetime-crops         ; lifetime of agricultural crops (in ticks)

]


patches-own [
  specialist-brood-cells ; amount of specialist brood cells that are currently on the patch
  generalist-brood-cells ; amount of generalist brood cells that are currently on the patch
  nest-amount            ; amount of nests that are currently on the patch
  ]


breed [flowers flower]
flowers-own [
  seeds                  ; amount of seeds the flower currently has
  pollen                 ; amount of pollen the flower currently has
]


breed [bees bee]
bees-own [
    energy               ; amount of energy the bee currently has
    nest?                ; does the bee have a nest (true/false)
    nest-cor             ; the patch that the bee's nest is located on
    home-cor             ; the point of birth from which the maximum flight distance is measured
    food-color           ; list of flower colors the bee feeds on
]



; ------------------------------------------------------------------------------------------------------------
;                                            SETUP
; ------------------------------------------------------------------------------------------------------------


to startup
  setup
end


to setup
  clear-all
  set-globals
  setup-patches
  setup-flowers
  setup-bees true (bee-number * percent-specialized-bees) ; create specialized bees
  setup-bees false (bee-number - count bees)              ; create generalist bees
  set specialized-bees bees with [food-color = specialist-food-colors]
  clear-all-plots
  reset-ticks
end



; ------------------------------------------------------------------------------------------------------------
;                                       INITIALIZE VARIABLES
; ------------------------------------------------------------------------------------------------------------


to set-globals

  ; flowers: general
  set density 10
  set flower-ratio 2
  set max-flowers-per-patch 10
  set flower-colors (list cyan magenta orange yellow)
  ; flowers: reproduction
  set percent-perennials 30
  set germ-prob 60
  ; flowers: pollen provision
  set max-pollen 8
  set pollen-reset-time 10

  ; bees: movement
  set max-flight-dist 15
  set initial-energy 10
  set energy-movement 0.5
  ; bees: reproduction
  set larvae-survival-rate 78.5
  set max-nest-amount 5
  set brood-energy 300
  ; bees: feeding
  set pollen-consumption 4
  set specialist-food-colors (list cyan)
  set generalist-food-colors (list cyan magenta orange yellow red)

  ; time passage
  set season-length 500
  set lifetime-crops 200

end



; ------------------------------------------------------------------------------------------------------------
;                                          SET UP PATCHES
; ------------------------------------------------------------------------------------------------------------


to setup-patches

  ; set all patches yellow (agriculture)
  ask patches [
    set pcolor yellow
  ]

  ; the agentset free-patches stores all patches that are still free and suitable for being the center of a new habitat
  ; so far these are all yellow (= agricultural) patches that are not too close to the edge of the world
  set free-patches patches with [ (pcolor = yellow) and (pxcor < max-pxcor - breeding-habitat-size) and (pxcor > min-pxcor + breeding-habitat-size)
    and (pycor < max-pycor - breeding-habitat-size) and (pycor > min-pycor + breeding-habitat-size) ]

  ; choose a random free patch as the center of the first breeding habitat
  ask one-of free-patches [ set pcolor brown ]

  ; create brown patches as centers of new breeding habitats, then green patches as centers of new feeding habitats (in the specified number and distance)
  setup-habitats brown (breeding-habitat-number - 1)(breeding-habitat-size - 1) min-distance-breed max-distance-breed
  setup-habitats green feeding-habitat-number (feeding-habitat-size - 1) min-distance-feed max-distance-feed

  ; enlarge the habitats until they reach the specified habitat size
  enlarge-habitats brown breeding-habitat-size
  enlarge-habitats green feeding-habitat-size

  ; assign the different colored patches to different agentsets for further code
  set agriculture patches with [pcolor = yellow]
  set feeding-habitat patches with [pcolor = green]
  set breeding-habitat patches with [pcolor = brown]

end


; create center patches for habitats in the specified color, number, size and distance
to setup-habitats [ habitat-color habitat-number habitat-size min-distance max-distance ]

  ; if the habitat distances given by the user are impossible, change max-distance and print error message
  if min-distance >= max-distance [
    set max-distance min-distance + 2
    print "max-distance cannot be smaller than or equal to min-distance. max-distance was automatically set to min-distance + 2"
  ]

  ; turn some other patches (amount: habitat-number) into habitat-color to create center patches that are within the set minimum/maximum distance of each other
  repeat (habitat-number)[

    ; carefully to avoid crash if no fitting patch is found
    carefully [

      ; update free-patches to contain all patches which only have yellow agricultural patches in their radius
      ; the radius is calculated in a way that accounts for the enlarging of habitats and complies with the minimum distance
      set free-patches free-patches with [ all? patches in-radius ((0.5 * habitat-size + 0.5 * breeding-habitat-size) * sqrt 2 + min-distance) [ pcolor = yellow ] ]

      ; choose a free patch as center for another habitat and turn it into habitat-color
      ; there must be at least one brown breeding patch within the maximum distance of the new habitat to comply with the habitat distances set by the user
      ; the new center patch must not be too close to other patches of the same habitat to avoid overlap even at larger habitat sizes
      ask one-of free-patches with [
        any? patches in-radius ((0.5 * habitat-size + 0.5 * breeding-habitat-size) * sqrt 2 + max-distance) with [ pcolor = brown ] and
        not any? patches in-radius (habitat-size * sqrt 2) with [ pcolor = habitat-color ]
      ] [
      set pcolor habitat-color
      ]

    ][
      ; error message if there are not enough fitting patches
      print "Not enough patches within distance parameters found. Number of patches may not match input."
    ]
  ]

end


; turn neighbouring patches of the center patches into habitat-color until the habitat size is reached
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
;                                       SET UP FLOWERS
; ------------------------------------------------------------------------------------------------------------


to setup-flowers
  ask feeding-habitat [
    sprout-flowers density          ; sprout flowers on feeding habitat with a specific density per patch
  ]
  create-crops                      ; create flowers on agricultural patches
  flowers-birth                     ; sort flowers into agentsets, set their appearance and initialize their variables
  ask wildflowers [
    set color one-of flower-colors  ; set wildflower color
  ]                                 ; (only used here bc flowers-birth function is recalled later and is not supposed to change wildflower color then)
end


; sprout a certain amount of flowers on randomly chosen agricultural patches
; the amount is calculated based on the initial density of flowers on feeding habitat:
; initial density of flowers on feeding habitat * ratio of flowers on agricultural vs feeding habitat * amount of agricultural patches
to create-crops
  let n 0
  while [ n < (density * flower-ratio * count agriculture) ] [
    ask one-of agriculture [ sprout-flowers 1 ]
    set n (n + 1)
  ]
end


; sort flowers into agentsets, set their appearance and initialize their variables
to flowers-birth
  set crops flowers with [pcolor = yellow]
  set wildflowers flowers with [pcolor = green]
  ask flowers [
    set shape "flower"
    set pollen max-pollen
    ]
  ask crops [ set color red ]
end



; ------------------------------------------------------------------------------------------------------------
;                                           SET UP BEES
; ------------------------------------------------------------------------------------------------------------


; create bees on randomly chosen breeding habitat patches, set their food preferences and appearance
to setup-bees [ specialized? bee-amount ]
  let n 0
  while [ n < bee-amount ] [
    ask one-of breeding-habitat [
      sprout-bees 1 [
        ifelse specialized? [ set food-color (list cyan) ]
        [ set food-color (list cyan red orange magenta yellow red) ]
      ]
      ]
    set n (n + 1)
  ]
  bee-birth
end


; set the bees' appearance and initialize their variables
to bee-birth
  ask bees [
    set home-cor patch-here
    set shape "bee"
    set color black
    set energy initial-energy
    set nest? false
  ]
end



; ------------------------------------------------------------------------------------------------------------
;                                           GO
; ------------------------------------------------------------------------------------------------------------


to go
  ask bees [
    create-nest ; bees might make a nest if they do not have one yet and there is space
    create-cell ; bees create a brood cell if they have enough energy and are at the location of their nest
    turn-bees   ; bees change direction
    move-bees   ; bees move
    eat-pollen  ; bees eat and pollinate
  ]

  ; flowers provide more pollen after some time
  ask flowers with [ pollen < max-pollen ] [ provide-more-pollen ]

  ; bees with zero energy die; crops die if they reach the end of their lifetime
  check-if-dead

  ; if the season is over, the old generation of flowers and bees dies and new ones are born
  generation-passage
  tick
end



; ------------------------------------------------------------------------------------------------------------
;                                           BEE MOVEMENT
; ------------------------------------------------------------------------------------------------------------


to turn-bees
  (ifelse

    ; if the bees are as far away from home as their maximum flight distance they turn towards their home
    distance home-cor >= max-flight-dist [
      set heading towards home-cor
    ]

    ; if bees have enough energy to breed, they turn towards their nest
    nest? = true and energy > brood-energy + 20 [
      set heading towards nest-cor
    ]

    ; otherwise they turn towards the nearest flower which belongs to their preferred food and has enough pollen to feed on it
    ; (except for the flower on the bee's current patch to prevent them from getting stuck at one place)
    [ let current-bee self
      let color-pref food-color
      let target-flower one-of flowers in-cone 10 250 with [ pollen >= pollen-consumption and distance current-bee >= 1 and color = one-of color-pref ]

      ; if there is no flower nearby that belongs to the bees' preferred food, they turn towards another flower that has enough pollen
      if target-flower = nobody [
        set target-flower one-of flowers in-cone 10 250 with [  pollen >= pollen-consumption and distance current-bee >= 1 ]
      ]

      ifelse target-flower != nobody [
      set heading towards target-flower
      ] [
        right random 90  ;; if no flower with enough pollen exists the bees choose a random direction
        left random 90   ;; turn right then left, so the average is straight ahead
      ]
    ]
  )
end


; bees move one patch forward which costs them energy
to move-bees
  forward 1
  set energy energy - energy-movement
end



; ------------------------------------------------------------------------------------------------------------
;                                           FEEDING
; ------------------------------------------------------------------------------------------------------------


; bees eat and pollinate if any of the flowers on their patch have enough energy to feed on
to eat-pollen

  ; bees feed on one of the flowers on their patch that has their prefered color and enough pollen if such a flower is present
  ; thereby bee energy is increased, pollen of the flower are decreased and the flower is pollinated, which leads to a seed with a 45% chance
  if any? flowers-here with [  pollen >= pollen-consumption ] [
    let color-pref food-color
    carefully [
      ask one-of flowers-here with [ pollen >= pollen-consumption and color = one-of color-pref ] [
        set pollen pollen - pollen-consumption
        set energy-gain pollen-consumption
        if random-float 100 < 45 [ set seeds seeds + 1 ]
      ]
    ]

    ; specialized bees may feed on nectar if there are no flowers they are specialized on available, but nectar gives them less energy
    [ ask one-of flowers-here with [ pollen >= pollen-consumption ] [
        set energy-gain (pollen-consumption * 0.25)
      ]
    ]

    set energy energy + energy-gain
    set energy-gain 0
  ]

end


; flowers provide more pollen after some time
to provide-more-pollen
  set pollen-timer pollen-timer + 1
  if pollen-timer = pollen-reset-time [
    set pollen pollen + pollen-consumption
    set pollen-timer 0
  ]
end



; ------------------------------------------------------------------------------------------------------------
;                                           REPRODUCTION
; ------------------------------------------------------------------------------------------------------------


; if bees do not have a nest yet and are on a patch of breeding habitat that still has free space, there is an 80% chance they will make a nest there
to create-nest
  if nest? = false and [pcolor] of patch-here = brown and [nest-amount] of patch-here < max-nest-amount and random-float 100 <= 80 [
      set nest-cor patch-here
      set nest? true
      ask patch-here [ set nest-amount nest-amount + 1 ]
   ]
end


; if they are at their nest, the bees create a brood cell and lose the required amount of energy
to create-cell
  if energy > brood-energy and patch-here = nest-cor [
    set energy energy - brood-energy
    ifelse member? self specialized-bees [
      ask patch-here [ set specialist-brood-cells specialist-brood-cells + 1 ]
    ][
      ask patch-here [ set generalist-brood-cells generalist-brood-cells + 1 ]
    ]
  ]
end



; ------------------------------------------------------------------------------------------------------------
;                                           PASSAGE OF SEASON
; ------------------------------------------------------------------------------------------------------------


to generation-passage

  ; tick counter increases every tick
  set tick-counter tick-counter + 1

  ; once the season is over ...
  if tick-counter > season-length [

    ; all bees die and new bees hatch from brood cells
    ask bees [ die ]
    ask breeding-habitat [
      bees-hatch specialist-food-colors specialist-brood-cells
      bees-hatch generalist-food-colors generalist-brood-cells
      set specialist-brood-cells 0
      set generalist-brood-cells 0
      set nest-amount 0
    ]

    ; flower seeds sprout and some of the old flowers die
    ask wildflowers [
      germinate-flowers
      some-flowers-die
      set seeds 0
    ]

    create-crops                     ; new crops grow regardless of pollination (as they are planted)
    ask flowers  [ flowers-compete ] ; some flowers die on overpopulated patches
    bee-birth                        ; set appearance of new bees and initialize their variables
    set specialized-bees bees with [food-color = specialist-food-colors]
    flowers-birth                    ; set appearance of new flowers and initialize their variables
    set tick-counter 0
    ]

end


; new bees hatch from brood cells with a certain chance (larvae-survival-rate)
to bees-hatch [ color-preference amount-bees ]
     repeat amount-bees [
        if random-float 100 < larvae-survival-rate [ sprout-bees 1 [ set food-color color-preference ] ]
    ]
end


; flowers (identical to the parental flower) germinate from seeds with a certain germination probability
; there is a 50% chance that the flower moves to a neighbouring patch; if this is no feeding habitat it dies
to germinate-flowers
  hatch seeds [
    if random-float 100 > germ-prob [ die ]
    if random-float 100 < 50 [
      set heading one-of [ 0 90 180 270 ]
      forward 1
      if member? patch-here agriculture or member? patch-here breeding-habitat [ die ]
    ]
  ]
end


; a certain percentage of flowers die (to simulate annuals), the others stay (to simulate perennials)
to some-flowers-die
  if random-float 100 > percent-perennials [
    die
  ]
end


; if too many flowers germinate on the same patch, some of them die
to flowers-compete
    if count flowers-here > max-flowers-per-patch [ die ]
end



; ------------------------------------------------------------------------------------------------------------
;                                              DEATH
; ------------------------------------------------------------------------------------------------------------


; crops die early and bees die if they have zero energy
to check-if-dead
  if tick-counter > lifetime-crops [
    ask crops [ die ]
  ]
  ask bees [if energy <= 0 [ die ] ]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
677
478
-1
-1
9.0
1
10
1
1
1
0
0
0
1
-25
25
-25
25
1
1
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
20
80
195
113
feeding-habitat-number
feeding-habitat-number
0
7
3.0
1
1
NIL
HORIZONTAL

SLIDER
20
115
195
148
breeding-habitat-number
breeding-habitat-number
1
7
3.0
1
1
NIL
HORIZONTAL

SLIDER
20
150
195
183
feeding-habitat-size
feeding-habitat-size
1
7
3.0
2
1
NIL
HORIZONTAL

SLIDER
20
240
192
273
min-distance-feed
min-distance-feed
1
7
2.0
1
1
NIL
HORIZONTAL

SLIDER
20
275
192
308
max-distance-feed
max-distance-feed
min-distance-feed + 1
9
4.0
1
1
NIL
HORIZONTAL

PLOT
690
10
910
165
Flower populations
Time
Number
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"wildflowers" 1.0 0 -10899396 true "" "plot count wildflowers"
"cyan flowers" 1.0 0 -11221820 true "" "plot count flowers with [color = cyan]"
"crops / 10" 1.0 0 -2674135 true "" "plot (count crops) / 10"

PLOT
690
170
910
320
Bee Population
Time
Number
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"bees (total)" 1.0 0 -16777216 true "" "plot count bees"
"specialists" 1.0 0 -11221820 true "" "plot count specialized-bees"
"generalists" 1.0 0 -2674135 true "" "plot count bees - count specialized-bees"

PLOT
690
325
910
480
Agricultural Pollination
Years
Seeds / Crop
0.0
10.0
0.0
0.1
true
false
"" ""
PENS
"yield" 1.0 1 -16777216 true "" "if tick-counter = lifetime-crops [ \n  plot ( sum [seeds] of crops / count crops )]"

SLIDER
20
185
195
218
breeding-habitat-size
breeding-habitat-size
1
7
3.0
2
1
NIL
HORIZONTAL

SLIDER
20
310
192
343
min-distance-breed
min-distance-breed
1
7
3.0
1
1
NIL
HORIZONTAL

SLIDER
20
345
192
378
max-distance-breed
max-distance-breed
min-distance-breed + 1
9
5.0
1
1
NIL
HORIZONTAL

SLIDER
15
400
190
433
percent-specialized-bees
percent-specialized-bees
0
1
0.5
0.05
1
NIL
HORIZONTAL

SLIDER
15
435
190
468
bee-number
bee-number
1
25
10.0
1
1
NIL
HORIZONTAL

TEXTBOX
60
225
210
243
Habitat Distances
11
0.0
1

TEXTBOX
65
385
215
403
Bee parameters
11
0.0
1

TEXTBOX
60
60
210
78
Habitat Parameters
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

Solitary bee populations have been declining globally, in particular due to landuse change and the intensification of agriculture, damning both native plants and crops depending on pollination. Due to solitary bees’ specific habitat requirements, agricultural fields alone are often unable to sustain them. This is especially true for solitary bees that are specialized to a specific plant and may be unable to utilize the resources of agricultural lands. Land sparing, or the practice of leaving patches of natural habitat for feeding and nesting, has been suggested to be beneficial to bee populations. Especially the size, amount and connectivity of patches may have a large impact on solitary bees. 

Therefore, this model tries to show the effect of landscape structure on solitary bee populations. It can not only be used to investigate the impact of habitat number, size and distance, but also the differences between generalist and specialist bees and the pollination outcome.


## HOW IT WORKS

### Entities
The main entities of the model are the patches, grouped into the agentsets agriculture, breeding-habitat and feeding-habitat, and two breeds of turtles: flowers and bees. Flowers are grouped into crops and wildflowers. For the bees there is an agentset specialized-bees who can only feed on the pollen of cyan-colored flowers. The other bees are considered generalists and can feed on all wildflowers and crops. 

### Setup
During the setup of the model, first, the global variables are initialized. Next, yellow patches are created to represent agricultural land. Breeding and feeding habitats are created in the number, size and distance specified by the user if possible. Flowers and crops sprout in a specific density on feeding habitat and agricultural patches respectively. The specified number of generalist and specialist bees are born on breeding habitat patches.

### Processes
The following procedures are executed within each tick when the model is running. First, bees that are currently on breeding habitat patches might create a nest if they do not have one yet and the patch is not yet populated by too many bees. They might also create a brood cell if they have enough energy. Then, all bees change direction and move one step into that direction which costs energy. The direction depends on several conditions: if a bee has reached its maximum flight distance, it turns towards its home and if it has enough energy to create another brood cell, it turns towards its nest. Else, if there is a suitable flower nearby to feed on, it turns towards that flower. Next, the bees feed on a flower/crop if there is a suitable one on their patch, gain energy and pollinate the flower. After a while, the flowers produce more pollen. It is also checked if a bee dies due to a lack of energy and if the crop lifetime is over. Lastly, it is checked if the season ends. If so, old bees die while new bees hatch from brood cells. Some wildflowers die and new wildflowers germinate from seeds. New crops sprout regardless of pollination since they are sown by the farmer. 


## HOW TO USE IT

1. Set the environmental parameters:
    - feeding-habitat-number, breeding-habitat-number: amount of feeding and breeding habitats
    - feeding-habitat-size, breeding-habitat size: size of breeding and feeding habitats, more specifically their side length (measured in patches)
    - min-distance-breed: minimum distance of breeding habitats to each other (measured in patches)
    - min-distance-feed: minimum distance of feeding habitats to breeding habitats (measured in patches)
    - max-distance-breed: maximum distance of each breeding habitat to at least one other breeding habitat
    - max-distance-feed: maximum distance of each feeding habitat to at least on breeding habitat 
(Note that the maximum distance cannot be smaller than the minimum distance. If it is, the model will automatically set the maximum distance to be minimum distance + 2.)
    - percent-specialized-bees: percentage of bees that are specialized, i.e. only feed on the pollen of cyan-colored flowers
    - bee-number: total number of bees to be born at the start  

2. Press setup.

3. Press go. 

4. Observe bee behavior in the view. Analyze flower and bee population dynamics and agricultural pollination in the corresponding plots.  
  
    If the model runs too slow or fast for your preferences, you can change the speed with the slider above the view or pause the model by pressing the go button again. You can also specify if the view should update continuously, only on ticks, or not at all.

5. Want to try different parameter values? Pause the model and repeat the steps.


## THINGS TO NOTICE

- Bee behaviour: can you notice when the bees return to reproduce or which bees are specialized?
- Can you see differences in the population dynamics of specialized and generalist bees?
- What happens when the crops die?
- Investigate the effect of the bees on agricultural pollination which ultimately determines yield for some crops.


## THINGS TO TRY

- Change the percentage of specialized bees and watch the wildflower patches.
- Right click on a bee and press "watch" to investigate the behavior of one bee more closely.
- AND MOST IMPORTANTLY: Play around with the sliders and try to find out which effect habitat number, size and distance have.


## EXTENDING THE MODEL
 
- Make the bee behavior more realistic with mating, pollination that only works if the bee visited a suitable flower before, etc.
- Add multiple specific bee and plant species including their specializations, pollen availability and lifespan.
- Create more habitat differentiation: different crops or different types of feeding habitat with different resources (e.g. forests vs meadows).
- Include switches for other conservation measures - e.g. wildflower strips and bee hotels.
- Add other natural and anthropogenic factors that limit bee population size, e.g. predators, disease, pesticides.
- Add weather.


## NETLOGO FEATURES

One workaround is used by the model: sub-breeds would have been useful, but were not available in NetLogo, so agentsets were used instead.


## RELATED MODELS

If you love bees, check out these models in the NetLogo Models Library: BeeSmart Hive Finding, Honeycomb


## COPYRIGHT AND LICENSE

Authors: Valentin Lorenz & Wanja Tolksdorf 
Copyright 2023 by Valentin Lorenz & Wanja Tolksdorf 
This work is licensed under a Creative Commons Attribution 4.0 International License (CC BY 4.0).
You are free to share and adapt this model, but have to give appropriate credit.
Full license terms: https://creativecommons.org/licenses/by/4.0/legalcode
Link to work: https://github.com/valentinlorenz/model_bees
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
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2199"/>
    <metric>count specialized-bees</metric>
    <metric>count bees - count specialized-bees</metric>
    <metric>count bees</metric>
    <metric>count crops</metric>
    <metric>count wildflowers</metric>
    <metric>count flowers with [color = cyan]</metric>
    <metric>count flowers</metric>
    <metric>sum [seeds] of crops</metric>
    <metric>count agriculture</metric>
    <metric>count feeding-habitat</metric>
    <metric>count breeding-habitat</metric>
    <enumeratedValueSet variable="breeding-habitat-number">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="feeding-habitat-number">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-specialized-bees">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="breeding-habitat-size">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-distance-breed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-distance-feed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="feeding-habitat-size">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bee-number">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-distance-breed">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-distance-feed">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
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
1
@#$#@#$#@
