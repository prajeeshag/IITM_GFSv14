#!/bin/bash

for i in {27..240..24}
   do 
convert RAIN_TCO-$i.png -gravity North -chop 0x150 -gravity South -chop 0x150 RAIN_TCO-$i.png
     done

for i in {0..240..3}
  do
convert MSLP_TCO-$i.png -gravity North -chop 0x150 -gravity South -chop 0x150 MSLP_TCO-$i.png
convert GH500_TCO-$i.png -gravity North -chop 0x150 -gravity South -chop 0x150 GH500_TCO-$i.png
convert WIND200_TCO-$i.png -gravity North -chop 0x150 -gravity South -chop 0x150 WIND200_TCO-$i.png
convert WIND850_TCO-$i.png -gravity North -chop 0x150 -gravity South -chop 0x150 WIND850_TCO-$i.png
   done

convert -delay 120 -loop 0 RAIN_TCO-{27..240..24}.png  RAIN.gif
convert -delay 120 -loop 0 MSLP_TCO-{0..240..3}.png  MSLP.gif
convert -delay 120 -loop 0 GH500_TCO-{0..240..3}.png  GH500.gif
convert -delay 120 -loop 0 WIND200_TCO-{0..240..3}.png  wind200.gif
convert -delay 120 -loop 0 WIND850_TCO-{0..240..3}.png  wind850.gif

