#!/bin/bash 

# Groups teams 
groupa="Senegal\nQuatar\nNetherland\nEcuador"
groupb="England\nIran\nUSA\nWales"
groupc="Argentina\nSaudi Arabia\nMexico\nPoland"
groupd="France\nAustralia\nDenmark\nTunisia"
groupe="Spain\nCosta Rica\nGermany\nJapan"
groupf="Belgium\nCanada\nMorocco\nCroatia"
groupg="Brazil\nSerbia\nSwitzerland\nCameroon"
grouph="Portugal\nGhana\nUruguay\nSouth Korea"

# Globes montage 
montage Day19/wc_teams*.png -mode Concatenate -tile 3x1-5+0  Day19/wc_teams_montage.png

# Groups Labels
convert -background '#000000' Day19/wc_teams_montage.png \
-gravity southwest -splice 0x20 -fill black -pointsize 40 -font 'GothamNarrow-Bold' \
-undercolor "#C62828"   -annotate +10+10 ' Group A ' \
-undercolor "#D68400"  -annotate +460+10 ' Group B ' \
-undercolor "#FDFD00"  -annotate +910+10 ' Group C ' \
-undercolor "#00A700"  -annotate +1360+10 ' Group D ' \
-undercolor "#00C2C2"  -annotate +1810+10 ' Group E ' \
-undercolor "#297CCA"  -annotate +2260+10 ' Group F ' \
-undercolor "#9048A4"  -annotate +2710+10 ' Group G ' \
-undercolor "#E5BAE5"  -annotate +3160+10 ' Group H ' Day19/wc_teams_montage.png

# Groups Teams
convert -background '#000000' Day19/wc_teams_montage.png \
-gravity southwest -splice 0x165 -fill white -pointsize 40 -font 'GothamNarrow-Bold' \
-annotate +10+5 "$groupa" \
-annotate +460+5 "$groupb" \
-annotate +910+5 "$groupc" \
-annotate +1360+5 "$groupd" \
-annotate +1810+5 "$groupe" \
-annotate +2260+5 "$groupf" \
-annotate +2710+5 "$groupg" \
-annotate +3160+5 "$grouph" Day19/wc_teams_montage.png