use gfx gui


type test.widget{sprite} mx my

test.input In =
| case In
  [mice_move _ X,Y]
    | $mx <= X*255/$sprite.w-127
    | $my <= Y*255/$sprite.h-127

test.draw FB BX BY =
| S = $sprite
| FB.clear{#B000B0}
| FB.blit{0 0 S.light{$mx $my}}

test.w = $sprite.w
test.h = $sprite.h

BG = gfx '/Users/nikita/Downloads/work/bump/bg6.png'
CH = gfx '/Users/nikita/Downloads/work/bump/ch.png'
T = test BG

gui T cursor/CH
