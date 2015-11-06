use gfx gui

type test.widget{bg} shift_x

test.input In = case In [mice_move _ X,Y] | $shift_x <= X*255/$bg.w

rgb R G B = form R*#10000 + G*#100 + B

rgb_r C = (C/#10000)^^#FF
rgb_g C = (C/#100)^^#FF
rgb_b C = C^^#FF

test.draw FB BX BY =
| W = $bg.w
| H = $bg.h
| BG = $bg
| FB.clear{#B000B0}
| J = 1
| while J < H:
  | I = 1
  | while I < W:
    | C = BG.get{I J}
    | R = rgb_r{C}
    | G = rgb_g{C}
    | B = rgb_b{C}
    | when R<2: R<=2
    | when G<2: G<=2
    | when B<2: B<=2
    | M = (R+G+B)/3
    | N = 255-M
    | V = M*$shift_x/255 + N*(255-$shift_x)/255
    | R <= R*V/M
    | G <= G*V/M
    | B <= B*V/M
    | when R>255: R<=255
    | when G>255: G<=255
    | when B>255: B<=255
    | when R<0: R<=0
    | when G<0: G<=0
    | when B<0: B<=0
    | less C^^#FF000000: FB.set{I J rgb{R G B}}
    | !I+1
  | !J+1

test.w = $bg.w
test.h = $bg.h

CH = gfx 'crosshair.png'
T = test: gfx 'test_image.png'
gui T cursor/CH