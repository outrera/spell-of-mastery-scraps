use gfx gui

get_lightmap R =
| G = gfx R R
| R2 = @float R/2
| Center = [R/2 R/2]
| for Y R: for X R:
  | V = 255-(([X Y]-Center).abs*255.0/R2).int
  | when V < 0: V <= 0
  | when V > 255: V <= 255
  | G.set{X Y V}
| G

type test.widget{bg bumpmap} light lmx lmy
| $light <= get_lightmap 256

test.input In =
| case In
  [mice_move _ X,Y]
    | $lmx <= X
    | $lmy <= Y

rgb R G B = form R*#10000 + G*#100 + B

rgb_r C = (C/#10000)^^#FF
rgb_g C = (C/#100)^^#FF
rgb_b C = C^^#FF

invert_gfx G =
| for Y G.h: for X G.w:  G.set{X Y 255-G.get{X Y}}
| G

test.draw FB BX BY =
| FactorA = 2
| FactorB = 3
| W = $bg.w
| H = $bg.h
| BG = $bg
| BM = $bumpmap
| Light = $light
| LW = Light.w
| LW2 = Light.w/2
| LH2 = Light.h/2
| FB.clear{#B000B0}
| J = 1
| LMX = $lmx*Light.w/W
| LMY = $lmy*Light.h/H
| while J < H:
  | I = 1
  | while I < W:
    | Intensity = 0
    | BMV = BM.get{I J}/2 + LW2
    | DX = BMV - BM.get{I-1 J}//*FactorA/FactorB
    | DY = BMV - BM.get{I J-1}//*FactorA/FactorB
    //| DX = DX + I-$lmx
    //| DY = DY + J-$lmy
    | DX = DX + LW2 - LMX
    | DY = DY + LH2 - LMY
    | when 0 << DX and DX < Light.w and 0 << DY and DY < Light.h:
      | Intensity <= Light.get{DX DY}
    | C = BG.get{I J}
    | R = rgb_r{C}
    | G = rgb_g{C}
    | B = rgb_b{C}
    | R <= R*Intensity/127
    | G <= G*Intensity/127
    | B <= B*Intensity/127
    | when R>255: R<=255
    | when G>255: G<=255
    | when B>255: B<=255
    | when R<0: R<=0
    | when G<0: G<=0
    | when B<0: B<=0
    | CC = rgb{R G B}
    //| CC = CC ^^ #C0C0C0
    //| FB.set{I J CC}
    | less C^^#FF000000: FB.set{I J CC}
    | !I+1
  | !J+1

test.w = $bg.w
test.h = $bg.h

BG = gfx '/Users/nikita/Downloads/work/bump/bg3.png'
BM = gfx '/Users/nikita/Downloads/work/bump/bm3.png'
//BM = invert_gfx BM
CH = gfx '/Users/nikita/Downloads/work/bump/ch.png'
T = test BG BM

gui T cursor/CH
