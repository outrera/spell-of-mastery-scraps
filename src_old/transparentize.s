// converts tile into checkboard patter alpha
transparentize Gfx =
| Empty = 255
| as R Gfx.copy
  | for X,Y points{0 0 64 64}:
    | when X&&&1 >< Y&&&1: R.set{X Y Empty}