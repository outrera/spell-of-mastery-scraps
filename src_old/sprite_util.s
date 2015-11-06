use gfx

draw_iso_line Color X,Y Size Step Axis G =
| ICount = Size.abs
| JCount = Step.abs
| S = Size.sign
| if Axis >< 0
  then times I ICount: times J JCount: G.set{X+I*Step+J Y+I*S Color}
  else times I ICount: times J JCount: G.set{X+I*S Y+I*Step+J Color}

generate_base_tile Fill XUnit YUnit ZUnit =
| Color = #00a0a0
| A = [XUnit/2  0]
| B = [0        YUnit/2]
| C = [XUnit/2  YUnit]
| D = [XUnit    YUnit/2]
| G = gfx XUnit YUnit+2
| G.clear{#FF000000}
| G.xy <= 0,-ZUnit
| when Fill
  | G.triangle{Color A B C}
  | G.triangle{Color A B D}
  | G.triangle{Color B C D}
| draw_iso_line 0 [0 YUnit/2] -YUnit/2 2 0 G
| draw_iso_line 0 [XUnit-2 YUnit/2] -YUnit/2 -2 0 G
| draw_iso_line 0 [0 YUnit/2] YUnit/2 2 0 G
| draw_iso_line 0 [XUnit-2 YUnit/2] YUnit/2 -2 0 G
| G

//| Base = generate_base_tile $params.editor.opaque_base 64 32 8
//| Sprites.tiles_base_ <= sprite tiles base_ frames/[Base]

export generate_base_tile