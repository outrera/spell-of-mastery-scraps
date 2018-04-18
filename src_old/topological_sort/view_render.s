use gfx gui util widgets macros isort_ unit_flags stack

ScreenXY = [0 0]
BrightFactor = 0
BlitItems = 0
BlitUnits = 0
XUnit2 =
YUnit2 =
XUnit =
YUnit =
ZUnit =
CS =
CS2 =
Folded = 0
Marked = 0
Unexplored = 0


to_iso X Y Z = [X-Y (X+Y)/2-Z]

/* Bounding Box Format:
           1    
         /   \      
       /       \ 
     2           3 
     | \   8   / |
     |   \   /   |
     |     4     |
     | /   |   \ |
     5     |     6
       \   |   /
         \ | /    
           7
*/

draw_bounding_box_front Color FB B =
| ZD = B.z2-B.z
| P = ScreenXY
| P2 = to_iso{B.x2 B.y B.z} - [0 ZD] + P
| P3 = to_iso{B.x B.y2 B.z} - [0 ZD] + P
| P4 = to_iso{B.x B.y B.z} - [0 ZD] + P
| P5 = to_iso{B.x2 B.y B.z} + P
| P6 = to_iso{B.x B.y2 B.z} + P
| P7 = to_iso{B.x B.y B.z} + P
| for A,B [P2,P4 P4,P3 P3,P6 P6,P7 P7,P5 P5,P2 P4,P7]
  | FB.line{Color A B}


draw_bounding_box_back Color FB B =
| ZD = B.z2-B.z
| P = ScreenXY
| P1 = to_iso{B.x2 B.y2 B.z} - [0 ZD] + P
| P2 = to_iso{B.x2 B.y B.z} - [0 ZD] + P
| P3 = to_iso{B.x B.y2 B.z} - [0 ZD] + P
| P5 = to_iso{B.x2 B.y B.z} + P
| P6 = to_iso{B.x B.y2 B.z} + P
| P8 = to_iso{B.x2 B.y2 B.z} + P
| for A,B [P2,P1 P1,P3 P3,P6 P6,P8 P8,P5 P5,P2 P1,P8]
  | FB.line{Color A B}

type special_blit{what}

special_blit.draw FB BlitItem =
| if $what >< box_front then draw_bounding_box_front #00FF00 FB BlitItem
  else if $what >< box_back then draw_bounding_box_back #FF0000 FB BlitItem
  else

draw_text FB X Y Msg =
| Font = font small
| ZB = FB.zbuffer
| FB.zbuffer <= 0
| Font.draw{FB X Y Msg}
| FB.zbuffer <= ZB

type blit_item{object x y z x2 y2 z2}
  id
  data
  sx sy // screen x,y
  flags
  brighten
  lx ly
  deps/[] //what items must be drawn before this one
  cover/[] //what itms must be drwan after this one

make_blit_item X Y Z XD YD ZD Object =
| blit_item Object X Y Z X-XD/2 Y-YD/2 Z+ZD

blit_item_from_unit Me =
| X,Y,Z = $fxyz
| DX,DY = $box_xy
| DDX = (DX+2*DY)/2
| DDY = 2*DY-DDX
| X += DDX
| Y += DDY
| XD,YD,ZD = $size
| when $mirror: swap XD YD
| BI = make_blit_item X Y Z+7 XD YD ZD Me //Z+7 is a hack to avoid cursor cluttering
| $blitem <= BI
| BI

UnitRects = 0
PickedRects = 0

unit.draw FB B =
| X = B.sx
| Y = B.sy
| G = $frame
| GW = G.w
| XX = X+XUnit2-GW/2
| YY = Y-YUnit2-G.h
| YY += $zhack
| when $mirror:
  | XX -= GW%2
  | G.flop
| S = $sprite
| when S.shadow:
  | S = $site.shadow
  | ZZ = $cell-$floor
  | I = min (ZZ/16).abs S.size-1
  | SGfx = S.I
  | SGfx.brighten{B.brighten}
//  | SGfx.light{B.lx B.ly}
  | FB.blit{X+8 Y-38+ZZ*ZUnit SGfx}
| Colors = $colors
| when Colors:
  | Rs = S.colors
  | when Rs:
    | CM = G.cmap{raw/1}
    | for I 5:
      | R = Rs.I
      | when got R: _ffi_set uint32_t CM R Colors.I
| when $flyer
  | YY -= 16
  | Y -= 16
| G.brighten{B.brighten}
//| G.light{B.lx B.ly}
| G.alpha{$alpha}
| FB.blit{XX YY G}
| less $pickable: leave
| RW,RH,RY = $sprite.rect
| RX = X+XUnit2 - RW/2
| RY = Y+RY-RH
| UnitRects.push{[RX RY RW RH],Me}
| less $picked: leave
| PickedRects.push{[RX RY RW RH],Me}
| $blitem <= 0


PickCorner = 0

draw_bar FB BGColor FGColor X Y W H Cur Max =
| Filled = min{W W*Cur/max{1 Max}}
| FB.rectangle{FGColor 1 X Y Filled H}
| FB.rectangle{BGColor 1 X+Filled Y W-Filled H} 

draw_picked_rects FB PickedRects =
| for [RX RY RW RH],Me PickedRects
  | less PickCorner:
    | PickCorner <= $main.img{ui_picked_corner}
  | PW = PickCorner.w
  | PH = PickCorner.h
  | FB.blit{RX RY PickCorner}
  | FB.blit{RX+RW-PW RY PickCorner.flop}
  | FB.blit{RX RY+RH-PW PickCorner.flip}
  | FB.blit{RX+RW-PW RY+RH-PW PickCorner.flip.flop}
  //| draw_bar FB #000000 #00FF00 RX RY+RH RW 4 $health $class.hp
  | Icons = []
  | for [_ Flag Icon] getUnitFlags{}: when Icon>>0:
    | when $flags^get_bit{Flag}: push Icon Icons
  | when Icons.size
    | XX = RX + RW/2 - Icons.size*8
    | YY = RY - 16
    | Fs = $main.effect.frames
    | for I Icons
      | F = Fs.I
      | FB.blit{XX YY F}
      | XX += 16

tile.draw FB BlitItem =
//| leave
| B = BlitItem
| G,Cell = B.data
| Cell.blitem <= 0
| when B.flags&&&#40: G.dither{1}
| G.brighten{B.brighten}
//| G.light{B.lx B.ly}
| FB.blit{B.sx B.sy G}
| for U B.cover:
  | UB = U.blitem
  | UB.deps <= UB.deps.skip{Cell}
  | when UB.deps.end: U.draw{FB UB}

type gfx_item

gfx_item.draw FB BlitItem =
| B = BlitItem
| G = B.data
| when B.flags&&&#40: G.dither{1}
| FB.blit{B.sx B.sy G}

render_cursor Me Wr BX BY CursorXYZ =
| X,Y,CurZ = CursorXYZ
| Z = 0
| UnitZ = 0
| EndZ = min CurZ Wr.height{X Y}
| Cell = Wr.cell{X Y 0}
| while Z < EndZ:
  | G = Cell.gfx
  | T = Cell.tile
  | TH = T.height
  | Cell += TH
  | when G.is_list: G <= G.((Wr.cycle/T.anim_wait)%G.size)
  | UnitZ <= Z + TH
  | TH = T.height
  | ZZ = Z*ZUnit
  | GH = if G then G.h else YUnit
  | B = make_blit_item X*CS-2 Y*CS-2 Z*CS CS2 CS2 TH*CS
                       special_blit{box_back}
  | B.sx <= BX
  | B.sy <= BY-GH-ZZ
  | push B BlitItems
  | B = make_blit_item X*CS Y*CS Z*CS+2 CS2 CS2 TH*CS
                       special_blit{box_front}
  | B.sx <= BX
  | B.sy <= BY-GH-ZZ
  | push B BlitItems
  | Z <= UnitZ

render_pilar Me Wr X Y BX BY CursorXYZ RoofZ Explored =
| DrawnFold = 0
| CurX,CurY,CurZ = CursorXYZ
| CurH = (CurX+CurY)/2
| XY2 = (X+Y)/2
| AboveCursor = CurH >> XY2
| ZCut = max CurZ 0
| Z = 0
| UnitZ = 0
| Fog = Explored><1
| Br = @int -([CurX CurY]-[X Y]).abs
| Br *= BrightFactor
| LXY = (to_iso{X*8 Y*8 0}-to_iso{CurX*8 CurY*8 0}){?float}
| LXY = LXY{?int} ///(LXY*256.0/LXY.abs){?int}
| LX,LY = LXY
| LX = LX.clip{-127 127}
| LY = LY.clip{-127 127}
| SkipZ = -1//if $brush.0 then -1 else 0
| Us = Wr.column_units_get{X Y}
| when Fog: Us <= Us.skip{(?owner.id or ?class.hp or ?bank><effect)}
//| draw_text FB BX+XUnit2 BY-ZUnit*Z-20 "[Explored]"
| for U Us:
  | if U.frame.w > 1 then
    | XYZ = U.xyz
    | UX,UY,Z = XYZ
    | TZ = Z-1
    | when TZ < RoofZ and (AboveCursor or TZ << ZCut) and UX><X and UY><Y:
      | when not U.invisible or U.owner.id><$player.id or $brush.0:
        | B = blit_item_from_unit U
        | FX,FY,FZ = U.fxyz
        | BX,BY = ScreenXY + to_iso{FX FY FZ}
        | B.sx <= BX - XUnit2
        | B.sy <= BY
        | B.lx <= LX
        | B.ly <= LY
        | B.brighten <= Br
        | push B BlitItems
        | push U BlitUnits
    else
| Cell = Wr.cell{X Y 0}
| EndZ = min RoofZ Wr.height{X Y}
| while Z < EndZ:
  | G = Cell.gfx
  | T = Cell.tile
  | TH = T.height
  | ZZ = Z*ZUnit
  | when G.is_list: G <= G.((Wr.cycle/T.anim_wait)%G.size)
  | UnitZ <= Z + TH
  | TZ = UnitZ - 1
  | less T.invisible
    | G = G
    | if AboveCursor or TZ << ZCut then
      else if not DrawnFold then
        | DrawnFold <= 1
        | G <= Folded
      else G <= 0
    | when G and Z>SkipZ:
      | Box = T.box
      | B = make_blit_item X*CS Y*CS Z*CS Box.0 Box.1 Box.2 T
      | B.data <= G,Cell
      | B.sx <= BX
      | B.sy <= BY-G.h-ZZ
      | B.lx <= LX
      | B.ly <= LY
      | B.brighten <= Br
      //| B.brighten <= LM.at{X Y Z}
      | when Fog: B.flags <= #40 //dither
      | push B BlitItems
      | Cell.blitem <= B
  | Cell += TH
  | Z <= UnitZ

render_unexplored Me Wr X Y BX BY =
| B = make_blit_item X*CS Y*CS 0 CS2 CS2 CS gfx_item{}
| B.data <= Unexplored
| B.sx <= BX
| B.sy <= BY-$zunit-Unexplored.h
| push B BlitItems

colorize G Layer Color =
| Alpha = Color >>> 24
| Color <= Color &&& #FFFFFF
| Layer.cmap <= dup 256 Color
| LW = Layer.w
| LH = Layer.h
| WN = (G.w+LW-1)/LW
| HN = (G.h+LH-1)/LH
| times X LW: times Y LH:
  | Layer.alpha{Alpha}
  | G.blit{X*LH Y*LH Layer}

draw_overlay FB Wr =
| CO = Wr.color_overlay
| when CO.end: leave
| S = Wr.color_overlay_step
| Wr.color_overlay_step <= S+1
| K = 0
| for (I = 0; K<CO.size; K++):
  | D = CO.K.0
  | when I << S and S < I+D: done
  | I += D
| when K><CO.size:
  | Wr.set_color_overlay{[]}
  | leave
| colorize FB Wr.main.img{"ui_colorizer"} CO.K.1

ShakeXY = [[10 10] [0 10] [0 -10] [0 0] [-10 -10] [10 0] [-10 0]]

unit.add_dep Cell =
| when Cell.empty: leave
| CB = Cell.blitem
| when not CB: leave
| push Cell $blitem.deps
| push Me CB.cover

view.find_blit_deps =
| for U BlitUnits:
  | X,Y,ZZ = U.xyz
  | C = U.cell+1
  | Z = ZZ+1
  | EndZ = min $d Z+3
  | while C.empty and Z<EndZ:
    | U.add_dep{$site.cell{X-1 Y Z}}
    | U.add_dep{$site.cell{X Y-1 Z}}
    | U.add_dep{$site.cell{X-1 Y-1 Z}}
    | C++
    | Z++
  | C = $site.cell{X Y+1 ZZ}+1
  | EndZ <= Z
  | Z <= ZZ+1
  | while C.empty and Z<EndZ:
    | U.add_dep{$site.cell{X-1 Y+1 Z}}
    | C++
    | Z++
  | C = $site.cell{X+1 Y ZZ}+1
  | EndZ <= Z
  | Z <= ZZ+1
  | while C.empty and Z<EndZ:
    | U.add_dep{$site.cell{X+1 Y-1 Z}}
    | C++
    | Z++

view.render_iso =
| Wr = $site
| BlitItems <= []
| BlitUnits <= []
| PickedRects <= stack 256
| UnitRects <= stack 1024
| Explored = Wr.human.sight
| FB = $fb
| Z = if $mice_click then $anchor.2 else $cursor.2
| RoofZ = Wr.roof{$cursor}
| CurX,CurY,CurZ = $cursor
| XUnit <= $xunit
| YUnit <= $yunit
| ZUnit <= $zunit
| XUnit2 <= XUnit/2
| YUnit2 <= YUnit/2
| CS <= $d
| CS2 <= CS*2
| TX,TY = $blit_origin + [0 YUnit] + [0 Z]*ZUnit
| VX,VY = $view_origin
| when Wr.cycle < Wr.shake_end:
  | D = Wr.cycle - Wr.shake_start
  | ShkX,ShkY = ShakeXY.(D%ShakeXY.size)
  | TX += ShkX
  | TY += ShkY
| ScreenXY.init{[TX+XUnit2 TY]+to_iso{-VX*XUnit2 -VY*YUnit 0}}
| WW = Wr.w
| WH = Wr.h
| VS = $view_size
| less Folded:
  | Folded <= Wr.main.img{ui_cell_folded}
  | Marked <= Wr.main.img{ui_cell_marked}
  | Unexplored <= Wr.main.img{ui_cell_unexplored}
| times YY VS
  | Y = YY + VY
  | when 0<Y and Y<<WH: times XX VS:
    | X = XX + VX
    | when 0<X and X<<WW: // FIXME: move this out of the loop
      | BX = TX + XX*XUnit2 - YY*XUnit2
      | BY = TY + XX*YUnit2 + YY*YUnit2
      | E = Explored.Y.X
      | if E then render_pilar Me Wr X Y BX BY $cursor RoofZ E
        else render_unexplored Me Wr X Y BX BY
| BX = TX + VY + CurX*XUnit2 - CurY*XUnit2
| BY = TY + VY + CurX*YUnit2 + CurY*YUnit2
| when $mice_click<>left or $brush.0:
  | render_cursor Me Wr BX BY $cursor
| less BlitItems.end
  | $find_blit_deps
  | Xs = BlitItems{B=>[(B.x+B.y+B.z)*1000 - B.z B]}
  | Xs <= Xs.sort{A B => A.0<B.0}
  | for X,B Xs: when B.deps.end:
    | O = B.object
    | O.draw{FB B}
/*| less BlitItems.end
  | DrawBoundingBox = $main.cfg.site.bounding_boxes><1
  | BL = BlitItems.list
  | isort_begin
  | for I,B BL.i: isort_add I 0 B.x B.y B.z B.x2 B.y2 B.z2
  | ResultSize = isort_end
  | Result = isort_result
  | less DrawBoundingBox: for I ResultSize:
      | N = _ffi_get int Result I
      | B = BL.N
      | O = B.object
      | O.draw{FB B}
  | when DrawBoundingBox: for I ResultSize:
      | N = _ffi_get int Result I
      | B = BL.N
      | O = B.object
      | ZD = B.z2-B.z
      | Color = if O.is_unit then #0000FF else #00FF00
      | draw_bounding_box_back Color FB B
      | O.draw{FB B}
      | draw_bounding_box_front Color FB B
  | isort_free_result*/
| draw_picked_rects FB PickedRects.list.flip
| draw_overlay FB Wr
| less $brush.0: $handle_pick{UnitRects.list.flip}
| BlitItems <= 0
| BlitUnits <= 0
| UnitRects <= 0

view.render_frame =
| IsNight = $site.data.night><1
| BrightFactor <= if IsNight then 10 else 0
//| $fb.clear{#929292/*#00A0C0*/}
| $fb.blit{0 0 $main.img{ui_stars}}
| $render_iso
| InfoText = []
| when $cfg.show_frame: push "frame=[$frame]" InfoText
| when $cfg.show_cycle: push "cycle=[$site.cycle]" InfoText
| when $cfg.show_fps: push "fps=[$fps]" InfoText
| $infoText.value <= InfoText.infix{'; '}.text
| $infoText.render.draw{$fb 200 ($h-50)}
| $infoText.value <= ''

// calculates current framerate and adjusts sleeping accordingly
view.calc_fps StartTime FinishTime =
| when $frame%24 >< 0
  | T = StartTime
  | $fps <= @int 24.0/(T - $fpsT)
  | when $fps < $fpsGoal and $fpsD < $fpsGoal.float*2.0: $fpsD += 1.0
  | when $fps > $fpsGoal and $fpsD > $fpsGoal.float/2.0: $fpsD -= 1.0
  | $fpsT <= T
| $frame++
| SleepTime = 1.0/$fpsD - (FinishTime-StartTime)
| SleepTime

view.draw FB X Y =
| $fb <= FB
| GUI = get_gui
| StartTime = GUI.ticks
| $update
| $render_frame
| FinishTime = GUI.ticks
| when $wakeupTime<<FinishTime:
  | SleepTime = $calc_fps{StartTime FinishTime}
  //| when SleepTime > 0.0: get_gui{}.sleep{SleepTime}
| $fb <= 0 //no framebuffer outside of view.draw

view.render = Me
