calc_floor Cell =
| till Cell.tile.empty: !Cell+1
| !Cell-1
| while Cell.tile.empty: !Cell-1
| !Cell+1
| Cell

calc_height Me Pilar = 
| Low = Pilar-1
| Cell = Low + $d
| while Low < Cell:
  | when CellsTile.Cell.id: leave Cell-Low
  | !Cell-1
| 0