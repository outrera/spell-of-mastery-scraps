type unit_panel.widget{ui} w/0 h/0 unit bg
| $bg <= $ui.img{ui_unit_panel}
unit_panel.set_unit Unit =
| $unit <= Unit
| if $unit
  then | $w <= $bg.w
       | $h <= $bg.h
  else | $w <= 0
       | $h <= 0

unit_panel.draw G X Y =
| less $unit: leave
| Main = $unit.main
| G.blit{X Y $bg}
| Icon = Main.sprites."icons_[$unit.icon or $unit.type]"
//| IconBg = Main.sprites.ui_icon_bg
//| G.blit{X+2 Y+35 IconBg.frames.0}
| when got Icon: G.blit{X+2 Y+35 Icon.frames.0}
| Font = font medium
//| Font.draw{G X+85 Y+48 "[$unit.owner.name]"}
| Font.draw{G X+4 Y+75 "[$unit.title]"}
| Font.draw{G X+27 Y+14"[max 0 $unit.health]"}
| Font.draw{G X+67 Y+28"[$unit.combat]"}
| Font.draw{G X+67 Y+57"[$unit.armor]"}
| when Main.params.ui.debug><1:
  | Font.draw{G X+3 Y-16 "sn:[max 0 $unit.serial]"}
  | Font.draw{G X+3 Y-32 "id:[max 0 $unit.id

| UnitPanel <= unit_panel Me


| GameUnitUI <= hidden: dlg: mtx
  | 0  0 | UnitPanel

  |  0 $height-136-UnitPanel.bg.h| GameUnitUI

| if Units.size><1
  then | GameUnitUI.show <= 1
       | UnitPanel.set_unit{Units.0}
  else | GameUnitUI.show <= 0
       | UnitPanel.set_unit{0}
