type zmap{size depth init} data
| $data <= dup $size: dup $size: dup $depth $init

zmap.clear Value = for Ys $data: for Zs Ys: Zs.clear{Value}

zmap.set X Y Z V = $data.X.Y.Z <= V

zmap.at X Y Z = $data.X.Y.Z

zmap.pilar_at X Y = $data.X.Y

zmap.setPilar X Y Zs =
| Vs = $data.X.Y
| Vs.clear{$init}
| times I Zs.size: Vs.I <= Zs.I

export zmap