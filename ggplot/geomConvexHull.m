(* Mathematica Source File *)
(* geomConvexHull: draws the convex hull of a set of points *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomConvexHull implementation *)
ClearAll[geomConvexHull];
geomConvexHull[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := <|
  "stat" -> statConvexHull,
  "geom" -> geomPathRender, (* Reuse geomPath rendering since it's just a connected line *)
  "statParams" -> FilterRules[{opts}, Options[statConvexHull]],
  "geomParams" -> FilterRules[{opts}, Options[geomPathRender]]
|>;
End[];
EndPackage[];
