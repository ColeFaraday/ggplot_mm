(* Mathematica Source File *)
(* statConvexHull: returns convex hull points for a set of x/y pairs *)
BeginPackage["ggplot`"];
Begin["`Private`"];
statConvexHull[data_, opts___] := Module[{points, hullRegion, hullPoints},
  points = Values@data;
	Print[points];
  If[AssociationQ[data[[1]]],
    points = data[[All, {"x", "y"}]] // Values
  ];
  hullRegion = ConvexHullRegion[points];
  hullPoints = hullRegion[[1]];
  hullPoints
]
End[];
EndPackage[];
