(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomSmooth implementation *)
ClearAll[geomSmooth];
geomSmooth[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statSmooth];
  geomFunc = Lookup[Association[opts], "geom", geomSmoothRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomSmoothRender] = {"data" -> {}, "x" -> Null, "y" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "fill" -> Null, "lineAlpha"->Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomSmoothRender[statData_, opts : OptionsPattern[]] := Module[{output, lineOpts, bandOpts},
  (* statData is a single group - a list of associations from statSmooth *)
  (* Use standardized string mappings for processed stat data *)
  
  lineOpts = Normal@Join[
    Association@FilterRules[{opts}, Options[geomLineRender]],
    <|"x" -> "x", "y" -> "y"|>  (* These override any original mappings *)
  ];
  
  bandOpts = Normal@Join[
    Association@FilterRules[{opts}, Options[geomBandRender]],
    <|"x" -> "x", "ymin" -> "ymin", "ymax" -> "ymax"|>  (* These override any original mappings *)
  ];
  
  output = Join[geomLineRender[statData, lineOpts], geomBandRender[statData, bandOpts]];
  output
];

End[];

EndPackage[];
