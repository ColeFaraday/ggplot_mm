(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomLine implementation *)
ClearAll[geomLine];
geomLine[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statIdentity];
  geomFunc = Lookup[Association[opts], "geom", geomLineRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomLineRender] = {"data" -> {}, "x" -> Null, "y" -> Null, "group" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "dashing" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomLineRender[statData_, opts : OptionsPattern[]] := Module[{output, xvals, yvals, pairs, sortedPairs, scaledPairs, sortedData, uniformAesthetics, colors, alphas, thicknesses},
  (* statData is a single group - a list of associations *)
  
  xvals = extractMappedValues[statData, OptionValue["x"]];
  yvals = extractMappedValues[statData, OptionValue["y"]];
  pairs = Transpose[{xvals, yvals}];
  sortedPairs = SortBy[pairs, First];
  scaledPairs = Map[{OptionValue["xScaleFunc"][#[[1]]], OptionValue["yScaleFunc"][#[[2]]]} &, sortedPairs];
  
  (* Sort data to match sorted pairs *)
  sortedData = statData[[Ordering[xvals]]];
  
  (* Extract aesthetics in sorted order *)
  colors = sortedData[[All, "color_aes"]];
  alphas = sortedData[[All, "alpha_aes"]];
  thicknesses = sortedData[[All, "thickness_aes"]];
  
  (* Check if aesthetics are uniform across all points *)
  uniformAesthetics = Length[DeleteDuplicates[colors]] == 1 && 
                      Length[DeleteDuplicates[alphas]] == 1 && 
                      Length[DeleteDuplicates[thicknesses]] == 1;
  
  If[uniformAesthetics,
    (* Uniform aesthetics: use single connected line *)
    output = {First[colors], First[alphas], First[thicknesses], Line[scaledPairs]},
    
    (* Variable aesthetics: use VertexColors for smooth transitions *)
    Module[{vertexColors},
      vertexColors = MapThread[Directive[#1, #2, #3] &, {colors, alphas, thicknesses}];
      output = Line[scaledPairs, VertexColors -> vertexColors]
    ]
  ];
  
  output

];

End[];

EndPackage[];
