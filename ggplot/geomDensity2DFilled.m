(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-02 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomDensity2DFilled implementation *)
ClearAll[geomDensity2DFilled];
geomDensity2DFilled[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statDensity2D];
  geomFunc = Lookup[Association[opts], "geom", geomDensity2DFilledRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomDensity2DFilledRender] = {
  "data" -> {}, 
  "x" -> Null, 
  "y" -> Null, 
  "color" -> Null, 
  "alpha" -> Null, 
  "fill" -> Null,
  "xScaleFunc" -> Function[Identity[#]], 
  "yScaleFunc" -> Function[Identity[#]]
};

geomDensity2DFilledRender[statData_, opts : OptionsPattern[]] := Module[{output},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, 
    Message[ggplot::xOrYNotGiven]; Throw[Null]
  ];

  (* statData is a single group - a list of associations from statDensity2D *)
  (* Create density-filled rectangles for each density point *)
  output = statData // Map[Function[row,
    Module[{colorDir, fillDir, alphaDir, xpos, ypos, density, level},
      colorDir = Lookup[row, "color_aes", Black];
      fillDir = Lookup[row, "fill_aes", Lookup[row, "color_aes", Black]];
      alphaDir = Lookup[row, "alpha_aes", Opacity[1]];
      
      xpos = OptionValue["xScaleFunc"][Lookup[row, OptionValue["x"]]];
      ypos = OptionValue["yScaleFunc"][Lookup[row, OptionValue["y"]]];
      density = Lookup[row, "density", 0];
      level = Lookup[row, "level", 1];
      
      (* Create filled rectangle representing this density point *)
      (* Use density for transparency and level for size *)
      {EdgeForm[{colorDir, alphaDir}], 
       fillDir, 
       Opacity[Min[1, density/10]], (* Scale density for visibility *)
       Rectangle[{xpos - 0.05 * level, ypos - 0.05 * level}, 
                {xpos + 0.05 * level, ypos + 0.05 * level}]
      }
    ]
  ]];

  output
];

End[];

EndPackage[];
