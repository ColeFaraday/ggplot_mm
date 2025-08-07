(* Mathematica Source File *)
(* :Author: colefaraday *)
(* :Date: 2025-08-02 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomDensity2DFilled implementation *)
ClearAll[geomDensity2DFilled];
geomDensity2DFilled[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := <|
  "stat" -> statDensity2D,
  "geom" -> geomDensity2DFilledRender,
  "statParams" -> FilterRules[{opts}, Options[statDensity2D]],
  "geomParams" -> FilterRules[{opts}, Options[geomDensity2DFilledRender]]
|>;

Options[geomDensity2DFilledRender] = {"data" -> {}, "x" -> Null, "y" -> Null, "color" -> Null, "alpha" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomDensity2DFilledRender[statData_, opts : OptionsPattern[]] := Module[{output},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, 
    Message[ggplot::xOrYNotGiven]; Throw[Null]
  ];

  (* Create density-filled rectangles for each density point *)
  output = statData // Map[Function[row,
    Module[{colorDir, alphaDir, xpos, ypos, density},
      colorDir = row["color_aes"];
      alphaDir = Opacity[row["density"] / 100]; (* Use density for alpha *)
      xpos = OptionValue["xScaleFunc"][row["x"]];
      ypos = OptionValue["yScaleFunc"][row["y"]];
      density = row["density"];
      
      (* Create a small rectangle representing this density point *)
      {colorDir, alphaDir, 
        Rectangle[{xpos - 0.1, ypos - 0.1}, {xpos + 0.1, ypos + 0.1}]
      }
    ]
  ]];

  output
];

End[];

EndPackage[];
