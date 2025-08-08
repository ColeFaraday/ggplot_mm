(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomHistogram implementation *)
ClearAll[geomHistogram];
geomHistogram[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statBin];
  geomFunc = Lookup[Association[opts], "geom", geomHistogramRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomHistogramRender] = {"data" -> {}, "x" -> Null, "y" -> "count", "color" -> Null, "alpha" -> Null, "lineAlpha" -> Null, "filled" -> True, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomHistogramRender[statData_, opts : OptionsPattern[]] := Module[{output, filled},
  filled = OptionValue["filled"];
  
  If[filled,
    (* Standard filled rectangles *)
    output = statData // Map[Function[row,
      Module[{colorDir, alphaDir, lineAlphaDir, xmin, xmax, yval, pos1, pos2},
        colorDir = row["color_aes"];
        alphaDir = row["alpha_aes"];
        lineAlphaDir = Lookup[row, "lineAlpha_aes", Opacity[1]];
        xmin = OptionValue["xScaleFunc"][row["xmin"]];
        xmax = OptionValue["xScaleFunc"][row["xmax"]];
        yval = OptionValue["yScaleFunc"][row["count"]];

        pos1 = {xmin, 0};
        pos2 = {xmax, yval};
        
        {
          EdgeForm[{colorDir, lineAlphaDir}], (* Rectangle outline with color and lineAlpha *)
          colorDir, alphaDir, (* Fill color and fill alpha *)
          Rectangle[pos1, pos2]
        }
      ]
    ]],
    
    (* Outline only - create a single polygon outline *)
    Module[{sortedData, points, colorDir, lineAlphaDir},
      (* Sort data by xmin for proper outline construction *)
      sortedData = SortBy[statData, #["xmin"] &];
      
      (* Get aesthetics from first bar *)
      colorDir = First[sortedData]["color_aes"];
      lineAlphaDir = Lookup[First[sortedData], "lineAlpha_aes", Opacity[1]];
      
      (* Construct proper histogram outline *)
      points = Join[
        (* Start at bottom-left of first bar *)
        {{OptionValue["xScaleFunc"][First[sortedData]["xmin"]], 0}},
        (* Go up and trace the top of each bar from left to right *)
        Flatten[Table[{
          {OptionValue["xScaleFunc"][bar["xmin"]], OptionValue["yScaleFunc"][bar["count"]]},
          {OptionValue["xScaleFunc"][bar["xmax"]], OptionValue["yScaleFunc"][bar["count"]]}
        }, {bar, sortedData}], 1],
        (* End at bottom-right of last bar *)
        {{OptionValue["xScaleFunc"][Last[sortedData]["xmax"]], 0}},
        (* Close the polygon by returning to start *)
        {{OptionValue["xScaleFunc"][First[sortedData]["xmin"]], 0}}
      ];
      
      output = {
        {colorDir, lineAlphaDir, Line[points]}
      }
    ]
  ];
  
  output
];

End[];

EndPackage[];
