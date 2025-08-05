(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomLine implementation *)

Options[geomLine] = {"data" -> {}, "x" -> Null, "y" -> Null, "group" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "dashing" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomLine[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0 := Module[{newDataset, groupbyKeys, output},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];

  newDataset = OptionValue["data"];

  (* Switch dates to absolute times *)
  newDataset = Replace[newDataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* For each key necessary, reconcile the aesthetics and append them to the dataset as a column name i.e. "color_aes" -> somecolor *)
  newDataset = reconcileAesthetics[newDataset, OptionValue["group"], "group"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["color"], "color"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["alpha"], "alpha"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["thickness"], "thickness"];
  (*newDataset = reconcileAesthetics[newDataset, OptionValue["dashing"], "dashing"];*) (* bug here with Dashing and Graphics that has been reported to Wolfram *)

  (* Group the data based on group aesthetic, or fall back to color-based grouping for backward compatibility *)
  groupbyKeys = If[OptionValue["group"] =!= Null,
    Function[{# ["group_aes"], # ["color_aes"], # ["alpha_aes"], # ["thickness_aes"]}],
    Function[{# ["color_aes"], # ["alpha_aes"], # ["thickness_aes"]}]
  ];

  output = If[OptionValue["group"] =!= Null,
    Module[{groupedData, groupedOutput},
      groupedData = GroupBy[newDataset, Function[# ["group_aes"]]];
      groupedOutput = Map[Function[groupData,
        Module[{xvals, yvals, pairs, sortedPairs, scaledPairs, connectedSegments},
          xvals = extractMappedValues[groupData, OptionValue["x"]];
          yvals = extractMappedValues[groupData, OptionValue["y"]];
          pairs = Transpose[{xvals, yvals}];
          sortedPairs = SortBy[pairs, First];
          scaledPairs = Map[{OptionValue["xScaleFunc"][#[[1]]], OptionValue["yScaleFunc"][#[[2]]]} &, sortedPairs];
          connectedSegments = {};
          Do[
            If[i < Length[scaledPairs],
              Module[{point1, point2, color1, alpha1, thickness1},
                point1 = groupData[[Ordering[xvals][[i]]]];
                point2 = groupData[[Ordering[xvals][[i + 1]]]];
                color1 = point1["color_aes"];
                alpha1 = point1["alpha_aes"];
                thickness1 = point1["thickness_aes"];
                AppendTo[connectedSegments, {
                  color1,
                  alpha1,
                  thickness1,
                  Line[{scaledPairs[[i]], scaledPairs[[i + 1]]}]
                }]
              ]
            ],
            {i, Length[scaledPairs]}
          ];
          connectedSegments
        ]
      ], Values[groupedData]];
      Flatten[groupedOutput, 1]
    ],
    (* Non-group case: group by color/alpha/thickness, then build lines *)
    Module[{grouped, result},
      grouped = GroupBy[newDataset, groupbyKeys];
      result = Values[grouped] // Map[
        Function[group,
          Module[{xvals, yvals, pairs, sortedPairs, scaledPairs},
            xvals = extractMappedValues[group, OptionValue["x"]];
            yvals = extractMappedValues[group, OptionValue["y"]];
            pairs = Transpose[{xvals, yvals}];
            sortedPairs = SortBy[pairs, First];
            scaledPairs = Map[{OptionValue["xScaleFunc"][#[[1]]], OptionValue["yScaleFunc"][#[[2]]]} &, sortedPairs];
            {
              group[[1, "color_aes"]],
              group[[1, "alpha_aes"]],
              group[[1, "thickness_aes"]],
              Line[scaledPairs]
            }
          ]
        ]
      ];
      result
    ]
  ];

  output
];

End[];

EndPackage[];
