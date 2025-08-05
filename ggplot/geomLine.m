(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomLine implementation *)

Options[geomLine] = {"data" -> {}, "x" -> Null, "y" -> Null, "group" -> Null, "color" -> Null, "thickness" -> Null, "alpha" -> Null, "dashing" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomLine[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0 := Module[{newDataset, groupbyKeys, output, legendRequests},
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
    (* Use explicit group aesthetic as primary grouping, but also consider color/alpha/thickness for line segments *)
    Function[{#["group_aes"], #["color_aes"], #["alpha_aes"], #["thickness_aes"]}],
    (* Fall back to color-based grouping for backward compatibility *)
    Function[{#["color_aes"], #["alpha_aes"], #["thickness_aes"]}]
  ];
  
  output = If[OptionValue["group"] =!= Null,
    (* When using group aesthetic, we need special handling to create connected lines within groups *)
    (* but with different colors/styles for different aesthetic combinations *)
    Module[{groupedData, groupedOutput},
      (* First group by the primary group aesthetic *)
      groupedData = GroupBy[newDataset, Function[#["group_aes"]]];
      
      (* For each group, create connected line segments with appropriate colors *)
      groupedOutput = Map[Function[groupData,
        Module[{sortedData, connectedSegments},
          (* Sort data by x values within each group for proper line connections *)
          sortedData = SortBy[groupData, Function[row, row[OptionValue["x"]]]];
          
          (* Create line segments that connect adjacent points, considering color changes *)
          connectedSegments = {};
          Do[
            If[i < Length[sortedData],
              Module[{point1, point2, color1, color2, alpha1, alpha2, thickness1, thickness2},
                point1 = sortedData[[i]];
                point2 = sortedData[[i + 1]];
                color1 = point1["color_aes"];
                alpha1 = point1["alpha_aes"];
                thickness1 = point1["thickness_aes"];
                color2 = point2["color_aes"];
                alpha2 = point2["alpha_aes"];
                thickness2 = point2["thickness_aes"];
                
                (* Use the color/style of the starting point for each segment *)
                AppendTo[connectedSegments, {
                  color1,
                  alpha1,
                  thickness1,
                  Line[{
                    {OptionValue["xScaleFunc"][point1[OptionValue["x"]]], OptionValue["yScaleFunc"][point1[OptionValue["y"]]]},
                    {OptionValue["xScaleFunc"][point2[OptionValue["x"]]], OptionValue["yScaleFunc"][point2[OptionValue["y"]]]}
                  }]
                }]
              ]
            ],
            {i, Length[sortedData]}
          ];
          connectedSegments
        ]
      ], Values[groupedData]];
      
      Flatten[groupedOutput, 1]
    ],
    (* Original behavior for non-group cases *)
    newDataset //
    GroupBy[groupbyKeys] //
    Values //
    Map[{
        #[[1, "color_aes"]],
        #[[1, "alpha_aes"]],
        #[[1, "thickness_aes"]],
        Line@Map[Function[point, {OptionValue["xScaleFunc"]@point[[1]], OptionValue["yScaleFunc"]@point[[2]]}]]@Sort@Transpose[{#[[All, OptionValue["x"]]], #[[All, OptionValue["y"]]]}]
    } &]
  ];

  (* Create legend requests based on what aesthetics are mapped *)
  legendRequests = {};
  
  (* Loop through all aesthetics that could be mapped to variables *)
  Module[{aesthetics, aestheticName, aestheticMapping, legendTitle, isDiscrete, reconciledDataset, uniqueValues, labels, values},
    aesthetics = {"color", "alpha", "thickness", "group"}; (* dashing excluded due to Graphics bug *)
    
    Do[
      aestheticMapping = OptionValue[aestheticName];
      
      (* Only create legend if aesthetic is mapped to a variable (not Null or direct value) *)
      If[aestheticMapping =!= Null && (StringQ[aestheticMapping] || Head[aestheticMapping] === Function),
        (* Determine legend title *)
        legendTitle = If[StringQ[aestheticMapping], 
          aestheticMapping, 
          aestheticName (* simplified title for function mappings *)
        ];
        
        (* Use reconcileAesthetics to get the same values that are being plotted *)
        reconciledDataset = reconcileAesthetics[OptionValue["data"], aestheticMapping, aestheticName];
        
        (* Check if this is discrete or continuous *)
        Module[{originalData},
          originalData = If[StringQ[aestheticMapping],
            OptionValue["data"][[All, aestheticMapping]],
            aestheticMapping /@ OptionValue["data"]
          ];
          isDiscrete = isDiscreteDataQ[originalData];
        ];
        
        If[isDiscrete,
          (* Extract unique values for discrete legend *)
          uniqueValues = DeleteDuplicates[reconciledDataset, #1[aestheticName <> "_aes"] === #2[aestheticName <> "_aes"] &];
          
          (* Get labels and aesthetic values *)
          labels = If[StringQ[aestheticMapping],
            Sort[DeleteDuplicates[reconciledDataset[[All, aestheticMapping]]]],
            Sort[DeleteDuplicates[aestheticMapping /@ OptionValue["data"]]]
          ];
          
          values = labels /. Association[
            If[StringQ[aestheticMapping],
              (#[aestheticMapping] -> #[aestheticName <> "_aes"]) & /@ uniqueValues,
              (aestheticMapping[#] -> #[aestheticName <> "_aes"]) & /@ uniqueValues
            ]
          ];
          
          AppendTo[legendRequests, <|
            "type" -> "line",
            "aesthetic" -> aestheticName, 
            "title" -> legendTitle,
            "isDiscrete" -> True,
            "labels" -> labels,
            "values" -> values
          |>];
        ];
      ],
      {aestheticName, aesthetics}
    ];
  ];

  (* Return both the graphics output and legend requirements *)
  <|"graphics" -> output, "legendRequests" -> legendRequests|>
];

End[];

EndPackage[];
