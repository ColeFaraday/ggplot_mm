(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* geomPoint implementation *)

Options[geomPoint] = {"data" -> {}, "x" -> Null, "y" -> Null, "color" -> Null, "size" -> Null, "alpha" -> Null, "shape" -> Null, "group" -> Null, "xScaleFunc" -> Function[Identity[#]], "yScaleFunc" -> Function[Identity[#]]};
geomPoint[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0 := Module[{newDataset, colorFunc, sizeFunc, alphaFunc, shapeFunc, output, legendRequests},
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];

  newDataset = OptionValue["data"];

  (* Switch dates to absolute times *)
  newDataset = Replace[newDataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* For each key necessary, reconcile the aesthetics and append them to the dataset as a column name i.e. "color_aes" -> somecolor *)
  newDataset = reconcileAesthetics[newDataset, OptionValue["color"], "color"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["size"], "size"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["alpha"], "alpha"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["shape"], "shape"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["group"], "group"];

  (*Grab the point data and for each Point apply the correct aesthetic*)
  output = newDataset // Map[Function[row,
    Module[{shapeObj, colorDir, alphaDir, sizeDir, pos, processedShape},
      shapeObj = row["shape_aes"];
      colorDir = row["color_aes"];
      alphaDir = row["alpha_aes"];
      sizeDir = row["size_aes"];
      pos = {OptionValue["xScaleFunc"][row[OptionValue["x"]]], OptionValue["yScaleFunc"][row[OptionValue["y"]]]};
      
      (* Check if shape has placeholder variables (from FilledMarkers[] or similar) *)
      If[StringContainsQ[ToString[shapeObj], "ggplotColorPlaceholder"],
        (* For markers with placeholders, substitute the actual values *)
        processedShape = shapeObj /. {
          ggplotColorPlaceholder -> colorDir,
          ggplotAlphaPlaceholder -> alphaDir,
          ggplotSizePlaceholder -> sizeDir
        };
        {Directive[], Directive[], Translate[processedShape, pos]},
        (* For simple string/symbol shapes, use the original approach *)
        {colorDir, alphaDir, Inset[Style[shapeObj, sizeDir], pos]}
      ]
    ]
  ]];

  (* Grouping data but doing a GeometricTransformation on similar Inset values to speed up the plotting once inside Graphics *)
  output = output // GroupBy[Function[{#[[1]], #[[2]], Inset[#[[3, 1]], {0, 0}]}] -> Function[#[[3, 2]]]] // Normal // Map[{#[[1, 1]], #[[1, 2]], GeometricTransformation[#[[1, 3]], List /@ #[[2]]]} &];

  (* Create legend requests based on what aesthetics are mapped *)
  legendRequests = {};
  
  (* Loop through all aesthetics that could be mapped to variables *)
  Module[{aesthetics, aestheticName, aestheticMapping, legendTitle, isDiscrete, reconciledDataset, uniqueValues, labels, values},
    aesthetics = {"color", "shape", "size", "alpha", "group"};
    
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
            "type" -> "point",
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
