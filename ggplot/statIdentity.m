BeginPackage["ggplot`"];

Begin["`Private`"];

ClearAll[statIdentity];
Options[statIdentity] = {
  "data" -> {},
  "x" -> Null,
  "y" -> Null, 
  "color" -> Null,
  "size" -> Null,
  "alpha" -> Null,
  "shape" -> Null,
	"thickness" -> Null,
  "group" -> Null
};

statIdentity[opts : OptionsPattern[]] := Module[{
  dataset, processedData, groupedData
},
  Print["[statIdentity] Starting statIdentity"];
  dataset = OptionValue["data"];
  Print["[statIdentity] dataset length:", Length[dataset]];
  Print["[statIdentity] x mapping head:", Head[OptionValue["x"]]];
  Print["[statIdentity] y mapping head:", Head[OptionValue["y"]]];
  
  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, 
    Print["[statIdentity] ERROR: x or y not given"];
    Message[ggplot::xOrYNotGiven]; 
    Throw[Null]
  ];
  
  processedData = dataset;
  Print["[statIdentity] Initial processedData length:", Length[processedData]];
  
  (* Switch dates to absolute times *)
  processedData = Replace[processedData, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* Apply aesthetic reconciliation - this is the grouping logic *)
  Print["[statIdentity] Reconciling aesthetics"];
  processedData = reconcileAesthetics[processedData, OptionValue["color"], "color"];
  processedData = reconcileAesthetics[processedData, OptionValue["size"], "size"];
  processedData = reconcileAesthetics[processedData, OptionValue["alpha"], "alpha"];
  processedData = reconcileAesthetics[processedData, OptionValue["shape"], "shape"];
  processedData = reconcileAesthetics[processedData, OptionValue["thickness"], "thickness"];
  processedData = reconcileAesthetics[processedData, OptionValue["group"], "group"];
  Print["[statIdentity] After aesthetic reconciliation, data length:", Length[processedData]];

  (* Group data: if group aesthetic is specified, only group by that; otherwise group by all aesthetics *)
  Print["[statIdentity] Grouping data"];
  groupedData = If[OptionValue["group"] =!= Null,
    Print["[statIdentity] Grouping by explicit group aesthetic"];
    (* Group only by the explicit group aesthetic *)
    GroupBy[processedData, Function[row, row["group_aes"]]],
    Print["[statIdentity] Grouping by all aesthetics"];
    (* Group by all aesthetic values - rows with same aesthetics = same group *)
    GroupBy[processedData, 
      Function[row,
        {row["color_aes"], row["size_aes"], row["alpha_aes"], row["shape_aes"]}
      ]
    ]
  ];
  Print["[statIdentity] groupedData keys count:", Length[Keys[groupedData]]];
  Print["[statIdentity] groupedData group sizes:", Length /@ groupedData];
  Print["[statIdentity] Returning groupedData"];
  groupedData
]

End[];

EndPackage[];