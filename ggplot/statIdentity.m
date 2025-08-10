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
  "group" -> Null,
	"lineAlpha"->Null
};

statIdentity[opts : OptionsPattern[]] := Module[{
  dataset, processedData, groupedData
},
  dataset = OptionValue["data"];

  (* Ensure X/Y has been given *)
  If[OptionValue["x"] === Null || OptionValue["y"] === Null, 
    Message[ggplot::xOrYNotGiven]; 
    Throw[Null]
  ];
  
  processedData = dataset;
  
  (* Switch dates to absolute times *)
  processedData = Replace[processedData, d_?DateObjectQ :> AbsoluteTime[d], Infinity];

  (* SIMPLIFIED: statIdentity is now purely about data transformation *)
  (* All aesthetic reconciliation should have been done before this point *)
  
  (* Group data: if group aesthetic is specified, only group by that; otherwise group by all aesthetics *)
  groupedData = If[KeyExistsQ[First[processedData, <||>], "group_aes"],
    (* Group only by the group aesthetic *)
    GroupBy[processedData, Function[row, row["group_aes"]]],
    (* Group by all aesthetic values - rows with same aesthetics = same group *)
    Module[{aestheticKeys, firstRow},
      firstRow = First[processedData, <||>];
      aestheticKeys = Select[Keys[firstRow], StringEndsQ[#, "_aes"] &];
      
      (* Group by all aesthetic values present in the data *)
      GroupBy[processedData, 
        Function[row, 
          Association[Table[key -> Lookup[row, key, Missing["NotAvailable"]], {key, aestheticKeys}]]
        ]
      ]
    ]
  ];
  groupedData
]

End[];

EndPackage[];