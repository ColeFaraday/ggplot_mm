(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: andrewyule *)
(* :Date: 2020-04-28 *)

BeginPackage["ggplot`"];

Begin["`Private`"];

(* First-Class Scale System *)

(* ============================== *)
(* SCALE CONSTRUCTOR FUNCTIONS    *)
(* ============================== *)

(* Continuous Scales *)
ClearAll[scaleContinuous];
Options[scaleContinuous] = {
  "aesthetic" -> "x",
  "name" -> Automatic,
  "breaks" -> Automatic,
  "labels" -> Automatic,
  "limits" -> Automatic,
  "transform" -> "identity",
  "palette" -> Automatic
};

scaleContinuous[opts : OptionsPattern[]] := <|
  "type" -> "continuous",
  "aesthetic" -> OptionValue["aesthetic"],
  "name" -> OptionValue["name"],
  "breaks" -> OptionValue["breaks"],
  "labels" -> OptionValue["labels"],  
  "limits" -> OptionValue["limits"],
  "transform" -> OptionValue["transform"],
  "palette" -> OptionValue["palette"],
  "options" -> Association[opts]
|>;

(* Discrete Scales *)
ClearAll[scaleDiscrete];
Options[scaleDiscrete] = {
  "aesthetic" -> "x",
  "name" -> Automatic,
  "labels" -> Automatic,
  "limits" -> Automatic,
  "palette" -> Automatic,
  "drop" -> True
};

scaleDiscrete[opts : OptionsPattern[]] := <|
  "type" -> "discrete",
  "aesthetic" -> OptionValue["aesthetic"],
  "name" -> OptionValue["name"],
  "labels" -> OptionValue["labels"],
  "limits" -> OptionValue["limits"],
  "palette" -> OptionValue["palette"],
  "drop" -> OptionValue["drop"],
  "options" -> Association[opts]
|>;

(* Log Scales *)
ClearAll[scaleLog];
Options[scaleLog] = {
  "aesthetic" -> "x",
  "name" -> Automatic,
  "breaks" -> Automatic,
  "labels" -> Automatic,
  "limits" -> Automatic,
  "base" -> E,
  "palette" -> Automatic
};

scaleLog[opts : OptionsPattern[]] := <|
  "type" -> "log",
  "aesthetic" -> OptionValue["aesthetic"],
  "name" -> OptionValue["name"],
  "breaks" -> OptionValue["breaks"],
  "labels" -> OptionValue["labels"],
  "limits" -> OptionValue["limits"],
  "base" -> OptionValue["base"],
  "palette" -> OptionValue["palette"],
  "options" -> Association[opts]
|>;

(* Date Scales *)
ClearAll[scaleDate];
Options[scaleDate] = {
  "aesthetic" -> "x",
  "name" -> Automatic,
  "breaks" -> Automatic,
  "labels" -> Automatic,
  "limits" -> Automatic,
  "dateBreaks" -> "1 month",
  "dateFormat" -> Automatic
};

scaleDate[opts : OptionsPattern[]] := <|
  "type" -> "date",
  "aesthetic" -> OptionValue["aesthetic"],
  "name" -> OptionValue["name"],
  "breaks" -> OptionValue["breaks"],
  "labels" -> OptionValue["labels"],
  "limits" -> OptionValue["limits"],
  "dateBreaks" -> OptionValue["dateBreaks"],
  "dateFormat" -> OptionValue["dateFormat"],
  "options" -> Association[opts]
|>;

(* ============================== *)
(* CONVENIENCE SCALE FUNCTIONS    *)
(* ============================== *)

(* Position scales *)
scaleXContinuous[opts___] := scaleContinuous["aesthetic" -> "x", opts];
scaleYContinuous[opts___] := scaleContinuous["aesthetic" -> "y", opts];
scaleXDiscrete[opts___] := scaleDiscrete["aesthetic" -> "x", opts];
scaleYDiscrete[opts___] := scaleDiscrete["aesthetic" -> "y", opts];
scaleXLog[opts___] := scaleLog["aesthetic" -> "x", opts];
scaleYLog[opts___] := scaleLog["aesthetic" -> "y", opts];
scaleXDate[opts___] := scaleDate["aesthetic" -> "x", opts];
scaleYDate[opts___] := scaleDate["aesthetic" -> "y", opts];

(* Color scales *)
scaleColorContinuous[opts___] := scaleContinuous["aesthetic" -> "color", opts];
scaleColorDiscrete[opts___] := scaleDiscrete["aesthetic" -> "color", opts];
scaleColorManual[values_, opts___] := scaleDiscrete["aesthetic" -> "color", "palette" -> values, opts];

(* Fill scales *)
scaleFillContinuous[opts___] := scaleContinuous["aesthetic" -> "fill", opts];
scaleFillDiscrete[opts___] := scaleDiscrete["aesthetic" -> "fill", opts];
scaleFillManual[values_, opts___] := scaleDiscrete["aesthetic" -> "fill", "palette" -> values, opts];

(* Size scales *)
scaleSizeContinuous[opts___] := scaleContinuous["aesthetic" -> "size", opts];
scaleSizeDiscrete[opts___] := scaleDiscrete["aesthetic" -> "size", opts];
scaleSizeManual[values_, opts___] := scaleDiscrete["aesthetic" -> "size", "palette" -> values, opts];

(* Shape scales *)
scaleShapeDiscrete[opts___] := scaleDiscrete["aesthetic" -> "shape", opts];
scaleShapeManual[values_, opts___] := scaleDiscrete["aesthetic" -> "shape", "palette" -> values, opts];

(* ============================== *)
(* SCALE TRAINING FUNCTIONS       *)
(* ============================== *)

(* Train a scale on data to compute domain, range, breaks, etc. *)
ClearAll[trainScale];
trainScale[scale_Association, data_List, mapping_] := Module[{
  values, domain, range, breaks, labels, palette, transform, inverse
},
  (* Extract values from data using mapping *)
  values = extractValues[data, mapping];
  
  (* Compute domain based on scale type *)
  domain = computeDomain[scale, values];
  
  (* Compute range based on aesthetic and scale type *)
  range = computeRange[scale, domain];
  
  (* Compute transformation functions *)
  {transform, inverse} = computeTransforms[scale];
  
  (* Compute breaks and labels *)
  {breaks, labels} = computeBreaksAndLabels[scale, domain, values];
  
  (* Get or compute palette *)
  palette = computePalette[scale, domain];
  
  (* Return trained scale *)
  <|scale,
    "domain" -> domain,
    "range" -> range,
    "breaks" -> breaks,
    "labels" -> labels,
    "palette" -> palette,
    "transform" -> transform,
    "inverse" -> inverse,
    "trained" -> True
  |>
];

(* Extract values from data using mapping *)
ClearAll[extractValues];
extractValues[data_List, mapping_] := Which[
  StringQ[mapping], data[[All, mapping]],
  Head[mapping] === Function, mapping /@ data,
  True, ConstantArray[mapping, Length[data]] (* constant value *)
];

(* Compute domain from values based on scale type *)
ClearAll[computeDomain];
computeDomain[scale_Association, values_List] := Module[{limits},
  limits = scale["limits"];
  
  Switch[scale["type"],
    "continuous" | "log",
    If[limits === Automatic, MinMax[values], limits],
    
    "date",
    If[limits === Automatic, MinMax[values], limits],
    
    "discrete",
    Module[{uniqueValues},
      uniqueValues = DeleteDuplicates[values];
      If[limits === Automatic, 
        Sort[uniqueValues],
        Intersection[Sort[uniqueValues], limits] (* Respect user limits *)
      ]
    ],
    
    _, MinMax[values] (* fallback *)
  ]
];

(* Compute range based on aesthetic *)
ClearAll[computeRange];
computeRange[scale_Association, domain_] := Module[{aesthetic},
  aesthetic = scale["aesthetic"];
  
  Switch[aesthetic,
    "x" | "y", 
    domain, (* Position scales use domain as range initially *)
    
    "color" | "fill",
    Automatic, (* Will use palette *)
    
    "size",
    Which[
      scale["type"] === "discrete", Range[3, 3 + Length[domain] - 1],
      True, {3, 8} (* Min/max sizes *)
    ],
    
    "alpha",
    Which[
      scale["type"] === "discrete", Table[0.3 + 0.7*i/(Length[domain]), {i, Length[domain]}],
      True, {0.3, 1.0}
    ],
    
    _, Automatic
  ]
];

(* Compute transformation functions *)
ClearAll[computeTransforms];
computeTransforms[scale_Association] := Switch[scale["type"],
  "log",
  {Log[scale["base"], #] &, scale["base"]^# &},
  
  "date",
  {AbsoluteTime, FromAbsoluteTime},
  
  _, (* continuous, discrete, etc. *)
  {Identity, Identity}
];

(* Compute breaks and labels *)
ClearAll[computeBreaksAndLabels];
computeBreaksAndLabels[scale_Association, domain_, values_List] := Module[{
  breaks, labels, userBreaks, userLabels
},
  userBreaks = scale["breaks"];
  userLabels = scale["labels"];
  
  Switch[scale["type"],
    "discrete",
    breaks = domain;
    labels = If[userLabels === Automatic, Map[ToString, domain], userLabels],
    
    "continuous",
    breaks = If[userBreaks === Automatic,
      Subdivide[domain[[1]], domain[[2]], 5],
      userBreaks
    ];
    labels = If[userLabels === Automatic, Map[ToString, breaks], userLabels],
    
    "log",
    Module[{logDomain, logBreaks},
      logDomain = Log[scale["base"]] /@ domain;
      logBreaks = If[userBreaks === Automatic,
        Subdivide[logDomain[[1]], logDomain[[2]], 4],
        Log[scale["base"]] /@ userBreaks
      ];
      breaks = scale["base"]^logBreaks;
      labels = If[userLabels === Automatic, Map[ToString, breaks], userLabels]
    ],
    
    "date",
    breaks = If[userBreaks === Automatic,
      generateDateBreaks[domain, scale["dateBreaks"]],
      userBreaks
    ];
    labels = If[userLabels === Automatic,
      formatDateLabels[breaks, scale["dateFormat"]],
      userLabels
    ]
  ];
  
  {breaks, labels}
];

(* Helper function for date breaks *)
ClearAll[generateDateBreaks];
generateDateBreaks[domain_, dateBreaks_] := Module[{start, end, interval},
  {start, end} = domain;
  (* Simple implementation - could be enhanced *)
  Subdivide[start, end, 5]
];

(* Helper function for date labels *)
ClearAll[formatDateLabels];
formatDateLabels[breaks_, format_] := Module[{},
  If[format === Automatic,
    Map[DateString[FromAbsoluteTime[#], {"Month", "/", "Day"}] &, breaks],
    Map[DateString[FromAbsoluteTime[#], format] &, breaks]
  ]
];

(* Compute palette *)
ClearAll[computePalette];
computePalette[scale_Association, domain_] := Module[{aesthetic, userPalette},
  aesthetic = scale["aesthetic"];
  userPalette = scale["palette"];
  
  If[userPalette =!= Automatic,
    userPalette, (* User provided palette *)
    
    Switch[aesthetic,
      "color" | "fill",
      Which[
        scale["type"] === "discrete", 
        getDefaultColorPalette[Length[domain]],
        True, 
        {"#0000FF", "#FFFFFF", "#FF0000"} (* Blue-White-Red *)
      ],
      
      "shape",
      If[scale["type"] === "discrete",
        getDefaultShapePalette[Length[domain]],
        {"\[FilledCircle]"}
      ],
      
      _, Automatic
    ]
  ]
];

(* Default palettes *)
ClearAll[getDefaultColorPalette, getDefaultShapePalette];

getDefaultColorPalette[n_Integer] := Take[{
  RGBColor[0.368, 0.507, 0.710],
  RGBColor[0.881, 0.611, 0.142], 
  RGBColor[0.560, 0.692, 0.194],
  RGBColor[0.923, 0.386, 0.209],
  RGBColor[0.650, 0.462, 0.113],
  RGBColor[0.740, 0.680, 0.227],
  RGBColor[0.272, 0.583, 0.816],
  RGBColor[0.915, 0.333, 0.517]
}, n];

getDefaultShapePalette[n_Integer] := Take[{
  "\[FilledCircle]", "\[FilledSquare]", "\[FilledUpTriangle]", 
  "\[FilledDiamond]", "\[FivePointedStar]", "\[FilledDownTriangle]"
}, n];

(* ============================== *)
(* SCALE APPLICATION FUNCTIONS    *)
(* ============================== *)

(* Apply a trained scale to map data values to visual values *)
ClearAll[applyScale];
applyScale[trainedScale_Association, value_] := Module[{
  domain, range, palette, transform, aesthetic
},
  domain = trainedScale["domain"];
  range = trainedScale["range"];  
  palette = trainedScale["palette"];
  transform = trainedScale["transform"];
  aesthetic = trainedScale["aesthetic"];
  
  Switch[trainedScale["type"],
    "discrete",
    Module[{pos},
      pos = FirstPosition[domain, value];
      If[pos =!= Missing["NotFound"],
        Which[
          aesthetic === "color" || aesthetic === "fill", palette[[First[pos]]],
          aesthetic === "shape", palette[[First[pos]]],
          aesthetic === "size", range[[First[pos]]],
          aesthetic === "alpha", range[[First[pos]]],
          True, First[pos] (* Position aesthetics return index *)
        ],
        Missing["NotFound"] (* Value not in domain *)
      ]
    ],
    
    "continuous" | "log" | "date",
    Module[{transformedValue, transformedDomain},
      transformedValue = transform[value];
      transformedDomain = transform /@ domain;
      
      Which[
        aesthetic === "color" || aesthetic === "fill",
        Blend[palette, Rescale[transformedValue, transformedDomain]],
        
        aesthetic === "size" || aesthetic === "alpha", 
        Rescale[transformedValue, transformedDomain, range],
        
        True, (* Position aesthetics *)
        transformedValue
      ]
    ]
  ]
];

End[];

EndPackage[];
