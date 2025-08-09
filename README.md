# ggplot: A Grammar of Graphics Implementation for Wolfram Language

A comprehensive Wolfram Language implementation of the Grammar of Graphics, inspired by R's ggplot2. This library provides a layered, composable approach to creating statistical graphics by combining data, aesthetic mappings, statistical transformations, geometric objects, coordinate systems, scales, and themes.

## Design Philosophy

This implementation follows the core principles of the Grammar of Graphics:

### 1. **Layered Architecture**
Graphics are built by combining independent, composable layers:
- **Data Layer**: Raw datasets (lists of associations)  
- **Aesthetic Layer**: Visual property mappings (color, size, shape, position)
- **Statistical Layer**: Data transformations (identity, binning, smoothing, density)
- **Geometric Layer**: Visual representations (points, lines, bars, contours)
- **Scale Layer**: Data-to-visual mappings (linear, log, discrete, date)
- **Coordinate Layer**: Coordinate system transformations
- **Facet Layer**: Small multiples for subgroup visualization
- **Theme Layer**: Overall visual styling and defaults

### 2. **Functional Composition & Immutable Data Flow**
All components are pure functions that compose naturally. Data flows through the system immutably:

```
Raw Data → Aesthetic Reconciliation → Statistical Transformation → Geometric Rendering → Scale Mapping → Final Graphics
```

### 3. **Separation of Statistical Computation from Visual Representation**
The new architecture separates `stat` functions (data transformation) from `geom` functions (visual rendering), enabling flexible combinations like `statSmooth` with `geomPoint` or `statBin` with `geomLine`.

## General Usage

```wolfram
(* Basic syntax patterns *)
ggplot[dataset, "x" -> "var", "y" -> "var", geomPoint[]]

ggplot["data" -> dataset, "x" -> "var", "y" -> "var", geomLine[]]

dataset // ggplot["x" -> "var", "y" -> "var", geomSmooth[]]

(* Advanced layering with stat/geom separation *)
ggplot[data, "x" -> "x", "y" -> "y",
  geomHistogram["stat" -> statBin["bins" -> 20], "fill" -> "red"],
  geomDensity2D["stat" -> statDensity2D["h" -> {0.5, 0.8}]]
]
```

## Architecture Overview

### The Central Pipeline

The system follows a **multi-stage rendering pipeline**:

```wolfram
ggplot[data, aesthetics, layers...] 
  ↓
1. Argument parsing & scale detection 
  ↓
2. Global aesthetic reconciliation
  ↓  
3. Facet data splitting (if faceted)
  ↓
4. Per-panel processing:
   Data → Stat Functions → Geom Functions → Graphics Primitives
  ↓
5. Legend generation from collected aesthetic data
  ↓
6. Final layout assembly (panels + legends + facet labels)
```

### Component Responsibilities

## 1. **Main Orchestrator: `ggplot.m`**

The `ggplot` function is the central coordinator with these key responsibilities:

**A. Argument Processing**
- Uses `HoldAllComplete` to capture unevaluated expressions
- Pattern matches to extract datasets, aesthetics, geoms, scales, and facet specifications
- Validates that required data is provided

**B. Scale System Setup**  
- Calls `reconcileXScales` and `reconcileYScales` to detect scale types (Linear, Log, Date, Discrete)
- Creates scale transformation functions via `createDiscreteScaleFunc` or mathematical functions
- Establishes consistent coordinate systems across faceted panels

**C. Global Aesthetic Resolution**
- Processes global aesthetic mappings (color, size, shape, etc.) before faceting
- Ensures consistent aesthetic scales across all panels
- Calls `reconcileAesthetics` for each mapped aesthetic

**D. Faceting Coordination**
- Detects `facetWrap` specifications and delegates to faceting system
- Splits data appropriately for multi-panel layouts
- Maintains consistent scales across panels when `scales = "fixed"`

**E. Layer Processing Pipeline**
- For each panel, processes layers through the **stat → geom pipeline**
- Collects graphics primitives from all layers
- Manages the interaction between statistics and geometry

**F. Legend Generation**
- Extracts legend information from aesthetic mappings  
- Handles complex legend merging when multiple aesthetics map to the same variable
- Positions legends according to theme settings

## 2. **Statistical Transformation Layer: `stat*.m` files**

Statistical functions transform raw data into computed values for visualization. The **new architecture** separates statistical computation from rendering:

**Core Stat Functions:**
- `statIdentity`: Pass-through for direct data plotting
- `statBin`: Histogram binning and frequency computation
- `statSmooth`: Regression lines and confidence intervals  
- `statDensity2D`: 2D kernel density estimation
- `statConvexHull`: Convex hull boundary computation

**Key Design Patterns:**
```wolfram
Options[statFunc] = {
  "data" -> {}, "x" -> Null, "y" -> Null,
  (* aesthetic options *),
  (* stat-specific parameters *)
};

statFunc[opts : OptionsPattern[]] := Module[{
  dataset, processedData, groupedData, computedResults
},
  (* 1. Validate inputs *)
  (* 2. Group data by aesthetics *)  
  (* 3. Apply statistical computation *)
  (* 4. Return list of groups with computed values *)
]
```

**Grouping Strategy:**
Stats group data by aesthetic values to ensure that statistical computations respect visual groupings (e.g., separate regression lines per color group).

## 3. **Geometric Rendering Layer: `geom*.m` files**

The **updated geom architecture** separates layer construction from rendering:

**Layer Construction Functions** (e.g., `geomPoint`, `geomLine`):
```wolfram
geomPoint[opts : OptionsPattern[]] := <|
  "stat" -> statIdentity,           (* Default stat *)
  "geom" -> geomPointRender,        (* Actual renderer *)
  "statParams" -> FilterRules[{opts}, Options[statIdentity]],
  "geomParams" -> FilterRules[{opts}, Options[geomPointRender]]
|>
```

**Rendering Functions** (e.g., `geomPointRender`, `geomLineRender`):
- Take **statistically processed data** as input
- Apply final aesthetic mappings and coordinate transformations
- Generate Wolfram graphics primitives  
- Handle grouping for multi-line/multi-group plots

**Key Geoms:**
- `geomPoint`/`geomPointRender`: Scatter plots with shape/size/color/alpha mapping
- `geomLine`/`geomLineRender`: Connected lines with sophisticated grouping logic
- `geomSmooth`/`geomSmoothRender`: Regression lines with confidence bands
- `geomHistogram`: Statistical bars with binning
- `geomDensity2D`/`geomDensity2DRender`: Contour plots from density estimation
- `geomText`: Text labels with positioning control

## 4. **Aesthetic Mapping System: `aes*.m` files**

Aesthetics control the mapping from data values to visual properties through the **dispatch pattern**:

```wolfram
(* Default case: no mapping *)
reconcileAesthetics[dataset_, Null, "color"] := 
  Map[Append[#, "color_aes" -> Black] &, dataset]

(* Constant value: direct assignment *)
reconcileAesthetics[dataset_, color_?ColorQ, "color"] := 
  Map[Append[#, "color_aes" -> color] &, dataset]

(* Column reference: map from data *)
reconcileAesthetics[dataset_, key_?StringQ, "color"] := 
  (* Determine discrete vs continuous and create appropriate mapping *)

(* Function: apply function to determine grouping *)  
reconcileAesthetics[dataset_, func_Function, "color"] := 
  (* Group by function result and assign colors *)
```

**Supported Aesthetics:**
- `color`: Point/line colors with discrete and continuous scales
- `fill`: Fill colors for bars, areas, and polygons
- `size`: Point/line sizes with scaling  
- `shape`: Point shapes with discrete categorical mapping
- `alpha`: Transparency levels
- `lineAlpha`: Line-specific transparency
- `thickness`: Line thickness control
- `group`: Explicit grouping for lines and paths

**Data Type Handling:**
- **Discrete Data**: Creates categorical color/shape/size mappings
- **Continuous Data**: Creates smooth gradients and scaled values
- **Date Data**: Automatic conversion via `AbsoluteTime` for temporal aesthetics

## 5. **Scale System: `scale*.m` and related files**

Scales control the mapping between data space and visual space:

**Scale Detection:**
```wolfram
reconcileXScales[args___] /; discreteScaleQ["x", args] := "Discrete"
reconcileXScales[args___] /; Count[args, scaleXLog2[___], Infinity] > 0 := "Log"  
reconcileXScales[args___] /; Count[args, scaleXDate2[___], Infinity] > 0 := "Date"
reconcileXScales[___] := "Linear"
```

**Scale Types:**
- **Discrete**: Categorical data → integer positions with label mapping
- **Linear**: Direct numeric mapping with range calculations
- **Log**: Logarithmic transformations for skewed data
- **Date**: Temporal data with automatic time axis formatting

**Scale Function Creation:**
- Discrete: `createDiscreteScaleFunc` builds categorical → numeric mappings  
- Continuous: Mathematical transformation functions (`Log`, `Identity`)
- Functions are passed to geoms as `"xScaleFunc"` and `"yScaleFunc"` options

## 6. **Faceting System: `facetWrap.m`**

Enables small multiples visualization:

**Facet Processing:**
1. `facetWrap[variable, options]` returns a data transformation function
2. Data is grouped by the faceting variable  
3. Each group becomes a separate panel with shared scales
4. `processPanelLayers` handles the stat → geom pipeline for each panel

**Panel Architecture:**
- Each panel processes all layers independently through the stat → geom pipeline
- Global scales ensure consistent coordinate systems across panels
- Strip labels are generated from faceting variable values

## 7. **Legend System: `legend.m`**

**Legend Generation Pipeline:**
1. **Extraction**: `extractLegendInfo` scans aesthetic mappings from arguments
2. **Processing**: Uses same `reconcileAesthetics` functions to ensure visual consistency  
3. **Combination**: `combineLegendsForSameVariable` merges legends when multiple aesthetics map to same data
4. **Conversion**: `convertLegendInfo` translates to Mathematica legend objects (`LineLegend`, `PointLegend`, `BarLegend`)

**Legend Types:**
- **Discrete Color**: `LineLegend` with categorical colors
- **Continuous Color**: `BarLegend` with gradient scales  
- **Shape**: `PointLegend` with different markers
- **Size**: `PointLegend` with scaled marker sizes
- **Combined**: Multi-aesthetic legends when multiple aesthetics map to same variable

## 8. **Theme System: `themes.m`**

Themes provide global styling defaults through Wolfram's `Options` system:

**Theme Architecture:**
```wolfram
ggplotSetTheme[themeName] := Module[{},
  SetOptions[ggplot, (* Graphics options *)];
  SetOptions[ticks2, (* Axis styling *)];  
  SetOptions[gridLines2, (* Grid styling *)];
  SetOptions[formatTicks, (* Tick formatting *)];
]
```

**Configurable Elements:**
- Overall plot styling (aspect ratio, margins, background)
- Axis appearance (frame style, tick style, labels)
- Default color palettes (`"categoricalColors"`, `"continuousColorPalette"`)
- Shape palettes (`"categoricalShapes"`) with automatic cycling
- Grid line styling and positioning

## Currently Supported Features

### Geoms (Geometric Objects)
- **`geomPoint`**: Scatter plots with full aesthetic mapping (color, size, shape, alpha)
- **`geomLine`**: Line plots with sophisticated grouping and aesthetic support  
- **`geomPath`**: Path plots following data order
- **`geomSmooth`**: Regression lines with confidence intervals via `statSmooth`
- **`geomHistogram`**: Histograms with customizable binning via `statBin`
- **`geomBar`**: Bar charts for categorical data  
- **`geomCol`**: Column charts for pre-computed values
- **`geomDensity2D`**: 2D contour plots via kernel density estimation
- **`geomDensity2DFilled`**: Filled contour plots  
- **`geomConvexHull`**: Convex hull boundaries via `statConvexHull`
- **`geomText`**: Text labels with positioning control
- **`geomHLine`**, **`geomVLine`**: Reference lines
- **`geomParityLine`**: y=x diagonal reference lines
- **`geomBand`**: Confidence/error bands
- **`geomBoxes`**: Box plots (basic implementation)

### Statistical Transformations
- **`statIdentity`**: Pass-through for raw data
- **`statBin`**: Histogram binning with flexible bin control
- **`statSmooth`**: Regression smoothing (linear and loess)
- **`statDensity2D`**: 2D kernel density estimation  
- **`statConvexHull`**: Convex hull computation

### Scales & Coordinate Systems
- **X/Y Scales**: `scaleXLinear2`, `scaleXLog2`, `scaleXDate2` and Y equivalents
- **Discrete Scales**: Automatic categorical data handling with integer mapping
- **Date Scales**: Temporal data with automatic axis formatting
- **Coordinate Transformations**: Full support for log and linear transformations

### Aesthetic Mappings
- **`color`**: Discrete and continuous color mapping with automatic palette selection
- **`fill`**: Fill colors for bars, areas, and polygons
- **`size`**: Point and line size scaling
- **`shape`**: Discrete shape mapping with configurable palettes  
- **`alpha`**: Transparency control  
- **`lineAlpha`**: Line-specific transparency
- **`thickness`**: Line thickness control
- **`group`**: Explicit grouping for multi-line plots
- **Function Mappings**: Support for `Function[condition]` in aesthetics

### Faceting
- **`facetWrap`**: Small multiples with flexible grid layouts
- **Consistent Scaling**: Shared coordinate systems across panels
- **Strip Labels**: Automatic panel labeling
- **Grid Control**: Configurable panel arrangements via `ncol`/`nrow`

### Legend System  
- **Automatic Generation**: Legends automatically created from aesthetic mappings
- **Multi-Aesthetic Legends**: Combines multiple aesthetics mapping to same variable
- **Legend Types**: Point, line, swatch, and color bar legends
- **Positioning**: Flexible legend positioning with theme control
- **Legend Merging**: Intelligent merging when multiple aesthetics map to same data

### Themes
- **Theme System**: Global styling via `ggplotSetTheme`  
- **Pre-built Themes**: White theme with professional defaults
- **Configurable Elements**: Colors, shapes, fonts, grid lines, margins
- **Dynamic Palettes**: Configurable color and shape palettes with automatic cycling

## Examples

### Basic Scatter Plot
```wolfram
mpg // ggplot[geomPoint["x" -> "displ", "y" -> "cty", "color" -> "class"]]
```
![](Imgs/Mpg_Example1.png)

### Time Series
```wolfram
economics // ggplot[
  "x" -> "date", "y" -> "uempmed", 
  geomLine[], 
  scaleXDate2[]
]
```
![](Imgs/Economics_Example1.png)

### Advanced Multi-Layer Plot
```wolfram
ggplot[data, "x" -> "xvar", "y" -> "yvar",
  geomPoint["color" -> "group", "size" -> "weight"],
  geomSmooth["color" -> "group", "stat" -> statSmooth["method" -> "loess"]],
  geomDensity2D["alpha" -> 0.6]
]
```

### Faceted Analysis
```wolfram
ggplot[data, "x" -> "xvar", "y" -> "yvar",
  geomPoint["color" -> "category"],
  geomSmooth[],
  facetWrap["panel_var", "ncol" -> 3]
]
```

## Development Status

### Recently Implemented ✅
- **Stat/Geom Pipeline Separation**: Complete architectural refactoring  
- **Faceting System**: Multi-panel layouts with `facetWrap`
- **Enhanced Legend System**: Automatic legend generation with merging
- **Statistical Layer**: Proper stat functions (`statBin`, `statSmooth`, `statDensity2D`)
- **Function Aesthetics**: Support for conditional aesthetic mapping
- **Theme Enhancements**: Configurable shape and color palettes

### Current Limitations & Future Work 🔄
- **Additional Geoms**: Box plots, violin plots, area plots need enhancement
- **More Statistical Functions**: Need `statBoxplot`, `statViolin`, `statQQLine`
- **Coordinate Systems**: Polar coordinates, map projections
- **Advanced Scales**: Custom transformations, reverse scales, date/time enhancements
- **Annotation System**: Rich text annotation and arrow/callout support
- **Performance Optimization**: Large dataset handling and rendering efficiency
- **Error Handling**: More descriptive error messages and input validation

## Extending the Library

### Adding a New Geom

The new architecture separates layer construction from rendering, making geom extension straightforward:

1. **Create the layer constructor** in `ggplot/geomNewGeom.m`:

```wolfram
BeginPackage["ggplot`"];
Begin["`Private`"];

ClearAll[geomNewGeom];
geomNewGeom[opts:OptionsPattern[] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0] := Module[{
  statFunc, geomFunc
},
  (* Allow overriding default stat and geom *)
  statFunc = Lookup[Association[opts], "stat", statIdentity];
  geomFunc = Lookup[Association[opts], "geom", geomNewGeomRender];
  
  <|
    "stat" -> statFunc,
    "geom" -> geomFunc,
    "statParams" -> FilterRules[{opts}, Options[statFunc]],
    "geomParams" -> FilterRules[{opts}, Options[geomFunc]]
  |>
];

Options[geomNewGeomRender] = {
  "data" -> {}, "x" -> Null, "y" -> Null,
  "color" -> Null, "alpha" -> Null,
  "xScaleFunc" -> Function[Identity[#]], 
  "yScaleFunc" -> Function[Identity[#]]
};

geomNewGeomRender[statData_, opts : OptionsPattern[]] := Module[{output},
  (* Validate required aesthetics *)
  If[OptionValue["x"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];
  
  (* statData is already processed by the stat function and grouped appropriately *)
  (* Generate graphics primitives from each group *)
  output = statData // Map[Function[row,
    (* Create graphics primitives using row data and scale functions *)
    {
      row["color_aes"], 
      row["alpha_aes"],
      YourGraphicsPrimitive[
        OptionValue["xScaleFunc"][row[OptionValue["x"]]],
        OptionValue["yScaleFunc"][row[OptionValue["y"]]]
      ]
    }
  ]];
  
  output
];

End[];
EndPackage[];
```

2. **Register the new geom**:
   - Add to `ggplot/Kernel/init.m`: `Get["ggplot`geomNewGeom`"];`
   - Add to `ggplotSymbolDeclaration.m`: `geomNewGeom::usage = "Description";`
   - Add to pattern matching in main `ggplot.m` function

### Adding a New Statistical Transformation

```wolfram
BeginPackage["ggplot`"];
Begin["`Private`"];

ClearAll[statNewStat];
Options[statNewStat] = {
  "data" -> {},
  "x" -> Null, "y" -> Null,
  "color" -> Null, "group" -> Null,
  (* stat-specific parameters *)
  "parameter1" -> defaultValue,
  "parameter2" -> Automatic
};

statNewStat[opts : OptionsPattern[]] := Module[{
  dataset, processedData, groupedData, computedResults
},
  dataset = OptionValue["data"];
  
  (* Validate required inputs *)
  If[OptionValue["x"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];
  
  processedData = dataset;
  
  (* Group data by aesthetics to respect visual groupings *)
  groupedData = If[KeyExistsQ[First[processedData, <||>], "group_aes"],
    GroupBy[processedData, Function[row, row["group_aes"]]],
    (* Group by all aesthetic values *)
    Module[{aestheticKeys, firstRow},
      firstRow = First[processedData, <||>];
      aestheticKeys = Select[Keys[firstRow], StringEndsQ[#, "_aes"] &];
      GroupBy[processedData, Function[row, 
        Association[Table[key -> Lookup[row, key, Missing["NotAvailable"]], {key, aestheticKeys}]]
      ]]
    ]
  ];
  
  (* Apply statistical computation to each group *)
  computedResults = KeyValueMap[Function[{groupKey, groupData},
    Module[{xValues, yValues, computedValues},
      xValues = extractMappedValues[groupData, OptionValue["x"]];
      yValues = If[OptionValue["y"] =!= Null, 
        extractMappedValues[groupData, OptionValue["y"]], 
        None
      ];
      
      (* Perform your statistical computation here *)
      computedValues = yourStatisticalFunction[xValues, yValues, (* parameters *)];
      
      (* Return computed data in same format as input, with computed columns added *)
      MapThread[Function[{originalRow, computedValue},
        Append[originalRow, "computed_column" -> computedValue]
      ], {groupData, computedValues}]
    ]
  ], groupedData];
  
  (* Return association of group -> computed data *)
  computedResults
];

End[];
EndPackage[];
```

### Adding a New Aesthetic

```wolfram
BeginPackage["ggplot`"];
Begin["`Private`"];

(* Default case: no mapping *)
reconcileAesthetics[dataset_, Null, "newAesthetic"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "newAesthetic_aes" -> defaultValue] &];
  newDataset
];

(* Direct value case *)
reconcileAesthetics[dataset_, value_, "newAesthetic"] := Module[{newDataset},
  newDataset = dataset;
  newDataset = newDataset // Map[Append[#, "newAesthetic_aes" -> value] &];
  newDataset
];

(* Column reference case *)
reconcileAesthetics[dataset_, key_?StringQ, "newAesthetic"] /; keyExistsQAll[dataset, key] := 
Module[{newDataset, data, aestheticFunc, discreteDataQ, keys, values},
  newDataset = dataset;
  data = newDataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  
  If[discreteDataQ,
    (* Discrete mapping *)
    keys = Sort[getDiscreteKeys[data]];
    values = yourDiscreteMappingFunction[Length[keys]];  (* e.g., color palette, size range *)
    aestheticFunc = Function[AssociationThread[keys, values][#]],
    (* Continuous mapping *)
    Module[{minMax, mappingFunc},
      minMax = getContinuousRange[data];
      mappingFunc = yourContinuousMappingFunction[minMax];  (* e.g., color gradient *)
      aestheticFunc = mappingFunc
    ]
  ];
  
  newDataset = newDataset // Map[Append[#, "newAesthetic_aes" -> aestheticFunc[#[key]]] &];
  newDataset
];

(* Function case *)
reconcileAesthetics[dataset_, func_Function, "newAesthetic"] := Module[{newDataset, groupedDataset, values},
  newDataset = dataset;
  groupedDataset = GroupBy[dataset, func] // KeySort;
  values = yourGroupMappingFunction[Keys[groupedDataset]];
  
  newDataset = groupedDataset // Values // MapIndexed[
    Function[{group, index}, 
      Map[Function[row, Append[row, "newAesthetic_aes" -> values[[First@index]]]], group]
    ]
  ] // Flatten;
  newDataset
];

End[];
EndPackage[];
```

### Adding a New Theme

```wolfram
ggplotSetTheme[ggplotThemeNewTheme] := Module[{},
  SetOptions[ggplot,
    (* Core graphics options *)
    ImageSize -> 450,
    AspectRatio -> 3/4,
    Frame -> True,
    Background -> GrayLevel[0.98],
    FrameStyle -> Directive[GrayLevel[0.5], Thickness[0.001]],
    LabelStyle -> Directive[14, FontFamily -> "Helvetica"],
    
    (* Color and shape palettes *)
    "categoricalColors" -> {Red, Blue, Green, Orange, Purple, Brown, Pink},
    "sequentialColors" -> {White, Blue},
    "categoricalShapes" -> {"\[FilledCircle]", "\[FilledSquare]", "\[FilledUpTriangle]"}
  ];
  
  SetOptions[ticks2,
    numberOfMajorTicks2 -> 6,
    majorTickStyle2 -> Directive[GrayLevel[0.3], Thickness[0.001]],
    majorGridLineStyle2 -> Directive[GrayLevel[0.85], Thickness[0.001]]
  ];
  
  (* Propagate tick options to related functions *)
  SetOptions[gridLines2, Options[ticks2]];
  SetOptions[formatTicks, Options[ticks2]];
  SetOptions[formatGridLines, Options[ticks2]];
  
  $ggplotTheme = ggplotThemeNewTheme;
  Print["Set theme to ggplotThemeNewTheme"]
];
```

## Key Design Patterns & Best Practices

### 1. **Stat/Geom Separation**
- Stats transform data, geoms render visuals
- This enables flexible combinations (e.g., `statSmooth` with `geomPoint`)
- Stats return grouped data, geoms process each group independently

### 2. **Aesthetic Processing Pipeline**  
- Global aesthetics processed once in main `ggplot` function
- Layer-specific aesthetics processed in `processPanelLayers`  
- Each aesthetic adds `"aesthetic_aes"` columns to data

### 3. **Function Options Pattern**
- All functions use `OptionsPattern[]` with string-based keys
- Scale functions passed as `"xScaleFunc"` and `"yScaleFunc"` options
- Consistent validation and error handling across components

### 4. **Data Grouping Strategy**
- Data automatically grouped by aesthetic values for multi-group plots
- Group aesthetic provides explicit control over grouping
- Statistical functions respect aesthetic groupings

### 5. **Graphics Primitive Generation**
- Each geom returns list of graphics primitives  
- Primitives combined in final `Graphics` object
- Enables layering and composition

### 6. **Theme Integration**
- Themes modify global options via `SetOptions`
- Components automatically inherit theme settings
- User options override theme defaults

This architecture provides a solid foundation for extending the grammar of graphics while maintaining consistency and composability across all components.