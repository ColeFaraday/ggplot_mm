# ggplot: A grammar of graphics implementation for Wolfram Language

A Wolfram Language implementation of the Grammar of Graphics, inspired by R's ggplot2. This library provides a layered approach to creating statistical graphics by combining data, aesthetic mappings, geometric objects, scales, and themes.

## General usage

```
ggplot[dataset, "x" -> "var", "y" -> "var", additional aesthetics and geoms...]

ggplot["data" -> dataset, "x" -> "var", "y" -> "var", additional aesthetics and geoms...]

dataset //
    ggplot["x" -> "var", "y" -> "var", additional aesthetics and geoms...]
```

## Currently supported geoms

```
geomPoint[]
geomLine[]
geomPath[]
geomSmooth[]
geomParityLine[]
geomHLine[]
geomVLine[]
```

## Currently supported scales

```
scaleXLinear2[]
scaleXLog2[]
scaleXDate2
scaleYLinear2[]
scaleYLog2[]
scaleYDate2
```

## Major functionality still to work or working through:

- Ability to use functions inside aesthetics (i.e. "color" -> `Function[#var < 8]`)
- Also need to be able to use functions inside x and y (i.e. "x" -> `Function[#x * 5]`)
- Scaling (ensure correct functionality and continue to add additional scaling functions)
- Legends
- Faceting
- geoms other than lines and points (columns, histograms, box plots etc.)
- Textual axes (for things like bar chart)
- Coordinates
- Labels

## Examples

```
mpg//ggplot[geomPoint["x"->"displ","y"->"cty","color"->"class"]]
```
![](Imgs/Mpg_Example1.png)

```
economics//
	ggplot[
		"x"->"date","y"->"uempmed",geomLine[],scaleXDate2[]
	]
```
![](Imgs/Economics_Example1.png)

---

## Design Philosophy & Architecture

This library implements the Grammar of Graphics as a layered system where each component has a specific responsibility. The core philosophy follows these principles:

### 1. **Separation of Concerns**
- **Data Layer**: Raw datasets (lists of associations)
- **Aesthetic Layer**: Visual mappings (color, size, shape, position)
- **Geometry Layer**: Visual representations (points, lines, bars)
- **Scale Layer**: Data transformations (linear, log, discrete, date)
- **Theme Layer**: Overall visual styling

### 2. **Functional Composition**
All components are functions that can be composed together. The main `ggplot` function orchestrates this composition by:
- Parsing arguments and extracting components
- Creating scale functions for coordinate transformations
- Applying aesthetic mappings to data
- Generating graphics primitives from geoms
- Assembling the final `Graphics` object

### 3. **Immutable Data Flow**
Data flows through the system without modification:
```
Raw Data → Aesthetic Reconciliation → Scale Functions → Graphics Primitives → Final Plot
```

---

## Core Architecture Components

### Main Entry Point: `ggplot.m`

The `ggplot` function is the central orchestrator that:

1. **Argument Processing**: Uses `HoldAllComplete` to capture unevaluated expressions
2. **Scale Resolution**: Determines x/y scale types (Linear, Log, Date, Discrete)
3. **Scale Function Creation**: Builds transformation functions for coordinate mapping
4. **Geom Processing**: Collects and processes all geometric objects
5. **Graphics Assembly**: Combines all elements into a `Graphics` object

**Key Pattern**: Scale functions are passed to geoms via options:
```wolfram
geomPoint[opts, FilterRules[options, Options[geomPoint]], 
  "xScaleFunc" -> xScaleFunc, "yScaleFunc" -> yScaleFunc]
```

### Aesthetic System: `aes*.m` files

Aesthetics handle visual mappings through the `reconcileAesthetics` function pattern:

```wolfram
reconcileAesthetics[dataset_, mapping_, aestheticType_]
```

**Input Types Supported**:
- `Null`: Uses default values
- Direct values: `"color" -> Red` 
- Column references: `"color" -> "species"`
- Functions: `"color" -> Function[#price > 100]`

**Output**: Dataset with added aesthetic columns (e.g., `"color_aes"`, `"size_aes"`)

### Geometry System: `geom*.m` files

Each geom follows a consistent pattern:

1. **Validation**: Check required aesthetics (x, y)
2. **Data Preparation**: Handle date conversion, aesthetic reconciliation
3. **Grouping**: Group data by aesthetic combinations
4. **Primitive Generation**: Create Wolfram graphics primitives
5. **Optimization**: Apply performance optimizations (e.g., `GeometricTransformation`)

**Standard Options Pattern**:
```wolfram
Options[geomSomething] = {
  "data" -> {}, "x" -> Null, "y" -> Null,
  "color" -> Null, "alpha" -> Null, 
  "xScaleFunc" -> Function[Identity[#]], 
  "yScaleFunc" -> Function[Identity[#]]
}
```

### Scale System: `scale*.m` files

Scales determine how data values map to visual coordinates:

- **Discrete Scales**: Map categorical data to integer positions
- **Continuous Scales**: Apply mathematical transformations (log, linear)
- **Date Scales**: Handle temporal data conversion

**Key Functions**:
- `reconcileXScales`/`reconcileYScales`: Determine scale type from arguments
- `createDiscreteScaleFunc`: Build categorical → numeric mappings
- `discreteScaleQ`: Detect if data should use discrete scaling

### Theme System: `themes.m`

Themes set global visual defaults by modifying Wolfram's `Options` system:

```wolfram
ggplotSetTheme[themeName] := Module[{},
  SetOptions[ggplot, (* graphics options *)];
  SetOptions[ticks2, (* tick styling *)];
  SetOptions[gridLines2, (* grid styling *)];
]
```

#### Shape Configuration

Themes now support configurable shape palettes through the `"categoricalShapes"` option:

```wolfram
SetOptions[ggplot, 
  "categoricalShapes" -> {"\[FilledCircle]", "\[FilledUpTriangle]", "\[FilledSquare]", ...}
]
```

**Key Features:**
- **Dynamic Length**: No longer limited to 7 shapes - use as many as needed
- **Automatic Cycling**: When there are more groups than shapes, the system automatically cycles through the available shapes
- **Theme-Specific**: Each theme can define its own default shape palette
- **Customizable**: Users can override shape palettes using `SetOptions[ggplot, "categoricalShapes" -> {...}]`

**Example:**
```wolfram
(* Set a custom shape palette *)
SetOptions[ggplot, "categoricalShapes" -> {
  "\[EmptyCircle]", "\[EmptySquare]", "\[EmptyUpTriangle]", 
  "\[EmptyDiamond]", "\[Times]", "+"
}];

(* This will use the shapes in order, cycling if needed *)
ggplot[data, geomPoint["x" -> "xvar", "y" -> "yvar", "shape" -> "groupvar"]]
```

---

## How to Extend the Library

### Adding a New Geom

1. **Create the file**: `ggplot/geomNewGeom.m`

2. **Follow the standard pattern**:

```wolfram
BeginPackage["ggplot`"];
Begin["`Private`"];

Options[geomNewGeom] = {
  "data" -> {}, "x" -> Null, "y" -> Null,
  "color" -> Null, "alpha" -> Null,
  "xScaleFunc" -> Function[Identity[#]], 
  "yScaleFunc" -> Function[Identity[#]]
};

geomNewGeom[opts : OptionsPattern[]] /; Count[Hold[opts], ("data" -> _), {0, Infinity}] > 0 := 
Module[{newDataset, output},
  (* Validate required aesthetics *)
  If[OptionValue["x"] === Null, Message[ggplot::xOrYNotGiven]; Throw[Null];];
  
  newDataset = OptionValue["data"];
  
  (* Handle date conversion *)
  newDataset = Replace[newDataset, d_?DateObjectQ :> AbsoluteTime[d], Infinity];
  
  (* Reconcile aesthetics *)
  newDataset = reconcileAesthetics[newDataset, OptionValue["color"], "color"];
  newDataset = reconcileAesthetics[newDataset, OptionValue["alpha"], "alpha"];
  
  (* Generate graphics primitives *)
  output = newDataset // Map[{
    #["color_aes"],
    #["alpha_aes"],
    (* Your primitive here, e.g.: *)
    YourPrimitive[OptionValue["xScaleFunc"][#[OptionValue["x"]]], 
                 OptionValue["yScaleFunc"][#[OptionValue["y"]]]]
  } &];
  
  output
];

End[];
EndPackage[];
```

3. **Register in the system**:
   - Add to `ggplot/Kernel/init.m`: `Get["ggplot`geomNewGeom`"];`
   - Add to `ggplotSymbolDeclaration.m`: `geomNewGeom::usage = "TBD";`
   - Add to `argPatternQ` in `ggplot.m`: `| geomNewGeom[___]`
   - Add geom processing in main `ggplot` function

### Adding a New Aesthetic

1. **Create the file**: `ggplot/aesNewAesthetic.m`

2. **Implement the reconciliation patterns**:

```wolfram
BeginPackage["ggplot`"];
Begin["`Private`"];

(* Default case *)
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
Module[{newDataset, data, aestheticFunc, discreteDataQ},
  newDataset = dataset;
  data = newDataset[[All, key]];
  discreteDataQ = isDiscreteDataQ[data];
  
  (* Implement your mapping logic here *)
  aestheticFunc = If[discreteDataQ, 
    discreteMappingFunction[data],
    continuousMappingFunction[data]
  ];
  
  newDataset = newDataset // Map[Append[#, "newAesthetic_aes" -> aestheticFunc[#[key]]] &];
  newDataset
];

(* Function case *)
reconcileAesthetics[dataset_, func_Function, "newAesthetic"] := Module[{newDataset, groupedDataset, values},
  newDataset = dataset;
  groupedDataset = GroupBy[dataset, func] // KeySort;
  values = (* generate appropriate values for each group *);
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

3. **Register the aesthetic**:
   - Add to `ggplot/Kernel/init.m`
   - Update relevant geom options to include the new aesthetic
   - Update geom implementations to call `reconcileAesthetics` for the new aesthetic

### Adding a New Scale

1. **Create scale detection logic** in `scale.m`:

```wolfram
reconcileXScales[args___] /; Count[args, scaleXNewScale[___], Infinity] > 0 := "NewScale";
reconcileYScales[args___] /; Count[args, scaleYNewScale[___], Infinity] > 0 := "NewScale";
```

2. **Implement scale functions** in the appropriate scale file:

```wolfram
(* In ticks.m *)
ticks2["NewScale", min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := 
  (* Return tick specification *);

(* In gridLines.m *)
gridLines2["NewScale", min_?NumericQ, max_?NumericQ, opts : OptionsPattern[]] := 
  formatGridLines[ticks2["NewScale", min, max, opts], opts];
```

3. **Update scale function creation** in `ggplot.m`:

```wolfram
xScaleFunc = If[xScaleType == "Discrete",
  createDiscreteScaleFunc["x", heldArgs],
  With[{f = ToExpression[xScaleType /. "Linear" | "Date" | "NewScale" -> "Identity"]}, 
    Function[f[#]]
  ]
];
```

### Adding a New Theme

Create a new theme function in `themes.m`:

```wolfram
ggplotSetTheme[ggplotThemeNewTheme] := Module[{},
  SetOptions[ggplot,
    (* Set all ggplot graphics options *)
    ImageSize -> 400,
    AspectRatio -> 7/10,
    Frame -> True,
    (* ... more options ... *)
  ];
  SetOptions[ticks2,
    (* Set tick styling options *)
    numberOfMajorTicks2 -> 6,
    majorTickStyle2 -> Directive[Black, Thickness[0.001]],
    (* ... more options ... *)
  ];
  SetOptions[gridLines2, Options[ticks2]];
  SetOptions[formatTicks, Options[ticks2]];
  SetOptions[formatGridLines, Options[ticks2]];
  $ggplotTheme = ggplotThemeNewTheme;
  Print["Set theme to ggplotThemeNewTheme"]
];
```

---

## Key Design Patterns

### 1. **Option Pattern Consistency**
All functions use Wolfram's `OptionsPattern[]` with string-based keys for consistency with the grammar of graphics syntax.

### 2. **Hold Attributes**
Functions that need to inspect argument structure use `HoldAllComplete` or `HoldAll` to prevent premature evaluation.

### 3. **Module-Based Isolation**
Each function uses `Module` to create local variable scopes, preventing naming conflicts.

### 4. **Error Handling with Messages**
Custom message definitions provide user-friendly error reporting:
```wolfram
ggplot::xOrYNotGiven = "A geom was given without specifying the x or y mapping";
```

### 5. **Graphics Primitive Lists**
All geoms return lists of graphics primitives that can be combined in the final `Graphics` object.

### 6. **Functional Data Processing**
Heavy use of functional programming patterns (`Map`, `GroupBy`, `Cases`, `Select`) for data transformation.

---

## Development Guidelines

### Code Organization
- One concept per file (geom, aesthetic, scale type)
- Consistent naming: `geomName`, `aesName`, `scaleName`
- Private context for implementation details

### Performance Considerations
- Use `GeometricTransformation` for repeated similar primitives
- Group data operations to minimize passes through datasets
- Leverage Wolfram's optimized functions where possible

### Testing Strategy
- Test with different data types (numeric, string, date)
- Verify scale function behavior across ranges
- Check aesthetic mapping with edge cases

This architecture provides a flexible foundation that closely mirrors the grammar of graphics while leveraging Wolfram Language's strengths in symbolic computation and graphics generation.