# Bug Fix: Continuous vs Discrete Color Data Detection

## Problem
When using function mappings for color aesthetics (e.g., `"color" -> (#cty &)`), continuous numeric data was being treated as discrete, resulting in categorical colors instead of a continuous color gradient.

## Root Causes

### 1. Function Color Mapping Logic (aesColor.m)
The `reconcileAesthetics[dataset_, func_Function, "color"]` function was always treating function results as discrete/categorical data, regardless of whether the function returned continuous numeric values.

### 2. Data Type Detection (aes.m)
The `isDiscreteDataQ` function was too simplistic, only checking for strings and booleans. It didn't properly handle numeric data that could be either discrete or continuous.

### 3. Legend Creation Logic (legend.m)
The `extractColorLegendInfo` function was incorrectly determining whether data was discrete or continuous by checking the resulting colors rather than the original data values.

## Solutions

### 1. Enhanced Function Color Mapping (aesColor.m)
Modified the function case to:
- Extract data by applying the function to the dataset
- Use `isDiscreteDataQ` to determine if the resulting data should be treated as discrete or continuous
- Handle continuous data with appropriate color gradients
- Handle discrete data with categorical colors (existing behavior)

### 2. Improved Data Type Detection (aes.m)
Enhanced `isDiscreteDataQ` to:
- Keep existing behavior for strings and booleans (always discrete)
- For numeric data, consider it discrete if:
  - 12 or fewer unique values, OR
  - All values are integers AND 20 or fewer unique values
- Otherwise treat numeric data as continuous

### 3. Fixed Legend Creation (legend.m)
Updated `extractColorLegendInfo` to:
- Use the original data values (not the resulting colors) to determine discrete vs continuous
- Apply the same `isDiscreteDataQ` logic used in color mapping
- Properly extract color palette information for continuous legends

## Testing
Created `test_continuous_color_fix.nb` to validate:
- Function mapping with continuous data (should show gradient)
- String mapping with continuous data (should show gradient)
- Function mapping with discrete data (should show categorical colors)
- String mapping with discrete data (should show categorical colors)
- Direct testing of `isDiscreteDataQ` function

## Files Modified
1. `ggplot/aes.m` - Enhanced `isDiscreteDataQ` function
2. `ggplot/aesColor.m` - Fixed function color mapping logic
3. `ggplot/legend.m` - Fixed legend creation for continuous colors

## Expected Result
The original problematic code:
```mathematica
mpg // ggplot["x" -> "displ", "y" -> "cty", "color" -> (#cty &), 
  "shape" -> "class", geomPoint[], geomLine[], geomSmooth[], 
  ImageSize -> 600, "sizeLegendTitle" -> "Heyo!"]
```

Should now correctly:
- Display a continuous color gradient for the `cty` variable
- Show a continuous color legend (BarLegend) instead of discrete color swatches
- Map colors smoothly across the range of `cty` values
