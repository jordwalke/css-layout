/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 * Converted to Reason:
 * Replace:
 *
 *   function() {
 *
 * With:
 *   fun () => {
 *
 * :%s/\<var\>/let/g
 *
 * For enums:
 * Either:
 *
 *    :%s/\<\([A-Z_]\+\)\>/\L&/g
 *
 * Or Better:
 *   Just define the types as such below, and then regex away the constants
 *   value declarations.
 *
 * %s/\_s*let \<\([A-Z_]\+\)\> = .*;//g
 *
 * Since we've defined our own css undefined, use it.
 * :%s/\<CSS_UNDEFINED\>/CSS_UNDEFINED/g
 */
/**
 * So we can replace:
 * `!blah(x,y)` with `not <| blah(x,y)` with the same precedence.
 */

open LayoutTypes;

let cssUndefined = nan;

let positive_flex_is_auto = false;

let gCurrentGenerationCount = 0;

let (<|) a b => a b;

let css_undefined = nan;

let isUndefined value => classify_float nan === FP_nan;

let dummyMeasure (context, width, widthMeasureMode, height, heightMeasureMode) => {width: 0.0, height: 0.0};

let dummyGetChild (context, i) => assert false;

let dummyIsDirty context => false;

/*
   node->style.align_items = CSS_ALIGN_STRETCH;
   node->style.align_content = CSS_ALIGN_FLEX_START;

   node->style.direction = CSS_DIRECTION_INHERIT;
   node->style.flex_direction = CSS_FLEX_DIRECTION_COLUMN;

   node->style.overflow = CSS_OVERFLOW_VISIBLE;


   node->layout.dimensions[CSS_WIDTH] = CSS_UNDEFINED;
   node->layout.dimensions[CSS_HEIGHT] = CSS_UNDEFINED;

   // Such that the comparison is always going to be false
   node->layout.last_parent_direction = (css_direction_t)-1;
   node->layout.should_update = true;
   node->layout.next_cached_measurements_index = 0;

   node->layout.measured_dimensions[CSS_WIDTH] = CSS_UNDEFINED;
   node->layout.measured_dimensions[CSS_HEIGHT] = CSS_UNDEFINED;
   node->layout.cached_layout.width_measure_mode = (css_measure_mode_t)-1;
   node->layout.cached_layout.height_measure_mode = (css_measure_mode_t)-1;

 */
let dummyCachedMeasurement = {
  availableWidth: 0.0,
  availableHeight: 0.0,
  widthMeasureMode: CSS_MEASURE_MODE_NEGATIVE_ONE_WHATEVER_THAT_MEANS,
  heightMeasureMode: CSS_MEASURE_MODE_NEGATIVE_ONE_WHATEVER_THAT_MEANS,
  computedWidth: 0.0,
  computedHeight: 0.0
};

let rec theNullNode = {
  style: {
    direction: CSS_DIRECTION_INHERIT,
    flexDirection: CSS_FLEX_DIRECTION_COLUMN,
    justifyContent: CSS_JUSTIFY_FLEX_START,
    alignContent: CSS_ALIGN_FLEX_START,
    alignItems: CSS_ALIGN_STRETCH,
    alignSelf: CSS_ALIGN_AUTO,
    positionType: CSS_POSITION_RELATIVE,
    flexWrap: CSS_NOWRAP,
    overflow: CSS_OVERFLOW_VISIBLE,
    /**
     * Properties that start out as zero.
     */
    flex: 0.0,
    marginLeft: 0.0,
    marginTop: 0.0,
    marginRight: 0.0,
    marginBottom: 0.0,
    paddingLeft: 0.0,
    paddingTop: 0.0,
    paddingRight: 0.0,
    paddingBottom: 0.0,
    borderLeft: 0.0,
    borderTop: 0.0,
    borderRight: 0.0,
    borderBottom: 0.0,
    /**
     * Properties that start out as undefined.
     */
    left: cssUndefined,
    top: cssUndefined,
    right: cssUndefined,
    bottom: cssUndefined,
    minWidth: cssUndefined,
    minHeight: cssUndefined,
    maxWidth: cssUndefined,
    maxHeight: cssUndefined,
    marginStart: cssUndefined,
    marginEnd: cssUndefined,
    paddingStart: cssUndefined,
    paddingEnd: cssUndefined,
    borderStart: cssUndefined,
    borderEnd: cssUndefined,
    width: cssUndefined,
    height: cssUndefined
  },
  layout: {
    direction: CSS_DIRECTION_INHERIT,
    /* Instead of recomputing the entire layout every single time, we
     * cache some information to break early when nothing changed */
    shouldUpdate: true,
    generationCount: 0,
    lastParentDirection: CSS_DIRECTION_NEGATIVE_ONE_WHATEVER_THAT_MEANS,
    lastDirection: CSS_DIRECTION_INHERIT,
    nextCachedMeasurementsIndex: 0,
    /**
     * Hardcoded to 6 previous measurements.
     */
    cachedMeasurement1: dummyCachedMeasurement,
    cachedMeasurement2: dummyCachedMeasurement,
    cachedMeasurement3: dummyCachedMeasurement,
    cachedMeasurement4: dummyCachedMeasurement,
    cachedMeasurement5: dummyCachedMeasurement,
    cachedMeasurement6: dummyCachedMeasurement,
    cachedLayout: dummyCachedMeasurement,
    /**
     * Start out as zero.
     */
    lastRequestedWidth: 0.0,
    lastRequestedHeight: 0.0,
    lastParentMaxWidth: 0.0,
    lastParentMaxHeight: 0.0,
    lastWidth: 0.0,
    lastHeight: 0.0,
    lastTop: 0.0,
    lastLeft: 0.0,
    flexBasis: 0.0,
    left: 0.0,
    top: 0.0,
    right: 0.0,
    bottom: 0.0,
    /**
     * Start out as undefined.
     */
    width: cssUndefined,
    height: cssUndefined,
    measuredWidth: cssUndefined,
    measuredHeight: cssUndefined
  },
  childrenCount: 0,
  lineIndex: 0,
  /**
   * As a clever trick, to encode "NULL" node, we can create a recursive
   * binding and point nextChild to itsself, and interpreting that as NULL.
   */
  nextChild: theNullNode,
  measure: dummyMeasure,
  print: None,
  getChild: dummyGetChild,
  isDirty: dummyIsDirty,
  context: ()
};


/**
 * Layout getters.
 */
let layoutMeasuredDimensionForAxis node axis =>
  switch axis {
  | CSS_FLEX_DIRECTION_COLUMN => node.layout.measuredHeight
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.layout.measuredHeight
  | CSS_FLEX_DIRECTION_ROW => node.layout.measuredWidth
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.layout.measuredWidth
  };


/**
 * Pos[] based layout position for axis.
 */
let layoutPosPositionForAxis node axis =>
  switch axis {
  | CSS_FLEX_DIRECTION_COLUMN => node.layout.top
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.layout.bottom
  | CSS_FLEX_DIRECTION_ROW => node.layout.left
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.layout.right
  };


/**
 * Leading[] based layout position for axis.
 */
let layoutLeadingPositionForAxis = layoutPosPositionForAxis;


/**
 * Trailing[] based layout position for axis.
 */
let layoutTrailingPositionForAxis node axis =>
  switch axis {
  | CSS_FLEX_DIRECTION_COLUMN => node.layout.bottom
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.layout.top
  | CSS_FLEX_DIRECTION_ROW => node.layout.right
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.layout.left
  };


/**
 * Dimension style getter.
 */
let styleDimensionForAxis node axis =>
  switch axis {
  | CSS_FLEX_DIRECTION_COLUMN => node.style.height
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.style.height
  | CSS_FLEX_DIRECTION_ROW => node.style.width
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.style.width
  };


/**
 * Position style getter.
 */
let styleForPosition node pos =>
  switch pos {
  | CSS_LEFT => node.style.left
  | CSS_RIGHT => node.style.right
  | CSS_TOP => node.style.top
  | CSS_BOTTOM => node.style.bottom
  };


/**
 * Leading style getters.
 */
let styleLeadingPositionForAxis node axis =>
  switch axis {
  | CSS_FLEX_DIRECTION_COLUMN => node.style.top
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.style.bottom
  | CSS_FLEX_DIRECTION_ROW => node.style.left
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.style.right
  };

let styleLeadingPositionForAxisOrZero node axis => {
  let leadingPos = styleLeadingPositionForAxis node axis;
  not (isUndefined leadingPos) ? leadingPos : 0.0
};

let styleLeadingPaddingForAxis node axis =>
  switch axis {
  | CSS_FLEX_DIRECTION_COLUMN => node.style.paddingTop
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.style.paddingBottom
  | CSS_FLEX_DIRECTION_ROW => node.style.paddingLeft
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.style.paddingRight
  };

let styleLeadingMarginForAxis node leadingAxis =>
  switch leadingAxis {
  | CSS_FLEX_DIRECTION_COLUMN => node.style.marginTop
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.style.marginBottom
  | CSS_FLEX_DIRECTION_ROW => node.style.marginLeft
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.style.marginRight
  };

let styleLeadingBorderForAxis node leadingAxis =>
  switch leadingAxis {
  | CSS_FLEX_DIRECTION_COLUMN => node.style.borderTop
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.style.borderBottom
  | CSS_FLEX_DIRECTION_ROW => node.style.borderLeft
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.style.borderRight
  };


/**
 * Trailing style getters.
 */
let styleTrailingPositionForAxis node axis =>
  switch axis {
  | CSS_FLEX_DIRECTION_COLUMN => node.style.bottom
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.style.top
  | CSS_FLEX_DIRECTION_ROW => node.style.right
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.style.left
  };

let styleTrailingPaddingForAxis node axis =>
  switch axis {
  | CSS_FLEX_DIRECTION_COLUMN => node.style.paddingBottom
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.style.paddingTop
  | CSS_FLEX_DIRECTION_ROW => node.style.paddingRight
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.style.paddingLeft
  };

let styleTrailingMarginForAxis node trailingAxis =>
  switch trailingAxis {
  | CSS_FLEX_DIRECTION_COLUMN => node.style.marginBottom
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.style.marginTop
  | CSS_FLEX_DIRECTION_ROW => node.style.marginRight
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.style.marginLeft
  };

let styleTrailingBorderForAxis node trailingAxis =>
  switch trailingAxis {
  | CSS_FLEX_DIRECTION_COLUMN => node.style.borderBottom
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.style.borderTop
  | CSS_FLEX_DIRECTION_ROW => node.style.borderRight
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.style.borderLeft
  };


/**
 * Dim[] based layout dimension setter.
 */
let setDimLayoutDimensionForAxis node axis value =>
  switch axis {
  | CSS_FLEX_DIRECTION_COLUMN => node.layout.height = value
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.layout.height = value
  | CSS_FLEX_DIRECTION_ROW => node.layout.width = value
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.layout.width = value
  };


/**
 * Pos[] based layout dimension setter.
 */
let setPosLayoutPositionForAxis node axis value =>
  switch axis {
  | CSS_FLEX_DIRECTION_COLUMN => node.layout.top = value
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.layout.bottom = value
  | CSS_FLEX_DIRECTION_ROW => node.layout.left = value
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.layout.right = value
  };

let setLayoutMeasuredDimensionForAxis node axis value =>
  switch axis {
  | CSS_FLEX_DIRECTION_COLUMN => node.layout.measuredHeight = value
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.layout.measuredHeight = value
  | CSS_FLEX_DIRECTION_ROW => node.layout.measuredWidth = value
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.layout.measuredWidth = value
  };


/**
 * Leading layout setter.
 */
let setLayoutLeadingPositionForAxis node axis value =>
  switch axis {
  | CSS_FLEX_DIRECTION_COLUMN => node.layout.top = value
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.layout.bottom = value
  | CSS_FLEX_DIRECTION_ROW => node.layout.left = value
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.layout.right = value
  };


/**
 * Trailing layout setter.
 */
let setLayoutTrailingPositionForAxis node axis value =>
  switch axis {
  | CSS_FLEX_DIRECTION_COLUMN => node.layout.bottom = value
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE => node.layout.top = value
  | CSS_FLEX_DIRECTION_ROW => node.layout.right = value
  | CSS_FLEX_DIRECTION_ROW_REVERSE => node.layout.left = value
  };

let resolveDirection (node, parentDirection) => {
  let direction = node.style.direction;
  if (direction === CSS_DIRECTION_INHERIT) {
    parentDirection === CSS_DIRECTION_INHERIT ? CSS_DIRECTION_LTR : parentDirection
  } else {
    direction
  }
};

let resolveAxis (axis, direction) =>
  if (direction === CSS_DIRECTION_RTL) {
    if (axis === CSS_FLEX_DIRECTION_ROW) {
      CSS_FLEX_DIRECTION_ROW_REVERSE
    } else if (
      axis === CSS_FLEX_DIRECTION_ROW_REVERSE
    ) {
      CSS_FLEX_DIRECTION_ROW
    } else {
      axis
    }
  } else {
    axis
  };


/**
 * Make sure to default to CSS_FLEX_DIRECTION_COLUMN.
 */
let getFlexDirection node => node.style.flexDirection;

let isRowDirection flexDirection =>
  flexDirection === CSS_FLEX_DIRECTION_ROW || flexDirection === CSS_FLEX_DIRECTION_ROW_REVERSE;

let isColumnDirection flexDirection =>
  flexDirection === CSS_FLEX_DIRECTION_COLUMN || flexDirection === CSS_FLEX_DIRECTION_COLUMN_REVERSE;

let getCrossFlexDirection (flexDirection, direction) =>
  if (isColumnDirection flexDirection) {
    resolveAxis (CSS_FLEX_DIRECTION_ROW, direction)
  } else {
    CSS_FLEX_DIRECTION_COLUMN
  };

let isFlex node => node.style.positionType === CSS_POSITION_RELATIVE && node.style.flex > 0.0;

let isFlexWrap node => node.style.flexWrap === CSS_WRAP;

let isFlexBasisAuto =
  positive_flex_is_auto ?
    /* All flex values are auto. */
    fun node => true :
    /* A flex value > 0 implies a basis of zero. */
    fun node => node.style.flex <= 0.0;

/* Flex grow is implied by positive values for flex. */
let getFlexGrowFactor node => node.style.flex > 0.0 ? node.style.flex : 0.0;

let getFlexShrinkFactor =
  /* A flex shrink factor of 1 is implied by non-zero values for flex. */
  positive_flex_is_auto ?
    fun node => (node.style.flex != 0.0 ? 1.0 : 0.0) :
    /* A flex shrink factor of 1 is implied by negative values for flex. */
    fun node => node.style.flex < 0.0 ? 1.0 : 0.0;

let getLeadingMargin (node, axis) =>
  if (isRowDirection axis && not (isUndefined node.style.marginStart)) {
    node.style.marginStart
  } else {
    styleLeadingMarginForAxis node axis
  };

let getTrailingMargin (node, axis) =>
  if (isRowDirection axis && not (isUndefined node.style.marginEnd)) {
    node.style.marginEnd
  } else {
    styleTrailingMarginForAxis node axis
  };

let getLeadingPadding (node, axis) =>
  if (isRowDirection axis && not (isUndefined node.style.paddingStart) && node.style.paddingStart >= 0.0) {
    node.style.paddingStart
  } else {
    let leadingPadding = styleLeadingPaddingForAxis node axis;
    if (leadingPadding >= 0.0) {
      leadingPadding
    } else {
      0.0
    }
  };

let getTrailingPadding (node, axis) =>
  if (isRowDirection axis && not (isUndefined node.style.paddingEnd) && node.style.paddingEnd >= 0.0) {
    node.style.paddingEnd
  } else {
    let trailingPadding = styleTrailingPaddingForAxis node axis;
    trailingPadding >= 0.0 ? trailingPadding : 0.0
  };

let getLeadingBorder (node, axis) =>
  if (isRowDirection axis && not (isUndefined node.style.borderStart) && node.style.borderStart >= 0.0) {
    node.style.borderStart
  } else {
    let leadingBorder = styleLeadingBorderForAxis node axis;
    leadingBorder >= 0.0 ? leadingBorder : 0.0
  };

let getTrailingBorder (node, axis) =>
  if (isRowDirection axis && not (isUndefined node.style.borderEnd) && node.style.borderEnd >= 0.0) {
    node.style.borderEnd
  } else {
    let trailingBorder = styleTrailingBorderForAxis node axis;
    trailingBorder >= 0.0 ? trailingBorder : 0.0
  };

let getLeadingPaddingAndBorder (node, axis) =>
  getLeadingPadding (node, axis) +. getLeadingBorder (node, axis);

let getTrailingPaddingAndBorder (node, axis) =>
  getTrailingPadding (node, axis) +. getTrailingBorder (node, axis);

let getMarginAxis (node, axis) => getLeadingMargin (node, axis) +. getTrailingMargin (node, axis);

let getPaddingAndBorderAxis (node, axis) =>
  getLeadingPaddingAndBorder (node, axis) +. getTrailingPaddingAndBorder (node, axis);

let getAlignItem (node, child) =>
  child.style.alignSelf !== CSS_ALIGN_AUTO ? child.style.alignSelf : node.style.alignItems;

let getFlexDirection node => node.style.flexDirection;

let resolveAxis (flex_direction, direction) =>
  if (direction === CSS_DIRECTION_RTL) {
    if (flex_direction === CSS_FLEX_DIRECTION_ROW) {
      CSS_FLEX_DIRECTION_ROW_REVERSE
    } else if (
      flex_direction === CSS_FLEX_DIRECTION_ROW_REVERSE
    ) {
      CSS_FLEX_DIRECTION_ROW
    } else {
      flex_direction
    }
  } else {
    flex_direction
  };

let getCrossFlexDirection (flex_direction, direction) =>
  isColumnDirection flex_direction ?
    resolveAxis (CSS_FLEX_DIRECTION_ROW, direction) : CSS_FLEX_DIRECTION_COLUMN;

let getFlex node => node.style.flex;

let isFlex node => node.style.positionType === CSS_POSITION_RELATIVE && getFlex node != 0.0;

let getDimWithMargin (node, axis) =>
  layoutMeasuredDimensionForAxis node axis +. getLeadingMargin (node, axis) +. getTrailingMargin (node, axis);

let isStyleDimDefined (node, axis) => {
  let value = styleDimensionForAxis node axis;
  not (isUndefined value) && value >= 0.0
};

let isLayoutDimDefined (node, axis) => {
  let value = layoutMeasuredDimensionForAxis node axis;
  not (isUndefined value) && value >= 0.0
};

let isPosDefined (node, position) => not (isUndefined (styleForPosition node position));

let isMeasureDefined node => node.measure !== dummyMeasure;

let normalizePosition position => not (isUndefined position) ? position : 0.0;

let getPosition (node, position) => {
  let pos = styleForPosition node position;
  not (isUndefined pos) ? pos : 0.0
};

let boundAxisWithinMinAndMax (node, axis, value) => {
  let (min, max) =
    if (isColumnDirection axis) {
      (node.style.minHeight, node.style.maxHeight)
    } else if (isRowDirection axis) {
      (node.style.minWidth, node.style.maxWidth)
    } else {
      (cssUndefined, cssUndefined)
    };
  let boundValue = value;
  let nextBoundValue =
    if (not (isUndefined max) && max >= 0.0 && boundValue > max) {
      max
    } else {
      boundValue
    };
  let nextNextBoundValue =
    if (not (isUndefined min) && min >= 0.0 && nextBoundValue < min) {
      min
    } else {
      nextBoundValue
    };
  nextNextBoundValue
};

let fmaxf (a, b) => a > b ? a : b;

/* Like boundAxisWithinMinAndMax but also ensures that the value doesn't go below the
 * padding and border amount. */
let boundAxis (node, axis, value) => fmaxf (
  boundAxisWithinMinAndMax (node, axis, value),
  getPaddingAndBorderAxis (node, axis)
);

/* /* When the user specifically sets a value for width or height */ */
/* let setDimensionFromStyle (node, axis) => */
/*   /* The parent already computed us a width or height. We just skip it */ */
/*   if (isLayoutDimDefined (node, axis)) { */
/*     () */
/*   } else if */
/*     /* We only run if there's a width or height defined */ */
/*     (not (isStyleDimDefined (node, axis))) { */
/*     () */
/*   } else { */
/*     /* The dimensions can never be smaller than the padding and border */ */
/*     let dimValue = */
/*       fmaxf (boundAxis (node, axis, node.style [dim [axis]])) (getPaddingAndBorderAxis (node, axis)); */
/*     setDimLayoutDimensionForAxis node dimValue */
/*   }; */
/**
 * Sets trailing position for a child node for a given axis.
 */
let setTrailingPosition (node, child, axis) => {
  let measuredChildDimensionForAxis =
    child.style.positionType === CSS_POSITION_ABSOLUTE ? 0.0 : layoutMeasuredDimensionForAxis child axis;
  let childLayoutPosValueForAxis = layoutPosPositionForAxis child axis;
  let nextValue =
    layoutMeasuredDimensionForAxis node axis -. measuredChildDimensionForAxis -. childLayoutPosValueForAxis;
  setLayoutTrailingPositionForAxis child axis nextValue
};

/* If both left and right are defined, then use left. Otherwise return */
/* +left or -right depending on which is defined. */
let getRelativePosition (node, axis) => {
  let lead = styleLeadingPositionForAxis node axis;
  if (not (isUndefined lead)) {
    lead
  } else {
    -. (normalizePosition (styleTrailingPositionForAxis node axis))
  }
};


/**
 * TODO: A more functional version of this.
 */
let setPosition (node, direction) => {
  let mainAxis = resolveAxis (getFlexDirection node, direction);
  let crossAxis = getCrossFlexDirection (mainAxis, direction);
  setLayoutLeadingPositionForAxis
    node mainAxis (getLeadingMargin (node, mainAxis) +. getRelativePosition (node, mainAxis));
  /* node.layout.position[trailing[mainAxis]] = getTrailingMargin(node, mainAxis) +. */
  /*   getRelativePosition(node, mainAxis); */
  setLayoutTrailingPositionForAxis
    node mainAxis (getTrailingMargin (node, mainAxis) +. getRelativePosition (node, mainAxis));
  /* node.layout.position[leading[crossAxis]] = getLeadingMargin(node, crossAxis) +. */
  /*   getRelativePosition(node, crossAxis); */
  setLayoutLeadingPositionForAxis
    node crossAxis (getLeadingMargin (node, crossAxis) +. getRelativePosition (node, crossAxis));
  /* node.layout.position[trailing[crossAxis]] = getTrailingMargin(node, crossAxis) +. */
  /*   getRelativePosition(node, crossAxis); */
  setLayoutTrailingPositionForAxis
    node crossAxis (getTrailingMargin (node, crossAxis) +. getRelativePosition (node, crossAxis))
};