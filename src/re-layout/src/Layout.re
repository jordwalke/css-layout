/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

/**
 * cd src

   ocamlc -pp refmt -impl ./LayoutTypes.re 2>&1 | refmterr
   ocamlc -pp refmt -impl ./LayoutSupport.re 2>&1 | refmterr
   ocamlc -pp refmt -impl ./Layout.re 2>&1 | refmterr

 */
open LayoutTypes;

open LayoutSupport;

let gCurrentGenerationCount = ref 0;

let gDepth = ref 0;

let gPrintTree = {contents: false};

let gPrintChanges = {contents: false};

let gPrintSkips = {contents: false};

let spacer = "                                                            ";

let getSpacer level => {
  let spacerLen = String.length spacer;
  let lvl = level > spacerLen ? level : spacerLen;
  String.sub spacer lvl (String.length spacer)
};

let getModeName (mode, isLayoutInsteadOfMeasure) =>
  switch mode {
  | CSS_MEASURE_MODE_NEGATIVE_ONE_WHATEVER_THAT_MEANS =>
    isLayoutInsteadOfMeasure ?
      "LAY_CSS_MEASURE_MODE_NEGATIVE_ONE_WHATEVER_THAT_MEANS" :
      "CSS_MEASURE_MODE_NEGATIVE_ONE_WHATEVER_THAT_MEANS"
  | CSS_MEASURE_MODE_UNDEFINED => isLayoutInsteadOfMeasure ? "LAY_UNDEFINED" : "UNDEFINED"
  | CSS_MEASURE_MODE_EXACTLY => isLayoutInsteadOfMeasure ? "LAY_EXACTLY" : "EXACTLY"
  | CSS_MEASURE_MODE_AT_MOST => isLayoutInsteadOfMeasure ? "LAY_AT_MOST" : "AT_MOST"
  };

let canUseCachedMeasurement
    (
      availableWidth,
      availableHeight,
      marginRow,
      marginColumn,
      widthMeasureMode,
      heightMeasureMode,
      cachedLayout
    ) =>
  if (
    cachedLayout.availableWidth == availableWidth &&
    cachedLayout.availableHeight == availableHeight &&
    cachedLayout.widthMeasureMode == widthMeasureMode && cachedLayout.heightMeasureMode == heightMeasureMode
  ) {
    true
  } else if
    /* Is it an exact match?*/
    /* If the width is an exact match, try a fuzzy match on the height.*/
    (
      cachedLayout.widthMeasureMode == widthMeasureMode &&
      cachedLayout.availableWidth == availableWidth &&
      heightMeasureMode === CSS_MEASURE_MODE_EXACTLY &&
      availableHeight -. marginColumn == cachedLayout.computedHeight
    ) {
    true
  } else if
    /* If the height is an exact match, try a fuzzy match on the width.*/
    (
      cachedLayout.heightMeasureMode == heightMeasureMode &&
      cachedLayout.availableHeight == availableHeight &&
      widthMeasureMode === CSS_MEASURE_MODE_EXACTLY &&
      availableWidth -. marginRow == cachedLayout.computedWidth
    ) {
    true
  } else {
    false
  };

let cachedMeasurementAt layout i =>
  switch i {
  | 0 => layout.cachedMeasurement1
  | 1 => layout.cachedMeasurement1
  | 2 => layout.cachedMeasurement2
  | 3 => layout.cachedMeasurement3
  | 4 => layout.cachedMeasurement4
  | 5 => layout.cachedMeasurement5
  | _ => raise (Invalid_argument ("No cached measurement at " ^ string_of_int i))
  };


/**
 * This is a wrapper around the layoutNodeImpl function. It determines
 * whether the layout request is redundant and can be skipped.
 *
 * Parameters:
 *  Input parameters are the same as layoutNodeImpl (see above)
 *  Return parameter is true if layout was performed, false if skipped
 */
let rec layoutNodeInternal
        node
        availableWidth
        availableHeight
        parentDirection
        widthMeasureMode
        heightMeasureMode
        performLayout
        reason => {
  let layout = node.layout;
  gDepth.contents = gDepth.contents + 1;
  let needToVisitNode =
    node.isDirty node.context && layout.generationCount != gCurrentGenerationCount.contents ||
    layout.lastParentDirection != parentDirection;
  if needToVisitNode {
    /* Invalidate the cached results.*/
    layout.nextCachedMeasurementsIndex = 0;
    layout.cachedLayout.widthMeasureMode = CSS_MEASURE_MODE_NEGATIVE_ONE_WHATEVER_THAT_MEANS;
    layout.cachedLayout.heightMeasureMode = CSS_MEASURE_MODE_NEGATIVE_ONE_WHATEVER_THAT_MEANS
  };
  let cachedResults = ref None;
  /* Determine whether the results are already cached. We maintain a separate*/
  /* cache for layouts and measurements. A layout operation modifies the positions*/
  /* and dimensions for nodes in the subtree. The algorithm assumes that each node*/
  /* gets layed out a maximum of one time per tree layout, but multiple measurements*/
  /* may be required to resolve all of the flex dimensions.*/
  /* We handle nodes with measure functions specially here because they are the most*/
  /* expensive to measure, so it's worth avoiding redundant measurements if at all possible.*/
  if (isMeasureDefined node) {
    let marginAxisRow = getMarginAxis node CSS_FLEX_DIRECTION_ROW;
    let marginAxisColumn = getMarginAxis node CSS_FLEX_DIRECTION_COLUMN;
    /* First, try to use the layout cache.*/
    if (
      canUseCachedMeasurement (
        availableWidth,
        availableHeight,
        marginAxisRow,
        marginAxisColumn,
        widthMeasureMode,
        heightMeasureMode,
        layout.cachedLayout
      )
    ) {
      cachedResults.contents = Some layout.cachedLayout
    } else {
      /* Try to use the measurement cache.*/
      let foundCached = {contents: false};
      for i in 0 to (layout.nextCachedMeasurementsIndex - 1) {
        /* This is basically the "break" */
        if (not foundCached.contents) {
          let cachedMeasurementAtIndex = cachedMeasurementAt layout i;
          if (
            canUseCachedMeasurement (
              availableWidth,
              availableHeight,
              marginAxisRow,
              marginAxisColumn,
              widthMeasureMode,
              heightMeasureMode,
              cachedMeasurementAtIndex
            )
          ) {
            cachedResults.contents = Some cachedMeasurementAtIndex;
            foundCached.contents = true
          }
        }
      }
    }
  } else if performLayout {
    if (
      layout.cachedLayout.availableWidth == availableWidth &&
      layout.cachedLayout.availableHeight == availableHeight &&
      layout.cachedLayout.widthMeasureMode == widthMeasureMode &&
      layout.cachedLayout.heightMeasureMode == heightMeasureMode
    ) {
      cachedResults.contents = Some layout.cachedLayout
    }
  } else {
    let foundCached = {contents: false};
    for i in 0 to (layout.nextCachedMeasurementsIndex - 1) {
      /* This is basically the "break" */
      if (not foundCached.contents) {
        let cachedMeasurementAtIndex = cachedMeasurementAt layout i;
        if (
          cachedMeasurementAtIndex.availableWidth == availableWidth &&
          cachedMeasurementAtIndex.availableHeight == availableHeight &&
          cachedMeasurementAtIndex.widthMeasureMode == widthMeasureMode &&
          cachedMeasurementAtIndex.heightMeasureMode == heightMeasureMode
        ) {
          cachedResults.contents = Some cachedMeasurementAtIndex;
          foundCached.contents = true
        }
      }
    }
  };
  if (not needToVisitNode && cachedResults.contents != None) {
    let cachedResults_ =
      switch cachedResults.contents {
      | None => raise (Invalid_argument "Not possible")
      | Some cr => cr
      };
    layout.measuredWidth = cachedResults_.computedWidth;
    layout.measuredHeight = cachedResults_.computedHeight;
    if (gPrintChanges.contents && gPrintSkips.contents) {
      Printf.printf "%s%d.{[skipped] " (getSpacer gDepth.contents) gDepth.contents;
      switch node.print {
      | None => ()
      | Some printer => printer node.context
      };
      Printf.printf
        "wm: %s, hm: %s, aw: %f ah: %f => d: (%f, %f) %s\n"
        (getModeName (widthMeasureMode, performLayout))
        (getModeName (heightMeasureMode, performLayout))
        availableWidth
        availableHeight
        cachedResults_.computedWidth
        cachedResults_.computedHeight
        reason
    }
  } else {
    if gPrintChanges.contents {
      Printf.printf "%s%d.{%s" (getSpacer gDepth.contents) gDepth.contents (needToVisitNode ? "*" : "");
      switch node.print {
      | None => ()
      | Some printer => printer node.context
      };
      Printf.printf
        "wm: %s, hm: %s, aw: %f ah: %f %s\n"
        (getModeName (widthMeasureMode, performLayout))
        (getModeName (heightMeasureMode, performLayout))
        availableWidth
        availableHeight
        reason
    };
    layoutNodeImpl (
      node,
      availableWidth,
      availableHeight,
      parentDirection,
      widthMeasureMode,
      heightMeasureMode,
      performLayout
    );
    if gPrintChanges.contents {
      Printf.printf "%s%d.}%s" (getSpacer gDepth.contents) gDepth.contents (needToVisitNode ? "*" : "");
      switch node.print {
      | None => ()
      | Some printer => printer node.context
      };
      Printf.printf
        "wm: %s, hm: %s, d: (%f, %f) %s\n"
        (getModeName (widthMeasureMode, performLayout))
        (getModeName (heightMeasureMode, performLayout))
        layout.measuredWidth
        layout.measuredHeight
        reason
    };
    layout.lastParentDirection = parentDirection;
    if (cachedResults.contents === None) {
      if (layout.nextCachedMeasurementsIndex == css_max_cached_result_count) {
        if gPrintChanges.contents {
          Printf.printf "Out of cache entries!\n"
        };
        layout.nextCachedMeasurementsIndex = 0
      };
      let newCacheEntry =
        performLayout ?
          /* Use the single layout cache entry.*/
          layout.cachedLayout :
          {
            /* Allocate a new measurement cache entry.*/
            let newCacheEntry_ = cachedMeasurementAt layout layout.nextCachedMeasurementsIndex;
            layout.nextCachedMeasurementsIndex = layout.nextCachedMeasurementsIndex + 1;
            newCacheEntry_
          };
      newCacheEntry.availableWidth = availableWidth;
      newCacheEntry.availableHeight = availableHeight;
      newCacheEntry.widthMeasureMode = widthMeasureMode;
      newCacheEntry.heightMeasureMode = heightMeasureMode;
      newCacheEntry.computedWidth = layout.measuredWidth;
      newCacheEntry.computedHeight = layout.measuredHeight
    }
  };
  if performLayout {
    node.layout.width = node.layout.measuredWidth;
    node.layout.height = node.layout.measuredHeight;
    layout.shouldUpdate = true
  };
  gDepth.contents = gDepth.contents - 1;
  layout.generationCount = gCurrentGenerationCount.contents;
  needToVisitNode || cachedResults.contents === None
}
/**
 * By default, mathematical operations are floating point.
 */
and layoutNodeImpl
    (
      node,
      availableWidth,
      availableHeight,
      parentDirection,
      widthMeasureMode,
      heightMeasureMode,
      performLayout
    ) => {
  let (-.) = (-)
  and (+.) = (+)
  /* and (/.) = (/) */
  and (-) = (-.)
  and (+) = (+.)
  and (/) = (/.)
  and ( * ) = ( *. )
  /* and (+=) a b => a.contents = a.contents +. b */
  /* and (-=) a b => a.contents = a.contents -. b */
  /* and (~-.) = (~-) */
  and (~-) = (~-.);
  /* and (!) a => not a */
  let re_assert cond msg =>
    if (not cond) {
      raise (Invalid_argument msg)
    };

  /** START_GENERATED **/
  open ReJsPrelude;
  re_assert
    (isUndefined availableWidth ? widthMeasureMode === CSS_MEASURE_MODE_UNDEFINED : true)
    "availableWidth is indefinite so widthMeasureMode must be CSS_MEASURE_MODE_UNDEFINED";
  re_assert
    (isUndefined availableHeight ? heightMeasureMode === CSS_MEASURE_MODE_UNDEFINED : true)
    "availableHeight is indefinite so heightMeasureMode must be CSS_MEASURE_MODE_UNDEFINED";
  let paddingAndBorderAxisRow = getPaddingAndBorderAxis node CSS_FLEX_DIRECTION_ROW;
  let paddingAndBorderAxisColumn = getPaddingAndBorderAxis node CSS_FLEX_DIRECTION_COLUMN;
  let marginAxisRow = getMarginAxis node CSS_FLEX_DIRECTION_ROW;
  let marginAxisColumn = getMarginAxis node CSS_FLEX_DIRECTION_COLUMN;
  let direction = resolveDirection node parentDirection;
  node.layout.direction = direction;
  if (isMeasureDefined node) {
    let innerWidth = availableWidth - marginAxisRow - paddingAndBorderAxisRow;
    let innerHeight = availableHeight - marginAxisColumn - paddingAndBorderAxisColumn;
    if (widthMeasureMode === CSS_MEASURE_MODE_EXACTLY && heightMeasureMode === CSS_MEASURE_MODE_EXACTLY) {
      node.layout.measuredWidth = boundAxis node CSS_FLEX_DIRECTION_ROW (availableWidth - marginAxisRow);
      node.layout.measuredHeight =
        boundAxis node CSS_FLEX_DIRECTION_COLUMN (availableHeight - marginAxisColumn)
    } else if (
      innerWidth <= 0.0 || innerHeight <= 0.0
    ) {
      node.layout.measuredWidth = boundAxis node CSS_FLEX_DIRECTION_ROW 0.0;
      node.layout.measuredHeight = boundAxis node CSS_FLEX_DIRECTION_COLUMN 0.0
    } else {
      let measureDim = node.measure node.context innerWidth widthMeasureMode innerHeight heightMeasureMode;
      node.layout.measuredWidth =
        boundAxis
          node
          CSS_FLEX_DIRECTION_ROW
          (
            widthMeasureMode === CSS_MEASURE_MODE_UNDEFINED || widthMeasureMode === CSS_MEASURE_MODE_AT_MOST ?
              measureDim.width + paddingAndBorderAxisRow : availableWidth - marginAxisRow
          );
      node.layout.measuredHeight =
        boundAxis
          node
          CSS_FLEX_DIRECTION_COLUMN
          (
            heightMeasureMode === CSS_MEASURE_MODE_UNDEFINED || heightMeasureMode === CSS_MEASURE_MODE_AT_MOST ?
              measureDim.height + paddingAndBorderAxisColumn : availableHeight - marginAxisColumn
          )
    }
  } else {
    let childCount = node.childrenCount;
    if (childCount === 0) {
      node.layout.measuredWidth =
        boundAxis
          node
          CSS_FLEX_DIRECTION_ROW
          (
            widthMeasureMode === CSS_MEASURE_MODE_UNDEFINED || widthMeasureMode === CSS_MEASURE_MODE_AT_MOST ?
              paddingAndBorderAxisRow : availableWidth - marginAxisRow
          );
      node.layout.measuredHeight =
        boundAxis
          node
          CSS_FLEX_DIRECTION_COLUMN
          (
            heightMeasureMode === CSS_MEASURE_MODE_UNDEFINED || heightMeasureMode === CSS_MEASURE_MODE_AT_MOST ?
              paddingAndBorderAxisColumn : availableHeight - marginAxisColumn
          )
    } else {
      let shouldContinue = {contents: true};
      if !performLayout {
        if (
          (
            (widthMeasureMode === CSS_MEASURE_MODE_AT_MOST && availableWidth <= 0.0) &&
            heightMeasureMode === CSS_MEASURE_MODE_AT_MOST
          ) &&
          availableHeight <= 0.0
        ) {
          node.layout.measuredWidth = boundAxis node CSS_FLEX_DIRECTION_ROW 0.0;
          node.layout.measuredHeight = boundAxis node CSS_FLEX_DIRECTION_COLUMN 0.0;
          shouldContinue.contents = false
        } else if (
          widthMeasureMode === CSS_MEASURE_MODE_AT_MOST && availableWidth <= 0.0
        ) {
          node.layout.measuredWidth = boundAxis node CSS_FLEX_DIRECTION_ROW 0.0;
          node.layout.measuredHeight =
            boundAxis
              node
              CSS_FLEX_DIRECTION_COLUMN
              (isUndefined availableHeight ? 0.0 : availableHeight - marginAxisColumn);
          shouldContinue.contents = false
        } else if (
          heightMeasureMode === CSS_MEASURE_MODE_AT_MOST && availableHeight <= 0.0
        ) {
          node.layout.measuredWidth =
            boundAxis
              node
              CSS_FLEX_DIRECTION_ROW
              (isUndefined availableWidth ? 0.0 : availableWidth - marginAxisRow);
          node.layout.measuredHeight = boundAxis node CSS_FLEX_DIRECTION_COLUMN 0.0;
          shouldContinue.contents = false
        } else if (
          widthMeasureMode === CSS_MEASURE_MODE_EXACTLY && heightMeasureMode === CSS_MEASURE_MODE_EXACTLY
        ) {
          node.layout.measuredWidth = boundAxis node CSS_FLEX_DIRECTION_ROW (availableWidth - marginAxisRow);
          node.layout.measuredHeight =
            boundAxis node CSS_FLEX_DIRECTION_COLUMN (availableHeight - marginAxisColumn);
          shouldContinue.contents = false
        }
      };
      if shouldContinue.contents {
        let mainAxis = resolveAxis (getFlexDirection node) direction;
        let crossAxis = getCrossFlexDirection mainAxis direction;
        let isMainAxisRow = isRowDirection mainAxis;
        let justifyContent = node.style.justifyContent;
        let isNodeFlexWrap = isFlexWrap node;
        let firstAbsoluteChild = {contents: theNullNode};
        let currentAbsoluteChild = {contents: theNullNode};
        let leadingPaddingAndBorderMain = getLeadingPaddingAndBorder node mainAxis;
        let trailingPaddingAndBorderMain = getTrailingPaddingAndBorder node mainAxis;
        let leadingPaddingAndBorderCross = getLeadingPaddingAndBorder node crossAxis;
        let paddingAndBorderAxisMain = getPaddingAndBorderAxis node mainAxis;
        let paddingAndBorderAxisCross = getPaddingAndBorderAxis node crossAxis;
        let measureModeMainDim = isMainAxisRow ? widthMeasureMode : heightMeasureMode;
        let measureModeCrossDim = isMainAxisRow ? heightMeasureMode : widthMeasureMode;
        let availableInnerWidth = availableWidth - marginAxisRow - paddingAndBorderAxisRow;
        let availableInnerHeight = availableHeight - marginAxisColumn - paddingAndBorderAxisColumn;
        let availableInnerMainDim = isMainAxisRow ? availableInnerWidth : availableInnerHeight;
        let availableInnerCrossDim = isMainAxisRow ? availableInnerHeight : availableInnerWidth;
        let child = {contents: theNullNode};
        /* let i = 0; */
        let childWidth = {contents: 0.0};
        let childHeight = {contents: 0.0};
        let childWidthMeasureMode = {contents: CSS_MEASURE_MODE_UNDEFINED};
        let childHeightMeasureMode = {contents: CSS_MEASURE_MODE_UNDEFINED};
        for i in 0 to (childCount -. 1) {
          child.contents = node.getChild node.context i;
          if performLayout {
            let childDirection = resolveDirection child.contents direction;
            setPosition child.contents childDirection
          };
          if (child.contents.style.positionType === CSS_POSITION_ABSOLUTE) {
            if (firstAbsoluteChild.contents === theNullNode) {
              firstAbsoluteChild.contents = child.contents
            };
            if (currentAbsoluteChild.contents !== theNullNode) {
              currentAbsoluteChild.contents.nextChild = child.contents
            };
            currentAbsoluteChild.contents = child.contents;
            child.contents.nextChild = theNullNode
          } else if (
            isMainAxisRow && isStyleDimDefined child.contents CSS_FLEX_DIRECTION_ROW
          ) {
            child.contents.layout.flexBasis =
              fmaxf
                child.contents.style.width (getPaddingAndBorderAxis child.contents CSS_FLEX_DIRECTION_ROW)
          } else if (
            !isMainAxisRow && isStyleDimDefined child.contents CSS_FLEX_DIRECTION_COLUMN
          ) {
            child.contents.layout.flexBasis =
              fmaxf
                child.contents.style.height
                (getPaddingAndBorderAxis child.contents CSS_FLEX_DIRECTION_COLUMN)
          } else if (
            !(isFlexBasisAuto child.contents) && !(isUndefined availableInnerMainDim)
          ) {
            child.contents.layout.flexBasis = fmaxf 0.0 (getPaddingAndBorderAxis child.contents mainAxis)
          } else {
            childWidth.contents = cssUndefined;
            childHeight.contents = cssUndefined;
            childWidthMeasureMode.contents = CSS_MEASURE_MODE_UNDEFINED;
            childHeightMeasureMode.contents = CSS_MEASURE_MODE_UNDEFINED;
            if (isStyleDimDefined child.contents CSS_FLEX_DIRECTION_ROW) {
              childWidth.contents =
                child.contents.style.width + getMarginAxis child.contents CSS_FLEX_DIRECTION_ROW;
              childWidthMeasureMode.contents = CSS_MEASURE_MODE_EXACTLY
            };
            if (isStyleDimDefined child.contents CSS_FLEX_DIRECTION_COLUMN) {
              childHeight.contents =
                child.contents.style.height + getMarginAxis child.contents CSS_FLEX_DIRECTION_COLUMN;
              childHeightMeasureMode.contents = CSS_MEASURE_MODE_EXACTLY
            };
            if ((!isMainAxisRow && isUndefined childWidth.contents) && !(isUndefined availableInnerWidth)) {
              childWidth.contents = availableInnerWidth;
              childWidthMeasureMode.contents = CSS_MEASURE_MODE_AT_MOST
            };
            if (node.style.overflow === CSS_OVERFLOW_HIDDEN) {
              if ((isMainAxisRow && isUndefined childHeight.contents) && !(isUndefined availableInnerHeight)) {
                childHeight.contents = availableInnerHeight;
                childHeightMeasureMode.contents = CSS_MEASURE_MODE_AT_MOST
              }
            };
            let _ =
              layoutNodeInternal
                child.contents
                childWidth.contents
                childHeight.contents
                direction
                childWidthMeasureMode.contents
                childHeightMeasureMode.contents
                false
                "measure";
            child.contents.layout.flexBasis =
              fmaxf
                (isMainAxisRow ? child.contents.layout.measuredWidth : child.contents.layout.measuredHeight)
                (getPaddingAndBorderAxis child.contents mainAxis)
          }
        };
        let startOfLineIndex = {contents: 0};
        let endOfLineIndex = {contents: 0};
        let lineCount = {contents: 0};
        let totalLineCrossDim = {contents: 0.0};
        let maxLineMainDim = {contents: 0.0};
        while (endOfLineIndex.contents < childCount) {
          let itemsOnLine = {contents: 0};
          let sizeConsumedOnCurrentLine = {contents: 0.0};
          let totalFlexGrowFactors = {contents: 0.0};
          let totalFlexShrinkScaledFactors = {contents: 0.0};
          let curIndex = {contents: startOfLineIndex.contents};
          let firstRelativeChild = {contents: theNullNode};
          let currentRelativeChild = {contents: theNullNode};
          let shouldContinue = {contents: true};
          while (curIndex.contents < childCount && shouldContinue.contents) {
            child.contents = node.getChild node.context curIndex.contents;
            child.contents.lineIndex = lineCount.contents;
            if (child.contents.style.positionType !== CSS_POSITION_ABSOLUTE) {
              let outerFlexBasis = child.contents.layout.flexBasis + getMarginAxis child.contents mainAxis;
              if (
                (
                  sizeConsumedOnCurrentLine.contents + outerFlexBasis > availableInnerMainDim && isNodeFlexWrap
                ) &&
                itemsOnLine.contents > 0
              ) {
                shouldContinue.contents = false
              } else {
                sizeConsumedOnCurrentLine.contents = sizeConsumedOnCurrentLine.contents + outerFlexBasis;
                itemsOnLine.contents = itemsOnLine.contents +. 1;
                if (isFlex child.contents) {
                  totalFlexGrowFactors.contents =
                    totalFlexGrowFactors.contents + getFlexGrowFactor child.contents;
                  totalFlexShrinkScaledFactors.contents =
                    totalFlexShrinkScaledFactors.contents +
                    getFlexShrinkFactor child.contents * child.contents.layout.flexBasis
                };
                if (firstRelativeChild.contents === theNullNode) {
                  firstRelativeChild.contents = child.contents
                };
                if (currentRelativeChild.contents !== theNullNode) {
                  currentRelativeChild.contents.nextChild = child.contents
                };
                currentRelativeChild.contents = child.contents;
                child.contents.nextChild = theNullNode;
                curIndex.contents = curIndex.contents +. 1;
                endOfLineIndex.contents = endOfLineIndex.contents +. 1
              }
            } else {
              curIndex.contents = curIndex.contents +. 1;
              endOfLineIndex.contents = endOfLineIndex.contents +. 1
            }
          };
          let canSkipFlex = !performLayout && measureModeCrossDim === CSS_MEASURE_MODE_EXACTLY;
          let leadingMainDim = {contents: 0.0};
          let betweenMainDim = {contents: 0.0};
          let remainingFreeSpace = {contents: 0.0};
          if !(isUndefined availableInnerMainDim) {
            remainingFreeSpace.contents = availableInnerMainDim - sizeConsumedOnCurrentLine.contents
          } else if (
            sizeConsumedOnCurrentLine.contents < 0.0
          ) {
            remainingFreeSpace.contents = - sizeConsumedOnCurrentLine.contents
          };
          let originalRemainingFreeSpace = remainingFreeSpace.contents;
          let deltaFreeSpace = {contents: 0.0};
          if !canSkipFlex {
            let childFlexBasis = {contents: 0.0};
            let flexShrinkScaledFactor = {contents: 0.0};
            let flexGrowFactor = {contents: 0.0};
            let baseMainSize = {contents: 0.0};
            let boundMainSize = {contents: 0.0};
            let deltaFlexShrinkScaledFactors = {contents: 0.0};
            let deltaFlexGrowFactors = {contents: 0.0};
            currentRelativeChild.contents = firstRelativeChild.contents;
            while (currentRelativeChild.contents !== theNullNode) {
              childFlexBasis.contents = currentRelativeChild.contents.layout.flexBasis;
              if (remainingFreeSpace.contents < 0.0) {
                flexShrinkScaledFactor.contents =
                  getFlexShrinkFactor currentRelativeChild.contents * childFlexBasis.contents;
                if (flexShrinkScaledFactor.contents !== 0.0) {
                  baseMainSize.contents =
                    childFlexBasis.contents +
                    remainingFreeSpace.contents / totalFlexShrinkScaledFactors.contents *
                    flexShrinkScaledFactor.contents;
                  boundMainSize.contents =
                    boundAxis currentRelativeChild.contents mainAxis baseMainSize.contents;
                  if (baseMainSize.contents !== boundMainSize.contents) {
                    deltaFreeSpace.contents =
                      deltaFreeSpace.contents - boundMainSize.contents - childFlexBasis.contents;
                    deltaFlexShrinkScaledFactors.contents =
                      deltaFlexShrinkScaledFactors.contents - flexShrinkScaledFactor.contents
                  }
                }
              } else if (
                remainingFreeSpace.contents > 0.0
              ) {
                flexGrowFactor.contents = getFlexGrowFactor currentRelativeChild.contents;
                if (flexGrowFactor.contents !== 0.0) {
                  baseMainSize.contents =
                    childFlexBasis.contents +
                    remainingFreeSpace.contents / totalFlexGrowFactors.contents * flexGrowFactor.contents;
                  boundMainSize.contents =
                    boundAxis currentRelativeChild.contents mainAxis baseMainSize.contents;
                  if (baseMainSize.contents !== boundMainSize.contents) {
                    deltaFreeSpace.contents =
                      deltaFreeSpace.contents - boundMainSize.contents - childFlexBasis.contents;
                    deltaFlexGrowFactors.contents = deltaFlexGrowFactors.contents - flexGrowFactor.contents
                  }
                }
              };
              currentRelativeChild.contents = currentRelativeChild.contents.nextChild
            };
            totalFlexShrinkScaledFactors.contents =
              totalFlexShrinkScaledFactors.contents + deltaFlexShrinkScaledFactors.contents;
            totalFlexGrowFactors.contents = totalFlexGrowFactors.contents + deltaFlexGrowFactors.contents;
            remainingFreeSpace.contents = remainingFreeSpace.contents + deltaFreeSpace.contents;
            deltaFreeSpace.contents = 0.0;
            currentRelativeChild.contents = firstRelativeChild.contents;
            while (currentRelativeChild.contents !== theNullNode) {
              childFlexBasis.contents = currentRelativeChild.contents.layout.flexBasis;
              let updatedMainSize = {contents: childFlexBasis.contents};
              if (remainingFreeSpace.contents < 0.0) {
                flexShrinkScaledFactor.contents =
                  getFlexShrinkFactor currentRelativeChild.contents * childFlexBasis.contents;
                if (flexShrinkScaledFactor.contents !== 0.0) {
                  updatedMainSize.contents =
                    boundAxis
                      currentRelativeChild.contents
                      mainAxis
                      (
                        childFlexBasis.contents +
                        remainingFreeSpace.contents / totalFlexShrinkScaledFactors.contents *
                        flexShrinkScaledFactor.contents
                      )
                }
              } else if (
                remainingFreeSpace.contents > 0.0
              ) {
                flexGrowFactor.contents = getFlexGrowFactor currentRelativeChild.contents;
                if (flexGrowFactor.contents !== 0.0) {
                  updatedMainSize.contents =
                    boundAxis
                      currentRelativeChild.contents
                      mainAxis
                      (
                        childFlexBasis.contents +
                        remainingFreeSpace.contents / totalFlexGrowFactors.contents * flexGrowFactor.contents
                      )
                }
              };
              deltaFreeSpace.contents =
                deltaFreeSpace.contents - updatedMainSize.contents - childFlexBasis.contents;
              if isMainAxisRow {
                childWidth.contents =
                  updatedMainSize.contents +
                  getMarginAxis currentRelativeChild.contents CSS_FLEX_DIRECTION_ROW;
                childWidthMeasureMode.contents = CSS_MEASURE_MODE_EXACTLY;
                if !(isStyleDimDefined currentRelativeChild.contents CSS_FLEX_DIRECTION_COLUMN) {
                  childHeight.contents = availableInnerCrossDim;
                  childHeightMeasureMode.contents =
                    isUndefined childHeight.contents ? CSS_MEASURE_MODE_UNDEFINED : CSS_MEASURE_MODE_AT_MOST
                } else {
                  childHeight.contents =
                    currentRelativeChild.contents.style.height +
                    getMarginAxis currentRelativeChild.contents CSS_FLEX_DIRECTION_COLUMN;
                  childHeightMeasureMode.contents = CSS_MEASURE_MODE_EXACTLY
                }
              } else {
                childHeight.contents =
                  updatedMainSize.contents +
                  getMarginAxis currentRelativeChild.contents CSS_FLEX_DIRECTION_COLUMN;
                childHeightMeasureMode.contents = CSS_MEASURE_MODE_EXACTLY;
                if !(isStyleDimDefined currentRelativeChild.contents CSS_FLEX_DIRECTION_ROW) {
                  childWidth.contents = availableInnerCrossDim;
                  childWidthMeasureMode.contents =
                    isUndefined childWidth.contents ? CSS_MEASURE_MODE_UNDEFINED : CSS_MEASURE_MODE_AT_MOST
                } else {
                  childWidth.contents =
                    currentRelativeChild.contents.style.width +
                    getMarginAxis currentRelativeChild.contents CSS_FLEX_DIRECTION_ROW;
                  childWidthMeasureMode.contents = CSS_MEASURE_MODE_EXACTLY
                }
              };
              let requiresStretchLayout =
                !(isStyleDimDefined currentRelativeChild.contents crossAxis) &&
                getAlignItem node currentRelativeChild.contents === CSS_ALIGN_STRETCH;
              let _ =
                layoutNodeInternal
                  currentRelativeChild.contents
                  childWidth.contents
                  childHeight.contents
                  direction
                  childWidthMeasureMode.contents
                  childHeightMeasureMode.contents
                  (performLayout && !requiresStretchLayout)
                  "flex";
              currentRelativeChild.contents = currentRelativeChild.contents.nextChild
            }
          };
          remainingFreeSpace.contents = originalRemainingFreeSpace + deltaFreeSpace.contents;
          if (measureModeMainDim === CSS_MEASURE_MODE_AT_MOST) {
            remainingFreeSpace.contents = 0.0
          };
          if (justifyContent !== CSS_JUSTIFY_FLEX_START) {
            if (justifyContent === CSS_JUSTIFY_CENTER) {
              leadingMainDim.contents = remainingFreeSpace.contents / 2.0
            } else if (
              justifyContent === CSS_JUSTIFY_FLEX_END
            ) {
              leadingMainDim.contents = remainingFreeSpace.contents
            } else if (
              justifyContent === CSS_JUSTIFY_SPACE_BETWEEN
            ) {
              remainingFreeSpace.contents = fmaxf remainingFreeSpace.contents 0.0;
              if (itemsOnLine.contents > 1) {
                betweenMainDim.contents =
                  remainingFreeSpace.contents / float_of_int (itemsOnLine.contents -. 1)
              } else {
                betweenMainDim.contents = 0.0
              }
            } else if (
              justifyContent === CSS_JUSTIFY_SPACE_AROUND
            ) {
              betweenMainDim.contents = remainingFreeSpace.contents / float_of_int itemsOnLine.contents;
              leadingMainDim.contents = betweenMainDim.contents / 2.0
            }
          };
          let mainDim = {contents: leadingPaddingAndBorderMain + leadingMainDim.contents};
          let crossDim = {contents: 0.0};
          for i in startOfLineIndex.contents to (endOfLineIndex.contents -. 1) {
            child.contents = node.getChild node.context i;
            if (
              child.contents.style.positionType === CSS_POSITION_ABSOLUTE &&
              !(isUndefined (styleLeadingPositionForAxis child.contents mainAxis))
            ) {
              if performLayout {
                setPosLayoutPositionForAxis
                  child.contents
                  mainAxis
                  (
                    styleLeadingPositionForAxisOrZero child.contents mainAxis + getLeadingBorder node mainAxis +
                    getLeadingMargin child.contents mainAxis
                  )
              }
            } else {
              if performLayout {
                setPosLayoutPositionForAxis
                  child.contents
                  mainAxis
                  (layoutPosPositionForAxis child.contents mainAxis + mainDim.contents)
              };
              if (child.contents.style.positionType === CSS_POSITION_RELATIVE) {
                if canSkipFlex {
                  mainDim.contents =
                    mainDim.contents + betweenMainDim.contents + getMarginAxis child.contents mainAxis +
                    child.contents.layout.flexBasis;
                  crossDim.contents = availableInnerCrossDim
                } else {
                  mainDim.contents =
                    mainDim.contents + betweenMainDim.contents + getDimWithMargin child.contents mainAxis;
                  crossDim.contents = fmaxf crossDim.contents (getDimWithMargin child.contents crossAxis)
                }
              }
            }
          };
          mainDim.contents = mainDim.contents + trailingPaddingAndBorderMain;
          let containerCrossAxis = {contents: availableInnerCrossDim};
          if (
            measureModeCrossDim === CSS_MEASURE_MODE_UNDEFINED ||
            measureModeCrossDim === CSS_MEASURE_MODE_AT_MOST
          ) {
            containerCrossAxis.contents =
              boundAxis node crossAxis (crossDim.contents + paddingAndBorderAxisCross) - paddingAndBorderAxisCross;
            if (measureModeCrossDim === CSS_MEASURE_MODE_AT_MOST) {
              containerCrossAxis.contents = fminf containerCrossAxis.contents availableInnerCrossDim
            }
          };
          if (!isNodeFlexWrap && measureModeCrossDim === CSS_MEASURE_MODE_EXACTLY) {
            crossDim.contents = availableInnerCrossDim
          };
          crossDim.contents =
            boundAxis node crossAxis (crossDim.contents + paddingAndBorderAxisCross) - paddingAndBorderAxisCross;
          if performLayout {
            for i in startOfLineIndex.contents to (endOfLineIndex.contents -. 1) {
              child.contents = node.getChild node.context i;
              if (child.contents.style.positionType === CSS_POSITION_ABSOLUTE) {
                if !(isUndefined (styleLeadingPositionForAxis child.contents crossAxis)) {
                  setPosLayoutPositionForAxis
                    child.contents
                    crossAxis
                    (
                      styleLeadingPositionForAxisOrZero child.contents crossAxis +
                      getLeadingBorder node crossAxis +
                      getLeadingMargin child.contents crossAxis
                    )
                } else {
                  setPosLayoutPositionForAxis
                    child.contents
                    crossAxis
                    (leadingPaddingAndBorderCross + getLeadingMargin child.contents crossAxis)
                }
              } else {
                let leadingCrossDim = {contents: leadingPaddingAndBorderCross};
                let alignItem = getAlignItem node child.contents;
                if (alignItem === CSS_ALIGN_STRETCH) {
                  childWidth.contents =
                    child.contents.layout.measuredWidth +
                    getMarginAxis child.contents CSS_FLEX_DIRECTION_ROW;
                  childHeight.contents =
                    child.contents.layout.measuredHeight +
                    getMarginAxis child.contents CSS_FLEX_DIRECTION_COLUMN;
                  let isCrossSizeDefinite = {contents: false};
                  if isMainAxisRow {
                    isCrossSizeDefinite.contents =
                      isStyleDimDefined child.contents CSS_FLEX_DIRECTION_COLUMN;
                    childHeight.contents = crossDim.contents
                  } else {
                    isCrossSizeDefinite.contents = isStyleDimDefined child.contents CSS_FLEX_DIRECTION_ROW;
                    childWidth.contents = crossDim.contents
                  };
                  if !isCrossSizeDefinite.contents {
                    childWidthMeasureMode.contents =
                      isUndefined childWidth.contents ?
                        CSS_MEASURE_MODE_UNDEFINED : CSS_MEASURE_MODE_EXACTLY;
                    childHeightMeasureMode.contents =
                      isUndefined childHeight.contents ?
                        CSS_MEASURE_MODE_UNDEFINED : CSS_MEASURE_MODE_EXACTLY;
                    let _ =
                      layoutNodeInternal
                        child.contents
                        childWidth.contents
                        childHeight.contents
                        direction
                        childWidthMeasureMode.contents
                        childHeightMeasureMode.contents
                        true
                        "stretch";
                    ()
                  }
                } else if (
                  alignItem !== CSS_ALIGN_FLEX_START
                ) {
                  let remainingCrossDim =
                    containerCrossAxis.contents - getDimWithMargin child.contents crossAxis;
                  if (alignItem === CSS_ALIGN_CENTER) {
                    leadingCrossDim.contents = leadingCrossDim.contents + remainingCrossDim / 2.0
                  } else {
                    leadingCrossDim.contents = leadingCrossDim.contents + remainingCrossDim
                  }
                };
                setPosLayoutPositionForAxis
                  child.contents
                  crossAxis
                  (
                    layoutPosPositionForAxis child.contents crossAxis + totalLineCrossDim.contents +
                    leadingCrossDim.contents
                  )
              }
            }
          };
          totalLineCrossDim.contents = totalLineCrossDim.contents + crossDim.contents;
          maxLineMainDim.contents = fmaxf maxLineMainDim.contents mainDim.contents;
          lineCount.contents = lineCount.contents +. 1;
          startOfLineIndex.contents = endOfLineIndex.contents;
          endOfLineIndex.contents = startOfLineIndex.contents
        };
        if ((lineCount.contents > 1 && performLayout) && !(isUndefined availableInnerCrossDim)) {
          let remainingAlignContentDim = availableInnerCrossDim - totalLineCrossDim.contents;
          let crossDimLead = {contents: 0.0};
          let currentLead = {contents: leadingPaddingAndBorderCross};
          let alignContent = node.style.alignContent;
          if (alignContent === CSS_ALIGN_FLEX_END) {
            currentLead.contents = currentLead.contents + remainingAlignContentDim
          } else if (
            alignContent === CSS_ALIGN_CENTER
          ) {
            currentLead.contents = currentLead.contents + remainingAlignContentDim / 2.0
          } else if (
            alignContent === CSS_ALIGN_STRETCH
          ) {
            if (availableInnerCrossDim > totalLineCrossDim.contents) {
              crossDimLead.contents = remainingAlignContentDim / float_of_int lineCount.contents
            }
          };
          let endIndex = {contents: 0};
          for i in 0 to (lineCount.contents -. 1) {
            let startIndex = endIndex.contents;
            let j = {contents: startIndex};
            let lineHeight = {contents: 0.0};
            let shouldContinue = {contents: false};
            while (j.contents < childCount && shouldContinue.contents) {
              child.contents = node.getChild node.context j.contents;
              if (child.contents.style.positionType === CSS_POSITION_RELATIVE) {
                if (child.contents.lineIndex !== i) {
                  shouldContinue.contents = false
                } else if (
                  isLayoutDimDefined child.contents crossAxis
                ) {
                  lineHeight.contents =
                    fmaxf
                      lineHeight.contents
                      (
                        layoutMeasuredDimensionForAxis child.contents crossAxis +
                        getMarginAxis child.contents crossAxis
                      )
                }
              };
              j.contents = j.contents +. 1
            };
            endIndex.contents = j.contents;
            lineHeight.contents = lineHeight.contents + crossDimLead.contents;
            if performLayout {
              for j in startIndex to (endIndex.contents -. 1) {
                child.contents = node.getChild node.context j;
                if (child.contents.style.positionType === CSS_POSITION_RELATIVE) {
                  let alignContentAlignItem = getAlignItem node child.contents;
                  if (alignContentAlignItem === CSS_ALIGN_FLEX_START) {
                    setPosLayoutPositionForAxis
                      child.contents
                      crossAxis
                      (currentLead.contents + getLeadingMargin child.contents crossAxis)
                  } else if (
                    alignContentAlignItem === CSS_ALIGN_FLEX_END
                  ) {
                    setPosLayoutPositionForAxis
                      child.contents
                      crossAxis
                      (
                        currentLead.contents + lineHeight.contents -
                        getTrailingMargin child.contents crossAxis -
                        layoutMeasuredDimensionForAxis child.contents crossAxis
                      )
                  } else if (
                    alignContentAlignItem === CSS_ALIGN_CENTER
                  ) {
                    childHeight.contents = layoutMeasuredDimensionForAxis child.contents crossAxis;
                    setPosLayoutPositionForAxis
                      child.contents
                      crossAxis
                      (currentLead.contents + (lineHeight.contents - childHeight.contents) / 2.0)
                  } else if (
                    alignContentAlignItem === CSS_ALIGN_STRETCH
                  ) {
                    setPosLayoutPositionForAxis
                      child.contents
                      crossAxis
                      (currentLead.contents + getLeadingMargin child.contents crossAxis)
                  }
                }
              }
            };
            currentLead.contents = currentLead.contents + lineHeight.contents
          }
        };
        node.layout.measuredWidth = boundAxis node CSS_FLEX_DIRECTION_ROW (availableWidth - marginAxisRow);
        node.layout.measuredHeight =
          boundAxis node CSS_FLEX_DIRECTION_COLUMN (availableHeight - marginAxisColumn);
        if (measureModeMainDim === CSS_MEASURE_MODE_UNDEFINED) {
          setLayoutMeasuredDimensionForAxis node mainAxis (boundAxis node mainAxis maxLineMainDim.contents)
        } else if (
          measureModeMainDim === CSS_MEASURE_MODE_AT_MOST
        ) {
          setLayoutMeasuredDimensionForAxis
            node
            mainAxis
            (
              fmaxf
                (
                  fminf
                    (availableInnerMainDim + paddingAndBorderAxisMain)
                    (boundAxisWithinMinAndMax node mainAxis maxLineMainDim.contents)
                )
                paddingAndBorderAxisMain
            )
        };
        if (measureModeCrossDim === CSS_MEASURE_MODE_UNDEFINED) {
          setLayoutMeasuredDimensionForAxis
            node
            crossAxis
            (boundAxis node crossAxis (totalLineCrossDim.contents + paddingAndBorderAxisCross))
        } else if (
          measureModeCrossDim === CSS_MEASURE_MODE_AT_MOST
        ) {
          setLayoutMeasuredDimensionForAxis
            node
            crossAxis
            (
              fmaxf
                (
                  fminf
                    (availableInnerCrossDim + paddingAndBorderAxisCross)
                    (
                      boundAxisWithinMinAndMax
                        node crossAxis (totalLineCrossDim.contents + paddingAndBorderAxisCross)
                    )
                )
                paddingAndBorderAxisCross
            )
        };
        if performLayout {
          let needsMainTrailingPos = {contents: false};
          let needsCrossTrailingPos = {contents: false};
          if (mainAxis === CSS_FLEX_DIRECTION_ROW_REVERSE || mainAxis === CSS_FLEX_DIRECTION_COLUMN_REVERSE) {
            needsMainTrailingPos.contents = true
          };
          if (
            crossAxis === CSS_FLEX_DIRECTION_ROW_REVERSE || crossAxis === CSS_FLEX_DIRECTION_COLUMN_REVERSE
          ) {
            needsCrossTrailingPos.contents = true
          };
          if (needsMainTrailingPos.contents || needsCrossTrailingPos.contents) {
            for i in 0 to (childCount -. 1) {
              child.contents = node.getChild node.context i;
              if needsMainTrailingPos.contents {
                setTrailingPosition node child.contents mainAxis
              };
              if needsCrossTrailingPos.contents {
                setTrailingPosition node child.contents crossAxis
              }
            }
          }
        };
        currentAbsoluteChild.contents = firstAbsoluteChild.contents;
        while (currentAbsoluteChild.contents !== theNullNode) {
          if performLayout {
            childWidth.contents = cssUndefined;
            childHeight.contents = cssUndefined;
            if (isStyleDimDefined currentAbsoluteChild.contents CSS_FLEX_DIRECTION_ROW) {
              childWidth.contents =
                currentAbsoluteChild.contents.style.width +
                getMarginAxis currentAbsoluteChild.contents CSS_FLEX_DIRECTION_ROW
            } else if (
              isPosDefined currentAbsoluteChild.contents CSS_LEFT &&
              isPosDefined currentAbsoluteChild.contents CSS_RIGHT
            ) {
              childWidth.contents =
                node.layout.measuredWidth - (
                  getLeadingBorder node CSS_FLEX_DIRECTION_ROW +
                  getTrailingBorder node CSS_FLEX_DIRECTION_ROW
                ) - (
                  currentAbsoluteChild.contents.style.left + currentAbsoluteChild.contents.style.right
                );
              childWidth.contents =
                boundAxis currentAbsoluteChild.contents CSS_FLEX_DIRECTION_ROW childWidth.contents
            };
            if (isStyleDimDefined currentAbsoluteChild.contents CSS_FLEX_DIRECTION_COLUMN) {
              childHeight.contents =
                currentAbsoluteChild.contents.style.height +
                getMarginAxis currentAbsoluteChild.contents CSS_FLEX_DIRECTION_COLUMN
            } else if (
              isPosDefined currentAbsoluteChild.contents CSS_TOP &&
              isPosDefined currentAbsoluteChild.contents CSS_BOTTOM
            ) {
              childHeight.contents =
                node.layout.measuredHeight - (
                  getLeadingBorder node CSS_FLEX_DIRECTION_COLUMN +
                  getTrailingBorder node CSS_FLEX_DIRECTION_COLUMN
                ) - (
                  currentAbsoluteChild.contents.style.top + currentAbsoluteChild.contents.style.bottom
                );
              childHeight.contents =
                boundAxis currentAbsoluteChild.contents CSS_FLEX_DIRECTION_COLUMN childHeight.contents
            };
            if (isUndefined childWidth.contents || isUndefined childHeight.contents) {
              childWidthMeasureMode.contents =
                isUndefined childWidth.contents ? CSS_MEASURE_MODE_UNDEFINED : CSS_MEASURE_MODE_EXACTLY;
              childHeightMeasureMode.contents =
                isUndefined childHeight.contents ? CSS_MEASURE_MODE_UNDEFINED : CSS_MEASURE_MODE_EXACTLY;
              if ((!isMainAxisRow && isUndefined childWidth.contents) && !(isUndefined availableInnerWidth)) {
                childWidth.contents = availableInnerWidth;
                childWidthMeasureMode.contents = CSS_MEASURE_MODE_AT_MOST
              };
              if (node.style.overflow === CSS_OVERFLOW_HIDDEN) {
                if (
                  (isMainAxisRow && isUndefined childHeight.contents) && !(isUndefined availableInnerHeight)
                ) {
                  childHeight.contents = availableInnerHeight;
                  childHeightMeasureMode.contents = CSS_MEASURE_MODE_AT_MOST
                }
              };
              let _ =
                layoutNodeInternal
                  currentAbsoluteChild.contents
                  childWidth.contents
                  childHeight.contents
                  direction
                  childWidthMeasureMode.contents
                  childHeightMeasureMode.contents
                  false
                  "abs-measure";
              childWidth.contents =
                currentAbsoluteChild.contents.layout.measuredWidth +
                getMarginAxis currentAbsoluteChild.contents CSS_FLEX_DIRECTION_ROW;
              childHeight.contents =
                currentAbsoluteChild.contents.layout.measuredHeight +
                getMarginAxis currentAbsoluteChild.contents CSS_FLEX_DIRECTION_COLUMN
            };
            let _ =
              layoutNodeInternal
                currentAbsoluteChild.contents
                childWidth.contents
                childHeight.contents
                direction
                CSS_MEASURE_MODE_EXACTLY
                CSS_MEASURE_MODE_EXACTLY
                true
                "abs-layout";
            if (
              !(
                isUndefined (
                  styleTrailingPositionForAxis currentAbsoluteChild.contents CSS_FLEX_DIRECTION_ROW
                )
              ) &&
              !
                !(
                  isUndefined (
                    styleLeadingPositionForAxis currentAbsoluteChild.contents CSS_FLEX_DIRECTION_ROW
                  )
                )
            ) {
              setLayoutLeadingPositionForAxis
                currentAbsoluteChild.contents
                CSS_FLEX_DIRECTION_ROW
                (
                  layoutMeasuredDimensionForAxis node CSS_FLEX_DIRECTION_ROW -
                  layoutMeasuredDimensionForAxis currentAbsoluteChild.contents CSS_FLEX_DIRECTION_ROW -
                  styleTrailingPositionForAxisOrZero currentAbsoluteChild.contents CSS_FLEX_DIRECTION_ROW
                )
            };
            if (
              !(
                isUndefined (
                  styleTrailingPositionForAxis currentAbsoluteChild.contents CSS_FLEX_DIRECTION_COLUMN
                )
              ) &&
              !
                !(
                  isUndefined (
                    styleLeadingPositionForAxis currentAbsoluteChild.contents CSS_FLEX_DIRECTION_COLUMN
                  )
                )
            ) {
              setLayoutLeadingPositionForAxis
                currentAbsoluteChild.contents
                CSS_FLEX_DIRECTION_COLUMN
                (
                  layoutMeasuredDimensionForAxis node CSS_FLEX_DIRECTION_COLUMN -
                  layoutMeasuredDimensionForAxis currentAbsoluteChild.contents CSS_FLEX_DIRECTION_COLUMN -
                  styleTrailingPositionForAxisOrZero currentAbsoluteChild.contents CSS_FLEX_DIRECTION_COLUMN
                )
            }
          };
          currentAbsoluteChild.contents = currentAbsoluteChild.contents.nextChild
        }
      }
    }
  }
  /** END_GENERATED **/
};

let layoutNode (node, availableWidth, availableHeight, parentDirection) => {
  /* Increment the generation count. This will force the recursive routine to visit*/
  /* all dirty nodes at least once. Subsequent visits will be skipped if the input*/
  /* parameters don't change.*/
  gCurrentGenerationCount.contents = gCurrentGenerationCount.contents + 1;
  /* If the caller didn't specify a height/width, use the dimensions*/
  /* specified in the style.*/
  let availableWidth =
    if (isUndefined availableWidth && isStyleDimDefined node CSS_FLEX_DIRECTION_ROW) {
      node.style.width +. getMarginAxis node CSS_FLEX_DIRECTION_ROW
    } else {
      availableWidth
    };
  let availableHeight =
    if (isUndefined availableHeight && isStyleDimDefined node CSS_FLEX_DIRECTION_COLUMN) {
      node.style.height +. getMarginAxis node CSS_FLEX_DIRECTION_COLUMN
    } else {
      availableHeight
    };
  let widthMeasureMode = isUndefined availableWidth ? CSS_MEASURE_MODE_UNDEFINED : CSS_MEASURE_MODE_EXACTLY;
  let heightMeasureMode =
    isUndefined availableHeight ? CSS_MEASURE_MODE_UNDEFINED : CSS_MEASURE_MODE_EXACTLY;
  if (
    layoutNodeInternal
      node availableWidth availableHeight parentDirection widthMeasureMode heightMeasureMode true "initial"
  ) {
    setPosition node node.layout.direction;
    if gPrintTree.contents {
      LayoutPrint.printCssNode (node, {printLayout: true, printChildren: true, printStyle: true})
    }
  }
};
