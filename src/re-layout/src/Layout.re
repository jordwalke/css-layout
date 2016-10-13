/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
/*
 * From css-layout comments:
 * The spec describes four different layout modes: "fill available", "max
 * content", "min content", and "fit content". Of these, we don't use
 * "min content" because we don't support default minimum main sizes (see
 * above for details). Each of our measure modes maps to a layout mode
 * from the spec (https://www.w3.org/TR/css3-sizing/#terms):
 *
 *   - CssMeasureModeUndefined: `max-content`
 *   - CssMeasureModeExactly: `fill-available`
 *   - CssMeasureModeAtMost: `fit-content`
 *      If infinite space available in that axis, then `max-content.`
 *      Else, `min(max-content size, max(min-content size, fill-available size))`
 *      (Although, we don't support min-content)
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
      "CSS_MEASURE_MODE_NEGATIVE_ONE_WHATEVER_THAT_MEANS" :
      "CSS_MEASURE_MODE_NEGATIVE_ONE_WHATEVER_THAT_MEANS"
  | CssMeasureModeUndefined => isLayoutInsteadOfMeasure ? "LAY_UNDEFINED" : "UNDEFINED"
  | CssMeasureModeExactly => isLayoutInsteadOfMeasure ? "LAY_EXACTLY" : "EXACTLY"
  | CssMeasureModeAtMost => isLayoutInsteadOfMeasure ? "LAY_AT_MOST" : "AT_MOST"
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
      heightMeasureMode === CssMeasureModeExactly &&
      availableHeight -. marginColumn == cachedLayout.computedHeight
    ) {
    true
  } else if
    /* If the height is an exact match, try a fuzzy match on the width.*/
    (
      cachedLayout.heightMeasureMode == heightMeasureMode &&
      cachedLayout.availableHeight == availableHeight &&
      widthMeasureMode === CssMeasureModeExactly && availableWidth -. marginRow == cachedLayout.computedWidth
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
  if (node.measure !== dummyMeasure) {
    let marginAxisRow = getMarginAxis node CssFlexDirectionRow;
    let marginAxisColumn = getMarginAxis node CssFlexDirectionColumn;
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
    layout.hasNewLayout = true
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
    (isUndefined availableWidth ? widthMeasureMode === CssMeasureModeUndefined : true)
    "availableWidth is indefinite so widthMeasureMode must be CssMeasureModeUndefined";
  re_assert
    (isUndefined availableHeight ? heightMeasureMode === CssMeasureModeUndefined : true)
    "availableHeight is indefinite so heightMeasureMode must be CssMeasureModeUndefined";
  let paddingAndBorderAxisRow = getPaddingAndBorderAxis node CssFlexDirectionRow;
  let paddingAndBorderAxisColumn = getPaddingAndBorderAxis node CssFlexDirectionColumn;
  let marginAxisRow = getMarginAxis node CssFlexDirectionRow;
  let marginAxisColumn = getMarginAxis node CssFlexDirectionColumn;
  let direction = resolveDirection node parentDirection;
  node.layout.direction = direction;
  if (node.measure !== dummyMeasure) {
    let innerWidth = availableWidth - marginAxisRow - paddingAndBorderAxisRow;
    let innerHeight = availableHeight - marginAxisColumn - paddingAndBorderAxisColumn;
    if (widthMeasureMode === CssMeasureModeExactly && heightMeasureMode === CssMeasureModeExactly) {
      node.layout.measuredWidth = boundAxis node CssFlexDirectionRow (availableWidth - marginAxisRow);
      node.layout.measuredHeight = boundAxis node CssFlexDirectionColumn (availableHeight - marginAxisColumn)
    } else if (
      innerWidth <= 0.0 || innerHeight <= 0.0
    ) {
      node.layout.measuredWidth = boundAxis node CssFlexDirectionRow 0.0;
      node.layout.measuredHeight = boundAxis node CssFlexDirectionColumn 0.0
    } else {
      let measureDim = node.measure node.context innerWidth widthMeasureMode innerHeight heightMeasureMode;
      node.layout.measuredWidth =
        boundAxis
          node
          CssFlexDirectionRow
          (
            widthMeasureMode === CssMeasureModeUndefined || widthMeasureMode === CssMeasureModeAtMost ?
              measureDim.width + paddingAndBorderAxisRow : availableWidth - marginAxisRow
          );
      node.layout.measuredHeight =
        boundAxis
          node
          CssFlexDirectionColumn
          (
            heightMeasureMode === CssMeasureModeUndefined || heightMeasureMode === CssMeasureModeAtMost ?
              measureDim.height + paddingAndBorderAxisColumn : availableHeight - marginAxisColumn
          )
    }
  } else {
    let childCount = Array.length node.children;
    if (childCount === 0) {
      node.layout.measuredWidth =
        boundAxis
          node
          CssFlexDirectionRow
          (
            widthMeasureMode === CssMeasureModeUndefined || widthMeasureMode === CssMeasureModeAtMost ?
              paddingAndBorderAxisRow : availableWidth - marginAxisRow
          );
      node.layout.measuredHeight =
        boundAxis
          node
          CssFlexDirectionColumn
          (
            heightMeasureMode === CssMeasureModeUndefined || heightMeasureMode === CssMeasureModeAtMost ?
              paddingAndBorderAxisColumn : availableHeight - marginAxisColumn
          )
    } else {
      let shouldContinue = {contents: true};
      if !performLayout {
        if (
          (
            (widthMeasureMode === CssMeasureModeAtMost && availableWidth <= 0.0) &&
            heightMeasureMode === CssMeasureModeAtMost
          ) &&
          availableHeight <= 0.0
        ) {
          node.layout.measuredWidth = boundAxis node CssFlexDirectionRow 0.0;
          node.layout.measuredHeight = boundAxis node CssFlexDirectionColumn 0.0;
          shouldContinue.contents = false
        } else if (
          widthMeasureMode === CssMeasureModeAtMost && availableWidth <= 0.0
        ) {
          node.layout.measuredWidth = boundAxis node CssFlexDirectionRow 0.0;
          node.layout.measuredHeight =
            boundAxis
              node
              CssFlexDirectionColumn
              (isUndefined availableHeight ? 0.0 : availableHeight - marginAxisColumn);
          shouldContinue.contents = false
        } else if (
          heightMeasureMode === CssMeasureModeAtMost && availableHeight <= 0.0
        ) {
          node.layout.measuredWidth =
            boundAxis
              node CssFlexDirectionRow (isUndefined availableWidth ? 0.0 : availableWidth - marginAxisRow);
          node.layout.measuredHeight = boundAxis node CssFlexDirectionColumn 0.0;
          shouldContinue.contents = false
        } else if (
          widthMeasureMode === CssMeasureModeExactly && heightMeasureMode === CssMeasureModeExactly
        ) {
          node.layout.measuredWidth = boundAxis node CssFlexDirectionRow (availableWidth - marginAxisRow);
          node.layout.measuredHeight =
            boundAxis node CssFlexDirectionColumn (availableHeight - marginAxisColumn);
          shouldContinue.contents = false
        }
      };
      if shouldContinue.contents {
        let mainAxis = resolveAxis node.style.flexDirection direction;
        let crossAxis = getCrossFlexDirection mainAxis direction;
        let isMainAxisRow = isRowDirection mainAxis;
        let justifyContent = node.style.justifyContent;
        let isNodeFlexWrap = node.style.flexWrap === CssWrap;
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
        let childWidthMeasureMode = {contents: CssMeasureModeUndefined};
        let childHeightMeasureMode = {contents: CssMeasureModeUndefined};
        for i in 0 to (childCount -. 1) {
          child.contents = node.children.(i);
          if performLayout {
            let childDirection = resolveDirection child.contents direction;
            setPosition child.contents childDirection
          };
          if (child.contents.style.positionType === CssPositionAbsolute) {
            if (firstAbsoluteChild.contents === theNullNode) {
              firstAbsoluteChild.contents = child.contents
            };
            if (currentAbsoluteChild.contents !== theNullNode) {
              currentAbsoluteChild.contents.nextChild = child.contents
            };
            currentAbsoluteChild.contents = child.contents;
            child.contents.nextChild = theNullNode
          } else if (
            isMainAxisRow && isStyleDimDefined child.contents CssFlexDirectionRow
          ) {
            child.contents.layout.computedFlexBasis =
              fmaxf child.contents.style.width (getPaddingAndBorderAxis child.contents CssFlexDirectionRow)
          } else if (
            !isMainAxisRow && isStyleDimDefined child.contents CssFlexDirectionColumn
          ) {
            child.contents.layout.computedFlexBasis =
              fmaxf
                child.contents.style.height (getPaddingAndBorderAxis child.contents CssFlexDirectionColumn)
          } else if (
            !(isUndefined child.contents.style.flexBasis) && !(isUndefined availableInnerMainDim)
          ) {
            if (isUndefined child.contents.layout.computedFlexBasis) {
              child.contents.layout.computedFlexBasis =
                fmaxf child.contents.style.flexBasis (getPaddingAndBorderAxis child.contents mainAxis)
            }
          } else {
            childWidth.contents = cssUndefined;
            childHeight.contents = cssUndefined;
            childWidthMeasureMode.contents = CssMeasureModeUndefined;
            childHeightMeasureMode.contents = CssMeasureModeUndefined;
            if (isStyleDimDefined child.contents CssFlexDirectionRow) {
              childWidth.contents =
                child.contents.style.width + getMarginAxis child.contents CssFlexDirectionRow;
              childWidthMeasureMode.contents = CssMeasureModeExactly
            };

            /**
             * Why can't this just be inlined to .height !== cssUndefined.
             */
            if (isStyleDimDefined child.contents CssFlexDirectionColumn) {
              childHeight.contents =
                child.contents.style.height + getMarginAxis child.contents CssFlexDirectionColumn;
              childHeightMeasureMode.contents = CssMeasureModeExactly
            };
            if (!isMainAxisRow && node.style.overflow === Scroll || node.style.overflow !== Scroll) {
              if (isUndefined childWidth.contents && !(isUndefined availableInnerWidth)) {
                childWidth.contents = availableInnerWidth;
                childWidthMeasureMode.contents = CssMeasureModeAtMost
              }
            };
            if (isMainAxisRow && node.style.overflow === Scroll || node.style.overflow !== Scroll) {
              if (isUndefined childHeight.contents && !(isUndefined availableInnerHeight)) {
                childHeight.contents = availableInnerHeight;
                childHeightMeasureMode.contents = CssMeasureModeAtMost
              }
            };
            /*
             * If child has no defined size in the cross axis and is set to
             * stretch, set the cross axis to be measured exactly with the
             * available inner width.
             */
            if (
              !isMainAxisRow &&
              !(isUndefined availableInnerWidth) &&
              !(isStyleDimDefined child.contents CssFlexDirectionRow) &&
              widthMeasureMode === CssMeasureModeExactly &&
              getAlignItem node child.contents === CssAlignStretch
            ) {
              childWidth.contents = availableInnerWidth;
              childWidthMeasureMode.contents = CssMeasureModeExactly
            };
            if (
              isMainAxisRow &&
              !(isUndefined availableInnerHeight) &&
              !(isStyleDimDefined child.contents CssFlexDirectionColumn) &&
              heightMeasureMode === CssMeasureModeExactly &&
              getAlignItem node child.contents === CssAlignStretch
            ) {
              childHeight.contents = availableInnerHeight;
              childHeightMeasureMode.contents = CssMeasureModeExactly
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
            child.contents.layout.computedFlexBasis =
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
            child.contents = node.children.(curIndex.contents);
            child.contents.lineIndex = lineCount.contents;
            if (child.contents.style.positionType !== CssPositionAbsolute) {
              let outerFlexBasis =
                child.contents.layout.computedFlexBasis + getMarginAxis child.contents mainAxis;
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
                    totalFlexGrowFactors.contents + child.contents.style.flexGrow;
                  totalFlexShrinkScaledFactors.contents =
                    totalFlexShrinkScaledFactors.contents +
                    - child.contents.style.flexShrink * child.contents.layout.computedFlexBasis
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
          let canSkipFlex = !performLayout && measureModeCrossDim === CssMeasureModeExactly;
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
              childFlexBasis.contents = currentRelativeChild.contents.layout.computedFlexBasis;
              if (remainingFreeSpace.contents < 0.0) {
                flexShrinkScaledFactor.contents =
                  - currentRelativeChild.contents.style.flexShrink * childFlexBasis.contents;
                if (flexShrinkScaledFactor.contents != 0.0) {
                  baseMainSize.contents =
                    childFlexBasis.contents +
                    remainingFreeSpace.contents / totalFlexShrinkScaledFactors.contents *
                    flexShrinkScaledFactor.contents;
                  boundMainSize.contents =
                    boundAxis currentRelativeChild.contents mainAxis baseMainSize.contents;
                  if (baseMainSize.contents != boundMainSize.contents) {
                    deltaFreeSpace.contents =
                      deltaFreeSpace.contents - (boundMainSize.contents - childFlexBasis.contents);
                    deltaFlexShrinkScaledFactors.contents =
                      deltaFlexShrinkScaledFactors.contents - flexShrinkScaledFactor.contents
                  }
                }
              } else if (
                remainingFreeSpace.contents > 0.0
              ) {
                flexGrowFactor.contents = currentRelativeChild.contents.style.flexGrow;
                if (flexGrowFactor.contents != 0.0) {
                  baseMainSize.contents =
                    childFlexBasis.contents +
                    remainingFreeSpace.contents / totalFlexGrowFactors.contents * flexGrowFactor.contents;
                  boundMainSize.contents =
                    boundAxis currentRelativeChild.contents mainAxis baseMainSize.contents;
                  if (baseMainSize.contents != boundMainSize.contents) {
                    deltaFreeSpace.contents =
                      deltaFreeSpace.contents - (boundMainSize.contents - childFlexBasis.contents);
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
              childFlexBasis.contents = currentRelativeChild.contents.layout.computedFlexBasis;
              let updatedMainSize = {contents: childFlexBasis.contents};
              if (remainingFreeSpace.contents < 0.0) {
                flexShrinkScaledFactor.contents =
                  - currentRelativeChild.contents.style.flexShrink * childFlexBasis.contents;
                if (flexShrinkScaledFactor.contents != 0.0) {
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
                flexGrowFactor.contents = currentRelativeChild.contents.style.flexGrow;
                if (flexGrowFactor.contents != 0.0) {
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
                deltaFreeSpace.contents - (updatedMainSize.contents - childFlexBasis.contents);
              if isMainAxisRow {
                childWidth.contents =
                  updatedMainSize.contents + getMarginAxis currentRelativeChild.contents CssFlexDirectionRow;
                childWidthMeasureMode.contents = CssMeasureModeExactly;
                if (
                  !(isUndefined availableInnerCrossDim) &&
                  !(isStyleDimDefined currentRelativeChild.contents CssFlexDirectionColumn) &&
                  heightMeasureMode === CssMeasureModeExactly &&
                  getAlignItem node currentRelativeChild.contents === CssAlignStretch
                ) {
                  childHeight.contents = availableInnerCrossDim;
                  childHeightMeasureMode.contents = CssMeasureModeExactly
                } else if
                  !(isStyleDimDefined currentRelativeChild.contents CssFlexDirectionColumn) {
                  childHeight.contents = availableInnerCrossDim;
                  childHeightMeasureMode.contents =
                    isUndefined childHeight.contents ? CssMeasureModeUndefined : CssMeasureModeAtMost
                } else {
                  childHeight.contents =
                    currentRelativeChild.contents.style.height +
                    getMarginAxis currentRelativeChild.contents CssFlexDirectionColumn;
                  childHeightMeasureMode.contents = CssMeasureModeExactly
                }
              } else {
                childHeight.contents =
                  updatedMainSize.contents +
                  getMarginAxis currentRelativeChild.contents CssFlexDirectionColumn;
                childHeightMeasureMode.contents = CssMeasureModeExactly;
                if (
                  !(isUndefined availableInnerCrossDim) &&
                  !(isStyleDimDefined currentRelativeChild.contents CssFlexDirectionRow) &&
                  widthMeasureMode === CssMeasureModeExactly &&
                  getAlignItem node currentRelativeChild.contents === CssAlignStretch
                ) {
                  childWidth.contents = availableInnerCrossDim;
                  childWidthMeasureMode.contents = CssMeasureModeExactly
                } else if
                  !(isStyleDimDefined currentRelativeChild.contents CssFlexDirectionRow) {
                  childWidth.contents = availableInnerCrossDim;
                  childWidthMeasureMode.contents =
                    isUndefined childWidth.contents ? CssMeasureModeUndefined : CssMeasureModeAtMost
                } else {
                  childWidth.contents =
                    currentRelativeChild.contents.style.width +
                    getMarginAxis currentRelativeChild.contents CssFlexDirectionRow;
                  childWidthMeasureMode.contents = CssMeasureModeExactly
                }
              };
              let requiresStretchLayout =
                !(isStyleDimDefined currentRelativeChild.contents crossAxis) &&
                getAlignItem node currentRelativeChild.contents === CssAlignStretch;
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
          if (measureModeMainDim === CssMeasureModeAtMost) {
            remainingFreeSpace.contents = 0.0
          };
          switch justifyContent {
          | CssJustifyCenter => leadingMainDim.contents = remainingFreeSpace.contents / 2.0
          | CssJustifyFlexEnd => leadingMainDim.contents = remainingFreeSpace.contents
          | CssJustifySpaceBetween =>
            if (itemsOnLine.contents > 1) {
              betweenMainDim.contents =
                fmaxf remainingFreeSpace.contents 0.0 / float_of_int (itemsOnLine.contents -. 1)
            } else {
              betweenMainDim.contents = 0.0
            }
          | CssJustifySpaceAround =>
            betweenMainDim.contents = remainingFreeSpace.contents / float_of_int itemsOnLine.contents;
            leadingMainDim.contents = betweenMainDim.contents / 2.0
          | CssJustifyFlexStart => ()
          };
          let mainDim = {contents: leadingPaddingAndBorderMain + leadingMainDim.contents};
          let crossDim = {contents: 0.0};
          for i in startOfLineIndex.contents to (endOfLineIndex.contents -. 1) {
            child.contents = node.children.(i);
            if (
              child.contents.style.positionType === CssPositionAbsolute &&
              isLeadingPosDefinedWithFallback child.contents mainAxis
            ) {
              if performLayout {
                setLayoutLeadingPositionForAxis
                  child.contents
                  mainAxis
                  (
                    getLeadingPositionWithFallback child.contents mainAxis + getLeadingBorder node mainAxis +
                    getLeadingMargin child.contents mainAxis
                  )
              }
            } else {
              if performLayout {
                setLayoutLeadingPositionForAxis
                  child.contents
                  mainAxis
                  (layoutPosPositionForAxis child.contents mainAxis + mainDim.contents)
              };
              if (child.contents.style.positionType === CssPositionRelative) {
                if canSkipFlex {
                  mainDim.contents =
                    mainDim.contents + betweenMainDim.contents + getMarginAxis child.contents mainAxis +
                    child.contents.layout.computedFlexBasis;
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
            measureModeCrossDim === CssMeasureModeUndefined || measureModeCrossDim === CssMeasureModeAtMost
          ) {
            containerCrossAxis.contents =
              boundAxis node crossAxis (crossDim.contents + paddingAndBorderAxisCross) - paddingAndBorderAxisCross;
            if (measureModeCrossDim === CssMeasureModeAtMost) {
              containerCrossAxis.contents = fminf containerCrossAxis.contents availableInnerCrossDim
            }
          };
          if (!isNodeFlexWrap && measureModeCrossDim === CssMeasureModeExactly) {
            crossDim.contents = availableInnerCrossDim
          };
          crossDim.contents =
            boundAxis node crossAxis (crossDim.contents + paddingAndBorderAxisCross) - paddingAndBorderAxisCross;
          /*
           * STEP 7: CROSS-AXIS ALIGNMENT We can skip child alignment if we're
           * just measuring the container.
           */
          if performLayout {
            for i in startOfLineIndex.contents to (endOfLineIndex.contents -. 1) {
              child.contents = node.children.(i);
              if (child.contents.style.positionType === CssPositionAbsolute) {
                if (isLeadingPosDefinedWithFallback child.contents crossAxis) {
                  setLayoutLeadingPositionForAxis
                    child.contents
                    crossAxis
                    (
                      getLeadingPositionWithFallback child.contents crossAxis +
                      getLeadingBorder node crossAxis +
                      getLeadingMargin child.contents crossAxis
                    )
                } else {
                  setLayoutLeadingPositionForAxis
                    child.contents
                    crossAxis
                    (leadingPaddingAndBorderCross + getLeadingMargin child.contents crossAxis)
                }
              } else {
                let leadingCrossDim = {contents: leadingPaddingAndBorderCross};
                let alignItem = getAlignItem node child.contents;
                if (alignItem === CssAlignStretch) {
                  childWidth.contents =
                    child.contents.layout.measuredWidth + getMarginAxis child.contents CssFlexDirectionRow;
                  childHeight.contents =
                    child.contents.layout.measuredHeight +
                    getMarginAxis child.contents CssFlexDirectionColumn;
                  let isCrossSizeDefinite = {contents: false};
                  if isMainAxisRow {
                    isCrossSizeDefinite.contents = isStyleDimDefined child.contents CssFlexDirectionColumn;
                    childHeight.contents = crossDim.contents
                  } else {
                    isCrossSizeDefinite.contents = isStyleDimDefined child.contents CssFlexDirectionRow;
                    childWidth.contents = crossDim.contents
                  };
                  if !isCrossSizeDefinite.contents {
                    childWidthMeasureMode.contents =
                      isUndefined childWidth.contents ? CssMeasureModeUndefined : CssMeasureModeExactly;
                    childHeightMeasureMode.contents =
                      isUndefined childHeight.contents ? CssMeasureModeUndefined : CssMeasureModeExactly;
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
                  alignItem !== CssAlignFlexStart
                ) {
                  let remainingCrossDim =
                    containerCrossAxis.contents - getDimWithMargin child.contents crossAxis;
                  if (alignItem === CssAlignCenter) {
                    leadingCrossDim.contents = leadingCrossDim.contents + remainingCrossDim / 2.0
                  } else {
                    leadingCrossDim.contents = leadingCrossDim.contents + remainingCrossDim
                  }
                };
                setLayoutLeadingPositionForAxis
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
          startOfLineIndex.contents = endOfLineIndex.contents
        };
        if (lineCount.contents > 1 && performLayout && !(isUndefined availableInnerCrossDim)) {
          let remainingAlignContentDim = availableInnerCrossDim - totalLineCrossDim.contents;
          let crossDimLead = {contents: 0.0};
          let currentLead = {contents: leadingPaddingAndBorderCross};
          let alignContent = node.style.alignContent;
          if (alignContent === CssAlignFlexEnd) {
            currentLead.contents = currentLead.contents + remainingAlignContentDim
          } else if (
            alignContent === CssAlignCenter
          ) {
            currentLead.contents = currentLead.contents + remainingAlignContentDim / 2.0
          } else if (
            alignContent === CssAlignStretch
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
              child.contents = node.children.(j.contents);
              if (child.contents.style.positionType === CssPositionRelative) {
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
                child.contents = node.children.(j);
                if (child.contents.style.positionType === CssPositionRelative) {
                  switch (getAlignItem node child.contents) {
                  | CssAlignFlexStart =>
                    setLayoutLeadingPositionForAxis
                      child.contents
                      crossAxis
                      (currentLead.contents + getLeadingMargin child.contents crossAxis)
                  | CssAlignFlexEnd =>
                    setLayoutLeadingPositionForAxis
                      child.contents
                      crossAxis
                      (
                        currentLead.contents + lineHeight.contents -
                        getTrailingMargin child.contents crossAxis -
                        layoutMeasuredDimensionForAxis child.contents crossAxis
                      )
                  | CssAlignCenter =>
                    childHeight.contents = layoutMeasuredDimensionForAxis child.contents crossAxis;
                    setLayoutLeadingPositionForAxis
                      child.contents
                      crossAxis
                      (currentLead.contents + (lineHeight.contents - childHeight.contents) / 2.0)
                  | CssAlignStretch =>
                    setLayoutLeadingPositionForAxis
                      child.contents
                      crossAxis
                      (currentLead.contents + getLeadingMargin child.contents crossAxis)
                  | CssAlignAuto => raise (Invalid_argument "getAlignItem should never return auto")
                  }
                }
              }
            };
            currentLead.contents = currentLead.contents + lineHeight.contents
          }
        };
        /* STEP 9: COMPUTING FINAL DIMENSIONS */
        node.layout.measuredWidth = boundAxis node CssFlexDirectionRow (availableWidth - marginAxisRow);
        node.layout.measuredHeight =
          boundAxis node CssFlexDirectionColumn (availableHeight - marginAxisColumn);
        /* If the user didn't specify a width or height for the node, set the
         * dimensions based on the children. */
        if (measureModeMainDim === CssMeasureModeUndefined) {
          setLayoutMeasuredDimensionForAxis node mainAxis (boundAxis node mainAxis maxLineMainDim.contents)
        } else if (
          measureModeMainDim === CssMeasureModeAtMost
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
        if (measureModeCrossDim === CssMeasureModeUndefined) {
          setLayoutMeasuredDimensionForAxis
            node
            crossAxis
            (boundAxis node crossAxis (totalLineCrossDim.contents + paddingAndBorderAxisCross))
        } else if (
          measureModeCrossDim === CssMeasureModeAtMost
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
        currentAbsoluteChild.contents = firstAbsoluteChild.contents;
        while (currentAbsoluteChild.contents !== theNullNode) {
          if performLayout {
            childWidth.contents = cssUndefined;
            childHeight.contents = cssUndefined;
            if (isStyleDimDefined currentAbsoluteChild.contents CssFlexDirectionRow) {
              childWidth.contents =
                currentAbsoluteChild.contents.style.width +
                getMarginAxis currentAbsoluteChild.contents CssFlexDirectionRow
            } else if (
              isLeadingPosDefinedWithFallback currentAbsoluteChild.contents CssFlexDirectionRow &&
              isTrailingPosDefinedWithFallback currentAbsoluteChild.contents CssFlexDirectionRow
            ) {
              childWidth.contents =
                node.layout.measuredWidth - (
                  getLeadingBorder node CssFlexDirectionRow + getTrailingBorder node CssFlexDirectionRow
                ) - (
                  getLeadingPositionWithFallback currentAbsoluteChild.contents CssFlexDirectionRow +
                  getTrailingPositionWithFallback currentAbsoluteChild.contents CssFlexDirectionRow
                );
              childWidth.contents =
                boundAxis currentAbsoluteChild.contents CssFlexDirectionRow childWidth.contents
            };
            if (isStyleDimDefined currentAbsoluteChild.contents CssFlexDirectionColumn) {
              childHeight.contents =
                currentAbsoluteChild.contents.style.height +
                getMarginAxis currentAbsoluteChild.contents CssFlexDirectionColumn
            } else if (
              /* If the child doesn't have a specified height, compute the height based on the top/bottom offsets if they're defined. */
              isLeadingPosDefinedWithFallback currentAbsoluteChild.contents CssFlexDirectionColumn &&
              isTrailingPosDefinedWithFallback currentAbsoluteChild.contents CssFlexDirectionColumn
            ) {
              childHeight.contents =
                node.layout.measuredHeight - (
                  getLeadingBorder node CssFlexDirectionColumn +
                  getTrailingBorder node CssFlexDirectionColumn
                ) - (
                  getLeadingPositionWithFallback currentAbsoluteChild.contents CssFlexDirectionColumn +
                  getTrailingPositionWithFallback currentAbsoluteChild.contents CssFlexDirectionColumn
                );
              childHeight.contents =
                boundAxis currentAbsoluteChild.contents CssFlexDirectionColumn childHeight.contents
            };
            if (isUndefined childWidth.contents || isUndefined childHeight.contents) {
              childWidthMeasureMode.contents =
                isUndefined childWidth.contents ? CssMeasureModeUndefined : CssMeasureModeExactly;
              childHeightMeasureMode.contents =
                isUndefined childHeight.contents ? CssMeasureModeUndefined : CssMeasureModeExactly;
              /*
               * According to the spec, if the main size is not definite and the
               * child's inline axis is parallel to the main axis (i.e. it's
               * horizontal), the child should be sized using "UNDEFINED" in
               * the main size. Otherwise use "AT_MOST" in the cross axis.
               */
              if ((!isMainAxisRow && isUndefined childWidth.contents) && !(isUndefined availableInnerWidth)) {
                childWidth.contents = availableInnerWidth;
                childWidthMeasureMode.contents = CssMeasureModeAtMost
              };
              /*
               * If child has no defined size in the cross axis and is set to stretch, set the cross
               * axis to be measured exactly with the available inner width
               */
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
                getMarginAxis currentAbsoluteChild.contents CssFlexDirectionRow;
              childHeight.contents =
                currentAbsoluteChild.contents.layout.measuredHeight +
                getMarginAxis currentAbsoluteChild.contents CssFlexDirectionColumn
            };
            let _ =
              layoutNodeInternal
                currentAbsoluteChild.contents
                childWidth.contents
                childHeight.contents
                direction
                CssMeasureModeExactly
                CssMeasureModeExactly
                true
                "abs-layout";
            if (
              isTrailingPosDefinedWithFallback currentAbsoluteChild.contents mainAxis &&
              !(isLeadingPosDefinedWithFallback currentAbsoluteChild.contents mainAxis)
            ) {
              setLayoutLeadingPositionForAxis
                currentAbsoluteChild.contents
                mainAxis
                (
                  layoutMeasuredDimensionForAxis node mainAxis -
                  layoutMeasuredDimensionForAxis currentAbsoluteChild.contents mainAxis -
                  getTrailingPositionWithFallback currentAbsoluteChild.contents mainAxis
                )
            };
            if (
              isTrailingPosDefinedWithFallback currentAbsoluteChild.contents crossAxis &&
              !(isLeadingPosDefinedWithFallback currentAbsoluteChild.contents crossAxis)
            ) {
              setLayoutLeadingPositionForAxis
                currentAbsoluteChild.contents
                crossAxis
                (
                  layoutMeasuredDimensionForAxis node crossAxis -
                  layoutMeasuredDimensionForAxis currentAbsoluteChild.contents crossAxis -
                  getTrailingPositionWithFallback currentAbsoluteChild.contents crossAxis
                )
            }
          };
          currentAbsoluteChild.contents = currentAbsoluteChild.contents.nextChild
        };
        /* STEP 11: SETTING TRAILING POSITIONS FOR CHILDREN */
        if performLayout {
          let needsMainTrailingPos =
            mainAxis == CssFlexDirectionRowReverse || mainAxis == CssFlexDirectionColumnReverse;
          let needsCrossTrailingPos =
            crossAxis == CssFlexDirectionRowReverse || crossAxis == CssFlexDirectionColumnReverse;
          /* Set trailing position if necessary. */
          if (needsMainTrailingPos || needsCrossTrailingPos) {
            for i in 0 to (childCount -. 1) {
              let child = node.children.(i);
              if needsMainTrailingPos {
                setTrailingPosition node child mainAxis
              };
              if needsCrossTrailingPos {
                setTrailingPosition node child crossAxis
              }
            }
          }
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
  open ReJsPrelude;
  let (availableWidth, widthMeasureMode) =
    if !(isUndefined availableWidth) {
      (availableWidth, CssMeasureModeExactly)
    } else if (
      isStyleDimDefined node CssFlexDirectionRow
    ) {
      (node.style.width +. getMarginAxis node CssFlexDirectionRow, CssMeasureModeExactly)
    } else if (
      node.style.maxWidth >= 0.0
    ) {
      (node.style.maxWidth, CssMeasureModeAtMost)
    } else {
      (availableWidth, CssMeasureModeUndefined)
    };
  let (availableHeight, heightMeasureMode) =
    if !(isUndefined availableHeight) {
      (availableHeight, CssMeasureModeExactly)
    } else if (
      isStyleDimDefined node CssFlexDirectionColumn
    ) {
      (node.style.height +. getMarginAxis node CssFlexDirectionColumn, CssMeasureModeExactly)
    } else if (
      node.style.maxHeight >= 0.0
    ) {
      (node.style.maxHeight, CssMeasureModeAtMost)
    } else {
      (availableHeight, CssMeasureModeUndefined)
    };
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
