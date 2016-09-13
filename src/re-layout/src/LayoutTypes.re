type printOptions = {printLayout: bool, printStyle: bool, printChildren: bool};

type direction =
  | CSS_DIRECTION_NEGATIVE_ONE_WHATEVER_THAT_MEANS /* 'inherit' */
  | CSS_DIRECTION_INHERIT /* 'inherit' */
  | CSS_DIRECTION_LTR /* 'ltr'     */
  | CSS_DIRECTION_RTL /* 'rtl'     */;

type flexDirection =
  | CSS_FLEX_DIRECTION_ROW /* 'row' */
  | CSS_FLEX_DIRECTION_ROW_REVERSE
  /* 'row-reverse' */
  | CSS_FLEX_DIRECTION_COLUMN /* 'column' */
  | CSS_FLEX_DIRECTION_COLUMN_REVERSE;

/* 'column-reverse' */
type justify =
  | CSS_JUSTIFY_FLEX_START /* 'flex-start' */
  | CSS_JUSTIFY_CENTER /* 'center' */
  | CSS_JUSTIFY_FLEX_END /* 'flex-end' */
  | CSS_JUSTIFY_SPACE_BETWEEN /* 'space-between' */
  | CSS_JUSTIFY_SPACE_AROUND /* space-around' */;

type align =
  | CSS_ALIGN_AUTO
  | CSS_ALIGN_FLEX_START /* 'flex-start' */
  | CSS_ALIGN_CENTER /* 'center' */
  | CSS_ALIGN_FLEX_END /* 'flex-end' */
  | CSS_ALIGN_STRETCH /* 'stretch' */;

type positionType =
  | CSS_POSITION_RELATIVE /* 'relative' */
  | CSS_POSITION_ABSOLUTE /* 'absolute' */;

type measureMode =
  /* there are some places in the reference implementation that set
   * the measure mode to negative one.*/
  | CSS_MEASURE_MODE_NEGATIVE_ONE_WHATEVER_THAT_MEANS
  | CSS_MEASURE_MODE_UNDEFINED /* 'undefined' */
  | CSS_MEASURE_MODE_EXACTLY /* 'exactly' */
  | CSS_MEASURE_MODE_AT_MOST /* 'at-most' */;

type position = | CSS_LEFT | CSS_RIGHT | CSS_TOP | CSS_BOTTOM;

let css_max_cached_result_count = 6;


/**
 * Intentionally, nothing is mutable inside each
 */
type cachedMeasurement = {
  mutable availableWidth: float,
  mutable availableHeight: float,
  mutable widthMeasureMode: measureMode,
  mutable heightMeasureMode: measureMode,
  mutable computedWidth: float,
  mutable computedHeight: float
};

type overflow = | CSS_OVERFLOW_VISIBLE | CSS_OVERFLOW_HIDDEN;

type wrapType = | CSS_NOWRAP | CSS_WRAP;

type dimensions = {width: float, height: float};

type specificDirection = | Left | Right | Top | Bottom;

type coordinates = {left: float, top: float};

type cssStyle = {
  direction: direction,
  flexDirection: flexDirection,
  justifyContent: justify,
  alignContent: align,
  alignItems: align,
  alignSelf: align,
  positionType: positionType,
  flexWrap: wrapType,
  overflow: overflow,
  flex: float,
  marginLeft: float,
  marginTop: float,
  marginRight: float,
  marginBottom: float,
  marginStart: float,
  marginEnd: float,
  width: float,
  height: float,
  minWidth: float,
  minHeight: float,
  maxWidth: float,
  maxHeight: float,
  left: float,
  top: float,
  right: float,
  bottom: float,
  /**
   * You should skip all the rules that contain negative values for the
   * following attributes. For example:
   *   {padding: 10, paddingLeft: -5}
   * should output:
   *   {left: 10 ...}
   * the following two are incorrect:
   *   {left: -5 ...}
   *   {left: 0 ...}
   */
  paddingLeft: float,
  paddingTop: float,
  paddingRight: float,
  paddingBottom: float,
  paddingStart: float,
  paddingEnd: float,
  borderLeft: float,
  borderTop: float,
  borderRight: float,
  borderBottom: float,
  borderStart: float,
  borderEnd: float
};


/**
 * Analog to "computed style" - the position takes into account all of the CSS
 * styles and inheritance.
 */
type cssLayout = {
  mutable left: float,
  mutable top: float,
  mutable right: float,
  mutable bottom: float,
  mutable width: float,
  mutable height: float,
  mutable direction: direction,
  /* Instead of recomputing the entire layout every single time, we
   * cache some information to break early when nothing changed */
  mutable shouldUpdate: bool,
  mutable generationCount: int,
  mutable lastRequestedWidth: float,
  mutable lastRequestedHeight: float,
  mutable lastParentMaxWidth: float,
  mutable lastParentMaxHeight: float,
  mutable lastParentDirection: direction,
  mutable lastWidth: float,
  mutable lastHeight: float,
  mutable lastTop: float,
  mutable lastLeft: float,
  mutable lastDirection: direction,
  mutable flexBasis: float,
  mutable nextCachedMeasurementsIndex: int,
  /**
   * Hardcoded to 6 previous measurements.
   */
  mutable cachedMeasurement1: cachedMeasurement,
  mutable cachedMeasurement2: cachedMeasurement,
  mutable cachedMeasurement3: cachedMeasurement,
  mutable cachedMeasurement4: cachedMeasurement,
  mutable cachedMeasurement5: cachedMeasurement,
  mutable cachedMeasurement6: cachedMeasurement,
  mutable measuredWidth: float,
  mutable measuredHeight: float,
  mutable cachedLayout: cachedMeasurement
};

type node 'context = {
  style: cssStyle,
  layout: cssLayout,
  childrenCount: int,
  mutable lineIndex: int,
  mutable nextChild: (node 'context),
  measure: ('context, float, measureMode, float, measureMode) => dimensions,
  print: option ('context => unit),
  getChild: ('context, int) => node 'context,
  isDirty: 'context => bool,
  context: 'context
};

/* static css_position_t leading[4] = { */
/*   /* CSS_FLEX_DIRECTION_COLUMN = */ CSS_TOP, */
/*   /* CSS_FLEX_DIRECTION_COLUMN_REVERSE = */ CSS_BOTTOM, */
/*   /* CSS_FLEX_DIRECTION_ROW = */ CSS_LEFT, */
/*   /* CSS_FLEX_DIRECTION_ROW_REVERSE = */ CSS_RIGHT */
/* }; */
/* static css_position_t trailing[4] = { */
/*   /* CSS_FLEX_DIRECTION_COLUMN = */ CSS_BOTTOM, */
/*   /* CSS_FLEX_DIRECTION_COLUMN_REVERSE = */ CSS_TOP, */
/*   /* CSS_FLEX_DIRECTION_ROW = */ CSS_RIGHT, */
/*   /* CSS_FLEX_DIRECTION_ROW_REVERSE = */ CSS_LEFT */
/* }; */
/* static css_position_t pos[4] = { */
/*   /* CSS_FLEX_DIRECTION_COLUMN = */ CSS_TOP, */
/*   /* CSS_FLEX_DIRECTION_COLUMN_REVERSE = */ CSS_BOTTOM, */
/*   /* CSS_FLEX_DIRECTION_ROW = */ CSS_LEFT, */
/*   /* CSS_FLEX_DIRECTION_ROW_REVERSE = */ CSS_RIGHT */
/* }; */
/* static css_dimension_t dim[4] = { */
/*   /* CSS_FLEX_DIRECTION_COLUMN = */ CSS_HEIGHT, */
/*   /* CSS_FLEX_DIRECTION_COLUMN_REVERSE = */ CSS_HEIGHT, */
/*   /* CSS_FLEX_DIRECTION_ROW = */ CSS_WIDTH, */
/*   /* CSS_FLEX_DIRECTION_ROW_REVERSE = */ CSS_WIDTH */
/* }; */
