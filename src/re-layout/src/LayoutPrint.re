open LayoutTypes;

let isNan value => classify_float value === FP_nan;

let shouldFilter = true;

let indent n =>
  for i in 0 to (n - 1) {
    print_string "  "
  };

let print_number_0 (indentNum, str, number) =>
  if (not shouldFilter || not (number == 0.0)) {
    indent indentNum;
    Printf.printf "%s: %g,\n" str number
  };

let print_number_nan (indentNum, str, number) =>
  if (not shouldFilter || not (isNan number)) {
    indent indentNum;
    Printf.printf "%s: %g,\n" str number
  };

let layoutStr layout =>
  "{left:" ^
  string_of_float layout.left ^
  ", top:" ^
  string_of_float layout.top ^
  ", width:" ^ string_of_float layout.width ^ ", height:" ^ string_of_float layout.height ^ "}";

let rec printCssNodeRec (node, options, level) => {
  indent level;
  Printf.printf "{\n";
  switch node.print {
  | None => ()
  | Some printer => printer node.context
  };
  if options.printLayout {
    indent (level + 1);
    Printf.printf "layout: {\n";
    print_number_nan (level + 2, "width", node.layout.width);
    print_number_nan (level + 2, "height", node.layout.height);
    print_number_nan (level + 2, "measuredWidth", node.layout.measuredWidth);
    print_number_nan (level + 2, "measuredHeight", node.layout.measuredHeight);
    print_number_0 (level + 2, "top", node.layout.top);
    print_number_0 (level + 2, "left", node.layout.left);
    /* indent (level + 2); */
    /* Printf.printf "shouldUpdate: %b,\n" node.layout.shouldUpdate; */
    print_number_0 (level + 2, "generationCount", float_of_int node.layout.generationCount);
    indent (level + 1);
    Printf.printf "},\n"
  };
  if options.printStyle {
    indent (level + 1);
    Printf.printf "style: {\n";
    if (node.style.flexDirection == CSS_FLEX_DIRECTION_COLUMN) {
      indent (level + 2);
      Printf.printf "flexDirection: 'column',\n"
    } else if (
      node.style.flexDirection == CSS_FLEX_DIRECTION_COLUMN_REVERSE
    ) {
      indent (level + 2);
      Printf.printf "flexDirection: 'column-reverse',\n"
    } else if (
      node.style.flexDirection == CSS_FLEX_DIRECTION_ROW
    ) {
      indent (level + 2);
      Printf.printf "flexDirection: 'row',\n"
    } else if (
      node.style.flexDirection == CSS_FLEX_DIRECTION_ROW_REVERSE
    ) {
      indent (level + 2);
      Printf.printf "flexDirection: 'row-reverse',\n"
    };
    if (node.style.justifyContent == CSS_JUSTIFY_CENTER) {
      indent (level + 2);
      Printf.printf "justifyContent: 'center',\n"
    } else if (
      node.style.justifyContent == CSS_JUSTIFY_FLEX_END
    ) {
      indent (level + 2);
      Printf.printf "justifyContent: 'flex-end',\n"
    } else if (
      node.style.justifyContent == CSS_JUSTIFY_SPACE_AROUND
    ) {
      indent (level + 2);
      Printf.printf "justifyContent: 'space-around',\n"
    } else if (
      node.style.justifyContent == CSS_JUSTIFY_SPACE_BETWEEN
    ) {
      indent (level + 2);
      Printf.printf "justifyContent: 'space-between',\n"
    };
    if (node.style.alignItems == CSS_ALIGN_CENTER) {
      indent (level + 2);
      Printf.printf "alignItems: 'center',\n"
    } else if (
      node.style.alignItems == CSS_ALIGN_FLEX_END
    ) {
      indent (level + 2);
      Printf.printf "alignItems: 'flex-end',\n"
    } else if (
      node.style.alignItems == CSS_ALIGN_STRETCH
    ) {
      indent (level + 2);
      Printf.printf "alignItems: 'stretch',\n"
    };
    if (node.style.alignContent == CSS_ALIGN_CENTER) {
      indent (level + 2);
      Printf.printf "alignContent: 'center',\n"
    } else if (
      node.style.alignContent == CSS_ALIGN_FLEX_END
    ) {
      indent (level + 2);
      Printf.printf "alignContent: 'flex-end',\n"
    } else if (
      node.style.alignContent == CSS_ALIGN_STRETCH
    ) {
      indent (level + 2);
      Printf.printf "alignContent: 'stretch',\n"
    };
    if (node.style.alignSelf == CSS_ALIGN_FLEX_START) {
      indent (level + 2);
      Printf.printf "alignSelf: 'flex-start',\n"
    } else if (
      node.style.alignSelf == CSS_ALIGN_CENTER
    ) {
      indent (level + 2);
      Printf.printf "alignSelf: 'center',\n"
    } else if (
      node.style.alignSelf == CSS_ALIGN_FLEX_END
    ) {
      indent (level + 2);
      Printf.printf "alignSelf: 'flex-end',\n"
    } else if (
      node.style.alignSelf == CSS_ALIGN_STRETCH
    ) {
      indent (level + 2);
      Printf.printf "alignSelf: 'stretch',\n"
    };
    print_number_nan (level + 2, "flex", LayoutSupport.cssNodeStyleGetFlex node);
    if (node.style.overflow == CSS_OVERFLOW_HIDDEN) {
      indent (level + 2);
      Printf.printf "overflow: 'hidden',\n"
    } else if (
      node.style.overflow == CSS_OVERFLOW_VISIBLE
    ) {
      indent (level + 2);
      Printf.printf "overflow: 'visible',\n"
    };
    print_number_0 (level + 2, "marginLeft", node.style.marginLeft);
    print_number_0 (level + 2, "marginRight", node.style.marginRight);
    print_number_0 (level + 2, "marginTop", node.style.marginTop);
    print_number_0 (level + 2, "marginBottom", node.style.marginBottom);
    print_number_0 (level + 2, "paddingLeft", node.style.paddingLeft);
    print_number_0 (level + 2, "paddingRight", node.style.paddingRight);
    print_number_0 (level + 2, "paddingTop", node.style.paddingTop);
    print_number_0 (level + 2, "paddingBottom", node.style.paddingBottom);
    print_number_0 (level + 2, "borderLeftWidth", node.style.borderLeft);
    print_number_0 (level + 2, "borderRightWidth", node.style.borderRight);
    print_number_0 (level + 2, "borderTopWidth", node.style.borderTop);
    print_number_0 (level + 2, "borderBottomWidth", node.style.borderBottom);
    print_number_nan (level + 2, "borderStartWidth", node.style.borderStart);
    print_number_nan (level + 2, "borderEndWidth", node.style.borderEnd);
    print_number_nan (level + 2, "paddingStart", node.style.paddingStart);
    print_number_nan (level + 2, "paddingEnd", node.style.paddingEnd);
    print_number_nan (level + 2, "marginStart", node.style.marginStart);
    print_number_nan (level + 2, "marginEnd", node.style.marginEnd);
    print_number_nan (level + 2, "width", node.style.width);
    print_number_nan (level + 2, "height", node.style.height);
    print_number_nan (level + 2, "maxWidth", node.style.maxWidth);
    print_number_nan (level + 2, "maxHeight", node.style.maxHeight);
    print_number_nan (level + 2, "minWidth", node.style.minWidth);
    print_number_nan (level + 2, "minHeight", node.style.minHeight);
    if (node.style.positionType == CSS_POSITION_ABSOLUTE) {
      indent (level + 2);
      Printf.printf "position: 'absolute', "
    };
    print_number_nan (level + 2, "left", node.style.left);
    print_number_nan (level + 2, "right", node.style.right);
    print_number_nan (level + 2, "top", node.style.top);
    print_number_nan (level + 2, "bottom", node.style.bottom);
    indent (level + 1);
    Printf.printf "},\n"
  };
  if (options.printChildren && Array.length node.children > 0) {
    indent (level + 1);
    Printf.printf "children: [\n";
    for i in 0 to (Array.length node.children - 1) {
      printCssNodeRec (node.children.(i), options, level + 2)
    };
    indent (level + 1);
    Printf.printf "]},\n"
  } else {
    indent level;
    Printf.printf "},\n"
  }
};

let printCssNode (node, options) => printCssNodeRec (node, options, 0);
