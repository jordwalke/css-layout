open LayoutTypes;

let isNan value => classify_float nan === FP_nan;

let indent n =>
  for i in 0 to (n - 1) {
    print_string "  "
  };

let print_number_0 (str, number) =>
  if (not (number == 0.0)) {
    Printf.printf "%s: %g, " str number
  };

let print_number_nan (str, number) =>
  if (not (isNan number)) {
    Printf.printf "%s: %g, " str number
  };

let rec printCssNodeRec (node, options, level) => {
  indent level;
  Printf.printf "{";
  switch node.print {
  | None => ()
  | Some printer => printer node.context
  };
  if options.printLayout {
    Printf.printf "layout: {";
    Printf.printf "width: %g, " node.layout.width;
    Printf.printf "height: %g, " node.layout.height;
    Printf.printf "top: %g, " node.layout.top;
    Printf.printf "left: %g" node.layout.left;
    Printf.printf "}, "
  };
  if options.printStyle {
    if (node.style.flexDirection == CSS_FLEX_DIRECTION_COLUMN) {
      Printf.printf "flexDirection: 'column', "
    } else if (
      node.style.flexDirection == CSS_FLEX_DIRECTION_COLUMN_REVERSE
    ) {
      Printf.printf "flexDirection: 'column-reverse', "
    } else if (
      node.style.flexDirection == CSS_FLEX_DIRECTION_ROW
    ) {
      Printf.printf "flexDirection: 'row', "
    } else if (
      node.style.flexDirection == CSS_FLEX_DIRECTION_ROW_REVERSE
    ) {
      Printf.printf "flexDirection: 'row-reverse', "
    };
    if (node.style.justifyContent == CSS_JUSTIFY_CENTER) {
      Printf.printf "justifyContent: 'center', "
    } else if (
      node.style.justifyContent == CSS_JUSTIFY_FLEX_END
    ) {
      Printf.printf "justifyContent: 'flex-end', "
    } else if (
      node.style.justifyContent == CSS_JUSTIFY_SPACE_AROUND
    ) {
      Printf.printf "justifyContent: 'space-around', "
    } else if (
      node.style.justifyContent == CSS_JUSTIFY_SPACE_BETWEEN
    ) {
      Printf.printf "justifyContent: 'space-between', "
    };
    if (node.style.alignItems == CSS_ALIGN_CENTER) {
      Printf.printf "alignItems: 'center', "
    } else if (
      node.style.alignItems == CSS_ALIGN_FLEX_END
    ) {
      Printf.printf "alignItems: 'flex-end', "
    } else if (
      node.style.alignItems == CSS_ALIGN_STRETCH
    ) {
      Printf.printf "alignItems: 'stretch', "
    };
    if (node.style.alignContent == CSS_ALIGN_CENTER) {
      Printf.printf "alignContent: 'center', "
    } else if (
      node.style.alignContent == CSS_ALIGN_FLEX_END
    ) {
      Printf.printf "alignContent: 'flex-end', "
    } else if (
      node.style.alignContent == CSS_ALIGN_STRETCH
    ) {
      Printf.printf "alignContent: 'stretch', "
    };
    if (node.style.alignSelf == CSS_ALIGN_FLEX_START) {
      Printf.printf "alignSelf: 'flex-start', "
    } else if (
      node.style.alignSelf == CSS_ALIGN_CENTER
    ) {
      Printf.printf "alignSelf: 'center', "
    } else if (
      node.style.alignSelf == CSS_ALIGN_FLEX_END
    ) {
      Printf.printf "alignSelf: 'flex-end', "
    } else if (
      node.style.alignSelf == CSS_ALIGN_STRETCH
    ) {
      Printf.printf "alignSelf: 'stretch', "
    };
    print_number_nan ("flex", node.style.flex);
    if (node.style.overflow == CSS_OVERFLOW_HIDDEN) {
      Printf.printf "overflow: 'hidden', "
    } else if (
      node.style.overflow == CSS_OVERFLOW_VISIBLE
    ) {
      Printf.printf "overflow: 'visible', "
    };
    print_number_0 ("marginLeft", node.style.marginLeft);
    print_number_0 ("marginRight", node.style.marginRight);
    print_number_0 ("marginTop", node.style.marginTop);
    print_number_0 ("marginBottom", node.style.marginBottom);
    print_number_0 ("marginStart", node.style.marginStart);
    print_number_0 ("marginEnd", node.style.marginEnd);
    print_number_0 ("paddingLeft", node.style.paddingLeft);
    print_number_0 ("paddingRight", node.style.paddingRight);
    print_number_0 ("paddingTop", node.style.paddingTop);
    print_number_0 ("paddingBottom", node.style.paddingBottom);
    print_number_0 ("paddingStart", node.style.paddingStart);
    print_number_0 ("paddingEnd", node.style.paddingEnd);
    print_number_0 ("borderLeftWidth", node.style.borderLeft);
    print_number_0 ("borderRightWidth", node.style.borderRight);
    print_number_0 ("borderTopWidth", node.style.borderTop);
    print_number_0 ("borderBottomWidth", node.style.borderBottom);
    print_number_0 ("borderStartWidth", node.style.borderStart);
    print_number_0 ("borderEndWidth", node.style.borderEnd);
    print_number_nan ("width", node.style.width);
    print_number_nan ("height", node.style.height);
    print_number_nan ("maxWidth", node.style.maxWidth);
    print_number_nan ("maxHeight", node.style.maxHeight);
    print_number_nan ("minWidth", node.style.minWidth);
    print_number_nan ("minHeight", node.style.minHeight);
    if (node.style.positionType == CSS_POSITION_ABSOLUTE) {
      Printf.printf "position: 'absolute', "
    };
    print_number_nan ("left", node.style.left);
    print_number_nan ("right", node.style.right);
    print_number_nan ("top", node.style.top);
    print_number_nan ("bottom", node.style.bottom)
  };
  if (options.printChildren && node.childrenCount > 0) {
    Printf.printf "children: [\n";
    for i in 0 to (node.childrenCount - 1) {
      printCssNodeRec (node.getChild node.context i, options, level + 1)
    };
    indent level;
    Printf.printf "]},\n"
  } else {
    Printf.printf "},\n"
  }
};

let printCssNode (node, options) => printCssNodeRec (node, options, 0);
