let failures = {contents: []};

external reraise : exn => _ = "%reraise";

let testCount = {contents: 0};

let it desc test => {
  testCount.contents = testCount.contents + 1;
  try (test ()) {
  | e => failures.contents = [(desc, e), ...failures.contents]
  }
};

let displayOutcomes () => {
  let forEach (desc: string, exc) => {
    print_string ("[Exception] " ^ desc);
    print_newline ()
  };
  failures.contents |> List.iter forEach;
  if (List.length failures.contents > 0) {
    let (tlDesc, tlExc) = List.hd (List.rev failures.contents);
    reraise tlExc
  }
};

it
  "should fill root node with layout, style, and children"
  {
    /* let measureSquare () innerWidth widthMeasureMode innerHeight heightMeasureMode => { */
    /*   LayoutTypes.width: 100.0, */
    /*   height: 100.0 */
    /* }; */
    fun () => {
      /*
       * From css-layout comments:
       * The spec describes four different layout modes: "fill available", "max
       * content", "min content", and "fit content". Of these, we don't use
       * "min content" because we don't support default minimum main sizes (see
       * above for details). Each of our measure modes maps to a layout mode
       * from the spec (https://www.w3.org/TR/css3-sizing/#terms):
       *
       *   - CSS_MEASURE_MODE_UNDEFINED: `max-content`
       *   - CSS_MEASURE_MODE_EXACTLY: `fill-available`
       *   - CSS_MEASURE_MODE_AT_MOST: `fit-content`
       *      If infinite space available in that axis, then `max-content.`
       *      Else, `min(max-content size, max(min-content size, fill-available size))`
       *      (Although, we don't support min-content)
       */
      let emptyNode0 = LayoutSupport.createEmptyNode ();
      let emptyNode0Style = {...emptyNode0.style, flex: 1.0};
      let child0 = {
        ...emptyNode0,
        style: emptyNode0Style,
        LayoutTypes.childrenCount: 0,
        measure: LayoutSupport.dummyMeasure,
        isDirty: fun () => true
      };
      let emptyNode1 = LayoutSupport.createEmptyNode ();
      let emptyNode1Style = {...emptyNode1.style, flex: 1.0};
      let child1 = {
        ...emptyNode1,
        style: emptyNode1Style,
        childrenCount: 0,
        measure: LayoutSupport.dummyMeasure,
        isDirty: fun () => true
      };
      let emptyNode = LayoutSupport.createEmptyNode ();
      let node = {
        ...emptyNode,
        style: {...emptyNode.style, width: 900.0, height: 900.0, flex: 1.0},
        /**
         * CSS_MEASURE_MODE_AT_MOST
         */
        childrenCount: 2,
        getChild: fun () index =>
          switch index {
          | 0 => child0
          | 1 => child1
          | i => raise (Invalid_argument ("No index 2" ^ string_of_int i))
          },
        isDirty: fun () => true
      };
      Layout.gPrintTree.contents = false;
      let availableWidth = 210.0;
      let availableHeight = 210.0;
      print_string ("BEFORE:");
      print_newline ();
      print_string ("===================");
      print_newline ();
      LayoutPrint.printCssNode (node, {printLayout: true, printStyle: true, printChildren: true});
      Layout.layoutNode (node, availableWidth, availableHeight, LayoutTypes.CSS_DIRECTION_LTR);
      print_string ("AFTER:");
      print_newline ();
      print_string ("===================");
      print_newline ();
      LayoutPrint.printCssNode (node, {printLayout: true, printStyle: true, printChildren: true});
    }
  };

displayOutcomes ();
