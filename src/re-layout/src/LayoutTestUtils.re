let exceptions = {contents: []};

let failures = {contents: []};

external reraise : exn => _ = "%reraise";

let testCount = {contents: 0};

let assertionCount = {contents: 0};

let currentTestName = {contents: ""};

let it desc test => {
  currentTestName.contents = desc;
  testCount.contents = testCount.contents + 1;
  try (test ()) {
  | e => exceptions.contents = [(desc, e), ...exceptions.contents]
  }
};

let displayOutcomes () => {
  let forEachException (desc: string, exc) => {
    print_string ("[Exception] " ^ desc);
    print_newline ()
  };
  let forEachFailure (desc: string, msg) => {
    print_string ("[TestFailure] " ^ desc ^ " - " ^ msg);
    print_newline ()
  };
  exceptions.contents |> List.iter forEachException;
  failures.contents |> List.iter forEachFailure;
  let exceptionsLength = List.length exceptions.contents;
  let failuresLength = List.length failures.contents;
  if (exceptionsLength === 0 && failuresLength === 0) {
    print_string ("[SUCCESS] " ^ string_of_int testCount.contents ^ " tests passed");
    print_newline ()
  } else {
    print_newline ();
    print_string "ð\159\146\128  ð\159\146\128  ð\159\146\128  ð\159\146\128";
    print_newline ();
    print_string (
      "[NON-SUCCESS] (" ^
      string_of_int exceptionsLength ^
      "/" ^
      string_of_int testCount.contents ^
      ") tests threw exceptions and (" ^
      string_of_int failuresLength ^ "/" ^ string_of_int assertionCount.contents ^ ") assertions failed."
    );
    print_newline ();
    print_string "ð\159\146\128  ð\159\146\128  ð\159\146\128  ð\159\146\128";
    print_newline ()
  };
  print_newline ();
  if (List.length exceptions.contents > 0) {
    let (tlDesc, tlExc) = List.hd (List.rev exceptions.contents);
    raise_notrace tlExc
  };
  testCount.contents = 0;
  assertionCount.contents = 0;
  exceptions.contents = [];
  failures.contents = []
};

let assertEq testNum layoutAttr a b => {
  assertionCount.contents = assertionCount.contents + 1;
  if (a != b) {
    failures.contents = [
      (
        currentTestName.contents,
        "Test " ^
        string_of_int testNum ^
        ": " ^
        layoutAttr ^ " not equal(expected=" ^ string_of_float a ^ ", observed=" ^ string_of_float b ^ ")"
      ),
      ...failures.contents
    ]
  }
};

let rec hasMismatchedLayout observedAndExpected =>
  switch observedAndExpected {
  | [] => false
  | [(expected, observed), ...tl] =>
    expected.LayoutTypes.top != observed.LayoutTypes.top ||
    expected.left != observed.left ||
    expected.width != observed.width || expected.height != observed.height || hasMismatchedLayout tl
  };

let rec best cmp extract hd tl =>
  switch tl {
  | [] => extract hd
  | [tlHd, ...tlTl] =>
    let hdVal = extract hd;
    let minFromTail = best cmp extract tlHd tlTl;
    cmp minFromTail hdVal ? minFromTail : hdVal
  };

let round num => {
  let floored = floor num;
  if (num -. floored > 0.5) {
    int_of_float (floor (num +. 1.0))
  } else {
    int_of_float floored
  }
};

let renderBox matrix minLeft minTop layout ch => {
  let leftIndex = int_of_float (floor (layout.LayoutTypes.left -. minLeft));
  let topIndex = int_of_float (floor (layout.top -. minTop));
  let rightIndex = round (layout.left +. layout.width -. minLeft);
  let bottomIndex = round (layout.top +. layout.height -. minTop);
  for y in topIndex to bottomIndex {
    for x in leftIndex to rightIndex {
      if (x === leftIndex || x === rightIndex || y === topIndex || y === bottomIndex) {
        matrix.(x).(y) = ch
      }
    }
  }
};

let renderDiagram (containerLayout: LayoutTypes.cssLayout) childLayouts containerChar childChar => {
  let minLeft = best (<) (fun layout => layout.LayoutTypes.left) containerLayout childLayouts;
  let minTop = best (<) (fun layout => layout.LayoutTypes.top) containerLayout childLayouts;
  let maxRight =
    best
      (>) (fun layout => layout.LayoutTypes.left +. layout.LayoutTypes.width) containerLayout childLayouts;
  let maxBottom =
    best
      (>) (fun layout => layout.LayoutTypes.top +. layout.LayoutTypes.height) containerLayout childLayouts;

  /**
   * Add + 1 so that we can render zero width as one wide.
   */
  let numCols = round (maxRight -. minLeft) + 1;
  let numRows = round (maxBottom -. minTop) + 1;
  let matrix = Array.make_matrix numCols numRows '.';
  renderBox matrix minLeft minTop containerLayout containerChar;
  List.iter (fun childLayout => renderBox matrix minLeft minTop childLayout childChar) childLayouts;
  let ret = {contents: ""};
  for y in 0 to (numRows - 1) {
    for x in 0 to (numCols - 1) {
      let ch = matrix.(x).(y);
      ret.contents = ret.contents ^ String.make 1 ch
    };
    ret.contents = ret.contents ^ "\n"
  };
  ret.contents
};

let mismatchText (expectedContainerLayout, observedContainerLayout) childExpectedAndObserved => {
  let childStr (expected, observed) =>
    "  Child[expected(e)]:" ^
    LayoutPrint.layoutStr expected ^ "\n  Child[observed(o)]:" ^ LayoutPrint.layoutStr observed;
  let containerExpected = "Container[expected(E)]:" ^ LayoutPrint.layoutStr expectedContainerLayout ^ "\n";
  let containerObserved = "Container[observed(O)]:" ^ LayoutPrint.layoutStr observedContainerLayout ^ "\n";
  containerExpected ^ containerObserved ^ String.concat "\n" (List.map childStr childExpectedAndObserved)
};

let assertLayouts testNum (expectedContainerLayout, observedContainerLayout) childExpectedAndObserved => {
  ();
  assertionCount.contents = assertionCount.contents + 1;
  if (hasMismatchedLayout [(expectedContainerLayout, observedContainerLayout), ...childExpectedAndObserved]) {
    let text = mismatchText (expectedContainerLayout, observedContainerLayout) childExpectedAndObserved;
    let expectedDiagram =
      renderDiagram expectedContainerLayout (List.map fst childExpectedAndObserved) 'E' 'e';
    let observedDiagram =
      renderDiagram observedContainerLayout (List.map snd childExpectedAndObserved) 'O' 'o';
    let title = "Test " ^ string_of_int testNum ^ ":\n";
    let expected = "\nEXPECTED\n========\n" ^ expectedDiagram;
    let observed = "\nOBSERVED\n========\n" ^ observedDiagram;
    failures.contents = [
      (currentTestName.contents, title ^ text ^ expected ^ observed),
      ...failures.contents
    ]
  }
};
