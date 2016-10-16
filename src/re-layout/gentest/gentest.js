/**
 * Copyright (c) 2014-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

window.onload = function() {
 printTest(document.body.children[0], document.body.children[1]);
}

function printTest(LTRContainer, RTLContainer) {
  var commentLines = [
    '/**',
    ' * Copyright (c) 2014-present, Facebook, Inc.',
    ' * All rights reserved.',
    ' *',
    ' * This source code is licensed under the BSD-style license found in the',
    ' * LICENSE file in the root directory of this source tree. An additional grant',
    ' * of patent rights can be found in the PATENTS file in the same directory.',
    ' */',
    '',
  ];

  commentLines.push('/**');
  commentLines.push(' * @Generated by gentest/gentest.sh with the following input');
  commentLines.push(' *');

  var indentation = 0;
  commentLines.push(LTRContainer.innerHTML.split('\n').map(function(line) {
    return line.trim();
  }).filter(function(line) {
    return line.length > 0 && line !== '<div id="default"></div>';
  }).map(function(line) {
    var result;
    if (line.indexOf('</div') == 0) {
      result = '  '.repeat(indentation - 1) + line;
    } else {
      result = '  '.repeat(indentation) + line;
    }

    indentation += (line.match(/<div/g) || []).length;
    indentation -= (line.match(/<\/div/g) || []).length;
    return result;
  }).reduce(function(curr, prev) {
    if (prev.indexOf('<div') == 0) {
      prev = '\n' + prev;
    }
    return curr + '\n' + prev;
  }));
  commentLines.push(' *');
  commentLines.push(' */');
  commentLines.push('');

  var LTRLayoutTree = calculateTree(LTRContainer);
  var RTLLayoutTree = calculateTree(RTLContainer);
  /**
   * Could use either LTR or RTL for this since they're the same html.
   * We're only finding the *computed* style (style that was described in the
   * style= property, not the actual layout).
   */
  var treeWithComputedStyle = LTRLayoutTree;

  let testNameLines = [];
  let testLines = [];
  /**
   * Lines containing function definitions, one for each benchmark.
   */
  let benchmarkFunctionDefinitions = [];
  /**
   * List of core_bench benchmarks.
   */
  let reasonListOfCoreBenchmarks = [
    'let benchmarks = [];'
  ];
  /**
   * All the function bodies of the reasonListOfCoreBenchmarks functions in one
   * list so the entire set may be tested as a single unit.
   */
  let allBenchmarkLinesAsOne = [];

  for (var i = 0; i < treeWithComputedStyle.length; i++) {
    let testName = treeWithComputedStyle[i].name;
    testNameLines.push('let ' + testName + ' = "'+ testName + '";');
    let setupTreeStr =
      '  ' + setupTestTree(
        testName,
        undefined,
        treeWithComputedStyle[i],
        'root',
        null
      ).join('\n');
    let performLtrLine = '  Layout.layoutNode root LayoutSupport.cssUndefined LayoutSupport.cssUndefined CssDirectionLtr;';
    let performRtlLine = '  Layout.layoutNode root LayoutSupport.cssUndefined LayoutSupport.cssUndefined CssDirectionRtl;';
    testLines = testLines.concat([
      'it ' + testName + ' (fun () => {',
      setupTreeStr,
      performLtrLine,
      '',
      '  ' + assertTestTree(LTRLayoutTree[i], 'root', null).join('\n  '),
      '',
      performRtlLine,
      '',
      '  ' + assertTestTree(RTLLayoutTree[i], 'root', null).join('\n  '),
      '});',
      ''
    ]);
    let thisBenchmarkFunctionName = 'bench_' + testName;
    let thisBenchmarkFunction = [
      ' let ' + thisBenchmarkFunctionName + ' () => {',
      '  ' + setupTreeStr,
      '  ' + performLtrLine,
      '  ' + performRtlLine,
      '};'
    ];
    allBenchmarkLinesAsOne = allBenchmarkLinesAsOne.concat([
      setupTreeStr,
      performLtrLine,
      performRtlLine,
    ]);

    benchmarkFunctionDefinitions = benchmarkFunctionDefinitions.concat (thisBenchmarkFunction);
    reasonListOfCoreBenchmarks = reasonListOfCoreBenchmarks.concat([
      'let benchmarks = LayoutTestUtils.shouldRun ' + testName + ' ?',
      '  [',
      '    Bench.Test.create name::' + testName,
      '    ' + thisBenchmarkFunctionName + ',',
      '    ...benchmarks',
      '  ] :',
      '  benchmarks;'
    ]);
  }

  testLines.push('  LayoutTestUtils.displayOutcomes ();');
  if (errors.length !== 0) {
    throw new Error(errors.join('\n'));
  }
  reasonListOfCoreBenchmarks = reasonListOfCoreBenchmarks.map((line) => '  ' + line);
  testLines = testLines.map((line) => '  ' + line);
  let totalLines = [
    '{commentLines}',
    'open LayoutTestUtils;',
    '{testNameLines}',
    'open Core.Std;',
    'open Core_bench.Std;',
    'if (LayoutTestUtils.runMode === Bench) {',
    '  if LayoutTestUtils.shouldBenchmarkAllAsOne {',
    '    Command.run (Bench.make_command [Bench.Test.create name::"all-benchmarks" (fun()=>{',
    '      {allBenchmarkLinesAsOne}',
    '    })]);',
    '  } else {',
    '    {benchmarkFunctionDefinitions}',
    '    {reasonListOfCoreBenchmarks}',
    '    Command.run (Bench.make_command benchmarks);',
    '  }',
    '} else {',
    '  {testLines}',
    '};'
  ].join('\n')
  .replace('{commentLines}', commentLines.join('\n'))
  .replace('{testNameLines}', testNameLines.join('\n'))
  .replace('{allBenchmarkLinesAsOne}', allBenchmarkLinesAsOne.join('\n        '))
  .replace('{benchmarkFunctionDefinitions}', benchmarkFunctionDefinitions.join('\n    '))
  .replace('{reasonListOfCoreBenchmarks}', reasonListOfCoreBenchmarks.join('\n    '))
  .replace('{testLines}', testLines.join('\n  '));
  console.log(totalLines);
}

let ensureFloat = (v) => (v + '').indexOf('.') === -1 ? (v + '.0') : v;

let errors = [];

let testNum = 0;

let createLayoutExtensionNode = (nodeName, top, left, width, height) =>
  ('{...' + nodeName + '.layout, top:layoutTop, left: layoutLeft, width: layoutWidth, height: layoutHeight}')
    .replace('layoutTop', ensureFloat(top))
    .replace('layoutLeft', ensureFloat(left))
    .replace('layoutWidth', ensureFloat(width))
    .replace('layoutHeight', ensureFloat(height));

const createInequalityChecker = (node, nodeName) => {
  return nodeName + '.layout.top != ' + ensureFloat(node.top) + ' || ' +
    nodeName + '.layout.left != ' + ensureFloat(node.left) + ' || ' +
    nodeName + '.layout.width != ' + ensureFloat(node.width) + ' || ' +
    nodeName + '.layout.height != ' + ensureFloat(node.height);
};

/**
 * Should render container validation iff
 *   parentName is null || hasChildren (because it's nice to see the node
 *   validated again as a container this time).
 */
function assertTestTreePrint(node, nodeName, parentNode) {
  let shouldValidate =
    parentNode == null || node.children.length !== 0;

  if (!shouldValidate) {
    return [].join('');
  } else {
    let childItems = node.children.map(
      (childNode, i) => {
        const childName = nodeName + '_child' + i;
        return '  (' + createLayoutExtensionNode(childName, childNode.top, childNode.left, childNode.width, childNode.height) + ', ' + childName + '.layout),';
      }
    );
    let childLines = ['['].concat(childItems).concat([']']);
    let assertionPrintingLines =
      [
        'assertLayouts testNum (expectedContainerLayout, observedContainerLayout)'
          .replace('testNum', testNum++)
          .replace('expectedContainerLayout', createLayoutExtensionNode(nodeName, node.top, node.left, node.width, node.height))
          .replace('observedContainerLayout', nodeName + '.layout'),
      ].concat(childLines).concat([';']);

    let recursePrettyPrintLines = [];
    for (var i = 0; i < node.children.length; i++) {
      recursePrettyPrintLines.push('');
      var childName = nodeName + '_child' + i;
      recursePrettyPrintLines = recursePrettyPrintLines.concat(assertTestTreePrint(node.children[i], childName, node));
    }
    let doThePrettyPrintingTestStr = assertionPrintingLines.concat(recursePrettyPrintLines);
    return doThePrettyPrintingTestStr.join('\n');
  }
};

function assertTestTreeFast(node, nodeName, parentNode) {
  return [
    createInequalityChecker(node, nodeName)
  ].concat(node.children.map((childNode, i) => {
    const childName = nodeName + '_child' + i;
    return assertTestTreeFast(childNode, childName, node);
  })).join(' ||\n');
};

function assertTestTree(node, nodeName, parentNode) {
  let fastNumericCheckExprStr = assertTestTreeFast(node, nodeName, parentNode);
  let testAndPrettyPrint = [
    'if (' + fastNumericCheckExprStr + ') {',
    '  ' + assertTestTreePrint(node, nodeName, parentNode),
    '};'
  ];
  return testAndPrettyPrint;
};

function setupTestTree(testName, parent, node, nodeName, parentName, index) {
  var lines = [];

  var styleLines = [];
  for (var style in node.computedStyleForKebabs) {

    // Skip position info for root as it messes up tests
    if (node.declaredStyle[style] === "" &&
        (style == 'position' ||
         style == 'left' ||
         style == 'start' ||
         style == 'top' ||
         style == 'right' ||
         style == 'end' ||
         style == 'bottom' ||
         style == 'width' ||
         style == 'height')) {
      continue;
    }
    let val = node.computedStyleForKebabs[style];
    if (val !== getDefaultStyleValue(style)) {
      switch (style) {
        case 'margin-left':
          if (node.rawStyle.indexOf('margin-left-because-start') !== -1) {
            styleLines.push('marginStart: ' + pixelValue(val));
          } else if (node.rawStyle.indexOf('margin-left-because-end') !== -1) {
            styleLines.push('marginEnd: ' + pixelValue(val));
          } else {
            styleLines.push('marginLeft: ' + pixelValue(val));
          }
          break;
        case 'margin-right':
          if (node.rawStyle.indexOf('margin-right-because-start') !== -1) {
            styleLines.push('marginStart: ' + pixelValue(val));
          } else if (node.rawStyle.indexOf('margin-right-because-end') !== -1) {
            styleLines.push('marginEnd: ' + pixelValue(val));
          } else {
            styleLines.push('marginRight: ' + pixelValue(val));
          }
          break;
        case 'padding-left':
          if (node.rawStyle.indexOf('padding-left-because-start') !== -1) {
            styleLines.push('paddingStart: ' + pixelValue(val));
          } else if (node.rawStyle.indexOf('padding-left-because-end') !== -1) {
            styleLines.push('paddingEnd: ' + pixelValue(val));
          } else {
            styleLines.push('paddingLeft: ' + pixelValue(val));
          }
          break;
        case 'padding-right':
          if (node.rawStyle.indexOf('padding-right-because-start') !== -1) {
            styleLines.push('paddingStart: ' + pixelValue(val));
          } else if (node.rawStyle.indexOf('padding-right-because-end') !== -1) {
            styleLines.push('paddingEnd: ' + pixelValue(val));
          } else {
            styleLines.push('paddingRight: ' + pixelValue(val));
          }
          break;
        case 'border-left-width':
          if (node.rawStyle.indexOf('border-left-width-because-start') !== -1) {
            styleLines.push('borderStart: ' + pixelValue(val));
          } else if (node.rawStyle.indexOf('border-left-width-because-end') !== -1) {
            styleLines.push('borderEnd: ' + pixelValue(val));
          } else {
            styleLines.push('borderLeft: ' + pixelValue(val));
          }
          break;
        case 'border-right-width':
          if (node.rawStyle.indexOf('border-right-width-because-start') !== -1) {
            styleLines.push('borderStart: ' + pixelValue(val));
          } else if (node.rawStyle.indexOf('border-right-width-because-end') !== -1) {
            styleLines.push('borderEnd: ' + pixelValue(val));
          } else {
            styleLines.push('borderRight: ' + pixelValue(val));
          }
          break;
        case 'direction':
          styleLines.push('direction:' + directionValue(val));
          break;
        case 'flex-direction':
          styleLines.push('flexDirection:'+ flexDirectionValue(val));
          break;
        case 'justify-content':
          styleLines.push('justifyContent: ' + justifyValue(val));
          break;
        case 'align-content':
          styleLines.push('alignContent: ' + alignValue(val));
          break;
        case 'align-items':
          styleLines.push('alignItems: ' + alignValue(val));
          break;
        case 'align-self':
          if (val) {
            styleLines.push('alignSelf: ' +  alignValue(val));
          }
          break;
        case 'position':
          styleLines.push('positionType: ' + positionValue(val));
          break;
        case 'flex-wrap':
          styleLines.push('flexWrap: ' + wrapValue(val));
          break;
        case 'overflow':
          styleLines.push('overflow: ' + overflowValue(val));
          break;
        case 'flex-grow':
          styleLines.push('flexGrow: ' + ensureFloat(val));
          break;
        case 'flex-shrink':
          styleLines.push('flexShrink: ' +  ensureFloat(val));
          break;
        case 'flex-basis':
          styleLines.push('flexBasis: ' +  pixelValue(val));
          break;
        case 'left':
          if (node.rawStyle.indexOf('left-because-start') !== -1) {
            styleLines.push('start: ' + pixelValue(val));
          } else if (node.rawStyle.indexOf('left-because-end') !== -1) {
            styleLines.push('endd: ' + pixelValue(val));
          } else {
            styleLines.push('left: ' + pixelValue(val));
          }
          break;
        case 'top':
          styleLines.push('top: ' + pixelValue(val));
          break;
        case 'right':
          if (node.rawStyle.indexOf('right-because-start') !== -1) {
            styleLines.push('start: ' + pixelValue(val));
          } else if (node.rawStyle.indexOf('right-because-end') !== -1) {
            styleLines.push('endd: ' + pixelValue(val));
          } else {
            styleLines.push('right: ' + pixelValue(val));
          }
          break;
        case 'bottom':
          styleLines.push('bottom: ' + pixelValue(val));
          break;
        case 'margin-top':
          styleLines.push('marginTop: ' + pixelValue(val));
          break;
        case 'margin-bottom':
          styleLines.push('marginBottom: ' + pixelValue(val));
          break;
        case 'padding-top':
          styleLines.push('paddingTop: ' + pixelValue(val));
          break;
        case 'padding-bottom':
          styleLines.push('paddingBottom: ' + pixelValue(val));
          break;
        case 'border-top-width':
          styleLines.push('borderTop: ' + pixelValue(val));
          break;
        case 'border-bottom-width':
          styleLines.push('borderBottom: ' + pixelValue(val));
          break;
        case 'width':
          styleLines.push('width: ' + pixelValue(val));
          break;
        case 'min-width':
          styleLines.push('minWidth: ' + pixelValue(val));
          break;
        case 'max-width':
          styleLines.push('maxWidth: ' + pixelValue(val));
          break;
        case 'height':
          styleLines.push('height: ' + pixelValue(val));
          break;
        case 'min-height':
          styleLines.push('minHeight: ' + pixelValue(val));
          break;
        case 'max-height':
          styleLines.push('maxHeight: ' +  pixelValue(val));
          break;
      }
    }
  }

  if (styleLines.length > 0) {
    lines = lines.concat([
      'let ' + nodeName + '_style = {',
      '  ...LayoutSupport.defaultStyle,'
    ]);
    lines = lines.concat(styleLines.map((sl) => '  ' + sl + ','));
    lines.push('};');
  } else {
    lines = lines.concat([ 'let ' + nodeName + '_style = LayoutSupport.defaultStyle;']);
  }

  let childrenArray = [];
  for (var i = 0; i < node.children.length; i++) {
    lines.push('');
    var childName = nodeName + '_child' + i;
    lines = lines.concat(
        setupTestTree(
            testName + ' (child)',
            node,
            node.children[i],
            childName,
            nodeName,
            i));

    childrenArray.push(childName);
  }

  lines.push(
    'let ' + nodeName + ' = LayoutSupport.createNode ' +
      'withChildren::[|' + childrenArray.join(',') + '|] ' +
      'andStyle::' + nodeName + '_style ();'
  );
  return lines;
}

function overflowValue(value) {
  switch (value) {
    case 'visible': return 'Visible';
    case 'hidden': return 'Hidden';
  }
}

function wrapValue(value) {
  switch (value) {
    case 'wrap': return 'CssWrap';
    case 'nowrap': return 'CssNoWrap';
  }
}

function flexDirectionValue(value) {
  switch (value) {
    case 'row': return 'CssFlexDirectionRow';
    case 'row-reverse': return 'CssFlexDirectionRowReverse';
    case 'column': return 'CssFlexDirectionColumn';
    case 'column-reverse': return 'CssFlexDirectionColumnReverse';
  }
}

function justifyValue(value) {
  switch (value) {
    case 'center': return 'CssJustifyCenter';
    case 'space-around': return 'CssJustifySpaceAround';
    case 'space-between': return 'CssJustifySpaceBetween';
    case 'flex-start': return 'CssJustifyFlexStart';
    case 'flex-end': return 'CssJustifyFlexEnd';
  }
}

function positionValue(value) {
  switch (value) {
    case 'absolute': return 'CssPositionAbsolute';
    default: return 'CssPositionRelative'
  }
}

function directionValue(value) {
  switch (value) {
    case 'ltr': return 'CssDirectionLtr';
    case 'rtl': return 'CssDirectionRtl';
    case 'inherit': return 'CssDirectionInherit';
  }
}

function alignValue(value) {
  switch (value) {
    case 'auto': return 'CssAlignAuto';
    case 'center': return 'CssAlignCenter';
    case 'stretch': return 'CssAlignStretch';
    case 'flex-start': return 'CssAlignFlexStart';
    case 'flex-end': return 'CssAlignFlexEnd';
  }
}

function pixelValue(value) {
  switch (value) {
    case 'auto': return 'LayoutSupport.cssUndefined';
    case 'undefined': return 'LayoutSupport.cssUndefined';
    default: return (
      (value.replace('px', '')).indexOf('.') === -1 ?
        (value.replace('px', '')) + '.0' :
        (value.replace('px', ''))
    );
  }
}

function getDefaultStyleValue(style) {
  if (style == 'position') {
    return 'relative';
  }
  var node = document.getElementById('default');
  return getComputedStyle(node, null).getPropertyValue(style);
}

function calculateTree(rootDOMNode) {
  var rootLayout = [];

  for (var i = 0; i < rootDOMNode.children.length; i++) {
    var childDOMNode = rootDOMNode.children[i];
    if (childDOMNode.id === 'default') {
      continue;
    }
    rootLayout.push({
      name: childDOMNode.id !== '' ? childDOMNode.id : 'iNSERT_NAME_HERE',
      left: childDOMNode.offsetLeft + childDOMNode.parentNode.clientLeft,
      top: childDOMNode.offsetTop + childDOMNode.parentNode.clientTop,
      width: childDOMNode.offsetWidth,
      height: childDOMNode.offsetHeight,
      children: calculateTree(childDOMNode),
      computedStyleForKebabs: getComputedStyleInKebabForm(childDOMNode),
      declaredStyle: childDOMNode.style,
      rawStyle: childDOMNode.getAttribute('style') || "",
    });
  }

  return rootLayout;
}

function getComputedStyleInKebabForm(node) {
  return [
    'direction',
    'flex-direction',
    'justify-content',
    'align-content',
    'align-items',
    'align-self',
    'position',
    'flex-wrap',
    'overflow',
    'flex-grow',
    'flex-shrink',
    'flex-basis',
    'top',
    'bottom',
    'left',
    'right',
    'margin-top',
    'margin-bottom',
    'padding-top',
    'padding-bottom',
    'border-top-width',
    'border-bottom-width',
    'width',
    'min-width',
    'max-width',
    'height',
    'min-height',
    'max-height',

    /**
     * We don't need to *read*.
     */
    'margin-left',
    'margin-right',
    'padding-left',
    'padding-right',
    'border-left-width',
    'border-right-width',
  ].reduce(function(map, key) {
    map[key] = getComputedStyle(node, null).getPropertyValue(key);
    return map;
  }, {});
}


/* Supported:
 
 <div id="align_items_stretch" style="width: 100px; height: 100px;">
   <div style="height: 10px;"></div>
 </div>

 <div id="align_items_center" style="width: 100px; height: 100px; align-items: center;">
   <div style="height: 10px; width: 10px;"></div>
 </div>

 <div id="align_items_flex_start" style="width: 100px; height: 100px; align-items: flex-start;">
   <div style="height: 10px; width: 10px;"></div>
 </div>

 <div id="align_items_flex_end" style="width: 100px; height: 100px; align-items: flex-end;">
   <div style="height: 10px; width: 10px;"></div>
 </div>

 <div id="align_self_center" style="width:100px; height: 100px;">
   <div style="height: 10px; width: 10px; align-self: center;"></div>
 </div>

 <div id="align_self_flex_end" style="width:100px; height: 100px;">
   <div style="height: 10px; width: 10px; align-self: flex-end;"></div>
 </div>

 <div id="align_self_flex_start" style="width:100px; height: 100px;">
   <div style="height: 10px; width: 10px; align-self: flex-start;"></div>
 </div>

 <div id="align_self_flex_end_override_flex_start" style="width:100px; height: 100px; align-items: flex-start;">
   <div style="height: 10px; width: 10px; align-self: flex-end;"></div>
 </div>

 <div id="border_no_size" style="border-width: 10px;">
 </div>

 <div id="border_container_match_child" style="border-width: 10px;">
   <div style="width: 10px; height: 10px;"></div>
 </div>

 <div id="border_stretch_child" style="width: 100px; height: 100px; border-width: 10px;">
   <div style="height: 10px;"></div>
 </div>

 <div id="border_center_child" style="width: 100px; height: 100px; border-replaceWithActualStart-width-because-start: 1; border-replaceWithActualStart-width: 10px; border-top-width: 10px; border-replaceWithActualEnd-width-because-end: 1; border-replaceWithActualEnd-width: 20px; border-bottom-width: 20px; align-items: center; justify-content: center;">
   <div style="height: 10px; width: 10px;"></div>
 </div>

 <div id="max_width" style="width: 100px; height: 100px;">
   <div style="height: 10px; max-width: 50px;"></div>
 </div>

 <div id="max_height" style="width: 100px; height: 100px; flex-direction: row;">
   <div style="width: 10px; max-height: 50px;"></div>
 </div>


 <div id="padding_no_size" style="padding: 10px;">
 </div>

 <div id="padding_container_match_child" style="padding: 10px;">
   <div style="width: 10px; height: 10px;"></div>
 </div>


 <div id="padding_stretch_child" style="width: 100px; height: 100px; padding: 10px;">
   <div style="height: 10px;"></div>
 </div>

 <div id="padding_center_child" style="width: 100px; height: 100px; padding-replaceWithActualStart-because-start: 1; padding-replaceWithActualStart: 10px; padding-top: 10px; padding-replaceWithActualEnd-because-end: 1;  padding-replaceWithActualEnd: 20px; padding-bottom: 20px; align-items: center; justify-content: center;">
   <div style="height: 10px; width: 10px;"></div>
 </div>

 <div id="absolute_layout_width_height_start_top" style="width: 100px; height: 100px;">
   <div style="width:10px; height: 10px; position: absolute; replaceWithActualStart-because-start: 1; replaceWithActualStart: 10px; top: 10px;"></div>
 </div>

 <div id="absolute_layout_width_height_end_bottom" style="width: 100px; height: 100px;">
   <div style="width:10px; height: 10px; position: absolute; replaceWithActualEnd-because-end: 1; replaceWithActualEnd: 10px; bottom: 10px;"></div>
 </div>

 <div id="absolute_layout_start_top_end_bottom" style="width: 100px; height: 100px;">
   <div style="position: absolute; replaceWithActualStart-because-start: 1;  start: 10px; top: 10px; replaceWithActualEnd-because-end: 1; replaceWithActualEnd: 10px; bottom: 10px;"></div>
 </div>

 <div id="absolute_layout_width_height_start_top_end_bottom" style="width: 100px; height: 100px;">
   <div style="width:10px; height: 10px; position: absolute; replaceWithActualStart-because-start: 1; replaceWithActualStart: 10px; top: 10px; replaceWithActualEnd-because-end: 1; replaceWithActualEnd: 10px; bottom: 10px;"></div>
 </div>

 <div id="do_not_clamp_height_of_absolute_node_to_height_of_its_overflow_hidden_parent" style="height: 50px; width: 50px; overflow: hidden; flex-direction: row;">
   <div style="position: absolute; replaceWithActualStart-because-start: 1; replaceWithActualStart: 0; top: 0;">
     <div style="width: 100px; height: 100px;"></div>
   </div>
 </div>

 <div id="flex_direction_column_no_height" style="width: 100px">
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
 </div>

 <div id="flex_direction_row_no_width" style="height: 100px; flex-direction: row;">
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
 </div>

 <div id="flex_direction_column" style="height: 100px; width: 100px;">
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
 </div>

 <div id="flex_direction_row" style="height: 100px; width: 100px; flex-direction: row;">
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
 </div>

 <div id="flex_direction_column_reverse" style="height: 100px; width: 100px; flex-direction: column-reverse;">
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
 </div>

 <div id="flex_direction_row_reverse" style="height: 100px; width: 100px; flex-direction: row-reverse;">
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
 </div>

 <div id="wrap_column" style="height: 100px; width: 60px; flex-wrap: wrap">
   <div style="height: 30px; width: 30px;"></div>
   <div style="height: 30px; width: 30px;"></div>
   <div style="height: 30px; width: 30px;"></div>
   <div style="height: 30px; width: 30px;"></div>
 </div>

 <div id="wrap_row" style="width: 100px; flex-direction: row; flex-wrap: wrap">
   <div style="height: 30px; width: 30px;"></div>
   <div style="height: 30px; width: 30px;"></div>
   <div style="height: 30px; width: 30px;"></div>
   <div style="height: 30px; width: 30px;"></div>
 </div>

 <div id="wrap_row_align_items_flex_end" style="width: 100px; flex-direction: row; flex-wrap: wrap; align-items: flex-end;">
   <div style="height: 10px; width: 30px;"></div>
   <div style="height: 20px; width: 30px;"></div>
   <div style="height: 30px; width: 30px;"></div>
   <div style="height: 30px; width: 30px;"></div>
 </div>

 <div id="wrap_row_align_items_center" style="width: 100px; flex-direction: row; flex-wrap: wrap; align-items: center;">
   <div style="height: 10px; width: 30px;"></div>
   <div style="height: 20px; width: 30px;"></div>
   <div style="height: 30px; width: 30px;"></div>
   <div style="height: 30px; width: 30px;"></div>
 </div>

 <div id="margin_start" style="width: 100px; height: 100px; flex-direction: row;">
   <div style="width: 10px; margin-replaceWithActualStart-because-start: 1;  margin-replaceWithActualStart: 10px;"></div>
 </div>

 <div id="margin_end" style="width: 100px; height: 100px; flex-direction: row; justify-content: flex-end;">
   <div style="width: 10px; margin-replaceWithActualEnd-because-end: 1; margin-replaceWithActualEnd: 10px;"></div>
 </div>

 <div id="margin_left" style="width: 100px; height: 100px; flex-direction: row;">
   <div style="width: 10px; margin-left: 10px;"></div>
 </div>

 <div id="margin_top" style="width: 100px; height: 100px;">
   <div style="height: 10px; margin-top: 10px;"></div>
 </div>

 <div id="margin_right" style="width: 100px; height: 100px; flex-direction: row; justify-content: flex-end;">
   <div style="width: 10px; margin-right: 10px;"></div>
 </div>

 <div id="margin_bottom" style="width: 100px; height: 100px; justify-content: flex-end;">
   <div style="height: 10px; margin-bottom: 10px;"></div>
 </div>

 <div id="align_content_flex_start" style="width: 100px; height: 100px; flex-wrap: wrap; flex-direction: column; align-content: flex-start;">
   <div style="width: 50px; height: 10px;"></div>
   <div style="width: 50px; height: 10px;"></div>
   <div style="width: 50px; height: 10px;"></div>
   <div style="width: 50px; height: 10px;"></div>
   <div style="width: 50px; height: 10px;"></div>
 </div>

 <div id="align_content_flex_end" style="width: 100px; height: 100px; flex-wrap: wrap; flex-direction: column; align-content: flex-end;">
   <div style="width: 50px; height: 10px;"></div>
   <div style="width: 50px; height: 10px;"></div>
   <div style="width: 50px; height: 10px;"></div>
   <div style="width: 50px; height: 10px;"></div>
   <div style="width: 50px; height: 10px;"></div>
 </div>

 <div id="align_content_center" style="width: 100px; height: 100px; flex-wrap: wrap; flex-direction: column; align-content: center;">
   <div style="width: 50px; height: 10px;"></div>
   <div style="width: 50px; height: 10px;"></div>
   <div style="width: 50px; height: 10px;"></div>
   <div style="width: 50px; height: 10px;"></div>
   <div style="width: 50px; height: 10px;"></div>
 </div>

 <div id="align_content_stretch" style="width: 100px; height: 100px; flex-wrap: wrap; flex-direction: column; align-content: stretch;">
   <div style="width: 50px;"></div>
   <div style="width: 50px;"></div>
   <div style="width: 50px;"></div>
   <div style="width: 50px;"></div>
   <div style="width: 50px;"></div>
 </div>

 <div id="justify_content_row_flex_start" style="width: 102px; height: 102px; flex-direction: row; justify-content: flex-start;">
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
 </div>

 <div id="justify_content_row_flex_end" style="width: 102px; height: 102px; flex-direction: row; justify-content: flex-end;">
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
 </div>

 <div id="justify_content_row_center" style="width: 102px; height: 102px; flex-direction: row; justify-content: center;">
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
 </div>

 <div id="justify_content_row_space_between" style="width: 102px; height: 102px; flex-direction: row; justify-content: space-between;">
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
 </div>

 <div id="justify_content_row_space_around" style="width: 102px; height: 102px; flex-direction: row; justify-content: space-around;">
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
   <div style="width: 10px;"></div>
 </div>

 <div id="justify_content_column_flex_start" style="width: 102px; height: 102px; justify-content: flex-start;">
   <div style="height: 10px;"></div>
   <div style="heigth: 10px;"></div>
   <div style="height: 10px;"></div>
 </div>

 <div id="justify_content_column_flex_end" style="width: 102px; height: 102px; justify-content: flex-end;">
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
 </div>

 <div id="justify_content_column_center" style="width: 102px; height: 102px; justify-content: center;">
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
 </div>

 <div id="justify_content_column_space_between" style="width: 102px; height: 102px; justify-content: space-between;">
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
 </div>

 <div id="justify_content_column_space_around" style="width: 102px; height: 102px; justify-content: space-around;">
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
   <div style="height: 10px;"></div>
 </div>
   
 <div id="border_flex_child" style="width: 100px; height: 100px; border-width: 10px;">
   <div style="width: 10px; flex-grow:1"></div>
 </div>

 <div id="min_height" style="width: 100px; height: 100px;">
   <div style="flex-grow: 1; min-height: 60px;"></div>
   <div style="flex-grow: 1;"></div>
 </div>

 <div id="min_width" style="width: 100px; height: 100px; flex-direction: row">
   <div style="flex-grow: 1; min-width: 60px;"></div>
   <div style="flex-grow: 1;"></div>
 </div>
 <div id="padding_flex_child" style="width: 100px; height: 100px; padding: 10px;">
   <div style="width: 10px; flex-grow:1"></div>
 </div>
 <div id="margin_and_flex_row" style="width: 100px; height: 100px; flex-direction: row;">
   <div style="margin-replaceWithActualStart-because-start: 1; margin-replaceWithActualStart: 10px; margin-replaceWithActualEnd-because-end: 1; margin-replaceWithActualEnd: 10px; flex-grow: 1;"></div>
 </div>

 <div id="margin_and_flex_column" style="width: 100px; height: 100px;">
   <div style="margin-top: 10px; margin-bottom: 10px; flex-grow: 1;"></div>
 </div>

 <div id="margin_and_stretch_row" style="width: 100px; height: 100px; flex-direction: row;">
   <div style="margin-top: 10px; margin-bottom: 10px; flex-grow: 1;"></div>
 </div>

 <div id="margin_and_stretch_column" style="width: 100px; height: 100px;">
   <div style="margin-replaceWithActualStart-because-start: 1; margin-replaceWithActualStart: 10px; margin-replaceWithActualEnd-because-end: 1; margin-replaceWithActualEnd: 10px; flex-grow: 1;"></div>
 </div>

 <div id="margin_with_sibling_row" style="width: 100px; height: 100px; flex-direction: row;">
   <div style="margin-replaceWithActualEnd-because-end: 1; margin-replaceWithActualEnd: 10px; flex-grow: 1;"></div>
   <div style="flex-grow: 1;"></div>
 </div>

 <div id="margin_with_sibling_column" style="width: 100px; height: 100px;">
   <div style="margin-bottom: 10px; flex-grow: 1;"></div>
   <div style="flex-grow: 1;"></div>
 </div>
 <div id="flex_basis_flex_grow_column" style="width: 100px; height: 100px;">
   <div style="flex-basis: 50px; flex-grow: 1;"></div>
   <div style="flex-grow: 1;"></div>
 </div>

 <div id="flex_basis_flex_grow_row" style="width: 100px; height: 100px; flex-direction: row;">
   <div style="flex-basis: 50px; flex-grow: 1;"></div>
   <div style="flex-grow: 1;"></div>
 </div>

 <div id="flex_basis_flex_shrink_column" style="width: 100px; height: 100px;">
   <div style="flex-basis: 100px; flex-shrink: 1;"></div>
   <div style="flex-basis: 50px;"></div>
 </div>

 <div id="flex_basis_flex_shrink_row" style="width: 100px; height: 100px; flex-direction: row;">
   <div style="flex-basis: 100px; flex-shrink: 1;"></div>
   <div style="flex-basis: 50px;"></div>
 </div>



 <div id="jwalke_border_width_only_start" style="width: 100px; height: 100px; border-replaceWithActualStart-width-because-start: 1; border-replaceWithActualStart-width: 10px; border-top-width: 10px; border-bottom-width: 20px; align-items: center; justify-content: center;">
   <div style="height: 10px; width: 10px;"></div>
 </div>

 <div id="jwalke_border_width_only_end" style="width: 100px; height: 100px; border-replaceWithActualEnd-width-because-end: 1; border-replaceWithActualEnd-width: 10px; border-top-width: 10px; border-bottom-width: 20px; align-items: center; justify-content: center;">
   <div style="height: 10px; width: 10px;"></div>
 </div>


*/





/*

This one doesn't work correctly in Chrome, so not sure what to do.

 <div id="wrap_column" style="height: 100px; flex-wrap: wrap">
   <div style="height: 30px; width: 30px;"></div>
   <div style="height: 30px; width: 30px;"></div>
   <div style="height: 30px; width: 30px;"></div>
   <div style="height: 30px; width: 30px;"></div>
 </div>

*/

