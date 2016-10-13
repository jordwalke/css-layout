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
  var lines = [
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

  lines.push('/**');
  lines.push(' * @Generated by gentest/gentest.sh with the following input');
  lines.push(' *');

  var indentation = 0;
  lines.push(LTRContainer.innerHTML.split('\n').map(function(line) {
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
  lines.push(' *');
  lines.push(' */');
  lines.push('');

  lines.push([
    'let startTime = Sys.time ();',
    'open LayoutTestUtils;',
    'let times = switch LayoutTestUtils.benchmarkTimes { | None => 0 | Some n => n};',
    'for ii in 0 to times {',
    '',
  ].reduce(function(curr, prev) {
    return curr + '\n' + prev;
  }));

  var LTRLayoutTree = calculateTree(LTRContainer);
  var RTLLayoutTree = calculateTree(RTLContainer);
  /**
   * Could use either LTR or RTL for this since they're the same html.
   * We're only finding the *computed* style (style that was described in the
   * style= property, not the actual layout).
   */
  var treeWithComputedStyle = LTRLayoutTree;

  let testStringLines = [];
  for (var i = 0; i < treeWithComputedStyle.length; i++) {
    testStringLines.push('let ' + treeWithComputedStyle[i].name + ' = "'+ treeWithComputedStyle[i].name + '";');
    lines.push('it ' + treeWithComputedStyle[i].name + ' (fun () => {');

    lines.push('  ' + setupTestTree(
        treeWithComputedStyle[i].name,
        undefined,
        treeWithComputedStyle[i],
        'root',
        null).reduce(function(curr, prev) {
      return curr + '\n  ' + prev;
    }));

    lines.push('  Layout.layoutNode (root, LayoutSupport.cssUndefined, LayoutSupport.cssUndefined, CSS_DIRECTION_LTR);');
    lines.push('');

    lines.push('  ' + assertTestTree(LTRLayoutTree[i], 'root', null).reduce(function(curr, prev) {
      return curr + '\n  ' + prev;
    }));
    lines.push('');

    lines.push('  Layout.layoutNode (root, LayoutSupport.cssUndefined, LayoutSupport.cssUndefined, CSS_DIRECTION_RTL);');
    lines.push('');

    lines.push('  ' + assertTestTree(RTLLayoutTree[i], 'root', null).reduce(function(curr, prev) {
      return curr + '\n  ' + prev;
    }));

    lines.push('});');
    lines.push('');
  }


  lines.push('};');
  lines.push('let endTime = Sys.time();');
  lines.push('if (times > 0) {\n');
  lines.push('  print_string "TOTAL TIME:\n";');
  lines.push('  print_float (endTime -. startTime);');
  lines.push('  print_newline ();');
  lines.push('} else {');
  lines.push('  LayoutTestUtils.displayOutcomes ();');
  lines.push('};');
  printLines(testStringLines.concat(lines));
  if (errors.length !== 0) {
    throw new Error(errors.join('\n'));
  }
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

/**
 * Should render container validation iff
 *   parentName is null || hasChildren (because it's nice to see the node
 *   validated again as a container this time).
 */
function assertTestTree(node, nodeName, parentNode) {
  let shouldValidate =
    parentNode == null || node.children.length !== 0;

  if (!shouldValidate) {
    return [];
  } else {
    let childItems = node.children.map(
      (childNode, i) => {
        const childName = nodeName + '_child' + i;
        return '  (' + createLayoutExtensionNode(childName, childNode.top, childNode.left, childNode.width, childNode.height) + ', ' + childName + '.layout),';
      }
    );
    let childLines = ['['].concat(childItems).concat([']']);
    let assertionLines =
      [
        'assertLayouts testNum (expectedContainerLayout, observedContainerLayout)'
          .replace('testNum', testNum++)
          .replace('expectedContainerLayout', createLayoutExtensionNode(nodeName, node.top, node.left, node.width, node.height))
          .replace('observedContainerLayout', nodeName + '.layout'),
      ].concat(childLines).concat(';');

    let recurseLines = [];
    for (var i = 0; i < node.children.length; i++) {
      recurseLines.push('');
      var childName = nodeName + '_child' + i;
      recurseLines = recurseLines.concat(assertTestTree(node.children[i], childName, node));
    }
    return assertionLines.concat(recurseLines);
  }
}

function setupTestTree(testName, parent, node, nodeName, parentName, index) {
  var lines = [
    'let ' + nodeName + ' = LayoutSupport.createNode ();',
  ];

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

  if (styleLines.length !== 0) {
    lines = lines.concat([
      'let ' + nodeName + ' = {',
      '  ...' + nodeName + ',',
      '  style: {',
      '    ...' + nodeName + '.style,'
    ]);
    lines = lines.concat(styleLines.map((sl) => '    ' + sl + ','));
    lines.push('  }');
    lines.push('};');
  }

  if (parentName) {
    lines.push('LayoutSupport.insertChild ' + parentName + ' ' + nodeName + ' ' + index + ';');
  }

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
  }

  return lines;
}

function overflowValue(value) {
  switch (value) {
    case 'visible': return 'CSS_OVERFLOW_VISIBLE';
    case 'hidden': return 'CSS_OVERFLOW_HIDDEN';
  }
}

function wrapValue(value) {
  switch (value) {
    case 'wrap': return 'CSS_WRAP';
    case 'nowrap': return 'CSS_NOWRAP';
  }
}

function flexDirectionValue(value) {
  switch (value) {
    case 'row': return 'CSS_FLEX_DIRECTION_ROW';
    case 'row-reverse': return 'CSS_FLEX_DIRECTION_ROW_REVERSE';
    case 'column': return 'CSS_FLEX_DIRECTION_COLUMN';
    case 'column-reverse': return 'CSS_FLEX_DIRECTION_COLUMN_REVERSE';
  }
}

function justifyValue(value) {
  switch (value) {
    case 'center': return 'CSS_JUSTIFY_CENTER';
    case 'space-around': return 'CSS_JUSTIFY_SPACE_AROUND';
    case 'space-between': return 'CSS_JUSTIFY_SPACE_BETWEEN';
    case 'flex-start': return 'CSS_JUSTIFY_FLEX_START';
    case 'flex-end': return 'CSS_JUSTIFY_FLEX_END';
  }
}

function positionValue(value) {
  switch (value) {
    case 'absolute': return 'CSS_POSITION_ABSOLUTE';
    default: return 'CSS_POSITION_RELATIVE'
  }
}

function directionValue(value) {
  switch (value) {
    case 'ltr': return 'CSS_DIRECTION_LTR';
    case 'rtl': return 'CSS_DIRECTION_RTL';
    case 'inherit': return 'CSS_DIRECTION_INHERIT';
  }
}

function alignValue(value) {
  switch (value) {
    case 'auto': return 'CSS_ALIGN_AUTO';
    case 'center': return 'CSS_ALIGN_CENTER';
    case 'stretch': return 'CSS_ALIGN_STRETCH';
    case 'flex-start': return 'CSS_ALIGN_FLEX_START';
    case 'flex-end': return 'CSS_ALIGN_FLEX_END';
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

function printLines(lines) {
  console.log(lines.map(function(value) {
    return value + '\n';
  }).reduce(function(prev, curr) {
    return prev + curr;
  }, ''));
}

function calculateTree(rootDOMNode) {
  var rootLayout = [];

  for (var i = 0; i < rootDOMNode.children.length; i++) {
    var childDOMNode = rootDOMNode.children[i];
    if (childDOMNode.id === 'default') {
      continue;
    }
    rootLayout.push({
      name: childDOMNode.id !== '' ? childDOMNode.id : 'INSERT_NAME_HERE',
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

 <div id="wrap_column" style="height: 100px; flex-wrap: wrap">
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
*/
