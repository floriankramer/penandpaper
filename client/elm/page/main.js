(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.0/optimize for better performance and smaller assets.');


var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




/**/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var author$project$Main$ModeCreateToken = {$: 'ModeCreateToken'};
var author$project$Main$None = {$: 'None'};
var author$project$Main$InitSession = function (a) {
	return {$: 'InitSession', a: a};
};
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$EQ = {$: 'EQ'};
var elm$core$Basics$GT = {$: 'GT'};
var elm$core$Basics$LT = {$: 'LT'};
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$List$cons = _List_cons;
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0.a;
	return elm$core$Dict$keys(dict);
};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var elm$core$Basics$False = {$: 'False'};
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var elm$core$Maybe$Nothing = {$: 'Nothing'};
var elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var elm$core$Basics$True = {$: 'True'};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 'Nothing') {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			elm$core$List$foldl,
			F2(
				function (_n0, obj) {
					var k = _n0.a;
					var v = _n0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var elm$json$Json$Encode$string = _Json_wrap;
var author$project$Main$encodePacket = F2(
	function (uid, p) {
		switch (p.$) {
			case 'CreateToken':
				var v = p.a;
				return elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							elm$json$Json$Encode$string('CreateToken')),
							_Utils_Tuple2(
							'uid',
							elm$json$Json$Encode$string(uid)),
							_Utils_Tuple2('data', v)
						]));
			case 'DeleteToken':
				var v = p.a;
				return elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							elm$json$Json$Encode$string('DeleteToken')),
							_Utils_Tuple2(
							'uid',
							elm$json$Json$Encode$string(uid)),
							_Utils_Tuple2('data', v)
						]));
			case 'MoveToken':
				var v = p.a;
				return elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							elm$json$Json$Encode$string('MoveToken')),
							_Utils_Tuple2(
							'uid',
							elm$json$Json$Encode$string(uid)),
							_Utils_Tuple2('data', v)
						]));
			case 'Init':
				return elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							elm$json$Json$Encode$string('unsupported'))
						]));
			case 'Chat':
				var v = p.a;
				return elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							elm$json$Json$Encode$string('Chat')),
							_Utils_Tuple2(
							'uid',
							elm$json$Json$Encode$string(uid)),
							_Utils_Tuple2('data', v)
						]));
			case 'CreateDoodadLine':
				var v = p.a;
				return elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							elm$json$Json$Encode$string('CreateDoodadLine')),
							_Utils_Tuple2(
							'uid',
							elm$json$Json$Encode$string(uid)),
							_Utils_Tuple2('data', v)
						]));
			case 'ClearDoodads':
				var v = p.a;
				return elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							elm$json$Json$Encode$string('ClearDoodads')),
							_Utils_Tuple2(
							'uid',
							elm$json$Json$Encode$string(uid)),
							_Utils_Tuple2('data', v)
						]));
			case 'ClearTokens':
				var v = p.a;
				return elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							elm$json$Json$Encode$string('ClearTokens')),
							_Utils_Tuple2(
							'uid',
							elm$json$Json$Encode$string(uid)),
							_Utils_Tuple2('data', v)
						]));
			case 'TokenToggleFoe':
				var v = p.a;
				return elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							elm$json$Json$Encode$string('TokenToggleFoe')),
							_Utils_Tuple2(
							'uid',
							elm$json$Json$Encode$string(uid)),
							_Utils_Tuple2('data', v)
						]));
			case 'InitSession':
				var v = p.a;
				return elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							elm$json$Json$Encode$string('InitSession')),
							_Utils_Tuple2(
							'uid',
							elm$json$Json$Encode$string(uid)),
							_Utils_Tuple2('data', v)
						]));
			case 'SetUsername':
				var v = p.a;
				return elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							elm$json$Json$Encode$string('SetUsername')),
							_Utils_Tuple2(
							'uid',
							elm$json$Json$Encode$string(uid)),
							_Utils_Tuple2('data', v)
						]));
			default:
				return elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							elm$json$Json$Encode$string('Error'))
						]));
		}
	});
var author$project$Main$encodeInitSession = F2(
	function (uid, cc) {
		var val = elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'uid',
					elm$json$Json$Encode$string(cc.uid))
				]));
		var packet = author$project$Main$InitSession(val);
		return A2(author$project$Main$encodePacket, uid, packet);
	});
var elm$core$Basics$identity = function (x) {
	return x;
};
var author$project$Main$wsSend = _Platform_outgoingPort('wsSend', elm$core$Basics$identity);
var elm$core$Basics$negate = function (n) {
	return -n;
};
var author$project$Main$init = function (_n0) {
	var w = _n0.a;
	var h = _n0.b;
	var uid = _n0.c;
	return _Utils_Tuple2(
		{
			action: author$project$Main$None,
			chat: _List_fromArray(
				[
					_Utils_Tuple2('Server', 'Welcome to GOATS ROCK!')
				]),
			chatText: '',
			createMode: author$project$Main$ModeCreateToken,
			doodads: _List_Nil,
			id: 128,
			mouse: {x: 0, y: 0},
			nextId: 0,
			selected: -1,
			sentMessagePos: 0,
			sentMessages: elm$core$Array$empty,
			tokens: _List_Nil,
			uid: uid,
			user: {id: 0},
			username: '',
			usernameSet: true,
			view: {height: 8, x: 0, y: 0},
			window: {height: h, width: w}
		},
		author$project$Main$wsSend(
			A2(
				author$project$Main$encodeInitSession,
				uid,
				{uid: uid})));
};
var author$project$Main$OnResize = function (a) {
	return {$: 'OnResize', a: a};
};
var author$project$Main$onResize = F2(
	function (w, h) {
		return author$project$Main$OnResize(
			_Utils_Tuple2(w, h));
	});
var author$project$Main$MsgShowError = function (a) {
	return {$: 'MsgShowError', a: a};
};
var author$project$Main$RawPacket = F2(
	function (t, d) {
		return {d: d, t: t};
	});
var author$project$Main$Chat = function (a) {
	return {$: 'Chat', a: a};
};
var author$project$Main$ClearDoodads = function (a) {
	return {$: 'ClearDoodads', a: a};
};
var author$project$Main$ClearTokens = function (a) {
	return {$: 'ClearTokens', a: a};
};
var author$project$Main$CreateDoodadLine = function (a) {
	return {$: 'CreateDoodadLine', a: a};
};
var author$project$Main$CreateToken = function (a) {
	return {$: 'CreateToken', a: a};
};
var author$project$Main$DeleteToken = function (a) {
	return {$: 'DeleteToken', a: a};
};
var author$project$Main$Init = function (a) {
	return {$: 'Init', a: a};
};
var author$project$Main$MoveToken = function (a) {
	return {$: 'MoveToken', a: a};
};
var author$project$Main$Session = function (a) {
	return {$: 'Session', a: a};
};
var author$project$Main$TokenToggleFoe = function (a) {
	return {$: 'TokenToggleFoe', a: a};
};
var author$project$Main$rawPacketToPacket = function (rawPacket) {
	return (rawPacket.t === 'CreateToken') ? elm$core$Result$Ok(
		author$project$Main$CreateToken(rawPacket.d)) : ((rawPacket.t === 'DeleteToken') ? elm$core$Result$Ok(
		author$project$Main$DeleteToken(rawPacket.d)) : ((rawPacket.t === 'MoveToken') ? elm$core$Result$Ok(
		author$project$Main$MoveToken(rawPacket.d)) : ((rawPacket.t === 'Init') ? elm$core$Result$Ok(
		author$project$Main$Init(rawPacket.d)) : ((rawPacket.t === 'Chat') ? elm$core$Result$Ok(
		author$project$Main$Chat(rawPacket.d)) : ((rawPacket.t === 'CreateDoodadLine') ? elm$core$Result$Ok(
		author$project$Main$CreateDoodadLine(rawPacket.d)) : ((rawPacket.t === 'ClearDoodads') ? elm$core$Result$Ok(
		author$project$Main$ClearDoodads(rawPacket.d)) : ((rawPacket.t === 'ClearTokens') ? elm$core$Result$Ok(
		author$project$Main$ClearTokens(rawPacket.d)) : ((rawPacket.t === 'TokenToggleFoe') ? elm$core$Result$Ok(
		author$project$Main$TokenToggleFoe(rawPacket.d)) : ((rawPacket.t === 'Session') ? elm$core$Result$Ok(
		author$project$Main$Session(rawPacket.d)) : elm$core$Result$Err('Unknown packet type ' + rawPacket.t))))))))));
};
var elm$json$Json$Decode$decodeValue = _Json_run;
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$json$Json$Decode$string = _Json_decodeString;
var elm$json$Json$Decode$value = _Json_decodeValue;
var author$project$Main$decodePacket = function (v) {
	var d = A3(
		elm$json$Json$Decode$map2,
		author$project$Main$RawPacket,
		A2(elm$json$Json$Decode$field, 'type', elm$json$Json$Decode$string),
		A2(elm$json$Json$Decode$field, 'data', elm$json$Json$Decode$value));
	var rawPacket = A2(elm$json$Json$Decode$decodeValue, d, v);
	if (rawPacket.$ === 'Ok') {
		var r = rawPacket.a;
		return author$project$Main$rawPacketToPacket(r);
	} else {
		var e = rawPacket.a;
		return elm$core$Result$Err(
			elm$json$Json$Decode$errorToString(e));
	}
};
var author$project$Main$MsgChat = F2(
	function (a, b) {
		return {$: 'MsgChat', a: a, b: b};
	});
var author$project$Main$PacketChat = F2(
	function (sender, message) {
		return {message: message, sender: sender};
	});
var author$project$Main$decodeChat = function (v) {
	var d = A3(
		elm$json$Json$Decode$map2,
		author$project$Main$PacketChat,
		A2(elm$json$Json$Decode$field, 'sender', elm$json$Json$Decode$string),
		A2(elm$json$Json$Decode$field, 'message', elm$json$Json$Decode$string));
	var cc = A2(elm$json$Json$Decode$decodeValue, d, v);
	if (cc.$ === 'Ok') {
		var p = cc.a;
		return elm$core$Result$Ok(p);
	} else {
		var e = cc.a;
		return elm$core$Result$Err(
			elm$json$Json$Decode$errorToString(e));
	}
};
var author$project$Main$onChat = function (v) {
	var p = author$project$Main$decodeChat(v);
	if (p.$ === 'Ok') {
		var o = p.a;
		return A2(author$project$Main$MsgChat, o.sender, o.message);
	} else {
		var e = p.a;
		return author$project$Main$MsgShowError(e);
	}
};
var author$project$Main$MsgClearDoodads = {$: 'MsgClearDoodads'};
var author$project$Main$PacketClearDoodads = {};
var author$project$Main$decodeClearDoodads = function (_n0) {
	return elm$core$Result$Ok(author$project$Main$PacketClearDoodads);
};
var author$project$Main$onClearDoodads = function (v) {
	var p = author$project$Main$decodeClearDoodads(v);
	if (p.$ === 'Ok') {
		var o = p.a;
		return author$project$Main$MsgClearDoodads;
	} else {
		var e = p.a;
		return author$project$Main$MsgShowError(e);
	}
};
var author$project$Main$MsgClearTokens = {$: 'MsgClearTokens'};
var author$project$Main$PacketClearTokens = {};
var author$project$Main$decodeClearTokens = function (_n0) {
	return elm$core$Result$Ok(author$project$Main$PacketClearTokens);
};
var author$project$Main$onClearTokens = function (v) {
	var p = author$project$Main$decodeClearTokens(v);
	if (p.$ === 'Ok') {
		var o = p.a;
		return author$project$Main$MsgClearTokens;
	} else {
		var e = p.a;
		return author$project$Main$MsgShowError(e);
	}
};
var author$project$Main$MsgCreateDoodadLine = function (a) {
	return {$: 'MsgCreateDoodadLine', a: a};
};
var author$project$Main$PacketCreateDoodadLine = F4(
	function (sx, sy, ex, ey) {
		return {ex: ex, ey: ey, sx: sx, sy: sy};
	});
var elm$json$Json$Decode$float = _Json_decodeFloat;
var elm$json$Json$Decode$map4 = _Json_map4;
var author$project$Main$decodeCreateDoodadLine = function (v) {
	var d = A5(
		elm$json$Json$Decode$map4,
		author$project$Main$PacketCreateDoodadLine,
		A2(elm$json$Json$Decode$field, 'sx', elm$json$Json$Decode$float),
		A2(elm$json$Json$Decode$field, 'sy', elm$json$Json$Decode$float),
		A2(elm$json$Json$Decode$field, 'ex', elm$json$Json$Decode$float),
		A2(elm$json$Json$Decode$field, 'ey', elm$json$Json$Decode$float));
	var cc = A2(elm$json$Json$Decode$decodeValue, d, v);
	if (cc.$ === 'Ok') {
		var p = cc.a;
		return elm$core$Result$Ok(p);
	} else {
		var e = cc.a;
		return elm$core$Result$Err(
			elm$json$Json$Decode$errorToString(e));
	}
};
var author$project$Main$onCreateDoodadLine = function (v) {
	var p = author$project$Main$decodeCreateDoodadLine(v);
	if (p.$ === 'Ok') {
		var o = p.a;
		return author$project$Main$MsgCreateDoodadLine(o);
	} else {
		var e = p.a;
		return author$project$Main$MsgShowError(e);
	}
};
var author$project$Main$Create = function (a) {
	return {$: 'Create', a: a};
};
var author$project$Main$PacketCreateToken = F2(
	function (x, y) {
		return {x: x, y: y};
	});
var author$project$Main$decodeCreateToken = function (v) {
	var d = A3(
		elm$json$Json$Decode$map2,
		author$project$Main$PacketCreateToken,
		A2(elm$json$Json$Decode$field, 'x', elm$json$Json$Decode$float),
		A2(elm$json$Json$Decode$field, 'y', elm$json$Json$Decode$float));
	var cc = A2(elm$json$Json$Decode$decodeValue, d, v);
	if (cc.$ === 'Ok') {
		var p = cc.a;
		return elm$core$Result$Ok(p);
	} else {
		var e = cc.a;
		return elm$core$Result$Err(
			elm$json$Json$Decode$errorToString(e));
	}
};
var author$project$Main$onCreateToken = function (v) {
	var p = author$project$Main$decodeCreateToken(v);
	if (p.$ === 'Ok') {
		var o = p.a;
		return author$project$Main$Create(
			_Utils_Tuple2(o.x, o.y));
	} else {
		var e = p.a;
		return author$project$Main$MsgShowError(e);
	}
};
var author$project$Main$Destroy = function (a) {
	return {$: 'Destroy', a: a};
};
var author$project$Main$PacketDeleteToken = function (id) {
	return {id: id};
};
var elm$json$Json$Decode$int = _Json_decodeInt;
var elm$json$Json$Decode$map = _Json_map1;
var author$project$Main$decodeDeleteToken = function (v) {
	var d = A2(
		elm$json$Json$Decode$map,
		author$project$Main$PacketDeleteToken,
		A2(elm$json$Json$Decode$field, 'id', elm$json$Json$Decode$int));
	var cc = A2(elm$json$Json$Decode$decodeValue, d, v);
	if (cc.$ === 'Ok') {
		var p = cc.a;
		return elm$core$Result$Ok(p);
	} else {
		var e = cc.a;
		return elm$core$Result$Err(
			elm$json$Json$Decode$errorToString(e));
	}
};
var author$project$Main$onDeleteToken = function (v) {
	var p = author$project$Main$decodeDeleteToken(v);
	if (p.$ === 'Ok') {
		var o = p.a;
		return author$project$Main$Destroy(o.id);
	} else {
		var e = p.a;
		return author$project$Main$MsgShowError(e);
	}
};
var author$project$Main$MsgInit = function (a) {
	return {$: 'MsgInit', a: a};
};
var author$project$Main$InitDoodad = F6(
	function (id, doodadType, sx, sy, ex, ey) {
		return {doodadType: doodadType, ex: ex, ey: ey, id: id, sx: sx, sy: sy};
	});
var author$project$Main$InitToken = F8(
	function (id, x, y, radius, r, g, b, foe) {
		return {b: b, foe: foe, g: g, id: id, r: r, radius: radius, x: x, y: y};
	});
var author$project$Main$PacketInit = F4(
	function (tokens, doodads, nextColor, nextId) {
		return {doodads: doodads, nextColor: nextColor, nextId: nextId, tokens: tokens};
	});
var elm$json$Json$Decode$bool = _Json_decodeBool;
var elm$json$Json$Decode$list = _Json_decodeList;
var elm$json$Json$Decode$map6 = _Json_map6;
var elm$json$Json$Decode$map8 = _Json_map8;
var author$project$Main$decodeInit = function (v) {
	var d = A5(
		elm$json$Json$Decode$map4,
		author$project$Main$PacketInit,
		A2(
			elm$json$Json$Decode$field,
			'tokens',
			elm$json$Json$Decode$list(
				A9(
					elm$json$Json$Decode$map8,
					author$project$Main$InitToken,
					A2(elm$json$Json$Decode$field, 'id', elm$json$Json$Decode$int),
					A2(elm$json$Json$Decode$field, 'x', elm$json$Json$Decode$float),
					A2(elm$json$Json$Decode$field, 'y', elm$json$Json$Decode$float),
					A2(elm$json$Json$Decode$field, 'radius', elm$json$Json$Decode$float),
					A2(elm$json$Json$Decode$field, 'r', elm$json$Json$Decode$float),
					A2(elm$json$Json$Decode$field, 'g', elm$json$Json$Decode$float),
					A2(elm$json$Json$Decode$field, 'b', elm$json$Json$Decode$float),
					A2(elm$json$Json$Decode$field, 'foe', elm$json$Json$Decode$bool)))),
		A2(
			elm$json$Json$Decode$field,
			'doodads',
			elm$json$Json$Decode$list(
				A7(
					elm$json$Json$Decode$map6,
					author$project$Main$InitDoodad,
					A2(elm$json$Json$Decode$field, 'id', elm$json$Json$Decode$int),
					A2(elm$json$Json$Decode$field, 'type', elm$json$Json$Decode$string),
					A2(elm$json$Json$Decode$field, 'sx', elm$json$Json$Decode$float),
					A2(elm$json$Json$Decode$field, 'sy', elm$json$Json$Decode$float),
					A2(elm$json$Json$Decode$field, 'ex', elm$json$Json$Decode$float),
					A2(elm$json$Json$Decode$field, 'ey', elm$json$Json$Decode$float)))),
		A2(elm$json$Json$Decode$field, 'nextColor', elm$json$Json$Decode$int),
		A2(elm$json$Json$Decode$field, 'nextId', elm$json$Json$Decode$int));
	var cc = A2(elm$json$Json$Decode$decodeValue, d, v);
	if (cc.$ === 'Ok') {
		var p = cc.a;
		return elm$core$Result$Ok(p);
	} else {
		var e = cc.a;
		return elm$core$Result$Err(
			elm$json$Json$Decode$errorToString(e));
	}
};
var author$project$Main$onInit = function (v) {
	var p = author$project$Main$decodeInit(v);
	if (p.$ === 'Ok') {
		var o = p.a;
		return author$project$Main$MsgInit(o);
	} else {
		var e = p.a;
		return author$project$Main$MsgShowError(e);
	}
};
var author$project$Main$Move = F2(
	function (a, b) {
		return {$: 'Move', a: a, b: b};
	});
var author$project$Main$PacketMoveToken = F3(
	function (id, x, y) {
		return {id: id, x: x, y: y};
	});
var elm$json$Json$Decode$map3 = _Json_map3;
var author$project$Main$decodeMoveToken = function (v) {
	var d = A4(
		elm$json$Json$Decode$map3,
		author$project$Main$PacketMoveToken,
		A2(elm$json$Json$Decode$field, 'id', elm$json$Json$Decode$int),
		A2(elm$json$Json$Decode$field, 'x', elm$json$Json$Decode$float),
		A2(elm$json$Json$Decode$field, 'y', elm$json$Json$Decode$float));
	var cc = A2(elm$json$Json$Decode$decodeValue, d, v);
	if (cc.$ === 'Ok') {
		var p = cc.a;
		return elm$core$Result$Ok(p);
	} else {
		var e = cc.a;
		return elm$core$Result$Err(
			elm$json$Json$Decode$errorToString(e));
	}
};
var author$project$Main$onMoveToken = function (v) {
	var p = author$project$Main$decodeMoveToken(v);
	if (p.$ === 'Ok') {
		var o = p.a;
		return A2(
			author$project$Main$Move,
			o.id,
			_Utils_Tuple2(o.x, o.y));
	} else {
		var e = p.a;
		return author$project$Main$MsgShowError(e);
	}
};
var author$project$Main$MsgRestoreSession = function (a) {
	return {$: 'MsgRestoreSession', a: a};
};
var author$project$Main$PacketSession = F2(
	function (id, name) {
		return {id: id, name: name};
	});
var author$project$Main$decodeSession = function (v) {
	var d = A3(
		elm$json$Json$Decode$map2,
		author$project$Main$PacketSession,
		A2(elm$json$Json$Decode$field, 'id', elm$json$Json$Decode$int),
		A2(elm$json$Json$Decode$field, 'name', elm$json$Json$Decode$string));
	var cc = A2(elm$json$Json$Decode$decodeValue, d, v);
	if (cc.$ === 'Ok') {
		var p = cc.a;
		return elm$core$Result$Ok(p);
	} else {
		var e = cc.a;
		return elm$core$Result$Err(
			elm$json$Json$Decode$errorToString(e));
	}
};
var author$project$Main$onSession = function (v) {
	var p = author$project$Main$decodeSession(v);
	if (p.$ === 'Ok') {
		var o = p.a;
		return author$project$Main$MsgRestoreSession(o);
	} else {
		var e = p.a;
		return author$project$Main$MsgShowError(e);
	}
};
var author$project$Main$MsgToggleFoe = function (a) {
	return {$: 'MsgToggleFoe', a: a};
};
var author$project$Main$PacketTokenToggleFoe = function (id) {
	return {id: id};
};
var author$project$Main$decodeTokenToggleFoe = function (v) {
	var d = A2(
		elm$json$Json$Decode$map,
		author$project$Main$PacketTokenToggleFoe,
		A2(elm$json$Json$Decode$field, 'id', elm$json$Json$Decode$int));
	var cc = A2(elm$json$Json$Decode$decodeValue, d, v);
	if (cc.$ === 'Ok') {
		var p = cc.a;
		return elm$core$Result$Ok(p);
	} else {
		var e = cc.a;
		return elm$core$Result$Err(
			elm$json$Json$Decode$errorToString(e));
	}
};
var author$project$Main$onTokenToggleFoe = function (v) {
	var p = author$project$Main$decodeTokenToggleFoe(v);
	if (p.$ === 'Ok') {
		var o = p.a;
		return author$project$Main$MsgToggleFoe(o.id);
	} else {
		var e = p.a;
		return author$project$Main$MsgShowError(e);
	}
};
var author$project$Main$onPacket = function (p) {
	switch (p.$) {
		case 'CreateToken':
			var d = p.a;
			return author$project$Main$onCreateToken(d);
		case 'MoveToken':
			var d = p.a;
			return author$project$Main$onMoveToken(d);
		case 'DeleteToken':
			var d = p.a;
			return author$project$Main$onDeleteToken(d);
		case 'Init':
			var d = p.a;
			return author$project$Main$onInit(d);
		case 'Chat':
			var d = p.a;
			return author$project$Main$onChat(d);
		case 'CreateDoodadLine':
			var d = p.a;
			return author$project$Main$onCreateDoodadLine(d);
		case 'ClearDoodads':
			var d = p.a;
			return author$project$Main$onClearDoodads(d);
		case 'ClearTokens':
			var d = p.a;
			return author$project$Main$onClearTokens(d);
		case 'TokenToggleFoe':
			var d = p.a;
			return author$project$Main$onTokenToggleFoe(d);
		case 'InitSession':
			var d = p.a;
			return author$project$Main$MsgShowError('Unexpected init session packet received.');
		case 'Session':
			var d = p.a;
			return author$project$Main$onSession(d);
		default:
			var d = p.a;
			return author$project$Main$MsgShowError('Unexpected init session packet received.');
	}
};
var author$project$Main$onWsReceive = function (v) {
	var p = author$project$Main$decodePacket(v);
	if (p.$ === 'Ok') {
		var o = p.a;
		return author$project$Main$onPacket(o);
	} else {
		var e = p.a;
		return author$project$Main$MsgShowError(e);
	}
};
var author$project$Main$wsReceive = _Platform_incomingPort('wsReceive', elm$json$Json$Decode$value);
var elm$browser$Browser$Events$Window = {$: 'Window'};
var elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$browser$Browser$Events$init = elm$core$Task$succeed(
	A2(elm$browser$Browser$Events$State, _List_Nil, elm$core$Dict$empty));
var elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0.a;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var elm$core$Task$init = elm$core$Task$succeed(_Utils_Tuple0);
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0.a;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return _Utils_Tuple0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(_Utils_Tuple0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0.a;
		return elm$core$Task$Perform(
			A2(elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(elm$core$Task$map, toMessage, task)));
	});
var elm$json$Json$Decode$succeed = _Json_succeed;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var elm$core$String$length = _String_length;
var elm$core$String$slice = _String_slice;
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$startsWith = _String_startsWith;
var elm$url$Url$Http = {$: 'Http'};
var elm$url$Url$Https = {$: 'Https'};
var elm$core$String$indexes = _String_indexes;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$core$String$toInt = _String_toInt;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 'Nothing') {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Http,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Https,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$Events$spawn = F3(
	function (router, key, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						elm$core$Platform$sendToSelf,
						router,
						A2(elm$browser$Browser$Events$Event, key, event));
				}));
	});
var elm$core$Dict$Black = {$: 'Black'};
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$Red = {$: 'Red'};
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1.$) {
				case 'LT':
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$fromList = function (assocs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, dict) {
				var key = _n0.a;
				var value = _n0.b;
				return A3(elm$core$Dict$insert, key, value, dict);
			}),
		elm$core$Dict$empty,
		assocs);
};
var elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _n0) {
				stepState:
				while (true) {
					var list = _n0.a;
					var result = _n0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _n2 = list.a;
						var lKey = _n2.a;
						var lValue = _n2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_n0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_n0 = $temp$_n0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _n3 = A3(
			elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _n3.a;
		var intermediateResult = _n3.b;
		return A3(
			elm$core$List$foldl,
			F2(
				function (_n4, result) {
					var k = _n4.a;
					var v = _n4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3(elm$core$Dict$foldl, elm$core$Dict$insert, t2, t1);
	});
var elm$core$Process$kill = _Scheduler_kill;
var elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _n6) {
				var deads = _n6.a;
				var lives = _n6.b;
				var news = _n6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						elm$core$List$cons,
						A3(elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_n4, pid, _n5) {
				var deads = _n5.a;
				var lives = _n5.b;
				var news = _n5.c;
				return _Utils_Tuple3(
					A2(elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _n2, _n3) {
				var deads = _n3.a;
				var lives = _n3.b;
				var news = _n3.c;
				return _Utils_Tuple3(
					deads,
					A3(elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2(elm$core$List$map, elm$browser$Browser$Events$addKey, subs);
		var _n0 = A6(
			elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, elm$core$Dict$empty, _List_Nil));
		var deadPids = _n0.a;
		var livePids = _n0.b;
		var makeNewPids = _n0.c;
		return A2(
			elm$core$Task$andThen,
			function (pids) {
				return elm$core$Task$succeed(
					A2(
						elm$browser$Browser$Events$State,
						newSubs,
						A2(
							elm$core$Dict$union,
							livePids,
							elm$core$Dict$fromList(pids))));
			},
			A2(
				elm$core$Task$andThen,
				function (_n1) {
					return elm$core$Task$sequence(makeNewPids);
				},
				elm$core$Task$sequence(
					A2(elm$core$List$map, elm$core$Process$kill, deadPids))));
	});
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (_n0.$ === 'Just') {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _n0, state) {
		var key = _n0.key;
		var event = _n0.event;
		var toMessage = function (_n2) {
			var subKey = _n2.a;
			var _n3 = _n2.b;
			var node = _n3.a;
			var name = _n3.b;
			var decoder = _n3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : elm$core$Maybe$Nothing;
		};
		var messages = A2(elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			elm$core$Task$andThen,
			function (_n1) {
				return elm$core$Task$succeed(state);
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Platform$sendToApp(router),
					messages)));
	});
var elm$browser$Browser$Events$subMap = F2(
	function (func, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var decoder = _n0.c;
		return A3(
			elm$browser$Browser$Events$MySub,
			node,
			name,
			A2(elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager(elm$browser$Browser$Events$init, elm$browser$Browser$Events$onEffects, elm$browser$Browser$Events$onSelfMsg, 0, elm$browser$Browser$Events$subMap);
var elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return elm$browser$Browser$Events$subscription(
			A3(elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		elm$browser$Browser$Events$on,
		elm$browser$Browser$Events$Window,
		'resize',
		A2(
			elm$json$Json$Decode$field,
			'target',
			A3(
				elm$json$Json$Decode$map2,
				func,
				A2(elm$json$Json$Decode$field, 'innerWidth', elm$json$Json$Decode$int),
				A2(elm$json$Json$Decode$field, 'innerHeight', elm$json$Json$Decode$int))));
};
var elm$core$Platform$Sub$batch = _Platform_batch;
var author$project$Main$subscriptions = function (_n0) {
	return elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				elm$browser$Browser$Events$onResize(author$project$Main$onResize),
				author$project$Main$wsReceive(author$project$Main$onWsReceive)
			]));
};
var author$project$Main$PacketSetUsername = function (name) {
	return {name: name};
};
var author$project$Main$SetUsername = function (a) {
	return {$: 'SetUsername', a: a};
};
var author$project$Main$encodeSetUsername = F2(
	function (uid, cc) {
		var val = elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'name',
					elm$json$Json$Encode$string(cc.name))
				]));
		var packet = author$project$Main$SetUsername(val);
		return A2(author$project$Main$encodePacket, uid, packet);
	});
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Down = {$: 'Down'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Enter = {$: 'Enter'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Up = {$: 'Up'};
var author$project$Main$encodeChat = F2(
	function (uid, cc) {
		var val = elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'sender',
					elm$json$Json$Encode$string(cc.sender)),
					_Utils_Tuple2(
					'message',
					elm$json$Json$Encode$string(cc.message))
				]));
		var packet = author$project$Main$Chat(val);
		return A2(author$project$Main$encodePacket, uid, packet);
	});
var elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, list);
			var jsArray = _n0.a;
			var remainingItems = _n0.b;
			if (_Utils_cmp(
				elm$core$Elm$JsArray$length(jsArray),
				elm$core$Array$branchFactor) < 0) {
				return A2(
					elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					elm$core$List$cons,
					elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return elm$core$Array$empty;
	} else {
		return A3(elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var elm$core$Array$bitMask = 4294967295 >>> (32 - elm$core$Array$shiftStep);
var elm$core$Bitwise$and = _Bitwise_and;
var elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = elm$core$Array$bitMask & (index >>> shift);
			var _n0 = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_n0.$ === 'SubTree') {
				var subTree = _n0.a;
				var $temp$shift = shift - elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _n0.a;
				return A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, values);
			}
		}
	});
var elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var elm$core$Basics$ge = _Utils_ge;
var elm$core$Array$get = F2(
	function (index, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			elm$core$Array$tailIndex(len)) > -1) ? elm$core$Maybe$Just(
			A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, tail)) : elm$core$Maybe$Just(
			A3(elm$core$Array$getHelp, startShift, index, tree)));
	});
var elm$core$Array$length = function (_n0) {
	var len = _n0.a;
	return len;
};
var elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2(elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var elm$core$List$takeTailRec = F2(
	function (n, list) {
		return elm$core$List$reverse(
			A3(elm$core$List$takeReverse, n, list, _List_Nil));
	});
var elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _n0 = _Utils_Tuple2(n, list);
			_n0$1:
			while (true) {
				_n0$5:
				while (true) {
					if (!_n0.b.b) {
						return list;
					} else {
						if (_n0.b.b.b) {
							switch (_n0.a) {
								case 1:
									break _n0$1;
								case 2:
									var _n2 = _n0.b;
									var x = _n2.a;
									var _n3 = _n2.b;
									var y = _n3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_n0.b.b.b.b) {
										var _n4 = _n0.b;
										var x = _n4.a;
										var _n5 = _n4.b;
										var y = _n5.a;
										var _n6 = _n5.b;
										var z = _n6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _n0$5;
									}
								default:
									if (_n0.b.b.b.b && _n0.b.b.b.b.b) {
										var _n7 = _n0.b;
										var x = _n7.a;
										var _n8 = _n7.b;
										var y = _n8.a;
										var _n9 = _n8.b;
										var z = _n9.a;
										var _n10 = _n9.b;
										var w = _n10.a;
										var tl = _n10.b;
										return (ctr > 1000) ? A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A2(elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A3(elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _n0$5;
									}
							}
						} else {
							if (_n0.a === 1) {
								break _n0$1;
							} else {
								break _n0$5;
							}
						}
					}
				}
				return list;
			}
			var _n1 = _n0.b;
			var x = _n1.a;
			return _List_fromArray(
				[x]);
		}
	});
var elm$core$List$take = F2(
	function (n, list) {
		return A3(elm$core$List$takeFast, 0, n, list);
	});
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$Main$onChatKeyDown = F2(
	function (event, model) {
		if (_Utils_eq(event.keyCode, SwiftsNamesake$proper_keyboard$Keyboard$Key$Enter)) {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						chatText: '',
						sentMessagePos: 0,
						sentMessages: elm$core$Array$fromList(
							A2(
								elm$core$List$take,
								25,
								A2(
									elm$core$List$cons,
									model.chatText,
									elm$core$Array$toList(model.sentMessages))))
					}),
				author$project$Main$wsSend(
					A2(
						author$project$Main$encodeChat,
						model.uid,
						{message: model.chatText, sender: model.username})));
		} else {
			if (_Utils_eq(event.keyCode, SwiftsNamesake$proper_keyboard$Keyboard$Key$Up)) {
				var tm = A2(elm$core$Array$get, model.sentMessagePos, model.sentMessages);
				var t = A2(elm$core$Maybe$withDefault, model.chatText, tm);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							chatText: t,
							sentMessagePos: A2(
								elm$core$Basics$min,
								model.sentMessagePos + 1,
								elm$core$Array$length(model.sentMessages))
						}),
					elm$core$Platform$Cmd$none);
			} else {
				if (_Utils_eq(event.keyCode, SwiftsNamesake$proper_keyboard$Keyboard$Key$Down)) {
					var tm = A2(elm$core$Array$get, model.sentMessagePos - 2, model.sentMessages);
					var t = A2(elm$core$Maybe$withDefault, '', tm);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								chatText: t,
								sentMessagePos: A2(elm$core$Basics$max, model.sentMessagePos - 1, 0)
							}),
						elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				}
			}
		}
	});
var author$project$Main$Token = function (a) {
	return {$: 'Token', a: a};
};
var avh4$elm_color$Color$RgbaSpace = F4(
	function (a, b, c, d) {
		return {$: 'RgbaSpace', a: a, b: b, c: c, d: d};
	});
var avh4$elm_color$Color$scaleFrom255 = function (c) {
	return c / 255;
};
var avh4$elm_color$Color$rgb255 = F3(
	function (r, g, b) {
		return A4(
			avh4$elm_color$Color$RgbaSpace,
			avh4$elm_color$Color$scaleFrom255(r),
			avh4$elm_color$Color$scaleFrom255(g),
			avh4$elm_color$Color$scaleFrom255(b),
			1.0);
	});
var author$project$Main$baseColors = elm$core$Array$fromList(
	_List_fromArray(
		[
			A3(avh4$elm_color$Color$rgb255, 240, 50, 50),
			A3(avh4$elm_color$Color$rgb255, 176, 30, 90),
			A3(avh4$elm_color$Color$rgb255, 201, 20, 201),
			A3(avh4$elm_color$Color$rgb255, 120, 61, 196),
			A3(avh4$elm_color$Color$rgb255, 24, 100, 171),
			A3(avh4$elm_color$Color$rgb255, 24, 172, 171),
			A3(avh4$elm_color$Color$rgb255, 8, 127, 91),
			A3(avh4$elm_color$Color$rgb255, 92, 148, 13),
			A3(avh4$elm_color$Color$rgb255, 217, 72, 15),
			A3(avh4$elm_color$Color$rgb255, 129, 96, 65)
		]));
var author$project$Main$numBaseColors = 10;
var avh4$elm_color$Color$rgb = F3(
	function (r, g, b) {
		return A4(avh4$elm_color$Color$RgbaSpace, r, g, b, 1.0);
	});
var author$project$Main$onCreate = F2(
	function (_n0, model) {
		var x = _n0.a;
		var y = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					nextId: model.nextId + 1,
					tokens: A2(
						elm$core$List$cons,
						author$project$Main$Token(
							{
								color: A2(
									elm$core$Maybe$withDefault,
									A3(avh4$elm_color$Color$rgb, 0.7, 0, 0.7),
									A2(elm$core$Array$get, model.nextId % author$project$Main$numBaseColors, author$project$Main$baseColors)),
								foe: false,
								id: model.nextId,
								owner: 0,
								radius: 0.25,
								x: x,
								y: y
							}),
						model.tokens)
				}),
			elm$core$Platform$Cmd$none);
	});
var author$project$Main$deleteToken = F2(
	function (i, l) {
		if (!l.b) {
			return _List_Nil;
		} else {
			var h = l.a;
			var t = l.b;
			var d = h.a;
			return _Utils_eq(d.id, i) ? t : A2(
				elm$core$List$cons,
				h,
				A2(author$project$Main$deleteToken, i, t));
		}
	});
var author$project$Main$onDestroy = F2(
	function (id, model) {
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					tokens: A2(author$project$Main$deleteToken, id, model.tokens)
				}),
			elm$core$Platform$Cmd$none);
	});
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Delete = {$: 'Delete'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$F = {$: 'F'};
var elm$json$Json$Encode$int = _Json_wrap;
var author$project$Main$encodeDeleteToken = F2(
	function (uid, cc) {
		var val = elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'id',
					elm$json$Json$Encode$int(cc.id))
				]));
		var packet = author$project$Main$DeleteToken(val);
		return A2(author$project$Main$encodePacket, uid, packet);
	});
var author$project$Main$encodeTokenToggleFoe = F2(
	function (uid, cc) {
		var val = elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'id',
					elm$json$Json$Encode$int(cc.id))
				]));
		var packet = author$project$Main$TokenToggleFoe(val);
		return A2(author$project$Main$encodePacket, uid, packet);
	});
var author$project$Main$onKeyDown = F2(
	function (event, model) {
		return (_Utils_eq(event.keyCode, SwiftsNamesake$proper_keyboard$Keyboard$Key$Delete) && (model.selected >= 0)) ? _Utils_Tuple2(
			_Utils_update(
				model,
				{selected: -1}),
			author$project$Main$wsSend(
				A2(
					author$project$Main$encodeDeleteToken,
					model.uid,
					{id: model.selected}))) : (_Utils_eq(event.keyCode, SwiftsNamesake$proper_keyboard$Keyboard$Key$F) ? _Utils_Tuple2(
			model,
			author$project$Main$wsSend(
				A2(
					author$project$Main$encodeTokenToggleFoe,
					model.uid,
					{id: model.selected}))) : _Utils_Tuple2(model, elm$core$Platform$Cmd$none));
	});
var author$project$Main$ActionCreateLine = function (a) {
	return {$: 'ActionCreateLine', a: a};
};
var author$project$Main$DragToken = function (a) {
	return {$: 'DragToken', a: a};
};
var author$project$Main$DragView = function (a) {
	return {$: 'DragView', a: a};
};
var author$project$Main$applyToToken = F3(
	function (f, i, l) {
		if (!l.b) {
			return _List_Nil;
		} else {
			var h = l.a;
			var t = l.b;
			var d = h.a;
			return _Utils_eq(d.id, i) ? A2(
				elm$core$List$cons,
				f(h),
				A3(author$project$Main$applyToToken, f, i, t)) : A2(
				elm$core$List$cons,
				h,
				A3(author$project$Main$applyToToken, f, i, t));
		}
	});
var elm$json$Json$Encode$float = _Json_wrap;
var author$project$Main$encodeCreateDoodadLine = F2(
	function (uid, cc) {
		var val = elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'sx',
					elm$json$Json$Encode$float(cc.sx)),
					_Utils_Tuple2(
					'sy',
					elm$json$Json$Encode$float(cc.sy)),
					_Utils_Tuple2(
					'ex',
					elm$json$Json$Encode$float(cc.ex)),
					_Utils_Tuple2(
					'ey',
					elm$json$Json$Encode$float(cc.ey))
				]));
		var packet = author$project$Main$CreateDoodadLine(val);
		return A2(author$project$Main$encodePacket, uid, packet);
	});
var author$project$Main$computeCanvasHeight = function (h) {
	return h - 48;
};
var author$project$Main$computeCanvasWidth = function (w) {
	return w - 370;
};
var author$project$Main$screenToWorld = F2(
	function (m, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		var scale = m.view.height / author$project$Main$computeCanvasHeight(m.window.height);
		return _Utils_Tuple2(
			((x - (0.5 * author$project$Main$computeCanvasWidth(m.window.width))) * scale) + m.view.x,
			((y - (0.5 * author$project$Main$computeCanvasHeight(m.window.height))) * scale) + m.view.y);
	});
var author$project$Main$tokenSetPosition = F3(
	function (x, y, t) {
		var c = t.a;
		return author$project$Main$Token(
			_Utils_update(
				c,
				{x: x, y: y}));
	});
var elm$core$Basics$pow = _Basics_pow;
var elm$core$Basics$sqrt = _Basics_sqrt;
var author$project$Main$onMouseMotion = F2(
	function (event, model) {
		var _n0 = A2(author$project$Main$screenToWorld, model, event.offsetPos);
		var x = _n0.a;
		var y = _n0.b;
		var _n1 = event.offsetPos;
		var sx = _n1.a;
		var sy = _n1.b;
		var _n2 = model.action;
		switch (_n2.$) {
			case 'DragToken':
				var d = _n2.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							action: author$project$Main$DragToken(
								{ox: d.ox, oy: d.oy, startx: d.startx, starty: d.starty, x: x - d.ox, y: y - d.oy}),
							tokens: A3(
								author$project$Main$applyToToken,
								A2(author$project$Main$tokenSetPosition, x - d.ox, y - d.oy),
								model.selected,
								model.tokens)
						}),
					elm$core$Platform$Cmd$none);
			case 'DragView':
				var d = _n2.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							action: author$project$Main$DragView(
								{lastX: sx, lastY: sy}),
							view: {height: model.view.height, x: model.view.x - (((sx - d.lastX) / model.window.height) * model.view.height), y: model.view.y - (((sy - d.lastY) / model.window.height) * model.view.height)}
						}),
					elm$core$Platform$Cmd$none);
			case 'ActionCreateLine':
				var l = _n2.a;
				var d = elm$core$Basics$sqrt(
					A2(elm$core$Basics$pow, x - l.sx, 2) + A2(elm$core$Basics$pow, y - l.sy, 2));
				return (d > 3) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{
							action: author$project$Main$ActionCreateLine(
								_Utils_update(
									l,
									{ex: x, ey: y, sx: l.ex, sy: l.ey}))
						}),
					author$project$Main$wsSend(
						A2(
							author$project$Main$encodeCreateDoodadLine,
							model.uid,
							{ex: l.ex, ey: l.ey, sx: l.sx, sy: l.sy}))) : _Utils_Tuple2(
					_Utils_update(
						model,
						{
							action: author$project$Main$ActionCreateLine(
								_Utils_update(
									l,
									{ex: x, ey: y}))
						}),
					elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							mouse: {x: x, y: y}
						}),
					elm$core$Platform$Cmd$none);
		}
	});
var author$project$Main$ChangedFocus = function (a) {
	return {$: 'ChangedFocus', a: a};
};
var author$project$Main$tokenIdAt = F3(
	function (x, y, l) {
		tokenIdAt:
		while (true) {
			if (!l.b) {
				return -1;
			} else {
				var h = l.a;
				var t = l.b;
				var c = h.a;
				if (_Utils_cmp(
					elm$core$Basics$sqrt(
						A2(elm$core$Basics$pow, x - c.x, 2) + A2(elm$core$Basics$pow, y - c.y, 2)),
					c.radius) < 0) {
					return c.id;
				} else {
					var $temp$x = x,
						$temp$y = y,
						$temp$l = t;
					x = $temp$x;
					y = $temp$y;
					l = $temp$l;
					continue tokenIdAt;
				}
			}
		}
	});
var author$project$Main$creatureIdAt = F3(
	function (x, y, l) {
		if (!l.b) {
			return -1;
		} else {
			var h = l.a;
			var t = l.b;
			var c = h.a;
			return (_Utils_cmp(
				elm$core$Basics$sqrt(
					A2(elm$core$Basics$pow, x - c.x, 2) + A2(elm$core$Basics$pow, y - c.y, 2)),
				c.radius) < 0) ? c.id : A3(author$project$Main$tokenIdAt, x, y, t);
		}
	});
var author$project$Main$encodeCreateToken = F2(
	function (uid, cc) {
		var val = elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'x',
					elm$json$Json$Encode$float(cc.x)),
					_Utils_Tuple2(
					'y',
					elm$json$Json$Encode$float(cc.y))
				]));
		var packet = author$project$Main$CreateToken(val);
		return A2(author$project$Main$encodePacket, uid, packet);
	});
var author$project$Main$getToken = F2(
	function (i, l) {
		getToken:
		while (true) {
			if (!l.b) {
				return elm$core$Maybe$Nothing;
			} else {
				var h = l.a;
				var t = l.b;
				var d = h.a;
				if (_Utils_eq(d.id, i)) {
					return elm$core$Maybe$Just(
						author$project$Main$Token(d));
				} else {
					var $temp$i = i,
						$temp$l = t;
					i = $temp$i;
					l = $temp$l;
					continue getToken;
				}
			}
		}
	});
var author$project$Main$isGm = function (model) {
	return !model.id;
};
var elm$browser$Browser$Dom$focus = _Browser_call('focus');
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$core$Task$onError = _Scheduler_onError;
var elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(
					elm$core$Task$onError,
					A2(
						elm$core$Basics$composeL,
						A2(elm$core$Basics$composeL, elm$core$Task$succeed, resultToMessage),
						elm$core$Result$Err),
					A2(
						elm$core$Task$andThen,
						A2(
							elm$core$Basics$composeL,
							A2(elm$core$Basics$composeL, elm$core$Task$succeed, resultToMessage),
							elm$core$Result$Ok),
						task))));
	});
var author$project$Main$onMousePress = F2(
	function (event, model) {
		var _n0 = A2(author$project$Main$screenToWorld, model, event.offsetPos);
		var x = _n0.a;
		var y = _n0.b;
		var _n1 = event.button;
		switch (_n1.$) {
			case 'MainButton':
				var setFocus = A2(
					elm$core$Task$attempt,
					author$project$Main$ChangedFocus,
					elm$browser$Browser$Dom$focus('canvas'));
				var s = A3(author$project$Main$creatureIdAt, x, y, model.tokens);
				var mt = A2(author$project$Main$getToken, s, model.tokens);
				var cy = function () {
					if (mt.$ === 'Just') {
						var c = mt.a;
						var cr = c.a;
						return cr.y;
					} else {
						return y;
					}
				}();
				var cx = function () {
					if (mt.$ === 'Just') {
						var c = mt.a;
						var cr = c.a;
						return cr.x;
					} else {
						return x;
					}
				}();
				var _n2 = event.offsetPos;
				var sx = _n2.a;
				var sy = _n2.b;
				var a = (s < 0) ? author$project$Main$DragView(
					{lastX: sx, lastY: sy}) : author$project$Main$DragToken(
					{ox: x - cx, oy: y - cy, startx: cx, starty: cy, x: cx, y: cy});
				if ((s < 0) && (event.keys.ctrl && author$project$Main$isGm(model))) {
					var _n3 = model.createMode;
					if (_n3.$ === 'ModeCreateToken') {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{action: author$project$Main$None, selected: -1}),
							elm$core$Platform$Cmd$batch(
								_List_fromArray(
									[
										author$project$Main$wsSend(
										A2(
											author$project$Main$encodeCreateToken,
											model.uid,
											{x: x, y: y})),
										setFocus
									])));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									action: author$project$Main$ActionCreateLine(
										{ex: x, ey: y, sx: x, sy: y}),
									selected: -1
								}),
							elm$core$Platform$Cmd$none);
					}
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{action: a, selected: s}),
						setFocus);
				}
			case 'MiddleButton':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							view: {height: 8, x: 0, y: 0}
						}),
					elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
		}
	});
var author$project$Main$encodeMoveToken = F2(
	function (uid, cc) {
		var val = elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'id',
					elm$json$Json$Encode$int(cc.id)),
					_Utils_Tuple2(
					'x',
					elm$json$Json$Encode$float(cc.x)),
					_Utils_Tuple2(
					'y',
					elm$json$Json$Encode$float(cc.y))
				]));
		var packet = author$project$Main$MoveToken(val);
		return A2(author$project$Main$encodePacket, uid, packet);
	});
var author$project$Main$onMouseRelease = F2(
	function (event, model) {
		var _n0 = A2(author$project$Main$screenToWorld, model, event.offsetPos);
		var x = _n0.a;
		var y = _n0.b;
		var _n1 = model.action;
		switch (_n1.$) {
			case 'DragToken':
				var d = _n1.a;
				return (elm$core$Basics$sqrt(
					A2(elm$core$Basics$pow, d.startx - d.x, 2) + A2(elm$core$Basics$pow, d.starty - d.y, 2)) > 0.1) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{action: author$project$Main$None}),
					author$project$Main$wsSend(
						A2(
							author$project$Main$encodeMoveToken,
							model.uid,
							{id: model.selected, x: d.x, y: d.y}))) : _Utils_Tuple2(
					_Utils_update(
						model,
						{action: author$project$Main$None}),
					author$project$Main$wsSend(
						A2(
							author$project$Main$encodeMoveToken,
							model.uid,
							{id: model.selected, x: d.startx, y: d.starty})));
			case 'ActionCreateLine':
				var l = _n1.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{action: author$project$Main$None}),
					author$project$Main$wsSend(
						A2(
							author$project$Main$encodeCreateDoodadLine,
							model.uid,
							{ex: l.ex, ey: l.ey, sx: l.sx, sy: l.sy})));
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{action: author$project$Main$None}),
					elm$core$Platform$Cmd$none);
		}
	});
var author$project$Main$onMouseWheel = F2(
	function (event, model) {
		var scrollDir = (event.deltaY > 0) ? 1 : (-1);
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					view: {height: model.view.height * (1 + (0.3 * scrollDir)), x: model.view.x, y: model.view.y}
				}),
			elm$core$Platform$Cmd$none);
	});
var author$project$Main$onMove = F3(
	function (id, _n0, model) {
		var x = _n0.a;
		var y = _n0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					tokens: A3(
						author$project$Main$applyToToken,
						A2(author$project$Main$tokenSetPosition, x, y),
						id,
						model.tokens)
				}),
			elm$core$Platform$Cmd$none);
	});
var author$project$Main$onMsgChat = F3(
	function (sender, message, model) {
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					chat: A2(
						elm$core$List$take,
						50,
						A2(
							elm$core$List$cons,
							_Utils_Tuple2(sender, message),
							model.chat))
				}),
			elm$core$Platform$Cmd$none);
	});
var author$project$Main$onMsgChatInput = F2(
	function (text, model) {
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{chatText: text}),
			elm$core$Platform$Cmd$none);
	});
var author$project$Main$onMsgClearDoodads = function (model) {
	return _Utils_Tuple2(
		_Utils_update(
			model,
			{doodads: _List_Nil}),
		elm$core$Platform$Cmd$none);
};
var author$project$Main$onMsgClearTokens = function (model) {
	return _Utils_Tuple2(
		_Utils_update(
			model,
			{tokens: _List_Nil}),
		elm$core$Platform$Cmd$none);
};
var author$project$Main$DoodadLine = function (a) {
	return {$: 'DoodadLine', a: a};
};
var author$project$Main$onMsgCreateDoodadLine = F3(
	function (_n0, _n1, model) {
		var sx = _n0.a;
		var sy = _n0.b;
		var ex = _n1.a;
		var ey = _n1.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					doodads: A2(
						elm$core$List$cons,
						author$project$Main$DoodadLine(
							{ex: ex, ey: ey, id: 0, sx: sx, sy: sy}),
						model.doodads)
				}),
			elm$core$Platform$Cmd$none);
	});
var author$project$Main$initDoodadToDoodad = function (t) {
	return author$project$Main$DoodadLine(
		{ex: t.ex, ey: t.ey, id: t.id, sx: t.sx, sy: t.sy});
};
var elm$core$Basics$round = _Basics_round;
var author$project$Main$initTokenToToken = function (t) {
	return author$project$Main$Token(
		{
			color: A3(
				avh4$elm_color$Color$rgb255,
				elm$core$Basics$round(t.r),
				elm$core$Basics$round(t.g),
				elm$core$Basics$round(t.b)),
			foe: t.foe,
			id: t.id,
			owner: 0,
			radius: t.radius,
			x: t.x,
			y: t.y
		});
};
var author$project$Main$onMsgInit = F2(
	function (d, model) {
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					doodads: A2(elm$core$List$map, author$project$Main$initDoodadToDoodad, d.doodads),
					nextId: d.nextId,
					tokens: A2(elm$core$List$map, author$project$Main$initTokenToToken, d.tokens)
				}),
			elm$core$Platform$Cmd$none);
	});
var elm$core$Basics$neq = _Utils_notEqual;
var author$project$Main$onMsgRestoreSession = F2(
	function (p, model) {
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{id: p.id, username: p.name, usernameSet: p.name !== ''}),
			elm$core$Platform$Cmd$none);
	});
var author$project$Main$encodeClearDoodads = F2(
	function (uid, cc) {
		var val = elm$json$Json$Encode$object(_List_Nil);
		var packet = author$project$Main$ClearDoodads(val);
		return A2(author$project$Main$encodePacket, uid, packet);
	});
var author$project$Main$onMsgSendClearDoodads = function (model) {
	return _Utils_Tuple2(
		model,
		author$project$Main$wsSend(
			A2(author$project$Main$encodeClearDoodads, model.uid, author$project$Main$PacketClearDoodads)));
};
var author$project$Main$encodeClearTokens = F2(
	function (uid, cc) {
		var val = elm$json$Json$Encode$object(_List_Nil);
		var packet = author$project$Main$ClearTokens(val);
		return A2(author$project$Main$encodePacket, uid, packet);
	});
var author$project$Main$onMsgSendClearTokens = function (model) {
	return _Utils_Tuple2(
		model,
		author$project$Main$wsSend(
			A2(author$project$Main$encodeClearTokens, model.uid, author$project$Main$PacketClearTokens)));
};
var elm$core$Basics$not = _Basics_not;
var author$project$Main$tokenToggleFoe = function (t) {
	var c = t.a;
	return author$project$Main$Token(
		_Utils_update(
			c,
			{foe: !c.foe}));
};
var author$project$Main$onMsgToggleFoe = F2(
	function (i, model) {
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					tokens: A3(author$project$Main$applyToToken, author$project$Main$tokenToggleFoe, i, model.tokens)
				}),
			elm$core$Platform$Cmd$none);
	});
var elm$core$Debug$log = _Debug_log;
var author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'Move':
				var id = msg.a;
				var _n1 = msg.b;
				var newx = _n1.a;
				var newy = _n1.b;
				return A3(
					author$project$Main$onMove,
					id,
					_Utils_Tuple2(newx, newy),
					model);
			case 'Create':
				var p = msg.a;
				return A2(author$project$Main$onCreate, p, model);
			case 'Destroy':
				var id = msg.a;
				return A2(author$project$Main$onDestroy, id, model);
			case 'MsgInit':
				var d = msg.a;
				return A2(author$project$Main$onMsgInit, d, model);
			case 'OnResize':
				var _n2 = msg.a;
				var w = _n2.a;
				var h = _n2.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							window: {height: h, width: w}
						}),
					elm$core$Platform$Cmd$none);
			case 'MsgSetUsername':
				var s = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{username: s}),
					elm$core$Platform$Cmd$none);
			case 'MsgFinishUsername':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{usernameSet: true}),
					author$project$Main$wsSend(
						A2(
							author$project$Main$encodeSetUsername,
							model.uid,
							author$project$Main$PacketSetUsername(model.username))));
			case 'MousePress':
				var e = msg.a;
				return A2(author$project$Main$onMousePress, e, model);
			case 'MouseMotion':
				var e = msg.a;
				return A2(author$project$Main$onMouseMotion, e, model);
			case 'MouseRelease':
				var e = msg.a;
				return A2(author$project$Main$onMouseRelease, e, model);
			case 'MouseWheel':
				var e = msg.a;
				return A2(author$project$Main$onMouseWheel, e, model);
			case 'KeyDown':
				var e = msg.a;
				return A2(author$project$Main$onKeyDown, e, model);
			case 'ChatKeyDown':
				var e = msg.a;
				return A2(author$project$Main$onChatKeyDown, e, model);
			case 'ChangedFocus':
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 'MsgChat':
				var s = msg.a;
				var m = msg.b;
				return A3(author$project$Main$onMsgChat, s, m, model);
			case 'ChatInput':
				var s = msg.a;
				return A2(author$project$Main$onMsgChatInput, s, model);
			case 'MsgSetCreateMode':
				var m = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{createMode: m}),
					elm$core$Platform$Cmd$none);
			case 'MsgCreateDoodadLine':
				var m = msg.a;
				return A3(
					author$project$Main$onMsgCreateDoodadLine,
					_Utils_Tuple2(m.sx, m.sy),
					_Utils_Tuple2(m.ex, m.ey),
					model);
			case 'MsgSendClearDoodads':
				return author$project$Main$onMsgSendClearDoodads(model);
			case 'MsgClearDoodads':
				return author$project$Main$onMsgClearDoodads(model);
			case 'MsgSendClearTokens':
				return author$project$Main$onMsgSendClearTokens(model);
			case 'MsgClearTokens':
				return author$project$Main$onMsgClearTokens(model);
			case 'MsgToggleFoe':
				var i = msg.a;
				return A2(author$project$Main$onMsgToggleFoe, i, model);
			case 'MsgRestoreSession':
				var p = msg.a;
				return A2(author$project$Main$onMsgRestoreSession, p, model);
			case 'MsgDoNothing':
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			default:
				var e = msg.a;
				var v = A2(elm$core$Debug$log, 'Error:', e);
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
		}
	});
var Gizra$elm_keyboard_event$Keyboard$Event$KeyboardEvent = F7(
	function (altKey, ctrlKey, key, keyCode, metaKey, repeat, shiftKey) {
		return {altKey: altKey, ctrlKey: ctrlKey, key: key, keyCode: keyCode, metaKey: metaKey, repeat: repeat, shiftKey: shiftKey};
	});
var elm$json$Json$Decode$andThen = _Json_andThen;
var elm$json$Json$Decode$fail = _Json_fail;
var elm$json$Json$Decode$oneOf = _Json_oneOf;
var elm$json$Json$Decode$maybe = function (decoder) {
	return elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(elm$json$Json$Decode$map, elm$core$Maybe$Just, decoder),
				elm$json$Json$Decode$succeed(elm$core$Maybe$Nothing)
			]));
};
var Gizra$elm_keyboard_event$Keyboard$Event$decodeKey = elm$json$Json$Decode$maybe(
	A2(
		elm$json$Json$Decode$andThen,
		function (key) {
			return elm$core$String$isEmpty(key) ? elm$json$Json$Decode$fail('empty key') : elm$json$Json$Decode$succeed(key);
		},
		A2(elm$json$Json$Decode$field, 'key', elm$json$Json$Decode$string)));
var Gizra$elm_keyboard_event$Keyboard$Event$decodeNonZero = A2(
	elm$json$Json$Decode$andThen,
	function (code) {
		return (!code) ? elm$json$Json$Decode$fail('code was zero') : elm$json$Json$Decode$succeed(code);
	},
	elm$json$Json$Decode$int);
var Gizra$elm_keyboard_event$Keyboard$Event$decodeKeyCode = elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(elm$json$Json$Decode$field, 'keyCode', Gizra$elm_keyboard_event$Keyboard$Event$decodeNonZero),
			A2(elm$json$Json$Decode$field, 'which', Gizra$elm_keyboard_event$Keyboard$Event$decodeNonZero),
			A2(elm$json$Json$Decode$field, 'charCode', Gizra$elm_keyboard_event$Keyboard$Event$decodeNonZero),
			elm$json$Json$Decode$succeed(0)
		]));
var SwiftsNamesake$proper_keyboard$Keyboard$Key$A = {$: 'A'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Add = {$: 'Add'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Alt = {$: 'Alt'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Ambiguous = function (a) {
	return {$: 'Ambiguous', a: a};
};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$B = {$: 'B'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Backspace = {$: 'Backspace'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$C = {$: 'C'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$CapsLock = {$: 'CapsLock'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$ChromeSearch = {$: 'ChromeSearch'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Command = {$: 'Command'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Ctrl = function (a) {
	return {$: 'Ctrl', a: a};
};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$D = {$: 'D'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Decimal = {$: 'Decimal'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Divide = {$: 'Divide'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$E = {$: 'E'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Eight = {$: 'Eight'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$End = {$: 'End'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Escape = {$: 'Escape'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$F1 = {$: 'F1'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$F10 = {$: 'F10'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$F11 = {$: 'F11'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$F12 = {$: 'F12'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$F2 = {$: 'F2'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$F3 = {$: 'F3'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$F4 = {$: 'F4'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$F5 = {$: 'F5'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$F6 = {$: 'F6'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$F7 = {$: 'F7'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$F8 = {$: 'F8'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$F9 = {$: 'F9'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Five = {$: 'Five'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Four = {$: 'Four'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$G = {$: 'G'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$H = {$: 'H'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Home = {$: 'Home'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$I = {$: 'I'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Insert = {$: 'Insert'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$J = {$: 'J'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$K = {$: 'K'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$L = {$: 'L'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Left = {$: 'Left'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$M = {$: 'M'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Multiply = {$: 'Multiply'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$N = {$: 'N'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Nine = {$: 'Nine'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$NumLock = {$: 'NumLock'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadEight = {$: 'NumpadEight'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadFive = {$: 'NumpadFive'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadFour = {$: 'NumpadFour'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadNine = {$: 'NumpadNine'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadOne = {$: 'NumpadOne'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadSeven = {$: 'NumpadSeven'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadSix = {$: 'NumpadSix'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadThree = {$: 'NumpadThree'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadTwo = {$: 'NumpadTwo'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadZero = {$: 'NumpadZero'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$O = {$: 'O'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$One = {$: 'One'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$P = {$: 'P'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$PageDown = {$: 'PageDown'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$PageUp = {$: 'PageUp'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$PauseBreak = {$: 'PauseBreak'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$PrintScreen = {$: 'PrintScreen'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Q = {$: 'Q'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$R = {$: 'R'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Right = {$: 'Right'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$S = {$: 'S'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$ScrollLock = {$: 'ScrollLock'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Seven = {$: 'Seven'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Shift = function (a) {
	return {$: 'Shift', a: a};
};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Six = {$: 'Six'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Spacebar = {$: 'Spacebar'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Subtract = {$: 'Subtract'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$T = {$: 'T'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Tab = {$: 'Tab'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Three = {$: 'Three'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Two = {$: 'Two'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$U = {$: 'U'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Unknown = function (a) {
	return {$: 'Unknown', a: a};
};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$V = {$: 'V'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$W = {$: 'W'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Windows = {$: 'Windows'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$X = {$: 'X'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Y = {$: 'Y'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Z = {$: 'Z'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$Zero = {$: 'Zero'};
var SwiftsNamesake$proper_keyboard$Keyboard$Key$fromCode = function (keyCode) {
	switch (keyCode) {
		case 8:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Backspace;
		case 9:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Tab;
		case 13:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Enter;
		case 16:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Shift(elm$core$Maybe$Nothing);
		case 17:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Ctrl(elm$core$Maybe$Nothing);
		case 18:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Alt;
		case 19:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$PauseBreak;
		case 20:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$CapsLock;
		case 27:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Escape;
		case 32:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Spacebar;
		case 33:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$PageUp;
		case 34:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$PageDown;
		case 35:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$End;
		case 36:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Home;
		case 37:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Left;
		case 38:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Up;
		case 39:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Right;
		case 40:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Down;
		case 44:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$PrintScreen;
		case 45:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Insert;
		case 46:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Delete;
		case 48:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Zero;
		case 49:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$One;
		case 50:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Two;
		case 51:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Three;
		case 52:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Four;
		case 53:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Five;
		case 54:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Six;
		case 55:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Seven;
		case 56:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Eight;
		case 57:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Nine;
		case 65:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$A;
		case 66:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$B;
		case 67:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$C;
		case 68:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$D;
		case 69:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$E;
		case 70:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$F;
		case 71:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$G;
		case 72:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$H;
		case 73:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$I;
		case 74:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$J;
		case 75:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$K;
		case 76:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$L;
		case 77:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$M;
		case 78:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$N;
		case 79:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$O;
		case 80:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$P;
		case 81:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Q;
		case 82:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$R;
		case 83:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$S;
		case 84:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$T;
		case 85:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$U;
		case 86:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$V;
		case 87:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$W;
		case 88:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$X;
		case 89:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Y;
		case 90:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Z;
		case 91:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Ambiguous(
				_List_fromArray(
					[SwiftsNamesake$proper_keyboard$Keyboard$Key$Windows, SwiftsNamesake$proper_keyboard$Keyboard$Key$Command, SwiftsNamesake$proper_keyboard$Keyboard$Key$ChromeSearch]));
		case 96:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadZero;
		case 97:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadOne;
		case 98:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadTwo;
		case 99:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadThree;
		case 100:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadFour;
		case 101:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadFive;
		case 102:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadSix;
		case 103:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadSeven;
		case 104:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadEight;
		case 105:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$NumpadNine;
		case 106:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Multiply;
		case 107:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Add;
		case 109:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Subtract;
		case 110:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Decimal;
		case 111:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Divide;
		case 112:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$F1;
		case 113:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$F2;
		case 114:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$F3;
		case 115:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$F4;
		case 116:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$F5;
		case 117:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$F6;
		case 118:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$F7;
		case 119:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$F8;
		case 120:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$F9;
		case 121:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$F10;
		case 122:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$F11;
		case 123:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$F12;
		case 144:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$NumLock;
		case 145:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$ScrollLock;
		default:
			return SwiftsNamesake$proper_keyboard$Keyboard$Key$Unknown(keyCode);
	}
};
var elm$json$Json$Decode$map7 = _Json_map7;
var Gizra$elm_keyboard_event$Keyboard$Event$decodeKeyboardEvent = A8(
	elm$json$Json$Decode$map7,
	Gizra$elm_keyboard_event$Keyboard$Event$KeyboardEvent,
	A2(elm$json$Json$Decode$field, 'altKey', elm$json$Json$Decode$bool),
	A2(elm$json$Json$Decode$field, 'ctrlKey', elm$json$Json$Decode$bool),
	Gizra$elm_keyboard_event$Keyboard$Event$decodeKey,
	A2(elm$json$Json$Decode$map, SwiftsNamesake$proper_keyboard$Keyboard$Key$fromCode, Gizra$elm_keyboard_event$Keyboard$Event$decodeKeyCode),
	A2(elm$json$Json$Decode$field, 'metaKey', elm$json$Json$Decode$bool),
	A2(elm$json$Json$Decode$field, 'repeat', elm$json$Json$Decode$bool),
	A2(elm$json$Json$Decode$field, 'shiftKey', elm$json$Json$Decode$bool));
var author$project$Main$ChatInput = function (a) {
	return {$: 'ChatInput', a: a};
};
var author$project$Main$ChatKeyDown = function (a) {
	return {$: 'ChatKeyDown', a: a};
};
var author$project$Main$KeyDown = function (a) {
	return {$: 'KeyDown', a: a};
};
var author$project$Main$ModeCreateLine = {$: 'ModeCreateLine'};
var author$project$Main$MouseMotion = function (a) {
	return {$: 'MouseMotion', a: a};
};
var author$project$Main$MousePress = function (a) {
	return {$: 'MousePress', a: a};
};
var author$project$Main$MouseRelease = function (a) {
	return {$: 'MouseRelease', a: a};
};
var author$project$Main$MouseWheel = function (a) {
	return {$: 'MouseWheel', a: a};
};
var author$project$Main$MsgSendClearDoodads = {$: 'MsgSendClearDoodads'};
var author$project$Main$MsgSendClearTokens = {$: 'MsgSendClearTokens'};
var author$project$Main$MsgSetCreateMode = function (a) {
	return {$: 'MsgSetCreateMode', a: a};
};
var joakin$elm_canvas$Canvas$Settings$Advanced$Scale = F2(
	function (a, b) {
		return {$: 'Scale', a: a, b: b};
	});
var joakin$elm_canvas$Canvas$Settings$Advanced$scale = joakin$elm_canvas$Canvas$Settings$Advanced$Scale;
var joakin$elm_canvas$Canvas$Internal$Canvas$SettingCommands = function (a) {
	return {$: 'SettingCommands', a: a};
};
var elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn = F2(
	function (name, args) {
		return elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					elm$json$Json$Encode$string('function')),
					_Utils_Tuple2(
					'name',
					elm$json$Json$Encode$string(name)),
					_Utils_Tuple2(
					'args',
					A2(elm$json$Json$Encode$list, elm$core$Basics$identity, args))
				]));
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$rotate = function (angle) {
	return A2(
		joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
		'rotate',
		_List_fromArray(
			[
				elm$json$Json$Encode$float(angle)
			]));
};
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$scale = F2(
	function (x, y) {
		return A2(
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'scale',
			_List_fromArray(
				[
					elm$json$Json$Encode$float(x),
					elm$json$Json$Encode$float(y)
				]));
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$transform = F6(
	function (a, b, c, d, e, f) {
		return A2(
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'transform',
			_List_fromArray(
				[
					elm$json$Json$Encode$float(a),
					elm$json$Json$Encode$float(b),
					elm$json$Json$Encode$float(c),
					elm$json$Json$Encode$float(d),
					elm$json$Json$Encode$float(e),
					elm$json$Json$Encode$float(f)
				]));
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$translate = F2(
	function (x, y) {
		return A2(
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'translate',
			_List_fromArray(
				[
					elm$json$Json$Encode$float(x),
					elm$json$Json$Encode$float(y)
				]));
	});
var joakin$elm_canvas$Canvas$Settings$Advanced$transform = function (transforms) {
	return joakin$elm_canvas$Canvas$Internal$Canvas$SettingCommands(
		A2(
			elm$core$List$map,
			function (t) {
				switch (t.$) {
					case 'Rotate':
						var angle = t.a;
						return joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$rotate(angle);
					case 'Scale':
						var x = t.a;
						var y = t.b;
						return A2(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$scale, x, y);
					case 'Translate':
						var x = t.a;
						var y = t.b;
						return A2(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$translate, x, y);
					default:
						var m11 = t.a.m11;
						var m12 = t.a.m12;
						var m21 = t.a.m21;
						var m22 = t.a.m22;
						var dx = t.a.dx;
						var dy = t.a.dy;
						return A6(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$transform, m11, m12, m21, m22, dx, dy);
				}
			},
			transforms));
};
var joakin$elm_canvas$Canvas$Settings$Advanced$Translate = F2(
	function (a, b) {
		return {$: 'Translate', a: a, b: b};
	});
var joakin$elm_canvas$Canvas$Settings$Advanced$translate = joakin$elm_canvas$Canvas$Settings$Advanced$Translate;
var joakin$elm_canvas$Canvas$Internal$Canvas$SettingCommand = function (a) {
	return {$: 'SettingCommand', a: a};
};
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$field = F2(
	function (name, value) {
		return elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					elm$json$Json$Encode$string('field')),
					_Utils_Tuple2(
					'name',
					elm$json$Json$Encode$string(name)),
					_Utils_Tuple2('value', value)
				]));
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$lineWidth = function (value) {
	return A2(
		joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$field,
		'lineWidth',
		elm$json$Json$Encode$float(value));
};
var joakin$elm_canvas$Canvas$Settings$Line$lineWidth = function (width) {
	return joakin$elm_canvas$Canvas$Internal$Canvas$SettingCommand(
		joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$lineWidth(width));
};
var author$project$Main$canvasTransform = F2(
	function (d, m) {
		var scale = d.canvasHeight / m.view.height;
		return _List_fromArray(
			[
				joakin$elm_canvas$Canvas$Settings$Advanced$transform(
				_List_fromArray(
					[
						A2(joakin$elm_canvas$Canvas$Settings$Advanced$translate, d.canvasWidth / 2, d.canvasHeight / 2),
						A2(joakin$elm_canvas$Canvas$Settings$Advanced$scale, scale, scale),
						A2(joakin$elm_canvas$Canvas$Settings$Advanced$translate, -m.view.x, -m.view.y)
					])),
				joakin$elm_canvas$Canvas$Settings$Line$lineWidth(2 / scale)
			]);
	});
var joakin$elm_canvas$Canvas$Internal$Canvas$Rect = F3(
	function (a, b, c) {
		return {$: 'Rect', a: a, b: b, c: c};
	});
var joakin$elm_canvas$Canvas$rect = F3(
	function (pos, width, height) {
		return A3(joakin$elm_canvas$Canvas$Internal$Canvas$Rect, pos, width, height);
	});
var joakin$elm_canvas$Canvas$Renderable = function (a) {
	return {$: 'Renderable', a: a};
};
var joakin$elm_canvas$Canvas$Internal$Canvas$Fill = function (a) {
	return {$: 'Fill', a: a};
};
var joakin$elm_canvas$Canvas$Internal$Canvas$FillAndStroke = F2(
	function (a, b) {
		return {$: 'FillAndStroke', a: a, b: b};
	});
var joakin$elm_canvas$Canvas$Internal$Canvas$Stroke = function (a) {
	return {$: 'Stroke', a: a};
};
var joakin$elm_canvas$Canvas$mergeDrawOp = F2(
	function (op1, op2) {
		var _n0 = _Utils_Tuple2(op1, op2);
		_n0$7:
		while (true) {
			switch (_n0.b.$) {
				case 'FillAndStroke':
					var _n1 = _n0.b;
					var c = _n1.a;
					var sc = _n1.b;
					return A2(joakin$elm_canvas$Canvas$Internal$Canvas$FillAndStroke, c, sc);
				case 'Fill':
					switch (_n0.a.$) {
						case 'Fill':
							var c = _n0.b.a;
							return joakin$elm_canvas$Canvas$Internal$Canvas$Fill(c);
						case 'Stroke':
							var c1 = _n0.a.a;
							var c2 = _n0.b.a;
							return A2(joakin$elm_canvas$Canvas$Internal$Canvas$FillAndStroke, c2, c1);
						case 'FillAndStroke':
							var _n2 = _n0.a;
							var c = _n2.a;
							var sc = _n2.b;
							var c2 = _n0.b.a;
							return A2(joakin$elm_canvas$Canvas$Internal$Canvas$FillAndStroke, c2, sc);
						default:
							break _n0$7;
					}
				case 'Stroke':
					switch (_n0.a.$) {
						case 'Stroke':
							var c = _n0.b.a;
							return joakin$elm_canvas$Canvas$Internal$Canvas$Stroke(c);
						case 'Fill':
							var c1 = _n0.a.a;
							var c2 = _n0.b.a;
							return A2(joakin$elm_canvas$Canvas$Internal$Canvas$FillAndStroke, c1, c2);
						case 'FillAndStroke':
							var _n3 = _n0.a;
							var c = _n3.a;
							var sc = _n3.b;
							var sc2 = _n0.b.a;
							return A2(joakin$elm_canvas$Canvas$Internal$Canvas$FillAndStroke, c, sc2);
						default:
							break _n0$7;
					}
				default:
					if (_n0.a.$ === 'NotSpecified') {
						break _n0$7;
					} else {
						var whatever = _n0.a;
						var _n5 = _n0.b;
						return whatever;
					}
			}
		}
		var _n4 = _n0.a;
		var whatever = _n0.b;
		return whatever;
	});
var joakin$elm_canvas$Canvas$addSettingsToRenderable = F2(
	function (settings, renderable) {
		var addSetting = F2(
			function (setting, _n1) {
				var r = _n1.a;
				return joakin$elm_canvas$Canvas$Renderable(
					function () {
						switch (setting.$) {
							case 'SettingCommand':
								var cmd = setting.a;
								return _Utils_update(
									r,
									{
										commands: A2(elm$core$List$cons, cmd, r.commands)
									});
							case 'SettingCommands':
								var cmds = setting.a;
								return _Utils_update(
									r,
									{
										commands: A3(elm$core$List$foldl, elm$core$List$cons, r.commands, cmds)
									});
							case 'SettingUpdateDrawable':
								var f = setting.a;
								return _Utils_update(
									r,
									{
										drawable: f(r.drawable)
									});
							default:
								var op = setting.a;
								return _Utils_update(
									r,
									{
										drawOp: A2(joakin$elm_canvas$Canvas$mergeDrawOp, r.drawOp, op)
									});
						}
					}());
			});
		return A3(elm$core$List$foldl, addSetting, renderable, settings);
	});
var joakin$elm_canvas$Canvas$Internal$Canvas$DrawableShapes = function (a) {
	return {$: 'DrawableShapes', a: a};
};
var joakin$elm_canvas$Canvas$Internal$Canvas$NotSpecified = {$: 'NotSpecified'};
var joakin$elm_canvas$Canvas$shapes = F2(
	function (settings, ss) {
		return A2(
			joakin$elm_canvas$Canvas$addSettingsToRenderable,
			settings,
			joakin$elm_canvas$Canvas$Renderable(
				{
					commands: _List_Nil,
					drawOp: joakin$elm_canvas$Canvas$Internal$Canvas$NotSpecified,
					drawable: joakin$elm_canvas$Canvas$Internal$Canvas$DrawableShapes(ss)
				}));
	});
var joakin$elm_canvas$Canvas$Internal$Canvas$SettingDrawOp = function (a) {
	return {$: 'SettingDrawOp', a: a};
};
var joakin$elm_canvas$Canvas$Settings$fill = function (color) {
	return joakin$elm_canvas$Canvas$Internal$Canvas$SettingDrawOp(
		joakin$elm_canvas$Canvas$Internal$Canvas$Fill(color));
};
var author$project$Main$clearCanvas = F2(
	function (w, h) {
		return A2(
			joakin$elm_canvas$Canvas$shapes,
			_List_fromArray(
				[
					joakin$elm_canvas$Canvas$Settings$fill(
					A3(avh4$elm_color$Color$rgb, 0.2, 0.2, 0.2))
				]),
			_List_fromArray(
				[
					A3(
					joakin$elm_canvas$Canvas$rect,
					_Utils_Tuple2(0, 0),
					w,
					h)
				]));
	});
var author$project$Main$dimensionsFromModel = function (m) {
	return {
		canvasHeight: author$project$Main$computeCanvasHeight(m.window.height),
		canvasWidth: author$project$Main$computeCanvasWidth(m.window.width),
		chatHeight: elm$core$Basics$floor(1 * m.window.height),
		chatWidth: 350,
		toolbarHeight: 40,
		toolbarWidth: author$project$Main$computeCanvasWidth(m.window.width) - 30,
		toolsHeight: elm$core$Basics$floor(0 * m.window.height),
		toolsWidth: 350
	};
};
var elm$html$Html$img = _VirtualDom_node('img');
var elm$html$Html$input = _VirtualDom_node('input');
var elm$html$Html$label = _VirtualDom_node('label');
var elm$json$Json$Encode$bool = _Json_wrap;
var elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$bool(bool));
	});
var elm$html$Html$Attributes$checked = elm$html$Html$Attributes$boolProperty('checked');
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$src = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var elm$html$Html$Attributes$type_ = elm$html$Html$Attributes$stringProperty('type');
var elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$html$Html$Events$targetValue = A2(
	elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	elm$json$Json$Decode$string);
var elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			elm$json$Json$Decode$map,
			elm$html$Html$Events$alwaysStop,
			A2(elm$json$Json$Decode$map, tagger, elm$html$Html$Events$targetValue)));
};
var author$project$Main$radioButton = F3(
	function (m, img, a) {
		return A2(
			elm$html$Html$label,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					elm$html$Html$input,
					_List_fromArray(
						[
							elm$html$Html$Attributes$type_('radio'),
							elm$html$Html$Events$onInput(
							function (x) {
								return m;
							}),
							elm$html$Html$Attributes$checked(a)
						]),
					_List_Nil),
					A2(
					elm$html$Html$img,
					_List_fromArray(
						[
							elm$html$Html$Attributes$src(img)
						]),
					_List_Nil)
				]));
	});
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$html$Html$dd = _VirtualDom_node('dd');
var elm$html$Html$dl = _VirtualDom_node('dl');
var elm$html$Html$dt = _VirtualDom_node('dt');
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var author$project$Main$viewChat = function (model) {
	return _List_fromArray(
		[
			A2(
			elm$html$Html$dl,
			_List_Nil,
			elm$core$List$concat(
				A2(
					elm$core$List$map,
					function (_n0) {
						var s = _n0.a;
						var m = _n0.b;
						return _List_fromArray(
							[
								A2(
								elm$html$Html$dt,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(s)
									])),
								A2(
								elm$html$Html$dd,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(m)
									]))
							]);
					},
					elm$core$List$reverse(model.chat))))
		]);
};
var joakin$elm_canvas$Canvas$Internal$Canvas$LineTo = function (a) {
	return {$: 'LineTo', a: a};
};
var joakin$elm_canvas$Canvas$lineTo = function (point) {
	return joakin$elm_canvas$Canvas$Internal$Canvas$LineTo(point);
};
var joakin$elm_canvas$Canvas$Internal$Canvas$Path = F2(
	function (a, b) {
		return {$: 'Path', a: a, b: b};
	});
var joakin$elm_canvas$Canvas$path = F2(
	function (startingPoint, segments) {
		return A2(joakin$elm_canvas$Canvas$Internal$Canvas$Path, startingPoint, segments);
	});
var joakin$elm_canvas$Canvas$Settings$stroke = function (color) {
	return joakin$elm_canvas$Canvas$Internal$Canvas$SettingDrawOp(
		joakin$elm_canvas$Canvas$Internal$Canvas$Stroke(color));
};
var author$project$Main$viewCurrentDoodad = F2(
	function (trans, model) {
		var _n0 = model.action;
		if (_n0.$ === 'ActionCreateLine') {
			var l = _n0.a;
			return _List_fromArray(
				[
					A2(
					joakin$elm_canvas$Canvas$shapes,
					_Utils_ap(
						trans,
						_List_fromArray(
							[
								joakin$elm_canvas$Canvas$Settings$stroke(
								A3(avh4$elm_color$Color$rgb, 1, 1, 1))
							])),
					_List_fromArray(
						[
							A2(
							joakin$elm_canvas$Canvas$path,
							_Utils_Tuple2(l.sx, l.sy),
							_List_fromArray(
								[
									joakin$elm_canvas$Canvas$lineTo(
									_Utils_Tuple2(l.ex, l.ey))
								]))
						]))
				]);
		} else {
			return _List_Nil;
		}
	});
var author$project$Main$worldToScreen = F2(
	function (m, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		var scale = author$project$Main$computeCanvasHeight(m.window.height) / m.view.height;
		return _Utils_Tuple2(
			((x - m.view.x) * scale) + (0.5 * author$project$Main$computeCanvasWidth(m.window.width)),
			((y - m.view.y) * scale) + (0.5 * author$project$Main$computeCanvasHeight(m.window.height)));
	});
var joakin$elm_canvas$Canvas$Internal$Canvas$DrawableText = function (a) {
	return {$: 'DrawableText', a: a};
};
var joakin$elm_canvas$Canvas$text = F3(
	function (settings, point, str) {
		return A2(
			joakin$elm_canvas$Canvas$addSettingsToRenderable,
			settings,
			joakin$elm_canvas$Canvas$Renderable(
				{
					commands: _List_Nil,
					drawOp: joakin$elm_canvas$Canvas$Internal$Canvas$NotSpecified,
					drawable: joakin$elm_canvas$Canvas$Internal$Canvas$DrawableText(
						{maxWidth: elm$core$Maybe$Nothing, point: point, text: str})
				}));
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$font = function (f) {
	return A2(
		joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$field,
		'font',
		elm$json$Json$Encode$string(f));
};
var joakin$elm_canvas$Canvas$Settings$Text$font = function (_n0) {
	var size = _n0.size;
	var family = _n0.family;
	return joakin$elm_canvas$Canvas$Internal$Canvas$SettingCommand(
		joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$font(
			elm$core$String$fromInt(size) + ('px ' + family)));
};
var elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var elm$core$Basics$isInfinite = _Basics_isInfinite;
var elm$core$Basics$isNaN = _Basics_isNaN;
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$core$String$fromFloat = _String_fromNumber;
var elm$core$String$cons = _String_cons;
var elm$core$String$fromChar = function (_char) {
	return A2(elm$core$String$cons, _char, '');
};
var elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3(elm$core$String$repeatHelp, n, chunk, '');
	});
var elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				elm$core$String$repeat,
				n - elm$core$String$length(string),
				elm$core$String$fromChar(_char)));
	});
var elm$core$String$reverse = _String_reverse;
var elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var elm$core$String$foldr = _String_foldr;
var elm$core$String$toList = function (string) {
	return A3(elm$core$String$foldr, elm$core$List$cons, _List_Nil, string);
};
var myrho$elm_round$Round$addSign = F2(
	function (signed, str) {
		var isNotZero = A2(
			elm$core$List$any,
			function (c) {
				return (!_Utils_eq(
					c,
					_Utils_chr('0'))) && (!_Utils_eq(
					c,
					_Utils_chr('.')));
			},
			elm$core$String$toList(str));
		return _Utils_ap(
			(signed && isNotZero) ? '-' : '',
			str);
	});
var elm$core$Char$fromCode = _Char_fromCode;
var myrho$elm_round$Round$increaseNum = function (_n0) {
	var head = _n0.a;
	var tail = _n0.b;
	if (_Utils_eq(
		head,
		_Utils_chr('9'))) {
		var _n1 = elm$core$String$uncons(tail);
		if (_n1.$ === 'Nothing') {
			return '01';
		} else {
			var headtail = _n1.a;
			return A2(
				elm$core$String$cons,
				_Utils_chr('0'),
				myrho$elm_round$Round$increaseNum(headtail));
		}
	} else {
		var c = elm$core$Char$toCode(head);
		return ((c >= 48) && (c < 57)) ? A2(
			elm$core$String$cons,
			elm$core$Char$fromCode(c + 1),
			tail) : '0';
	}
};
var myrho$elm_round$Round$splitComma = function (str) {
	var _n0 = A2(elm$core$String$split, '.', str);
	if (_n0.b) {
		if (_n0.b.b) {
			var before = _n0.a;
			var _n1 = _n0.b;
			var after = _n1.a;
			return _Utils_Tuple2(before, after);
		} else {
			var before = _n0.a;
			return _Utils_Tuple2(before, '0');
		}
	} else {
		return _Utils_Tuple2('0', '0');
	}
};
var elm$core$Tuple$mapFirst = F2(
	function (func, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var myrho$elm_round$Round$toDecimal = function (fl) {
	var _n0 = A2(
		elm$core$String$split,
		'e',
		elm$core$String$fromFloat(
			elm$core$Basics$abs(fl)));
	if (_n0.b) {
		if (_n0.b.b) {
			var num = _n0.a;
			var _n1 = _n0.b;
			var exp = _n1.a;
			var e = A2(
				elm$core$Maybe$withDefault,
				0,
				elm$core$String$toInt(
					A2(elm$core$String$startsWith, '+', exp) ? A2(elm$core$String$dropLeft, 1, exp) : exp));
			var _n2 = myrho$elm_round$Round$splitComma(num);
			var before = _n2.a;
			var after = _n2.b;
			var total = _Utils_ap(before, after);
			var zeroed = (e < 0) ? A2(
				elm$core$Maybe$withDefault,
				'0',
				A2(
					elm$core$Maybe$map,
					function (_n3) {
						var a = _n3.a;
						var b = _n3.b;
						return a + ('.' + b);
					},
					A2(
						elm$core$Maybe$map,
						elm$core$Tuple$mapFirst(elm$core$String$fromChar),
						elm$core$String$uncons(
							_Utils_ap(
								A2(
									elm$core$String$repeat,
									elm$core$Basics$abs(e),
									'0'),
								total))))) : A3(
				elm$core$String$padRight,
				e + 1,
				_Utils_chr('0'),
				total);
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				zeroed);
		} else {
			var num = _n0.a;
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				num);
		}
	} else {
		return '';
	}
};
var myrho$elm_round$Round$roundFun = F3(
	function (functor, s, fl) {
		if (elm$core$Basics$isInfinite(fl) || elm$core$Basics$isNaN(fl)) {
			return elm$core$String$fromFloat(fl);
		} else {
			var signed = fl < 0;
			var _n0 = myrho$elm_round$Round$splitComma(
				myrho$elm_round$Round$toDecimal(
					elm$core$Basics$abs(fl)));
			var before = _n0.a;
			var after = _n0.b;
			var r = elm$core$String$length(before) + s;
			var normalized = _Utils_ap(
				A2(elm$core$String$repeat, (-r) + 1, '0'),
				A3(
					elm$core$String$padRight,
					r,
					_Utils_chr('0'),
					_Utils_ap(before, after)));
			var totalLen = elm$core$String$length(normalized);
			var roundDigitIndex = A2(elm$core$Basics$max, 1, r);
			var increase = A2(
				functor,
				signed,
				A3(elm$core$String$slice, roundDigitIndex, totalLen, normalized));
			var remains = A3(elm$core$String$slice, 0, roundDigitIndex, normalized);
			var num = increase ? elm$core$String$reverse(
				A2(
					elm$core$Maybe$withDefault,
					'1',
					A2(
						elm$core$Maybe$map,
						myrho$elm_round$Round$increaseNum,
						elm$core$String$uncons(
							elm$core$String$reverse(remains))))) : remains;
			var numLen = elm$core$String$length(num);
			var numZeroed = (num === '0') ? num : ((s <= 0) ? _Utils_ap(
				num,
				A2(
					elm$core$String$repeat,
					elm$core$Basics$abs(s),
					'0')) : ((_Utils_cmp(
				s,
				elm$core$String$length(after)) < 0) ? (A3(elm$core$String$slice, 0, numLen - s, num) + ('.' + A3(elm$core$String$slice, numLen - s, numLen, num))) : _Utils_ap(
				before + '.',
				A3(
					elm$core$String$padRight,
					s,
					_Utils_chr('0'),
					after))));
			return A2(myrho$elm_round$Round$addSign, signed, numZeroed);
		}
	});
var myrho$elm_round$Round$round = myrho$elm_round$Round$roundFun(
	F2(
		function (signed, str) {
			var _n0 = elm$core$String$uncons(str);
			if (_n0.$ === 'Nothing') {
				return false;
			} else {
				if ('5' === _n0.a.a.valueOf()) {
					if (_n0.a.b === '') {
						var _n1 = _n0.a;
						return !signed;
					} else {
						var _n2 = _n0.a;
						return true;
					}
				} else {
					var _n3 = _n0.a;
					var _int = _n3.a;
					return function (i) {
						return ((i > 53) && signed) || ((i >= 53) && (!signed));
					}(
						elm$core$Char$toCode(_int));
				}
			}
		}));
var author$project$Main$distanceLineRenderable = F5(
	function (_n0, _n1, m, d, trans) {
		var sx = _n0.a;
		var sy = _n0.b;
		var ex = _n1.a;
		var ey = _n1.b;
		var length = elm$core$Basics$sqrt(
			A2(elm$core$Basics$pow, sx - ex, 2) + A2(elm$core$Basics$pow, sy - ey, 2));
		var _n2 = A2(
			author$project$Main$worldToScreen,
			m,
			_Utils_Tuple2(sx, sy));
		var ssx = _n2.a;
		var ssy = _n2.b;
		var _n3 = A2(
			author$project$Main$worldToScreen,
			m,
			_Utils_Tuple2(ex, ey));
		var sex = _n3.a;
		var sey = _n3.b;
		return _List_fromArray(
			[
				A2(
				joakin$elm_canvas$Canvas$shapes,
				_List_fromArray(
					[
						joakin$elm_canvas$Canvas$Settings$stroke(
						A3(avh4$elm_color$Color$rgb, 0.5, 0.5, 0.5))
					]),
				_List_fromArray(
					[
						A2(
						joakin$elm_canvas$Canvas$path,
						_Utils_Tuple2(ssx, ssy),
						_List_fromArray(
							[
								joakin$elm_canvas$Canvas$lineTo(
								_Utils_Tuple2(sex, sey))
							]))
					])),
				A3(
				joakin$elm_canvas$Canvas$text,
				_List_fromArray(
					[
						joakin$elm_canvas$Canvas$Settings$stroke(
						A3(avh4$elm_color$Color$rgb, 0.3, 0.3, 0.3)),
						joakin$elm_canvas$Canvas$Settings$fill(
						A3(avh4$elm_color$Color$rgb, 1, 1, 1)),
						joakin$elm_canvas$Canvas$Settings$Text$font(
						{family: 'sans serif', size: 22})
					]),
				_Utils_Tuple2(sex + 10, sey),
				A2(myrho$elm_round$Round$round, 2, length) + 'm')
			]);
	});
var author$project$Main$getSelectedPos = function (m) {
	var t = A2(author$project$Main$getToken, m.selected, m.tokens);
	if (t.$ === 'Just') {
		var a = t.a;
		var b = a.a;
		return _Utils_Tuple2(b.x, b.y);
	} else {
		return _Utils_Tuple2(0, 0);
	}
};
var author$project$Main$viewDistanceLine = F3(
	function (m, d, trans) {
		if (m.selected < 0) {
			return _List_Nil;
		} else {
			var _n0 = m.action;
			if (_n0.$ === 'DragToken') {
				var a = _n0.a;
				return A5(
					author$project$Main$distanceLineRenderable,
					_Utils_Tuple2(a.startx, a.starty),
					_Utils_Tuple2(a.x, a.y),
					m,
					d,
					trans);
			} else {
				var _n1 = author$project$Main$getSelectedPos(m);
				var x = _n1.a;
				var y = _n1.b;
				return A5(
					author$project$Main$distanceLineRenderable,
					_Utils_Tuple2(x, y),
					_Utils_Tuple2(m.mouse.x, m.mouse.y),
					m,
					d,
					trans);
			}
		}
	});
var author$project$Main$viewDoodad = F2(
	function (trans, t) {
		var l = t.a;
		return A2(
			joakin$elm_canvas$Canvas$shapes,
			_Utils_ap(
				trans,
				_List_fromArray(
					[
						joakin$elm_canvas$Canvas$Settings$stroke(
						A3(avh4$elm_color$Color$rgb, 1, 1, 1))
					])),
			_List_fromArray(
				[
					A2(
					joakin$elm_canvas$Canvas$path,
					_Utils_Tuple2(l.sx, l.sy),
					_List_fromArray(
						[
							joakin$elm_canvas$Canvas$lineTo(
							_Utils_Tuple2(l.ex, l.ey))
						]))
				]));
	});
var author$project$Main$nextHighestMult = F2(
	function (a, mul) {
		return elm$core$Basics$ceiling(a / mul) * mul;
	});
var joakin$elm_canvas$Canvas$Internal$Canvas$MoveTo = function (a) {
	return {$: 'MoveTo', a: a};
};
var joakin$elm_canvas$Canvas$moveTo = function (point) {
	return joakin$elm_canvas$Canvas$Internal$Canvas$MoveTo(point);
};
var author$project$Main$viewGrid = F3(
	function (s, m, d) {
		var step = (s * d.canvasHeight) / m.view.height;
		var numy = elm$core$Basics$ceiling(d.canvasHeight / step);
		var numx = elm$core$Basics$ceiling(d.canvasWidth / step);
		var _n0 = A2(
			author$project$Main$screenToWorld,
			m,
			_Utils_Tuple2(0, 0));
		var minx = _n0.a;
		var miny = _n0.b;
		var minxg = A2(author$project$Main$nextHighestMult, minx, s);
		var minyg = A2(author$project$Main$nextHighestMult, miny, s);
		var _n1 = A2(
			author$project$Main$worldToScreen,
			m,
			_Utils_Tuple2(minxg, minyg));
		var offxn = _n1.a;
		var offyn = _n1.b;
		var offx = offxn;
		var offy = offyn;
		return A2(
			joakin$elm_canvas$Canvas$shapes,
			_List_fromArray(
				[
					joakin$elm_canvas$Canvas$Settings$stroke(
					A3(avh4$elm_color$Color$rgb, 0.3, 0.3, 0.3))
				]),
			_List_fromArray(
				[
					A2(
					joakin$elm_canvas$Canvas$path,
					_Utils_Tuple2(0, 0),
					elm$core$List$concat(
						A2(
							elm$core$List$map,
							function (i) {
								return _List_fromArray(
									[
										joakin$elm_canvas$Canvas$moveTo(
										_Utils_Tuple2(offx + (i * step), 0.0)),
										joakin$elm_canvas$Canvas$lineTo(
										_Utils_Tuple2(offx + (i * step), d.canvasHeight))
									]);
							},
							A2(elm$core$List$range, 0, numx)))),
					A2(
					joakin$elm_canvas$Canvas$path,
					_Utils_Tuple2(0, 0),
					elm$core$List$concat(
						A2(
							elm$core$List$map,
							function (i) {
								return _List_fromArray(
									[
										joakin$elm_canvas$Canvas$moveTo(
										_Utils_Tuple2(0.0, offy + (i * step))),
										joakin$elm_canvas$Canvas$lineTo(
										_Utils_Tuple2(d.canvasWidth, offy + (i * step)))
									]);
							},
							A2(elm$core$List$range, 0, numy))))
				]));
	});
var author$project$Main$MsgFinishUsername = {$: 'MsgFinishUsername'};
var author$project$Main$MsgSetUsername = function (a) {
	return {$: 'MsgSetUsername', a: a};
};
var elm$html$Html$button = _VirtualDom_node('button');
var elm$html$Html$div = _VirtualDom_node('div');
var elm$html$Html$Attributes$id = elm$html$Html$Attributes$stringProperty('id');
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$Main$viewSetUsername = function (model) {
	return (!model.usernameSet) ? _List_fromArray(
		[
			A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$id('username-popup')
				]),
			_List_fromArray(
				[
					elm$html$Html$text('Please enter a Username:'),
					A2(
					elm$html$Html$input,
					_List_fromArray(
						[
							elm$html$Html$Events$onInput(author$project$Main$MsgSetUsername)
						]),
					_List_Nil),
					A2(
					elm$html$Html$button,
					_List_fromArray(
						[
							elm$html$Html$Events$onClick(author$project$Main$MsgFinishUsername)
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Ok')
						]))
				]))
		]) : _List_Nil;
};
var avh4$elm_color$Color$rgba = F4(
	function (r, g, b, a) {
		return A4(avh4$elm_color$Color$RgbaSpace, r, g, b, a);
	});
var joakin$elm_canvas$Canvas$Internal$Canvas$Circle = F2(
	function (a, b) {
		return {$: 'Circle', a: a, b: b};
	});
var joakin$elm_canvas$Canvas$circle = F2(
	function (pos, radius) {
		return A2(joakin$elm_canvas$Canvas$Internal$Canvas$Circle, pos, radius);
	});
var author$project$Main$viewToken = F3(
	function (highlighted, trans, t) {
		var d = t.a;
		var strokeSize = d.foe ? 5.0e-2 : 0;
		var stroke = _Utils_eq(highlighted, d.id) ? A3(avh4$elm_color$Color$rgb, 1, 1, 1) : (d.foe ? A3(avh4$elm_color$Color$rgb, 1, 0, 0) : A4(avh4$elm_color$Color$rgba, 0, 0, 0, 0));
		return A2(
			joakin$elm_canvas$Canvas$shapes,
			_Utils_ap(
				trans,
				_List_fromArray(
					[
						joakin$elm_canvas$Canvas$Settings$fill(d.color),
						joakin$elm_canvas$Canvas$Settings$stroke(stroke),
						joakin$elm_canvas$Canvas$Settings$Line$lineWidth(strokeSize)
					])),
			_List_fromArray(
				[
					A2(
					joakin$elm_canvas$Canvas$circle,
					_Utils_Tuple2(d.x, d.y),
					d.radius)
				]));
	});
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		elm$core$String$fromInt(n));
};
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var elm$html$Html$Attributes$height = function (n) {
	return A2(
		_VirtualDom_attribute,
		'height',
		elm$core$String$fromInt(n));
};
var elm$html$Html$Attributes$width = function (n) {
	return A2(
		_VirtualDom_attribute,
		'width',
		elm$core$String$fromInt(n));
};
var elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var elm$html$Html$Keyed$node = elm$virtual_dom$VirtualDom$keyedNode;
var elm$html$Html$canvas = _VirtualDom_node('canvas');
var joakin$elm_canvas$Canvas$cnvs = A2(elm$html$Html$canvas, _List_Nil, _List_Nil);
var elm$core$Basics$cos = _Basics_cos;
var elm$core$Basics$sin = _Basics_sin;
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$arcTo = F5(
	function (x1, y1, x2, y2, radius) {
		return A2(
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'arcTo',
			_List_fromArray(
				[
					elm$json$Json$Encode$float(x1),
					elm$json$Json$Encode$float(y1),
					elm$json$Json$Encode$float(x2),
					elm$json$Json$Encode$float(y2),
					elm$json$Json$Encode$float(radius)
				]));
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$bezierCurveTo = F6(
	function (cp1x, cp1y, cp2x, cp2y, x, y) {
		return A2(
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'bezierCurveTo',
			_List_fromArray(
				[
					elm$json$Json$Encode$float(cp1x),
					elm$json$Json$Encode$float(cp1y),
					elm$json$Json$Encode$float(cp2x),
					elm$json$Json$Encode$float(cp2y),
					elm$json$Json$Encode$float(x),
					elm$json$Json$Encode$float(y)
				]));
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$lineTo = F2(
	function (x, y) {
		return A2(
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'lineTo',
			_List_fromArray(
				[
					elm$json$Json$Encode$float(x),
					elm$json$Json$Encode$float(y)
				]));
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$moveTo = F2(
	function (x, y) {
		return A2(
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'moveTo',
			_List_fromArray(
				[
					elm$json$Json$Encode$float(x),
					elm$json$Json$Encode$float(y)
				]));
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$quadraticCurveTo = F4(
	function (cpx, cpy, x, y) {
		return A2(
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'quadraticCurveTo',
			_List_fromArray(
				[
					elm$json$Json$Encode$float(cpx),
					elm$json$Json$Encode$float(cpy),
					elm$json$Json$Encode$float(x),
					elm$json$Json$Encode$float(y)
				]));
	});
var joakin$elm_canvas$Canvas$renderLineSegment = F2(
	function (segment, cmds) {
		switch (segment.$) {
			case 'ArcTo':
				var _n1 = segment.a;
				var x = _n1.a;
				var y = _n1.b;
				var _n2 = segment.b;
				var x2 = _n2.a;
				var y2 = _n2.b;
				var radius = segment.c;
				return A2(
					elm$core$List$cons,
					A5(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$arcTo, x, y, x2, y2, radius),
					cmds);
			case 'BezierCurveTo':
				var _n3 = segment.a;
				var cp1x = _n3.a;
				var cp1y = _n3.b;
				var _n4 = segment.b;
				var cp2x = _n4.a;
				var cp2y = _n4.b;
				var _n5 = segment.c;
				var x = _n5.a;
				var y = _n5.b;
				return A2(
					elm$core$List$cons,
					A6(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$bezierCurveTo, cp1x, cp1y, cp2x, cp2y, x, y),
					cmds);
			case 'LineTo':
				var _n6 = segment.a;
				var x = _n6.a;
				var y = _n6.b;
				return A2(
					elm$core$List$cons,
					A2(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$lineTo, x, y),
					cmds);
			case 'MoveTo':
				var _n7 = segment.a;
				var x = _n7.a;
				var y = _n7.b;
				return A2(
					elm$core$List$cons,
					A2(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$moveTo, x, y),
					cmds);
			default:
				var _n8 = segment.a;
				var cpx = _n8.a;
				var cpy = _n8.b;
				var _n9 = segment.b;
				var x = _n9.a;
				var y = _n9.b;
				return A2(
					elm$core$List$cons,
					A4(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$quadraticCurveTo, cpx, cpy, x, y),
					cmds);
		}
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$arc = F6(
	function (x, y, radius, startAngle, endAngle, anticlockwise) {
		return A2(
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'arc',
			_List_fromArray(
				[
					elm$json$Json$Encode$float(x),
					elm$json$Json$Encode$float(y),
					elm$json$Json$Encode$float(radius),
					elm$json$Json$Encode$float(startAngle),
					elm$json$Json$Encode$float(endAngle),
					elm$json$Json$Encode$bool(anticlockwise)
				]));
	});
var elm$core$Basics$pi = _Basics_pi;
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$circle = F3(
	function (x, y, r) {
		return A6(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$arc, x, y, r, 0, 2 * elm$core$Basics$pi, false);
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$rect = F4(
	function (x, y, w, h) {
		return A2(
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'rect',
			_List_fromArray(
				[
					elm$json$Json$Encode$float(x),
					elm$json$Json$Encode$float(y),
					elm$json$Json$Encode$float(w),
					elm$json$Json$Encode$float(h)
				]));
	});
var joakin$elm_canvas$Canvas$renderShape = F2(
	function (shape, cmds) {
		switch (shape.$) {
			case 'Rect':
				var _n1 = shape.a;
				var x = _n1.a;
				var y = _n1.b;
				var w = shape.b;
				var h = shape.c;
				return A2(
					elm$core$List$cons,
					A4(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$rect, x, y, w, h),
					A2(
						elm$core$List$cons,
						A2(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$moveTo, x, y),
						cmds));
			case 'Circle':
				var _n2 = shape.a;
				var x = _n2.a;
				var y = _n2.b;
				var r = shape.b;
				return A2(
					elm$core$List$cons,
					A3(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$circle, x, y, r),
					A2(
						elm$core$List$cons,
						A2(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$moveTo, x + r, y),
						cmds));
			case 'Path':
				var _n3 = shape.a;
				var x = _n3.a;
				var y = _n3.b;
				var segments = shape.b;
				return A3(
					elm$core$List$foldl,
					joakin$elm_canvas$Canvas$renderLineSegment,
					A2(
						elm$core$List$cons,
						A2(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$moveTo, x, y),
						cmds),
					segments);
			default:
				var _n4 = shape.a;
				var x = _n4.a;
				var y = _n4.b;
				var radius = shape.b;
				var startAngle = shape.c;
				var endAngle = shape.d;
				var anticlockwise = shape.e;
				return A2(
					elm$core$List$cons,
					A6(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$arc, x, y, radius, startAngle, endAngle, anticlockwise),
					A2(
						elm$core$List$cons,
						A2(
							joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$moveTo,
							x + elm$core$Basics$cos(startAngle),
							y + elm$core$Basics$sin(startAngle)),
						cmds));
		}
	});
var avh4$elm_color$Color$black = A4(avh4$elm_color$Color$RgbaSpace, 0 / 255, 0 / 255, 0 / 255, 1.0);
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$NonZero = {$: 'NonZero'};
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillRuleToString = function (fillRule) {
	if (fillRule.$ === 'NonZero') {
		return 'nonzero';
	} else {
		return 'evenodd';
	}
};
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fill = function (fillRule) {
	return A2(
		joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
		'fill',
		_List_fromArray(
			[
				elm$json$Json$Encode$string(
				joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillRuleToString(fillRule))
			]));
};
var elm$core$String$concat = function (strings) {
	return A2(elm$core$String$join, '', strings);
};
var avh4$elm_color$Color$toCssString = function (_n0) {
	var r = _n0.a;
	var g = _n0.b;
	var b = _n0.c;
	var a = _n0.d;
	var roundTo = function (x) {
		return elm$core$Basics$round(x * 1000) / 1000;
	};
	var pct = function (x) {
		return elm$core$Basics$round(x * 10000) / 100;
	};
	return elm$core$String$concat(
		_List_fromArray(
			[
				'rgba(',
				elm$core$String$fromFloat(
				pct(r)),
				'%,',
				elm$core$String$fromFloat(
				pct(g)),
				'%,',
				elm$core$String$fromFloat(
				pct(b)),
				'%,',
				elm$core$String$fromFloat(
				roundTo(a)),
				')'
			]));
};
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillStyle = function (color) {
	return A2(
		joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$field,
		'fillStyle',
		elm$json$Json$Encode$string(
			avh4$elm_color$Color$toCssString(color)));
};
var joakin$elm_canvas$Canvas$renderShapeFill = F2(
	function (c, cmds) {
		return A2(
			elm$core$List$cons,
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fill(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$NonZero),
			A2(
				elm$core$List$cons,
				joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillStyle(c),
				cmds));
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$stroke = A2(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn, 'stroke', _List_Nil);
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$strokeStyle = function (color) {
	return A2(
		joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$field,
		'strokeStyle',
		elm$json$Json$Encode$string(
			avh4$elm_color$Color$toCssString(color)));
};
var joakin$elm_canvas$Canvas$renderShapeStroke = F2(
	function (c, cmds) {
		return A2(
			elm$core$List$cons,
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$stroke,
			A2(
				elm$core$List$cons,
				joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$strokeStyle(c),
				cmds));
	});
var joakin$elm_canvas$Canvas$renderShapeDrawOp = F2(
	function (drawOp, cmds) {
		switch (drawOp.$) {
			case 'NotSpecified':
				return A2(joakin$elm_canvas$Canvas$renderShapeFill, avh4$elm_color$Color$black, cmds);
			case 'Fill':
				var c = drawOp.a;
				return A2(joakin$elm_canvas$Canvas$renderShapeFill, c, cmds);
			case 'Stroke':
				var c = drawOp.a;
				return A2(joakin$elm_canvas$Canvas$renderShapeStroke, c, cmds);
			default:
				var fc = drawOp.a;
				var sc = drawOp.b;
				return A2(
					joakin$elm_canvas$Canvas$renderShapeStroke,
					sc,
					A2(joakin$elm_canvas$Canvas$renderShapeFill, fc, cmds));
		}
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillText = F4(
	function (text, x, y, maybeMaxWidth) {
		if (maybeMaxWidth.$ === 'Nothing') {
			return A2(
				joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
				'fillText',
				_List_fromArray(
					[
						elm$json$Json$Encode$string(text),
						elm$json$Json$Encode$float(x),
						elm$json$Json$Encode$float(y)
					]));
		} else {
			var maxWidth = maybeMaxWidth.a;
			return A2(
				joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
				'fillText',
				_List_fromArray(
					[
						elm$json$Json$Encode$string(text),
						elm$json$Json$Encode$float(x),
						elm$json$Json$Encode$float(y),
						elm$json$Json$Encode$float(maxWidth)
					]));
		}
	});
var joakin$elm_canvas$Canvas$renderTextFill = F5(
	function (txt, x, y, color, cmds) {
		return A2(
			elm$core$List$cons,
			A4(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillText, txt.text, x, y, txt.maxWidth),
			A2(
				elm$core$List$cons,
				joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillStyle(color),
				cmds));
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$strokeText = F4(
	function (text, x, y, maybeMaxWidth) {
		if (maybeMaxWidth.$ === 'Nothing') {
			return A2(
				joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
				'strokeText',
				_List_fromArray(
					[
						elm$json$Json$Encode$string(text),
						elm$json$Json$Encode$float(x),
						elm$json$Json$Encode$float(y)
					]));
		} else {
			var maxWidth = maybeMaxWidth.a;
			return A2(
				joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
				'strokeText',
				_List_fromArray(
					[
						elm$json$Json$Encode$string(text),
						elm$json$Json$Encode$float(x),
						elm$json$Json$Encode$float(y),
						elm$json$Json$Encode$float(maxWidth)
					]));
		}
	});
var joakin$elm_canvas$Canvas$renderTextStroke = F5(
	function (txt, x, y, color, cmds) {
		return A2(
			elm$core$List$cons,
			A4(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$strokeText, txt.text, x, y, txt.maxWidth),
			A2(
				elm$core$List$cons,
				joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$strokeStyle(color),
				cmds));
	});
var joakin$elm_canvas$Canvas$renderTextDrawOp = F3(
	function (drawOp, txt, cmds) {
		var _n0 = txt.point;
		var x = _n0.a;
		var y = _n0.b;
		switch (drawOp.$) {
			case 'NotSpecified':
				return A5(joakin$elm_canvas$Canvas$renderTextFill, txt, x, y, avh4$elm_color$Color$black, cmds);
			case 'Fill':
				var c = drawOp.a;
				return A5(joakin$elm_canvas$Canvas$renderTextFill, txt, x, y, c, cmds);
			case 'Stroke':
				var c = drawOp.a;
				return A5(joakin$elm_canvas$Canvas$renderTextStroke, txt, x, y, c, cmds);
			default:
				var fc = drawOp.a;
				var sc = drawOp.b;
				return A5(
					joakin$elm_canvas$Canvas$renderTextStroke,
					txt,
					x,
					y,
					sc,
					A5(joakin$elm_canvas$Canvas$renderTextFill, txt, x, y, fc, cmds));
		}
	});
var joakin$elm_canvas$Canvas$renderText = F3(
	function (drawOp, txt, cmds) {
		return A3(joakin$elm_canvas$Canvas$renderTextDrawOp, drawOp, txt, cmds);
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$drawImage = F9(
	function (sx, sy, sw, sh, dx, dy, dw, dh, imageObj) {
		return A2(
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'drawImage',
			_List_fromArray(
				[
					imageObj,
					elm$json$Json$Encode$float(sx),
					elm$json$Json$Encode$float(sy),
					elm$json$Json$Encode$float(sw),
					elm$json$Json$Encode$float(sh),
					elm$json$Json$Encode$float(dx),
					elm$json$Json$Encode$float(dy),
					elm$json$Json$Encode$float(dw),
					elm$json$Json$Encode$float(dh)
				]));
	});
var joakin$elm_canvas$Canvas$Internal$Texture$drawTexture = F4(
	function (x, y, t, cmds) {
		return A2(
			elm$core$List$cons,
			function () {
				if (t.$ === 'TImage') {
					var image = t.a;
					return A9(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$drawImage, 0, 0, image.width, image.height, x, y, image.width, image.height, image.json);
				} else {
					var sprite = t.a;
					var image = t.b;
					return A9(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$drawImage, sprite.x, sprite.y, sprite.width, sprite.height, x, y, sprite.width, sprite.height, image.json);
				}
			}(),
			cmds);
	});
var joakin$elm_canvas$Canvas$renderTexture = F3(
	function (_n0, t, cmds) {
		var x = _n0.a;
		var y = _n0.b;
		return A4(joakin$elm_canvas$Canvas$Internal$Texture$drawTexture, x, y, t, cmds);
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$beginPath = A2(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn, 'beginPath', _List_Nil);
var joakin$elm_canvas$Canvas$renderDrawable = F3(
	function (drawable, drawOp, cmds) {
		switch (drawable.$) {
			case 'DrawableText':
				var txt = drawable.a;
				return A3(joakin$elm_canvas$Canvas$renderText, drawOp, txt, cmds);
			case 'DrawableShapes':
				var ss = drawable.a;
				return A2(
					joakin$elm_canvas$Canvas$renderShapeDrawOp,
					drawOp,
					A3(
						elm$core$List$foldl,
						joakin$elm_canvas$Canvas$renderShape,
						A2(elm$core$List$cons, joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$beginPath, cmds),
						ss));
			default:
				var p = drawable.a;
				var t = drawable.b;
				return A3(joakin$elm_canvas$Canvas$renderTexture, p, t, cmds);
		}
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$restore = A2(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn, 'restore', _List_Nil);
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$save = A2(joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn, 'save', _List_Nil);
var joakin$elm_canvas$Canvas$renderOne = F2(
	function (_n0, cmds) {
		var data = _n0.a;
		var commands = data.commands;
		var drawable = data.drawable;
		var drawOp = data.drawOp;
		return A2(
			elm$core$List$cons,
			joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$restore,
			A3(
				joakin$elm_canvas$Canvas$renderDrawable,
				drawable,
				drawOp,
				_Utils_ap(
					commands,
					A2(elm$core$List$cons, joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$save, cmds))));
	});
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$empty = _List_Nil;
var joakin$elm_canvas$Canvas$render = function (entities) {
	return A3(elm$core$List$foldl, joakin$elm_canvas$Canvas$renderOne, joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$empty, entities);
};
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var joakin$elm_canvas$Canvas$decodeTextureImageInfo = A2(
	elm$json$Json$Decode$andThen,
	function (target) {
		return A3(
			elm$json$Json$Decode$map2,
			F2(
				function (width, height) {
					return {height: height, json: target, width: width};
				}),
			A2(
				elm$json$Json$Decode$at,
				_List_fromArray(
					['target', 'width']),
				elm$json$Json$Decode$float),
			A2(
				elm$json$Json$Decode$at,
				_List_fromArray(
					['target', 'height']),
				elm$json$Json$Decode$float));
	},
	A2(elm$json$Json$Decode$field, 'target', elm$json$Json$Decode$value));
var joakin$elm_canvas$Canvas$Internal$Texture$TImage = function (a) {
	return {$: 'TImage', a: a};
};
var joakin$elm_canvas$Canvas$renderTextureSource = function (textureSource) {
	var url = textureSource.a;
	var onLoad = textureSource.b;
	return _Utils_Tuple2(
		url,
		A2(
			elm$html$Html$img,
			_List_fromArray(
				[
					elm$html$Html$Attributes$src(url),
					A2(elm$html$Html$Attributes$style, 'display', 'none'),
					A2(
					elm$html$Html$Events$on,
					'load',
					A2(
						elm$json$Json$Decode$map,
						A2(
							elm$core$Basics$composeR,
							joakin$elm_canvas$Canvas$Internal$Texture$TImage,
							A2(elm$core$Basics$composeR, elm$core$Maybe$Just, onLoad)),
						joakin$elm_canvas$Canvas$decodeTextureImageInfo)),
					A2(
					elm$html$Html$Events$on,
					'error',
					elm$json$Json$Decode$succeed(
						onLoad(elm$core$Maybe$Nothing)))
				]),
			_List_Nil));
};
var elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var elm$html$Html$Attributes$property = elm$virtual_dom$VirtualDom$property;
var joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$commands = function (list) {
	return A2(
		elm$html$Html$Attributes$property,
		'cmds',
		A2(elm$json$Json$Encode$list, elm$core$Basics$identity, list));
};
var joakin$elm_canvas$Canvas$toHtmlWith = F3(
	function (options, attrs, entities) {
		return A3(
			elm$html$Html$Keyed$node,
			'elm-canvas',
			A2(
				elm$core$List$cons,
				joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$commands(
					joakin$elm_canvas$Canvas$render(entities)),
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$height(options.height),
					A2(
						elm$core$List$cons,
						elm$html$Html$Attributes$width(options.width),
						attrs))),
			A2(
				elm$core$List$cons,
				_Utils_Tuple2('__canvas', joakin$elm_canvas$Canvas$cnvs),
				A2(elm$core$List$map, joakin$elm_canvas$Canvas$renderTextureSource, options.textures)));
	});
var joakin$elm_canvas$Canvas$toHtml = F3(
	function (_n0, attrs, entities) {
		var w = _n0.a;
		var h = _n0.b;
		return A3(
			joakin$elm_canvas$Canvas$toHtmlWith,
			{height: h, textures: _List_Nil, width: w},
			attrs,
			entities);
	});
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions = {preventDefault: true, stopPropagation: false};
var elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 'Custom', a: a};
};
var elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$Event = F6(
	function (keys, button, clientPos, offsetPos, pagePos, screenPos) {
		return {button: button, clientPos: clientPos, keys: keys, offsetPos: offsetPos, pagePos: pagePos, screenPos: screenPos};
	});
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$BackButton = {$: 'BackButton'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ErrorButton = {$: 'ErrorButton'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ForwardButton = {$: 'ForwardButton'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MainButton = {$: 'MainButton'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MiddleButton = {$: 'MiddleButton'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$SecondButton = {$: 'SecondButton'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonFromId = function (id) {
	switch (id) {
		case 0:
			return mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MainButton;
		case 1:
			return mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MiddleButton;
		case 2:
			return mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$SecondButton;
		case 3:
			return mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$BackButton;
		case 4:
			return mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ForwardButton;
		default:
			return mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ErrorButton;
	}
};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonDecoder = A2(
	elm$json$Json$Decode$map,
	mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonFromId,
	A2(elm$json$Json$Decode$field, 'button', elm$json$Json$Decode$int));
var mpizenberg$elm_pointer_events$Internal$Decode$clientPos = A3(
	elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2(elm$json$Json$Decode$field, 'clientX', elm$json$Json$Decode$float),
	A2(elm$json$Json$Decode$field, 'clientY', elm$json$Json$Decode$float));
var mpizenberg$elm_pointer_events$Internal$Decode$Keys = F3(
	function (alt, ctrl, shift) {
		return {alt: alt, ctrl: ctrl, shift: shift};
	});
var mpizenberg$elm_pointer_events$Internal$Decode$keys = A4(
	elm$json$Json$Decode$map3,
	mpizenberg$elm_pointer_events$Internal$Decode$Keys,
	A2(elm$json$Json$Decode$field, 'altKey', elm$json$Json$Decode$bool),
	A2(elm$json$Json$Decode$field, 'ctrlKey', elm$json$Json$Decode$bool),
	A2(elm$json$Json$Decode$field, 'shiftKey', elm$json$Json$Decode$bool));
var mpizenberg$elm_pointer_events$Internal$Decode$offsetPos = A3(
	elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2(elm$json$Json$Decode$field, 'offsetX', elm$json$Json$Decode$float),
	A2(elm$json$Json$Decode$field, 'offsetY', elm$json$Json$Decode$float));
var mpizenberg$elm_pointer_events$Internal$Decode$pagePos = A3(
	elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2(elm$json$Json$Decode$field, 'pageX', elm$json$Json$Decode$float),
	A2(elm$json$Json$Decode$field, 'pageY', elm$json$Json$Decode$float));
var mpizenberg$elm_pointer_events$Internal$Decode$screenPos = A3(
	elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2(elm$json$Json$Decode$field, 'screenX', elm$json$Json$Decode$float),
	A2(elm$json$Json$Decode$field, 'screenY', elm$json$Json$Decode$float));
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$eventDecoder = A7(elm$json$Json$Decode$map6, mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$Event, mpizenberg$elm_pointer_events$Internal$Decode$keys, mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonDecoder, mpizenberg$elm_pointer_events$Internal$Decode$clientPos, mpizenberg$elm_pointer_events$Internal$Decode$offsetPos, mpizenberg$elm_pointer_events$Internal$Decode$pagePos, mpizenberg$elm_pointer_events$Internal$Decode$screenPos);
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions = F3(
	function (event, options, tag) {
		return A2(
			elm$html$Html$Events$custom,
			event,
			A2(
				elm$json$Json$Decode$map,
				function (ev) {
					return {
						message: tag(ev),
						preventDefault: options.preventDefault,
						stopPropagation: options.stopPropagation
					};
				},
				mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$eventDecoder));
	});
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onDown = A2(mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions, 'mousedown', mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions);
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onMove = A2(mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions, 'mousemove', mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions);
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onUp = A2(mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions, 'mouseup', mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions);
var mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$defaultOptions = {preventDefault: true, stopPropagation: false};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$Event = F3(
	function (mouseEvent, deltaY, deltaMode) {
		return {deltaMode: deltaMode, deltaY: deltaY, mouseEvent: mouseEvent};
	});
var mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$DeltaLine = {$: 'DeltaLine'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$DeltaPage = {$: 'DeltaPage'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$DeltaPixel = {$: 'DeltaPixel'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$deltaModeDecoder = function () {
	var intToMode = function (_int) {
		switch (_int) {
			case 1:
				return mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$DeltaLine;
			case 2:
				return mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$DeltaPage;
			default:
				return mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$DeltaPixel;
		}
	};
	return A2(elm$json$Json$Decode$map, intToMode, elm$json$Json$Decode$int);
}();
var mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$eventDecoder = A4(
	elm$json$Json$Decode$map3,
	mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$Event,
	mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$eventDecoder,
	A2(elm$json$Json$Decode$field, 'deltaY', elm$json$Json$Decode$float),
	A2(elm$json$Json$Decode$field, 'deltaMode', mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$deltaModeDecoder));
var mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$onWithOptions = F2(
	function (options, tag) {
		return A2(
			elm$html$Html$Events$custom,
			'wheel',
			A2(
				elm$json$Json$Decode$map,
				function (ev) {
					return {
						message: tag(ev),
						preventDefault: options.preventDefault,
						stopPropagation: options.stopPropagation
					};
				},
				mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$eventDecoder));
	});
var mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$onWheel = mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$onWithOptions(mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$defaultOptions);
var author$project$Main$view = function (model) {
	var dim = author$project$Main$dimensionsFromModel(model);
	var trans = A2(author$project$Main$canvasTransform, dim, model);
	return {
		body: A2(
			elm$core$List$append,
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$id('main-screen')
						]),
					_List_fromArray(
						[
							A2(
							elm$html$Html$div,
							_List_fromArray(
								[
									elm$html$Html$Attributes$id('toolbar'),
									A2(
									elm$html$Html$Attributes$style,
									'height',
									elm$core$String$fromInt(dim.toolbarHeight)),
									A2(
									elm$html$Html$Attributes$style,
									'width',
									elm$core$String$fromInt(dim.toolbarWidth))
								]),
							author$project$Main$isGm(model) ? _List_fromArray(
								[
									A3(
									author$project$Main$radioButton,
									author$project$Main$MsgSetCreateMode(author$project$Main$ModeCreateToken),
									'images/circle.png',
									_Utils_eq(model.createMode, author$project$Main$ModeCreateToken)),
									A3(
									author$project$Main$radioButton,
									author$project$Main$MsgSetCreateMode(author$project$Main$ModeCreateLine),
									'images/line.png',
									_Utils_eq(model.createMode, author$project$Main$ModeCreateLine)),
									A2(
									elm$html$Html$button,
									_List_fromArray(
										[
											elm$html$Html$Attributes$id('button-clear-doodads'),
											elm$html$Html$Events$onClick(author$project$Main$MsgSendClearDoodads)
										]),
									_List_fromArray(
										[
											elm$html$Html$text('Clear Doodads')
										])),
									A2(
									elm$html$Html$button,
									_List_fromArray(
										[
											elm$html$Html$Attributes$id('button-clear-tokens'),
											elm$html$Html$Events$onClick(author$project$Main$MsgSendClearTokens)
										]),
									_List_fromArray(
										[
											elm$html$Html$text('Clear Tokens')
										]))
								]) : _List_Nil),
							A3(
							joakin$elm_canvas$Canvas$toHtml,
							_Utils_Tuple2(dim.canvasWidth, dim.canvasHeight),
							_List_fromArray(
								[
									mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onDown(
									function (event) {
										return author$project$Main$MousePress(event);
									}),
									mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onMove(
									function (event) {
										return author$project$Main$MouseMotion(event);
									}),
									mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onUp(
									function (event) {
										return author$project$Main$MouseRelease(event);
									}),
									mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$onWheel(
									function (event) {
										return author$project$Main$MouseWheel(event);
									}),
									A2(
									elm$html$Html$Events$on,
									'keydown',
									A2(elm$json$Json$Decode$map, author$project$Main$KeyDown, Gizra$elm_keyboard_event$Keyboard$Event$decodeKeyboardEvent)),
									elm$html$Html$Attributes$tabindex(0),
									elm$html$Html$Attributes$id('canvas')
								]),
							elm$core$List$concat(
								_List_fromArray(
									[
										_List_fromArray(
										[
											A2(author$project$Main$clearCanvas, dim.canvasWidth, dim.canvasHeight)
										]),
										_List_fromArray(
										[
											A3(author$project$Main$viewGrid, 5, model, dim)
										]),
										A2(
										elm$core$List$map,
										author$project$Main$viewDoodad(trans),
										model.doodads),
										A2(author$project$Main$viewCurrentDoodad, trans, model),
										A2(
										elm$core$List$map,
										A2(author$project$Main$viewToken, model.selected, trans),
										model.tokens),
										A3(author$project$Main$viewDistanceLine, model, dim, trans)
									])))
						])),
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$id('right-bar')
						]),
					_List_fromArray(
						[
							A2(
							elm$html$Html$div,
							_List_fromArray(
								[
									elm$html$Html$Attributes$id('area-tools'),
									A2(
									elm$html$Html$Attributes$style,
									'height',
									elm$core$String$fromInt(dim.toolsHeight)),
									A2(
									elm$html$Html$Attributes$style,
									'width',
									elm$core$String$fromInt(dim.toolsWidth))
								]),
							_List_Nil),
							A2(
							elm$html$Html$div,
							_List_fromArray(
								[
									elm$html$Html$Attributes$id('chat-area'),
									A2(
									elm$html$Html$Attributes$style,
									'height',
									elm$core$String$fromInt(dim.chatHeight)),
									A2(
									elm$html$Html$Attributes$style,
									'width',
									elm$core$String$fromInt(dim.chatWidth))
								]),
							_List_fromArray(
								[
									A2(
									elm$html$Html$div,
									_List_fromArray(
										[
											elm$html$Html$Attributes$id('chat-text')
										]),
									author$project$Main$viewChat(model)),
									A2(
									elm$html$Html$input,
									_List_fromArray(
										[
											elm$html$Html$Attributes$id('chat-input'),
											elm$html$Html$Attributes$value(model.chatText),
											elm$html$Html$Events$onInput(author$project$Main$ChatInput),
											A2(
											elm$html$Html$Events$on,
											'keydown',
											A2(elm$json$Json$Decode$map, author$project$Main$ChatKeyDown, Gizra$elm_keyboard_event$Keyboard$Event$decodeKeyboardEvent))
										]),
									_List_Nil)
								]))
						]))
				]),
			author$project$Main$viewSetUsername(model)),
		title: 'Goats Rock'
	};
};
var elm$browser$Browser$document = _Browser_document;
var elm$json$Json$Decode$index = _Json_decodeIndex;
var author$project$Main$main = elm$browser$Browser$document(
	{init: author$project$Main$init, subscriptions: author$project$Main$subscriptions, update: author$project$Main$update, view: author$project$Main$view});
_Platform_export({'Main':{'init':author$project$Main$main(
	A2(
		elm$json$Json$Decode$andThen,
		function (x0) {
			return A2(
				elm$json$Json$Decode$andThen,
				function (x1) {
					return A2(
						elm$json$Json$Decode$andThen,
						function (x2) {
							return elm$json$Json$Decode$succeed(
								_Utils_Tuple3(x0, x1, x2));
						},
						A2(elm$json$Json$Decode$index, 2, elm$json$Json$Decode$string));
				},
				A2(elm$json$Json$Decode$index, 1, elm$json$Json$Decode$int));
		},
		A2(elm$json$Json$Decode$index, 0, elm$json$Json$Decode$int)))(0)}});}(this));