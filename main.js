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
var elm$core$Basics$False = {$: 'False'};
var elm$core$Basics$True = {$: 'True'};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
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
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
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
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var author$project$Main$subscriptions = function (_n0) {
	return elm$core$Platform$Sub$none;
};
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$MapEditor$init = function (_n0) {
	var windowWidth = _n0.a;
	var windowHeight = _n0.b;
	return _Utils_Tuple2(
		{dialog: elm$core$Maybe$Nothing, height: 10, layerSelection: elm$core$Maybe$Nothing, layers: _List_Nil, name: 'untitled', width: 10, windowHeight: windowHeight, windowWidth: windowWidth},
		elm$core$Platform$Cmd$none);
};
var author$project$DiscreteGradientEditor$State = function (a) {
	return {$: 'State', a: a};
};
var author$project$DiscreteGradient$DiscreteGradient = function (a) {
	return {$: 'DiscreteGradient', a: a};
};
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
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var author$project$DiscreteGradient$stopsAbove = function (value) {
	return elm$core$List$filter(
		function (s) {
			return _Utils_cmp(s.value, value) > 0;
		});
};
var author$project$DiscreteGradient$stopsBelow = function (value) {
	return elm$core$List$filter(
		function (s) {
			return _Utils_cmp(s.value, value) < 0;
		});
};
var elm$core$Basics$identity = function (x) {
	return x;
};
var author$project$DiscreteGradient$addStop = F2(
	function (stop, _n0) {
		var stops = _n0.a;
		return author$project$DiscreteGradient$DiscreteGradient(
			_Utils_ap(
				A2(author$project$DiscreteGradient$stopsBelow, stop.value, stops),
				_Utils_ap(
					_List_fromArray(
						[stop]),
					A2(author$project$DiscreteGradient$stopsAbove, stop.value, stops))));
	});
var author$project$DiscreteGradient$fractionBetween = F3(
	function (lower, upper, x) {
		var xF = x;
		var upperF = upper;
		var lowerF = lower;
		return (xF - lowerF) / (upperF - lowerF);
	});
var author$project$DiscreteGradient$interpolateFloat = F3(
	function (lower, upper, factor) {
		return lower + ((upper - lower) * factor);
	});
var avh4$elm_color$Color$RgbaSpace = F4(
	function (a, b, c, d) {
		return {$: 'RgbaSpace', a: a, b: b, c: c, d: d};
	});
var avh4$elm_color$Color$fromRgba = function (components) {
	return A4(avh4$elm_color$Color$RgbaSpace, components.red, components.green, components.blue, components.alpha);
};
var avh4$elm_color$Color$toRgba = function (_n0) {
	var r = _n0.a;
	var g = _n0.b;
	var b = _n0.c;
	var a = _n0.d;
	return {alpha: a, blue: b, green: g, red: r};
};
var author$project$DiscreteGradient$interpolateRgba = F3(
	function (lower, upper, factor) {
		var upperRgba = avh4$elm_color$Color$toRgba(upper);
		var lowerRgba = avh4$elm_color$Color$toRgba(lower);
		return avh4$elm_color$Color$fromRgba(
			{
				alpha: A3(author$project$DiscreteGradient$interpolateFloat, lowerRgba.alpha, upperRgba.alpha, factor),
				blue: A3(author$project$DiscreteGradient$interpolateFloat, lowerRgba.blue, upperRgba.blue, factor),
				green: A3(author$project$DiscreteGradient$interpolateFloat, lowerRgba.green, upperRgba.green, factor),
				red: A3(author$project$DiscreteGradient$interpolateFloat, lowerRgba.red, upperRgba.red, factor)
			});
	});
var author$project$DiscreteGradient$interpolateColor = author$project$DiscreteGradient$interpolateRgba;
var author$project$DiscreteGradient$interpolateBetween = F3(
	function (lower, upper, value) {
		if (_Utils_eq(lower, upper)) {
			return lower.color;
		} else {
			var factor = A3(author$project$DiscreteGradient$fractionBetween, lower.value, upper.value, value);
			return A3(author$project$DiscreteGradient$interpolateColor, lower.color, upper.color, factor);
		}
	});
var elm$core$Basics$ge = _Utils_ge;
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var author$project$DiscreteGradient$nearestStopAtOrAbove = F2(
	function (value, _n0) {
		var stops = _n0.a;
		return elm$core$List$head(
			A2(
				elm$core$List$filter,
				function (s) {
					return _Utils_cmp(s.value, value) > -1;
				},
				stops));
	});
var author$project$DiscreteGradient$nearestStopAtOrUnder = F2(
	function (value, _n0) {
		var stops = _n0.a;
		return elm$core$List$head(
			elm$core$List$reverse(
				A2(
					elm$core$List$filter,
					function (s) {
						return _Utils_cmp(s.value, value) < 1;
					},
					stops)));
	});
var avh4$elm_color$Color$black = A4(avh4$elm_color$Color$RgbaSpace, 0 / 255, 0 / 255, 0 / 255, 1.0);
var author$project$DiscreteGradient$getColorAt = F2(
	function (value, gradient) {
		var maybeUpperStop = A2(author$project$DiscreteGradient$nearestStopAtOrAbove, value, gradient);
		var maybeLowerStop = A2(author$project$DiscreteGradient$nearestStopAtOrUnder, value, gradient);
		var _n0 = _Utils_Tuple2(maybeLowerStop, maybeUpperStop);
		if (_n0.a.$ === 'Just') {
			if (_n0.b.$ === 'Just') {
				var lower = _n0.a.a;
				var upper = _n0.b.a;
				return A3(author$project$DiscreteGradient$interpolateBetween, lower, upper, value);
			} else {
				var lower = _n0.a.a;
				var _n1 = _n0.b;
				return lower.color;
			}
		} else {
			if (_n0.b.$ === 'Just') {
				var _n2 = _n0.a;
				var upper = _n0.b.a;
				return upper.color;
			} else {
				var _n3 = _n0.a;
				var _n4 = _n0.b;
				return avh4$elm_color$Color$black;
			}
		}
	});
var author$project$DiscreteGradient$removeStopAt = F2(
	function (value, _n0) {
		var stops = _n0.a;
		return (elm$core$List$length(stops) > 1) ? author$project$DiscreteGradient$DiscreteGradient(
			_Utils_ap(
				A2(author$project$DiscreteGradient$stopsBelow, value, stops),
				A2(author$project$DiscreteGradient$stopsAbove, value, stops))) : author$project$DiscreteGradient$DiscreteGradient(stops);
	});
var author$project$DiscreteGradientEditor$Cancel = {$: 'Cancel'};
var author$project$DiscreteGradientEditor$Save = function (a) {
	return {$: 'Save', a: a};
};
var author$project$DiscreteGradientEditor$EditInProgress = {$: 'EditInProgress'};
var author$project$DiscreteGradientEditor$andEditInProgress = function (a) {
	return _Utils_Tuple2(a, author$project$DiscreteGradientEditor$EditInProgress);
};
var author$project$DiscreteGradientEditor$floatFrom255 = function (i) {
	return (i < 0) ? 0.0 : ((i > 255) ? 1.0 : (i / 255.0));
};
var author$project$DiscreteGradientEditor$tryColorFromRgb255 = F3(
	function (r, g, b) {
		return ((r >= 0) && ((r <= 255) && ((g >= 0) && ((g <= 255) && ((b >= 0) && (b <= 255)))))) ? elm$core$Maybe$Just(
			avh4$elm_color$Color$fromRgba(
				{
					alpha: 1.0,
					blue: author$project$DiscreteGradientEditor$floatFrom255(b),
					green: author$project$DiscreteGradientEditor$floatFrom255(g),
					red: author$project$DiscreteGradientEditor$floatFrom255(r)
				})) : elm$core$Maybe$Nothing;
	});
var elm$core$Result$toMaybe = function (result) {
	if (result.$ === 'Ok') {
		var v = result.a;
		return elm$core$Maybe$Just(v);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$String$length = _String_length;
var elm$core$String$slice = _String_slice;
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(xs);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return elm$core$Result$Err(
				f(e));
		}
	});
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$core$String$startsWith = _String_startsWith;
var elm$core$String$foldr = _String_foldr;
var elm$core$String$toList = function (string) {
	return A3(elm$core$String$foldr, elm$core$List$cons, _List_Nil, string);
};
var elm$core$Basics$pow = _Basics_pow;
var elm$core$String$cons = _String_cons;
var elm$core$String$fromChar = function (_char) {
	return A2(elm$core$String$cons, _char, '');
};
var rtfeldman$elm_hex$Hex$fromStringHelp = F3(
	function (position, chars, accumulated) {
		fromStringHelp:
		while (true) {
			if (!chars.b) {
				return elm$core$Result$Ok(accumulated);
			} else {
				var _char = chars.a;
				var rest = chars.b;
				switch (_char.valueOf()) {
					case '0':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated;
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '1':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + A2(elm$core$Basics$pow, 16, position);
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '2':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (2 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '3':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (3 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '4':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (4 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '5':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (5 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '6':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (6 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '7':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (7 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '8':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (8 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '9':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (9 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'a':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (10 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'b':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (11 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'c':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (12 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'd':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (13 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'e':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (14 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'f':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (15 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					default:
						var nonHex = _char;
						return elm$core$Result$Err(
							elm$core$String$fromChar(nonHex) + ' is not a valid hexadecimal character.');
				}
			}
		}
	});
var rtfeldman$elm_hex$Hex$fromString = function (str) {
	if (elm$core$String$isEmpty(str)) {
		return elm$core$Result$Err('Empty strings are not valid hexadecimal strings.');
	} else {
		var result = function () {
			if (A2(elm$core$String$startsWith, '-', str)) {
				var list = A2(
					elm$core$Maybe$withDefault,
					_List_Nil,
					elm$core$List$tail(
						elm$core$String$toList(str)));
				return A2(
					elm$core$Result$map,
					elm$core$Basics$negate,
					A3(
						rtfeldman$elm_hex$Hex$fromStringHelp,
						elm$core$List$length(list) - 1,
						list,
						0));
			} else {
				return A3(
					rtfeldman$elm_hex$Hex$fromStringHelp,
					elm$core$String$length(str) - 1,
					elm$core$String$toList(str),
					0);
			}
		}();
		var formatError = function (err) {
			return A2(
				elm$core$String$join,
				' ',
				_List_fromArray(
					['\"' + (str + '\"'), 'is not a valid hexadecimal string because', err]));
		};
		return A2(elm$core$Result$mapError, formatError, result);
	}
};
var author$project$DiscreteGradientEditor$colorFromRgbHex = function (hex) {
	if (elm$core$String$length(hex) === 6) {
		var maybeR = elm$core$Result$toMaybe(
			rtfeldman$elm_hex$Hex$fromString(
				A3(elm$core$String$slice, 0, 2, hex)));
		var maybeG = elm$core$Result$toMaybe(
			rtfeldman$elm_hex$Hex$fromString(
				A3(elm$core$String$slice, 2, 4, hex)));
		var maybeB = elm$core$Result$toMaybe(
			rtfeldman$elm_hex$Hex$fromString(
				A3(elm$core$String$slice, 4, 6, hex)));
		var _n0 = _Utils_Tuple3(maybeR, maybeG, maybeB);
		if (((_n0.a.$ === 'Just') && (_n0.b.$ === 'Just')) && (_n0.c.$ === 'Just')) {
			var r = _n0.a.a;
			var g = _n0.b.a;
			var b = _n0.c.a;
			return A3(author$project$DiscreteGradientEditor$tryColorFromRgb255, r, g, b);
		} else {
			return elm$core$Maybe$Nothing;
		}
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var author$project$DiscreteGradient$getStops = function (_n0) {
	var stops = _n0.a;
	return stops;
};
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
var elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var author$project$DiscreteGradientEditor$hasStopAt = F2(
	function (val, dg) {
		return A2(
			elm$core$List$member,
			val,
			A2(
				elm$core$List$map,
				function (s) {
					return s.value;
				},
				author$project$DiscreteGradient$getStops(dg)));
	});
var author$project$DiscreteGradientEditor$resetSelectedColor = function (model) {
	return _Utils_update(
		model,
		{
			selectedColor: A2(author$project$DiscreteGradient$getColorAt, model.selectedValue, model.gradient)
		});
};
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var author$project$DiscreteGradientEditor$boundInt = F2(
	function (lowerBound, upperBound) {
		return A2(
			elm$core$Basics$composeL,
			elm$core$Basics$min(upperBound),
			elm$core$Basics$max(lowerBound));
	});
var simonh1000$elm_colorpicker$ColorPicker$State = function (a) {
	return {$: 'State', a: a};
};
var simonh1000$elm_colorpicker$ColorPicker$Unpressed = {$: 'Unpressed'};
var simonh1000$elm_colorpicker$ColorPicker$blankModel = {hue: elm$core$Maybe$Nothing, mouseTarget: simonh1000$elm_colorpicker$ColorPicker$Unpressed};
var simonh1000$elm_colorpicker$ColorPicker$empty = simonh1000$elm_colorpicker$ColorPicker$State(simonh1000$elm_colorpicker$ColorPicker$blankModel);
var author$project$DiscreteGradientEditor$selectGradientVal = F2(
	function (model, val) {
		var nextGradientVal = A3(author$project$DiscreteGradientEditor$boundInt, model.gradientMin, model.gradientMax, val);
		return _Utils_update(
			model,
			{
				colorPickerState: simonh1000$elm_colorpicker$ColorPicker$empty,
				selectedColor: A2(author$project$DiscreteGradient$getColorAt, nextGradientVal, model.gradient),
				selectedValue: nextGradientVal
			});
	});
var author$project$DiscreteGradientEditor$setSelectedStopColor = F2(
	function (model, color) {
		return A2(author$project$DiscreteGradientEditor$hasStopAt, model.selectedValue, model.gradient) ? _Utils_update(
			model,
			{
				gradient: A2(
					author$project$DiscreteGradient$addStop,
					{color: color, value: model.selectedValue},
					model.gradient),
				selectedColor: color
			}) : model;
	});
var author$project$DiscreteGradientEditor$updateBlueChannel = F2(
	function (color, blue) {
		var rgba = avh4$elm_color$Color$toRgba(color);
		return avh4$elm_color$Color$fromRgba(
			_Utils_update(
				rgba,
				{
					blue: author$project$DiscreteGradientEditor$floatFrom255(blue)
				}));
	});
var author$project$DiscreteGradientEditor$updateGreenChannel = F2(
	function (color, green) {
		var rgba = avh4$elm_color$Color$toRgba(color);
		return avh4$elm_color$Color$fromRgba(
			_Utils_update(
				rgba,
				{
					green: author$project$DiscreteGradientEditor$floatFrom255(green)
				}));
	});
var avh4$elm_color$Color$hsla = F4(
	function (hue, sat, light, alpha) {
		var _n0 = _Utils_Tuple3(hue, sat, light);
		var h = _n0.a;
		var s = _n0.b;
		var l = _n0.c;
		var m2 = (l <= 0.5) ? (l * (s + 1)) : ((l + s) - (l * s));
		var m1 = (l * 2) - m2;
		var hueToRgb = function (h__) {
			var h_ = (h__ < 0) ? (h__ + 1) : ((h__ > 1) ? (h__ - 1) : h__);
			return ((h_ * 6) < 1) ? (m1 + (((m2 - m1) * h_) * 6)) : (((h_ * 2) < 1) ? m2 : (((h_ * 3) < 2) ? (m1 + (((m2 - m1) * ((2 / 3) - h_)) * 6)) : m1));
		};
		var b = hueToRgb(h - (1 / 3));
		var g = hueToRgb(h);
		var r = hueToRgb(h + (1 / 3));
		return A4(avh4$elm_color$Color$RgbaSpace, r, g, b, alpha);
	});
var avh4$elm_color$Color$fromHsla = function (_n0) {
	var hue = _n0.hue;
	var saturation = _n0.saturation;
	var lightness = _n0.lightness;
	var alpha = _n0.alpha;
	return A4(avh4$elm_color$Color$hsla, hue, saturation, lightness, alpha);
};
var elm$core$Basics$isNaN = _Basics_isNaN;
var avh4$elm_color$Color$toHsla = function (_n0) {
	var r = _n0.a;
	var g = _n0.b;
	var b = _n0.c;
	var a = _n0.d;
	var minColor = A2(
		elm$core$Basics$min,
		r,
		A2(elm$core$Basics$min, g, b));
	var maxColor = A2(
		elm$core$Basics$max,
		r,
		A2(elm$core$Basics$max, g, b));
	var l = (minColor + maxColor) / 2;
	var s = _Utils_eq(minColor, maxColor) ? 0 : ((l < 0.5) ? ((maxColor - minColor) / (maxColor + minColor)) : ((maxColor - minColor) / ((2 - maxColor) - minColor)));
	var h1 = _Utils_eq(maxColor, r) ? ((g - b) / (maxColor - minColor)) : (_Utils_eq(maxColor, g) ? (2 + ((b - r) / (maxColor - minColor))) : (4 + ((r - g) / (maxColor - minColor))));
	var h2 = h1 * (1 / 6);
	var h3 = elm$core$Basics$isNaN(h2) ? 0 : ((h2 < 0) ? (h2 + 1) : h2);
	return {alpha: a, hue: h3, lightness: l, saturation: s};
};
var author$project$DiscreteGradientEditor$updateHueChannel = F2(
	function (color, hue) {
		var hsl = avh4$elm_color$Color$toHsla(color);
		return avh4$elm_color$Color$fromHsla(
			_Utils_update(
				hsl,
				{
					hue: author$project$DiscreteGradientEditor$floatFrom255(hue)
				}));
	});
var author$project$DiscreteGradientEditor$updateLightnessChannel = F2(
	function (color, lightness) {
		var hsl = avh4$elm_color$Color$toHsla(color);
		return avh4$elm_color$Color$fromHsla(
			_Utils_update(
				hsl,
				{
					lightness: author$project$DiscreteGradientEditor$floatFrom255(lightness)
				}));
	});
var author$project$DiscreteGradientEditor$updateRedChannel = F2(
	function (color, red) {
		var rgba = avh4$elm_color$Color$toRgba(color);
		return avh4$elm_color$Color$fromRgba(
			_Utils_update(
				rgba,
				{
					red: author$project$DiscreteGradientEditor$floatFrom255(red)
				}));
	});
var author$project$DiscreteGradientEditor$updateSaturationChannel = F2(
	function (color, saturation) {
		var hsl = avh4$elm_color$Color$toHsla(color);
		return avh4$elm_color$Color$fromHsla(
			_Utils_update(
				hsl,
				{
					saturation: author$project$DiscreteGradientEditor$floatFrom255(saturation)
				}));
	});
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
var elm$core$String$toInt = _String_toInt;
var author$project$DiscreteGradientEditor$updateSelectedStopColorChannel = F3(
	function (updateChannel, newChannelVal, model) {
		return A2(
			elm$core$Maybe$withDefault,
			model,
			A2(
				elm$core$Maybe$map,
				author$project$DiscreteGradientEditor$setSelectedStopColor(model),
				A2(
					elm$core$Maybe$map,
					updateChannel(model.selectedColor),
					elm$core$String$toInt(newChannelVal))));
	});
var elm$core$Tuple$mapFirst = F2(
	function (func, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var elm$core$Basics$not = _Basics_not;
var simonh1000$elm_colorpicker$ColorPicker$widgetWidth = 200;
var simonh1000$elm_colorpicker$ColorPicker$calcHue = F2(
	function (col, _n0) {
		var x = _n0.x;
		var mousePressed = _n0.mousePressed;
		var hue = x / simonh1000$elm_colorpicker$ColorPicker$widgetWidth;
		var hsla = avh4$elm_color$Color$toHsla(col);
		var saturation = hsla.saturation;
		var lightness = hsla.lightness;
		var alpha = hsla.alpha;
		var newCol = ((!saturation) && (lightness < 2.0e-2)) ? _Utils_update(
			hsla,
			{hue: hue, lightness: 0.5, saturation: 0.5}) : _Utils_update(
			hsla,
			{hue: hue});
		return avh4$elm_color$Color$fromHsla(newCol);
	});
var simonh1000$elm_colorpicker$ColorPicker$calcOpacity = F3(
	function (col, _n0, _n1) {
		var x = _n1.x;
		var mousePressed = _n1.mousePressed;
		var hsla = avh4$elm_color$Color$toHsla(col);
		return avh4$elm_color$Color$fromHsla(
			_Utils_update(
				hsla,
				{alpha: x / simonh1000$elm_colorpicker$ColorPicker$widgetWidth}));
	});
var simonh1000$elm_colorpicker$ColorPicker$widgetHeight = 150;
var simonh1000$elm_colorpicker$ColorPicker$calcSatLight = F3(
	function (col, currHue, _n0) {
		var x = _n0.x;
		var y = _n0.y;
		var mousePressed = _n0.mousePressed;
		var hsla = avh4$elm_color$Color$toHsla(col);
		return avh4$elm_color$Color$fromHsla(
			_Utils_update(
				hsla,
				{hue: currHue, lightness: 1 - (y / simonh1000$elm_colorpicker$ColorPicker$widgetHeight), saturation: x / simonh1000$elm_colorpicker$ColorPicker$widgetWidth}));
	});
var simonh1000$elm_colorpicker$ColorPicker$setHue = F3(
	function (mouseTarget, mouseInfo, model) {
		switch (mouseTarget.$) {
			case 'HueSlider':
				return _Utils_update(
					model,
					{
						hue: elm$core$Maybe$Just(mouseInfo.x / simonh1000$elm_colorpicker$ColorPicker$widgetWidth)
					});
			case 'SatLight':
				var hue = mouseTarget.a;
				return _Utils_update(
					model,
					{
						hue: elm$core$Maybe$Just(
							A2(elm$core$Maybe$withDefault, hue, model.hue))
					});
			case 'OpacitySlider':
				var hue = mouseTarget.a;
				return _Utils_update(
					model,
					{
						hue: elm$core$Maybe$Just(
							A2(elm$core$Maybe$withDefault, hue, model.hue))
					});
			default:
				return model;
		}
	});
var simonh1000$elm_colorpicker$ColorPicker$setMouseTarget = F2(
	function (mouseTarget, model) {
		return _Utils_update(
			model,
			{mouseTarget: mouseTarget});
	});
var simonh1000$elm_colorpicker$ColorPicker$update_ = F3(
	function (message, col, model) {
		var calcNewColour = function (mouseTarget) {
			switch (mouseTarget.$) {
				case 'SatLight':
					var hue = mouseTarget.a;
					return A2(
						elm$core$Basics$composeL,
						elm$core$Maybe$Just,
						A2(
							simonh1000$elm_colorpicker$ColorPicker$calcSatLight,
							col,
							A2(elm$core$Maybe$withDefault, hue, model.hue)));
				case 'HueSlider':
					return A2(
						elm$core$Basics$composeL,
						elm$core$Maybe$Just,
						simonh1000$elm_colorpicker$ColorPicker$calcHue(col));
				case 'OpacitySlider':
					var hue = mouseTarget.a;
					return A2(
						elm$core$Basics$composeL,
						elm$core$Maybe$Just,
						A2(
							simonh1000$elm_colorpicker$ColorPicker$calcOpacity,
							col,
							A2(elm$core$Maybe$withDefault, hue, model.hue)));
				default:
					return function (_n2) {
						return elm$core$Maybe$Nothing;
					};
			}
		};
		var handleMouseMove = F2(
			function (mouseTarget, mouseInfo) {
				return (mouseInfo.mousePressed && _Utils_eq(model.mouseTarget, mouseTarget)) ? _Utils_Tuple2(
					A3(simonh1000$elm_colorpicker$ColorPicker$setHue, mouseTarget, mouseInfo, model),
					A2(calcNewColour, mouseTarget, mouseInfo)) : (((!mouseInfo.mousePressed) && _Utils_eq(model.mouseTarget, mouseTarget)) ? _Utils_Tuple2(
					A2(simonh1000$elm_colorpicker$ColorPicker$setMouseTarget, simonh1000$elm_colorpicker$ColorPicker$Unpressed, model),
					elm$core$Maybe$Nothing) : _Utils_Tuple2(model, elm$core$Maybe$Nothing));
			});
		switch (message.$) {
			case 'OnMouseDown':
				var mouseTarget = message.a;
				var mouseInfo = message.b;
				return _Utils_Tuple2(
					A3(
						simonh1000$elm_colorpicker$ColorPicker$setHue,
						mouseTarget,
						mouseInfo,
						A2(simonh1000$elm_colorpicker$ColorPicker$setMouseTarget, mouseTarget, model)),
					A2(calcNewColour, mouseTarget, mouseInfo));
			case 'OnMouseMove':
				var mouseTarget = message.a;
				var mouseInfo = message.b;
				return A2(handleMouseMove, mouseTarget, mouseInfo);
			case 'OnClick':
				var mouseTarget = message.a;
				var mouseInfo = message.b;
				return _Utils_Tuple2(
					A3(simonh1000$elm_colorpicker$ColorPicker$setHue, mouseTarget, mouseInfo, model),
					A2(calcNewColour, mouseTarget, mouseInfo));
			case 'OnMouseUp':
				return _Utils_Tuple2(
					A2(simonh1000$elm_colorpicker$ColorPicker$setMouseTarget, simonh1000$elm_colorpicker$ColorPicker$Unpressed, model),
					elm$core$Maybe$Nothing);
			default:
				return _Utils_Tuple2(model, elm$core$Maybe$Nothing);
		}
	});
var simonh1000$elm_colorpicker$ColorPicker$update = F3(
	function (message, col, _n0) {
		var model = _n0.a;
		return A2(
			elm$core$Tuple$mapFirst,
			simonh1000$elm_colorpicker$ColorPicker$State,
			A3(simonh1000$elm_colorpicker$ColorPicker$update_, message, col, model));
	});
var author$project$DiscreteGradientEditor$update_ = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'ClickGradientCell':
				var cellVal = msg.a;
				return author$project$DiscreteGradientEditor$andEditInProgress(
					A2(author$project$DiscreteGradientEditor$selectGradientVal, model, cellVal));
			case 'ColorPickerMsg':
				var cpMsg = msg.a;
				if (A2(author$project$DiscreteGradientEditor$hasStopAt, model.selectedValue, model.gradient)) {
					var _n1 = A3(simonh1000$elm_colorpicker$ColorPicker$update, cpMsg, model.selectedColor, model.colorPickerState);
					var colorPicker = _n1.a;
					var maybeColor = _n1.b;
					var newColor = A2(elm$core$Maybe$withDefault, model.selectedColor, maybeColor);
					return author$project$DiscreteGradientEditor$andEditInProgress(
						_Utils_update(
							model,
							{
								colorPickerState: colorPicker,
								gradient: A2(
									author$project$DiscreteGradient$addStop,
									{color: newColor, value: model.selectedValue},
									model.gradient),
								selectedColor: newColor
							}));
				} else {
					return author$project$DiscreteGradientEditor$andEditInProgress(model);
				}
			case 'ChangeRed':
				var red = msg.a;
				return author$project$DiscreteGradientEditor$andEditInProgress(
					A3(author$project$DiscreteGradientEditor$updateSelectedStopColorChannel, author$project$DiscreteGradientEditor$updateRedChannel, red, model));
			case 'ChangeGreen':
				var green = msg.a;
				return author$project$DiscreteGradientEditor$andEditInProgress(
					A3(author$project$DiscreteGradientEditor$updateSelectedStopColorChannel, author$project$DiscreteGradientEditor$updateGreenChannel, green, model));
			case 'ChangeBlue':
				var blue = msg.a;
				return author$project$DiscreteGradientEditor$andEditInProgress(
					A3(author$project$DiscreteGradientEditor$updateSelectedStopColorChannel, author$project$DiscreteGradientEditor$updateBlueChannel, blue, model));
			case 'ChangeHue':
				var hue = msg.a;
				return author$project$DiscreteGradientEditor$andEditInProgress(
					A3(author$project$DiscreteGradientEditor$updateSelectedStopColorChannel, author$project$DiscreteGradientEditor$updateHueChannel, hue, model));
			case 'ChangeSaturation':
				var saturation = msg.a;
				return author$project$DiscreteGradientEditor$andEditInProgress(
					A3(author$project$DiscreteGradientEditor$updateSelectedStopColorChannel, author$project$DiscreteGradientEditor$updateSaturationChannel, saturation, model));
			case 'ChangeLightness':
				var lightness = msg.a;
				return author$project$DiscreteGradientEditor$andEditInProgress(
					A3(author$project$DiscreteGradientEditor$updateSelectedStopColorChannel, author$project$DiscreteGradientEditor$updateLightnessChannel, lightness, model));
			case 'ChangeHex':
				var hex = msg.a;
				return author$project$DiscreteGradientEditor$andEditInProgress(
					A2(
						elm$core$Maybe$withDefault,
						model,
						A2(
							elm$core$Maybe$map,
							author$project$DiscreteGradientEditor$setSelectedStopColor(model),
							author$project$DiscreteGradientEditor$colorFromRgbHex(hex))));
			case 'ChangeStopValue':
				var stopVal = msg.a;
				return author$project$DiscreteGradientEditor$andEditInProgress(
					A2(
						elm$core$Maybe$withDefault,
						model,
						A2(
							elm$core$Maybe$map,
							author$project$DiscreteGradientEditor$selectGradientVal(model),
							elm$core$String$toInt(stopVal))));
			case 'ClickCreateStop':
				var colorAtSelectedVal = A2(author$project$DiscreteGradient$getColorAt, model.selectedValue, model.gradient);
				var newStop = {color: colorAtSelectedVal, value: model.selectedValue};
				return author$project$DiscreteGradientEditor$andEditInProgress(
					_Utils_update(
						model,
						{
							gradient: A2(author$project$DiscreteGradient$addStop, newStop, model.gradient)
						}));
			case 'ClickDeleteStop':
				return author$project$DiscreteGradientEditor$andEditInProgress(
					author$project$DiscreteGradientEditor$resetSelectedColor(
						_Utils_update(
							model,
							{
								gradient: A2(author$project$DiscreteGradient$removeStopAt, model.selectedValue, model.gradient)
							})));
			case 'ClickCancel':
				return _Utils_Tuple2(model, author$project$DiscreteGradientEditor$Cancel);
			default:
				return _Utils_Tuple2(
					model,
					author$project$DiscreteGradientEditor$Save(model.gradient));
		}
	});
var author$project$DiscreteGradientEditor$update = F2(
	function (msg, _n0) {
		var model = _n0.a;
		return A2(
			elm$core$Tuple$mapFirst,
			author$project$DiscreteGradientEditor$State,
			A2(author$project$DiscreteGradientEditor$update_, msg, model));
	});
var author$project$GridEditor$State = function (a) {
	return {$: 'State', a: a};
};
var author$project$GridEditor$update_ = F2(
	function (msg, model) {
		return _Utils_Tuple2(model, model.grid);
	});
var elm$core$Debug$log = _Debug_log;
var author$project$GridEditor$update = F2(
	function (msg, _n0) {
		var model = _n0.a;
		return A2(
			elm$core$Debug$log,
			'State',
			A2(
				elm$core$Tuple$mapFirst,
				author$project$GridEditor$State,
				A2(author$project$GridEditor$update_, msg, model)));
	});
var author$project$MapEditor$closeDialog = function (state) {
	return _Utils_update(
		state,
		{dialog: elm$core$Maybe$Nothing});
};
var author$project$DiscreteGradient$init = function (stop) {
	return author$project$DiscreteGradient$DiscreteGradient(
		_List_fromArray(
			[stop]));
};
var author$project$Grid$rectangle = F3(
	function (w, h, filler) {
		return A2(
			elm$core$Array$initialize,
			h,
			function (y) {
				return A2(
					elm$core$Array$initialize,
					w,
					function (x) {
						return A2(filler, x, y);
					});
			});
	});
var elm$core$Basics$always = F2(
	function (a, _n0) {
		return a;
	});
var author$project$Grid$repeat = F3(
	function (x, y, occupant) {
		return A3(
			author$project$Grid$rectangle,
			x,
			y,
			A2(
				elm$core$Basics$composeL,
				elm$core$Basics$always,
				elm$core$Basics$always(occupant)));
	});
var author$project$Layer$Layer = function (a) {
	return {$: 'Layer', a: a};
};
var avh4$elm_color$Color$blue = A4(avh4$elm_color$Color$RgbaSpace, 52 / 255, 101 / 255, 164 / 255, 1.0);
var author$project$Layer$defaultColor = avh4$elm_color$Color$blue;
var author$project$Layer$init = F5(
	function (name, width, height, minVal, maxVal) {
		return author$project$Layer$Layer(
			{
				colorGradient: author$project$DiscreteGradient$init(
					{color: author$project$Layer$defaultColor, value: minVal}),
				grid: A3(author$project$Grid$repeat, width, height, minVal),
				height: A2(elm$core$Basics$max, height, 1),
				max: maxVal,
				min: minVal,
				name: name,
				width: A2(elm$core$Basics$max, width, 1)
			});
	});
var author$project$MapEditor$createLayer = F2(
	function (name, state) {
		var newLayer = A5(author$project$Layer$init, name, state.width, state.height, 0, 0);
		return _Utils_update(
			state,
			{
				layers: _Utils_ap(
					state.layers,
					_List_fromArray(
						[newLayer]))
			});
	});
var author$project$MapEditor$getSelectedLayerIndex = function (state) {
	return A2(
		elm$core$Maybe$map,
		function (s) {
			return s.layerIndex;
		},
		state.layerSelection);
};
var elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
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
var author$project$MapEditor$remove = F2(
	function (index, xs) {
		return _Utils_ap(
			A2(elm$core$List$take, index, xs),
			A2(elm$core$List$drop, index + 1, xs));
	});
var author$project$MapEditor$deleteSelectedLayer = function (state) {
	var _n0 = author$project$MapEditor$getSelectedLayerIndex(state);
	if (_n0.$ === 'Just') {
		var index = _n0.a;
		return _Utils_update(
			state,
			{
				layerSelection: elm$core$Maybe$Nothing,
				layers: A2(author$project$MapEditor$remove, index, state.layers)
			});
	} else {
		return state;
	}
};
var author$project$MapEditor$getDialog = function (state) {
	return state.dialog;
};
var author$project$MapEditor$getGridEditor = function (state) {
	return A2(
		elm$core$Maybe$map,
		function (s) {
			return s.gridEditor;
		},
		state.layerSelection);
};
var author$project$MapEditor$getLayerCount = function (state) {
	return elm$core$List$length(state.layers);
};
var author$project$DiscreteGradientEditor$initModel = F3(
	function (dg, gradientMin, gradientMax) {
		return {
			colorPickerState: simonh1000$elm_colorpicker$ColorPicker$empty,
			gradient: dg,
			gradientMax: gradientMax,
			gradientMin: gradientMin,
			selectedColor: A2(author$project$DiscreteGradient$getColorAt, gradientMin, dg),
			selectedValue: gradientMin
		};
	});
var author$project$DiscreteGradientEditor$init = F3(
	function (dg, gradientMin, gradientMax) {
		return author$project$DiscreteGradientEditor$State(
			A3(author$project$DiscreteGradientEditor$initModel, dg, gradientMin, gradientMax));
	});
var author$project$Layer$getColorGradient = function (_n0) {
	var inner = _n0.a;
	return inner.colorGradient;
};
var author$project$Layer$getMax = function (_n0) {
	var inner = _n0.a;
	return inner.max;
};
var author$project$Layer$getMin = function (_n0) {
	var inner = _n0.a;
	return inner.min;
};
var author$project$MapEditor$GradientEditorDialog = function (a) {
	return {$: 'GradientEditorDialog', a: a};
};
var author$project$MapEditor$flip = function (f) {
	return F2(
		function (b, a) {
			return A2(f, a, b);
		});
};
var elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm_community$list_extra$List$Extra$getAt = F2(
	function (idx, xs) {
		return (idx < 0) ? elm$core$Maybe$Nothing : elm$core$List$head(
			A2(elm$core$List$drop, idx, xs));
	});
var author$project$MapEditor$getSelectedLayer = function (state) {
	return A2(
		elm$core$Maybe$andThen,
		A2(author$project$MapEditor$flip, elm_community$list_extra$List$Extra$getAt, state.layers),
		author$project$MapEditor$getSelectedLayerIndex(state));
};
var author$project$MapEditor$openGradientEditorDialog = function (state) {
	var _n0 = author$project$MapEditor$getSelectedLayer(state);
	if (_n0.$ === 'Just') {
		var layer = _n0.a;
		var editorState = A3(
			author$project$DiscreteGradientEditor$init,
			author$project$Layer$getColorGradient(layer),
			author$project$Layer$getMin(layer),
			author$project$Layer$getMax(layer));
		return _Utils_update(
			state,
			{
				dialog: elm$core$Maybe$Just(
					author$project$MapEditor$GradientEditorDialog(editorState))
			});
	} else {
		return state;
	}
};
var author$project$MapEditor$NewLayerDialog = function (a) {
	return {$: 'NewLayerDialog', a: a};
};
var author$project$MapEditor$openNewLayerDialog = function (state) {
	return _Utils_update(
		state,
		{
			dialog: elm$core$Maybe$Just(
				author$project$MapEditor$NewLayerDialog('New Layer'))
		});
};
var author$project$MapEditor$Pan = {$: 'Pan'};
var author$project$MapEditor$toolbarWidth = 300.0;
var author$project$MapEditor$gridEditorPaneWidth = function (windowWidth) {
	return A2(elm$core$Basics$max, 0.0, windowWidth - author$project$MapEditor$toolbarWidth);
};
var elm$core$Elm$JsArray$map = _JsArray_map;
var elm$core$Array$map = F2(
	function (func, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = function (node) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return elm$core$Array$SubTree(
					A2(elm$core$Elm$JsArray$map, helper, subTree));
			} else {
				var values = node.a;
				return elm$core$Array$Leaf(
					A2(elm$core$Elm$JsArray$map, func, values));
			}
		};
		return A4(
			elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A2(elm$core$Elm$JsArray$map, helper, tree),
			A2(elm$core$Elm$JsArray$map, func, tail));
	});
var author$project$Grid$map = F2(
	function (f, grid) {
		return A2(
			elm$core$Array$map,
			elm$core$Array$map(f),
			grid);
	});
var author$project$GridEditor$boundInt = F2(
	function (lower, upper) {
		return A2(
			elm$core$Basics$composeL,
			elm$core$Basics$max(lower),
			elm$core$Basics$min(upper));
	});
var author$project$GridEditor$boundGrid = F2(
	function (lower, upper) {
		return author$project$Grid$map(
			A2(author$project$GridEditor$boundInt, lower, upper));
	});
var author$project$GridEditor$init = F6(
	function (grid, cellMin, cellMax, gradient, paneWidth, paneHeight) {
		var g = A3(author$project$GridEditor$boundGrid, cellMin, cellMax, grid);
		return author$project$GridEditor$State(
			{cellMax: cellMax, cellMin: cellMin, gradient: gradient, grid: g, paneHeight: paneHeight, paneWidth: paneWidth});
	});
var author$project$Layer$getGrid = function (_n0) {
	var inner = _n0.a;
	return inner.grid;
};
var author$project$MapEditor$initGridEditor = F3(
	function (layer, paneWidth, paneHeight) {
		return A6(
			author$project$GridEditor$init,
			author$project$Layer$getGrid(layer),
			author$project$Layer$getMin(layer),
			author$project$Layer$getMax(layer),
			author$project$Layer$getColorGradient(layer),
			paneWidth,
			paneHeight);
	});
var author$project$MapEditor$selectLayer = F2(
	function (index, state) {
		if (index.$ === 'Nothing') {
			return _Utils_update(
				state,
				{layerSelection: elm$core$Maybe$Nothing});
		} else {
			var i = index.a;
			var _n1 = A2(elm_community$list_extra$List$Extra$getAt, i, state.layers);
			if (_n1.$ === 'Nothing') {
				return state;
			} else {
				var layer = _n1.a;
				return _Utils_update(
					state,
					{
						layerSelection: elm$core$Maybe$Just(
							{
								gridEditor: A3(
									author$project$MapEditor$initGridEditor,
									layer,
									author$project$MapEditor$gridEditorPaneWidth(state.windowWidth),
									state.windowHeight),
								layerIndex: i,
								tool: author$project$MapEditor$Pan
							})
					});
			}
		}
	});
var author$project$MapEditor$setNewLayerDialogName = F2(
	function (layerName, state) {
		var _n0 = state.dialog;
		if ((_n0.$ === 'Just') && (_n0.a.$ === 'NewLayerDialog')) {
			return _Utils_update(
				state,
				{
					dialog: elm$core$Maybe$Just(
						author$project$MapEditor$NewLayerDialog(layerName))
				});
		} else {
			return state;
		}
	});
var author$project$MapEditor$updateGradientEditorDialog = F2(
	function (dialog, state) {
		var _n0 = state.dialog;
		if ((_n0.$ === 'Just') && (_n0.a.$ === 'GradientEditorDialog')) {
			return _Utils_update(
				state,
				{
					dialog: elm$core$Maybe$Just(
						author$project$MapEditor$GradientEditorDialog(dialog))
				});
		} else {
			return state;
		}
	});
var author$project$Layer$removeColorStops = F2(
	function (stops, gradient) {
		return A3(
			elm$core$List$foldl,
			elm$core$Basics$apL,
			gradient,
			A2(
				elm$core$List$map,
				author$project$DiscreteGradient$removeStopAt,
				A2(
					elm$core$List$map,
					function (s) {
						return s.value;
					},
					stops)));
	});
var author$project$Layer$applyCeilingToColorStops = F2(
	function (ceil, gradient) {
		var stopsAboveCeiling = A2(
			elm$core$List$filter,
			function (s) {
				return _Utils_cmp(s.value, ceil) > 0;
			},
			author$project$DiscreteGradient$getStops(gradient));
		var colorAtCeiling = A2(author$project$DiscreteGradient$getColorAt, ceil, gradient);
		return A2(
			author$project$DiscreteGradient$addStop,
			{color: colorAtCeiling, value: ceil},
			A2(author$project$Layer$removeColorStops, stopsAboveCeiling, gradient));
	});
var author$project$Layer$applyFloorToColorStops = F2(
	function (flr, gradient) {
		var stopsBelowFloor = A2(
			elm$core$List$filter,
			function (s) {
				return _Utils_cmp(s.value, flr) < 0;
			},
			author$project$DiscreteGradient$getStops(gradient));
		var colorAtFloor = A2(author$project$DiscreteGradient$getColorAt, flr, gradient);
		return A2(
			author$project$DiscreteGradient$addStop,
			{color: colorAtFloor, value: flr},
			A2(author$project$Layer$removeColorStops, stopsBelowFloor, gradient));
	});
var author$project$Layer$setColorGradient = F2(
	function (dg, _n0) {
		var inner = _n0.a;
		var newGradient = A2(
			author$project$Layer$applyCeilingToColorStops,
			inner.max,
			A2(author$project$Layer$applyFloorToColorStops, inner.min, dg));
		return author$project$Layer$Layer(
			_Utils_update(
				inner,
				{colorGradient: newGradient}));
	});
var author$project$MapEditor$updateSelectedLayer = F2(
	function (f, state) {
		var _n0 = author$project$MapEditor$getSelectedLayerIndex(state);
		if (_n0.$ === 'Just') {
			var selectedIndex = _n0.a;
			var _n1 = A2(elm_community$list_extra$List$Extra$getAt, selectedIndex, state.layers);
			if (_n1.$ === 'Just') {
				var updatedLayers = A2(
					elm$core$List$indexedMap,
					F2(
						function (i, layer) {
							return _Utils_eq(i, selectedIndex) ? f(layer) : layer;
						}),
					state.layers);
				return _Utils_update(
					state,
					{layers: updatedLayers});
			} else {
				return state;
			}
		} else {
			return state;
		}
	});
var author$project$MapEditor$updateSelectedLayerColorGradient = A2(elm$core$Basics$composeL, author$project$MapEditor$updateSelectedLayer, author$project$Layer$setColorGradient);
var author$project$Layer$applyCeilingToGrid = function (ceil) {
	return author$project$Grid$map(
		elm$core$Basics$min(ceil));
};
var author$project$Layer$applyFloorToGrid = function (flr) {
	return author$project$Grid$map(
		elm$core$Basics$max(flr));
};
var author$project$Grid$toColumn = elm$core$Tuple$first;
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var author$project$Grid$toRow = elm$core$Tuple$second;
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
var author$project$Grid$get = F2(
	function (coord, grid) {
		return A2(
			elm$core$Maybe$andThen,
			elm$core$Array$get(
				author$project$Grid$toColumn(coord)),
			A2(
				elm$core$Array$get,
				author$project$Grid$toRow(coord),
				grid));
	});
var elm$core$Array$length = function (_n0) {
	var len = _n0.a;
	return len;
};
var author$project$Grid$height = function (grid) {
	return elm$core$Array$length(grid);
};
var author$project$Grid$width = function (grid) {
	return A2(
		elm$core$Maybe$withDefault,
		0,
		A2(
			elm$core$Maybe$map,
			elm$core$Array$length,
			A2(elm$core$Array$get, 0, grid)));
};
var author$project$Layer$resizeGrid_ = F4(
	function (_default, width, height, grid) {
		if ((width < 1) || ((height < 1) || (_Utils_eq(
			width,
			author$project$Grid$width(grid)) && _Utils_eq(
			height,
			author$project$Grid$height(grid))))) {
			return grid;
		} else {
			var filler = F2(
				function (x, y) {
					var _n0 = A2(
						author$project$Grid$get,
						_Utils_Tuple2(x, y),
						grid);
					if (_n0.$ === 'Just') {
						var cell = _n0.a;
						return cell;
					} else {
						return _default;
					}
				});
			return A3(author$project$Grid$rectangle, width, height, filler);
		}
	});
var author$project$Layer$setGrid = F2(
	function (grid, _n0) {
		var inner = _n0.a;
		var newGrid = A2(
			author$project$Layer$applyCeilingToGrid,
			inner.max,
			A2(
				author$project$Layer$applyFloorToGrid,
				inner.min,
				A4(author$project$Layer$resizeGrid_, inner.min, inner.width, inner.height, grid)));
		return author$project$Layer$Layer(
			_Utils_update(
				inner,
				{grid: grid}));
	});
var author$project$MapEditor$updateSelectedLayerGrid = A2(elm$core$Basics$composeL, author$project$MapEditor$updateSelectedLayer, author$project$Layer$setGrid);
var author$project$Layer$setDecreasedMax = F2(
	function (newMax, inner) {
		return _Utils_update(
			inner,
			{
				colorGradient: A2(author$project$Layer$applyCeilingToColorStops, newMax, inner.colorGradient),
				grid: A2(author$project$Layer$applyCeilingToGrid, newMax, inner.grid),
				max: newMax
			});
	});
var author$project$Layer$setIncreasedMax = F2(
	function (newMax, inner) {
		return _Utils_update(
			inner,
			{max: newMax});
	});
var author$project$Layer$setMax = F2(
	function (newMax, _n0) {
		var inner = _n0.a;
		return ((_Utils_cmp(newMax, inner.min) < 0) || _Utils_eq(newMax, inner.max)) ? author$project$Layer$Layer(inner) : ((_Utils_cmp(newMax, inner.max) < 0) ? author$project$Layer$Layer(
			A2(author$project$Layer$setDecreasedMax, newMax, inner)) : author$project$Layer$Layer(
			A2(author$project$Layer$setIncreasedMax, newMax, inner)));
	});
var author$project$MapEditor$updateSelectedLayerMax = A2(elm$core$Basics$composeL, author$project$MapEditor$updateSelectedLayer, author$project$Layer$setMax);
var author$project$Layer$setDecreasedMin = F2(
	function (newMin, inner) {
		return _Utils_update(
			inner,
			{min: newMin});
	});
var author$project$Layer$setIncreasedMin = F2(
	function (newMin, inner) {
		return _Utils_update(
			inner,
			{
				colorGradient: A2(author$project$Layer$applyFloorToColorStops, newMin, inner.colorGradient),
				grid: A2(author$project$Layer$applyFloorToGrid, newMin, inner.grid),
				min: newMin
			});
	});
var author$project$Layer$setMin = F2(
	function (newMin, _n0) {
		var inner = _n0.a;
		return ((_Utils_cmp(newMin, inner.max) > 0) || _Utils_eq(newMin, inner.min)) ? author$project$Layer$Layer(inner) : ((_Utils_cmp(newMin, inner.min) < 0) ? author$project$Layer$Layer(
			A2(author$project$Layer$setDecreasedMin, newMin, inner)) : author$project$Layer$Layer(
			A2(author$project$Layer$setIncreasedMin, newMin, inner)));
	});
var author$project$MapEditor$updateSelectedLayerMin = A2(elm$core$Basics$composeL, author$project$MapEditor$updateSelectedLayer, author$project$Layer$setMin);
var author$project$MapEditor$update = F2(
	function (msg, state) {
		return function (s) {
			return _Utils_Tuple2(s, elm$core$Platform$Cmd$none);
		}(
			A2(
				elm$core$Debug$log,
				'State',
				function () {
					var _n0 = A2(elm$core$Debug$log, 'msg', msg);
					switch (_n0.$) {
						case 'SelectLayer':
							var index = _n0.a;
							return A2(author$project$MapEditor$selectLayer, index, state);
						case 'DeleteSelectedLayer':
							return author$project$MapEditor$deleteSelectedLayer(state);
						case 'OpenNewLayerDialog':
							return author$project$MapEditor$openNewLayerDialog(state);
						case 'SetNewLayerDialogNameField':
							var layerName = _n0.a;
							return A2(author$project$MapEditor$setNewLayerDialogName, layerName, state);
						case 'NewLayerDialogCancel':
							return author$project$MapEditor$closeDialog(state);
						case 'NewLayerDialogCreate':
							var _n1 = author$project$MapEditor$getDialog(state);
							if ((_n1.$ === 'Just') && (_n1.a.$ === 'NewLayerDialog')) {
								var layerName = _n1.a.a;
								return author$project$MapEditor$closeDialog(
									A2(
										author$project$MapEditor$selectLayer,
										elm$core$Maybe$Just(
											author$project$MapEditor$getLayerCount(state)),
										A2(author$project$MapEditor$createLayer, layerName, state)));
							} else {
								return state;
							}
						case 'SetLayerMin':
							var newMin = _n0.a;
							if (newMin.$ === 'Just') {
								var m = newMin.a;
								return A2(author$project$MapEditor$updateSelectedLayerMin, m, state);
							} else {
								return state;
							}
						case 'SetLayerMax':
							var newMax = _n0.a;
							if (newMax.$ === 'Just') {
								var m = newMax.a;
								return A2(author$project$MapEditor$updateSelectedLayerMax, m, state);
							} else {
								return state;
							}
						case 'OpenGradientEditorDialog':
							return author$project$MapEditor$openGradientEditorDialog(state);
						case 'GradientEditorMsg':
							var editorMsg = _n0.a;
							var _n4 = author$project$MapEditor$getDialog(state);
							if ((_n4.$ === 'Just') && (_n4.a.$ === 'GradientEditorDialog')) {
								var dialog = _n4.a.a;
								var _n5 = A2(author$project$DiscreteGradientEditor$update, editorMsg, dialog);
								var newDialog = _n5.a;
								var output = _n5.b;
								switch (output.$) {
									case 'EditInProgress':
										return A2(author$project$MapEditor$updateGradientEditorDialog, newDialog, state);
									case 'Cancel':
										return author$project$MapEditor$closeDialog(state);
									default:
										var gradient = output.a;
										return author$project$MapEditor$closeDialog(
											A2(author$project$MapEditor$updateSelectedLayerColorGradient, gradient, state));
								}
							} else {
								return state;
							}
						default:
							var editorMsg = _n0.a;
							var _n7 = author$project$MapEditor$getGridEditor(state);
							if (_n7.$ === 'Just') {
								var gridEditor = _n7.a;
								var _n8 = A2(author$project$GridEditor$update, editorMsg, gridEditor);
								var newGridEditor = _n8.a;
								var grid = _n8.b;
								return A2(author$project$MapEditor$updateSelectedLayerGrid, grid, state);
							} else {
								return state;
							}
					}
				}()));
	});
var author$project$DiscreteGradientEditor$ClickCancel = {$: 'ClickCancel'};
var author$project$DiscreteGradientEditor$ClickSave = {$: 'ClickSave'};
var author$project$DiscreteGradientEditor$ColorPickerMsg = function (a) {
	return {$: 'ColorPickerMsg', a: a};
};
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$map2 = _Json_map2;
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
var elm$html$Html$div = _VirtualDom_node('div');
var elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var elm$html$Html$map = elm$virtual_dom$VirtualDom$map;
var elm$json$Json$Encode$string = _Json_wrap;
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var avh4$elm_color$Color$hsl = F3(
	function (h, s, l) {
		return A4(avh4$elm_color$Color$hsla, h, s, l, 1.0);
	});
var elm$core$Basics$round = _Basics_round;
var elm$core$String$concat = function (strings) {
	return A2(elm$core$String$join, '', strings);
};
var elm$core$String$fromFloat = _String_fromNumber;
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
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var simonh1000$elm_colorpicker$ColorPicker$markerAttrs = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'position', 'absolute'),
		A2(elm$html$Html$Attributes$style, 'top', '1px'),
		A2(elm$html$Html$Attributes$style, 'bottom', '1px'),
		A2(elm$html$Html$Attributes$style, 'border', '1px solid #ddd'),
		A2(elm$html$Html$Attributes$style, 'background-color', '#ffffff'),
		A2(elm$html$Html$Attributes$style, 'width', '6px'),
		A2(elm$html$Html$Attributes$style, 'pointer-events', 'none')
	]);
var simonh1000$elm_colorpicker$ColorPicker$alphaMarker = function (alpha) {
	var correction = 4;
	var xVal = elm$core$String$fromInt(
		elm$core$Basics$round((alpha * simonh1000$elm_colorpicker$ColorPicker$widgetWidth) - correction));
	return A2(
		elm$html$Html$div,
		A2(
			elm$core$List$cons,
			A2(elm$html$Html$Attributes$style, 'left', xVal + 'px'),
			simonh1000$elm_colorpicker$ColorPicker$markerAttrs),
		_List_Nil);
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
var simonh1000$elm_colorpicker$ColorPicker$NoOp = {$: 'NoOp'};
var simonh1000$elm_colorpicker$ColorPicker$bubblePreventer = A2(
	elm$html$Html$Events$stopPropagationOn,
	'click',
	elm$json$Json$Decode$succeed(
		_Utils_Tuple2(simonh1000$elm_colorpicker$ColorPicker$NoOp, true)));
var simonh1000$elm_colorpicker$ColorPicker$checkedBkgStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background-size', '12px 12px'),
		A2(elm$html$Html$Attributes$style, 'background-position', '0 0, 0 6px, 6px -6px, -6px 0px'),
		A2(elm$html$Html$Attributes$style, 'background-image', 'linear-gradient(45deg, #808080 25%, transparent 25%), linear-gradient(-45deg, #808080 25%, transparent 25%), linear-gradient(45deg, transparent 75%, #808080 75%), linear-gradient(-45deg, transparent 75%, #808080 75%)')
	]);
var simonh1000$elm_colorpicker$ColorPicker$hueMarker = function (lastHue) {
	var correction = 4;
	var xVal = elm$core$String$fromInt(
		elm$core$Basics$round((lastHue * simonh1000$elm_colorpicker$ColorPicker$widgetWidth) - correction));
	return A2(
		elm$html$Html$div,
		A2(
			elm$core$List$cons,
			A2(elm$html$Html$Attributes$style, 'left', xVal + 'px'),
			simonh1000$elm_colorpicker$ColorPicker$markerAttrs),
		_List_Nil);
};
var elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var elm$svg$Svg$defs = elm$svg$Svg$trustedNode('defs');
var elm$svg$Svg$linearGradient = elm$svg$Svg$trustedNode('linearGradient');
var elm$svg$Svg$rect = elm$svg$Svg$trustedNode('rect');
var elm$svg$Svg$stop = elm$svg$Svg$trustedNode('stop');
var elm$svg$Svg$svg = elm$svg$Svg$trustedNode('svg');
var elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var elm$svg$Svg$Attributes$offset = _VirtualDom_attribute('offset');
var elm$svg$Svg$Attributes$stopColor = _VirtualDom_attribute('stop-color');
var elm$svg$Svg$Attributes$stopOpacity = _VirtualDom_attribute('stop-opacity');
var elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var simonh1000$elm_colorpicker$ColorPicker$HueSlider = {$: 'HueSlider'};
var simonh1000$elm_colorpicker$ColorPicker$OnMouseMove = F2(
	function (a, b) {
		return {$: 'OnMouseMove', a: a, b: b};
	});
var elm$svg$Svg$Attributes$display = _VirtualDom_attribute('display');
var simonh1000$elm_colorpicker$ColorPicker$sliderStyles = _List_fromArray(
	[
		elm$svg$Svg$Attributes$width(
		elm$core$String$fromInt(simonh1000$elm_colorpicker$ColorPicker$widgetWidth)),
		elm$svg$Svg$Attributes$height('100%'),
		elm$svg$Svg$Attributes$display('block')
	]);
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
var elm$svg$Svg$Events$onMouseUp = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'mouseup',
		elm$json$Json$Decode$succeed(msg));
};
var simonh1000$elm_colorpicker$ColorPicker$OnClick = F2(
	function (a, b) {
		return {$: 'OnClick', a: a, b: b};
	});
var simonh1000$elm_colorpicker$ColorPicker$OnMouseDown = F2(
	function (a, b) {
		return {$: 'OnMouseDown', a: a, b: b};
	});
var simonh1000$elm_colorpicker$ColorPicker$OnMouseUp = {$: 'OnMouseUp'};
var elm$svg$Svg$Events$on = elm$html$Html$Events$on;
var elm$core$Basics$neq = _Utils_notEqual;
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$int = _Json_decodeInt;
var elm$json$Json$Decode$map3 = _Json_map3;
var simonh1000$elm_colorpicker$ColorPicker$MouseInfo = F3(
	function (x, y, mousePressed) {
		return {mousePressed: mousePressed, x: x, y: y};
	});
var simonh1000$elm_colorpicker$ColorPicker$decodeMouseInfo = A4(
	elm$json$Json$Decode$map3,
	simonh1000$elm_colorpicker$ColorPicker$MouseInfo,
	A2(elm$json$Json$Decode$field, 'offsetX', elm$json$Json$Decode$int),
	A2(elm$json$Json$Decode$field, 'offsetY', elm$json$Json$Decode$int),
	A2(
		elm$json$Json$Decode$map,
		elm$core$Basics$neq(0),
		A2(elm$json$Json$Decode$field, 'buttons', elm$json$Json$Decode$int)));
var simonh1000$elm_colorpicker$ColorPicker$onClickSvg = function (msgCreator) {
	return A2(
		elm$svg$Svg$Events$on,
		'click',
		A2(elm$json$Json$Decode$map, msgCreator, simonh1000$elm_colorpicker$ColorPicker$decodeMouseInfo));
};
var simonh1000$elm_colorpicker$ColorPicker$onMouseDownSvg = function (msgCreator) {
	return A2(
		elm$svg$Svg$Events$on,
		'mousedown',
		A2(elm$json$Json$Decode$map, msgCreator, simonh1000$elm_colorpicker$ColorPicker$decodeMouseInfo));
};
var simonh1000$elm_colorpicker$ColorPicker$onMouseMoveSvg = function (msgCreator) {
	return A2(
		elm$svg$Svg$Events$on,
		'mousemove',
		A2(elm$json$Json$Decode$map, msgCreator, simonh1000$elm_colorpicker$ColorPicker$decodeMouseInfo));
};
var simonh1000$elm_colorpicker$ColorPicker$svgDragAttrs = F3(
	function (currMouseTgt, thisTgt, onMoveMsg) {
		var common = _List_fromArray(
			[
				simonh1000$elm_colorpicker$ColorPicker$onMouseDownSvg(
				simonh1000$elm_colorpicker$ColorPicker$OnMouseDown(thisTgt)),
				elm$svg$Svg$Events$onMouseUp(simonh1000$elm_colorpicker$ColorPicker$OnMouseUp),
				simonh1000$elm_colorpicker$ColorPicker$onClickSvg(
				simonh1000$elm_colorpicker$ColorPicker$OnClick(thisTgt))
			]);
		return _Utils_eq(currMouseTgt, thisTgt) ? A2(
			elm$core$List$cons,
			simonh1000$elm_colorpicker$ColorPicker$onMouseMoveSvg(onMoveMsg),
			common) : common;
	});
var simonh1000$elm_colorpicker$ColorPicker$huePalette = function (mouseTarget) {
	var stops = _List_fromArray(
		[
			_Utils_Tuple2('0%', '#FF0000'),
			_Utils_Tuple2('17%', '#FF00FF'),
			_Utils_Tuple2('33%', '#0000FF'),
			_Utils_Tuple2('50%', '#00FFFF'),
			_Utils_Tuple2('66%', '#00FF00'),
			_Utils_Tuple2('83%', '#FFFF00'),
			_Utils_Tuple2('100%', '#FF0000')
		]);
	var mkStop = function (_n0) {
		var os = _n0.a;
		var sc = _n0.b;
		return A2(
			elm$svg$Svg$stop,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$offset(os),
					elm$svg$Svg$Attributes$stopColor(sc),
					elm$svg$Svg$Attributes$stopOpacity('1')
				]),
			_List_Nil);
	};
	return A2(
		elm$svg$Svg$svg,
		A2(
			elm$core$List$cons,
			elm$svg$Svg$Attributes$class('hue-picker'),
			simonh1000$elm_colorpicker$ColorPicker$sliderStyles),
		_List_fromArray(
			[
				A2(
				elm$svg$Svg$defs,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$svg$Svg$linearGradient,
						_List_fromArray(
							[
								elm$svg$Svg$Attributes$id('gradient-hsv'),
								elm$svg$Svg$Attributes$x1('100%'),
								elm$svg$Svg$Attributes$y1('0%'),
								elm$svg$Svg$Attributes$x2('0%'),
								elm$svg$Svg$Attributes$y2('0%')
							]),
						A2(elm$core$List$map, mkStop, stops))
					])),
				A2(
				elm$svg$Svg$rect,
				_Utils_ap(
					_List_fromArray(
						[
							elm$svg$Svg$Attributes$x('0'),
							elm$svg$Svg$Attributes$y('0'),
							elm$svg$Svg$Attributes$width(
							elm$core$String$fromInt(simonh1000$elm_colorpicker$ColorPicker$widgetWidth)),
							elm$svg$Svg$Attributes$height('100%'),
							elm$svg$Svg$Attributes$fill('url(#gradient-hsv)')
						]),
					A3(
						simonh1000$elm_colorpicker$ColorPicker$svgDragAttrs,
						mouseTarget,
						simonh1000$elm_colorpicker$ColorPicker$HueSlider,
						simonh1000$elm_colorpicker$ColorPicker$OnMouseMove(simonh1000$elm_colorpicker$ColorPicker$HueSlider))),
				_List_Nil)
			]));
};
var simonh1000$elm_colorpicker$ColorPicker$OpacitySlider = function (a) {
	return {$: 'OpacitySlider', a: a};
};
var elm$html$Html$Events$onMouseUp = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'mouseup',
		elm$json$Json$Decode$succeed(msg));
};
var simonh1000$elm_colorpicker$ColorPicker$onClickHtml = function (msgCreator) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		A2(elm$json$Json$Decode$map, msgCreator, simonh1000$elm_colorpicker$ColorPicker$decodeMouseInfo));
};
var simonh1000$elm_colorpicker$ColorPicker$htmlDragAttrs = F3(
	function (currMouseTgt, thisTgt, onMoveMsg) {
		var common = _List_fromArray(
			[
				A2(
				elm$html$Html$Events$on,
				'mousedown',
				A2(
					elm$json$Json$Decode$map,
					simonh1000$elm_colorpicker$ColorPicker$OnMouseDown(thisTgt),
					simonh1000$elm_colorpicker$ColorPicker$decodeMouseInfo)),
				elm$html$Html$Events$onMouseUp(simonh1000$elm_colorpicker$ColorPicker$OnMouseUp),
				simonh1000$elm_colorpicker$ColorPicker$onClickHtml(
				simonh1000$elm_colorpicker$ColorPicker$OnClick(thisTgt))
			]);
		return _Utils_eq(currMouseTgt, thisTgt) ? A2(
			elm$core$List$cons,
			A2(
				elm$html$Html$Events$on,
				'mousemove',
				A2(elm$json$Json$Decode$map, onMoveMsg, simonh1000$elm_colorpicker$ColorPicker$decodeMouseInfo)),
			common) : common;
	});
var simonh1000$elm_colorpicker$ColorPicker$opacityPalette = F2(
	function (hue, model) {
		var mouseTarget = simonh1000$elm_colorpicker$ColorPicker$OpacitySlider(hue);
		var mkCol = function (op) {
			return avh4$elm_color$Color$toCssString(
				A4(avh4$elm_color$Color$hsla, hue, 1, 0.5, op));
		};
		var grad = 'linear-gradient(0.25turn, ' + (mkCol(0) + (', ' + (mkCol(1) + ')')));
		var overlay = _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'background', grad),
				A2(elm$html$Html$Attributes$style, 'height', '100%'),
				A2(elm$html$Html$Attributes$style, 'width', '100%')
			]);
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				overlay,
				A3(
					simonh1000$elm_colorpicker$ColorPicker$htmlDragAttrs,
					model.mouseTarget,
					mouseTarget,
					simonh1000$elm_colorpicker$ColorPicker$OnMouseMove(mouseTarget))),
			_List_Nil);
	});
var simonh1000$elm_colorpicker$ColorPicker$pickerIndicator = function (col) {
	var adjustment = 4;
	var _n0 = avh4$elm_color$Color$toHsla(col);
	var saturation = _n0.saturation;
	var lightness = _n0.lightness;
	var borderColor = (lightness > 0.95) ? '#cccccc' : '#ffffff';
	var cy_ = elm$core$String$fromInt(
		elm$core$Basics$round((simonh1000$elm_colorpicker$ColorPicker$widgetHeight - (lightness * simonh1000$elm_colorpicker$ColorPicker$widgetHeight)) - adjustment));
	var cx_ = elm$core$String$fromInt(
		elm$core$Basics$round((saturation * simonh1000$elm_colorpicker$ColorPicker$widgetWidth) - adjustment));
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'position', 'absolute'),
				A2(elm$html$Html$Attributes$style, 'top', cy_ + 'px'),
				A2(elm$html$Html$Attributes$style, 'left', cx_ + 'px'),
				A2(elm$html$Html$Attributes$style, 'border-radius', '100%'),
				A2(elm$html$Html$Attributes$style, 'border', '2px solid ' + borderColor),
				A2(elm$html$Html$Attributes$style, 'width', '6px'),
				A2(elm$html$Html$Attributes$style, 'height', '6px'),
				A2(elm$html$Html$Attributes$style, 'pointer-events', 'none')
			]),
		_List_Nil);
};
var simonh1000$elm_colorpicker$ColorPicker$pickerStyles = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'cursor', 'crosshair'),
		A2(elm$html$Html$Attributes$style, 'position', 'relative')
	]);
var simonh1000$elm_colorpicker$ColorPicker$SatLight = function (a) {
	return {$: 'SatLight', a: a};
};
var simonh1000$elm_colorpicker$ColorPicker$satLightPalette = F3(
	function (hue, colCss, mouseTarget) {
		return A2(
			elm$svg$Svg$svg,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$width(
					elm$core$String$fromInt(simonh1000$elm_colorpicker$ColorPicker$widgetWidth)),
					elm$svg$Svg$Attributes$height(
					elm$core$String$fromInt(simonh1000$elm_colorpicker$ColorPicker$widgetHeight)),
					elm$svg$Svg$Attributes$class('main-picker'),
					elm$svg$Svg$Attributes$display('block')
				]),
			_List_fromArray(
				[
					A2(
					elm$svg$Svg$defs,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							elm$svg$Svg$linearGradient,
							_List_fromArray(
								[
									elm$svg$Svg$Attributes$id('pickerSaturation')
								]),
							_List_fromArray(
								[
									A2(
									elm$svg$Svg$stop,
									_List_fromArray(
										[
											elm$svg$Svg$Attributes$offset('0'),
											elm$svg$Svg$Attributes$stopColor('#808080'),
											elm$svg$Svg$Attributes$stopOpacity('1')
										]),
									_List_Nil),
									A2(
									elm$svg$Svg$stop,
									_List_fromArray(
										[
											elm$svg$Svg$Attributes$offset('1'),
											elm$svg$Svg$Attributes$stopColor('#808080'),
											elm$svg$Svg$Attributes$stopOpacity('0')
										]),
									_List_Nil)
								])),
							A2(
							elm$svg$Svg$linearGradient,
							_List_fromArray(
								[
									elm$svg$Svg$Attributes$id('pickerBrightness'),
									elm$svg$Svg$Attributes$x1('0'),
									elm$svg$Svg$Attributes$y1('0'),
									elm$svg$Svg$Attributes$x2('0'),
									elm$svg$Svg$Attributes$y2('1')
								]),
							_List_fromArray(
								[
									A2(
									elm$svg$Svg$stop,
									_List_fromArray(
										[
											elm$svg$Svg$Attributes$offset('0'),
											elm$svg$Svg$Attributes$stopColor('#fff'),
											elm$svg$Svg$Attributes$stopOpacity('1')
										]),
									_List_Nil),
									A2(
									elm$svg$Svg$stop,
									_List_fromArray(
										[
											elm$svg$Svg$Attributes$offset('0.499'),
											elm$svg$Svg$Attributes$stopColor('#fff'),
											elm$svg$Svg$Attributes$stopOpacity('0')
										]),
									_List_Nil),
									A2(
									elm$svg$Svg$stop,
									_List_fromArray(
										[
											elm$svg$Svg$Attributes$offset('0.5'),
											elm$svg$Svg$Attributes$stopColor('#000'),
											elm$svg$Svg$Attributes$stopOpacity('0')
										]),
									_List_Nil),
									A2(
									elm$svg$Svg$stop,
									_List_fromArray(
										[
											elm$svg$Svg$Attributes$offset('1'),
											elm$svg$Svg$Attributes$stopColor('#000'),
											elm$svg$Svg$Attributes$stopOpacity('1')
										]),
									_List_Nil)
								]))
						])),
					A2(
					elm$svg$Svg$rect,
					_List_fromArray(
						[
							elm$svg$Svg$Attributes$width(
							elm$core$String$fromInt(simonh1000$elm_colorpicker$ColorPicker$widgetWidth)),
							elm$svg$Svg$Attributes$height(
							elm$core$String$fromInt(simonh1000$elm_colorpicker$ColorPicker$widgetHeight)),
							elm$svg$Svg$Attributes$fill(colCss),
							elm$svg$Svg$Attributes$id('picker')
						]),
					_List_Nil),
					A2(
					elm$svg$Svg$rect,
					_List_fromArray(
						[
							elm$svg$Svg$Attributes$width(
							elm$core$String$fromInt(simonh1000$elm_colorpicker$ColorPicker$widgetWidth)),
							elm$svg$Svg$Attributes$height(
							elm$core$String$fromInt(simonh1000$elm_colorpicker$ColorPicker$widgetHeight)),
							elm$svg$Svg$Attributes$fill('url(#pickerSaturation)')
						]),
					_List_Nil),
					A2(
					elm$svg$Svg$rect,
					_Utils_ap(
						_List_fromArray(
							[
								elm$svg$Svg$Attributes$width(
								elm$core$String$fromInt(simonh1000$elm_colorpicker$ColorPicker$widgetWidth)),
								elm$svg$Svg$Attributes$height(
								elm$core$String$fromInt(simonh1000$elm_colorpicker$ColorPicker$widgetHeight)),
								elm$svg$Svg$Attributes$fill('url(#pickerBrightness)')
							]),
						A3(
							simonh1000$elm_colorpicker$ColorPicker$svgDragAttrs,
							mouseTarget,
							simonh1000$elm_colorpicker$ColorPicker$SatLight(hue),
							simonh1000$elm_colorpicker$ColorPicker$OnMouseMove(
								simonh1000$elm_colorpicker$ColorPicker$SatLight(hue)))),
					_List_Nil)
				]));
	});
var simonh1000$elm_colorpicker$ColorPicker$sliderContainerStyles = function (name) {
	return _List_fromArray(
		[
			A2(
			elm$html$Html$Attributes$style,
			'width',
			elm$core$String$fromInt(simonh1000$elm_colorpicker$ColorPicker$widgetWidth) + 'px'),
			A2(elm$html$Html$Attributes$style, 'height', '12px'),
			A2(elm$html$Html$Attributes$style, 'marginTop', '8px'),
			elm$html$Html$Attributes$class('color-picker-slider ' + name)
		]);
};
var simonh1000$elm_colorpicker$ColorPicker$view = F2(
	function (col, _n0) {
		var model = _n0.a;
		var hsla = avh4$elm_color$Color$toHsla(col);
		var hue = A2(elm$core$Maybe$withDefault, hsla.hue, model.hue);
		var colCss = avh4$elm_color$Color$toCssString(
			A3(avh4$elm_color$Color$hsl, hue, 1, 0.5));
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					A2(elm$html$Html$Attributes$style, 'background-color', 'white'),
					A2(elm$html$Html$Attributes$style, 'padding', '6px'),
					A2(elm$html$Html$Attributes$style, 'display', 'inline-block'),
					A2(elm$html$Html$Attributes$style, 'border-radius', '5px'),
					A2(elm$html$Html$Attributes$style, 'box-shadow', 'rgba(0, 0, 0, 0.15) 0px 0px 0px 1px, rgba(0, 0, 0, 0.15) 0px 8px 16px'),
					elm$html$Html$Attributes$class('color-picker-container'),
					simonh1000$elm_colorpicker$ColorPicker$bubblePreventer
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					simonh1000$elm_colorpicker$ColorPicker$pickerStyles,
					_List_fromArray(
						[
							A3(simonh1000$elm_colorpicker$ColorPicker$satLightPalette, hue, colCss, model.mouseTarget),
							simonh1000$elm_colorpicker$ColorPicker$pickerIndicator(col)
						])),
					A2(
					elm$html$Html$div,
					_Utils_ap(
						simonh1000$elm_colorpicker$ColorPicker$pickerStyles,
						simonh1000$elm_colorpicker$ColorPicker$sliderContainerStyles('hue')),
					_List_fromArray(
						[
							simonh1000$elm_colorpicker$ColorPicker$huePalette(model.mouseTarget),
							simonh1000$elm_colorpicker$ColorPicker$hueMarker(hue)
						])),
					A2(
					elm$html$Html$div,
					_Utils_ap(
						simonh1000$elm_colorpicker$ColorPicker$checkedBkgStyles,
						_Utils_ap(
							simonh1000$elm_colorpicker$ColorPicker$pickerStyles,
							simonh1000$elm_colorpicker$ColorPicker$sliderContainerStyles('opacity'))),
					_List_fromArray(
						[
							A2(simonh1000$elm_colorpicker$ColorPicker$opacityPalette, hue, model),
							simonh1000$elm_colorpicker$ColorPicker$alphaMarker(hsla.alpha)
						]))
				]));
	});
var author$project$DiscreteGradientEditor$colorPickerView = F2(
	function (color, colorPicker) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('gradient-editor__color-picker')
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$map,
					author$project$DiscreteGradientEditor$ColorPickerMsg,
					A2(simonh1000$elm_colorpicker$ColorPicker$view, color, colorPicker))
				]));
	});
var author$project$DiscreteGradientEditor$colorSwatchView = function (color) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('gradient-editor__color-swatch'),
				A2(
				elm$html$Html$Attributes$style,
				'background-color',
				avh4$elm_color$Color$toCssString(color))
			]),
		_List_Nil);
};
var author$project$DiscreteGradientEditor$ClickGradientCell = function (a) {
	return {$: 'ClickGradientCell', a: a};
};
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var elm$html$Html$Attributes$classList = function (classes) {
	return elm$html$Html$Attributes$class(
		A2(
			elm$core$String$join,
			' ',
			A2(
				elm$core$List$map,
				elm$core$Tuple$first,
				A2(elm$core$List$filter, elm$core$Tuple$second, classes))));
};
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$DiscreteGradientEditor$cellView = F2(
	function (dg, val) {
		var color = A2(author$project$DiscreteGradient$getColorAt, val, dg);
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('gradient__cell', true),
							_Utils_Tuple2(
							'gradient__cell_has-stop',
							A2(author$project$DiscreteGradientEditor$hasStopAt, val, dg))
						])),
					A2(
					elm$html$Html$Attributes$style,
					'background-color',
					avh4$elm_color$Color$toCssString(color)),
					elm$html$Html$Events$onClick(
					author$project$DiscreteGradientEditor$ClickGradientCell(val))
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('tooltip')
						]),
					_List_fromArray(
						[
							elm$html$Html$text(
							elm$core$String$fromInt(val)),
							A2(
							elm$html$Html$div,
							_List_fromArray(
								[
									elm$html$Html$Attributes$class('tooltip__triangle')
								]),
							_List_Nil)
						]))
				]));
	});
var author$project$DiscreteGradientEditor$gradientView = F3(
	function (dg, gradientMin, gradientMax) {
		var cellViews = A2(
			elm$core$List$map,
			author$project$DiscreteGradientEditor$cellView(dg),
			A2(elm$core$List$range, gradientMin, gradientMax));
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('gradient')
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient__row')
						]),
					cellViews)
				]));
	});
var author$project$DiscreteGradientEditor$ChangeHex = function (a) {
	return {$: 'ChangeHex', a: a};
};
var author$project$DiscreteGradientEditor$floatTo255 = function (f) {
	return (f < 0.0) ? 0 : ((f > 1.0) ? 255 : elm$core$Basics$round(f * 255));
};
var author$project$DiscreteGradientEditor$toRgb255 = function (color) {
	var rgba = avh4$elm_color$Color$toRgba(color);
	return _Utils_Tuple3(
		author$project$DiscreteGradientEditor$floatTo255(rgba.red),
		author$project$DiscreteGradientEditor$floatTo255(rgba.green),
		author$project$DiscreteGradientEditor$floatTo255(rgba.blue));
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
var elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				elm$core$String$repeat,
				n - elm$core$String$length(string),
				elm$core$String$fromChar(_char)),
			string);
	});
var elm$core$String$fromList = _String_fromList;
var elm$core$Basics$modBy = _Basics_modBy;
var rtfeldman$elm_hex$Hex$unsafeToDigit = function (num) {
	unsafeToDigit:
	while (true) {
		switch (num) {
			case 0:
				return _Utils_chr('0');
			case 1:
				return _Utils_chr('1');
			case 2:
				return _Utils_chr('2');
			case 3:
				return _Utils_chr('3');
			case 4:
				return _Utils_chr('4');
			case 5:
				return _Utils_chr('5');
			case 6:
				return _Utils_chr('6');
			case 7:
				return _Utils_chr('7');
			case 8:
				return _Utils_chr('8');
			case 9:
				return _Utils_chr('9');
			case 10:
				return _Utils_chr('a');
			case 11:
				return _Utils_chr('b');
			case 12:
				return _Utils_chr('c');
			case 13:
				return _Utils_chr('d');
			case 14:
				return _Utils_chr('e');
			case 15:
				return _Utils_chr('f');
			default:
				var $temp$num = num;
				num = $temp$num;
				continue unsafeToDigit;
		}
	}
};
var rtfeldman$elm_hex$Hex$unsafePositiveToDigits = F2(
	function (digits, num) {
		unsafePositiveToDigits:
		while (true) {
			if (num < 16) {
				return A2(
					elm$core$List$cons,
					rtfeldman$elm_hex$Hex$unsafeToDigit(num),
					digits);
			} else {
				var $temp$digits = A2(
					elm$core$List$cons,
					rtfeldman$elm_hex$Hex$unsafeToDigit(
						A2(elm$core$Basics$modBy, 16, num)),
					digits),
					$temp$num = (num / 16) | 0;
				digits = $temp$digits;
				num = $temp$num;
				continue unsafePositiveToDigits;
			}
		}
	});
var rtfeldman$elm_hex$Hex$toString = function (num) {
	return elm$core$String$fromList(
		(num < 0) ? A2(
			elm$core$List$cons,
			_Utils_chr('-'),
			A2(rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, -num)) : A2(rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, num));
};
var author$project$DiscreteGradientEditor$toRgbHex = function (color) {
	var _n0 = author$project$DiscreteGradientEditor$toRgb255(color);
	var red = _n0.a;
	var green = _n0.b;
	var blue = _n0.c;
	var blueHex = A3(
		elm$core$String$padLeft,
		2,
		_Utils_chr('0'),
		rtfeldman$elm_hex$Hex$toString(blue));
	var greenHex = A3(
		elm$core$String$padLeft,
		2,
		_Utils_chr('0'),
		rtfeldman$elm_hex$Hex$toString(green));
	var redHex = A3(
		elm$core$String$padLeft,
		2,
		_Utils_chr('0'),
		rtfeldman$elm_hex$Hex$toString(red));
	return _Utils_ap(
		redHex,
		_Utils_ap(greenHex, blueHex));
};
var elm$html$Html$input = _VirtualDom_node('input');
var elm$json$Json$Encode$bool = _Json_wrap;
var elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$bool(bool));
	});
var elm$html$Html$Attributes$disabled = elm$html$Html$Attributes$boolProperty('disabled');
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$json$Json$Decode$string = _Json_decodeString;
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
var author$project$DiscreteGradientEditor$hexRowView = F2(
	function (color, isDisabled) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('gradient-editor__hex-row')
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__hex-label')
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Hex #')
						])),
					A2(
					elm$html$Html$input,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__hex-input'),
							elm$html$Html$Attributes$disabled(isDisabled),
							elm$html$Html$Attributes$value(
							author$project$DiscreteGradientEditor$toRgbHex(color)),
							elm$html$Html$Events$onInput(author$project$DiscreteGradientEditor$ChangeHex)
						]),
					_List_Nil)
				]));
	});
var author$project$DiscreteGradientEditor$ChangeHue = function (a) {
	return {$: 'ChangeHue', a: a};
};
var author$project$DiscreteGradientEditor$ChangeLightness = function (a) {
	return {$: 'ChangeLightness', a: a};
};
var author$project$DiscreteGradientEditor$ChangeSaturation = function (a) {
	return {$: 'ChangeSaturation', a: a};
};
var author$project$DiscreteGradientEditor$toHsl255 = function (color) {
	var hsla = avh4$elm_color$Color$toHsla(color);
	return _Utils_Tuple3(
		author$project$DiscreteGradientEditor$floatTo255(hsla.hue),
		author$project$DiscreteGradientEditor$floatTo255(hsla.saturation),
		author$project$DiscreteGradientEditor$floatTo255(hsla.lightness));
};
var elm$html$Html$Attributes$type_ = elm$html$Html$Attributes$stringProperty('type');
var author$project$DiscreteGradientEditor$hslColumnView = F2(
	function (color, isDisabled) {
		var _n0 = author$project$DiscreteGradientEditor$toHsl255(color);
		var hue = _n0.a;
		var saturation = _n0.b;
		var lightness = _n0.c;
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('gradient-editor__hsl-column')
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__column-header gradient-editor__column-item')
						]),
					_List_fromArray(
						[
							elm$html$Html$text('HSL')
						])),
					A2(
					elm$html$Html$input,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__number-input'),
							elm$html$Html$Attributes$class('gradient-editor__column-item'),
							elm$html$Html$Attributes$type_('number'),
							elm$html$Html$Attributes$disabled(isDisabled),
							elm$html$Html$Attributes$value(
							elm$core$String$fromInt(hue)),
							elm$html$Html$Events$onInput(author$project$DiscreteGradientEditor$ChangeHue)
						]),
					_List_Nil),
					A2(
					elm$html$Html$input,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__number-input'),
							elm$html$Html$Attributes$class('gradient-editor__column-item'),
							elm$html$Html$Attributes$type_('number'),
							elm$html$Html$Attributes$disabled(isDisabled),
							elm$html$Html$Attributes$value(
							elm$core$String$fromInt(saturation)),
							elm$html$Html$Events$onInput(author$project$DiscreteGradientEditor$ChangeSaturation)
						]),
					_List_Nil),
					A2(
					elm$html$Html$input,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__number-input'),
							elm$html$Html$Attributes$class('gradient-editor__column-item'),
							elm$html$Html$Attributes$type_('number'),
							elm$html$Html$Attributes$disabled(isDisabled),
							elm$html$Html$Attributes$value(
							elm$core$String$fromInt(lightness)),
							elm$html$Html$Events$onInput(author$project$DiscreteGradientEditor$ChangeLightness)
						]),
					_List_Nil)
				]));
	});
var author$project$DiscreteGradientEditor$ChangeBlue = function (a) {
	return {$: 'ChangeBlue', a: a};
};
var author$project$DiscreteGradientEditor$ChangeGreen = function (a) {
	return {$: 'ChangeGreen', a: a};
};
var author$project$DiscreteGradientEditor$ChangeRed = function (a) {
	return {$: 'ChangeRed', a: a};
};
var author$project$DiscreteGradientEditor$rgbColumnView = F2(
	function (color, isDisabled) {
		var _n0 = author$project$DiscreteGradientEditor$toRgb255(color);
		var red = _n0.a;
		var green = _n0.b;
		var blue = _n0.c;
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('gradient-editor__rgb-column')
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__column-header gradient-editor__column-item')
						]),
					_List_fromArray(
						[
							elm$html$Html$text('RGB')
						])),
					A2(
					elm$html$Html$input,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__number-input'),
							elm$html$Html$Attributes$class('gradient-editor__column-item'),
							elm$html$Html$Attributes$type_('number'),
							elm$html$Html$Attributes$disabled(isDisabled),
							elm$html$Html$Attributes$value(
							elm$core$String$fromInt(red)),
							elm$html$Html$Events$onInput(author$project$DiscreteGradientEditor$ChangeRed)
						]),
					_List_Nil),
					A2(
					elm$html$Html$input,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__number-input'),
							elm$html$Html$Attributes$class('gradient-editor__column-item'),
							elm$html$Html$Attributes$type_('number'),
							elm$html$Html$Attributes$disabled(isDisabled),
							elm$html$Html$Attributes$value(
							elm$core$String$fromInt(green)),
							elm$html$Html$Events$onInput(author$project$DiscreteGradientEditor$ChangeGreen)
						]),
					_List_Nil),
					A2(
					elm$html$Html$input,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__number-input'),
							elm$html$Html$Attributes$class('gradient-editor__column-item'),
							elm$html$Html$Attributes$type_('number'),
							elm$html$Html$Attributes$disabled(isDisabled),
							elm$html$Html$Attributes$value(
							elm$core$String$fromInt(blue)),
							elm$html$Html$Events$onInput(author$project$DiscreteGradientEditor$ChangeBlue)
						]),
					_List_Nil)
				]));
	});
var author$project$DiscreteGradientEditor$ChangeStopValue = function (a) {
	return {$: 'ChangeStopValue', a: a};
};
var author$project$DiscreteGradientEditor$ClickCreateStop = {$: 'ClickCreateStop'};
var author$project$DiscreteGradientEditor$ClickDeleteStop = {$: 'ClickDeleteStop'};
var elm$html$Html$button = _VirtualDom_node('button');
var author$project$DiscreteGradientEditor$stopColumnView = F2(
	function (dg, gradientVal) {
		var stopCount = elm$core$List$length(
			author$project$DiscreteGradient$getStops(dg));
		var hasStop = A2(author$project$DiscreteGradientEditor$hasStopAt, gradientVal, dg);
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('gradient-editor__stop-column')
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__column-header gradient-editor__column-item')
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Stop')
						])),
					A2(
					elm$html$Html$input,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__number-input'),
							elm$html$Html$Attributes$class('gradient-editor__column-item'),
							elm$html$Html$Attributes$type_('number'),
							elm$html$Html$Attributes$value(
							elm$core$String$fromInt(gradientVal)),
							elm$html$Html$Events$onInput(author$project$DiscreteGradientEditor$ChangeStopValue)
						]),
					_List_Nil),
					A2(
					elm$html$Html$button,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__column-item'),
							elm$html$Html$Attributes$disabled(hasStop),
							elm$html$Html$Events$onClick(author$project$DiscreteGradientEditor$ClickCreateStop)
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Create')
						])),
					A2(
					elm$html$Html$button,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('gradient-editor__column-item'),
							elm$html$Html$Attributes$disabled((!hasStop) || (stopCount <= 1)),
							elm$html$Html$Events$onClick(author$project$DiscreteGradientEditor$ClickDeleteStop)
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Delete')
						]))
				]));
	});
var author$project$DiscreteGradientEditor$view = function (_n0) {
	var model = _n0.a;
	var isStopAtSelectedValue = A2(author$project$DiscreteGradientEditor$hasStopAt, model.selectedValue, model.gradient);
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('gradient-editor')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('dialog__header'),
						elm$html$Html$Attributes$class('gradient-editor__header')
					]),
				_List_fromArray(
					[
						elm$html$Html$text('Edit Gradient')
					])),
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('gradient-editor__gradient')
					]),
				_List_fromArray(
					[
						A3(author$project$DiscreteGradientEditor$gradientView, model.gradient, model.gradientMin, model.gradientMax)
					])),
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('gradient-editor__color-and-stop-row')
					]),
				_List_fromArray(
					[
						author$project$DiscreteGradientEditor$colorSwatchView(model.selectedColor),
						A2(author$project$DiscreteGradientEditor$colorPickerView, model.selectedColor, model.colorPickerState),
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('gradient-editor__color-column')
							]),
						_List_fromArray(
							[
								A2(
								elm$html$Html$div,
								_List_fromArray(
									[
										elm$html$Html$Attributes$class('gradient-editor__rgb-hsl-row')
									]),
								_List_fromArray(
									[
										A2(author$project$DiscreteGradientEditor$rgbColumnView, model.selectedColor, !isStopAtSelectedValue),
										A2(author$project$DiscreteGradientEditor$hslColumnView, model.selectedColor, !isStopAtSelectedValue)
									])),
								A2(author$project$DiscreteGradientEditor$hexRowView, model.selectedColor, !isStopAtSelectedValue)
							])),
						A2(author$project$DiscreteGradientEditor$stopColumnView, model.gradient, model.selectedValue)
					])),
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('gradient-editor__close-button-row')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$button,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('gradient-editor__close-button'),
								elm$html$Html$Events$onClick(author$project$DiscreteGradientEditor$ClickCancel)
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Cancel')
							])),
						A2(
						elm$html$Html$button,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('gradient-editor__close-button'),
								elm$html$Html$Events$onClick(author$project$DiscreteGradientEditor$ClickSave)
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Save')
							]))
					]))
			]));
};
var author$project$MapEditor$GradientEditorMsg = function (a) {
	return {$: 'GradientEditorMsg', a: a};
};
var author$project$MapEditor$gradientEditorDialog = function (state) {
	return A2(
		elm$html$Html$map,
		author$project$MapEditor$GradientEditorMsg,
		author$project$DiscreteGradientEditor$view(state));
};
var author$project$MapEditor$NewLayerDialogCancel = {$: 'NewLayerDialogCancel'};
var author$project$MapEditor$NewLayerDialogCreate = {$: 'NewLayerDialogCreate'};
var author$project$MapEditor$SetNewLayerDialogNameField = function (a) {
	return {$: 'SetNewLayerDialogNameField', a: a};
};
var author$project$MapEditor$newLayerDialog = function (layerName) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('new-layer-dialog')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('dialog__header'),
						elm$html$Html$Attributes$class('new-layer-dialog__header')
					]),
				_List_fromArray(
					[
						elm$html$Html$text('New Layer')
					])),
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('new-layer-dialog__name-field')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('new-layer-dialog__name-label')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Name')
							])),
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('new-layer-dialog__name-label'),
								elm$html$Html$Attributes$value(layerName),
								elm$html$Html$Events$onInput(author$project$MapEditor$SetNewLayerDialogNameField)
							]),
						_List_Nil)
					])),
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('new-layer-dialog__button-container')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$button,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('new-layer-dialog__button'),
								elm$html$Html$Events$onClick(author$project$MapEditor$NewLayerDialogCancel)
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Cancel')
							])),
						A2(
						elm$html$Html$button,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('new-layer-dialog__button'),
								elm$html$Html$Events$onClick(author$project$MapEditor$NewLayerDialogCreate)
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Create')
							]))
					]))
			]));
};
var author$project$MapEditor$dialogContent = function (dialog) {
	if (dialog.$ === 'NewLayerDialog') {
		var layerName = dialog.a;
		return author$project$MapEditor$newLayerDialog(layerName);
	} else {
		var state = dialog.a;
		return author$project$MapEditor$gradientEditorDialog(state);
	}
};
var author$project$MapEditor$dialogView = function (dialog) {
	if (dialog.$ === 'Just') {
		var m = dialog.a;
		return _List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('mask')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('dialog')
							]),
						_List_fromArray(
							[
								author$project$MapEditor$dialogContent(m)
							]))
					]))
			]);
	} else {
		return _List_Nil;
	}
};
var author$project$GridEditor$darken = function (color) {
	var c = avh4$elm_color$Color$toHsla(color);
	return A4(
		avh4$elm_color$Color$hsla,
		c.hue,
		c.saturation,
		A2(elm$core$Basics$max, 0, c.lightness - 0.125),
		c.alpha);
};
var timjs$elm_collage$Collage$Flat = {$: 'Flat'};
var timjs$elm_collage$Collage$Sharp = {$: 'Sharp'};
var timjs$elm_collage$Collage$thin = 2.0;
var timjs$elm_collage$Collage$Core$Uniform = function (a) {
	return {$: 'Uniform', a: a};
};
var timjs$elm_collage$Collage$uniform = timjs$elm_collage$Collage$Core$Uniform;
var timjs$elm_collage$Collage$defaultLineStyle = {
	cap: timjs$elm_collage$Collage$Flat,
	dashPattern: _List_Nil,
	dashPhase: 0,
	fill: timjs$elm_collage$Collage$uniform(avh4$elm_color$Color$black),
	join: timjs$elm_collage$Collage$Sharp,
	thickness: timjs$elm_collage$Collage$thin
};
var timjs$elm_collage$Collage$broken = F3(
	function (dashes, thickness, fill) {
		return _Utils_update(
			timjs$elm_collage$Collage$defaultLineStyle,
			{dashPattern: dashes, fill: fill, thickness: thickness});
	});
var timjs$elm_collage$Collage$solid = timjs$elm_collage$Collage$broken(_List_Nil);
var timjs$elm_collage$Collage$Core$Rectangle = F3(
	function (a, b, c) {
		return {$: 'Rectangle', a: a, b: b, c: c};
	});
var timjs$elm_collage$Collage$roundedRectangle = timjs$elm_collage$Collage$Core$Rectangle;
var timjs$elm_collage$Collage$rectangle = F2(
	function (w, h) {
		return A3(timjs$elm_collage$Collage$roundedRectangle, w, h, 0);
	});
var timjs$elm_collage$Collage$square = function (size) {
	return A2(timjs$elm_collage$Collage$rectangle, size, size);
};
var timjs$elm_collage$Collage$Core$Shape = F2(
	function (a, b) {
		return {$: 'Shape', a: a, b: b};
	});
var timjs$elm_collage$Collage$Core$collage = function (basic) {
	return {
		basic: basic,
		handlers: _List_Nil,
		name: elm$core$Maybe$Nothing,
		opacity: 1,
		rotation: 0,
		scale: _Utils_Tuple2(1, 1),
		shift: _Utils_Tuple2(0, 0)
	};
};
var timjs$elm_collage$Collage$styled = function (style) {
	return A2(
		elm$core$Basics$composeL,
		timjs$elm_collage$Collage$Core$collage,
		timjs$elm_collage$Collage$Core$Shape(style));
};
var author$project$GridEditor$cellView = function (_n0) {
	var color = avh4$elm_color$Color$blue;
	var fill = timjs$elm_collage$Collage$uniform(color);
	var border = A2(
		timjs$elm_collage$Collage$solid,
		1.5,
		timjs$elm_collage$Collage$uniform(
			author$project$GridEditor$darken(color)));
	return A2(
		timjs$elm_collage$Collage$styled,
		_Utils_Tuple2(fill, border),
		timjs$elm_collage$Collage$square(30.5));
};
var timjs$elm_collage$Collage$Layout$Right = {$: 'Right'};
var timjs$elm_collage$Collage$shift = F2(
	function (_n0, collage) {
		var dx = _n0.a;
		var dy = _n0.b;
		var _n1 = collage.shift;
		var x = _n1.a;
		var y = _n1.b;
		return _Utils_update(
			collage,
			{
				shift: _Utils_Tuple2(x + dx, y + dy)
			});
	});
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
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
var elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(
			A3(elm$core$List$foldl, elm$core$Basics$max, x, xs));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(
			A3(elm$core$List$foldl, elm$core$Basics$min, x, xs));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$List$unzip = function (pairs) {
	var step = F2(
		function (_n0, _n1) {
			var x = _n0.a;
			var y = _n0.b;
			var xs = _n1.a;
			var ys = _n1.b;
			return _Utils_Tuple2(
				A2(elm$core$List$cons, x, xs),
				A2(elm$core$List$cons, y, ys));
		});
	return A3(
		elm$core$List$foldr,
		step,
		_Utils_Tuple2(_List_Nil, _List_Nil),
		pairs);
};
var timjs$elm_collage$Collage$Core$Path = F2(
	function (a, b) {
		return {$: 'Path', a: a, b: b};
	});
var elm$core$Basics$cos = _Basics_cos;
var elm$core$Basics$sin = _Basics_sin;
var timjs$elm_collage$Collage$Core$apply = function (_n0) {
	var shift = _n0.shift;
	var scale = _n0.scale;
	var rotation = _n0.rotation;
	var rotated = function (_n5) {
		var x = _n5.a;
		var y = _n5.b;
		var s = elm$core$Basics$sin(rotation);
		var c = elm$core$Basics$cos(rotation);
		return _Utils_Tuple2((c * x) - (s * y), (s * x) + (c * y));
	};
	var _n1 = scale;
	var sx = _n1.a;
	var sy = _n1.b;
	var scaled = function (_n4) {
		var x = _n4.a;
		var y = _n4.b;
		return _Utils_Tuple2(sx * x, sy * y);
	};
	var _n2 = shift;
	var dx = _n2.a;
	var dy = _n2.b;
	var shifted = function (_n3) {
		var x = _n3.a;
		var y = _n3.b;
		return _Utils_Tuple2(x + dx, y + dy);
	};
	return A2(
		elm$core$Basics$composeL,
		A2(elm$core$Basics$composeL, shifted, scaled),
		rotated);
};
var timjs$elm_collage$Collage$Layout$handlePoints = function (thickness) {
	var thicken = function (_n0) {
		var x = _n0.a;
		var y = _n0.b;
		var t = thickness / 2;
		return _Utils_Tuple2(
			(x < 0) ? (x - t) : (x + t),
			(y < 0) ? (y - t) : (y + t));
	};
	return elm$core$List$map(thicken);
};
var timjs$elm_collage$Collage$Layout$handleBox = F2(
	function (thickness, _n0) {
		var w = _n0.a;
		var h = _n0.b;
		var y = h / 2;
		var x = w / 2;
		return A2(
			timjs$elm_collage$Collage$Layout$handlePoints,
			thickness,
			_List_fromArray(
				[
					_Utils_Tuple2(-x, -y),
					_Utils_Tuple2(x, -y),
					_Utils_Tuple2(x, y),
					_Utils_Tuple2(-x, y)
				]));
	});
var timjs$elm_collage$Collage$Layout$unpack = function (_n0) {
	var toTop = _n0.toTop;
	var toBottom = _n0.toBottom;
	var toRight = _n0.toRight;
	var toLeft = _n0.toLeft;
	return _List_fromArray(
		[
			_Utils_Tuple2(-toLeft, -toBottom),
			_Utils_Tuple2(toRight, -toBottom),
			_Utils_Tuple2(toRight, toTop),
			_Utils_Tuple2(-toLeft, toTop)
		]);
};
var timjs$elm_collage$Collage$Layout$distances = function (col) {
	var points = timjs$elm_collage$Collage$Layout$handleBasic(col.basic);
	var _n8 = elm$core$List$unzip(
		A2(
			elm$core$List$map,
			timjs$elm_collage$Collage$Core$apply(col),
			points));
	var xs = _n8.a;
	var ys = _n8.b;
	return {
		toBottom: -A2(
			elm$core$Maybe$withDefault,
			0,
			elm$core$List$minimum(ys)),
		toLeft: -A2(
			elm$core$Maybe$withDefault,
			0,
			elm$core$List$minimum(xs)),
		toRight: A2(
			elm$core$Maybe$withDefault,
			0,
			elm$core$List$maximum(xs)),
		toTop: A2(
			elm$core$Maybe$withDefault,
			0,
			elm$core$List$maximum(ys))
	};
};
var timjs$elm_collage$Collage$Layout$handleBasic = function (basic) {
	handleBasic:
	while (true) {
		switch (basic.$) {
			case 'Shape':
				switch (basic.b.$) {
					case 'Circle':
						var _n1 = basic.a;
						var thickness = _n1.b.thickness;
						var r = basic.b.a;
						var d = 2 * r;
						return A2(
							timjs$elm_collage$Collage$Layout$handleBox,
							thickness,
							_Utils_Tuple2(d, d));
					case 'Ellipse':
						var _n2 = basic.a;
						var thickness = _n2.b.thickness;
						var _n3 = basic.b;
						var rx = _n3.a;
						var ry = _n3.b;
						return A2(
							timjs$elm_collage$Collage$Layout$handleBox,
							thickness,
							_Utils_Tuple2(2 * rx, 2 * ry));
					case 'Rectangle':
						var _n4 = basic.a;
						var thickness = _n4.b.thickness;
						var _n5 = basic.b;
						var w = _n5.a;
						var h = _n5.b;
						return A2(
							timjs$elm_collage$Collage$Layout$handleBox,
							thickness,
							_Utils_Tuple2(w, h));
					case 'Polygon':
						var _n6 = basic.a;
						var thickness = _n6.b.thickness;
						var ps = basic.b.a;
						return A2(timjs$elm_collage$Collage$Layout$handlePoints, thickness, ps);
					default:
						var _n7 = basic.a;
						var line = _n7.b;
						var path = basic.b.a;
						var $temp$basic = A2(timjs$elm_collage$Collage$Core$Path, line, path);
						basic = $temp$basic;
						continue handleBasic;
				}
			case 'Path':
				var thickness = basic.a.thickness;
				var cap = basic.a.cap;
				var ps = basic.b.a;
				return A2(
					timjs$elm_collage$Collage$Layout$handlePoints,
					_Utils_eq(cap, timjs$elm_collage$Collage$Flat) ? 0 : thickness,
					ps);
			case 'Text':
				var dims = basic.a;
				return A2(timjs$elm_collage$Collage$Layout$handleBox, 0, dims);
			case 'Image':
				var dims = basic.a;
				return A2(timjs$elm_collage$Collage$Layout$handleBox, 0, dims);
			case 'Html':
				var dims = basic.a;
				return A2(timjs$elm_collage$Collage$Layout$handleBox, 0, dims);
			case 'Group':
				var cols = basic.a;
				return A2(
					timjs$elm_collage$Collage$Layout$handlePoints,
					0,
					elm$core$List$concat(
						A2(
							elm$core$List$map,
							A2(elm$core$Basics$composeR, timjs$elm_collage$Collage$Layout$distances, timjs$elm_collage$Collage$Layout$unpack),
							cols)));
			default:
				var back = basic.b;
				return A2(
					timjs$elm_collage$Collage$Layout$handlePoints,
					0,
					timjs$elm_collage$Collage$Layout$unpack(
						timjs$elm_collage$Collage$Layout$distances(back)));
		}
	}
};
var timjs$elm_collage$Collage$Layout$envelope = F2(
	function (dir, col) {
		var _n0 = timjs$elm_collage$Collage$Layout$distances(col);
		var toTop = _n0.toTop;
		var toBottom = _n0.toBottom;
		var toLeft = _n0.toLeft;
		var toRight = _n0.toRight;
		switch (dir.$) {
			case 'Up':
				return toTop;
			case 'Down':
				return toBottom;
			case 'Right':
				return toRight;
			default:
				return toLeft;
		}
	});
var timjs$elm_collage$Collage$Layout$Down = {$: 'Down'};
var timjs$elm_collage$Collage$Layout$Left = {$: 'Left'};
var timjs$elm_collage$Collage$Layout$Up = {$: 'Up'};
var timjs$elm_collage$Collage$Layout$facing = function (dir) {
	switch (dir.$) {
		case 'Up':
			return timjs$elm_collage$Collage$Layout$Down;
		case 'Down':
			return timjs$elm_collage$Collage$Layout$Up;
		case 'Right':
			return timjs$elm_collage$Collage$Layout$Left;
		default:
			return timjs$elm_collage$Collage$Layout$Right;
	}
};
var timjs$elm_collage$Collage$Layout$place = F3(
	function (dir, a, b) {
		var len = A2(timjs$elm_collage$Collage$Layout$envelope, dir, a) + A2(
			timjs$elm_collage$Collage$Layout$envelope,
			timjs$elm_collage$Collage$Layout$facing(dir),
			b);
		var move = function () {
			switch (dir.$) {
				case 'Up':
					return _Utils_Tuple2(0, len);
				case 'Down':
					return _Utils_Tuple2(0, -len);
				case 'Right':
					return _Utils_Tuple2(len, 0);
				default:
					return _Utils_Tuple2(-len, 0);
			}
		}();
		return A2(timjs$elm_collage$Collage$shift, move, b);
	});
var timjs$elm_collage$Collage$Core$Group = function (a) {
	return {$: 'Group', a: a};
};
var timjs$elm_collage$Collage$group = A2(elm$core$Basics$composeL, timjs$elm_collage$Collage$Core$collage, timjs$elm_collage$Collage$Core$Group);
var timjs$elm_collage$Collage$Layout$stack = timjs$elm_collage$Collage$group;
var timjs$elm_collage$Collage$Layout$beside = F3(
	function (dir, a, b) {
		return timjs$elm_collage$Collage$Layout$stack(
			_List_fromArray(
				[
					a,
					A3(timjs$elm_collage$Collage$Layout$place, dir, a, b)
				]));
	});
var timjs$elm_collage$Collage$Core$Transparent = {$: 'Transparent'};
var timjs$elm_collage$Collage$transparent = timjs$elm_collage$Collage$Core$Transparent;
var timjs$elm_collage$Collage$invisible = A2(timjs$elm_collage$Collage$solid, 0, timjs$elm_collage$Collage$transparent);
var timjs$elm_collage$Collage$Layout$spacer = F2(
	function (w, h) {
		return A2(
			timjs$elm_collage$Collage$styled,
			_Utils_Tuple2(timjs$elm_collage$Collage$transparent, timjs$elm_collage$Collage$invisible),
			A2(timjs$elm_collage$Collage$rectangle, w, h));
	});
var timjs$elm_collage$Collage$Layout$empty = A2(timjs$elm_collage$Collage$Layout$spacer, 0, 0);
var timjs$elm_collage$Collage$Layout$horizontal = A2(
	elm$core$List$foldr,
	timjs$elm_collage$Collage$Layout$beside(timjs$elm_collage$Collage$Layout$Right),
	timjs$elm_collage$Collage$Layout$empty);
var author$project$GridEditor$rowView = function (cells) {
	return timjs$elm_collage$Collage$Layout$horizontal(
		A2(
			elm$core$List$map,
			author$project$GridEditor$cellView,
			elm$core$Array$toList(cells)));
};
var timjs$elm_collage$Collage$Layout$vertical = A2(
	elm$core$List$foldr,
	timjs$elm_collage$Collage$Layout$beside(timjs$elm_collage$Collage$Layout$Down),
	timjs$elm_collage$Collage$Layout$empty);
var author$project$GridEditor$gridView = function (rows) {
	return timjs$elm_collage$Collage$Layout$vertical(
		A2(
			elm$core$List$map,
			author$project$GridEditor$rowView,
			elm$core$Array$toList(rows)));
};
var timjs$elm_collage$Collage$opposite = function (_n0) {
	var x = _n0.a;
	var y = _n0.b;
	return _Utils_Tuple2(-x, -y);
};
var timjs$elm_collage$Collage$Layout$align = F2(
	function (anchor, col) {
		return A2(
			timjs$elm_collage$Collage$shift,
			timjs$elm_collage$Collage$opposite(
				anchor(col)),
			col);
	});
var timjs$elm_collage$Collage$Layout$height = function (col) {
	var _n0 = timjs$elm_collage$Collage$Layout$distances(col);
	var toTop = _n0.toTop;
	var toBottom = _n0.toBottom;
	return toTop + toBottom;
};
var timjs$elm_collage$Collage$Layout$topLeft = function (col) {
	var _n0 = timjs$elm_collage$Collage$Layout$distances(col);
	var toLeft = _n0.toLeft;
	var toTop = _n0.toTop;
	return _Utils_Tuple2(-toLeft, toTop);
};
var timjs$elm_collage$Collage$Layout$width = function (col) {
	var _n0 = timjs$elm_collage$Collage$Layout$distances(col);
	var toLeft = _n0.toLeft;
	var toRight = _n0.toRight;
	return toLeft + toRight;
};
var elm$svg$Svg$Attributes$version = _VirtualDom_attribute('version');
var elm$svg$Svg$circle = elm$svg$Svg$trustedNode('circle');
var elm$svg$Svg$ellipse = elm$svg$Svg$trustedNode('ellipse');
var elm$svg$Svg$foreignObject = elm$svg$Svg$trustedNode('foreignObject');
var elm$svg$Svg$g = elm$svg$Svg$trustedNode('g');
var elm$svg$Svg$image = elm$svg$Svg$trustedNode('image');
var elm$svg$Svg$polygon = elm$svg$Svg$trustedNode('polygon');
var elm$svg$Svg$polyline = elm$svg$Svg$trustedNode('polyline');
var elm$svg$Svg$text = elm$virtual_dom$VirtualDom$text;
var elm$svg$Svg$text_ = elm$svg$Svg$trustedNode('text');
var elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var elm$svg$Svg$Attributes$rx = _VirtualDom_attribute('rx');
var elm$svg$Svg$Attributes$ry = _VirtualDom_attribute('ry');
var elm$svg$Svg$Attributes$xlinkHref = function (value) {
	return A3(
		_VirtualDom_attributeNS,
		'http://www.w3.org/1999/xlink',
		'xlink:href',
		_VirtualDom_noJavaScriptUri(value));
};
var elm$svg$Svg$Attributes$dominantBaseline = _VirtualDom_attribute('dominant-baseline');
var elm$svg$Svg$Attributes$fillOpacity = _VirtualDom_attribute('fill-opacity');
var elm$svg$Svg$Attributes$fontFamily = _VirtualDom_attribute('font-family');
var elm$svg$Svg$Attributes$fontSize = _VirtualDom_attribute('font-size');
var elm$svg$Svg$Attributes$fontStyle = _VirtualDom_attribute('font-style');
var elm$svg$Svg$Attributes$fontVariant = _VirtualDom_attribute('font-variant');
var elm$svg$Svg$Attributes$fontWeight = _VirtualDom_attribute('font-weight');
var elm$svg$Svg$Attributes$opacity = _VirtualDom_attribute('opacity');
var elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var elm$svg$Svg$Attributes$strokeDasharray = _VirtualDom_attribute('stroke-dasharray');
var elm$svg$Svg$Attributes$strokeDashoffset = _VirtualDom_attribute('stroke-dashoffset');
var elm$svg$Svg$Attributes$strokeLinecap = _VirtualDom_attribute('stroke-linecap');
var elm$svg$Svg$Attributes$strokeLinejoin = _VirtualDom_attribute('stroke-linejoin');
var elm$svg$Svg$Attributes$strokeOpacity = _VirtualDom_attribute('stroke-opacity');
var elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var elm$svg$Svg$Attributes$textAnchor = _VirtualDom_attribute('text-anchor');
var elm$svg$Svg$Attributes$textDecoration = _VirtualDom_attribute('text-decoration');
var elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var timjs$elm_collage$Collage$Render$decodeCap = function (cap) {
	switch (cap.$) {
		case 'Round':
			return 'round';
		case 'Padded':
			return 'square';
		default:
			return 'butt';
	}
};
var timjs$elm_collage$Collage$Render$decodeDashing = function (ds) {
	var decodeOnOff = function (_n0) {
		var x = _n0.a;
		var y = _n0.b;
		return A2(
			elm$core$String$join,
			',',
			_List_fromArray(
				[
					elm$core$String$fromInt(x),
					elm$core$String$fromInt(y)
				]));
	};
	return A2(
		elm$core$String$join,
		' ',
		A2(elm$core$List$map, decodeOnOff, ds));
};
var avh4$elm_color$Color$rgb = F3(
	function (r, g, b) {
		return A4(avh4$elm_color$Color$RgbaSpace, r, g, b, 1.0);
	});
var timjs$elm_collage$Collage$Render$decodeColor = function (c) {
	var _n0 = avh4$elm_color$Color$toRgba(c);
	var red = _n0.red;
	var green = _n0.green;
	var blue = _n0.blue;
	return avh4$elm_color$Color$toCssString(
		A3(avh4$elm_color$Color$rgb, red, green, blue));
};
var timjs$elm_collage$Collage$Render$decodeFill = function (fs) {
	if (fs.$ === 'Uniform') {
		var c = fs.a;
		return timjs$elm_collage$Collage$Render$decodeColor(c);
	} else {
		return 'none';
	}
};
var timjs$elm_collage$Collage$Render$decodeOpacity = function (c) {
	var _n0 = avh4$elm_color$Color$toRgba(c);
	var alpha = _n0.alpha;
	return elm$core$String$fromFloat(alpha);
};
var timjs$elm_collage$Collage$Render$decodeFillOpacity = function (fs) {
	if (fs.$ === 'Uniform') {
		var c = fs.a;
		return timjs$elm_collage$Collage$Render$decodeOpacity(c);
	} else {
		return '0';
	}
};
var timjs$elm_collage$Collage$Render$decodeJoin = function (join) {
	switch (join.$) {
		case 'Smooth':
			return 'round';
		case 'Sharp':
			return 'miter';
		default:
			return 'bevel';
	}
};
var elm$core$Basics$pi = _Basics_pi;
var timjs$elm_collage$Collage$Render$decodeTransform = function (collage) {
	var sy = elm$core$String$fromFloat(collage.scale.b);
	var sx = elm$core$String$fromFloat(collage.scale.a);
	var r = elm$core$String$fromFloat((((-collage.rotation) / 2) / elm$core$Basics$pi) * 360);
	var dy = elm$core$String$fromFloat(-collage.shift.b);
	var dx = elm$core$String$fromFloat(collage.shift.a);
	return elm$core$String$concat(
		_List_fromArray(
			['translate(', dx, ',', dy, ') scale(', sx, ',', sy, ') rotate(', r, ')']));
};
var timjs$elm_collage$Collage$Render$attrs = function (collage) {
	var _n0 = collage.basic;
	switch (_n0.$) {
		case 'Path':
			var line = _n0.a;
			return _List_fromArray(
				[
					elm$svg$Svg$Attributes$stroke(
					timjs$elm_collage$Collage$Render$decodeFill(line.fill)),
					elm$svg$Svg$Attributes$strokeOpacity(
					timjs$elm_collage$Collage$Render$decodeFillOpacity(line.fill)),
					elm$svg$Svg$Attributes$strokeWidth(
					elm$core$String$fromFloat(line.thickness)),
					elm$svg$Svg$Attributes$strokeLinecap(
					timjs$elm_collage$Collage$Render$decodeCap(line.cap)),
					elm$svg$Svg$Attributes$strokeLinejoin(
					timjs$elm_collage$Collage$Render$decodeJoin(line.join)),
					elm$svg$Svg$Attributes$fill('none'),
					elm$svg$Svg$Attributes$opacity(
					elm$core$String$fromFloat(collage.opacity)),
					elm$svg$Svg$Attributes$transform(
					timjs$elm_collage$Collage$Render$decodeTransform(collage)),
					elm$svg$Svg$Attributes$strokeDashoffset(
					elm$core$String$fromInt(line.dashPhase)),
					elm$svg$Svg$Attributes$strokeDasharray(
					timjs$elm_collage$Collage$Render$decodeDashing(line.dashPattern))
				]);
		case 'Shape':
			var _n1 = _n0.a;
			var fill = _n1.a;
			var line = _n1.b;
			return _List_fromArray(
				[
					elm$svg$Svg$Attributes$fill(
					timjs$elm_collage$Collage$Render$decodeFill(fill)),
					elm$svg$Svg$Attributes$fillOpacity(
					timjs$elm_collage$Collage$Render$decodeFillOpacity(fill)),
					elm$svg$Svg$Attributes$stroke(
					timjs$elm_collage$Collage$Render$decodeFill(line.fill)),
					elm$svg$Svg$Attributes$strokeOpacity(
					timjs$elm_collage$Collage$Render$decodeFillOpacity(line.fill)),
					elm$svg$Svg$Attributes$strokeWidth(
					elm$core$String$fromFloat(line.thickness)),
					elm$svg$Svg$Attributes$strokeLinecap(
					timjs$elm_collage$Collage$Render$decodeCap(line.cap)),
					elm$svg$Svg$Attributes$strokeLinejoin(
					timjs$elm_collage$Collage$Render$decodeJoin(line.join)),
					elm$svg$Svg$Attributes$opacity(
					elm$core$String$fromFloat(collage.opacity)),
					elm$svg$Svg$Attributes$transform(
					timjs$elm_collage$Collage$Render$decodeTransform(collage)),
					elm$svg$Svg$Attributes$strokeDashoffset(
					elm$core$String$fromInt(line.dashPhase)),
					elm$svg$Svg$Attributes$strokeDasharray(
					timjs$elm_collage$Collage$Render$decodeDashing(line.dashPattern))
				]);
		case 'Text':
			var _n2 = _n0.b;
			var style = _n2.a;
			var str = _n2.b;
			return _List_fromArray(
				[
					elm$svg$Svg$Attributes$fill(
					timjs$elm_collage$Collage$Render$decodeFill(
						timjs$elm_collage$Collage$Core$Uniform(style.color))),
					elm$svg$Svg$Attributes$fontFamily(
					function () {
						var _n3 = style.typeface;
						switch (_n3.$) {
							case 'Serif':
								return 'serif';
							case 'Sansserif':
								return 'sans-serif';
							case 'Monospace':
								return 'monospace';
							default:
								var name = _n3.a;
								return name;
						}
					}()),
					elm$svg$Svg$Attributes$fontSize(
					elm$core$String$fromInt(style.size)),
					elm$svg$Svg$Attributes$fontWeight(
					function () {
						var _n4 = style.weight;
						switch (_n4.$) {
							case 'Thin':
								return '200';
							case 'Light':
								return '300';
							case 'Regular':
								return 'normal';
							case 'Medium':
								return '500';
							case 'SemiBold':
								return '600';
							case 'Bold':
								return 'bold';
							default:
								return '800';
						}
					}()),
					elm$svg$Svg$Attributes$fontStyle(
					function () {
						var _n5 = style.shape;
						switch (_n5.$) {
							case 'Upright':
								return 'normal';
							case 'SmallCaps':
								return 'normal';
							case 'Slanted':
								return 'oblique';
							default:
								return 'italic';
						}
					}()),
					elm$svg$Svg$Attributes$fontVariant(
					function () {
						var _n6 = style.shape;
						if (_n6.$ === 'SmallCaps') {
							return 'small-caps';
						} else {
							return 'normal';
						}
					}()),
					elm$svg$Svg$Attributes$textDecoration(
					function () {
						var _n7 = style.line;
						switch (_n7.$) {
							case 'None':
								return 'none';
							case 'Under':
								return 'underline';
							case 'Over':
								return 'overline';
							default:
								return 'line-through';
						}
					}()),
					elm$svg$Svg$Attributes$textAnchor('middle'),
					elm$svg$Svg$Attributes$dominantBaseline('middle'),
					elm$svg$Svg$Attributes$opacity(
					elm$core$String$fromFloat(collage.opacity)),
					elm$svg$Svg$Attributes$transform(
					timjs$elm_collage$Collage$Render$decodeTransform(collage))
				]);
		default:
			return _List_fromArray(
				[
					elm$svg$Svg$Attributes$opacity(
					elm$core$String$fromFloat(collage.opacity)),
					elm$svg$Svg$Attributes$transform(
					timjs$elm_collage$Collage$Render$decodeTransform(collage))
				]);
	}
};
var timjs$elm_collage$Collage$Render$box = F2(
	function (w, h) {
		return _List_fromArray(
			[
				elm$svg$Svg$Attributes$width(
				elm$core$String$fromFloat(w)),
				elm$svg$Svg$Attributes$height(
				elm$core$String$fromFloat(h)),
				elm$svg$Svg$Attributes$x(
				elm$core$String$fromFloat((-w) / 2)),
				elm$svg$Svg$Attributes$y(
				elm$core$String$fromFloat((-h) / 2))
			]);
	});
var timjs$elm_collage$Collage$Render$decodePoints = function (ps) {
	return A2(
		elm$core$String$join,
		' ',
		A2(
			elm$core$List$map,
			function (_n0) {
				var x = _n0.a;
				var y = _n0.b;
				return A2(
					elm$core$String$join,
					',',
					_List_fromArray(
						[
							elm$core$String$fromFloat(x),
							elm$core$String$fromFloat(-y)
						]));
			},
			ps));
};
var elm_community$basics_extra$Basics$Extra$uncurry = F2(
	function (f, _n0) {
		var a = _n0.a;
		var b = _n0.b;
		return A2(f, a, b);
	});
var timjs$elm_collage$Collage$Render$events = function (handlers) {
	return A2(
		elm$core$List$map,
		elm_community$basics_extra$Basics$Extra$uncurry(elm$svg$Svg$Events$on),
		handlers);
};
var timjs$elm_collage$Collage$Render$render = function (collage) {
	render:
	while (true) {
		var name = A2(elm$core$Maybe$withDefault, '_unnamed_', collage.name);
		var _n0 = collage.basic;
		switch (_n0.$) {
			case 'Path':
				var style = _n0.a;
				var path = _n0.b;
				var ps = path.a;
				return A2(
					elm$svg$Svg$polyline,
					_Utils_ap(
						_List_fromArray(
							[
								elm$svg$Svg$Attributes$id(name),
								elm$svg$Svg$Attributes$points(
								timjs$elm_collage$Collage$Render$decodePoints(ps))
							]),
						_Utils_ap(
							timjs$elm_collage$Collage$Render$attrs(collage),
							timjs$elm_collage$Collage$Render$events(collage.handlers))),
					_List_Nil);
			case 'Shape':
				var _n2 = _n0.a;
				var fill = _n2.a;
				var line = _n2.b;
				var shape = _n0.b;
				switch (shape.$) {
					case 'Polygon':
						var ps = shape.a;
						return A2(
							elm$svg$Svg$polygon,
							_Utils_ap(
								_List_fromArray(
									[
										elm$svg$Svg$Attributes$id(name),
										elm$svg$Svg$Attributes$points(
										timjs$elm_collage$Collage$Render$decodePoints(ps))
									]),
								_Utils_ap(
									timjs$elm_collage$Collage$Render$attrs(collage),
									timjs$elm_collage$Collage$Render$events(collage.handlers))),
							_List_Nil);
					case 'Circle':
						var r = shape.a;
						return A2(
							elm$svg$Svg$circle,
							_Utils_ap(
								_List_fromArray(
									[
										elm$svg$Svg$Attributes$id(name),
										elm$svg$Svg$Attributes$r(
										elm$core$String$fromFloat(r))
									]),
								_Utils_ap(
									timjs$elm_collage$Collage$Render$attrs(collage),
									timjs$elm_collage$Collage$Render$events(collage.handlers))),
							_List_Nil);
					case 'Ellipse':
						var rx = shape.a;
						var ry = shape.b;
						return A2(
							elm$svg$Svg$ellipse,
							_Utils_ap(
								_List_fromArray(
									[
										elm$svg$Svg$Attributes$id(name),
										elm$svg$Svg$Attributes$rx(
										elm$core$String$fromFloat(rx)),
										elm$svg$Svg$Attributes$ry(
										elm$core$String$fromFloat(ry))
									]),
								_Utils_ap(
									timjs$elm_collage$Collage$Render$attrs(collage),
									timjs$elm_collage$Collage$Render$events(collage.handlers))),
							_List_Nil);
					case 'Rectangle':
						var w = shape.a;
						var h = shape.b;
						var r = shape.c;
						return A2(
							elm$svg$Svg$rect,
							_Utils_ap(
								_List_fromArray(
									[
										elm$svg$Svg$Attributes$id(name),
										elm$svg$Svg$Attributes$rx(
										elm$core$String$fromFloat(r)),
										elm$svg$Svg$Attributes$ry(
										elm$core$String$fromFloat(r))
									]),
								_Utils_ap(
									A2(timjs$elm_collage$Collage$Render$box, w, h),
									_Utils_ap(
										timjs$elm_collage$Collage$Render$attrs(collage),
										timjs$elm_collage$Collage$Render$events(collage.handlers)))),
							_List_Nil);
					default:
						var path = shape.a;
						var $temp$collage = _Utils_update(
							collage,
							{
								basic: A2(timjs$elm_collage$Collage$Core$Path, line, path)
							});
						collage = $temp$collage;
						continue render;
				}
			case 'Text':
				var _n4 = _n0.b;
				var style = _n4.a;
				var str = _n4.b;
				return A2(
					elm$svg$Svg$text_,
					_Utils_ap(
						_List_fromArray(
							[
								elm$svg$Svg$Attributes$id(name)
							]),
						_Utils_ap(
							timjs$elm_collage$Collage$Render$attrs(collage),
							timjs$elm_collage$Collage$Render$events(collage.handlers))),
					_List_fromArray(
						[
							elm$svg$Svg$text(str)
						]));
			case 'Image':
				var _n5 = _n0.a;
				var w = _n5.a;
				var h = _n5.b;
				var url = _n0.b;
				return A2(
					elm$svg$Svg$image,
					_Utils_ap(
						_List_fromArray(
							[
								elm$svg$Svg$Attributes$id(name),
								elm$svg$Svg$Attributes$xlinkHref(url)
							]),
						_Utils_ap(
							A2(timjs$elm_collage$Collage$Render$box, w, h),
							_Utils_ap(
								timjs$elm_collage$Collage$Render$attrs(collage),
								timjs$elm_collage$Collage$Render$events(collage.handlers)))),
					_List_Nil);
			case 'Html':
				var _n6 = _n0.a;
				var w = _n6.a;
				var h = _n6.b;
				var html = _n0.b;
				return A2(
					elm$svg$Svg$foreignObject,
					_Utils_ap(
						_List_fromArray(
							[
								elm$svg$Svg$Attributes$id(name)
							]),
						_Utils_ap(
							A2(timjs$elm_collage$Collage$Render$box, w, h),
							_Utils_ap(
								timjs$elm_collage$Collage$Render$attrs(collage),
								timjs$elm_collage$Collage$Render$events(collage.handlers)))),
					_List_fromArray(
						[html]));
			case 'Group':
				var collages = _n0.a;
				return A2(
					elm$svg$Svg$g,
					A2(
						elm$core$List$cons,
						elm$svg$Svg$Attributes$id(name),
						_Utils_ap(
							timjs$elm_collage$Collage$Render$attrs(collage),
							timjs$elm_collage$Collage$Render$events(collage.handlers))),
					A3(
						elm$core$List$foldl,
						F2(
							function (col, res) {
								return A2(
									elm$core$List$cons,
									timjs$elm_collage$Collage$Render$render(col),
									res);
							}),
						_List_Nil,
						collages));
			default:
				var fore = _n0.a;
				var back = _n0.b;
				var $temp$collage = _Utils_update(
					collage,
					{
						basic: timjs$elm_collage$Collage$Core$Group(
							_List_fromArray(
								[fore, back]))
					});
				collage = $temp$collage;
				continue render;
		}
	}
};
var timjs$elm_collage$Collage$Render$svgAbsolute = F2(
	function (_n0, collage) {
		var width = _n0.a;
		var height = _n0.b;
		var w = elm$core$String$fromFloat(width);
		var h = elm$core$String$fromFloat(height);
		return A2(
			elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					elm$svg$Svg$svg,
					_List_fromArray(
						[
							elm$svg$Svg$Attributes$width(w),
							elm$svg$Svg$Attributes$height(h),
							elm$svg$Svg$Attributes$version('1.1')
						]),
					_List_fromArray(
						[
							timjs$elm_collage$Collage$Render$render(collage)
						]))
				]));
	});
var timjs$elm_collage$Collage$Render$svg = function (collage) {
	return A2(
		timjs$elm_collage$Collage$Render$svgAbsolute,
		_Utils_Tuple2(
			timjs$elm_collage$Collage$Layout$width(collage),
			timjs$elm_collage$Collage$Layout$height(collage)),
		A2(timjs$elm_collage$Collage$Layout$align, timjs$elm_collage$Collage$Layout$topLeft, collage));
};
var author$project$GridEditor$view = function (_n0) {
	var model = _n0.a;
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('grid-editor')
			]),
		_List_fromArray(
			[
				timjs$elm_collage$Collage$Render$svg(
				author$project$GridEditor$gridView(model.grid))
			]));
};
var author$project$MapEditor$GridEditorMsg = function (a) {
	return {$: 'GridEditorMsg', a: a};
};
var elm_community$maybe_extra$Maybe$Extra$toList = function (m) {
	if (m.$ === 'Nothing') {
		return _List_Nil;
	} else {
		var x = m.a;
		return _List_fromArray(
			[x]);
	}
};
var author$project$MapEditor$gridEditorPaneView = function (maybeGridEditor) {
	var content = elm_community$maybe_extra$Maybe$Extra$toList(
		A2(
			elm$core$Maybe$map,
			elm$html$Html$map(author$project$MapEditor$GridEditorMsg),
			A2(elm$core$Maybe$map, author$project$GridEditor$view, maybeGridEditor)));
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('grid-editor-pane')
			]),
		content);
};
var author$project$MapEditor$colorFieldLabel = A2(
	elm$html$Html$div,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('color-field__label')
		]),
	_List_fromArray(
		[
			elm$html$Html$text('Color')
		]));
var author$project$MapEditor$OpenGradientEditorDialog = {$: 'OpenGradientEditorDialog'};
var author$project$MapEditor$colorGradientEditButton = function (isDisabled) {
	return A2(
		elm$html$Html$button,
		_List_fromArray(
			[
				elm$html$Html$Attributes$disabled(isDisabled),
				elm$html$Html$Events$onClick(author$project$MapEditor$OpenGradientEditorDialog)
			]),
		_List_fromArray(
			[
				elm$html$Html$text('Edit')
			]));
};
var author$project$MapEditor$disabledColorGradient = A2(
	elm$html$Html$div,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('color-gradient'),
			elm$html$Html$Attributes$class('color-gradient_disabled')
		]),
	_List_Nil);
var author$project$MapEditor$simpleGradientCell = F2(
	function (gradient, val) {
		var color = A2(author$project$DiscreteGradient$getColorAt, val, gradient);
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('simple-gradient__cell'),
					A2(
					elm$html$Html$Attributes$style,
					'background-color',
					avh4$elm_color$Color$toCssString(color))
				]),
			_List_Nil);
	});
var author$project$MapEditor$simpleGradient = F3(
	function (gradient, layerMin, layerMax) {
		var cells = A2(
			elm$core$List$map,
			author$project$MapEditor$simpleGradientCell(gradient),
			A2(elm$core$List$range, layerMin, layerMax));
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('simple-gradient')
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('simple-gradient__row')
						]),
					cells)
				]));
	});
var author$project$MapEditor$enabledColorGradient = F3(
	function (gradient, layerMin, layerMax) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('color-gradient')
				]),
			_List_fromArray(
				[
					A3(author$project$MapEditor$simpleGradient, gradient, layerMin, layerMax)
				]));
	});
var author$project$MapEditor$colorGradientRow = function (maybeLayer) {
	var contents = function () {
		if (maybeLayer.$ === 'Just') {
			var layer = maybeLayer.a;
			return _List_fromArray(
				[
					A3(
					author$project$MapEditor$enabledColorGradient,
					author$project$Layer$getColorGradient(layer),
					author$project$Layer$getMin(layer),
					author$project$Layer$getMax(layer)),
					author$project$MapEditor$colorGradientEditButton(false)
				]);
		} else {
			return _List_fromArray(
				[
					author$project$MapEditor$disabledColorGradient,
					author$project$MapEditor$colorGradientEditButton(true)
				]);
		}
	}();
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('color-gradient__row')
			]),
		contents);
};
var author$project$MapEditor$colorGradientField = function (layer) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('color-field')
			]),
		_List_fromArray(
			[
				author$project$MapEditor$colorFieldLabel,
				author$project$MapEditor$colorGradientRow(layer)
			]));
};
var author$project$MapEditor$getSelectedTool = function (state) {
	return A2(
		elm$core$Maybe$map,
		function (s) {
			return s.tool;
		},
		state.layerSelection);
};
var author$project$MapEditor$DeleteSelectedLayer = {$: 'DeleteSelectedLayer'};
var author$project$MapEditor$layerFieldDeleteButton = A2(
	elm$html$Html$button,
	_List_fromArray(
		[
			elm$html$Html$Events$onClick(author$project$MapEditor$DeleteSelectedLayer)
		]),
	_List_fromArray(
		[
			elm$html$Html$text('-')
		]));
var author$project$MapEditor$OpenNewLayerDialog = {$: 'OpenNewLayerDialog'};
var author$project$MapEditor$layerFieldNewButton = A2(
	elm$html$Html$button,
	_List_fromArray(
		[
			elm$html$Html$Events$onClick(author$project$MapEditor$OpenNewLayerDialog)
		]),
	_List_fromArray(
		[
			elm$html$Html$text('+')
		]));
var elm$html$Html$option = _VirtualDom_node('option');
var elm$html$Html$Attributes$selected = elm$html$Html$Attributes$boolProperty('selected');
var author$project$MapEditor$layerFieldEmptyOption = function (isSelected) {
	return A2(
		elm$html$Html$option,
		_List_fromArray(
			[
				elm$html$Html$Attributes$value(''),
				elm$html$Html$Attributes$selected(isSelected)
			]),
		_List_fromArray(
			[
				elm$html$Html$text('')
			]));
};
var author$project$MapEditor$isJustEqual = F2(
	function (maybeX, y) {
		if (maybeX.$ === 'Nothing') {
			return false;
		} else {
			var x = maybeX.a;
			return _Utils_eq(x, y);
		}
	});
var author$project$Layer$getName = function (_n0) {
	var inner = _n0.a;
	return inner.name;
};
var author$project$MapEditor$layerFieldOption = F3(
	function (index, layer, isSelected) {
		return A2(
			elm$html$Html$option,
			_List_fromArray(
				[
					elm$html$Html$Attributes$value(
					elm$core$String$fromInt(index)),
					elm$html$Html$Attributes$selected(isSelected)
				]),
			_List_fromArray(
				[
					elm$html$Html$text(
					author$project$Layer$getName(layer))
				]));
	});
var author$project$MapEditor$layerFieldOptions = function (selectedIndex) {
	return elm$core$List$indexedMap(
		F2(
			function (i, layer) {
				return A3(
					author$project$MapEditor$layerFieldOption,
					i,
					layer,
					A2(author$project$MapEditor$isJustEqual, selectedIndex, i));
			}));
};
var author$project$MapEditor$SelectLayer = function (a) {
	return {$: 'SelectLayer', a: a};
};
var author$project$MapEditor$toSelectLayerMsg = A2(elm$core$Basics$composeL, author$project$MapEditor$SelectLayer, elm$core$String$toInt);
var elm$html$Html$select = _VirtualDom_node('select');
var elm_community$maybe_extra$Maybe$Extra$isNothing = function (m) {
	if (m.$ === 'Nothing') {
		return true;
	} else {
		return false;
	}
};
var author$project$MapEditor$layerFieldSelect = F2(
	function (selectedIndex, layers) {
		var emptyOption = author$project$MapEditor$layerFieldEmptyOption(
			elm_community$maybe_extra$Maybe$Extra$isNothing(selectedIndex));
		var options = _Utils_ap(
			_List_fromArray(
				[emptyOption]),
			A2(author$project$MapEditor$layerFieldOptions, selectedIndex, layers));
		return A2(
			elm$html$Html$select,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('layer-field__select'),
					elm$html$Html$Events$onInput(author$project$MapEditor$toSelectLayerMsg)
				]),
			options);
	});
var author$project$MapEditor$layerField = F2(
	function (selectedIndex, layers) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('layer-field')
				]),
			_List_fromArray(
				[
					A2(author$project$MapEditor$layerFieldSelect, selectedIndex, layers),
					author$project$MapEditor$layerFieldNewButton,
					author$project$MapEditor$layerFieldDeleteButton
				]));
	});
var author$project$MapEditor$disabledMinMaxFieldInput = A2(
	elm$html$Html$input,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('min-max-field__input'),
			elm$html$Html$Attributes$type_('number'),
			elm$html$Html$Attributes$disabled(true)
		]),
	_List_Nil);
var author$project$MapEditor$enabledMinMaxFieldInput = F2(
	function (val, toMsg) {
		return A2(
			elm$html$Html$input,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('min-max-field__input'),
					elm$html$Html$Attributes$type_('number'),
					elm$html$Html$Attributes$value(
					elm$core$String$fromInt(val)),
					elm$html$Html$Events$onInput(toMsg)
				]),
			_List_Nil);
	});
var author$project$MapEditor$SetLayerMax = function (a) {
	return {$: 'SetLayerMax', a: a};
};
var author$project$MapEditor$toSetLayerMaxMsg = A2(elm$core$Basics$composeL, author$project$MapEditor$SetLayerMax, elm$core$String$toInt);
var author$project$MapEditor$maxInput = function (val) {
	if (val.$ === 'Just') {
		var v = val.a;
		return A2(author$project$MapEditor$enabledMinMaxFieldInput, v, author$project$MapEditor$toSetLayerMaxMsg);
	} else {
		return author$project$MapEditor$disabledMinMaxFieldInput;
	}
};
var author$project$MapEditor$SetLayerMin = function (a) {
	return {$: 'SetLayerMin', a: a};
};
var author$project$MapEditor$toSetLayerMinMsg = A2(elm$core$Basics$composeL, author$project$MapEditor$SetLayerMin, elm$core$String$toInt);
var author$project$MapEditor$minInput = function (val) {
	if (val.$ === 'Just') {
		var v = val.a;
		return A2(author$project$MapEditor$enabledMinMaxFieldInput, v, author$project$MapEditor$toSetLayerMinMsg);
	} else {
		return author$project$MapEditor$disabledMinMaxFieldInput;
	}
};
var author$project$MapEditor$minMaxFieldInputSeparator = A2(
	elm$html$Html$div,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('min-max-field__input-separator')
		]),
	_List_fromArray(
		[
			elm$html$Html$text('/')
		]));
var author$project$MapEditor$minMaxFieldLabel = A2(
	elm$html$Html$div,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('min-max-field__label')
		]),
	_List_fromArray(
		[
			elm$html$Html$text('Min / Max')
		]));
var author$project$MapEditor$minMaxField = function (layer) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('min-max-field')
			]),
		_List_fromArray(
			[
				author$project$MapEditor$minMaxFieldLabel,
				author$project$MapEditor$minInput(
				A2(elm$core$Maybe$map, author$project$Layer$getMin, layer)),
				author$project$MapEditor$minMaxFieldInputSeparator,
				author$project$MapEditor$maxInput(
				A2(elm$core$Maybe$map, author$project$Layer$getMax, layer))
			]));
};
var author$project$MapEditor$toolbarSectionContents = function (contents) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('toolbar__section-contents')
			]),
		contents);
};
var author$project$MapEditor$toolbarSectionHeader = function (header) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('toolbar__section-header')
			]),
		_List_fromArray(
			[
				elm$html$Html$text(header)
			]));
};
var author$project$MapEditor$toolbar = function (state) {
	var tool = author$project$MapEditor$getSelectedTool(state);
	var layerIndex = author$project$MapEditor$getSelectedLayerIndex(state);
	var layer = author$project$MapEditor$getSelectedLayer(state);
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('toolbar')
			]),
		_List_fromArray(
			[
				author$project$MapEditor$toolbarSectionHeader('Layer'),
				author$project$MapEditor$toolbarSectionContents(
				_List_fromArray(
					[
						A2(author$project$MapEditor$layerField, layerIndex, state.layers),
						author$project$MapEditor$minMaxField(layer),
						author$project$MapEditor$colorGradientField(layer)
					])),
				author$project$MapEditor$toolbarSectionHeader('Tools'),
				author$project$MapEditor$toolbarSectionHeader('Brush')
			]));
};
var author$project$MapEditor$view = function (state) {
	var content = _Utils_ap(
		_List_fromArray(
			[
				author$project$MapEditor$toolbar(state),
				author$project$MapEditor$gridEditorPaneView(
				author$project$MapEditor$getGridEditor(state))
			]),
		author$project$MapEditor$dialogView(
			author$project$MapEditor$getDialog(state)));
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('page')
			]),
		content);
};
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
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$core$Task$init = elm$core$Task$succeed(_Utils_Tuple0);
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
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$url$Url$Http = {$: 'Http'};
var elm$url$Url$Https = {$: 'Https'};
var elm$core$String$indexes = _String_indexes;
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
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
var elm$browser$Browser$element = _Browser_element;
var elm$json$Json$Decode$andThen = _Json_andThen;
var elm$json$Json$Decode$float = _Json_decodeFloat;
var elm$json$Json$Decode$index = _Json_decodeIndex;
var author$project$Main$main = elm$browser$Browser$element(
	{init: author$project$MapEditor$init, subscriptions: author$project$Main$subscriptions, update: author$project$MapEditor$update, view: author$project$MapEditor$view});
_Platform_export({'Main':{'init':author$project$Main$main(
	A2(
		elm$json$Json$Decode$andThen,
		function (x0) {
			return A2(
				elm$json$Json$Decode$andThen,
				function (x1) {
					return elm$json$Json$Decode$succeed(
						_Utils_Tuple2(x0, x1));
				},
				A2(elm$json$Json$Decode$index, 1, elm$json$Json$Decode$float));
		},
		A2(elm$json$Json$Decode$index, 0, elm$json$Json$Decode$float)))(0)}});}(this));