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



// DECODER

var _File_decoder = _Json_decodePrim(function(value) {
	// NOTE: checks if `File` exists in case this is run on node
	return (typeof File !== 'undefined' && value instanceof File)
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FILE', value);
});


// METADATA

function _File_name(file) { return file.name; }
function _File_mime(file) { return file.type; }
function _File_size(file) { return file.size; }

function _File_lastModified(file)
{
	return elm$time$Time$millisToPosix(file.lastModified);
}


// DOWNLOAD

var _File_downloadNode;

function _File_getDownloadNode()
{
	return _File_downloadNode || (_File_downloadNode = document.createElement('a'));
}

var _File_download = F3(function(name, mime, content)
{
	return _Scheduler_binding(function(callback)
	{
		var blob = new Blob([content], {type: mime});

		// for IE10+
		if (navigator.msSaveOrOpenBlob)
		{
			navigator.msSaveOrOpenBlob(blob, name);
			return;
		}

		// for HTML5
		var node = _File_getDownloadNode();
		var objectUrl = URL.createObjectURL(blob);
		node.href = objectUrl;
		node.download = name;
		_File_click(node);
		URL.revokeObjectURL(objectUrl);
	});
});

function _File_downloadUrl(href)
{
	return _Scheduler_binding(function(callback)
	{
		var node = _File_getDownloadNode();
		node.href = href;
		node.download = '';
		node.origin === location.origin || (node.target = '_blank');
		_File_click(node);
	});
}


// IE COMPATIBILITY

function _File_makeBytesSafeForInternetExplorer(bytes)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/10
	// all other browsers can just run `new Blob([bytes])` directly with no problem
	//
	return new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength);
}

function _File_click(node)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/11
	// all other browsers have MouseEvent and do not need this conditional stuff
	//
	if (typeof MouseEvent === 'function')
	{
		node.dispatchEvent(new MouseEvent('click'));
	}
	else
	{
		var event = document.createEvent('MouseEvents');
		event.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
		document.body.appendChild(node);
		node.dispatchEvent(event);
		document.body.removeChild(node);
	}
}


// UPLOAD

var _File_node;

function _File_uploadOne(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.accept = A2(elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			callback(_Scheduler_succeed(event.target.files[0]));
		});
		_File_click(_File_node);
	});
}

function _File_uploadOneOrMore(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.multiple = true;
		_File_node.accept = A2(elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			var elmFiles = _List_fromArray(event.target.files);
			callback(_Scheduler_succeed(_Utils_Tuple2(elmFiles.a, elmFiles.b)));
		});
		_File_click(_File_node);
	});
}


// CONTENT

function _File_toString(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsText(blob);
		return function() { reader.abort(); };
	});
}

function _File_toBytes(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(new DataView(reader.result)));
		});
		reader.readAsArrayBuffer(blob);
		return function() { reader.abort(); };
	});
}

function _File_toUrl(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsDataURL(blob);
		return function() { reader.abort(); };
	});
}



/*
 * Copyright (c) 2010 Mozilla Corporation
 * Copyright (c) 2010 Vladimir Vukicevic
 * Copyright (c) 2013 John Mayer
 * Copyright (c) 2018 Andrey Kuzmin
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

// Vector2

var _MJS_v2 = F2(function(x, y) {
    return new Float64Array([x, y]);
});

var _MJS_v2getX = function(a) {
    return a[0];
};

var _MJS_v2getY = function(a) {
    return a[1];
};

var _MJS_v2setX = F2(function(x, a) {
    return new Float64Array([x, a[1]]);
});

var _MJS_v2setY = F2(function(y, a) {
    return new Float64Array([a[0], y]);
});

var _MJS_v2toRecord = function(a) {
    return { x: a[0], y: a[1] };
};

var _MJS_v2fromRecord = function(r) {
    return new Float64Array([r.x, r.y]);
};

var _MJS_v2add = F2(function(a, b) {
    var r = new Float64Array(2);
    r[0] = a[0] + b[0];
    r[1] = a[1] + b[1];
    return r;
});

var _MJS_v2sub = F2(function(a, b) {
    var r = new Float64Array(2);
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    return r;
});

var _MJS_v2negate = function(a) {
    var r = new Float64Array(2);
    r[0] = -a[0];
    r[1] = -a[1];
    return r;
};

var _MJS_v2direction = F2(function(a, b) {
    var r = new Float64Array(2);
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    var im = 1.0 / _MJS_v2lengthLocal(r);
    r[0] = r[0] * im;
    r[1] = r[1] * im;
    return r;
});

function _MJS_v2lengthLocal(a) {
    return Math.sqrt(a[0] * a[0] + a[1] * a[1]);
}
var _MJS_v2length = _MJS_v2lengthLocal;

var _MJS_v2lengthSquared = function(a) {
    return a[0] * a[0] + a[1] * a[1];
};

var _MJS_v2distance = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    return Math.sqrt(dx * dx + dy * dy);
});

var _MJS_v2distanceSquared = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    return dx * dx + dy * dy;
});

var _MJS_v2normalize = function(a) {
    var r = new Float64Array(2);
    var im = 1.0 / _MJS_v2lengthLocal(a);
    r[0] = a[0] * im;
    r[1] = a[1] * im;
    return r;
};

var _MJS_v2scale = F2(function(k, a) {
    var r = new Float64Array(2);
    r[0] = a[0] * k;
    r[1] = a[1] * k;
    return r;
});

var _MJS_v2dot = F2(function(a, b) {
    return a[0] * b[0] + a[1] * b[1];
});

// Vector3

var _MJS_v3temp1Local = new Float64Array(3);
var _MJS_v3temp2Local = new Float64Array(3);
var _MJS_v3temp3Local = new Float64Array(3);

var _MJS_v3 = F3(function(x, y, z) {
    return new Float64Array([x, y, z]);
});

var _MJS_v3getX = function(a) {
    return a[0];
};

var _MJS_v3getY = function(a) {
    return a[1];
};

var _MJS_v3getZ = function(a) {
    return a[2];
};

var _MJS_v3setX = F2(function(x, a) {
    return new Float64Array([x, a[1], a[2]]);
});

var _MJS_v3setY = F2(function(y, a) {
    return new Float64Array([a[0], y, a[2]]);
});

var _MJS_v3setZ = F2(function(z, a) {
    return new Float64Array([a[0], a[1], z]);
});

var _MJS_v3toRecord = function(a) {
    return { x: a[0], y: a[1], z: a[2] };
};

var _MJS_v3fromRecord = function(r) {
    return new Float64Array([r.x, r.y, r.z]);
};

var _MJS_v3add = F2(function(a, b) {
    var r = new Float64Array(3);
    r[0] = a[0] + b[0];
    r[1] = a[1] + b[1];
    r[2] = a[2] + b[2];
    return r;
});

function _MJS_v3subLocal(a, b, r) {
    if (r === undefined) {
        r = new Float64Array(3);
    }
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    r[2] = a[2] - b[2];
    return r;
}
var _MJS_v3sub = F2(_MJS_v3subLocal);

var _MJS_v3negate = function(a) {
    var r = new Float64Array(3);
    r[0] = -a[0];
    r[1] = -a[1];
    r[2] = -a[2];
    return r;
};

function _MJS_v3directionLocal(a, b, r) {
    if (r === undefined) {
        r = new Float64Array(3);
    }
    return _MJS_v3normalizeLocal(_MJS_v3subLocal(a, b, r), r);
}
var _MJS_v3direction = F2(_MJS_v3directionLocal);

function _MJS_v3lengthLocal(a) {
    return Math.sqrt(a[0] * a[0] + a[1] * a[1] + a[2] * a[2]);
}
var _MJS_v3length = _MJS_v3lengthLocal;

var _MJS_v3lengthSquared = function(a) {
    return a[0] * a[0] + a[1] * a[1] + a[2] * a[2];
};

var _MJS_v3distance = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    var dz = a[2] - b[2];
    return Math.sqrt(dx * dx + dy * dy + dz * dz);
});

var _MJS_v3distanceSquared = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    var dz = a[2] - b[2];
    return dx * dx + dy * dy + dz * dz;
});

function _MJS_v3normalizeLocal(a, r) {
    if (r === undefined) {
        r = new Float64Array(3);
    }
    var im = 1.0 / _MJS_v3lengthLocal(a);
    r[0] = a[0] * im;
    r[1] = a[1] * im;
    r[2] = a[2] * im;
    return r;
}
var _MJS_v3normalize = _MJS_v3normalizeLocal;

var _MJS_v3scale = F2(function(k, a) {
    return new Float64Array([a[0] * k, a[1] * k, a[2] * k]);
});

var _MJS_v3dotLocal = function(a, b) {
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
};
var _MJS_v3dot = F2(_MJS_v3dotLocal);

function _MJS_v3crossLocal(a, b, r) {
    if (r === undefined) {
        r = new Float64Array(3);
    }
    r[0] = a[1] * b[2] - a[2] * b[1];
    r[1] = a[2] * b[0] - a[0] * b[2];
    r[2] = a[0] * b[1] - a[1] * b[0];
    return r;
}
var _MJS_v3cross = F2(_MJS_v3crossLocal);

var _MJS_v3mul4x4 = F2(function(m, v) {
    var w;
    var tmp = _MJS_v3temp1Local;
    var r = new Float64Array(3);

    tmp[0] = m[3];
    tmp[1] = m[7];
    tmp[2] = m[11];
    w = _MJS_v3dotLocal(v, tmp) + m[15];
    tmp[0] = m[0];
    tmp[1] = m[4];
    tmp[2] = m[8];
    r[0] = (_MJS_v3dotLocal(v, tmp) + m[12]) / w;
    tmp[0] = m[1];
    tmp[1] = m[5];
    tmp[2] = m[9];
    r[1] = (_MJS_v3dotLocal(v, tmp) + m[13]) / w;
    tmp[0] = m[2];
    tmp[1] = m[6];
    tmp[2] = m[10];
    r[2] = (_MJS_v3dotLocal(v, tmp) + m[14]) / w;
    return r;
});

// Vector4

var _MJS_v4 = F4(function(x, y, z, w) {
    return new Float64Array([x, y, z, w]);
});

var _MJS_v4getX = function(a) {
    return a[0];
};

var _MJS_v4getY = function(a) {
    return a[1];
};

var _MJS_v4getZ = function(a) {
    return a[2];
};

var _MJS_v4getW = function(a) {
    return a[3];
};

var _MJS_v4setX = F2(function(x, a) {
    return new Float64Array([x, a[1], a[2], a[3]]);
});

var _MJS_v4setY = F2(function(y, a) {
    return new Float64Array([a[0], y, a[2], a[3]]);
});

var _MJS_v4setZ = F2(function(z, a) {
    return new Float64Array([a[0], a[1], z, a[3]]);
});

var _MJS_v4setW = F2(function(w, a) {
    return new Float64Array([a[0], a[1], a[2], w]);
});

var _MJS_v4toRecord = function(a) {
    return { x: a[0], y: a[1], z: a[2], w: a[3] };
};

var _MJS_v4fromRecord = function(r) {
    return new Float64Array([r.x, r.y, r.z, r.w]);
};

var _MJS_v4add = F2(function(a, b) {
    var r = new Float64Array(4);
    r[0] = a[0] + b[0];
    r[1] = a[1] + b[1];
    r[2] = a[2] + b[2];
    r[3] = a[3] + b[3];
    return r;
});

var _MJS_v4sub = F2(function(a, b) {
    var r = new Float64Array(4);
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    r[2] = a[2] - b[2];
    r[3] = a[3] - b[3];
    return r;
});

var _MJS_v4negate = function(a) {
    var r = new Float64Array(4);
    r[0] = -a[0];
    r[1] = -a[1];
    r[2] = -a[2];
    r[3] = -a[3];
    return r;
};

var _MJS_v4direction = F2(function(a, b) {
    var r = new Float64Array(4);
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    r[2] = a[2] - b[2];
    r[3] = a[3] - b[3];
    var im = 1.0 / _MJS_v4lengthLocal(r);
    r[0] = r[0] * im;
    r[1] = r[1] * im;
    r[2] = r[2] * im;
    r[3] = r[3] * im;
    return r;
});

function _MJS_v4lengthLocal(a) {
    return Math.sqrt(a[0] * a[0] + a[1] * a[1] + a[2] * a[2] + a[3] * a[3]);
}
var _MJS_v4length = _MJS_v4lengthLocal;

var _MJS_v4lengthSquared = function(a) {
    return a[0] * a[0] + a[1] * a[1] + a[2] * a[2] + a[3] * a[3];
};

var _MJS_v4distance = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    var dz = a[2] - b[2];
    var dw = a[3] - b[3];
    return Math.sqrt(dx * dx + dy * dy + dz * dz + dw * dw);
});

var _MJS_v4distanceSquared = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    var dz = a[2] - b[2];
    var dw = a[3] - b[3];
    return dx * dx + dy * dy + dz * dz + dw * dw;
});

var _MJS_v4normalize = function(a) {
    var r = new Float64Array(4);
    var im = 1.0 / _MJS_v4lengthLocal(a);
    r[0] = a[0] * im;
    r[1] = a[1] * im;
    r[2] = a[2] * im;
    r[3] = a[3] * im;
    return r;
};

var _MJS_v4scale = F2(function(k, a) {
    var r = new Float64Array(4);
    r[0] = a[0] * k;
    r[1] = a[1] * k;
    r[2] = a[2] * k;
    r[3] = a[3] * k;
    return r;
});

var _MJS_v4dot = F2(function(a, b) {
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * b[3];
});

// Matrix4

var _MJS_m4x4temp1Local = new Float64Array(16);
var _MJS_m4x4temp2Local = new Float64Array(16);

var _MJS_m4x4identity = new Float64Array([
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0
]);

var _MJS_m4x4fromRecord = function(r) {
    var m = new Float64Array(16);
    m[0] = r.m11;
    m[1] = r.m21;
    m[2] = r.m31;
    m[3] = r.m41;
    m[4] = r.m12;
    m[5] = r.m22;
    m[6] = r.m32;
    m[7] = r.m42;
    m[8] = r.m13;
    m[9] = r.m23;
    m[10] = r.m33;
    m[11] = r.m43;
    m[12] = r.m14;
    m[13] = r.m24;
    m[14] = r.m34;
    m[15] = r.m44;
    return m;
};

var _MJS_m4x4toRecord = function(m) {
    return {
        m11: m[0], m21: m[1], m31: m[2], m41: m[3],
        m12: m[4], m22: m[5], m32: m[6], m42: m[7],
        m13: m[8], m23: m[9], m33: m[10], m43: m[11],
        m14: m[12], m24: m[13], m34: m[14], m44: m[15]
    };
};

var _MJS_m4x4inverse = function(m) {
    var r = new Float64Array(16);

    r[0] = m[5] * m[10] * m[15] - m[5] * m[11] * m[14] - m[9] * m[6] * m[15] +
        m[9] * m[7] * m[14] + m[13] * m[6] * m[11] - m[13] * m[7] * m[10];
    r[4] = -m[4] * m[10] * m[15] + m[4] * m[11] * m[14] + m[8] * m[6] * m[15] -
        m[8] * m[7] * m[14] - m[12] * m[6] * m[11] + m[12] * m[7] * m[10];
    r[8] = m[4] * m[9] * m[15] - m[4] * m[11] * m[13] - m[8] * m[5] * m[15] +
        m[8] * m[7] * m[13] + m[12] * m[5] * m[11] - m[12] * m[7] * m[9];
    r[12] = -m[4] * m[9] * m[14] + m[4] * m[10] * m[13] + m[8] * m[5] * m[14] -
        m[8] * m[6] * m[13] - m[12] * m[5] * m[10] + m[12] * m[6] * m[9];
    r[1] = -m[1] * m[10] * m[15] + m[1] * m[11] * m[14] + m[9] * m[2] * m[15] -
        m[9] * m[3] * m[14] - m[13] * m[2] * m[11] + m[13] * m[3] * m[10];
    r[5] = m[0] * m[10] * m[15] - m[0] * m[11] * m[14] - m[8] * m[2] * m[15] +
        m[8] * m[3] * m[14] + m[12] * m[2] * m[11] - m[12] * m[3] * m[10];
    r[9] = -m[0] * m[9] * m[15] + m[0] * m[11] * m[13] + m[8] * m[1] * m[15] -
        m[8] * m[3] * m[13] - m[12] * m[1] * m[11] + m[12] * m[3] * m[9];
    r[13] = m[0] * m[9] * m[14] - m[0] * m[10] * m[13] - m[8] * m[1] * m[14] +
        m[8] * m[2] * m[13] + m[12] * m[1] * m[10] - m[12] * m[2] * m[9];
    r[2] = m[1] * m[6] * m[15] - m[1] * m[7] * m[14] - m[5] * m[2] * m[15] +
        m[5] * m[3] * m[14] + m[13] * m[2] * m[7] - m[13] * m[3] * m[6];
    r[6] = -m[0] * m[6] * m[15] + m[0] * m[7] * m[14] + m[4] * m[2] * m[15] -
        m[4] * m[3] * m[14] - m[12] * m[2] * m[7] + m[12] * m[3] * m[6];
    r[10] = m[0] * m[5] * m[15] - m[0] * m[7] * m[13] - m[4] * m[1] * m[15] +
        m[4] * m[3] * m[13] + m[12] * m[1] * m[7] - m[12] * m[3] * m[5];
    r[14] = -m[0] * m[5] * m[14] + m[0] * m[6] * m[13] + m[4] * m[1] * m[14] -
        m[4] * m[2] * m[13] - m[12] * m[1] * m[6] + m[12] * m[2] * m[5];
    r[3] = -m[1] * m[6] * m[11] + m[1] * m[7] * m[10] + m[5] * m[2] * m[11] -
        m[5] * m[3] * m[10] - m[9] * m[2] * m[7] + m[9] * m[3] * m[6];
    r[7] = m[0] * m[6] * m[11] - m[0] * m[7] * m[10] - m[4] * m[2] * m[11] +
        m[4] * m[3] * m[10] + m[8] * m[2] * m[7] - m[8] * m[3] * m[6];
    r[11] = -m[0] * m[5] * m[11] + m[0] * m[7] * m[9] + m[4] * m[1] * m[11] -
        m[4] * m[3] * m[9] - m[8] * m[1] * m[7] + m[8] * m[3] * m[5];
    r[15] = m[0] * m[5] * m[10] - m[0] * m[6] * m[9] - m[4] * m[1] * m[10] +
        m[4] * m[2] * m[9] + m[8] * m[1] * m[6] - m[8] * m[2] * m[5];

    var det = m[0] * r[0] + m[1] * r[4] + m[2] * r[8] + m[3] * r[12];

    if (det === 0) {
        return elm$core$Maybe$Nothing;
    }

    det = 1.0 / det;

    for (var i = 0; i < 16; i = i + 1) {
        r[i] = r[i] * det;
    }

    return elm$core$Maybe$Just(r);
};

var _MJS_m4x4inverseOrthonormal = function(m) {
    var r = _MJS_m4x4transposeLocal(m);
    var t = [m[12], m[13], m[14]];
    r[3] = r[7] = r[11] = 0;
    r[12] = -_MJS_v3dotLocal([r[0], r[4], r[8]], t);
    r[13] = -_MJS_v3dotLocal([r[1], r[5], r[9]], t);
    r[14] = -_MJS_v3dotLocal([r[2], r[6], r[10]], t);
    return r;
};

function _MJS_m4x4makeFrustumLocal(left, right, bottom, top, znear, zfar) {
    var r = new Float64Array(16);

    r[0] = 2 * znear / (right - left);
    r[1] = 0;
    r[2] = 0;
    r[3] = 0;
    r[4] = 0;
    r[5] = 2 * znear / (top - bottom);
    r[6] = 0;
    r[7] = 0;
    r[8] = (right + left) / (right - left);
    r[9] = (top + bottom) / (top - bottom);
    r[10] = -(zfar + znear) / (zfar - znear);
    r[11] = -1;
    r[12] = 0;
    r[13] = 0;
    r[14] = -2 * zfar * znear / (zfar - znear);
    r[15] = 0;

    return r;
}
var _MJS_m4x4makeFrustum = F6(_MJS_m4x4makeFrustumLocal);

var _MJS_m4x4makePerspective = F4(function(fovy, aspect, znear, zfar) {
    var ymax = znear * Math.tan(fovy * Math.PI / 360.0);
    var ymin = -ymax;
    var xmin = ymin * aspect;
    var xmax = ymax * aspect;

    return _MJS_m4x4makeFrustumLocal(xmin, xmax, ymin, ymax, znear, zfar);
});

function _MJS_m4x4makeOrthoLocal(left, right, bottom, top, znear, zfar) {
    var r = new Float64Array(16);

    r[0] = 2 / (right - left);
    r[1] = 0;
    r[2] = 0;
    r[3] = 0;
    r[4] = 0;
    r[5] = 2 / (top - bottom);
    r[6] = 0;
    r[7] = 0;
    r[8] = 0;
    r[9] = 0;
    r[10] = -2 / (zfar - znear);
    r[11] = 0;
    r[12] = -(right + left) / (right - left);
    r[13] = -(top + bottom) / (top - bottom);
    r[14] = -(zfar + znear) / (zfar - znear);
    r[15] = 1;

    return r;
}
var _MJS_m4x4makeOrtho = F6(_MJS_m4x4makeOrthoLocal);

var _MJS_m4x4makeOrtho2D = F4(function(left, right, bottom, top) {
    return _MJS_m4x4makeOrthoLocal(left, right, bottom, top, -1, 1);
});

function _MJS_m4x4mulLocal(a, b) {
    var r = new Float64Array(16);
    var a11 = a[0];
    var a21 = a[1];
    var a31 = a[2];
    var a41 = a[3];
    var a12 = a[4];
    var a22 = a[5];
    var a32 = a[6];
    var a42 = a[7];
    var a13 = a[8];
    var a23 = a[9];
    var a33 = a[10];
    var a43 = a[11];
    var a14 = a[12];
    var a24 = a[13];
    var a34 = a[14];
    var a44 = a[15];
    var b11 = b[0];
    var b21 = b[1];
    var b31 = b[2];
    var b41 = b[3];
    var b12 = b[4];
    var b22 = b[5];
    var b32 = b[6];
    var b42 = b[7];
    var b13 = b[8];
    var b23 = b[9];
    var b33 = b[10];
    var b43 = b[11];
    var b14 = b[12];
    var b24 = b[13];
    var b34 = b[14];
    var b44 = b[15];

    r[0] = a11 * b11 + a12 * b21 + a13 * b31 + a14 * b41;
    r[1] = a21 * b11 + a22 * b21 + a23 * b31 + a24 * b41;
    r[2] = a31 * b11 + a32 * b21 + a33 * b31 + a34 * b41;
    r[3] = a41 * b11 + a42 * b21 + a43 * b31 + a44 * b41;
    r[4] = a11 * b12 + a12 * b22 + a13 * b32 + a14 * b42;
    r[5] = a21 * b12 + a22 * b22 + a23 * b32 + a24 * b42;
    r[6] = a31 * b12 + a32 * b22 + a33 * b32 + a34 * b42;
    r[7] = a41 * b12 + a42 * b22 + a43 * b32 + a44 * b42;
    r[8] = a11 * b13 + a12 * b23 + a13 * b33 + a14 * b43;
    r[9] = a21 * b13 + a22 * b23 + a23 * b33 + a24 * b43;
    r[10] = a31 * b13 + a32 * b23 + a33 * b33 + a34 * b43;
    r[11] = a41 * b13 + a42 * b23 + a43 * b33 + a44 * b43;
    r[12] = a11 * b14 + a12 * b24 + a13 * b34 + a14 * b44;
    r[13] = a21 * b14 + a22 * b24 + a23 * b34 + a24 * b44;
    r[14] = a31 * b14 + a32 * b24 + a33 * b34 + a34 * b44;
    r[15] = a41 * b14 + a42 * b24 + a43 * b34 + a44 * b44;

    return r;
}
var _MJS_m4x4mul = F2(_MJS_m4x4mulLocal);

var _MJS_m4x4mulAffine = F2(function(a, b) {
    var r = new Float64Array(16);
    var a11 = a[0];
    var a21 = a[1];
    var a31 = a[2];
    var a12 = a[4];
    var a22 = a[5];
    var a32 = a[6];
    var a13 = a[8];
    var a23 = a[9];
    var a33 = a[10];
    var a14 = a[12];
    var a24 = a[13];
    var a34 = a[14];

    var b11 = b[0];
    var b21 = b[1];
    var b31 = b[2];
    var b12 = b[4];
    var b22 = b[5];
    var b32 = b[6];
    var b13 = b[8];
    var b23 = b[9];
    var b33 = b[10];
    var b14 = b[12];
    var b24 = b[13];
    var b34 = b[14];

    r[0] = a11 * b11 + a12 * b21 + a13 * b31;
    r[1] = a21 * b11 + a22 * b21 + a23 * b31;
    r[2] = a31 * b11 + a32 * b21 + a33 * b31;
    r[3] = 0;
    r[4] = a11 * b12 + a12 * b22 + a13 * b32;
    r[5] = a21 * b12 + a22 * b22 + a23 * b32;
    r[6] = a31 * b12 + a32 * b22 + a33 * b32;
    r[7] = 0;
    r[8] = a11 * b13 + a12 * b23 + a13 * b33;
    r[9] = a21 * b13 + a22 * b23 + a23 * b33;
    r[10] = a31 * b13 + a32 * b23 + a33 * b33;
    r[11] = 0;
    r[12] = a11 * b14 + a12 * b24 + a13 * b34 + a14;
    r[13] = a21 * b14 + a22 * b24 + a23 * b34 + a24;
    r[14] = a31 * b14 + a32 * b24 + a33 * b34 + a34;
    r[15] = 1;

    return r;
});

var _MJS_m4x4makeRotate = F2(function(angle, axis) {
    var r = new Float64Array(16);
    axis = _MJS_v3normalizeLocal(axis, _MJS_v3temp1Local);
    var x = axis[0];
    var y = axis[1];
    var z = axis[2];
    var c = Math.cos(angle);
    var c1 = 1 - c;
    var s = Math.sin(angle);

    r[0] = x * x * c1 + c;
    r[1] = y * x * c1 + z * s;
    r[2] = z * x * c1 - y * s;
    r[3] = 0;
    r[4] = x * y * c1 - z * s;
    r[5] = y * y * c1 + c;
    r[6] = y * z * c1 + x * s;
    r[7] = 0;
    r[8] = x * z * c1 + y * s;
    r[9] = y * z * c1 - x * s;
    r[10] = z * z * c1 + c;
    r[11] = 0;
    r[12] = 0;
    r[13] = 0;
    r[14] = 0;
    r[15] = 1;

    return r;
});

var _MJS_m4x4rotate = F3(function(angle, axis, m) {
    var r = new Float64Array(16);
    var im = 1.0 / _MJS_v3lengthLocal(axis);
    var x = axis[0] * im;
    var y = axis[1] * im;
    var z = axis[2] * im;
    var c = Math.cos(angle);
    var c1 = 1 - c;
    var s = Math.sin(angle);
    var xs = x * s;
    var ys = y * s;
    var zs = z * s;
    var xyc1 = x * y * c1;
    var xzc1 = x * z * c1;
    var yzc1 = y * z * c1;
    var t11 = x * x * c1 + c;
    var t21 = xyc1 + zs;
    var t31 = xzc1 - ys;
    var t12 = xyc1 - zs;
    var t22 = y * y * c1 + c;
    var t32 = yzc1 + xs;
    var t13 = xzc1 + ys;
    var t23 = yzc1 - xs;
    var t33 = z * z * c1 + c;
    var m11 = m[0], m21 = m[1], m31 = m[2], m41 = m[3];
    var m12 = m[4], m22 = m[5], m32 = m[6], m42 = m[7];
    var m13 = m[8], m23 = m[9], m33 = m[10], m43 = m[11];
    var m14 = m[12], m24 = m[13], m34 = m[14], m44 = m[15];

    r[0] = m11 * t11 + m12 * t21 + m13 * t31;
    r[1] = m21 * t11 + m22 * t21 + m23 * t31;
    r[2] = m31 * t11 + m32 * t21 + m33 * t31;
    r[3] = m41 * t11 + m42 * t21 + m43 * t31;
    r[4] = m11 * t12 + m12 * t22 + m13 * t32;
    r[5] = m21 * t12 + m22 * t22 + m23 * t32;
    r[6] = m31 * t12 + m32 * t22 + m33 * t32;
    r[7] = m41 * t12 + m42 * t22 + m43 * t32;
    r[8] = m11 * t13 + m12 * t23 + m13 * t33;
    r[9] = m21 * t13 + m22 * t23 + m23 * t33;
    r[10] = m31 * t13 + m32 * t23 + m33 * t33;
    r[11] = m41 * t13 + m42 * t23 + m43 * t33;
    r[12] = m14,
    r[13] = m24;
    r[14] = m34;
    r[15] = m44;

    return r;
});

function _MJS_m4x4makeScale3Local(x, y, z) {
    var r = new Float64Array(16);

    r[0] = x;
    r[1] = 0;
    r[2] = 0;
    r[3] = 0;
    r[4] = 0;
    r[5] = y;
    r[6] = 0;
    r[7] = 0;
    r[8] = 0;
    r[9] = 0;
    r[10] = z;
    r[11] = 0;
    r[12] = 0;
    r[13] = 0;
    r[14] = 0;
    r[15] = 1;

    return r;
}
var _MJS_m4x4makeScale3 = F3(_MJS_m4x4makeScale3Local);

var _MJS_m4x4makeScale = function(v) {
    return _MJS_m4x4makeScale3Local(v[0], v[1], v[2]);
};

var _MJS_m4x4scale3 = F4(function(x, y, z, m) {
    var r = new Float64Array(16);

    r[0] = m[0] * x;
    r[1] = m[1] * x;
    r[2] = m[2] * x;
    r[3] = m[3] * x;
    r[4] = m[4] * y;
    r[5] = m[5] * y;
    r[6] = m[6] * y;
    r[7] = m[7] * y;
    r[8] = m[8] * z;
    r[9] = m[9] * z;
    r[10] = m[10] * z;
    r[11] = m[11] * z;
    r[12] = m[12];
    r[13] = m[13];
    r[14] = m[14];
    r[15] = m[15];

    return r;
});

var _MJS_m4x4scale = F2(function(v, m) {
    var r = new Float64Array(16);
    var x = v[0];
    var y = v[1];
    var z = v[2];

    r[0] = m[0] * x;
    r[1] = m[1] * x;
    r[2] = m[2] * x;
    r[3] = m[3] * x;
    r[4] = m[4] * y;
    r[5] = m[5] * y;
    r[6] = m[6] * y;
    r[7] = m[7] * y;
    r[8] = m[8] * z;
    r[9] = m[9] * z;
    r[10] = m[10] * z;
    r[11] = m[11] * z;
    r[12] = m[12];
    r[13] = m[13];
    r[14] = m[14];
    r[15] = m[15];

    return r;
});

function _MJS_m4x4makeTranslate3Local(x, y, z) {
    var r = new Float64Array(16);

    r[0] = 1;
    r[1] = 0;
    r[2] = 0;
    r[3] = 0;
    r[4] = 0;
    r[5] = 1;
    r[6] = 0;
    r[7] = 0;
    r[8] = 0;
    r[9] = 0;
    r[10] = 1;
    r[11] = 0;
    r[12] = x;
    r[13] = y;
    r[14] = z;
    r[15] = 1;

    return r;
}
var _MJS_m4x4makeTranslate3 = F3(_MJS_m4x4makeTranslate3Local);

var _MJS_m4x4makeTranslate = function(v) {
    return _MJS_m4x4makeTranslate3Local(v[0], v[1], v[2]);
};

var _MJS_m4x4translate3 = F4(function(x, y, z, m) {
    var r = new Float64Array(16);
    var m11 = m[0];
    var m21 = m[1];
    var m31 = m[2];
    var m41 = m[3];
    var m12 = m[4];
    var m22 = m[5];
    var m32 = m[6];
    var m42 = m[7];
    var m13 = m[8];
    var m23 = m[9];
    var m33 = m[10];
    var m43 = m[11];

    r[0] = m11;
    r[1] = m21;
    r[2] = m31;
    r[3] = m41;
    r[4] = m12;
    r[5] = m22;
    r[6] = m32;
    r[7] = m42;
    r[8] = m13;
    r[9] = m23;
    r[10] = m33;
    r[11] = m43;
    r[12] = m11 * x + m12 * y + m13 * z + m[12];
    r[13] = m21 * x + m22 * y + m23 * z + m[13];
    r[14] = m31 * x + m32 * y + m33 * z + m[14];
    r[15] = m41 * x + m42 * y + m43 * z + m[15];

    return r;
});

var _MJS_m4x4translate = F2(function(v, m) {
    var r = new Float64Array(16);
    var x = v[0];
    var y = v[1];
    var z = v[2];
    var m11 = m[0];
    var m21 = m[1];
    var m31 = m[2];
    var m41 = m[3];
    var m12 = m[4];
    var m22 = m[5];
    var m32 = m[6];
    var m42 = m[7];
    var m13 = m[8];
    var m23 = m[9];
    var m33 = m[10];
    var m43 = m[11];

    r[0] = m11;
    r[1] = m21;
    r[2] = m31;
    r[3] = m41;
    r[4] = m12;
    r[5] = m22;
    r[6] = m32;
    r[7] = m42;
    r[8] = m13;
    r[9] = m23;
    r[10] = m33;
    r[11] = m43;
    r[12] = m11 * x + m12 * y + m13 * z + m[12];
    r[13] = m21 * x + m22 * y + m23 * z + m[13];
    r[14] = m31 * x + m32 * y + m33 * z + m[14];
    r[15] = m41 * x + m42 * y + m43 * z + m[15];

    return r;
});

var _MJS_m4x4makeLookAt = F3(function(eye, center, up) {
    var z = _MJS_v3directionLocal(eye, center, _MJS_v3temp1Local);
    var x = _MJS_v3normalizeLocal(_MJS_v3crossLocal(up, z, _MJS_v3temp2Local), _MJS_v3temp2Local);
    var y = _MJS_v3normalizeLocal(_MJS_v3crossLocal(z, x, _MJS_v3temp3Local), _MJS_v3temp3Local);
    var tm1 = _MJS_m4x4temp1Local;
    var tm2 = _MJS_m4x4temp2Local;

    tm1[0] = x[0];
    tm1[1] = y[0];
    tm1[2] = z[0];
    tm1[3] = 0;
    tm1[4] = x[1];
    tm1[5] = y[1];
    tm1[6] = z[1];
    tm1[7] = 0;
    tm1[8] = x[2];
    tm1[9] = y[2];
    tm1[10] = z[2];
    tm1[11] = 0;
    tm1[12] = 0;
    tm1[13] = 0;
    tm1[14] = 0;
    tm1[15] = 1;

    tm2[0] = 1; tm2[1] = 0; tm2[2] = 0; tm2[3] = 0;
    tm2[4] = 0; tm2[5] = 1; tm2[6] = 0; tm2[7] = 0;
    tm2[8] = 0; tm2[9] = 0; tm2[10] = 1; tm2[11] = 0;
    tm2[12] = -eye[0]; tm2[13] = -eye[1]; tm2[14] = -eye[2]; tm2[15] = 1;

    return _MJS_m4x4mulLocal(tm1, tm2);
});


function _MJS_m4x4transposeLocal(m) {
    var r = new Float64Array(16);

    r[0] = m[0]; r[1] = m[4]; r[2] = m[8]; r[3] = m[12];
    r[4] = m[1]; r[5] = m[5]; r[6] = m[9]; r[7] = m[13];
    r[8] = m[2]; r[9] = m[6]; r[10] = m[10]; r[11] = m[14];
    r[12] = m[3]; r[13] = m[7]; r[14] = m[11]; r[15] = m[15];

    return r;
}
var _MJS_m4x4transpose = _MJS_m4x4transposeLocal;

var _MJS_m4x4makeBasis = F3(function(vx, vy, vz) {
    var r = new Float64Array(16);

    r[0] = vx[0];
    r[1] = vx[1];
    r[2] = vx[2];
    r[3] = 0;
    r[4] = vy[0];
    r[5] = vy[1];
    r[6] = vy[2];
    r[7] = 0;
    r[8] = vz[0];
    r[9] = vz[1];
    r[10] = vz[2];
    r[11] = 0;
    r[12] = 0;
    r[13] = 0;
    r[14] = 0;
    r[15] = 1;

    return r;
});


function _WebGL_log(/* msg */) {
  // console.log(msg);
}

var _WebGL_guid = 0;

function _WebGL_listEach(fn, list) {
  for (; list.b; list = list.b) {
    fn(list.a);
  }
}

function _WebGL_listLength(list) {
  var length = 0;
  for (; list.b; list = list.b) {
    length++;
  }
  return length;
}

var _WebGL_rAF = typeof requestAnimationFrame !== 'undefined' ?
  requestAnimationFrame :
  function (cb) { setTimeout(cb, 1000 / 60); };

// eslint-disable-next-line no-unused-vars
var _WebGL_entity = F5(function (settings, vert, frag, mesh, uniforms) {
  return {
    $: 0,
    a: settings,
    b: vert,
    c: frag,
    d: mesh,
    e: uniforms
  };
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableBlend = F2(function (gl, setting) {
  gl.enable(gl.BLEND);
  // a   b   c   d   e   f   g h i j
  // eq1 f11 f12 eq2 f21 f22 r g b a
  gl.blendEquationSeparate(setting.a, setting.d);
  gl.blendFuncSeparate(setting.b, setting.c, setting.e, setting.f);
  gl.blendColor(setting.g, setting.h, setting.i, setting.j);
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableDepthTest = F2(function (gl, setting) {
  gl.enable(gl.DEPTH_TEST);
  // a    b    c    d
  // func mask near far
  gl.depthFunc(setting.a);
  gl.depthMask(setting.b);
  gl.depthRange(setting.c, setting.d);
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableStencilTest = F2(function (gl, setting) {
  gl.enable(gl.STENCIL_TEST);
  // a   b    c         d     e     f      g      h     i     j      k
  // ref mask writeMask test1 fail1 zfail1 zpass1 test2 fail2 zfail2 zpass2
  gl.stencilFuncSeparate(gl.FRONT, setting.d, setting.a, setting.b);
  gl.stencilOpSeparate(gl.FRONT, setting.e, setting.f, setting.g);
  gl.stencilMaskSeparate(gl.FRONT, setting.c);
  gl.stencilFuncSeparate(gl.BACK, setting.h, setting.a, setting.b);
  gl.stencilOpSeparate(gl.BACK, setting.i, setting.j, setting.k);
  gl.stencilMaskSeparate(gl.BACK, setting.c);
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableScissor = F2(function (gl, setting) {
  gl.enable(gl.SCISSOR_TEST);
  gl.scissor(setting.a, setting.b, setting.c, setting.d);
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableColorMask = F2(function (gl, setting) {
  gl.colorMask(setting.a, setting.b, setting.c, setting.d);
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableCullFace = F2(function (gl, setting) {
  gl.enable(gl.CULL_FACE);
  gl.cullFace(setting.a);
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enablePolygonOffset = F2(function (gl, setting) {
  gl.enable(gl.POLYGON_OFFSET_FILL);
  gl.polygonOffset(setting.a, setting.b);
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableSampleCoverage = F2(function (gl, setting) {
  gl.enable(gl.SAMPLE_COVERAGE);
  gl.sampleCoverage(setting.a, setting.b);
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableSampleAlphaToCoverage = F2(function (gl, setting) {
  gl.enable(gl.SAMPLE_ALPHA_TO_COVERAGE);
});

// eslint-disable-next-line no-unused-vars
var _WebGL_disableBlend = function (cache) {
  cache.gl.disable(cache.gl.BLEND);
};

// eslint-disable-next-line no-unused-vars
var _WebGL_disableDepthTest = function (cache) {
  cache.gl.disable(cache.gl.DEPTH_TEST);
  cache.gl.depthMask(true);
};

// eslint-disable-next-line no-unused-vars
var _WebGL_disableStencilTest = function (cache) {
  cache.gl.disable(cache.gl.STENCIL_TEST);
  cache.gl.stencilMask(cache.STENCIL_WRITEMASK);
};

// eslint-disable-next-line no-unused-vars
var _WebGL_disableScissor = function (cache) {
  cache.gl.disable(cache.gl.SCISSOR_TEST);
};

// eslint-disable-next-line no-unused-vars
var _WebGL_disableColorMask = function (cache) {
  cache.gl.colorMask(true, true, true, true);
};

// eslint-disable-next-line no-unused-vars
var _WebGL_disableCullFace = function (cache) {
  cache.gl.disable(cache.gl.CULL_FACE);
};

// eslint-disable-next-line no-unused-vars
var _WebGL_disablePolygonOffset = function (cache) {
  cache.gl.disable(cache.gl.POLYGON_OFFSET_FILL);
};

// eslint-disable-next-line no-unused-vars
var _WebGL_disableSampleCoverage = function (cache) {
  cache.gl.disable(cache.gl.SAMPLE_COVERAGE);
};

// eslint-disable-next-line no-unused-vars
var _WebGL_disableSampleAlphaToCoverage = function (cache) {
  cache.gl.disable(cache.gl.SAMPLE_ALPHA_TO_COVERAGE);
};

function _WebGL_doCompile(gl, src, type) {

  var shader = gl.createShader(type);
  _WebGL_log('Created shader');

  gl.shaderSource(shader, src);
  gl.compileShader(shader);
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    throw gl.getShaderInfoLog(shader);
  }

  return shader;

}

function _WebGL_doLink(gl, vshader, fshader) {

  var program = gl.createProgram();
  _WebGL_log('Created program');

  gl.attachShader(program, vshader);
  gl.attachShader(program, fshader);
  gl.linkProgram(program);
  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    throw gl.getProgramInfoLog(program);
  }

  return program;

}

function _WebGL_getAttributeInfo(gl, type) {
  switch (type) {
    case gl.FLOAT:
      return { size: 1, arraySize: 1, type: Float32Array, baseType: gl.FLOAT };
    case gl.FLOAT_VEC2:
      return { size: 2, arraySize: 1, type: Float32Array, baseType: gl.FLOAT };
    case gl.FLOAT_VEC3:
      return { size: 3, arraySize: 1, type: Float32Array, baseType: gl.FLOAT };
    case gl.FLOAT_VEC4:
      return { size: 4, arraySize: 1, type: Float32Array, baseType: gl.FLOAT };
    case gl.FLOAT_MAT4:
      return { size: 4, arraySize: 4, type: Float32Array, baseType: gl.FLOAT };
    case gl.INT:
      return { size: 1, arraySize: 1, type: Int32Array, baseType: gl.INT };
  }
}

/**
 *  Form the buffer for a given attribute.
 *
 *  @param {WebGLRenderingContext} gl context
 *  @param {WebGLActiveInfo} attribute the attribute to bind to.
 *         We use its name to grab the record by name and also to know
 *         how many elements we need to grab.
 *  @param {Mesh} mesh The mesh coming in from Elm.
 *  @param {Object} attributes The mapping between the attribute names and Elm fields
 *  @return {WebGLBuffer}
 */
function _WebGL_doBindAttribute(gl, attribute, mesh, attributes) {
  // The length of the number of vertices that
  // complete one 'thing' based on the drawing mode.
  // ie, 2 for Lines, 3 for Triangles, etc.
  var elemSize = mesh.a.elemSize;

  var idxKeys = [];
  for (var i = 0; i < elemSize; i++) {
    idxKeys.push(String.fromCharCode(97 + i));
  }

  function dataFill(data, cnt, fillOffset, elem, key) {
    var i;
    if (elemSize === 1) {
      for (i = 0; i < cnt; i++) {
        data[fillOffset++] = cnt === 1 ? elem[key] : elem[key][i];
      }
    } else {
      idxKeys.forEach(function (idx) {
        for (i = 0; i < cnt; i++) {
          data[fillOffset++] = cnt === 1 ? elem[idx][key] : elem[idx][key][i];
        }
      });
    }
  }

  var attributeInfo = _WebGL_getAttributeInfo(gl, attribute.type);

  if (attributeInfo === undefined) {
    throw new Error('No info available for: ' + attribute.type);
  }

  var dataIdx = 0;
  var dataOffset = attributeInfo.size * attributeInfo.arraySize * elemSize;
  var array = new attributeInfo.type(_WebGL_listLength(mesh.b) * dataOffset);

  _WebGL_listEach(function (elem) {
    dataFill(array, attributeInfo.size * attributeInfo.arraySize, dataIdx, elem, attributes[attribute.name] || attribute.name);
    dataIdx += dataOffset;
  }, mesh.b);

  var buffer = gl.createBuffer();
  _WebGL_log('Created attribute buffer ' + attribute.name);

  gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  gl.bufferData(gl.ARRAY_BUFFER, array, gl.STATIC_DRAW);
  return buffer;
}

/**
 *  This sets up the binding caching buffers.
 *
 *  We don't actually bind any buffers now except for the indices buffer.
 *  The problem with filling the buffers here is that it is possible to
 *  have a buffer shared between two webgl shaders;
 *  which could have different active attributes. If we bind it here against
 *  a particular program, we might not bind them all. That final bind is now
 *  done right before drawing.
 *
 *  @param {WebGLRenderingContext} gl context
 *  @param {Mesh} mesh a mesh object from Elm
 *  @return {Object} buffer - an object with the following properties
 *  @return {Number} buffer.numIndices
 *  @return {WebGLBuffer|null} buffer.indexBuffer - optional index buffer
 *  @return {Object} buffer.buffers - will be used to buffer attributes
 */
function _WebGL_doBindSetup(gl, mesh) {
  if (mesh.a.indexSize > 0) {
    _WebGL_log('Created index buffer');
    var indexBuffer = gl.createBuffer();
    var indices = _WebGL_makeIndexedBuffer(mesh.c, mesh.a.indexSize);
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, indices, gl.STATIC_DRAW);
    return {
      numIndices: indices.length,
      indexBuffer: indexBuffer,
      buffers: {}
    };
  } else {
    return {
      numIndices: mesh.a.elemSize * _WebGL_listLength(mesh.b),
      indexBuffer: null,
      buffers: {}
    };
  }
}

/**
 *  Create an indices array and fill it from indices
 *  based on the size of the index
 *
 *  @param {List} indicesList the list of indices
 *  @param {Number} indexSize the size of the index
 *  @return {Uint16Array} indices
 */
function _WebGL_makeIndexedBuffer(indicesList, indexSize) {
  var indices = new Uint16Array(_WebGL_listLength(indicesList) * indexSize);
  var fillOffset = 0;
  var i;
  _WebGL_listEach(function (elem) {
    if (indexSize === 1) {
      indices[fillOffset++] = elem;
    } else {
      for (i = 0; i < indexSize; i++) {
        indices[fillOffset++] = elem[String.fromCharCode(97 + i)];
      }
    }
  }, indicesList);
  return indices;
}

function _WebGL_getProgID(vertID, fragID) {
  return vertID + '#' + fragID;
}

var _WebGL_drawGL = F2(function (model, domNode) {

  var gl = model.f.gl;

  if (!gl) {
    return domNode;
  }

  gl.viewport(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight);
  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT | gl.STENCIL_BUFFER_BIT);
  _WebGL_log('Drawing');

  function drawEntity(entity) {
    if (!entity.d.b.b) {
      return; // Empty list
    }

    var progid;
    var program;
    if (entity.b.id && entity.c.id) {
      progid = _WebGL_getProgID(entity.b.id, entity.c.id);
      program = model.f.programs[progid];
    }

    if (!program) {

      var vshader;
      if (entity.b.id) {
        vshader = model.f.shaders[entity.b.id];
      } else {
        entity.b.id = _WebGL_guid++;
      }

      if (!vshader) {
        vshader = _WebGL_doCompile(gl, entity.b.src, gl.VERTEX_SHADER);
        model.f.shaders[entity.b.id] = vshader;
      }

      var fshader;
      if (entity.c.id) {
        fshader = model.f.shaders[entity.c.id];
      } else {
        entity.c.id = _WebGL_guid++;
      }

      if (!fshader) {
        fshader = _WebGL_doCompile(gl, entity.c.src, gl.FRAGMENT_SHADER);
        model.f.shaders[entity.c.id] = fshader;
      }

      var glProgram = _WebGL_doLink(gl, vshader, fshader);

      program = {
        glProgram: glProgram,
        attributes: Object.assign({}, entity.b.attributes, entity.c.attributes),
        uniformSetters: _WebGL_createUniformSetters(
          gl,
          model,
          glProgram,
          Object.assign({}, entity.b.uniforms, entity.c.uniforms)
        )
      };

      progid = _WebGL_getProgID(entity.b.id, entity.c.id);
      model.f.programs[progid] = program;

    }

    gl.useProgram(program.glProgram);

    _WebGL_setUniforms(program.uniformSetters, entity.e);

    var buffer = model.f.buffers.get(entity.d);

    if (!buffer) {
      buffer = _WebGL_doBindSetup(gl, entity.d);
      model.f.buffers.set(entity.d, buffer);
    }

    var numAttributes = gl.getProgramParameter(program.glProgram, gl.ACTIVE_ATTRIBUTES);

    for (var i = 0; i < numAttributes; i++) {
      var attribute = gl.getActiveAttrib(program.glProgram, i);

      var attribLocation = gl.getAttribLocation(program.glProgram, attribute.name);
      gl.enableVertexAttribArray(attribLocation);

      if (buffer.buffers[attribute.name] === undefined) {
        buffer.buffers[attribute.name] = _WebGL_doBindAttribute(gl, attribute, entity.d, program.attributes);
      }
      var attributeBuffer = buffer.buffers[attribute.name];
      var attributeInfo = _WebGL_getAttributeInfo(gl, attribute.type);

      gl.bindBuffer(gl.ARRAY_BUFFER, attributeBuffer);

      if (attributeInfo.arraySize === 1) {
        gl.vertexAttribPointer(attribLocation, attributeInfo.size, attributeInfo.baseType, false, 0, 0);
      } else {
        // Point to four vec4 in case of mat4
        var offset = attributeInfo.size * 4; // float32 takes 4 bytes
        var stride = offset * attributeInfo.arraySize;
        for (var m = 0; m < attributeInfo.arraySize; m++) {
          gl.enableVertexAttribArray(attribLocation + m);
          gl.vertexAttribPointer(attribLocation + m, attributeInfo.size, attributeInfo.baseType, false, stride, offset * m);
        }
      }
    }
    _WebGL_listEach(elm_explorations$webgl$WebGL$Internal$enableSetting(gl), entity.a);

    if (buffer.indexBuffer) {
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, buffer.indexBuffer);
      gl.drawElements(entity.d.a.mode, buffer.numIndices, gl.UNSIGNED_SHORT, 0);
    } else {
      gl.drawArrays(entity.d.a.mode, 0, buffer.numIndices);
    }

    _WebGL_listEach(elm_explorations$webgl$WebGL$Internal$disableSetting(model.f), entity.a);

  }

  _WebGL_listEach(drawEntity, model.g);
  return domNode;
});

function _WebGL_createUniformSetters(gl, model, program, uniformsMap) {
  var textureCounter = 0;
  function createUniformSetter(program, uniform) {
    var uniformLocation = gl.getUniformLocation(program, uniform.name);
    switch (uniform.type) {
      case gl.INT:
        return function (value) {
          gl.uniform1i(uniformLocation, value);
        };
      case gl.FLOAT:
        return function (value) {
          gl.uniform1f(uniformLocation, value);
        };
      case gl.FLOAT_VEC2:
        return function (value) {
          gl.uniform2fv(uniformLocation, new Float32Array(value));
        };
      case gl.FLOAT_VEC3:
        return function (value) {
          gl.uniform3fv(uniformLocation, new Float32Array(value));
        };
      case gl.FLOAT_VEC4:
        return function (value) {
          gl.uniform4fv(uniformLocation, new Float32Array(value));
        };
      case gl.FLOAT_MAT4:
        return function (value) {
          gl.uniformMatrix4fv(uniformLocation, false, new Float32Array(value));
        };
      case gl.SAMPLER_2D:
        var currentTexture = textureCounter++;
        return function (texture) {
          gl.activeTexture(gl.TEXTURE0 + currentTexture);
          var tex = model.f.textures.get(texture);
          if (!tex) {
            _WebGL_log('Created texture');
            tex = texture.createTexture(gl);
            model.f.textures.set(texture, tex);
          }
          gl.bindTexture(gl.TEXTURE_2D, tex);
          gl.uniform1i(uniformLocation, currentTexture);
        };
      case gl.BOOL:
        return function (value) {
          gl.uniform1i(uniformLocation, value);
        };
      default:
        _WebGL_log('Unsupported uniform type: ' + uniform.type);
        return function () { };
    }
  }

  var uniformSetters = {};
  var numUniforms = gl.getProgramParameter(program, gl.ACTIVE_UNIFORMS);
  for (var i = 0; i < numUniforms; i++) {
    var uniform = gl.getActiveUniform(program, i);
    uniformSetters[uniformsMap[uniform.name] || uniform.name] = createUniformSetter(program, uniform);
  }

  return uniformSetters;
}

function _WebGL_setUniforms(setters, values) {
  Object.keys(values).forEach(function (name) {
    var setter = setters[name];
    if (setter) {
      setter(values[name]);
    }
  });
}

// VIRTUAL-DOM WIDGET

// eslint-disable-next-line no-unused-vars
var _WebGL_toHtml = F3(function (options, factList, entities) {
  return _VirtualDom_custom(
    factList,
    {
      g: entities,
      f: {},
      h: options
    },
    _WebGL_render,
    _WebGL_diff
  );
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableAlpha = F2(function (options, option) {
  options.contextAttributes.alpha = true;
  options.contextAttributes.premultipliedAlpha = option.a;
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableDepth = F2(function (options, option) {
  options.contextAttributes.depth = true;
  options.sceneSettings.push(function (gl) {
    gl.clearDepth(option.a);
  });
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableStencil = F2(function (options, option) {
  options.contextAttributes.stencil = true;
  options.sceneSettings.push(function (gl) {
    gl.clearStencil(option.a);
  });
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableAntialias = F2(function (options, option) {
  options.contextAttributes.antialias = true;
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableClearColor = F2(function (options, option) {
  options.sceneSettings.push(function (gl) {
    gl.clearColor(option.a, option.b, option.c, option.d);
  });
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enablePreserveDrawingBuffer = F2(function (options, option) {
  options.contextAttributes.preserveDrawingBuffer = true;
});

/**
 *  Creates canvas and schedules initial _WebGL_drawGL
 *  @param {Object} model
 *  @param {Object} model.f that may contain the following properties:
           gl, shaders, programs, buffers, textures
 *  @param {List<Option>} model.h list of options coming from Elm
 *  @param {List<Entity>} model.g list of entities coming from Elm
 *  @return {HTMLElement} <canvas> if WebGL is supported, otherwise a <div>
 */
function _WebGL_render(model) {
  var options = {
    contextAttributes: {
      alpha: false,
      depth: false,
      stencil: false,
      antialias: false,
      premultipliedAlpha: false,
      preserveDrawingBuffer: false
    },
    sceneSettings: []
  };

  _WebGL_listEach(function (option) {
    return A2(elm_explorations$webgl$WebGL$Internal$enableOption, options, option);
  }, model.h);

  _WebGL_log('Render canvas');
  var canvas = _VirtualDom_doc.createElement('canvas');
  var gl = canvas.getContext && (
    canvas.getContext('webgl', options.contextAttributes) ||
    canvas.getContext('experimental-webgl', options.contextAttributes)
  );

  if (gl && typeof WeakMap !== 'undefined') {
    options.sceneSettings.forEach(function (sceneSetting) {
      sceneSetting(gl);
    });

    model.f.gl = gl;
    model.f.shaders = [];
    model.f.programs = {};
    model.f.buffers = new WeakMap();
    model.f.textures = new WeakMap();
    // Memorize the initial stencil write mask, because
    // browsers may have different number of stencil bits
    model.f.STENCIL_WRITEMASK = gl.getParameter(gl.STENCIL_WRITEMASK);

    // Render for the first time.
    // This has to be done in animation frame,
    // because the canvas is not in the DOM yet
    _WebGL_rAF(function () {
      return A2(_WebGL_drawGL, model, canvas);
    });

  } else {
    canvas = _VirtualDom_doc.createElement('div');
    canvas.innerHTML = '<a href="https://get.webgl.org/">Enable WebGL</a> to see this content!';
  }

  return canvas;
}

function _WebGL_diff(oldModel, newModel) {
  newModel.f = oldModel.f;
  return _WebGL_drawGL(newModel);
}
var author$project$Brush$Circle = {$: 'Circle'};
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
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$toFloat = _Basics_toFloat;
var author$project$Brush$closestCorner = F2(
	function (_n0, _n1) {
		var targetX = _n0.a;
		var targetY = _n0.b;
		var cellX = _n1.a;
		var cellY = _n1.b;
		var cellCenterY = cellY + 0.5;
		var y = (_Utils_cmp(targetY, cellCenterY) < 1) ? cellY : (cellY + 1);
		var cellCenterX = cellX + 0.5;
		var x = (_Utils_cmp(targetX, cellCenterX) < 1) ? cellX : (cellX + 1);
		return _Utils_Tuple2(x, y);
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$sub = _Basics_sub;
var author$project$Brush$sqrDist = F2(
	function (_n0, _n1) {
		var x = _n0.a;
		var y = _n0.b;
		var x2 = _n1.a;
		var y2 = _n1.b;
		return ((x2 - x) * (x2 - x)) + ((y2 - y) * (y2 - y));
	});
var author$project$Brush$isInCircle = F3(
	function (center, radius, point) {
		return _Utils_cmp(
			A2(author$project$Brush$sqrDist, center, point),
			radius * radius) < 1;
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$fdiv = _Basics_fdiv;
var author$project$Brush$isInCircleMask = F3(
	function (maskWidth, x, y) {
		var radius = maskWidth / 2.0;
		var centerPoint = _Utils_Tuple2(radius, radius);
		var closestCornerToCenter = A2(
			author$project$Brush$closestCorner,
			centerPoint,
			_Utils_Tuple2(x, y));
		return A3(author$project$Brush$isInCircle, centerPoint, radius, closestCornerToCenter);
	});
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
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
var author$project$Grid$square = F2(
	function (size, filler) {
		return A3(author$project$Grid$rectangle, size, size, filler);
	});
var author$project$Brush$circleMask = function (brushWidth) {
	return A2(
		author$project$Grid$square,
		brushWidth,
		author$project$Brush$isInCircleMask(brushWidth));
};
var elm$core$Basics$True = {$: 'True'};
var author$project$Brush$squareMask = function (brushWidth) {
	return A2(
		author$project$Grid$square,
		brushWidth,
		F2(
			function (_n0, _n1) {
				return true;
			}));
};
var author$project$Brush$makeBrush = F3(
	function (shape, width_, paintValue) {
		var mask = function () {
			if (shape.$ === 'Circle') {
				return author$project$Brush$circleMask(width_);
			} else {
				return author$project$Brush$squareMask(width_);
			}
		}();
		return {
			centerOffset: {x: (width_ / 2) | 0, y: (width_ / 2) | 0},
			mask: mask,
			paintValue: paintValue
		};
	});
var author$project$Layer$getMin = function (_n0) {
	var inner = _n0.a;
	return inner.min;
};
var author$project$LayerSelection$LayerSelection = function (a) {
	return {$: 'LayerSelection', a: a};
};
var elm$core$Basics$identity = function (x) {
	return x;
};
var mgold$elm_nonempty_list$List$Nonempty$Nonempty = F2(
	function (a, b) {
		return {$: 'Nonempty', a: a, b: b};
	});
var mgold$elm_nonempty_list$List$Nonempty$fromElement = function (x) {
	return A2(mgold$elm_nonempty_list$List$Nonempty$Nonempty, x, _List_Nil);
};
var author$project$LayerSelection$singleton = function (layer) {
	return author$project$LayerSelection$LayerSelection(
		{
			list: mgold$elm_nonempty_list$List$Nonempty$fromElement(layer),
			selectedIndex: 0
		});
};
var author$project$MapEditor$defaultLayerName = 'New Layer';
var author$project$MapEditor$gridEditorPaneHeight = function (windowHeight) {
	return windowHeight;
};
var author$project$MapEditor$toolbarWidth = 250;
var author$project$MapEditor$gridEditorPaneWidth = function (windowWidth) {
	return A2(elm$core$Basics$max, 0, windowWidth - author$project$MapEditor$toolbarWidth);
};
var author$project$GridEditor$State = function (a) {
	return {$: 'State', a: a};
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
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
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
var elm$core$Maybe$Nothing = {$: 'Nothing'};
var author$project$GridEditor$init = F7(
	function (grid, cellMin, cellMax, gradient, brush, canvasWidth, canvasHeight) {
		var g = A3(author$project$GridEditor$boundGrid, cellMin, cellMax, grid);
		return author$project$GridEditor$State(
			{
				brush: brush,
				cameraCenter: _Utils_Tuple2(0, 0),
				cellMax: cellMax,
				cellMin: cellMin,
				currentTime: 0,
				gradient: gradient,
				grid: g,
				height: canvasHeight,
				mouseAndActiveAction: elm$core$Maybe$Nothing,
				width: canvasWidth,
				zoomStep: 0,
				zoomStepDestination: 0
			});
	});
var author$project$Layer$getColorGradient = function (_n0) {
	var inner = _n0.a;
	return inner.colorGradient;
};
var author$project$Layer$getGrid = function (_n0) {
	var inner = _n0.a;
	return inner.grid;
};
var author$project$Layer$getMax = function (_n0) {
	var inner = _n0.a;
	return inner.max;
};
var author$project$MapEditor$makeGridEditor = F4(
	function (layer, brush, paneWidth, paneHeight) {
		return A7(
			author$project$GridEditor$init,
			author$project$Layer$getGrid(layer),
			author$project$Layer$getMin(layer),
			author$project$Layer$getMax(layer),
			author$project$Layer$getColorGradient(layer),
			brush,
			paneWidth,
			paneHeight);
	});
var author$project$DiscreteGradient$DiscreteGradient = function (a) {
	return {$: 'DiscreteGradient', a: a};
};
var author$project$DiscreteGradient$init = function (stop) {
	return author$project$DiscreteGradient$DiscreteGradient(
		_List_fromArray(
			[stop]));
};
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
var author$project$Layer$defaultColor = A3(avh4$elm_color$Color$rgb255, 226, 192, 141);
var author$project$Layer$init = F5(
	function (name, width, height, minVal, maxVal) {
		var positiveWidth = A2(elm$core$Basics$max, width, 1);
		var positiveHeight = A2(elm$core$Basics$max, height, 1);
		return author$project$Layer$Layer(
			{
				colorGradient: author$project$DiscreteGradient$init(
					{color: author$project$Layer$defaultColor, value: minVal}),
				grid: A3(author$project$Grid$repeat, positiveWidth, positiveHeight, minVal),
				max: maxVal,
				min: minVal,
				name: name
			});
	});
var author$project$MapEditor$makeLayerWithDefaultMinMax = F3(
	function (name, width, height) {
		var defaultCellMin = 0;
		var defaultCellMax = 9;
		return A5(author$project$Layer$init, name, width, height, defaultCellMin, defaultCellMax);
	});
var author$project$MapEditor$init = function (_n0) {
	var windowWidth = _n0.a;
	var windowHeight = _n0.b;
	var defaultMapWidth = 25;
	var defaultMapHeight = 25;
	var initLayer = A3(author$project$MapEditor$makeLayerWithDefaultMinMax, author$project$MapEditor$defaultLayerName, defaultMapWidth, defaultMapHeight);
	var initBrush = A3(
		author$project$Brush$makeBrush,
		author$project$Brush$Circle,
		3,
		author$project$Layer$getMin(initLayer));
	var initGridEditor = A4(
		author$project$MapEditor$makeGridEditor,
		initLayer,
		initBrush,
		author$project$MapEditor$gridEditorPaneWidth(windowWidth),
		author$project$MapEditor$gridEditorPaneHeight(windowHeight));
	var initLayerSelection = author$project$LayerSelection$singleton(initLayer);
	return {brush: initBrush, dialog: elm$core$Maybe$Nothing, gridEditor: initGridEditor, layerSelection: initLayerSelection, mapHeight: defaultMapHeight, mapWidth: defaultMapWidth, name: 'map.json', windowHeight: windowHeight, windowWidth: windowWidth};
};
var author$project$MapEditor$ElapsedTime = function (a) {
	return {$: 'ElapsedTime', a: a};
};
var author$project$MapEditor$WindowResize = F2(
	function (a, b) {
		return {$: 'WindowResize', a: a, b: b};
	});
var elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 'Delta', a: a};
};
var elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {oldTime: oldTime, request: request, subs: subs};
	});
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$browser$Browser$AnimationManager$init = elm$core$Task$succeed(
	A3(elm$browser$Browser$AnimationManager$State, _List_Nil, elm$core$Maybe$Nothing, 0));
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
var elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
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
var elm$browser$Browser$AnimationManager$now = _Browser_now(_Utils_Tuple0);
var elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(_Utils_Tuple0);
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$core$Process$kill = _Scheduler_kill;
var elm$core$Process$spawn = _Scheduler_spawn;
var elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _n0) {
		var request = _n0.request;
		var oldTime = _n0.oldTime;
		var _n1 = _Utils_Tuple2(request, subs);
		if (_n1.a.$ === 'Nothing') {
			if (!_n1.b.b) {
				var _n2 = _n1.a;
				return elm$browser$Browser$AnimationManager$init;
			} else {
				var _n4 = _n1.a;
				return A2(
					elm$core$Task$andThen,
					function (pid) {
						return A2(
							elm$core$Task$andThen,
							function (time) {
								return elm$core$Task$succeed(
									A3(
										elm$browser$Browser$AnimationManager$State,
										subs,
										elm$core$Maybe$Just(pid),
										time));
							},
							elm$browser$Browser$AnimationManager$now);
					},
					elm$core$Process$spawn(
						A2(
							elm$core$Task$andThen,
							elm$core$Platform$sendToSelf(router),
							elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_n1.b.b) {
				var pid = _n1.a.a;
				return A2(
					elm$core$Task$andThen,
					function (_n3) {
						return elm$browser$Browser$AnimationManager$init;
					},
					elm$core$Process$kill(pid));
			} else {
				return elm$core$Task$succeed(
					A3(elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var elm$time$Time$millisToPosix = elm$time$Time$Posix;
var elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _n0) {
		var subs = _n0.subs;
		var oldTime = _n0.oldTime;
		var send = function (sub) {
			if (sub.$ === 'Time') {
				var tagger = sub.a;
				return A2(
					elm$core$Platform$sendToApp,
					router,
					tagger(
						elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			elm$core$Task$andThen,
			function (pid) {
				return A2(
					elm$core$Task$andThen,
					function (_n1) {
						return elm$core$Task$succeed(
							A3(
								elm$browser$Browser$AnimationManager$State,
								subs,
								elm$core$Maybe$Just(pid),
								newTime));
					},
					elm$core$Task$sequence(
						A2(elm$core$List$map, send, subs)));
			},
			elm$core$Process$spawn(
				A2(
					elm$core$Task$andThen,
					elm$core$Platform$sendToSelf(router),
					elm$browser$Browser$AnimationManager$rAF)));
	});
var elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 'Time', a: a};
};
var elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (sub.$ === 'Time') {
			var tagger = sub.a;
			return elm$browser$Browser$AnimationManager$Time(
				A2(elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return elm$browser$Browser$AnimationManager$Delta(
				A2(elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager(elm$browser$Browser$AnimationManager$init, elm$browser$Browser$AnimationManager$onEffects, elm$browser$Browser$AnimationManager$onSelfMsg, 0, elm$browser$Browser$AnimationManager$subMap);
var elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var elm$browser$Browser$AnimationManager$onAnimationFrameDelta = function (tagger) {
	return elm$browser$Browser$AnimationManager$subscription(
		elm$browser$Browser$AnimationManager$Delta(tagger));
};
var elm$browser$Browser$Events$onAnimationFrameDelta = elm$browser$Browser$AnimationManager$onAnimationFrameDelta;
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
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$int = _Json_decodeInt;
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
var author$project$MapEditor$subscriptions = function (_n0) {
	return elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				elm$browser$Browser$Events$onAnimationFrameDelta(author$project$MapEditor$ElapsedTime),
				elm$browser$Browser$Events$onResize(author$project$MapEditor$WindowResize)
			]));
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
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var author$project$Grid$width = function (grid) {
	return A2(
		elm$core$Maybe$withDefault,
		0,
		A2(
			elm$core$Maybe$map,
			elm$core$Array$length,
			A2(elm$core$Array$get, 0, grid)));
};
var author$project$Brush$width = function (brush) {
	return author$project$Grid$width(brush.mask);
};
var author$project$DiscreteGradientEditor$State = function (a) {
	return {$: 'State', a: a};
};
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
var simonh1000$elm_colorpicker$ColorPicker$State = function (a) {
	return {$: 'State', a: a};
};
var simonh1000$elm_colorpicker$ColorPicker$Unpressed = {$: 'Unpressed'};
var simonh1000$elm_colorpicker$ColorPicker$blankModel = {hue: elm$core$Maybe$Nothing, mouseTarget: simonh1000$elm_colorpicker$ColorPicker$Unpressed};
var simonh1000$elm_colorpicker$ColorPicker$empty = simonh1000$elm_colorpicker$ColorPicker$State(simonh1000$elm_colorpicker$ColorPicker$blankModel);
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
var author$project$DiscreteGradientEditor$boundInt = F2(
	function (lowerBound, upperBound) {
		return A2(
			elm$core$Basics$composeL,
			elm$core$Basics$min(upperBound),
			elm$core$Basics$max(lowerBound));
	});
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
var author$project$GridEditor$PaintAction = {$: 'PaintAction'};
var author$project$GridEditor$PanAction = function (a) {
	return {$: 'PanAction', a: a};
};
var author$project$Grid$toColumn = elm$core$Tuple$first;
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var author$project$Grid$toRow = elm$core$Tuple$second;
var elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return elm$core$Maybe$Nothing;
		}
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
var author$project$Grid$coordinate = F2(
	function (x, y) {
		return _Utils_Tuple2(x, y);
	});
var elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var elm$core$Elm$JsArray$indexedMap = _JsArray_indexedMap;
var elm$core$Array$indexedMap = F2(
	function (func, _n0) {
		var len = _n0.a;
		var tree = _n0.c;
		var tail = _n0.d;
		var initialBuilder = {
			nodeList: _List_Nil,
			nodeListSize: 0,
			tail: A3(
				elm$core$Elm$JsArray$indexedMap,
				func,
				elm$core$Array$tailIndex(len),
				tail)
		};
		var helper = F2(
			function (node, builder) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldl, helper, builder, subTree);
				} else {
					var leaf = node.a;
					var offset = builder.nodeListSize * elm$core$Array$branchFactor;
					var mappedLeaf = elm$core$Array$Leaf(
						A3(elm$core$Elm$JsArray$indexedMap, func, offset, leaf));
					return {
						nodeList: A2(elm$core$List$cons, mappedLeaf, builder.nodeList),
						nodeListSize: builder.nodeListSize + 1,
						tail: builder.tail
					};
				}
			});
		return A2(
			elm$core$Array$builderToArray,
			true,
			A3(elm$core$Elm$JsArray$foldl, helper, initialBuilder, tree));
	});
var author$project$Grid$mapWithCoordinate = F2(
	function (f, grid) {
		return A2(
			elm$core$Array$indexedMap,
			function (y) {
				return elm$core$Array$indexedMap(
					function (x) {
						return f(
							A2(author$project$Grid$coordinate, x, y));
					});
			},
			grid);
	});
var author$project$GridEditor$applyBrushMask = F2(
	function (brush, _n0) {
		var brushCenterX = _n0.a;
		var brushCenterY = _n0.b;
		return author$project$Grid$mapWithCoordinate(
			F2(
				function (_n1, cell) {
					var x = _n1.a;
					var y = _n1.b;
					var yRelativeToMask = (y - brushCenterY) + brush.centerOffset.y;
					var xRelativeToMask = (x - brushCenterX) + brush.centerOffset.x;
					var isInMask = function () {
						var _n2 = A2(
							author$project$Grid$get,
							_Utils_Tuple2(xRelativeToMask, yRelativeToMask),
							brush.mask);
						if ((_n2.$ === 'Just') && _n2.a) {
							return true;
						} else {
							return false;
						}
					}();
					return _Utils_Tuple2(cell, isInMask);
				}));
	});
var author$project$GridEditor$zoomFactorPerStep = 1.333;
var author$project$GridEditor$cameraBounds = F4(
	function (_n0, canvasWidth, canvasHeight, zoomStep) {
		var cameraCenterX = _n0.a;
		var cameraCenterY = _n0.b;
		var zoomFactor = A2(elm$core$Basics$pow, author$project$GridEditor$zoomFactorPerStep, zoomStep);
		var pixelsPerSceneUnit = 250.0;
		var cameraWidth = (canvasWidth / pixelsPerSceneUnit) * zoomFactor;
		var cameraHeight = (canvasHeight / pixelsPerSceneUnit) * zoomFactor;
		return {bottom: (((-1) * cameraHeight) / 2.0) + cameraCenterY, left: (((-1) * cameraWidth) / 2.0) + cameraCenterX, right: (cameraWidth / 2.0) + cameraCenterX, top: (cameraHeight / 2.0) + cameraCenterY};
	});
var author$project$GridEditor$maskedMap = function (f) {
	return author$project$Grid$map(
		function (_n0) {
			var cell = _n0.a;
			var isMasked = _n0.b;
			return isMasked ? _Utils_Tuple2(
				f(cell),
				isMasked) : _Utils_Tuple2(cell, isMasked);
		});
};
var author$project$GridEditor$mouseToScenePos = F4(
	function (bounds, canvasWidth, canvasHeight, _n0) {
		var mouseX = _n0.a;
		var mouseY = _n0.b;
		var cameraWidth = bounds.right - bounds.left;
		var x = bounds.left + ((mouseX / canvasWidth) * cameraWidth);
		var cameraHeight = bounds.top - bounds.bottom;
		var y = bounds.top - ((mouseY / canvasHeight) * cameraHeight);
		return _Utils_Tuple2(x, y);
	});
var author$project$GridEditor$removeMask = author$project$Grid$map(
	function (_n0) {
		var cell = _n0.a;
		return cell;
	});
var author$project$Grid$height = function (grid) {
	return elm$core$Array$length(grid);
};
var author$project$GridEditor$cellWidth = 0.1;
var author$project$GridEditor$gridBottomInScene = function (rowCount) {
	return (((-1) * author$project$GridEditor$cellWidth) * rowCount) * 0.5;
};
var author$project$GridEditor$gridLeftInScene = function (columnCount) {
	return (((-1) * author$project$GridEditor$cellWidth) * columnCount) * 0.5;
};
var author$project$GridEditor$scenePosToCellIndex = F2(
	function (grid, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		var rowCount = author$project$Grid$height(grid);
		var gridBottomY = author$project$GridEditor$gridBottomInScene(rowCount);
		var j = elm$core$Basics$floor((y - gridBottomY) / author$project$GridEditor$cellWidth);
		var columnCount = author$project$Grid$width(grid);
		var gridLeftX = author$project$GridEditor$gridLeftInScene(columnCount);
		var i = elm$core$Basics$floor((x - gridLeftX) / author$project$GridEditor$cellWidth);
		return ((i >= 0) && ((_Utils_cmp(i, columnCount) < 0) && ((j >= 0) && (_Utils_cmp(j, rowCount) < 0)))) ? elm$core$Maybe$Just(
			_Utils_Tuple2(i, j)) : elm$core$Maybe$Nothing;
	});
var author$project$GridEditor$applyPaintAction = F9(
	function (cameraCenter, canvasWidth, canvasHeight, zoomStep, cellMin, cellMax, grid, brush, mousePos) {
		var bounds = A4(author$project$GridEditor$cameraBounds, cameraCenter, canvasWidth, canvasHeight, zoomStep);
		var brushCenter = A2(
			author$project$GridEditor$scenePosToCellIndex,
			grid,
			A4(author$project$GridEditor$mouseToScenePos, bounds, canvasWidth, canvasHeight, mousePos));
		if (brushCenter.$ === 'Just') {
			var bc = brushCenter.a;
			return A3(
				author$project$GridEditor$boundGrid,
				cellMin,
				cellMax,
				author$project$GridEditor$removeMask(
					A2(
						author$project$GridEditor$maskedMap,
						function (_n1) {
							return brush.paintValue;
						},
						A3(author$project$GridEditor$applyBrushMask, brush, bc, grid))));
		} else {
			return grid;
		}
	});
var author$project$GridEditor$applyPanAction = F6(
	function (cameraCenter, canvasWidth, canvasHeight, zoomStep, panHistory, mousePos) {
		var bounds = A4(author$project$GridEditor$cameraBounds, cameraCenter, canvasWidth, canvasHeight, zoomStep);
		var currentMousePosInScene = A4(author$project$GridEditor$mouseToScenePos, bounds, canvasWidth, canvasHeight, mousePos);
		var lastMousePosInScene = A4(author$project$GridEditor$mouseToScenePos, bounds, canvasWidth, canvasHeight, panHistory.lastMousePosition);
		var deltaX = currentMousePosInScene.a - lastMousePosInScene.a;
		var deltaY = currentMousePosInScene.b - lastMousePosInScene.b;
		var newCameraCenter = _Utils_Tuple2(cameraCenter.a - deltaX, cameraCenter.b - deltaY);
		return _Utils_Tuple2(
			newCameraCenter,
			{lastMousePosition: mousePos});
	});
var author$project$GridEditor$applyActiveAction = function (model) {
	var _n0 = model.mouseAndActiveAction;
	if (_n0.$ === 'Just') {
		var ma = _n0.a;
		var _n1 = ma.activeAction;
		if (_n1.$ === 'Just') {
			if (_n1.a.$ === 'PanAction') {
				var panHistory = _n1.a.a;
				var _n2 = A6(author$project$GridEditor$applyPanAction, model.cameraCenter, model.width, model.height, model.zoomStep, panHistory, ma.mousePosition);
				var newCameraCenter = _n2.a;
				var updatedPanHistory = _n2.b;
				return _Utils_update(
					model,
					{
						cameraCenter: newCameraCenter,
						mouseAndActiveAction: elm$core$Maybe$Just(
							_Utils_update(
								ma,
								{
									activeAction: elm$core$Maybe$Just(
										author$project$GridEditor$PanAction(updatedPanHistory))
								}))
					});
			} else {
				var _n3 = _n1.a;
				var updatedGrid = A9(author$project$GridEditor$applyPaintAction, model.cameraCenter, model.width, model.height, model.zoomStep, model.cellMin, model.cellMax, model.grid, model.brush, ma.mousePosition);
				return _Utils_update(
					model,
					{grid: updatedGrid});
			}
		} else {
			return model;
		}
	} else {
		return model;
	}
};
var author$project$GridEditor$updateActiveAction = F2(
	function (activeAction, model) {
		return _Utils_update(
			model,
			{
				mouseAndActiveAction: A2(
					elm$core$Maybe$map,
					function (ma) {
						return _Utils_update(
							ma,
							{activeAction: activeAction});
					},
					model.mouseAndActiveAction)
			});
	});
var author$project$GridEditor$updateMousePosition = F2(
	function (pos, model) {
		var newMouseAndActiveAction = function () {
			var _n0 = model.mouseAndActiveAction;
			if (_n0.$ === 'Just') {
				var ma = _n0.a;
				return _Utils_update(
					ma,
					{mousePosition: pos});
			} else {
				return {activeAction: elm$core$Maybe$Nothing, mousePosition: pos};
			}
		}();
		return _Utils_update(
			model,
			{
				mouseAndActiveAction: elm$core$Maybe$Just(newMouseAndActiveAction)
			});
	});
var author$project$GridEditor$update_ = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'Resize':
				var w = msg.a;
				var h = msg.b;
				return _Utils_update(
					model,
					{height: h, width: w});
			case 'Scroll':
				var dy = msg.a;
				return _Utils_update(
					model,
					{zoomStepDestination: model.zoomStepDestination + (dy / 50.0)});
			case 'MouseDown':
				var button = msg.a;
				var pos = msg.b;
				var newActiveAction = function () {
					switch (button.$) {
						case 'MiddleButton':
							return elm$core$Maybe$Just(
								author$project$GridEditor$PanAction(
									{lastMousePosition: pos}));
						case 'MainButton':
							return elm$core$Maybe$Just(author$project$GridEditor$PaintAction);
						default:
							return elm$core$Maybe$Nothing;
					}
				}();
				return author$project$GridEditor$applyActiveAction(
					A2(
						author$project$GridEditor$updateActiveAction,
						newActiveAction,
						A2(author$project$GridEditor$updateMousePosition, pos, model)));
			case 'MouseUp':
				return A2(author$project$GridEditor$updateActiveAction, elm$core$Maybe$Nothing, model);
			case 'MouseMove':
				var pos = msg.a;
				return author$project$GridEditor$applyActiveAction(
					A2(author$project$GridEditor$updateMousePosition, pos, model));
			default:
				return A2(author$project$GridEditor$updateActiveAction, elm$core$Maybe$Nothing, model);
		}
	});
var author$project$GridEditor$update = F2(
	function (msg, _n0) {
		var model = _n0.a;
		return _Utils_Tuple2(
			author$project$GridEditor$State(
				A2(author$project$GridEditor$update_, msg, model)),
			model.grid);
	});
var author$project$GridEditor$updateBrush = F2(
	function (brush, _n0) {
		var model = _n0.a;
		return author$project$GridEditor$State(
			_Utils_update(
				model,
				{brush: brush}));
	});
var author$project$GridEditor$updateCanvasSize = F3(
	function (w, h, _n0) {
		var model = _n0.a;
		return author$project$GridEditor$State(
			_Utils_update(
				model,
				{height: h, width: w}));
	});
var author$project$GridEditor$secondsPerZoomStep = 0.1;
var author$project$GridEditor$updateWithElapsedTime = F2(
	function (t, _n0) {
		var model = _n0.a;
		return author$project$GridEditor$State(
			_Utils_update(
				model,
				{
					currentTime: model.currentTime + t,
					zoomStep: function () {
						if (_Utils_eq(model.zoomStep, model.zoomStepDestination)) {
							return model.zoomStep;
						} else {
							var dStep = (t / 1000.0) / author$project$GridEditor$secondsPerZoomStep;
							return (_Utils_cmp(model.zoomStep, model.zoomStepDestination) < 0) ? A2(elm$core$Basics$min, model.zoomStepDestination, model.zoomStep + dStep) : A2(elm$core$Basics$max, model.zoomStepDestination, model.zoomStep - dStep);
						}
					}()
				}));
	});
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
var author$project$Layer$resizeGrid = F3(
	function (width, height, _n0) {
		var inner = _n0.a;
		return author$project$Layer$Layer(
			_Utils_update(
				inner,
				{
					grid: A4(author$project$Layer$resizeGrid_, inner.min, width, height, inner.grid)
				}));
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
var author$project$Layer$applyCeilingToGrid = function (ceil) {
	return author$project$Grid$map(
		elm$core$Basics$min(ceil));
};
var author$project$Layer$applyFloorToGrid = function (flr) {
	return author$project$Grid$map(
		elm$core$Basics$max(flr));
};
var author$project$Layer$setGrid = F2(
	function (grid, _n0) {
		var inner = _n0.a;
		var newGrid = A2(
			author$project$Layer$applyCeilingToGrid,
			inner.max,
			A2(
				author$project$Layer$applyFloorToGrid,
				inner.min,
				A4(
					author$project$Layer$resizeGrid_,
					inner.min,
					author$project$Grid$width(inner.grid),
					author$project$Grid$height(inner.grid),
					grid)));
		return author$project$Layer$Layer(
			_Utils_update(
				inner,
				{grid: grid}));
	});
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
var mgold$elm_nonempty_list$List$Nonempty$append = F2(
	function (_n0, _n1) {
		var x = _n0.a;
		var xs = _n0.b;
		var y = _n1.a;
		var ys = _n1.b;
		return A2(
			mgold$elm_nonempty_list$List$Nonempty$Nonempty,
			x,
			_Utils_ap(
				xs,
				A2(elm$core$List$cons, y, ys)));
	});
var author$project$LayerSelection$add = F2(
	function (layer, _n0) {
		var inner = _n0.a;
		return author$project$LayerSelection$LayerSelection(
			_Utils_update(
				inner,
				{
					list: A2(
						mgold$elm_nonempty_list$List$Nonempty$append,
						inner.list,
						mgold$elm_nonempty_list$List$Nonempty$fromElement(layer))
				}));
	});
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
var mgold$elm_nonempty_list$List$Nonempty$fromList = function (ys) {
	if (ys.b) {
		var x = ys.a;
		var xs = ys.b;
		return elm$core$Maybe$Just(
			A2(mgold$elm_nonempty_list$List$Nonempty$Nonempty, x, xs));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var mgold$elm_nonempty_list$List$Nonempty$toList = function (_n0) {
	var x = _n0.a;
	var xs = _n0.b;
	return A2(elm$core$List$cons, x, xs);
};
var author$project$LayerSelection$deleteSelected = function (_n0) {
	var inner = _n0.a;
	var unsafeList = mgold$elm_nonempty_list$List$Nonempty$toList(inner.list);
	var unsafeListAfterRemove = _Utils_ap(
		A2(elm$core$List$take, inner.selectedIndex, unsafeList),
		A2(elm$core$List$drop, inner.selectedIndex + 1, unsafeList));
	var _n1 = mgold$elm_nonempty_list$List$Nonempty$fromList(unsafeListAfterRemove);
	if (_n1.$ === 'Just') {
		var nonemptyListAfterRemove = _n1.a;
		return author$project$LayerSelection$LayerSelection(
			_Utils_update(
				inner,
				{list: nonemptyListAfterRemove, selectedIndex: inner.selectedIndex - 1}));
	} else {
		return author$project$LayerSelection$LayerSelection(inner);
	}
};
var mgold$elm_nonempty_list$List$Nonempty$length = function (_n0) {
	var x = _n0.a;
	var xs = _n0.b;
	return elm$core$List$length(xs) + 1;
};
var author$project$LayerSelection$length = function (_n0) {
	var inner = _n0.a;
	return mgold$elm_nonempty_list$List$Nonempty$length(inner.list);
};
var mgold$elm_nonempty_list$List$Nonempty$map = F2(
	function (f, _n0) {
		var x = _n0.a;
		var xs = _n0.b;
		return A2(
			mgold$elm_nonempty_list$List$Nonempty$Nonempty,
			f(x),
			A2(elm$core$List$map, f, xs));
	});
var author$project$LayerSelection$map = F2(
	function (f, _n0) {
		var inner = _n0.a;
		return author$project$LayerSelection$LayerSelection(
			_Utils_update(
				inner,
				{
					list: A2(mgold$elm_nonempty_list$List$Nonempty$map, f, inner.list)
				}));
	});
var mgold$elm_nonempty_list$List$Nonempty$indexedMap = F2(
	function (f, _n0) {
		var x = _n0.a;
		var xs = _n0.b;
		var wrapped = F2(
			function (i, d) {
				return A2(f, i + 1, d);
			});
		return A2(
			mgold$elm_nonempty_list$List$Nonempty$Nonempty,
			A2(f, 0, x),
			A2(elm$core$List$indexedMap, wrapped, xs));
	});
var author$project$LayerSelection$mapSelected = F2(
	function (f, _n0) {
		var inner = _n0.a;
		return author$project$LayerSelection$LayerSelection(
			_Utils_update(
				inner,
				{
					list: A2(
						mgold$elm_nonempty_list$List$Nonempty$indexedMap,
						F2(
							function (i, layer) {
								return _Utils_eq(i, inner.selectedIndex) ? f(layer) : layer;
							}),
						inner.list)
				}));
	});
var author$project$LayerSelection$select = F2(
	function (index, _n0) {
		var inner = _n0.a;
		var boundedIndex = A2(
			elm$core$Basics$max,
			0,
			A2(
				elm$core$Basics$min,
				mgold$elm_nonempty_list$List$Nonempty$length(inner.list) - 1,
				index));
		return author$project$LayerSelection$LayerSelection(
			_Utils_update(
				inner,
				{selectedIndex: boundedIndex}));
	});
var elm$core$Basics$modBy = _Basics_modBy;
var mgold$elm_nonempty_list$List$Nonempty$get = F2(
	function (i, ne) {
		var x = ne.a;
		var xs = ne.b;
		var j = A2(
			elm$core$Basics$modBy,
			mgold$elm_nonempty_list$List$Nonempty$length(ne),
			i);
		var find = F2(
			function (k, ys) {
				find:
				while (true) {
					if (!ys.b) {
						return x;
					} else {
						var z = ys.a;
						var zs = ys.b;
						if (!k) {
							return z;
						} else {
							var $temp$k = k - 1,
								$temp$ys = zs;
							k = $temp$k;
							ys = $temp$ys;
							continue find;
						}
					}
				}
			});
		return (!j) ? x : A2(find, j - 1, xs);
	});
var author$project$LayerSelection$selectedLayer = function (_n0) {
	var inner = _n0.a;
	return A2(mgold$elm_nonempty_list$List$Nonempty$get, inner.selectedIndex, inner.list);
};
var author$project$MapEditor$GradientEditorDialog = function (a) {
	return {$: 'GradientEditorDialog', a: a};
};
var author$project$MapEditor$NewLayerDialog = function (a) {
	return {$: 'NewLayerDialog', a: a};
};
var author$project$MapEditor$SavedFileLoaded = function (a) {
	return {$: 'SavedFileLoaded', a: a};
};
var author$project$MapEditor$SavedFileSelected = function (a) {
	return {$: 'SavedFileSelected', a: a};
};
var author$project$MapEditor$boundInt = F2(
	function (lower, upper) {
		return A2(
			elm$core$Basics$composeL,
			elm$core$Basics$max(lower),
			elm$core$Basics$min(upper));
	});
var author$project$GridEditor$updateCellMax = F2(
	function (cellMax, _n0) {
		var model = _n0.a;
		return author$project$GridEditor$State(
			_Utils_update(
				model,
				{
					cellMax: cellMax,
					grid: A3(author$project$GridEditor$boundGrid, model.cellMin, cellMax, model.grid)
				}));
	});
var author$project$GridEditor$updateCellMin = F2(
	function (cellMin, _n0) {
		var model = _n0.a;
		return author$project$GridEditor$State(
			_Utils_update(
				model,
				{
					cellMin: cellMin,
					grid: A3(author$project$GridEditor$boundGrid, cellMin, model.cellMax, model.grid)
				}));
	});
var author$project$GridEditor$updateGradient = F2(
	function (gradient, _n0) {
		var model = _n0.a;
		return author$project$GridEditor$State(
			_Utils_update(
				model,
				{gradient: gradient}));
	});
var author$project$GridEditor$updateGrid = F2(
	function (grid, _n0) {
		var model = _n0.a;
		return author$project$GridEditor$State(
			_Utils_update(
				model,
				{
					grid: A3(author$project$GridEditor$boundGrid, model.cellMin, model.cellMax, grid)
				}));
	});
var author$project$MapEditor$loadSelecteLayerIntoGridEditor = function (state) {
	var layer = author$project$LayerSelection$selectedLayer(state.layerSelection);
	return _Utils_update(
		state,
		{
			gridEditor: A2(
				author$project$GridEditor$updateGradient,
				author$project$Layer$getColorGradient(layer),
				A2(
					author$project$GridEditor$updateGrid,
					author$project$Layer$getGrid(layer),
					A2(
						author$project$GridEditor$updateCellMax,
						author$project$Layer$getMax(layer),
						A2(
							author$project$GridEditor$updateCellMin,
							author$project$Layer$getMin(layer),
							state.gridEditor))))
		});
};
var elm$json$Json$Decode$float = _Json_decodeFloat;
var elm$json$Json$Decode$map3 = _Json_map3;
var author$project$DiscreteGradient$colorDecoder = A4(
	elm$json$Json$Decode$map3,
	F3(
		function (r, g, b) {
			return avh4$elm_color$Color$fromRgba(
				{alpha: 1.0, blue: b, green: g, red: r});
		}),
	A2(elm$json$Json$Decode$field, 'red', elm$json$Json$Decode$float),
	A2(elm$json$Json$Decode$field, 'green', elm$json$Json$Decode$float),
	A2(elm$json$Json$Decode$field, 'blue', elm$json$Json$Decode$float));
var author$project$DiscreteGradient$colorStopDecoder = A3(
	elm$json$Json$Decode$map2,
	F2(
		function (value, color) {
			return {color: color, value: value};
		}),
	A2(elm$json$Json$Decode$field, 'value', elm$json$Json$Decode$int),
	A2(elm$json$Json$Decode$field, 'color', author$project$DiscreteGradient$colorDecoder));
var elm$json$Json$Decode$list = _Json_decodeList;
var author$project$DiscreteGradient$decoder = A2(
	elm$json$Json$Decode$map,
	author$project$DiscreteGradient$DiscreteGradient,
	elm$json$Json$Decode$list(author$project$DiscreteGradient$colorStopDecoder));
var elm$json$Json$Decode$array = _Json_decodeArray;
var author$project$Grid$decoder = function (cellDecoder) {
	return elm$json$Json$Decode$array(
		elm$json$Json$Decode$array(cellDecoder));
};
var elm$json$Json$Decode$map5 = _Json_map5;
var elm$json$Json$Decode$string = _Json_decodeString;
var author$project$Layer$decoder = A6(
	elm$json$Json$Decode$map5,
	F5(
		function (name, cellMin, cellMax, grid, gradient) {
			return author$project$Layer$Layer(
				{colorGradient: gradient, grid: grid, max: cellMax, min: cellMin, name: name});
		}),
	A2(elm$json$Json$Decode$field, 'name', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'cellMin', elm$json$Json$Decode$int),
	A2(elm$json$Json$Decode$field, 'cellMax', elm$json$Json$Decode$int),
	A2(
		elm$json$Json$Decode$field,
		'grid',
		author$project$Grid$decoder(elm$json$Json$Decode$int)),
	A2(elm$json$Json$Decode$field, 'gradient', author$project$DiscreteGradient$decoder));
var author$project$MapEditor$savedMapDecoder = A4(
	elm$json$Json$Decode$map3,
	F3(
		function (mapWidth, mapHeight, layers) {
			return {layers: layers, mapHeight: mapHeight, mapWidth: mapWidth};
		}),
	A2(elm$json$Json$Decode$field, 'mapWidth', elm$json$Json$Decode$int),
	A2(elm$json$Json$Decode$field, 'mapHeight', elm$json$Json$Decode$int),
	A2(
		elm$json$Json$Decode$field,
		'layers',
		elm$json$Json$Decode$list(author$project$Layer$decoder)));
var elm$json$Json$Encode$float = _Json_wrap;
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
var author$project$DiscreteGradient$colorToJson = function (color) {
	var rgba = avh4$elm_color$Color$toRgba(color);
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'red',
				elm$json$Json$Encode$float(rgba.red)),
				_Utils_Tuple2(
				'green',
				elm$json$Json$Encode$float(rgba.green)),
				_Utils_Tuple2(
				'blue',
				elm$json$Json$Encode$float(rgba.blue))
			]));
};
var elm$json$Json$Encode$int = _Json_wrap;
var author$project$DiscreteGradient$colorStopToJson = function (colorStop) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'color',
				author$project$DiscreteGradient$colorToJson(colorStop.color)),
				_Utils_Tuple2(
				'value',
				elm$json$Json$Encode$int(colorStop.value))
			]));
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
var author$project$DiscreteGradient$toJson = function (_n0) {
	var colorStops = _n0.a;
	return A2(elm$json$Json$Encode$list, author$project$DiscreteGradient$colorStopToJson, colorStops);
};
var elm$core$Array$foldl = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldl, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldl, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldl,
			func,
			A3(elm$core$Elm$JsArray$foldl, helper, baseCase, tree),
			tail);
	});
var elm$json$Json$Encode$array = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				elm$core$Array$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var author$project$Grid$toJson = function (encodeCell) {
	return elm$json$Json$Encode$array(
		elm$json$Json$Encode$array(encodeCell));
};
var elm$json$Json$Encode$string = _Json_wrap;
var author$project$Layer$toJson = function (_n0) {
	var inner = _n0.a;
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'name',
				elm$json$Json$Encode$string(inner.name)),
				_Utils_Tuple2(
				'cellMin',
				elm$json$Json$Encode$int(inner.min)),
				_Utils_Tuple2(
				'cellMax',
				elm$json$Json$Encode$int(inner.max)),
				_Utils_Tuple2(
				'grid',
				A2(author$project$Grid$toJson, elm$json$Json$Encode$int, inner.grid)),
				_Utils_Tuple2(
				'gradient',
				author$project$DiscreteGradient$toJson(inner.colorGradient))
			]));
};
var author$project$LayerSelection$toList = function (_n0) {
	var inner = _n0.a;
	return mgold$elm_nonempty_list$List$Nonempty$toList(inner.list);
};
var author$project$MapEditor$toJson = function (state) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'mapWidth',
				elm$json$Json$Encode$int(state.mapWidth)),
				_Utils_Tuple2(
				'mapHeight',
				elm$json$Json$Encode$int(state.mapHeight)),
				_Utils_Tuple2(
				'layers',
				A2(
					elm$json$Json$Encode$list,
					author$project$Layer$toJson,
					author$project$LayerSelection$toList(state.layerSelection)))
			]));
};
var author$project$LayerSelection$fromList = function (layers) {
	return A2(
		elm$core$Maybe$map,
		function (nonEmptyList) {
			return author$project$LayerSelection$LayerSelection(
				{list: nonEmptyList, selectedIndex: 0});
		},
		mgold$elm_nonempty_list$List$Nonempty$fromList(layers));
};
var author$project$MapEditor$tryToLoadDecodedContent = F2(
	function (state, decodedContent) {
		return A2(
			elm$core$Maybe$map,
			function (s) {
				return _Utils_update(
					s,
					{
						gridEditor: A2(author$project$GridEditor$updateBrush, s.brush, s.gridEditor)
					});
			},
			A2(
				elm$core$Maybe$map,
				author$project$MapEditor$loadSelecteLayerIntoGridEditor,
				A2(
					elm$core$Maybe$map,
					function (layerSelection) {
						var selectedLayer = author$project$LayerSelection$selectedLayer(layerSelection);
						var initBrush = A3(
							author$project$Brush$makeBrush,
							author$project$Brush$Circle,
							3,
							author$project$Layer$getMin(selectedLayer));
						return _Utils_update(
							state,
							{brush: initBrush, layerSelection: layerSelection, mapHeight: decodedContent.mapHeight, mapWidth: decodedContent.mapWidth});
					},
					author$project$LayerSelection$fromList(decodedContent.layers))));
	});
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$MapEditor$withNoCmd = function (x) {
	return _Utils_Tuple2(x, elm$core$Platform$Cmd$none);
};
var elm$file$File$toString = _File_toString;
var elm$file$File$Download$string = F3(
	function (name, mime, content) {
		return A2(
			elm$core$Task$perform,
			elm$core$Basics$never,
			A3(_File_download, name, mime, content));
	});
var elm$file$File$Select$file = F2(
	function (mimes, toMsg) {
		return A2(
			elm$core$Task$perform,
			toMsg,
			_File_uploadOne(mimes));
	});
var elm$json$Json$Decode$decodeString = _Json_runOnString;
var author$project$MapEditor$update = F2(
	function (msg, state) {
		switch (msg.$) {
			case 'ElapsedTime':
				var t = msg.a;
				return author$project$MapEditor$withNoCmd(
					_Utils_update(
						state,
						{
							gridEditor: A2(author$project$GridEditor$updateWithElapsedTime, t, state.gridEditor)
						}));
			case 'WindowResize':
				var w = msg.a;
				var h = msg.b;
				return author$project$MapEditor$withNoCmd(
					_Utils_update(
						state,
						{
							gridEditor: A3(
								author$project$GridEditor$updateCanvasSize,
								author$project$MapEditor$gridEditorPaneWidth(w),
								author$project$MapEditor$gridEditorPaneHeight(h),
								state.gridEditor)
						}));
			case 'SetMapWidth':
				var w = msg.a;
				return (w > 0) ? author$project$MapEditor$withNoCmd(
					author$project$MapEditor$loadSelecteLayerIntoGridEditor(
						_Utils_update(
							state,
							{
								layerSelection: A2(
									author$project$LayerSelection$map,
									A2(author$project$Layer$resizeGrid, w, state.mapHeight),
									state.layerSelection),
								mapWidth: w
							}))) : author$project$MapEditor$withNoCmd(state);
			case 'SetMapHeight':
				var h = msg.a;
				return (h > 0) ? author$project$MapEditor$withNoCmd(
					author$project$MapEditor$loadSelecteLayerIntoGridEditor(
						_Utils_update(
							state,
							{
								layerSelection: A2(
									author$project$LayerSelection$map,
									A2(author$project$Layer$resizeGrid, state.mapWidth, h),
									state.layerSelection),
								mapHeight: h
							}))) : author$project$MapEditor$withNoCmd(state);
			case 'SelectLayer':
				var index = msg.a;
				return author$project$MapEditor$withNoCmd(
					author$project$MapEditor$loadSelecteLayerIntoGridEditor(
						_Utils_update(
							state,
							{
								layerSelection: A2(author$project$LayerSelection$select, index, state.layerSelection)
							})));
			case 'DeleteSelectedLayer':
				return author$project$MapEditor$withNoCmd(
					author$project$MapEditor$loadSelecteLayerIntoGridEditor(
						_Utils_update(
							state,
							{
								layerSelection: author$project$LayerSelection$deleteSelected(state.layerSelection)
							})));
			case 'SetLayerMin':
				var newMin = msg.a;
				return author$project$MapEditor$withNoCmd(
					author$project$MapEditor$loadSelecteLayerIntoGridEditor(
						_Utils_update(
							state,
							{
								layerSelection: A2(
									author$project$LayerSelection$mapSelected,
									author$project$Layer$setMin(newMin),
									state.layerSelection)
							})));
			case 'SetLayerMax':
				var newMax = msg.a;
				return author$project$MapEditor$withNoCmd(
					author$project$MapEditor$loadSelecteLayerIntoGridEditor(
						_Utils_update(
							state,
							{
								layerSelection: A2(
									author$project$LayerSelection$mapSelected,
									author$project$Layer$setMax(newMax),
									state.layerSelection)
							})));
			case 'SetBrushWidth':
				var width = msg.a;
				var isIncreasing = _Utils_cmp(
					width,
					author$project$Brush$width(state.brush)) > 0;
				var nextWidth = (A2(elm$core$Basics$modBy, width, 2) === 1) ? width : (isIncreasing ? (width + 1) : (width - 1));
				var nextBrush = A3(author$project$Brush$makeBrush, author$project$Brush$Circle, nextWidth, state.brush.paintValue);
				return author$project$MapEditor$withNoCmd(
					_Utils_update(
						state,
						{
							brush: nextBrush,
							gridEditor: A2(author$project$GridEditor$updateBrush, nextBrush, state.gridEditor)
						}));
			case 'SetBrushPaintValue':
				var paintVal = msg.a;
				var layer = author$project$LayerSelection$selectedLayer(state.layerSelection);
				var nextPaintVal = A3(
					author$project$MapEditor$boundInt,
					author$project$Layer$getMin(layer),
					author$project$Layer$getMax(layer),
					paintVal);
				var nextBrush = A3(
					author$project$Brush$makeBrush,
					author$project$Brush$Circle,
					author$project$Brush$width(state.brush),
					nextPaintVal);
				return author$project$MapEditor$withNoCmd(
					_Utils_update(
						state,
						{
							brush: nextBrush,
							gridEditor: A2(author$project$GridEditor$updateBrush, nextBrush, state.gridEditor)
						}));
			case 'OpenNewLayerDialog':
				return author$project$MapEditor$withNoCmd(
					_Utils_update(
						state,
						{
							dialog: elm$core$Maybe$Just(
								author$project$MapEditor$NewLayerDialog(author$project$MapEditor$defaultLayerName))
						}));
			case 'SetNewLayerDialogNameField':
				var layerName = msg.a;
				var _n1 = state.dialog;
				if ((_n1.$ === 'Just') && (_n1.a.$ === 'NewLayerDialog')) {
					return author$project$MapEditor$withNoCmd(
						_Utils_update(
							state,
							{
								dialog: elm$core$Maybe$Just(
									author$project$MapEditor$NewLayerDialog(layerName))
							}));
				} else {
					return author$project$MapEditor$withNoCmd(state);
				}
			case 'NewLayerDialogCancel':
				return author$project$MapEditor$withNoCmd(
					_Utils_update(
						state,
						{dialog: elm$core$Maybe$Nothing}));
			case 'NewLayerDialogCreate':
				var _n2 = state.dialog;
				if ((_n2.$ === 'Just') && (_n2.a.$ === 'NewLayerDialog')) {
					var layerName = _n2.a.a;
					return author$project$MapEditor$withNoCmd(
						author$project$MapEditor$loadSelecteLayerIntoGridEditor(
							_Utils_update(
								state,
								{
									dialog: elm$core$Maybe$Nothing,
									layerSelection: A2(
										author$project$LayerSelection$select,
										author$project$LayerSelection$length(state.layerSelection),
										A2(
											author$project$LayerSelection$add,
											A3(author$project$MapEditor$makeLayerWithDefaultMinMax, layerName, state.mapWidth, state.mapHeight),
											state.layerSelection))
								})));
				} else {
					return author$project$MapEditor$withNoCmd(state);
				}
			case 'OpenGradientEditorDialog':
				var layer = author$project$LayerSelection$selectedLayer(state.layerSelection);
				var editorState = A3(
					author$project$DiscreteGradientEditor$init,
					author$project$Layer$getColorGradient(layer),
					author$project$Layer$getMin(layer),
					author$project$Layer$getMax(layer));
				return author$project$MapEditor$withNoCmd(
					_Utils_update(
						state,
						{
							dialog: elm$core$Maybe$Just(
								author$project$MapEditor$GradientEditorDialog(editorState))
						}));
			case 'GradientEditorMsg':
				var editorMsg = msg.a;
				var _n3 = state.dialog;
				if ((_n3.$ === 'Just') && (_n3.a.$ === 'GradientEditorDialog')) {
					var dialog = _n3.a.a;
					var _n4 = A2(author$project$DiscreteGradientEditor$update, editorMsg, dialog);
					var newDialog = _n4.a;
					var output = _n4.b;
					switch (output.$) {
						case 'EditInProgress':
							return author$project$MapEditor$withNoCmd(
								_Utils_update(
									state,
									{
										dialog: elm$core$Maybe$Just(
											author$project$MapEditor$GradientEditorDialog(newDialog))
									}));
						case 'Cancel':
							return author$project$MapEditor$withNoCmd(
								_Utils_update(
									state,
									{dialog: elm$core$Maybe$Nothing}));
						default:
							var gradient = output.a;
							return author$project$MapEditor$withNoCmd(
								author$project$MapEditor$loadSelecteLayerIntoGridEditor(
									_Utils_update(
										state,
										{
											dialog: elm$core$Maybe$Nothing,
											layerSelection: A2(
												author$project$LayerSelection$mapSelected,
												author$project$Layer$setColorGradient(gradient),
												state.layerSelection)
										})));
					}
				} else {
					return author$project$MapEditor$withNoCmd(state);
				}
			case 'GridEditorMsg':
				var editorMsg = msg.a;
				var _n6 = A2(author$project$GridEditor$update, editorMsg, state.gridEditor);
				var newGridEditor = _n6.a;
				var grid = _n6.b;
				return author$project$MapEditor$withNoCmd(
					_Utils_update(
						state,
						{
							gridEditor: newGridEditor,
							layerSelection: A2(
								author$project$LayerSelection$mapSelected,
								author$project$Layer$setGrid(grid),
								state.layerSelection)
						}));
			case 'Download':
				var downloadCmd = A3(
					elm$file$File$Download$string,
					state.name,
					'application/json',
					A2(
						elm$json$Json$Encode$encode,
						2,
						author$project$MapEditor$toJson(state)));
				return _Utils_Tuple2(state, downloadCmd);
			case 'SavedFileRequested':
				return _Utils_Tuple2(
					state,
					A2(
						elm$file$File$Select$file,
						_List_fromArray(
							['application/json']),
						author$project$MapEditor$SavedFileSelected));
			case 'SavedFileSelected':
				var file = msg.a;
				return _Utils_Tuple2(
					state,
					A2(
						elm$core$Task$perform,
						author$project$MapEditor$SavedFileLoaded,
						elm$file$File$toString(file)));
			case 'SavedFileLoaded':
				var content = msg.a;
				var nextState = A2(
					elm$core$Maybe$withDefault,
					state,
					A2(
						elm$core$Maybe$andThen,
						author$project$MapEditor$tryToLoadDecodedContent(state),
						elm$core$Result$toMaybe(
							A2(elm$json$Json$Decode$decodeString, author$project$MapEditor$savedMapDecoder, content))));
				return _Utils_Tuple2(nextState, elm$core$Platform$Cmd$none);
			default:
				return author$project$MapEditor$withNoCmd(state);
		}
	});
var author$project$DiscreteGradientEditor$ClickCancel = {$: 'ClickCancel'};
var author$project$DiscreteGradientEditor$ClickSave = {$: 'ClickSave'};
var author$project$DiscreteGradientEditor$ColorPickerMsg = function (a) {
	return {$: 'ColorPickerMsg', a: a};
};
var elm$html$Html$div = _VirtualDom_node('div');
var elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var elm$html$Html$map = elm$virtual_dom$VirtualDom$map;
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
var author$project$GridEditor$MouseDown = F2(
	function (a, b) {
		return {$: 'MouseDown', a: a, b: b};
	});
var author$project$GridEditor$MouseLeave = {$: 'MouseLeave'};
var author$project$GridEditor$MouseMove = function (a) {
	return {$: 'MouseMove', a: a};
};
var author$project$GridEditor$MouseUp = {$: 'MouseUp'};
var author$project$GridEditor$Scroll = function (a) {
	return {$: 'Scroll', a: a};
};
var elm_explorations$linear_algebra$Math$Matrix4$makeOrtho2D = _MJS_m4x4makeOrtho2D;
var author$project$GridEditor$cameraFromBounds = function (bounds) {
	return A4(elm_explorations$linear_algebra$Math$Matrix4$makeOrtho2D, bounds.left, bounds.right, bounds.bottom, bounds.top);
};
var author$project$GridEditor$fragmentShader = {
	src: '\n        precision mediump float;\n        varying vec3 vcolor;\n\n        void main () {\n            gl_FragColor = vec4(vcolor, 1.0);\n        }\n    ',
	attributes: {},
	uniforms: {}
};
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
var author$project$Grid$toListWithCoordinates = function (grid) {
	return elm$core$List$concat(
		elm$core$Array$toList(
			A2(
				elm$core$Array$map,
				elm$core$Array$toList,
				A2(
					author$project$Grid$mapWithCoordinate,
					F2(
						function (coord, cell) {
							return _Utils_Tuple2(coord, cell);
						}),
					grid))));
};
var author$project$GridEditor$Vertex = F2(
	function (position, color) {
		return {color: color, position: position};
	});
var author$project$GridEditor$borderInset = 5.0e-3;
var author$project$GridEditor$makeBorderColor = function (color) {
	var c = avh4$elm_color$Color$toHsla(color);
	return A4(
		avh4$elm_color$Color$hsla,
		c.hue,
		A2(elm$core$Basics$min, 1.0, c.saturation + 0.125),
		A2(elm$core$Basics$max, 0.0, c.lightness - 0.125),
		c.alpha);
};
var elm_explorations$linear_algebra$Math$Vector3$vec3 = _MJS_v3;
var author$project$GridEditor$vec3FromColor = function (color) {
	var rbga = avh4$elm_color$Color$toRgba(color);
	return A3(elm_explorations$linear_algebra$Math$Vector3$vec3, rbga.red, rbga.green, rbga.blue);
};
var author$project$GridEditor$trianglesFromCell = F6(
	function (gradient, gridOriginX, gridOriginY, cellIndexI, cellIndexJ, cellValue) {
		var colorFromGradient = A2(author$project$DiscreteGradient$getColorAt, cellValue, gradient);
		var color = author$project$GridEditor$vec3FromColor(colorFromGradient);
		var cellLeft = gridOriginX + (cellIndexI * author$project$GridEditor$cellWidth);
		var cellRight = cellLeft + author$project$GridEditor$cellWidth;
		var insetRight = cellRight - author$project$GridEditor$borderInset;
		var insetLeft = cellLeft + author$project$GridEditor$borderInset;
		var cellBottom = gridOriginY + (cellIndexJ * author$project$GridEditor$cellWidth);
		var cellTop = cellBottom + author$project$GridEditor$cellWidth;
		var insetTop = cellTop - author$project$GridEditor$borderInset;
		var innerCell = _List_fromArray(
			[
				_Utils_Tuple3(
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellLeft, cellTop, 0),
					color),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellRight, cellTop, 0),
					color),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellRight, cellBottom, 0),
					color)),
				_Utils_Tuple3(
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellLeft, cellTop, 0),
					color),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellRight, cellBottom, 0),
					color),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellLeft, cellBottom, 0),
					color))
			]);
		var insetBottom = cellBottom + author$project$GridEditor$borderInset;
		var borderColor = author$project$GridEditor$vec3FromColor(
			author$project$GridEditor$makeBorderColor(colorFromGradient));
		var bottomBorder = _List_fromArray(
			[
				_Utils_Tuple3(
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, insetLeft, insetBottom, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, insetRight, insetBottom, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellRight, cellBottom, 0),
					borderColor)),
				_Utils_Tuple3(
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, insetLeft, insetBottom, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellRight, cellBottom, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellLeft, cellBottom, 0),
					borderColor))
			]);
		var leftBorder = _List_fromArray(
			[
				_Utils_Tuple3(
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellLeft, cellTop, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, insetLeft, insetTop, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellLeft, cellBottom, 0),
					borderColor)),
				_Utils_Tuple3(
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, insetLeft, insetTop, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, insetLeft, insetBottom, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellLeft, cellBottom, 0),
					borderColor))
			]);
		var rightBorder = _List_fromArray(
			[
				_Utils_Tuple3(
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, insetRight, insetTop, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellRight, cellTop, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellRight, cellBottom, 0),
					borderColor)),
				_Utils_Tuple3(
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, insetRight, insetTop, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellRight, cellBottom, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, insetRight, insetBottom, 0),
					borderColor))
			]);
		var topBorder = _List_fromArray(
			[
				_Utils_Tuple3(
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellLeft, cellTop, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellRight, cellTop, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, insetRight, insetTop, 0),
					borderColor)),
				_Utils_Tuple3(
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, cellLeft, cellTop, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, insetRight, insetTop, 0),
					borderColor),
				A2(
					author$project$GridEditor$Vertex,
					A3(elm_explorations$linear_algebra$Math$Vector3$vec3, insetLeft, insetTop, 0),
					borderColor))
			]);
		return elm$core$List$concat(
			_List_fromArray(
				[leftBorder, rightBorder, topBorder, bottomBorder, innerCell]));
	});
var author$project$GridEditor$trianglesFromGrid = F2(
	function (gradient, grid) {
		var gridOriginY = author$project$GridEditor$gridBottomInScene(
			author$project$Grid$height(grid));
		var gridOriginX = author$project$GridEditor$gridLeftInScene(
			author$project$Grid$width(grid));
		return elm$core$List$concat(
			A2(
				elm$core$List$map,
				function (_n0) {
					var _n1 = _n0.a;
					var i = _n1.a;
					var j = _n1.b;
					var cellValue = _n0.b;
					return A6(author$project$GridEditor$trianglesFromCell, gradient, gridOriginX, gridOriginY, i, j, cellValue);
				},
				author$project$Grid$toListWithCoordinates(grid)));
	});
var elm_explorations$webgl$WebGL$Mesh3 = F2(
	function (a, b) {
		return {$: 'Mesh3', a: a, b: b};
	});
var elm_explorations$webgl$WebGL$triangles = elm_explorations$webgl$WebGL$Mesh3(
	{elemSize: 3, indexSize: 0, mode: 4});
var author$project$GridEditor$mesh = F2(
	function (gradient, grid) {
		return elm_explorations$webgl$WebGL$triangles(
			A2(author$project$GridEditor$trianglesFromGrid, gradient, grid));
	});
var author$project$GridEditor$vertexShader = {
	src: '\n        attribute vec3 position;\n        attribute vec3 color;\n        uniform mat4 camera;\n        varying vec3 vcolor;\n\n        void main () {\n            gl_Position = camera * vec4(position, 1.0);\n            vcolor = color;\n        }\n    ',
	attributes: {color: 'color', position: 'position'},
	uniforms: {camera: 'camera'}
};
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
var elm_explorations$webgl$WebGL$Internal$disableSetting = F2(
	function (cache, setting) {
		switch (setting.$) {
			case 'Blend':
				return _WebGL_disableBlend(cache);
			case 'DepthTest':
				return _WebGL_disableDepthTest(cache);
			case 'StencilTest':
				return _WebGL_disableStencilTest(cache);
			case 'Scissor':
				return _WebGL_disableScissor(cache);
			case 'ColorMask':
				return _WebGL_disableColorMask(cache);
			case 'CullFace':
				return _WebGL_disableCullFace(cache);
			case 'PolygonOffset':
				return _WebGL_disablePolygonOffset(cache);
			case 'SampleCoverage':
				return _WebGL_disableSampleCoverage(cache);
			default:
				return _WebGL_disableSampleAlphaToCoverage(cache);
		}
	});
var elm_explorations$webgl$WebGL$Internal$enableOption = F2(
	function (ctx, option) {
		switch (option.$) {
			case 'Alpha':
				return A2(_WebGL_enableAlpha, ctx, option);
			case 'Depth':
				return A2(_WebGL_enableDepth, ctx, option);
			case 'Stencil':
				return A2(_WebGL_enableStencil, ctx, option);
			case 'Antialias':
				return A2(_WebGL_enableAntialias, ctx, option);
			case 'ClearColor':
				return A2(_WebGL_enableClearColor, ctx, option);
			default:
				return A2(_WebGL_enablePreserveDrawingBuffer, ctx, option);
		}
	});
var elm_explorations$webgl$WebGL$Internal$enableSetting = F2(
	function (gl, setting) {
		switch (setting.$) {
			case 'Blend':
				return A2(_WebGL_enableBlend, gl, setting);
			case 'DepthTest':
				return A2(_WebGL_enableDepthTest, gl, setting);
			case 'StencilTest':
				return A2(_WebGL_enableStencilTest, gl, setting);
			case 'Scissor':
				return A2(_WebGL_enableScissor, gl, setting);
			case 'ColorMask':
				return A2(_WebGL_enableColorMask, gl, setting);
			case 'CullFace':
				return A2(_WebGL_enableCullFace, gl, setting);
			case 'PolygonOffset':
				return A2(_WebGL_enablePolygonOffset, gl, setting);
			case 'SampleCoverage':
				return A2(_WebGL_enableSampleCoverage, gl, setting);
			default:
				return A2(_WebGL_enableSampleAlphaToCoverage, gl, setting);
		}
	});
var elm_explorations$webgl$WebGL$entityWith = _WebGL_entity;
var elm_explorations$webgl$WebGL$Internal$DepthTest = F4(
	function (a, b, c, d) {
		return {$: 'DepthTest', a: a, b: b, c: c, d: d};
	});
var elm_explorations$webgl$WebGL$Settings$DepthTest$less = function (_n0) {
	var write = _n0.write;
	var near = _n0.near;
	var far = _n0.far;
	return A4(elm_explorations$webgl$WebGL$Internal$DepthTest, 513, write, near, far);
};
var elm_explorations$webgl$WebGL$Settings$DepthTest$default = elm_explorations$webgl$WebGL$Settings$DepthTest$less(
	{far: 1, near: 0, write: true});
var elm_explorations$webgl$WebGL$entity = elm_explorations$webgl$WebGL$entityWith(
	_List_fromArray(
		[elm_explorations$webgl$WebGL$Settings$DepthTest$default]));
var elm_explorations$webgl$WebGL$Internal$Alpha = function (a) {
	return {$: 'Alpha', a: a};
};
var elm_explorations$webgl$WebGL$alpha = elm_explorations$webgl$WebGL$Internal$Alpha;
var elm_explorations$webgl$WebGL$Internal$Antialias = {$: 'Antialias'};
var elm_explorations$webgl$WebGL$antialias = elm_explorations$webgl$WebGL$Internal$Antialias;
var elm_explorations$webgl$WebGL$Internal$Depth = function (a) {
	return {$: 'Depth', a: a};
};
var elm_explorations$webgl$WebGL$depth = elm_explorations$webgl$WebGL$Internal$Depth;
var elm_explorations$webgl$WebGL$toHtmlWith = F3(
	function (options, attributes, entities) {
		return A3(_WebGL_toHtml, options, attributes, entities);
	});
var elm_explorations$webgl$WebGL$toHtml = elm_explorations$webgl$WebGL$toHtmlWith(
	_List_fromArray(
		[
			elm_explorations$webgl$WebGL$alpha(true),
			elm_explorations$webgl$WebGL$antialias,
			elm_explorations$webgl$WebGL$depth(1)
		]));
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
var elm$json$Json$Decode$map6 = _Json_map6;
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
var elm$json$Json$Decode$bool = _Json_decodeBool;
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
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onLeave = A2(mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions, 'mouseleave', mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions);
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
var author$project$GridEditor$view = function (_n0) {
	var model = _n0.a;
	return A2(
		elm_explorations$webgl$WebGL$toHtml,
		_List_fromArray(
			[
				elm$html$Html$Attributes$width(
				elm$core$Basics$round(model.width)),
				elm$html$Html$Attributes$height(
				elm$core$Basics$round(model.height)),
				A2(elm$html$Html$Attributes$style, 'display', 'block'),
				mpizenberg$elm_pointer_events$Html$Events$Extra$Wheel$onWheel(
				function (event) {
					return author$project$GridEditor$Scroll(event.deltaY);
				}),
				mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onDown(
				function (event) {
					return A2(author$project$GridEditor$MouseDown, event.button, event.offsetPos);
				}),
				mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onUp(
				function (_n1) {
					return author$project$GridEditor$MouseUp;
				}),
				mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onMove(
				function (event) {
					return author$project$GridEditor$MouseMove(event.offsetPos);
				}),
				mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onLeave(
				function (_n2) {
					return author$project$GridEditor$MouseLeave;
				})
			]),
		_List_fromArray(
			[
				A4(
				elm_explorations$webgl$WebGL$entity,
				author$project$GridEditor$vertexShader,
				author$project$GridEditor$fragmentShader,
				A2(author$project$GridEditor$mesh, model.gradient, model.grid),
				{
					camera: author$project$GridEditor$cameraFromBounds(
						A4(author$project$GridEditor$cameraBounds, model.cameraCenter, model.width, model.height, model.zoomStep))
				})
			]));
};
var author$project$MapEditor$GridEditorMsg = function (a) {
	return {$: 'GridEditorMsg', a: a};
};
var author$project$MapEditor$gridEditorPaneView = function (gridEditor) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('grid-editor-pane')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$map,
				author$project$MapEditor$GridEditorMsg,
				author$project$GridEditor$view(gridEditor))
			]));
};
var author$project$LayerSelection$selectedIndex = function (_n0) {
	var inner = _n0.a;
	return inner.selectedIndex;
};
var author$project$MapEditor$brushValueFieldLabel = A2(
	elm$html$Html$div,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('brush-value-field__label')
		]),
	_List_fromArray(
		[
			elm$html$Html$text('Value')
		]));
var author$project$MapEditor$SetBrushPaintValue = function (a) {
	return {$: 'SetBrushPaintValue', a: a};
};
var author$project$MapEditor$NoOp = {$: 'NoOp'};
var author$project$MapEditor$stringToIntMsgOrNoOp = F2(
	function (toMsg, s) {
		return A2(
			elm$core$Maybe$withDefault,
			author$project$MapEditor$NoOp,
			A2(
				elm$core$Maybe$map,
				toMsg,
				elm$core$String$toInt(s)));
	});
var author$project$MapEditor$toSetBrushPaintValue = author$project$MapEditor$stringToIntMsgOrNoOp(author$project$MapEditor$SetBrushPaintValue);
var author$project$MapEditor$brushValueInput = function (brushValue) {
	return A2(
		elm$html$Html$input,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('brush-value-field__input'),
				elm$html$Html$Attributes$type_('number'),
				elm$html$Html$Attributes$value(
				elm$core$String$fromInt(brushValue)),
				elm$html$Html$Events$onInput(author$project$MapEditor$toSetBrushPaintValue)
			]),
		_List_Nil);
};
var author$project$MapEditor$brushValueRow = function (brushValue) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('brush-value-field__row')
			]),
		_List_fromArray(
			[
				author$project$MapEditor$brushValueInput(brushValue)
			]));
};
var author$project$MapEditor$brushValueField = function (brushValue) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('brush-value-field')
			]),
		_List_fromArray(
			[
				author$project$MapEditor$brushValueFieldLabel,
				author$project$MapEditor$brushValueRow(brushValue)
			]));
};
var author$project$MapEditor$brushWidthFieldLabel = A2(
	elm$html$Html$div,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('brush-width-field__label')
		]),
	_List_fromArray(
		[
			elm$html$Html$text('Width')
		]));
var author$project$MapEditor$SetBrushWidth = function (a) {
	return {$: 'SetBrushWidth', a: a};
};
var author$project$MapEditor$toSetBrushWidth = author$project$MapEditor$stringToIntMsgOrNoOp(author$project$MapEditor$SetBrushWidth);
var author$project$MapEditor$brushWidthInput = function (brushWidth) {
	return A2(
		elm$html$Html$input,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('brush-width-field__input'),
				elm$html$Html$Attributes$type_('number'),
				elm$html$Html$Attributes$value(
				elm$core$String$fromInt(brushWidth)),
				elm$html$Html$Events$onInput(author$project$MapEditor$toSetBrushWidth)
			]),
		_List_Nil);
};
var author$project$MapEditor$brushWidthRow = function (brushWidth) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('brush-width-field__row')
			]),
		_List_fromArray(
			[
				author$project$MapEditor$brushWidthInput(brushWidth)
			]));
};
var author$project$MapEditor$brushWidthField = function (brushWidth) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('brush-width-field')
			]),
		_List_fromArray(
			[
				author$project$MapEditor$brushWidthFieldLabel,
				author$project$MapEditor$brushWidthRow(brushWidth)
			]));
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
var author$project$MapEditor$colorGradient = F3(
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
var author$project$MapEditor$colorGradientRow = function (layer) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('color-gradient__row')
			]),
		_List_fromArray(
			[
				A3(
				author$project$MapEditor$colorGradient,
				author$project$Layer$getColorGradient(layer),
				author$project$Layer$getMin(layer),
				author$project$Layer$getMax(layer)),
				author$project$MapEditor$colorGradientEditButton(false)
			]));
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
var author$project$MapEditor$Download = {$: 'Download'};
var author$project$MapEditor$downloadButton = A2(
	elm$html$Html$button,
	_List_fromArray(
		[
			elm$html$Html$Events$onClick(author$project$MapEditor$Download),
			A2(elm$html$Html$Attributes$style, 'margin-bottom', '8px'),
			A2(elm$html$Html$Attributes$style, 'width', '100px')
		]),
	_List_fromArray(
		[
			elm$html$Html$text('Download')
		]));
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
var author$project$Layer$getName = function (_n0) {
	var inner = _n0.a;
	return inner.name;
};
var elm$html$Html$option = _VirtualDom_node('option');
var elm$html$Html$Attributes$selected = elm$html$Html$Attributes$boolProperty('selected');
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
					_Utils_eq(i, selectedIndex));
			}));
};
var author$project$MapEditor$SelectLayer = function (a) {
	return {$: 'SelectLayer', a: a};
};
var author$project$MapEditor$toSelectLayerMsg = author$project$MapEditor$stringToIntMsgOrNoOp(author$project$MapEditor$SelectLayer);
var elm$html$Html$select = _VirtualDom_node('select');
var author$project$MapEditor$layerFieldSelect = F2(
	function (selectedIndex, layers) {
		return A2(
			elm$html$Html$select,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('layer-field__select'),
					elm$html$Html$Events$onInput(author$project$MapEditor$toSelectLayerMsg)
				]),
			A2(author$project$MapEditor$layerFieldOptions, selectedIndex, layers));
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
var author$project$MapEditor$minMaxFieldInput = F2(
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
var author$project$MapEditor$toSetLayerMaxMsg = author$project$MapEditor$stringToIntMsgOrNoOp(author$project$MapEditor$SetLayerMax);
var author$project$MapEditor$maxInput = function (val) {
	return A2(author$project$MapEditor$minMaxFieldInput, val, author$project$MapEditor$toSetLayerMaxMsg);
};
var author$project$MapEditor$SetLayerMin = function (a) {
	return {$: 'SetLayerMin', a: a};
};
var author$project$MapEditor$toSetLayerMinMsg = author$project$MapEditor$stringToIntMsgOrNoOp(author$project$MapEditor$SetLayerMin);
var author$project$MapEditor$minInput = function (val) {
	return A2(author$project$MapEditor$minMaxFieldInput, val, author$project$MapEditor$toSetLayerMinMsg);
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
				author$project$Layer$getMin(layer)),
				author$project$MapEditor$minMaxFieldInputSeparator,
				author$project$MapEditor$maxInput(
				author$project$Layer$getMax(layer))
			]));
};
var author$project$MapEditor$SavedFileRequested = {$: 'SavedFileRequested'};
var author$project$MapEditor$openButton = A2(
	elm$html$Html$button,
	_List_fromArray(
		[
			elm$html$Html$Events$onClick(author$project$MapEditor$SavedFileRequested),
			A2(elm$html$Html$Attributes$style, 'margin-bottom', '8px'),
			A2(elm$html$Html$Attributes$style, 'width', '100px')
		]),
	_List_fromArray(
		[
			elm$html$Html$text('Open')
		]));
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
var author$project$MapEditor$SetMapHeight = function (a) {
	return {$: 'SetMapHeight', a: a};
};
var author$project$MapEditor$toSetMapHeight = author$project$MapEditor$stringToIntMsgOrNoOp(author$project$MapEditor$SetMapHeight);
var author$project$MapEditor$SetMapWidth = function (a) {
	return {$: 'SetMapWidth', a: a};
};
var author$project$MapEditor$toSetMapWidth = author$project$MapEditor$stringToIntMsgOrNoOp(author$project$MapEditor$SetMapWidth);
var author$project$MapEditor$widthHeightFieldInput = F2(
	function (val, toMsg) {
		return A2(
			elm$html$Html$input,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('width-height-field__input'),
					elm$html$Html$Attributes$type_('number'),
					elm$html$Html$Attributes$value(
					elm$core$String$fromInt(val)),
					elm$html$Html$Events$onInput(toMsg)
				]),
			_List_Nil);
	});
var author$project$MapEditor$widthHeightFieldInputSeparator = A2(
	elm$html$Html$div,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('width-height-field__input-separator')
		]),
	_List_fromArray(
		[
			elm$html$Html$text('/')
		]));
var author$project$MapEditor$widthHeightFieldLabel = A2(
	elm$html$Html$div,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('width-height-field__label')
		]),
	_List_fromArray(
		[
			elm$html$Html$text('Width / Height')
		]));
var author$project$MapEditor$widthHeightField = F2(
	function (mapWidth, mapHeight) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('width-height-field')
				]),
			_List_fromArray(
				[
					author$project$MapEditor$widthHeightFieldLabel,
					A2(author$project$MapEditor$widthHeightFieldInput, mapWidth, author$project$MapEditor$toSetMapWidth),
					author$project$MapEditor$widthHeightFieldInputSeparator,
					A2(author$project$MapEditor$widthHeightFieldInput, mapHeight, author$project$MapEditor$toSetMapHeight)
				]));
	});
var author$project$MapEditor$toolbar = function (state) {
	var layerIndex = author$project$LayerSelection$selectedIndex(state.layerSelection);
	var layer = author$project$LayerSelection$selectedLayer(state.layerSelection);
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('toolbar'),
				A2(
				elm$html$Html$Attributes$style,
				'width',
				elm$core$String$fromInt(author$project$MapEditor$toolbarWidth) + 'px')
			]),
		_List_fromArray(
			[
				author$project$MapEditor$toolbarSectionHeader('Map'),
				author$project$MapEditor$toolbarSectionContents(
				_List_fromArray(
					[
						author$project$MapEditor$downloadButton,
						author$project$MapEditor$openButton,
						A2(author$project$MapEditor$widthHeightField, state.mapWidth, state.mapHeight)
					])),
				author$project$MapEditor$toolbarSectionHeader('Layer'),
				author$project$MapEditor$toolbarSectionContents(
				_List_fromArray(
					[
						A2(
						author$project$MapEditor$layerField,
						layerIndex,
						author$project$LayerSelection$toList(state.layerSelection)),
						author$project$MapEditor$minMaxField(layer),
						author$project$MapEditor$colorGradientField(layer)
					])),
				author$project$MapEditor$toolbarSectionHeader('Brush'),
				author$project$MapEditor$toolbarSectionContents(
				_List_fromArray(
					[
						author$project$MapEditor$brushWidthField(
						author$project$Brush$width(state.brush)),
						author$project$MapEditor$brushValueField(state.brush.paintValue)
					]))
			]));
};
var author$project$MapEditor$view = function (state) {
	var content = _Utils_ap(
		_List_fromArray(
			[
				author$project$MapEditor$toolbar(state),
				author$project$MapEditor$gridEditorPaneView(state.gridEditor)
			]),
		author$project$MapEditor$dialogView(state.dialog));
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('page')
			]),
		content);
};
var elm$browser$Browser$element = _Browser_element;
var elm$json$Json$Decode$andThen = _Json_andThen;
var elm$json$Json$Decode$index = _Json_decodeIndex;
var author$project$Main$main = elm$browser$Browser$element(
	{
		init: function (windowSize) {
			return _Utils_Tuple2(
				author$project$MapEditor$init(windowSize),
				elm$core$Platform$Cmd$none);
		},
		subscriptions: author$project$MapEditor$subscriptions,
		update: author$project$MapEditor$update,
		view: author$project$MapEditor$view
	});
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
				A2(elm$json$Json$Decode$index, 1, elm$json$Json$Decode$int));
		},
		A2(elm$json$Json$Decode$index, 0, elm$json$Json$Decode$int)))(0)}});}(this));