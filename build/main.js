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

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


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
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
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
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
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
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



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
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
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

	if (typeof File !== 'undefined' && value instanceof File)
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
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
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



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
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
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
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



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
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
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
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
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
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
				? $elm$core$Result$Ok(decoder.c)
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
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

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
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

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
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
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
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

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
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


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
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
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
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

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
		u: converter,
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
	var converter = _Platform_effectManagers[name].u;

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

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

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
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
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
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
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
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
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

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

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
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
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
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
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
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
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
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
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
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
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



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
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
        return $elm$core$Maybe$Nothing;
    }

    det = 1.0 / det;

    for (var i = 0; i < 16; i = i + 1) {
        r[i] = r[i] * det;
    }

    return $elm$core$Maybe$Just(r);
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
var _WebGL_enableBlend = F2(function (cache, setting) {
  var blend = cache.blend;
  blend.toggle = cache.toggle;

  if (!blend.enabled) {
    cache.gl.enable(cache.gl.BLEND);
    blend.enabled = true;
  }

  // a   b   c   d   e   f   g h i j
  // eq1 f11 f12 eq2 f21 f22 r g b a
  if (blend.a !== setting.a || blend.d !== setting.d) {
    cache.gl.blendEquationSeparate(setting.a, setting.d);
    blend.a = setting.a;
    blend.d = setting.d;
  }
  if (blend.b !== setting.b || blend.c !== setting.c || blend.e !== setting.e || blend.f !== setting.f) {
    cache.gl.blendFuncSeparate(setting.b, setting.c, setting.e, setting.f);
    blend.b = setting.b;
    blend.c = setting.c;
    blend.e = setting.e;
    blend.f = setting.f;
  }
  if (blend.g !== setting.g || blend.h !== setting.h || blend.i !== setting.i || blend.j !== setting.j) {
    cache.gl.blendColor(setting.g, setting.h, setting.i, setting.j);
    blend.g = setting.g;
    blend.h = setting.h;
    blend.i = setting.i;
    blend.j = setting.j;
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableDepthTest = F2(function (cache, setting) {
  var depthTest = cache.depthTest;
  depthTest.toggle = cache.toggle;

  if (!depthTest.enabled) {
    cache.gl.enable(cache.gl.DEPTH_TEST);
    depthTest.enabled = true;
  }

  // a    b    c    d
  // func mask near far
  if (depthTest.a !== setting.a) {
    cache.gl.depthFunc(setting.a);
    depthTest.a = setting.a;
  }
  if (depthTest.b !== setting.b) {
    cache.gl.depthMask(setting.b);
    depthTest.b = setting.b;
  }
  if (depthTest.c !== setting.c || depthTest.d !== setting.d) {
    cache.gl.depthRange(setting.c, setting.d);
    depthTest.c = setting.c;
    depthTest.d = setting.d;
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableStencilTest = F2(function (cache, setting) {
  var stencilTest = cache.stencilTest;
  stencilTest.toggle = cache.toggle;

  if (!stencilTest.enabled) {
    cache.gl.enable(cache.gl.STENCIL_TEST);
    stencilTest.enabled = true;
  }

  // a   b    c         d     e     f      g      h     i     j      k
  // ref mask writeMask test1 fail1 zfail1 zpass1 test2 fail2 zfail2 zpass2
  if (stencilTest.d !== setting.d || stencilTest.a !== setting.a || stencilTest.b !== setting.b) {
    cache.gl.stencilFuncSeparate(cache.gl.FRONT, setting.d, setting.a, setting.b);
    stencilTest.d = setting.d;
    // a and b are set in the cache.gl.BACK diffing because they should be the same
  }
  if (stencilTest.e !== setting.e || stencilTest.f !== setting.f || stencilTest.g !== setting.g) {
    cache.gl.stencilOpSeparate(cache.gl.FRONT, setting.e, setting.f, setting.g);
    stencilTest.e = setting.e;
    stencilTest.f = setting.f;
    stencilTest.g = setting.g;
  }
  if (stencilTest.c !== setting.c) {
    cache.gl.stencilMask(setting.c);
    stencilTest.c = setting.c;
  }
  if (stencilTest.h !== setting.h || stencilTest.a !== setting.a || stencilTest.b !== setting.b) {
    cache.gl.stencilFuncSeparate(cache.gl.BACK, setting.h, setting.a, setting.b);
    stencilTest.h = setting.h;
    stencilTest.a = setting.a;
    stencilTest.b = setting.b;
  }
  if (stencilTest.i !== setting.i || stencilTest.j !== setting.j || stencilTest.k !== setting.k) {
    cache.gl.stencilOpSeparate(cache.gl.BACK, setting.i, setting.j, setting.k);
    stencilTest.i = setting.i;
    stencilTest.j = setting.j;
    stencilTest.k = setting.k;
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableScissor = F2(function (cache, setting) {
  var scissor = cache.scissor;
  scissor.toggle = cache.toggle;

  if (!scissor.enabled) {
    cache.gl.enable(cache.gl.SCISSOR_TEST);
    scissor.enabled = true;
  }

  if (scissor.a !== setting.a || scissor.b !== setting.b || scissor.c !== setting.c || scissor.d !== setting.d) {
    cache.gl.scissor(setting.a, setting.b, setting.c, setting.d);
    scissor.a = setting.a;
    scissor.b = setting.b;
    scissor.c = setting.c;
    scissor.d = setting.d;
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableColorMask = F2(function (cache, setting) {
  var colorMask = cache.colorMask;
  colorMask.toggle = cache.toggle;
  colorMask.enabled = true;

  if (colorMask.a !== setting.a || colorMask.b !== setting.b || colorMask.c !== setting.c || colorMask.d !== setting.d) {
    cache.gl.colorMask(setting.a, setting.b, setting.c, setting.d);
    colorMask.a = setting.a;
    colorMask.b = setting.b;
    colorMask.c = setting.c;
    colorMask.d = setting.d;
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableCullFace = F2(function (cache, setting) {
  var cullFace = cache.cullFace;
  cullFace.toggle = cache.toggle;

  if (!cullFace.enabled) {
    cache.gl.enable(cache.gl.CULL_FACE);
    cullFace.enabled = true;
  }

  if (cullFace.a !== setting.a) {
    cache.gl.cullFace(setting.a);
    cullFace.a = setting.a;
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enablePolygonOffset = F2(function (cache, setting) {
  var polygonOffset = cache.polygonOffset;
  polygonOffset.toggle = cache.toggle;

  if (!polygonOffset.enabled) {
    cache.gl.enable(cache.gl.POLYGON_OFFSET_FILL);
    polygonOffset.enabled = true;
  }

  if (polygonOffset.a !== setting.a || polygonOffset.b !== setting.b) {
    cache.gl.polygonOffset(setting.a, setting.b);
    polygonOffset.a = setting.a;
    polygonOffset.b = setting.b;
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableSampleCoverage = F2(function (cache, setting) {
  var sampleCoverage = cache.sampleCoverage;
  sampleCoverage.toggle = cache.toggle;

  if (!sampleCoverage.enabled) {
    cache.gl.enable(cache.gl.SAMPLE_COVERAGE);
    sampleCoverage.enabled = true;
  }

  if (sampleCoverage.a !== setting.a || sampleCoverage.b !== setting.b) {
    cache.gl.sampleCoverage(setting.a, setting.b);
    sampleCoverage.a = setting.a;
    sampleCoverage.b = setting.b;
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableSampleAlphaToCoverage = function (cache) {
  var sampleAlphaToCoverage = cache.sampleAlphaToCoverage;
  sampleAlphaToCoverage.toggle = cache.toggle;

  if (!sampleAlphaToCoverage.enabled) {
    cache.gl.enable(cache.gl.SAMPLE_ALPHA_TO_COVERAGE);
    sampleAlphaToCoverage.enabled = true;
  }
};

var _WebGL_disableBlend = function (cache) {
  if (cache.blend.enabled) {
    cache.gl.disable(cache.gl.BLEND);
    cache.blend.enabled = false;
  }
};

var _WebGL_disableDepthTest = function (cache) {
  if (cache.depthTest.enabled) {
    cache.gl.disable(cache.gl.DEPTH_TEST);
    cache.depthTest.enabled = false;
  }
};

var _WebGL_disableStencilTest = function (cache) {
  if (cache.stencilTest.enabled) {
    cache.gl.disable(cache.gl.STENCIL_TEST);
    cache.stencilTest.enabled = false;
  }
};

var _WebGL_disableScissor = function (cache) {
  if (cache.scissor.enabled) {
    cache.gl.disable(cache.gl.SCISSOR_TEST);
    cache.scissor.enabled = false;
  }
};

var _WebGL_disableColorMask = function (cache) {
  var colorMask = cache.colorMask;
  if (!colorMask.a || !colorMask.b || !colorMask.c || !colorMask.d) {
    cache.gl.colorMask(true, true, true, true);
    colorMask.a = true;
    colorMask.b = true;
    colorMask.c = true;
    colorMask.d = true;
  }
};

var _WebGL_disableCullFace = function (cache) {
  cache.gl.disable(cache.gl.CULL_FACE);
};

var _WebGL_disablePolygonOffset = function (cache) {
  cache.gl.disable(cache.gl.POLYGON_OFFSET_FILL);
};

var _WebGL_disableSampleCoverage = function (cache) {
  cache.gl.disable(cache.gl.SAMPLE_COVERAGE);
};

var _WebGL_disableSampleAlphaToCoverage = function (cache) {
  cache.gl.disable(cache.gl.SAMPLE_ALPHA_TO_COVERAGE);
};

var _WebGL_settings = ['blend', 'depthTest', 'stencilTest', 'scissor', 'colorMask', 'cullFace', 'polygonOffset', 'sampleCoverage', 'sampleAlphaToCoverage'];
var _WebGL_disableFunctions = [_WebGL_disableBlend, _WebGL_disableDepthTest, _WebGL_disableStencilTest, _WebGL_disableScissor, _WebGL_disableColorMask, _WebGL_disableCullFace, _WebGL_disablePolygonOffset, _WebGL_disableSampleCoverage, _WebGL_disableSampleAlphaToCoverage];

function _WebGL_doCompile(gl, src, type) {
  var shader = gl.createShader(type);
  // Enable OES_standard_derivatives extension
  gl.shaderSource(shader, '#extension GL_OES_standard_derivatives : enable\n' + src);
  gl.compileShader(shader);
  return shader;
}

function _WebGL_doLink(gl, vshader, fshader) {
  var program = gl.createProgram();

  gl.attachShader(program, vshader);
  gl.attachShader(program, fshader);
  gl.linkProgram(program);
  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    throw ('Link failed: ' + gl.getProgramInfoLog(program) +
      '\nvs info-log: ' + gl.getShaderInfoLog(vshader) +
      '\nfs info-log: ' + gl.getShaderInfoLog(fshader));
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
 *  @return {Uint32Array} indices
 */
function _WebGL_makeIndexedBuffer(indicesList, indexSize) {
  var indices = new Uint32Array(_WebGL_listLength(indicesList) * indexSize);
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
  var cache = model.f;
  var gl = cache.gl;

  if (!gl) {
    return domNode;
  }

  gl.viewport(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight);

  if (!cache.depthTest.b) {
    gl.depthMask(true);
    cache.depthTest.b = true;
  }
  if (cache.stencilTest.c !== cache.STENCIL_WRITEMASK) {
    gl.stencilMask(cache.STENCIL_WRITEMASK);
    cache.stencilTest.c = cache.STENCIL_WRITEMASK;
  }
  _WebGL_disableScissor(cache);
  _WebGL_disableColorMask(cache);
  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT | gl.STENCIL_BUFFER_BIT);

  function drawEntity(entity) {
    if (!entity.d.b.b) {
      return; // Empty list
    }

    var progid;
    var program;
    var i;

    if (entity.b.id && entity.c.id) {
      progid = _WebGL_getProgID(entity.b.id, entity.c.id);
      program = cache.programs[progid];
    }

    if (!program) {

      var vshader;
      if (entity.b.id) {
        vshader = cache.shaders[entity.b.id];
      } else {
        entity.b.id = _WebGL_guid++;
      }

      if (!vshader) {
        vshader = _WebGL_doCompile(gl, entity.b.src, gl.VERTEX_SHADER);
        cache.shaders[entity.b.id] = vshader;
      }

      var fshader;
      if (entity.c.id) {
        fshader = cache.shaders[entity.c.id];
      } else {
        entity.c.id = _WebGL_guid++;
      }

      if (!fshader) {
        fshader = _WebGL_doCompile(gl, entity.c.src, gl.FRAGMENT_SHADER);
        cache.shaders[entity.c.id] = fshader;
      }

      var glProgram = _WebGL_doLink(gl, vshader, fshader);

      program = {
        glProgram: glProgram,
        attributes: Object.assign({}, entity.b.attributes, entity.c.attributes),
        currentUniforms: {},
        activeAttributes: [],
        activeAttributeLocations: []
      };

      program.uniformSetters = _WebGL_createUniformSetters(
        gl,
        model,
        program,
        Object.assign({}, entity.b.uniforms, entity.c.uniforms)
      );

      var numActiveAttributes = gl.getProgramParameter(glProgram, gl.ACTIVE_ATTRIBUTES);
      for (i = 0; i < numActiveAttributes; i++) {
        var attribute = gl.getActiveAttrib(glProgram, i);
        var attribLocation = gl.getAttribLocation(glProgram, attribute.name);
        program.activeAttributes.push(attribute);
        program.activeAttributeLocations.push(attribLocation);
      }

      progid = _WebGL_getProgID(entity.b.id, entity.c.id);
      cache.programs[progid] = program;
    }

    if (cache.lastProgId !== progid) {
      gl.useProgram(program.glProgram);
      cache.lastProgId = progid;
    }

    _WebGL_setUniforms(program.uniformSetters, entity.e);

    var buffer = cache.buffers.get(entity.d);

    if (!buffer) {
      buffer = _WebGL_doBindSetup(gl, entity.d);
      cache.buffers.set(entity.d, buffer);
    }

    for (i = 0; i < program.activeAttributes.length; i++) {
      attribute = program.activeAttributes[i];
      attribLocation = program.activeAttributeLocations[i];

      if (buffer.buffers[attribute.name] === undefined) {
        buffer.buffers[attribute.name] = _WebGL_doBindAttribute(gl, attribute, entity.d, program.attributes);
      }
      gl.bindBuffer(gl.ARRAY_BUFFER, buffer.buffers[attribute.name]);

      var attributeInfo = _WebGL_getAttributeInfo(gl, attribute.type);
      if (attributeInfo.arraySize === 1) {
        gl.enableVertexAttribArray(attribLocation);
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

    // Apply all the new settings
    cache.toggle = !cache.toggle;
    _WebGL_listEach($elm_explorations$webgl$WebGL$Internal$enableSetting(cache), entity.a);
    // Disable the settings that were applied in the previous draw call
    for (i = 0; i < _WebGL_settings.length; i++) {
      var setting = cache[_WebGL_settings[i]];
      if (setting.toggle !== cache.toggle && setting.enabled) {
        _WebGL_disableFunctions[i](cache);
        setting.enabled = false;
        setting.toggle = cache.toggle;
      }
    }

    if (buffer.indexBuffer) {
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, buffer.indexBuffer);
      gl.drawElements(entity.d.a.mode, buffer.numIndices, gl.UNSIGNED_INT, 0);
    } else {
      gl.drawArrays(entity.d.a.mode, 0, buffer.numIndices);
    }
  }

  _WebGL_listEach(drawEntity, model.g);
  return domNode;
});

function _WebGL_createUniformSetters(gl, model, program, uniformsMap) {
  var glProgram = program.glProgram;
  var currentUniforms = program.currentUniforms;
  var textureCounter = 0;
  var cache = model.f;
  function createUniformSetter(glProgram, uniform) {
    var uniformName = uniform.name;
    var uniformLocation = gl.getUniformLocation(glProgram, uniformName);
    switch (uniform.type) {
      case gl.INT:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniform1i(uniformLocation, value);
            currentUniforms[uniformName] = value;
          }
        };
      case gl.FLOAT:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniform1f(uniformLocation, value);
            currentUniforms[uniformName] = value;
          }
        };
      case gl.FLOAT_VEC2:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniform2f(uniformLocation, value[0], value[1]);
            currentUniforms[uniformName] = value;
          }
        };
      case gl.FLOAT_VEC3:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniform3f(uniformLocation, value[0], value[1], value[2]);
            currentUniforms[uniformName] = value;
          }
        };
      case gl.FLOAT_VEC4:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniform4f(uniformLocation, value[0], value[1], value[2], value[3]);
            currentUniforms[uniformName] = value;
          }
        };
      case gl.FLOAT_MAT4:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniformMatrix4fv(uniformLocation, false, new Float32Array(value));
            currentUniforms[uniformName] = value;
          }
        };
      case gl.SAMPLER_2D:
        var currentTexture = textureCounter++;
        return function (texture) {
          gl.activeTexture(gl.TEXTURE0 + currentTexture);
          var tex = cache.textures.get(texture);
          if (!tex) {
            tex = texture.createTexture(gl);
            cache.textures.set(texture, tex);
          }
          gl.bindTexture(gl.TEXTURE_2D, tex);
          if (currentUniforms[uniformName] !== texture) {
            gl.uniform1i(uniformLocation, currentTexture);
            currentUniforms[uniformName] = texture;
          }
        };
      case gl.BOOL:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniform1i(uniformLocation, value);
            currentUniforms[uniformName] = value;
          }
        };
      default:
        return function () { };
    }
  }

  var uniformSetters = {};
  var numUniforms = gl.getProgramParameter(glProgram, gl.ACTIVE_UNIFORMS);
  for (var i = 0; i < numUniforms; i++) {
    var uniform = gl.getActiveUniform(glProgram, i);
    uniformSetters[uniformsMap[uniform.name] || uniform.name] = createUniformSetter(glProgram, uniform);
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
    return A2($elm_explorations$webgl$WebGL$Internal$enableOption, options, option);
  }, model.h);

  var canvas = _VirtualDom_doc.createElement('canvas');
  var gl = canvas.getContext && (
    canvas.getContext('webgl', options.contextAttributes) ||
    canvas.getContext('experimental-webgl', options.contextAttributes)
  );

  if (gl && typeof WeakMap !== 'undefined') {
    options.sceneSettings.forEach(function (sceneSetting) {
      sceneSetting(gl);
    });

    // Activate extensions
    gl.getExtension('OES_standard_derivatives');
    gl.getExtension('OES_element_index_uint');

    model.f.gl = gl;

    // Cache the current settings in order to diff them to avoid redundant calls
    // https://emscripten.org/docs/optimizing/Optimizing-WebGL.html#avoid-redundant-calls
    model.f.toggle = false; // used to diff the settings from the previous and current draw calls
    model.f.blend = { enabled: false, toggle: false };
    model.f.depthTest = { enabled: false, toggle: false };
    model.f.stencilTest = { enabled: false, toggle: false };
    model.f.scissor = { enabled: false, toggle: false };
    model.f.colorMask = { enabled: false, toggle: false };
    model.f.cullFace = { enabled: false, toggle: false };
    model.f.polygonOffset = { enabled: false, toggle: false };
    model.f.sampleCoverage = { enabled: false, toggle: false };
    model.f.sampleAlphaToCoverage = { enabled: false, toggle: false };

    model.f.shaders = [];
    model.f.programs = {};
    model.f.lastProgId = null;
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
var $author$project$Main$accessAudioOfKind = function (kind) {
	if (kind.$ === 'AudioRoomChange') {
		return function ($) {
			return $.roomChange;
		};
	} else {
		return function ($) {
			return $.music;
		};
	}
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
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
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Basics$add = _Basics_add;
var $ianmackenzie$elm_units$Duration$inSeconds = function (_v0) {
	var numSeconds = _v0.a;
	return numSeconds;
};
var $elm$core$Basics$mul = _Basics_mul;
var $ianmackenzie$elm_units$Duration$inMilliseconds = function (duration) {
	return $ianmackenzie$elm_units$Duration$inSeconds(duration) * 1000;
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$core$Basics$round = _Basics_round;
var $ianmackenzie$elm_units$Duration$addTo = F2(
	function (time, duration) {
		return $elm$time$Time$millisToPosix(
			$elm$time$Time$posixToMillis(time) + $elm$core$Basics$round(
				$ianmackenzie$elm_units$Duration$inMilliseconds(duration)));
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $ianmackenzie$elm_units$Quantity$Quantity = function (a) {
	return {$: 'Quantity', a: a};
};
var $ianmackenzie$elm_units$Quantity$zero = $ianmackenzie$elm_units$Quantity$Quantity(0);
var $MartinSStewart$elm_audio$Audio$audioDefaultConfig = {loop: $elm$core$Maybe$Nothing, playbackRate: 1, startAt: $ianmackenzie$elm_units$Quantity$zero};
var $MartinSStewart$elm_audio$Audio$BasicAudio = function (a) {
	return {$: 'BasicAudio', a: a};
};
var $MartinSStewart$elm_audio$Audio$audioWithConfig = F3(
	function (audioSettings, source, startTime) {
		return $MartinSStewart$elm_audio$Audio$BasicAudio(
			{settings: audioSettings, source: source, startTime: startTime});
	});
var $MartinSStewart$elm_audio$Audio$audio = F2(
	function (source, startTime) {
		return A3($MartinSStewart$elm_audio$Audio$audioWithConfig, $MartinSStewart$elm_audio$Audio$audioDefaultConfig, source, startTime);
	});
var $author$project$Main$AudioMusic = {$: 'AudioMusic'};
var $author$project$Main$AudioRoomChange = {$: 'AudioRoomChange'};
var $author$project$Main$audioKinds = _List_fromArray(
	[$author$project$Main$AudioRoomChange, $author$project$Main$AudioMusic]);
var $elm$core$Basics$floor = _Basics_floor;
var $ianmackenzie$elm_units$Duration$seconds = function (numSeconds) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numSeconds);
};
var $ianmackenzie$elm_units$Duration$milliseconds = function (numMilliseconds) {
	return $ianmackenzie$elm_units$Duration$seconds(0.001 * numMilliseconds);
};
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $ianmackenzie$elm_units$Duration$from = F2(
	function (startTime, endTime) {
		var numMilliseconds = $elm$time$Time$posixToMillis(endTime) - $elm$time$Time$posixToMillis(startTime);
		return $ianmackenzie$elm_units$Duration$milliseconds(numMilliseconds);
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $MartinSStewart$elm_audio$Audio$audioSourceBufferId = function (_v0) {
	var audioSource = _v0.a;
	return audioSource.bufferId;
};
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $MartinSStewart$elm_audio$Audio$rawBufferId = function (_v0) {
	var bufferId = _v0.a;
	return bufferId;
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $MartinSStewart$elm_audio$Audio$length = F2(
	function (_v0, source) {
		var audioData_ = _v0.a;
		return A2(
			$elm$core$Maybe$withDefault,
			$ianmackenzie$elm_units$Quantity$zero,
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.duration;
				},
				A2(
					$elm$core$Dict$get,
					$MartinSStewart$elm_audio$Audio$rawBufferId(
						$MartinSStewart$elm_audio$Audio$audioSourceBufferId(source)),
					audioData_.sourceData)));
	});
var $ianmackenzie$elm_units$Quantity$multiplyBy = F2(
	function (scale, _v0) {
		var value = _v0.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(scale * value);
	});
var $author$project$Main$audioLoop = function (_v0) {
	var audioData = _v0.audioData;
	var initialTime = _v0.initialTime;
	var lastTick = _v0.lastTick;
	return function (audio_) {
		var audioLength = A2($MartinSStewart$elm_audio$Audio$length, audioData, audio_);
		var alreadyCompletedLoops = ($elm$core$Basics$floor(
			$ianmackenzie$elm_units$Duration$inMilliseconds(
				A2($ianmackenzie$elm_units$Duration$from, initialTime, lastTick))) / $elm$core$Basics$floor(
			$ianmackenzie$elm_units$Duration$inMilliseconds(audioLength))) | 0;
		var startTime = A2(
			$ianmackenzie$elm_units$Duration$addTo,
			initialTime,
			A2($ianmackenzie$elm_units$Quantity$multiplyBy, alreadyCompletedLoops, audioLength));
		return A2($MartinSStewart$elm_audio$Audio$audio, audio_, startTime);
	};
};
var $MartinSStewart$elm_audio$Audio$Group = function (a) {
	return {$: 'Group', a: a};
};
var $MartinSStewart$elm_audio$Audio$group = function (audios) {
	return $MartinSStewart$elm_audio$Audio$Group(audios);
};
var $MartinSStewart$elm_audio$Audio$silence = $MartinSStewart$elm_audio$Audio$group(_List_Nil);
var $author$project$Main$audioWith = F2(
	function (source, _with) {
		if (source.$ === 'Err') {
			return $MartinSStewart$elm_audio$Audio$silence;
		} else {
			var loadedAudio = source.a;
			return _with(loadedAudio);
		}
	});
var $elm$core$List$foldl = F3(
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
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$List$foldrHelper = F4(
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
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
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
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $author$project$Main$audio = function (audioData) {
	return function (state) {
		return $MartinSStewart$elm_audio$Audio$group(
			A2(
				$elm$core$List$cons,
				A2(
					$author$project$Main$audioWith,
					state.audio.music,
					function (music) {
						return A2(
							$author$project$Main$audioLoop,
							{audioData: audioData, initialTime: state.initialTime, lastTick: state.lastTick},
							music);
					}),
				A2(
					$elm$core$List$map,
					function (audioKind) {
						return A2(
							$author$project$Main$audioWith,
							A2($author$project$Main$accessAudioOfKind, audioKind, state.audio),
							function (loadedAudio) {
								return $MartinSStewart$elm_audio$Audio$group(
									A2(
										$elm$core$List$map,
										function (time) {
											return A2(
												$MartinSStewart$elm_audio$Audio$audio,
												loadedAudio,
												A2(
													$ianmackenzie$elm_units$Duration$addTo,
													time,
													$ianmackenzie$elm_units$Duration$seconds(0.07)));
										},
										A2($author$project$Main$accessAudioOfKind, audioKind, state.audioTimes)));
							});
					},
					$author$project$Main$audioKinds)));
	};
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
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
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
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
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
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
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
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
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Main$audioPortFromJS = _Platform_incomingPort('audioPortFromJS', $elm$json$Json$Decode$value);
var $author$project$Main$audioPortToJS = _Platform_outgoingPort('audioPortToJS', $elm$core$Basics$identity);
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $MartinSStewart$elm_audio$Audio$UserMsg = function (a) {
	return {$: 'UserMsg', a: a};
};
var $MartinSStewart$elm_audio$Audio$AudioData = function (a) {
	return {$: 'AudioData', a: a};
};
var $MartinSStewart$elm_audio$Audio$audioData = function (_v0) {
	var model = _v0.a;
	return $MartinSStewart$elm_audio$Audio$AudioData(
		{sourceData: model.sourceData});
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
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
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$document = _Browser_document;
var $MartinSStewart$elm_audio$Audio$getUserModel = function (_v0) {
	var model = _v0.a;
	return model.userModel;
};
var $MartinSStewart$elm_audio$Audio$Model = function (a) {
	return {$: 'Model', a: a};
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $MartinSStewart$elm_audio$Audio$audioStartTime = function (audio_) {
	return A2($ianmackenzie$elm_units$Duration$addTo, audio_.startTime, audio_.offset);
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $MartinSStewart$elm_audio$Audio$encodeBufferId = function (_v0) {
	var bufferId = _v0.a;
	return $elm$json$Json$Encode$int(bufferId);
};
var $elm$json$Json$Encode$float = _Json_wrap;
var $MartinSStewart$elm_audio$Audio$encodeDuration = A2($elm$core$Basics$composeR, $ianmackenzie$elm_units$Duration$inMilliseconds, $elm$json$Json$Encode$float);
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $MartinSStewart$elm_audio$Audio$encodeLoopConfig = function (maybeLoop) {
	if (maybeLoop.$ === 'Just') {
		var loop = maybeLoop.a;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'loopStart',
					$MartinSStewart$elm_audio$Audio$encodeDuration(loop.loopStart)),
					_Utils_Tuple2(
					'loopEnd',
					$MartinSStewart$elm_audio$Audio$encodeDuration(loop.loopEnd))
				]));
	} else {
		return $elm$json$Json$Encode$null;
	}
};
var $MartinSStewart$elm_audio$Audio$encodeTime = A2($elm$core$Basics$composeR, $elm$time$Time$posixToMillis, $elm$json$Json$Encode$int);
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $mgold$elm_nonempty_list$List$Nonempty$toList = function (_v0) {
	var x = _v0.a;
	var xs = _v0.b;
	return A2($elm$core$List$cons, x, xs);
};
var $MartinSStewart$elm_audio$Audio$encodeVolumeTimeline = function (volumeTimeline) {
	return A2(
		$elm$json$Json$Encode$list,
		function (_v0) {
			var time = _v0.a;
			var volume = _v0.b;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'time',
						$MartinSStewart$elm_audio$Audio$encodeTime(time)),
						_Utils_Tuple2(
						'volume',
						$elm$json$Json$Encode$float(volume))
					]));
		},
		$mgold$elm_nonempty_list$List$Nonempty$toList(volumeTimeline));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $mgold$elm_nonempty_list$List$Nonempty$Nonempty = F2(
	function (a, b) {
		return {$: 'Nonempty', a: a, b: b};
	});
var $mgold$elm_nonempty_list$List$Nonempty$map = F2(
	function (f, _v0) {
		var x = _v0.a;
		var xs = _v0.b;
		return A2(
			$mgold$elm_nonempty_list$List$Nonempty$Nonempty,
			f(x),
			A2($elm$core$List$map, f, xs));
	});
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $MartinSStewart$elm_audio$Audio$volumeTimelines = function (audio_) {
	return A2(
		$elm$core$List$map,
		$mgold$elm_nonempty_list$List$Nonempty$map(
			$elm$core$Tuple$mapFirst(
				function (a) {
					return A2($ianmackenzie$elm_units$Duration$addTo, a, audio_.offset);
				})),
		audio_.volumeTimelines);
};
var $MartinSStewart$elm_audio$Audio$encodeStartSound = F2(
	function (nodeGroupId, audio_) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('startSound')),
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'bufferId',
					$MartinSStewart$elm_audio$Audio$encodeBufferId(
						$MartinSStewart$elm_audio$Audio$audioSourceBufferId(audio_.source))),
					_Utils_Tuple2(
					'startTime',
					$MartinSStewart$elm_audio$Audio$encodeTime(
						$MartinSStewart$elm_audio$Audio$audioStartTime(audio_))),
					_Utils_Tuple2(
					'startAt',
					$MartinSStewart$elm_audio$Audio$encodeDuration(audio_.startAt)),
					_Utils_Tuple2(
					'volume',
					$elm$json$Json$Encode$float(audio_.volume)),
					_Utils_Tuple2(
					'volumeTimelines',
					A2(
						$elm$json$Json$Encode$list,
						$MartinSStewart$elm_audio$Audio$encodeVolumeTimeline,
						$MartinSStewart$elm_audio$Audio$volumeTimelines(audio_))),
					_Utils_Tuple2(
					'loop',
					$MartinSStewart$elm_audio$Audio$encodeLoopConfig(audio_.loop)),
					_Utils_Tuple2(
					'playbackRate',
					$elm$json$Json$Encode$float(audio_.playbackRate))
				]));
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $ianmackenzie$elm_units$Quantity$plus = F2(
	function (_v0, _v1) {
		var y = _v0.a;
		var x = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(x + y);
	});
var $MartinSStewart$elm_audio$Audio$flattenAudio = function (audio_) {
	switch (audio_.$) {
		case 'Group':
			var group_ = audio_.a;
			return $elm$core$List$concat(
				A2($elm$core$List$map, $MartinSStewart$elm_audio$Audio$flattenAudio, group_));
		case 'BasicAudio':
			var source = audio_.a.source;
			var startTime = audio_.a.startTime;
			var settings = audio_.a.settings;
			return _List_fromArray(
				[
					{loop: settings.loop, offset: $ianmackenzie$elm_units$Quantity$zero, playbackRate: settings.playbackRate, source: source, startAt: settings.startAt, startTime: startTime, volume: 1, volumeTimelines: _List_Nil}
				]);
		default:
			var effect = audio_.a;
			var _v1 = effect.effectType;
			switch (_v1.$) {
				case 'ScaleVolume':
					var scaleVolume_ = _v1.a;
					return A2(
						$elm$core$List$map,
						function (a) {
							return _Utils_update(
								a,
								{volume: scaleVolume_.scaleBy * a.volume});
						},
						$MartinSStewart$elm_audio$Audio$flattenAudio(effect.audio));
				case 'ScaleVolumeAt':
					var volumeAt = _v1.a.volumeAt;
					return A2(
						$elm$core$List$map,
						function (a) {
							return _Utils_update(
								a,
								{
									volumeTimelines: A2($elm$core$List$cons, volumeAt, a.volumeTimelines)
								});
						},
						$MartinSStewart$elm_audio$Audio$flattenAudio(effect.audio));
				default:
					var duration = _v1.a;
					return A2(
						$elm$core$List$map,
						function (a) {
							return _Utils_update(
								a,
								{
									offset: A2($ianmackenzie$elm_units$Quantity$plus, duration, a.offset)
								});
						},
						$MartinSStewart$elm_audio$Audio$flattenAudio(effect.audio));
			}
	}
};
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $MartinSStewart$elm_audio$Audio$encodeSetLoopConfig = F2(
	function (nodeGroupId, loop) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('setLoopConfig')),
					_Utils_Tuple2(
					'loop',
					$MartinSStewart$elm_audio$Audio$encodeLoopConfig(loop))
				]));
	});
var $MartinSStewart$elm_audio$Audio$encodeSetPlaybackRate = F2(
	function (nodeGroupId, playbackRate) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('setPlaybackRate')),
					_Utils_Tuple2(
					'playbackRate',
					$elm$json$Json$Encode$float(playbackRate))
				]));
	});
var $MartinSStewart$elm_audio$Audio$encodeSetVolume = F2(
	function (nodeGroupId, volume) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('setVolume')),
					_Utils_Tuple2(
					'volume',
					$elm$json$Json$Encode$float(volume))
				]));
	});
var $MartinSStewart$elm_audio$Audio$encodeSetVolumeAt = F2(
	function (nodeGroupId, volumeTimelines_) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('setVolumeAt')),
					_Utils_Tuple2(
					'volumeAt',
					A2($elm$json$Json$Encode$list, $MartinSStewart$elm_audio$Audio$encodeVolumeTimeline, volumeTimelines_))
				]));
	});
var $MartinSStewart$elm_audio$Audio$encodeStopSound = function (nodeGroupId) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'action',
				$elm$json$Json$Encode$string('stopSound')),
				_Utils_Tuple2(
				'nodeGroupId',
				$elm$json$Json$Encode$int(nodeGroupId))
			]));
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $MartinSStewart$elm_audio$Audio$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var first = list.a;
				var rest = list.b;
				if (predicate(first)) {
					return $elm$core$Maybe$Just(first);
				} else {
					var $temp$predicate = predicate,
						$temp$list = rest;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$List$drop = F2(
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
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$takeReverse = F3(
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
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $MartinSStewart$elm_audio$Audio$removeAt = F2(
	function (index, l) {
		if (index < 0) {
			return l;
		} else {
			var tail = $elm$core$List$tail(
				A2($elm$core$List$drop, index, l));
			var head = A2($elm$core$List$take, index, l);
			if (tail.$ === 'Nothing') {
				return l;
			} else {
				var t = tail.a;
				return A2($elm$core$List$append, head, t);
			}
		}
	});
var $MartinSStewart$elm_audio$Audio$updateAudioState = F2(
	function (_v0, _v1) {
		var nodeGroupId = _v0.a;
		var audioGroup = _v0.b;
		var flattenedAudio = _v1.a;
		var audioState = _v1.b;
		var json = _v1.c;
		var validAudio = A2(
			$elm$core$List$filter,
			function (_v7) {
				var a = _v7.b;
				return _Utils_eq(a.source, audioGroup.source) && (_Utils_eq(
					$MartinSStewart$elm_audio$Audio$audioStartTime(a),
					$MartinSStewart$elm_audio$Audio$audioStartTime(audioGroup)) && _Utils_eq(a.startAt, audioGroup.startAt));
			},
			A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, flattenedAudio));
		var _v2 = A2(
			$MartinSStewart$elm_audio$Audio$find,
			function (_v3) {
				var a = _v3.b;
				return _Utils_eq(a, audioGroup);
			},
			validAudio);
		if (_v2.$ === 'Just') {
			var _v4 = _v2.a;
			var index = _v4.a;
			return _Utils_Tuple3(
				A2($MartinSStewart$elm_audio$Audio$removeAt, index, flattenedAudio),
				audioState,
				json);
		} else {
			if (validAudio.b) {
				var _v6 = validAudio.a;
				var index = _v6.a;
				var a = _v6.b;
				var encodeValue = F2(
					function (getter, encoder) {
						return _Utils_eq(
							getter(audioGroup),
							getter(a)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
							A2(
								encoder,
								nodeGroupId,
								getter(a)));
					});
				var effects = A2(
					$elm$core$List$filterMap,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							A2(
							encodeValue,
							function ($) {
								return $.volume;
							},
							$MartinSStewart$elm_audio$Audio$encodeSetVolume),
							A2(
							encodeValue,
							function ($) {
								return $.loop;
							},
							$MartinSStewart$elm_audio$Audio$encodeSetLoopConfig),
							A2(
							encodeValue,
							function ($) {
								return $.playbackRate;
							},
							$MartinSStewart$elm_audio$Audio$encodeSetPlaybackRate),
							A2(encodeValue, $MartinSStewart$elm_audio$Audio$volumeTimelines, $MartinSStewart$elm_audio$Audio$encodeSetVolumeAt)
						]));
				return _Utils_Tuple3(
					A2($MartinSStewart$elm_audio$Audio$removeAt, index, flattenedAudio),
					A3($elm$core$Dict$insert, nodeGroupId, a, audioState),
					_Utils_ap(effects, json));
			} else {
				return _Utils_Tuple3(
					flattenedAudio,
					A2($elm$core$Dict$remove, nodeGroupId, audioState),
					A2(
						$elm$core$List$cons,
						$MartinSStewart$elm_audio$Audio$encodeStopSound(nodeGroupId),
						json));
			}
		}
	});
var $MartinSStewart$elm_audio$Audio$diffAudioState = F3(
	function (nodeGroupIdCounter, audioState, newAudio) {
		var _v0 = A3(
			$elm$core$List$foldl,
			$MartinSStewart$elm_audio$Audio$updateAudioState,
			_Utils_Tuple3(
				$MartinSStewart$elm_audio$Audio$flattenAudio(newAudio),
				audioState,
				_List_Nil),
			$elm$core$Dict$toList(audioState));
		var newAudioLeft = _v0.a;
		var newAudioState = _v0.b;
		var json2 = _v0.c;
		var _v1 = A3(
			$elm$core$List$foldl,
			F2(
				function (audioLeft, _v2) {
					var counter = _v2.a;
					var audioState_ = _v2.b;
					var json_ = _v2.c;
					return _Utils_Tuple3(
						counter + 1,
						A3($elm$core$Dict$insert, counter, audioLeft, audioState_),
						A2(
							$elm$core$List$cons,
							A2($MartinSStewart$elm_audio$Audio$encodeStartSound, counter, audioLeft),
							json_));
				}),
			_Utils_Tuple3(nodeGroupIdCounter, newAudioState, json2),
			newAudioLeft);
		var newNodeGroupIdCounter = _v1.a;
		var newAudioState2 = _v1.b;
		var json3 = _v1.c;
		return _Utils_Tuple3(newAudioState2, newNodeGroupIdCounter, json3);
	});
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $MartinSStewart$elm_audio$Audio$encodeAudioLoadRequest = F2(
	function (index, audioLoad) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'audioUrl',
					$elm$json$Json$Encode$string(audioLoad.audioUrl)),
					_Utils_Tuple2(
					'requestId',
					$elm$json$Json$Encode$int(index))
				]));
	});
var $MartinSStewart$elm_audio$Audio$flattenAudioCmd = function (audioCmd) {
	if (audioCmd.$ === 'AudioLoadRequest') {
		var data = audioCmd.a;
		return _List_fromArray(
			[data]);
	} else {
		var list = audioCmd.a;
		return $elm$core$List$concat(
			A2($elm$core$List$map, $MartinSStewart$elm_audio$Audio$flattenAudioCmd, list));
	}
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Dict$foldl = F3(
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
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $MartinSStewart$elm_audio$Audio$encodeAudioCmd = F2(
	function (_v0, audioCmd) {
		var model = _v0.a;
		var flattenedAudioCmd = $MartinSStewart$elm_audio$Audio$flattenAudioCmd(audioCmd);
		var newPendingRequests = A2(
			$elm$core$List$indexedMap,
			F2(
				function (index, request) {
					return _Utils_Tuple2(model.requestCount + index, request);
				}),
			flattenedAudioCmd);
		return _Utils_Tuple2(
			$MartinSStewart$elm_audio$Audio$Model(
				_Utils_update(
					model,
					{
						pendingRequests: A2(
							$elm$core$Dict$union,
							model.pendingRequests,
							$elm$core$Dict$fromList(newPendingRequests)),
						requestCount: model.requestCount + $elm$core$List$length(flattenedAudioCmd)
					})),
			A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				A2(
					$elm$core$List$map,
					function (_v1) {
						var index = _v1.a;
						var value = _v1.b;
						return A2($MartinSStewart$elm_audio$Audio$encodeAudioLoadRequest, index, value);
					},
					newPendingRequests)));
	});
var $elm$core$Platform$Cmd$map = _Platform_map;
var $MartinSStewart$elm_audio$Audio$initHelper = F3(
	function (audioPort, audioFunc, _v0) {
		var model = _v0.a;
		var cmds = _v0.b;
		var audioCmds = _v0.c;
		var _v1 = A3(
			$MartinSStewart$elm_audio$Audio$diffAudioState,
			0,
			$elm$core$Dict$empty,
			A2(
				audioFunc,
				$MartinSStewart$elm_audio$Audio$AudioData(
					{sourceData: $elm$core$Dict$empty}),
				model));
		var audioState = _v1.a;
		var newNodeGroupIdCounter = _v1.b;
		var json = _v1.c;
		var initialModel = $MartinSStewart$elm_audio$Audio$Model(
			{audioState: audioState, nodeGroupIdCounter: newNodeGroupIdCounter, pendingRequests: $elm$core$Dict$empty, requestCount: 0, samplesPerSecond: $elm$core$Maybe$Nothing, sourceData: $elm$core$Dict$empty, userModel: model});
		var _v2 = A2($MartinSStewart$elm_audio$Audio$encodeAudioCmd, initialModel, audioCmds);
		var initialModel2 = _v2.a;
		var audioRequests = _v2.b;
		var portMessage = $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'audio',
					A2($elm$json$Json$Encode$list, $elm$core$Basics$identity, json)),
					_Utils_Tuple2('audioCmds', audioRequests)
				]));
		return _Utils_Tuple2(
			initialModel2,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						A2($elm$core$Platform$Cmd$map, $MartinSStewart$elm_audio$Audio$UserMsg, cmds),
						audioPort(portMessage)
					])));
	});
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $MartinSStewart$elm_audio$Audio$FromJSMsg = function (a) {
	return {$: 'FromJSMsg', a: a};
};
var $MartinSStewart$elm_audio$Audio$JsonParseError = function (a) {
	return {$: 'JsonParseError', a: a};
};
var $MartinSStewart$elm_audio$Audio$AudioLoadFailed = function (a) {
	return {$: 'AudioLoadFailed', a: a};
};
var $MartinSStewart$elm_audio$Audio$AudioLoadSuccess = function (a) {
	return {$: 'AudioLoadSuccess', a: a};
};
var $MartinSStewart$elm_audio$Audio$InitAudioContext = function (a) {
	return {$: 'InitAudioContext', a: a};
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $MartinSStewart$elm_audio$Audio$BufferId = function (a) {
	return {$: 'BufferId', a: a};
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $MartinSStewart$elm_audio$Audio$decodeBufferId = A2($elm$json$Json$Decode$map, $MartinSStewart$elm_audio$Audio$BufferId, $elm$json$Json$Decode$int);
var $MartinSStewart$elm_audio$Audio$FailedToDecode = {$: 'FailedToDecode'};
var $MartinSStewart$elm_audio$Audio$NetworkError = {$: 'NetworkError'};
var $MartinSStewart$elm_audio$Audio$UnknownError = {$: 'UnknownError'};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $MartinSStewart$elm_audio$Audio$decodeLoadError = A2(
	$elm$json$Json$Decode$andThen,
	function (value) {
		switch (value) {
			case 'NetworkError':
				return $elm$json$Json$Decode$succeed($MartinSStewart$elm_audio$Audio$NetworkError);
			case 'MediaDecodeAudioDataUnknownContentType':
				return $elm$json$Json$Decode$succeed($MartinSStewart$elm_audio$Audio$FailedToDecode);
			case 'DOMException: The buffer passed to decodeAudioData contains an unknown content type.':
				return $elm$json$Json$Decode$succeed($MartinSStewart$elm_audio$Audio$FailedToDecode);
			default:
				return $elm$json$Json$Decode$succeed($MartinSStewart$elm_audio$Audio$UnknownError);
		}
	},
	$elm$json$Json$Decode$string);
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $MartinSStewart$elm_audio$Audio$decodeFromJSMsg = A2(
	$elm$json$Json$Decode$andThen,
	function (value) {
		switch (value) {
			case 0:
				return A3(
					$elm$json$Json$Decode$map2,
					F2(
						function (requestId, error) {
							return $MartinSStewart$elm_audio$Audio$AudioLoadFailed(
								{error: error, requestId: requestId});
						}),
					A2($elm$json$Json$Decode$field, 'requestId', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'error', $MartinSStewart$elm_audio$Audio$decodeLoadError));
			case 1:
				return A4(
					$elm$json$Json$Decode$map3,
					F3(
						function (requestId, bufferId, duration) {
							return $MartinSStewart$elm_audio$Audio$AudioLoadSuccess(
								{
									bufferId: bufferId,
									duration: $ianmackenzie$elm_units$Duration$seconds(duration),
									requestId: requestId
								});
						}),
					A2($elm$json$Json$Decode$field, 'requestId', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'bufferId', $MartinSStewart$elm_audio$Audio$decodeBufferId),
					A2($elm$json$Json$Decode$field, 'durationInSeconds', $elm$json$Json$Decode$float));
			case 2:
				return A2(
					$elm$json$Json$Decode$map,
					function (samplesPerSecond) {
						return $MartinSStewart$elm_audio$Audio$InitAudioContext(
							{samplesPerSecond: samplesPerSecond});
					},
					A2($elm$json$Json$Decode$field, 'samplesPerSecond', $elm$json$Json$Decode$int));
			default:
				return $elm$json$Json$Decode$succeed(
					$MartinSStewart$elm_audio$Audio$JsonParseError(
						{
							error: 'Type ' + ($elm$core$String$fromInt(value) + ' not handled.')
						}));
		}
	},
	A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$int));
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $MartinSStewart$elm_audio$Audio$fromJSPortSub = function (json) {
	var _v0 = A2($elm$json$Json$Decode$decodeValue, $MartinSStewart$elm_audio$Audio$decodeFromJSMsg, json);
	if (_v0.$ === 'Ok') {
		var value = _v0.a;
		return $MartinSStewart$elm_audio$Audio$FromJSMsg(value);
	} else {
		var error = _v0.a;
		return $MartinSStewart$elm_audio$Audio$FromJSMsg(
			$MartinSStewart$elm_audio$Audio$JsonParseError(
				{
					error: $elm$json$Json$Decode$errorToString(error)
				}));
	}
};
var $elm$core$Platform$Sub$map = _Platform_map;
var $MartinSStewart$elm_audio$Audio$subscriptions = F2(
	function (app, _v0) {
		var model = _v0.a;
		return $elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					A2(
					$elm$core$Platform$Sub$map,
					$MartinSStewart$elm_audio$Audio$UserMsg,
					A2(
						app.subscriptions,
						$MartinSStewart$elm_audio$Audio$audioData(
							$MartinSStewart$elm_audio$Audio$Model(model)),
						model.userModel)),
					app.audioPort.fromJS($MartinSStewart$elm_audio$Audio$fromJSPortSub)
				]));
	});
var $MartinSStewart$elm_audio$Audio$File = function (a) {
	return {$: 'File', a: a};
};
var $MartinSStewart$elm_audio$Audio$flip = F3(
	function (func, a, b) {
		return A2(func, b, a);
	});
var $mgold$elm_nonempty_list$List$Nonempty$head = function (_v0) {
	var x = _v0.a;
	var xs = _v0.b;
	return x;
};
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $MartinSStewart$elm_audio$Audio$updateHelper = F4(
	function (audioPort, audioFunc, userUpdate, _v0) {
		var model = _v0.a;
		var audioData_ = $MartinSStewart$elm_audio$Audio$audioData(
			$MartinSStewart$elm_audio$Audio$Model(model));
		var _v1 = A2(userUpdate, audioData_, model.userModel);
		var newUserModel = _v1.a;
		var userCmd = _v1.b;
		var audioCmds = _v1.c;
		var _v2 = A3(
			$MartinSStewart$elm_audio$Audio$diffAudioState,
			model.nodeGroupIdCounter,
			model.audioState,
			A2(audioFunc, audioData_, newUserModel));
		var audioState = _v2.a;
		var newNodeGroupIdCounter = _v2.b;
		var json = _v2.c;
		var newModel = $MartinSStewart$elm_audio$Audio$Model(
			_Utils_update(
				model,
				{audioState: audioState, nodeGroupIdCounter: newNodeGroupIdCounter, userModel: newUserModel}));
		var _v3 = A2($MartinSStewart$elm_audio$Audio$encodeAudioCmd, newModel, audioCmds);
		var newModel2 = _v3.a;
		var audioRequests = _v3.b;
		var portMessage = $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'audio',
					A2($elm$json$Json$Encode$list, $elm$core$Basics$identity, json)),
					_Utils_Tuple2('audioCmds', audioRequests)
				]));
		return _Utils_Tuple2(
			newModel2,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						A2($elm$core$Platform$Cmd$map, $MartinSStewart$elm_audio$Audio$UserMsg, userCmd),
						audioPort(portMessage)
					])));
	});
var $MartinSStewart$elm_audio$Audio$update = F3(
	function (app, msg, _v0) {
		var model = _v0.a;
		if (msg.$ === 'UserMsg') {
			var userMsg = msg.a;
			return A4(
				$MartinSStewart$elm_audio$Audio$updateHelper,
				app.audioPort.toJS,
				app.audio,
				A2($MartinSStewart$elm_audio$Audio$flip, app.update, userMsg),
				$MartinSStewart$elm_audio$Audio$Model(model));
		} else {
			var response = msg.a;
			switch (response.$) {
				case 'AudioLoadSuccess':
					var requestId = response.a.requestId;
					var bufferId = response.a.bufferId;
					var duration = response.a.duration;
					var _v3 = A2($elm$core$Dict$get, requestId, model.pendingRequests);
					if (_v3.$ === 'Just') {
						var pendingRequest = _v3.a;
						var sourceData = A3(
							$elm$core$Dict$insert,
							$MartinSStewart$elm_audio$Audio$rawBufferId(bufferId),
							{duration: duration},
							model.sourceData);
						var source = $elm$core$Result$Ok(
							$MartinSStewart$elm_audio$Audio$File(
								{bufferId: bufferId}));
						var maybeUserMsg = A2(
							$MartinSStewart$elm_audio$Audio$find,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$Tuple$first,
								$elm$core$Basics$eq(source)),
							$mgold$elm_nonempty_list$List$Nonempty$toList(pendingRequest.userMsg));
						if (maybeUserMsg.$ === 'Just') {
							var _v5 = maybeUserMsg.a;
							var userMsg = _v5.b;
							return A4(
								$MartinSStewart$elm_audio$Audio$updateHelper,
								app.audioPort.toJS,
								app.audio,
								A2($MartinSStewart$elm_audio$Audio$flip, app.update, userMsg),
								$MartinSStewart$elm_audio$Audio$Model(
									_Utils_update(
										model,
										{
											pendingRequests: A2($elm$core$Dict$remove, requestId, model.pendingRequests),
											sourceData: sourceData
										})));
						} else {
							return A4(
								$MartinSStewart$elm_audio$Audio$updateHelper,
								app.audioPort.toJS,
								app.audio,
								A2(
									$MartinSStewart$elm_audio$Audio$flip,
									app.update,
									$mgold$elm_nonempty_list$List$Nonempty$head(pendingRequest.userMsg).b),
								$MartinSStewart$elm_audio$Audio$Model(
									_Utils_update(
										model,
										{
											pendingRequests: A2($elm$core$Dict$remove, requestId, model.pendingRequests),
											sourceData: sourceData
										})));
						}
					} else {
						return _Utils_Tuple2(
							$MartinSStewart$elm_audio$Audio$Model(model),
							$elm$core$Platform$Cmd$none);
					}
				case 'AudioLoadFailed':
					var requestId = response.a.requestId;
					var error = response.a.error;
					var _v6 = A2($elm$core$Dict$get, requestId, model.pendingRequests);
					if (_v6.$ === 'Just') {
						var pendingRequest = _v6.a;
						var a = $elm$core$Result$Err(error);
						var b = A2(
							$MartinSStewart$elm_audio$Audio$find,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$Tuple$first,
								$elm$core$Basics$eq(a)),
							$mgold$elm_nonempty_list$List$Nonempty$toList(pendingRequest.userMsg));
						if (b.$ === 'Just') {
							var _v8 = b.a;
							var userMsg = _v8.b;
							return A4(
								$MartinSStewart$elm_audio$Audio$updateHelper,
								app.audioPort.toJS,
								app.audio,
								A2($MartinSStewart$elm_audio$Audio$flip, app.update, userMsg),
								$MartinSStewart$elm_audio$Audio$Model(
									_Utils_update(
										model,
										{
											pendingRequests: A2($elm$core$Dict$remove, requestId, model.pendingRequests)
										})));
						} else {
							return A4(
								$MartinSStewart$elm_audio$Audio$updateHelper,
								app.audioPort.toJS,
								app.audio,
								A2(
									$MartinSStewart$elm_audio$Audio$flip,
									app.update,
									$mgold$elm_nonempty_list$List$Nonempty$head(pendingRequest.userMsg).b),
								$MartinSStewart$elm_audio$Audio$Model(
									_Utils_update(
										model,
										{
											pendingRequests: A2($elm$core$Dict$remove, requestId, model.pendingRequests)
										})));
						}
					} else {
						return _Utils_Tuple2(
							$MartinSStewart$elm_audio$Audio$Model(model),
							$elm$core$Platform$Cmd$none);
					}
				case 'InitAudioContext':
					var samplesPerSecond = response.a.samplesPerSecond;
					return _Utils_Tuple2(
						$MartinSStewart$elm_audio$Audio$Model(
							_Utils_update(
								model,
								{
									samplesPerSecond: $elm$core$Maybe$Just(samplesPerSecond)
								})),
						$elm$core$Platform$Cmd$none);
				default:
					var error = response.a.error;
					return _Utils_Tuple2(
						$MartinSStewart$elm_audio$Audio$Model(model),
						$elm$core$Platform$Cmd$none);
			}
		}
	});
var $MartinSStewart$elm_audio$Audio$Effect = function (a) {
	return {$: 'Effect', a: a};
};
var $MartinSStewart$elm_audio$Audio$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $MartinSStewart$elm_audio$Audio$offsetBy = F2(
	function (offset_, audio_) {
		return $MartinSStewart$elm_audio$Audio$Effect(
			{
				audio: audio_,
				effectType: $MartinSStewart$elm_audio$Audio$Offset(offset_)
			});
	});
var $MartinSStewart$elm_audio$Audio$withAudioOffset = function (app) {
	return _Utils_update(
		app,
		{
			audio: F2(
				function (audioData_, model) {
					return A2(
						$MartinSStewart$elm_audio$Audio$offsetBy,
						$ianmackenzie$elm_units$Duration$milliseconds(50),
						A2(app.audio, audioData_, model));
				})
		});
};
var $MartinSStewart$elm_audio$Audio$documentWithAudio = A2(
	$elm$core$Basics$composeR,
	$MartinSStewart$elm_audio$Audio$withAudioOffset,
	function (app) {
		return $elm$browser$Browser$document(
			{
				init: A2(
					$elm$core$Basics$composeR,
					app.init,
					A2($MartinSStewart$elm_audio$Audio$initHelper, app.audioPort.toJS, app.audio)),
				subscriptions: $MartinSStewart$elm_audio$Audio$subscriptions(app),
				update: $MartinSStewart$elm_audio$Audio$update(app),
				view: function (model) {
					var _v0 = A2(
						app.view,
						$MartinSStewart$elm_audio$Audio$audioData(model),
						$MartinSStewart$elm_audio$Audio$getUserModel(model));
					var title = _v0.title;
					var body = _v0.body;
					return {
						body: A2(
							$elm$core$List$map,
							$elm$html$Html$map($MartinSStewart$elm_audio$Audio$UserMsg),
							body),
						title: title
					};
				}
			});
	});
var $author$project$Main$GameRequestInitialWindowSize = {$: 'GameRequestInitialWindowSize'};
var $author$project$Main$LoadAudio = function (a) {
	return {$: 'LoadAudio', a: a};
};
var $author$project$Main$PlayerLeavingTrail = function (a) {
	return {$: 'PlayerLeavingTrail', a: a};
};
var $author$project$Main$RequestInitialRandomSeed = {$: 'RequestInitialRandomSeed'};
var $author$project$Main$RequestInitialTime = {$: 'RequestInitialTime'};
var $author$project$Main$cameraHeight = 1.05;
var $elm$core$Basics$pi = _Basics_pi;
var $ianmackenzie$elm_units$Angle$radians = function (numRadians) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numRadians);
};
var $ianmackenzie$elm_units$Angle$degrees = function (numDegrees) {
	return $ianmackenzie$elm_units$Angle$radians($elm$core$Basics$pi * (numDegrees / 180));
};
var $author$project$Main$eachAudio = function (perKind) {
	return {music: perKind, roomChange: perKind};
};
var $author$project$Reaction$effects = function ($) {
	return $.effects;
};
var $author$project$Reaction$state = function ($) {
	return $.state;
};
var $author$project$Reaction$effectsAdd = function (effectsAdditional) {
	return function (reaction) {
		return {
			effects: _Utils_ap(
				$author$project$Reaction$effects(reaction),
				effectsAdditional),
			state: $author$project$Reaction$state(reaction)
		};
	};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $author$project$Main$Floor = {$: 'Floor'};
var $author$project$Main$Player = {$: 'Player'};
var $author$project$Main$PlayerPast = {$: 'PlayerPast'};
var $w0rm$elm_physics$Internal$World$Protected = function (a) {
	return {$: 'Protected', a: a};
};
var $w0rm$elm_physics$Physics$World$add = F2(
	function (_v0, _v1) {
		var body = _v0.a;
		var world = _v1.a;
		var _v2 = world.freeIds;
		if (!_v2.b) {
			return $w0rm$elm_physics$Internal$World$Protected(
				_Utils_update(
					world,
					{
						bodies: A2(
							$elm$core$List$cons,
							_Utils_update(
								body,
								{id: world.nextBodyId}),
							world.bodies),
						nextBodyId: world.nextBodyId + 1
					}));
		} else {
			var freeId = _v2.a;
			var restFreeIds = _v2.b;
			return $w0rm$elm_physics$Internal$World$Protected(
				_Utils_update(
					world,
					{
						bodies: A2(
							$elm$core$List$cons,
							_Utils_update(
								body,
								{id: freeId}),
							world.bodies),
						freeIds: restFreeIds
					}));
		}
	});
var $ianmackenzie$elm_geometry$Geometry$Types$Sphere3d = function (a) {
	return {$: 'Sphere3d', a: a};
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $ianmackenzie$elm_units$Quantity$abs = function (_v0) {
	var value = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(
		$elm$core$Basics$abs(value));
};
var $ianmackenzie$elm_geometry$Sphere3d$withRadius = F2(
	function (givenRadius, givenCenterPoint) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Sphere3d(
			{
				centerPoint: givenCenterPoint,
				radius: $ianmackenzie$elm_units$Quantity$abs(givenRadius)
			});
	});
var $ianmackenzie$elm_geometry$Sphere3d$atPoint = F2(
	function (givenCenterPoint, givenRadius) {
		return A2($ianmackenzie$elm_geometry$Sphere3d$withRadius, givenRadius, givenCenterPoint);
	});
var $ianmackenzie$elm_geometry$Geometry$Types$Point3d = function (a) {
	return {$: 'Point3d', a: a};
};
var $ianmackenzie$elm_geometry$Point3d$origin = $ianmackenzie$elm_geometry$Geometry$Types$Point3d(
	{x: 0, y: 0, z: 0});
var $ianmackenzie$elm_geometry$Sphere3d$atOrigin = function (givenRadius) {
	return A2($ianmackenzie$elm_geometry$Sphere3d$atPoint, $ianmackenzie$elm_geometry$Point3d$origin, givenRadius);
};
var $author$project$Main$BlockingImmovableWall = function (a) {
	return {$: 'BlockingImmovableWall', a: a};
};
var $w0rm$elm_physics$Internal$Shape$Convex = function (a) {
	return {$: 'Convex', a: a};
};
var $w0rm$elm_physics$Internal$Shape$Protected = function (a) {
	return {$: 'Protected', a: a};
};
var $ianmackenzie$elm_geometry$Geometry$Types$Frame3d = function (a) {
	return {$: 'Frame3d', a: a};
};
var $ianmackenzie$elm_geometry$Frame3d$copy = function (_v0) {
	var properties = _v0.a;
	return $ianmackenzie$elm_geometry$Geometry$Types$Frame3d(properties);
};
var $ianmackenzie$elm_geometry$Block3d$axes = function (_v0) {
	var block = _v0.a;
	return $ianmackenzie$elm_geometry$Frame3d$copy(block.axes);
};
var $ianmackenzie$elm_geometry$Block3d$dimensions = function (_v0) {
	var block = _v0.a;
	return block.dimensions;
};
var $w0rm$elm_physics$Internal$Vector3$xAxis = {x: 1, y: 0, z: 0};
var $w0rm$elm_physics$Internal$Vector3$yAxis = {x: 0, y: 1, z: 0};
var $w0rm$elm_physics$Internal$Vector3$zAxis = {x: 0, y: 0, z: 1};
var $w0rm$elm_physics$Internal$Vector3$basis = _List_fromArray(
	[$w0rm$elm_physics$Internal$Vector3$xAxis, $w0rm$elm_physics$Internal$Vector3$yAxis, $w0rm$elm_physics$Internal$Vector3$zAxis]);
var $w0rm$elm_physics$Internal$Vector3$xNegative = {x: -1, y: 0, z: 0};
var $w0rm$elm_physics$Internal$Vector3$yNegative = {x: 0, y: -1, z: 0};
var $w0rm$elm_physics$Internal$Vector3$zNegative = {x: 0, y: 0, z: -1};
var $w0rm$elm_physics$Internal$Vector3$zero = {x: 0, y: 0, z: 0};
var $w0rm$elm_physics$Shapes$Convex$fromBlock = F3(
	function (sizeX, sizeY, sizeZ) {
		var z = sizeZ / 2;
		var y = sizeY / 2;
		var x = sizeX / 2;
		var volume = (sizeX * sizeY) * sizeZ;
		var v7 = {x: -x, y: y, z: z};
		var v6 = {x: x, y: y, z: z};
		var v5 = {x: x, y: -y, z: z};
		var v4 = {x: -x, y: -y, z: z};
		var v3 = {x: -x, y: y, z: -z};
		var v2 = {x: x, y: y, z: -z};
		var v1 = {x: x, y: -y, z: -z};
		var v0 = {x: -x, y: -y, z: -z};
		var inertia = {m11: (volume / 12) * ((sizeY * sizeY) + (sizeZ * sizeZ)), m12: 0, m13: 0, m21: 0, m22: (volume / 12) * ((sizeX * sizeX) + (sizeZ * sizeZ)), m23: 0, m31: 0, m32: 0, m33: (volume / 12) * ((sizeY * sizeY) + (sizeX * sizeX))};
		return {
			faces: _List_fromArray(
				[
					{
					normal: $w0rm$elm_physics$Internal$Vector3$zNegative,
					vertices: _List_fromArray(
						[v3, v2, v1, v0])
				},
					{
					normal: $w0rm$elm_physics$Internal$Vector3$zAxis,
					vertices: _List_fromArray(
						[v4, v5, v6, v7])
				},
					{
					normal: $w0rm$elm_physics$Internal$Vector3$yNegative,
					vertices: _List_fromArray(
						[v5, v4, v0, v1])
				},
					{
					normal: $w0rm$elm_physics$Internal$Vector3$yAxis,
					vertices: _List_fromArray(
						[v2, v3, v7, v6])
				},
					{
					normal: $w0rm$elm_physics$Internal$Vector3$xNegative,
					vertices: _List_fromArray(
						[v0, v4, v7, v3])
				},
					{
					normal: $w0rm$elm_physics$Internal$Vector3$xAxis,
					vertices: _List_fromArray(
						[v1, v2, v6, v5])
				}
				]),
			inertia: inertia,
			position: $w0rm$elm_physics$Internal$Vector3$zero,
			uniqueEdges: $w0rm$elm_physics$Internal$Vector3$basis,
			uniqueNormals: $w0rm$elm_physics$Internal$Vector3$basis,
			vertices: _List_fromArray(
				[v0, v1, v2, v3, v4, v5, v6, v7]),
			volume: volume
		};
	});
var $w0rm$elm_physics$Internal$Transform3d$Orientation3d = F4(
	function (a, b, c, d) {
		return {$: 'Orientation3d', a: a, b: b, c: c, d: d};
	});
var $w0rm$elm_physics$Internal$Transform3d$Transform3d = F2(
	function (a, b) {
		return {$: 'Transform3d', a: a, b: b};
	});
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $w0rm$elm_physics$Internal$Transform3d$fromOriginAndBasis = F4(
	function (origin, x, y, z) {
		var m22 = z.z;
		var m21 = y.z;
		var m20 = x.z;
		var m12 = z.y;
		var m11 = y.y;
		var m10 = x.y;
		var m02 = z.x;
		var m01 = y.x;
		var m00 = x.x;
		var tr = (m00 + m11) + m22;
		if (tr > 0) {
			var s = $elm$core$Basics$sqrt(tr + 1.0) * 2;
			return A2(
				$w0rm$elm_physics$Internal$Transform3d$Transform3d,
				origin,
				A4($w0rm$elm_physics$Internal$Transform3d$Orientation3d, (m21 - m12) / s, (m02 - m20) / s, (m10 - m01) / s, 0.25 * s));
		} else {
			if ((_Utils_cmp(m00, m11) > 0) && (_Utils_cmp(m00, m22) > 0)) {
				var s = $elm$core$Basics$sqrt(((1.0 + m00) - m11) - m22) * 2;
				return A2(
					$w0rm$elm_physics$Internal$Transform3d$Transform3d,
					origin,
					A4($w0rm$elm_physics$Internal$Transform3d$Orientation3d, 0.25 * s, (m01 + m10) / s, (m02 + m20) / s, (m21 - m12) / s));
			} else {
				if (_Utils_cmp(m11, m22) > 0) {
					var s = $elm$core$Basics$sqrt(((1.0 + m11) - m00) - m22) * 2;
					return A2(
						$w0rm$elm_physics$Internal$Transform3d$Transform3d,
						origin,
						A4($w0rm$elm_physics$Internal$Transform3d$Orientation3d, (m01 + m10) / s, 0.25 * s, (m12 + m21) / s, (m02 - m20) / s));
				} else {
					var s = $elm$core$Basics$sqrt(((1.0 + m22) - m00) - m11) * 2;
					return A2(
						$w0rm$elm_physics$Internal$Transform3d$Transform3d,
						origin,
						A4($w0rm$elm_physics$Internal$Transform3d$Orientation3d, (m02 + m20) / s, (m12 + m21) / s, 0.25 * s, (m10 - m01) / s));
				}
			}
		}
	});
var $ianmackenzie$elm_units$Length$inMeters = function (_v0) {
	var numMeters = _v0.a;
	return numMeters;
};
var $ianmackenzie$elm_geometry$Direction3d$xComponent = function (_v0) {
	var d = _v0.a;
	return d.x;
};
var $ianmackenzie$elm_geometry$Direction3d$yComponent = function (_v0) {
	var d = _v0.a;
	return d.y;
};
var $ianmackenzie$elm_geometry$Direction3d$zComponent = function (_v0) {
	var d = _v0.a;
	return d.z;
};
var $ianmackenzie$elm_geometry$Frame3d$isRightHanded = function (_v0) {
	var frame = _v0.a;
	var i = $ianmackenzie$elm_geometry$Direction3d$zComponent(frame.zDirection);
	var h = $ianmackenzie$elm_geometry$Direction3d$yComponent(frame.zDirection);
	var g = $ianmackenzie$elm_geometry$Direction3d$xComponent(frame.zDirection);
	var f = $ianmackenzie$elm_geometry$Direction3d$zComponent(frame.yDirection);
	var e = $ianmackenzie$elm_geometry$Direction3d$yComponent(frame.yDirection);
	var d = $ianmackenzie$elm_geometry$Direction3d$xComponent(frame.yDirection);
	var c = $ianmackenzie$elm_geometry$Direction3d$zComponent(frame.xDirection);
	var b = $ianmackenzie$elm_geometry$Direction3d$yComponent(frame.xDirection);
	var a = $ianmackenzie$elm_geometry$Direction3d$xComponent(frame.xDirection);
	return (((((((a * e) * i) + ((b * f) * g)) + ((c * d) * h)) - ((c * e) * g)) - ((b * d) * i)) - ((a * f) * h)) > 0;
};
var $ianmackenzie$elm_geometry$Frame3d$originPoint = function (_v0) {
	var properties = _v0.a;
	return properties.originPoint;
};
var $w0rm$elm_physics$Internal$Transform3d$rotate = F2(
	function (_v0, _v1) {
		var qx = _v0.a;
		var qy = _v0.b;
		var qz = _v0.c;
		var qw = _v0.d;
		var x = _v1.x;
		var y = _v1.y;
		var z = _v1.z;
		var iz = ((qw * z) + (qx * y)) - (qy * x);
		var iy = ((qw * y) + (qz * x)) - (qx * z);
		var ix = ((qw * x) + (qy * z)) - (qz * y);
		var iw = (((-qx) * x) - (qy * y)) - (qz * z);
		return {x: (((ix * qw) + (iw * (-qx))) + (iy * (-qz))) - (iz * (-qy)), y: (((iy * qw) + (iw * (-qy))) + (iz * (-qx))) - (ix * (-qz)), z: (((iz * qw) + (iw * (-qz))) + (ix * (-qy))) - (iy * (-qx))};
	});
var $w0rm$elm_physics$Internal$Transform3d$directionPlaceIn = F2(
	function (_v0, worldVector) {
		var globalOrientation = _v0.b;
		return A2($w0rm$elm_physics$Internal$Transform3d$rotate, globalOrientation, worldVector);
	});
var $w0rm$elm_physics$Internal$Transform3d$directionsPlaceInHelp = F3(
	function (transform, directions, result) {
		directionsPlaceInHelp:
		while (true) {
			if (directions.b) {
				var point = directions.a;
				var remainingdirections = directions.b;
				var $temp$transform = transform,
					$temp$directions = remainingdirections,
					$temp$result = A2(
					$elm$core$List$cons,
					A2($w0rm$elm_physics$Internal$Transform3d$directionPlaceIn, transform, point),
					result);
				transform = $temp$transform;
				directions = $temp$directions;
				result = $temp$result;
				continue directionsPlaceInHelp;
			} else {
				return result;
			}
		}
	});
var $w0rm$elm_physics$Internal$Transform3d$directionsPlaceIn = F2(
	function (transform, directions) {
		return A3($w0rm$elm_physics$Internal$Transform3d$directionsPlaceInHelp, transform, directions, _List_Nil);
	});
var $w0rm$elm_physics$Internal$Transform3d$pointPlaceIn = F2(
	function (_v0, _v2) {
		var globalOrigin = _v0.a;
		var _v1 = _v0.b;
		var qx = _v1.a;
		var qy = _v1.b;
		var qz = _v1.c;
		var qw = _v1.d;
		var x = _v2.x;
		var y = _v2.y;
		var z = _v2.z;
		var iz = ((qw * z) + (qx * y)) - (qy * x);
		var iy = ((qw * y) + (qz * x)) - (qx * z);
		var ix = ((qw * x) + (qy * z)) - (qz * y);
		var iw = (((-qx) * x) - (qy * y)) - (qz * z);
		return {x: ((((ix * qw) + (iw * (-qx))) + (iy * (-qz))) - (iz * (-qy))) + globalOrigin.x, y: ((((iy * qw) + (iw * (-qy))) + (iz * (-qx))) - (ix * (-qz))) + globalOrigin.y, z: ((((iz * qw) + (iw * (-qz))) + (ix * (-qy))) - (iy * (-qx))) + globalOrigin.z};
	});
var $w0rm$elm_physics$Internal$Transform3d$pointsPlaceInHelp = F3(
	function (transform, points, result) {
		pointsPlaceInHelp:
		while (true) {
			if (points.b) {
				var point = points.a;
				var remainingPoints = points.b;
				var $temp$transform = transform,
					$temp$points = remainingPoints,
					$temp$result = A2(
					$elm$core$List$cons,
					A2($w0rm$elm_physics$Internal$Transform3d$pointPlaceIn, transform, point),
					result);
				transform = $temp$transform;
				points = $temp$points;
				result = $temp$result;
				continue pointsPlaceInHelp;
			} else {
				return result;
			}
		}
	});
var $w0rm$elm_physics$Internal$Transform3d$pointsPlaceIn = F2(
	function (transform, points) {
		return A3($w0rm$elm_physics$Internal$Transform3d$pointsPlaceInHelp, transform, points, _List_Nil);
	});
var $w0rm$elm_physics$Shapes$Convex$facesPlaceInHelp = F3(
	function (transform3d, faces, result) {
		facesPlaceInHelp:
		while (true) {
			if (faces.b) {
				var vertices = faces.a.vertices;
				var normal = faces.a.normal;
				var remainingFaces = faces.b;
				var $temp$transform3d = transform3d,
					$temp$faces = remainingFaces,
					$temp$result = A2(
					$elm$core$List$cons,
					{
						normal: A2($w0rm$elm_physics$Internal$Transform3d$directionPlaceIn, transform3d, normal),
						vertices: $elm$core$List$reverse(
							A2($w0rm$elm_physics$Internal$Transform3d$pointsPlaceIn, transform3d, vertices))
					},
					result);
				transform3d = $temp$transform3d;
				faces = $temp$faces;
				result = $temp$result;
				continue facesPlaceInHelp;
			} else {
				return result;
			}
		}
	});
var $w0rm$elm_physics$Internal$Matrix3$mul = F2(
	function (a, b) {
		return {m11: ((a.m11 * b.m11) + (a.m12 * b.m21)) + (a.m13 * b.m31), m12: ((a.m11 * b.m12) + (a.m12 * b.m22)) + (a.m13 * b.m32), m13: ((a.m11 * b.m13) + (a.m12 * b.m23)) + (a.m13 * b.m33), m21: ((a.m21 * b.m11) + (a.m22 * b.m21)) + (a.m23 * b.m31), m22: ((a.m21 * b.m12) + (a.m22 * b.m22)) + (a.m23 * b.m32), m23: ((a.m21 * b.m13) + (a.m22 * b.m23)) + (a.m23 * b.m33), m31: ((a.m31 * b.m11) + (a.m32 * b.m21)) + (a.m33 * b.m31), m32: ((a.m31 * b.m12) + (a.m32 * b.m22)) + (a.m33 * b.m32), m33: ((a.m31 * b.m13) + (a.m32 * b.m23)) + (a.m33 * b.m33)};
	});
var $w0rm$elm_physics$Internal$Transform3d$orientation = function (_v0) {
	var _v1 = _v0.b;
	var x = _v1.a;
	var y = _v1.b;
	var z = _v1.c;
	var w = _v1.d;
	return {m11: (1 - ((2 * y) * y)) - ((2 * z) * z), m12: ((2 * x) * y) - ((2 * w) * z), m13: ((2 * x) * z) + ((2 * w) * y), m21: ((2 * x) * y) + ((2 * w) * z), m22: (1 - ((2 * x) * x)) - ((2 * z) * z), m23: ((2 * y) * z) - ((2 * w) * x), m31: ((2 * x) * z) - ((2 * w) * y), m32: ((2 * y) * z) + ((2 * w) * x), m33: (1 - ((2 * x) * x)) - ((2 * y) * y)};
};
var $w0rm$elm_physics$Internal$Matrix3$transpose = function (m) {
	return {m11: m.m11, m12: m.m21, m13: m.m31, m21: m.m12, m22: m.m22, m23: m.m32, m31: m.m13, m32: m.m23, m33: m.m33};
};
var $w0rm$elm_physics$Internal$Transform3d$inertiaRotateIn = F2(
	function (transform3d, inertia) {
		var rotation = $w0rm$elm_physics$Internal$Transform3d$orientation(transform3d);
		return A2(
			$w0rm$elm_physics$Internal$Matrix3$mul,
			rotation,
			A2(
				$w0rm$elm_physics$Internal$Matrix3$mul,
				inertia,
				$w0rm$elm_physics$Internal$Matrix3$transpose(rotation)));
	});
var $w0rm$elm_physics$Shapes$Convex$placeIn = F2(
	function (transform3d, _v0) {
		var faces = _v0.faces;
		var vertices = _v0.vertices;
		var uniqueEdges = _v0.uniqueEdges;
		var uniqueNormals = _v0.uniqueNormals;
		var position = _v0.position;
		var volume = _v0.volume;
		var inertia = _v0.inertia;
		return {
			faces: A3($w0rm$elm_physics$Shapes$Convex$facesPlaceInHelp, transform3d, faces, _List_Nil),
			inertia: A2($w0rm$elm_physics$Internal$Transform3d$inertiaRotateIn, transform3d, inertia),
			position: A2($w0rm$elm_physics$Internal$Transform3d$pointPlaceIn, transform3d, position),
			uniqueEdges: A2($w0rm$elm_physics$Internal$Transform3d$directionsPlaceIn, transform3d, uniqueEdges),
			uniqueNormals: A2($w0rm$elm_physics$Internal$Transform3d$directionsPlaceIn, transform3d, uniqueNormals),
			vertices: A2($w0rm$elm_physics$Internal$Transform3d$pointsPlaceIn, transform3d, vertices),
			volume: volume
		};
	});
var $ianmackenzie$elm_geometry$Geometry$Types$Direction3d = function (a) {
	return {$: 'Direction3d', a: a};
};
var $ianmackenzie$elm_geometry$Direction3d$reverse = function (_v0) {
	var d = _v0.a;
	return $ianmackenzie$elm_geometry$Geometry$Types$Direction3d(
		{x: -d.x, y: -d.y, z: -d.z});
};
var $ianmackenzie$elm_geometry$Frame3d$unsafe = function (properties) {
	return $ianmackenzie$elm_geometry$Geometry$Types$Frame3d(properties);
};
var $ianmackenzie$elm_geometry$Frame3d$xDirection = function (_v0) {
	var properties = _v0.a;
	return properties.xDirection;
};
var $ianmackenzie$elm_geometry$Frame3d$yDirection = function (_v0) {
	var properties = _v0.a;
	return properties.yDirection;
};
var $ianmackenzie$elm_geometry$Frame3d$zDirection = function (_v0) {
	var properties = _v0.a;
	return properties.zDirection;
};
var $ianmackenzie$elm_geometry$Frame3d$reverseZ = function (frame) {
	return $ianmackenzie$elm_geometry$Frame3d$unsafe(
		{
			originPoint: $ianmackenzie$elm_geometry$Frame3d$originPoint(frame),
			xDirection: $ianmackenzie$elm_geometry$Frame3d$xDirection(frame),
			yDirection: $ianmackenzie$elm_geometry$Frame3d$yDirection(frame),
			zDirection: $ianmackenzie$elm_geometry$Direction3d$reverse(
				$ianmackenzie$elm_geometry$Frame3d$zDirection(frame))
		});
};
var $ianmackenzie$elm_geometry$Direction3d$unwrap = function (_v0) {
	var coordinates = _v0.a;
	return coordinates;
};
var $ianmackenzie$elm_geometry$Point3d$unwrap = function (_v0) {
	var pointCoordinates = _v0.a;
	return pointCoordinates;
};
var $w0rm$elm_physics$Physics$Shape$block = function (block3d) {
	var frame3d = $ianmackenzie$elm_geometry$Block3d$axes(block3d);
	var rightHandedFrame3d = $ianmackenzie$elm_geometry$Frame3d$isRightHanded(frame3d) ? frame3d : $ianmackenzie$elm_geometry$Frame3d$reverseZ(frame3d);
	var origin = $ianmackenzie$elm_geometry$Point3d$unwrap(
		$ianmackenzie$elm_geometry$Frame3d$originPoint(rightHandedFrame3d));
	var x = $ianmackenzie$elm_geometry$Direction3d$unwrap(
		$ianmackenzie$elm_geometry$Frame3d$xDirection(rightHandedFrame3d));
	var y = $ianmackenzie$elm_geometry$Direction3d$unwrap(
		$ianmackenzie$elm_geometry$Frame3d$yDirection(rightHandedFrame3d));
	var z = $ianmackenzie$elm_geometry$Direction3d$unwrap(
		$ianmackenzie$elm_geometry$Frame3d$zDirection(rightHandedFrame3d));
	var tranform3d = A4($w0rm$elm_physics$Internal$Transform3d$fromOriginAndBasis, origin, x, y, z);
	var _v0 = $ianmackenzie$elm_geometry$Block3d$dimensions(block3d);
	var sizeX = _v0.a;
	var sizeY = _v0.b;
	var sizeZ = _v0.c;
	return $w0rm$elm_physics$Internal$Shape$Protected(
		$w0rm$elm_physics$Internal$Shape$Convex(
			A2(
				$w0rm$elm_physics$Shapes$Convex$placeIn,
				tranform3d,
				A3(
					$w0rm$elm_physics$Shapes$Convex$fromBlock,
					$ianmackenzie$elm_units$Length$inMeters(sizeX),
					$ianmackenzie$elm_units$Length$inMeters(sizeY),
					$ianmackenzie$elm_units$Length$inMeters(sizeZ)))));
};
var $w0rm$elm_physics$Internal$Body$Protected = function (a) {
	return {$: 'Protected', a: a};
};
var $w0rm$elm_physics$Internal$Transform3d$identity = A4($w0rm$elm_physics$Internal$Transform3d$Orientation3d, 0, 0, 0, 1);
var $w0rm$elm_physics$Internal$Transform3d$atOrigin = A2($w0rm$elm_physics$Internal$Transform3d$Transform3d, $w0rm$elm_physics$Internal$Vector3$zero, $w0rm$elm_physics$Internal$Transform3d$identity);
var $w0rm$elm_physics$Internal$Transform3d$atPoint = function (point) {
	return A2($w0rm$elm_physics$Internal$Transform3d$Transform3d, point, $w0rm$elm_physics$Internal$Transform3d$identity);
};
var $w0rm$elm_physics$Internal$Vector3$add = F2(
	function (a, b) {
		return {x: a.x + b.x, y: a.y + b.y, z: a.z + b.z};
	});
var $w0rm$elm_physics$Internal$Vector3$scale = F2(
	function (s, v3) {
		return {x: s * v3.x, y: s * v3.y, z: s * v3.z};
	});
var $w0rm$elm_physics$Internal$Shape$volume = function (shape) {
	switch (shape.$) {
		case 'Sphere':
			var sphere = shape.a;
			return sphere.volume;
		case 'Convex':
			var convex = shape.a;
			return convex.volume;
		case 'Plane':
			return 0;
		default:
			return 0;
	}
};
var $w0rm$elm_physics$Internal$Body$centerOfMass = function (shapes) {
	var totalVolume = A3(
		$elm$core$List$foldl,
		F2(
			function (shape, sum) {
				return sum + $w0rm$elm_physics$Internal$Shape$volume(shape);
			}),
		0,
		shapes);
	return (totalVolume > 0) ? A3(
		$elm$core$List$foldl,
		function (shape) {
			return $w0rm$elm_physics$Internal$Vector3$add(
				A2(
					$w0rm$elm_physics$Internal$Vector3$scale,
					$w0rm$elm_physics$Internal$Shape$volume(shape) / totalVolume,
					function () {
						switch (shape.$) {
							case 'Convex':
								var position = shape.a.position;
								return position;
							case 'Particle':
								var position = shape.a;
								return position;
							case 'Sphere':
								var position = shape.a.position;
								return position;
							default:
								var position = shape.a.position;
								return position;
						}
					}()));
		},
		$w0rm$elm_physics$Internal$Vector3$zero,
		shapes) : $w0rm$elm_physics$Internal$Vector3$zero;
};
var $w0rm$elm_physics$Internal$Material$default = {bounciness: 0, friction: 0.3};
var $w0rm$elm_physics$Internal$Vector3$lengthSquared = function (_v0) {
	var x = _v0.x;
	var y = _v0.y;
	var z = _v0.z;
	return ((x * x) + (y * y)) + (z * z);
};
var $w0rm$elm_physics$Shapes$Convex$expandBoundingSphereRadius = F2(
	function (_v0, boundingSphereRadius) {
		var vertices = _v0.vertices;
		return $elm$core$Basics$sqrt(
			A3(
				$elm$core$List$foldl,
				function (vertex) {
					return $elm$core$Basics$max(
						$w0rm$elm_physics$Internal$Vector3$lengthSquared(vertex));
				},
				boundingSphereRadius * boundingSphereRadius,
				vertices));
	});
var $w0rm$elm_physics$Internal$Vector3$length = function (_v0) {
	var x = _v0.x;
	var y = _v0.y;
	var z = _v0.z;
	return $elm$core$Basics$sqrt(((x * x) + (y * y)) + (z * z));
};
var $w0rm$elm_physics$Shapes$Sphere$expandBoundingSphereRadius = F2(
	function (_v0, boundingSphereRadius) {
		var radius = _v0.radius;
		var position = _v0.position;
		return A2(
			$elm$core$Basics$max,
			$w0rm$elm_physics$Internal$Vector3$length(position) + radius,
			boundingSphereRadius);
	});
var $w0rm$elm_physics$Internal$Const$maxNumber = 3.40282347e38;
var $w0rm$elm_physics$Internal$Shape$expandBoundingSphereRadius = F2(
	function (shape, boundingSphereRadius) {
		switch (shape.$) {
			case 'Convex':
				var convex = shape.a;
				return A2($w0rm$elm_physics$Shapes$Convex$expandBoundingSphereRadius, convex, boundingSphereRadius);
			case 'Sphere':
				var sphere = shape.a;
				return A2($w0rm$elm_physics$Shapes$Sphere$expandBoundingSphereRadius, sphere, boundingSphereRadius);
			case 'Plane':
				return $w0rm$elm_physics$Internal$Const$maxNumber;
			default:
				var position = shape.a;
				return A2(
					$elm$core$Basics$max,
					boundingSphereRadius,
					$w0rm$elm_physics$Internal$Vector3$length(position));
		}
	});
var $w0rm$elm_physics$Internal$Transform3d$derotate = F2(
	function (_v0, _v1) {
		var qx = _v0.a;
		var qy = _v0.b;
		var qz = _v0.c;
		var qw = _v0.d;
		var x = _v1.x;
		var y = _v1.y;
		var z = _v1.z;
		var iz = (((-qw) * z) + (qx * y)) - (qy * x);
		var iy = (((-qw) * y) + (qz * x)) - (qx * z);
		var ix = (((-qw) * x) + (qy * z)) - (qz * y);
		var iw = (((-qx) * x) - (qy * y)) - (qz * z);
		return {x: (((ix * (-qw)) + (iw * (-qx))) + (iy * (-qz))) - (iz * (-qy)), y: (((iy * (-qw)) + (iw * (-qy))) + (iz * (-qx))) - (ix * (-qz)), z: (((iz * (-qw)) + (iw * (-qz))) + (ix * (-qy))) - (iy * (-qx))};
	});
var $w0rm$elm_physics$Internal$Vector3$sub = F2(
	function (a, b) {
		return {x: a.x - b.x, y: a.y - b.y, z: a.z - b.z};
	});
var $w0rm$elm_physics$Internal$Transform3d$pointRelativeTo = F2(
	function (_v0, worldPoint) {
		var localOrigin = _v0.a;
		var localOrientation = _v0.b;
		return A2(
			$w0rm$elm_physics$Internal$Transform3d$derotate,
			localOrientation,
			A2($w0rm$elm_physics$Internal$Vector3$sub, worldPoint, localOrigin));
	});
var $w0rm$elm_physics$Internal$Transform3d$inverse = function (transform3d) {
	var _v0 = transform3d.b;
	var x = _v0.a;
	var y = _v0.b;
	var z = _v0.c;
	var w = _v0.d;
	return A2(
		$w0rm$elm_physics$Internal$Transform3d$Transform3d,
		A2($w0rm$elm_physics$Internal$Transform3d$pointRelativeTo, transform3d, $w0rm$elm_physics$Internal$Vector3$zero),
		A4($w0rm$elm_physics$Internal$Transform3d$Orientation3d, -x, -y, -z, w));
};
var $w0rm$elm_physics$Internal$Transform3d$mul = F2(
	function (_v0, _v1) {
		var q1x = _v0.a;
		var q1y = _v0.b;
		var q1z = _v0.c;
		var q1w = _v0.d;
		var q2x = _v1.a;
		var q2y = _v1.b;
		var q2z = _v1.c;
		var q2w = _v1.d;
		return A4($w0rm$elm_physics$Internal$Transform3d$Orientation3d, (((q1x * q2w) + (q1y * q2z)) - (q1z * q2y)) + (q1w * q2x), ((((-q1x) * q2z) + (q1y * q2w)) + (q1z * q2x)) + (q1w * q2y), (((q1x * q2y) - (q1y * q2x)) + (q1z * q2w)) + (q1w * q2z), ((((-q1x) * q2x) - (q1y * q2y)) - (q1z * q2z)) + (q1w * q2w));
	});
var $w0rm$elm_physics$Internal$Transform3d$placeIn = F2(
	function (_v0, _v1) {
		var globalPosition = _v0.a;
		var globalOrientation = _v0.b;
		var localPosition = _v1.a;
		var localOrientation = _v1.b;
		return A2(
			$w0rm$elm_physics$Internal$Transform3d$Transform3d,
			A2(
				$w0rm$elm_physics$Internal$Vector3$add,
				globalPosition,
				A2($w0rm$elm_physics$Internal$Transform3d$rotate, globalOrientation, localPosition)),
			A2($w0rm$elm_physics$Internal$Transform3d$mul, globalOrientation, localOrientation));
	});
var $w0rm$elm_physics$Internal$Shape$Particle = function (a) {
	return {$: 'Particle', a: a};
};
var $w0rm$elm_physics$Internal$Shape$Plane = function (a) {
	return {$: 'Plane', a: a};
};
var $w0rm$elm_physics$Internal$Shape$Sphere = function (a) {
	return {$: 'Sphere', a: a};
};
var $w0rm$elm_physics$Shapes$Plane$placeIn = F2(
	function (transform3d, _v0) {
		var normal = _v0.normal;
		var position = _v0.position;
		return {
			normal: A2($w0rm$elm_physics$Internal$Transform3d$directionPlaceIn, transform3d, normal),
			position: A2($w0rm$elm_physics$Internal$Transform3d$pointPlaceIn, transform3d, position)
		};
	});
var $w0rm$elm_physics$Shapes$Sphere$placeIn = F2(
	function (transform3d, _v0) {
		var radius = _v0.radius;
		var position = _v0.position;
		var volume = _v0.volume;
		var inertia = _v0.inertia;
		return {
			inertia: inertia,
			position: A2($w0rm$elm_physics$Internal$Transform3d$pointPlaceIn, transform3d, position),
			radius: radius,
			volume: volume
		};
	});
var $w0rm$elm_physics$Internal$Shape$shapesPlaceInHelp = F3(
	function (transform3d, shapes, result) {
		shapesPlaceInHelp:
		while (true) {
			if (shapes.b) {
				var shape = shapes.a;
				var remainingShapes = shapes.b;
				var $temp$transform3d = transform3d,
					$temp$shapes = remainingShapes,
					$temp$result = A2(
					$elm$core$List$cons,
					function () {
						switch (shape.$) {
							case 'Convex':
								var convex = shape.a;
								return $w0rm$elm_physics$Internal$Shape$Convex(
									A2($w0rm$elm_physics$Shapes$Convex$placeIn, transform3d, convex));
							case 'Plane':
								var plane = shape.a;
								return $w0rm$elm_physics$Internal$Shape$Plane(
									A2($w0rm$elm_physics$Shapes$Plane$placeIn, transform3d, plane));
							case 'Sphere':
								var sphere = shape.a;
								return $w0rm$elm_physics$Internal$Shape$Sphere(
									A2($w0rm$elm_physics$Shapes$Sphere$placeIn, transform3d, sphere));
							default:
								var position = shape.a;
								return $w0rm$elm_physics$Internal$Shape$Particle(
									A2($w0rm$elm_physics$Internal$Transform3d$pointPlaceIn, transform3d, position));
						}
					}(),
					result);
				transform3d = $temp$transform3d;
				shapes = $temp$shapes;
				result = $temp$result;
				continue shapesPlaceInHelp;
			} else {
				return result;
			}
		}
	});
var $w0rm$elm_physics$Internal$Shape$shapesPlaceIn = F2(
	function (transform3d, shapes) {
		return A3($w0rm$elm_physics$Internal$Shape$shapesPlaceInHelp, transform3d, shapes, _List_Nil);
	});
var $w0rm$elm_physics$Internal$Matrix3$add = F2(
	function (a, b) {
		return {m11: a.m11 + b.m11, m12: a.m12 + b.m12, m13: a.m13 + b.m13, m21: a.m21 + b.m21, m22: a.m22 + b.m22, m23: a.m23 + b.m23, m31: a.m31 + b.m31, m32: a.m32 + b.m32, m33: a.m33 + b.m33};
	});
var $w0rm$elm_physics$Internal$Shape$centerOfMass = function (shape) {
	switch (shape.$) {
		case 'Sphere':
			var sphere = shape.a;
			return sphere.position;
		case 'Convex':
			var convex = shape.a;
			return convex.position;
		case 'Plane':
			return $w0rm$elm_physics$Internal$Vector3$zero;
		default:
			return $w0rm$elm_physics$Internal$Vector3$zero;
	}
};
var $w0rm$elm_physics$Internal$Matrix3$zero = {m11: 0, m12: 0, m13: 0, m21: 0, m22: 0, m23: 0, m31: 0, m32: 0, m33: 0};
var $w0rm$elm_physics$Internal$Shape$inertia = function (shape) {
	switch (shape.$) {
		case 'Sphere':
			var sphere = shape.a;
			return sphere.inertia;
		case 'Convex':
			var convex = shape.a;
			return convex.inertia;
		case 'Plane':
			return $w0rm$elm_physics$Internal$Matrix3$zero;
		default:
			return $w0rm$elm_physics$Internal$Matrix3$zero;
	}
};
var $w0rm$elm_physics$Internal$Matrix3$pointInertia = F4(
	function (m, x, y, z) {
		var m32 = ((-m) * y) * z;
		var m31 = ((-m) * x) * z;
		var m21 = ((-m) * x) * y;
		return {m11: m * ((y * y) + (z * z)), m12: m21, m13: m31, m21: m21, m22: m * ((z * z) + (x * x)), m23: m32, m31: m31, m32: m32, m33: m * ((x * x) + (y * y))};
	});
var $w0rm$elm_physics$Internal$Transform3d$inertiaPlaceIn = F4(
	function (transform3d, centerOfMass, mass, inertia) {
		var x = transform3d.a.x;
		var y = transform3d.a.y;
		var z = transform3d.a.z;
		var rotatedInertia = A2($w0rm$elm_physics$Internal$Transform3d$inertiaRotateIn, transform3d, inertia);
		var inertiaOffset = A4($w0rm$elm_physics$Internal$Matrix3$pointInertia, mass, x - centerOfMass.x, y - centerOfMass.y, z - centerOfMass.z);
		return A2($w0rm$elm_physics$Internal$Matrix3$add, rotatedInertia, inertiaOffset);
	});
var $w0rm$elm_physics$Internal$Matrix3$inverse = function (_v0) {
	var m11 = _v0.m11;
	var m21 = _v0.m21;
	var m31 = _v0.m31;
	var m12 = _v0.m12;
	var m22 = _v0.m22;
	var m32 = _v0.m32;
	var m13 = _v0.m13;
	var m23 = _v0.m23;
	var m33 = _v0.m33;
	var det = ((m11 * ((m22 * m33) - (m32 * m23))) - (m12 * ((m21 * m33) - (m23 * m31)))) + (m13 * ((m21 * m32) - (m22 * m31)));
	var invdet = 1 / det;
	return (!det) ? $w0rm$elm_physics$Internal$Matrix3$zero : {m11: ((m22 * m33) - (m32 * m23)) * invdet, m12: ((m13 * m32) - (m12 * m33)) * invdet, m13: ((m12 * m23) - (m13 * m22)) * invdet, m21: ((m23 * m31) - (m21 * m33)) * invdet, m22: ((m11 * m33) - (m13 * m31)) * invdet, m23: ((m21 * m13) - (m11 * m23)) * invdet, m31: ((m21 * m32) - (m31 * m22)) * invdet, m32: ((m31 * m12) - (m11 * m32)) * invdet, m33: ((m11 * m22) - (m21 * m12)) * invdet};
};
var $w0rm$elm_physics$Internal$Transform3d$invertedInertiaRotateIn = F2(
	function (transform3d, inertia) {
		var rotation = $w0rm$elm_physics$Internal$Transform3d$orientation(transform3d);
		return A2(
			$w0rm$elm_physics$Internal$Matrix3$mul,
			$w0rm$elm_physics$Internal$Matrix3$transpose(rotation),
			A2($w0rm$elm_physics$Internal$Matrix3$mul, inertia, rotation));
	});
var $w0rm$elm_physics$Internal$Matrix3$scale = F2(
	function (k, m) {
		return {m11: k * m.m11, m12: k * m.m12, m13: k * m.m13, m21: k * m.m21, m22: k * m.m22, m23: k * m.m23, m31: k * m.m31, m32: k * m.m32, m33: k * m.m33};
	});
var $w0rm$elm_physics$Internal$Body$updateMassProperties = function (body) {
	var mass = body.mass;
	var shapes = body.shapes;
	var totalVolume = A3(
		$elm$core$List$foldl,
		F2(
			function (shape, result) {
				return $w0rm$elm_physics$Internal$Shape$volume(shape) + result;
			}),
		0,
		shapes);
	var invMass = (!mass) ? 0 : (1 / mass);
	var density = mass / totalVolume;
	var inertia = A2(
		$w0rm$elm_physics$Internal$Matrix3$scale,
		density,
		A3(
			$elm$core$List$foldl,
			function (shape) {
				var shapeVolume = $w0rm$elm_physics$Internal$Shape$volume(shape);
				var shapeInertia = $w0rm$elm_physics$Internal$Shape$inertia(shape);
				var shapeCenterOfMass = $w0rm$elm_physics$Internal$Shape$centerOfMass(shape);
				var resultInertia = A4(
					$w0rm$elm_physics$Internal$Transform3d$inertiaPlaceIn,
					body.centerOfMassTransform3d,
					A2($w0rm$elm_physics$Internal$Transform3d$pointPlaceIn, body.centerOfMassTransform3d, shapeCenterOfMass),
					shapeVolume,
					shapeInertia);
				return $w0rm$elm_physics$Internal$Matrix3$add(resultInertia);
			},
			$w0rm$elm_physics$Internal$Matrix3$zero,
			shapes));
	var invInertia = $w0rm$elm_physics$Internal$Matrix3$inverse(inertia);
	return _Utils_update(
		body,
		{
			invInertia: invInertia,
			invInertiaWorld: A2($w0rm$elm_physics$Internal$Transform3d$invertedInertiaRotateIn, body.transform3d, invInertia),
			invMass: invMass
		});
};
var $w0rm$elm_physics$Internal$Body$compound = F2(
	function (shapes, data) {
		var centerOfMassPoint = $w0rm$elm_physics$Internal$Body$centerOfMass(shapes);
		var centerOfMassTransform3d = $w0rm$elm_physics$Internal$Transform3d$atPoint(centerOfMassPoint);
		var inverseCenterOfMassTransform3d = $w0rm$elm_physics$Internal$Transform3d$inverse(centerOfMassTransform3d);
		var movedShapes = A2($w0rm$elm_physics$Internal$Shape$shapesPlaceIn, inverseCenterOfMassTransform3d, shapes);
		var bodyTransform3d = $w0rm$elm_physics$Internal$Transform3d$atOrigin;
		var transform3d = A2($w0rm$elm_physics$Internal$Transform3d$placeIn, bodyTransform3d, centerOfMassTransform3d);
		var shapeTransform = A2($w0rm$elm_physics$Internal$Transform3d$placeIn, transform3d, inverseCenterOfMassTransform3d);
		return $w0rm$elm_physics$Internal$Body$updateMassProperties(
			{
				angularDamping: 0.01,
				angularVelocity: $w0rm$elm_physics$Internal$Vector3$zero,
				boundingSphereRadius: A3($elm$core$List$foldl, $w0rm$elm_physics$Internal$Shape$expandBoundingSphereRadius, 0, movedShapes),
				centerOfMassTransform3d: centerOfMassTransform3d,
				data: data,
				force: $w0rm$elm_physics$Internal$Vector3$zero,
				id: -1,
				invInertia: $w0rm$elm_physics$Internal$Matrix3$zero,
				invInertiaWorld: $w0rm$elm_physics$Internal$Matrix3$zero,
				invMass: 0,
				linearDamping: 0.01,
				mass: 0,
				material: $w0rm$elm_physics$Internal$Material$default,
				shapes: movedShapes,
				torque: $w0rm$elm_physics$Internal$Vector3$zero,
				transform3d: transform3d,
				velocity: $w0rm$elm_physics$Internal$Vector3$zero,
				worldShapes: A2($w0rm$elm_physics$Internal$Shape$shapesPlaceIn, shapeTransform, shapes)
			});
	});
var $w0rm$elm_physics$Physics$Body$compound = F2(
	function (shapes, newData) {
		var unprotectedShapes = A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, result) {
					var shape = _v0.a;
					return A2($elm$core$List$cons, shape, result);
				}),
			_List_Nil,
			shapes);
		return $w0rm$elm_physics$Internal$Body$Protected(
			A2($w0rm$elm_physics$Internal$Body$compound, unprotectedShapes, newData));
	});
var $ianmackenzie$elm_geometry$Geometry$Types$Block3d = function (a) {
	return {$: 'Block3d', a: a};
};
var $elm$core$Basics$ge = _Utils_ge;
var $ianmackenzie$elm_units$Quantity$greaterThanOrEqualTo = F2(
	function (_v0, _v1) {
		var y = _v0.a;
		var x = _v1.a;
		return _Utils_cmp(x, y) > -1;
	});
var $ianmackenzie$elm_units$Quantity$midpoint = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(x + (0.5 * (y - x)));
	});
var $ianmackenzie$elm_units$Quantity$minus = F2(
	function (_v0, _v1) {
		var y = _v0.a;
		var x = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(x - y);
	});
var $ianmackenzie$elm_geometry$Direction3d$unsafe = function (givenComponents) {
	return $ianmackenzie$elm_geometry$Geometry$Types$Direction3d(givenComponents);
};
var $ianmackenzie$elm_geometry$Direction3d$negativeX = $ianmackenzie$elm_geometry$Direction3d$unsafe(
	{x: -1, y: 0, z: 0});
var $ianmackenzie$elm_geometry$Direction3d$negativeY = $ianmackenzie$elm_geometry$Direction3d$unsafe(
	{x: 0, y: -1, z: 0});
var $ianmackenzie$elm_geometry$Direction3d$negativeZ = $ianmackenzie$elm_geometry$Direction3d$unsafe(
	{x: 0, y: 0, z: -1});
var $ianmackenzie$elm_geometry$Direction3d$positiveX = $ianmackenzie$elm_geometry$Direction3d$unsafe(
	{x: 1, y: 0, z: 0});
var $ianmackenzie$elm_geometry$Direction3d$positiveY = $ianmackenzie$elm_geometry$Direction3d$unsafe(
	{x: 0, y: 1, z: 0});
var $ianmackenzie$elm_geometry$Direction3d$positiveZ = $ianmackenzie$elm_geometry$Direction3d$unsafe(
	{x: 0, y: 0, z: 1});
var $ianmackenzie$elm_geometry$Point3d$xyz = F3(
	function (_v0, _v1, _v2) {
		var x = _v0.a;
		var y = _v1.a;
		var z = _v2.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Point3d(
			{x: x, y: y, z: z});
	});
var $ianmackenzie$elm_geometry$Block3d$axisAligned = F6(
	function (x1, y1, z1, x2, y2, z2) {
		var computedZDirection = A2($ianmackenzie$elm_units$Quantity$greaterThanOrEqualTo, z1, z2) ? $ianmackenzie$elm_geometry$Direction3d$positiveZ : $ianmackenzie$elm_geometry$Direction3d$negativeZ;
		var computedYDirection = A2($ianmackenzie$elm_units$Quantity$greaterThanOrEqualTo, y1, y2) ? $ianmackenzie$elm_geometry$Direction3d$positiveY : $ianmackenzie$elm_geometry$Direction3d$negativeY;
		var computedXDirection = A2($ianmackenzie$elm_units$Quantity$greaterThanOrEqualTo, x1, x2) ? $ianmackenzie$elm_geometry$Direction3d$positiveX : $ianmackenzie$elm_geometry$Direction3d$negativeX;
		var computedDimensions = _Utils_Tuple3(
			$ianmackenzie$elm_units$Quantity$abs(
				A2($ianmackenzie$elm_units$Quantity$minus, x1, x2)),
			$ianmackenzie$elm_units$Quantity$abs(
				A2($ianmackenzie$elm_units$Quantity$minus, y1, y2)),
			$ianmackenzie$elm_units$Quantity$abs(
				A2($ianmackenzie$elm_units$Quantity$minus, z1, z2)));
		var computedCenterPoint = A3(
			$ianmackenzie$elm_geometry$Point3d$xyz,
			A2($ianmackenzie$elm_units$Quantity$midpoint, x1, x2),
			A2($ianmackenzie$elm_units$Quantity$midpoint, y1, y2),
			A2($ianmackenzie$elm_units$Quantity$midpoint, z1, z2));
		var computedAxes = $ianmackenzie$elm_geometry$Frame3d$unsafe(
			{originPoint: computedCenterPoint, xDirection: computedXDirection, yDirection: computedYDirection, zDirection: computedZDirection});
		return $ianmackenzie$elm_geometry$Geometry$Types$Block3d(
			{axes: computedAxes, dimensions: computedDimensions});
	});
var $ianmackenzie$elm_geometry$Point3d$xCoordinate = function (_v0) {
	var p = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(p.x);
};
var $ianmackenzie$elm_geometry$Point3d$yCoordinate = function (_v0) {
	var p = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(p.y);
};
var $ianmackenzie$elm_geometry$Point3d$zCoordinate = function (_v0) {
	var p = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(p.z);
};
var $ianmackenzie$elm_geometry$Block3d$from = F2(
	function (p1, p2) {
		return A6(
			$ianmackenzie$elm_geometry$Block3d$axisAligned,
			$ianmackenzie$elm_geometry$Point3d$xCoordinate(p1),
			$ianmackenzie$elm_geometry$Point3d$yCoordinate(p1),
			$ianmackenzie$elm_geometry$Point3d$zCoordinate(p1),
			$ianmackenzie$elm_geometry$Point3d$xCoordinate(p2),
			$ianmackenzie$elm_geometry$Point3d$yCoordinate(p2),
			$ianmackenzie$elm_geometry$Point3d$zCoordinate(p2));
	});
var $ianmackenzie$elm_geometry$Point3d$meters = F3(
	function (x, y, z) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Point3d(
			{x: x, y: y, z: z});
	});
var $author$project$Main$thickLinearBlock = function (dimensions) {
	var w = 0.1;
	var l = $ianmackenzie$elm_units$Length$inMeters(dimensions.length);
	var h = 0.3;
	return A2(
		$ianmackenzie$elm_geometry$Block3d$from,
		A3($ianmackenzie$elm_geometry$Point3d$meters, -(l / 2), 0, 0),
		A3($ianmackenzie$elm_geometry$Point3d$meters, l / 2, -w, h));
};
var $author$project$Main$blockingImmovableWallBody = function (dimensions) {
	return A2(
		$w0rm$elm_physics$Physics$Body$compound,
		_List_fromArray(
			[
				$w0rm$elm_physics$Physics$Shape$block(
				$author$project$Main$thickLinearBlock(dimensions))
			]),
		$author$project$Main$BlockingImmovableWall(dimensions));
};
var $author$project$Main$MouseImitation = function (a) {
	return {$: 'MouseImitation', a: a};
};
var $w0rm$elm_physics$Physics$Body$Dynamic = function (a) {
	return {$: 'Dynamic', a: a};
};
var $w0rm$elm_physics$Physics$Body$Static = {$: 'Static'};
var $ianmackenzie$elm_units$Mass$inKilograms = function (_v0) {
	var numKilograms = _v0.a;
	return numKilograms;
};
var $elm$core$Basics$isInfinite = _Basics_isInfinite;
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $w0rm$elm_physics$Physics$Body$dynamic = function (kilos) {
	var mass_ = $ianmackenzie$elm_units$Mass$inKilograms(kilos);
	return ($elm$core$Basics$isNaN(mass_) || ($elm$core$Basics$isInfinite(mass_) || (mass_ <= 0))) ? $w0rm$elm_physics$Physics$Body$Static : $w0rm$elm_physics$Physics$Body$Dynamic(mass_);
};
var $ianmackenzie$elm_units$Mass$kilograms = function (numKilograms) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numKilograms);
};
var $ianmackenzie$elm_units$Length$meters = function (numMeters) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numMeters);
};
var $w0rm$elm_physics$Internal$Transform3d$moveTo = F2(
	function (newOrigin, _v0) {
		var localOrientation = _v0.b;
		return A2($w0rm$elm_physics$Internal$Transform3d$Transform3d, newOrigin, localOrientation);
	});
var $ianmackenzie$elm_geometry$Point3d$toMeters = function (_v0) {
	var pointCoordinates = _v0.a;
	return pointCoordinates;
};
var $w0rm$elm_physics$Physics$Body$moveTo = F2(
	function (point3d, _v0) {
		var body = _v0.a;
		var bodyCoordinatesTransform3d = A2(
			$w0rm$elm_physics$Internal$Transform3d$placeIn,
			body.transform3d,
			$w0rm$elm_physics$Internal$Transform3d$inverse(body.centerOfMassTransform3d));
		var newTransform3d = A2(
			$w0rm$elm_physics$Internal$Transform3d$placeIn,
			A2(
				$w0rm$elm_physics$Internal$Transform3d$moveTo,
				$ianmackenzie$elm_geometry$Point3d$toMeters(point3d),
				bodyCoordinatesTransform3d),
			body.centerOfMassTransform3d);
		return $w0rm$elm_physics$Internal$Body$Protected(
			_Utils_update(
				body,
				{
					transform3d: newTransform3d,
					worldShapes: A2($w0rm$elm_physics$Internal$Shape$shapesPlaceIn, newTransform3d, body.shapes)
				}));
	});
var $elm$core$Basics$pow = _Basics_pow;
var $w0rm$elm_physics$Internal$Matrix3$sphereInertia = F2(
	function (m, radius) {
		var i = (((m * 2) / 5) * radius) * radius;
		return {m11: i, m12: 0, m13: 0, m21: 0, m22: i, m23: 0, m31: 0, m32: 0, m33: i};
	});
var $w0rm$elm_physics$Shapes$Sphere$atOrigin = function (radius) {
	var volume = ((4 / 3) * $elm$core$Basics$pi) * A2($elm$core$Basics$pow, radius, 3);
	return {
		inertia: A2($w0rm$elm_physics$Internal$Matrix3$sphereInertia, volume, radius),
		position: $w0rm$elm_physics$Internal$Vector3$zero,
		radius: radius,
		volume: volume
	};
};
var $ianmackenzie$elm_geometry$Sphere3d$centerPoint = function (_v0) {
	var properties = _v0.a;
	return properties.centerPoint;
};
var $ianmackenzie$elm_geometry$Sphere3d$radius = function (_v0) {
	var properties = _v0.a;
	return properties.radius;
};
var $w0rm$elm_physics$Physics$Shape$sphere = function (sphere3d) {
	var radius = $ianmackenzie$elm_units$Length$inMeters(
		$ianmackenzie$elm_geometry$Sphere3d$radius(sphere3d));
	var origin = $ianmackenzie$elm_geometry$Point3d$toMeters(
		$ianmackenzie$elm_geometry$Sphere3d$centerPoint(sphere3d));
	return $w0rm$elm_physics$Internal$Shape$Protected(
		$w0rm$elm_physics$Internal$Shape$Sphere(
			A2(
				$w0rm$elm_physics$Shapes$Sphere$placeIn,
				$w0rm$elm_physics$Internal$Transform3d$atPoint(origin),
				$w0rm$elm_physics$Shapes$Sphere$atOrigin(radius))));
};
var $w0rm$elm_physics$Physics$Body$withBehavior = F2(
	function (behavior, _v0) {
		var body = _v0.a;
		if (behavior.$ === 'Dynamic') {
			var mass_ = behavior.a;
			var _v2 = body.shapes;
			if (!_v2.b) {
				return $w0rm$elm_physics$Internal$Body$Protected(body);
			} else {
				if (!_v2.b.b) {
					var shape = _v2.a;
					if (shape.$ === 'Plane') {
						return $w0rm$elm_physics$Internal$Body$Protected(body);
					} else {
						return $w0rm$elm_physics$Internal$Body$Protected(
							$w0rm$elm_physics$Internal$Body$updateMassProperties(
								_Utils_update(
									body,
									{mass: mass_})));
					}
				} else {
					return $w0rm$elm_physics$Internal$Body$Protected(
						$w0rm$elm_physics$Internal$Body$updateMassProperties(
							_Utils_update(
								body,
								{mass: mass_})));
				}
			}
		} else {
			return $w0rm$elm_physics$Internal$Body$Protected(
				$w0rm$elm_physics$Internal$Body$updateMassProperties(
					_Utils_update(
						body,
						{mass: 0})));
		}
	});
var $author$project$Main$mouseImitationAtXYWithData = F3(
	function (x, y, data) {
		return A2(
			$w0rm$elm_physics$Physics$Body$moveTo,
			A3($ianmackenzie$elm_geometry$Point3d$meters, x, y, 0),
			A2(
				$w0rm$elm_physics$Physics$Body$withBehavior,
				$w0rm$elm_physics$Physics$Body$dynamic(
					$ianmackenzie$elm_units$Mass$kilograms(1.4)),
				A2(
					$w0rm$elm_physics$Physics$Body$compound,
					_List_fromArray(
						[
							$w0rm$elm_physics$Physics$Shape$sphere(
							$ianmackenzie$elm_geometry$Sphere3d$atOrigin(
								$ianmackenzie$elm_units$Length$meters(0.02)))
						]),
					$author$project$Main$MouseImitation(data))));
	});
var $author$project$Main$PlayerImitation = function (a) {
	return {$: 'PlayerImitation', a: a};
};
var $author$project$Main$playerDefaultDamping = {angular: 0.15, linear: 0.1};
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $w0rm$elm_physics$Physics$Body$withDamping = F2(
	function (_v0, _v1) {
		var linear = _v0.linear;
		var angular = _v0.angular;
		var body = _v1.a;
		return $w0rm$elm_physics$Internal$Body$Protected(
			_Utils_update(
				body,
				{
					angularDamping: A3($elm$core$Basics$clamp, 0, 1, angular),
					linearDamping: A3($elm$core$Basics$clamp, 0, 1, linear)
				}));
	});
var $author$project$Main$playerImitationWith = function (data) {
	return A2(
		$w0rm$elm_physics$Physics$Body$withDamping,
		$author$project$Main$playerDefaultDamping,
		A2(
			$w0rm$elm_physics$Physics$Body$withBehavior,
			$w0rm$elm_physics$Physics$Body$dynamic(
				$ianmackenzie$elm_units$Mass$kilograms(1)),
			A2(
				$w0rm$elm_physics$Physics$Body$compound,
				_List_fromArray(
					[
						$w0rm$elm_physics$Physics$Shape$sphere(
						$ianmackenzie$elm_geometry$Sphere3d$atOrigin(
							$ianmackenzie$elm_units$Length$meters(0.15)))
					]),
				$author$project$Main$PlayerImitation(data))));
};
var $author$project$Main$ratioWidthToHeight = 16 / 9;
var $author$project$Main$connectedWithMouseImitations = A2(
	$elm$core$List$indexedMap,
	F2(
		function (i, f) {
			return f(
				$elm$core$Maybe$Just(i));
		}),
	_List_fromArray(
		[
			function (id) {
			return _Utils_Tuple2(
				A3(
					$author$project$Main$mouseImitationAtXYWithData,
					14.6,
					-(0.7 * $author$project$Main$ratioWidthToHeight),
					{id: id}),
				A2(
					$w0rm$elm_physics$Physics$Body$moveTo,
					A3($ianmackenzie$elm_geometry$Point3d$meters, 13.9, -(1.1 * $author$project$Main$ratioWidthToHeight), 0),
					$author$project$Main$playerImitationWith(
						{id: id})));
		},
			function (id) {
			return _Utils_Tuple2(
				A3(
					$author$project$Main$mouseImitationAtXYWithData,
					14.5,
					-(1.4 * $author$project$Main$ratioWidthToHeight),
					{id: id}),
				A2(
					$w0rm$elm_physics$Physics$Body$moveTo,
					A3($ianmackenzie$elm_geometry$Point3d$meters, 13.6, -(1.4 * $author$project$Main$ratioWidthToHeight), 0),
					$author$project$Main$playerImitationWith(
						{id: id})));
		},
			function (id) {
			return _Utils_Tuple2(
				A3(
					$author$project$Main$mouseImitationAtXYWithData,
					14.05,
					-(0.6 * $author$project$Main$ratioWidthToHeight),
					{id: id}),
				A2(
					$w0rm$elm_physics$Physics$Body$moveTo,
					A3($ianmackenzie$elm_geometry$Point3d$meters, 13.75, -(0.9 * $author$project$Main$ratioWidthToHeight), 0),
					$author$project$Main$playerImitationWith(
						{id: id})));
		},
			function (id) {
			return _Utils_Tuple2(
				A3(
					$author$project$Main$mouseImitationAtXYWithData,
					13.7,
					-(1.1 * $author$project$Main$ratioWidthToHeight),
					{id: id}),
				A2(
					$w0rm$elm_physics$Physics$Body$moveTo,
					A3($ianmackenzie$elm_geometry$Point3d$meters, 13.9, -(1.3 * $author$project$Main$ratioWidthToHeight), 0),
					$author$project$Main$playerImitationWith(
						{id: id})));
		},
			function (id) {
			return _Utils_Tuple2(
				A3(
					$author$project$Main$mouseImitationAtXYWithData,
					12.6,
					-(0.7 * $author$project$Main$ratioWidthToHeight),
					{id: id}),
				A2(
					$w0rm$elm_physics$Physics$Body$moveTo,
					A3($ianmackenzie$elm_geometry$Point3d$meters, 12.9, -(1.1 * $author$project$Main$ratioWidthToHeight), 0),
					$author$project$Main$playerImitationWith(
						{id: id})));
		},
			function (id) {
			return _Utils_Tuple2(
				A3(
					$author$project$Main$mouseImitationAtXYWithData,
					13,
					-(1.4 * $author$project$Main$ratioWidthToHeight),
					{id: id}),
				A2(
					$w0rm$elm_physics$Physics$Body$moveTo,
					A3($ianmackenzie$elm_geometry$Point3d$meters, 12.6, -(1.4 * $author$project$Main$ratioWidthToHeight), 0),
					$author$project$Main$playerImitationWith(
						{id: id})));
		},
			function (id) {
			return _Utils_Tuple2(
				A3(
					$author$project$Main$mouseImitationAtXYWithData,
					13.05,
					-(0.4 * $author$project$Main$ratioWidthToHeight),
					{id: id}),
				A2(
					$w0rm$elm_physics$Physics$Body$moveTo,
					A3($ianmackenzie$elm_geometry$Point3d$meters, 12.75, -(0.9 * $author$project$Main$ratioWidthToHeight), 0),
					$author$project$Main$playerImitationWith(
						{id: id})));
		},
			function (id) {
			return _Utils_Tuple2(
				A3(
					$author$project$Main$mouseImitationAtXYWithData,
					13.3,
					-(1.5 * $author$project$Main$ratioWidthToHeight),
					{id: id}),
				A2(
					$w0rm$elm_physics$Physics$Body$moveTo,
					A3($ianmackenzie$elm_geometry$Point3d$meters, 12.9, -(1.3 * $author$project$Main$ratioWidthToHeight), 0),
					$author$project$Main$playerImitationWith(
						{id: id})));
		},
			function (id) {
			return _Utils_Tuple2(
				A3(
					$author$project$Main$mouseImitationAtXYWithData,
					11.6,
					-(0.7 * $author$project$Main$ratioWidthToHeight),
					{id: id}),
				A2(
					$w0rm$elm_physics$Physics$Body$moveTo,
					A3($ianmackenzie$elm_geometry$Point3d$meters, 11.9, -(1.1 * $author$project$Main$ratioWidthToHeight), 0),
					$author$project$Main$playerImitationWith(
						{id: id})));
		},
			function (id) {
			return _Utils_Tuple2(
				A3(
					$author$project$Main$mouseImitationAtXYWithData,
					12.6,
					-(0.2 * $author$project$Main$ratioWidthToHeight),
					{id: id}),
				A2(
					$w0rm$elm_physics$Physics$Body$moveTo,
					A3($ianmackenzie$elm_geometry$Point3d$meters, 12.9, -(1.1 * $author$project$Main$ratioWidthToHeight), 0),
					$author$project$Main$playerImitationWith(
						{id: id})));
		},
			function (id) {
			return _Utils_Tuple2(
				A3(
					$author$project$Main$mouseImitationAtXYWithData,
					14.3,
					-(1.4 * $author$project$Main$ratioWidthToHeight),
					{id: id}),
				A2(
					$w0rm$elm_physics$Physics$Body$moveTo,
					A3($ianmackenzie$elm_geometry$Point3d$meters, 14, -(1.4 * $author$project$Main$ratioWidthToHeight), 0),
					$author$project$Main$playerImitationWith(
						{id: id})));
		},
			function (id) {
			return _Utils_Tuple2(
				A3(
					$author$project$Main$mouseImitationAtXYWithData,
					12.05,
					-(0.4 * $author$project$Main$ratioWidthToHeight),
					{id: id}),
				A2(
					$w0rm$elm_physics$Physics$Body$moveTo,
					A3($ianmackenzie$elm_geometry$Point3d$meters, 11.75, -(0.9 * $author$project$Main$ratioWidthToHeight), 0),
					$author$project$Main$playerImitationWith(
						{id: id})));
		},
			function (id) {
			return _Utils_Tuple2(
				A3(
					$author$project$Main$mouseImitationAtXYWithData,
					13.3,
					-(1.5 * $author$project$Main$ratioWidthToHeight),
					{id: id}),
				A2(
					$w0rm$elm_physics$Physics$Body$moveTo,
					A3($ianmackenzie$elm_geometry$Point3d$meters, 12.9, -(1.3 * $author$project$Main$ratioWidthToHeight), 0),
					$author$project$Main$playerImitationWith(
						{id: id})));
		}
		]));
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$core$List$any = F2(
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
var $elm$core$Basics$not = _Basics_not;
var $w0rm$elm_physics$Internal$Constraint$Distance = function (a) {
	return {$: 'Distance', a: a};
};
var $w0rm$elm_physics$Internal$Constraint$Hinge = F4(
	function (a, b, c, d) {
		return {$: 'Hinge', a: a, b: b, c: c, d: d};
	});
var $w0rm$elm_physics$Internal$Constraint$Lock = F8(
	function (a, b, c, d, e, f, g, h) {
		return {$: 'Lock', a: a, b: b, c: c, d: d, e: e, f: f, g: g, h: h};
	});
var $w0rm$elm_physics$Internal$Constraint$PointToPoint = F2(
	function (a, b) {
		return {$: 'PointToPoint', a: a, b: b};
	});
var $w0rm$elm_physics$Internal$Transform3d$directionRelativeTo = F2(
	function (_v0, worldVector) {
		var localOrientation = _v0.b;
		return A2($w0rm$elm_physics$Internal$Transform3d$derotate, localOrientation, worldVector);
	});
var $w0rm$elm_physics$Internal$Constraint$relativeToCenterOfMass = F3(
	function (centerOfMassFrame3d1, centerOfMassFrame3d2, _v0) {
		var constraint = _v0.a;
		switch (constraint.$) {
			case 'PointToPoint':
				var pivot1 = constraint.a;
				var pivot2 = constraint.b;
				return A2(
					$w0rm$elm_physics$Internal$Constraint$PointToPoint,
					A2($w0rm$elm_physics$Internal$Transform3d$pointRelativeTo, centerOfMassFrame3d1, pivot1),
					A2($w0rm$elm_physics$Internal$Transform3d$pointRelativeTo, centerOfMassFrame3d2, pivot2));
			case 'Hinge':
				var pivot1 = constraint.a;
				var axis1 = constraint.b;
				var pivot2 = constraint.c;
				var axis2 = constraint.d;
				return A4(
					$w0rm$elm_physics$Internal$Constraint$Hinge,
					A2($w0rm$elm_physics$Internal$Transform3d$pointRelativeTo, centerOfMassFrame3d1, pivot1),
					A2($w0rm$elm_physics$Internal$Transform3d$directionRelativeTo, centerOfMassFrame3d1, axis1),
					A2($w0rm$elm_physics$Internal$Transform3d$pointRelativeTo, centerOfMassFrame3d2, pivot2),
					A2($w0rm$elm_physics$Internal$Transform3d$directionRelativeTo, centerOfMassFrame3d2, axis2));
			case 'Lock':
				var pivot1 = constraint.a;
				var x1 = constraint.b;
				var y1 = constraint.c;
				var z1 = constraint.d;
				var pivot2 = constraint.e;
				var x2 = constraint.f;
				var y2 = constraint.g;
				var z2 = constraint.h;
				return A8(
					$w0rm$elm_physics$Internal$Constraint$Lock,
					A2($w0rm$elm_physics$Internal$Transform3d$pointRelativeTo, centerOfMassFrame3d1, pivot1),
					A2($w0rm$elm_physics$Internal$Transform3d$directionRelativeTo, centerOfMassFrame3d1, x1),
					A2($w0rm$elm_physics$Internal$Transform3d$directionRelativeTo, centerOfMassFrame3d1, y1),
					A2($w0rm$elm_physics$Internal$Transform3d$directionRelativeTo, centerOfMassFrame3d1, z1),
					A2($w0rm$elm_physics$Internal$Transform3d$pointRelativeTo, centerOfMassFrame3d2, pivot2),
					A2($w0rm$elm_physics$Internal$Transform3d$directionRelativeTo, centerOfMassFrame3d2, x2),
					A2($w0rm$elm_physics$Internal$Transform3d$directionRelativeTo, centerOfMassFrame3d2, y2),
					A2($w0rm$elm_physics$Internal$Transform3d$directionRelativeTo, centerOfMassFrame3d2, z2));
			default:
				var length = constraint.a;
				return $w0rm$elm_physics$Internal$Constraint$Distance(length);
		}
	});
var $w0rm$elm_physics$Physics$World$constrainIf = F3(
	function (test, fn, _v0) {
		var world = _v0.a;
		var filteredBodies = A2(
			$elm$core$List$filter,
			function (body) {
				return test(
					$w0rm$elm_physics$Internal$Body$Protected(body));
			},
			world.bodies);
		var filteredConstraints = A2(
			$elm$core$List$filter,
			function (_v3) {
				var bodyId1 = _v3.bodyId1;
				var bodyId2 = _v3.bodyId2;
				return !A2(
					$elm$core$List$any,
					function (body) {
						return _Utils_eq(body.id, bodyId1) || _Utils_eq(body.id, bodyId2);
					},
					filteredBodies);
			},
			world.constraints);
		var addFor = F3(
			function (body1, body2, constraintGroup) {
				var _v2 = A2(
					fn,
					$w0rm$elm_physics$Internal$Body$Protected(body1),
					$w0rm$elm_physics$Internal$Body$Protected(body2));
				if (!_v2.b) {
					return constraintGroup;
				} else {
					var constraints = _v2;
					return A2(
						$elm$core$List$cons,
						{
							bodyId1: body1.id,
							bodyId2: body2.id,
							constraints: A3(
								$elm$core$List$foldl,
								F2(
									function (constraint, result) {
										return A2(
											$elm$core$List$cons,
											A3($w0rm$elm_physics$Internal$Constraint$relativeToCenterOfMass, body1.centerOfMassTransform3d, body2.centerOfMassTransform3d, constraint),
											result);
									}),
								_List_Nil,
								constraints)
						},
						constraintGroup);
				}
			});
		var addConstraintsHelp = F2(
			function (list, result) {
				addConstraintsHelp:
				while (true) {
					if (list.b) {
						var body1 = list.a;
						var rest = list.b;
						var $temp$list = rest,
							$temp$result = A3(
							$elm$core$List$foldl,
							F2(
								function (body2, constraints) {
									return A3(
										addFor,
										body2,
										body1,
										A3(addFor, body1, body2, constraints));
								}),
							result,
							rest);
						list = $temp$list;
						result = $temp$result;
						continue addConstraintsHelp;
					} else {
						return result;
					}
				}
			});
		return $w0rm$elm_physics$Internal$World$Protected(
			_Utils_update(
				world,
				{
					constraints: A2(addConstraintsHelp, filteredBodies, filteredConstraints)
				}));
	});
var $w0rm$elm_physics$Physics$World$constrain = $w0rm$elm_physics$Physics$World$constrainIf(
	$elm$core$Basics$always(true));
var $w0rm$elm_physics$Physics$Body$data = function (_v0) {
	var body = _v0.a;
	return body.data;
};
var $w0rm$elm_physics$Internal$Constraint$Protected = function (a) {
	return {$: 'Protected', a: a};
};
var $w0rm$elm_physics$Physics$Constraint$distance = function (length) {
	return $w0rm$elm_physics$Internal$Constraint$Protected(
		$w0rm$elm_physics$Internal$Constraint$Distance(
			$ianmackenzie$elm_units$Length$inMeters(length)));
};
var $w0rm$elm_physics$Physics$World$empty = $w0rm$elm_physics$Internal$World$Protected(
	{bodies: _List_Nil, constraints: _List_Nil, contactGroups: _List_Nil, freeIds: _List_Nil, gravity: $w0rm$elm_physics$Internal$Vector3$zero, nextBodyId: 0, simulatedBodies: $elm$core$Array$empty});
var $ianmackenzie$elm_geometry$Point3d$fromMeters = function (givenCoordinates) {
	return $ianmackenzie$elm_geometry$Geometry$Types$Point3d(givenCoordinates);
};
var $ianmackenzie$elm_units$Constants$meter = 1.0;
var $ianmackenzie$elm_units$Constants$second = 1;
var $ianmackenzie$elm_units$Constants$gee = (9.80665 * $ianmackenzie$elm_units$Constants$meter) / ($ianmackenzie$elm_units$Constants$second * $ianmackenzie$elm_units$Constants$second);
var $ianmackenzie$elm_units$Acceleration$metersPerSecondSquared = function (numMetersPerSecondSquared) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numMetersPerSecondSquared);
};
var $ianmackenzie$elm_units$Acceleration$gees = function (numGees) {
	return $ianmackenzie$elm_units$Acceleration$metersPerSecondSquared($ianmackenzie$elm_units$Constants$gee * numGees);
};
var $author$project$Main$DraggableBallWithCubeBehavior = function (a) {
	return {$: 'DraggableBallWithCubeBehavior', a: a};
};
var $ianmackenzie$elm_geometry$Direction3d$x = $ianmackenzie$elm_geometry$Direction3d$positiveX;
var $ianmackenzie$elm_geometry$Direction3d$y = $ianmackenzie$elm_geometry$Direction3d$positiveY;
var $ianmackenzie$elm_geometry$Direction3d$z = $ianmackenzie$elm_geometry$Direction3d$positiveZ;
var $ianmackenzie$elm_geometry$Frame3d$atOrigin = $ianmackenzie$elm_geometry$Geometry$Types$Frame3d(
	{originPoint: $ianmackenzie$elm_geometry$Point3d$origin, xDirection: $ianmackenzie$elm_geometry$Direction3d$x, yDirection: $ianmackenzie$elm_geometry$Direction3d$y, zDirection: $ianmackenzie$elm_geometry$Direction3d$z});
var $w0rm$elm_physics$Physics$Body$block = function (block3d) {
	return $w0rm$elm_physics$Physics$Body$compound(
		_List_fromArray(
			[
				$w0rm$elm_physics$Physics$Shape$block(block3d)
			]));
};
var $ianmackenzie$elm_geometry$Block3d$centeredOn = F2(
	function (givenAxes, _v0) {
		var xDimension = _v0.a;
		var yDimension = _v0.b;
		var zDimension = _v0.c;
		return $ianmackenzie$elm_geometry$Geometry$Types$Block3d(
			{
				axes: $ianmackenzie$elm_geometry$Frame3d$copy(givenAxes),
				dimensions: _Utils_Tuple3(
					$ianmackenzie$elm_units$Quantity$abs(xDimension),
					$ianmackenzie$elm_units$Quantity$abs(yDimension),
					$ianmackenzie$elm_units$Quantity$abs(zDimension))
			});
	});
var $author$project$Main$draggableBallWithCubeBehavior = function (data) {
	var colliderSideLength = A2($ianmackenzie$elm_units$Quantity$multiplyBy, 1.3, data.radius);
	return A2(
		$w0rm$elm_physics$Physics$Body$withBehavior,
		$w0rm$elm_physics$Physics$Body$dynamic(
			$ianmackenzie$elm_units$Mass$kilograms(1)),
		A2(
			$w0rm$elm_physics$Physics$Body$block,
			A2(
				$ianmackenzie$elm_geometry$Block3d$centeredOn,
				$ianmackenzie$elm_geometry$Frame3d$atOrigin,
				_Utils_Tuple3(colliderSideLength, colliderSideLength, colliderSideLength)),
			$author$project$Main$DraggableBallWithCubeBehavior(data)));
};
var $author$project$Main$DraggableBlock = function (a) {
	return {$: 'DraggableBlock', a: a};
};
var $author$project$Main$draggableBlockBody = function (dimensions) {
	return A2(
		$w0rm$elm_physics$Physics$Body$withBehavior,
		$w0rm$elm_physics$Physics$Body$dynamic(
			$ianmackenzie$elm_units$Mass$kilograms(100)),
		A2(
			$w0rm$elm_physics$Physics$Body$compound,
			_List_fromArray(
				[
					$w0rm$elm_physics$Physics$Shape$block(
					$author$project$Main$thickLinearBlock(dimensions))
				]),
			$author$project$Main$DraggableBlock(dimensions)));
};
var $author$project$Main$mouseImitationAtXY = F2(
	function (x, y) {
		return A3(
			$author$project$Main$mouseImitationAtXYWithData,
			x,
			y,
			{id: $elm$core$Maybe$Nothing});
	});
var $author$project$Main$playerImitation = $author$project$Main$playerImitationWith(
	{id: $elm$core$Maybe$Nothing});
var $ianmackenzie$elm_geometry$Axis3d$direction = function (_v0) {
	var axis = _v0.a;
	return axis.direction;
};
var $ianmackenzie$elm_units$Angle$inRadians = function (_v0) {
	var numRadians = _v0.a;
	return numRadians;
};
var $w0rm$elm_physics$Internal$Transform3d$originPoint = function (_v0) {
	var localOrigin = _v0.a;
	return localOrigin;
};
var $elm$core$Basics$cos = _Basics_cos;
var $elm$core$Basics$sin = _Basics_sin;
var $ianmackenzie$elm_geometry$Point3d$rotateAround = F3(
	function (_v0, _v1, _v2) {
		var axis = _v0.a;
		var angle = _v1.a;
		var p = _v2.a;
		var halfAngle = 0.5 * angle;
		var qw = $elm$core$Basics$cos(halfAngle);
		var sinHalfAngle = $elm$core$Basics$sin(halfAngle);
		var _v3 = axis.originPoint;
		var p0 = _v3.a;
		var deltaX = p.x - p0.x;
		var deltaY = p.y - p0.y;
		var deltaZ = p.z - p0.z;
		var _v4 = axis.direction;
		var d = _v4.a;
		var qx = d.x * sinHalfAngle;
		var wx = qw * qx;
		var xx = qx * qx;
		var qy = d.y * sinHalfAngle;
		var wy = qw * qy;
		var xy = qx * qy;
		var yy = qy * qy;
		var a22 = 1 - (2 * (xx + yy));
		var qz = d.z * sinHalfAngle;
		var wz = qw * qz;
		var a01 = 2 * (xy - wz);
		var a10 = 2 * (xy + wz);
		var xz = qx * qz;
		var a02 = 2 * (xz + wy);
		var a20 = 2 * (xz - wy);
		var yz = qy * qz;
		var a12 = 2 * (yz - wx);
		var a21 = 2 * (yz + wx);
		var zz = qz * qz;
		var a00 = 1 - (2 * (yy + zz));
		var a11 = 1 - (2 * (xx + zz));
		return $ianmackenzie$elm_geometry$Geometry$Types$Point3d(
			{x: ((p0.x + (a00 * deltaX)) + (a01 * deltaY)) + (a02 * deltaZ), y: ((p0.y + (a10 * deltaX)) + (a11 * deltaY)) + (a12 * deltaZ), z: ((p0.z + (a20 * deltaX)) + (a21 * deltaY)) + (a22 * deltaZ)});
	});
var $w0rm$elm_physics$Internal$Vector3$normalize = function (v3) {
	var len = $w0rm$elm_physics$Internal$Vector3$length(v3);
	return {x: v3.x / len, y: v3.y / len, z: v3.z / len};
};
var $w0rm$elm_physics$Internal$Transform3d$fromAngleAxis = F2(
	function (angle, axis) {
		var theta = angle * 0.5;
		var s = $elm$core$Basics$sin(theta);
		var c = $elm$core$Basics$cos(theta);
		var _v0 = $w0rm$elm_physics$Internal$Vector3$normalize(axis);
		var x = _v0.x;
		var y = _v0.y;
		var z = _v0.z;
		return A4($w0rm$elm_physics$Internal$Transform3d$Orientation3d, x * s, y * s, z * s, c);
	});
var $w0rm$elm_physics$Internal$Transform3d$rotateAroundOwn = F3(
	function (axis, angle, _v0) {
		var localOrigin = _v0.a;
		var localOrientation = _v0.b;
		return A2(
			$w0rm$elm_physics$Internal$Transform3d$Transform3d,
			localOrigin,
			A2(
				$w0rm$elm_physics$Internal$Transform3d$mul,
				A2($w0rm$elm_physics$Internal$Transform3d$fromAngleAxis, angle, axis),
				localOrientation));
	});
var $w0rm$elm_physics$Physics$Body$rotateAround = F3(
	function (axis, angle, _v0) {
		var body = _v0.a;
		var bodyCoordinatesTransform3d = A2(
			$w0rm$elm_physics$Internal$Transform3d$placeIn,
			body.transform3d,
			$w0rm$elm_physics$Internal$Transform3d$inverse(body.centerOfMassTransform3d));
		var rotatedOrigin = A3(
			$ianmackenzie$elm_geometry$Point3d$rotateAround,
			axis,
			angle,
			$ianmackenzie$elm_geometry$Point3d$fromMeters(
				$w0rm$elm_physics$Internal$Transform3d$originPoint(bodyCoordinatesTransform3d)));
		var newBodyCoordinatesTransform3d = A3(
			$w0rm$elm_physics$Internal$Transform3d$rotateAroundOwn,
			$ianmackenzie$elm_geometry$Direction3d$unwrap(
				$ianmackenzie$elm_geometry$Axis3d$direction(axis)),
			$ianmackenzie$elm_units$Angle$inRadians(angle),
			A2(
				$w0rm$elm_physics$Internal$Transform3d$moveTo,
				$ianmackenzie$elm_geometry$Point3d$toMeters(rotatedOrigin),
				bodyCoordinatesTransform3d));
		var newTransform3d = A2($w0rm$elm_physics$Internal$Transform3d$placeIn, newBodyCoordinatesTransform3d, body.centerOfMassTransform3d);
		return $w0rm$elm_physics$Internal$Body$Protected(
			_Utils_update(
				body,
				{
					invInertiaWorld: A2($w0rm$elm_physics$Internal$Transform3d$invertedInertiaRotateIn, newTransform3d, body.invInertia),
					transform3d: newTransform3d,
					worldShapes: A2($w0rm$elm_physics$Internal$Shape$shapesPlaceIn, newTransform3d, body.shapes)
				}));
	});
var $ianmackenzie$elm_units$Angle$turns = function (numTurns) {
	return $ianmackenzie$elm_units$Angle$radians((2 * $elm$core$Basics$pi) * numTurns);
};
var $ianmackenzie$elm_geometry$Geometry$Types$Axis3d = function (a) {
	return {$: 'Axis3d', a: a};
};
var $ianmackenzie$elm_geometry$Axis3d$through = F2(
	function (givenPoint, givenDirection) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Axis3d(
			{direction: givenDirection, originPoint: givenPoint});
	});
var $ianmackenzie$elm_geometry$Axis3d$z = A2($ianmackenzie$elm_geometry$Axis3d$through, $ianmackenzie$elm_geometry$Point3d$origin, $ianmackenzie$elm_geometry$Direction3d$z);
var $author$project$Main$mainRoute = _List_fromArray(
	[
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 0, -($author$project$Main$ratioWidthToHeight / 2), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 0, $author$project$Main$ratioWidthToHeight / 2, 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 1, -($author$project$Main$ratioWidthToHeight / 2), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 1, $author$project$Main$ratioWidthToHeight / 2, 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 0.9, $author$project$Main$ratioWidthToHeight / 4, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(-0.3),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 2)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 1.3, -($author$project$Main$ratioWidthToHeight / 4), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.3),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 2)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 2, -($author$project$Main$ratioWidthToHeight / 2), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 2, $author$project$Main$ratioWidthToHeight / 2, 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 2.55, -($author$project$Main$ratioWidthToHeight / 2), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.3),
			$author$project$Main$draggableBlockBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 2)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 2.54, $author$project$Main$ratioWidthToHeight / 2, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.2),
			$author$project$Main$draggableBlockBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 2)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 2.7, 0, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.2),
			$author$project$Main$draggableBlockBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 4)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 2.7, -($author$project$Main$ratioWidthToHeight / 4), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.3),
			$author$project$Main$draggableBlockBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 4)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 2.35, 0, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$draggableBlockBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 2)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 2.1, -($author$project$Main$ratioWidthToHeight * 0.35), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.11),
			$author$project$Main$draggableBlockBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight * 0.24)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 2.1, $author$project$Main$ratioWidthToHeight * 0.32, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.37),
			$author$project$Main$draggableBlockBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight * 0.24)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 3, -(1.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 2.5, (-1) * $author$project$Main$ratioWidthToHeight, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 3.5, (-1) * $author$project$Main$ratioWidthToHeight, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 3.25, -(0.7 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$draggableBlockBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 3.25, -(0.9 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$draggableBlockBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 3.25, -(1.1 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$draggableBlockBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 3.25, -(1.3 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$draggableBlockBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 3, 1.5 * $author$project$Main$ratioWidthToHeight, 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 2.5, 1 * $author$project$Main$ratioWidthToHeight, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 3.5, 1 * $author$project$Main$ratioWidthToHeight, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 3.25, 0.9 * $author$project$Main$ratioWidthToHeight, 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 4)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 2.75, 1.2 * $author$project$Main$ratioWidthToHeight, 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 4)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 4, -($author$project$Main$ratioWidthToHeight / 2), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 4, $author$project$Main$ratioWidthToHeight / 2, 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5, $author$project$Main$ratioWidthToHeight / 2, 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 4.92, -(0.345 * $author$project$Main$ratioWidthToHeight), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.11),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters(1)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5, 0.35 * $author$project$Main$ratioWidthToHeight, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.39),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters(1)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6, -($author$project$Main$ratioWidthToHeight / 2), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6, $author$project$Main$ratioWidthToHeight / 2, 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6.5, 0, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6, -(0.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6, -(1.5 * $author$project$Main$ratioWidthToHeight), -0.298),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5, -(1.5 * $author$project$Main$ratioWidthToHeight), -0.298),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 4.5, -$author$project$Main$ratioWidthToHeight, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.4, -(0.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(0.1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.4, -(0.5 * $author$project$Main$ratioWidthToHeight), 0.2),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.09)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.3, -(0.55 * $author$project$Main$ratioWidthToHeight), 0.2),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.09)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.2, -(0.6 * $author$project$Main$ratioWidthToHeight), 0.2),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.09)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.9, -(1.25 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.6, -(1.35 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.07)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.9, -(1 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6.1, -(0.9 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.07)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6.3, -(0.7 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.07)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.9, -(1.75 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.6, -(1.85 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.07)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6, -(2.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5, -(2.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6.5, -(2 * $author$project$Main$ratioWidthToHeight), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 4.5, -(2 * $author$project$Main$ratioWidthToHeight), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.5, -(0.65 * $author$project$Main$ratioWidthToHeight), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.5, -(0.8 * $author$project$Main$ratioWidthToHeight), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.5, -(0.95 * $author$project$Main$ratioWidthToHeight), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.5, -(1.1 * $author$project$Main$ratioWidthToHeight), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.5, -(1.25 * $author$project$Main$ratioWidthToHeight), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 5.5, -(1.4 * $author$project$Main$ratioWidthToHeight), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6.25, -(1.7 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$draggableBlockBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6.25, -(1.9 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$draggableBlockBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6.25, -(2.1 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$draggableBlockBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6.25, -(2.3 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$draggableBlockBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6, -(1.7 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$draggableBlockBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6, -(1.9 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$draggableBlockBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6, -(2.1 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$draggableBlockBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6, -(2.3 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$draggableBlockBody(
			{
				length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight / 10)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 7, -(0.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 7, -(1.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6.9, -(1.25 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 7.8, -(1.35 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.07)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 7.9, -(1 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 7.1, -(0.9 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.07)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 6.3, -(0.7 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.07)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 8, -(0.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 8, -(1.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 7.9, -(1.25 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.11)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 8.8, -(1.35 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.09)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 8.9, -(1 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.05)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 8.1, -(0.86 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.07)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 8.3, -(0.6 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.18)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 9, -(0.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 9, -(1.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 8.9, -(1.25 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 9.8, -(1.35 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.07)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 9.9, -(1 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 9.1, -(0.9 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.07)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 10.3, -(0.7 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.07)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 10, -(0.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 10, -(1.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 9.9, -(1.25 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.11)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 10.8, -(1.35 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.09)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 10.9, -(1 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.05)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 10.1, -(0.86 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.07)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 10.3, -(0.6 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.18)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 11, -(0.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 11, -(0.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 11, -(1.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12, -(0.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12, -(1.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 8.5, -(1 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 9.8, -(0.7 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 10.2, -(1.3 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 10.6, -(1 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 10.75, -(0.7 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 10.9, -(1.3 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 11, -(1.05 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 11.15, -(0.6 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 11.3, -(1.2 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 11.2, -(1.3 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 11.6, -(1 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 11.75, -(0.7 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 11.9, -(1.3 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12, -(1.05 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12.15, -(0.6 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12.3, -(1.2 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 11.6, -(1 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 11.75, -(0.7 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12.9, -(1.1 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12.6, -(1.4 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12.75, -(0.9 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12.9, -(1.3 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$playerImitation),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 13, -(2.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 13, $author$project$Main$ratioWidthToHeight / 2, 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 14, -(2.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 14, $author$project$Main$ratioWidthToHeight / 2, 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12.5, -(2 * $author$project$Main$ratioWidthToHeight), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12.5, 0 * $author$project$Main$ratioWidthToHeight, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 14.5, -(2 * $author$project$Main$ratioWidthToHeight), 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight)
				}))),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 14.5, 0 * $author$project$Main$ratioWidthToHeight, 0),
		A3(
			$w0rm$elm_physics$Physics$Body$rotateAround,
			$ianmackenzie$elm_geometry$Axis3d$z,
			$ianmackenzie$elm_units$Angle$turns(0.25),
			$author$project$Main$blockingImmovableWallBody(
				{
					length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight)
				}))),
		A2($author$project$Main$mouseImitationAtXY, 14.8, -(0.9 * $author$project$Main$ratioWidthToHeight)),
		A2($author$project$Main$mouseImitationAtXY, 14.2, -(1.2 * $author$project$Main$ratioWidthToHeight)),
		A2($author$project$Main$mouseImitationAtXY, 14, -(0.9 * $author$project$Main$ratioWidthToHeight)),
		A2($author$project$Main$mouseImitationAtXY, 13.6, -(0.7 * $author$project$Main$ratioWidthToHeight)),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 14.2, -(2.2 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.11)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 14.2, 0.4 * $author$project$Main$ratioWidthToHeight, 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.09)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 13.9, -(2 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.11)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 13.8, -(0 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.09)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 13.2, -(2.4 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.11)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 13.4, 0.4 * $author$project$Main$ratioWidthToHeight, 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.09)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12.9, -(2 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.11)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12.8, -(0 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.09)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 14.2, -(1.7 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.11)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 14.2, -(0.1 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.09)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 13.9, -(1.5 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.11)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 13.8, -(0.5 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.09)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 13.2, -(1.9 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.11)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 13.4, -(0.1 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.09)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12.9, -(1.5 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.11)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 12.8, -(0.5 * $author$project$Main$ratioWidthToHeight), 0.3),
		$author$project$Main$draggableBallWithCubeBehavior(
			{
				radius: $ianmackenzie$elm_units$Length$meters(0.09)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 15, -($author$project$Main$ratioWidthToHeight / 2), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 15, -(1.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 16, -($author$project$Main$ratioWidthToHeight / 2), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			})),
		A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		A3($ianmackenzie$elm_geometry$Point3d$meters, 16, -(1.5 * $author$project$Main$ratioWidthToHeight), 0),
		$author$project$Main$blockingImmovableWallBody(
			{
				length: $ianmackenzie$elm_units$Length$meters(1)
			}))
	]);
var $ianmackenzie$elm_geometry$Geometry$Types$Vector3d = function (a) {
	return {$: 'Vector3d', a: a};
};
var $ianmackenzie$elm_geometry$Vector3d$meters = F3(
	function (x, y, z) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Vector3d(
			{x: x, y: y, z: z});
	});
var $w0rm$elm_physics$Physics$Body$plane = $w0rm$elm_physics$Physics$Body$compound(
	_List_fromArray(
		[
			$w0rm$elm_physics$Internal$Shape$Protected(
			$w0rm$elm_physics$Internal$Shape$Plane(
				{normal: $w0rm$elm_physics$Internal$Vector3$zAxis, position: $w0rm$elm_physics$Internal$Vector3$zero}))
		]));
var $ianmackenzie$elm_geometry$Vector3d$toMeters = function (_v0) {
	var vectorComponents = _v0.a;
	return vectorComponents;
};
var $w0rm$elm_physics$Internal$Transform3d$translateBy = F2(
	function (vector, _v0) {
		var localOrigin = _v0.a;
		var localOrientation = _v0.b;
		return A2(
			$w0rm$elm_physics$Internal$Transform3d$Transform3d,
			A2($w0rm$elm_physics$Internal$Vector3$add, vector, localOrigin),
			localOrientation);
	});
var $w0rm$elm_physics$Physics$Body$translateBy = F2(
	function (vector3d, _v0) {
		var body = _v0.a;
		var bodyCoordinatesTransform3d = A2(
			$w0rm$elm_physics$Internal$Transform3d$placeIn,
			body.transform3d,
			$w0rm$elm_physics$Internal$Transform3d$inverse(body.centerOfMassTransform3d));
		var newTransform3d = A2(
			$w0rm$elm_physics$Internal$Transform3d$placeIn,
			A2(
				$w0rm$elm_physics$Internal$Transform3d$translateBy,
				$ianmackenzie$elm_geometry$Vector3d$toMeters(vector3d),
				bodyCoordinatesTransform3d),
			body.centerOfMassTransform3d);
		return $w0rm$elm_physics$Internal$Body$Protected(
			_Utils_update(
				body,
				{
					transform3d: newTransform3d,
					worldShapes: A2($w0rm$elm_physics$Internal$Shape$shapesPlaceIn, newTransform3d, body.shapes)
				}));
	});
var $ianmackenzie$elm_units$Acceleration$inMetersPerSecondSquared = function (_v0) {
	var numMetersPerSecondSquared = _v0.a;
	return numMetersPerSecondSquared;
};
var $w0rm$elm_physics$Physics$World$withGravity = F3(
	function (acceleration, direction, _v0) {
		var world = _v0.a;
		return $w0rm$elm_physics$Internal$World$Protected(
			_Utils_update(
				world,
				{
					gravity: A2(
						$w0rm$elm_physics$Internal$Vector3$scale,
						$ianmackenzie$elm_units$Acceleration$inMetersPerSecondSquared(acceleration),
						$ianmackenzie$elm_geometry$Direction3d$unwrap(direction))
				}));
	});
var $author$project$Main$worldAddBodies = F2(
	function (additionalBodies, world) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (body, worldSoFar) {
					return A2($w0rm$elm_physics$Physics$World$add, body, worldSoFar);
				}),
			world,
			additionalBodies);
	});
var $author$project$Main$initialWorld = A2(
	$w0rm$elm_physics$Physics$World$add,
	A2(
		$w0rm$elm_physics$Physics$Body$moveTo,
		$ianmackenzie$elm_geometry$Point3d$fromMeters(
			{x: 17, y: (-1) * $author$project$Main$ratioWidthToHeight, z: 0.22}),
		A2(
			$w0rm$elm_physics$Physics$Body$withDamping,
			$author$project$Main$playerDefaultDamping,
			A2(
				$w0rm$elm_physics$Physics$Body$withBehavior,
				$w0rm$elm_physics$Physics$Body$dynamic(
					$ianmackenzie$elm_units$Mass$kilograms(20)),
				A2($w0rm$elm_physics$Physics$Body$compound, _List_Nil, $author$project$Main$PlayerPast)))),
	A2(
		$w0rm$elm_physics$Physics$World$add,
		A2(
			$w0rm$elm_physics$Physics$Body$moveTo,
			$ianmackenzie$elm_geometry$Point3d$fromMeters(
				{x: 0, y: 0, z: 0.22}),
			A2(
				$w0rm$elm_physics$Physics$Body$withDamping,
				$author$project$Main$playerDefaultDamping,
				A2(
					$w0rm$elm_physics$Physics$Body$withBehavior,
					$w0rm$elm_physics$Physics$Body$dynamic(
						$ianmackenzie$elm_units$Mass$kilograms(1)),
					A2(
						$w0rm$elm_physics$Physics$Body$compound,
						_List_fromArray(
							[
								$w0rm$elm_physics$Physics$Shape$sphere(
								$ianmackenzie$elm_geometry$Sphere3d$atOrigin(
									$ianmackenzie$elm_units$Length$meters(0.15)))
							]),
						$author$project$Main$Player)))),
		A2(
			$w0rm$elm_physics$Physics$World$add,
			$w0rm$elm_physics$Physics$Body$plane($author$project$Main$Floor),
			A2(
				$author$project$Main$worldAddBodies,
				A2(
					$elm$core$List$map,
					$w0rm$elm_physics$Physics$Body$translateBy(
						A3($ianmackenzie$elm_geometry$Vector3d$meters, 17, -(1 * $author$project$Main$ratioWidthToHeight), 0)),
					$author$project$Main$mainRoute),
				A2(
					$w0rm$elm_physics$Physics$World$constrain,
					F2(
						function (body0, body1) {
							var _v0 = _Utils_Tuple2(
								$w0rm$elm_physics$Physics$Body$data(body0),
								$w0rm$elm_physics$Physics$Body$data(body1));
							if ((_v0.a.$ === 'MouseImitation') && (_v0.b.$ === 'PlayerImitation')) {
								var mouseImitationData = _v0.a.a;
								var playerImitationData = _v0.b.a;
								var _v1 = _Utils_Tuple2(mouseImitationData.id, playerImitationData.id);
								if ((_v1.a.$ === 'Just') && (_v1.b.$ === 'Just')) {
									var mouseId = _v1.a.a;
									var playerId = _v1.b.a;
									return _Utils_eq(mouseId, playerId) ? _List_fromArray(
										[
											$w0rm$elm_physics$Physics$Constraint$distance(
											$ianmackenzie$elm_units$Length$meters(0.25))
										]) : _List_Nil;
								} else {
									return _List_Nil;
								}
							} else {
								return _List_Nil;
							}
						}),
					A2(
						$author$project$Main$worldAddBodies,
						A2($elm$core$List$map, $elm$core$Tuple$second, $author$project$Main$connectedWithMouseImitations),
						A2(
							$author$project$Main$worldAddBodies,
							A2($elm$core$List$map, $elm$core$Tuple$first, $author$project$Main$connectedWithMouseImitations),
							A2(
								$author$project$Main$worldAddBodies,
								$author$project$Main$mainRoute,
								A2(
									$w0rm$elm_physics$Physics$World$add,
									A2(
										$w0rm$elm_physics$Physics$Body$moveTo,
										A3($ianmackenzie$elm_geometry$Point3d$meters, -(1 / 2), 0, 0),
										A3(
											$w0rm$elm_physics$Physics$Body$rotateAround,
											$ianmackenzie$elm_geometry$Axis3d$z,
											$ianmackenzie$elm_units$Angle$turns(1 / 4),
											$author$project$Main$blockingImmovableWallBody(
												{
													length: $ianmackenzie$elm_units$Length$meters($author$project$Main$ratioWidthToHeight)
												}))),
									A3(
										$w0rm$elm_physics$Physics$World$withGravity,
										$ianmackenzie$elm_units$Acceleration$gees(1),
										$ianmackenzie$elm_geometry$Direction3d$negativeZ,
										$w0rm$elm_physics$Physics$World$empty))))))))));
var $ianmackenzie$elm_3d_camera$Camera3d$Types$Viewpoint3d = function (a) {
	return {$: 'Viewpoint3d', a: a};
};
var $ianmackenzie$elm_geometry$Vector3d$cross = F2(
	function (_v0, _v1) {
		var v2 = _v0.a;
		var v1 = _v1.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Vector3d(
			{x: (v1.y * v2.z) - (v1.z * v2.y), y: (v1.z * v2.x) - (v1.x * v2.z), z: (v1.x * v2.y) - (v1.y * v2.x)});
	});
var $ianmackenzie$elm_geometry$Vector3d$direction = function (_v0) {
	var v = _v0.a;
	var largestComponent = A2(
		$elm$core$Basics$max,
		$elm$core$Basics$abs(v.x),
		A2(
			$elm$core$Basics$max,
			$elm$core$Basics$abs(v.y),
			$elm$core$Basics$abs(v.z)));
	if (!largestComponent) {
		return $elm$core$Maybe$Nothing;
	} else {
		var scaledZ = v.z / largestComponent;
		var scaledY = v.y / largestComponent;
		var scaledX = v.x / largestComponent;
		var scaledLength = $elm$core$Basics$sqrt(((scaledX * scaledX) + (scaledY * scaledY)) + (scaledZ * scaledZ));
		return $elm$core$Maybe$Just(
			$ianmackenzie$elm_geometry$Geometry$Types$Direction3d(
				{x: scaledX / scaledLength, y: scaledY / scaledLength, z: scaledZ / scaledLength}));
	}
};
var $ianmackenzie$elm_geometry$Vector3d$from = F2(
	function (_v0, _v1) {
		var p1 = _v0.a;
		var p2 = _v1.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Vector3d(
			{x: p2.x - p1.x, y: p2.y - p1.y, z: p2.z - p1.z});
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $ianmackenzie$elm_geometry$Vector3d$dot = F2(
	function (_v0, _v1) {
		var v2 = _v0.a;
		var v1 = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(((v1.x * v2.x) + (v1.y * v2.y)) + (v1.z * v2.z));
	});
var $ianmackenzie$elm_units$Quantity$greaterThan = F2(
	function (_v0, _v1) {
		var y = _v0.a;
		var x = _v1.a;
		return _Utils_cmp(x, y) > 0;
	});
var $ianmackenzie$elm_units$Quantity$lessThan = F2(
	function (_v0, _v1) {
		var y = _v0.a;
		var x = _v1.a;
		return _Utils_cmp(x, y) < 0;
	});
var $ianmackenzie$elm_geometry$Vector3d$minus = F2(
	function (_v0, _v1) {
		var v2 = _v0.a;
		var v1 = _v1.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Vector3d(
			{x: v1.x - v2.x, y: v1.y - v2.y, z: v1.z - v2.z});
	});
var $ianmackenzie$elm_geometry$Vector3d$projectionIn = F2(
	function (_v0, _v1) {
		var d = _v0.a;
		var v = _v1.a;
		var projectedLength = ((v.x * d.x) + (v.y * d.y)) + (v.z * d.z);
		return $ianmackenzie$elm_geometry$Geometry$Types$Vector3d(
			{x: d.x * projectedLength, y: d.y * projectedLength, z: d.z * projectedLength});
	});
var $ianmackenzie$elm_geometry$Vector3d$reverse = function (_v0) {
	var v = _v0.a;
	return $ianmackenzie$elm_geometry$Geometry$Types$Vector3d(
		{x: -v.x, y: -v.y, z: -v.z});
};
var $ianmackenzie$elm_geometry$Vector3d$zero = $ianmackenzie$elm_geometry$Geometry$Types$Vector3d(
	{x: 0, y: 0, z: 0});
var $ianmackenzie$elm_geometry$Direction3d$orthonormalize = F3(
	function (xVector, xyVector, xyzVector) {
		return A2(
			$elm$core$Maybe$andThen,
			function (xDirection) {
				var yVector = A2(
					$ianmackenzie$elm_geometry$Vector3d$minus,
					A2($ianmackenzie$elm_geometry$Vector3d$projectionIn, xDirection, xyVector),
					xyVector);
				return A2(
					$elm$core$Maybe$andThen,
					function (yDirection) {
						var rightHandedZVector = A2($ianmackenzie$elm_geometry$Vector3d$cross, xyVector, xVector);
						var tripleProduct = A2($ianmackenzie$elm_geometry$Vector3d$dot, xyzVector, rightHandedZVector);
						var zVector = A2($ianmackenzie$elm_units$Quantity$greaterThan, $ianmackenzie$elm_units$Quantity$zero, tripleProduct) ? rightHandedZVector : (A2($ianmackenzie$elm_units$Quantity$lessThan, $ianmackenzie$elm_units$Quantity$zero, tripleProduct) ? $ianmackenzie$elm_geometry$Vector3d$reverse(rightHandedZVector) : $ianmackenzie$elm_geometry$Vector3d$zero);
						return A2(
							$elm$core$Maybe$map,
							function (zDirection) {
								return _Utils_Tuple3(xDirection, yDirection, zDirection);
							},
							$ianmackenzie$elm_geometry$Vector3d$direction(zVector));
					},
					$ianmackenzie$elm_geometry$Vector3d$direction(yVector));
			},
			$ianmackenzie$elm_geometry$Vector3d$direction(xVector));
	});
var $ianmackenzie$elm_geometry$Direction3d$perpendicularTo = function (_v0) {
	var d = _v0.a;
	var absZ = $elm$core$Basics$abs(d.z);
	var absY = $elm$core$Basics$abs(d.y);
	var absX = $elm$core$Basics$abs(d.x);
	if (_Utils_cmp(absX, absY) < 1) {
		if (_Utils_cmp(absX, absZ) < 1) {
			var scale = $elm$core$Basics$sqrt((d.z * d.z) + (d.y * d.y));
			return $ianmackenzie$elm_geometry$Geometry$Types$Direction3d(
				{x: 0, y: (-d.z) / scale, z: d.y / scale});
		} else {
			var scale = $elm$core$Basics$sqrt((d.y * d.y) + (d.x * d.x));
			return $ianmackenzie$elm_geometry$Geometry$Types$Direction3d(
				{x: (-d.y) / scale, y: d.x / scale, z: 0});
		}
	} else {
		if (_Utils_cmp(absY, absZ) < 1) {
			var scale = $elm$core$Basics$sqrt((d.z * d.z) + (d.x * d.x));
			return $ianmackenzie$elm_geometry$Geometry$Types$Direction3d(
				{x: d.z / scale, y: 0, z: (-d.x) / scale});
		} else {
			var scale = $elm$core$Basics$sqrt((d.x * d.x) + (d.y * d.y));
			return $ianmackenzie$elm_geometry$Geometry$Types$Direction3d(
				{x: (-d.y) / scale, y: d.x / scale, z: 0});
		}
	}
};
var $ianmackenzie$elm_geometry$Direction3d$perpendicularBasis = function (direction) {
	var xDirection = $ianmackenzie$elm_geometry$Direction3d$perpendicularTo(direction);
	var _v0 = xDirection;
	var dX = _v0.a;
	var _v1 = direction;
	var d = _v1.a;
	var yDirection = $ianmackenzie$elm_geometry$Geometry$Types$Direction3d(
		{x: (d.y * dX.z) - (d.z * dX.y), y: (d.z * dX.x) - (d.x * dX.z), z: (d.x * dX.y) - (d.y * dX.x)});
	return _Utils_Tuple2(xDirection, yDirection);
};
var $ianmackenzie$elm_geometry$Direction3d$toVector = function (_v0) {
	var directionComponents = _v0.a;
	return $ianmackenzie$elm_geometry$Geometry$Types$Vector3d(directionComponents);
};
var $ianmackenzie$elm_geometry$Frame3d$withZDirection = F2(
	function (givenZDirection, givenOrigin) {
		var _v0 = $ianmackenzie$elm_geometry$Direction3d$perpendicularBasis(givenZDirection);
		var computedXDirection = _v0.a;
		var computedYDirection = _v0.b;
		return $ianmackenzie$elm_geometry$Frame3d$unsafe(
			{originPoint: givenOrigin, xDirection: computedXDirection, yDirection: computedYDirection, zDirection: givenZDirection});
	});
var $ianmackenzie$elm_3d_camera$Viewpoint3d$lookAt = function (_arguments) {
	var zVector = A2($ianmackenzie$elm_geometry$Vector3d$from, _arguments.focalPoint, _arguments.eyePoint);
	var yVector = $ianmackenzie$elm_geometry$Direction3d$toVector(_arguments.upDirection);
	var xVector = A2($ianmackenzie$elm_geometry$Vector3d$cross, zVector, yVector);
	var _v0 = A3($ianmackenzie$elm_geometry$Direction3d$orthonormalize, zVector, yVector, xVector);
	if (_v0.$ === 'Just') {
		var _v1 = _v0.a;
		var normalizedZDirection = _v1.a;
		var normalizedYDirection = _v1.b;
		var normalizedXDirection = _v1.c;
		return $ianmackenzie$elm_3d_camera$Camera3d$Types$Viewpoint3d(
			$ianmackenzie$elm_geometry$Frame3d$unsafe(
				{originPoint: _arguments.eyePoint, xDirection: normalizedXDirection, yDirection: normalizedYDirection, zDirection: normalizedZDirection}));
	} else {
		var _v2 = $ianmackenzie$elm_geometry$Vector3d$direction(zVector);
		if (_v2.$ === 'Just') {
			var zDirection = _v2.a;
			return $ianmackenzie$elm_3d_camera$Camera3d$Types$Viewpoint3d(
				A2($ianmackenzie$elm_geometry$Frame3d$withZDirection, zDirection, _arguments.eyePoint));
		} else {
			var _v3 = $ianmackenzie$elm_geometry$Direction3d$perpendicularBasis(_arguments.upDirection);
			var arbitraryZDirection = _v3.a;
			var arbitraryXDirection = _v3.b;
			return $ianmackenzie$elm_3d_camera$Camera3d$Types$Viewpoint3d(
				$ianmackenzie$elm_geometry$Frame3d$unsafe(
					{originPoint: _arguments.eyePoint, xDirection: arbitraryXDirection, yDirection: _arguments.upDirection, zDirection: arbitraryZDirection}));
		}
	}
};
var $ianmackenzie$elm_3d_camera$Camera3d$Types$Camera3d = function (a) {
	return {$: 'Camera3d', a: a};
};
var $ianmackenzie$elm_3d_camera$Camera3d$Types$Perspective = function (a) {
	return {$: 'Perspective', a: a};
};
var $ianmackenzie$elm_units$Quantity$half = function (_v0) {
	var value = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(0.5 * value);
};
var $elm$core$Basics$tan = _Basics_tan;
var $ianmackenzie$elm_units$Angle$tan = function (_v0) {
	var angle = _v0.a;
	return $elm$core$Basics$tan(angle);
};
var $ianmackenzie$elm_3d_camera$Camera3d$perspective = function (_arguments) {
	var halfFieldOfView = $ianmackenzie$elm_units$Quantity$half(
		$ianmackenzie$elm_units$Quantity$abs(_arguments.verticalFieldOfView));
	var frustumSlope = $ianmackenzie$elm_units$Angle$tan(halfFieldOfView);
	return $ianmackenzie$elm_3d_camera$Camera3d$Types$Camera3d(
		{
			projection: $ianmackenzie$elm_3d_camera$Camera3d$Types$Perspective(frustumSlope),
			viewpoint: _arguments.viewpoint
		});
};
var $author$project$Reaction$to = function (stateAltered) {
	return {effects: _List_Nil, state: stateAltered};
};
var $author$project$Main$init = function (_v0) {
	return A2(
		$author$project$Reaction$effectsAdd,
		A2($elm$core$List$map, $author$project$Main$LoadAudio, $author$project$Main$audioKinds),
		A2(
			$author$project$Reaction$effectsAdd,
			_List_fromArray(
				[$author$project$Main$RequestInitialRandomSeed, $author$project$Main$RequestInitialTime, $author$project$Main$GameRequestInitialWindowSize]),
			$author$project$Reaction$to(
				{
					audio: $author$project$Main$eachAudio(
						$elm$core$Result$Err($MartinSStewart$elm_audio$Audio$UnknownError)),
					audioTimes: $author$project$Main$eachAudio(_List_Nil),
					camera: $ianmackenzie$elm_3d_camera$Camera3d$perspective(
						{
							verticalFieldOfView: $ianmackenzie$elm_units$Angle$degrees(60),
							viewpoint: $ianmackenzie$elm_3d_camera$Viewpoint3d$lookAt(
								{
									eyePoint: A3($ianmackenzie$elm_geometry$Point3d$meters, 0, 0, $author$project$Main$cameraHeight),
									focalPoint: $ianmackenzie$elm_geometry$Point3d$origin,
									upDirection: $ianmackenzie$elm_geometry$Direction3d$positiveZ
								})
						}),
					initialTime: $elm$time$Time$millisToPosix(-1),
					keysPressed: _List_Nil,
					lastTick: $elm$time$Time$millisToPosix(-1),
					maybeRaycastResult: $elm$core$Maybe$Nothing,
					playerPast: $author$project$Main$PlayerLeavingTrail(_List_Nil),
					randomSeed: $elm$random$Random$initialSeed(1635127483),
					windowSize: {height: 0, width: 0},
					world: $author$project$Main$initialWorld
				})));
};
var $author$project$Main$AudioLoaded = function (a) {
	return {$: 'AudioLoaded', a: a};
};
var $author$project$Main$GameWindowSized = function (a) {
	return {$: 'GameWindowSized', a: a};
};
var $author$project$Main$InitialRandomSeedReceived = function (a) {
	return {$: 'InitialRandomSeedReceived', a: a};
};
var $author$project$Main$InitialTimeReceived = function (a) {
	return {$: 'InitialTimeReceived', a: a};
};
var $author$project$Reaction$audioCommands = function (audioCommandList) {
	return {audioCommands: audioCommandList, commands: _List_Nil};
};
var $author$project$Main$audioPieceToName = function (audioPiece) {
	if (audioPiece.$ === 'AudioRoomChange') {
		return 'room-change';
	} else {
		return 'music';
	}
};
var $author$project$Reaction$commands = function (commandList) {
	return {audioCommands: _List_Nil, commands: commandList};
};
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$random$Random$Generate = function (a) {
	return {$: 'Generate', a: a};
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0.a;
		return $elm$random$Random$Generate(
			A2($elm$random$Random$map, func, generator));
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			$elm$random$Random$Generate(
				A2($elm$random$Random$map, tagger, generator)));
	});
var $elm$browser$Browser$Dom$getViewport = _Browser_withWindow(_Browser_getViewport);
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $elm$random$Random$map3 = F4(
	function (func, _v0, _v1, _v2) {
		var genA = _v0.a;
		var genB = _v1.a;
		var genC = _v2.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v3 = genA(seed0);
				var a = _v3.a;
				var seed1 = _v3.b;
				var _v4 = genB(seed1);
				var b = _v4.a;
				var seed2 = _v4.b;
				var _v5 = genC(seed2);
				var c = _v5.a;
				var seed3 = _v5.b;
				return _Utils_Tuple2(
					A3(func, a, b, c),
					seed3);
			});
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $elm$random$Random$independentSeed = $elm$random$Random$Generator(
	function (seed0) {
		var makeIndependentSeed = F3(
			function (state, b, c) {
				return $elm$random$Random$next(
					A2($elm$random$Random$Seed, state, (1 | (b ^ c)) >>> 0));
			});
		var gen = A2($elm$random$Random$int, 0, 4294967295);
		return A2(
			$elm$random$Random$step,
			A4($elm$random$Random$map3, makeIndependentSeed, gen, gen, gen),
			seed0);
	});
var $MartinSStewart$elm_audio$Audio$AudioLoadRequest = function (a) {
	return {$: 'AudioLoadRequest', a: a};
};
var $MartinSStewart$elm_audio$Audio$ErrorThatHappensWhenYouLoadMoreThan1000SoundsDueToHackyWorkAroundToMakeThisPackageBehaveMoreLikeAnEffectPackage = {$: 'ErrorThatHappensWhenYouLoadMoreThan1000SoundsDueToHackyWorkAroundToMakeThisPackageBehaveMoreLikeAnEffectPackage'};
var $MartinSStewart$elm_audio$Audio$enumeratedResults = A2(
	$mgold$elm_nonempty_list$List$Nonempty$Nonempty,
	$elm$core$Result$Err($MartinSStewart$elm_audio$Audio$ErrorThatHappensWhenYouLoadMoreThan1000SoundsDueToHackyWorkAroundToMakeThisPackageBehaveMoreLikeAnEffectPackage),
	_Utils_ap(
		_List_fromArray(
			[
				$elm$core$Result$Err($MartinSStewart$elm_audio$Audio$FailedToDecode),
				$elm$core$Result$Err($MartinSStewart$elm_audio$Audio$NetworkError),
				$elm$core$Result$Err($MartinSStewart$elm_audio$Audio$UnknownError)
			]),
		A2(
			$elm$core$List$map,
			function (bufferId) {
				return $elm$core$Result$Ok(
					$MartinSStewart$elm_audio$Audio$File(
						{
							bufferId: $MartinSStewart$elm_audio$Audio$BufferId(bufferId)
						}));
			},
			A2($elm$core$List$range, 0, 1000))));
var $MartinSStewart$elm_audio$Audio$loadAudio = F2(
	function (userMsg, url) {
		return $MartinSStewart$elm_audio$Audio$AudioLoadRequest(
			{
				audioUrl: url,
				userMsg: A2(
					$mgold$elm_nonempty_list$List$Nonempty$map,
					function (results) {
						return _Utils_Tuple2(
							results,
							userMsg(results));
					},
					$MartinSStewart$elm_audio$Audio$enumeratedResults)
			});
	});
var $author$project$Main$interpretEffect = function (effect) {
	switch (effect.$) {
		case 'LoadAudio':
			var piece = effect.a;
			return $author$project$Reaction$audioCommands(
				_List_fromArray(
					[
						A2(
						$MartinSStewart$elm_audio$Audio$loadAudio,
						function (result) {
							return $author$project$Main$AudioLoaded(
								{piece: piece, result: result});
						},
						$elm$core$String$concat(
							_List_fromArray(
								[
									'public/',
									$author$project$Main$audioPieceToName(piece),
									'.mp3'
								])))
					]));
		case 'RequestInitialRandomSeed':
			return $author$project$Reaction$commands(
				_List_fromArray(
					[
						A2($elm$random$Random$generate, $author$project$Main$InitialRandomSeedReceived, $elm$random$Random$independentSeed)
					]));
		case 'RequestInitialTime':
			return $author$project$Reaction$commands(
				_List_fromArray(
					[
						A2($elm$core$Task$perform, $author$project$Main$InitialTimeReceived, $elm$time$Time$now)
					]));
		default:
			return $author$project$Reaction$commands(
				_List_fromArray(
					[
						A2(
						$elm$core$Task$perform,
						function (viewport) {
							return $author$project$Main$GameWindowSized(
								{height: viewport.viewport.height, width: viewport.viewport.width});
						},
						$elm$browser$Browser$Dom$getViewport)
					]));
	}
};
var $author$project$Main$Mouse = {$: 'Mouse'};
var $author$project$Main$PlayerPastFrozen = function (a) {
	return {$: 'PlayerPastFrozen', a: a};
};
var $author$project$Main$alterAudioOfKind = F2(
	function (kind, f) {
		if (kind.$ === 'AudioRoomChange') {
			return function (r) {
				return _Utils_update(
					r,
					{
						roomChange: f(r.roomChange)
					});
			};
		} else {
			return function (r) {
				return _Utils_update(
					r,
					{
						music: f(r.music)
					});
			};
		}
	});
var $w0rm$elm_physics$Internal$Vector3$cross = F2(
	function (a, b) {
		return {x: (a.y * b.z) - (a.z * b.y), y: (a.z * b.x) - (a.x * b.z), z: (a.x * b.y) - (a.y * b.x)};
	});
var $w0rm$elm_physics$Internal$Body$applyForce = F4(
	function (amount, direction, point, body) {
		var relativePoint = A2(
			$w0rm$elm_physics$Internal$Vector3$sub,
			point,
			$w0rm$elm_physics$Internal$Transform3d$originPoint(body.transform3d));
		var force = A2($w0rm$elm_physics$Internal$Vector3$scale, amount, direction);
		var torque = A2($w0rm$elm_physics$Internal$Vector3$cross, relativePoint, force);
		return _Utils_update(
			body,
			{
				force: A2($w0rm$elm_physics$Internal$Vector3$add, body.force, force),
				torque: A2($w0rm$elm_physics$Internal$Vector3$add, body.torque, torque)
			});
	});
var $w0rm$elm_physics$Physics$Body$applyForce = F4(
	function (_v0, direction, point, _v1) {
		var force = _v0.a;
		var body = _v1.a;
		return (body.mass > 0) ? $w0rm$elm_physics$Internal$Body$Protected(
			A4(
				$w0rm$elm_physics$Internal$Body$applyForce,
				force,
				$ianmackenzie$elm_geometry$Direction3d$unwrap(direction),
				$ianmackenzie$elm_geometry$Point3d$toMeters(point),
				body)) : $w0rm$elm_physics$Internal$Body$Protected(body);
	});
var $w0rm$elm_physics$Physics$World$bodies = function (_v0) {
	var world = _v0.a;
	return A3(
		$elm$core$List$foldl,
		F2(
			function (body, result) {
				return A2(
					$elm$core$List$cons,
					$w0rm$elm_physics$Internal$Body$Protected(body),
					result);
			}),
		_List_Nil,
		world.bodies);
};
var $ianmackenzie$elm_3d_camera$Viewpoint3d$eyePoint = function (_v0) {
	var frame = _v0.a;
	return $ianmackenzie$elm_geometry$Frame3d$originPoint(frame);
};
var $ianmackenzie$elm_geometry$Vector3d$for = F2(
	function (_v0, _v1) {
		var a = _v0.a;
		var v = _v1.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Vector3d(
			{x: v.x * a, y: v.y * a, z: v.z * a});
	});
var $w0rm$elm_physics$Physics$Body$frame = function (_v0) {
	var transform3d = _v0.a.transform3d;
	var centerOfMassTransform3d = _v0.a.centerOfMassTransform3d;
	var bodyCoordinatesTransform3d = A2(
		$w0rm$elm_physics$Internal$Transform3d$placeIn,
		transform3d,
		$w0rm$elm_physics$Internal$Transform3d$inverse(centerOfMassTransform3d));
	var _v1 = $w0rm$elm_physics$Internal$Transform3d$orientation(bodyCoordinatesTransform3d);
	var m11 = _v1.m11;
	var m21 = _v1.m21;
	var m31 = _v1.m31;
	var m12 = _v1.m12;
	var m22 = _v1.m22;
	var m32 = _v1.m32;
	var m13 = _v1.m13;
	var m23 = _v1.m23;
	var m33 = _v1.m33;
	return $ianmackenzie$elm_geometry$Frame3d$unsafe(
		{
			originPoint: $ianmackenzie$elm_geometry$Point3d$fromMeters(
				$w0rm$elm_physics$Internal$Transform3d$originPoint(bodyCoordinatesTransform3d)),
			xDirection: $ianmackenzie$elm_geometry$Direction3d$unsafe(
				{x: m11, y: m21, z: m31}),
			yDirection: $ianmackenzie$elm_geometry$Direction3d$unsafe(
				{x: m12, y: m22, z: m32}),
			zDirection: $ianmackenzie$elm_geometry$Direction3d$unsafe(
				{x: m13, y: m23, z: m33})
		});
};
var $ianmackenzie$elm_geometry$Direction3d$componentIn = F2(
	function (_v0, _v1) {
		var d2 = _v0.a;
		var d1 = _v1.a;
		return ((d1.x * d2.x) + (d1.y * d2.y)) + (d1.z * d2.z);
	});
var $ianmackenzie$elm_geometry$Axis3d$originPoint = function (_v0) {
	var axis = _v0.a;
	return axis.originPoint;
};
var $ianmackenzie$elm_geometry$Point3d$signedDistanceFrom = F2(
	function (_v0, _v1) {
		var plane = _v0.a;
		var p = _v1.a;
		var _v2 = plane.originPoint;
		var p0 = _v2.a;
		var _v3 = plane.normalDirection;
		var n = _v3.a;
		return $ianmackenzie$elm_units$Quantity$Quantity((((p.x - p0.x) * n.x) + ((p.y - p0.y) * n.y)) + ((p.z - p0.z) * n.z));
	});
var $ianmackenzie$elm_geometry$Point3d$translateIn = F3(
	function (_v0, _v1, _v2) {
		var d = _v0.a;
		var distance = _v1.a;
		var p = _v2.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Point3d(
			{x: p.x + (distance * d.x), y: p.y + (distance * d.y), z: p.z + (distance * d.z)});
	});
var $ianmackenzie$elm_geometry$Axis3d$intersectionWithPlane = F2(
	function (plane, axis) {
		var axisDirection = $ianmackenzie$elm_geometry$Axis3d$direction(axis);
		var _v0 = plane;
		var normalDirection = _v0.a.normalDirection;
		var normalComponent = A2($ianmackenzie$elm_geometry$Direction3d$componentIn, normalDirection, axisDirection);
		if (!normalComponent) {
			return $elm$core$Maybe$Nothing;
		} else {
			var axisOrigin = $ianmackenzie$elm_geometry$Axis3d$originPoint(axis);
			var normalDistance = A2($ianmackenzie$elm_geometry$Point3d$signedDistanceFrom, plane, axisOrigin);
			var axialDistance = A2($ianmackenzie$elm_units$Quantity$multiplyBy, (-1) / normalComponent, normalDistance);
			return $elm$core$Maybe$Just(
				A3($ianmackenzie$elm_geometry$Point3d$translateIn, axisDirection, axialDistance, axisOrigin));
		}
	});
var $author$project$Main$isDraggable = function (kind) {
	switch (kind.$) {
		case 'Player':
			return true;
		case 'DraggableBlock':
			return true;
		case 'DraggableBallWithCubeBehavior':
			return true;
		case 'PlayerImitation':
			return true;
		case 'PlayerPast':
			return false;
		case 'BlockingImmovableWall':
			return false;
		case 'Mouse':
			return false;
		case 'MouseImitation':
			return false;
		default:
			return false;
	}
};
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $elm$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _v0) {
				var trues = _v0.a;
				var falses = _v0.b;
				return pred(x) ? _Utils_Tuple2(
					A2($elm$core$List$cons, x, trues),
					falses) : _Utils_Tuple2(
					trues,
					A2($elm$core$List$cons, x, falses));
			});
		return A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			list);
	});
var $w0rm$elm_physics$Physics$World$keepIf = F2(
	function (fn, _v0) {
		var world = _v0.a;
		var _v1 = A2(
			$elm$core$List$partition,
			A2($elm$core$Basics$composeR, $w0rm$elm_physics$Internal$Body$Protected, fn),
			world.bodies);
		var keptBodies = _v1.a;
		var removedBodies = _v1.b;
		var removedIds = A3(
			$elm$core$List$foldl,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.id;
				},
				$elm$core$List$cons),
			_List_Nil,
			removedBodies);
		var keptConstraints = A3(
			$elm$core$List$foldl,
			F2(
				function (c, result) {
					return (A2($elm$core$List$member, c.bodyId1, removedIds) || A2($elm$core$List$member, c.bodyId2, removedIds)) ? result : A2($elm$core$List$cons, c, result);
				}),
			_List_Nil,
			world.constraints);
		return $w0rm$elm_physics$Internal$World$Protected(
			_Utils_update(
				world,
				{
					bodies: keptBodies,
					constraints: keptConstraints,
					freeIds: _Utils_ap(removedIds, world.freeIds)
				}));
	});
var $ianmackenzie$elm_geometry$Vector3d$length = function (_v0) {
	var v = _v0.a;
	var largestComponent = A2(
		$elm$core$Basics$max,
		$elm$core$Basics$abs(v.x),
		A2(
			$elm$core$Basics$max,
			$elm$core$Basics$abs(v.y),
			$elm$core$Basics$abs(v.z)));
	if (!largestComponent) {
		return $ianmackenzie$elm_units$Quantity$zero;
	} else {
		var scaledZ = v.z / largestComponent;
		var scaledY = v.y / largestComponent;
		var scaledX = v.x / largestComponent;
		var scaledLength = $elm$core$Basics$sqrt(((scaledX * scaledX) + (scaledY * scaledY)) + (scaledZ * scaledZ));
		return $ianmackenzie$elm_units$Quantity$Quantity(scaledLength * largestComponent);
	}
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $w0rm$elm_physics$Physics$Body$originPoint = function (_v0) {
	var transform3d = _v0.a.transform3d;
	var centerOfMassTransform3d = _v0.a.centerOfMassTransform3d;
	var bodyCoordinatesTransform3d = A2(
		$w0rm$elm_physics$Internal$Transform3d$placeIn,
		transform3d,
		$w0rm$elm_physics$Internal$Transform3d$inverse(centerOfMassTransform3d));
	return $ianmackenzie$elm_geometry$Point3d$fromMeters(
		$w0rm$elm_physics$Internal$Transform3d$originPoint(bodyCoordinatesTransform3d));
};
var $ianmackenzie$elm_geometry$Point3d$placeIn = F2(
	function (_v0, _v1) {
		var frame = _v0.a;
		var p = _v1.a;
		var _v2 = frame.originPoint;
		var p0 = _v2.a;
		var _v3 = frame.zDirection;
		var k = _v3.a;
		var _v4 = frame.yDirection;
		var j = _v4.a;
		var _v5 = frame.xDirection;
		var i = _v5.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Point3d(
			{x: ((p0.x + (p.x * i.x)) + (p.y * j.x)) + (p.z * k.x), y: ((p0.y + (p.x * i.y)) + (p.y * j.y)) + (p.z * k.y), z: ((p0.z + (p.x * i.z)) + (p.y * j.z)) + (p.z * k.z)});
	});
var $w0rm$elm_physics$Physics$Constraint$pointToPoint = F2(
	function (pivot1, pivot2) {
		return $w0rm$elm_physics$Internal$Constraint$Protected(
			A2(
				$w0rm$elm_physics$Internal$Constraint$PointToPoint,
				$ianmackenzie$elm_geometry$Point3d$toMeters(pivot1),
				$ianmackenzie$elm_geometry$Point3d$toMeters(pivot2)));
	});
var $w0rm$elm_physics$Internal$Vector3$dot = F2(
	function (a, b) {
		return ((a.x * b.x) + (a.y * b.y)) + (a.z * b.z);
	});
var $w0rm$elm_physics$Shapes$Convex$foldFaceEdgesHelp = F4(
	function (fn, seed, resultSeed, vertices) {
		foldFaceEdgesHelp:
		while (true) {
			if (vertices.b) {
				var el1 = vertices.a;
				var rest1 = vertices.b;
				if (!rest1.b) {
					return A3(fn, el1, seed, resultSeed);
				} else {
					var el2 = rest1.a;
					var $temp$fn = fn,
						$temp$seed = seed,
						$temp$resultSeed = A3(fn, el1, el2, resultSeed),
						$temp$vertices = rest1;
					fn = $temp$fn;
					seed = $temp$seed;
					resultSeed = $temp$resultSeed;
					vertices = $temp$vertices;
					continue foldFaceEdgesHelp;
				}
			} else {
				return resultSeed;
			}
		}
	});
var $w0rm$elm_physics$Shapes$Convex$foldFaceEdges = F3(
	function (fn, resultSeed, vertices) {
		if (vertices.b && vertices.b.b) {
			var first = vertices.a;
			var _v1 = vertices.b;
			return A4($w0rm$elm_physics$Shapes$Convex$foldFaceEdgesHelp, fn, first, resultSeed, vertices);
		} else {
			return resultSeed;
		}
	});
var $w0rm$elm_physics$Shapes$Convex$raycast = F2(
	function (_v0, convex) {
		var direction = _v0.direction;
		var from = _v0.from;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v1, maybeHit) {
					var normal = _v1.normal;
					var vertices = _v1.vertices;
					var point = function () {
						if (vertices.b) {
							var first = vertices.a;
							return first;
						} else {
							return $w0rm$elm_physics$Internal$Vector3$zero;
						}
					}();
					var dot = A2($w0rm$elm_physics$Internal$Vector3$dot, direction, normal);
					if (dot < 0) {
						var pointToFrom = A2($w0rm$elm_physics$Internal$Vector3$sub, point, from);
						var scalar = A2($w0rm$elm_physics$Internal$Vector3$dot, normal, pointToFrom) / dot;
						if (scalar >= 0) {
							var intersectionPoint = {x: (direction.x * scalar) + from.x, y: (direction.y * scalar) + from.y, z: (direction.z * scalar) + from.z};
							var isInsidePolygon = A3(
								$w0rm$elm_physics$Shapes$Convex$foldFaceEdges,
								F3(
									function (p1, p2, result) {
										return result && (A2(
											$w0rm$elm_physics$Internal$Vector3$dot,
											A2($w0rm$elm_physics$Internal$Vector3$sub, intersectionPoint, p1),
											A2(
												$w0rm$elm_physics$Internal$Vector3$cross,
												normal,
												A2($w0rm$elm_physics$Internal$Vector3$sub, p2, p1))) > 0);
									}),
								true,
								vertices);
							if (isInsidePolygon) {
								if (maybeHit.$ === 'Just') {
									var distance = maybeHit.a.distance;
									return ((scalar - distance) < 0) ? $elm$core$Maybe$Just(
										{distance: scalar, normal: normal, point: intersectionPoint}) : maybeHit;
								} else {
									return $elm$core$Maybe$Just(
										{distance: scalar, normal: normal, point: intersectionPoint});
								}
							} else {
								return maybeHit;
							}
						} else {
							return maybeHit;
						}
					} else {
						return maybeHit;
					}
				}),
			$elm$core$Maybe$Nothing,
			convex.faces);
	});
var $w0rm$elm_physics$Shapes$Plane$raycast = F2(
	function (_v0, _v1) {
		var from = _v0.from;
		var direction = _v0.direction;
		var normal = _v1.normal;
		var position = _v1.position;
		var dot = A2($w0rm$elm_physics$Internal$Vector3$dot, direction, normal);
		if (dot < 0) {
			var pointToFrom = A2($w0rm$elm_physics$Internal$Vector3$sub, position, from);
			var scalar = A2($w0rm$elm_physics$Internal$Vector3$dot, normal, pointToFrom) / dot;
			return (scalar >= 0) ? $elm$core$Maybe$Just(
				{
					distance: scalar,
					normal: normal,
					point: {x: (direction.x * scalar) + from.x, y: (direction.y * scalar) + from.y, z: (direction.z * scalar) + from.z}
				}) : $elm$core$Maybe$Nothing;
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $w0rm$elm_physics$Shapes$Sphere$raycast = F2(
	function (_v0, _v1) {
		var from = _v0.from;
		var direction = _v0.direction;
		var position = _v1.position;
		var radius = _v1.radius;
		var c = ((((from.x - position.x) * (from.x - position.x)) + ((from.y - position.y) * (from.y - position.y))) + ((from.z - position.z) * (from.z - position.z))) - (radius * radius);
		var b = 2 * (((direction.x * (from.x - position.x)) + (direction.y * (from.y - position.y))) + (direction.z * (from.z - position.z)));
		var a = ((direction.x * direction.x) + (direction.y * direction.y)) + (direction.z * direction.z);
		var delta = (b * b) - ((4 * a) * c);
		if (delta < 0) {
			return $elm$core$Maybe$Nothing;
		} else {
			var distance = ((-b) - $elm$core$Basics$sqrt(delta)) / (2 * a);
			if (distance >= 0) {
				var point = {x: from.x + (direction.x * distance), y: from.y + (direction.y * distance), z: from.z + (direction.z * distance)};
				var normal = A2($w0rm$elm_physics$Internal$Vector3$sub, point, position);
				return $elm$core$Maybe$Just(
					{distance: distance, normal: normal, point: point});
			} else {
				return $elm$core$Maybe$Nothing;
			}
		}
	});
var $w0rm$elm_physics$Internal$Shape$raycast = F2(
	function (ray, shape) {
		switch (shape.$) {
			case 'Plane':
				var plane = shape.a;
				return A2($w0rm$elm_physics$Shapes$Plane$raycast, ray, plane);
			case 'Sphere':
				var sphere = shape.a;
				return A2($w0rm$elm_physics$Shapes$Sphere$raycast, ray, sphere);
			case 'Convex':
				var convex = shape.a;
				return A2($w0rm$elm_physics$Shapes$Convex$raycast, ray, convex);
			default:
				return $elm$core$Maybe$Nothing;
		}
	});
var $w0rm$elm_physics$Internal$Body$raycast = F2(
	function (ray, body) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (shape, maybeClosestRaycastResult) {
					var _v0 = A2($w0rm$elm_physics$Internal$Shape$raycast, ray, shape);
					if (_v0.$ === 'Just') {
						var raycastResult = _v0.a;
						if (maybeClosestRaycastResult.$ === 'Just') {
							var closestRaycastResult = maybeClosestRaycastResult.a;
							return ((raycastResult.distance - closestRaycastResult.distance) < 0) ? $elm$core$Maybe$Just(raycastResult) : maybeClosestRaycastResult;
						} else {
							return $elm$core$Maybe$Just(raycastResult);
						}
					} else {
						return maybeClosestRaycastResult;
					}
				}),
			$elm$core$Maybe$Nothing,
			body.worldShapes);
	});
var $w0rm$elm_physics$Internal$World$raycast = F2(
	function (ray, _v0) {
		var bodies = _v0.bodies;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (body, maybeClosestRaycastResult) {
					var _v1 = A2($w0rm$elm_physics$Internal$Body$raycast, ray, body);
					if (_v1.$ === 'Just') {
						var raycastResult = _v1.a;
						if (maybeClosestRaycastResult.$ === 'Just') {
							var closestRaycastResult = maybeClosestRaycastResult.a;
							return ((raycastResult.distance - closestRaycastResult.distance) < 0) ? $elm$core$Maybe$Just(
								{body: body, distance: raycastResult.distance, normal: raycastResult.normal, point: raycastResult.point}) : maybeClosestRaycastResult;
						} else {
							return $elm$core$Maybe$Just(
								{body: body, distance: raycastResult.distance, normal: raycastResult.normal, point: raycastResult.point});
						}
					} else {
						return maybeClosestRaycastResult;
					}
				}),
			$elm$core$Maybe$Nothing,
			bodies);
	});
var $w0rm$elm_physics$Physics$World$raycast = F2(
	function (ray, _v0) {
		var world = _v0.a;
		var _v1 = A2(
			$w0rm$elm_physics$Internal$World$raycast,
			{
				direction: $ianmackenzie$elm_geometry$Direction3d$unwrap(
					$ianmackenzie$elm_geometry$Axis3d$direction(ray)),
				from: $ianmackenzie$elm_geometry$Point3d$toMeters(
					$ianmackenzie$elm_geometry$Axis3d$originPoint(ray))
			},
			world);
		if (_v1.$ === 'Just') {
			var body = _v1.a.body;
			var point = _v1.a.point;
			var normal = _v1.a.normal;
			return $elm$core$Maybe$Just(
				{
					body: $w0rm$elm_physics$Internal$Body$Protected(body),
					normal: $ianmackenzie$elm_geometry$Direction3d$unsafe(
						A2(
							$w0rm$elm_physics$Internal$Transform3d$directionRelativeTo,
							A2(
								$w0rm$elm_physics$Internal$Transform3d$placeIn,
								body.transform3d,
								$w0rm$elm_physics$Internal$Transform3d$inverse(body.centerOfMassTransform3d)),
							normal)),
					point: $ianmackenzie$elm_geometry$Point3d$fromMeters(
						A2(
							$w0rm$elm_physics$Internal$Transform3d$pointRelativeTo,
							A2(
								$w0rm$elm_physics$Internal$Transform3d$placeIn,
								body.transform3d,
								$w0rm$elm_physics$Internal$Transform3d$inverse(body.centerOfMassTransform3d)),
							point))
				});
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $ianmackenzie$elm_units$Duration$second = $ianmackenzie$elm_units$Duration$seconds(1);
var $w0rm$elm_physics$Internal$Vector3$distanceSquared = F2(
	function (a, b) {
		return $w0rm$elm_physics$Internal$Vector3$lengthSquared(
			A2($w0rm$elm_physics$Internal$Vector3$sub, a, b));
	});
var $w0rm$elm_physics$Internal$BroadPhase$bodiesMayContact = F2(
	function (body1, body2) {
		var distanceSquared = A2(
			$w0rm$elm_physics$Internal$Vector3$distanceSquared,
			$w0rm$elm_physics$Internal$Transform3d$originPoint(body1.transform3d),
			$w0rm$elm_physics$Internal$Transform3d$originPoint(body2.transform3d));
		var boundingRadiuses = body1.boundingSphereRadius + body2.boundingSphereRadius;
		return (((boundingRadiuses * boundingRadiuses) - distanceSquared) > 0) && (!(!(body1.mass + body2.mass)));
	});
var $w0rm$elm_physics$Collision$ConvexConvex$bestFaceHelp = F4(
	function (separatingAxis, faces, currentBestFace, currentBestDistance) {
		bestFaceHelp:
		while (true) {
			if (faces.b) {
				var face = faces.a;
				var remainingFaces = faces.b;
				var faceDistance = A2($w0rm$elm_physics$Internal$Vector3$dot, face.normal, separatingAxis);
				if ((currentBestDistance - faceDistance) > 0) {
					var $temp$separatingAxis = separatingAxis,
						$temp$faces = remainingFaces,
						$temp$currentBestFace = face,
						$temp$currentBestDistance = faceDistance;
					separatingAxis = $temp$separatingAxis;
					faces = $temp$faces;
					currentBestFace = $temp$currentBestFace;
					currentBestDistance = $temp$currentBestDistance;
					continue bestFaceHelp;
				} else {
					var $temp$separatingAxis = separatingAxis,
						$temp$faces = remainingFaces,
						$temp$currentBestFace = currentBestFace,
						$temp$currentBestDistance = currentBestDistance;
					separatingAxis = $temp$separatingAxis;
					faces = $temp$faces;
					currentBestFace = $temp$currentBestFace;
					currentBestDistance = $temp$currentBestDistance;
					continue bestFaceHelp;
				}
			} else {
				return currentBestFace;
			}
		}
	});
var $w0rm$elm_physics$Collision$ConvexConvex$bestFace = F2(
	function (faces, separatingAxis) {
		if (faces.b) {
			var face = faces.a;
			var restFaces = faces.b;
			return $elm$core$Maybe$Just(
				A4(
					$w0rm$elm_physics$Collision$ConvexConvex$bestFaceHelp,
					separatingAxis,
					restFaces,
					face,
					A2($w0rm$elm_physics$Internal$Vector3$dot, face.normal, separatingAxis)));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $w0rm$elm_physics$Internal$Vector3$lerp = F3(
	function (t, v1, v2) {
		return {x: v1.x + (t * (v2.x - v1.x)), y: v1.y + (t * (v2.y - v1.y)), z: v1.z + (t * (v2.z - v1.z))};
	});
var $w0rm$elm_physics$Collision$ConvexConvex$clipFaceAgainstPlaneAdd = F5(
	function (planeNormal, planeConstant, prev, next, result) {
		var nDotPrev = A2($w0rm$elm_physics$Internal$Vector3$dot, planeNormal, prev) + planeConstant;
		var nDotNext = A2($w0rm$elm_physics$Internal$Vector3$dot, planeNormal, next) + planeConstant;
		return (nDotPrev < 0) ? ((nDotNext < 0) ? A2($elm$core$List$cons, next, result) : A2(
			$elm$core$List$cons,
			A3($w0rm$elm_physics$Internal$Vector3$lerp, nDotPrev / (nDotPrev - nDotNext), prev, next),
			result)) : ((nDotNext < 0) ? A2(
			$elm$core$List$cons,
			next,
			A2(
				$elm$core$List$cons,
				A3($w0rm$elm_physics$Internal$Vector3$lerp, nDotPrev / (nDotPrev - nDotNext), prev, next),
				result)) : result);
	});
var $w0rm$elm_physics$Collision$ConvexConvex$clipAgainstAdjacentFaces = F2(
	function (_v0, faceVertices) {
		var vertices = _v0.vertices;
		var normal = _v0.normal;
		return A3(
			$w0rm$elm_physics$Shapes$Convex$foldFaceEdges,
			F2(
				function (v1, v2) {
					var edge = $w0rm$elm_physics$Internal$Vector3$normalize(
						A2($w0rm$elm_physics$Internal$Vector3$sub, v1, v2));
					var planeNormal = A2($w0rm$elm_physics$Internal$Vector3$cross, normal, edge);
					var planeConstant = -A2($w0rm$elm_physics$Internal$Vector3$dot, v1, planeNormal);
					return A2(
						$w0rm$elm_physics$Shapes$Convex$foldFaceEdges,
						A2($w0rm$elm_physics$Collision$ConvexConvex$clipFaceAgainstPlaneAdd, planeNormal, planeConstant),
						_List_Nil);
				}),
			faceVertices,
			vertices);
	});
var $w0rm$elm_physics$Collision$ConvexConvex$clipTwoFacesHelp = F5(
	function (separatingAxis, face, facePlaneConstant, vertices, result) {
		clipTwoFacesHelp:
		while (true) {
			if (vertices.b) {
				var vertex = vertices.a;
				var remainingVertices = vertices.b;
				var depth = A2($w0rm$elm_physics$Internal$Vector3$dot, face.normal, vertex) + facePlaneConstant;
				if (depth <= 0) {
					var $temp$separatingAxis = separatingAxis,
						$temp$face = face,
						$temp$facePlaneConstant = facePlaneConstant,
						$temp$vertices = remainingVertices,
						$temp$result = A2(
						$elm$core$List$cons,
						{
							ni: separatingAxis,
							pi: {x: vertex.x - (depth * face.normal.x), y: vertex.y - (depth * face.normal.y), z: vertex.z - (depth * face.normal.z)},
							pj: vertex
						},
						result);
					separatingAxis = $temp$separatingAxis;
					face = $temp$face;
					facePlaneConstant = $temp$facePlaneConstant;
					vertices = $temp$vertices;
					result = $temp$result;
					continue clipTwoFacesHelp;
				} else {
					var $temp$separatingAxis = separatingAxis,
						$temp$face = face,
						$temp$facePlaneConstant = facePlaneConstant,
						$temp$vertices = remainingVertices,
						$temp$result = result;
					separatingAxis = $temp$separatingAxis;
					face = $temp$face;
					facePlaneConstant = $temp$facePlaneConstant;
					vertices = $temp$vertices;
					result = $temp$result;
					continue clipTwoFacesHelp;
				}
			} else {
				return result;
			}
		}
	});
var $w0rm$elm_physics$Collision$ConvexConvex$clipTwoFaces = F4(
	function (face, _v0, separatingAxis, contacts) {
		var vertices = _v0.vertices;
		var point = function () {
			var _v1 = face.vertices;
			if (_v1.b) {
				var first = _v1.a;
				return first;
			} else {
				return $w0rm$elm_physics$Internal$Vector3$zero;
			}
		}();
		var facePlaneConstant = -A2($w0rm$elm_physics$Internal$Vector3$dot, face.normal, point);
		return A5(
			$w0rm$elm_physics$Collision$ConvexConvex$clipTwoFacesHelp,
			separatingAxis,
			face,
			facePlaneConstant,
			A2($w0rm$elm_physics$Collision$ConvexConvex$clipAgainstAdjacentFaces, face, vertices),
			contacts);
	});
var $w0rm$elm_physics$Collision$ConvexConvex$project = F4(
	function (localAxis, minVal, maxVal, currentVertices) {
		project:
		while (true) {
			if (!currentVertices.b) {
				return {max: maxVal, min: minVal};
			} else {
				var vec = currentVertices.a;
				var remainingVertices = currentVertices.b;
				var val = ((vec.x * localAxis.x) + (vec.y * localAxis.y)) + (vec.z * localAxis.z);
				var $temp$localAxis = localAxis,
					$temp$minVal = ((minVal - val) > 0) ? val : minVal,
					$temp$maxVal = ((maxVal - val) > 0) ? maxVal : val,
					$temp$currentVertices = remainingVertices;
				localAxis = $temp$localAxis;
				minVal = $temp$minVal;
				maxVal = $temp$maxVal;
				currentVertices = $temp$currentVertices;
				continue project;
			}
		}
	});
var $w0rm$elm_physics$Collision$ConvexConvex$testSeparatingAxis = F3(
	function (convex1, convex2, separatingAxis) {
		var p2 = A4($w0rm$elm_physics$Collision$ConvexConvex$project, separatingAxis, $w0rm$elm_physics$Internal$Const$maxNumber, -$w0rm$elm_physics$Internal$Const$maxNumber, convex2.vertices);
		var p1 = A4($w0rm$elm_physics$Collision$ConvexConvex$project, separatingAxis, $w0rm$elm_physics$Internal$Const$maxNumber, -$w0rm$elm_physics$Internal$Const$maxNumber, convex1.vertices);
		var d2 = p2.max - p1.min;
		var d1 = p1.max - p2.min;
		return ((d1 < 0) || (d2 < 0)) ? $elm$core$Maybe$Nothing : (((d1 - d2) > 0) ? $elm$core$Maybe$Just(d2) : $elm$core$Maybe$Just(d1));
	});
var $w0rm$elm_physics$Internal$Const$precision = 1.0e-6;
var $w0rm$elm_physics$Internal$Vector3$almostZero = function (_v0) {
	var x = _v0.x;
	var y = _v0.y;
	var z = _v0.z;
	return (($elm$core$Basics$abs(x) - $w0rm$elm_physics$Internal$Const$precision) <= 0) && ((($elm$core$Basics$abs(y) - $w0rm$elm_physics$Internal$Const$precision) <= 0) && (($elm$core$Basics$abs(z) - $w0rm$elm_physics$Internal$Const$precision) <= 0));
};
var $w0rm$elm_physics$Internal$Vector3$negate = function (v3) {
	return {x: -v3.x, y: -v3.y, z: -v3.z};
};
var $w0rm$elm_physics$Collision$ConvexConvex$testUniqueEdges = F7(
	function (convex1, convex2, initEdges2, edges1, edges2, target, dmin) {
		testUniqueEdges:
		while (true) {
			if (!edges1.b) {
				return (A2(
					$w0rm$elm_physics$Internal$Vector3$dot,
					A2($w0rm$elm_physics$Internal$Vector3$sub, convex2.position, convex1.position),
					target) > 0) ? $elm$core$Maybe$Just(
					$w0rm$elm_physics$Internal$Vector3$negate(target)) : $elm$core$Maybe$Just(target);
			} else {
				var edge1 = edges1.a;
				var remainingEdges1 = edges1.b;
				if (!edges2.b) {
					var $temp$convex1 = convex1,
						$temp$convex2 = convex2,
						$temp$initEdges2 = initEdges2,
						$temp$edges1 = remainingEdges1,
						$temp$edges2 = initEdges2,
						$temp$target = target,
						$temp$dmin = dmin;
					convex1 = $temp$convex1;
					convex2 = $temp$convex2;
					initEdges2 = $temp$initEdges2;
					edges1 = $temp$edges1;
					edges2 = $temp$edges2;
					target = $temp$target;
					dmin = $temp$dmin;
					continue testUniqueEdges;
				} else {
					var edge2 = edges2.a;
					var remainingEdges2 = edges2.b;
					var cross = A2($w0rm$elm_physics$Internal$Vector3$cross, edge1, edge2);
					if ($w0rm$elm_physics$Internal$Vector3$almostZero(cross)) {
						var $temp$convex1 = convex1,
							$temp$convex2 = convex2,
							$temp$initEdges2 = initEdges2,
							$temp$edges1 = edges1,
							$temp$edges2 = remainingEdges2,
							$temp$target = target,
							$temp$dmin = dmin;
						convex1 = $temp$convex1;
						convex2 = $temp$convex2;
						initEdges2 = $temp$initEdges2;
						edges1 = $temp$edges1;
						edges2 = $temp$edges2;
						target = $temp$target;
						dmin = $temp$dmin;
						continue testUniqueEdges;
					} else {
						var normalizedCross = $w0rm$elm_physics$Internal$Vector3$normalize(cross);
						var _v2 = A3($w0rm$elm_physics$Collision$ConvexConvex$testSeparatingAxis, convex1, convex2, normalizedCross);
						if (_v2.$ === 'Nothing') {
							return $elm$core$Maybe$Nothing;
						} else {
							var dist = _v2.a;
							if ((dist - dmin) < 0) {
								var $temp$convex1 = convex1,
									$temp$convex2 = convex2,
									$temp$initEdges2 = initEdges2,
									$temp$edges1 = edges1,
									$temp$edges2 = remainingEdges2,
									$temp$target = normalizedCross,
									$temp$dmin = dist;
								convex1 = $temp$convex1;
								convex2 = $temp$convex2;
								initEdges2 = $temp$initEdges2;
								edges1 = $temp$edges1;
								edges2 = $temp$edges2;
								target = $temp$target;
								dmin = $temp$dmin;
								continue testUniqueEdges;
							} else {
								var $temp$convex1 = convex1,
									$temp$convex2 = convex2,
									$temp$initEdges2 = initEdges2,
									$temp$edges1 = edges1,
									$temp$edges2 = remainingEdges2,
									$temp$target = target,
									$temp$dmin = dmin;
								convex1 = $temp$convex1;
								convex2 = $temp$convex2;
								initEdges2 = $temp$initEdges2;
								edges1 = $temp$edges1;
								edges2 = $temp$edges2;
								target = $temp$target;
								dmin = $temp$dmin;
								continue testUniqueEdges;
							}
						}
					}
				}
			}
		}
	});
var $w0rm$elm_physics$Collision$ConvexConvex$testUniqueNormals = F5(
	function (convex1, convex2, normals, target, dmin) {
		testUniqueNormals:
		while (true) {
			if (!normals.b) {
				return A7($w0rm$elm_physics$Collision$ConvexConvex$testUniqueEdges, convex1, convex2, convex2.uniqueEdges, convex1.uniqueEdges, convex2.uniqueEdges, target, dmin);
			} else {
				var normal = normals.a;
				var restNormals = normals.b;
				var _v1 = A3($w0rm$elm_physics$Collision$ConvexConvex$testSeparatingAxis, convex1, convex2, normal);
				if (_v1.$ === 'Nothing') {
					return $elm$core$Maybe$Nothing;
				} else {
					var dist = _v1.a;
					if ((dist - dmin) < 0) {
						var $temp$convex1 = convex1,
							$temp$convex2 = convex2,
							$temp$normals = restNormals,
							$temp$target = normal,
							$temp$dmin = dist;
						convex1 = $temp$convex1;
						convex2 = $temp$convex2;
						normals = $temp$normals;
						target = $temp$target;
						dmin = $temp$dmin;
						continue testUniqueNormals;
					} else {
						var $temp$convex1 = convex1,
							$temp$convex2 = convex2,
							$temp$normals = restNormals,
							$temp$target = target,
							$temp$dmin = dmin;
						convex1 = $temp$convex1;
						convex2 = $temp$convex2;
						normals = $temp$normals;
						target = $temp$target;
						dmin = $temp$dmin;
						continue testUniqueNormals;
					}
				}
			}
		}
	});
var $w0rm$elm_physics$Collision$ConvexConvex$findSeparatingAxis = F2(
	function (convex1, convex2) {
		return A5(
			$w0rm$elm_physics$Collision$ConvexConvex$testUniqueNormals,
			convex1,
			convex2,
			_Utils_ap(convex1.uniqueNormals, convex2.uniqueNormals),
			$w0rm$elm_physics$Internal$Vector3$zero,
			$w0rm$elm_physics$Internal$Const$maxNumber);
	});
var $w0rm$elm_physics$Collision$ConvexConvex$addContacts = F3(
	function (convex1, convex2, contacts) {
		var _v0 = A2($w0rm$elm_physics$Collision$ConvexConvex$findSeparatingAxis, convex1, convex2);
		if (_v0.$ === 'Just') {
			var separatingAxis = _v0.a;
			var reversedSeparatingAxis = $w0rm$elm_physics$Internal$Vector3$negate(separatingAxis);
			var _v1 = A2($w0rm$elm_physics$Collision$ConvexConvex$bestFace, convex1.faces, separatingAxis);
			if (_v1.$ === 'Just') {
				var face1 = _v1.a;
				var _v2 = A2($w0rm$elm_physics$Collision$ConvexConvex$bestFace, convex2.faces, reversedSeparatingAxis);
				if (_v2.$ === 'Just') {
					var face2 = _v2.a;
					return A4($w0rm$elm_physics$Collision$ConvexConvex$clipTwoFaces, face1, face2, reversedSeparatingAxis, contacts);
				} else {
					return contacts;
				}
			} else {
				return contacts;
			}
		} else {
			return contacts;
		}
	});
var $w0rm$elm_physics$Collision$ParticleConvex$convexContact = F4(
	function (particlePosition, faces, bestDepth, bestContact) {
		convexContact:
		while (true) {
			if (!faces.b) {
				return bestContact;
			} else {
				var vertices = faces.a.vertices;
				var normal = faces.a.normal;
				var remainingFaces = faces.b;
				var point = function () {
					if (vertices.b) {
						var first = vertices.a;
						return first;
					} else {
						return $w0rm$elm_physics$Internal$Vector3$zero;
					}
				}();
				var dot = A2(
					$w0rm$elm_physics$Internal$Vector3$dot,
					normal,
					A2($w0rm$elm_physics$Internal$Vector3$sub, point, particlePosition));
				if (dot >= 0) {
					if (_Utils_cmp(dot, bestDepth) < 0) {
						var $temp$particlePosition = particlePosition,
							$temp$faces = remainingFaces,
							$temp$bestDepth = dot,
							$temp$bestContact = $elm$core$Maybe$Just(
							{
								ni: $w0rm$elm_physics$Internal$Vector3$negate(normal),
								pi: particlePosition,
								pj: A2(
									$w0rm$elm_physics$Internal$Vector3$add,
									particlePosition,
									A2($w0rm$elm_physics$Internal$Vector3$scale, dot, normal))
							});
						particlePosition = $temp$particlePosition;
						faces = $temp$faces;
						bestDepth = $temp$bestDepth;
						bestContact = $temp$bestContact;
						continue convexContact;
					} else {
						var $temp$particlePosition = particlePosition,
							$temp$faces = remainingFaces,
							$temp$bestDepth = bestDepth,
							$temp$bestContact = bestContact;
						particlePosition = $temp$particlePosition;
						faces = $temp$faces;
						bestDepth = $temp$bestDepth;
						bestContact = $temp$bestContact;
						continue convexContact;
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $w0rm$elm_physics$Collision$ParticleConvex$addContacts = F4(
	function (orderContact, particlePosition, _v0, contacts) {
		var faces = _v0.faces;
		var _v1 = A4($w0rm$elm_physics$Collision$ParticleConvex$convexContact, particlePosition, faces, $w0rm$elm_physics$Internal$Const$maxNumber, $elm$core$Maybe$Nothing);
		if (_v1.$ === 'Just') {
			var contact = _v1.a;
			return A2(
				$elm$core$List$cons,
				orderContact(contact),
				contacts);
		} else {
			return contacts;
		}
	});
var $w0rm$elm_physics$Collision$PlaneConvex$addContactsHelp = F5(
	function (orderContact, planePosition, planeNormal, vertices, contacts) {
		addContactsHelp:
		while (true) {
			if (vertices.b) {
				var vertex = vertices.a;
				var remainingVertices = vertices.b;
				var dot = (((vertex.x - planePosition.x) * planeNormal.x) + ((vertex.y - planePosition.y) * planeNormal.y)) + ((vertex.z - planePosition.z) * planeNormal.z);
				if (dot <= 0) {
					var $temp$orderContact = orderContact,
						$temp$planePosition = planePosition,
						$temp$planeNormal = planeNormal,
						$temp$vertices = remainingVertices,
						$temp$contacts = A2(
						$elm$core$List$cons,
						orderContact(
							{
								ni: planeNormal,
								pi: {x: vertex.x - (dot * planeNormal.x), y: vertex.y - (dot * planeNormal.y), z: vertex.z - (dot * planeNormal.z)},
								pj: vertex
							}),
						contacts);
					orderContact = $temp$orderContact;
					planePosition = $temp$planePosition;
					planeNormal = $temp$planeNormal;
					vertices = $temp$vertices;
					contacts = $temp$contacts;
					continue addContactsHelp;
				} else {
					var $temp$orderContact = orderContact,
						$temp$planePosition = planePosition,
						$temp$planeNormal = planeNormal,
						$temp$vertices = remainingVertices,
						$temp$contacts = contacts;
					orderContact = $temp$orderContact;
					planePosition = $temp$planePosition;
					planeNormal = $temp$planeNormal;
					vertices = $temp$vertices;
					contacts = $temp$contacts;
					continue addContactsHelp;
				}
			} else {
				return contacts;
			}
		}
	});
var $w0rm$elm_physics$Collision$PlaneConvex$addContacts = F4(
	function (orderContact, plane, _v0, contacts) {
		var vertices = _v0.vertices;
		return A5($w0rm$elm_physics$Collision$PlaneConvex$addContactsHelp, orderContact, plane.position, plane.normal, vertices, contacts);
	});
var $w0rm$elm_physics$Collision$PlaneParticle$addContacts = F4(
	function (orderContact, _v0, particlePosition, contacts) {
		var position = _v0.position;
		var normal = _v0.normal;
		var dot = (((particlePosition.x - position.x) * normal.x) + ((particlePosition.y - position.y) * normal.y)) + ((particlePosition.z - position.z) * normal.z);
		return (dot <= 0) ? A2(
			$elm$core$List$cons,
			orderContact(
				{
					ni: normal,
					pi: {x: particlePosition.x - (dot * normal.x), y: particlePosition.y - (dot * normal.y), z: particlePosition.z - (dot * normal.z)},
					pj: particlePosition
				}),
			contacts) : contacts;
	});
var $w0rm$elm_physics$Collision$PlaneSphere$addContacts = F4(
	function (orderContact, _v0, sphere, contacts) {
		var normal = _v0.normal;
		var position = _v0.position;
		var _v1 = sphere.position;
		var x = _v1.x;
		var y = _v1.y;
		var z = _v1.z;
		var vertex = {x: x - (sphere.radius * normal.x), y: y - (sphere.radius * normal.y), z: z - (sphere.radius * normal.z)};
		var dot = (((vertex.x - position.x) * normal.x) + ((vertex.y - position.y) * normal.y)) + ((vertex.z - position.z) * normal.z);
		return (dot <= 0) ? A2(
			$elm$core$List$cons,
			orderContact(
				{
					ni: normal,
					pi: {x: vertex.x - (dot * normal.x), y: vertex.y - (dot * normal.y), z: vertex.z - (dot * normal.z)},
					pj: vertex
				}),
			contacts) : contacts;
	});
var $w0rm$elm_physics$Internal$Vector3$direction = F2(
	function (a, b) {
		var c = A2($w0rm$elm_physics$Internal$Vector3$sub, a, b);
		var len = $w0rm$elm_physics$Internal$Vector3$length(c);
		return {x: c.x / len, y: c.y / len, z: c.z / len};
	});
var $w0rm$elm_physics$Collision$SphereConvex$QualifiedEdges = function (a) {
	return {$: 'QualifiedEdges', a: a};
};
var $w0rm$elm_physics$Collision$SphereConvex$isAFaceContact = function (testFaceResult) {
	if (testFaceResult.$ === 'FaceContact') {
		return true;
	} else {
		return false;
	}
};
var $w0rm$elm_physics$Collision$SphereConvex$listRecurseUntil = F4(
	function (test, fn, resultSoFar, list) {
		listRecurseUntil:
		while (true) {
			if (test(resultSoFar)) {
				return resultSoFar;
			} else {
				if (list.b) {
					var head = list.a;
					var tail = list.b;
					var acc = A2(fn, head, resultSoFar);
					var $temp$test = test,
						$temp$fn = fn,
						$temp$resultSoFar = acc,
						$temp$list = tail;
					test = $temp$test;
					fn = $temp$fn;
					resultSoFar = $temp$resultSoFar;
					list = $temp$list;
					continue listRecurseUntil;
				} else {
					return resultSoFar;
				}
			}
		}
	});
var $w0rm$elm_physics$Collision$SphereConvex$PossibleVertexContact = function (a) {
	return {$: 'PossibleVertexContact', a: a};
};
var $w0rm$elm_physics$Collision$SphereConvex$isAnEdgeContact = function (testEdgeResult) {
	if (testEdgeResult.$ === 'EdgeContact') {
		return true;
	} else {
		return false;
	}
};
var $w0rm$elm_physics$Collision$SphereConvex$EdgeContact = function (a) {
	return {$: 'EdgeContact', a: a};
};
var $w0rm$elm_physics$Collision$SphereConvex$sphereTestEdge = F3(
	function (prevVertex, vertex, statusQuo) {
		var minDistanceSq = statusQuo.b;
		var edge = A2($w0rm$elm_physics$Internal$Vector3$sub, vertex, prevVertex);
		var edgeUnit = $w0rm$elm_physics$Internal$Vector3$normalize(edge);
		var offset = -A2($w0rm$elm_physics$Internal$Vector3$dot, prevVertex, edgeUnit);
		var betterVertexContact = function (candidate) {
			var vertexLengthSq = $w0rm$elm_physics$Internal$Vector3$lengthSquared(candidate);
			return ((vertexLengthSq - minDistanceSq) < 0) ? _Utils_Tuple2(
				$elm$core$Maybe$Just(candidate),
				vertexLengthSq) : statusQuo;
		};
		if (offset < 0) {
			return $w0rm$elm_physics$Collision$SphereConvex$PossibleVertexContact(
				betterVertexContact(prevVertex));
		} else {
			if (((offset * offset) - $w0rm$elm_physics$Internal$Vector3$lengthSquared(edge)) > 0) {
				return $w0rm$elm_physics$Collision$SphereConvex$PossibleVertexContact(
					betterVertexContact(vertex));
			} else {
				var edgeContact = A2(
					$w0rm$elm_physics$Internal$Vector3$add,
					prevVertex,
					A2($w0rm$elm_physics$Internal$Vector3$scale, offset, edgeUnit));
				var edgeDistanceSq = $w0rm$elm_physics$Internal$Vector3$lengthSquared(edgeContact);
				return ((edgeDistanceSq - minDistanceSq) < 0) ? $w0rm$elm_physics$Collision$SphereConvex$EdgeContact(
					_Utils_Tuple2(edgeContact, edgeDistanceSq)) : $w0rm$elm_physics$Collision$SphereConvex$PossibleVertexContact(statusQuo);
			}
		}
	});
var $w0rm$elm_physics$Collision$SphereConvex$sphereTestBoundary = F2(
	function (faceEdges, statusQuo) {
		return A4(
			$w0rm$elm_physics$Collision$SphereConvex$listRecurseUntil,
			$w0rm$elm_physics$Collision$SphereConvex$isAnEdgeContact,
			F2(
				function (_v0, statusQuo1) {
					var prevVertex = _v0.a;
					var vertex = _v0.b;
					if (statusQuo1.$ === 'PossibleVertexContact') {
						var soFar = statusQuo1.a;
						return A3($w0rm$elm_physics$Collision$SphereConvex$sphereTestEdge, prevVertex, vertex, soFar);
					} else {
						return statusQuo1;
					}
				}),
			statusQuo,
			faceEdges);
	});
var $w0rm$elm_physics$Collision$SphereConvex$sphereTestBoundaries = F2(
	function (radius, faceEdgeList) {
		return A3(
			$elm$core$List$foldl,
			$w0rm$elm_physics$Collision$SphereConvex$sphereTestBoundary,
			$w0rm$elm_physics$Collision$SphereConvex$PossibleVertexContact(
				_Utils_Tuple2($elm$core$Maybe$Nothing, radius * radius)),
			faceEdgeList);
	});
var $w0rm$elm_physics$Collision$SphereConvex$FaceContact = F2(
	function (a, b) {
		return {$: 'FaceContact', a: a, b: b};
	});
var $w0rm$elm_physics$Collision$SphereConvex$originProjection = F2(
	function (vertices, normal) {
		return A3(
			$w0rm$elm_physics$Shapes$Convex$foldFaceEdges,
			F3(
				function (prevVertex, vertex, acc) {
					var edge_x_normal = A2(
						$w0rm$elm_physics$Internal$Vector3$cross,
						normal,
						A2($w0rm$elm_physics$Internal$Vector3$sub, vertex, prevVertex));
					return (A2($w0rm$elm_physics$Internal$Vector3$dot, edge_x_normal, prevVertex) < 0) ? A2(
						$elm$core$List$cons,
						_Utils_Tuple2(prevVertex, vertex),
						acc) : acc;
				}),
			_List_Nil,
			vertices);
	});
var $w0rm$elm_physics$Collision$SphereConvex$sphereTestFace = F4(
	function (radius, normal, vertices, acc) {
		var faceDistance = function () {
			if (vertices.b) {
				var point = vertices.a;
				return -A2($w0rm$elm_physics$Internal$Vector3$dot, normal, point);
			} else {
				return -1;
			}
		}();
		if (((faceDistance - radius) < 0) && (faceDistance > 0.0)) {
			var _v0 = A2($w0rm$elm_physics$Collision$SphereConvex$originProjection, vertices, normal);
			if (!_v0.b) {
				return A2($w0rm$elm_physics$Collision$SphereConvex$FaceContact, normal, faceDistance);
			} else {
				var separatingEdges = _v0;
				return $w0rm$elm_physics$Collision$SphereConvex$QualifiedEdges(
					A2($elm$core$List$cons, separatingEdges, acc));
			}
		} else {
			return $w0rm$elm_physics$Collision$SphereConvex$QualifiedEdges(acc);
		}
	});
var $w0rm$elm_physics$Collision$SphereConvex$sphereContact = F3(
	function (center, radius, _v0) {
		var faces = _v0.faces;
		var sphereFaceContact = F2(
			function (normal, distance) {
				return _Utils_Tuple2(
					$elm$core$Maybe$Just(
						A2(
							$w0rm$elm_physics$Internal$Vector3$sub,
							center,
							A2($w0rm$elm_physics$Internal$Vector3$scale, distance, normal))),
					radius - distance);
			});
		var sphereBoundaryContact = F2(
			function (localContact, distanceSq) {
				return _Utils_Tuple2(
					$elm$core$Maybe$Just(
						A2($w0rm$elm_physics$Internal$Vector3$add, localContact, center)),
					radius - $elm$core$Basics$sqrt(distanceSq));
			});
		var spherePossibleBoundaryContact = function (faceEdgeList) {
			var _v3 = A2($w0rm$elm_physics$Collision$SphereConvex$sphereTestBoundaries, radius, faceEdgeList);
			if (_v3.$ === 'PossibleVertexContact') {
				if (_v3.a.a.$ === 'Just') {
					var _v4 = _v3.a;
					var localContact = _v4.a.a;
					var distanceSq = _v4.b;
					return A2(sphereBoundaryContact, localContact, distanceSq);
				} else {
					var noContact = _v3.a;
					return noContact;
				}
			} else {
				var _v5 = _v3.a;
				var localContact = _v5.a;
				var distanceSq = _v5.b;
				return A2(sphereBoundaryContact, localContact, distanceSq);
			}
		};
		var reframedVertices = function (faceVertices) {
			return A3(
				$elm$core$List$foldl,
				F2(
					function (vertex, acc) {
						return A2(
							$elm$core$List$cons,
							A2($w0rm$elm_physics$Internal$Vector3$sub, vertex, center),
							acc);
					}),
				_List_Nil,
				faceVertices);
		};
		var testFaceResult = A4(
			$w0rm$elm_physics$Collision$SphereConvex$listRecurseUntil,
			$w0rm$elm_physics$Collision$SphereConvex$isAFaceContact,
			F2(
				function (face, statusQuo) {
					if (statusQuo.$ === 'QualifiedEdges') {
						var acc = statusQuo.a;
						return A4(
							$w0rm$elm_physics$Collision$SphereConvex$sphereTestFace,
							radius,
							face.normal,
							reframedVertices(face.vertices),
							acc);
					} else {
						return statusQuo;
					}
				}),
			$w0rm$elm_physics$Collision$SphereConvex$QualifiedEdges(_List_Nil),
			faces);
		if (testFaceResult.$ === 'QualifiedEdges') {
			var faceEdgeList = testFaceResult.a;
			return spherePossibleBoundaryContact(faceEdgeList);
		} else {
			var faceNormal = testFaceResult.a;
			var faceDistance = testFaceResult.b;
			return A2(sphereFaceContact, faceNormal, faceDistance);
		}
	});
var $w0rm$elm_physics$Collision$SphereConvex$addContacts = F4(
	function (orderContact, _v0, hull2, contacts) {
		var radius = _v0.radius;
		var position = _v0.position;
		var _v1 = A3($w0rm$elm_physics$Collision$SphereConvex$sphereContact, position, radius, hull2);
		var maybeContact = _v1.a;
		var penetration = _v1.b;
		if (maybeContact.$ === 'Just') {
			var contact2 = maybeContact.a;
			var normal = A2($w0rm$elm_physics$Internal$Vector3$direction, contact2, position);
			return A2(
				$elm$core$List$cons,
				orderContact(
					{
						ni: normal,
						pi: {x: contact2.x + (penetration * normal.x), y: contact2.y + (penetration * normal.y), z: contact2.z + (penetration * normal.z)},
						pj: contact2
					}),
				contacts);
		} else {
			return contacts;
		}
	});
var $w0rm$elm_physics$Internal$Vector3$distance = F2(
	function (a, b) {
		var z = b.z - a.z;
		var y = b.y - a.y;
		var x = b.x - a.x;
		return $elm$core$Basics$sqrt(((x * x) + (y * y)) + (z * z));
	});
var $w0rm$elm_physics$Collision$SphereParticle$addContacts = F4(
	function (orderContact, _v0, particlePosition, contacts) {
		var radius = _v0.radius;
		var position = _v0.position;
		var normal = A2($w0rm$elm_physics$Internal$Vector3$direction, particlePosition, position);
		var distance = A2($w0rm$elm_physics$Internal$Vector3$distance, particlePosition, position) - radius;
		return (distance > 0) ? contacts : A2(
			$elm$core$List$cons,
			orderContact(
				{
					ni: normal,
					pi: A2(
						$w0rm$elm_physics$Internal$Vector3$add,
						position,
						A2($w0rm$elm_physics$Internal$Vector3$scale, radius - distance, normal)),
					pj: particlePosition
				}),
			contacts);
	});
var $w0rm$elm_physics$Collision$SphereSphere$addContacts = F3(
	function (sphere1, sphere2, contacts) {
		var radius2 = sphere2.radius;
		var radius1 = sphere1.radius;
		var center2 = sphere2.position;
		var center1 = sphere1.position;
		var distance = (A2($w0rm$elm_physics$Internal$Vector3$distance, center2, center1) - radius1) - radius2;
		var normal = A2($w0rm$elm_physics$Internal$Vector3$direction, center2, center1);
		return (distance > 0) ? contacts : A2(
			$elm$core$List$cons,
			{
				ni: normal,
				pi: A2(
					$w0rm$elm_physics$Internal$Vector3$add,
					center1,
					A2($w0rm$elm_physics$Internal$Vector3$scale, radius1 - distance, normal)),
				pj: A2(
					$w0rm$elm_physics$Internal$Vector3$add,
					center2,
					A2($w0rm$elm_physics$Internal$Vector3$scale, -radius2, normal))
			},
			contacts);
	});
var $w0rm$elm_physics$Internal$Contact$flip = function (contact) {
	return {
		ni: $w0rm$elm_physics$Internal$Vector3$negate(contact.ni),
		pi: contact.pj,
		pj: contact.pi
	};
};
var $w0rm$elm_physics$Internal$NarrowPhase$addShapeContacts = F3(
	function (shape1, shape2, contacts) {
		switch (shape1.$) {
			case 'Convex':
				var convex1 = shape1.a;
				switch (shape2.$) {
					case 'Convex':
						var convex2 = shape2.a;
						return A3($w0rm$elm_physics$Collision$ConvexConvex$addContacts, convex1, convex2, contacts);
					case 'Plane':
						var plane2 = shape2.a;
						return A4($w0rm$elm_physics$Collision$PlaneConvex$addContacts, $w0rm$elm_physics$Internal$Contact$flip, plane2, convex1, contacts);
					case 'Sphere':
						var sphere2 = shape2.a;
						return A4($w0rm$elm_physics$Collision$SphereConvex$addContacts, $w0rm$elm_physics$Internal$Contact$flip, sphere2, convex1, contacts);
					default:
						var particle2 = shape2.a;
						return A4($w0rm$elm_physics$Collision$ParticleConvex$addContacts, $w0rm$elm_physics$Internal$Contact$flip, particle2, convex1, contacts);
				}
			case 'Plane':
				var plane1 = shape1.a;
				switch (shape2.$) {
					case 'Plane':
						return contacts;
					case 'Convex':
						var convex2 = shape2.a;
						return A4($w0rm$elm_physics$Collision$PlaneConvex$addContacts, $elm$core$Basics$identity, plane1, convex2, contacts);
					case 'Sphere':
						var sphere2 = shape2.a;
						return A4($w0rm$elm_physics$Collision$PlaneSphere$addContacts, $elm$core$Basics$identity, plane1, sphere2, contacts);
					default:
						var particle2 = shape2.a;
						return A4($w0rm$elm_physics$Collision$PlaneParticle$addContacts, $elm$core$Basics$identity, plane1, particle2, contacts);
				}
			case 'Sphere':
				var sphere1 = shape1.a;
				switch (shape2.$) {
					case 'Plane':
						var plane2 = shape2.a;
						return A4($w0rm$elm_physics$Collision$PlaneSphere$addContacts, $w0rm$elm_physics$Internal$Contact$flip, plane2, sphere1, contacts);
					case 'Convex':
						var convex2 = shape2.a;
						return A4($w0rm$elm_physics$Collision$SphereConvex$addContacts, $elm$core$Basics$identity, sphere1, convex2, contacts);
					case 'Sphere':
						var sphere2 = shape2.a;
						return A3($w0rm$elm_physics$Collision$SphereSphere$addContacts, sphere1, sphere2, contacts);
					default:
						var particle2 = shape2.a;
						return A4($w0rm$elm_physics$Collision$SphereParticle$addContacts, $elm$core$Basics$identity, sphere1, particle2, contacts);
				}
			default:
				var particle1 = shape1.a;
				switch (shape2.$) {
					case 'Plane':
						var plane2 = shape2.a;
						return A4($w0rm$elm_physics$Collision$PlaneParticle$addContacts, $w0rm$elm_physics$Internal$Contact$flip, plane2, particle1, contacts);
					case 'Convex':
						var convex2 = shape2.a;
						return A4($w0rm$elm_physics$Collision$ParticleConvex$addContacts, $elm$core$Basics$identity, particle1, convex2, contacts);
					case 'Sphere':
						var sphere2 = shape2.a;
						return A4($w0rm$elm_physics$Collision$SphereParticle$addContacts, $w0rm$elm_physics$Internal$Contact$flip, sphere2, particle1, contacts);
					default:
						return contacts;
				}
		}
	});
var $w0rm$elm_physics$Internal$NarrowPhase$getContactsHelp = F5(
	function (shape1, currentShapes1, currentShapes2, shapes2, result) {
		getContactsHelp:
		while (true) {
			if (currentShapes2.b) {
				var shape2 = currentShapes2.a;
				var remainingShapes2 = currentShapes2.b;
				var $temp$shape1 = shape1,
					$temp$currentShapes1 = currentShapes1,
					$temp$currentShapes2 = remainingShapes2,
					$temp$shapes2 = shapes2,
					$temp$result = A3($w0rm$elm_physics$Internal$NarrowPhase$addShapeContacts, shape1, shape2, result);
				shape1 = $temp$shape1;
				currentShapes1 = $temp$currentShapes1;
				currentShapes2 = $temp$currentShapes2;
				shapes2 = $temp$shapes2;
				result = $temp$result;
				continue getContactsHelp;
			} else {
				if (currentShapes1.b) {
					var newShape1 = currentShapes1.a;
					var remainingShapes1 = currentShapes1.b;
					var $temp$shape1 = newShape1,
						$temp$currentShapes1 = remainingShapes1,
						$temp$currentShapes2 = shapes2,
						$temp$shapes2 = shapes2,
						$temp$result = result;
					shape1 = $temp$shape1;
					currentShapes1 = $temp$currentShapes1;
					currentShapes2 = $temp$currentShapes2;
					shapes2 = $temp$shapes2;
					result = $temp$result;
					continue getContactsHelp;
				} else {
					return result;
				}
			}
		}
	});
var $w0rm$elm_physics$Internal$NarrowPhase$getContacts = F2(
	function (shapes1, shapes2) {
		if (shapes1.b) {
			var shape1 = shapes1.a;
			var remainingShapes1 = shapes1.b;
			return A5($w0rm$elm_physics$Internal$NarrowPhase$getContactsHelp, shape1, remainingShapes1, shapes2, shapes2, _List_Nil);
		} else {
			return _List_Nil;
		}
	});
var $w0rm$elm_physics$Internal$BroadPhase$addContactsHelp = F4(
	function (body1, currentBodies, restBodies, result) {
		addContactsHelp:
		while (true) {
			if (restBodies.b) {
				var body2 = restBodies.a;
				var newRestBodies = restBodies.b;
				var $temp$body1 = body1,
					$temp$currentBodies = currentBodies,
					$temp$restBodies = newRestBodies,
					$temp$result = function () {
					if (A2($w0rm$elm_physics$Internal$BroadPhase$bodiesMayContact, body1, body2)) {
						var _v1 = A2($w0rm$elm_physics$Internal$NarrowPhase$getContacts, body1.worldShapes, body2.worldShapes);
						if (!_v1.b) {
							return result;
						} else {
							var contacts = _v1;
							return A2(
								$elm$core$List$cons,
								{body1: body1, body2: body2, contacts: contacts},
								result);
						}
					} else {
						return result;
					}
				}();
				body1 = $temp$body1;
				currentBodies = $temp$currentBodies;
				restBodies = $temp$restBodies;
				result = $temp$result;
				continue addContactsHelp;
			} else {
				if (currentBodies.b) {
					var newBody1 = currentBodies.a;
					var newRestBodies = currentBodies.b;
					var $temp$body1 = newBody1,
						$temp$currentBodies = newRestBodies,
						$temp$restBodies = newRestBodies,
						$temp$result = result;
					body1 = $temp$body1;
					currentBodies = $temp$currentBodies;
					restBodies = $temp$restBodies;
					result = $temp$result;
					continue addContactsHelp;
				} else {
					return result;
				}
			}
		}
	});
var $w0rm$elm_physics$Internal$BroadPhase$addContacts = function (world) {
	return _Utils_update(
		world,
		{
			contactGroups: function () {
				var _v0 = world.bodies;
				if (_v0.b) {
					var body = _v0.a;
					var restBodies = _v0.b;
					return A4($w0rm$elm_physics$Internal$BroadPhase$addContactsHelp, body, restBodies, restBodies, _List_Nil);
				} else {
					return _List_Nil;
				}
			}()
		});
};
var $w0rm$elm_physics$Internal$Equation$computeContactB = F5(
	function (bounciness, _v0, bi, bj, _v1) {
		var pi = _v0.pi;
		var pj = _v0.pj;
		var ni = _v0.ni;
		var spookA = _v1.spookA;
		var spookB = _v1.spookB;
		var wA = _v1.wA;
		var wB = _v1.wB;
		var gW = (((bounciness + 1) * (A2($w0rm$elm_physics$Internal$Vector3$dot, bj.velocity, ni) - A2($w0rm$elm_physics$Internal$Vector3$dot, bi.velocity, ni))) + A2($w0rm$elm_physics$Internal$Vector3$dot, bj.angularVelocity, wB)) + A2($w0rm$elm_physics$Internal$Vector3$dot, bi.angularVelocity, wA);
		var g = (((pj.x - pi.x) * ni.x) + ((pj.y - pi.y) * ni.y)) + ((pj.z - pi.z) * ni.z);
		return ((-g) * spookA) - (gW * spookB);
	});
var $w0rm$elm_physics$Internal$Equation$defaultRelaxation = 3;
var $w0rm$elm_physics$Internal$Equation$defaultStiffness = 10000000;
var $w0rm$elm_physics$Internal$Equation$computeC = F3(
	function (bi, bj, _v0) {
		var wA = _v0.wA;
		var wB = _v0.wB;
		var spookEps = _v0.spookEps;
		return (((((((bi.invMass + bj.invMass) + (wA.x * (((bi.invInertiaWorld.m11 * wA.x) + (bi.invInertiaWorld.m12 * wA.y)) + (bi.invInertiaWorld.m13 * wA.z)))) + (wA.y * (((bi.invInertiaWorld.m21 * wA.x) + (bi.invInertiaWorld.m22 * wA.y)) + (bi.invInertiaWorld.m23 * wA.z)))) + (wA.z * (((bi.invInertiaWorld.m31 * wA.x) + (bi.invInertiaWorld.m32 * wA.y)) + (bi.invInertiaWorld.m33 * wA.z)))) + (wB.x * (((bj.invInertiaWorld.m11 * wB.x) + (bj.invInertiaWorld.m12 * wB.y)) + (bj.invInertiaWorld.m13 * wB.z)))) + (wB.y * (((bj.invInertiaWorld.m21 * wB.x) + (bj.invInertiaWorld.m22 * wB.y)) + (bj.invInertiaWorld.m23 * wB.z)))) + (wB.z * (((bj.invInertiaWorld.m31 * wB.x) + (bj.invInertiaWorld.m32 * wB.y)) + (bj.invInertiaWorld.m33 * wB.z)))) + spookEps;
	});
var $w0rm$elm_physics$Internal$Equation$computeGiMf = F4(
	function (gravity, bi, bj, _v0) {
		var wA = _v0.wA;
		var vB = _v0.vB;
		var wB = _v0.wB;
		var gravityj = (bj.mass > 0) ? gravity : $w0rm$elm_physics$Internal$Vector3$zero;
		var gravityi = (bi.mass > 0) ? gravity : $w0rm$elm_physics$Internal$Vector3$zero;
		return (((((((-(((vB.x * ((bi.invMass * bi.force.x) + gravityi.x)) + (vB.y * ((bi.invMass * bi.force.y) + gravityi.y))) + (vB.z * ((bi.invMass * bi.force.z) + gravityi.z)))) + (((vB.x * ((bj.invMass * bj.force.x) + gravityj.x)) + (vB.y * ((bj.invMass * bj.force.y) + gravityj.y))) + (vB.z * ((bj.invMass * bj.force.z) + gravityj.z)))) + (wA.x * (((bi.invInertiaWorld.m11 * bi.torque.x) + (bi.invInertiaWorld.m12 * bi.torque.y)) + (bi.invInertiaWorld.m13 * bi.torque.z)))) + (wA.y * (((bi.invInertiaWorld.m21 * bi.torque.x) + (bi.invInertiaWorld.m22 * bi.torque.y)) + (bi.invInertiaWorld.m23 * bi.torque.z)))) + (wA.z * (((bi.invInertiaWorld.m31 * bi.torque.x) + (bi.invInertiaWorld.m32 * bi.torque.y)) + (bi.invInertiaWorld.m33 * bi.torque.z)))) + (wB.x * (((bj.invInertiaWorld.m11 * bj.torque.x) + (bj.invInertiaWorld.m12 * bj.torque.y)) + (bj.invInertiaWorld.m13 * bj.torque.z)))) + (wB.y * (((bj.invInertiaWorld.m21 * bj.torque.x) + (bj.invInertiaWorld.m22 * bj.torque.y)) + (bj.invInertiaWorld.m23 * bj.torque.z)))) + (wB.z * (((bj.invInertiaWorld.m31 * bj.torque.x) + (bj.invInertiaWorld.m32 * bj.torque.y)) + (bj.invInertiaWorld.m33 * bj.torque.z)));
	});
var $w0rm$elm_physics$Internal$Equation$initSolverParams = F5(
	function (computeB, ctx, bi, bj, solverEquation) {
		return {
			equation: {
				maxForce: solverEquation.maxForce,
				minForce: solverEquation.minForce,
				solverB: A3(computeB, bi, bj, solverEquation) - (ctx.dt * A4($w0rm$elm_physics$Internal$Equation$computeGiMf, ctx.gravity, bi, bj, solverEquation)),
				solverInvC: 1 / A3($w0rm$elm_physics$Internal$Equation$computeC, bi, bj, solverEquation),
				spookA: solverEquation.spookA,
				spookB: solverEquation.spookB,
				spookEps: solverEquation.spookEps,
				vB: solverEquation.vB,
				wA: solverEquation.wA,
				wB: solverEquation.wB
			},
			solverLambda: 0
		};
	});
var $w0rm$elm_physics$Internal$Equation$addDistanceConstraintEquations = F4(
	function (ctx, body1, body2, distance) {
		var spookEps = 4.0 / (((ctx.dt * ctx.dt) * $w0rm$elm_physics$Internal$Equation$defaultStiffness) * (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation)));
		var spookB = (4.0 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation) / (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation));
		var spookA = 4.0 / (ctx.dt * (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation)));
		var ni = A2(
			$w0rm$elm_physics$Internal$Vector3$direction,
			$w0rm$elm_physics$Internal$Transform3d$originPoint(body2.transform3d),
			$w0rm$elm_physics$Internal$Transform3d$originPoint(body1.transform3d));
		var halfDistance = distance / 2;
		var ri = A2($w0rm$elm_physics$Internal$Vector3$scale, halfDistance, ni);
		var rj = A2($w0rm$elm_physics$Internal$Vector3$scale, -halfDistance, ni);
		return $elm$core$List$cons(
			A5(
				$w0rm$elm_physics$Internal$Equation$initSolverParams,
				A2(
					$w0rm$elm_physics$Internal$Equation$computeContactB,
					0,
					{
						ni: ni,
						pi: A2(
							$w0rm$elm_physics$Internal$Vector3$add,
							ri,
							$w0rm$elm_physics$Internal$Transform3d$originPoint(body1.transform3d)),
						pj: A2(
							$w0rm$elm_physics$Internal$Vector3$add,
							rj,
							$w0rm$elm_physics$Internal$Transform3d$originPoint(body2.transform3d))
					}),
				ctx,
				body1,
				body2,
				{
					maxForce: 1000000,
					minForce: -1000000,
					solverB: 0,
					solverInvC: 0,
					spookA: spookA,
					spookB: spookB,
					spookEps: spookEps,
					vB: ni,
					wA: A2($w0rm$elm_physics$Internal$Vector3$cross, ni, ri),
					wB: A2($w0rm$elm_physics$Internal$Vector3$cross, rj, ni)
				}));
	});
var $w0rm$elm_physics$Internal$Equation$computeGW = F3(
	function (bi, bj, _v0) {
		var wA = _v0.wA;
		var vB = _v0.vB;
		var wB = _v0.wB;
		return (((-(((vB.x * bi.velocity.x) + (vB.y * bi.velocity.y)) + (vB.z * bi.velocity.z))) + (((wA.x * bi.angularVelocity.x) + (wA.y * bi.angularVelocity.y)) + (wA.z * bi.angularVelocity.z))) + (((vB.x * bj.velocity.x) + (vB.y * bj.velocity.y)) + (vB.z * bj.velocity.z))) + (((wB.x * bj.angularVelocity.x) + (wB.y * bj.angularVelocity.y)) + (wB.z * bj.angularVelocity.z));
	});
var $w0rm$elm_physics$Internal$Equation$computeRotationalB = F4(
	function (_v0, bi, bj, solverEquation) {
		var ni = _v0.ni;
		var nj = _v0.nj;
		var maxAngleCos = _v0.maxAngleCos;
		var spookA = solverEquation.spookA;
		var spookB = solverEquation.spookB;
		var gW = A3($w0rm$elm_physics$Internal$Equation$computeGW, bi, bj, solverEquation);
		var g = maxAngleCos - A2($w0rm$elm_physics$Internal$Vector3$dot, ni, nj);
		return ((-g) * spookA) - (gW * spookB);
	});
var $w0rm$elm_physics$Internal$Vector3$tangents = function (vec) {
	if ($w0rm$elm_physics$Internal$Vector3$lengthSquared(vec) > 0) {
		var normalized = $w0rm$elm_physics$Internal$Vector3$normalize(vec);
		var v = ($elm$core$Basics$abs(normalized.x) < 0.9) ? A2($w0rm$elm_physics$Internal$Vector3$cross, normalized, $w0rm$elm_physics$Internal$Vector3$xAxis) : A2($w0rm$elm_physics$Internal$Vector3$cross, normalized, $w0rm$elm_physics$Internal$Vector3$yAxis);
		return _Utils_Tuple2(
			v,
			A2($w0rm$elm_physics$Internal$Vector3$cross, normalized, v));
	} else {
		return _Utils_Tuple2($w0rm$elm_physics$Internal$Vector3$xAxis, $w0rm$elm_physics$Internal$Vector3$yAxis);
	}
};
var $w0rm$elm_physics$Internal$Equation$addHingeRotationalConstraintEquations = F6(
	function (ctx, body1, body2, axis1, axis2, equations) {
		var worldAxis2 = A2($w0rm$elm_physics$Internal$Transform3d$directionPlaceIn, body2.transform3d, axis2);
		var worldAxis1 = A2($w0rm$elm_physics$Internal$Transform3d$directionPlaceIn, body1.transform3d, axis1);
		var spookEps = 4.0 / (((ctx.dt * ctx.dt) * $w0rm$elm_physics$Internal$Equation$defaultStiffness) * (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation)));
		var spookB = (4.0 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation) / (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation));
		var spookA = 4.0 / (ctx.dt * (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation)));
		var nj2 = worldAxis2;
		var nj1 = worldAxis2;
		var _v0 = $w0rm$elm_physics$Internal$Vector3$tangents(worldAxis1);
		var ni1 = _v0.a;
		var ni2 = _v0.b;
		return A2(
			$elm$core$List$cons,
			A5(
				$w0rm$elm_physics$Internal$Equation$initSolverParams,
				$w0rm$elm_physics$Internal$Equation$computeRotationalB(
					{maxAngleCos: 0, ni: ni1, nj: nj1}),
				ctx,
				body1,
				body2,
				{
					maxForce: 1000000,
					minForce: -1000000,
					solverB: 0,
					solverInvC: 0,
					spookA: spookA,
					spookB: spookB,
					spookEps: spookEps,
					vB: $w0rm$elm_physics$Internal$Vector3$zero,
					wA: A2($w0rm$elm_physics$Internal$Vector3$cross, nj1, ni1),
					wB: A2($w0rm$elm_physics$Internal$Vector3$cross, ni1, nj1)
				}),
			A2(
				$elm$core$List$cons,
				A5(
					$w0rm$elm_physics$Internal$Equation$initSolverParams,
					$w0rm$elm_physics$Internal$Equation$computeRotationalB(
						{maxAngleCos: 0, ni: ni2, nj: nj2}),
					ctx,
					body1,
					body2,
					{
						maxForce: 1000000,
						minForce: -1000000,
						solverB: 0,
						solverInvC: 0,
						spookA: spookA,
						spookB: spookB,
						spookEps: spookEps,
						vB: $w0rm$elm_physics$Internal$Vector3$zero,
						wA: A2($w0rm$elm_physics$Internal$Vector3$cross, nj2, ni2),
						wB: A2($w0rm$elm_physics$Internal$Vector3$cross, ni2, nj2)
					}),
				equations));
	});
var $w0rm$elm_physics$Internal$Equation$addLockRotationalConstraintEquations = function (ctx) {
	return function (body1) {
		return function (body2) {
			return function (x1) {
				return function (x2) {
					return function (y1) {
						return function (y2) {
							return function (z1) {
								return function (z2) {
									return function (equations) {
										var spookEps = 4.0 / (((ctx.dt * ctx.dt) * $w0rm$elm_physics$Internal$Equation$defaultStiffness) * (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation)));
										var spookB = (4.0 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation) / (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation));
										var spookA = 4.0 / (ctx.dt * (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation)));
										var nj3 = A2($w0rm$elm_physics$Internal$Transform3d$directionPlaceIn, body2.transform3d, x2);
										var nj2 = A2($w0rm$elm_physics$Internal$Transform3d$directionPlaceIn, body2.transform3d, z2);
										var nj1 = A2($w0rm$elm_physics$Internal$Transform3d$directionPlaceIn, body2.transform3d, y2);
										var ni3 = A2($w0rm$elm_physics$Internal$Transform3d$directionPlaceIn, body1.transform3d, z1);
										var ni2 = A2($w0rm$elm_physics$Internal$Transform3d$directionPlaceIn, body1.transform3d, y1);
										var ni1 = A2($w0rm$elm_physics$Internal$Transform3d$directionPlaceIn, body1.transform3d, x1);
										return A2(
											$elm$core$List$cons,
											A5(
												$w0rm$elm_physics$Internal$Equation$initSolverParams,
												$w0rm$elm_physics$Internal$Equation$computeRotationalB(
													{maxAngleCos: 0, ni: ni1, nj: nj1}),
												ctx,
												body1,
												body2,
												{
													maxForce: 1000000,
													minForce: -1000000,
													solverB: 0,
													solverInvC: 0,
													spookA: spookA,
													spookB: spookB,
													spookEps: spookEps,
													vB: $w0rm$elm_physics$Internal$Vector3$zero,
													wA: A2($w0rm$elm_physics$Internal$Vector3$cross, nj1, ni1),
													wB: A2($w0rm$elm_physics$Internal$Vector3$cross, ni1, nj1)
												}),
											A2(
												$elm$core$List$cons,
												A5(
													$w0rm$elm_physics$Internal$Equation$initSolverParams,
													$w0rm$elm_physics$Internal$Equation$computeRotationalB(
														{maxAngleCos: 0, ni: ni2, nj: nj2}),
													ctx,
													body1,
													body2,
													{
														maxForce: 1000000,
														minForce: -1000000,
														solverB: 0,
														solverInvC: 0,
														spookA: spookA,
														spookB: spookB,
														spookEps: spookEps,
														vB: $w0rm$elm_physics$Internal$Vector3$zero,
														wA: A2($w0rm$elm_physics$Internal$Vector3$cross, nj2, ni2),
														wB: A2($w0rm$elm_physics$Internal$Vector3$cross, ni2, nj2)
													}),
												A2(
													$elm$core$List$cons,
													A5(
														$w0rm$elm_physics$Internal$Equation$initSolverParams,
														$w0rm$elm_physics$Internal$Equation$computeRotationalB(
															{maxAngleCos: 0, ni: ni3, nj: nj3}),
														ctx,
														body1,
														body2,
														{
															maxForce: 1000000,
															minForce: -1000000,
															solverB: 0,
															solverInvC: 0,
															spookA: spookA,
															spookB: spookB,
															spookEps: spookEps,
															vB: $w0rm$elm_physics$Internal$Vector3$zero,
															wA: A2($w0rm$elm_physics$Internal$Vector3$cross, nj3, ni3),
															wB: A2($w0rm$elm_physics$Internal$Vector3$cross, ni3, nj3)
														}),
													equations)));
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $w0rm$elm_physics$Internal$Equation$addPointToPointConstraintEquations = F6(
	function (ctx, body1, body2, pivot1, pivot2, equations) {
		var spookEps = 4.0 / (((ctx.dt * ctx.dt) * $w0rm$elm_physics$Internal$Equation$defaultStiffness) * (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation)));
		var spookB = (4.0 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation) / (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation));
		var spookA = 4.0 / (ctx.dt * (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation)));
		var rj = A2($w0rm$elm_physics$Internal$Transform3d$directionPlaceIn, body2.transform3d, pivot2);
		var ri = A2($w0rm$elm_physics$Internal$Transform3d$directionPlaceIn, body1.transform3d, pivot1);
		return A3(
			$elm$core$List$foldl,
			function (ni) {
				return $elm$core$List$cons(
					A5(
						$w0rm$elm_physics$Internal$Equation$initSolverParams,
						A2(
							$w0rm$elm_physics$Internal$Equation$computeContactB,
							0,
							{
								ni: ni,
								pi: A2(
									$w0rm$elm_physics$Internal$Vector3$add,
									$w0rm$elm_physics$Internal$Transform3d$originPoint(body1.transform3d),
									ri),
								pj: A2(
									$w0rm$elm_physics$Internal$Vector3$add,
									$w0rm$elm_physics$Internal$Transform3d$originPoint(body2.transform3d),
									rj)
							}),
						ctx,
						body1,
						body2,
						{
							maxForce: 1000000,
							minForce: -1000000,
							solverB: 0,
							solverInvC: 0,
							spookA: spookA,
							spookB: spookB,
							spookEps: spookEps,
							vB: ni,
							wA: A2($w0rm$elm_physics$Internal$Vector3$cross, ni, ri),
							wB: A2($w0rm$elm_physics$Internal$Vector3$cross, rj, ni)
						}));
			},
			equations,
			$w0rm$elm_physics$Internal$Vector3$basis);
	});
var $w0rm$elm_physics$Internal$Equation$addConstraintEquations = F4(
	function (ctx, body1, body2, constraint) {
		switch (constraint.$) {
			case 'PointToPoint':
				var pivot1 = constraint.a;
				var pivot2 = constraint.b;
				return A5($w0rm$elm_physics$Internal$Equation$addPointToPointConstraintEquations, ctx, body1, body2, pivot1, pivot2);
			case 'Hinge':
				var pivot1 = constraint.a;
				var axis1 = constraint.b;
				var pivot2 = constraint.c;
				var axis2 = constraint.d;
				return A2(
					$elm$core$Basics$composeR,
					A5($w0rm$elm_physics$Internal$Equation$addPointToPointConstraintEquations, ctx, body1, body2, pivot1, pivot2),
					A5($w0rm$elm_physics$Internal$Equation$addHingeRotationalConstraintEquations, ctx, body1, body2, axis1, axis2));
			case 'Lock':
				var pivot1 = constraint.a;
				var x1 = constraint.b;
				var y1 = constraint.c;
				var z1 = constraint.d;
				var pivot2 = constraint.e;
				var x2 = constraint.f;
				var y2 = constraint.g;
				var z2 = constraint.h;
				return A2(
					$elm$core$Basics$composeR,
					A5($w0rm$elm_physics$Internal$Equation$addPointToPointConstraintEquations, ctx, body1, body2, pivot1, pivot2),
					A9($w0rm$elm_physics$Internal$Equation$addLockRotationalConstraintEquations, ctx, body1, body2, x1, x2, y1, y2, z1, z2));
			default:
				var distance = constraint.a;
				return A4($w0rm$elm_physics$Internal$Equation$addDistanceConstraintEquations, ctx, body1, body2, distance);
		}
	});
var $w0rm$elm_physics$Internal$Equation$constraintEquationsGroup = F4(
	function (ctx, body1, body2, constraints) {
		return {
			bodyId1: body1.id,
			bodyId2: body2.id,
			equations: A3(
				$elm$core$List$foldl,
				A3($w0rm$elm_physics$Internal$Equation$addConstraintEquations, ctx, body1, body2),
				_List_Nil,
				constraints)
		};
	});
var $w0rm$elm_physics$Internal$Equation$computeFrictionB = F3(
	function (bi, bj, solverEquation) {
		var spookB = solverEquation.spookB;
		var gW = A3($w0rm$elm_physics$Internal$Equation$computeGW, bi, bj, solverEquation);
		return (-gW) * spookB;
	});
var $w0rm$elm_physics$Internal$Equation$addContactEquations = F7(
	function (ctx, maxFrictionForce, bounciness, body1, body2, contact, equations) {
		var spookEps = 4.0 / (((ctx.dt * ctx.dt) * $w0rm$elm_physics$Internal$Equation$defaultStiffness) * (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation)));
		var spookB = (4.0 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation) / (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation));
		var spookA = 4.0 / (ctx.dt * (1 + (4 * $w0rm$elm_physics$Internal$Equation$defaultRelaxation)));
		var rj = A2(
			$w0rm$elm_physics$Internal$Vector3$sub,
			contact.pj,
			$w0rm$elm_physics$Internal$Transform3d$originPoint(body2.transform3d));
		var ri = A2(
			$w0rm$elm_physics$Internal$Vector3$sub,
			contact.pi,
			$w0rm$elm_physics$Internal$Transform3d$originPoint(body1.transform3d));
		var _v0 = $w0rm$elm_physics$Internal$Vector3$tangents(contact.ni);
		var t1 = _v0.a;
		var t2 = _v0.b;
		return A2(
			$elm$core$List$cons,
			A5(
				$w0rm$elm_physics$Internal$Equation$initSolverParams,
				A2($w0rm$elm_physics$Internal$Equation$computeContactB, bounciness, contact),
				ctx,
				body1,
				body2,
				{
					maxForce: 1000000,
					minForce: 0,
					solverB: 0,
					solverInvC: 0,
					spookA: spookA,
					spookB: spookB,
					spookEps: spookEps,
					vB: contact.ni,
					wA: A2($w0rm$elm_physics$Internal$Vector3$cross, contact.ni, ri),
					wB: A2($w0rm$elm_physics$Internal$Vector3$cross, rj, contact.ni)
				}),
			A2(
				$elm$core$List$cons,
				A5(
					$w0rm$elm_physics$Internal$Equation$initSolverParams,
					$w0rm$elm_physics$Internal$Equation$computeFrictionB,
					ctx,
					body1,
					body2,
					{
						maxForce: maxFrictionForce,
						minForce: -maxFrictionForce,
						solverB: 0,
						solverInvC: 0,
						spookA: spookA,
						spookB: spookB,
						spookEps: spookEps,
						vB: t1,
						wA: A2($w0rm$elm_physics$Internal$Vector3$cross, t1, ri),
						wB: A2($w0rm$elm_physics$Internal$Vector3$cross, rj, t1)
					}),
				A2(
					$elm$core$List$cons,
					A5(
						$w0rm$elm_physics$Internal$Equation$initSolverParams,
						$w0rm$elm_physics$Internal$Equation$computeFrictionB,
						ctx,
						body1,
						body2,
						{
							maxForce: maxFrictionForce,
							minForce: -maxFrictionForce,
							solverB: 0,
							solverInvC: 0,
							spookA: spookA,
							spookB: spookB,
							spookEps: spookEps,
							vB: t2,
							wA: A2($w0rm$elm_physics$Internal$Vector3$cross, t2, ri),
							wB: A2($w0rm$elm_physics$Internal$Vector3$cross, rj, t2)
						}),
					equations)));
	});
var $w0rm$elm_physics$Internal$Material$combine = F2(
	function (v1, v2) {
		var avg = (v1 + v2) * 0.5;
		var temp = (1 + avg) - $elm$core$Basics$abs(1 - avg);
		return (temp + $elm$core$Basics$abs(temp)) * 0.25;
	});
var $w0rm$elm_physics$Internal$Material$contactBounciness = F2(
	function (m1, m2) {
		return A2($w0rm$elm_physics$Internal$Material$combine, m1.bounciness, m2.bounciness);
	});
var $w0rm$elm_physics$Internal$Material$contactFriction = F2(
	function (m1, m2) {
		return A2($w0rm$elm_physics$Internal$Material$combine, m1.friction, m2.friction);
	});
var $w0rm$elm_physics$Internal$Equation$contactEquationsGroup = F2(
	function (ctx, _v0) {
		var body1 = _v0.body1;
		var body2 = _v0.body2;
		var contacts = _v0.contacts;
		var maxFrictionForce = ((body1.invMass + body2.invMass) > 0) ? ((A2($w0rm$elm_physics$Internal$Material$contactFriction, body1.material, body2.material) * ctx.gravityLength) / (body1.invMass + body2.invMass)) : 0;
		var bounciness = A2($w0rm$elm_physics$Internal$Material$contactBounciness, body1.material, body2.material);
		return {
			bodyId1: body1.id,
			bodyId2: body2.id,
			equations: A3(
				$elm$core$List$foldl,
				A5($w0rm$elm_physics$Internal$Equation$addContactEquations, ctx, maxFrictionForce, bounciness, body1, body2),
				_List_Nil,
				contacts)
		};
	});
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $w0rm$elm_physics$Internal$SolverBody$fromBody = function (body) {
	return {body: body, vX: 0, vY: 0, vZ: 0, wX: 0, wY: 0, wZ: 0};
};
var $elm$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			$elm$core$Array$initialize,
			n,
			function (_v0) {
				return e;
			});
	});
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (_v0.$ === 'SubTree') {
			var subTree = _v0.a;
			var newSub = A4($elm$core$Array$setHelp, shift - $elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _v0.a;
			var newLeaf = A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, values);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var $elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, tail)) : A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4($elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var $w0rm$elm_physics$Internal$Solver$makeSolverBodies = F2(
	function (nextBodyId, bodies) {
		if (!bodies.b) {
			return _Utils_Tuple2($elm$core$Array$empty, $elm$core$Maybe$Nothing);
		} else {
			var firstBody = bodies.a;
			var fillingBody = $w0rm$elm_physics$Internal$SolverBody$fromBody(
				_Utils_update(
					firstBody,
					{id: -1}));
			var allBodies = A2($elm$core$Array$repeat, nextBodyId, fillingBody);
			return _Utils_Tuple2(
				A3(
					$elm$core$List$foldl,
					function (body) {
						return A2(
							$elm$core$Array$set,
							body.id,
							$w0rm$elm_physics$Internal$SolverBody$fromBody(body));
					},
					allBodies,
					bodies),
				$elm$core$Maybe$Just(fillingBody));
		}
	});
var $w0rm$elm_physics$Internal$Solver$maxIterations = 20;
var $w0rm$elm_physics$Internal$Solver$solveEquationsGroup = F5(
	function (body1, body2, equations, deltalambdaTot, currentEquations) {
		solveEquationsGroup:
		while (true) {
			if (!currentEquations.b) {
				return {body1: body1, body2: body2, deltalambdaTot: deltalambdaTot, equations: equations};
			} else {
				var solverLambda = currentEquations.a.solverLambda;
				var equation = currentEquations.a.equation;
				var remainingEquations = currentEquations.b;
				var invI2 = body2.body.invInertiaWorld;
				var invI1 = body1.body.invInertiaWorld;
				var _v1 = equation;
				var wA = _v1.wA;
				var vB = _v1.vB;
				var wB = _v1.wB;
				var minForce = _v1.minForce;
				var maxForce = _v1.maxForce;
				var solverB = _v1.solverB;
				var spookEps = _v1.spookEps;
				var solverInvC = _v1.solverInvC;
				var gWlambda = (((-(((vB.x * body1.vX) + (vB.y * body1.vY)) + (vB.z * body1.vZ))) + (((wA.x * body1.wX) + (wA.y * body1.wY)) + (wA.z * body1.wZ))) + (((vB.x * body2.vX) + (vB.y * body2.vY)) + (vB.z * body2.vZ))) + (((wB.x * body2.wX) + (wB.y * body2.wY)) + (wB.z * body2.wZ));
				var deltalambdaPrev = solverInvC * ((solverB - gWlambda) - (spookEps * solverLambda));
				var deltalambda = (((solverLambda + deltalambdaPrev) - minForce) < 0) ? (minForce - solverLambda) : ((((solverLambda + deltalambdaPrev) - maxForce) > 0) ? (maxForce - solverLambda) : deltalambdaPrev);
				var k1 = deltalambda * body1.body.invMass;
				var k2 = deltalambda * body2.body.invMass;
				var newBody1 = (body1.body.mass > 0) ? {body: body1.body, vX: body1.vX - (k1 * vB.x), vY: body1.vY - (k1 * vB.y), vZ: body1.vZ - (k1 * vB.z), wX: body1.wX + ((((invI1.m11 * wA.x) + (invI1.m12 * wA.y)) + (invI1.m13 * wA.z)) * deltalambda), wY: body1.wY + ((((invI1.m21 * wA.x) + (invI1.m22 * wA.y)) + (invI1.m23 * wA.z)) * deltalambda), wZ: body1.wZ + ((((invI1.m31 * wA.x) + (invI1.m32 * wA.y)) + (invI1.m33 * wA.z)) * deltalambda)} : body1;
				var newBody2 = (body2.body.mass > 0) ? {body: body2.body, vX: body2.vX + (k2 * vB.x), vY: body2.vY + (k2 * vB.y), vZ: body2.vZ + (k2 * vB.z), wX: body2.wX + ((((invI2.m11 * wB.x) + (invI2.m12 * wB.y)) + (invI2.m13 * wB.z)) * deltalambda), wY: body2.wY + ((((invI2.m21 * wB.x) + (invI2.m22 * wB.y)) + (invI2.m23 * wB.z)) * deltalambda), wZ: body2.wZ + ((((invI2.m31 * wB.x) + (invI2.m32 * wB.y)) + (invI2.m33 * wB.z)) * deltalambda)} : body2;
				var $temp$body1 = newBody1,
					$temp$body2 = newBody2,
					$temp$equations = A2(
					$elm$core$List$cons,
					{equation: equation, solverLambda: solverLambda + deltalambda},
					equations),
					$temp$deltalambdaTot = deltalambdaTot + $elm$core$Basics$abs(deltalambda),
					$temp$currentEquations = remainingEquations;
				body1 = $temp$body1;
				body2 = $temp$body2;
				equations = $temp$equations;
				deltalambdaTot = $temp$deltalambdaTot;
				currentEquations = $temp$currentEquations;
				continue solveEquationsGroup;
			}
		}
	});
var $w0rm$elm_physics$Internal$Solver$step = F6(
	function (number, deltalambdaTot, equationsGroups, currentEquationsGroups, prevBody1, solverBodies) {
		step:
		while (true) {
			if (!currentEquationsGroups.b) {
				if ((!number) || ((deltalambdaTot - $w0rm$elm_physics$Internal$Const$precision) < 0)) {
					return A3($elm$core$Array$set, prevBody1.body.id, prevBody1, solverBodies);
				} else {
					var $temp$number = number - 1,
						$temp$deltalambdaTot = 0,
						$temp$equationsGroups = _List_Nil,
						$temp$currentEquationsGroups = $elm$core$List$reverse(equationsGroups),
						$temp$prevBody1 = prevBody1,
						$temp$solverBodies = solverBodies;
					number = $temp$number;
					deltalambdaTot = $temp$deltalambdaTot;
					equationsGroups = $temp$equationsGroups;
					currentEquationsGroups = $temp$currentEquationsGroups;
					prevBody1 = $temp$prevBody1;
					solverBodies = $temp$solverBodies;
					continue step;
				}
			} else {
				var bodyId1 = currentEquationsGroups.a.bodyId1;
				var bodyId2 = currentEquationsGroups.a.bodyId2;
				var equations = currentEquationsGroups.a.equations;
				var remainingEquationsGroups = currentEquationsGroups.b;
				var newSolverBodies = ((!(bodyId1 - prevBody1.body.id)) || (!prevBody1.body.mass)) ? solverBodies : A3($elm$core$Array$set, prevBody1.body.id, prevBody1, solverBodies);
				var maybeBody1 = (!(bodyId1 - prevBody1.body.id)) ? $elm$core$Maybe$Just(prevBody1) : A2($elm$core$Array$get, bodyId1, solverBodies);
				if (maybeBody1.$ === 'Just') {
					var body1 = maybeBody1.a;
					var _v2 = A2($elm$core$Array$get, bodyId2, newSolverBodies);
					if (_v2.$ === 'Just') {
						var body2 = _v2.a;
						var groupContext = A5($w0rm$elm_physics$Internal$Solver$solveEquationsGroup, body1, body2, _List_Nil, deltalambdaTot, equations);
						var $temp$number = number,
							$temp$deltalambdaTot = groupContext.deltalambdaTot,
							$temp$equationsGroups = A2(
							$elm$core$List$cons,
							{bodyId1: bodyId1, bodyId2: bodyId2, equations: groupContext.equations},
							equationsGroups),
							$temp$currentEquationsGroups = remainingEquationsGroups,
							$temp$prevBody1 = groupContext.body1,
							$temp$solverBodies = (groupContext.body2.body.mass > 0) ? A3($elm$core$Array$set, bodyId2, groupContext.body2, newSolverBodies) : newSolverBodies;
						number = $temp$number;
						deltalambdaTot = $temp$deltalambdaTot;
						equationsGroups = $temp$equationsGroups;
						currentEquationsGroups = $temp$currentEquationsGroups;
						prevBody1 = $temp$prevBody1;
						solverBodies = $temp$solverBodies;
						continue step;
					} else {
						var $temp$number = number,
							$temp$deltalambdaTot = deltalambdaTot,
							$temp$equationsGroups = equationsGroups,
							$temp$currentEquationsGroups = remainingEquationsGroups,
							$temp$prevBody1 = prevBody1,
							$temp$solverBodies = newSolverBodies;
						number = $temp$number;
						deltalambdaTot = $temp$deltalambdaTot;
						equationsGroups = $temp$equationsGroups;
						currentEquationsGroups = $temp$currentEquationsGroups;
						prevBody1 = $temp$prevBody1;
						solverBodies = $temp$solverBodies;
						continue step;
					}
				} else {
					var $temp$number = number,
						$temp$deltalambdaTot = deltalambdaTot,
						$temp$equationsGroups = equationsGroups,
						$temp$currentEquationsGroups = remainingEquationsGroups,
						$temp$prevBody1 = prevBody1,
						$temp$solverBodies = newSolverBodies;
					number = $temp$number;
					deltalambdaTot = $temp$deltalambdaTot;
					equationsGroups = $temp$equationsGroups;
					currentEquationsGroups = $temp$currentEquationsGroups;
					prevBody1 = $temp$prevBody1;
					solverBodies = $temp$solverBodies;
					continue step;
				}
			}
		}
	});
var $elm$core$Elm$JsArray$map = _JsArray_map;
var $elm$core$Array$map = F2(
	function (func, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = function (node) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return $elm$core$Array$SubTree(
					A2($elm$core$Elm$JsArray$map, helper, subTree));
			} else {
				var values = node.a;
				return $elm$core$Array$Leaf(
					A2($elm$core$Elm$JsArray$map, func, values));
			}
		};
		return A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A2($elm$core$Elm$JsArray$map, helper, tree),
			A2($elm$core$Elm$JsArray$map, func, tail));
	});
var $w0rm$elm_physics$Internal$Transform3d$normalize = function (_v0) {
	var localOrigin = _v0.a;
	var _v1 = _v0.b;
	var x = _v1.a;
	var y = _v1.b;
	var z = _v1.c;
	var w = _v1.d;
	var len = $elm$core$Basics$sqrt((((x * x) + (y * y)) + (z * z)) + (w * w));
	return A2(
		$w0rm$elm_physics$Internal$Transform3d$Transform3d,
		localOrigin,
		A4($w0rm$elm_physics$Internal$Transform3d$Orientation3d, x / len, y / len, z / len, w / len));
};
var $w0rm$elm_physics$Internal$Transform3d$rotateBy = F2(
	function (_v0, _v1) {
		var x = _v0.x;
		var y = _v0.y;
		var z = _v0.z;
		var localOrigin = _v1.a;
		var _v2 = _v1.b;
		var qx = _v2.a;
		var qy = _v2.b;
		var qz = _v2.c;
		var qw = _v2.d;
		return A2(
			$w0rm$elm_physics$Internal$Transform3d$Transform3d,
			localOrigin,
			A4($w0rm$elm_physics$Internal$Transform3d$Orientation3d, qx + ((((x * qw) + (y * qz)) - (z * qy)) * 0.5), qy + ((((y * qw) + (z * qx)) - (x * qz)) * 0.5), qz + ((((z * qw) + (x * qy)) - (y * qx)) * 0.5), qw + (((((-x) * qx) - (y * qy)) - (z * qz)) * 0.5)));
	});
var $w0rm$elm_physics$Internal$SolverBody$toBody = F2(
	function (_v0, _v1) {
		var dt = _v0.dt;
		var gravity = _v0.gravity;
		var body = _v1.body;
		var vX = _v1.vX;
		var vY = _v1.vY;
		var vZ = _v1.vZ;
		var wX = _v1.wX;
		var wY = _v1.wY;
		var wZ = _v1.wZ;
		var ld = A2($elm$core$Basics$pow, 1.0 - body.linearDamping, dt);
		var newVelocity = {x: (((gravity.x + (body.force.x * body.invMass)) * dt) + (body.velocity.x * ld)) + vX, y: (((gravity.y + (body.force.y * body.invMass)) * dt) + (body.velocity.y * ld)) + vY, z: (((gravity.z + (body.force.z * body.invMass)) * dt) + (body.velocity.z * ld)) + vZ};
		var velocityLength = $w0rm$elm_physics$Internal$Vector3$length(newVelocity);
		var cappedVelocity = ((!velocityLength) || ((!body.boundingSphereRadius) || (((velocityLength * dt) - body.boundingSphereRadius) < 0))) ? newVelocity : A2($w0rm$elm_physics$Internal$Vector3$scale, body.boundingSphereRadius / (velocityLength * dt), newVelocity);
		var ad = A2($elm$core$Basics$pow, 1.0 - body.angularDamping, dt);
		var newAngularVelocity = {x: (((((body.invInertiaWorld.m11 * body.torque.x) + (body.invInertiaWorld.m12 * body.torque.y)) + (body.invInertiaWorld.m13 * body.torque.z)) * dt) + (body.angularVelocity.x * ad)) + wX, y: (((((body.invInertiaWorld.m21 * body.torque.x) + (body.invInertiaWorld.m22 * body.torque.y)) + (body.invInertiaWorld.m23 * body.torque.z)) * dt) + (body.angularVelocity.y * ad)) + wY, z: (((((body.invInertiaWorld.m31 * body.torque.x) + (body.invInertiaWorld.m32 * body.torque.y)) + (body.invInertiaWorld.m33 * body.torque.z)) * dt) + (body.angularVelocity.z * ad)) + wZ};
		var newTransform3d = $w0rm$elm_physics$Internal$Transform3d$normalize(
			A2(
				$w0rm$elm_physics$Internal$Transform3d$translateBy,
				{x: cappedVelocity.x * dt, y: cappedVelocity.y * dt, z: cappedVelocity.z * dt},
				A2(
					$w0rm$elm_physics$Internal$Transform3d$rotateBy,
					{x: newAngularVelocity.x * dt, y: newAngularVelocity.y * dt, z: newAngularVelocity.z * dt},
					body.transform3d)));
		return {
			angularDamping: body.angularDamping,
			angularVelocity: newAngularVelocity,
			boundingSphereRadius: body.boundingSphereRadius,
			centerOfMassTransform3d: body.centerOfMassTransform3d,
			data: body.data,
			force: $w0rm$elm_physics$Internal$Vector3$zero,
			id: body.id,
			invInertia: body.invInertia,
			invInertiaWorld: A2($w0rm$elm_physics$Internal$Transform3d$invertedInertiaRotateIn, newTransform3d, body.invInertia),
			invMass: body.invMass,
			linearDamping: body.linearDamping,
			mass: body.mass,
			material: body.material,
			shapes: body.shapes,
			torque: $w0rm$elm_physics$Internal$Vector3$zero,
			transform3d: newTransform3d,
			velocity: newVelocity,
			worldShapes: A2($w0rm$elm_physics$Internal$Shape$shapesPlaceIn, newTransform3d, body.shapes)
		};
	});
var $w0rm$elm_physics$Internal$Solver$updateBodies = F3(
	function (ctx, bodies, world) {
		var simulatedBodies = A2(
			$elm$core$Array$map,
			function (solverBody) {
				return ((solverBody.body.id + 1) > 0) ? ((solverBody.body.mass > 0) ? A2($w0rm$elm_physics$Internal$SolverBody$toBody, ctx, solverBody) : solverBody.body) : solverBody.body;
			},
			bodies);
		return _Utils_update(
			world,
			{
				bodies: A2(
					$elm$core$List$filter,
					function (_v0) {
						var id = _v0.id;
						return (id + 1) > 0;
					},
					$elm$core$Array$toList(simulatedBodies)),
				simulatedBodies: simulatedBodies
			});
	});
var $w0rm$elm_physics$Internal$Solver$solve = F2(
	function (dt, world) {
		var ctx = {
			dt: dt,
			gravity: world.gravity,
			gravityLength: $w0rm$elm_physics$Internal$Vector3$length(world.gravity)
		};
		var contactEquationsGroups = A3(
			$elm$core$List$foldl,
			F2(
				function (contactGroup, groups) {
					return A2(
						$elm$core$List$cons,
						A2($w0rm$elm_physics$Internal$Equation$contactEquationsGroup, ctx, contactGroup),
						groups);
				}),
			_List_Nil,
			world.contactGroups);
		var _v0 = A2($w0rm$elm_physics$Internal$Solver$makeSolverBodies, world.nextBodyId, world.bodies);
		var solverBodies = _v0.a;
		var maybeFillingBody = _v0.b;
		var equationsGroups = A3(
			$elm$core$List$foldl,
			F2(
				function (_v2, groups) {
					var bodyId1 = _v2.bodyId1;
					var bodyId2 = _v2.bodyId2;
					var constraints = _v2.constraints;
					var _v3 = A2($elm$core$Array$get, bodyId1, solverBodies);
					if (_v3.$ === 'Nothing') {
						return groups;
					} else {
						var body1 = _v3.a;
						var _v4 = A2($elm$core$Array$get, bodyId2, solverBodies);
						if (_v4.$ === 'Nothing') {
							return groups;
						} else {
							var body2 = _v4.a;
							return A2(
								$elm$core$List$cons,
								A4($w0rm$elm_physics$Internal$Equation$constraintEquationsGroup, ctx, body1.body, body2.body, constraints),
								groups);
						}
					}
				}),
			contactEquationsGroups,
			world.constraints);
		var solvedBodies = function () {
			if (maybeFillingBody.$ === 'Just') {
				var fillingBody = maybeFillingBody.a;
				return A6($w0rm$elm_physics$Internal$Solver$step, $w0rm$elm_physics$Internal$Solver$maxIterations, 0, _List_Nil, equationsGroups, fillingBody, solverBodies);
			} else {
				return solverBodies;
			}
		}();
		return A3($w0rm$elm_physics$Internal$Solver$updateBodies, ctx, solvedBodies, world);
	});
var $w0rm$elm_physics$Physics$World$simulate = F2(
	function (dt, _v0) {
		var world = _v0.a;
		return $w0rm$elm_physics$Internal$World$Protected(
			A2(
				$w0rm$elm_physics$Internal$Solver$solve,
				$ianmackenzie$elm_units$Duration$inSeconds(dt),
				$w0rm$elm_physics$Internal$BroadPhase$addContacts(world)));
	});
var $ianmackenzie$elm_geometry$Geometry$Types$Plane3d = function (a) {
	return {$: 'Plane3d', a: a};
};
var $ianmackenzie$elm_geometry$Plane3d$through = F2(
	function (givenPoint, givenNormalDirection) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Plane3d(
			{normalDirection: givenNormalDirection, originPoint: givenPoint});
	});
var $ianmackenzie$elm_units$Quantity$times = F2(
	function (_v0, _v1) {
		var y = _v0.a;
		var x = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(x * y);
	});
var $ianmackenzie$elm_geometry$Point3d$toTuple = F2(
	function (fromQuantity, point) {
		return _Utils_Tuple3(
			fromQuantity(
				$ianmackenzie$elm_geometry$Point3d$xCoordinate(point)),
			fromQuantity(
				$ianmackenzie$elm_geometry$Point3d$yCoordinate(point)),
			fromQuantity(
				$ianmackenzie$elm_geometry$Point3d$zCoordinate(point)));
	});
var $ianmackenzie$elm_geometry$Point3d$translateBy = F2(
	function (_v0, _v1) {
		var v = _v0.a;
		var p = _v1.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Point3d(
			{x: p.x + v.x, y: p.y + v.y, z: p.z + v.z});
	});
var $w0rm$elm_physics$Physics$World$update = F2(
	function (fn, _v0) {
		var world = _v0.a;
		var internalUpdate = function (body) {
			var _v1 = fn(
				$w0rm$elm_physics$Internal$Body$Protected(body));
			var updatedBody = _v1.a;
			return _Utils_update(
				updatedBody,
				{id: body.id});
		};
		return $w0rm$elm_physics$Internal$World$Protected(
			_Utils_update(
				world,
				{
					bodies: A3(
						$elm$core$List$foldl,
						F2(
							function (body, result) {
								return A2(
									$elm$core$List$cons,
									internalUpdate(body),
									result);
							}),
						_List_Nil,
						world.bodies)
				}));
	});
var $ianmackenzie$elm_geometry$Vector3d$unsafe = function (givenComponents) {
	return $ianmackenzie$elm_geometry$Geometry$Types$Vector3d(givenComponents);
};
var $w0rm$elm_physics$Physics$Body$velocity = function (_v0) {
	var body = _v0.a;
	return $ianmackenzie$elm_geometry$Vector3d$unsafe(body.velocity);
};
var $ianmackenzie$elm_3d_camera$Viewpoint3d$viewDirection = function (_v0) {
	var frame = _v0.a;
	return $ianmackenzie$elm_geometry$Direction3d$reverse(
		$ianmackenzie$elm_geometry$Frame3d$zDirection(frame));
};
var $ianmackenzie$elm_3d_camera$Camera3d$viewpoint = function (_v0) {
	var camera = _v0.a;
	return camera.viewpoint;
};
var $author$project$Main$reactTo = function (event) {
	switch (event.$) {
		case 'AudioLoaded':
			var audioLoaded = event.a;
			return function (state) {
				return $author$project$Reaction$to(
					_Utils_update(
						state,
						{
							audio: A3(
								$author$project$Main$alterAudioOfKind,
								audioLoaded.piece,
								function (_v1) {
									return audioLoaded.result;
								},
								state.audio)
						}));
			};
		case 'GameWindowSized':
			var size = event.a;
			return function (state) {
				return $author$project$Reaction$to(
					_Utils_update(
						state,
						{windowSize: size}));
			};
		case 'InitialRandomSeedReceived':
			var initialRandomSeed = event.a;
			return function (state) {
				return $author$project$Reaction$to(
					_Utils_update(
						state,
						{randomSeed: initialRandomSeed}));
			};
		case 'InitialTimeReceived':
			var initialTime = event.a;
			return function (state) {
				return $author$project$Reaction$to(
					_Utils_update(
						state,
						{initialTime: initialTime, lastTick: initialTime}));
			};
		case 'FrameTickPassed':
			var newSimulationTime = event.a;
			return function (state) {
				var withNewLastTick = _Utils_update(
					state,
					{lastTick: newSimulationTime});
				var sinceLastTick = A2($ianmackenzie$elm_units$Duration$from, state.lastTick, newSimulationTime);
				var newSimulatedPhysicsWorld = A2($w0rm$elm_physics$Physics$World$simulate, sinceLastTick, state.world);
				var _v2 = A2(
					$elm$core$List$filterMap,
					function (body) {
						var _v3 = $w0rm$elm_physics$Physics$Body$data(body);
						if (_v3.$ === 'Player') {
							return $elm$core$Maybe$Just(body);
						} else {
							return $elm$core$Maybe$Nothing;
						}
					},
					$w0rm$elm_physics$Physics$World$bodies(newSimulatedPhysicsWorld));
				if (!_v2.b) {
					return $author$project$Reaction$to(withNewLastTick);
				} else {
					var playerBody = _v2.a;
					var newCameraHeight = function () {
						var _v12 = withNewLastTick.playerPast;
						if (_v12.$ === 'PlayerLeavingTrail') {
							return $author$project$Main$cameraHeight;
						} else {
							return ($ianmackenzie$elm_units$Length$inMeters(
								$ianmackenzie$elm_geometry$Point3d$zCoordinate(
									$ianmackenzie$elm_3d_camera$Viewpoint3d$eyePoint(
										$ianmackenzie$elm_3d_camera$Camera3d$viewpoint(withNewLastTick.camera)))) + 0.001) * 1.0015;
						}
					}();
					var _v4 = A2(
						$ianmackenzie$elm_geometry$Point3d$toTuple,
						$ianmackenzie$elm_units$Length$inMeters,
						$w0rm$elm_physics$Physics$Body$originPoint(playerBody));
					var playerX = _v4.a;
					var playerY = _v4.b;
					var newPhysicsWorld = A2(
						$w0rm$elm_physics$Physics$World$update,
						function (body) {
							var _v9 = $w0rm$elm_physics$Physics$Body$data(body);
							switch (_v9.$) {
								case 'MouseImitation':
									var mouseImitation = _v9.a;
									if ((_Utils_cmp(
										playerX,
										$ianmackenzie$elm_units$Length$inMeters(
											$ianmackenzie$elm_geometry$Point3d$xCoordinate(
												$w0rm$elm_physics$Physics$Body$originPoint(body))) - 1) > -1) && (playerX < 16.5)) {
										var toPlayerX = A2(
											$ianmackenzie$elm_geometry$Vector3d$from,
											$w0rm$elm_physics$Physics$Body$originPoint(body),
											A3(
												$ianmackenzie$elm_geometry$Point3d$meters,
												playerX,
												$ianmackenzie$elm_units$Length$inMeters(
													$ianmackenzie$elm_geometry$Point3d$yCoordinate(
														$w0rm$elm_physics$Physics$Body$originPoint(body))),
												0));
										return A4(
											$w0rm$elm_physics$Physics$Body$applyForce,
											A2(
												$ianmackenzie$elm_units$Quantity$times,
												$ianmackenzie$elm_units$Acceleration$metersPerSecondSquared(
													$ianmackenzie$elm_units$Length$inMeters(
														$ianmackenzie$elm_geometry$Vector3d$length(toPlayerX))),
												$ianmackenzie$elm_units$Mass$kilograms(0.2)),
											A2(
												$elm$core$Maybe$withDefault,
												$ianmackenzie$elm_geometry$Direction3d$positiveX,
												$ianmackenzie$elm_geometry$Vector3d$direction(toPlayerX)),
											$w0rm$elm_physics$Physics$Body$originPoint(body),
											body);
									} else {
										return body;
									}
								case 'PlayerPast':
									var _v10 = withNewLastTick.playerPast;
									if (_v10.$ === 'PlayerLeavingTrail') {
										return body;
									} else {
										if (!_v10.a.b) {
											return body;
										} else {
											var _v11 = _v10.a;
											var position = _v11.a;
											var trailAfterPosition = _v11.b;
											return A3(
												$w0rm$elm_physics$Physics$Body$rotateAround,
												$ianmackenzie$elm_geometry$Axis3d$z,
												$ianmackenzie$elm_units$Angle$turns(0.01),
												A2(
													$w0rm$elm_physics$Physics$Body$moveTo,
													A2(
														$ianmackenzie$elm_geometry$Point3d$translateBy,
														A3($ianmackenzie$elm_geometry$Vector3d$meters, 17, -(1.5 * $author$project$Main$ratioWidthToHeight), 0),
														position),
													body));
										}
									}
								default:
									return body;
							}
						},
						newSimulatedPhysicsWorld);
					var roomCenter = {
						x: $elm$core$Basics$round(playerX),
						y: $elm$core$Basics$round(playerY / $author$project$Main$ratioWidthToHeight) * $author$project$Main$ratioWidthToHeight
					};
					var cameraInNewRoom = $ianmackenzie$elm_3d_camera$Camera3d$perspective(
						{
							verticalFieldOfView: $ianmackenzie$elm_units$Angle$degrees(60),
							viewpoint: $ianmackenzie$elm_3d_camera$Viewpoint3d$lookAt(
								{
									eyePoint: A3($ianmackenzie$elm_geometry$Point3d$meters, roomCenter.x, roomCenter.y, newCameraHeight),
									focalPoint: A3($ianmackenzie$elm_geometry$Point3d$meters, roomCenter.x, roomCenter.y, 0),
									upDirection: $ianmackenzie$elm_geometry$Direction3d$positiveZ
								})
						});
					var withCameraInNewRoom = _Utils_update(
						withNewLastTick,
						{
							camera: cameraInNewRoom,
							playerPast: function () {
								if (playerX >= 16.94) {
									var _v6 = withNewLastTick.playerPast;
									if (_v6.$ === 'PlayerLeavingTrail') {
										var trail = _v6.a;
										return $author$project$Main$PlayerPastFrozen(
											$elm$core$List$reverse(trail));
									} else {
										return withNewLastTick.playerPast;
									}
								} else {
									var _v7 = withNewLastTick.playerPast;
									if (_v7.$ === 'PlayerLeavingTrail') {
										var trail = _v7.a;
										return A2(
											$ianmackenzie$elm_units$Quantity$lessThan,
											$ianmackenzie$elm_units$Length$meters(0.0001),
											$ianmackenzie$elm_geometry$Vector3d$length(
												A2(
													$ianmackenzie$elm_geometry$Vector3d$for,
													$ianmackenzie$elm_units$Duration$second,
													$w0rm$elm_physics$Physics$Body$velocity(playerBody)))) ? withNewLastTick.playerPast : $author$project$Main$PlayerLeavingTrail(
											A2(
												$elm$core$List$cons,
												$w0rm$elm_physics$Physics$Body$originPoint(playerBody),
												trail));
									} else {
										if (!_v7.a.b) {
											return $author$project$Main$PlayerPastFrozen(_List_Nil);
										} else {
											var _v8 = _v7.a;
											var trailAfterPosition = _v8.b;
											return $author$project$Main$PlayerPastFrozen(trailAfterPosition);
										}
									}
								}
							}()
						});
					var _v5 = A2(
						$ianmackenzie$elm_geometry$Point3d$toTuple,
						$ianmackenzie$elm_units$Length$inMeters,
						$ianmackenzie$elm_3d_camera$Viewpoint3d$eyePoint(
							$ianmackenzie$elm_3d_camera$Camera3d$viewpoint(state.camera)));
					var oldCameraX = _v5.a;
					var oldCameraY = _v5.b;
					return ((!_Utils_eq(oldCameraX, roomCenter.x)) || (!_Utils_eq(oldCameraY, roomCenter.y))) ? $author$project$Reaction$to(
						_Utils_update(
							withCameraInNewRoom,
							{
								audioTimes: function (r) {
									return _Utils_update(
										r,
										{
											roomChange: A2($elm$core$List$cons, newSimulationTime, r.roomChange)
										});
								}(withCameraInNewRoom.audioTimes),
								world: A2(
									$w0rm$elm_physics$Physics$World$keepIf,
									function (body) {
										return !_Utils_eq(
											$w0rm$elm_physics$Physics$Body$data(body),
											$author$project$Main$Mouse);
									},
									newPhysicsWorld)
							})) : (A2(
						$ianmackenzie$elm_units$Quantity$greaterThan,
						$ianmackenzie$elm_units$Length$meters(1.4),
						$ianmackenzie$elm_geometry$Vector3d$length(
							A2(
								$ianmackenzie$elm_geometry$Vector3d$for,
								$ianmackenzie$elm_units$Duration$second,
								$w0rm$elm_physics$Physics$Body$velocity(playerBody)))) ? $author$project$Reaction$to(
						_Utils_update(
							withCameraInNewRoom,
							{
								world: A2(
									$w0rm$elm_physics$Physics$World$update,
									function (body) {
										return _Utils_eq(body, playerBody) ? A2(
											$w0rm$elm_physics$Physics$Body$withDamping,
											{angular: 0.2, linear: 0.5},
											body) : A2($w0rm$elm_physics$Physics$Body$withDamping, $author$project$Main$playerDefaultDamping, body);
									},
									A2(
										$w0rm$elm_physics$Physics$World$keepIf,
										function (body) {
											return !_Utils_eq(
												$w0rm$elm_physics$Physics$Body$data(body),
												$author$project$Main$Mouse);
										},
										newPhysicsWorld))
							})) : $author$project$Reaction$to(
						_Utils_update(
							withCameraInNewRoom,
							{world: newPhysicsWorld})));
				}
			};
		case 'KeyPressed':
			var key = event.a;
			return function (state) {
				return $author$project$Reaction$to(
					_Utils_update(
						state,
						{
							keysPressed: A2($elm$core$List$cons, key, state.keysPressed)
						}));
			};
		case 'KeyReleased':
			var key = event.a;
			return function (state) {
				return $author$project$Reaction$to(
					_Utils_update(
						state,
						{
							keysPressed: A2(
								$elm$core$List$filter,
								function (keyPressed) {
									return !_Utils_eq(keyPressed, key);
								},
								state.keysPressed)
						}));
			};
		case 'MousePressed':
			var mouseRay = event.a;
			return function (model) {
				var _v13 = A2($w0rm$elm_physics$Physics$World$raycast, mouseRay, model.world);
				if (_v13.$ === 'Just') {
					var raycastResult = _v13.a;
					if ($author$project$Main$isDraggable(
						$w0rm$elm_physics$Physics$Body$data(raycastResult.body))) {
						var worldPoint = A2(
							$ianmackenzie$elm_geometry$Point3d$placeIn,
							$w0rm$elm_physics$Physics$Body$frame(raycastResult.body),
							raycastResult.point);
						var mouse = A2(
							$w0rm$elm_physics$Physics$Body$moveTo,
							worldPoint,
							A2($w0rm$elm_physics$Physics$Body$compound, _List_Nil, $author$project$Main$Mouse));
						return $author$project$Reaction$to(
							_Utils_update(
								model,
								{
									maybeRaycastResult: $elm$core$Maybe$Just(raycastResult),
									world: A2(
										$w0rm$elm_physics$Physics$World$constrain,
										F2(
											function (b1, b2) {
												var _v14 = $w0rm$elm_physics$Physics$Body$data(b1);
												if (_v14.$ === 'Mouse') {
													return _Utils_eq(b2, raycastResult.body) ? _List_fromArray(
														[
															A2($w0rm$elm_physics$Physics$Constraint$pointToPoint, $ianmackenzie$elm_geometry$Point3d$origin, raycastResult.point)
														]) : _List_Nil;
												} else {
													return _List_Nil;
												}
											}),
										A2($w0rm$elm_physics$Physics$World$add, mouse, model.world))
								}));
					} else {
						return $author$project$Reaction$to(model);
					}
				} else {
					return $author$project$Reaction$to(model);
				}
			};
		case 'MouseMoved':
			var mouseRay = event.a;
			return function (state) {
				var _v15 = state.maybeRaycastResult;
				if (_v15.$ === 'Just') {
					var raycastResult = _v15.a;
					var worldPoint = A2(
						$ianmackenzie$elm_geometry$Point3d$placeIn,
						$w0rm$elm_physics$Physics$Body$frame(raycastResult.body),
						raycastResult.point);
					var plane = A2(
						$ianmackenzie$elm_geometry$Plane3d$through,
						worldPoint,
						$ianmackenzie$elm_3d_camera$Viewpoint3d$viewDirection(
							$ianmackenzie$elm_3d_camera$Camera3d$viewpoint(state.camera)));
					return $author$project$Reaction$to(
						_Utils_update(
							state,
							{
								world: A2(
									$w0rm$elm_physics$Physics$World$update,
									function (body) {
										if (_Utils_eq(
											$w0rm$elm_physics$Physics$Body$data(body),
											$author$project$Main$Mouse)) {
											var _v16 = A2($ianmackenzie$elm_geometry$Axis3d$intersectionWithPlane, plane, mouseRay);
											if (_v16.$ === 'Just') {
												var intersection = _v16.a;
												return A2($w0rm$elm_physics$Physics$Body$moveTo, intersection, body);
											} else {
												return body;
											}
										} else {
											return body;
										}
									},
									state.world)
							}));
				} else {
					return $author$project$Reaction$to(state);
				}
			};
		default:
			return function (state) {
				return $author$project$Reaction$to(
					_Utils_update(
						state,
						{
							maybeRaycastResult: $elm$core$Maybe$Nothing,
							world: A2(
								$w0rm$elm_physics$Physics$World$keepIf,
								function (body) {
									return !_Utils_eq(
										$w0rm$elm_physics$Physics$Body$data(body),
										$author$project$Main$Mouse);
								},
								state.world)
						}));
			};
	}
};
var $author$project$Main$FrameTickPassed = function (a) {
	return {$: 'FrameTickPassed', a: a};
};
var $author$project$Main$KeyPressed = function (a) {
	return {$: 'KeyPressed', a: a};
};
var $author$project$Main$KeyReleased = function (a) {
	return {$: 'KeyReleased', a: a};
};
var $JohnBugner$elm_keyboard$Key$A = {$: 'A'};
var $JohnBugner$elm_keyboard$Key$Alt = function (a) {
	return {$: 'Alt', a: a};
};
var $JohnBugner$elm_keyboard$Key$ArrowDown = {$: 'ArrowDown'};
var $JohnBugner$elm_keyboard$Key$ArrowLeft = {$: 'ArrowLeft'};
var $JohnBugner$elm_keyboard$Key$ArrowRight = {$: 'ArrowRight'};
var $JohnBugner$elm_keyboard$Key$ArrowUp = {$: 'ArrowUp'};
var $JohnBugner$elm_keyboard$Key$B = {$: 'B'};
var $JohnBugner$elm_keyboard$Key$Backquote = {$: 'Backquote'};
var $JohnBugner$elm_keyboard$Key$Backslash = {$: 'Backslash'};
var $JohnBugner$elm_keyboard$Key$Backspace = {$: 'Backspace'};
var $JohnBugner$elm_keyboard$Key$BracketLeft = {$: 'BracketLeft'};
var $JohnBugner$elm_keyboard$Key$BracketRight = {$: 'BracketRight'};
var $JohnBugner$elm_keyboard$Key$C = {$: 'C'};
var $JohnBugner$elm_keyboard$Key$CapsLock = {$: 'CapsLock'};
var $JohnBugner$elm_keyboard$Key$Comma = {$: 'Comma'};
var $JohnBugner$elm_keyboard$Key$ContextMenu = {$: 'ContextMenu'};
var $JohnBugner$elm_keyboard$Key$Control = function (a) {
	return {$: 'Control', a: a};
};
var $JohnBugner$elm_keyboard$Key$D = {$: 'D'};
var $JohnBugner$elm_keyboard$Key$Delete = {$: 'Delete'};
var $JohnBugner$elm_keyboard$Key$Digit0 = {$: 'Digit0'};
var $JohnBugner$elm_keyboard$Key$Digit1 = {$: 'Digit1'};
var $JohnBugner$elm_keyboard$Key$Digit2 = {$: 'Digit2'};
var $JohnBugner$elm_keyboard$Key$Digit3 = {$: 'Digit3'};
var $JohnBugner$elm_keyboard$Key$Digit4 = {$: 'Digit4'};
var $JohnBugner$elm_keyboard$Key$Digit5 = {$: 'Digit5'};
var $JohnBugner$elm_keyboard$Key$Digit6 = {$: 'Digit6'};
var $JohnBugner$elm_keyboard$Key$Digit7 = {$: 'Digit7'};
var $JohnBugner$elm_keyboard$Key$Digit8 = {$: 'Digit8'};
var $JohnBugner$elm_keyboard$Key$Digit9 = {$: 'Digit9'};
var $JohnBugner$elm_keyboard$Key$E = {$: 'E'};
var $JohnBugner$elm_keyboard$Key$End = {$: 'End'};
var $JohnBugner$elm_keyboard$Key$Enter = {$: 'Enter'};
var $JohnBugner$elm_keyboard$Key$Equal = {$: 'Equal'};
var $JohnBugner$elm_keyboard$Key$Escape = {$: 'Escape'};
var $JohnBugner$elm_keyboard$Key$F = {$: 'F'};
var $JohnBugner$elm_keyboard$Key$F1 = {$: 'F1'};
var $JohnBugner$elm_keyboard$Key$F10 = {$: 'F10'};
var $JohnBugner$elm_keyboard$Key$F11 = {$: 'F11'};
var $JohnBugner$elm_keyboard$Key$F12 = {$: 'F12'};
var $JohnBugner$elm_keyboard$Key$F2 = {$: 'F2'};
var $JohnBugner$elm_keyboard$Key$F3 = {$: 'F3'};
var $JohnBugner$elm_keyboard$Key$F4 = {$: 'F4'};
var $JohnBugner$elm_keyboard$Key$F5 = {$: 'F5'};
var $JohnBugner$elm_keyboard$Key$F6 = {$: 'F6'};
var $JohnBugner$elm_keyboard$Key$F7 = {$: 'F7'};
var $JohnBugner$elm_keyboard$Key$F8 = {$: 'F8'};
var $JohnBugner$elm_keyboard$Key$F9 = {$: 'F9'};
var $JohnBugner$elm_keyboard$Key$G = {$: 'G'};
var $JohnBugner$elm_keyboard$Key$H = {$: 'H'};
var $JohnBugner$elm_keyboard$Key$Home = {$: 'Home'};
var $JohnBugner$elm_keyboard$Key$I = {$: 'I'};
var $JohnBugner$elm_keyboard$Key$Insert = {$: 'Insert'};
var $JohnBugner$elm_keyboard$Key$IntlBackslash = {$: 'IntlBackslash'};
var $JohnBugner$elm_keyboard$Key$J = {$: 'J'};
var $JohnBugner$elm_keyboard$Key$K = {$: 'K'};
var $JohnBugner$elm_keyboard$Key$L = {$: 'L'};
var $JohnBugner$elm_keyboard$Side$Left = {$: 'Left'};
var $JohnBugner$elm_keyboard$Key$M = {$: 'M'};
var $JohnBugner$elm_keyboard$Key$Meta = function (a) {
	return {$: 'Meta', a: a};
};
var $JohnBugner$elm_keyboard$Key$Minus = {$: 'Minus'};
var $JohnBugner$elm_keyboard$Key$N = {$: 'N'};
var $JohnBugner$elm_keyboard$Key$NumLock = {$: 'NumLock'};
var $JohnBugner$elm_keyboard$Key$Numpad0 = {$: 'Numpad0'};
var $JohnBugner$elm_keyboard$Key$Numpad1 = {$: 'Numpad1'};
var $JohnBugner$elm_keyboard$Key$Numpad2 = {$: 'Numpad2'};
var $JohnBugner$elm_keyboard$Key$Numpad3 = {$: 'Numpad3'};
var $JohnBugner$elm_keyboard$Key$Numpad4 = {$: 'Numpad4'};
var $JohnBugner$elm_keyboard$Key$Numpad5 = {$: 'Numpad5'};
var $JohnBugner$elm_keyboard$Key$Numpad6 = {$: 'Numpad6'};
var $JohnBugner$elm_keyboard$Key$Numpad7 = {$: 'Numpad7'};
var $JohnBugner$elm_keyboard$Key$Numpad8 = {$: 'Numpad8'};
var $JohnBugner$elm_keyboard$Key$Numpad9 = {$: 'Numpad9'};
var $JohnBugner$elm_keyboard$Key$NumpadAdd = {$: 'NumpadAdd'};
var $JohnBugner$elm_keyboard$Key$NumpadDecimal = {$: 'NumpadDecimal'};
var $JohnBugner$elm_keyboard$Key$NumpadDivide = {$: 'NumpadDivide'};
var $JohnBugner$elm_keyboard$Key$NumpadEnter = {$: 'NumpadEnter'};
var $JohnBugner$elm_keyboard$Key$NumpadMultiply = {$: 'NumpadMultiply'};
var $JohnBugner$elm_keyboard$Key$NumpadSubtract = {$: 'NumpadSubtract'};
var $JohnBugner$elm_keyboard$Key$O = {$: 'O'};
var $JohnBugner$elm_keyboard$Key$Other = function (a) {
	return {$: 'Other', a: a};
};
var $JohnBugner$elm_keyboard$Key$P = {$: 'P'};
var $JohnBugner$elm_keyboard$Key$PageDown = {$: 'PageDown'};
var $JohnBugner$elm_keyboard$Key$PageUp = {$: 'PageUp'};
var $JohnBugner$elm_keyboard$Key$Pause = {$: 'Pause'};
var $JohnBugner$elm_keyboard$Key$Period = {$: 'Period'};
var $JohnBugner$elm_keyboard$Key$PrintScreen = {$: 'PrintScreen'};
var $JohnBugner$elm_keyboard$Key$Q = {$: 'Q'};
var $JohnBugner$elm_keyboard$Key$Quote = {$: 'Quote'};
var $JohnBugner$elm_keyboard$Key$R = {$: 'R'};
var $JohnBugner$elm_keyboard$Side$Right = {$: 'Right'};
var $JohnBugner$elm_keyboard$Key$S = {$: 'S'};
var $JohnBugner$elm_keyboard$Key$ScrollLock = {$: 'ScrollLock'};
var $JohnBugner$elm_keyboard$Key$Semicolon = {$: 'Semicolon'};
var $JohnBugner$elm_keyboard$Key$Shift = function (a) {
	return {$: 'Shift', a: a};
};
var $JohnBugner$elm_keyboard$Key$Slash = {$: 'Slash'};
var $JohnBugner$elm_keyboard$Key$Space = {$: 'Space'};
var $JohnBugner$elm_keyboard$Key$T = {$: 'T'};
var $JohnBugner$elm_keyboard$Key$Tab = {$: 'Tab'};
var $JohnBugner$elm_keyboard$Key$U = {$: 'U'};
var $JohnBugner$elm_keyboard$Key$V = {$: 'V'};
var $JohnBugner$elm_keyboard$Key$W = {$: 'W'};
var $JohnBugner$elm_keyboard$Key$X = {$: 'X'};
var $JohnBugner$elm_keyboard$Key$Y = {$: 'Y'};
var $JohnBugner$elm_keyboard$Key$Z = {$: 'Z'};
var $JohnBugner$elm_keyboard$Key$fromString = function (s) {
	switch (s) {
		case 'Escape':
			return $JohnBugner$elm_keyboard$Key$Escape;
		case 'F1':
			return $JohnBugner$elm_keyboard$Key$F1;
		case 'F2':
			return $JohnBugner$elm_keyboard$Key$F2;
		case 'F3':
			return $JohnBugner$elm_keyboard$Key$F3;
		case 'F4':
			return $JohnBugner$elm_keyboard$Key$F4;
		case 'F5':
			return $JohnBugner$elm_keyboard$Key$F5;
		case 'F6':
			return $JohnBugner$elm_keyboard$Key$F6;
		case 'F7':
			return $JohnBugner$elm_keyboard$Key$F7;
		case 'F8':
			return $JohnBugner$elm_keyboard$Key$F8;
		case 'F9':
			return $JohnBugner$elm_keyboard$Key$F9;
		case 'F10':
			return $JohnBugner$elm_keyboard$Key$F10;
		case 'F11':
			return $JohnBugner$elm_keyboard$Key$F11;
		case 'F12':
			return $JohnBugner$elm_keyboard$Key$F12;
		case 'Backquote':
			return $JohnBugner$elm_keyboard$Key$Backquote;
		case 'Digit1':
			return $JohnBugner$elm_keyboard$Key$Digit1;
		case 'Digit2':
			return $JohnBugner$elm_keyboard$Key$Digit2;
		case 'Digit3':
			return $JohnBugner$elm_keyboard$Key$Digit3;
		case 'Digit4':
			return $JohnBugner$elm_keyboard$Key$Digit4;
		case 'Digit5':
			return $JohnBugner$elm_keyboard$Key$Digit5;
		case 'Digit6':
			return $JohnBugner$elm_keyboard$Key$Digit6;
		case 'Digit7':
			return $JohnBugner$elm_keyboard$Key$Digit7;
		case 'Digit8':
			return $JohnBugner$elm_keyboard$Key$Digit8;
		case 'Digit9':
			return $JohnBugner$elm_keyboard$Key$Digit9;
		case 'Digit0':
			return $JohnBugner$elm_keyboard$Key$Digit0;
		case 'Minus':
			return $JohnBugner$elm_keyboard$Key$Minus;
		case 'Equal':
			return $JohnBugner$elm_keyboard$Key$Equal;
		case 'Backspace':
			return $JohnBugner$elm_keyboard$Key$Backspace;
		case 'Tab':
			return $JohnBugner$elm_keyboard$Key$Tab;
		case 'KeyQ':
			return $JohnBugner$elm_keyboard$Key$Q;
		case 'KeyW':
			return $JohnBugner$elm_keyboard$Key$W;
		case 'KeyE':
			return $JohnBugner$elm_keyboard$Key$E;
		case 'KeyR':
			return $JohnBugner$elm_keyboard$Key$R;
		case 'KeyT':
			return $JohnBugner$elm_keyboard$Key$T;
		case 'KeyY':
			return $JohnBugner$elm_keyboard$Key$Y;
		case 'KeyU':
			return $JohnBugner$elm_keyboard$Key$U;
		case 'KeyI':
			return $JohnBugner$elm_keyboard$Key$I;
		case 'KeyO':
			return $JohnBugner$elm_keyboard$Key$O;
		case 'KeyP':
			return $JohnBugner$elm_keyboard$Key$P;
		case 'BracketLeft':
			return $JohnBugner$elm_keyboard$Key$BracketLeft;
		case 'BracketRight':
			return $JohnBugner$elm_keyboard$Key$BracketRight;
		case 'Backslash':
			return $JohnBugner$elm_keyboard$Key$Backslash;
		case 'CapsLock':
			return $JohnBugner$elm_keyboard$Key$CapsLock;
		case 'KeyA':
			return $JohnBugner$elm_keyboard$Key$A;
		case 'KeyS':
			return $JohnBugner$elm_keyboard$Key$S;
		case 'KeyD':
			return $JohnBugner$elm_keyboard$Key$D;
		case 'KeyF':
			return $JohnBugner$elm_keyboard$Key$F;
		case 'KeyG':
			return $JohnBugner$elm_keyboard$Key$G;
		case 'KeyH':
			return $JohnBugner$elm_keyboard$Key$H;
		case 'KeyJ':
			return $JohnBugner$elm_keyboard$Key$J;
		case 'KeyK':
			return $JohnBugner$elm_keyboard$Key$K;
		case 'KeyL':
			return $JohnBugner$elm_keyboard$Key$L;
		case 'Semicolon':
			return $JohnBugner$elm_keyboard$Key$Semicolon;
		case 'Quote':
			return $JohnBugner$elm_keyboard$Key$Quote;
		case 'Enter':
			return $JohnBugner$elm_keyboard$Key$Enter;
		case 'ShiftLeft':
			return $JohnBugner$elm_keyboard$Key$Shift($JohnBugner$elm_keyboard$Side$Left);
		case 'IntlBackslash':
			return $JohnBugner$elm_keyboard$Key$IntlBackslash;
		case 'KeyZ':
			return $JohnBugner$elm_keyboard$Key$Z;
		case 'KeyX':
			return $JohnBugner$elm_keyboard$Key$X;
		case 'KeyC':
			return $JohnBugner$elm_keyboard$Key$C;
		case 'KeyV':
			return $JohnBugner$elm_keyboard$Key$V;
		case 'KeyB':
			return $JohnBugner$elm_keyboard$Key$B;
		case 'KeyN':
			return $JohnBugner$elm_keyboard$Key$N;
		case 'KeyM':
			return $JohnBugner$elm_keyboard$Key$M;
		case 'Comma':
			return $JohnBugner$elm_keyboard$Key$Comma;
		case 'Period':
			return $JohnBugner$elm_keyboard$Key$Period;
		case 'Slash':
			return $JohnBugner$elm_keyboard$Key$Slash;
		case 'ShiftRight':
			return $JohnBugner$elm_keyboard$Key$Shift($JohnBugner$elm_keyboard$Side$Right);
		case 'ControlLeft':
			return $JohnBugner$elm_keyboard$Key$Control($JohnBugner$elm_keyboard$Side$Left);
		case 'MetaLeft':
			return $JohnBugner$elm_keyboard$Key$Meta($JohnBugner$elm_keyboard$Side$Left);
		case 'AltLeft':
			return $JohnBugner$elm_keyboard$Key$Alt($JohnBugner$elm_keyboard$Side$Left);
		case 'Space':
			return $JohnBugner$elm_keyboard$Key$Space;
		case 'AltRight':
			return $JohnBugner$elm_keyboard$Key$Alt($JohnBugner$elm_keyboard$Side$Right);
		case 'MetaRight':
			return $JohnBugner$elm_keyboard$Key$Meta($JohnBugner$elm_keyboard$Side$Right);
		case 'ContextMenu':
			return $JohnBugner$elm_keyboard$Key$ContextMenu;
		case 'ControlRight':
			return $JohnBugner$elm_keyboard$Key$Control($JohnBugner$elm_keyboard$Side$Right);
		case 'PrintScreen':
			return $JohnBugner$elm_keyboard$Key$PrintScreen;
		case 'ScrollLock':
			return $JohnBugner$elm_keyboard$Key$ScrollLock;
		case 'Pause':
			return $JohnBugner$elm_keyboard$Key$Pause;
		case 'Insert':
			return $JohnBugner$elm_keyboard$Key$Insert;
		case 'Home':
			return $JohnBugner$elm_keyboard$Key$Home;
		case 'PageUp':
			return $JohnBugner$elm_keyboard$Key$PageUp;
		case 'Delete':
			return $JohnBugner$elm_keyboard$Key$Delete;
		case 'End':
			return $JohnBugner$elm_keyboard$Key$End;
		case 'PageDown':
			return $JohnBugner$elm_keyboard$Key$PageDown;
		case 'ArrowUp':
			return $JohnBugner$elm_keyboard$Key$ArrowUp;
		case 'ArrowLeft':
			return $JohnBugner$elm_keyboard$Key$ArrowLeft;
		case 'ArrowDown':
			return $JohnBugner$elm_keyboard$Key$ArrowDown;
		case 'ArrowRight':
			return $JohnBugner$elm_keyboard$Key$ArrowRight;
		case 'NumLock':
			return $JohnBugner$elm_keyboard$Key$NumLock;
		case 'NumpadDivide':
			return $JohnBugner$elm_keyboard$Key$NumpadDivide;
		case 'NumpadMultiply':
			return $JohnBugner$elm_keyboard$Key$NumpadMultiply;
		case 'NumpadSubtract':
			return $JohnBugner$elm_keyboard$Key$NumpadSubtract;
		case 'Numpad7':
			return $JohnBugner$elm_keyboard$Key$Numpad7;
		case 'Numpad8':
			return $JohnBugner$elm_keyboard$Key$Numpad8;
		case 'Numpad9':
			return $JohnBugner$elm_keyboard$Key$Numpad9;
		case 'NumpadAdd':
			return $JohnBugner$elm_keyboard$Key$NumpadAdd;
		case 'Numpad4':
			return $JohnBugner$elm_keyboard$Key$Numpad4;
		case 'Numpad5':
			return $JohnBugner$elm_keyboard$Key$Numpad5;
		case 'Numpad6':
			return $JohnBugner$elm_keyboard$Key$Numpad6;
		case 'Numpad1':
			return $JohnBugner$elm_keyboard$Key$Numpad1;
		case 'Numpad2':
			return $JohnBugner$elm_keyboard$Key$Numpad2;
		case 'Numpad3':
			return $JohnBugner$elm_keyboard$Key$Numpad3;
		case 'NumpadEnter':
			return $JohnBugner$elm_keyboard$Key$NumpadEnter;
		case 'Numpad0':
			return $JohnBugner$elm_keyboard$Key$Numpad0;
		case 'NumpadDecimal':
			return $JohnBugner$elm_keyboard$Key$NumpadDecimal;
		default:
			return $JohnBugner$elm_keyboard$Key$Other(s);
	}
};
var $JohnBugner$elm_keyboard$Key$decoder = A2(
	$elm$json$Json$Decode$map,
	$JohnBugner$elm_keyboard$Key$fromString,
	A2($elm$json$Json$Decode$field, 'code', $elm$json$Json$Decode$string));
var $elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 'Time', a: a};
};
var $elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {oldTime: oldTime, request: request, subs: subs};
	});
var $elm$browser$Browser$AnimationManager$init = $elm$core$Task$succeed(
	A3($elm$browser$Browser$AnimationManager$State, _List_Nil, $elm$core$Maybe$Nothing, 0));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$browser$Browser$AnimationManager$now = _Browser_now(_Utils_Tuple0);
var $elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(_Utils_Tuple0);
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _v0) {
		var request = _v0.request;
		var oldTime = _v0.oldTime;
		var _v1 = _Utils_Tuple2(request, subs);
		if (_v1.a.$ === 'Nothing') {
			if (!_v1.b.b) {
				var _v2 = _v1.a;
				return $elm$browser$Browser$AnimationManager$init;
			} else {
				var _v4 = _v1.a;
				return A2(
					$elm$core$Task$andThen,
					function (pid) {
						return A2(
							$elm$core$Task$andThen,
							function (time) {
								return $elm$core$Task$succeed(
									A3(
										$elm$browser$Browser$AnimationManager$State,
										subs,
										$elm$core$Maybe$Just(pid),
										time));
							},
							$elm$browser$Browser$AnimationManager$now);
					},
					$elm$core$Process$spawn(
						A2(
							$elm$core$Task$andThen,
							$elm$core$Platform$sendToSelf(router),
							$elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_v1.b.b) {
				var pid = _v1.a.a;
				return A2(
					$elm$core$Task$andThen,
					function (_v3) {
						return $elm$browser$Browser$AnimationManager$init;
					},
					$elm$core$Process$kill(pid));
			} else {
				return $elm$core$Task$succeed(
					A3($elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var $elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _v0) {
		var subs = _v0.subs;
		var oldTime = _v0.oldTime;
		var send = function (sub) {
			if (sub.$ === 'Time') {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(
						$elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			$elm$core$Task$andThen,
			function (pid) {
				return A2(
					$elm$core$Task$andThen,
					function (_v1) {
						return $elm$core$Task$succeed(
							A3(
								$elm$browser$Browser$AnimationManager$State,
								subs,
								$elm$core$Maybe$Just(pid),
								newTime));
					},
					$elm$core$Task$sequence(
						A2($elm$core$List$map, send, subs)));
			},
			$elm$core$Process$spawn(
				A2(
					$elm$core$Task$andThen,
					$elm$core$Platform$sendToSelf(router),
					$elm$browser$Browser$AnimationManager$rAF)));
	});
var $elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 'Delta', a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (sub.$ === 'Time') {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Time(
				A2($elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Delta(
				A2($elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager($elm$browser$Browser$AnimationManager$init, $elm$browser$Browser$AnimationManager$onEffects, $elm$browser$Browser$AnimationManager$onSelfMsg, 0, $elm$browser$Browser$AnimationManager$subMap);
var $elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var $elm$browser$Browser$AnimationManager$onAnimationFrame = function (tagger) {
	return $elm$browser$Browser$AnimationManager$subscription(
		$elm$browser$Browser$AnimationManager$Time(tagger));
};
var $elm$browser$Browser$Events$onAnimationFrame = $elm$browser$Browser$AnimationManager$onAnimationFrame;
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
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
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keydown');
var $elm$browser$Browser$Events$onKeyUp = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keyup');
var $elm$browser$Browser$Events$Window = {$: 'Window'};
var $elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		$elm$browser$Browser$Events$on,
		$elm$browser$Browser$Events$Window,
		'resize',
		A2(
			$elm$json$Json$Decode$field,
			'target',
			A3(
				$elm$json$Json$Decode$map2,
				func,
				A2($elm$json$Json$Decode$field, 'innerWidth', $elm$json$Json$Decode$int),
				A2($elm$json$Json$Decode$field, 'innerHeight', $elm$json$Json$Decode$int))));
};
var $author$project$Main$subscriptions = function (state) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$elm$browser$Browser$Events$onResize(
				F2(
					function (width, height) {
						return $author$project$Main$GameWindowSized(
							{height: height, width: width});
					})),
				$elm$browser$Browser$Events$onAnimationFrame($author$project$Main$FrameTickPassed),
				$elm$browser$Browser$Events$onKeyDown(
				A2($elm$json$Json$Decode$map, $author$project$Main$KeyPressed, $JohnBugner$elm_keyboard$Key$decoder)),
				$elm$browser$Browser$Events$onKeyUp(
				A2($elm$json$Json$Decode$map, $author$project$Main$KeyReleased, $JohnBugner$elm_keyboard$Key$decoder))
			]));
};
var $MartinSStewart$elm_audio$Audio$AudioCmdGroup = function (a) {
	return {$: 'AudioCmdGroup', a: a};
};
var $MartinSStewart$elm_audio$Audio$cmdBatch = function (audioCmds) {
	return $MartinSStewart$elm_audio$Audio$AudioCmdGroup(audioCmds);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $author$project$Reaction$toTuple3 = function (interpretEffect) {
	return function (step) {
		var commandsList = A2(
			$elm$core$List$map,
			interpretEffect,
			$author$project$Reaction$effects(step));
		return _Utils_Tuple3(
			$author$project$Reaction$state(step),
			$elm$core$Platform$Cmd$batch(
				A2(
					$elm$core$List$concatMap,
					function ($) {
						return $.commands;
					},
					commandsList)),
			$MartinSStewart$elm_audio$Audio$cmdBatch(
				A2(
					$elm$core$List$concatMap,
					function ($) {
						return $.audioCommands;
					},
					commandsList)));
	};
};
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $author$project$Main$MouseMoved = function (a) {
	return {$: 'MouseMoved', a: a};
};
var $author$project$Main$MousePressed = function (a) {
	return {$: 'MousePressed', a: a};
};
var $author$project$Main$MouseReleased = {$: 'MouseReleased'};
var $ianmackenzie$elm_3d_scene$Scene3d$BackgroundColor = function (a) {
	return {$: 'BackgroundColor', a: a};
};
var $ianmackenzie$elm_3d_scene$Scene3d$backgroundColor = function (color) {
	return $ianmackenzie$elm_3d_scene$Scene3d$BackgroundColor(color);
};
var $ianmackenzie$elm_3d_scene$Scene3d$Types$CullBackFaces = {$: 'CullBackFaces'};
var $ianmackenzie$elm_3d_scene$Scene3d$Types$Facets = F4(
	function (a, b, c, d) {
		return {$: 'Facets', a: a, b: b, c: c, d: d};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Types$Indexed = F4(
	function (a, b, c, d) {
		return {$: 'Indexed', a: a, b: b, c: c, d: d};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Types$MeshWithNormals = F4(
	function (a, b, c, d) {
		return {$: 'MeshWithNormals', a: a, b: b, c: c, d: d};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Types$MeshWithNormalsAndUvs = F4(
	function (a, b, c, d) {
		return {$: 'MeshWithNormalsAndUvs', a: a, b: b, c: c, d: d};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Types$MeshWithTangents = F4(
	function (a, b, c, d) {
		return {$: 'MeshWithTangents', a: a, b: b, c: c, d: d};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Types$MeshWithUvs = F4(
	function (a, b, c, d) {
		return {$: 'MeshWithUvs', a: a, b: b, c: c, d: d};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Types$Triangles = F4(
	function (a, b, c, d) {
		return {$: 'Triangles', a: a, b: b, c: c, d: d};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$cullBackFaces = function (mesh) {
	switch (mesh.$) {
		case 'EmptyMesh':
			return mesh;
		case 'Triangles':
			var boundingBox = mesh.a;
			var meshTriangles = mesh.b;
			var webGLMesh = mesh.c;
			return A4($ianmackenzie$elm_3d_scene$Scene3d$Types$Triangles, boundingBox, meshTriangles, webGLMesh, $ianmackenzie$elm_3d_scene$Scene3d$Types$CullBackFaces);
		case 'Facets':
			var boundingBox = mesh.a;
			var meshTriangles = mesh.b;
			var webGLMesh = mesh.c;
			return A4($ianmackenzie$elm_3d_scene$Scene3d$Types$Facets, boundingBox, meshTriangles, webGLMesh, $ianmackenzie$elm_3d_scene$Scene3d$Types$CullBackFaces);
		case 'Indexed':
			var boundingBox = mesh.a;
			var triangularMesh = mesh.b;
			var webGLMesh = mesh.c;
			return A4($ianmackenzie$elm_3d_scene$Scene3d$Types$Indexed, boundingBox, triangularMesh, webGLMesh, $ianmackenzie$elm_3d_scene$Scene3d$Types$CullBackFaces);
		case 'MeshWithNormals':
			var boundingBox = mesh.a;
			var triangularMesh = mesh.b;
			var webGLMesh = mesh.c;
			return A4($ianmackenzie$elm_3d_scene$Scene3d$Types$MeshWithNormals, boundingBox, triangularMesh, webGLMesh, $ianmackenzie$elm_3d_scene$Scene3d$Types$CullBackFaces);
		case 'MeshWithUvs':
			var boundingBox = mesh.a;
			var triangularMesh = mesh.b;
			var webGLMesh = mesh.c;
			return A4($ianmackenzie$elm_3d_scene$Scene3d$Types$MeshWithUvs, boundingBox, triangularMesh, webGLMesh, $ianmackenzie$elm_3d_scene$Scene3d$Types$CullBackFaces);
		case 'MeshWithNormalsAndUvs':
			var boundingBox = mesh.a;
			var triangularMesh = mesh.b;
			var webGLMesh = mesh.c;
			return A4($ianmackenzie$elm_3d_scene$Scene3d$Types$MeshWithNormalsAndUvs, boundingBox, triangularMesh, webGLMesh, $ianmackenzie$elm_3d_scene$Scene3d$Types$CullBackFaces);
		case 'MeshWithTangents':
			var boundingBox = mesh.a;
			var triangularMesh = mesh.b;
			var webGLMesh = mesh.c;
			return A4($ianmackenzie$elm_3d_scene$Scene3d$Types$MeshWithTangents, boundingBox, triangularMesh, webGLMesh, $ianmackenzie$elm_3d_scene$Scene3d$Types$CullBackFaces);
		case 'LineSegments':
			return mesh;
		case 'Polyline':
			return mesh;
		default:
			return mesh;
	}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Types$EmptyMesh = {$: 'EmptyMesh'};
var $ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces = {$: 'KeepBackFaces'};
var $ianmackenzie$elm_geometry$Geometry$Types$BoundingBox3d = function (a) {
	return {$: 'BoundingBox3d', a: a};
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $ianmackenzie$elm_geometry$BoundingBox3d$aggregateOfHelp = F8(
	function (currentMinX, currentMaxX, currentMinY, currentMaxY, currentMinZ, currentMaxZ, getBoundingBox, items) {
		aggregateOfHelp:
		while (true) {
			if (items.b) {
				var next = items.a;
				var rest = items.b;
				var _v1 = getBoundingBox(next);
				var b = _v1.a;
				var $temp$currentMinX = A2($elm$core$Basics$min, b.minX, currentMinX),
					$temp$currentMaxX = A2($elm$core$Basics$max, b.maxX, currentMaxX),
					$temp$currentMinY = A2($elm$core$Basics$min, b.minY, currentMinY),
					$temp$currentMaxY = A2($elm$core$Basics$max, b.maxY, currentMaxY),
					$temp$currentMinZ = A2($elm$core$Basics$min, b.minZ, currentMinZ),
					$temp$currentMaxZ = A2($elm$core$Basics$max, b.maxZ, currentMaxZ),
					$temp$getBoundingBox = getBoundingBox,
					$temp$items = rest;
				currentMinX = $temp$currentMinX;
				currentMaxX = $temp$currentMaxX;
				currentMinY = $temp$currentMinY;
				currentMaxY = $temp$currentMaxY;
				currentMinZ = $temp$currentMinZ;
				currentMaxZ = $temp$currentMaxZ;
				getBoundingBox = $temp$getBoundingBox;
				items = $temp$items;
				continue aggregateOfHelp;
			} else {
				return $ianmackenzie$elm_geometry$Geometry$Types$BoundingBox3d(
					{maxX: currentMaxX, maxY: currentMaxY, maxZ: currentMaxZ, minX: currentMinX, minY: currentMinY, minZ: currentMinZ});
			}
		}
	});
var $ianmackenzie$elm_geometry$BoundingBox3d$aggregateOf = F3(
	function (getBoundingBox, first, rest) {
		var _v0 = getBoundingBox(first);
		var b1 = _v0.a;
		return A8($ianmackenzie$elm_geometry$BoundingBox3d$aggregateOfHelp, b1.minX, b1.maxX, b1.minY, b1.maxY, b1.minZ, b1.maxZ, getBoundingBox, rest);
	});
var $ianmackenzie$elm_geometry$BoundingBox3d$fromExtrema = function (given) {
	var _v0 = given.maxZ;
	var z2 = _v0.a;
	var _v1 = given.minZ;
	var z1 = _v1.a;
	var _v2 = given.maxY;
	var y2 = _v2.a;
	var _v3 = given.minY;
	var y1 = _v3.a;
	var _v4 = given.maxX;
	var x2 = _v4.a;
	var _v5 = given.minX;
	var x1 = _v5.a;
	return $ianmackenzie$elm_geometry$Geometry$Types$BoundingBox3d(
		{
			maxX: A2($elm$core$Basics$max, x1, x2),
			maxY: A2($elm$core$Basics$max, y1, y2),
			maxZ: A2($elm$core$Basics$max, z1, z2),
			minX: A2($elm$core$Basics$min, x1, x2),
			minY: A2($elm$core$Basics$min, y1, y2),
			minZ: A2($elm$core$Basics$min, z1, z2)
		});
};
var $ianmackenzie$elm_units$Quantity$max = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(
			A2($elm$core$Basics$max, x, y));
	});
var $ianmackenzie$elm_units$Quantity$min = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(
			A2($elm$core$Basics$min, x, y));
	});
var $ianmackenzie$elm_geometry$Triangle3d$vertices = function (_v0) {
	var triangleVertices = _v0.a;
	return triangleVertices;
};
var $ianmackenzie$elm_geometry$Triangle3d$boundingBox = function (triangle) {
	var _v0 = $ianmackenzie$elm_geometry$Triangle3d$vertices(triangle);
	var p1 = _v0.a;
	var p2 = _v0.b;
	var p3 = _v0.c;
	var x1 = $ianmackenzie$elm_geometry$Point3d$xCoordinate(p1);
	var y1 = $ianmackenzie$elm_geometry$Point3d$yCoordinate(p1);
	var z1 = $ianmackenzie$elm_geometry$Point3d$zCoordinate(p1);
	var x2 = $ianmackenzie$elm_geometry$Point3d$xCoordinate(p2);
	var y2 = $ianmackenzie$elm_geometry$Point3d$yCoordinate(p2);
	var z2 = $ianmackenzie$elm_geometry$Point3d$zCoordinate(p2);
	var x3 = $ianmackenzie$elm_geometry$Point3d$xCoordinate(p3);
	var y3 = $ianmackenzie$elm_geometry$Point3d$yCoordinate(p3);
	var z3 = $ianmackenzie$elm_geometry$Point3d$zCoordinate(p3);
	return $ianmackenzie$elm_geometry$BoundingBox3d$fromExtrema(
		{
			maxX: A2(
				$ianmackenzie$elm_units$Quantity$max,
				x1,
				A2($ianmackenzie$elm_units$Quantity$max, x2, x3)),
			maxY: A2(
				$ianmackenzie$elm_units$Quantity$max,
				y1,
				A2($ianmackenzie$elm_units$Quantity$max, y2, y3)),
			maxZ: A2(
				$ianmackenzie$elm_units$Quantity$max,
				z1,
				A2($ianmackenzie$elm_units$Quantity$max, z2, z3)),
			minX: A2(
				$ianmackenzie$elm_units$Quantity$min,
				x1,
				A2($ianmackenzie$elm_units$Quantity$min, x2, x3)),
			minY: A2(
				$ianmackenzie$elm_units$Quantity$min,
				y1,
				A2($ianmackenzie$elm_units$Quantity$min, y2, y3)),
			minZ: A2(
				$ianmackenzie$elm_units$Quantity$min,
				z1,
				A2($ianmackenzie$elm_units$Quantity$min, z2, z3))
		});
};
var $elm_explorations$linear_algebra$Math$Vector3$fromRecord = _MJS_v3fromRecord;
var $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Point3d$toVec3 = function (point) {
	return $elm_explorations$linear_algebra$Math$Vector3$fromRecord(
		$ianmackenzie$elm_geometry$Point3d$unwrap(point));
};
var $ianmackenzie$elm_geometry$Vector3d$unwrap = function (_v0) {
	var givenComponents = _v0.a;
	return givenComponents;
};
var $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Vector3d$toVec3 = function (vector) {
	return $elm_explorations$linear_algebra$Math$Vector3$fromRecord(
		$ianmackenzie$elm_geometry$Vector3d$unwrap(vector));
};
var $ianmackenzie$elm_units$Quantity$float = function (value) {
	return $ianmackenzie$elm_units$Quantity$Quantity(value);
};
var $ianmackenzie$elm_geometry$Vector3d$scaleTo = F2(
	function (_v0, _v1) {
		var q = _v0.a;
		var v = _v1.a;
		var largestComponent = A2(
			$elm$core$Basics$max,
			$elm$core$Basics$abs(v.x),
			A2(
				$elm$core$Basics$max,
				$elm$core$Basics$abs(v.y),
				$elm$core$Basics$abs(v.z)));
		if (!largestComponent) {
			return $ianmackenzie$elm_geometry$Vector3d$zero;
		} else {
			var scaledZ = v.z / largestComponent;
			var scaledY = v.y / largestComponent;
			var scaledX = v.x / largestComponent;
			var scaledLength = $elm$core$Basics$sqrt(((scaledX * scaledX) + (scaledY * scaledY)) + (scaledZ * scaledZ));
			return $ianmackenzie$elm_geometry$Geometry$Types$Vector3d(
				{x: (q * scaledX) / scaledLength, y: (q * scaledY) / scaledLength, z: (q * scaledZ) / scaledLength});
		}
	});
var $ianmackenzie$elm_geometry$Vector3d$normalize = $ianmackenzie$elm_geometry$Vector3d$scaleTo(
	$ianmackenzie$elm_units$Quantity$float(1));
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$triangleNormal = F3(
	function (p1, p2, p3) {
		var v2 = A2($ianmackenzie$elm_geometry$Vector3d$from, p2, p3);
		var v1 = A2($ianmackenzie$elm_geometry$Vector3d$from, p1, p2);
		return $ianmackenzie$elm_geometry$Vector3d$normalize(
			A2($ianmackenzie$elm_geometry$Vector3d$cross, v2, v1));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$facetAttributes = function (triangle) {
	var _v0 = $ianmackenzie$elm_geometry$Triangle3d$vertices(triangle);
	var p1 = _v0.a;
	var p2 = _v0.b;
	var p3 = _v0.c;
	var normal = $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Vector3d$toVec3(
		A3($ianmackenzie$elm_3d_scene$Scene3d$Mesh$triangleNormal, p1, p2, p3));
	return _Utils_Tuple3(
		{
			normal: normal,
			position: $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Point3d$toVec3(p1)
		},
		{
			normal: normal,
			position: $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Point3d$toVec3(p2)
		},
		{
			normal: normal,
			position: $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Point3d$toVec3(p3)
		});
};
var $elm_explorations$webgl$WebGL$Mesh3 = F2(
	function (a, b) {
		return {$: 'Mesh3', a: a, b: b};
	});
var $elm_explorations$webgl$WebGL$triangles = $elm_explorations$webgl$WebGL$Mesh3(
	{elemSize: 3, indexSize: 0, mode: 4});
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$facets = function (givenTriangles) {
	if (!givenTriangles.b) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$EmptyMesh;
	} else {
		var first = givenTriangles.a;
		var rest = givenTriangles.b;
		var webGLMesh = $elm_explorations$webgl$WebGL$triangles(
			A2($elm$core$List$map, $ianmackenzie$elm_3d_scene$Scene3d$Mesh$facetAttributes, givenTriangles));
		var bounds = A3($ianmackenzie$elm_geometry$BoundingBox3d$aggregateOf, $ianmackenzie$elm_geometry$Triangle3d$boundingBox, first, rest);
		return A4($ianmackenzie$elm_3d_scene$Scene3d$Types$Facets, bounds, givenTriangles, webGLMesh, $ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces);
	}
};
var $ianmackenzie$elm_geometry$Geometry$Types$Triangle3d = function (a) {
	return {$: 'Triangle3d', a: a};
};
var $ianmackenzie$elm_geometry$Triangle3d$from = F3(
	function (p1, p2, p3) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Triangle3d(
			_Utils_Tuple3(p1, p2, p3));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Primitives$block = function () {
	var z = $ianmackenzie$elm_units$Length$meters(1);
	var y = $ianmackenzie$elm_units$Length$meters(1);
	var x = $ianmackenzie$elm_units$Length$meters(1);
	var minZ = A2($ianmackenzie$elm_units$Quantity$multiplyBy, -0.5, z);
	var minY = A2($ianmackenzie$elm_units$Quantity$multiplyBy, -0.5, y);
	var minX = A2($ianmackenzie$elm_units$Quantity$multiplyBy, -0.5, x);
	var p0 = A3($ianmackenzie$elm_geometry$Point3d$xyz, minX, minY, minZ);
	var maxZ = A2($ianmackenzie$elm_units$Quantity$multiplyBy, 0.5, z);
	var p4 = A3($ianmackenzie$elm_geometry$Point3d$xyz, minX, minY, maxZ);
	var maxY = A2($ianmackenzie$elm_units$Quantity$multiplyBy, 0.5, y);
	var p3 = A3($ianmackenzie$elm_geometry$Point3d$xyz, minX, maxY, minZ);
	var p7 = A3($ianmackenzie$elm_geometry$Point3d$xyz, minX, maxY, maxZ);
	var maxX = A2($ianmackenzie$elm_units$Quantity$multiplyBy, 0.5, x);
	var p1 = A3($ianmackenzie$elm_geometry$Point3d$xyz, maxX, minY, minZ);
	var p2 = A3($ianmackenzie$elm_geometry$Point3d$xyz, maxX, maxY, minZ);
	var p5 = A3($ianmackenzie$elm_geometry$Point3d$xyz, maxX, minY, maxZ);
	var p6 = A3($ianmackenzie$elm_geometry$Point3d$xyz, maxX, maxY, maxZ);
	return $ianmackenzie$elm_3d_scene$Scene3d$Mesh$cullBackFaces(
		$ianmackenzie$elm_3d_scene$Scene3d$Mesh$facets(
			_List_fromArray(
				[
					A3($ianmackenzie$elm_geometry$Triangle3d$from, p0, p2, p1),
					A3($ianmackenzie$elm_geometry$Triangle3d$from, p0, p3, p2),
					A3($ianmackenzie$elm_geometry$Triangle3d$from, p4, p5, p6),
					A3($ianmackenzie$elm_geometry$Triangle3d$from, p4, p6, p7),
					A3($ianmackenzie$elm_geometry$Triangle3d$from, p1, p2, p6),
					A3($ianmackenzie$elm_geometry$Triangle3d$from, p1, p6, p5),
					A3($ianmackenzie$elm_geometry$Triangle3d$from, p0, p7, p3),
					A3($ianmackenzie$elm_geometry$Triangle3d$from, p0, p4, p7),
					A3($ianmackenzie$elm_geometry$Triangle3d$from, p0, p1, p5),
					A3($ianmackenzie$elm_geometry$Triangle3d$from, p0, p5, p4),
					A3($ianmackenzie$elm_geometry$Triangle3d$from, p3, p6, p2),
					A3($ianmackenzie$elm_geometry$Triangle3d$from, p3, p7, p6)
				])));
}();
var $ianmackenzie$elm_3d_scene$Scene3d$Types$EmptyShadow = {$: 'EmptyShadow'};
var $ianmackenzie$elm_3d_scene$Scene3d$Types$Shadow = F3(
	function (a, b, c) {
		return {$: 'Shadow', a: a, b: b, c: c};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$collectShadowVertices = F3(
	function (getPosition, _v0, accumulated) {
		var mv1 = _v0.a;
		var mv2 = _v0.b;
		var mv3 = _v0.c;
		var p3 = getPosition(mv3);
		var p2 = getPosition(mv2);
		var p1 = getPosition(mv1);
		var faceNormal = $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Vector3d$toVec3(
			A3($ianmackenzie$elm_3d_scene$Scene3d$Mesh$triangleNormal, p1, p2, p3));
		var sv1 = {
			normal: faceNormal,
			position: $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Point3d$toVec3(p1)
		};
		var sv2 = {
			normal: faceNormal,
			position: $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Point3d$toVec3(p2)
		};
		var sv3 = {
			normal: faceNormal,
			position: $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Point3d$toVec3(p3)
		};
		return A2(
			$elm$core$List$cons,
			sv1,
			A2(
				$elm$core$List$cons,
				sv2,
				A2($elm$core$List$cons, sv3, accumulated)));
	});
var $ianmackenzie$elm_triangular_mesh$TriangularMesh$faceIndices = function (_v0) {
	var mesh = _v0.a;
	return mesh.faceIndices;
};
var $elm$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				if (mc.$ === 'Nothing') {
					return $elm$core$Maybe$Nothing;
				} else {
					var c = mc.a;
					return $elm$core$Maybe$Just(
						A3(func, a, b, c));
				}
			}
		}
	});
var $ianmackenzie$elm_triangular_mesh$TriangularMesh$vertices = function (_v0) {
	var mesh = _v0.a;
	return mesh.vertices;
};
var $ianmackenzie$elm_triangular_mesh$TriangularMesh$vertex = F2(
	function (index, mesh) {
		return A2(
			$elm$core$Array$get,
			index,
			$ianmackenzie$elm_triangular_mesh$TriangularMesh$vertices(mesh));
	});
var $ianmackenzie$elm_triangular_mesh$TriangularMesh$faceVertices = function (mesh) {
	var toFace = function (_v0) {
		var i = _v0.a;
		var j = _v0.b;
		var k = _v0.c;
		return A4(
			$elm$core$Maybe$map3,
			F3(
				function (firstVertex, secondVertex, thirdVertex) {
					return _Utils_Tuple3(firstVertex, secondVertex, thirdVertex);
				}),
			A2($ianmackenzie$elm_triangular_mesh$TriangularMesh$vertex, i, mesh),
			A2($ianmackenzie$elm_triangular_mesh$TriangularMesh$vertex, j, mesh),
			A2($ianmackenzie$elm_triangular_mesh$TriangularMesh$vertex, k, mesh));
	};
	return A2(
		$elm$core$List$filterMap,
		toFace,
		$ianmackenzie$elm_triangular_mesh$TriangularMesh$faceIndices(mesh));
};
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $ianmackenzie$elm_triangular_mesh$TriangularMesh$TriangularMesh = function (a) {
	return {$: 'TriangularMesh', a: a};
};
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $ianmackenzie$elm_triangular_mesh$TriangularMesh$indexed = F2(
	function (vertices_, faceIndices_) {
		var numVertices = $elm$core$Array$length(vertices_);
		var validIndices = function (_v0) {
			var i = _v0.a;
			var j = _v0.b;
			var k = _v0.c;
			return ((i >= 0) && (_Utils_cmp(i, numVertices) < 0)) && (((j >= 0) && (_Utils_cmp(j, numVertices) < 0)) && ((k >= 0) && (_Utils_cmp(k, numVertices) < 0)));
		};
		return A2($elm$core$List$all, validIndices, faceIndices_) ? $ianmackenzie$elm_triangular_mesh$TriangularMesh$TriangularMesh(
			{faceIndices: faceIndices_, vertices: vertices_}) : $ianmackenzie$elm_triangular_mesh$TriangularMesh$TriangularMesh(
			{
				faceIndices: A2($elm$core$List$filter, validIndices, faceIndices_),
				vertices: vertices_
			});
	});
var $elm_explorations$webgl$WebGL$MeshIndexed3 = F3(
	function (a, b, c) {
		return {$: 'MeshIndexed3', a: a, b: b, c: c};
	});
var $elm_explorations$webgl$WebGL$indexedTriangles = $elm_explorations$webgl$WebGL$MeshIndexed3(
	{elemSize: 1, indexSize: 3, mode: 4});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$edgeKey = F2(
	function (firstPoint, secondPoint) {
		var p2 = $ianmackenzie$elm_geometry$Point3d$toMeters(secondPoint);
		var p1 = $ianmackenzie$elm_geometry$Point3d$toMeters(firstPoint);
		return _Utils_Tuple2(
			_Utils_Tuple3(p1.x, p1.y, p1.z),
			_Utils_Tuple3(p2.x, p2.y, p2.z));
	});
var $elm_explorations$linear_algebra$Math$Vector3$vec3 = _MJS_v3;
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$zeroVec3 = A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0);
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$joinEdge = F6(
	function (p1, p2, start, end, neighborDict, _v0) {
		var shadowFaceIndices = _v0.a;
		var extraShadowVertices = _v0.b;
		var nextShadowVertexIndex = _v0.c;
		var _v1 = A2(
			$elm$core$Dict$get,
			A2($ianmackenzie$elm_3d_scene$Scene3d$Mesh$edgeKey, p1, p2),
			neighborDict);
		if (_v1.$ === 'Just') {
			var opposite = _v1.a;
			return _Utils_Tuple3(
				A2(
					$elm$core$List$cons,
					_Utils_Tuple3(start, opposite, end),
					shadowFaceIndices),
				extraShadowVertices,
				nextShadowVertexIndex);
		} else {
			var v2 = {
				normal: $ianmackenzie$elm_3d_scene$Scene3d$Mesh$zeroVec3,
				position: $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Point3d$toVec3(p2)
			};
			var v1 = {
				normal: $ianmackenzie$elm_3d_scene$Scene3d$Mesh$zeroVec3,
				position: $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Point3d$toVec3(p1)
			};
			var b = nextShadowVertexIndex + 1;
			var a = nextShadowVertexIndex;
			return _Utils_Tuple3(
				A2(
					$elm$core$List$cons,
					_Utils_Tuple3(start, a, b),
					A2(
						$elm$core$List$cons,
						_Utils_Tuple3(start, b, end),
						shadowFaceIndices)),
				A2(
					$elm$core$List$cons,
					v2,
					A2($elm$core$List$cons, v1, extraShadowVertices)),
				nextShadowVertexIndex + 2);
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$joinEdges = F5(
	function (getPosition, neighborDict, meshFaceVertices, nextShadowVertexIndex, state) {
		joinEdges:
		while (true) {
			if (meshFaceVertices.b) {
				var _v1 = meshFaceVertices.a;
				var mv1 = _v1.a;
				var mv2 = _v1.b;
				var mv3 = _v1.c;
				var remainingMeshFaceVertices = meshFaceVertices.b;
				var p3 = getPosition(mv3);
				var p2 = getPosition(mv2);
				var p1 = getPosition(mv1);
				var c = nextShadowVertexIndex + 2;
				var b = nextShadowVertexIndex + 1;
				var a = nextShadowVertexIndex;
				var $temp$getPosition = getPosition,
					$temp$neighborDict = neighborDict,
					$temp$meshFaceVertices = remainingMeshFaceVertices,
					$temp$nextShadowVertexIndex = nextShadowVertexIndex + 3,
					$temp$state = A6(
					$ianmackenzie$elm_3d_scene$Scene3d$Mesh$joinEdge,
					p3,
					p1,
					c,
					a,
					neighborDict,
					A6(
						$ianmackenzie$elm_3d_scene$Scene3d$Mesh$joinEdge,
						p2,
						p3,
						b,
						c,
						neighborDict,
						A6($ianmackenzie$elm_3d_scene$Scene3d$Mesh$joinEdge, p1, p2, a, b, neighborDict, state)));
				getPosition = $temp$getPosition;
				neighborDict = $temp$neighborDict;
				meshFaceVertices = $temp$meshFaceVertices;
				nextShadowVertexIndex = $temp$nextShadowVertexIndex;
				state = $temp$state;
				continue joinEdges;
			} else {
				var _v2 = state;
				var shadowFaceIndices = _v2.a;
				var extraShadowVertices = _v2.b;
				return _Utils_Tuple2(
					shadowFaceIndices,
					$elm$core$List$reverse(extraShadowVertices));
			}
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$visitFaces = F5(
	function (getPosition, meshFaceVertices, nextShadowVertexIndex, shadowFaceIndices, neighborDict) {
		visitFaces:
		while (true) {
			if (meshFaceVertices.b) {
				var _v1 = meshFaceVertices.a;
				var mv1 = _v1.a;
				var mv2 = _v1.b;
				var mv3 = _v1.c;
				var remainingMeshFaceVertices = meshFaceVertices.b;
				var p3 = getPosition(mv3);
				var p2 = getPosition(mv2);
				var p1 = getPosition(mv1);
				var c = nextShadowVertexIndex + 2;
				var b = nextShadowVertexIndex + 1;
				var a = nextShadowVertexIndex;
				var updatedNeighborDict = A3(
					$elm$core$Dict$insert,
					A2($ianmackenzie$elm_3d_scene$Scene3d$Mesh$edgeKey, p1, p3),
					c,
					A3(
						$elm$core$Dict$insert,
						A2($ianmackenzie$elm_3d_scene$Scene3d$Mesh$edgeKey, p3, p2),
						b,
						A3(
							$elm$core$Dict$insert,
							A2($ianmackenzie$elm_3d_scene$Scene3d$Mesh$edgeKey, p2, p1),
							a,
							neighborDict)));
				var updatedShadowFaceIndices = A2(
					$elm$core$List$cons,
					_Utils_Tuple3(a, b, c),
					shadowFaceIndices);
				var $temp$getPosition = getPosition,
					$temp$meshFaceVertices = remainingMeshFaceVertices,
					$temp$nextShadowVertexIndex = nextShadowVertexIndex + 3,
					$temp$shadowFaceIndices = updatedShadowFaceIndices,
					$temp$neighborDict = updatedNeighborDict;
				getPosition = $temp$getPosition;
				meshFaceVertices = $temp$meshFaceVertices;
				nextShadowVertexIndex = $temp$nextShadowVertexIndex;
				shadowFaceIndices = $temp$shadowFaceIndices;
				neighborDict = $temp$neighborDict;
				continue visitFaces;
			} else {
				return _Utils_Tuple3(shadowFaceIndices, neighborDict, nextShadowVertexIndex);
			}
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$shadowImpl = F3(
	function (meshBounds, getPosition, triangularMesh) {
		var meshFaceVertices = $ianmackenzie$elm_triangular_mesh$TriangularMesh$faceVertices(triangularMesh);
		var initialShadowVertices = A3(
			$elm$core$List$foldr,
			$ianmackenzie$elm_3d_scene$Scene3d$Mesh$collectShadowVertices(getPosition),
			_List_Nil,
			meshFaceVertices);
		var _v0 = A5($ianmackenzie$elm_3d_scene$Scene3d$Mesh$visitFaces, getPosition, meshFaceVertices, 0, _List_Nil, $elm$core$Dict$empty);
		var initialShadowFaceIndices = _v0.a;
		var neighborDict = _v0.b;
		var nextShadowVertexIndex = _v0.c;
		var _v1 = A5(
			$ianmackenzie$elm_3d_scene$Scene3d$Mesh$joinEdges,
			getPosition,
			neighborDict,
			meshFaceVertices,
			0,
			_Utils_Tuple3(initialShadowFaceIndices, _List_Nil, nextShadowVertexIndex));
		var allShadowFaceIndices = _v1.a;
		var extraShadowVertices = _v1.b;
		var allShadowVertices = $elm$core$List$isEmpty(extraShadowVertices) ? initialShadowVertices : _Utils_ap(initialShadowVertices, extraShadowVertices);
		return A3(
			$ianmackenzie$elm_3d_scene$Scene3d$Types$Shadow,
			meshBounds,
			A2(
				$ianmackenzie$elm_triangular_mesh$TriangularMesh$indexed,
				$elm$core$Array$fromList(allShadowVertices),
				allShadowFaceIndices),
			A2($elm_explorations$webgl$WebGL$indexedTriangles, allShadowVertices, allShadowFaceIndices));
	});
var $ianmackenzie$elm_triangular_mesh$TriangularMesh$triangles = function (faceVertices_) {
	return $ianmackenzie$elm_triangular_mesh$TriangularMesh$TriangularMesh(
		{
			faceIndices: A2(
				$elm$core$List$map,
				function (i) {
					return _Utils_Tuple3(3 * i, (3 * i) + 1, (3 * i) + 2);
				},
				A2(
					$elm$core$List$range,
					0,
					$elm$core$List$length(faceVertices_) - 1)),
			vertices: $elm$core$Array$fromList(
				$elm$core$List$concat(
					A2(
						$elm$core$List$map,
						function (_v0) {
							var v1 = _v0.a;
							var v2 = _v0.b;
							var v3 = _v0.c;
							return _List_fromArray(
								[v1, v2, v3]);
						},
						faceVertices_)))
		});
};
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$shadow = function (mesh) {
	switch (mesh.$) {
		case 'EmptyMesh':
			return $ianmackenzie$elm_3d_scene$Scene3d$Types$EmptyShadow;
		case 'Triangles':
			var boundingBox = mesh.a;
			var meshTriangles = mesh.b;
			var vertexTriples = A2($elm$core$List$map, $ianmackenzie$elm_geometry$Triangle3d$vertices, meshTriangles);
			return A3(
				$ianmackenzie$elm_3d_scene$Scene3d$Mesh$shadowImpl,
				boundingBox,
				$elm$core$Basics$identity,
				$ianmackenzie$elm_triangular_mesh$TriangularMesh$triangles(vertexTriples));
		case 'Facets':
			var boundingBox = mesh.a;
			var meshTriangles = mesh.b;
			var vertexTriples = A2($elm$core$List$map, $ianmackenzie$elm_geometry$Triangle3d$vertices, meshTriangles);
			return A3(
				$ianmackenzie$elm_3d_scene$Scene3d$Mesh$shadowImpl,
				boundingBox,
				$elm$core$Basics$identity,
				$ianmackenzie$elm_triangular_mesh$TriangularMesh$triangles(vertexTriples));
		case 'Indexed':
			var boundingBox = mesh.a;
			var triangularMesh = mesh.b;
			return A3($ianmackenzie$elm_3d_scene$Scene3d$Mesh$shadowImpl, boundingBox, $elm$core$Basics$identity, triangularMesh);
		case 'MeshWithNormals':
			var boundingBox = mesh.a;
			var triangularMesh = mesh.b;
			return A3(
				$ianmackenzie$elm_3d_scene$Scene3d$Mesh$shadowImpl,
				boundingBox,
				function ($) {
					return $.position;
				},
				triangularMesh);
		case 'MeshWithUvs':
			var boundingBox = mesh.a;
			var triangularMesh = mesh.b;
			return A3(
				$ianmackenzie$elm_3d_scene$Scene3d$Mesh$shadowImpl,
				boundingBox,
				function ($) {
					return $.position;
				},
				triangularMesh);
		case 'MeshWithNormalsAndUvs':
			var boundingBox = mesh.a;
			var triangularMesh = mesh.b;
			return A3(
				$ianmackenzie$elm_3d_scene$Scene3d$Mesh$shadowImpl,
				boundingBox,
				function ($) {
					return $.position;
				},
				triangularMesh);
		case 'MeshWithTangents':
			var boundingBox = mesh.a;
			var triangularMesh = mesh.b;
			return A3(
				$ianmackenzie$elm_3d_scene$Scene3d$Mesh$shadowImpl,
				boundingBox,
				function ($) {
					return $.position;
				},
				triangularMesh);
		case 'LineSegments':
			return $ianmackenzie$elm_3d_scene$Scene3d$Types$EmptyShadow;
		case 'Polyline':
			return $ianmackenzie$elm_3d_scene$Scene3d$Types$EmptyShadow;
		default:
			return $ianmackenzie$elm_3d_scene$Scene3d$Types$EmptyShadow;
	}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Primitives$blockShadow = $ianmackenzie$elm_3d_scene$Scene3d$Mesh$shadow($ianmackenzie$elm_3d_scene$Scene3d$Primitives$block);
var $ianmackenzie$elm_3d_scene$Scene3d$Types$EmptyNode = {$: 'EmptyNode'};
var $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity = function (a) {
	return {$: 'Entity', a: a};
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty = $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity($ianmackenzie$elm_3d_scene$Scene3d$Types$EmptyNode);
var $ianmackenzie$elm_3d_scene$Scene3d$Types$Group = function (a) {
	return {$: 'Group', a: a};
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$collectNodes = F2(
	function (drawables, accumulated) {
		collectNodes:
		while (true) {
			if (!drawables.b) {
				return accumulated;
			} else {
				var node = drawables.a.a;
				var rest = drawables.b;
				var $temp$drawables = rest,
					$temp$accumulated = A2($elm$core$List$cons, node, accumulated);
				drawables = $temp$drawables;
				accumulated = $temp$accumulated;
				continue collectNodes;
			}
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$group = function (drawables) {
	return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
		$ianmackenzie$elm_3d_scene$Scene3d$Types$Group(
			A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$collectNodes, drawables, _List_Nil)));
};
var $ianmackenzie$elm_3d_scene$Scene3d$Types$MeshNode = F2(
	function (a, b) {
		return {$: 'MeshNode', a: a, b: b};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$colorTextureFragment = {
	src: '\n        precision mediump float;\n        \n        uniform mediump sampler2D colorTexture;\n        \n        varying mediump vec2 interpolatedUv;\n        \n        void main () {\n            gl_FragColor = texture2D(colorTexture, interpolatedUv);\n        }\n    ',
	attributes: {},
	uniforms: {colorTexture: 'colorTexture'}
};
var $elm_explorations$webgl$WebGL$Internal$enableOption = F2(
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
var $elm_explorations$webgl$WebGL$Internal$enableSetting = F2(
	function (cache, setting) {
		switch (setting.$) {
			case 'Blend':
				return A2(_WebGL_enableBlend, cache, setting);
			case 'DepthTest':
				return A2(_WebGL_enableDepthTest, cache, setting);
			case 'StencilTest':
				return A2(_WebGL_enableStencilTest, cache, setting);
			case 'Scissor':
				return A2(_WebGL_enableScissor, cache, setting);
			case 'ColorMask':
				return A2(_WebGL_enableColorMask, cache, setting);
			case 'CullFace':
				return A2(_WebGL_enableCullFace, cache, setting);
			case 'PolygonOffset':
				return A2(_WebGL_enablePolygonOffset, cache, setting);
			case 'SampleCoverage':
				return A2(_WebGL_enableSampleCoverage, cache, setting);
			default:
				return _WebGL_enableSampleAlphaToCoverage(cache);
		}
	});
var $elm_explorations$webgl$WebGL$entityWith = _WebGL_entity;
var $elm_explorations$webgl$WebGL$Settings$FaceMode = function (a) {
	return {$: 'FaceMode', a: a};
};
var $elm_explorations$webgl$WebGL$Settings$back = $elm_explorations$webgl$WebGL$Settings$FaceMode(1029);
var $elm_explorations$webgl$WebGL$Internal$CullFace = function (a) {
	return {$: 'CullFace', a: a};
};
var $elm_explorations$webgl$WebGL$Settings$cullFace = function (_v0) {
	var faceMode = _v0.a;
	return $elm_explorations$webgl$WebGL$Internal$CullFace(faceMode);
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$cullBackFaceSetting = $elm_explorations$webgl$WebGL$Settings$cullFace($elm_explorations$webgl$WebGL$Settings$back);
var $elm_explorations$webgl$WebGL$Settings$front = $elm_explorations$webgl$WebGL$Settings$FaceMode(1028);
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$cullFrontFaceSetting = $elm_explorations$webgl$WebGL$Settings$cullFace($elm_explorations$webgl$WebGL$Settings$front);
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings = F3(
	function (isRightHanded, backFaceSetting, settings) {
		if (backFaceSetting.$ === 'CullBackFaces') {
			return isRightHanded ? A2($elm$core$List$cons, $ianmackenzie$elm_3d_scene$Scene3d$Entity$cullBackFaceSetting, settings) : A2($elm$core$List$cons, $ianmackenzie$elm_3d_scene$Scene3d$Entity$cullFrontFaceSetting, settings);
		} else {
			return settings;
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$unlitVertex = {
	src: '\n        precision highp float;\n        \n        attribute highp vec3 position;\n        attribute mediump vec2 uv;\n        \n        uniform highp vec4 modelScale;\n        uniform highp mat4 modelMatrix;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 projectionMatrix;\n        uniform highp mat4 sceneProperties;\n        \n        varying mediump vec2 interpolatedUv;\n        \n        vec4 getWorldPosition(vec3 modelPosition, vec4 modelScale, mat4 modelMatrix) {\n            vec4 scaledPosition = vec4(modelScale.xyz * modelPosition, 1.0);\n            return modelMatrix * scaledPosition;\n        }\n        \n        void main() {\n            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);\n            gl_Position = projectionMatrix * (viewMatrix * worldPosition);\n            interpolatedUv = uv;\n        }\n    ',
	attributes: {position: 'position', uv: 'uv'},
	uniforms: {modelMatrix: 'modelMatrix', modelScale: 'modelScale', projectionMatrix: 'projectionMatrix', sceneProperties: 'sceneProperties', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$colorTextureMesh = F4(
	function (data, bounds, webGLMesh, backFaceSetting) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Types$MeshNode,
				bounds,
				F8(
					function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, lights, settings) {
						return A5(
							$elm_explorations$webgl$WebGL$entityWith,
							A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, backFaceSetting, settings),
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$unlitVertex,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$colorTextureFragment,
							webGLMesh,
							{colorTexture: data, modelMatrix: modelMatrix, modelScale: modelScale, projectionMatrix: projectionMatrix, sceneProperties: sceneProperties, viewMatrix: viewMatrix});
					})));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$constantFragment = {
	src: '\n        precision lowp float;\n        \n        uniform lowp vec3 constantColor;\n        \n        void main () {\n            gl_FragColor = vec4(constantColor, 1.0);\n        }\n    ',
	attributes: {},
	uniforms: {constantColor: 'constantColor'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$plainVertex = {
	src: '\n        precision highp float;\n        \n        attribute highp vec3 position;\n        \n        uniform highp vec4 modelScale;\n        uniform highp mat4 modelMatrix;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 projectionMatrix;\n        uniform highp mat4 sceneProperties;\n        \n        vec4 getWorldPosition(vec3 modelPosition, vec4 modelScale, mat4 modelMatrix) {\n            vec4 scaledPosition = vec4(modelScale.xyz * modelPosition, 1.0);\n            return modelMatrix * scaledPosition;\n        }\n        \n        void main () {\n            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);\n            gl_Position = projectionMatrix * (viewMatrix * worldPosition);\n        }\n    ',
	attributes: {position: 'position'},
	uniforms: {modelMatrix: 'modelMatrix', modelScale: 'modelScale', projectionMatrix: 'projectionMatrix', sceneProperties: 'sceneProperties', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$constantMesh = F4(
	function (color, bounds, webGLMesh, backFaceSetting) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Types$MeshNode,
				bounds,
				F8(
					function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, lights, settings) {
						return A5(
							$elm_explorations$webgl$WebGL$entityWith,
							A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, backFaceSetting, settings),
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$plainVertex,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$constantFragment,
							webGLMesh,
							{constantColor: color, modelMatrix: modelMatrix, modelScale: modelScale, projectionMatrix: projectionMatrix, sceneProperties: sceneProperties, viewMatrix: viewMatrix});
					})));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Types$PointNode = F2(
	function (a, b) {
		return {$: 'PointNode', a: a, b: b};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$constantPointFragment = {
	src: '\n        precision lowp float;\n        \n        uniform lowp vec3 constantColor;\n        uniform lowp float pointRadius;\n        uniform highp mat4 sceneProperties;\n        \n        float pointAlpha(float pointRadius, vec2 pointCoord) {\n            float pointSize = 2.0 * pointRadius;\n            float x = (pointSize + 2.0) * (pointCoord.s - 0.5);\n            float y = (pointSize + 2.0) * (pointCoord.t - 0.5);\n            float r = sqrt(x * x + y * y);\n            float innerRadius = pointRadius;\n            float outerRadius = pointRadius + 1.0;\n            if (r > outerRadius) {\n                return 0.0;\n            } else if (r > innerRadius) {\n                return outerRadius - r;\n            } else {\n                return 1.0;\n            }\n        }\n        \n        void main () {\n            float supersampling = sceneProperties[3][0];\n            float alpha = pointAlpha(pointRadius * supersampling, gl_PointCoord);\n            gl_FragColor = vec4(constantColor, alpha);\n        }\n    ',
	attributes: {},
	uniforms: {constantColor: 'constantColor', pointRadius: 'pointRadius', sceneProperties: 'sceneProperties'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$pointVertex = {
	src: '\n        precision highp float;\n        \n        attribute highp vec3 position;\n        \n        uniform highp vec4 modelScale;\n        uniform highp mat4 modelMatrix;\n        uniform lowp float pointRadius;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 projectionMatrix;\n        uniform highp mat4 sceneProperties;\n        \n        vec4 getWorldPosition(vec3 modelPosition, vec4 modelScale, mat4 modelMatrix) {\n            vec4 scaledPosition = vec4(modelScale.xyz * modelPosition, 1.0);\n            return modelMatrix * scaledPosition;\n        }\n        \n        void main () {\n            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);\n            gl_Position = projectionMatrix * (viewMatrix * worldPosition);\n            float supersampling = sceneProperties[3][0];\n            gl_PointSize = 2.0 * pointRadius * supersampling + 2.0;\n        }\n    ',
	attributes: {position: 'position'},
	uniforms: {modelMatrix: 'modelMatrix', modelScale: 'modelScale', pointRadius: 'pointRadius', projectionMatrix: 'projectionMatrix', sceneProperties: 'sceneProperties', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$constantPointMesh = F4(
	function (color, radius, bounds, webGLMesh) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Types$PointNode,
				bounds,
				F8(
					function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, lights, settings) {
						return A5(
							$elm_explorations$webgl$WebGL$entityWith,
							settings,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$pointVertex,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$constantPointFragment,
							webGLMesh,
							{constantColor: color, modelMatrix: modelMatrix, modelScale: modelScale, pointRadius: radius, projectionMatrix: projectionMatrix, sceneProperties: sceneProperties, viewMatrix: viewMatrix});
					})));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$emissiveFragment = {
	src: '\n        precision mediump float;\n        \n        uniform mediump vec3 emissiveColor;\n        uniform highp mat4 sceneProperties;\n        \n        float gammaCorrect(float u) {\n            if (u <= 0.0031308) {\n                return 12.92 * u;\n            } else {\n                return 1.055 * pow(u, 1.0 / 2.4) - 0.055;\n            }\n        }\n        \n        vec3 gammaCorrectedColor(vec3 color) {\n            float red = gammaCorrect(color.r);\n            float green = gammaCorrect(color.g);\n            float blue = gammaCorrect(color.b);\n            return vec3(red, green, blue);\n        }\n        \n        vec3 reinhardLuminanceToneMap(vec3 color) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scale = 1.0 / (1.0 + luminance);\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 reinhardPerChannelToneMap(vec3 color) {\n            return gammaCorrectedColor(color / (color + 1.0));\n        }\n        \n        float extendedReinhardToneMap(float x, float xMax) {\n            return x * (1.0 + (x / (xMax * xMax))) / (1.0 + x);\n        }\n        \n        vec3 extendedReinhardLuminanceToneMap(vec3 color, float overexposureLimit) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scaledLuminance = extendedReinhardToneMap(luminance, overexposureLimit);\n            float scale = scaledLuminance / luminance;\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 extendedReinhardPerChannelToneMap(vec3 color, float overexposureLimit) {\n            float red = extendedReinhardToneMap(color.r, overexposureLimit);\n            float green = extendedReinhardToneMap(color.g, overexposureLimit);\n            float blue = extendedReinhardToneMap(color.b, overexposureLimit);\n            return gammaCorrectedColor(vec3(red, green, blue));\n        }\n        \n        vec3 hableFilmicHelper(vec3 color) {\n            float a = 0.15;\n            float b = 0.5;\n            float c = 0.1;\n            float d = 0.2;\n            float e = 0.02;\n            float f = 0.3;\n            return (color * (a * color + c * b) + d * e) / (color * (a * color + b) + d * f) - e / f;\n        }\n        \n        vec3 hableFilmicToneMap(vec3 color) {\n            float exposureBias = 2.0;\n            vec3 unscaled = hableFilmicHelper(exposureBias * color);\n            vec3 scale = 1.0 / hableFilmicHelper(vec3(11.2));\n            return gammaCorrectedColor(scale * unscaled);\n        }\n        \n        vec3 toneMap(vec3 color, float toneMapType, float toneMapParam) {\n            if (toneMapType == 0.0) {\n                return gammaCorrectedColor(color);\n            } else if (toneMapType == 1.0) {\n                return reinhardLuminanceToneMap(color);\n            } else if (toneMapType == 2.0) {\n                return reinhardPerChannelToneMap(color);\n            } else if (toneMapType == 3.0) {\n                return extendedReinhardLuminanceToneMap(color, toneMapParam);\n            } else if (toneMapType == 4.0) {\n                return extendedReinhardPerChannelToneMap(color, toneMapParam);\n            } else if (toneMapType == 5.0) {\n                return hableFilmicToneMap(color);\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        vec4 toSrgb(vec3 linearColor, mat4 sceneProperties) {\n            vec3 referenceWhite = sceneProperties[2].rgb;\n            float unitR = linearColor.r / referenceWhite.r;\n            float unitG = linearColor.g / referenceWhite.g;\n            float unitB = linearColor.b / referenceWhite.b;\n            float toneMapType = sceneProperties[3][2];\n            float toneMapParam = sceneProperties[3][3];\n            vec3 toneMapped = toneMap(vec3(unitR, unitG, unitB), toneMapType, toneMapParam);\n            return vec4(toneMapped, 1.0);\n        }\n        \n        void main () {\n            gl_FragColor = toSrgb(emissiveColor, sceneProperties);\n        }\n    ',
	attributes: {},
	uniforms: {emissiveColor: 'emissiveColor', sceneProperties: 'sceneProperties'}
};
var $ianmackenzie$elm_units$Luminance$inNits = function (_v0) {
	var numNits = _v0.a;
	return numNits;
};
var $elm_explorations$linear_algebra$Math$Vector3$scale = _MJS_v3scale;
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$emissiveMesh = F5(
	function (color, backlight, bounds, webGLMesh, backFaceSetting) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Types$MeshNode,
				bounds,
				F8(
					function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, lights, settings) {
						return A5(
							$elm_explorations$webgl$WebGL$entityWith,
							A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, backFaceSetting, settings),
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$plainVertex,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$emissiveFragment,
							webGLMesh,
							{
								emissiveColor: A2(
									$elm_explorations$linear_algebra$Math$Vector3$scale,
									$ianmackenzie$elm_units$Luminance$inNits(backlight),
									color),
								modelMatrix: modelMatrix,
								modelScale: modelScale,
								projectionMatrix: projectionMatrix,
								sceneProperties: sceneProperties,
								viewMatrix: viewMatrix
							});
					})));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$emissivePointFragment = {
	src: '\n        precision mediump float;\n        \n        uniform mediump vec3 emissiveColor;\n        uniform lowp float pointRadius;\n        uniform highp mat4 sceneProperties;\n        \n        float gammaCorrect(float u) {\n            if (u <= 0.0031308) {\n                return 12.92 * u;\n            } else {\n                return 1.055 * pow(u, 1.0 / 2.4) - 0.055;\n            }\n        }\n        \n        vec3 gammaCorrectedColor(vec3 color) {\n            float red = gammaCorrect(color.r);\n            float green = gammaCorrect(color.g);\n            float blue = gammaCorrect(color.b);\n            return vec3(red, green, blue);\n        }\n        \n        vec3 reinhardLuminanceToneMap(vec3 color) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scale = 1.0 / (1.0 + luminance);\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 reinhardPerChannelToneMap(vec3 color) {\n            return gammaCorrectedColor(color / (color + 1.0));\n        }\n        \n        float extendedReinhardToneMap(float x, float xMax) {\n            return x * (1.0 + (x / (xMax * xMax))) / (1.0 + x);\n        }\n        \n        vec3 extendedReinhardLuminanceToneMap(vec3 color, float overexposureLimit) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scaledLuminance = extendedReinhardToneMap(luminance, overexposureLimit);\n            float scale = scaledLuminance / luminance;\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 extendedReinhardPerChannelToneMap(vec3 color, float overexposureLimit) {\n            float red = extendedReinhardToneMap(color.r, overexposureLimit);\n            float green = extendedReinhardToneMap(color.g, overexposureLimit);\n            float blue = extendedReinhardToneMap(color.b, overexposureLimit);\n            return gammaCorrectedColor(vec3(red, green, blue));\n        }\n        \n        vec3 hableFilmicHelper(vec3 color) {\n            float a = 0.15;\n            float b = 0.5;\n            float c = 0.1;\n            float d = 0.2;\n            float e = 0.02;\n            float f = 0.3;\n            return (color * (a * color + c * b) + d * e) / (color * (a * color + b) + d * f) - e / f;\n        }\n        \n        vec3 hableFilmicToneMap(vec3 color) {\n            float exposureBias = 2.0;\n            vec3 unscaled = hableFilmicHelper(exposureBias * color);\n            vec3 scale = 1.0 / hableFilmicHelper(vec3(11.2));\n            return gammaCorrectedColor(scale * unscaled);\n        }\n        \n        vec3 toneMap(vec3 color, float toneMapType, float toneMapParam) {\n            if (toneMapType == 0.0) {\n                return gammaCorrectedColor(color);\n            } else if (toneMapType == 1.0) {\n                return reinhardLuminanceToneMap(color);\n            } else if (toneMapType == 2.0) {\n                return reinhardPerChannelToneMap(color);\n            } else if (toneMapType == 3.0) {\n                return extendedReinhardLuminanceToneMap(color, toneMapParam);\n            } else if (toneMapType == 4.0) {\n                return extendedReinhardPerChannelToneMap(color, toneMapParam);\n            } else if (toneMapType == 5.0) {\n                return hableFilmicToneMap(color);\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        vec4 toSrgb(vec3 linearColor, mat4 sceneProperties) {\n            vec3 referenceWhite = sceneProperties[2].rgb;\n            float unitR = linearColor.r / referenceWhite.r;\n            float unitG = linearColor.g / referenceWhite.g;\n            float unitB = linearColor.b / referenceWhite.b;\n            float toneMapType = sceneProperties[3][2];\n            float toneMapParam = sceneProperties[3][3];\n            vec3 toneMapped = toneMap(vec3(unitR, unitG, unitB), toneMapType, toneMapParam);\n            return vec4(toneMapped, 1.0);\n        }\n        \n        float pointAlpha(float pointRadius, vec2 pointCoord) {\n            float pointSize = 2.0 * pointRadius;\n            float x = (pointSize + 2.0) * (pointCoord.s - 0.5);\n            float y = (pointSize + 2.0) * (pointCoord.t - 0.5);\n            float r = sqrt(x * x + y * y);\n            float innerRadius = pointRadius;\n            float outerRadius = pointRadius + 1.0;\n            if (r > outerRadius) {\n                return 0.0;\n            } else if (r > innerRadius) {\n                return outerRadius - r;\n            } else {\n                return 1.0;\n            }\n        }\n        \n        void main () {\n            vec4 color = toSrgb(emissiveColor, sceneProperties);\n            float supersampling = sceneProperties[3][0];\n            float alpha = pointAlpha(pointRadius * supersampling, gl_PointCoord);\n            gl_FragColor = vec4(color.rgb, alpha);\n        }\n    ',
	attributes: {},
	uniforms: {emissiveColor: 'emissiveColor', pointRadius: 'pointRadius', sceneProperties: 'sceneProperties'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$emissivePointMesh = F5(
	function (color, backlight, radius, bounds, webGLMesh) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Types$PointNode,
				bounds,
				F8(
					function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, lights, settings) {
						return A5(
							$elm_explorations$webgl$WebGL$entityWith,
							settings,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$pointVertex,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$emissivePointFragment,
							webGLMesh,
							{
								emissiveColor: A2(
									$elm_explorations$linear_algebra$Math$Vector3$scale,
									$ianmackenzie$elm_units$Luminance$inNits(backlight),
									color),
								modelMatrix: modelMatrix,
								modelScale: modelScale,
								pointRadius: radius,
								projectionMatrix: projectionMatrix,
								sceneProperties: sceneProperties,
								viewMatrix: viewMatrix
							});
					})));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$lambertianFragment = {
	src: '\n        precision highp float;\n        \n        uniform highp mat4 sceneProperties;\n        uniform highp mat4 lights12;\n        uniform highp mat4 lights34;\n        uniform highp mat4 lights56;\n        uniform highp mat4 lights78;\n        uniform lowp vec4 enabledLights;\n        uniform lowp vec3 materialColor;\n        uniform highp mat4 viewMatrix;\n        \n        varying highp vec3 interpolatedPosition;\n        varying highp vec3 interpolatedNormal;\n        \n        const lowp float kPerspectiveProjection = 0.0;\n        const lowp float kOrthographicProjection = 1.0;\n        const lowp float kDirectionalLight = 1.0;\n        const lowp float kPointLight = 2.0;\n        const highp float kPi = 3.14159265359;\n        const lowp float kDisabledLight = 0.0;\n        const lowp float kSoftLighting = 3.0;\n        \n        float getNormalSign() {\n            return 2.0 * float(gl_FrontFacing) - 1.0;\n        }\n        \n        vec3 getDirectionToCamera(vec3 surfacePosition, mat4 sceneProperties) {\n            float projectionType = sceneProperties[1].w;\n            if (projectionType == kPerspectiveProjection) {\n                vec3 cameraPoint = sceneProperties[1].xyz;\n                return normalize(cameraPoint - surfacePosition);\n            } else if (projectionType == kOrthographicProjection) {\n                return sceneProperties[1].xyz;\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        void getDirectionToLightAndNormalIlluminance(\n            vec4 xyz_type,\n            vec4 rgb_parameter,\n            vec3 surfacePosition,\n            out vec3 directionToLight,\n            out vec3 normalIlluminance\n        ) {\n            float lightType = xyz_type.w;\n            if (lightType == kDirectionalLight) {\n                directionToLight = xyz_type.xyz;\n                normalIlluminance = rgb_parameter.rgb;\n            } else if (lightType == kPointLight) {\n                vec3 lightPosition = xyz_type.xyz;\n                vec3 displacement = lightPosition - surfacePosition;\n                float distance = length(displacement);\n                directionToLight = displacement / distance;\n                normalIlluminance = rgb_parameter.rgb / (4.0 * kPi * distance * distance);\n            }\n        }\n        \n        float positiveDotProduct(vec3 v1, vec3 v2) {\n            return clamp(dot(v1, v2), 0.0, 1.0);\n        }\n        \n        vec3 softLightingLuminance(\n            vec3 aboveLuminance,\n            vec3 belowLuminance,\n            vec3 localUpDirection,\n            vec3 localLightDirection\n        ) {\n            float sinElevation = dot(localLightDirection, localUpDirection);\n            float t = (sinElevation + 1.0) / 2.0;\n            return aboveLuminance * t + belowLuminance * (1.0 - t);\n        }\n        \n        vec3 lambertianLight(\n            vec3 surfacePosition,\n            vec3 surfaceNormal,\n            vec3 materialColor,\n            vec4 xyz_type,\n            vec4 rgb_parameter\n        ) {\n            float lightType = xyz_type.w;\n            if (lightType == kDisabledLight) {\n                return vec3(0.0, 0.0, 0.0);\n            } else if (lightType == kSoftLighting) {\n                vec3 upDirection = xyz_type.xyz;\n                vec3 aboveLuminance = rgb_parameter.rgb;\n                vec3 belowLuminance = rgb_parameter.a * aboveLuminance;\n                vec3 luminance = softLightingLuminance(aboveLuminance, belowLuminance, upDirection, surfaceNormal);\n                return luminance * materialColor;\n            }\n        \n            vec3 directionToLight = vec3(0.0, 0.0, 0.0);\n            vec3 normalIlluminance = vec3(0.0, 0.0, 0.0);\n            getDirectionToLightAndNormalIlluminance(\n                xyz_type,\n                rgb_parameter,\n                surfacePosition,\n                directionToLight,\n                normalIlluminance\n            );\n        \n            float dotNL = positiveDotProduct(directionToLight, surfaceNormal);\n            return (normalIlluminance * dotNL) * (materialColor / kPi);\n        }\n        \n        vec3 lambertianLighting(\n            vec3 surfacePosition,\n            vec3 surfaceNormal,\n            vec3 materialColor,\n            mat4 lights12,\n            mat4 lights34,\n            mat4 lights56,\n            mat4 lights78,\n            vec4 enabledLights\n        ) {\n            vec3 litColor1 = enabledLights[0] == 1.0 ? lambertianLight(surfacePosition, surfaceNormal, materialColor, lights12[0], lights12[1]) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor2 = enabledLights[1] == 1.0 ? lambertianLight(surfacePosition, surfaceNormal, materialColor, lights12[2], lights12[3]) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor3 = enabledLights[2] == 1.0 ? lambertianLight(surfacePosition, surfaceNormal, materialColor, lights34[0], lights34[1]) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor4 = enabledLights[3] == 1.0 ? lambertianLight(surfacePosition, surfaceNormal, materialColor, lights34[2], lights34[3]) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor5 = lambertianLight(surfacePosition, surfaceNormal, materialColor, lights56[0], lights56[1]);\n            vec3 litColor6 = lambertianLight(surfacePosition, surfaceNormal, materialColor, lights56[2], lights56[3]);\n            vec3 litColor7 = lambertianLight(surfacePosition, surfaceNormal, materialColor, lights78[0], lights78[1]);\n            vec3 litColor8 = lambertianLight(surfacePosition, surfaceNormal, materialColor, lights78[2], lights78[3]);\n            return litColor1 + litColor2 + litColor3 + litColor4 + litColor5 + litColor6 + litColor7 + litColor8;\n        }\n        \n        float gammaCorrect(float u) {\n            if (u <= 0.0031308) {\n                return 12.92 * u;\n            } else {\n                return 1.055 * pow(u, 1.0 / 2.4) - 0.055;\n            }\n        }\n        \n        vec3 gammaCorrectedColor(vec3 color) {\n            float red = gammaCorrect(color.r);\n            float green = gammaCorrect(color.g);\n            float blue = gammaCorrect(color.b);\n            return vec3(red, green, blue);\n        }\n        \n        vec3 reinhardLuminanceToneMap(vec3 color) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scale = 1.0 / (1.0 + luminance);\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 reinhardPerChannelToneMap(vec3 color) {\n            return gammaCorrectedColor(color / (color + 1.0));\n        }\n        \n        float extendedReinhardToneMap(float x, float xMax) {\n            return x * (1.0 + (x / (xMax * xMax))) / (1.0 + x);\n        }\n        \n        vec3 extendedReinhardLuminanceToneMap(vec3 color, float overexposureLimit) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scaledLuminance = extendedReinhardToneMap(luminance, overexposureLimit);\n            float scale = scaledLuminance / luminance;\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 extendedReinhardPerChannelToneMap(vec3 color, float overexposureLimit) {\n            float red = extendedReinhardToneMap(color.r, overexposureLimit);\n            float green = extendedReinhardToneMap(color.g, overexposureLimit);\n            float blue = extendedReinhardToneMap(color.b, overexposureLimit);\n            return gammaCorrectedColor(vec3(red, green, blue));\n        }\n        \n        vec3 hableFilmicHelper(vec3 color) {\n            float a = 0.15;\n            float b = 0.5;\n            float c = 0.1;\n            float d = 0.2;\n            float e = 0.02;\n            float f = 0.3;\n            return (color * (a * color + c * b) + d * e) / (color * (a * color + b) + d * f) - e / f;\n        }\n        \n        vec3 hableFilmicToneMap(vec3 color) {\n            float exposureBias = 2.0;\n            vec3 unscaled = hableFilmicHelper(exposureBias * color);\n            vec3 scale = 1.0 / hableFilmicHelper(vec3(11.2));\n            return gammaCorrectedColor(scale * unscaled);\n        }\n        \n        vec3 toneMap(vec3 color, float toneMapType, float toneMapParam) {\n            if (toneMapType == 0.0) {\n                return gammaCorrectedColor(color);\n            } else if (toneMapType == 1.0) {\n                return reinhardLuminanceToneMap(color);\n            } else if (toneMapType == 2.0) {\n                return reinhardPerChannelToneMap(color);\n            } else if (toneMapType == 3.0) {\n                return extendedReinhardLuminanceToneMap(color, toneMapParam);\n            } else if (toneMapType == 4.0) {\n                return extendedReinhardPerChannelToneMap(color, toneMapParam);\n            } else if (toneMapType == 5.0) {\n                return hableFilmicToneMap(color);\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        vec4 toSrgb(vec3 linearColor, mat4 sceneProperties) {\n            vec3 referenceWhite = sceneProperties[2].rgb;\n            float unitR = linearColor.r / referenceWhite.r;\n            float unitG = linearColor.g / referenceWhite.g;\n            float unitB = linearColor.b / referenceWhite.b;\n            float toneMapType = sceneProperties[3][2];\n            float toneMapParam = sceneProperties[3][3];\n            vec3 toneMapped = toneMap(vec3(unitR, unitG, unitB), toneMapType, toneMapParam);\n            return vec4(toneMapped, 1.0);\n        }\n        \n        void main() {\n            vec3 normalDirection = normalize(interpolatedNormal) * getNormalSign();\n            vec3 directionToCamera = getDirectionToCamera(interpolatedPosition, sceneProperties);\n        \n            vec3 linearColor = lambertianLighting(\n                interpolatedPosition,\n                normalDirection,\n                materialColor,\n                lights12,\n                lights34,\n                lights56,\n                lights78,\n                enabledLights\n            );\n        \n            gl_FragColor = toSrgb(linearColor, sceneProperties);\n        }\n    ',
	attributes: {},
	uniforms: {enabledLights: 'enabledLights', lights12: 'lights12', lights34: 'lights34', lights56: 'lights56', lights78: 'lights78', materialColor: 'materialColor', sceneProperties: 'sceneProperties', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$uniformVertex = {
	src: '\n        precision highp float;\n        \n        attribute highp vec3 position;\n        attribute highp vec3 normal;\n        \n        uniform highp vec4 modelScale;\n        uniform highp mat4 modelMatrix;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 projectionMatrix;\n        uniform highp mat4 sceneProperties;\n        \n        varying highp vec3 interpolatedPosition;\n        varying highp vec3 interpolatedNormal;\n        \n        vec4 getWorldPosition(vec3 modelPosition, vec4 modelScale, mat4 modelMatrix) {\n            vec4 scaledPosition = vec4(modelScale.xyz * modelPosition, 1.0);\n            return modelMatrix * scaledPosition;\n        }\n        \n        vec3 safeNormalize(vec3 vector) {\n            if (vector == vec3(0.0, 0.0, 0.0)) {\n                return vector;\n            } else {\n                return normalize(vector);\n            }\n        }\n        \n        vec3 getWorldNormal(vec3 modelNormal, vec4 modelScale, mat4 modelMatrix) {\n            vec3 normalScale = vec3(modelScale.w / modelScale.x, modelScale.w / modelScale.y, modelScale.w / modelScale.z);\n            return (modelMatrix * vec4(safeNormalize(normalScale * modelNormal), 0.0)).xyz;\n        }\n        \n        void main () {\n            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);\n            gl_Position = projectionMatrix * (viewMatrix * worldPosition);\n            interpolatedPosition = worldPosition.xyz;\n            interpolatedNormal = getWorldNormal(normal, modelScale, modelMatrix);\n        }\n    ',
	attributes: {normal: 'normal', position: 'position'},
	uniforms: {modelMatrix: 'modelMatrix', modelScale: 'modelScale', projectionMatrix: 'projectionMatrix', sceneProperties: 'sceneProperties', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$lambertianMesh = F4(
	function (color, bounds, webGLMesh, backFaceSetting) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Types$MeshNode,
				bounds,
				F8(
					function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, _v0, settings) {
						var lights = _v0.a;
						var enabledLights = _v0.b;
						return A5(
							$elm_explorations$webgl$WebGL$entityWith,
							A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, backFaceSetting, settings),
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$uniformVertex,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$lambertianFragment,
							webGLMesh,
							{enabledLights: enabledLights, lights12: lights.lights12, lights34: lights.lights34, lights56: lights.lights56, lights78: lights.lights78, materialColor: color, modelMatrix: modelMatrix, modelScale: modelScale, projectionMatrix: projectionMatrix, sceneProperties: sceneProperties, viewMatrix: viewMatrix});
					})));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$lambertianTextureFragment = {
	src: '\n        precision highp float;\n        \n        uniform highp mat4 sceneProperties;\n        uniform highp mat4 lights12;\n        uniform highp mat4 lights34;\n        uniform highp mat4 lights56;\n        uniform highp mat4 lights78;\n        uniform lowp vec4 enabledLights;\n        uniform mediump sampler2D materialColorTexture;\n        uniform mediump sampler2D normalMapTexture;\n        uniform lowp float useNormalMap;\n        uniform highp mat4 viewMatrix;\n        \n        varying highp vec3 interpolatedPosition;\n        varying highp vec3 interpolatedNormal;\n        varying mediump vec2 interpolatedUv;\n        varying highp vec3 interpolatedTangent;\n        \n        const lowp float kPerspectiveProjection = 0.0;\n        const lowp float kOrthographicProjection = 1.0;\n        const lowp float kDirectionalLight = 1.0;\n        const lowp float kPointLight = 2.0;\n        const highp float kPi = 3.14159265359;\n        const lowp float kDisabledLight = 0.0;\n        const lowp float kSoftLighting = 3.0;\n        \n        vec3 getLocalNormal(sampler2D normalMap, float useNormalMap, vec2 uv) {\n            vec3 rgb = useNormalMap * texture2D(normalMap, uv).rgb + (1.0 - useNormalMap) * vec3(0.5, 0.5, 1.0);\n            float x = 2.0 * (rgb.r - 0.5);\n            float y = 2.0 * (rgb.g - 0.5);\n            float z = 2.0 * (rgb.b - 0.5);\n            return normalize(vec3(-x, -y, z));\n        }\n        \n        float getNormalSign() {\n            return 2.0 * float(gl_FrontFacing) - 1.0;\n        }\n        \n        vec3 getMappedNormal(vec3 normal, vec3 tangent, float normalSign, vec3 localNormal) {\n            vec3 bitangent = cross(normal, tangent) * normalSign;\n            return normalize(localNormal.x * tangent + localNormal.y * bitangent + localNormal.z * normal);\n        }\n        \n        vec3 getDirectionToCamera(vec3 surfacePosition, mat4 sceneProperties) {\n            float projectionType = sceneProperties[1].w;\n            if (projectionType == kPerspectiveProjection) {\n                vec3 cameraPoint = sceneProperties[1].xyz;\n                return normalize(cameraPoint - surfacePosition);\n            } else if (projectionType == kOrthographicProjection) {\n                return sceneProperties[1].xyz;\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        void getDirectionToLightAndNormalIlluminance(\n            vec4 xyz_type,\n            vec4 rgb_parameter,\n            vec3 surfacePosition,\n            out vec3 directionToLight,\n            out vec3 normalIlluminance\n        ) {\n            float lightType = xyz_type.w;\n            if (lightType == kDirectionalLight) {\n                directionToLight = xyz_type.xyz;\n                normalIlluminance = rgb_parameter.rgb;\n            } else if (lightType == kPointLight) {\n                vec3 lightPosition = xyz_type.xyz;\n                vec3 displacement = lightPosition - surfacePosition;\n                float distance = length(displacement);\n                directionToLight = displacement / distance;\n                normalIlluminance = rgb_parameter.rgb / (4.0 * kPi * distance * distance);\n            }\n        }\n        \n        float positiveDotProduct(vec3 v1, vec3 v2) {\n            return clamp(dot(v1, v2), 0.0, 1.0);\n        }\n        \n        vec3 softLightingLuminance(\n            vec3 aboveLuminance,\n            vec3 belowLuminance,\n            vec3 localUpDirection,\n            vec3 localLightDirection\n        ) {\n            float sinElevation = dot(localLightDirection, localUpDirection);\n            float t = (sinElevation + 1.0) / 2.0;\n            return aboveLuminance * t + belowLuminance * (1.0 - t);\n        }\n        \n        vec3 lambertianLight(\n            vec3 surfacePosition,\n            vec3 surfaceNormal,\n            vec3 materialColor,\n            vec4 xyz_type,\n            vec4 rgb_parameter\n        ) {\n            float lightType = xyz_type.w;\n            if (lightType == kDisabledLight) {\n                return vec3(0.0, 0.0, 0.0);\n            } else if (lightType == kSoftLighting) {\n                vec3 upDirection = xyz_type.xyz;\n                vec3 aboveLuminance = rgb_parameter.rgb;\n                vec3 belowLuminance = rgb_parameter.a * aboveLuminance;\n                vec3 luminance = softLightingLuminance(aboveLuminance, belowLuminance, upDirection, surfaceNormal);\n                return luminance * materialColor;\n            }\n        \n            vec3 directionToLight = vec3(0.0, 0.0, 0.0);\n            vec3 normalIlluminance = vec3(0.0, 0.0, 0.0);\n            getDirectionToLightAndNormalIlluminance(\n                xyz_type,\n                rgb_parameter,\n                surfacePosition,\n                directionToLight,\n                normalIlluminance\n            );\n        \n            float dotNL = positiveDotProduct(directionToLight, surfaceNormal);\n            return (normalIlluminance * dotNL) * (materialColor / kPi);\n        }\n        \n        vec3 lambertianLighting(\n            vec3 surfacePosition,\n            vec3 surfaceNormal,\n            vec3 materialColor,\n            mat4 lights12,\n            mat4 lights34,\n            mat4 lights56,\n            mat4 lights78,\n            vec4 enabledLights\n        ) {\n            vec3 litColor1 = enabledLights[0] == 1.0 ? lambertianLight(surfacePosition, surfaceNormal, materialColor, lights12[0], lights12[1]) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor2 = enabledLights[1] == 1.0 ? lambertianLight(surfacePosition, surfaceNormal, materialColor, lights12[2], lights12[3]) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor3 = enabledLights[2] == 1.0 ? lambertianLight(surfacePosition, surfaceNormal, materialColor, lights34[0], lights34[1]) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor4 = enabledLights[3] == 1.0 ? lambertianLight(surfacePosition, surfaceNormal, materialColor, lights34[2], lights34[3]) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor5 = lambertianLight(surfacePosition, surfaceNormal, materialColor, lights56[0], lights56[1]);\n            vec3 litColor6 = lambertianLight(surfacePosition, surfaceNormal, materialColor, lights56[2], lights56[3]);\n            vec3 litColor7 = lambertianLight(surfacePosition, surfaceNormal, materialColor, lights78[0], lights78[1]);\n            vec3 litColor8 = lambertianLight(surfacePosition, surfaceNormal, materialColor, lights78[2], lights78[3]);\n            return litColor1 + litColor2 + litColor3 + litColor4 + litColor5 + litColor6 + litColor7 + litColor8;\n        }\n        \n        float inverseGamma(float u) {\n            if (u <= 0.04045) {\n                return clamp(u / 12.92, 0.0, 1.0);\n            } else {\n                return clamp(pow((u + 0.055) / 1.055, 2.4), 0.0, 1.0);\n            }\n        }\n        \n        vec3 fromSrgb(vec3 srgbColor) {\n            return vec3(\n                inverseGamma(srgbColor.r),\n                inverseGamma(srgbColor.g),\n                inverseGamma(srgbColor.b)\n            );\n        }\n        \n        float gammaCorrect(float u) {\n            if (u <= 0.0031308) {\n                return 12.92 * u;\n            } else {\n                return 1.055 * pow(u, 1.0 / 2.4) - 0.055;\n            }\n        }\n        \n        vec3 gammaCorrectedColor(vec3 color) {\n            float red = gammaCorrect(color.r);\n            float green = gammaCorrect(color.g);\n            float blue = gammaCorrect(color.b);\n            return vec3(red, green, blue);\n        }\n        \n        vec3 reinhardLuminanceToneMap(vec3 color) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scale = 1.0 / (1.0 + luminance);\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 reinhardPerChannelToneMap(vec3 color) {\n            return gammaCorrectedColor(color / (color + 1.0));\n        }\n        \n        float extendedReinhardToneMap(float x, float xMax) {\n            return x * (1.0 + (x / (xMax * xMax))) / (1.0 + x);\n        }\n        \n        vec3 extendedReinhardLuminanceToneMap(vec3 color, float overexposureLimit) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scaledLuminance = extendedReinhardToneMap(luminance, overexposureLimit);\n            float scale = scaledLuminance / luminance;\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 extendedReinhardPerChannelToneMap(vec3 color, float overexposureLimit) {\n            float red = extendedReinhardToneMap(color.r, overexposureLimit);\n            float green = extendedReinhardToneMap(color.g, overexposureLimit);\n            float blue = extendedReinhardToneMap(color.b, overexposureLimit);\n            return gammaCorrectedColor(vec3(red, green, blue));\n        }\n        \n        vec3 hableFilmicHelper(vec3 color) {\n            float a = 0.15;\n            float b = 0.5;\n            float c = 0.1;\n            float d = 0.2;\n            float e = 0.02;\n            float f = 0.3;\n            return (color * (a * color + c * b) + d * e) / (color * (a * color + b) + d * f) - e / f;\n        }\n        \n        vec3 hableFilmicToneMap(vec3 color) {\n            float exposureBias = 2.0;\n            vec3 unscaled = hableFilmicHelper(exposureBias * color);\n            vec3 scale = 1.0 / hableFilmicHelper(vec3(11.2));\n            return gammaCorrectedColor(scale * unscaled);\n        }\n        \n        vec3 toneMap(vec3 color, float toneMapType, float toneMapParam) {\n            if (toneMapType == 0.0) {\n                return gammaCorrectedColor(color);\n            } else if (toneMapType == 1.0) {\n                return reinhardLuminanceToneMap(color);\n            } else if (toneMapType == 2.0) {\n                return reinhardPerChannelToneMap(color);\n            } else if (toneMapType == 3.0) {\n                return extendedReinhardLuminanceToneMap(color, toneMapParam);\n            } else if (toneMapType == 4.0) {\n                return extendedReinhardPerChannelToneMap(color, toneMapParam);\n            } else if (toneMapType == 5.0) {\n                return hableFilmicToneMap(color);\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        vec4 toSrgb(vec3 linearColor, mat4 sceneProperties) {\n            vec3 referenceWhite = sceneProperties[2].rgb;\n            float unitR = linearColor.r / referenceWhite.r;\n            float unitG = linearColor.g / referenceWhite.g;\n            float unitB = linearColor.b / referenceWhite.b;\n            float toneMapType = sceneProperties[3][2];\n            float toneMapParam = sceneProperties[3][3];\n            vec3 toneMapped = toneMap(vec3(unitR, unitG, unitB), toneMapType, toneMapParam);\n            return vec4(toneMapped, 1.0);\n        }\n        \n        void main() {\n            vec3 localNormal = getLocalNormal(normalMapTexture, useNormalMap, interpolatedUv);\n            float normalSign = getNormalSign();\n            vec3 originalNormal = normalize(interpolatedNormal) * normalSign;\n            vec3 normalDirection = getMappedNormal(originalNormal, interpolatedTangent, normalSign, localNormal);\n            vec3 directionToCamera = getDirectionToCamera(interpolatedPosition, sceneProperties);\n            vec3 materialColor = fromSrgb(texture2D(materialColorTexture, interpolatedUv).rgb);\n        \n            vec3 linearColor = lambertianLighting(\n                interpolatedPosition,\n                normalDirection,\n                materialColor,\n                lights12,\n                lights34,\n                lights56,\n                lights78,\n                enabledLights\n            );\n        \n            gl_FragColor = toSrgb(linearColor, sceneProperties);\n        }\n    ',
	attributes: {},
	uniforms: {enabledLights: 'enabledLights', lights12: 'lights12', lights34: 'lights34', lights56: 'lights56', lights78: 'lights78', materialColorTexture: 'materialColorTexture', normalMapTexture: 'normalMapTexture', sceneProperties: 'sceneProperties', useNormalMap: 'useNormalMap', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$normalMappedVertex = {
	src: '\n        precision highp float;\n        \n        attribute highp vec3 position;\n        attribute highp vec3 normal;\n        attribute mediump vec2 uv;\n        attribute highp vec3 tangent;\n        \n        uniform highp vec4 modelScale;\n        uniform highp mat4 modelMatrix;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 projectionMatrix;\n        uniform highp mat4 sceneProperties;\n        \n        varying highp vec3 interpolatedPosition;\n        varying highp vec3 interpolatedNormal;\n        varying mediump vec2 interpolatedUv;\n        varying highp vec3 interpolatedTangent;\n        \n        vec4 getWorldPosition(vec3 modelPosition, vec4 modelScale, mat4 modelMatrix) {\n            vec4 scaledPosition = vec4(modelScale.xyz * modelPosition, 1.0);\n            return modelMatrix * scaledPosition;\n        }\n        \n        vec3 safeNormalize(vec3 vector) {\n            if (vector == vec3(0.0, 0.0, 0.0)) {\n                return vector;\n            } else {\n                return normalize(vector);\n            }\n        }\n        \n        vec3 getWorldNormal(vec3 modelNormal, vec4 modelScale, mat4 modelMatrix) {\n            vec3 normalScale = vec3(modelScale.w / modelScale.x, modelScale.w / modelScale.y, modelScale.w / modelScale.z);\n            return (modelMatrix * vec4(safeNormalize(normalScale * modelNormal), 0.0)).xyz;\n        }\n        \n        vec3 getWorldTangent(vec3 modelTangent, vec4 modelScale, mat4 modelMatrix) {\n            return (modelMatrix * vec4(safeNormalize(modelScale.xyz * modelTangent), 0.0)).xyz;\n        }\n        \n        void main () {\n            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);\n            gl_Position = projectionMatrix * (viewMatrix * worldPosition);\n            interpolatedPosition = worldPosition.xyz;\n            interpolatedNormal = getWorldNormal(normal, modelScale, modelMatrix);\n            interpolatedUv = uv;\n            interpolatedTangent = getWorldTangent(tangent, modelScale, modelMatrix);\n        }\n    ',
	attributes: {normal: 'normal', position: 'position', tangent: 'tangent', uv: 'uv'},
	uniforms: {modelMatrix: 'modelMatrix', modelScale: 'modelScale', projectionMatrix: 'projectionMatrix', sceneProperties: 'sceneProperties', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$normalMappedLambertianMesh = F6(
	function (materialColorData, normalMapData, useNormalMap, bounds, webGLMesh, backFaceSetting) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Types$MeshNode,
				bounds,
				F8(
					function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, _v0, settings) {
						var lights = _v0.a;
						var enabledLights = _v0.b;
						return A5(
							$elm_explorations$webgl$WebGL$entityWith,
							A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, backFaceSetting, settings),
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$normalMappedVertex,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$lambertianTextureFragment,
							webGLMesh,
							{enabledLights: enabledLights, lights12: lights.lights12, lights34: lights.lights34, lights56: lights.lights56, lights78: lights.lights78, materialColorTexture: materialColorData, modelMatrix: modelMatrix, modelScale: modelScale, normalMapTexture: normalMapData, projectionMatrix: projectionMatrix, sceneProperties: sceneProperties, useNormalMap: useNormalMap, viewMatrix: viewMatrix});
					})));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$physicalTexturesFragment = {
	src: '\n        precision highp float;\n        \n        uniform highp mat4 sceneProperties;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 lights12;\n        uniform highp mat4 lights34;\n        uniform highp mat4 lights56;\n        uniform highp mat4 lights78;\n        uniform lowp vec4 enabledLights;\n        uniform mediump sampler2D baseColorTexture;\n        uniform lowp vec4 constantBaseColor;\n        uniform mediump sampler2D roughnessTexture;\n        uniform lowp vec2 constantRoughness;\n        uniform mediump sampler2D metallicTexture;\n        uniform lowp vec2 constantMetallic;\n        uniform mediump sampler2D normalMapTexture;\n        uniform lowp float useNormalMap;\n        \n        varying highp vec3 interpolatedPosition;\n        varying highp vec3 interpolatedNormal;\n        varying mediump vec2 interpolatedUv;\n        varying highp vec3 interpolatedTangent;\n        \n        const lowp float kPerspectiveProjection = 0.0;\n        const lowp float kOrthographicProjection = 1.0;\n        const lowp float kDirectionalLight = 1.0;\n        const lowp float kPointLight = 2.0;\n        const highp float kPi = 3.14159265359;\n        const mediump float kMediumpFloatMax = 65504.0;\n        const lowp float kDisabledLight = 0.0;\n        const lowp float kSoftLighting = 3.0;\n        \n        float getFloatValue(sampler2D texture, vec2 uv, vec2 constantValue) {\n            if (constantValue.y == 1.0) {\n                return constantValue.x;\n            } else {\n                vec4 textureColor = texture2D(texture, uv);\n                return dot(textureColor, vec4(0.2126, 0.7152, 0.0722, 0.0));\n            }\n        }\n        \n        vec3 getLocalNormal(sampler2D normalMap, float useNormalMap, vec2 uv) {\n            vec3 rgb = useNormalMap * texture2D(normalMap, uv).rgb + (1.0 - useNormalMap) * vec3(0.5, 0.5, 1.0);\n            float x = 2.0 * (rgb.r - 0.5);\n            float y = 2.0 * (rgb.g - 0.5);\n            float z = 2.0 * (rgb.b - 0.5);\n            return normalize(vec3(-x, -y, z));\n        }\n        \n        float getNormalSign() {\n            return 2.0 * float(gl_FrontFacing) - 1.0;\n        }\n        \n        vec3 getMappedNormal(vec3 normal, vec3 tangent, float normalSign, vec3 localNormal) {\n            vec3 bitangent = cross(normal, tangent) * normalSign;\n            return normalize(localNormal.x * tangent + localNormal.y * bitangent + localNormal.z * normal);\n        }\n        \n        vec3 getDirectionToCamera(vec3 surfacePosition, mat4 sceneProperties) {\n            float projectionType = sceneProperties[1].w;\n            if (projectionType == kPerspectiveProjection) {\n                vec3 cameraPoint = sceneProperties[1].xyz;\n                return normalize(cameraPoint - surfacePosition);\n            } else if (projectionType == kOrthographicProjection) {\n                return sceneProperties[1].xyz;\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        void getDirectionToLightAndNormalIlluminance(\n            vec4 xyz_type,\n            vec4 rgb_parameter,\n            vec3 surfacePosition,\n            out vec3 directionToLight,\n            out vec3 normalIlluminance\n        ) {\n            float lightType = xyz_type.w;\n            if (lightType == kDirectionalLight) {\n                directionToLight = xyz_type.xyz;\n                normalIlluminance = rgb_parameter.rgb;\n            } else if (lightType == kPointLight) {\n                vec3 lightPosition = xyz_type.xyz;\n                vec3 displacement = lightPosition - surfacePosition;\n                float distance = length(displacement);\n                directionToLight = displacement / distance;\n                normalIlluminance = rgb_parameter.rgb / (4.0 * kPi * distance * distance);\n            }\n        }\n        \n        float positiveDotProduct(vec3 v1, vec3 v2) {\n            return clamp(dot(v1, v2), 0.0, 1.0);\n        }\n        \n        // Adapted from https://google.github.io/filament/Filament.md.html#materialsystem/specularbrdf/normaldistributionfunction(speculard)\n        float specularD(float alpha, float dotNH, vec3 normalDirection, vec3 halfDirection) {\n            vec3 crossNH = cross(normalDirection, halfDirection);\n            float a = dotNH * alpha;\n            float k = alpha / (dot(crossNH, crossNH) + a * a);\n            float d = k * k * (1.0 / kPi);\n            return min(d, kMediumpFloatMax);\n        }\n        \n        float safeQuotient(float numerator, float denominator) {\n            if (denominator == 0.0) {\n                return 0.0;\n            } else {\n                return numerator / denominator;\n            }\n        }\n        \n        float g1(float dotNV, float alphaSquared) {\n            return safeQuotient(2.0 * dotNV, dotNV + sqrt(alphaSquared + (1.0 - alphaSquared) * dotNV * dotNV));\n        }\n        \n        float specularG(float dotNL, float dotNV, float alphaSquared) {\n            return g1(dotNV, alphaSquared) * g1(dotNL, alphaSquared);\n        }\n        \n        vec3 fresnelColor(vec3 specularBaseColor, float dotVH) {\n            vec3 one = vec3(1.0, 1.0, 1.0);\n            float scale = exp2((-5.55473 * dotVH - 6.98316) * dotVH);\n            return specularBaseColor + (one - specularBaseColor) * scale;\n        }\n        \n        vec3 brdf(vec3 normalDirection, vec3 directionToCamera, vec3 directionToLight, float alpha, float dotNV, float dotNL, vec3 specularBaseColor, vec3 normalIlluminance) {\n            vec3 halfDirection = normalize(directionToCamera + directionToLight);\n            float dotVH = positiveDotProduct(directionToCamera, halfDirection);\n            float dotNH = positiveDotProduct(normalDirection, halfDirection);\n            float dotNHSquared = dotNH * dotNH;\n        \n            float d = specularD(alpha, dotNH, normalDirection, halfDirection);\n            float g = specularG(dotNL, dotNV, alpha * alpha);\n            vec3 f = fresnelColor(specularBaseColor, dotVH);\n            return safeQuotient(d * g, 4.0 * dotNL * dotNV) * f;\n        }\n        \n        vec3 sampleFacetNormal(vec3 vH, vec3 vT1, vec3 vT2, float s, float alpha) {\n            float t2 = (1.0 - s);\n            vec3 vNh = t2 * vT2 + sqrt(max(0.0, 1.0 - t2 * t2)) * vH;\n            return normalize(vec3(alpha * vNh.x, alpha * vNh.y, max(0.0, vNh.z)));\n        }\n        \n        vec3 softLightingLuminance(\n            vec3 aboveLuminance,\n            vec3 belowLuminance,\n            vec3 localUpDirection,\n            vec3 localLightDirection\n        ) {\n            float sinElevation = dot(localLightDirection, localUpDirection);\n            float t = (sinElevation + 1.0) / 2.0;\n            return aboveLuminance * t + belowLuminance * (1.0 - t);\n        }\n        \n        vec3 softLightingSpecularSample(\n            vec3 aboveLuminance,\n            vec3 belowLuminance,\n            vec3 localUpDirection,\n            vec3 localViewDirection,\n            vec3 localLightDirection,\n            vec3 localHalfDirection,\n            float alphaSquared,\n            vec3 specularBaseColor\n        ) {\n            vec3 luminance = softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection);\n            float dotVH = positiveDotProduct(localViewDirection, localHalfDirection);\n            float dotNL = localLightDirection.z;\n            return luminance * (fresnelColor(specularBaseColor, dotVH) * g1(dotNL, alphaSquared));\n        }\n        \n        vec3 softLighting(\n            vec3 normalDirection,\n            vec3 diffuseBaseColor,\n            vec3 specularBaseColor,\n            float alpha,\n            vec3 directionToCamera,\n            vec3 viewY,\n            vec4 xyz_type,\n            vec4 rgb_parameter\n        ) {\n            float alphaSquared = alpha * alpha;\n            vec3 upDirection = xyz_type.xyz;\n            vec3 luminanceAbove = rgb_parameter.rgb;\n            vec3 luminanceBelow = rgb_parameter.a * luminanceAbove;\n            vec3 crossProduct = cross(normalDirection, directionToCamera);\n            float crossMagnitude = length(crossProduct);\n            vec3 xDirection = vec3(0.0, 0.0, 0.0);\n            vec3 yDirection = vec3(0.0, 0.0, 0.0);\n            if (crossMagnitude > 1.0e-6) {\n                yDirection = (1.0 / crossMagnitude) * crossProduct;\n                xDirection = cross(yDirection, normalDirection);\n            } else {\n                vec3 viewY = vec3(viewMatrix[0][1], viewMatrix[1][1], viewMatrix[2][1]);\n                xDirection = normalize(cross(viewY, normalDirection));\n                yDirection = cross(normalDirection, xDirection);\n            }\n            float localViewX = dot(directionToCamera, xDirection);\n            float localViewZ = dot(directionToCamera, normalDirection);\n            vec3 localViewDirection = vec3(localViewX, 0, localViewZ);\n            float localUpX = dot(upDirection, xDirection);\n            float localUpY = dot(upDirection, yDirection);\n            float localUpZ = dot(upDirection, normalDirection);\n            vec3 localUpDirection = vec3(localUpX, localUpY, localUpZ);\n        \n            vec3 vH = normalize(vec3(alpha * localViewX, 0.0, localViewZ));\n            vec3 vT1 = vec3(0.0, 1.0, 0.0);\n            vec3 vT2 = cross(vH, vT1);\n            float s = 0.5 * (1.0 + vH.z);\n            \n            vec3 localHalfDirection = sampleFacetNormal(vH, vT1, vT2, s, alpha);\n            vec3 localLightDirection = vec3(0.0, 0.0, 0.0);\n            \n            localLightDirection = -reflect(localViewDirection, localHalfDirection);\n            vec3 specular = softLightingSpecularSample(luminanceAbove, luminanceBelow, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);\n            \n            localLightDirection = vec3(0.000000, 0.000000, 1.000000);\n            vec3 diffuse = softLightingLuminance(luminanceAbove, luminanceBelow, localUpDirection, localLightDirection) * localLightDirection.z;\n            \n            return specular + diffuse * diffuseBaseColor;\n        }\n        \n        vec3 physicalLight(\n            vec4 xyz_type,\n            vec4 rgb_parameter,\n            vec3 surfacePosition,\n            vec3 normalDirection,\n            vec3 directionToCamera,\n            vec3 viewY,\n            float dotNV,\n            vec3 diffuseBaseColor,\n            vec3 specularBaseColor,\n            float alpha\n        ) {\n            float lightType = xyz_type.w;\n            if (lightType == kDisabledLight) {\n                return vec3(0.0, 0.0, 0.0);\n            } else if (lightType == kSoftLighting) {\n                return softLighting(normalDirection, diffuseBaseColor, specularBaseColor, alpha, directionToCamera, viewY, xyz_type, rgb_parameter);\n            }\n        \n            vec3 directionToLight = vec3(0.0, 0.0, 0.0);\n            vec3 normalIlluminance = vec3(0.0, 0.0, 0.0);\n            getDirectionToLightAndNormalIlluminance(xyz_type, rgb_parameter, surfacePosition, directionToLight, normalIlluminance);\n        \n            float dotNL = positiveDotProduct(normalDirection, directionToLight);\n            vec3 specularColor = brdf(normalDirection, directionToCamera, directionToLight, alpha, dotNV, dotNL, specularBaseColor, normalIlluminance);\n            return (normalIlluminance * dotNL) * ((diffuseBaseColor / kPi) + specularColor);\n        }\n        \n        vec3 physicalLighting(\n            vec3 surfacePosition,\n            vec3 surfaceNormal,\n            vec3 baseColor,\n            vec3 directionToCamera,\n            mat4 viewMatrix,\n            float roughness,\n            float metallic,\n            mat4 lights12,\n            mat4 lights34,\n            mat4 lights56,\n            mat4 lights78,\n            vec4 enabledLights\n        ) {\n            float dotNV = positiveDotProduct(surfaceNormal, directionToCamera);\n            float alpha = roughness * roughness;\n            float nonmetallic = 1.0 - metallic;\n            vec3 diffuseBaseColor = nonmetallic * 0.96 * baseColor;\n            vec3 specularBaseColor = nonmetallic * 0.04 * vec3(1.0, 1.0, 1.0) + metallic * baseColor;\n            vec3 viewY = vec3(viewMatrix[0][1], viewMatrix[1][1], viewMatrix[2][1]);\n        \n            vec3 litColor1 = enabledLights[0] == 1.0 ? physicalLight(lights12[0], lights12[1], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor2 = enabledLights[1] == 1.0 ? physicalLight(lights12[2], lights12[3], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor3 = enabledLights[2] == 1.0 ? physicalLight(lights34[0], lights34[1], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor4 = enabledLights[3] == 1.0 ? physicalLight(lights34[2], lights34[3], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor5 = physicalLight(lights56[0], lights56[1], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha);\n            vec3 litColor6 = physicalLight(lights56[2], lights56[3], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha);\n            vec3 litColor7 = physicalLight(lights78[0], lights78[1], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha);\n            vec3 litColor8 = physicalLight(lights78[2], lights78[3], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha);\n            return litColor1 + litColor2 + litColor3 + litColor4 + litColor5 + litColor6 + litColor7 + litColor8;\n        }\n        \n        float inverseGamma(float u) {\n            if (u <= 0.04045) {\n                return clamp(u / 12.92, 0.0, 1.0);\n            } else {\n                return clamp(pow((u + 0.055) / 1.055, 2.4), 0.0, 1.0);\n            }\n        }\n        \n        vec3 fromSrgb(vec3 srgbColor) {\n            return vec3(\n                inverseGamma(srgbColor.r),\n                inverseGamma(srgbColor.g),\n                inverseGamma(srgbColor.b)\n            );\n        }\n        \n        float gammaCorrect(float u) {\n            if (u <= 0.0031308) {\n                return 12.92 * u;\n            } else {\n                return 1.055 * pow(u, 1.0 / 2.4) - 0.055;\n            }\n        }\n        \n        vec3 gammaCorrectedColor(vec3 color) {\n            float red = gammaCorrect(color.r);\n            float green = gammaCorrect(color.g);\n            float blue = gammaCorrect(color.b);\n            return vec3(red, green, blue);\n        }\n        \n        vec3 reinhardLuminanceToneMap(vec3 color) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scale = 1.0 / (1.0 + luminance);\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 reinhardPerChannelToneMap(vec3 color) {\n            return gammaCorrectedColor(color / (color + 1.0));\n        }\n        \n        float extendedReinhardToneMap(float x, float xMax) {\n            return x * (1.0 + (x / (xMax * xMax))) / (1.0 + x);\n        }\n        \n        vec3 extendedReinhardLuminanceToneMap(vec3 color, float overexposureLimit) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scaledLuminance = extendedReinhardToneMap(luminance, overexposureLimit);\n            float scale = scaledLuminance / luminance;\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 extendedReinhardPerChannelToneMap(vec3 color, float overexposureLimit) {\n            float red = extendedReinhardToneMap(color.r, overexposureLimit);\n            float green = extendedReinhardToneMap(color.g, overexposureLimit);\n            float blue = extendedReinhardToneMap(color.b, overexposureLimit);\n            return gammaCorrectedColor(vec3(red, green, blue));\n        }\n        \n        vec3 hableFilmicHelper(vec3 color) {\n            float a = 0.15;\n            float b = 0.5;\n            float c = 0.1;\n            float d = 0.2;\n            float e = 0.02;\n            float f = 0.3;\n            return (color * (a * color + c * b) + d * e) / (color * (a * color + b) + d * f) - e / f;\n        }\n        \n        vec3 hableFilmicToneMap(vec3 color) {\n            float exposureBias = 2.0;\n            vec3 unscaled = hableFilmicHelper(exposureBias * color);\n            vec3 scale = 1.0 / hableFilmicHelper(vec3(11.2));\n            return gammaCorrectedColor(scale * unscaled);\n        }\n        \n        vec3 toneMap(vec3 color, float toneMapType, float toneMapParam) {\n            if (toneMapType == 0.0) {\n                return gammaCorrectedColor(color);\n            } else if (toneMapType == 1.0) {\n                return reinhardLuminanceToneMap(color);\n            } else if (toneMapType == 2.0) {\n                return reinhardPerChannelToneMap(color);\n            } else if (toneMapType == 3.0) {\n                return extendedReinhardLuminanceToneMap(color, toneMapParam);\n            } else if (toneMapType == 4.0) {\n                return extendedReinhardPerChannelToneMap(color, toneMapParam);\n            } else if (toneMapType == 5.0) {\n                return hableFilmicToneMap(color);\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        vec4 toSrgb(vec3 linearColor, mat4 sceneProperties) {\n            vec3 referenceWhite = sceneProperties[2].rgb;\n            float unitR = linearColor.r / referenceWhite.r;\n            float unitG = linearColor.g / referenceWhite.g;\n            float unitB = linearColor.b / referenceWhite.b;\n            float toneMapType = sceneProperties[3][2];\n            float toneMapParam = sceneProperties[3][3];\n            vec3 toneMapped = toneMap(vec3(unitR, unitG, unitB), toneMapType, toneMapParam);\n            return vec4(toneMapped, 1.0);\n        }\n        \n        void main() {\n            vec3 baseColor = fromSrgb(texture2D(baseColorTexture, interpolatedUv).rgb) * (1.0 - constantBaseColor.w) + constantBaseColor.rgb * constantBaseColor.w;\n            float roughness = getFloatValue(roughnessTexture, interpolatedUv, constantRoughness);\n            float metallic = getFloatValue(metallicTexture, interpolatedUv, constantMetallic);\n        \n            vec3 localNormal = getLocalNormal(normalMapTexture, useNormalMap, interpolatedUv);\n            float normalSign = getNormalSign();\n            vec3 originalNormal = normalize(interpolatedNormal) * normalSign;\n            vec3 normalDirection = getMappedNormal(originalNormal, interpolatedTangent, normalSign, localNormal);\n            vec3 directionToCamera = getDirectionToCamera(interpolatedPosition, sceneProperties);\n        \n            vec3 linearColor = physicalLighting(\n                interpolatedPosition,\n                normalDirection,\n                baseColor,\n                directionToCamera,\n                viewMatrix,\n                roughness,\n                metallic,\n                lights12,\n                lights34,\n                lights56,\n                lights78,\n                enabledLights\n            );\n        \n            gl_FragColor = toSrgb(linearColor, sceneProperties);\n        }\n    ',
	attributes: {},
	uniforms: {baseColorTexture: 'baseColorTexture', constantBaseColor: 'constantBaseColor', constantMetallic: 'constantMetallic', constantRoughness: 'constantRoughness', enabledLights: 'enabledLights', lights12: 'lights12', lights34: 'lights34', lights56: 'lights56', lights78: 'lights78', metallicTexture: 'metallicTexture', normalMapTexture: 'normalMapTexture', roughnessTexture: 'roughnessTexture', sceneProperties: 'sceneProperties', useNormalMap: 'useNormalMap', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$normalMappedPhysicalMesh = function (baseColorData) {
	return function (constantBaseColor) {
		return function (roughnessData) {
			return function (constantRoughness) {
				return function (metallicData) {
					return function (constantMetallic) {
						return function (normalMapData) {
							return function (useNormalMap) {
								return function (bounds) {
									return function (webGLMesh) {
										return function (backFaceSetting) {
											return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
												A2(
													$ianmackenzie$elm_3d_scene$Scene3d$Types$MeshNode,
													bounds,
													F8(
														function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, _v0, settings) {
															var lights = _v0.a;
															var enabledLights = _v0.b;
															return A5(
																$elm_explorations$webgl$WebGL$entityWith,
																A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, backFaceSetting, settings),
																$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$normalMappedVertex,
																$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$physicalTexturesFragment,
																webGLMesh,
																{baseColorTexture: baseColorData, constantBaseColor: constantBaseColor, constantMetallic: constantMetallic, constantRoughness: constantRoughness, enabledLights: enabledLights, lights12: lights.lights12, lights34: lights.lights34, lights56: lights.lights56, lights78: lights.lights78, metallicTexture: metallicData, modelMatrix: modelMatrix, modelScale: modelScale, normalMapTexture: normalMapData, projectionMatrix: projectionMatrix, roughnessTexture: roughnessData, sceneProperties: sceneProperties, useNormalMap: useNormalMap, viewMatrix: viewMatrix});
														})));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$physicalFragment = {
	src: '\n        precision highp float;\n        \n        uniform highp mat4 sceneProperties;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 lights12;\n        uniform highp mat4 lights34;\n        uniform highp mat4 lights56;\n        uniform highp mat4 lights78;\n        uniform lowp vec4 enabledLights;\n        uniform lowp vec3 baseColor;\n        uniform lowp float roughness;\n        uniform lowp float metallic;\n        \n        varying highp vec3 interpolatedPosition;\n        varying highp vec3 interpolatedNormal;\n        \n        const lowp float kPerspectiveProjection = 0.0;\n        const lowp float kOrthographicProjection = 1.0;\n        const lowp float kDirectionalLight = 1.0;\n        const lowp float kPointLight = 2.0;\n        const highp float kPi = 3.14159265359;\n        const mediump float kMediumpFloatMax = 65504.0;\n        const lowp float kDisabledLight = 0.0;\n        const lowp float kSoftLighting = 3.0;\n        \n        float getNormalSign() {\n            return 2.0 * float(gl_FrontFacing) - 1.0;\n        }\n        \n        vec3 getDirectionToCamera(vec3 surfacePosition, mat4 sceneProperties) {\n            float projectionType = sceneProperties[1].w;\n            if (projectionType == kPerspectiveProjection) {\n                vec3 cameraPoint = sceneProperties[1].xyz;\n                return normalize(cameraPoint - surfacePosition);\n            } else if (projectionType == kOrthographicProjection) {\n                return sceneProperties[1].xyz;\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        void getDirectionToLightAndNormalIlluminance(\n            vec4 xyz_type,\n            vec4 rgb_parameter,\n            vec3 surfacePosition,\n            out vec3 directionToLight,\n            out vec3 normalIlluminance\n        ) {\n            float lightType = xyz_type.w;\n            if (lightType == kDirectionalLight) {\n                directionToLight = xyz_type.xyz;\n                normalIlluminance = rgb_parameter.rgb;\n            } else if (lightType == kPointLight) {\n                vec3 lightPosition = xyz_type.xyz;\n                vec3 displacement = lightPosition - surfacePosition;\n                float distance = length(displacement);\n                directionToLight = displacement / distance;\n                normalIlluminance = rgb_parameter.rgb / (4.0 * kPi * distance * distance);\n            }\n        }\n        \n        float positiveDotProduct(vec3 v1, vec3 v2) {\n            return clamp(dot(v1, v2), 0.0, 1.0);\n        }\n        \n        // Adapted from https://google.github.io/filament/Filament.md.html#materialsystem/specularbrdf/normaldistributionfunction(speculard)\n        float specularD(float alpha, float dotNH, vec3 normalDirection, vec3 halfDirection) {\n            vec3 crossNH = cross(normalDirection, halfDirection);\n            float a = dotNH * alpha;\n            float k = alpha / (dot(crossNH, crossNH) + a * a);\n            float d = k * k * (1.0 / kPi);\n            return min(d, kMediumpFloatMax);\n        }\n        \n        float safeQuotient(float numerator, float denominator) {\n            if (denominator == 0.0) {\n                return 0.0;\n            } else {\n                return numerator / denominator;\n            }\n        }\n        \n        float g1(float dotNV, float alphaSquared) {\n            return safeQuotient(2.0 * dotNV, dotNV + sqrt(alphaSquared + (1.0 - alphaSquared) * dotNV * dotNV));\n        }\n        \n        float specularG(float dotNL, float dotNV, float alphaSquared) {\n            return g1(dotNV, alphaSquared) * g1(dotNL, alphaSquared);\n        }\n        \n        vec3 fresnelColor(vec3 specularBaseColor, float dotVH) {\n            vec3 one = vec3(1.0, 1.0, 1.0);\n            float scale = exp2((-5.55473 * dotVH - 6.98316) * dotVH);\n            return specularBaseColor + (one - specularBaseColor) * scale;\n        }\n        \n        vec3 brdf(vec3 normalDirection, vec3 directionToCamera, vec3 directionToLight, float alpha, float dotNV, float dotNL, vec3 specularBaseColor, vec3 normalIlluminance) {\n            vec3 halfDirection = normalize(directionToCamera + directionToLight);\n            float dotVH = positiveDotProduct(directionToCamera, halfDirection);\n            float dotNH = positiveDotProduct(normalDirection, halfDirection);\n            float dotNHSquared = dotNH * dotNH;\n        \n            float d = specularD(alpha, dotNH, normalDirection, halfDirection);\n            float g = specularG(dotNL, dotNV, alpha * alpha);\n            vec3 f = fresnelColor(specularBaseColor, dotVH);\n            return safeQuotient(d * g, 4.0 * dotNL * dotNV) * f;\n        }\n        \n        vec3 sampleFacetNormal(vec3 vH, vec3 vT1, vec3 vT2, float s, float alpha) {\n            float t2 = (1.0 - s);\n            vec3 vNh = t2 * vT2 + sqrt(max(0.0, 1.0 - t2 * t2)) * vH;\n            return normalize(vec3(alpha * vNh.x, alpha * vNh.y, max(0.0, vNh.z)));\n        }\n        \n        vec3 softLightingLuminance(\n            vec3 aboveLuminance,\n            vec3 belowLuminance,\n            vec3 localUpDirection,\n            vec3 localLightDirection\n        ) {\n            float sinElevation = dot(localLightDirection, localUpDirection);\n            float t = (sinElevation + 1.0) / 2.0;\n            return aboveLuminance * t + belowLuminance * (1.0 - t);\n        }\n        \n        vec3 softLightingSpecularSample(\n            vec3 aboveLuminance,\n            vec3 belowLuminance,\n            vec3 localUpDirection,\n            vec3 localViewDirection,\n            vec3 localLightDirection,\n            vec3 localHalfDirection,\n            float alphaSquared,\n            vec3 specularBaseColor\n        ) {\n            vec3 luminance = softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection);\n            float dotVH = positiveDotProduct(localViewDirection, localHalfDirection);\n            float dotNL = localLightDirection.z;\n            return luminance * (fresnelColor(specularBaseColor, dotVH) * g1(dotNL, alphaSquared));\n        }\n        \n        vec3 softLighting(\n            vec3 normalDirection,\n            vec3 diffuseBaseColor,\n            vec3 specularBaseColor,\n            float alpha,\n            vec3 directionToCamera,\n            vec3 viewY,\n            vec4 xyz_type,\n            vec4 rgb_parameter\n        ) {\n            float alphaSquared = alpha * alpha;\n            vec3 upDirection = xyz_type.xyz;\n            vec3 luminanceAbove = rgb_parameter.rgb;\n            vec3 luminanceBelow = rgb_parameter.a * luminanceAbove;\n            vec3 crossProduct = cross(normalDirection, directionToCamera);\n            float crossMagnitude = length(crossProduct);\n            vec3 xDirection = vec3(0.0, 0.0, 0.0);\n            vec3 yDirection = vec3(0.0, 0.0, 0.0);\n            if (crossMagnitude > 1.0e-6) {\n                yDirection = (1.0 / crossMagnitude) * crossProduct;\n                xDirection = cross(yDirection, normalDirection);\n            } else {\n                vec3 viewY = vec3(viewMatrix[0][1], viewMatrix[1][1], viewMatrix[2][1]);\n                xDirection = normalize(cross(viewY, normalDirection));\n                yDirection = cross(normalDirection, xDirection);\n            }\n            float localViewX = dot(directionToCamera, xDirection);\n            float localViewZ = dot(directionToCamera, normalDirection);\n            vec3 localViewDirection = vec3(localViewX, 0, localViewZ);\n            float localUpX = dot(upDirection, xDirection);\n            float localUpY = dot(upDirection, yDirection);\n            float localUpZ = dot(upDirection, normalDirection);\n            vec3 localUpDirection = vec3(localUpX, localUpY, localUpZ);\n        \n            vec3 vH = normalize(vec3(alpha * localViewX, 0.0, localViewZ));\n            vec3 vT1 = vec3(0.0, 1.0, 0.0);\n            vec3 vT2 = cross(vH, vT1);\n            float s = 0.5 * (1.0 + vH.z);\n            \n            vec3 localHalfDirection = sampleFacetNormal(vH, vT1, vT2, s, alpha);\n            vec3 localLightDirection = vec3(0.0, 0.0, 0.0);\n            \n            localLightDirection = -reflect(localViewDirection, localHalfDirection);\n            vec3 specular = softLightingSpecularSample(luminanceAbove, luminanceBelow, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);\n            \n            localLightDirection = vec3(0.000000, 0.000000, 1.000000);\n            vec3 diffuse = softLightingLuminance(luminanceAbove, luminanceBelow, localUpDirection, localLightDirection) * localLightDirection.z;\n            \n            return specular + diffuse * diffuseBaseColor;\n        }\n        \n        vec3 physicalLight(\n            vec4 xyz_type,\n            vec4 rgb_parameter,\n            vec3 surfacePosition,\n            vec3 normalDirection,\n            vec3 directionToCamera,\n            vec3 viewY,\n            float dotNV,\n            vec3 diffuseBaseColor,\n            vec3 specularBaseColor,\n            float alpha\n        ) {\n            float lightType = xyz_type.w;\n            if (lightType == kDisabledLight) {\n                return vec3(0.0, 0.0, 0.0);\n            } else if (lightType == kSoftLighting) {\n                return softLighting(normalDirection, diffuseBaseColor, specularBaseColor, alpha, directionToCamera, viewY, xyz_type, rgb_parameter);\n            }\n        \n            vec3 directionToLight = vec3(0.0, 0.0, 0.0);\n            vec3 normalIlluminance = vec3(0.0, 0.0, 0.0);\n            getDirectionToLightAndNormalIlluminance(xyz_type, rgb_parameter, surfacePosition, directionToLight, normalIlluminance);\n        \n            float dotNL = positiveDotProduct(normalDirection, directionToLight);\n            vec3 specularColor = brdf(normalDirection, directionToCamera, directionToLight, alpha, dotNV, dotNL, specularBaseColor, normalIlluminance);\n            return (normalIlluminance * dotNL) * ((diffuseBaseColor / kPi) + specularColor);\n        }\n        \n        vec3 physicalLighting(\n            vec3 surfacePosition,\n            vec3 surfaceNormal,\n            vec3 baseColor,\n            vec3 directionToCamera,\n            mat4 viewMatrix,\n            float roughness,\n            float metallic,\n            mat4 lights12,\n            mat4 lights34,\n            mat4 lights56,\n            mat4 lights78,\n            vec4 enabledLights\n        ) {\n            float dotNV = positiveDotProduct(surfaceNormal, directionToCamera);\n            float alpha = roughness * roughness;\n            float nonmetallic = 1.0 - metallic;\n            vec3 diffuseBaseColor = nonmetallic * 0.96 * baseColor;\n            vec3 specularBaseColor = nonmetallic * 0.04 * vec3(1.0, 1.0, 1.0) + metallic * baseColor;\n            vec3 viewY = vec3(viewMatrix[0][1], viewMatrix[1][1], viewMatrix[2][1]);\n        \n            vec3 litColor1 = enabledLights[0] == 1.0 ? physicalLight(lights12[0], lights12[1], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor2 = enabledLights[1] == 1.0 ? physicalLight(lights12[2], lights12[3], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor3 = enabledLights[2] == 1.0 ? physicalLight(lights34[0], lights34[1], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor4 = enabledLights[3] == 1.0 ? physicalLight(lights34[2], lights34[3], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha) : vec3(0.0, 0.0, 0.0);\n            vec3 litColor5 = physicalLight(lights56[0], lights56[1], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha);\n            vec3 litColor6 = physicalLight(lights56[2], lights56[3], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha);\n            vec3 litColor7 = physicalLight(lights78[0], lights78[1], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha);\n            vec3 litColor8 = physicalLight(lights78[2], lights78[3], surfacePosition, surfaceNormal, directionToCamera, viewY, dotNV, diffuseBaseColor, specularBaseColor, alpha);\n            return litColor1 + litColor2 + litColor3 + litColor4 + litColor5 + litColor6 + litColor7 + litColor8;\n        }\n        \n        float gammaCorrect(float u) {\n            if (u <= 0.0031308) {\n                return 12.92 * u;\n            } else {\n                return 1.055 * pow(u, 1.0 / 2.4) - 0.055;\n            }\n        }\n        \n        vec3 gammaCorrectedColor(vec3 color) {\n            float red = gammaCorrect(color.r);\n            float green = gammaCorrect(color.g);\n            float blue = gammaCorrect(color.b);\n            return vec3(red, green, blue);\n        }\n        \n        vec3 reinhardLuminanceToneMap(vec3 color) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scale = 1.0 / (1.0 + luminance);\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 reinhardPerChannelToneMap(vec3 color) {\n            return gammaCorrectedColor(color / (color + 1.0));\n        }\n        \n        float extendedReinhardToneMap(float x, float xMax) {\n            return x * (1.0 + (x / (xMax * xMax))) / (1.0 + x);\n        }\n        \n        vec3 extendedReinhardLuminanceToneMap(vec3 color, float overexposureLimit) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scaledLuminance = extendedReinhardToneMap(luminance, overexposureLimit);\n            float scale = scaledLuminance / luminance;\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 extendedReinhardPerChannelToneMap(vec3 color, float overexposureLimit) {\n            float red = extendedReinhardToneMap(color.r, overexposureLimit);\n            float green = extendedReinhardToneMap(color.g, overexposureLimit);\n            float blue = extendedReinhardToneMap(color.b, overexposureLimit);\n            return gammaCorrectedColor(vec3(red, green, blue));\n        }\n        \n        vec3 hableFilmicHelper(vec3 color) {\n            float a = 0.15;\n            float b = 0.5;\n            float c = 0.1;\n            float d = 0.2;\n            float e = 0.02;\n            float f = 0.3;\n            return (color * (a * color + c * b) + d * e) / (color * (a * color + b) + d * f) - e / f;\n        }\n        \n        vec3 hableFilmicToneMap(vec3 color) {\n            float exposureBias = 2.0;\n            vec3 unscaled = hableFilmicHelper(exposureBias * color);\n            vec3 scale = 1.0 / hableFilmicHelper(vec3(11.2));\n            return gammaCorrectedColor(scale * unscaled);\n        }\n        \n        vec3 toneMap(vec3 color, float toneMapType, float toneMapParam) {\n            if (toneMapType == 0.0) {\n                return gammaCorrectedColor(color);\n            } else if (toneMapType == 1.0) {\n                return reinhardLuminanceToneMap(color);\n            } else if (toneMapType == 2.0) {\n                return reinhardPerChannelToneMap(color);\n            } else if (toneMapType == 3.0) {\n                return extendedReinhardLuminanceToneMap(color, toneMapParam);\n            } else if (toneMapType == 4.0) {\n                return extendedReinhardPerChannelToneMap(color, toneMapParam);\n            } else if (toneMapType == 5.0) {\n                return hableFilmicToneMap(color);\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        vec4 toSrgb(vec3 linearColor, mat4 sceneProperties) {\n            vec3 referenceWhite = sceneProperties[2].rgb;\n            float unitR = linearColor.r / referenceWhite.r;\n            float unitG = linearColor.g / referenceWhite.g;\n            float unitB = linearColor.b / referenceWhite.b;\n            float toneMapType = sceneProperties[3][2];\n            float toneMapParam = sceneProperties[3][3];\n            vec3 toneMapped = toneMap(vec3(unitR, unitG, unitB), toneMapType, toneMapParam);\n            return vec4(toneMapped, 1.0);\n        }\n        \n        void main() {\n            vec3 normalDirection = normalize(interpolatedNormal) * getNormalSign();\n            vec3 directionToCamera = getDirectionToCamera(interpolatedPosition, sceneProperties);\n        \n            vec3 linearColor = physicalLighting(\n                interpolatedPosition,\n                normalDirection,\n                baseColor,\n                directionToCamera,\n                viewMatrix,\n                roughness,\n                metallic,\n                lights12,\n                lights34,\n                lights56,\n                lights78,\n                enabledLights\n            );\n        \n            gl_FragColor = toSrgb(linearColor, sceneProperties);\n        }\n    ',
	attributes: {},
	uniforms: {baseColor: 'baseColor', enabledLights: 'enabledLights', lights12: 'lights12', lights34: 'lights34', lights56: 'lights56', lights78: 'lights78', metallic: 'metallic', roughness: 'roughness', sceneProperties: 'sceneProperties', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$physicalMesh = F6(
	function (color, roughness, metallic, bounds, webGLMesh, backFaceSetting) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Types$MeshNode,
				bounds,
				F8(
					function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, _v0, settings) {
						var lights = _v0.a;
						var enabledLights = _v0.b;
						return A5(
							$elm_explorations$webgl$WebGL$entityWith,
							A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, backFaceSetting, settings),
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$uniformVertex,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$physicalFragment,
							webGLMesh,
							{baseColor: color, enabledLights: enabledLights, lights12: lights.lights12, lights34: lights.lights34, lights56: lights.lights56, lights78: lights.lights78, metallic: metallic, modelMatrix: modelMatrix, modelScale: modelScale, projectionMatrix: projectionMatrix, roughness: roughness, sceneProperties: sceneProperties, viewMatrix: viewMatrix});
					})));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$ConstantLambertianMaterial = function (a) {
	return {$: 'ConstantLambertianMaterial', a: a};
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$TexturedLambertianMaterial = F2(
	function (a, b) {
		return {$: 'TexturedLambertianMaterial', a: a, b: b};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$normalMapTuple = F2(
	function (fallbackData, channel) {
		if (channel.$ === 'Constant') {
			var _v1 = channel.a;
			return _Utils_Tuple2(fallbackData, 0.0);
		} else {
			var data = channel.a.data;
			return _Utils_Tuple2(data, 1.0);
		}
	});
var $elm_explorations$linear_algebra$Math$Vector3$getX = _MJS_v3getX;
var $elm_explorations$linear_algebra$Math$Vector3$getY = _MJS_v3getY;
var $elm_explorations$linear_algebra$Math$Vector3$getZ = _MJS_v3getZ;
var $elm_explorations$linear_algebra$Math$Vector4$vec4 = _MJS_v4;
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$enabledVec3 = function (vector) {
	return A4(
		$elm_explorations$linear_algebra$Math$Vector4$vec4,
		$elm_explorations$linear_algebra$Math$Vector3$getX(vector),
		$elm_explorations$linear_algebra$Math$Vector3$getY(vector),
		$elm_explorations$linear_algebra$Math$Vector3$getZ(vector),
		1);
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$zeroVec4 = A4($elm_explorations$linear_algebra$Math$Vector4$vec4, 0, 0, 0, 0);
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$vec3Tuple = F2(
	function (fallbackData, texture) {
		if (texture.$ === 'Constant') {
			var baseColor = texture.a.a;
			return _Utils_Tuple2(
				fallbackData,
				$ianmackenzie$elm_3d_scene$Scene3d$Entity$enabledVec3(baseColor));
		} else {
			var data = texture.a.data;
			return _Utils_Tuple2(data, $ianmackenzie$elm_3d_scene$Scene3d$Entity$zeroVec4);
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$resolveLambertian = F2(
	function (materialColorTexture, normalMapTexture) {
		var _v0 = _Utils_Tuple2(materialColorTexture, normalMapTexture);
		if (_v0.a.$ === 'Constant') {
			if (_v0.b.$ === 'Constant') {
				var materialColor = _v0.a.a;
				var _v1 = _v0.b.a;
				return $ianmackenzie$elm_3d_scene$Scene3d$Entity$ConstantLambertianMaterial(materialColor);
			} else {
				var data = _v0.b.a.data;
				return A2(
					$ianmackenzie$elm_3d_scene$Scene3d$Entity$TexturedLambertianMaterial,
					A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$vec3Tuple, data, materialColorTexture),
					A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$normalMapTuple, data, normalMapTexture));
			}
		} else {
			var data = _v0.a.a.data;
			return A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Entity$TexturedLambertianMaterial,
				_Utils_Tuple2(data, $ianmackenzie$elm_3d_scene$Scene3d$Entity$zeroVec4),
				A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$normalMapTuple, data, normalMapTexture));
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$ConstantPbrMaterial = F3(
	function (a, b, c) {
		return {$: 'ConstantPbrMaterial', a: a, b: b, c: c};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$TexturedPbrMaterial = F4(
	function (a, b, c, d) {
		return {$: 'TexturedPbrMaterial', a: a, b: b, c: c, d: d};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$Tuple4 = F4(
	function (a, b, c, d) {
		return {$: 'Tuple4', a: a, b: b, c: c, d: d};
	});
var $elm_explorations$linear_algebra$Math$Vector2$vec2 = _MJS_v2;
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$enabledFloat = function (value) {
	return A2($elm_explorations$linear_algebra$Math$Vector2$vec2, value, 1);
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$zeroVec2 = A2($elm_explorations$linear_algebra$Math$Vector2$vec2, 0, 0);
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$floatTuple = F2(
	function (fallbackData, texture) {
		if (texture.$ === 'Constant') {
			var value = texture.a;
			return _Utils_Tuple2(
				fallbackData,
				$ianmackenzie$elm_3d_scene$Scene3d$Entity$enabledFloat(value));
		} else {
			var data = texture.a.data;
			return _Utils_Tuple2(data, $ianmackenzie$elm_3d_scene$Scene3d$Entity$zeroVec2);
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$resolvePbr = F4(
	function (baseColorTexture, roughnessTexture, metallicTexture, normalMapTexture) {
		var _v0 = A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$Tuple4, baseColorTexture, roughnessTexture, metallicTexture, normalMapTexture);
		if (_v0.a.$ === 'Constant') {
			if (_v0.b.$ === 'Constant') {
				if (_v0.c.$ === 'Constant') {
					if (_v0.d.$ === 'Constant') {
						var baseColor = _v0.a.a;
						var roughness = _v0.b.a;
						var metallic = _v0.c.a;
						var _v1 = _v0.d.a;
						return A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$ConstantPbrMaterial, baseColor, roughness, metallic);
					} else {
						var data = _v0.d.a.data;
						return A4(
							$ianmackenzie$elm_3d_scene$Scene3d$Entity$TexturedPbrMaterial,
							A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$vec3Tuple, data, baseColorTexture),
							A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$floatTuple, data, roughnessTexture),
							A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$floatTuple, data, metallicTexture),
							_Utils_Tuple2(data, 1.0));
					}
				} else {
					var data = _v0.c.a.data;
					return A4(
						$ianmackenzie$elm_3d_scene$Scene3d$Entity$TexturedPbrMaterial,
						A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$vec3Tuple, data, baseColorTexture),
						A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$floatTuple, data, roughnessTexture),
						_Utils_Tuple2(data, $ianmackenzie$elm_3d_scene$Scene3d$Entity$zeroVec2),
						A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$normalMapTuple, data, normalMapTexture));
				}
			} else {
				var data = _v0.b.a.data;
				return A4(
					$ianmackenzie$elm_3d_scene$Scene3d$Entity$TexturedPbrMaterial,
					A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$vec3Tuple, data, baseColorTexture),
					_Utils_Tuple2(data, $ianmackenzie$elm_3d_scene$Scene3d$Entity$zeroVec2),
					A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$floatTuple, data, metallicTexture),
					A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$normalMapTuple, data, normalMapTexture));
			}
		} else {
			var data = _v0.a.a.data;
			return A4(
				$ianmackenzie$elm_3d_scene$Scene3d$Entity$TexturedPbrMaterial,
				_Utils_Tuple2(data, $ianmackenzie$elm_3d_scene$Scene3d$Entity$zeroVec4),
				A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$floatTuple, data, roughnessTexture),
				A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$floatTuple, data, metallicTexture),
				A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$normalMapTuple, data, normalMapTexture));
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$emissiveTextureFragment = {
	src: '\n        precision mediump float;\n        \n        uniform mediump sampler2D colorTexture;\n        uniform mediump float backlight;\n        uniform highp mat4 sceneProperties;\n        \n        varying mediump vec2 interpolatedUv;\n        \n        float inverseGamma(float u) {\n            if (u <= 0.04045) {\n                return clamp(u / 12.92, 0.0, 1.0);\n            } else {\n                return clamp(pow((u + 0.055) / 1.055, 2.4), 0.0, 1.0);\n            }\n        }\n        \n        vec3 fromSrgb(vec3 srgbColor) {\n            return vec3(\n                inverseGamma(srgbColor.r),\n                inverseGamma(srgbColor.g),\n                inverseGamma(srgbColor.b)\n            );\n        }\n        \n        float gammaCorrect(float u) {\n            if (u <= 0.0031308) {\n                return 12.92 * u;\n            } else {\n                return 1.055 * pow(u, 1.0 / 2.4) - 0.055;\n            }\n        }\n        \n        vec3 gammaCorrectedColor(vec3 color) {\n            float red = gammaCorrect(color.r);\n            float green = gammaCorrect(color.g);\n            float blue = gammaCorrect(color.b);\n            return vec3(red, green, blue);\n        }\n        \n        vec3 reinhardLuminanceToneMap(vec3 color) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scale = 1.0 / (1.0 + luminance);\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 reinhardPerChannelToneMap(vec3 color) {\n            return gammaCorrectedColor(color / (color + 1.0));\n        }\n        \n        float extendedReinhardToneMap(float x, float xMax) {\n            return x * (1.0 + (x / (xMax * xMax))) / (1.0 + x);\n        }\n        \n        vec3 extendedReinhardLuminanceToneMap(vec3 color, float overexposureLimit) {\n            float luminance = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;\n            float scaledLuminance = extendedReinhardToneMap(luminance, overexposureLimit);\n            float scale = scaledLuminance / luminance;\n            return gammaCorrectedColor(color * scale);\n        }\n        \n        vec3 extendedReinhardPerChannelToneMap(vec3 color, float overexposureLimit) {\n            float red = extendedReinhardToneMap(color.r, overexposureLimit);\n            float green = extendedReinhardToneMap(color.g, overexposureLimit);\n            float blue = extendedReinhardToneMap(color.b, overexposureLimit);\n            return gammaCorrectedColor(vec3(red, green, blue));\n        }\n        \n        vec3 hableFilmicHelper(vec3 color) {\n            float a = 0.15;\n            float b = 0.5;\n            float c = 0.1;\n            float d = 0.2;\n            float e = 0.02;\n            float f = 0.3;\n            return (color * (a * color + c * b) + d * e) / (color * (a * color + b) + d * f) - e / f;\n        }\n        \n        vec3 hableFilmicToneMap(vec3 color) {\n            float exposureBias = 2.0;\n            vec3 unscaled = hableFilmicHelper(exposureBias * color);\n            vec3 scale = 1.0 / hableFilmicHelper(vec3(11.2));\n            return gammaCorrectedColor(scale * unscaled);\n        }\n        \n        vec3 toneMap(vec3 color, float toneMapType, float toneMapParam) {\n            if (toneMapType == 0.0) {\n                return gammaCorrectedColor(color);\n            } else if (toneMapType == 1.0) {\n                return reinhardLuminanceToneMap(color);\n            } else if (toneMapType == 2.0) {\n                return reinhardPerChannelToneMap(color);\n            } else if (toneMapType == 3.0) {\n                return extendedReinhardLuminanceToneMap(color, toneMapParam);\n            } else if (toneMapType == 4.0) {\n                return extendedReinhardPerChannelToneMap(color, toneMapParam);\n            } else if (toneMapType == 5.0) {\n                return hableFilmicToneMap(color);\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        vec4 toSrgb(vec3 linearColor, mat4 sceneProperties) {\n            vec3 referenceWhite = sceneProperties[2].rgb;\n            float unitR = linearColor.r / referenceWhite.r;\n            float unitG = linearColor.g / referenceWhite.g;\n            float unitB = linearColor.b / referenceWhite.b;\n            float toneMapType = sceneProperties[3][2];\n            float toneMapParam = sceneProperties[3][3];\n            vec3 toneMapped = toneMap(vec3(unitR, unitG, unitB), toneMapType, toneMapParam);\n            return vec4(toneMapped, 1.0);\n        }\n        \n        void main () {\n            vec3 emissiveColor = fromSrgb(texture2D(colorTexture, interpolatedUv).rgb) * backlight;\n            gl_FragColor = toSrgb(emissiveColor, sceneProperties);\n        }\n    ',
	attributes: {},
	uniforms: {backlight: 'backlight', colorTexture: 'colorTexture', sceneProperties: 'sceneProperties'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$texturedEmissiveMesh = F5(
	function (colorData, backlight, bounds, webGLMesh, backFaceSetting) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Types$MeshNode,
				bounds,
				F8(
					function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, lights, settings) {
						return A5(
							$elm_explorations$webgl$WebGL$entityWith,
							A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, backFaceSetting, settings),
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$unlitVertex,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$emissiveTextureFragment,
							webGLMesh,
							{
								backlight: $ianmackenzie$elm_units$Luminance$inNits(backlight),
								colorTexture: colorData,
								modelMatrix: modelMatrix,
								modelScale: modelScale,
								projectionMatrix: projectionMatrix,
								sceneProperties: sceneProperties,
								viewMatrix: viewMatrix
							});
					})));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$texturedVertex = {
	src: '\n        precision highp float;\n        \n        attribute highp vec3 position;\n        attribute highp vec3 normal;\n        attribute mediump vec2 uv;\n        \n        uniform highp vec4 modelScale;\n        uniform highp mat4 modelMatrix;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 projectionMatrix;\n        uniform highp mat4 sceneProperties;\n        \n        varying highp vec3 interpolatedPosition;\n        varying highp vec3 interpolatedNormal;\n        varying mediump vec2 interpolatedUv;\n        varying highp vec3 interpolatedTangent;\n        \n        vec4 getWorldPosition(vec3 modelPosition, vec4 modelScale, mat4 modelMatrix) {\n            vec4 scaledPosition = vec4(modelScale.xyz * modelPosition, 1.0);\n            return modelMatrix * scaledPosition;\n        }\n        \n        vec3 safeNormalize(vec3 vector) {\n            if (vector == vec3(0.0, 0.0, 0.0)) {\n                return vector;\n            } else {\n                return normalize(vector);\n            }\n        }\n        \n        vec3 getWorldNormal(vec3 modelNormal, vec4 modelScale, mat4 modelMatrix) {\n            vec3 normalScale = vec3(modelScale.w / modelScale.x, modelScale.w / modelScale.y, modelScale.w / modelScale.z);\n            return (modelMatrix * vec4(safeNormalize(normalScale * modelNormal), 0.0)).xyz;\n        }\n        \n        void main () {\n            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);\n            gl_Position = projectionMatrix * (viewMatrix * worldPosition);\n            interpolatedPosition = worldPosition.xyz;\n            interpolatedNormal = getWorldNormal(normal, modelScale, modelMatrix);\n            interpolatedUv = uv;\n            interpolatedTangent = vec3(0.0, 0.0, 0.0);\n        }\n    ',
	attributes: {normal: 'normal', position: 'position', uv: 'uv'},
	uniforms: {modelMatrix: 'modelMatrix', modelScale: 'modelScale', projectionMatrix: 'projectionMatrix', sceneProperties: 'sceneProperties', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$texturedLambertianMesh = F4(
	function (materialColorData, bounds, webGLMesh, backFaceSetting) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Types$MeshNode,
				bounds,
				F8(
					function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, _v0, settings) {
						var lights = _v0.a;
						var enabledLights = _v0.b;
						return A5(
							$elm_explorations$webgl$WebGL$entityWith,
							A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, backFaceSetting, settings),
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$texturedVertex,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$lambertianTextureFragment,
							webGLMesh,
							{enabledLights: enabledLights, lights12: lights.lights12, lights34: lights.lights34, lights56: lights.lights56, lights78: lights.lights78, materialColorTexture: materialColorData, modelMatrix: modelMatrix, modelScale: modelScale, normalMapTexture: materialColorData, projectionMatrix: projectionMatrix, sceneProperties: sceneProperties, useNormalMap: 0.0, viewMatrix: viewMatrix});
					})));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$texturedPhysicalMesh = F9(
	function (baseColorData, constantBaseColor, roughnessData, constantRoughness, metallicData, constantMetallic, bounds, webGLMesh, backFaceSetting) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Types$MeshNode,
				bounds,
				F8(
					function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, _v0, settings) {
						var lights = _v0.a;
						var enabledLights = _v0.b;
						return A5(
							$elm_explorations$webgl$WebGL$entityWith,
							A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, backFaceSetting, settings),
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$texturedVertex,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$physicalTexturesFragment,
							webGLMesh,
							{baseColorTexture: baseColorData, constantBaseColor: constantBaseColor, constantMetallic: constantMetallic, constantRoughness: constantRoughness, enabledLights: enabledLights, lights12: lights.lights12, lights34: lights.lights34, lights56: lights.lights56, lights78: lights.lights78, metallicTexture: metallicData, modelMatrix: modelMatrix, modelScale: modelScale, normalMapTexture: baseColorData, projectionMatrix: projectionMatrix, roughnessTexture: roughnessData, sceneProperties: sceneProperties, useNormalMap: 0.0, viewMatrix: viewMatrix});
					})));
	});
var $ianmackenzie$elm_geometry$BoundingBox3d$centerPoint = function (boundingBox) {
	var _v0 = boundingBox;
	var b = _v0.a;
	var x1 = b.minX;
	var x2 = b.maxX;
	var y1 = b.minY;
	var y2 = b.maxY;
	var z1 = b.minZ;
	var z2 = b.maxZ;
	return $ianmackenzie$elm_geometry$Geometry$Types$Point3d(
		{x: x1 + (0.5 * (x2 - x1)), y: y1 + (0.5 * (y2 - y1)), z: z1 + (0.5 * (z2 - z1))});
};
var $ianmackenzie$elm_geometry$BoundingBox3d$maxX = function (_v0) {
	var boundingBox = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(boundingBox.maxX);
};
var $ianmackenzie$elm_geometry$BoundingBox3d$maxY = function (_v0) {
	var boundingBox = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(boundingBox.maxY);
};
var $ianmackenzie$elm_geometry$BoundingBox3d$maxZ = function (_v0) {
	var boundingBox = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(boundingBox.maxZ);
};
var $ianmackenzie$elm_geometry$BoundingBox3d$minX = function (_v0) {
	var boundingBox = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(boundingBox.minX);
};
var $ianmackenzie$elm_geometry$BoundingBox3d$minY = function (_v0) {
	var boundingBox = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(boundingBox.minY);
};
var $ianmackenzie$elm_geometry$BoundingBox3d$minZ = function (_v0) {
	var boundingBox = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(boundingBox.minZ);
};
var $ianmackenzie$elm_geometry$BoundingBox3d$dimensions = function (boundingBox) {
	return _Utils_Tuple3(
		A2(
			$ianmackenzie$elm_units$Quantity$minus,
			$ianmackenzie$elm_geometry$BoundingBox3d$minX(boundingBox),
			$ianmackenzie$elm_geometry$BoundingBox3d$maxX(boundingBox)),
		A2(
			$ianmackenzie$elm_units$Quantity$minus,
			$ianmackenzie$elm_geometry$BoundingBox3d$minY(boundingBox),
			$ianmackenzie$elm_geometry$BoundingBox3d$maxY(boundingBox)),
		A2(
			$ianmackenzie$elm_units$Quantity$minus,
			$ianmackenzie$elm_geometry$BoundingBox3d$minZ(boundingBox),
			$ianmackenzie$elm_geometry$BoundingBox3d$maxZ(boundingBox)));
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds = function (boundingBox) {
	var _v0 = $ianmackenzie$elm_geometry$BoundingBox3d$dimensions(boundingBox);
	var xDimension = _v0.a.a;
	var yDimension = _v0.b.a;
	var zDimension = _v0.c.a;
	return {
		centerPoint: $ianmackenzie$elm_geometry$Point3d$unwrap(
			$ianmackenzie$elm_geometry$BoundingBox3d$centerPoint(boundingBox)),
		halfX: xDimension / 2,
		halfY: yDimension / 2,
		halfZ: zDimension / 2
	};
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$mesh = F2(
	function (givenMaterial, givenMesh) {
		switch (givenMaterial.$) {
			case 'UnlitMaterial':
				if (givenMaterial.b.$ === 'Constant') {
					var color = givenMaterial.b.a;
					switch (givenMesh.$) {
						case 'EmptyMesh':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Triangles':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$constantMesh,
								color,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'Facets':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$constantMesh,
								color,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'Indexed':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$constantMesh,
								color,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithNormals':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$constantMesh,
								color,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithUvs':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$constantMesh,
								color,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithNormalsAndUvs':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$constantMesh,
								color,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithTangents':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$constantMesh,
								color,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'LineSegments':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$constantMesh,
								color,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								$ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces);
						case 'Polyline':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$constantMesh,
								color,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								$ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces);
						default:
							var boundingBox = givenMesh.a;
							var radius = givenMesh.b;
							var webGLMesh = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$constantPointMesh,
								color,
								radius,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh);
					}
				} else {
					var _v2 = givenMaterial.a;
					var data = givenMaterial.b.a.data;
					switch (givenMesh.$) {
						case 'EmptyMesh':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Triangles':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Facets':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Indexed':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithNormals':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithUvs':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$colorTextureMesh,
								data,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithNormalsAndUvs':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$colorTextureMesh,
								data,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithTangents':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$colorTextureMesh,
								data,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'LineSegments':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Polyline':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						default:
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
					}
				}
			case 'EmissiveMaterial':
				if (givenMaterial.b.$ === 'Constant') {
					var emissiveColor = givenMaterial.b.a.a;
					var backlight = givenMaterial.c;
					switch (givenMesh.$) {
						case 'EmptyMesh':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Triangles':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A5(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$emissiveMesh,
								emissiveColor,
								backlight,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'Facets':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A5(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$emissiveMesh,
								emissiveColor,
								backlight,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'Indexed':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A5(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$emissiveMesh,
								emissiveColor,
								backlight,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithNormals':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A5(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$emissiveMesh,
								emissiveColor,
								backlight,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithUvs':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A5(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$emissiveMesh,
								emissiveColor,
								backlight,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithNormalsAndUvs':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A5(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$emissiveMesh,
								emissiveColor,
								backlight,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithTangents':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A5(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$emissiveMesh,
								emissiveColor,
								backlight,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'LineSegments':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							return A5(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$emissiveMesh,
								emissiveColor,
								backlight,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								$ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces);
						case 'Polyline':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							return A5(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$emissiveMesh,
								emissiveColor,
								backlight,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								$ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces);
						default:
							var boundingBox = givenMesh.a;
							var radius = givenMesh.b;
							var webGLMesh = givenMesh.d;
							return A5(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$emissivePointMesh,
								emissiveColor,
								backlight,
								radius,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh);
					}
				} else {
					var _v5 = givenMaterial.a;
					var data = givenMaterial.b.a.data;
					var backlight = givenMaterial.c;
					switch (givenMesh.$) {
						case 'EmptyMesh':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Triangles':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Facets':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Indexed':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithNormals':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithUvs':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A5(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$texturedEmissiveMesh,
								data,
								backlight,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithNormalsAndUvs':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A5(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$texturedEmissiveMesh,
								data,
								backlight,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithTangents':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A5(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$texturedEmissiveMesh,
								data,
								backlight,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'LineSegments':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Polyline':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						default:
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
					}
				}
			case 'LambertianMaterial':
				var _v7 = givenMaterial.a;
				var materialColorTexture = givenMaterial.b;
				var normalMapTexture = givenMaterial.c;
				var _v8 = A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$resolveLambertian, materialColorTexture, normalMapTexture);
				if (_v8.$ === 'ConstantLambertianMaterial') {
					var materialColor = _v8.a.a;
					switch (givenMesh.$) {
						case 'EmptyMesh':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Triangles':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Facets':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var cullBackFaces = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$lambertianMesh,
								materialColor,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								cullBackFaces);
						case 'Indexed':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithNormals':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var cullBackFaces = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$lambertianMesh,
								materialColor,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								cullBackFaces);
						case 'MeshWithUvs':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithNormalsAndUvs':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var cullBackFaces = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$lambertianMesh,
								materialColor,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								cullBackFaces);
						case 'MeshWithTangents':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var cullBackFaces = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$lambertianMesh,
								materialColor,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								cullBackFaces);
						case 'LineSegments':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Polyline':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						default:
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
					}
				} else {
					var _v10 = _v8.a;
					var materialColorData = _v10.a;
					var constantMaterialColor = _v10.b;
					var _v11 = _v8.b;
					var normalMapData = _v11.a;
					var useNormalMap = _v11.b;
					switch (givenMesh.$) {
						case 'EmptyMesh':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Triangles':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Facets':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Indexed':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithNormals':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithUvs':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithNormalsAndUvs':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var cullBackFaces = givenMesh.d;
							return A4(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$texturedLambertianMesh,
								materialColorData,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								cullBackFaces);
						case 'MeshWithTangents':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var cullBackFaces = givenMesh.d;
							return A6(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$normalMappedLambertianMesh,
								materialColorData,
								normalMapData,
								useNormalMap,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								cullBackFaces);
						case 'LineSegments':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Polyline':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						default:
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
					}
				}
			default:
				var _v13 = givenMaterial.a;
				var baseColorTexture = givenMaterial.b;
				var roughnessTexture = givenMaterial.c;
				var metallicTexture = givenMaterial.d;
				var normalMapTexture = givenMaterial.e;
				var _v14 = A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$resolvePbr, baseColorTexture, roughnessTexture, metallicTexture, normalMapTexture);
				if (_v14.$ === 'ConstantPbrMaterial') {
					var baseColor = _v14.a.a;
					var roughness = _v14.b;
					var metallic = _v14.c;
					switch (givenMesh.$) {
						case 'EmptyMesh':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Triangles':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Facets':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A6(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$physicalMesh,
								baseColor,
								roughness,
								metallic,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'Indexed':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithNormals':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A6(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$physicalMesh,
								baseColor,
								roughness,
								metallic,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithUvs':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithNormalsAndUvs':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A6(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$physicalMesh,
								baseColor,
								roughness,
								metallic,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithTangents':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A6(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$physicalMesh,
								baseColor,
								roughness,
								metallic,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'LineSegments':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Polyline':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						default:
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
					}
				} else {
					var _v16 = _v14.a;
					var baseColorData = _v16.a;
					var constantBaseColor = _v16.b;
					var _v17 = _v14.b;
					var roughnessData = _v17.a;
					var constantRoughness = _v17.b;
					var _v18 = _v14.c;
					var metallicData = _v18.a;
					var constantMetallic = _v18.b;
					var _v19 = _v14.d;
					var normalMapData = _v19.a;
					var useNormalMap = _v19.b;
					switch (givenMesh.$) {
						case 'EmptyMesh':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Triangles':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Facets':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Indexed':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithNormals':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithUvs':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'MeshWithNormalsAndUvs':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return A9(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$texturedPhysicalMesh,
								baseColorData,
								constantBaseColor,
								roughnessData,
								constantRoughness,
								metallicData,
								constantMetallic,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox),
								webGLMesh,
								backFaceSetting);
						case 'MeshWithTangents':
							var boundingBox = givenMesh.a;
							var webGLMesh = givenMesh.c;
							var backFaceSetting = givenMesh.d;
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$normalMappedPhysicalMesh(baseColorData)(constantBaseColor)(roughnessData)(constantRoughness)(metallicData)(constantMetallic)(normalMapData)(useNormalMap)(
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox))(webGLMesh)(backFaceSetting);
						case 'LineSegments':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						case 'Polyline':
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
						default:
							return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
					}
				}
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Transformation$placeIn = function (frame) {
	var p0 = $ianmackenzie$elm_geometry$Point3d$unwrap(
		$ianmackenzie$elm_geometry$Frame3d$originPoint(frame));
	var k = $ianmackenzie$elm_geometry$Direction3d$unwrap(
		$ianmackenzie$elm_geometry$Frame3d$zDirection(frame));
	var j = $ianmackenzie$elm_geometry$Direction3d$unwrap(
		$ianmackenzie$elm_geometry$Frame3d$yDirection(frame));
	var i = $ianmackenzie$elm_geometry$Direction3d$unwrap(
		$ianmackenzie$elm_geometry$Frame3d$xDirection(frame));
	return {
		isRightHanded: $ianmackenzie$elm_geometry$Frame3d$isRightHanded(frame),
		ix: i.x,
		iy: i.y,
		iz: i.z,
		jx: j.x,
		jy: j.y,
		jz: j.z,
		kx: k.x,
		ky: k.y,
		kz: k.z,
		px: p0.x,
		py: p0.y,
		pz: p0.z,
		scale: 1
	};
};
var $ianmackenzie$elm_3d_scene$Scene3d$Types$Transformed = F2(
	function (a, b) {
		return {$: 'Transformed', a: a, b: b};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Transformation$compose = F2(
	function (t1, t2) {
		return {
			isRightHanded: _Utils_eq(t1.isRightHanded, t2.isRightHanded),
			ix: ((t1.ix * t2.ix) + (t1.iy * t2.jx)) + (t1.iz * t2.kx),
			iy: ((t1.ix * t2.iy) + (t1.iy * t2.jy)) + (t1.iz * t2.ky),
			iz: ((t1.ix * t2.iz) + (t1.iy * t2.jz)) + (t1.iz * t2.kz),
			jx: ((t1.jx * t2.ix) + (t1.jy * t2.jx)) + (t1.jz * t2.kx),
			jy: ((t1.jx * t2.iy) + (t1.jy * t2.jy)) + (t1.jz * t2.ky),
			jz: ((t1.jx * t2.iz) + (t1.jy * t2.jz)) + (t1.jz * t2.kz),
			kx: ((t1.kx * t2.ix) + (t1.ky * t2.jx)) + (t1.kz * t2.kx),
			ky: ((t1.kx * t2.iy) + (t1.ky * t2.jy)) + (t1.kz * t2.ky),
			kz: ((t1.kx * t2.iz) + (t1.ky * t2.jz)) + (t1.kz * t2.kz),
			px: t2.px + ((((t1.px * t2.ix) + (t1.py * t2.jx)) + (t1.pz * t2.kx)) * t2.scale),
			py: t2.py + ((((t1.px * t2.iy) + (t1.py * t2.jy)) + (t1.pz * t2.ky)) * t2.scale),
			pz: t2.pz + ((((t1.px * t2.iz) + (t1.py * t2.jz)) + (t1.pz * t2.kz)) * t2.scale),
			scale: t1.scale * t2.scale
		};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$transformBy = F2(
	function (transformation, _v0) {
		var node = _v0.a;
		switch (node.$) {
			case 'EmptyNode':
				return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
			case 'Transformed':
				var existingTransformation = node.a;
				var underlyingNode = node.b;
				var compositeTransformation = A2($ianmackenzie$elm_3d_scene$Scene3d$Transformation$compose, existingTransformation, transformation);
				return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
					A2($ianmackenzie$elm_3d_scene$Scene3d$Types$Transformed, compositeTransformation, underlyingNode));
			case 'MeshNode':
				return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
					A2($ianmackenzie$elm_3d_scene$Scene3d$Types$Transformed, transformation, node));
			case 'PointNode':
				return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
					A2($ianmackenzie$elm_3d_scene$Scene3d$Types$Transformed, transformation, node));
			case 'ShadowNode':
				return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
					A2($ianmackenzie$elm_3d_scene$Scene3d$Types$Transformed, transformation, node));
			default:
				return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
					A2($ianmackenzie$elm_3d_scene$Scene3d$Types$Transformed, transformation, node));
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$placeIn = F2(
	function (frame, givenDrawable) {
		return A2(
			$ianmackenzie$elm_3d_scene$Scene3d$Entity$transformBy,
			$ianmackenzie$elm_3d_scene$Scene3d$Transformation$placeIn(frame),
			givenDrawable);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Types$ShadowNode = function (a) {
	return {$: 'ShadowNode', a: a};
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$preScaleBounds = F2(
	function (_v0, bounds) {
		var scaleX = _v0.a;
		var scaleY = _v0.b;
		var scaleZ = _v0.c;
		var originalCenterPoint = bounds.centerPoint;
		return {
			centerPoint: {x: scaleX * originalCenterPoint.x, y: scaleY * originalCenterPoint.y, z: scaleZ * originalCenterPoint.z},
			halfX: scaleX * bounds.halfX,
			halfY: scaleY * bounds.halfY,
			halfZ: scaleZ * bounds.halfZ
		};
	});
var $elm_explorations$linear_algebra$Math$Vector4$fromRecord = _MJS_v4fromRecord;
var $elm_explorations$linear_algebra$Math$Vector4$toRecord = _MJS_v4toRecord;
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$preScaleDrawFunction = function (_v0) {
	return function (originalDrawFunction) {
		return function (sceneProperties) {
			return function (modelScale) {
				return function (modelMatrix) {
					return function (isRightHanded) {
						return function (viewMatrix) {
							return function (projectionMatrix) {
								return function (lights) {
									return function (settings) {
										var scaleX = _v0.a;
										var scaleY = _v0.b;
										var scaleZ = _v0.c;
										var _v1 = $elm_explorations$linear_algebra$Math$Vector4$toRecord(modelScale);
										var x = _v1.x;
										var y = _v1.y;
										var z = _v1.z;
										var w = _v1.w;
										var updatedModelScale = $elm_explorations$linear_algebra$Math$Vector4$fromRecord(
											{w: w, x: x * scaleX, y: y * scaleY, z: z * scaleZ});
										return A8(originalDrawFunction, sceneProperties, updatedModelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, lights, settings);
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$preScaleNode = F2(
	function (scalingFactors, node) {
		switch (node.$) {
			case 'EmptyNode':
				return $ianmackenzie$elm_3d_scene$Scene3d$Types$EmptyNode;
			case 'Transformed':
				var transformation = node.a;
				var underlyingNode = node.b;
				return A2(
					$ianmackenzie$elm_3d_scene$Scene3d$Types$Transformed,
					transformation,
					A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$preScaleNode, scalingFactors, underlyingNode));
			case 'MeshNode':
				var bounds = node.a;
				var drawFunction = node.b;
				return A2(
					$ianmackenzie$elm_3d_scene$Scene3d$Types$MeshNode,
					A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$preScaleBounds, scalingFactors, bounds),
					A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$preScaleDrawFunction, scalingFactors, drawFunction));
			case 'PointNode':
				return node;
			case 'ShadowNode':
				var drawFunction = node.a;
				return $ianmackenzie$elm_3d_scene$Scene3d$Types$ShadowNode(
					A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$preScaleDrawFunction, scalingFactors, drawFunction));
			default:
				var childNodes = node.a;
				return $ianmackenzie$elm_3d_scene$Scene3d$Types$Group(
					A2(
						$elm$core$List$map,
						$ianmackenzie$elm_3d_scene$Scene3d$Entity$preScaleNode(scalingFactors),
						childNodes));
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$preScale = F2(
	function (scalingFactors, _v0) {
		var node = _v0.a;
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$preScaleNode, scalingFactors, node));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$shadowFragment = {
	src: '\n        precision lowp float;\n        \n        void main () {\n            gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);\n        }\n    ',
	attributes: {},
	uniforms: {}
};
var $elm_explorations$webgl$WebGL$Settings$StencilTest$Test = function (a) {
	return {$: 'Test', a: a};
};
var $elm_explorations$webgl$WebGL$Settings$StencilTest$always = $elm_explorations$webgl$WebGL$Settings$StencilTest$Test(519);
var $elm_explorations$webgl$WebGL$Settings$StencilTest$Operation = function (a) {
	return {$: 'Operation', a: a};
};
var $elm_explorations$webgl$WebGL$Settings$StencilTest$decrement = $elm_explorations$webgl$WebGL$Settings$StencilTest$Operation(7683);
var $elm_explorations$webgl$WebGL$Settings$StencilTest$increment = $elm_explorations$webgl$WebGL$Settings$StencilTest$Operation(7682);
var $elm_explorations$webgl$WebGL$Settings$StencilTest$keep = $elm_explorations$webgl$WebGL$Settings$StencilTest$Operation(7680);
var $elm_explorations$webgl$WebGL$Internal$StencilTest = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return {$: 'StencilTest', a: a, b: b, c: c, d: d, e: e, f: f, g: g, h: h, i: i, j: j, k: k};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $elm_explorations$webgl$WebGL$Settings$StencilTest$testSeparate = F3(
	function (_v0, options1, options2) {
		var ref = _v0.ref;
		var mask = _v0.mask;
		var writeMask = _v0.writeMask;
		var expandTest = F2(
			function (_v2, fn) {
				var expandedTest = _v2.a;
				return fn(expandedTest);
			});
		var expandOp = F2(
			function (_v1, fn) {
				var op = _v1.a;
				return fn(op);
			});
		var expand = function (options) {
			return A2(
				$elm$core$Basics$composeR,
				expandTest(options.test),
				A2(
					$elm$core$Basics$composeR,
					expandOp(options.fail),
					A2(
						$elm$core$Basics$composeR,
						expandOp(options.zfail),
						expandOp(options.zpass))));
		};
		return A2(
			expand,
			options2,
			A2(
				expand,
				options1,
				A3($elm_explorations$webgl$WebGL$Internal$StencilTest, ref, mask, writeMask)));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$leftHandedStencilTest = A3(
	$elm_explorations$webgl$WebGL$Settings$StencilTest$testSeparate,
	{mask: 0, ref: 0, writeMask: 15},
	{fail: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep, test: $elm_explorations$webgl$WebGL$Settings$StencilTest$always, zfail: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep, zpass: $elm_explorations$webgl$WebGL$Settings$StencilTest$decrement},
	{fail: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep, test: $elm_explorations$webgl$WebGL$Settings$StencilTest$always, zfail: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep, zpass: $elm_explorations$webgl$WebGL$Settings$StencilTest$increment});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$rightHandedStencilTest = A3(
	$elm_explorations$webgl$WebGL$Settings$StencilTest$testSeparate,
	{mask: 0, ref: 0, writeMask: 15},
	{fail: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep, test: $elm_explorations$webgl$WebGL$Settings$StencilTest$always, zfail: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep, zpass: $elm_explorations$webgl$WebGL$Settings$StencilTest$increment},
	{fail: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep, test: $elm_explorations$webgl$WebGL$Settings$StencilTest$always, zfail: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep, zpass: $elm_explorations$webgl$WebGL$Settings$StencilTest$decrement});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$shadowSettings = F2(
	function (isRightHanded, settings) {
		return isRightHanded ? A2($elm$core$List$cons, $ianmackenzie$elm_3d_scene$Scene3d$Entity$rightHandedStencilTest, settings) : A2($elm$core$List$cons, $ianmackenzie$elm_3d_scene$Scene3d$Entity$leftHandedStencilTest, settings);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$shadowVertex = {
	src: '\n        precision highp float;\n        \n        attribute highp vec3 position;\n        attribute highp vec3 normal;\n        \n        uniform highp vec4 modelScale;\n        uniform highp mat4 modelMatrix;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 projectionMatrix;\n        uniform highp mat4 sceneProperties;\n        uniform highp mat4 shadowLight;\n        \n        const lowp float kDirectionalLight = 1.0;\n        const lowp float kPointLight = 2.0;\n        \n        vec4 getWorldPosition(vec3 modelPosition, vec4 modelScale, mat4 modelMatrix) {\n            vec4 scaledPosition = vec4(modelScale.xyz * modelPosition, 1.0);\n            return modelMatrix * scaledPosition;\n        }\n        \n        vec3 safeNormalize(vec3 vector) {\n            if (vector == vec3(0.0, 0.0, 0.0)) {\n                return vector;\n            } else {\n                return normalize(vector);\n            }\n        }\n        \n        vec3 getWorldNormal(vec3 modelNormal, vec4 modelScale, mat4 modelMatrix) {\n            vec3 normalScale = vec3(modelScale.w / modelScale.x, modelScale.w / modelScale.y, modelScale.w / modelScale.z);\n            return (modelMatrix * vec4(safeNormalize(normalScale * modelNormal), 0.0)).xyz;\n        }\n        \n        vec3 getDirectionToLight(vec3 surfacePosition, vec4 xyz_type, vec4 rgb_parameter) {\n            float lightType = xyz_type.w;\n            if (lightType == kDirectionalLight) {\n                return xyz_type.xyz;\n            } else if (lightType == kPointLight) {\n                vec3 lightPosition = xyz_type.xyz;\n                return normalize(lightPosition - surfacePosition);\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        vec4 shadowVertexPosition(vec3 position, vec3 normal, mat4 shadowLight, vec4 modelScale, mat4 modelMatrix, mat4 viewMatrix, mat4 projectionMatrix, mat4 sceneProperties) {\n            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);\n            vec3 worldNormal = getWorldNormal(normal, vec4(modelScale.xyz, 1.0), modelMatrix);\n            vec4 xyz_type = shadowLight[0];\n            vec4 rgb_parameter = shadowLight[1];\n            vec3 directionToLight = getDirectionToLight(worldPosition.xyz, xyz_type, rgb_parameter);\n            vec3 offset = vec3(0.0, 0.0, 0.0);\n            float sceneDiameter = sceneProperties[3][1];\n            if (dot(directionToLight, worldNormal) <= 0.0) {\n                offset = -sceneDiameter * directionToLight;\n            } else {\n                offset = -0.001 * sceneDiameter * directionToLight;\n            }\n            vec4 offsetPosition = worldPosition + vec4(offset, 0.0);\n            return projectionMatrix * (viewMatrix * offsetPosition);\n        }\n        \n        void main () {\n            gl_Position = shadowVertexPosition(\n                position,\n                normal,\n                shadowLight,\n                modelScale,\n                modelMatrix,\n                viewMatrix,\n                projectionMatrix,\n                sceneProperties\n            );\n        }\n    ',
	attributes: {normal: 'normal', position: 'position'},
	uniforms: {modelMatrix: 'modelMatrix', modelScale: 'modelScale', projectionMatrix: 'projectionMatrix', sceneProperties: 'sceneProperties', shadowLight: 'shadowLight', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$shadowDrawFunction = function (givenShadow) {
	if (givenShadow.$ === 'EmptyShadow') {
		return $elm$core$Maybe$Nothing;
	} else {
		var webGLMesh = givenShadow.c;
		return $elm$core$Maybe$Just(
			F8(
				function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, shadowLight, settings) {
					return A5(
						$elm_explorations$webgl$WebGL$entityWith,
						A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$shadowSettings, isRightHanded, settings),
						$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$shadowVertex,
						$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$shadowFragment,
						webGLMesh,
						{modelMatrix: modelMatrix, modelScale: modelScale, projectionMatrix: projectionMatrix, sceneProperties: sceneProperties, shadowLight: shadowLight, viewMatrix: viewMatrix});
				}));
	}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$shadow = function (givenShadow) {
	var _v0 = $ianmackenzie$elm_3d_scene$Scene3d$Entity$shadowDrawFunction(givenShadow);
	if (_v0.$ === 'Just') {
		var drawFunction = _v0.a;
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			$ianmackenzie$elm_3d_scene$Scene3d$Types$ShadowNode(drawFunction));
	} else {
		return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
	}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$block = F4(
	function (renderObject, renderShadow, givenMaterial, givenBlock) {
		var baseEntity = A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$mesh, givenMaterial, $ianmackenzie$elm_3d_scene$Scene3d$Primitives$block);
		var untransformedEntity = function () {
			var _v1 = _Utils_Tuple2(renderObject, renderShadow);
			if (_v1.a) {
				if (_v1.b) {
					return $ianmackenzie$elm_3d_scene$Scene3d$Entity$group(
						_List_fromArray(
							[
								baseEntity,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$shadow($ianmackenzie$elm_3d_scene$Scene3d$Primitives$blockShadow)
							]));
				} else {
					return baseEntity;
				}
			} else {
				if (_v1.b) {
					return $ianmackenzie$elm_3d_scene$Scene3d$Entity$shadow($ianmackenzie$elm_3d_scene$Scene3d$Primitives$blockShadow);
				} else {
					return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
				}
			}
		}();
		var _v0 = $ianmackenzie$elm_geometry$Block3d$dimensions(givenBlock);
		var scaleX = _v0.a.a;
		var scaleY = _v0.b.a;
		var scaleZ = _v0.c.a;
		return A2(
			$ianmackenzie$elm_3d_scene$Scene3d$Entity$placeIn,
			$ianmackenzie$elm_geometry$Block3d$axes(givenBlock),
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Entity$preScale,
				_Utils_Tuple3(scaleX, scaleY, scaleZ),
				untransformedEntity));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$blockWithShadow = F2(
	function (givenMaterial, givenBlock) {
		return A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$block, true, true, givenMaterial, givenBlock);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Types$Constant = function (a) {
	return {$: 'Constant', a: a};
};
var $ianmackenzie$elm_3d_scene$Scene3d$Types$UnlitMaterial = F2(
	function (a, b) {
		return {$: 'UnlitMaterial', a: a, b: b};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Types$UseMeshUvs = {$: 'UseMeshUvs'};
var $avh4$elm_color$Color$toRgba = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	return {alpha: a, blue: b, green: g, red: r};
};
var $ianmackenzie$elm_3d_scene$Scene3d$Material$toVec3 = function (givenColor) {
	var _v0 = $avh4$elm_color$Color$toRgba(givenColor);
	var red = _v0.red;
	var green = _v0.green;
	var blue = _v0.blue;
	return A3($elm_explorations$linear_algebra$Math$Vector3$vec3, red, green, blue);
};
var $ianmackenzie$elm_3d_scene$Scene3d$Material$color = function (givenColor) {
	return A2(
		$ianmackenzie$elm_3d_scene$Scene3d$Types$UnlitMaterial,
		$ianmackenzie$elm_3d_scene$Scene3d$Types$UseMeshUvs,
		$ianmackenzie$elm_3d_scene$Scene3d$Types$Constant(
			$ianmackenzie$elm_3d_scene$Scene3d$Material$toVec3(givenColor)));
};
var $ianmackenzie$elm_3d_scene$Scene3d$Types$LambertianMaterial = F3(
	function (a, b, c) {
		return {$: 'LambertianMaterial', a: a, b: b, c: c};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Types$VerticalNormal = {$: 'VerticalNormal'};
var $ianmackenzie$elm_3d_scene$Scene3d$Types$LinearRgb = function (a) {
	return {$: 'LinearRgb', a: a};
};
var $ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$inverseGamma = function (u) {
	return A3(
		$elm$core$Basics$clamp,
		0,
		1,
		(u <= 0.04045) ? (u / 12.92) : A2($elm$core$Basics$pow, (u + 0.055) / 1.055, 2.4));
};
var $ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$colorToLinearRgb = function (color) {
	var _v0 = $avh4$elm_color$Color$toRgba(color);
	var red = _v0.red;
	var green = _v0.green;
	var blue = _v0.blue;
	return $ianmackenzie$elm_3d_scene$Scene3d$Types$LinearRgb(
		A3(
			$elm_explorations$linear_algebra$Math$Vector3$vec3,
			$ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$inverseGamma(red),
			$ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$inverseGamma(green),
			$ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$inverseGamma(blue)));
};
var $ianmackenzie$elm_3d_scene$Scene3d$Material$matte = function (materialColor) {
	return A3(
		$ianmackenzie$elm_3d_scene$Scene3d$Types$LambertianMaterial,
		$ianmackenzie$elm_3d_scene$Scene3d$Types$UseMeshUvs,
		$ianmackenzie$elm_3d_scene$Scene3d$Types$Constant(
			$ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$colorToLinearRgb(materialColor)),
		$ianmackenzie$elm_3d_scene$Scene3d$Types$Constant($ianmackenzie$elm_3d_scene$Scene3d$Types$VerticalNormal));
};
var $ianmackenzie$elm_3d_scene$Scene3d$group = function (entities) {
	return $ianmackenzie$elm_3d_scene$Scene3d$Entity$group(entities);
};
var $ianmackenzie$elm_3d_scene$Scene3d$meshWithShadow = F3(
	function (givenMaterial, givenMesh, givenShadow) {
		return $ianmackenzie$elm_3d_scene$Scene3d$group(
			_List_fromArray(
				[
					A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$mesh, givenMaterial, givenMesh),
					$ianmackenzie$elm_3d_scene$Scene3d$Entity$shadow(givenShadow)
				]));
	});
var $ianmackenzie$elm_units$Length$millimeters = function (numMillimeters) {
	return $ianmackenzie$elm_units$Length$meters(0.001 * numMillimeters);
};
var $ianmackenzie$elm_3d_scene$Scene3d$placeIn = F2(
	function (frame, entity) {
		return A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$placeIn, frame, entity);
	});
var $ianmackenzie$elm_geometry$BoundingBox3d$hullHelp = F7(
	function (currentMinX, currentMaxX, currentMinY, currentMaxY, currentMinZ, currentMaxZ, points) {
		hullHelp:
		while (true) {
			if (points.b) {
				var next = points.a;
				var rest = points.b;
				var _v1 = next;
				var x = _v1.a.x;
				var y = _v1.a.y;
				var z = _v1.a.z;
				var $temp$currentMinX = A2($elm$core$Basics$min, x, currentMinX),
					$temp$currentMaxX = A2($elm$core$Basics$max, x, currentMaxX),
					$temp$currentMinY = A2($elm$core$Basics$min, y, currentMinY),
					$temp$currentMaxY = A2($elm$core$Basics$max, y, currentMaxY),
					$temp$currentMinZ = A2($elm$core$Basics$min, z, currentMinZ),
					$temp$currentMaxZ = A2($elm$core$Basics$max, z, currentMaxZ),
					$temp$points = rest;
				currentMinX = $temp$currentMinX;
				currentMaxX = $temp$currentMaxX;
				currentMinY = $temp$currentMinY;
				currentMaxY = $temp$currentMaxY;
				currentMinZ = $temp$currentMinZ;
				currentMaxZ = $temp$currentMaxZ;
				points = $temp$points;
				continue hullHelp;
			} else {
				return $ianmackenzie$elm_geometry$Geometry$Types$BoundingBox3d(
					{maxX: currentMaxX, maxY: currentMaxY, maxZ: currentMaxZ, minX: currentMinX, minY: currentMinY, minZ: currentMinZ});
			}
		}
	});
var $ianmackenzie$elm_geometry$BoundingBox3d$hull = F2(
	function (first, rest) {
		var _v0 = first;
		var x = _v0.a.x;
		var y = _v0.a.y;
		var z = _v0.a.z;
		return A7($ianmackenzie$elm_geometry$BoundingBox3d$hullHelp, x, x, y, y, z, z, rest);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$plainQuadVertex = {
	src: '\n        precision highp float;\n        \n        attribute highp vec3 quadVertex;\n        \n        uniform highp vec4 modelScale;\n        uniform highp mat4 modelMatrix;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 projectionMatrix;\n        uniform highp mat4 sceneProperties;\n        uniform highp mat4 quadVertexPositions;\n        \n        void getQuadVertex(int quadVertexIndex, mat4 quadVertexPositions, out vec3 position, out vec3 normal, out vec3 tangent) {\n            vec3 next = vec3(0.0, 0.0, 0.0);\n            vec3 prev = vec3(0.0, 0.0, 0.0);\n            if (quadVertexIndex == 0) {\n                prev = quadVertexPositions[3].xyz;\n                position = quadVertexPositions[0].xyz;\n                next = quadVertexPositions[1].xyz;\n                tangent = normalize(next - position);\n            } else if (quadVertexIndex == 1) {\n                prev = quadVertexPositions[0].xyz;\n                position = quadVertexPositions[1].xyz;\n                next = quadVertexPositions[2].xyz;\n                tangent = normalize(position - prev);\n            } else if (quadVertexIndex == 2) {\n                prev = quadVertexPositions[1].xyz;\n                position = quadVertexPositions[2].xyz;\n                next = quadVertexPositions[3].xyz;\n                tangent = normalize(position - next);\n            } else {\n                prev = quadVertexPositions[2].xyz;\n                position = quadVertexPositions[3].xyz;\n                next = quadVertexPositions[0].xyz;\n                tangent = normalize(prev - position);\n            }\n            normal = normalize(cross(next - position, prev - position));\n        }\n        \n        vec4 getWorldPosition(vec3 modelPosition, vec4 modelScale, mat4 modelMatrix) {\n            vec4 scaledPosition = vec4(modelScale.xyz * modelPosition, 1.0);\n            return modelMatrix * scaledPosition;\n        }\n        \n        void main() {\n            vec3 position = vec3(0.0, 0.0, 0.0);\n            vec3 normal = vec3(0.0, 0.0, 0.0);\n            vec3 tangent = vec3(0.0, 0.0, 0.0);\n            getQuadVertex(int(quadVertex.z), quadVertexPositions, position, normal, tangent);\n            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);\n            gl_Position = projectionMatrix * (viewMatrix * worldPosition);\n        }\n    ',
	attributes: {quadVertex: 'quadVertex'},
	uniforms: {modelMatrix: 'modelMatrix', modelScale: 'modelScale', projectionMatrix: 'projectionMatrix', quadVertexPositions: 'quadVertexPositions', sceneProperties: 'sceneProperties', viewMatrix: 'viewMatrix'}
};
var $elm_explorations$linear_algebra$Math$Matrix4$fromRecord = _MJS_m4x4fromRecord;
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertexPositions = F4(
	function (firstPoint, secondPoint, thirdPoint, fourthPoint) {
		var p4 = $ianmackenzie$elm_geometry$Point3d$toMeters(fourthPoint);
		var p3 = $ianmackenzie$elm_geometry$Point3d$toMeters(thirdPoint);
		var p2 = $ianmackenzie$elm_geometry$Point3d$toMeters(secondPoint);
		var p1 = $ianmackenzie$elm_geometry$Point3d$toMeters(firstPoint);
		return $elm_explorations$linear_algebra$Math$Matrix4$fromRecord(
			{m11: p1.x, m12: p2.x, m13: p3.x, m14: p4.x, m21: p1.y, m22: p2.y, m23: p3.y, m24: p4.y, m31: p1.z, m32: p2.z, m33: p3.z, m34: p4.z, m41: 0, m42: 0, m43: 0, m44: 0});
	});
var $elm_explorations$webgl$WebGL$Mesh1 = F2(
	function (a, b) {
		return {$: 'Mesh1', a: a, b: b};
	});
var $elm_explorations$webgl$WebGL$triangleFan = $elm_explorations$webgl$WebGL$Mesh1(
	{elemSize: 1, indexSize: 0, mode: 6});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertices = $elm_explorations$webgl$WebGL$triangleFan(
	_List_fromArray(
		[
			{
			quadVertex: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0)
		},
			{
			quadVertex: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, 1)
		},
			{
			quadVertex: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 1, 2)
		},
			{
			quadVertex: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 1, 3)
		}
		]));
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$smoothQuadVertex = {
	src: '\n        precision highp float;\n        \n        attribute highp vec3 quadVertex;\n        \n        uniform highp vec4 modelScale;\n        uniform highp mat4 modelMatrix;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 projectionMatrix;\n        uniform highp mat4 sceneProperties;\n        uniform highp mat4 quadVertexPositions;\n        \n        varying highp vec3 interpolatedPosition;\n        varying highp vec3 interpolatedNormal;\n        \n        void getQuadVertex(int quadVertexIndex, mat4 quadVertexPositions, out vec3 position, out vec3 normal, out vec3 tangent) {\n            vec3 next = vec3(0.0, 0.0, 0.0);\n            vec3 prev = vec3(0.0, 0.0, 0.0);\n            if (quadVertexIndex == 0) {\n                prev = quadVertexPositions[3].xyz;\n                position = quadVertexPositions[0].xyz;\n                next = quadVertexPositions[1].xyz;\n                tangent = normalize(next - position);\n            } else if (quadVertexIndex == 1) {\n                prev = quadVertexPositions[0].xyz;\n                position = quadVertexPositions[1].xyz;\n                next = quadVertexPositions[2].xyz;\n                tangent = normalize(position - prev);\n            } else if (quadVertexIndex == 2) {\n                prev = quadVertexPositions[1].xyz;\n                position = quadVertexPositions[2].xyz;\n                next = quadVertexPositions[3].xyz;\n                tangent = normalize(position - next);\n            } else {\n                prev = quadVertexPositions[2].xyz;\n                position = quadVertexPositions[3].xyz;\n                next = quadVertexPositions[0].xyz;\n                tangent = normalize(prev - position);\n            }\n            normal = normalize(cross(next - position, prev - position));\n        }\n        \n        vec4 getWorldPosition(vec3 modelPosition, vec4 modelScale, mat4 modelMatrix) {\n            vec4 scaledPosition = vec4(modelScale.xyz * modelPosition, 1.0);\n            return modelMatrix * scaledPosition;\n        }\n        \n        vec3 safeNormalize(vec3 vector) {\n            if (vector == vec3(0.0, 0.0, 0.0)) {\n                return vector;\n            } else {\n                return normalize(vector);\n            }\n        }\n        \n        vec3 getWorldNormal(vec3 modelNormal, vec4 modelScale, mat4 modelMatrix) {\n            vec3 normalScale = vec3(modelScale.w / modelScale.x, modelScale.w / modelScale.y, modelScale.w / modelScale.z);\n            return (modelMatrix * vec4(safeNormalize(normalScale * modelNormal), 0.0)).xyz;\n        }\n        \n        void main() {\n            vec3 position = vec3(0.0, 0.0, 0.0);\n            vec3 normal = vec3(0.0, 0.0, 0.0);\n            vec3 tangent = vec3(0.0, 0.0, 0.0);\n            getQuadVertex(int(quadVertex.z), quadVertexPositions, position, normal, tangent);\n            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);\n            gl_Position = projectionMatrix * (viewMatrix * worldPosition);\n            interpolatedPosition = worldPosition.xyz;\n            interpolatedNormal = getWorldNormal(normal, modelScale, modelMatrix);\n        }\n    ',
	attributes: {quadVertex: 'quadVertex'},
	uniforms: {modelMatrix: 'modelMatrix', modelScale: 'modelScale', projectionMatrix: 'projectionMatrix', quadVertexPositions: 'quadVertexPositions', sceneProperties: 'sceneProperties', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$texturedQuadVertex = {
	src: '\n        precision highp float;\n        \n        attribute highp vec3 quadVertex;\n        \n        uniform highp vec4 modelScale;\n        uniform highp mat4 modelMatrix;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 projectionMatrix;\n        uniform highp mat4 sceneProperties;\n        uniform highp mat4 quadVertexPositions;\n        \n        varying highp vec3 interpolatedPosition;\n        varying highp vec3 interpolatedNormal;\n        varying mediump vec2 interpolatedUv;\n        varying highp vec3 interpolatedTangent;\n        \n        void getQuadVertex(int quadVertexIndex, mat4 quadVertexPositions, out vec3 position, out vec3 normal, out vec3 tangent) {\n            vec3 next = vec3(0.0, 0.0, 0.0);\n            vec3 prev = vec3(0.0, 0.0, 0.0);\n            if (quadVertexIndex == 0) {\n                prev = quadVertexPositions[3].xyz;\n                position = quadVertexPositions[0].xyz;\n                next = quadVertexPositions[1].xyz;\n                tangent = normalize(next - position);\n            } else if (quadVertexIndex == 1) {\n                prev = quadVertexPositions[0].xyz;\n                position = quadVertexPositions[1].xyz;\n                next = quadVertexPositions[2].xyz;\n                tangent = normalize(position - prev);\n            } else if (quadVertexIndex == 2) {\n                prev = quadVertexPositions[1].xyz;\n                position = quadVertexPositions[2].xyz;\n                next = quadVertexPositions[3].xyz;\n                tangent = normalize(position - next);\n            } else {\n                prev = quadVertexPositions[2].xyz;\n                position = quadVertexPositions[3].xyz;\n                next = quadVertexPositions[0].xyz;\n                tangent = normalize(prev - position);\n            }\n            normal = normalize(cross(next - position, prev - position));\n        }\n        \n        vec4 getWorldPosition(vec3 modelPosition, vec4 modelScale, mat4 modelMatrix) {\n            vec4 scaledPosition = vec4(modelScale.xyz * modelPosition, 1.0);\n            return modelMatrix * scaledPosition;\n        }\n        \n        vec3 safeNormalize(vec3 vector) {\n            if (vector == vec3(0.0, 0.0, 0.0)) {\n                return vector;\n            } else {\n                return normalize(vector);\n            }\n        }\n        \n        vec3 getWorldNormal(vec3 modelNormal, vec4 modelScale, mat4 modelMatrix) {\n            vec3 normalScale = vec3(modelScale.w / modelScale.x, modelScale.w / modelScale.y, modelScale.w / modelScale.z);\n            return (modelMatrix * vec4(safeNormalize(normalScale * modelNormal), 0.0)).xyz;\n        }\n        \n        void main() {\n            vec3 position = vec3(0.0, 0.0, 0.0);\n            vec3 normal = vec3(0.0, 0.0, 0.0);\n            vec3 tangent = vec3(0.0, 0.0, 0.0);\n            getQuadVertex(int(quadVertex.z), quadVertexPositions, position, normal, tangent);\n            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);\n            gl_Position = projectionMatrix * (viewMatrix * worldPosition);\n            interpolatedPosition = worldPosition.xyz;\n            interpolatedNormal = getWorldNormal(normal, modelScale, modelMatrix);\n            interpolatedUv = quadVertex.xy;\n            interpolatedTangent = tangent;\n        }\n    ',
	attributes: {quadVertex: 'quadVertex'},
	uniforms: {modelMatrix: 'modelMatrix', modelScale: 'modelScale', projectionMatrix: 'projectionMatrix', quadVertexPositions: 'quadVertexPositions', sceneProperties: 'sceneProperties', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$unlitQuadVertex = {
	src: '\n        precision highp float;\n        \n        attribute highp vec3 quadVertex;\n        \n        uniform highp vec4 modelScale;\n        uniform highp mat4 modelMatrix;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 projectionMatrix;\n        uniform highp mat4 sceneProperties;\n        uniform highp mat4 quadVertexPositions;\n        \n        varying mediump vec2 interpolatedUv;\n        \n        void getQuadVertex(int quadVertexIndex, mat4 quadVertexPositions, out vec3 position, out vec3 normal, out vec3 tangent) {\n            vec3 next = vec3(0.0, 0.0, 0.0);\n            vec3 prev = vec3(0.0, 0.0, 0.0);\n            if (quadVertexIndex == 0) {\n                prev = quadVertexPositions[3].xyz;\n                position = quadVertexPositions[0].xyz;\n                next = quadVertexPositions[1].xyz;\n                tangent = normalize(next - position);\n            } else if (quadVertexIndex == 1) {\n                prev = quadVertexPositions[0].xyz;\n                position = quadVertexPositions[1].xyz;\n                next = quadVertexPositions[2].xyz;\n                tangent = normalize(position - prev);\n            } else if (quadVertexIndex == 2) {\n                prev = quadVertexPositions[1].xyz;\n                position = quadVertexPositions[2].xyz;\n                next = quadVertexPositions[3].xyz;\n                tangent = normalize(position - next);\n            } else {\n                prev = quadVertexPositions[2].xyz;\n                position = quadVertexPositions[3].xyz;\n                next = quadVertexPositions[0].xyz;\n                tangent = normalize(prev - position);\n            }\n            normal = normalize(cross(next - position, prev - position));\n        }\n        \n        vec4 getWorldPosition(vec3 modelPosition, vec4 modelScale, mat4 modelMatrix) {\n            vec4 scaledPosition = vec4(modelScale.xyz * modelPosition, 1.0);\n            return modelMatrix * scaledPosition;\n        }\n        \n        void main() {\n            vec3 position = vec3(0.0, 0.0, 0.0);\n            vec3 normal = vec3(0.0, 0.0, 0.0);\n            vec3 tangent = vec3(0.0, 0.0, 0.0);\n            getQuadVertex(int(quadVertex.z), quadVertexPositions, position, normal, tangent);\n            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);\n            gl_Position = projectionMatrix * (viewMatrix * worldPosition);\n            interpolatedUv = quadVertex.xy;\n        }\n    ',
	attributes: {quadVertex: 'quadVertex'},
	uniforms: {modelMatrix: 'modelMatrix', modelScale: 'modelScale', projectionMatrix: 'projectionMatrix', quadVertexPositions: 'quadVertexPositions', sceneProperties: 'sceneProperties', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$quadMesh = F5(
	function (givenMaterial, firstPoint, secondPoint, thirdPoint, fourthPoint) {
		var boundingBox = A2(
			$ianmackenzie$elm_geometry$BoundingBox3d$hull,
			firstPoint,
			_List_fromArray(
				[secondPoint, thirdPoint, fourthPoint]));
		var bounds = $ianmackenzie$elm_3d_scene$Scene3d$Entity$toBounds(boundingBox);
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Types$MeshNode,
				bounds,
				function () {
					switch (givenMaterial.$) {
						case 'UnlitMaterial':
							if (givenMaterial.b.$ === 'Constant') {
								var color = givenMaterial.b.a;
								return F8(
									function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, lights, settings) {
										return A5(
											$elm_explorations$webgl$WebGL$entityWith,
											A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, $ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces, settings),
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$plainQuadVertex,
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$constantFragment,
											$ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertices,
											{
												constantColor: color,
												modelMatrix: modelMatrix,
												modelScale: modelScale,
												projectionMatrix: projectionMatrix,
												quadVertexPositions: A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertexPositions, firstPoint, secondPoint, thirdPoint, fourthPoint),
												sceneProperties: sceneProperties,
												viewMatrix: viewMatrix
											});
									});
							} else {
								var _v1 = givenMaterial.a;
								var data = givenMaterial.b.a.data;
								return F8(
									function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, lights, settings) {
										return A5(
											$elm_explorations$webgl$WebGL$entityWith,
											A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, $ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces, settings),
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$unlitQuadVertex,
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$colorTextureFragment,
											$ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertices,
											{
												colorTexture: data,
												modelMatrix: modelMatrix,
												modelScale: modelScale,
												projectionMatrix: projectionMatrix,
												quadVertexPositions: A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertexPositions, firstPoint, secondPoint, thirdPoint, fourthPoint),
												sceneProperties: sceneProperties,
												viewMatrix: viewMatrix
											});
									});
							}
						case 'EmissiveMaterial':
							if (givenMaterial.b.$ === 'Constant') {
								var emissiveColor = givenMaterial.b.a.a;
								var backlight = givenMaterial.c;
								return F8(
									function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, lights, settings) {
										return A5(
											$elm_explorations$webgl$WebGL$entityWith,
											A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, $ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces, settings),
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$plainQuadVertex,
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$emissiveFragment,
											$ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertices,
											{
												backlight: backlight,
												emissiveColor: A2(
													$elm_explorations$linear_algebra$Math$Vector3$scale,
													$ianmackenzie$elm_units$Luminance$inNits(backlight),
													emissiveColor),
												modelMatrix: modelMatrix,
												modelScale: modelScale,
												projectionMatrix: projectionMatrix,
												quadVertexPositions: A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertexPositions, firstPoint, secondPoint, thirdPoint, fourthPoint),
												sceneProperties: sceneProperties,
												viewMatrix: viewMatrix
											});
									});
							} else {
								var _v2 = givenMaterial.a;
								var data = givenMaterial.b.a.data;
								var backlight = givenMaterial.c;
								return F8(
									function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, lights, settings) {
										return A5(
											$elm_explorations$webgl$WebGL$entityWith,
											A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, $ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces, settings),
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$unlitQuadVertex,
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$emissiveTextureFragment,
											$ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertices,
											{
												backlight: $ianmackenzie$elm_units$Luminance$inNits(backlight),
												colorTexture: data,
												modelMatrix: modelMatrix,
												modelScale: modelScale,
												projectionMatrix: projectionMatrix,
												quadVertexPositions: A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertexPositions, firstPoint, secondPoint, thirdPoint, fourthPoint),
												sceneProperties: sceneProperties,
												viewMatrix: viewMatrix
											});
									});
							}
						case 'LambertianMaterial':
							var _v3 = givenMaterial.a;
							var materialColorTexture = givenMaterial.b;
							var normalMapTexture = givenMaterial.c;
							var _v4 = A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$resolveLambertian, materialColorTexture, normalMapTexture);
							if (_v4.$ === 'ConstantLambertianMaterial') {
								var materialColor = _v4.a.a;
								return F8(
									function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, _v5, settings) {
										var lights = _v5.a;
										var enabledLights = _v5.b;
										return A5(
											$elm_explorations$webgl$WebGL$entityWith,
											A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, $ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces, settings),
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$smoothQuadVertex,
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$lambertianFragment,
											$ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertices,
											{
												enabledLights: enabledLights,
												lights12: lights.lights12,
												lights34: lights.lights34,
												lights56: lights.lights56,
												lights78: lights.lights78,
												materialColor: materialColor,
												modelMatrix: modelMatrix,
												modelScale: modelScale,
												projectionMatrix: projectionMatrix,
												quadVertexPositions: A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertexPositions, firstPoint, secondPoint, thirdPoint, fourthPoint),
												sceneProperties: sceneProperties,
												viewMatrix: viewMatrix
											});
									});
							} else {
								var _v6 = _v4.a;
								var materialColorData = _v6.a;
								var constantMaterialColor = _v6.b;
								var _v7 = _v4.b;
								var normalMapData = _v7.a;
								var useNormalMap = _v7.b;
								return F8(
									function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, _v8, settings) {
										var lights = _v8.a;
										var enabledLights = _v8.b;
										return A5(
											$elm_explorations$webgl$WebGL$entityWith,
											A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, $ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces, settings),
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$texturedQuadVertex,
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$lambertianTextureFragment,
											$ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertices,
											{
												enabledLights: enabledLights,
												lights12: lights.lights12,
												lights34: lights.lights34,
												lights56: lights.lights56,
												lights78: lights.lights78,
												materialColorTexture: materialColorData,
												modelMatrix: modelMatrix,
												modelScale: modelScale,
												normalMapTexture: normalMapData,
												projectionMatrix: projectionMatrix,
												quadVertexPositions: A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertexPositions, firstPoint, secondPoint, thirdPoint, fourthPoint),
												sceneProperties: sceneProperties,
												useNormalMap: useNormalMap,
												viewMatrix: viewMatrix
											});
									});
							}
						default:
							var _v9 = givenMaterial.a;
							var baseColorTexture = givenMaterial.b;
							var roughnessTexture = givenMaterial.c;
							var metallicTexture = givenMaterial.d;
							var normalMapTexture = givenMaterial.e;
							var _v10 = A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$resolvePbr, baseColorTexture, roughnessTexture, metallicTexture, normalMapTexture);
							if (_v10.$ === 'ConstantPbrMaterial') {
								var baseColor = _v10.a.a;
								var roughness = _v10.b;
								var metallic = _v10.c;
								return F8(
									function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, _v11, settings) {
										var lights = _v11.a;
										var enabledLights = _v11.b;
										return A5(
											$elm_explorations$webgl$WebGL$entityWith,
											A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, $ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces, settings),
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$smoothQuadVertex,
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$physicalFragment,
											$ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertices,
											{
												baseColor: baseColor,
												enabledLights: enabledLights,
												lights12: lights.lights12,
												lights34: lights.lights34,
												lights56: lights.lights56,
												lights78: lights.lights78,
												metallic: metallic,
												modelMatrix: modelMatrix,
												modelScale: modelScale,
												projectionMatrix: projectionMatrix,
												quadVertexPositions: A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertexPositions, firstPoint, secondPoint, thirdPoint, fourthPoint),
												roughness: roughness,
												sceneProperties: sceneProperties,
												viewMatrix: viewMatrix
											});
									});
							} else {
								var _v12 = _v10.a;
								var baseColorData = _v12.a;
								var constantBaseColor = _v12.b;
								var _v13 = _v10.b;
								var roughnessData = _v13.a;
								var constantRoughness = _v13.b;
								var _v14 = _v10.c;
								var metallicData = _v14.a;
								var constantMetallic = _v14.b;
								var _v15 = _v10.d;
								var normalMapData = _v15.a;
								var useNormalMap = _v15.b;
								return F8(
									function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, _v16, settings) {
										var lights = _v16.a;
										var enabledLights = _v16.b;
										return A5(
											$elm_explorations$webgl$WebGL$entityWith,
											A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$meshSettings, isRightHanded, $ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces, settings),
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$texturedQuadVertex,
											$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$physicalTexturesFragment,
											$ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertices,
											{
												baseColorTexture: baseColorData,
												constantBaseColor: constantBaseColor,
												constantMetallic: constantMetallic,
												constantRoughness: constantRoughness,
												enabledLights: enabledLights,
												lights12: lights.lights12,
												lights34: lights.lights34,
												lights56: lights.lights56,
												lights78: lights.lights78,
												metallicTexture: metallicData,
												modelMatrix: modelMatrix,
												modelScale: modelScale,
												normalMapTexture: normalMapData,
												projectionMatrix: projectionMatrix,
												quadVertexPositions: A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertexPositions, firstPoint, secondPoint, thirdPoint, fourthPoint),
												roughnessTexture: roughnessData,
												sceneProperties: sceneProperties,
												useNormalMap: useNormalMap,
												viewMatrix: viewMatrix
											});
									});
							}
					}
				}()));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$quadShadowMesh = function () {
	var quadShadowVertices = _List_fromArray(
		[
			{
			quadShadowVertex: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, 0, 1)
		},
			{
			quadShadowVertex: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, 1, 1)
		},
			{
			quadShadowVertex: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, 2, 1)
		},
			{
			quadShadowVertex: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, 3, 1)
		},
			{
			quadShadowVertex: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, 0, -1)
		},
			{
			quadShadowVertex: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, 1, -1)
		},
			{
			quadShadowVertex: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, 2, -1)
		},
			{
			quadShadowVertex: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, 3, -1)
		}
		]);
	var quadShadowFaces = _List_fromArray(
		[
			_Utils_Tuple3(0, 1, 2),
			_Utils_Tuple3(0, 2, 3),
			_Utils_Tuple3(4, 6, 5),
			_Utils_Tuple3(4, 7, 6),
			_Utils_Tuple3(4, 5, 1),
			_Utils_Tuple3(1, 0, 4),
			_Utils_Tuple3(5, 6, 2),
			_Utils_Tuple3(2, 1, 5),
			_Utils_Tuple3(6, 7, 3),
			_Utils_Tuple3(3, 2, 6),
			_Utils_Tuple3(7, 4, 0),
			_Utils_Tuple3(0, 3, 7)
		]);
	return A2($elm_explorations$webgl$WebGL$indexedTriangles, quadShadowVertices, quadShadowFaces);
}();
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$quadShadowVertex = {
	src: '\n        precision highp float;\n        \n        attribute highp vec2 quadShadowVertex;\n        \n        uniform highp vec4 modelScale;\n        uniform highp mat4 modelMatrix;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 projectionMatrix;\n        uniform highp mat4 sceneProperties;\n        uniform highp mat4 shadowLight;\n        uniform highp mat4 quadVertexPositions;\n        \n        const lowp float kDirectionalLight = 1.0;\n        const lowp float kPointLight = 2.0;\n        \n        void getQuadVertex(int quadVertexIndex, mat4 quadVertexPositions, out vec3 position, out vec3 normal, out vec3 tangent) {\n            vec3 next = vec3(0.0, 0.0, 0.0);\n            vec3 prev = vec3(0.0, 0.0, 0.0);\n            if (quadVertexIndex == 0) {\n                prev = quadVertexPositions[3].xyz;\n                position = quadVertexPositions[0].xyz;\n                next = quadVertexPositions[1].xyz;\n                tangent = normalize(next - position);\n            } else if (quadVertexIndex == 1) {\n                prev = quadVertexPositions[0].xyz;\n                position = quadVertexPositions[1].xyz;\n                next = quadVertexPositions[2].xyz;\n                tangent = normalize(position - prev);\n            } else if (quadVertexIndex == 2) {\n                prev = quadVertexPositions[1].xyz;\n                position = quadVertexPositions[2].xyz;\n                next = quadVertexPositions[3].xyz;\n                tangent = normalize(position - next);\n            } else {\n                prev = quadVertexPositions[2].xyz;\n                position = quadVertexPositions[3].xyz;\n                next = quadVertexPositions[0].xyz;\n                tangent = normalize(prev - position);\n            }\n            normal = normalize(cross(next - position, prev - position));\n        }\n        \n        vec4 getWorldPosition(vec3 modelPosition, vec4 modelScale, mat4 modelMatrix) {\n            vec4 scaledPosition = vec4(modelScale.xyz * modelPosition, 1.0);\n            return modelMatrix * scaledPosition;\n        }\n        \n        vec3 safeNormalize(vec3 vector) {\n            if (vector == vec3(0.0, 0.0, 0.0)) {\n                return vector;\n            } else {\n                return normalize(vector);\n            }\n        }\n        \n        vec3 getWorldNormal(vec3 modelNormal, vec4 modelScale, mat4 modelMatrix) {\n            vec3 normalScale = vec3(modelScale.w / modelScale.x, modelScale.w / modelScale.y, modelScale.w / modelScale.z);\n            return (modelMatrix * vec4(safeNormalize(normalScale * modelNormal), 0.0)).xyz;\n        }\n        \n        vec3 getDirectionToLight(vec3 surfacePosition, vec4 xyz_type, vec4 rgb_parameter) {\n            float lightType = xyz_type.w;\n            if (lightType == kDirectionalLight) {\n                return xyz_type.xyz;\n            } else if (lightType == kPointLight) {\n                vec3 lightPosition = xyz_type.xyz;\n                return normalize(lightPosition - surfacePosition);\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        vec4 shadowVertexPosition(vec3 position, vec3 normal, mat4 shadowLight, vec4 modelScale, mat4 modelMatrix, mat4 viewMatrix, mat4 projectionMatrix, mat4 sceneProperties) {\n            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);\n            vec3 worldNormal = getWorldNormal(normal, vec4(modelScale.xyz, 1.0), modelMatrix);\n            vec4 xyz_type = shadowLight[0];\n            vec4 rgb_parameter = shadowLight[1];\n            vec3 directionToLight = getDirectionToLight(worldPosition.xyz, xyz_type, rgb_parameter);\n            vec3 offset = vec3(0.0, 0.0, 0.0);\n            float sceneDiameter = sceneProperties[3][1];\n            if (dot(directionToLight, worldNormal) <= 0.0) {\n                offset = -sceneDiameter * directionToLight;\n            } else {\n                offset = -0.001 * sceneDiameter * directionToLight;\n            }\n            vec4 offsetPosition = worldPosition + vec4(offset, 0.0);\n            return projectionMatrix * (viewMatrix * offsetPosition);\n        }\n        \n        void main () {\n            vec3 position = vec3(0.0, 0.0, 0.0);\n            vec3 normal = vec3(0.0, 0.0, 0.0);\n            vec3 tangent = vec3(0.0, 0.0, 0.0);\n            getQuadVertex(int(quadShadowVertex.x), quadVertexPositions, position, normal, tangent);\n            normal *= quadShadowVertex.y;\n            gl_Position = shadowVertexPosition(\n                position,\n                normal,\n                shadowLight,\n                modelScale,\n                modelMatrix,\n                viewMatrix,\n                projectionMatrix,\n                sceneProperties\n            );\n        }\n    ',
	attributes: {quadShadowVertex: 'quadShadowVertex'},
	uniforms: {modelMatrix: 'modelMatrix', modelScale: 'modelScale', projectionMatrix: 'projectionMatrix', quadVertexPositions: 'quadVertexPositions', sceneProperties: 'sceneProperties', shadowLight: 'shadowLight', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$quadShadow = F4(
	function (firstPoint, secondPoint, thirdPoint, fourthPoint) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
			$ianmackenzie$elm_3d_scene$Scene3d$Types$ShadowNode(
				F8(
					function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, shadowLight, settings) {
						return A5(
							$elm_explorations$webgl$WebGL$entityWith,
							A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$shadowSettings, isRightHanded, settings),
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$quadShadowVertex,
							$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$shadowFragment,
							$ianmackenzie$elm_3d_scene$Scene3d$Entity$quadShadowMesh,
							{
								modelMatrix: modelMatrix,
								modelScale: modelScale,
								projectionMatrix: projectionMatrix,
								quadVertexPositions: A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$quadVertexPositions, firstPoint, secondPoint, thirdPoint, fourthPoint),
								sceneProperties: sceneProperties,
								shadowLight: shadowLight,
								viewMatrix: viewMatrix
							});
					})));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$quad = F7(
	function (renderObject, renderShadow, givenMaterial, firstPoint, secondPoint, thirdPoint, fourthPoint) {
		var meshEntity = A5($ianmackenzie$elm_3d_scene$Scene3d$Entity$quadMesh, givenMaterial, firstPoint, secondPoint, thirdPoint, fourthPoint);
		var _v0 = _Utils_Tuple2(renderObject, renderShadow);
		if (_v0.a) {
			if (_v0.b) {
				return $ianmackenzie$elm_3d_scene$Scene3d$Entity$group(
					_List_fromArray(
						[
							meshEntity,
							A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$quadShadow, firstPoint, secondPoint, thirdPoint, fourthPoint)
						]));
			} else {
				return meshEntity;
			}
		} else {
			if (_v0.b) {
				return A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$quadShadow, firstPoint, secondPoint, thirdPoint, fourthPoint);
			} else {
				return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
			}
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$quad = F5(
	function (givenMaterial, p1, p2, p3, p4) {
		return A7($ianmackenzie$elm_3d_scene$Scene3d$Entity$quad, true, false, givenMaterial, p1, p2, p3, p4);
	});
var $avh4$elm_color$Color$RgbaSpace = F4(
	function (a, b, c, d) {
		return {$: 'RgbaSpace', a: a, b: b, c: c, d: d};
	});
var $avh4$elm_color$Color$rgb = F3(
	function (r, g, b) {
		return A4($avh4$elm_color$Color$RgbaSpace, r, g, b, 1.0);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Transformation$rotateAround = F2(
	function (axis, _v0) {
		var angle = _v0.a;
		var p0 = $ianmackenzie$elm_geometry$Point3d$unwrap(
			$ianmackenzie$elm_geometry$Axis3d$originPoint(axis));
		var halfAngle = 0.5 * angle;
		var qw = $elm$core$Basics$cos(halfAngle);
		var sinHalfAngle = $elm$core$Basics$sin(halfAngle);
		var a = $ianmackenzie$elm_geometry$Direction3d$unwrap(
			$ianmackenzie$elm_geometry$Axis3d$direction(axis));
		var qx = a.x * sinHalfAngle;
		var wx = qw * qx;
		var xx = qx * qx;
		var qy = a.y * sinHalfAngle;
		var wy = qw * qy;
		var xy = qx * qy;
		var yy = qy * qy;
		var a22 = 1 - (2 * (xx + yy));
		var qz = a.z * sinHalfAngle;
		var wz = qw * qz;
		var a01 = 2 * (xy - wz);
		var a10 = 2 * (xy + wz);
		var xz = qx * qz;
		var a02 = 2 * (xz + wy);
		var a20 = 2 * (xz - wy);
		var yz = qy * qz;
		var a12 = 2 * (yz - wx);
		var a21 = 2 * (yz + wx);
		var zz = qz * qz;
		var a00 = 1 - (2 * (yy + zz));
		var a11 = 1 - (2 * (xx + zz));
		return {isRightHanded: true, ix: a00, iy: a10, iz: a20, jx: a01, jy: a11, jz: a21, kx: a02, ky: a12, kz: a22, px: ((p0.x - (a00 * p0.x)) - (a01 * p0.y)) - (a02 * p0.z), py: ((p0.y - (a10 * p0.x)) - (a11 * p0.y)) - (a12 * p0.z), pz: ((p0.z - (a20 * p0.x)) - (a21 * p0.y)) - (a22 * p0.z), scale: 1};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$rotateAround = F3(
	function (axis, angle, givenDrawable) {
		return A2(
			$ianmackenzie$elm_3d_scene$Scene3d$Entity$transformBy,
			A2($ianmackenzie$elm_3d_scene$Scene3d$Transformation$rotateAround, axis, angle),
			givenDrawable);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$rotateAround = F3(
	function (axis, angle, entity) {
		return A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$rotateAround, axis, angle, entity);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Transformation$scaleAbout = F2(
	function (point, k) {
		var p = $ianmackenzie$elm_geometry$Point3d$unwrap(point);
		var oneMinusK = 1 - k;
		return {isRightHanded: k >= 0, ix: 1, iy: 0, iz: 0, jx: 0, jy: 1, jz: 0, kx: 0, ky: 0, kz: 1, px: oneMinusK * p.x, py: oneMinusK * p.y, pz: oneMinusK * p.z, scale: k};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$scaleAbout = F3(
	function (centerPoint, scale, givenDrawable) {
		return A2(
			$ianmackenzie$elm_3d_scene$Scene3d$Entity$transformBy,
			A2($ianmackenzie$elm_3d_scene$Scene3d$Transformation$scaleAbout, centerPoint, scale),
			givenDrawable);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$scaleAbout = F3(
	function (centerPoint, scale, entity) {
		return A3($ianmackenzie$elm_3d_scene$Scene3d$Entity$scaleAbout, centerPoint, scale, entity);
	});
var $ianmackenzie$elm_units$Angle$cos = function (_v0) {
	var angle = _v0.a;
	return $elm$core$Basics$cos(angle);
};
var $ianmackenzie$elm_units$Quantity$interpolateFrom = F3(
	function (_v0, _v1, parameter) {
		var start = _v0.a;
		var end = _v1.a;
		return (parameter <= 0.5) ? $ianmackenzie$elm_units$Quantity$Quantity(start + (parameter * (end - start))) : $ianmackenzie$elm_units$Quantity$Quantity(end + ((1 - parameter) * (start - end)));
	});
var $ianmackenzie$elm_units$Quantity$ratio = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v1.a;
		return x / y;
	});
var $ianmackenzie$elm_units$Angle$sin = function (_v0) {
	var angle = _v0.a;
	return $elm$core$Basics$sin(angle);
};
var $ianmackenzie$elm_1d_parameter$Parameter1d$range = F5(
	function (startIndex, index, divisor, _function, accumulated) {
		range:
		while (true) {
			var newValue = _function(index / divisor);
			var newAccumulated = A2($elm$core$List$cons, newValue, accumulated);
			if (_Utils_eq(index, startIndex)) {
				return newAccumulated;
			} else {
				var $temp$startIndex = startIndex,
					$temp$index = index - 1,
					$temp$divisor = divisor,
					$temp$function = _function,
					$temp$accumulated = newAccumulated;
				startIndex = $temp$startIndex;
				index = $temp$index;
				divisor = $temp$divisor;
				_function = $temp$function;
				accumulated = $temp$accumulated;
				continue range;
			}
		}
	});
var $ianmackenzie$elm_1d_parameter$Parameter1d$steps = F2(
	function (n, _function) {
		return (n < 1) ? _List_Nil : A5($ianmackenzie$elm_1d_parameter$Parameter1d$range, 0, n, n, _function, _List_Nil);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$collectSmoothTextured = F2(
	function (_v0, accumulated) {
		var position = _v0.position;
		var normal = _v0.normal;
		var uv = _v0.uv;
		var _v1 = uv;
		var u = _v1.a;
		var v = _v1.b;
		return A2(
			$elm$core$List$cons,
			{
				normal: $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Vector3d$toVec3(normal),
				position: $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Point3d$toVec3(position),
				uv: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, u, v)
			},
			accumulated);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$vertexBoundsHelp = F7(
	function (minX, maxX, minY, maxY, minZ, maxZ, remaining) {
		vertexBoundsHelp:
		while (true) {
			if (remaining.b) {
				var next = remaining.a;
				var rest = remaining.b;
				var z = $elm_explorations$linear_algebra$Math$Vector3$getZ(next.position);
				var y = $elm_explorations$linear_algebra$Math$Vector3$getY(next.position);
				var x = $elm_explorations$linear_algebra$Math$Vector3$getX(next.position);
				var $temp$minX = A2($elm$core$Basics$min, minX, x),
					$temp$maxX = A2($elm$core$Basics$max, maxX, x),
					$temp$minY = A2($elm$core$Basics$min, minY, y),
					$temp$maxY = A2($elm$core$Basics$max, maxY, y),
					$temp$minZ = A2($elm$core$Basics$min, minZ, z),
					$temp$maxZ = A2($elm$core$Basics$max, maxZ, z),
					$temp$remaining = rest;
				minX = $temp$minX;
				maxX = $temp$maxX;
				minY = $temp$minY;
				maxY = $temp$maxY;
				minZ = $temp$minZ;
				maxZ = $temp$maxZ;
				remaining = $temp$remaining;
				continue vertexBoundsHelp;
			} else {
				return $ianmackenzie$elm_geometry$BoundingBox3d$fromExtrema(
					{
						maxX: $ianmackenzie$elm_units$Quantity$Quantity(maxX),
						maxY: $ianmackenzie$elm_units$Quantity$Quantity(maxY),
						maxZ: $ianmackenzie$elm_units$Quantity$Quantity(maxZ),
						minX: $ianmackenzie$elm_units$Quantity$Quantity(minX),
						minY: $ianmackenzie$elm_units$Quantity$Quantity(minY),
						minZ: $ianmackenzie$elm_units$Quantity$Quantity(minZ)
					});
			}
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$vertexBounds = F2(
	function (first, rest) {
		var z = $elm_explorations$linear_algebra$Math$Vector3$getZ(first.position);
		var y = $elm_explorations$linear_algebra$Math$Vector3$getY(first.position);
		var x = $elm_explorations$linear_algebra$Math$Vector3$getX(first.position);
		return A7($ianmackenzie$elm_3d_scene$Scene3d$Mesh$vertexBoundsHelp, x, x, y, y, z, z, rest);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Mesh$texturedFaces = function (givenMesh) {
	var collectedVertices = A3(
		$elm$core$Array$foldr,
		$ianmackenzie$elm_3d_scene$Scene3d$Mesh$collectSmoothTextured,
		_List_Nil,
		$ianmackenzie$elm_triangular_mesh$TriangularMesh$vertices(givenMesh));
	if (!collectedVertices.b) {
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$EmptyMesh;
	} else {
		var first = collectedVertices.a;
		var rest = collectedVertices.b;
		var webGLMesh = A2(
			$elm_explorations$webgl$WebGL$indexedTriangles,
			collectedVertices,
			$ianmackenzie$elm_triangular_mesh$TriangularMesh$faceIndices(givenMesh));
		var bounds = A2($ianmackenzie$elm_3d_scene$Scene3d$Mesh$vertexBounds, first, rest);
		return A4($ianmackenzie$elm_3d_scene$Scene3d$Types$MeshWithNormalsAndUvs, bounds, givenMesh, webGLMesh, $ianmackenzie$elm_3d_scene$Scene3d$Types$KeepBackFaces);
	}
};
var $ianmackenzie$elm_geometry$Direction3d$xyZ = F2(
	function (_v0, _v1) {
		var theta = _v0.a;
		var phi = _v1.a;
		var cosPhi = $elm$core$Basics$cos(phi);
		return $ianmackenzie$elm_geometry$Geometry$Types$Direction3d(
			{
				x: cosPhi * $elm$core$Basics$cos(theta),
				y: cosPhi * $elm$core$Basics$sin(theta),
				z: $elm$core$Basics$sin(phi)
			});
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Primitives$sphere = function () {
	var radius = $ianmackenzie$elm_units$Length$meters(1);
	var n = 72;
	var thetaStartIndices = A2($elm$core$List$range, 0, n - 1);
	var thetaValues = A2(
		$ianmackenzie$elm_1d_parameter$Parameter1d$steps,
		n,
		A2(
			$ianmackenzie$elm_units$Quantity$interpolateFrom,
			$ianmackenzie$elm_units$Quantity$zero,
			$ianmackenzie$elm_units$Angle$turns(1)));
	var m = $elm$core$Basics$ceiling(n / 2);
	var phiStartIndices = A2($elm$core$List$range, 0, m - 1);
	var phiValues = A2(
		$ianmackenzie$elm_1d_parameter$Parameter1d$steps,
		m,
		A2(
			$ianmackenzie$elm_units$Quantity$interpolateFrom,
			$ianmackenzie$elm_units$Angle$degrees(90),
			$ianmackenzie$elm_units$Angle$degrees(-90)));
	var vertices = $elm$core$Array$fromList(
		$elm$core$List$concat(
			A2(
				$elm$core$List$map,
				function (theta) {
					return A2(
						$elm$core$List$map,
						function (phi) {
							return {
								normal: $ianmackenzie$elm_geometry$Direction3d$toVector(
									A2($ianmackenzie$elm_geometry$Direction3d$xyZ, theta, phi)),
								position: A3(
									$ianmackenzie$elm_geometry$Point3d$xyz,
									A2(
										$ianmackenzie$elm_units$Quantity$multiplyBy,
										$ianmackenzie$elm_units$Angle$cos(phi) * $ianmackenzie$elm_units$Angle$cos(theta),
										radius),
									A2(
										$ianmackenzie$elm_units$Quantity$multiplyBy,
										$ianmackenzie$elm_units$Angle$cos(phi) * $ianmackenzie$elm_units$Angle$sin(theta),
										radius),
									A2(
										$ianmackenzie$elm_units$Quantity$multiplyBy,
										$ianmackenzie$elm_units$Angle$sin(phi),
										radius)),
								uv: _Utils_Tuple2(
									A2(
										$ianmackenzie$elm_units$Quantity$ratio,
										theta,
										$ianmackenzie$elm_units$Angle$turns(1)),
									A2(
										$ianmackenzie$elm_units$Quantity$ratio,
										A2(
											$ianmackenzie$elm_units$Quantity$plus,
											$ianmackenzie$elm_units$Angle$degrees(90),
											phi),
										$ianmackenzie$elm_units$Angle$degrees(180)))
							};
						},
						phiValues);
				},
				thetaValues)));
	var linearIndex = F2(
		function (i, j) {
			return (i * (m + 1)) + j;
		});
	var faces = $elm$core$List$concat(
		A2(
			$elm$core$List$map,
			function (i) {
				return $elm$core$List$concat(
					A2(
						$elm$core$List$map,
						function (j) {
							var topRightIndex = A2(linearIndex, i + 1, j);
							var topLeftIndex = A2(linearIndex, i, j);
							var bottomRightIndex = A2(linearIndex, i + 1, j + 1);
							var bottomLeftIndex = A2(linearIndex, i, j + 1);
							return _List_fromArray(
								[
									_Utils_Tuple3(bottomLeftIndex, bottomRightIndex, topRightIndex),
									_Utils_Tuple3(bottomLeftIndex, topRightIndex, topLeftIndex)
								]);
						},
						phiStartIndices));
			},
			thetaStartIndices));
	return $ianmackenzie$elm_3d_scene$Scene3d$Mesh$cullBackFaces(
		$ianmackenzie$elm_3d_scene$Scene3d$Mesh$texturedFaces(
			A2($ianmackenzie$elm_triangular_mesh$TriangularMesh$indexed, vertices, faces)));
}();
var $elm$core$Basics$modBy = _Basics_modBy;
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$numStrips = 72;
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$numOutlineVertices = 2 * $ianmackenzie$elm_3d_scene$Scene3d$Entity$numStrips;
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$buildSphereShadowIndices = F2(
	function (stripIndex, accumulated) {
		buildSphereShadowIndices:
		while (true) {
			var f = $ianmackenzie$elm_3d_scene$Scene3d$Entity$numOutlineVertices + 1;
			var e = A2($elm$core$Basics$modBy, $ianmackenzie$elm_3d_scene$Scene3d$Entity$numOutlineVertices, (2 * stripIndex) + 3);
			var d = A2($elm$core$Basics$modBy, $ianmackenzie$elm_3d_scene$Scene3d$Entity$numOutlineVertices, (2 * stripIndex) + 2);
			var c = (2 * stripIndex) + 1;
			var b = 2 * stripIndex;
			var a = $ianmackenzie$elm_3d_scene$Scene3d$Entity$numOutlineVertices;
			var updated = A2(
				$elm$core$List$cons,
				_Utils_Tuple3(a, b, d),
				A2(
					$elm$core$List$cons,
					_Utils_Tuple3(b, e, d),
					A2(
						$elm$core$List$cons,
						_Utils_Tuple3(b, c, e),
						A2(
							$elm$core$List$cons,
							_Utils_Tuple3(c, f, e),
							accumulated))));
			if (!stripIndex) {
				return updated;
			} else {
				var $temp$stripIndex = stripIndex - 1,
					$temp$accumulated = updated;
				stripIndex = $temp$stripIndex;
				accumulated = $temp$accumulated;
				continue buildSphereShadowIndices;
			}
		}
	});
var $ianmackenzie$elm_float_extra$Float$Extra$interpolateFrom = F3(
	function (start, end, parameter) {
		return (parameter <= 0.5) ? (start + (parameter * (end - start))) : (end + ((1 - parameter) * (start - end)));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$buildSphereShadowVertices = F2(
	function (stripIndex, accumulated) {
		buildSphereShadowVertices:
		while (true) {
			var angle = A3($ianmackenzie$elm_float_extra$Float$Extra$interpolateFrom, 0, 2 * $elm$core$Basics$pi, stripIndex / $ianmackenzie$elm_3d_scene$Scene3d$Entity$numStrips);
			var left = {angle: angle, offsetScale: 0, radiusScale: 1};
			var right = {angle: angle, offsetScale: 1, radiusScale: 1};
			var updated = A2(
				$elm$core$List$cons,
				left,
				A2($elm$core$List$cons, right, accumulated));
			if (!stripIndex) {
				return updated;
			} else {
				var $temp$stripIndex = stripIndex - 1,
					$temp$accumulated = updated;
				stripIndex = $temp$stripIndex;
				accumulated = $temp$accumulated;
				continue buildSphereShadowVertices;
			}
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$sphereShadowMesh = function () {
	var sphereShadowVertices = A2(
		$ianmackenzie$elm_3d_scene$Scene3d$Entity$buildSphereShadowVertices,
		$ianmackenzie$elm_3d_scene$Scene3d$Entity$numStrips - 1,
		_List_fromArray(
			[
				{angle: 0, offsetScale: 0, radiusScale: 0},
				{angle: 0, offsetScale: 1, radiusScale: 0}
			]));
	var sphereShadowIndices = A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$buildSphereShadowIndices, $ianmackenzie$elm_3d_scene$Scene3d$Entity$numStrips - 1, _List_Nil);
	return A2($elm_explorations$webgl$WebGL$indexedTriangles, sphereShadowVertices, sphereShadowIndices);
}();
var $ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$sphereShadowVertex = {
	src: '\n        precision highp float;\n        \n        attribute highp float angle;\n        attribute highp float offsetScale;\n        attribute highp float radiusScale;\n        \n        uniform highp vec4 modelScale;\n        uniform highp mat4 modelMatrix;\n        uniform highp mat4 viewMatrix;\n        uniform highp mat4 projectionMatrix;\n        uniform highp mat4 sceneProperties;\n        uniform highp mat4 shadowLight;\n        \n        const lowp float kDirectionalLight = 1.0;\n        const lowp float kPointLight = 2.0;\n        const lowp float kPerspectiveProjection = 0.0;\n        \n        vec4 getWorldPosition(vec3 modelPosition, vec4 modelScale, mat4 modelMatrix) {\n            vec4 scaledPosition = vec4(modelScale.xyz * modelPosition, 1.0);\n            return modelMatrix * scaledPosition;\n        }\n        \n        vec3 getDirectionToLight(vec3 surfacePosition, vec4 xyz_type, vec4 rgb_parameter) {\n            float lightType = xyz_type.w;\n            if (lightType == kDirectionalLight) {\n                return xyz_type.xyz;\n            } else if (lightType == kPointLight) {\n                vec3 lightPosition = xyz_type.xyz;\n                return normalize(lightPosition - surfacePosition);\n            } else {\n                return vec3(0.0, 0.0, 0.0);\n            }\n        }\n        \n        vec3 perpendicularTo(vec3 d) {\n            float absX = abs(d.x);\n            float absY = abs(d.y);\n            float absZ = abs(d.z);\n            if (absX <= absY) {\n                if (absX <= absZ) {\n                    float scale = 1.0 / length(d.zy);\n                    return vec3(0.0, -d.z * scale, d.y * scale);\n                } else {\n                    float scale = 1.0 / length(d.xy);\n                    return vec3(-d.y * scale, d.x * scale, 0.0);\n                }\n            } else {\n                if (absY <= absZ) {\n                    float scale = 1.0 / length(d.xz);\n                    return vec3(d.z * scale, 0.0, -d.x * scale);\n                } else {\n                    float scale = 1.0 / length(d.xy);\n                    return vec3(-d.y * scale, d.x * scale, 0.0);\n                }\n            }\n        }\n        \n        void main () {\n            vec4 worldCenter = getWorldPosition(vec3(0.0, 0.0, 0.0), modelScale, modelMatrix);\n            vec4 xyz_type = shadowLight[0];\n            vec4 rgb_parameter = shadowLight[1];\n            vec3 zDirection = getDirectionToLight(worldCenter.xyz, xyz_type, rgb_parameter);\n            vec3 xDirection = perpendicularTo(zDirection);\n            vec3 yDirection = cross(zDirection, xDirection);\n            float r = modelScale.x;\n            float adjustedRadius = r;\n            float zOffset = 0.0;\n            if (xyz_type.w == kPointLight) {\n                float distanceToLight = length(xyz_type.xyz - worldCenter.xyz);\n                float rSquared = r * r;\n                zOffset = rSquared / distanceToLight;\n                float zSquared = zOffset * zOffset;\n                adjustedRadius = sqrt(rSquared - zSquared) * radiusScale;\n            }\n            vec3 worldPosition =\n                worldCenter.xyz\n                    + zDirection * zOffset\n                    + xDirection * adjustedRadius * cos(angle)\n                    + yDirection * adjustedRadius * sin(angle);\n            vec3 directionToLight = getDirectionToLight(worldPosition, xyz_type, rgb_parameter);\n            float sceneDiameter = sceneProperties[3][1];\n            vec3 offset = -sceneDiameter * offsetScale * directionToLight;\n            vec4 offsetPosition = vec4(worldPosition + offset, 1.0);\n            gl_Position = projectionMatrix * (viewMatrix * offsetPosition);\n        }\n    ',
	attributes: {angle: 'angle', offsetScale: 'offsetScale', radiusScale: 'radiusScale'},
	uniforms: {modelMatrix: 'modelMatrix', modelScale: 'modelScale', projectionMatrix: 'projectionMatrix', sceneProperties: 'sceneProperties', shadowLight: 'shadowLight', viewMatrix: 'viewMatrix'}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$sphereShadow = function (givenSphere) {
	return $ianmackenzie$elm_3d_scene$Scene3d$Types$Entity(
		$ianmackenzie$elm_3d_scene$Scene3d$Types$ShadowNode(
			F8(
				function (sceneProperties, modelScale, modelMatrix, isRightHanded, viewMatrix, projectionMatrix, shadowLight, settings) {
					return A5(
						$elm_explorations$webgl$WebGL$entityWith,
						A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$shadowSettings, true, settings),
						$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$sphereShadowVertex,
						$ianmackenzie$elm_3d_scene$Scene3d$UnoptimizedShaders$shadowFragment,
						$ianmackenzie$elm_3d_scene$Scene3d$Entity$sphereShadowMesh,
						{
							constantColor: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 1),
							modelMatrix: modelMatrix,
							modelScale: modelScale,
							projectionMatrix: projectionMatrix,
							sceneProperties: sceneProperties,
							shadowLight: shadowLight,
							viewMatrix: viewMatrix
						});
				})));
};
var $ianmackenzie$elm_3d_scene$Scene3d$Transformation$translateBy = function (displacement) {
	var v = $ianmackenzie$elm_geometry$Vector3d$unwrap(displacement);
	return {isRightHanded: true, ix: 1, iy: 0, iz: 0, jx: 0, jy: 1, jz: 0, kx: 0, ky: 0, kz: 1, px: v.x, py: v.y, pz: v.z, scale: 1};
};
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$translateBy = F2(
	function (displacement, givenDrawable) {
		return A2(
			$ianmackenzie$elm_3d_scene$Scene3d$Entity$transformBy,
			$ianmackenzie$elm_3d_scene$Scene3d$Transformation$translateBy(displacement),
			givenDrawable);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Entity$sphere = F4(
	function (renderObject, renderShadow, givenMaterial, givenSphere) {
		var baseEntity = A2($ianmackenzie$elm_3d_scene$Scene3d$Entity$mesh, givenMaterial, $ianmackenzie$elm_3d_scene$Scene3d$Primitives$sphere);
		var untransformedEntity = function () {
			var _v1 = _Utils_Tuple2(renderObject, renderShadow);
			if (_v1.a) {
				if (_v1.b) {
					return $ianmackenzie$elm_3d_scene$Scene3d$Entity$group(
						_List_fromArray(
							[
								baseEntity,
								$ianmackenzie$elm_3d_scene$Scene3d$Entity$sphereShadow(givenSphere)
							]));
				} else {
					return baseEntity;
				}
			} else {
				if (_v1.b) {
					return $ianmackenzie$elm_3d_scene$Scene3d$Entity$sphereShadow(givenSphere);
				} else {
					return $ianmackenzie$elm_3d_scene$Scene3d$Entity$empty;
				}
			}
		}();
		var _v0 = $ianmackenzie$elm_geometry$Sphere3d$radius(givenSphere);
		var r = _v0.a;
		return A2(
			$ianmackenzie$elm_3d_scene$Scene3d$Entity$translateBy,
			A2(
				$ianmackenzie$elm_geometry$Vector3d$from,
				$ianmackenzie$elm_geometry$Point3d$origin,
				$ianmackenzie$elm_geometry$Sphere3d$centerPoint(givenSphere)),
			A2(
				$ianmackenzie$elm_3d_scene$Scene3d$Entity$preScale,
				_Utils_Tuple3(r, r, r),
				untransformedEntity));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$sphere = F2(
	function (givenMaterial, givenSphere) {
		return A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$sphere, true, false, givenMaterial, givenSphere);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$sphereWithShadow = F2(
	function (givenMaterial, givenSphere) {
		return A4($ianmackenzie$elm_3d_scene$Scene3d$Entity$sphere, true, true, givenMaterial, givenSphere);
	});
var $author$project$Main$tetrahedronWithRadius1 = function () {
	var p3 = A3($ianmackenzie$elm_geometry$Point3d$meters, 0, 0, 1);
	var p2 = A3(
		$ianmackenzie$elm_geometry$Point3d$meters,
		-$elm$core$Basics$sqrt(2 / 9),
		-$elm$core$Basics$sqrt(2 / 3),
		(-1) / 3);
	var p1 = A3(
		$ianmackenzie$elm_geometry$Point3d$meters,
		-$elm$core$Basics$sqrt(2 / 9),
		$elm$core$Basics$sqrt(2 / 3),
		(-1) / 3);
	var p0 = A3(
		$ianmackenzie$elm_geometry$Point3d$meters,
		$elm$core$Basics$sqrt(8 / 9),
		0,
		(-1) / 3);
	return $ianmackenzie$elm_3d_scene$Scene3d$Mesh$facets(
		_List_fromArray(
			[
				A3($ianmackenzie$elm_geometry$Triangle3d$from, p0, p1, p2),
				A3($ianmackenzie$elm_geometry$Triangle3d$from, p0, p1, p3),
				A3($ianmackenzie$elm_geometry$Triangle3d$from, p0, p2, p3),
				A3($ianmackenzie$elm_geometry$Triangle3d$from, p1, p2, p3)
			]));
}();
var $author$project$Main$tetrahedronWithRadius1Shadow = $ianmackenzie$elm_3d_scene$Scene3d$Mesh$shadow($author$project$Main$tetrahedronWithRadius1);
var $avh4$elm_color$Color$white = A4($avh4$elm_color$Color$RgbaSpace, 255 / 255, 255 / 255, 255 / 255, 1.0);
var $author$project$Main$bodyToEntity = function (body) {
	var id = $w0rm$elm_physics$Physics$Body$data(body);
	var frame = $w0rm$elm_physics$Physics$Body$frame(body);
	return A2(
		$ianmackenzie$elm_3d_scene$Scene3d$placeIn,
		frame,
		function () {
			switch (id.$) {
				case 'Mouse':
					return A2(
						$ianmackenzie$elm_3d_scene$Scene3d$sphere,
						$ianmackenzie$elm_3d_scene$Scene3d$Material$color(
							A3($avh4$elm_color$Color$rgb, 1, 0.2, 0)),
						$ianmackenzie$elm_geometry$Sphere3d$atOrigin(
							$ianmackenzie$elm_units$Length$millimeters(20)));
				case 'MouseImitation':
					var fixed = id.a;
					return A2(
						$ianmackenzie$elm_3d_scene$Scene3d$sphere,
						$ianmackenzie$elm_3d_scene$Scene3d$Material$color(
							A3($avh4$elm_color$Color$rgb, 1, 0.2, 0)),
						$ianmackenzie$elm_geometry$Sphere3d$atOrigin(
							$ianmackenzie$elm_units$Length$millimeters(27)));
				case 'Player':
					return A3(
						$ianmackenzie$elm_3d_scene$Scene3d$rotateAround,
						$ianmackenzie$elm_geometry$Axis3d$z,
						$ianmackenzie$elm_units$Angle$turns(0.418),
						A3(
							$ianmackenzie$elm_3d_scene$Scene3d$scaleAbout,
							$ianmackenzie$elm_geometry$Point3d$origin,
							0.125,
							A3(
								$ianmackenzie$elm_3d_scene$Scene3d$meshWithShadow,
								$ianmackenzie$elm_3d_scene$Scene3d$Material$color(
									A3($avh4$elm_color$Color$rgb, 1, 0.5, 0.1)),
								$author$project$Main$tetrahedronWithRadius1,
								$author$project$Main$tetrahedronWithRadius1Shadow)));
				case 'PlayerPast':
					return A3(
						$ianmackenzie$elm_3d_scene$Scene3d$rotateAround,
						$ianmackenzie$elm_geometry$Axis3d$z,
						$ianmackenzie$elm_units$Angle$turns(0.418),
						A3(
							$ianmackenzie$elm_3d_scene$Scene3d$scaleAbout,
							$ianmackenzie$elm_geometry$Point3d$origin,
							0.125,
							A3(
								$ianmackenzie$elm_3d_scene$Scene3d$meshWithShadow,
								$ianmackenzie$elm_3d_scene$Scene3d$Material$color(
									A3($avh4$elm_color$Color$rgb, 1, 0.5, 0.1)),
								$author$project$Main$tetrahedronWithRadius1,
								$author$project$Main$tetrahedronWithRadius1Shadow)));
				case 'PlayerImitation':
					return A3(
						$ianmackenzie$elm_3d_scene$Scene3d$scaleAbout,
						$ianmackenzie$elm_geometry$Point3d$origin,
						0.125,
						A3(
							$ianmackenzie$elm_3d_scene$Scene3d$meshWithShadow,
							$ianmackenzie$elm_3d_scene$Scene3d$Material$color(
								A3($avh4$elm_color$Color$rgb, 1, 0.5, 0.1)),
							$author$project$Main$tetrahedronWithRadius1,
							$author$project$Main$tetrahedronWithRadius1Shadow));
				case 'BlockingImmovableWall':
					var dimensions = id.a;
					return A2(
						$ianmackenzie$elm_3d_scene$Scene3d$blockWithShadow,
						$ianmackenzie$elm_3d_scene$Scene3d$Material$color($avh4$elm_color$Color$white),
						$author$project$Main$thickLinearBlock(dimensions));
				case 'DraggableBlock':
					var dimensions = id.a;
					return A2(
						$ianmackenzie$elm_3d_scene$Scene3d$blockWithShadow,
						$ianmackenzie$elm_3d_scene$Scene3d$Material$color(
							A3($avh4$elm_color$Color$rgb, 0.95, 1, 1)),
						$author$project$Main$thickLinearBlock(dimensions));
				case 'DraggableBallWithCubeBehavior':
					var data = id.a;
					return A2(
						$ianmackenzie$elm_3d_scene$Scene3d$sphereWithShadow,
						$ianmackenzie$elm_3d_scene$Scene3d$Material$color(
							A3($avh4$elm_color$Color$rgb, 1, 1, 1)),
						$ianmackenzie$elm_geometry$Sphere3d$atOrigin(data.radius));
				default:
					return A5(
						$ianmackenzie$elm_3d_scene$Scene3d$quad,
						$ianmackenzie$elm_3d_scene$Scene3d$Material$matte(
							A3($avh4$elm_color$Color$rgb, 0, 0, 0)),
						A3($ianmackenzie$elm_geometry$Point3d$meters, -15, -15, 0),
						A3($ianmackenzie$elm_geometry$Point3d$meters, -15, 15, 0),
						A3($ianmackenzie$elm_geometry$Point3d$meters, 15, 15, 0),
						A3($ianmackenzie$elm_geometry$Point3d$meters, 15, -15, 0));
			}
		}());
};
var $ianmackenzie$elm_geometry$Geometry$Types$Point2d = function (a) {
	return {$: 'Point2d', a: a};
};
var $ianmackenzie$elm_geometry$Point2d$pixels = F2(
	function (x, y) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Point2d(
			{x: x, y: y});
	});
var $author$project$Main$decodeMousePosition = A3(
	$elm$json$Json$Decode$map2,
	$ianmackenzie$elm_geometry$Point2d$pixels,
	A2($elm$json$Json$Decode$field, 'offsetX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'offsetY', $elm$json$Json$Decode$float));
var $elm$html$Html$div = _VirtualDom_node('div');
var $ianmackenzie$elm_units$Pixels$float = function (numPixels) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numPixels);
};
var $ianmackenzie$elm_units$Pixels$int = function (numPixels) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numPixels);
};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onMouseUp = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseup',
		$elm$json$Json$Decode$succeed(msg));
};
var $ianmackenzie$elm_units$Pixels$pixels = function (numPixels) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numPixels);
};
var $ianmackenzie$elm_units$Quantity$at = F2(
	function (_v0, _v1) {
		var rateOfChange = _v0.a;
		var independentValue = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(rateOfChange * independentValue);
	});
var $ianmackenzie$elm_geometry$Geometry$Types$Frame2d = function (a) {
	return {$: 'Frame2d', a: a};
};
var $ianmackenzie$elm_geometry$Frame2d$copy = function (_v0) {
	var properties = _v0.a;
	return $ianmackenzie$elm_geometry$Geometry$Types$Frame2d(properties);
};
var $ianmackenzie$elm_geometry$Rectangle2d$axes = function (_v0) {
	var rectangle = _v0.a;
	return $ianmackenzie$elm_geometry$Frame2d$copy(rectangle.axes);
};
var $ianmackenzie$elm_geometry$Rectangle2d$dimensions = function (_v0) {
	var rectangle = _v0.a;
	return rectangle.dimensions;
};
var $ianmackenzie$elm_units$Quantity$divideBy = F2(
	function (divisor, _v0) {
		var value = _v0.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(value / divisor);
	});
var $ianmackenzie$elm_units$Quantity$negate = function (_v0) {
	var value = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(-value);
};
var $ianmackenzie$elm_units$Quantity$per = F2(
	function (_v0, _v1) {
		var independentValue = _v0.a;
		var dependentValue = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(dependentValue / independentValue);
	});
var $ianmackenzie$elm_geometry$Direction3d$placeIn = F2(
	function (_v0, _v1) {
		var frame = _v0.a;
		var d = _v1.a;
		var _v2 = frame.zDirection;
		var k = _v2.a;
		var _v3 = frame.yDirection;
		var j = _v3.a;
		var _v4 = frame.xDirection;
		var i = _v4.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Direction3d(
			{x: ((i.x * d.x) + (j.x * d.y)) + (k.x * d.z), y: ((i.y * d.x) + (j.y * d.y)) + (k.y * d.z), z: ((i.z * d.x) + (j.z * d.y)) + (k.z * d.z)});
	});
var $ianmackenzie$elm_geometry$Point2d$xCoordinateIn = F2(
	function (_v0, _v1) {
		var frame = _v0.a;
		var p = _v1.a;
		var _v2 = frame.originPoint;
		var p0 = _v2.a;
		var _v3 = frame.xDirection;
		var d = _v3.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(((p.x - p0.x) * d.x) + ((p.y - p0.y) * d.y));
	});
var $ianmackenzie$elm_geometry$Vector3d$xyz = F3(
	function (_v0, _v1, _v2) {
		var x = _v0.a;
		var y = _v1.a;
		var z = _v2.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Vector3d(
			{x: x, y: y, z: z});
	});
var $ianmackenzie$elm_geometry$Point3d$xyzIn = F4(
	function (_v0, _v1, _v2, _v3) {
		var frame = _v0.a;
		var x = _v1.a;
		var y = _v2.a;
		var z = _v3.a;
		var _v4 = frame.originPoint;
		var p0 = _v4.a;
		var _v5 = frame.zDirection;
		var k = _v5.a;
		var _v6 = frame.yDirection;
		var j = _v6.a;
		var _v7 = frame.xDirection;
		var i = _v7.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Point3d(
			{x: ((p0.x + (x * i.x)) + (y * j.x)) + (z * k.x), y: ((p0.y + (x * i.y)) + (y * j.y)) + (z * k.y), z: ((p0.z + (x * i.z)) + (y * j.z)) + (z * k.z)});
	});
var $ianmackenzie$elm_geometry$Point2d$yCoordinateIn = F2(
	function (_v0, _v1) {
		var frame = _v0.a;
		var p = _v1.a;
		var _v2 = frame.originPoint;
		var p0 = _v2.a;
		var _v3 = frame.yDirection;
		var d = _v3.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(((p.x - p0.x) * d.x) + ((p.y - p0.y) * d.y));
	});
var $ianmackenzie$elm_3d_camera$Camera3d$ray = F3(
	function (_v0, screen, point) {
		var camera = _v0.a;
		var screenY = A2(
			$ianmackenzie$elm_geometry$Point2d$yCoordinateIn,
			$ianmackenzie$elm_geometry$Rectangle2d$axes(screen),
			point);
		var screenX = A2(
			$ianmackenzie$elm_geometry$Point2d$xCoordinateIn,
			$ianmackenzie$elm_geometry$Rectangle2d$axes(screen),
			point);
		var _v1 = camera.viewpoint;
		var viewpointFrame = _v1.a;
		var _v2 = $ianmackenzie$elm_geometry$Rectangle2d$dimensions(screen);
		var screenWidth = _v2.a;
		var screenHeight = _v2.b;
		var _v3 = camera.projection;
		if (_v3.$ === 'Perspective') {
			var frustumSlope = _v3.a;
			var screenZ = $ianmackenzie$elm_units$Quantity$negate(
				A2(
					$ianmackenzie$elm_units$Quantity$divideBy,
					frustumSlope,
					A2($ianmackenzie$elm_units$Quantity$multiplyBy, 0.5, screenHeight)));
			var direction = A2(
				$ianmackenzie$elm_geometry$Direction3d$placeIn,
				viewpointFrame,
				A2(
					$elm$core$Maybe$withDefault,
					$ianmackenzie$elm_geometry$Direction3d$negativeZ,
					$ianmackenzie$elm_geometry$Vector3d$direction(
						A3($ianmackenzie$elm_geometry$Vector3d$xyz, screenX, screenY, screenZ))));
			return A2(
				$ianmackenzie$elm_geometry$Axis3d$through,
				$ianmackenzie$elm_3d_camera$Viewpoint3d$eyePoint(camera.viewpoint),
				direction);
		} else {
			var viewpointHeight = _v3.a;
			var resolution = A2($ianmackenzie$elm_units$Quantity$per, screenHeight, viewpointHeight);
			var origin = A4(
				$ianmackenzie$elm_geometry$Point3d$xyzIn,
				viewpointFrame,
				A2($ianmackenzie$elm_units$Quantity$at, resolution, screenX),
				A2($ianmackenzie$elm_units$Quantity$at, resolution, screenY),
				$ianmackenzie$elm_units$Quantity$zero);
			return A2(
				$ianmackenzie$elm_geometry$Axis3d$through,
				origin,
				$ianmackenzie$elm_3d_camera$Viewpoint3d$viewDirection(camera.viewpoint));
		}
	});
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $ianmackenzie$elm_3d_scene$Scene3d$Light$CastsShadows = function (a) {
	return {$: 'CastsShadows', a: a};
};
var $ianmackenzie$elm_3d_scene$Scene3d$Light$castsShadows = function (flag) {
	return $ianmackenzie$elm_3d_scene$Scene3d$Light$CastsShadows(flag);
};
var $elm_explorations$webgl$WebGL$Internal$Alpha = function (a) {
	return {$: 'Alpha', a: a};
};
var $elm_explorations$webgl$WebGL$alpha = $elm_explorations$webgl$WebGL$Internal$Alpha;
var $elm_explorations$webgl$WebGL$Internal$Antialias = {$: 'Antialias'};
var $elm_explorations$webgl$WebGL$antialias = $elm_explorations$webgl$WebGL$Internal$Antialias;
var $elm_explorations$webgl$WebGL$Internal$ClearColor = F4(
	function (a, b, c, d) {
		return {$: 'ClearColor', a: a, b: b, c: c, d: d};
	});
var $elm_explorations$webgl$WebGL$clearColor = $elm_explorations$webgl$WebGL$Internal$ClearColor;
var $elm_explorations$webgl$WebGL$Internal$Depth = function (a) {
	return {$: 'Depth', a: a};
};
var $elm_explorations$webgl$WebGL$depth = $elm_explorations$webgl$WebGL$Internal$Depth;
var $elm$html$Html$Attributes$height = function (n) {
	return A2(
		_VirtualDom_attribute,
		'height',
		$elm$core$String$fromInt(n));
};
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $elm_explorations$webgl$WebGL$Internal$Stencil = function (a) {
	return {$: 'Stencil', a: a};
};
var $elm_explorations$webgl$WebGL$stencil = $elm_explorations$webgl$WebGL$Internal$Stencil;
var $elm$core$String$fromFloat = _String_fromNumber;
var $avh4$elm_color$Color$toCssString = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	var roundTo = function (x) {
		return $elm$core$Basics$round(x * 1000) / 1000;
	};
	var pct = function (x) {
		return $elm$core$Basics$round(x * 10000) / 100;
	};
	return $elm$core$String$concat(
		_List_fromArray(
			[
				'rgba(',
				$elm$core$String$fromFloat(
				pct(r)),
				'%,',
				$elm$core$String$fromFloat(
				pct(g)),
				'%,',
				$elm$core$String$fromFloat(
				pct(b)),
				'%,',
				$elm$core$String$fromFloat(
				roundTo(a)),
				')'
			]));
};
var $elm_explorations$webgl$WebGL$toHtmlWith = F3(
	function (options, attributes, entities) {
		return A3(_WebGL_toHtml, options, attributes, entities);
	});
var $ianmackenzie$elm_units$Pixels$toInt = function (_v0) {
	var numPixels = _v0.a;
	return numPixels;
};
var $ianmackenzie$elm_3d_scene$Scene3d$allLightsEnabled = A4($elm_explorations$linear_algebra$Math$Vector4$vec4, 1, 1, 1, 1);
var $ianmackenzie$elm_3d_scene$Scene3d$call = F3(
	function (renderPasses, lights, settings) {
		return A2(
			$elm$core$List$map,
			function (renderPass) {
				return A2(renderPass, lights, settings);
			},
			renderPasses);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Types$CieXyz = F3(
	function (a, b, c) {
		return {$: 'CieXyz', a: a, b: b, c: c};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$chromaticityToCieXyz = F2(
	function (_v0, _v1) {
		var intensity = _v0.a;
		var x = _v1.a.x;
		var y = _v1.a.y;
		return A3($ianmackenzie$elm_3d_scene$Scene3d$Types$CieXyz, (intensity * x) / y, intensity, (intensity * ((1 - x) - y)) / y);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$cieXyzToLinearRgb = function (_v0) {
	var bigX = _v0.a;
	var bigY = _v0.b;
	var bigZ = _v0.c;
	return $ianmackenzie$elm_3d_scene$Scene3d$Types$LinearRgb(
		A3($elm_explorations$linear_algebra$Math$Vector3$vec3, ((3.2406 * bigX) - (1.5372 * bigY)) - (0.4986 * bigZ), (((-0.9689) * bigX) + (1.8758 * bigY)) + (0.0415 * bigZ), ((0.0557 * bigX) - (0.204 * bigY)) + (1.057 * bigZ)));
};
var $ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$chromaticityToLinearRgb = F2(
	function (intensity, chromaticity) {
		return $ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$cieXyzToLinearRgb(
			A2($ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$chromaticityToCieXyz, intensity, chromaticity));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Transformation$modelMatrix = function (transformation) {
	return $elm_explorations$linear_algebra$Math$Matrix4$fromRecord(
		{m11: transformation.ix, m12: transformation.jx, m13: transformation.kx, m14: transformation.px, m21: transformation.iy, m22: transformation.jy, m23: transformation.ky, m24: transformation.py, m31: transformation.iz, m32: transformation.jz, m33: transformation.kz, m34: transformation.pz, m41: 0, m42: 0, m43: 0, m44: 1});
};
var $ianmackenzie$elm_3d_scene$Scene3d$createRenderPass = F5(
	function (sceneProperties, viewMatrix, projectionMatrix, transformation, drawFunction) {
		var normalSign = transformation.isRightHanded ? 1 : (-1);
		var modelScale = A4($elm_explorations$linear_algebra$Math$Vector4$vec4, transformation.scale, transformation.scale, transformation.scale, normalSign);
		return A6(
			drawFunction,
			sceneProperties,
			modelScale,
			$ianmackenzie$elm_3d_scene$Scene3d$Transformation$modelMatrix(transformation),
			transformation.isRightHanded,
			viewMatrix,
			projectionMatrix);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$collectRenderPasses = F6(
	function (sceneProperties, viewMatrix, projectionMatrix, currentTransformation, node, accumulated) {
		collectRenderPasses:
		while (true) {
			switch (node.$) {
				case 'EmptyNode':
					return accumulated;
				case 'Transformed':
					var transformation = node.a;
					var childNode = node.b;
					var $temp$sceneProperties = sceneProperties,
						$temp$viewMatrix = viewMatrix,
						$temp$projectionMatrix = projectionMatrix,
						$temp$currentTransformation = A2($ianmackenzie$elm_3d_scene$Scene3d$Transformation$compose, transformation, currentTransformation),
						$temp$node = childNode,
						$temp$accumulated = accumulated;
					sceneProperties = $temp$sceneProperties;
					viewMatrix = $temp$viewMatrix;
					projectionMatrix = $temp$projectionMatrix;
					currentTransformation = $temp$currentTransformation;
					node = $temp$node;
					accumulated = $temp$accumulated;
					continue collectRenderPasses;
				case 'MeshNode':
					var meshDrawFunction = node.b;
					var updatedMeshes = A2(
						$elm$core$List$cons,
						A5($ianmackenzie$elm_3d_scene$Scene3d$createRenderPass, sceneProperties, viewMatrix, projectionMatrix, currentTransformation, meshDrawFunction),
						accumulated.meshes);
					return {meshes: updatedMeshes, points: accumulated.points, shadows: accumulated.shadows};
				case 'PointNode':
					var pointDrawFunction = node.b;
					var updatedPoints = A2(
						$elm$core$List$cons,
						A5($ianmackenzie$elm_3d_scene$Scene3d$createRenderPass, sceneProperties, viewMatrix, projectionMatrix, currentTransformation, pointDrawFunction),
						accumulated.points);
					return {meshes: accumulated.meshes, points: updatedPoints, shadows: accumulated.shadows};
				case 'ShadowNode':
					var shadowDrawFunction = node.a;
					var updatedShadows = A2(
						$elm$core$List$cons,
						A5($ianmackenzie$elm_3d_scene$Scene3d$createRenderPass, sceneProperties, viewMatrix, projectionMatrix, currentTransformation, shadowDrawFunction),
						accumulated.shadows);
					return {meshes: accumulated.meshes, points: accumulated.points, shadows: updatedShadows};
				default:
					var childNodes = node.a;
					return A3(
						$elm$core$List$foldl,
						A4($ianmackenzie$elm_3d_scene$Scene3d$collectRenderPasses, sceneProperties, viewMatrix, projectionMatrix, currentTransformation),
						accumulated,
						childNodes);
			}
		}
	});
var $elm_explorations$webgl$WebGL$Internal$ColorMask = F4(
	function (a, b, c, d) {
		return {$: 'ColorMask', a: a, b: b, c: c, d: d};
	});
var $elm_explorations$webgl$WebGL$Settings$colorMask = $elm_explorations$webgl$WebGL$Internal$ColorMask;
var $elm_explorations$webgl$WebGL$Internal$DepthTest = F4(
	function (a, b, c, d) {
		return {$: 'DepthTest', a: a, b: b, c: c, d: d};
	});
var $elm_explorations$webgl$WebGL$Settings$DepthTest$greaterOrEqual = function (_v0) {
	var write = _v0.write;
	var near = _v0.near;
	var far = _v0.far;
	return A4($elm_explorations$webgl$WebGL$Internal$DepthTest, 518, write, near, far);
};
var $elm_explorations$webgl$WebGL$Internal$PolygonOffset = F2(
	function (a, b) {
		return {$: 'PolygonOffset', a: a, b: b};
	});
var $elm_explorations$webgl$WebGL$Settings$polygonOffset = $elm_explorations$webgl$WebGL$Internal$PolygonOffset;
var $ianmackenzie$elm_3d_scene$Scene3d$createShadowStencil = _List_fromArray(
	[
		$elm_explorations$webgl$WebGL$Settings$DepthTest$greaterOrEqual(
		{far: 1, near: 0, write: false}),
		A4($elm_explorations$webgl$WebGL$Settings$colorMask, false, false, false, false),
		A2($elm_explorations$webgl$WebGL$Settings$polygonOffset, 0.0, 1.0)
	]);
var $ianmackenzie$elm_3d_scene$Scene3d$initialStencilCount = 8;
var $ianmackenzie$elm_3d_scene$Scene3d$lowerFourBits = 15;
var $elm_explorations$webgl$WebGL$Settings$StencilTest$replace = $elm_explorations$webgl$WebGL$Settings$StencilTest$Operation(7681);
var $ianmackenzie$elm_3d_scene$Scene3d$dummyFragmentShader = {
	src: '\n        precision lowp float;\n\n        void main() {\n            gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);\n        }\n    ',
	attributes: {},
	uniforms: {}
};
var $elm_explorations$webgl$WebGL$triangleStrip = $elm_explorations$webgl$WebGL$Mesh1(
	{elemSize: 1, indexSize: 0, mode: 5});
var $ianmackenzie$elm_3d_scene$Scene3d$fullScreenQuadMesh = $elm_explorations$webgl$WebGL$triangleStrip(
	_List_fromArray(
		[
			{
			position: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, -1, -1)
		},
			{
			position: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, 1, -1)
		},
			{
			position: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, -1, 1)
		},
			{
			position: A2($elm_explorations$linear_algebra$Math$Vector2$vec2, 1, 1)
		}
		]));
var $ianmackenzie$elm_3d_scene$Scene3d$fullScreenQuadVertexShader = {
	src: '\n        precision lowp float;\n\n        attribute vec2 position;\n\n        void main() {\n            gl_Position = vec4(position, 0.0, 1.0);\n        }\n    ',
	attributes: {position: 'position'},
	uniforms: {}
};
var $elm_explorations$webgl$WebGL$Settings$StencilTest$test = function (stencilTest) {
	return A3(
		$elm_explorations$webgl$WebGL$Settings$StencilTest$testSeparate,
		{mask: stencilTest.mask, ref: stencilTest.ref, writeMask: stencilTest.writeMask},
		{fail: stencilTest.fail, test: stencilTest.test, zfail: stencilTest.zfail, zpass: stencilTest.zpass},
		{fail: stencilTest.fail, test: stencilTest.test, zfail: stencilTest.zfail, zpass: stencilTest.zpass});
};
var $ianmackenzie$elm_3d_scene$Scene3d$updateStencil = function (test) {
	return A5(
		$elm_explorations$webgl$WebGL$entityWith,
		_List_fromArray(
			[
				$elm_explorations$webgl$WebGL$Settings$StencilTest$test(test),
				A4($elm_explorations$webgl$WebGL$Settings$colorMask, false, false, false, false)
			]),
		$ianmackenzie$elm_3d_scene$Scene3d$fullScreenQuadVertexShader,
		$ianmackenzie$elm_3d_scene$Scene3d$dummyFragmentShader,
		$ianmackenzie$elm_3d_scene$Scene3d$fullScreenQuadMesh,
		{});
};
var $ianmackenzie$elm_3d_scene$Scene3d$resetStencil = $ianmackenzie$elm_3d_scene$Scene3d$updateStencil(
	{fail: $elm_explorations$webgl$WebGL$Settings$StencilTest$replace, mask: 0, ref: $ianmackenzie$elm_3d_scene$Scene3d$initialStencilCount, test: $elm_explorations$webgl$WebGL$Settings$StencilTest$always, writeMask: $ianmackenzie$elm_3d_scene$Scene3d$lowerFourBits, zfail: $elm_explorations$webgl$WebGL$Settings$StencilTest$replace, zpass: $elm_explorations$webgl$WebGL$Settings$StencilTest$replace});
var $elm_explorations$webgl$WebGL$Settings$StencilTest$greater = $elm_explorations$webgl$WebGL$Settings$StencilTest$Test(516);
var $elm_explorations$webgl$WebGL$Settings$StencilTest$invert = $elm_explorations$webgl$WebGL$Settings$StencilTest$Operation(5386);
var $ianmackenzie$elm_3d_scene$Scene3d$singleLightMask = function (index) {
	return A2($elm$core$Basics$pow, 2, index + 4);
};
var $ianmackenzie$elm_3d_scene$Scene3d$storeStencilValue = function (lightIndex) {
	return $ianmackenzie$elm_3d_scene$Scene3d$updateStencil(
		{
			fail: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep,
			mask: $ianmackenzie$elm_3d_scene$Scene3d$lowerFourBits,
			ref: $ianmackenzie$elm_3d_scene$Scene3d$initialStencilCount,
			test: $elm_explorations$webgl$WebGL$Settings$StencilTest$greater,
			writeMask: $ianmackenzie$elm_3d_scene$Scene3d$singleLightMask(lightIndex),
			zfail: $elm_explorations$webgl$WebGL$Settings$StencilTest$invert,
			zpass: $elm_explorations$webgl$WebGL$Settings$StencilTest$invert
		});
};
var $ianmackenzie$elm_3d_scene$Scene3d$createShadow = F3(
	function (shadowRenderPasses, lightIndex, lightMatrix) {
		return $elm$core$List$concat(
			_List_fromArray(
				[
					A3($ianmackenzie$elm_3d_scene$Scene3d$call, shadowRenderPasses, lightMatrix, $ianmackenzie$elm_3d_scene$Scene3d$createShadowStencil),
					_List_fromArray(
					[
						$ianmackenzie$elm_3d_scene$Scene3d$storeStencilValue(lightIndex),
						$ianmackenzie$elm_3d_scene$Scene3d$resetStencil
					])
				]));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$createShadows = F2(
	function (shadowRenderPasses, shadowCasters) {
		return $elm$core$List$concat(
			A2(
				$elm$core$List$indexedMap,
				$ianmackenzie$elm_3d_scene$Scene3d$createShadow(shadowRenderPasses),
				shadowCasters));
	});
var $elm_explorations$webgl$WebGL$Settings$DepthTest$less = function (_v0) {
	var write = _v0.write;
	var near = _v0.near;
	var far = _v0.far;
	return A4($elm_explorations$webgl$WebGL$Internal$DepthTest, 513, write, near, far);
};
var $elm_explorations$webgl$WebGL$Settings$DepthTest$default = $elm_explorations$webgl$WebGL$Settings$DepthTest$less(
	{far: 1, near: 0, write: true});
var $elm_explorations$webgl$WebGL$Internal$Blend = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return {$: 'Blend', a: a, b: b, c: c, d: d, e: e, f: f, g: g, h: h, i: i, j: j};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $elm_explorations$webgl$WebGL$Settings$Blend$custom = function (_v0) {
	var r = _v0.r;
	var g = _v0.g;
	var b = _v0.b;
	var a = _v0.a;
	var color = _v0.color;
	var alpha = _v0.alpha;
	var expand = F2(
		function (_v1, _v2) {
			var eq1 = _v1.a;
			var f11 = _v1.b;
			var f12 = _v1.c;
			var eq2 = _v2.a;
			var f21 = _v2.b;
			var f22 = _v2.c;
			return $elm_explorations$webgl$WebGL$Internal$Blend(eq1)(f11)(f12)(eq2)(f21)(f22)(r)(g)(b)(a);
		});
	return A2(expand, color, alpha);
};
var $elm_explorations$webgl$WebGL$Settings$Blend$Blender = F3(
	function (a, b, c) {
		return {$: 'Blender', a: a, b: b, c: c};
	});
var $elm_explorations$webgl$WebGL$Settings$Blend$customAdd = F2(
	function (_v0, _v1) {
		var factor1 = _v0.a;
		var factor2 = _v1.a;
		return A3($elm_explorations$webgl$WebGL$Settings$Blend$Blender, 32774, factor1, factor2);
	});
var $elm_explorations$webgl$WebGL$Settings$Blend$Factor = function (a) {
	return {$: 'Factor', a: a};
};
var $elm_explorations$webgl$WebGL$Settings$Blend$one = $elm_explorations$webgl$WebGL$Settings$Blend$Factor(1);
var $elm_explorations$webgl$WebGL$Settings$Blend$oneMinusSrcAlpha = $elm_explorations$webgl$WebGL$Settings$Blend$Factor(771);
var $elm_explorations$webgl$WebGL$Settings$Blend$srcAlpha = $elm_explorations$webgl$WebGL$Settings$Blend$Factor(770);
var $ianmackenzie$elm_3d_scene$Scene3d$defaultBlend = $elm_explorations$webgl$WebGL$Settings$Blend$custom(
	{
		a: 0,
		alpha: A2($elm_explorations$webgl$WebGL$Settings$Blend$customAdd, $elm_explorations$webgl$WebGL$Settings$Blend$one, $elm_explorations$webgl$WebGL$Settings$Blend$oneMinusSrcAlpha),
		b: 0,
		color: A2($elm_explorations$webgl$WebGL$Settings$Blend$customAdd, $elm_explorations$webgl$WebGL$Settings$Blend$srcAlpha, $elm_explorations$webgl$WebGL$Settings$Blend$oneMinusSrcAlpha),
		g: 0,
		r: 0
	});
var $ianmackenzie$elm_3d_scene$Scene3d$depthTestDefault = _List_fromArray(
	[$elm_explorations$webgl$WebGL$Settings$DepthTest$default, $ianmackenzie$elm_3d_scene$Scene3d$defaultBlend]);
var $ianmackenzie$elm_geometry$Point3d$unsafe = function (givenCoordinates) {
	return $ianmackenzie$elm_geometry$Geometry$Types$Point3d(givenCoordinates);
};
var $ianmackenzie$elm_3d_scene$Scene3d$Transformation$placementFrame = function (transformation) {
	return $ianmackenzie$elm_geometry$Frame3d$unsafe(
		{
			originPoint: $ianmackenzie$elm_geometry$Point3d$unsafe(
				{x: transformation.px, y: transformation.py, z: transformation.pz}),
			xDirection: $ianmackenzie$elm_geometry$Direction3d$unsafe(
				{x: transformation.ix, y: transformation.iy, z: transformation.iz}),
			yDirection: $ianmackenzie$elm_geometry$Direction3d$unsafe(
				{x: transformation.jx, y: transformation.jy, z: transformation.jz}),
			zDirection: $ianmackenzie$elm_geometry$Direction3d$unsafe(
				{x: transformation.kx, y: transformation.ky, z: transformation.kz})
		});
};
var $ianmackenzie$elm_geometry$Direction3d$relativeTo = F2(
	function (_v0, _v1) {
		var frame = _v0.a;
		var d = _v1.a;
		var _v2 = frame.zDirection;
		var k = _v2.a;
		var _v3 = frame.yDirection;
		var j = _v3.a;
		var _v4 = frame.xDirection;
		var i = _v4.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Direction3d(
			{x: ((d.x * i.x) + (d.y * i.y)) + (d.z * i.z), y: ((d.x * j.x) + (d.y * j.y)) + (d.z * j.z), z: ((d.x * k.x) + (d.y * k.y)) + (d.z * k.z)});
	});
var $ianmackenzie$elm_geometry$Point3d$relativeTo = F2(
	function (_v0, _v1) {
		var frame = _v0.a;
		var p = _v1.a;
		var _v2 = frame.originPoint;
		var p0 = _v2.a;
		var deltaX = p.x - p0.x;
		var deltaY = p.y - p0.y;
		var deltaZ = p.z - p0.z;
		var _v3 = frame.zDirection;
		var k = _v3.a;
		var _v4 = frame.yDirection;
		var j = _v4.a;
		var _v5 = frame.xDirection;
		var i = _v5.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Point3d(
			{x: ((deltaX * i.x) + (deltaY * i.y)) + (deltaZ * i.z), y: ((deltaX * j.x) + (deltaY * j.y)) + (deltaZ * j.z), z: ((deltaX * k.x) + (deltaY * k.y)) + (deltaZ * k.z)});
	});
var $ianmackenzie$elm_geometry$Frame3d$relativeTo = F2(
	function (otherFrame, frame) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Frame3d(
			{
				originPoint: A2(
					$ianmackenzie$elm_geometry$Point3d$relativeTo,
					otherFrame,
					$ianmackenzie$elm_geometry$Frame3d$originPoint(frame)),
				xDirection: A2(
					$ianmackenzie$elm_geometry$Direction3d$relativeTo,
					otherFrame,
					$ianmackenzie$elm_geometry$Frame3d$xDirection(frame)),
				yDirection: A2(
					$ianmackenzie$elm_geometry$Direction3d$relativeTo,
					otherFrame,
					$ianmackenzie$elm_geometry$Frame3d$yDirection(frame)),
				zDirection: A2(
					$ianmackenzie$elm_geometry$Direction3d$relativeTo,
					otherFrame,
					$ianmackenzie$elm_geometry$Frame3d$zDirection(frame))
			});
	});
var $ianmackenzie$elm_geometry$BoundingBox3d$union = F2(
	function (firstBox, secondBox) {
		var _v0 = secondBox;
		var b2 = _v0.a;
		var _v1 = firstBox;
		var b1 = _v1.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$BoundingBox3d(
			{
				maxX: A2($elm$core$Basics$max, b1.maxX, b2.maxX),
				maxY: A2($elm$core$Basics$max, b1.maxY, b2.maxY),
				maxZ: A2($elm$core$Basics$max, b1.maxZ, b2.maxZ),
				minX: A2($elm$core$Basics$min, b1.minX, b2.minX),
				minY: A2($elm$core$Basics$min, b1.minY, b2.minY),
				minZ: A2($elm$core$Basics$min, b1.minZ, b2.minZ)
			});
	});
var $ianmackenzie$elm_geometry$BoundingBox3d$withDimensions = F2(
	function (givenDimensions, givenCenterPoint) {
		var _v0 = givenCenterPoint;
		var x = _v0.a.x;
		var y = _v0.a.y;
		var z = _v0.a.z;
		var _v1 = givenDimensions;
		var dx = _v1.a.a;
		var dy = _v1.b.a;
		var dz = _v1.c.a;
		var halfDx = $elm$core$Basics$abs(dx) / 2;
		var halfDy = $elm$core$Basics$abs(dy) / 2;
		var halfDz = $elm$core$Basics$abs(dz) / 2;
		return $ianmackenzie$elm_geometry$Geometry$Types$BoundingBox3d(
			{maxX: x + halfDx, maxY: y + halfDy, maxZ: z + halfDz, minX: x - halfDx, minY: y - halfDy, minZ: z - halfDz});
	});
var $ianmackenzie$elm_3d_scene$Scene3d$updateViewBounds = F4(
	function (viewFrame, scale, modelBounds, current) {
		var originalCenter = modelBounds.centerPoint;
		var modelZDimension = (2 * modelBounds.halfZ) * scale;
		var modelYDimension = (2 * modelBounds.halfY) * scale;
		var modelXDimension = (2 * modelBounds.halfX) * scale;
		var modelCenterZ = originalCenter.z * scale;
		var modelCenterY = originalCenter.y * scale;
		var modelCenterX = originalCenter.x * scale;
		var k = $ianmackenzie$elm_geometry$Direction3d$unwrap(
			$ianmackenzie$elm_geometry$Frame3d$zDirection(viewFrame));
		var zDimension = ($elm$core$Basics$abs(modelXDimension * k.x) + $elm$core$Basics$abs(modelYDimension * k.y)) + $elm$core$Basics$abs(modelZDimension * k.z);
		var j = $ianmackenzie$elm_geometry$Direction3d$unwrap(
			$ianmackenzie$elm_geometry$Frame3d$yDirection(viewFrame));
		var yDimension = ($elm$core$Basics$abs(modelXDimension * j.x) + $elm$core$Basics$abs(modelYDimension * j.y)) + $elm$core$Basics$abs(modelZDimension * j.z);
		var i = $ianmackenzie$elm_geometry$Direction3d$unwrap(
			$ianmackenzie$elm_geometry$Frame3d$xDirection(viewFrame));
		var xDimension = ($elm$core$Basics$abs(modelXDimension * i.x) + $elm$core$Basics$abs(modelYDimension * i.y)) + $elm$core$Basics$abs(modelZDimension * i.z);
		var nodeBounds = A2(
			$ianmackenzie$elm_geometry$BoundingBox3d$withDimensions,
			_Utils_Tuple3(
				$ianmackenzie$elm_units$Quantity$Quantity(xDimension),
				$ianmackenzie$elm_units$Quantity$Quantity(yDimension),
				$ianmackenzie$elm_units$Quantity$Quantity(zDimension)),
			A2(
				$ianmackenzie$elm_geometry$Point3d$relativeTo,
				viewFrame,
				A3($ianmackenzie$elm_geometry$Point3d$meters, modelCenterX, modelCenterY, modelCenterZ)));
		if (current.$ === 'Just') {
			var currentBounds = current.a;
			return $elm$core$Maybe$Just(
				A2($ianmackenzie$elm_geometry$BoundingBox3d$union, currentBounds, nodeBounds));
		} else {
			return $elm$core$Maybe$Just(nodeBounds);
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$getViewBounds = F4(
	function (viewFrame, scale, current, nodes) {
		getViewBounds:
		while (true) {
			if (nodes.b) {
				var first = nodes.a;
				var rest = nodes.b;
				switch (first.$) {
					case 'EmptyNode':
						var $temp$viewFrame = viewFrame,
							$temp$scale = scale,
							$temp$current = current,
							$temp$nodes = rest;
						viewFrame = $temp$viewFrame;
						scale = $temp$scale;
						current = $temp$current;
						nodes = $temp$nodes;
						continue getViewBounds;
					case 'MeshNode':
						var modelBounds = first.a;
						var updated = A4($ianmackenzie$elm_3d_scene$Scene3d$updateViewBounds, viewFrame, scale, modelBounds, current);
						var $temp$viewFrame = viewFrame,
							$temp$scale = scale,
							$temp$current = updated,
							$temp$nodes = rest;
						viewFrame = $temp$viewFrame;
						scale = $temp$scale;
						current = $temp$current;
						nodes = $temp$nodes;
						continue getViewBounds;
					case 'ShadowNode':
						var $temp$viewFrame = viewFrame,
							$temp$scale = scale,
							$temp$current = current,
							$temp$nodes = rest;
						viewFrame = $temp$viewFrame;
						scale = $temp$scale;
						current = $temp$current;
						nodes = $temp$nodes;
						continue getViewBounds;
					case 'PointNode':
						var modelBounds = first.a;
						var updated = A4($ianmackenzie$elm_3d_scene$Scene3d$updateViewBounds, viewFrame, scale, modelBounds, current);
						var $temp$viewFrame = viewFrame,
							$temp$scale = scale,
							$temp$current = updated,
							$temp$nodes = rest;
						viewFrame = $temp$viewFrame;
						scale = $temp$scale;
						current = $temp$current;
						nodes = $temp$nodes;
						continue getViewBounds;
					case 'Group':
						var childNodes = first.a;
						var $temp$viewFrame = viewFrame,
							$temp$scale = scale,
							$temp$current = A4($ianmackenzie$elm_3d_scene$Scene3d$getViewBounds, viewFrame, scale, current, childNodes),
							$temp$nodes = rest;
						viewFrame = $temp$viewFrame;
						scale = $temp$scale;
						current = $temp$current;
						nodes = $temp$nodes;
						continue getViewBounds;
					default:
						var transformation = first.a;
						var childNode = first.b;
						var localViewFrame = A2(
							$ianmackenzie$elm_geometry$Frame3d$relativeTo,
							$ianmackenzie$elm_3d_scene$Scene3d$Transformation$placementFrame(transformation),
							viewFrame);
						var localScale = scale * transformation.scale;
						var $temp$viewFrame = viewFrame,
							$temp$scale = scale,
							$temp$current = A4(
							$ianmackenzie$elm_3d_scene$Scene3d$getViewBounds,
							localViewFrame,
							localScale,
							current,
							_List_fromArray(
								[childNode])),
							$temp$nodes = rest;
						viewFrame = $temp$viewFrame;
						scale = $temp$scale;
						current = $temp$current;
						nodes = $temp$nodes;
						continue getViewBounds;
				}
			} else {
				return current;
			}
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Transformation$identity = {isRightHanded: true, ix: 1, iy: 0, iz: 0, jx: 0, jy: 1, jz: 0, kx: 0, ky: 0, kz: 1, px: 0, py: 0, pz: 0, scale: 1};
var $ianmackenzie$elm_3d_scene$Scene3d$initStencil = $ianmackenzie$elm_3d_scene$Scene3d$updateStencil(
	{fail: $elm_explorations$webgl$WebGL$Settings$StencilTest$replace, mask: 0, ref: $ianmackenzie$elm_3d_scene$Scene3d$initialStencilCount, test: $elm_explorations$webgl$WebGL$Settings$StencilTest$always, writeMask: 255, zfail: $elm_explorations$webgl$WebGL$Settings$StencilTest$replace, zpass: $elm_explorations$webgl$WebGL$Settings$StencilTest$replace});
var $ianmackenzie$elm_3d_scene$Scene3d$Types$Light = function (a) {
	return {$: 'Light', a: a};
};
var $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled = $ianmackenzie$elm_3d_scene$Scene3d$Types$Light(
	{b: 0, castsShadows: false, g: 0, parameter: 0, r: 0, type_: 0, x: 0, y: 0, z: 0});
var $ianmackenzie$elm_3d_scene$Scene3d$lightPair = F2(
	function (_v0, _v1) {
		var first = _v0.a;
		var second = _v1.a;
		return $elm_explorations$linear_algebra$Math$Matrix4$fromRecord(
			{m11: first.x, m12: first.r, m13: second.x, m14: second.r, m21: first.y, m22: first.g, m23: second.y, m24: second.g, m31: first.z, m32: first.b, m33: second.z, m34: second.b, m41: first.type_, m42: first.parameter, m43: second.type_, m44: second.parameter});
	});
var $ianmackenzie$elm_3d_scene$Scene3d$lightingDisabled = _Utils_Tuple2(
	{
		lights12: A2($ianmackenzie$elm_3d_scene$Scene3d$lightPair, $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled, $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled),
		lights34: A2($ianmackenzie$elm_3d_scene$Scene3d$lightPair, $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled, $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled),
		lights56: A2($ianmackenzie$elm_3d_scene$Scene3d$lightPair, $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled, $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled),
		lights78: A2($ianmackenzie$elm_3d_scene$Scene3d$lightPair, $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled, $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled)
	},
	A4($elm_explorations$linear_algebra$Math$Vector4$vec4, 0, 0, 0, 0));
var $elm_explorations$webgl$WebGL$Settings$StencilTest$equal = $elm_explorations$webgl$WebGL$Settings$StencilTest$Test(514);
var $elm_explorations$webgl$WebGL$Settings$DepthTest$lessOrEqual = function (_v0) {
	var write = _v0.write;
	var near = _v0.near;
	var far = _v0.far;
	return A4($elm_explorations$webgl$WebGL$Internal$DepthTest, 515, write, near, far);
};
var $ianmackenzie$elm_3d_scene$Scene3d$upperFourBits = 240;
var $ianmackenzie$elm_3d_scene$Scene3d$outsideStencil = _List_fromArray(
	[
		$elm_explorations$webgl$WebGL$Settings$DepthTest$lessOrEqual(
		{far: 1, near: 0, write: true}),
		$elm_explorations$webgl$WebGL$Settings$StencilTest$test(
		{fail: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep, mask: $ianmackenzie$elm_3d_scene$Scene3d$upperFourBits, ref: 0, test: $elm_explorations$webgl$WebGL$Settings$StencilTest$equal, writeMask: 0, zfail: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep, zpass: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep}),
		$ianmackenzie$elm_3d_scene$Scene3d$defaultBlend
	]);
var $ianmackenzie$elm_3d_camera$WebGL$Matrices$projectionMatrix = F2(
	function (_v0, _v1) {
		var camera = _v0.a;
		var nearClipDepth = _v1.nearClipDepth;
		var farClipDepth = _v1.farClipDepth;
		var aspectRatio = _v1.aspectRatio;
		var _v2 = $ianmackenzie$elm_units$Quantity$abs(nearClipDepth);
		var n = _v2.a;
		var _v3 = $ianmackenzie$elm_units$Quantity$abs(farClipDepth);
		var f = _v3.a;
		var _v4 = camera.projection;
		if (_v4.$ === 'Perspective') {
			var frustumSlope = _v4.a;
			return $elm$core$Basics$isInfinite(f) ? $elm_explorations$linear_algebra$Math$Matrix4$fromRecord(
				{m11: 1 / (aspectRatio * frustumSlope), m12: 0, m13: 0, m14: 0, m21: 0, m22: 1 / frustumSlope, m23: 0, m24: 0, m31: 0, m32: 0, m33: -1, m34: (-2) * n, m41: 0, m42: 0, m43: -1, m44: 0}) : $elm_explorations$linear_algebra$Math$Matrix4$fromRecord(
				{m11: 1 / (aspectRatio * frustumSlope), m12: 0, m13: 0, m14: 0, m21: 0, m22: 1 / frustumSlope, m23: 0, m24: 0, m31: 0, m32: 0, m33: (-(f + n)) / (f - n), m34: (((-2) * f) * n) / (f - n), m41: 0, m42: 0, m43: -1, m44: 0});
		} else {
			var viewportHeight = _v4.a.a;
			return $elm$core$Basics$isInfinite(f) ? $elm_explorations$linear_algebra$Math$Matrix4$fromRecord(
				{m11: 2 / (aspectRatio * viewportHeight), m12: 0, m13: 0, m14: 0, m21: 0, m22: 2 / viewportHeight, m23: 0, m24: 0, m31: 0, m32: 0, m33: 0, m34: -1, m41: 0, m42: 0, m43: 0, m44: 1}) : $elm_explorations$linear_algebra$Math$Matrix4$fromRecord(
				{m11: 2 / (aspectRatio * viewportHeight), m12: 0, m13: 0, m14: 0, m21: 0, m22: 2 / viewportHeight, m23: 0, m24: 0, m31: 0, m32: 0, m33: (-2) / (f - n), m34: (-(f + n)) / (f - n), m41: 0, m42: 0, m43: 0, m44: 1});
		}
	});
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $ianmackenzie$elm_3d_scene$Scene3d$enabledFlag = F2(
	function (lightMask, lightIndex) {
		return ((1 & (lightMask >> lightIndex)) === 1) ? 0 : 1;
	});
var $ianmackenzie$elm_3d_scene$Scene3d$insideStencil = function (lightMask) {
	return _List_fromArray(
		[
			$elm_explorations$webgl$WebGL$Settings$DepthTest$lessOrEqual(
			{far: 1, near: 0, write: true}),
			$elm_explorations$webgl$WebGL$Settings$StencilTest$test(
			{fail: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep, mask: $ianmackenzie$elm_3d_scene$Scene3d$upperFourBits, ref: lightMask, test: $elm_explorations$webgl$WebGL$Settings$StencilTest$equal, writeMask: 0, zfail: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep, zpass: $elm_explorations$webgl$WebGL$Settings$StencilTest$keep}),
			$ianmackenzie$elm_3d_scene$Scene3d$defaultBlend
		]);
};
var $ianmackenzie$elm_3d_scene$Scene3d$renderWithinShadows = F3(
	function (meshRenderPasses, lightMatrices, numShadowingLights) {
		return $elm$core$List$concat(
			A2(
				$elm$core$List$map,
				function (lightMask) {
					var stencilMask = lightMask << 4;
					var enabledLights = A4(
						$elm_explorations$linear_algebra$Math$Vector4$vec4,
						A2($ianmackenzie$elm_3d_scene$Scene3d$enabledFlag, lightMask, 0),
						A2($ianmackenzie$elm_3d_scene$Scene3d$enabledFlag, lightMask, 1),
						A2($ianmackenzie$elm_3d_scene$Scene3d$enabledFlag, lightMask, 2),
						A2($ianmackenzie$elm_3d_scene$Scene3d$enabledFlag, lightMask, 3));
					return A3(
						$ianmackenzie$elm_3d_scene$Scene3d$call,
						meshRenderPasses,
						_Utils_Tuple2(lightMatrices, enabledLights),
						$ianmackenzie$elm_3d_scene$Scene3d$insideStencil(stencilMask));
				},
				A2(
					$elm$core$List$range,
					1,
					A2($elm$core$Basics$pow, 2, numShadowingLights) - 1)));
	});
var $elm_explorations$linear_algebra$Math$Matrix4$toRecord = _MJS_m4x4toRecord;
var $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Frame3d$toMat4 = function (frame) {
	var p = $ianmackenzie$elm_geometry$Point3d$unwrap(
		$ianmackenzie$elm_geometry$Frame3d$originPoint(frame));
	var k = $ianmackenzie$elm_geometry$Direction3d$unwrap(
		$ianmackenzie$elm_geometry$Frame3d$zDirection(frame));
	var j = $ianmackenzie$elm_geometry$Direction3d$unwrap(
		$ianmackenzie$elm_geometry$Frame3d$yDirection(frame));
	var i = $ianmackenzie$elm_geometry$Direction3d$unwrap(
		$ianmackenzie$elm_geometry$Frame3d$xDirection(frame));
	return $elm_explorations$linear_algebra$Math$Matrix4$fromRecord(
		{m11: i.x, m12: j.x, m13: k.x, m14: p.x, m21: i.y, m22: j.y, m23: k.y, m24: p.y, m31: i.z, m32: j.z, m33: k.z, m34: p.z, m41: 0, m42: 0, m43: 0, m44: 1});
};
var $ianmackenzie$elm_3d_camera$WebGL$Matrices$modelViewMatrix = F2(
	function (modelFrame, _v0) {
		var viewpointFrame = _v0.a;
		return $ianmackenzie$elm_geometry_linear_algebra_interop$Geometry$Interop$LinearAlgebra$Frame3d$toMat4(
			A2($ianmackenzie$elm_geometry$Frame3d$relativeTo, viewpointFrame, modelFrame));
	});
var $ianmackenzie$elm_3d_camera$WebGL$Matrices$viewMatrix = function (camera) {
	return A2($ianmackenzie$elm_3d_camera$WebGL$Matrices$modelViewMatrix, $ianmackenzie$elm_geometry$Frame3d$atOrigin, camera);
};
var $ianmackenzie$elm_3d_camera$Viewpoint3d$xDirection = function (_v0) {
	var frame = _v0.a;
	return $ianmackenzie$elm_geometry$Frame3d$xDirection(frame);
};
var $ianmackenzie$elm_3d_camera$Viewpoint3d$yDirection = function (_v0) {
	var frame = _v0.a;
	return $ianmackenzie$elm_geometry$Frame3d$yDirection(frame);
};
var $ianmackenzie$elm_3d_scene$Scene3d$toWebGLEntities = function (_arguments) {
	var viewpoint = $ianmackenzie$elm_3d_camera$Camera3d$viewpoint(_arguments.camera);
	var viewFrame = $ianmackenzie$elm_geometry$Frame3d$unsafe(
		{
			originPoint: $ianmackenzie$elm_3d_camera$Viewpoint3d$eyePoint(viewpoint),
			xDirection: $ianmackenzie$elm_3d_camera$Viewpoint3d$xDirection(viewpoint),
			yDirection: $ianmackenzie$elm_3d_camera$Viewpoint3d$yDirection(viewpoint),
			zDirection: $ianmackenzie$elm_geometry$Direction3d$reverse(
				$ianmackenzie$elm_3d_camera$Viewpoint3d$viewDirection(viewpoint))
		});
	var _v0 = $ianmackenzie$elm_3d_scene$Scene3d$Entity$group(_arguments.entities);
	var rootNode = _v0.a;
	var _v1 = A4(
		$ianmackenzie$elm_3d_scene$Scene3d$getViewBounds,
		viewFrame,
		1,
		$elm$core$Maybe$Nothing,
		_List_fromArray(
			[rootNode]));
	if (_v1.$ === 'Nothing') {
		return _List_Nil;
	} else {
		var viewBounds = _v1.a;
		var viewMatrix = $ianmackenzie$elm_3d_camera$WebGL$Matrices$viewMatrix(viewpoint);
		var nearClipDepth = A2(
			$ianmackenzie$elm_units$Quantity$multiplyBy,
			0.99,
			A2(
				$ianmackenzie$elm_units$Quantity$max,
				$ianmackenzie$elm_units$Quantity$abs(_arguments.clipDepth),
				$ianmackenzie$elm_units$Quantity$negate(
					$ianmackenzie$elm_geometry$BoundingBox3d$maxZ(viewBounds))));
		var _v2 = $ianmackenzie$elm_geometry$BoundingBox3d$dimensions(viewBounds);
		var xDimension = _v2.a;
		var yDimension = _v2.b;
		var zDimension = _v2.c;
		var sceneDiameter = $ianmackenzie$elm_geometry$Vector3d$length(
			A3($ianmackenzie$elm_geometry$Vector3d$xyz, xDimension, yDimension, zDimension));
		var farClipDepth = A2(
			$ianmackenzie$elm_units$Quantity$multiplyBy,
			1.01,
			A2(
				$ianmackenzie$elm_units$Quantity$plus,
				sceneDiameter,
				$ianmackenzie$elm_units$Quantity$negate(
					$ianmackenzie$elm_geometry$BoundingBox3d$minZ(viewBounds))));
		var projectionMatrix = A2(
			$ianmackenzie$elm_3d_camera$WebGL$Matrices$projectionMatrix,
			_arguments.camera,
			{aspectRatio: _arguments.aspectRatio, farClipDepth: farClipDepth, nearClipDepth: nearClipDepth});
		var projectionType = $elm_explorations$linear_algebra$Math$Matrix4$toRecord(projectionMatrix).m44;
		var eyePointOrDirectionToCamera = (!projectionType) ? $ianmackenzie$elm_geometry$Point3d$toMeters(
			$ianmackenzie$elm_3d_camera$Viewpoint3d$eyePoint(viewpoint)) : $ianmackenzie$elm_geometry$Direction3d$unwrap(
			$ianmackenzie$elm_geometry$Direction3d$reverse(
				$ianmackenzie$elm_3d_camera$Viewpoint3d$viewDirection(viewpoint)));
		var _v3 = function () {
			var _v4 = _arguments.toneMapping;
			switch (_v4.$) {
				case 'NoToneMapping':
					return _Utils_Tuple2(0, 0);
				case 'ReinhardLuminanceToneMapping':
					return _Utils_Tuple2(1, 0);
				case 'ReinhardPerChannelToneMapping':
					return _Utils_Tuple2(2, 0);
				case 'ExtendedReinhardLuminanceToneMapping':
					var overexposureLimit = _v4.a;
					return _Utils_Tuple2(3, overexposureLimit);
				case 'ExtendedReinhardPerChannelToneMapping':
					var overexposureLimit = _v4.a;
					return _Utils_Tuple2(4, overexposureLimit);
				default:
					return _Utils_Tuple2(5, 0);
			}
		}();
		var toneMapType = _v3.a;
		var toneMapParam = _v3.b;
		var _v5 = _arguments.exposure;
		var exposureLuminance = _v5.a;
		var _v6 = A2($ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$chromaticityToLinearRgb, exposureLuminance, _arguments.whiteBalance);
		var referenceWhite = _v6.a;
		var sceneProperties = $elm_explorations$linear_algebra$Math$Matrix4$fromRecord(
			{
				m11: 0,
				m12: eyePointOrDirectionToCamera.x,
				m13: $elm_explorations$linear_algebra$Math$Vector3$getX(referenceWhite),
				m14: _arguments.supersampling,
				m21: 0,
				m22: eyePointOrDirectionToCamera.y,
				m23: $elm_explorations$linear_algebra$Math$Vector3$getY(referenceWhite),
				m24: $ianmackenzie$elm_units$Length$inMeters(sceneDiameter),
				m31: 0,
				m32: eyePointOrDirectionToCamera.z,
				m33: $elm_explorations$linear_algebra$Math$Vector3$getZ(referenceWhite),
				m34: toneMapType,
				m41: 0,
				m42: projectionType,
				m43: 0,
				m44: toneMapParam
			});
		var renderPasses = A6(
			$ianmackenzie$elm_3d_scene$Scene3d$collectRenderPasses,
			sceneProperties,
			viewMatrix,
			projectionMatrix,
			$ianmackenzie$elm_3d_scene$Scene3d$Transformation$identity,
			rootNode,
			{meshes: _List_Nil, points: _List_Nil, shadows: _List_Nil});
		var _v7 = _arguments.lights;
		switch (_v7.$) {
			case 'SingleUnshadowedPass':
				var lightMatrices = _v7.a;
				return $elm$core$List$concat(
					_List_fromArray(
						[
							A3(
							$ianmackenzie$elm_3d_scene$Scene3d$call,
							renderPasses.meshes,
							_Utils_Tuple2(lightMatrices, $ianmackenzie$elm_3d_scene$Scene3d$allLightsEnabled),
							$ianmackenzie$elm_3d_scene$Scene3d$depthTestDefault),
							A3($ianmackenzie$elm_3d_scene$Scene3d$call, renderPasses.points, $ianmackenzie$elm_3d_scene$Scene3d$lightingDisabled, $ianmackenzie$elm_3d_scene$Scene3d$depthTestDefault)
						]));
			case 'SingleShadowedPass':
				var lightMatrices = _v7.a;
				return $elm$core$List$concat(
					_List_fromArray(
						[
							A3($ianmackenzie$elm_3d_scene$Scene3d$call, renderPasses.meshes, $ianmackenzie$elm_3d_scene$Scene3d$lightingDisabled, $ianmackenzie$elm_3d_scene$Scene3d$depthTestDefault),
							_List_fromArray(
							[$ianmackenzie$elm_3d_scene$Scene3d$initStencil]),
							A3($ianmackenzie$elm_3d_scene$Scene3d$call, renderPasses.shadows, lightMatrices.lights12, $ianmackenzie$elm_3d_scene$Scene3d$createShadowStencil),
							_List_fromArray(
							[
								$ianmackenzie$elm_3d_scene$Scene3d$storeStencilValue(0)
							]),
							A3(
							$ianmackenzie$elm_3d_scene$Scene3d$call,
							renderPasses.meshes,
							_Utils_Tuple2(lightMatrices, $ianmackenzie$elm_3d_scene$Scene3d$allLightsEnabled),
							$ianmackenzie$elm_3d_scene$Scene3d$outsideStencil),
							A3($ianmackenzie$elm_3d_scene$Scene3d$call, renderPasses.points, $ianmackenzie$elm_3d_scene$Scene3d$lightingDisabled, $ianmackenzie$elm_3d_scene$Scene3d$depthTestDefault)
						]));
			default:
				var shadowCasters = _v7.a;
				var allLightMatrices = _v7.b;
				return $elm$core$List$concat(
					_List_fromArray(
						[
							A3(
							$ianmackenzie$elm_3d_scene$Scene3d$call,
							renderPasses.meshes,
							_Utils_Tuple2(allLightMatrices, $ianmackenzie$elm_3d_scene$Scene3d$allLightsEnabled),
							$ianmackenzie$elm_3d_scene$Scene3d$depthTestDefault),
							_List_fromArray(
							[$ianmackenzie$elm_3d_scene$Scene3d$initStencil]),
							A2($ianmackenzie$elm_3d_scene$Scene3d$createShadows, renderPasses.shadows, shadowCasters),
							A3(
							$ianmackenzie$elm_3d_scene$Scene3d$renderWithinShadows,
							renderPasses.meshes,
							allLightMatrices,
							$elm$core$List$length(shadowCasters)),
							A3($ianmackenzie$elm_3d_scene$Scene3d$call, renderPasses.points, $ianmackenzie$elm_3d_scene$Scene3d$lightingDisabled, $ianmackenzie$elm_3d_scene$Scene3d$depthTestDefault)
						]));
		}
	}
};
var $elm$html$Html$Attributes$width = function (n) {
	return A2(
		_VirtualDom_attribute,
		'width',
		$elm$core$String$fromInt(n));
};
var $ianmackenzie$elm_3d_scene$Scene3d$composite = F2(
	function (_arguments, scenes) {
		var commonWebGLOptions = _List_fromArray(
			[
				$elm_explorations$webgl$WebGL$depth(1),
				$elm_explorations$webgl$WebGL$stencil(0),
				$elm_explorations$webgl$WebGL$alpha(true),
				A4($elm_explorations$webgl$WebGL$clearColor, 0, 0, 0, 0)
			]);
		var _v0 = function () {
			var _v1 = _arguments.antialiasing;
			switch (_v1.$) {
				case 'NoAntialiasing':
					return _Utils_Tuple3(commonWebGLOptions, '0', 1);
				case 'Multisampling':
					return _Utils_Tuple3(
						A2($elm$core$List$cons, $elm_explorations$webgl$WebGL$antialias, commonWebGLOptions),
						'1',
						1);
				default:
					var value = _v1.a;
					return _Utils_Tuple3(commonWebGLOptions, '0', value);
			}
		}();
		var webGLOptions = _v0.a;
		var key = _v0.b;
		var scalingFactor = _v0.c;
		var _v2 = _arguments.dimensions;
		var width = _v2.a;
		var height = _v2.b;
		var heightInPixels = $ianmackenzie$elm_units$Pixels$toInt(height);
		var heightCss = A2(
			$elm$html$Html$Attributes$style,
			'height',
			$elm$core$String$fromInt(heightInPixels) + 'px');
		var widthInPixels = $ianmackenzie$elm_units$Pixels$toInt(width);
		var aspectRatio = widthInPixels / heightInPixels;
		var webGLEntities = A2(
			$elm$core$List$concatMap,
			function (scene) {
				return $ianmackenzie$elm_3d_scene$Scene3d$toWebGLEntities(
					{aspectRatio: aspectRatio, camera: _arguments.camera, clipDepth: _arguments.clipDepth, entities: scene.entities, exposure: scene.exposure, lights: scene.lights, supersampling: scalingFactor, toneMapping: scene.toneMapping, whiteBalance: scene.whiteBalance});
			},
			scenes);
		var widthCss = A2(
			$elm$html$Html$Attributes$style,
			'width',
			$elm$core$String$fromInt(widthInPixels) + 'px');
		var _v3 = _arguments.background;
		var givenBackgroundColor = _v3.a;
		var backgroundColorString = $avh4$elm_color$Color$toCssString(givenBackgroundColor);
		return A3(
			$elm$html$Html$Keyed$node,
			'div',
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'padding', '0px'),
					widthCss,
					heightCss
				]),
			_List_fromArray(
				[
					_Utils_Tuple2(
					key,
					A3(
						$elm_explorations$webgl$WebGL$toHtmlWith,
						webGLOptions,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$width(
								$elm$core$Basics$round(widthInPixels * scalingFactor)),
								$elm$html$Html$Attributes$height(
								$elm$core$Basics$round(heightInPixels * scalingFactor)),
								widthCss,
								heightCss,
								A2($elm$html$Html$Attributes$style, 'display', 'block'),
								A2($elm$html$Html$Attributes$style, 'background-color', backgroundColorString)
							]),
						webGLEntities))
				]));
	});
var $ianmackenzie$elm_3d_scene$Scene3d$custom = function (_arguments) {
	return A2(
		$ianmackenzie$elm_3d_scene$Scene3d$composite,
		{antialiasing: _arguments.antialiasing, background: _arguments.background, camera: _arguments.camera, clipDepth: _arguments.clipDepth, dimensions: _arguments.dimensions},
		_List_fromArray(
			[
				{entities: _arguments.entities, exposure: _arguments.exposure, lights: _arguments.lights, toneMapping: _arguments.toneMapping, whiteBalance: _arguments.whiteBalance}
			]));
};
var $ianmackenzie$elm_3d_scene$Scene3d$Types$Chromaticity = function (a) {
	return {$: 'Chromaticity', a: a};
};
var $ianmackenzie$elm_3d_scene$Scene3d$Light$chromaticity = function (xy) {
	return $ianmackenzie$elm_3d_scene$Scene3d$Types$Chromaticity(xy);
};
var $ianmackenzie$elm_3d_scene$Scene3d$Light$daylight = $ianmackenzie$elm_3d_scene$Scene3d$Light$chromaticity(
	{x: 0.31271, y: 0.32902});
var $ianmackenzie$elm_3d_scene$Scene3d$Light$directional = F2(
	function (_v0, light) {
		var shadowFlag = _v0.a;
		var _v1 = $ianmackenzie$elm_geometry$Direction3d$unwrap(light.direction);
		var x = _v1.x;
		var y = _v1.y;
		var z = _v1.z;
		var _v2 = A2($ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$chromaticityToLinearRgb, light.intensity, light.chromaticity);
		var rgb = _v2.a;
		return $ianmackenzie$elm_3d_scene$Scene3d$Types$Light(
			{
				b: $elm_explorations$linear_algebra$Math$Vector3$getZ(rgb),
				castsShadows: shadowFlag,
				g: $elm_explorations$linear_algebra$Math$Vector3$getY(rgb),
				parameter: 0,
				r: $elm_explorations$linear_algebra$Math$Vector3$getX(rgb),
				type_: 1,
				x: -x,
				y: -y,
				z: -z
			});
	});
var $ianmackenzie$elm_3d_scene$Scene3d$Exposure = function (a) {
	return {$: 'Exposure', a: a};
};
var $ianmackenzie$elm_units$Luminance$nits = function (numNits) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numNits);
};
var $ianmackenzie$elm_3d_scene$Scene3d$exposureValue = function (ev100) {
	return $ianmackenzie$elm_3d_scene$Scene3d$Exposure(
		$ianmackenzie$elm_units$Luminance$nits(
			1.2 * A2($elm$core$Basics$pow, 2, ev100)));
};
var $ianmackenzie$elm_units$Illuminance$lux = function (numLux) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numLux);
};
var $ianmackenzie$elm_3d_scene$Scene3d$Multisampling = {$: 'Multisampling'};
var $ianmackenzie$elm_3d_scene$Scene3d$multisampling = $ianmackenzie$elm_3d_scene$Scene3d$Multisampling;
var $ianmackenzie$elm_3d_scene$Scene3d$NoToneMapping = {$: 'NoToneMapping'};
var $ianmackenzie$elm_3d_scene$Scene3d$noToneMapping = $ianmackenzie$elm_3d_scene$Scene3d$NoToneMapping;
var $ianmackenzie$elm_units$Illuminance$inLux = function (_v0) {
	var numLux = _v0.a;
	return numLux;
};
var $ianmackenzie$elm_3d_scene$Scene3d$Light$soft = function (light) {
	soft:
	while (true) {
		if (_Utils_eq(light.intensityAbove, $ianmackenzie$elm_units$Quantity$zero) && _Utils_eq(light.intensityBelow, $ianmackenzie$elm_units$Quantity$zero)) {
			return $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled;
		} else {
			if (A2(
				$ianmackenzie$elm_units$Quantity$greaterThan,
				$ianmackenzie$elm_units$Quantity$abs(light.intensityAbove),
				$ianmackenzie$elm_units$Quantity$abs(light.intensityBelow))) {
				var $temp$light = {
					chromaticity: light.chromaticity,
					intensityAbove: light.intensityBelow,
					intensityBelow: light.intensityAbove,
					upDirection: $ianmackenzie$elm_geometry$Direction3d$reverse(light.upDirection)
				};
				light = $temp$light;
				continue soft;
			} else {
				var nitsBelow = $elm$core$Basics$abs(
					$ianmackenzie$elm_units$Illuminance$inLux(light.intensityBelow) / $elm$core$Basics$pi);
				var nitsAbove = $elm$core$Basics$abs(
					$ianmackenzie$elm_units$Illuminance$inLux(light.intensityAbove) / $elm$core$Basics$pi);
				var _v0 = $ianmackenzie$elm_geometry$Direction3d$unwrap(light.upDirection);
				var x = _v0.x;
				var y = _v0.y;
				var z = _v0.z;
				var _v1 = A2(
					$ianmackenzie$elm_3d_scene$Scene3d$ColorConversions$chromaticityToLinearRgb,
					$ianmackenzie$elm_units$Quantity$float(1),
					light.chromaticity);
				var rgb = _v1.a;
				return $ianmackenzie$elm_3d_scene$Scene3d$Types$Light(
					{
						b: nitsAbove * $elm_explorations$linear_algebra$Math$Vector3$getZ(rgb),
						castsShadows: false,
						g: nitsAbove * $elm_explorations$linear_algebra$Math$Vector3$getY(rgb),
						parameter: nitsBelow / nitsAbove,
						r: nitsAbove * $elm_explorations$linear_algebra$Math$Vector3$getX(rgb),
						type_: 3,
						x: x,
						y: y,
						z: z
					});
			}
		}
	}
};
var $ianmackenzie$elm_3d_scene$Scene3d$Light$overhead = function (_arguments) {
	return $ianmackenzie$elm_3d_scene$Scene3d$Light$soft(
		{chromaticity: _arguments.chromaticity, intensityAbove: _arguments.intensity, intensityBelow: $ianmackenzie$elm_units$Quantity$zero, upDirection: _arguments.upDirection});
};
var $ianmackenzie$elm_units$Temperature$inKelvins = function (_v0) {
	var numKelvins = _v0.a;
	return numKelvins;
};
var $ianmackenzie$elm_3d_scene$Scene3d$Light$colorTemperature = function (temperature) {
	var t = A3(
		$elm$core$Basics$clamp,
		1667,
		25000,
		$ianmackenzie$elm_units$Temperature$inKelvins(temperature));
	var x = (t <= 4000) ? ((((((-0.2661239) * 1.0e9) / ((t * t) * t)) - ((0.2343589 * 1.0e6) / (t * t))) + ((0.8776956 * 1.0e3) / t)) + 0.17991) : ((((((-3.0258469) * 1.0e9) / ((t * t) * t)) + ((2.1070379 * 1.0e6) / (t * t))) + ((0.2226347 * 1.0e3) / t)) + 0.24039);
	var y = (t <= 2222) ? (((((-1.1063814) * ((x * x) * x)) - (1.3481102 * (x * x))) + (2.18555832 * x)) - 0.20219683) : ((t <= 4000) ? (((((-0.9549476) * ((x * x) * x)) - (1.37418593 * (x * x))) + (2.09137015 * x)) - 0.16748867) : ((((3.081758 * ((x * x) * x)) - (5.8733867 * (x * x))) + (3.75112997 * x)) - 0.37001483));
	return $ianmackenzie$elm_3d_scene$Scene3d$Light$chromaticity(
		{x: x, y: y});
};
var $ianmackenzie$elm_units$Temperature$Temperature = function (a) {
	return {$: 'Temperature', a: a};
};
var $ianmackenzie$elm_units$Temperature$kelvins = function (numKelvins) {
	return $ianmackenzie$elm_units$Temperature$Temperature(numKelvins);
};
var $ianmackenzie$elm_3d_scene$Scene3d$Light$skylight = $ianmackenzie$elm_3d_scene$Scene3d$Light$colorTemperature(
	$ianmackenzie$elm_units$Temperature$kelvins(12000));
var $ianmackenzie$elm_3d_scene$Scene3d$Light$sunlight = $ianmackenzie$elm_3d_scene$Scene3d$Light$colorTemperature(
	$ianmackenzie$elm_units$Temperature$kelvins(5600));
var $ianmackenzie$elm_3d_scene$Scene3d$MultiplePasses = F2(
	function (a, b) {
		return {$: 'MultiplePasses', a: a, b: b};
	});
var $ianmackenzie$elm_3d_scene$Scene3d$SingleUnshadowedPass = function (a) {
	return {$: 'SingleUnshadowedPass', a: a};
};
var $ianmackenzie$elm_3d_scene$Scene3d$eraseLight = function (_v0) {
	var light = _v0.a;
	return $ianmackenzie$elm_3d_scene$Scene3d$Types$Light(light);
};
var $ianmackenzie$elm_3d_scene$Scene3d$lightCastsShadows = function (_v0) {
	var properties = _v0.a;
	return properties.castsShadows;
};
var $ianmackenzie$elm_3d_scene$Scene3d$noLights = $ianmackenzie$elm_3d_scene$Scene3d$SingleUnshadowedPass($ianmackenzie$elm_3d_scene$Scene3d$lightingDisabled.a);
var $ianmackenzie$elm_3d_scene$Scene3d$singleLight = function (_v0) {
	var light = _v0.a;
	return $elm_explorations$linear_algebra$Math$Matrix4$fromRecord(
		{m11: light.x, m12: light.r, m13: 0, m14: 0, m21: light.y, m22: light.g, m23: 0, m24: 0, m31: light.z, m32: light.b, m33: 0, m34: 0, m41: light.type_, m42: light.parameter, m43: 0, m44: 0});
};
var $ianmackenzie$elm_3d_scene$Scene3d$eightLights = F8(
	function (first, second, third, fourth, fifth, sixth, seventh, eigth) {
		var _v0 = A2(
			$elm$core$List$partition,
			$ianmackenzie$elm_3d_scene$Scene3d$lightCastsShadows,
			_List_fromArray(
				[
					$ianmackenzie$elm_3d_scene$Scene3d$eraseLight(first),
					$ianmackenzie$elm_3d_scene$Scene3d$eraseLight(second),
					$ianmackenzie$elm_3d_scene$Scene3d$eraseLight(third),
					$ianmackenzie$elm_3d_scene$Scene3d$eraseLight(fourth)
				]));
		var enabledShadowCasters = _v0.a;
		var disabledShadowCasters = _v0.b;
		if (!enabledShadowCasters.b) {
			return $ianmackenzie$elm_3d_scene$Scene3d$SingleUnshadowedPass(
				{
					lights12: A2($ianmackenzie$elm_3d_scene$Scene3d$lightPair, first, second),
					lights34: A2($ianmackenzie$elm_3d_scene$Scene3d$lightPair, third, fourth),
					lights56: A2($ianmackenzie$elm_3d_scene$Scene3d$lightPair, fifth, sixth),
					lights78: A2($ianmackenzie$elm_3d_scene$Scene3d$lightPair, seventh, eigth)
				});
		} else {
			var sortedLights = _Utils_ap(enabledShadowCasters, disabledShadowCasters);
			if ((((sortedLights.b && sortedLights.b.b) && sortedLights.b.b.b) && sortedLights.b.b.b.b) && (!sortedLights.b.b.b.b.b)) {
				var light0 = sortedLights.a;
				var _v3 = sortedLights.b;
				var light1 = _v3.a;
				var _v4 = _v3.b;
				var light2 = _v4.a;
				var _v5 = _v4.b;
				var light3 = _v5.a;
				return A2(
					$ianmackenzie$elm_3d_scene$Scene3d$MultiplePasses,
					A2($elm$core$List$map, $ianmackenzie$elm_3d_scene$Scene3d$singleLight, enabledShadowCasters),
					{
						lights12: A2($ianmackenzie$elm_3d_scene$Scene3d$lightPair, light0, light1),
						lights34: A2($ianmackenzie$elm_3d_scene$Scene3d$lightPair, light2, light3),
						lights56: A2($ianmackenzie$elm_3d_scene$Scene3d$lightPair, fifth, sixth),
						lights78: A2($ianmackenzie$elm_3d_scene$Scene3d$lightPair, seventh, eigth)
					});
			} else {
				return $ianmackenzie$elm_3d_scene$Scene3d$noLights;
			}
		}
	});
var $ianmackenzie$elm_3d_scene$Scene3d$threeLights = F3(
	function (first, second, third) {
		return A8($ianmackenzie$elm_3d_scene$Scene3d$eightLights, first, second, third, $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled, $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled, $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled, $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled, $ianmackenzie$elm_3d_scene$Scene3d$Light$disabled);
	});
var $ianmackenzie$elm_3d_scene$Scene3d$sunny = function (_arguments) {
	var sun = A2(
		$ianmackenzie$elm_3d_scene$Scene3d$Light$directional,
		$ianmackenzie$elm_3d_scene$Scene3d$Light$castsShadows(_arguments.shadows),
		{
			chromaticity: $ianmackenzie$elm_3d_scene$Scene3d$Light$sunlight,
			direction: _arguments.sunlightDirection,
			intensity: $ianmackenzie$elm_units$Illuminance$lux(80000)
		});
	var sky = $ianmackenzie$elm_3d_scene$Scene3d$Light$overhead(
		{
			chromaticity: $ianmackenzie$elm_3d_scene$Scene3d$Light$skylight,
			intensity: $ianmackenzie$elm_units$Illuminance$lux(20000),
			upDirection: _arguments.upDirection
		});
	var environment = $ianmackenzie$elm_3d_scene$Scene3d$Light$overhead(
		{
			chromaticity: $ianmackenzie$elm_3d_scene$Scene3d$Light$daylight,
			intensity: $ianmackenzie$elm_units$Illuminance$lux(15000),
			upDirection: $ianmackenzie$elm_geometry$Direction3d$reverse(_arguments.upDirection)
		});
	var lights = A3($ianmackenzie$elm_3d_scene$Scene3d$threeLights, sun, sky, environment);
	return $ianmackenzie$elm_3d_scene$Scene3d$custom(
		{
			antialiasing: $ianmackenzie$elm_3d_scene$Scene3d$multisampling,
			background: _arguments.background,
			camera: _arguments.camera,
			clipDepth: _arguments.clipDepth,
			dimensions: _arguments.dimensions,
			entities: _arguments.entities,
			exposure: $ianmackenzie$elm_3d_scene$Scene3d$exposureValue(15),
			lights: lights,
			toneMapping: $ianmackenzie$elm_3d_scene$Scene3d$noToneMapping,
			whiteBalance: $ianmackenzie$elm_3d_scene$Scene3d$Light$daylight
		});
};
var $ianmackenzie$elm_geometry$Geometry$Types$Rectangle2d = function (a) {
	return {$: 'Rectangle2d', a: a};
};
var $ianmackenzie$elm_geometry$Geometry$Types$Direction2d = function (a) {
	return {$: 'Direction2d', a: a};
};
var $ianmackenzie$elm_geometry$Direction2d$negativeX = $ianmackenzie$elm_geometry$Geometry$Types$Direction2d(
	{x: -1, y: 0});
var $ianmackenzie$elm_geometry$Direction2d$negativeY = $ianmackenzie$elm_geometry$Geometry$Types$Direction2d(
	{x: 0, y: -1});
var $ianmackenzie$elm_geometry$Direction2d$positiveX = $ianmackenzie$elm_geometry$Geometry$Types$Direction2d(
	{x: 1, y: 0});
var $ianmackenzie$elm_geometry$Direction2d$positiveY = $ianmackenzie$elm_geometry$Geometry$Types$Direction2d(
	{x: 0, y: 1});
var $ianmackenzie$elm_geometry$Frame2d$unsafe = function (properties) {
	return $ianmackenzie$elm_geometry$Geometry$Types$Frame2d(properties);
};
var $ianmackenzie$elm_geometry$Point2d$xy = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v1.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Point2d(
			{x: x, y: y});
	});
var $ianmackenzie$elm_geometry$Rectangle2d$axisAligned = F4(
	function (x1, y1, x2, y2) {
		var computedYDirection = A2($ianmackenzie$elm_units$Quantity$greaterThanOrEqualTo, y1, y2) ? $ianmackenzie$elm_geometry$Direction2d$positiveY : $ianmackenzie$elm_geometry$Direction2d$negativeY;
		var computedXDirection = A2($ianmackenzie$elm_units$Quantity$greaterThanOrEqualTo, x1, x2) ? $ianmackenzie$elm_geometry$Direction2d$positiveX : $ianmackenzie$elm_geometry$Direction2d$negativeX;
		var computedDimensions = _Utils_Tuple2(
			$ianmackenzie$elm_units$Quantity$abs(
				A2($ianmackenzie$elm_units$Quantity$minus, x1, x2)),
			$ianmackenzie$elm_units$Quantity$abs(
				A2($ianmackenzie$elm_units$Quantity$minus, y1, y2)));
		var computedCenterPoint = A2(
			$ianmackenzie$elm_geometry$Point2d$xy,
			A2($ianmackenzie$elm_units$Quantity$midpoint, x1, x2),
			A2($ianmackenzie$elm_units$Quantity$midpoint, y1, y2));
		var computedAxes = $ianmackenzie$elm_geometry$Frame2d$unsafe(
			{originPoint: computedCenterPoint, xDirection: computedXDirection, yDirection: computedYDirection});
		return $ianmackenzie$elm_geometry$Geometry$Types$Rectangle2d(
			{axes: computedAxes, dimensions: computedDimensions});
	});
var $ianmackenzie$elm_geometry$Rectangle2d$with = function (_v0) {
	var x1 = _v0.x1;
	var y1 = _v0.y1;
	var x2 = _v0.x2;
	var y2 = _v0.y2;
	return A4($ianmackenzie$elm_geometry$Rectangle2d$axisAligned, x1, y1, x2, y2);
};
var $author$project$Main$ui = function (state) {
	var ratioScreenHeightToSceneMeter = 1;
	var adjustedSize = (_Utils_cmp(state.windowSize.width, state.windowSize.height * $author$project$Main$ratioWidthToHeight) < 0) ? {height: state.windowSize.width / $author$project$Main$ratioWidthToHeight, width: state.windowSize.width} : {height: state.windowSize.height, width: state.windowSize.height * $author$project$Main$ratioWidthToHeight};
	var rayTo = function (point) {
		return A3(
			$ianmackenzie$elm_3d_camera$Camera3d$ray,
			state.camera,
			$ianmackenzie$elm_geometry$Rectangle2d$with(
				{
					x1: $ianmackenzie$elm_units$Pixels$pixels(0),
					x2: $ianmackenzie$elm_units$Pixels$float(adjustedSize.width),
					y1: $ianmackenzie$elm_units$Pixels$float(adjustedSize.height),
					y2: $ianmackenzie$elm_units$Pixels$pixels(0)
				}),
			point);
	};
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'display', 'flex'),
				A2($elm$html$Html$Attributes$style, 'justify-content', 'center'),
				A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
				A2(
				$elm$html$Html$Events$on,
				'mousedown',
				A2(
					$elm$json$Json$Decode$map,
					function (pos) {
						return $author$project$Main$MousePressed(
							rayTo(pos));
					},
					$author$project$Main$decodeMousePosition)),
				A2(
				$elm$html$Html$Events$on,
				'mousemove',
				A2(
					$elm$json$Json$Decode$map,
					function (pos) {
						return $author$project$Main$MouseMoved(
							rayTo(pos));
					},
					$author$project$Main$decodeMousePosition)),
				$elm$html$Html$Events$onMouseUp($author$project$Main$MouseReleased),
				A2(
				$elm$html$Html$Attributes$style,
				'cursor',
				function () {
					var _v0 = state.maybeRaycastResult;
					if (_v0.$ === 'Just') {
						return 'grabbing';
					} else {
						return 'grab';
					}
				}())
			]),
		_List_fromArray(
			[
				$ianmackenzie$elm_3d_scene$Scene3d$sunny(
				{
					background: $ianmackenzie$elm_3d_scene$Scene3d$backgroundColor(
						A3($avh4$elm_color$Color$rgb, 0, 0, 0)),
					camera: state.camera,
					clipDepth: $ianmackenzie$elm_units$Length$meters(0.1),
					dimensions: _Utils_Tuple2(
						$ianmackenzie$elm_units$Pixels$int(
							$elm$core$Basics$round(adjustedSize.width)),
						$ianmackenzie$elm_units$Pixels$int(
							$elm$core$Basics$round(adjustedSize.height))),
					entities: A2(
						$elm$core$List$map,
						A2($ianmackenzie$elm_3d_scene$Scene3d$scaleAbout, $ianmackenzie$elm_geometry$Point3d$origin, ratioScreenHeightToSceneMeter),
						A2(
							$elm$core$List$map,
							$author$project$Main$bodyToEntity,
							$w0rm$elm_physics$Physics$World$bodies(state.world))),
					shadows: true,
					sunlightDirection: A2(
						$ianmackenzie$elm_geometry$Direction3d$xyZ,
						$ianmackenzie$elm_units$Angle$degrees(135),
						$ianmackenzie$elm_units$Angle$degrees(-60)),
					upDirection: $ianmackenzie$elm_geometry$Direction3d$z
				})
			]));
};
var $author$project$Main$uiDocument = function (state) {
	return {
		body: $elm$core$List$singleton(
			$author$project$Main$ui(state)),
		title: 'dragahedron'
	};
};
var $author$project$Main$main = $MartinSStewart$elm_audio$Audio$documentWithAudio(
	{
		audio: $author$project$Main$audio,
		audioPort: {fromJS: $author$project$Main$audioPortFromJS, toJS: $author$project$Main$audioPortToJS},
		init: A2(
			$elm$core$Basics$composeR,
			$author$project$Main$init,
			$author$project$Reaction$toTuple3($author$project$Main$interpretEffect)),
		subscriptions: function (_v0) {
			return $author$project$Main$subscriptions;
		},
		update: F2(
			function (_v1, event) {
				return A2(
					$elm$core$Basics$composeR,
					$author$project$Main$reactTo(event),
					$author$project$Reaction$toTuple3($author$project$Main$interpretEffect));
			}),
		view: function (_v2) {
			return $author$project$Main$uiDocument;
		}
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));