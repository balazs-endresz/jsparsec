// -------------------------------------------------
// Algebraic Data Types
// -------------------------------------------------

function Record(){}
var record = new Record;


//this is used for simulating the record syntax
//but it's not guaranteed to work in every javascript environment
//beacause it assumes that the keys of objects are iterated in the order they were defined
//but that is not part of the ECMAScript standard
function getNthKey(obj, nth) {
	if(!/[0-9]/.test(nth))
		return nth;
	if(typeof obj != "object")
		return nth;
    var i = 0;
    for (var key in obj){
		if (obj.hasOwnProperty(key) && i == nth)  
			return key;  
		i++
	}
    return -1;  
}


function data(type, constr){
	if(type.constructors)
		throw "Type constructor has been already defined: '" + type.name + "'";
	type.constructors = constr;

	for(var i = 0, l = constr.length; i < l; ++i){
		var single = typeof constr[i] != "object",
			name =  single  ? constr[i] : constr[i][0];
		if(name in {})
			throw "The name of the data constructor can't be a property of Object.prototype as well!";

		type[name] = single ? value(name)() : value(name, slice(constr[i], 1));
		if(!single)
			type[name]._length = slice(constr[i], 1).length
	}

	function value(constr, fields){
		var recordDef = fields && typeof fields[0] == "object";
		function create(_isrecord, rec){
			var isrecord = (_isrecord instanceof Record),
				args = isrecord ? rec : slice(arguments),
				that = new type,
				i = 0;
			
			that.update = function(newRecords){
				var obj = {};
				for(var n in fields[0]){
					obj[n] = this[n]
					if(n in newRecords)
						obj[n] = newRecords[n];
				}
				return create(record, obj);
			}

			that[constr] = true;

			if(args !== undef)
				for(var name in args)
					if(args.hasOwnProperty(name) && name != constr){

						if(isrecord && fields && recordDef)
							if( !(name in fields[0]))
								throw "The accessor '" + name + "' is not defined for the data constructor '" + constr + "'";

						var recName = getNthKey(fields[0], name);
						var arg = (args[i] !== undefined) ? args[i] : args[recName];

						var check = recordDef ? fields[0][recName] : fields[i];
						if(check.name && !((check == arg.constructor) || (arg instanceof check) ))
							throw "Type mismatch: expecting '" + check.name + "' instead of '" + arg.constructor.name +"' in the argument '" + (recName || i) + "' of the data constructor '" + constr + "' of type '" + type.name +"'"

						that[recName] = that[i] = that[name] = args[name];
						i++;

					}else if(name == constr)
						throw "Accessor has the same name as the data constructor: '" + constr + "'";

			return that;
		}
		return create;
	}
}

//currently type variables on the lhs cannot be declared, and they are not checked at all:

//  function Maybe(){}
//  data(Maybe, [["Just", "a"], "Nothing"]);

//  var Just    = Maybe.Just;
//  var Nothing = Maybe.Nothing;

//Just("a") instanceof Maybe
//Just("a").constructor == Maybe
//
//Just("a").Just == true //can be used in place of pattern matching: if(maybeval.Just) ... if(maybeval.Nothing) ...
//Just("a")[0] == "a"    //access arguments by index
//
//Nothing.Nothing == true
//Nothing == Nothing


// using record syntax:
 
/*
function Type(){}

data(Type, [["Constr1", Number, "a"]
			,"Constr2"
			,["Constr3", {acc: Number}]
			,["Constr4", Number]
			]);

//in Haskell:
data Number = ... -- javascript number type
data Type a = Constr1 Number a
			| Constr2
			| Constr3 {acc :: Number}
			| Constr4 Number
*/

//Type.Constr3(record, {acc:1}).Constr3 == true
//Type.Constr3(record, {acc:1}).acc == 1
//Type.Constr3(record, {acc:1})[0] == 1
//Type.Constr3(1)[0] == 1
//Type.Constr3(1).a == 1
//Type.Constr2.Constr2 == true
//Type.Constr2 == Type.Constr2


//record update (creates a new object):

//function T(){}
//data(T, [["C", {a: String,b: String}]]);
//T.C("2","3").update({a:"4"}).a == "4"

function Maybe(){}
data(Maybe, [["Just", "a"], "Nothing"]);

function Ordering(){}
data(Ordering, ["LT", "EQ", "GT"])

function Either(){}
data(Either, [["Left", "a"], ["Right", "b"]])

// -------------------------------------------------
// Operators
// -------------------------------------------------

function infixl(strength){ return ["l", strength] }
function infixr(strength){ return ["r", strength] }
function infix (strength){ return ["x", strength] }

function getFixity(opstr){
	if(opstr._String)
		return;
	var op = operators[opstr];
	if(opstr._Op && !op)
		return ["l", 9];

	return op && op.fixity;
}
function getFixityDir(opstr){ 
	if(opstr._String)
		return;
	var op = operators[opstr];
	if(opstr._Op && !op)
		return "l";

	return op && (op.fixity[0] || "l" );
}
function getFixityStrn(opstr){
	if(opstr._String)
		return;
	var op = operators[opstr];
	if(opstr._Op && !op)
		return 9;

	return op && (isDefined(op.fixity[1]) ? op.fixity[1] : 9);
}


var operators = {
	"$" : {
		func:	call,
		fixity: infixr(0)
		//,type:	[Function, "*", "*"]
	},
	"." : {
		func:	compose1,
		fixity: infixr(9)
		//,type:	[Function, Function, Function]
	},
	":" : {
		func:	cons,
		fixity: infixr(5)
	}
	
};




// -------------------------------------------------
// Array expressions
// -------------------------------------------------

// -- see usage in Char

function splice_args(args, i, rec){
	var op;
	if(args[i]._Op){
		delete args[i]._Op;
		op = args[i];
	}else
		op = operators[args[i]].func
	
	var	item = op(args[i-1], args[i+1]);
	args.splice(i-1, 3 , item);
	return resolve(args, rec);
}

//in array-expressions if the square brackets are 
//not intended for groupping subexpressions
//but an actual array is needed the it should be wrapped in `arr`
//
//if a string might be the same as an operator then use `str`
//
//and functions can be used as operators by wrapping them in `op`

//var p = [string, str("<|>"), op(parserPlus), return_, arr([])].resolve();

//but usually this can be done by simply using the javascript call operator:
//
//var p = [string("<|>"), "<|>", return_([])].resolve();


function arr(a){ a._Array = true; return a;}

function str(s){ 
	var str = new String(s);
	str._String = true;
	return str;
}

function op(fn){ fn._Op = true; return fn;}


//TODO: reject multiple infix operators in the same expression
function resolve(args, rec){
	//recurse on nested array-expressions or callstreams
	args = map(function(e){ 
		if(e && e._Array){
			delete e._Array;
			return e;
		}
		return isArray(e) ? resolve(e, rec) :
				(e && e.CallStream) ? e.resolve() : e;
	}, args);
	
	//inject recursive calls
	if(rec)
		args = map(function(e){return e instanceof Recurse ? rec : e}, args);
	
	//execute functions between operators
	var fna = [], fn, newfna = [], i = 0, l = args.length;
	for(; i < l; ++i){
		var e = args[i], isOp = false;
		
		if(operators[e])
			isOp = true;
		if(e && e._String){
			isOp = false;
			e = e.toString();
		}
		if(e && e._Op)
			isOp = true;

		if(!isOp && i != (l-1))
			fna.push(e);
		else{
			if(i == (l-1))
				fna.push(e);
			if(fna.length> 1)
				fn = fna[0].apply(null, fna.slice(1));
			else
				fn = fna[0];
			newfna.push(fn);
			if(i != l-1)
				newfna.push(e);
			fna=[];
		}
	}
	args = newfna;

	//execute operators
	var	dir    = map(getFixityDir , args),
		strn   = map(getFixityStrn, args),
		max    = filter(strn, isDefined).sort().pop(),
		maxfst = indexOf(strn, max),
		maxlst = lastIndexOf(strn, max);
	
	return  dir[maxfst] == "l" ? splice_args(args, maxfst, rec) :
			dir[maxlst] == "r" ? splice_args(args, maxlst, rec) :
			dir[maxfst] == "x" ? splice_args(args, maxfst, rec) :
			args[0];
}

Array.prototype.resolve = function(){ return resolve(this) };

// -------------------------------------------------
// Callstream interface for the do notation
// -------------------------------------------------

function Recurse(){}

var recurse = new Recurse();

function cs(){
	return (function(args){
		function rec(s){return p(s)}

		var lines = [], p, resolved;
		lines.push(resolve(args, rec));

		function line(s){
			if(s instanceof ParseState)
				return (resolved ? p : line.resolve())(s);
				
			lines.push(resolve(arguments, rec));
			return line;
		}

		line.resolve = function(){
			if(resolved)
				return p;
			p = do_.apply(null, lines);
			lines = null;
			resolved = true;
			return p;
		}

		line.CallStream = true;

		return line;
	})(arguments);
}

