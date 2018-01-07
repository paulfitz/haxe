/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
enum ValueType {
	TNull;
	TInt;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass( c : Class<Dynamic> );
	TEnum( e : Enum<Dynamic> );
	TUnknown;
}

@:coreApi class Type {

	public static function getClass<T>( o : T ) : Class<T> untyped {
		if( o == null )
			return null;
		return untyped __dotcall__(o,"class");
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic> untyped {
		if( o == null )
			return null;
		if (!(untyped __dotcall__(o,"respond_to?", "ISENUM__"))) {
		  return null;
		}
		return untyped __dotcall__(o,"class");
	}

	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> untyped {
		return untyped __dotcall__(c,"superclass");
	}


	public static function getClassName( c : Class<Dynamic> ) : String {
	  if (c==null) return null;
	  var name : String = null;
	  if (untyped __dotcall__(c,"respond_to?","haxe_name")) {
	    name = untyped __dotcall__(c,"haxe_name");
	  } else {
	    name = untyped __dotcall__(c,"name");
	  }
	  if (name=="Integer" || name=="Fixnum") name = "Int";
	  if (name=="Haxe::Ds::StringMap") name = "haxe.ds.StringMap";
	  return name;
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
	  if (e==null) return null;
	  var name : String = null;
	  if (untyped __dotcall__(e,"respond_to?","haxe_name")) {
	    name = untyped __dotcall__(e,"haxe_name");
	  } else {
	    name = untyped __dotcall__(e,"name");
	  }
	  if (name=="TrueClass") name = "Bool";
	  if (name=="FalseClass") name = "Bool";
	  return name;
	}

	public static function resolveClass( name : String ) : Class<Dynamic> untyped {
	  switch (name) {
	  case "Int": return Int;
	  case "Bool": return Bool;
	  case "Float": return Float;
	  case "String": return String;
	  case "Array": return Array;
	  default:
	    var cl : Class<Dynamic> = $hx_types[name];
	    return cl;
	  }
	  return null;
	}

	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped {
	  if (name=="Bool") return Bool;
	  var e : Dynamic = $hx_types[name];
	  return e;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T untyped {
		switch( args.length ) {
		case 0:
			return __new__(cl);
		case 1:
			return __new__(cl,args[0]);
		case 2:
			return __new__(cl,args[0],args[1]);
		case 3:
			return __new__(cl,args[0],args[1],args[2]);
		case 4:
			return __new__(cl,args[0],args[1],args[2],args[3]);
		case 5:
			return __new__(cl,args[0],args[1],args[2],args[3],args[4]);
		case 6:
			return __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5]);
		case 7:
			return __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6]);
		case 8:
			return __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7]);
		default:
			throw "Too many arguments";
		}
		return null;
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T untyped {
		__js__("function empty() {}; empty.prototype = cl.prototype");
		return __js__("new empty()");
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T {
  	        var f:Dynamic = Reflect.field(e,constr);
		if( f == null ) throw "No such constructor "+constr;
		if( Reflect.isFunction(f) ) {
			if( params == null ) throw "Constructor "+constr+" need parameters";
			return Reflect.callMethod(e,f,params);
		}
		if( params != null && params.length != 0 )
			throw "Constructor "+constr+" does not need parameters";
		return f;
	}

	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		var c : String = (untyped e.__constructs__)[index];
		if( c == null ) throw index+" is not a valid enum constructor index";
		return createEnum(e,c,params);
	}

	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
	  return untyped __js__("c.instance_methods.map{|x| x.to_s}"); /* need to add more here */
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		return Reflect.fields(c);
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		var a : Array<String> = untyped e.__constructs__;
		return a.copy();
	}

	public static function typeof( v : Dynamic ) : ValueType untyped {
	  if (v==null) return TNull;
	  switch( untyped __dotcall__(v,"class.to_s")) {
	  case "TrueClass": return TBool;
	  case "FalseClass": return TBool;
	  case "String": return TClass(String);
	  case "Integer": return TInt;
	  case "Fixnum": return TInt;
	  case "Float": return TFloat;
	  case "Proc": return TFunction;
	  case "Array": return TClass(Array);
	  case "Hash": return TObject;
	  default:
	    if (untyped __dotcall__(v,"respond_to?","ISENUM__"))
	      return TEnum(untyped __dotcall__(v,"class"));
	    if (untyped __dotcall__(v,"respond_to?","class"))
	      return TClass(untyped __dotcall__(v,"class"));
	    return TUnknown;
	  }
	}

	public static function enumEq<T>( a : T, b : T ) : Bool untyped {
	  return (a == b);
	}

	public inline static function enumConstructor( e : EnumValue ) : String {
		return untyped e.tag;
	}

	public inline static function enumParameters( e : EnumValue ) : Array<Dynamic> {
		return untyped e.params;
	}

	public inline static function enumIndex( e : EnumValue ) : Int {
		return untyped e.index;
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T> {
		var all = [];
		var cst : Array<String> = untyped e.__constructs__;
		for( c in cst ) {
			var v = Reflect.field(e,c);
			if( !Reflect.isFunction(v) )
				all.push(v);
		}
		return all;
	}

}

