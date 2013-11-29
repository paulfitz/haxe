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
package haxe.ds;

@:coreApi class StringMap<T> implements Map.IMap<String,T> {

  public function new() : Void {
  }

	public inline function set( key : String, value : T ) : Void {
	  //untyped h[key] = value;
	  untyped __set__(this,key,value);
	}

	public inline function get( key : String ) : Null<T> {
	  //return untyped h[key];
	  return untyped __get__(this,key);
	}

        public inline function exists( key : String ) : Bool {
	  return untyped __dotcall__(this,"include?",key);
	  //return untyped __dotcall__(h,"include?",key);
	}

	public inline function remove( key : String ) : Bool {
	  //if (!exists(key)) return false;
	  return untyped __dotcall__(this,"delete",key);
	  //untyped __js__("@h.delete(key)");
	  //return true;
	}

	public inline function keys() : Iterator<String> {
	  return new rb.RubyIterator(untyped __dotcall__(this,"keys"),null);
	}

	public inline function iterator() : Iterator<T> {
	  return new rb.RubyIterator(untyped __dotcall__(this,"keys"),this);
	}

	public inline function toString() : String {
	  return "not implemented yet";
	}

}
