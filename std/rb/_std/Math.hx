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

//@:coreApi
class Math
{
  public static var PI(get_PI,null) : Float;
  
  public static inline function get_PI() : Float {
    return untyped __dotcall__(Math,"PI");
  }

  public static var NaN(get_NaN,null) : Float;

  public static inline function get_NaN() : Float {
    return untyped __dotcall__(Math,"NAN");
  }

  public static var NEGATIVE_INFINITY(get_NEGATIVE_INFINITY,null) : Float;

  public static inline function get_NEGATIVE_INFINITY() : Float {
    return - untyped __dotcall__(Math,"INFINITY");
  }

  public static var POSITIVE_INFINITY(get_POSITIVE_INFINITY,null) : Float;

  public static inline function get_POSITIVE_INFINITY() : Float {
    return untyped __dotcall__(Math,"INFINITY");
  }

  public static inline function abs(v:Float):Float {
    return untyped v.abs;
  }

  public static inline function min(a:Float,b:Float):Float {
    return untyped [a,b].min;
  }

  public static inline function max(a:Float,b:Float):Float {
    return untyped [a,b].max;
  }

  public static inline function sin(v:Float):Float {
    return untyped __dotcall__(Math,"sin",v);
  }

  public static inline function cos(v:Float):Float {
    return untyped __dotcall__(Math,"cos",v);
  }
  
  public static inline function atan2(y:Float,x:Float):Float {
    return untyped __dotcall__(Math,"atan2",y,x);
  }

  public static inline function tan(v:Float):Float {
    return untyped __dotcall__(Math,"tan",v);
  }

  public static inline function exp(v:Float):Float {
    return untyped __dotcall__(Math,"exp",v);
  }
  
  public static inline function log(v:Float):Float {
    return untyped __dotcall__(Math,"log",v);
  }

  public static inline function sqrt(v:Float):Float {
    return untyped __dotcall__(Math,"sqrt",v);
  }

  public static inline function round(v:Float):Int {
    return untyped v.round;
  }

  public static inline function floor(v:Float):Int {
    return untyped v.floor;
  }

  public static inline function ceil(v:Float):Int {
    return untyped v.ceil;
  }

  //static function atan(v:Float):Float;
  
  public static inline function fround(v:Float):Float {
    return untyped v.round;
  }

  public static inline function ffloor(v:Float):Float {
    return untyped v.floor;
  }

  public static inline function fceil(v:Float):Float {
    return untyped v.ceil;
  }

  //static function asin(v:Float):Float;
  //static function acos(v:Float):Float;

  public static inline function pow(v:Float,exp:Float):Float {
    return untyped __pow__(v,exp);
  }

  //static function random() : Float;

  public static inline function isFinite( f : Float ) : Bool {
    return untyped __dotcall__(f,"finite?");
  }

  public static inline function isNaN( f : Float ) : Bool {
    return untyped __dotcall__(f,"nan?");
  }
}


