package rb;

class RubyIterator {
  private var base : Dynamic;
  private var ref : Dynamic;
  private var at : Int;

  public function new(x : Dynamic, base : Dynamic) {
    ref = x;
    at = 0;
    this.base = base;
  }

  public function hasNext() : Bool {
    return at < ref.length;
  }

  public function next() : Dynamic {
    if (base==null) {
      var v = (untyped __js__("@ref[@at]"));
      at++;
      return v;
    }
    var v = (untyped __js__("@base[@ref[@at]]"));
    at++;
    return v;
  }
}

