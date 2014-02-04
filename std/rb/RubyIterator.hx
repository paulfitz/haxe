package rb;

class RubyIterator {
  private var ref : Dynamic;
  private var at : Int;
  private var len : Int;

  public function new(x : Dynamic) {
    if (untyped __dotcall__(x,"is_a?",__js__("Hash"))) {
      ref = untyped __dotcall__(x,"values.each");
      at = 0;
      len = untyped __dotcall__(x,"size");
    } else if (untyped __dotcall__(x,"respond_to?","each")) {
      ref = untyped __dotcall__(x,"each");
      at = 0;
      len = untyped __dotcall__(x,"size");
    } else if (untyped __dotcall__(x,"respond_to?","iterator")) {
      ref = untyped __dotcall__(x,"iterator");
      at = -1;
      if (!untyped __dotcall__(ref,"respond_to?","has_next")) {
	at = -2;
      }
    } else {
      ref = x;
      at = -2;
    }
  }

  public function hasNext() : Bool {
    if (at==-1) return ref.hasNext();
    if (at==-2) return ref[untyped __js__(":has_next")].call();
    return at < len;
  }

  public function next() : Dynamic {
    if (at==-1) return ref.next();
    if (at==-2) return ref[untyped __js__(":_next")].call();
    at++;
    return untyped __dotcall__(ref,"next");
  }
}

