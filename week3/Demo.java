
interface Func<B, A> {
    B m(A x);
}

interface Pred<A> {
    boolean m(A x);
}

// Assuming null for empty list here, a choice we may regret
class List<T> {
    T head;
    List<T> tail;
    List(T x, List<T> xs) {
        head = x;
        tail = xs;
    }
    // ...

    // the advantage of a static method is it lets xs be null
    //  -- a more OO way would be a subclass for empty lists
    // a more efficient way in Java would be a messy while loop 
    // where you keep a mutable pointer to the previous element 
    // -- (try it if you do not believe it is messy)
    static <A,B>List <B> map (Func<B, A> f, List<A> xs) {
        if (xs == null)
            return null;
        return new List<B>(f.m(xs.head), map(f, xs.tail));
    }

    static <A> List<A> filter(Pred<A> f, List<A> xs) {
        if (xs == null)
            return null;
        if (f.m(xs.head))
            return new List<A>(xs.head, filter(f, xs.tail));
        return filter(f, xs.tail);
    }

    // * again recursion would be more elegant but less efficient
    // * again an instance method would be more common, but then
    // all clients have to special-case null
    static <A> int length(List<A> xs) {
        int ans = 0;
        while (xs != null) {
            ++ans;
            xs = xs.tail;
        }
        return ans;
    }

    // a more OO approach would be instance methods
    // can work but interacts poorly with null for empty list
    <B> List<B> map(Func<B, T> f) {
        if (this.tail == null)
            return null;
        return new List<B>(f.m(this.head), this.tail.map(f));
    }

    List<T> filter(Pred<T> f) {
        if (this.tail == null)
            return null;
        if (f.m(this.head))
            return new List<T>(this.head, this.tail.filter(f) );
        return filter(f, this.tail);
    }

    int length() {
        int ans = 0;
        List<T> self = this;
        while (self != null) {
            ++ans;
            self = self.tail;  // error: this = this.tail
        }
        return ans;
    }

}



class ExampleClients {
    static List<Integer> doubleAll(List<Integer> xs) {
        return List.map((new Func<Integer, Integer>() {
            // implementation
            public Integer m(Integer x) {
                return x * 2;
            }
        }), xs);
    }

    static int countNs(List<Integer> xs, final int n) {
        return List.length(List.filter(
            new Pred<Integer>() {
                // implementation
                public boolean m(Integer x) {
                    return x == n;
                }
            }, xs));
    }
}