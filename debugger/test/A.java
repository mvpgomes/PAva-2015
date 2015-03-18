public class A {
    int a = 1;

    public double foo(B b) {
        System.out.println("Inside A.foo");
        if (a == 1) {
            return b.bar(0);
        } else {
            return b.baz(null);
        }
    }
}