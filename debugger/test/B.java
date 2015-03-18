public class B {
    double b = 3.14;

    public double bar(int x) {
        System.out.println("Inside B.bar");
        return (1 / x);
    }

    public double baz(Object x) {
        System.out.println("Inside B.baz");
        System.out.println(x.toString());
        return b;
    }
}