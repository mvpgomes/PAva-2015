package test;

public class Example3 {
    public static class A {
        public A() throws Exception {
            System.out.println("Ola");
            throw new RuntimeException("No shit");
        }
    }

    public static void foo() {
        System.out.println("Hello");
    }
    public static void main(String[] args) throws Throwable {
        Integer.parseInt("a");
    }
}
