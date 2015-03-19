package test;

public class Example {
    public static double foo() {
        throw new RuntimeException("What an error...");
    }
    public static void main(String[] args) {
        System.out.println(foo());
    }
}
