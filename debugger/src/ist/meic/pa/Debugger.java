package ist.meic.pa;

import java.util.Stack;

public class Debugger {
    private static Stack<CallInstance> callStack = new Stack<>();

    public static Object inspect(Class instanceClass, Object instance, String methodName,
                                 Class[] methodSig, Object[] methodArgs, Class resultSig) {
        System.out.println("Debugging...");
        System.out.println("Classname: " + instanceClass.toString());

        if (instance != null) {
            System.out.println("Instance.toString(): " + instance.toString());
        }

        if (resultSig == null) {
            System.out.print("Method: void ");
        } else {
            System.out.print("Method: " + resultSig.toString() + " ");
        }

        System.out.print(methodName);
        System.out.print("(");
        for (int i = 0; i < methodSig.length; i++) {
            System.out.print(methodSig[i].toString() + "=" + methodArgs[i].toString() + ", ");
        }
        System.out.println(")");
        return null;
    }

    public static void inspect() {
        System.out.println("Nice");
    }
}
