package ist.meic.pa;

import java.util.Stack;

public class Debugger {
    private static Stack<CallInstance> callStack = new Stack<>();

    public static void addCall(Class instanceClass, Object instance, String methodName,
                               Class[] methodSig, Object[] methodArgs, Class resultSig) {
        final CallInstance i = new CallInstance(instanceClass, instance, methodName, methodSig, methodArgs, resultSig);
        callStack.push(i);
        
        print(i);
    }

    // Called when an exception occurs.
    public static void inspect() {
        // TODO to implement
    }

    private static void print(CallInstance i) {
        System.out.println("Debugging...");
        System.out.println("Classname: " + i.getInstanceClass().toString());

        if (i.getInstance() != null) {
            System.out.println("Instance.toString(): " + i.getInstance().toString());
        }

        if (i.getResultSig() == null) {
            System.out.print("Method: void ");
        } else {
            System.out.print("Method: " + i.getResultSig().toString() + " ");
        }

        System.out.print(i.getMethodName());
        System.out.print("(");
        for (Class m : i.getMethodSig()) {
            System.out.print(m.toString());
        }
        System.out.println(")");
    }
}
