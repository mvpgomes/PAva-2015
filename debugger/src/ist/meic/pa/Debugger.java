package ist.meic.pa;

import ist.meic.pa.command.Command;
import ist.meic.pa.parser.Parser;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

public class Debugger {

    private static Stack<MethodCallEntry> callStack = new Stack<>();

    private static Map<String, Command> commands = new HashMap<>();

    private static Map<String, Parser> parameterParser = new HashMap<>();

    public static void addCommand(String commandName, Command command) {
        commands.put(commandName, command);
    }

    public static void addParameterParser(String parameterType, Parser parser){
        parameterParser.put(parameterType,parser);
    }

    public static Parser getParameterParser(String parameterType) {
        return parameterParser.get(parameterType);
    }

    public static void addCall(Class instanceClass, Object instance, String methodName,
                               Class[] methodSig, Object[] methodArgs, Class resultSig) {
        final MethodCallEntry i = new MethodCallEntry(instanceClass, instance, methodName, methodSig, methodArgs, resultSig);
        callStack.push(i);
        System.out.println("Added method to call stack.");
        print(i);
    }

    /**
     * Called when an exception in the debugged code occurs.
     * @return may return values, depending on the debugger command used.
     */
    public static Object inspect() {
        // TODO to implement
        System.out.println("Inspecting method");
        return null;
    }

    private static void print(MethodCallEntry i) {
        System.out.println("Classname: " + i.getInstanceClass().getName());

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
            System.out.print(m.getSimpleName() + ", ");
        }
        System.out.println(")");
    }
}
