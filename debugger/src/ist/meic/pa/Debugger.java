package ist.meic.pa;

import ist.meic.pa.command.Command;
import ist.meic.pa.parser.Parser;

import java.util.Map;
import java.util.Stack;

public class Debugger {

    private static Stack<MethodCallEntry> callStack = new Stack<>();

    private Map<String, Command> commands;

    private Map<String, Parser> parameterParser;


    public Debugger(Map<String, Command> commands, Map<String, Parser> parameterParser) {
        this.commands = commands;
        this.parameterParser = parameterParser;
    }

    public void addCommand(String commandName, Command command) {
        this.commands.put(commandName, command);
    }

    public void addParameterParser(String parameterType, Parser parser){
        this.parameterParser.put(parameterType,parser);
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
    public Object inspect() {
        // TODO to implement
        DebuggerBootstrap.bootstrap(this);
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
