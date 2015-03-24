package ist.meic.pa;

import ist.meic.pa.command.*;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

public class Debugger {
    private static final Map<String, Command> commands = new HashMap<String, Command>() {{
        put("Abort", new AbortCommand());
        put("Info", new InfoCommand());
        put("Return", new ReturnCommand());
        put("Throw", new ThrowCommand());
        put("Get", new GetCommand());
        put("Set", new SetCommand());
        put("Retry", new RetryCommand());
    }};

    private static Debugger instance;

    private BufferedReader in;
    private Stack<MethodCallEntry> callStack;

    private Debugger() {
        this.in = new BufferedReader(new InputStreamReader(System.in));
        this.callStack = new Stack<>();
    }

    public static Debugger getInstance() {
        if (instance == null) {
            instance = new Debugger();
        }
        return instance;
    }

    public void addCall(Class instanceClass, Object instance, String methodName,
                        Class[] methodSig, Object[] methodArgs, Class resultSig) {
        final MethodCallEntry i = new MethodCallEntry(instanceClass, instance, methodName, methodSig, methodArgs, resultSig);
        callStack.push(i);
        System.out.println("Added method to call stack.");
        print(i);
    }

    /**
     * Called when an exception in the debugged code occurs.
     *
     * @return may return values, depending on the debugger command used.
     */
    public Object inspect(Throwable t) throws Throwable {
        Object response;
        do {
            System.out.print(":> ");
            String[] cmdArgs = null;

            try {
                cmdArgs = in.readLine().split(" ");
            } catch (Throwable e) {
                System.out.println("Invalid input");
                System.exit(1);
            }

            Command cmd = commands.get(cmdArgs[0]);
            response = cmd.execute(callStack, callStack.peek().getInstance(), Arrays.copyOfRange(cmdArgs, 1, cmdArgs.length), t);
        } while (response == null);

        return response;
    }

    private void print(MethodCallEntry i) {
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
