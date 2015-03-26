package ist.meic.pa;

import ist.meic.pa.command.*;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
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

    public void addCall(MethodCallEntry e) {
        callStack.push(e);
//        System.out.println(String.format("Added method \"%s\" to call stack.", e.getInstanceClass().getName() + "." + e.getMethodName()));
    }

    public void removeLastCall() {
        MethodCallEntry e = callStack.pop();
//        System.out.println(String.format("Removed method \"%s\" from call stack.", e.getInstanceClass().getName() + "." + e.getMethodName()));
    }

    @SuppressWarnings("unchecked")
    public Object proxyMethod(Class instanceClass, Object instance, String methodName, Class[] methodArgsSig, Object[] methodArgs, Class resultSig) throws Throwable {
        final MethodCallEntry e = new MethodCallEntry(instanceClass, instance, methodName, methodArgsSig, methodArgs, resultSig);
        addCall(e);
        try {
            Method m = instanceClass.getDeclaredMethod(methodName, methodArgsSig);
            return m.invoke(instance, methodArgs);
        } catch (Throwable t) {
            System.out.println(t.getCause().toString());
            return repl(t);
        } finally {
            removeLastCall();
        }
    }

    @SuppressWarnings("unchecked")
    public Object proxyConstructor(String methodName, Class[] methodArgsSig, Object[] methodArgs, Class resultSig) throws Throwable {
        final MethodCallEntry e = new MethodCallEntry(null, null, methodName, methodArgsSig, methodArgs, resultSig);
        addCall(e);
        try {
            Constructor c = resultSig.getDeclaredConstructor(methodArgsSig);
            return c.newInstance(methodArgs);
        } catch (Throwable t) {
            System.out.println(t.getCause().toString());
            return repl(t);
        } finally {
            removeLastCall();
        }
    }

    /**
     * Called when an exception in the debugged code occurs.
     *
     * @return may return values, depending on the debugger command used.
     */
    public Object repl(Throwable t) throws Throwable {
        Tuple<Boolean, Object> result;
        do {
            result = null;
            System.out.print(":> ");
            String[] cmdArgs = null;

            try {
                cmdArgs = in.readLine().split(" ");
            } catch (Throwable e) {
                System.out.println("Error reading from input.");
                System.exit(1);
            }

            Command cmd = commands.get(cmdArgs[0]);
            if (cmd == null) {
                System.out.println("Unknown command. Try again.");
            } else {
                // TODO instead of passing callStack, pass the callStack.Iterator(). This prevents commands from changing the callStack directly.
                result = cmd.execute(callStack, Arrays.copyOfRange(cmdArgs, 1, cmdArgs.length), t);
            }
        } while (!result.getFirst());
        return result.getSecond();
    }
}
