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

    public Object callProxyMethod(Class instanceClass, Object instance, String methodName, Class[] methodArgsSig, Object[] methodArgs, Class resultSig) throws Throwable {
        return (instanceClass != null || instance != null) ? proxyMethod(instanceClass, instance, methodName, methodArgsSig, methodArgs, resultSig) :
                proxyConstructor(methodName, methodArgsSig, methodArgs, resultSig);
    }

    @SuppressWarnings("unchecked")
    public Object proxyMethod(Class instanceClass, Object instance, String methodName, Class[] methodArgsSig, Object[] methodArgs, Class resultSig) throws Throwable {
        final MethodCallEntry e = new MethodCallEntry(instanceClass, instance, methodName, methodArgsSig, methodArgs, resultSig);
        callStack.push(e);
        try {
            Method m = instanceClass.getDeclaredMethod(methodName, methodArgsSig);
            Object res = m.invoke(instance, methodArgs);
            callStack.pop();
            return res;
        } catch (Throwable t) {
            System.out.println(t.getCause().toString());
            return repl(t);
        }
    }

    @SuppressWarnings("unchecked")
    public Object proxyConstructor(String methodName, Class[] methodArgsSig, Object[] methodArgs, Class resultSig) throws Throwable {
        final MethodCallEntry e = new MethodCallEntry(null, null, methodName, methodArgsSig, methodArgs, resultSig);
        callStack.push(e);
        try {
            Constructor c = resultSig.getDeclaredConstructor(methodArgsSig);
            Object res =  c.newInstance(methodArgs);
            callStack.pop();
            return res;
        } catch (Throwable t) {
            System.out.println(t.getCause().toString());
            return repl(t);
        }
    }

    /**
     * Called when an exception in the debugged code occurs.
     *
     * @return may return values, depending on the debugger command used.
     */
    private Object repl(Throwable t) throws Throwable {
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
                result = cmd.execute(callStack, Arrays.copyOfRange(cmdArgs, 1, cmdArgs.length), t);
            }
        } while (!result.getFirst());
        return result.getSecond();
    }
}
