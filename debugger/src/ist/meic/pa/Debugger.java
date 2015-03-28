package ist.meic.pa;

import ist.meic.pa.command.*;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

/**
 * This class provides a REPL when an exception occurs. It provides introspection capabilities that allow the developer
 * to better understand the state of the application when the exception occured. It also provides intercession capabilities
 * that allow the developer to change the state and test new hypothesis.
 */
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
        return (instanceClass != null || instance != null) ?
                proxyMethod(instanceClass, instance, methodName, methodArgsSig, methodArgs, resultSig) :
                proxyConstructor(methodName, methodArgsSig, methodArgs, resultSig);
    }

    /**
     * This method just saves the information about the method call and then executes it. If an exception occurs, the
     * REPL is invoked.
     */
    @SuppressWarnings("unchecked")
    public Object proxyMethod(Class instanceClass, Object instance, String methodName, Class[] methodArgsSig, Object[] methodArgs, Class resultSig) throws Throwable {
        final MethodCallEntry e = new MethodCallEntry(instanceClass, instance, methodName, methodArgsSig, methodArgs, resultSig);
        callStack.push(e);
        try {
            Method m = instanceClass.getDeclaredMethod(methodName, methodArgsSig);
            Object res = m.invoke(instance, methodArgs);
            callStack.pop();
            return res;
        } catch (NoSuchMethodException | IllegalAccessException t) {
            // This error should never happen.
            t.printStackTrace();
            System.exit(1);
            return null;
        } catch (InvocationTargetException t) {
            System.out.println(t.getTargetException().toString());
            return repl(t.getTargetException());
        }
    }

    /**
     * This method just saves the information about the constructor call and then executes it. If an exception occurs,
     * the REPL is invoked.
     */
    @SuppressWarnings("unchecked")
    public Object proxyConstructor(String methodName, Class[] methodArgsSig, Object[] methodArgs, Class resultSig) throws Throwable {
        final MethodCallEntry e = new MethodCallEntry(null, null, methodName, methodArgsSig, methodArgs, resultSig);
        callStack.push(e);
        try {
            Constructor c = resultSig.getDeclaredConstructor(methodArgsSig);
            Object res =  c.newInstance(methodArgs);
            callStack.pop();
            return res;
        } catch (NoSuchMethodException | IllegalAccessException | InstantiationException t) {
            // This error should never happen.
            t.printStackTrace();
            System.exit(1);
            return null;
        } catch (InvocationTargetException t) {
            System.out.println(t.getTargetException().toString());
            return repl(t.getTargetException());
        }
    }

    /**
     * Called when an exception in the debugged code occurs.
     *
     * @return may return values, depending on the command used.
     */
    private Object repl(Throwable t) throws Throwable {
        Tuple<Boolean, Object> result;
        do {
            result = new Tuple<>(Boolean.FALSE, null);
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
