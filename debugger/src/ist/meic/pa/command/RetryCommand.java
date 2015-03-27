package ist.meic.pa.command;

import ist.meic.pa.Debugger;
import ist.meic.pa.GenericParser;
import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.lang.reflect.InvocationTargetException;
import java.util.Stack;

/**
 * The retry command invokes the last method in the method call stack (the method that generated the exception that led
 * to the repl).
 */
public class RetryCommand implements Command {
    /**
     * Note that this implementation removes the last element from the stack.
     */
    public Object executeRetryCommand(Class instanceClass, Object instance, String methodName,
                                      Class[] methodArgsSig, Object[] args, Class resultSig) throws Throwable {

        return Debugger.getInstance().callProxyMethod(instanceClass, instance, methodName, methodArgsSig, args, resultSig);
    }

    public Object executeModifiedRetryCommand(Class instanceClass, Object instance, String methodName,
                                              Class[] methodArgsSig, String[] args, Class resultSig) throws Throwable {

        Object[] methodArgs = new Object[args.length];
        for(int i = 0; i < args.length; i++) {
            methodArgs[i] = GenericParser.parse(methodArgsSig[i], args[i]);
        }

        return Debugger.getInstance().callProxyMethod(instanceClass, instance, methodName, methodArgsSig, methodArgs, resultSig);
    }

    @Override
    @SuppressWarnings("unchecked")
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) throws Throwable {
        try {
            final MethodCallEntry calledMethod = stack.pop();

            Class instanceClass = calledMethod.getInstanceClass();
            Object instance = calledMethod.getInstance();

            String methodName = calledMethod.getMethodName();
            Class[] methodArgsSig = calledMethod.getMethodArgsSig();
            Object[] methodArgs = calledMethod.getMethodArgs();
            Class resultSig = calledMethod.getResultSig();

            Object res = (args.length == 0) ? executeRetryCommand(instanceClass, instance, methodName, methodArgsSig, methodArgs, resultSig) :
                    executeModifiedRetryCommand(instanceClass, instance, methodName, methodArgsSig, args, resultSig);

            return new Tuple<>(Boolean.TRUE, res);
        } catch (NoSuchMethodException | IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            // This exception occurs when an exception is thrown in the proxy method, and
            // it is wrapped by the InvocationTargetException, which we do not want (we
            // want the real message).
            throw e.getCause();
        }
    }
}
