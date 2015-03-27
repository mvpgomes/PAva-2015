package ist.meic.pa.command;

import ist.meic.pa.Debugger;
import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Stack;

public class RetryCommand extends Command {

    public Object executeRetryCommand(Class instanceClass, Object instance, String methodName,
                                      Class[] methodArgsSig, Object[] args, Class resultSig) throws Throwable {

        return Debugger.getInstance().callProxyMethod(instanceClass, instance, methodName, methodArgsSig, args, resultSig);
    }

    public Object executeModifiedRetryCommand(Class instanceClass, Object instance, String methodName,
                                              Class[] methodArgsSig, String[] args, Class resultSig) throws Throwable {

        Object[] methodArgs = new Object[args.length];
        for(int i = 0; i < args.length; i++) {
            methodArgs[i] = getParameterParser(methodArgsSig[i].getSimpleName()).parse(args[i]);
        }

        return Debugger.getInstance().callProxyMethod(instanceClass, instance, methodName, methodArgsSig, methodArgs, resultSig);
    }

    @Override
    @SuppressWarnings("unchecked")
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) throws Throwable {
        try {
            final MethodCallEntry calledMethod = stack.peek();
            Debugger.getInstance().removeLastCall();

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
