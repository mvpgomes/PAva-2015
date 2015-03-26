package ist.meic.pa.command;

import ist.meic.pa.Debugger;
import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Stack;

/**
 * TODO Note that this command can lead to StackOverflowError.
 * TODO ERROR here replace this code by calling proxy directly.
 */
public class RetryCommand extends Command {
    @Override
    @SuppressWarnings("unchecked")
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) throws Throwable {
        try {
            MethodCallEntry calledMethod = stack.peek();
            Debugger.getInstance().removeLastCall();

            Class instanceClass = calledMethod.getInstanceClass();
            Object instance = calledMethod.getInstance();

            String methodName = calledMethod.getMethodName();
            Class[] methodArgsSig = calledMethod.getMethodArgsSig();
            Object[] methodArgs = calledMethod.getMethodArgs();
            Class resultSig = calledMethod.getResultSig();

            Object res = Debugger.getInstance().callProxyMethod(instanceClass, instance, methodName, methodArgsSig, methodArgs, resultSig);

            return new Tuple<>(Boolean.TRUE, res);
        } catch (NoSuchMethodException | IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            throw e.getCause();
        }
    }
}
