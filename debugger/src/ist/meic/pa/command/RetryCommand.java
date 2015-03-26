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

            Class instanceClass = calledMethod.getInstanceClass();
            String methodName = calledMethod.getMethodName();
            Class[] argsClass = calledMethod.getMethodArgsSig();
            Method m  = instanceClass.getDeclaredMethod(methodName, argsClass);

            Object[] methodArgs = calledMethod.getMethodArgs();
            Object instance = calledMethod.getInstance();
            return new Tuple<>(Boolean.TRUE, m.invoke(instance, methodArgs));
        } catch (NoSuchMethodException | IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            throw e.getCause();
        }
    }
}
