package ist.meic.pa.command;

import ist.meic.pa.Debugger;
import ist.meic.pa.MethodCallEntry;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Stack;

/**
 * TODO Note that this command can lead to StackOverflowError.
 */
public class RetryCommand extends Command {
    @Override
    @SuppressWarnings("unchecked")
    public Object execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) {
        try {
            MethodCallEntry calledMethod = stack.peek();

            Class instanceClass = calledMethod.getInstanceClass();
            String methodName = calledMethod.getMethodName();
            Class[] argsClass = calledMethod.getMethodArgsSig();
            Method m  = instanceClass.getDeclaredMethod(methodName, argsClass);

            Object[] methodArgs = calledMethod.getMethodArgs();
            Object instance = calledMethod.getInstance();
            return m.invoke(instance, methodArgs);
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }
}
