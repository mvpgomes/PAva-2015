package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Stack;

/**
 * TODO Note that this command can lead to StackOverflowError.
 */
public class RetryCommand extends Command {
    @Override
    @SuppressWarnings("unchecked")
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) throws Throwable {
        try {
            final MethodCallEntry e = stack.peek();
            final Method m = e.getInstanceClass().getDeclaredMethod(e.getMethodName(), e.getMethodArgsSig());
            return new Tuple<>(Boolean.TRUE, m.invoke(e.getInstance(), e.getMethodArgs()));
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
