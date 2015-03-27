package ist.meic.pa.command;

import ist.meic.pa.Debugger;
import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.lang.reflect.InvocationTargetException;
import java.util.Stack;

/**
 * The retry command invokes the last method in the method call stack (the method that generated the exception that led
 * to the repl).
 */
public class RetryCommand extends Command {
    /**
     * Note that this implementation removes the last element from the stack.
     */
    @Override
    @SuppressWarnings("unchecked")
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) throws Throwable {
        try {
            final MethodCallEntry e = stack.pop();
            return new Tuple<>(Boolean.TRUE, Debugger.getInstance().callProxyMethod(e.getInstanceClass(),
                    e.getInstance(), e.getMethodName(), e.getMethodArgsSig(), e.getMethodArgs(), e.getResultSig()));
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
